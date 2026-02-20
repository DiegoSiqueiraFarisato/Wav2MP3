unit UThreadConverter;

interface

uses
  Windows, SysUtils, Classes, Forms, ComCtrls, StdCtrls,
  UEncoderTypes, BladeEnc, LameEnc;

type
  TThreadConverter = class(TThread)
  private
    FEncoders: TInterfaceList;
    FEncoderIndex: Integer;
    FEncoderConfig: TEncoderConfig;
    FOwnedWavList: TStringList;
    FOutputDir: string;
    FProgressBar: TProgressBar;
    FFileLabel: TLabel;
    FCancelled: PBoolean;
    FWorkEvent: THandle;
    FOnComplete: TNotifyEvent;

    { Sync transfer fields }
    FSyncLabelText: string;
    FSyncProgressPos: Integer;
    FSyncProgressMax: Integer;
    FSyncErrorMsg: string;

    procedure ProcessAllFiles;
    procedure PrepareStreams(var WavFile, Mp3File: TFileStream;
      var WavMemory, Mp3Memory: TMemoryStream;
      const WavPath, Mp3Path: string);
    procedure CleanupStreams(var WavFile, Mp3File: TFileStream;
      var WavMemory, Mp3Memory: TMemoryStream;
      var Stream: THBE_STREAM; const Mp3Path: string);
    procedure ConvertFile(const WavPath, Mp3Path: string);

    { Sync helpers — executed on main thread }
    procedure SyncUpdateLabel;
    procedure SyncUpdateProgress;
    procedure SyncSetProgressMax;
    procedure SyncResetProgress;
    procedure SyncShowError;
    procedure SyncComplete;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ConvertWavToMP3(Priority: TThreadPriority;
      WavList: TStringList; const OutputDir: string;
      const Config: TEncoderConfig;
      FileLabel: TLabel; ProgressBar: TProgressBar;
      CancelFlag: PBoolean);

    function GetEncoderVersions: TStrings;
    function GetEncoderCount: Integer;

    property EncoderIndex: Integer read FEncoderIndex write FEncoderIndex;
    property OnComplete: TNotifyEvent read FOnComplete write FOnComplete;
  end;

function ExtractFileNameWithoutExt(const Path: string): string;
function IsFileInUse(const FileName: string): Boolean;

implementation

{ Utility functions }

function ExtractFileNameWithoutExt(const Path: string): string;
var
  Name: string;
begin
  Name := ExtractFileName(Path);
  Result := Copy(Name, 1, Length(Name) - Length(ExtractFileExt(Name)));
end;

function IsFileInUse(const FileName: string): Boolean;
var
  HFileRes: HFILE;
begin
  Result := False;
  if not FileExists(FileName) then
    Exit;
  HFileRes := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE, 0, nil,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  Result := (HFileRes = INVALID_HANDLE_VALUE);
  if not Result then
    CloseHandle(HFileRes);
end;

function SkipWavHeader(WavFile: TFileStream): DWORD;
var
  ChunkID: array[0..3] of AnsiChar;
  ChunkSize: DWORD;
begin
  Result := 44; // fallback: standard PCM header size
  if WavFile.Size < 12 then Exit;
  WavFile.Seek(12, soFromBeginning); // skip 'RIFF' + file-size + 'WAVE'
  while WavFile.Position + 8 <= WavFile.Size do
  begin
    if WavFile.Read(ChunkID, 4) < 4 then Break;
    if WavFile.Read(ChunkSize, 4) < 4 then Break;
    if (ChunkID[0] = 'd') and (ChunkID[1] = 'a') and
       (ChunkID[2] = 't') and (ChunkID[3] = 'a') then
    begin
      Result := DWORD(WavFile.Position); // now sitting at first PCM byte
      Exit;
    end;
    WavFile.Seek(ChunkSize, soFromCurrent);
  end;
  WavFile.Seek(Result, soFromBeginning); // seek to fallback offset
end;

{ TThreadConverter }

constructor TThreadConverter.Create;
var
  AppPath: string;
  Encoder: IEncoder;
  Rec: TSearchRec;
  DLLName: string;
begin
  inherited Create(True);  // create suspended
  FreeOnTerminate := False;
  FEncoders := TInterfaceList.Create;
  FEncoderIndex := 0;
  FOwnedWavList := TStringList.Create;

  AppPath := ExtractFilePath(Application.ExeName);

  if FindFirst(AppPath + '*.DLL', faAnyFile, Rec) = 0 then
  begin
    try
      repeat
        DLLName := UpperCase(Rec.Name);
        Encoder := nil;

        if DLLName = 'BLADE_ENC.DLL' then
        begin
          Encoder := TBladeEncoder.Create;
          Encoder.LoadDLL(AppPath + Rec.Name);
        end
        else if (DLLName = 'LAME_ENC.DLL') or (DLLName = 'LIBMP3LAME.DLL') then
        begin
          Encoder := TLameEncoder.Create;
          Encoder.LoadDLL(AppPath + Rec.Name);
        end;

        if (Encoder <> nil) and Encoder.IsLoaded then
          FEncoders.Add(Encoder);
      until FindNext(Rec) <> 0;
    finally
      FindClose(Rec);
    end;
  end;

  FWorkEvent := CreateEvent(nil, True, False, nil);  // manual-reset, initially non-signaled

end;

destructor TThreadConverter.Destroy;
begin
  Terminate;
  SetEvent(FWorkEvent);  // wake thread so it can exit
  WaitFor;
  CloseHandle(FWorkEvent);
  FOwnedWavList.Free;
  FEncoders.Free;
  inherited Destroy;
end;

function TThreadConverter.GetEncoderVersions: TStrings;
begin
  Result := TStringList.Create;
  for var i := 0 to FEncoders.Count - 1 do
    Result.Add((FEncoders[i] as IEncoder).GetVersion);
end;

function TThreadConverter.GetEncoderCount: Integer;
begin
  Result := FEncoders.Count;
end;

procedure TThreadConverter.ConvertWavToMP3(Priority: TThreadPriority;
  WavList: TStringList; const OutputDir: string;
  const Config: TEncoderConfig;
  FileLabel: TLabel; ProgressBar: TProgressBar;
  CancelFlag: PBoolean);
begin
  try // Add try block
    FOwnedWavList.Assign(WavList);
    FOutputDir := OutputDir;
    FEncoderConfig := Config;
    FFileLabel := FileLabel;
    FProgressBar := ProgressBar;
    Self.Priority := Priority;
    FCancelled := CancelFlag;
    if not Started then
      Start;  // begin the wait loop
    SetEvent(FWorkEvent);  // wake the thread
    //ShowMessage('ConvertWavToMP3 called and event signaled.'); // <--- ADDED FOR DIAGNOSIS
  except
    on E: Exception do
      //ShowMessage('Exception in ConvertWavToMP3: ' + E.Message); // Catch and report exception
  end; // End try block
end;

procedure TThreadConverter.Execute;
begin
  //ShowMessage('Thread Execute method entered.'); // <--- ADDED FOR DIAGNOSIS
  while not Terminated do
  begin
    WaitForSingleObject(FWorkEvent, INFINITE);
    if Terminated then
      Break;
    ResetEvent(FWorkEvent);
    ProcessAllFiles;
    Synchronize(SyncComplete);
  end;
end;

procedure TThreadConverter.ProcessAllFiles;
var
  i: Integer;
  WavPath, Mp3Path, FileName: string;
begin
  for i := 0 to FOwnedWavList.Count - 1 do
  begin
    if FCancelled^ then
      Break;

    WavPath := FOwnedWavList.Strings[i];
    Mp3Path := FOutputDir + ExtractFileNameWithoutExt(WavPath) + '.mp3';
    FileName := ExtractFileName(WavPath);

    FSyncLabelText := 'Convertendo: '#13 + FileName;
    Synchronize(SyncUpdateLabel);

    if (not IsFileInUse(Mp3Path)) and (not IsFileInUse(WavPath)) then
      ConvertFile(WavPath, Mp3Path);
  end;

  FSyncLabelText := 'Selecione os Arquivos';
  Synchronize(SyncUpdateLabel);
end;

procedure TThreadConverter.PrepareStreams(var WavFile, Mp3File: TFileStream;
  var WavMemory, Mp3Memory: TMemoryStream;
  const WavPath, Mp3Path: string);
begin
  if (WavFile = nil) and (Mp3File = nil) then
  begin
    try
      Mp3File := TFileStream.Create(Mp3Path, fmCreate or fmShareDenyNone);
      Mp3Memory := TMemoryStream.Create;
      WavFile := TFileStream.Create(WavPath, fmOpenRead);
      WavMemory := TMemoryStream.Create;
    except
      on E: Exception do
      begin
        FreeAndNil(WavFile);
        FreeAndNil(Mp3File);
        FreeAndNil(Mp3Memory);
        FreeAndNil(WavMemory);
      end;
    end;
  end;
end;

procedure TThreadConverter.CleanupStreams(var WavFile, Mp3File: TFileStream;
  var WavMemory, Mp3Memory: TMemoryStream;
  var Stream: THBE_STREAM; const Mp3Path: string);
var
  Encoder: IEncoder;
begin
  if (Stream <> 0) and (FEncoderIndex >= 0) and (FEncoderIndex < FEncoders.Count) then
  begin
    Encoder := FEncoders[FEncoderIndex] as IEncoder;
    Encoder.CloseStream(Stream);
    Stream := 0;
  end;

  FreeAndNil(WavFile);
  FreeAndNil(Mp3File);
  FreeAndNil(Mp3Memory);
  FreeAndNil(WavMemory);

  if FCancelled^ then
    DeleteFile(Mp3Path);
end;

procedure TThreadConverter.ConvertFile(const WavPath, Mp3Path: string);
var
  Encoder: IEncoder;
  NumSamples, MP3BufferSize: DWORD;
  FileSize, Completed, TotalChunk, ReadSize, BytesRead, BytesWritten,
    WriteSize, FinalSize: DWORD;
  Offset: DWORD;
  DataOffset: DWORD;
  Err: BE_ERR;
  WavFile, Mp3File: TFileStream;
  WavMemory, Mp3Memory: TMemoryStream;
  Stream: THBE_STREAM;
begin
  if (FEncoderIndex < 0) or (FEncoderIndex >= FEncoders.Count) then
    Exit;

  Encoder := FEncoders[FEncoderIndex] as IEncoder;

  WavFile := nil;
  Mp3File := nil;
  WavMemory := nil;
  Mp3Memory := nil;
  Stream := 0;

  FSyncProgressMax := 0;
  Synchronize(SyncSetProgressMax);

  if FCancelled^ then
    Exit;

  PrepareStreams(WavFile, Mp3File, WavMemory, Mp3Memory, WavPath, Mp3Path);

  if (WavFile = nil) or (Mp3File = nil) then
    Exit;

  Encoder.Configure(FEncoderConfig);

  Err := Encoder.InitStream(NumSamples, MP3BufferSize, Stream);
  if Err <> BE_ERR_SUCCESSFUL then
  begin
    FSyncErrorMsg := 'Erro ao Abrir : ' + IntToStr(Err);
    Synchronize(SyncShowError);
    CleanupStreams(WavFile, Mp3File, WavMemory, Mp3Memory, Stream, Mp3Path);
    Exit;
  end;

  if (Mp3Memory = nil) or FCancelled^ then
  begin
    CleanupStreams(WavFile, Mp3File, WavMemory, Mp3Memory, Stream, Mp3Path);
    Exit;
  end;
  Mp3Memory.SetSize(MP3BufferSize);

  if (WavMemory = nil) or FCancelled^ then
  begin
    CleanupStreams(WavFile, Mp3File, WavMemory, Mp3Memory, Stream, Mp3Path);
    Exit;
  end;
  WavMemory.SetSize(NumSamples);

  DataOffset := SkipWavHeader(WavFile); // positions file at first PCM byte
  FileSize := WavFile.Size - DataOffset;
  Completed := 0;
  FSyncProgressMax := FileSize;
  Synchronize(SyncSetProgressMax);
  TotalChunk := NumSamples * 2;

  while (Completed <> FileSize) and (not FCancelled^) do
  begin
    Offset := Completed + TotalChunk;
    if Offset < FileSize then
      ReadSize := TotalChunk
    else
      ReadSize := FileSize - Completed;

    if WavFile <> nil then
      BytesRead := WavFile.Read(WavMemory.Memory^, ReadSize)
    else
    begin
      FCancelled^ := True;
      Break;
    end;

    if BytesRead <> ReadSize then
    begin
      FSyncErrorMsg := 'Erro de Leitura!';
      Synchronize(SyncShowError);
      FCancelled^ := True;
      CleanupStreams(WavFile, Mp3File, WavMemory, Mp3Memory, Stream, Mp3Path);
      Break;
    end;

    Offset := ReadSize div 2;
    if (Mp3Memory <> nil) and (WavMemory <> nil) then
      Err := Encoder.EncodeChunk(Stream, Offset, WavMemory.Memory,
        Mp3Memory.Memory, WriteSize)
    else
    begin
      FCancelled^ := True;
      Break;
    end;

    if Err <> BE_ERR_SUCCESSFUL then
    begin
      FSyncErrorMsg := 'Falha na Convers' + #227 + 'o!' + IntToStr(Err);
      Synchronize(SyncShowError);
      FCancelled^ := True;
      CleanupStreams(WavFile, Mp3File, WavMemory, Mp3Memory, Stream, Mp3Path);
      Break;
    end;

    if Mp3Memory <> nil then
      BytesWritten := Mp3File.Write(Mp3Memory.Memory^, WriteSize)
    else
    begin
      FCancelled^ := True;
      Break;
    end;

    if WriteSize <> BytesWritten then
    begin
      FSyncErrorMsg := 'Erro de Grava' + #231 + #227 + 'o!';
      Synchronize(SyncShowError);
      FCancelled^ := True;
      CleanupStreams(WavFile, Mp3File, WavMemory, Mp3Memory, Stream, Mp3Path);
      Break;
    end;

    Completed := Completed + ReadSize;
    FSyncProgressPos := Completed;
    Synchronize(SyncUpdateProgress);
  end;

  if FCancelled^ then
  begin
    CleanupStreams(WavFile, Mp3File, WavMemory, Mp3Memory, Stream, Mp3Path);
    Synchronize(SyncResetProgress);
    Exit;
  end;

  if Mp3Memory <> nil then
    Err := Encoder.DeinitStream(Stream, Mp3Memory.Memory, FinalSize)
  else
  begin
    CleanupStreams(WavFile, Mp3File, WavMemory, Mp3Memory, Stream, Mp3Path);
    Exit;
  end;

  if Err <> BE_ERR_SUCCESSFUL then
  begin
    FSyncErrorMsg := 'Erro de Fechamento!' + IntToStr(Err);
    Synchronize(SyncShowError);
    CleanupStreams(WavFile, Mp3File, WavMemory, Mp3Memory, Stream, Mp3Path);
    Exit;
  end;

  if Mp3Memory <> nil then
    BytesWritten := Mp3File.Write(Mp3Memory.Memory^, FinalSize)
  else
  begin
    CleanupStreams(WavFile, Mp3File, WavMemory, Mp3Memory, Stream, Mp3Path);
    Exit;
  end;

  if FinalSize <> BytesWritten then
  begin
    FSyncErrorMsg := 'Falha ao Salvar!';
    Synchronize(SyncShowError);
    CleanupStreams(WavFile, Mp3File, WavMemory, Mp3Memory, Stream, Mp3Path);
    Exit;
  end;

  CleanupStreams(WavFile, Mp3File, WavMemory, Mp3Memory, Stream, Mp3Path);
  Synchronize(SyncResetProgress);
end;

{ Sync helpers — executed on main thread via Synchronize }

procedure TThreadConverter.SyncUpdateLabel;
begin
  FFileLabel.Caption := FSyncLabelText;
end;

procedure TThreadConverter.SyncUpdateProgress;
begin
  FProgressBar.Position := FSyncProgressPos;
end;

procedure TThreadConverter.SyncSetProgressMax;
begin
  FProgressBar.Max := FSyncProgressMax;
  FProgressBar.Position := 0;
end;

procedure TThreadConverter.SyncResetProgress;
begin
  FProgressBar.Position := 0;
end;

procedure TThreadConverter.SyncShowError;
begin
  Application.MessageBox(PChar(FSyncErrorMsg),
    PChar('Aten' + #231 + #227 + 'o!'), MB_OK + MB_ICONERROR);
end;

procedure TThreadConverter.SyncComplete;
begin
  if Assigned(FOnComplete) then
    FOnComplete(Self);
end;

end.
