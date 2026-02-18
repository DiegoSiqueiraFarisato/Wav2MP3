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
    FWavList: TStringList;
    FOutputDir: string;
    FProgressBar: TProgressBar;
    FFileLabel: TLabel;
    FCancelled: PBoolean;

    FWavFile, FMp3File: TFileStream;
    FMp3Memory, FWavMemory: TMemoryStream;
    FWavPath, FMp3Path: string;
    FStream: THBE_STREAM;

    procedure PrepareStreams;
    procedure CleanupStreams;
    procedure ConvertFile;
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

{ TThreadConverter }

constructor TThreadConverter.Create;
var
  AppPath: string;
  Encoder: IEncoder;
  Rec: TSearchRec;
  DLLName: string;
begin
  inherited Create(False);
  FEncoders := TInterfaceList.Create;
  FEncoderIndex := 0;

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
        else if DLLName = 'LAME_ENC.DLL' then
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
end;

destructor TThreadConverter.Destroy;
begin
  FEncoders.Free;
  inherited Destroy;
end;

function TThreadConverter.GetEncoderVersions: TStrings;
var
  i: Integer;
  List: TStringList;
begin
  List := TStringList.Create;
  for i := 0 to FEncoders.Count - 1 do
    List.Add((FEncoders[i] as IEncoder).GetVersion);
  Result := List;
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
  FWavList := WavList;
  FOutputDir := OutputDir;
  FEncoderConfig := Config;
  FFileLabel := FileLabel;
  FProgressBar := ProgressBar;
  Self.Priority := Priority;
  FCancelled := CancelFlag;
  Execute;
  FreeOnTerminate := True;
end;

procedure TThreadConverter.PrepareStreams;
begin
  if (FWavFile = nil) and (FMp3File = nil) then
  begin
    try
      FMp3File := TFileStream.Create(FMp3Path, fmCreate or fmShareDenyNone);
      FMp3Memory := TMemoryStream.Create;
      FWavFile := TFileStream.Create(FWavPath, fmOpenRead);
      FWavMemory := TMemoryStream.Create;
    except
      on E: Exception do
      begin
        CleanupStreams;
      end;
    end;
  end;
end;

procedure TThreadConverter.CleanupStreams;
var
  Encoder: IEncoder;
begin
  if (FStream <> 0) and (FEncoderIndex >= 0) and (FEncoderIndex < FEncoders.Count) then
  begin
    Encoder := FEncoders[FEncoderIndex] as IEncoder;
    Encoder.CloseStream(FStream);
    FStream := 0;
  end;

  FreeAndNil(FWavFile);
  FreeAndNil(FMp3File);
  FreeAndNil(FMp3Memory);
  FreeAndNil(FWavMemory);

  if FCancelled^ then
    DeleteFile(FMp3Path);
end;

procedure TThreadConverter.ConvertFile;
var
  Encoder: IEncoder;
  NumSamples, MP3BufferSize: DWORD;
  FileSize, Completed, TotalChunk, ReadSize, BytesRead, BytesWritten,
    WriteSize, FinalSize: DWORD;
  Offset: DWORD;
  Err: BE_ERR;
begin
  if (FEncoderIndex < 0) or (FEncoderIndex >= FEncoders.Count) then
    Exit;

  Encoder := FEncoders[FEncoderIndex] as IEncoder;

  FProgressBar.Position := 0;
  FProgressBar.Min := 0;

  if FCancelled^ then
    Exit;

  PrepareStreams;

  if (FWavFile = nil) or (FMp3File = nil) then
    Exit;

  Encoder.Configure(FEncoderConfig);

  FStream := 0;
  Err := Encoder.InitStream(NumSamples, MP3BufferSize, FStream);
  if Err <> BE_ERR_SUCCESSFUL then
  begin
    Application.MessageBox(PChar('Erro ao Abrir : ' + IntToStr(Err)),
      'Aten' + #231 + #227 + 'o!', MB_OK + MB_ICONERROR);
    CleanupStreams;
    Exit;
  end;

  if (FMp3Memory = nil) or FCancelled^ then
  begin
    CleanupStreams;
    Exit;
  end;
  FMp3Memory.SetSize(MP3BufferSize);

  if (FWavMemory = nil) or FCancelled^ then
  begin
    CleanupStreams;
    Exit;
  end;
  FWavMemory.SetSize(NumSamples);

  FileSize := FWavFile.Size;
  Completed := 0;
  FProgressBar.Max := FileSize;
  TotalChunk := NumSamples * 2;

  while (Completed <> FileSize) and (not FCancelled^) do
  begin
    Offset := Completed + TotalChunk;
    if Offset < FileSize then
      ReadSize := TotalChunk
    else
      ReadSize := FileSize - Completed;

    if FWavFile <> nil then
      BytesRead := FWavFile.Read(FWavMemory.Memory^, ReadSize)
    else
    begin
      FCancelled^ := True;
      Break;
    end;

    if BytesRead <> ReadSize then
    begin
      Application.MessageBox(PChar('Erro de Leitura!'),
        'Aten' + #231 + #227 + 'o!', MB_OK + MB_ICONERROR);
      FCancelled^ := True;
      CleanupStreams;
      Break;
    end;

    Offset := ReadSize div 2;
    if (FMp3Memory <> nil) and (FWavMemory <> nil) then
      Err := Encoder.EncodeChunk(FStream, Offset, FWavMemory.Memory,
        FMp3Memory.Memory, WriteSize)
    else
    begin
      FCancelled^ := True;
      Break;
    end;

    if Err <> BE_ERR_SUCCESSFUL then
    begin
      Application.MessageBox(PChar('Falha na Convers' + #227 + 'o!' + IntToStr(Err)),
        'Aten' + #231 + #227 + 'o!', MB_OK + MB_ICONERROR);
      FCancelled^ := True;
      CleanupStreams;
      Break;
    end;

    if FMp3Memory <> nil then
      BytesWritten := FMp3File.Write(FMp3Memory.Memory^, WriteSize)
    else
    begin
      FCancelled^ := True;
      Break;
    end;

    if WriteSize <> BytesWritten then
    begin
      Application.MessageBox(PChar('Erro de Grava' + #231 + #227 + 'o!'),
        'Aten' + #231 + #227 + 'o!', MB_OK + MB_ICONERROR);
      FCancelled^ := True;
      CleanupStreams;
      Break;
    end;

    Completed := Completed + ReadSize;
    FProgressBar.Position := FProgressBar.Position + Int64(ReadSize);
    Application.ProcessMessages;
  end;

  if FCancelled^ then
  begin
    CleanupStreams;
    FProgressBar.Position := 0;
    Exit;
  end;

  if FMp3Memory <> nil then
    Err := Encoder.DeinitStream(FStream, FMp3Memory.Memory, FinalSize)
  else
  begin
    CleanupStreams;
    Exit;
  end;

  if Err <> BE_ERR_SUCCESSFUL then
  begin
    Application.MessageBox(PChar('Erro de Fechamento!' + IntToStr(Err)),
      'Aten' + #231 + #227 + 'o!', MB_OK + MB_ICONERROR);
    CleanupStreams;
    Exit;
  end;

  if FMp3Memory <> nil then
    BytesWritten := FMp3File.Write(FMp3Memory.Memory^, FinalSize)
  else
  begin
    CleanupStreams;
    Exit;
  end;

  if FinalSize <> BytesWritten then
  begin
    Application.MessageBox(PChar('Falha ao Salvar!'),
      'Aten' + #231 + #227 + 'o!', MB_OK + MB_ICONERROR);
    CleanupStreams;
    Exit;
  end;

  CleanupStreams;
  FProgressBar.Position := 0;
end;

procedure TThreadConverter.Execute;
var
  i: Integer;
  FileName: string;
begin
  if FWavList = nil then
    Exit;

  for i := 0 to FWavList.Count - 1 do
  begin
    if FCancelled^ then
      Break;

    FWavPath := FWavList.Strings[i];
    FMp3Path := FOutputDir + ExtractFileNameWithoutExt(FWavPath) + '.mp3';
    FileName := ExtractFileName(FWavPath);

    FFileLabel.Caption := 'Convertendo: '#13 + FileName;
    Application.ProcessMessages;

    if (not IsFileInUse(FMp3Path)) and (not IsFileInUse(FWavPath)) then
      Synchronize(ConvertFile)
    else
      CleanupStreams;
  end;

  FFileLabel.Caption := 'Selecione os Arquivos';
end;

end.
