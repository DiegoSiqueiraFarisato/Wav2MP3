unit LameEnc;

interface

uses
  Windows, SysUtils, UEncoderTypes;

type
  TLameEncoder = class(TInterfacedObject, IEncoder)
  private
    FHandle: THandle;
    FConfig: TBE_CONFIG;
    FVersion: TBE_VERSION;
    FLoaded: Boolean;

    FInitStream: function(var pbeConfig: TBE_CONFIG; var dwSamples: DWORD;
      var dwBufferSize: DWORD; var phbeStream: THBE_STREAM): BE_ERR; cdecl;
    FEncodeChunk: function(hbeStream: THBE_STREAM; nSamples: DWORD;
      pSamples: PSHORT; pOutput: PByte; var pdwOutput: DWORD): BE_ERR; cdecl;
    FDeinitStream: function(hbeStream: THBE_STREAM; pOutput: PByte;
      var pdwOutput: DWORD): BE_ERR; cdecl;
    FCloseStream: function(hbeStream: THBE_STREAM): BE_ERR; cdecl;
    FGetVersion: procedure(var pbeVersion: TBE_VERSION); cdecl;
  public
    constructor Create;
    destructor Destroy; override;

    { IEncoder }
    procedure LoadDLL(const DLLPath: string);
    procedure UnloadDLL;
    procedure Configure(const Config: TEncoderConfig);
    function InitStream(var Samples: DWORD; var BufferSize: DWORD;
      var Stream: THBE_STREAM): BE_ERR;
    function EncodeChunk(Stream: THBE_STREAM; NumSamples: DWORD;
      InBuffer: PSHORT; OutBuffer: PByte; var OutputSize: DWORD): BE_ERR;
    function DeinitStream(Stream: THBE_STREAM; OutBuffer: PByte;
      var OutputSize: DWORD): BE_ERR;
    function CloseStream(Stream: THBE_STREAM): BE_ERR;
    function GetVersion: string;
    function IsLoaded: Boolean;
  end;

implementation

constructor TLameEncoder.Create;
begin
  inherited Create;
  FHandle := 0;
  FLoaded := False;
end;

destructor TLameEncoder.Destroy;
begin
  UnloadDLL;
  inherited Destroy;
end;

procedure TLameEncoder.LoadDLL(const DLLPath: string);
begin
  UnloadDLL;
  FHandle := LoadLibrary(PChar(DLLPath));
  if FHandle > HINSTANCE_ERROR then
  begin
    @FInitStream   := GetProcAddress(FHandle, 'beInitStream');
    @FEncodeChunk  := GetProcAddress(FHandle, 'beEncodeChunk');
    @FDeinitStream := GetProcAddress(FHandle, 'beDeinitStream');
    @FCloseStream  := GetProcAddress(FHandle, 'beCloseStream');
    @FGetVersion   := GetProcAddress(FHandle, 'beVersion');
    FLoaded := Assigned(@FInitStream) and Assigned(@FEncodeChunk) and
               Assigned(@FDeinitStream) and Assigned(@FCloseStream) and
               Assigned(@FGetVersion);
  end;
end;

procedure TLameEncoder.UnloadDLL;
begin
  if FHandle > HINSTANCE_ERROR then
  begin
    FreeLibrary(FHandle);
    FHandle := 0;
  end;
  @FInitStream   := nil;
  @FEncodeChunk  := nil;
  @FDeinitStream := nil;
  @FCloseStream  := nil;
  @FGetVersion   := nil;
  FLoaded := False;
end;

procedure TLameEncoder.Configure(const Config: TEncoderConfig);
begin
  FillChar(FConfig, SizeOf(FConfig), #0);
  FConfig.dwConfig := BE_CONFIG_MP3;
  with FConfig.MP3 do
  begin
    dwSampleRate := Config.SampleRate;
    byMode       := Config.Mode;
    wBitrate     := Config.BitRate;
    bPrivate     := Config.IsPrivate;
    bCRC         := Config.CRC;
    bCopyright   := Config.Copyright;
    bOriginal    := Config.Original;
  end;
end;

function TLameEncoder.InitStream(var Samples: DWORD; var BufferSize: DWORD;
  var Stream: THBE_STREAM): BE_ERR;
begin
  Result := FInitStream(FConfig, Samples, BufferSize, Stream);
end;

function TLameEncoder.EncodeChunk(Stream: THBE_STREAM; NumSamples: DWORD;
  InBuffer: PSHORT; OutBuffer: PByte; var OutputSize: DWORD): BE_ERR;
begin
  Result := FEncodeChunk(Stream, NumSamples, InBuffer, OutBuffer, OutputSize);
end;

function TLameEncoder.DeinitStream(Stream: THBE_STREAM; OutBuffer: PByte;
  var OutputSize: DWORD): BE_ERR;
begin
  Result := FDeinitStream(Stream, OutBuffer, OutputSize);
end;

function TLameEncoder.CloseStream(Stream: THBE_STREAM): BE_ERR;
begin
  Result := FCloseStream(Stream);
end;

function TLameEncoder.GetVersion: string;
begin
  FGetVersion(FVersion);
  Result := 'Lame MP3 Encoder '
    + '( Vers' + #227 + 'o ' + IntToStr(FVersion.byDLLMajorVersion) + '.' + FormatFloat('00', FVersion.byDLLMinorVersion)
    + ', Engine ' + IntToStr(FVersion.byMajorVersion) + '.' + FormatFloat('00', FVersion.byMinorVersion) + ' )';
end;

function TLameEncoder.IsLoaded: Boolean;
begin
  Result := FLoaded;
end;

end.
