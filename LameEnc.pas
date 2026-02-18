unit LameEnc;

interface

uses
  Windows, SysUtils, UEncoderTypes;

type
  TLameEncoder = class(TInterfacedObject, IEncoder)
  private
    FHandle: THandle;
    FLoaded: Boolean;
    FEncoderConfig: TEncoderConfig;
    FNumChannels: Integer;

    { Native LAME function pointers - Core lifecycle }
    FLameInit: function: Pointer; cdecl;
    FLameClose: function(gfp: Pointer): Integer; cdecl;
    FLameInitParams: function(gfp: Pointer): Integer; cdecl;

    { Native LAME function pointers - Encoding }
    FLameEncodeBufferInterleaved: function(gfp: Pointer; pcm: PSmallInt;
      numSamples: Integer; mp3buf: PByte; mp3bufSize: Integer): Integer; cdecl;
    FLameEncodeBuffer: function(gfp: Pointer; bufferL, bufferR: PSmallInt;
      nsamples: Integer; mp3buf: PByte; mp3bufSize: Integer): Integer; cdecl;
    FLameEncodeFlush: function(gfp: Pointer; mp3buf: PByte;
      size: Integer): Integer; cdecl;

    { Native LAME function pointers - Configuration (may be nil) }
    FLameSetInSamplerate: function(gfp: Pointer; rate: Integer): Integer; cdecl;
    FLameSetNumChannels: function(gfp: Pointer; ch: Integer): Integer; cdecl;
    FLameSetOutSamplerate: function(gfp: Pointer; rate: Integer): Integer; cdecl;
    FLameSetBrate: function(gfp: Pointer; brate: Integer): Integer; cdecl;
    FLameSetMode: function(gfp: Pointer; mode: Integer): Integer; cdecl;
    FLameSetQuality: function(gfp: Pointer; q: Integer): Integer; cdecl;
    FLameSetVBR: function(gfp: Pointer; vbrMode: Integer): Integer; cdecl;
    FLameSetVBRq: function(gfp: Pointer; q: Integer): Integer; cdecl;
    FLameSetBWriteVbrTag: function(gfp: Pointer; val: Integer): Integer; cdecl;
    FLameSetCopyright: function(gfp: Pointer; val: Integer): Integer; cdecl;
    FLameSetOriginal: function(gfp: Pointer; val: Integer): Integer; cdecl;
    FLameSetErrorProtection: function(gfp: Pointer; val: Integer): Integer; cdecl;
    FLameSetExtension: function(gfp: Pointer; val: Integer): Integer; cdecl;

    { Native LAME function pointers - Version }
    FGetLameVersion: function: PAnsiChar; cdecl;

    { Blade fallback for config when native setters unavailable }
    FBeInitStream: function(var pbeConfig: TBE_CONFIG; var dwSamples: DWORD;
      var dwBufferSize: DWORD; var phbeStream: THBE_STREAM): BE_ERR; cdecl;

    FHasNativeConfig: Boolean;

    function BoolToInt(B: LongBool): Integer;
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
  FHasNativeConfig := False;
  FNumChannels := 2;
end;

destructor TLameEncoder.Destroy;
begin
  UnloadDLL;
  inherited Destroy;
end;

function TLameEncoder.BoolToInt(B: LongBool): Integer;
begin
  if B then Result := 1 else Result := 0;
end;

procedure TLameEncoder.LoadDLL(const DLLPath: string);
begin
  UnloadDLL;
  FHandle := LoadLibrary(PChar(DLLPath));
  if FHandle > HINSTANCE_ERROR then
  begin
    { Core lifecycle - required }
    @FLameInit       := GetProcAddress(FHandle, 'lame_init');
    @FLameClose      := GetProcAddress(FHandle, 'lame_close');
    @FLameInitParams := GetProcAddress(FHandle, 'lame_init_params');

    { Encoding - required }
    @FLameEncodeBufferInterleaved := GetProcAddress(FHandle, 'lame_encode_buffer_interleaved');
    @FLameEncodeBuffer := GetProcAddress(FHandle, 'lame_encode_buffer');
    @FLameEncodeFlush  := GetProcAddress(FHandle, 'lame_encode_flush');

    { Configuration - optional (may not be exported by lame_enc.dll) }
    @FLameSetInSamplerate   := GetProcAddress(FHandle, 'lame_set_in_samplerate');
    @FLameSetNumChannels    := GetProcAddress(FHandle, 'lame_set_num_channels');
    @FLameSetOutSamplerate  := GetProcAddress(FHandle, 'lame_set_out_samplerate');
    @FLameSetBrate          := GetProcAddress(FHandle, 'lame_set_brate');
    @FLameSetMode           := GetProcAddress(FHandle, 'lame_set_mode');
    @FLameSetQuality        := GetProcAddress(FHandle, 'lame_set_quality');
    @FLameSetVBR            := GetProcAddress(FHandle, 'lame_set_VBR');
    @FLameSetVBRq           := GetProcAddress(FHandle, 'lame_set_VBR_q');
    @FLameSetBWriteVbrTag   := GetProcAddress(FHandle, 'lame_set_bWriteVbrTag');
    @FLameSetCopyright      := GetProcAddress(FHandle, 'lame_set_copyright');
    @FLameSetOriginal       := GetProcAddress(FHandle, 'lame_set_original');
    @FLameSetErrorProtection := GetProcAddress(FHandle, 'lame_set_error_protection');
    @FLameSetExtension      := GetProcAddress(FHandle, 'lame_set_extension');

    { Version }
    @FGetLameVersion := GetProcAddress(FHandle, 'get_lame_version');

    { Blade fallback }
    @FBeInitStream := GetProcAddress(FHandle, 'beInitStream');

    { Check if all essential native config setters are available }
    FHasNativeConfig := Assigned(@FLameSetInSamplerate) and
                        Assigned(@FLameSetNumChannels) and
                        Assigned(@FLameSetBrate) and
                        Assigned(@FLameSetMode) and
                        Assigned(@FLameSetQuality);

    { Minimum requirement: core lifecycle + encoding functions }
    FLoaded := Assigned(@FLameInit) and Assigned(@FLameClose) and
               Assigned(@FLameInitParams) and
               Assigned(@FLameEncodeBufferInterleaved) and
               Assigned(@FLameEncodeFlush) and
               (FHasNativeConfig or Assigned(@FBeInitStream));
  end;
end;

procedure TLameEncoder.UnloadDLL;
begin
  if FHandle > HINSTANCE_ERROR then
  begin
    FreeLibrary(FHandle);
    FHandle := 0;
  end;
  @FLameInit := nil;
  @FLameClose := nil;
  @FLameInitParams := nil;
  @FLameEncodeBufferInterleaved := nil;
  @FLameEncodeBuffer := nil;
  @FLameEncodeFlush := nil;
  @FLameSetInSamplerate := nil;
  @FLameSetNumChannels := nil;
  @FLameSetOutSamplerate := nil;
  @FLameSetBrate := nil;
  @FLameSetMode := nil;
  @FLameSetQuality := nil;
  @FLameSetVBR := nil;
  @FLameSetVBRq := nil;
  @FLameSetBWriteVbrTag := nil;
  @FLameSetCopyright := nil;
  @FLameSetOriginal := nil;
  @FLameSetErrorProtection := nil;
  @FLameSetExtension := nil;
  @FGetLameVersion := nil;
  @FBeInitStream := nil;
  FLoaded := False;
  FHasNativeConfig := False;
end;

procedure TLameEncoder.Configure(const Config: TEncoderConfig);
begin
  FEncoderConfig := Config;
end;

function TLameEncoder.InitStream(var Samples: DWORD; var BufferSize: DWORD;
  var Stream: THBE_STREAM): BE_ERR;
var
  gfp: Pointer;
  LameMode, NumChannels: Integer;
  SamplesPerChannel: DWORD;
  IsMPEG1: Boolean;
  Config: TBE_CONFIG;
begin
  if FHasNativeConfig then
  begin
    { === Native path (e.g. libmp3lame.dll) === }
    gfp := FLameInit();
    if gfp = nil then
    begin
      Result := BE_ERR_INVALID_FORMAT;
      Exit;
    end;

    { Sample rate }
    FLameSetInSamplerate(gfp, FEncoderConfig.SampleRate);

    { Disable VBR tag by default }
    if Assigned(@FLameSetBWriteVbrTag) then
      FLameSetBWriteVbrTag(gfp, 0);

    { Mode and channels }
    case FEncoderConfig.Mode of
      BE_MP3_MODE_MONO:
      begin
        LameMode := 3; { MONO }
        NumChannels := 1;
      end;
      BE_MP3_MODE_DUALCHANNEL:
      begin
        LameMode := 2; { DUAL_CHANNEL }
        NumChannels := 2;
      end;
    else { BE_MP3_MODE_STEREO and default }
      begin
        LameMode := 1; { JOINT_STEREO }
        NumChannels := 2;
      end;
    end;

    FLameSetMode(gfp, LameMode);
    FLameSetNumChannels(gfp, NumChannels);
    FNumChannels := NumChannels;

    { VBR settings }
    if FEncoderConfig.EnableVBR then
    begin
      if Assigned(@FLameSetVBR) then
        FLameSetVBR(gfp, 2); { vbr_default = 2 in lame.h }
      if Assigned(@FLameSetVBRq) then
        FLameSetVBRq(gfp, FEncoderConfig.VBRQuality);
    end;

    { Bitrate }
    FLameSetBrate(gfp, FEncoderConfig.BitRate);

    { Output sample rate (resample) }
    if (FEncoderConfig.ReSampleRate > 0) and Assigned(@FLameSetOutSamplerate) then
      FLameSetOutSamplerate(gfp, FEncoderConfig.ReSampleRate);

    { Copyright, Original, CRC, Private flags }
    if Assigned(@FLameSetCopyright) then
      FLameSetCopyright(gfp, BoolToInt(FEncoderConfig.Copyright));
    if Assigned(@FLameSetOriginal) then
      FLameSetOriginal(gfp, BoolToInt(FEncoderConfig.Original));
    if Assigned(@FLameSetErrorProtection) then
      FLameSetErrorProtection(gfp, BoolToInt(FEncoderConfig.CRC));
    if Assigned(@FLameSetExtension) then
      FLameSetExtension(gfp, BoolToInt(FEncoderConfig.IsPrivate));

    { Quality }
    FLameSetQuality(gfp, FEncoderConfig.Quality);

    { Finalize configuration }
    if FLameInitParams(gfp) < 0 then
    begin
      FLameClose(gfp);
      Result := BE_ERR_INVALID_FORMAT_PARAMETERS;
      Exit;
    end;

    { Compute Samples and BufferSize (from BladeMP3EncDLL.c lines 530-545) }
    IsMPEG1 := (FEncoderConfig.MpegVersion = MPEG1);
    if IsMPEG1 then
      SamplesPerChannel := 1152
    else
      SamplesPerChannel := 576;

    Samples := SamplesPerChannel * DWORD(NumChannels);
    BufferSize := Trunc(1.25 * SamplesPerChannel + 7200);

    { The gfp pointer IS the stream handle }
    Stream := THBE_STREAM(gfp);
    Result := BE_ERR_SUCCESSFUL;
  end
  else
  begin
    { === Fallback path (e.g. lame_enc.dll) === }
    FillChar(Config, SizeOf(Config), #0);
    Config.dwConfig := BE_CONFIG_LAME;
    with Config.LHV1 do
    begin
      dwStructVersion := 1;
      dwStructSize    := SizeOf(TBE_CONFIG);
      dwSampleRate    := FEncoderConfig.SampleRate;
      dwReSampleRate  := FEncoderConfig.ReSampleRate;
      nMode           := FEncoderConfig.Mode;
      dwBitrate       := FEncoderConfig.BitRate;
      dwMaxBitrate    := FEncoderConfig.MaxBitRate;
      nQuality        := FEncoderConfig.Quality;
      dwMpegVersion   := FEncoderConfig.MpegVersion;
      dwPsyModel      := 0;
      dwEmphasis      := 0;
      bPrivate        := FEncoderConfig.IsPrivate;
      bCRC            := FEncoderConfig.CRC;
      bCopyright      := FEncoderConfig.Copyright;
      bOriginal       := FEncoderConfig.Original;
      bWriteVBRHeader := FEncoderConfig.WriteVBRHeader;
      bEnableVBR      := FEncoderConfig.EnableVBR;
      nVBRQuality     := FEncoderConfig.VBRQuality;
    end;

    Result := FBeInitStream(Config, Samples, BufferSize, Stream);

    { Determine channel count from mode for EncodeChunk }
    if FEncoderConfig.Mode = BE_MP3_MODE_MONO then
      FNumChannels := 1
    else
      FNumChannels := 2;
  end;
end;

function TLameEncoder.EncodeChunk(Stream: THBE_STREAM; NumSamples: DWORD;
  InBuffer: PSHORT; OutBuffer: PByte; var OutputSize: DWORD): BE_ERR;
var
  gfp: Pointer;
  SamplesPerChannel: Integer;
  OutputBytes: Integer;
begin
  gfp := Pointer(Stream);
  SamplesPerChannel := NumSamples div FNumChannels;

  if (FNumChannels = 1) and Assigned(@FLameEncodeBuffer) then
    OutputBytes := FLameEncodeBuffer(gfp, PSmallInt(InBuffer),
      PSmallInt(InBuffer), SamplesPerChannel, OutBuffer, 0)
  else
    OutputBytes := FLameEncodeBufferInterleaved(gfp, PSmallInt(InBuffer),
      SamplesPerChannel, OutBuffer, 0);

  if OutputBytes < 0 then
  begin
    OutputSize := 0;
    Result := BE_ERR_BUFFER_TOO_SMALL;
  end
  else
  begin
    OutputSize := OutputBytes;
    Result := BE_ERR_SUCCESSFUL;
  end;
end;

function TLameEncoder.DeinitStream(Stream: THBE_STREAM; OutBuffer: PByte;
  var OutputSize: DWORD): BE_ERR;
var
  gfp: Pointer;
  OutputBytes: Integer;
begin
  gfp := Pointer(Stream);
  OutputBytes := FLameEncodeFlush(gfp, OutBuffer, 0);

  if OutputBytes < 0 then
  begin
    OutputSize := 0;
    Result := BE_ERR_BUFFER_TOO_SMALL;
  end
  else
  begin
    OutputSize := OutputBytes;
    Result := BE_ERR_SUCCESSFUL;
  end;
end;

function TLameEncoder.CloseStream(Stream: THBE_STREAM): BE_ERR;
var
  gfp: Pointer;
begin
  gfp := Pointer(Stream);
  FLameClose(gfp);
  Result := BE_ERR_SUCCESSFUL;
end;

function TLameEncoder.GetVersion: string;
var
  VersionStr: PAnsiChar;
begin
  if Assigned(@FGetLameVersion) then
  begin
    VersionStr := FGetLameVersion();
    if VersionStr <> nil then
      Result := 'Lame MP3 Encoder ( Vers' + #227 + 'o ' + string(AnsiString(VersionStr)) + ' )'
    else
      Result := 'Lame MP3 Encoder';
  end
  else
    Result := 'Lame MP3 Encoder';
end;

function TLameEncoder.IsLoaded: Boolean;
begin
  Result := FLoaded;
end;

end.
