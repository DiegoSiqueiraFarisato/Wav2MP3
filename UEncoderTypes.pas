unit UEncoderTypes;

interface

uses
  Windows, SysUtils, Classes, Forms;

const
  BE_CONFIG_MP3                    = 0;
  BE_ERR_SUCCESSFUL                = $00000000;
  BE_ERR_INVALID_FORMAT            = $00000001;
  BE_ERR_INVALID_FORMAT_PARAMETERS = $00000002;
  BE_ERR_NO_MORE_HANDLES           = $00000003;
  BE_ERR_INVALID_HANDLE            = $00000004;
  BE_ERR_BUFFER_TOO_SMALL          = $00000005;
  BE_MAX_HOMEPAGE                  = 256;
  BE_MP3_MODE_STEREO               = 0;
  BE_MP3_MODE_DUALCHANNEL          = 2;
  BE_MP3_MODE_MONO                 = 3;

  { LAME LHV1 MPEG versions }
  MPEG1                            = 1;
  MPEG2                            = 0;

  { LAME LHV1 quality presets }
  QUALITY_NORMAL                   = 0;
  QUALITY_LOW                      = 1;
  QUALITY_HIGH                     = 2;
  QUALITY_VOICE                    = 3;

  { BE_CONFIG format selector for LHV1 }
  BE_CONFIG_LAME                   = 256;

type
  SHORT = ShortInt;
  PSHORT = ^SHORT;
  THBE_STREAM = DWORD;
  PHBE_STREAM = ^THBE_STREAM;
  BE_ERR = DWORD;

  TBE_CONFIG = packed record
    dwConfig: DWORD;
    case Integer of
      0: (MP3: packed record
             dwSampleRate: DWORD;
             byMode: Byte;
             wBitrate: Word;
             bPrivate: Bool;
             bCRC: Bool;
             bCopyright: Bool;
             bOriginal: Bool;
           end);
      1: (LHV1: packed record
             dwStructVersion: DWORD;
             dwStructSize: DWORD;
             dwSampleRate: DWORD;
             dwReSampleRate: DWORD;
             nMode: Integer;
             dwBitrate: DWORD;
             dwMaxBitrate: DWORD;
             nQuality: Integer;
             dwMpegVersion: DWORD;
             dwPsyModel: DWORD;
             dwEmphasis: DWORD;
             bPrivate: LONGBOOL;
             bCRC: LONGBOOL;
             bCopyright: LONGBOOL;
             bOriginal: LONGBOOL;
             bWriteVBRHeader: LONGBOOL;
             bEnableVBR: LONGBOOL;
             nVBRQuality: Integer;
             btReserved: array[0..255] of Byte;
           end);
      2: (AAC: packed record
             dwSampleRate: DWORD;
             byMode: Byte;
             wBitrate: Word;
             byEncodingMethod: Byte;
           end);
  end;

  TBE_VERSION = packed record
    byDLLMajorVersion: Byte;
    byDLLMinorVersion: Byte;
    byMajorVersion: Byte;
    byMinorVersion: Byte;
    byDay: Byte;
    byMonth: Byte;
    wYear: Byte;
    zHomepage: array[0..BE_MAX_HOMEPAGE] of Char;
  end;

  PBE_CONFIG = ^TBE_CONFIG;
  PBE_VERSION = ^TBE_VERSION;

  TEncoderConfig = record
    SampleRate: DWORD;
    Mode: Byte;
    BitRate: Word;
    IsPrivate: LongBool;
    CRC: LongBool;
    Copyright: LongBool;
    Original: LongBool;
    { LHV1-specific fields (used by LAME) }
    ReSampleRate: DWORD;
    MaxBitRate: DWORD;
    Quality: Integer;
    MpegVersion: DWORD;
    EnableVBR: LongBool;
    VBRQuality: Integer;
    WriteVBRHeader: LongBool;
  end;

  IEncoder = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    procedure LoadDLL(const DLLPath: string);
    procedure UnloadDLL;
    procedure Configure(const Config: TEncoderConfig);
    function InitStream(var Samples: DWORD; var BufferSize: DWORD; var Stream: THBE_STREAM): BE_ERR;
    function EncodeChunk(Stream: THBE_STREAM; NumSamples: DWORD; InBuffer: PSHORT; OutBuffer: PByte; var OutputSize: DWORD): BE_ERR;
    function DeinitStream(Stream: THBE_STREAM; OutBuffer: PByte; var OutputSize: DWORD): BE_ERR;
    function CloseStream(Stream: THBE_STREAM): BE_ERR;
    function GetVersion: string;
    function IsLoaded: Boolean;
  end;

function iif(Condition: Bool; R1, R2: Variant): Variant;

implementation

function iif(Condition: Bool; R1, R2: Variant): Variant;
begin
  if Condition then
    Result := R1
  else
    Result := R2;
end;

end.
