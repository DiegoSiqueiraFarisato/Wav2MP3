unit UConfigura;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, ShlObj,
  UEncoderTypes;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    PageControl1: TPageControl;
    TabSheet2: TTabSheet;
    Button1: TButton;
    Panel2: TPanel;
    Label1: TLabel;
    ComboBox1: TComboBox;
    GroupBox1: TGroupBox;
    Lkbp: TLabel;
    kbps: TTrackBar;
    GroupBox2: TGroupBox;
    LPrioridade: TLabel;
    Prioridade: TTrackBar;
    Panel3: TPanel;
    ComboBox2: TComboBox;
    Label2: TLabel;
    RadioGroup1: TRadioGroup;
    GroupBox3: TGroupBox;
    cbPrivado: TCheckBox;
    cbCRC: TCheckBox;
    cbCopy: TCheckBox;
    cbOriginal: TCheckBox;
    TabSheet1: TTabSheet;
    Panel4: TPanel;
    GroupBox4: TGroupBox;
    StaticText1: TStaticText;
    Edit1: TEdit;
    Button2: TButton;
    procedure kbpsChange(Sender: TObject);
    procedure PrioridadeChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FBitRate: Word;
    FThreadPriority: TThreadPriority;
    FOnSaveConfig: TNotifyEvent;
    FOnEncoderChanged: TNotifyEvent;
  public
    property BitRate: Word read FBitRate;
    property ThreadPriority: TThreadPriority read FThreadPriority;
    property OnSaveConfig: TNotifyEvent read FOnSaveConfig write FOnSaveConfig;
    property OnEncoderChanged: TNotifyEvent read FOnEncoderChanged write FOnEncoderChanged;

    function GetEncoderConfig: TEncoderConfig;
    procedure SetEncoderConfig(const AConfig: TEncoderConfig);
    function GetOutputDir: string;
    function GetEncoderIndex: Integer;
    function GetSampleRate: DWORD;
    function GetMode: Byte;
  end;

var
  Form2: TForm2;

function BrowseForFolder(handle: HWND; strTitle: string;
  var strPath: string): Boolean;

implementation

{$R *.DFM}

function BrowseForFolder(handle: HWND; strTitle: string;
  var strPath: string): Boolean;
var
  info: TBROWSEINFO;
  path: array [0 .. MAX_PATH] of Char;
  items: PITEMIDLIST;
begin
  Result := False;
  path := '';

  with info do
  begin
    hwndOwner := handle;
    pidlRoot := nil;
    pszDisplayName := nil;
    lpszTitle := PChar(strTitle);
    ulFlags := BIF_RETURNONLYFSDIRS;
    lpfn := nil;
  end;

  items := SHBrowseForFolder(info);

  if Assigned(items) then
  begin
    SHGetPathFromIDList(items, path);
    Result := True;
  end;
  strPath := path;
end;

procedure TForm2.kbpsChange(Sender: TObject);
begin
  case kbps.Position of
    00: FBitRate := 32;
    01: FBitRate := 40;
    02: FBitRate := 48;
    03: FBitRate := 56;
    04: FBitRate := 64;
    05: FBitRate := 80;
    06: FBitRate := 96;
    07: FBitRate := 112;
    08: FBitRate := 128;
    09: FBitRate := 160;
    10: FBitRate := 192;
    11: FBitRate := 224;
    12: FBitRate := 256;
    13: FBitRate := 320;
  end;
  Lkbp.Caption := IntToStr(FBitRate) + ' kbps';
end;

procedure TForm2.PrioridadeChange(Sender: TObject);
begin
  case Prioridade.Position of
    0: begin
         LPrioridade.Caption := 'Baixa';
         FThreadPriority := tpLowest;
       end;
    1: begin
         LPrioridade.Caption := 'Abaixo do Normal';
         FThreadPriority := tpLower;
       end;
    2: begin
         LPrioridade.Caption := 'Normal';
         FThreadPriority := tpNormal;
       end;
    3: begin
         LPrioridade.Caption := 'Acima do Normal';
         FThreadPriority := tpHigher;
       end;
    4: begin
         LPrioridade.Caption := 'M' + #225 + 'xima';
         FThreadPriority := tpHighest;
       end;
  end;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  if Assigned(FOnSaveConfig) then
    FOnSaveConfig(Self);
  Hide;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  Self.AutoSize := True;
  Self.Position := poScreenCenter;
  FBitRate := 128;
  FThreadPriority := tpLowest;
end;

procedure TForm2.ComboBox1Change(Sender: TObject);
begin
  if Assigned(FOnEncoderChanged) then
    FOnEncoderChanged(Self);
end;

procedure TForm2.Button2Click(Sender: TObject);
var
  NewFolder: string;
begin
  NewFolder := '';
  if BrowseForFolder(Handle, 'Selecione uma pasta', NewFolder) then
    Edit1.Text := NewFolder;
end;

function TForm2.GetEncoderConfig: TEncoderConfig;
begin
  Result.SampleRate := GetSampleRate;
  Result.Mode := GetMode;
  Result.BitRate := FBitRate;
  Result.IsPrivate := cbPrivado.Checked;
  Result.CRC := cbCRC.Checked;
  Result.Copyright := cbCopy.Checked;
  Result.Original := cbOriginal.Checked;
end;

procedure TForm2.SetEncoderConfig(const AConfig: TEncoderConfig);
begin
  cbPrivado.Checked := AConfig.IsPrivate;
  cbCRC.Checked := AConfig.CRC;
  cbCopy.Checked := AConfig.Copyright;
  cbOriginal.Checked := AConfig.Original;
end;

function TForm2.GetOutputDir: string;
begin
  Result := Edit1.Text;
end;

function TForm2.GetEncoderIndex: Integer;
begin
  Result := ComboBox1.ItemIndex;
end;

function TForm2.GetSampleRate: DWORD;
begin
  if ComboBox2.ItemIndex >= 0 then
    Result := StrToIntDef(ComboBox2.Items[ComboBox2.ItemIndex], 44100)
  else
    Result := 44100;
end;

function TForm2.GetMode: Byte;
begin
  case RadioGroup1.ItemIndex of
    0: Result := BE_MP3_MODE_STEREO;
    1: Result := BE_MP3_MODE_MONO;
  else
    Result := BE_MP3_MODE_STEREO;
  end;
end;

end.
