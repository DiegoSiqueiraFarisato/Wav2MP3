unit Uprincipal;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, FileCtrl, ComCtrls, MMSystem, Menus, ExtCtrls, CheckLst,
  UEncoderTypes, UThreadConverter, jpeg;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button3: TButton;
    DriveComboBox1: TDriveComboBox;
    GroupBox3: TGroupBox;
    LBDirMp3: TLabel;
    DirectoryListBox2: TDirectoryListBox;
    GroupBox4: TGroupBox;
    LBDirWav: TLabel;
    DirectoryListBox1: TDirectoryListBox;
    GroupBox5: TGroupBox;
    Button4: TButton;
    FileListBox1: TFileListBox;
    Button5: TButton;
    Label1: TLabel;
    Label2: TLabel;
    DriveComboBox2: TDriveComboBox;
    Image1: TImage;
    CheckBox1: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    FConfig: TStringList;
    FWavList: TStringList;
    FThreadConverter: TThreadConverter;
    FCancelled: Boolean;

    procedure LoadConfig;
    procedure SaveConfig(Sender: TObject);
    procedure OnEncoderChanged(Sender: TObject);
    procedure OnConversionComplete(Sender: TObject);
  public
    property Cancelled: Boolean read FCancelled write FCancelled;
  end;

var
  Form1: TForm1;

implementation

uses UConfigura, UTelaBP;

{$R *.DFM}

procedure TForm1.LoadConfig;
var
  ConfigPath: string;
  DefaultDir: string;
begin
  ConfigPath := ExtractFilePath(Application.ExeName) + 'Configura.txt';
  if FileExists(ConfigPath) then
  begin
    FConfig.LoadFromFile(ConfigPath);
    Form2.Edit1.Text := FConfig.Strings[0];
    Form2.ComboBox1.ItemIndex := StrToInt(FConfig.Strings[1]);
    Form2.cbPrivado.Checked := iif(FConfig.Strings[2] = 'Sim', True, False);
    Form2.cbCRC.Checked := iif(FConfig.Strings[3] = 'Sim', True, False);
    Form2.cbCopy.Checked := iif(FConfig.Strings[4] = 'Sim', True, False);
    Form2.cbOriginal.Checked := iif(FConfig.Strings[5] = 'Sim', True, False);
    Form2.ComboBox2.ItemIndex := StrToInt(FConfig.Strings[6]);
    Form2.RadioGroup1.ItemIndex := StrToInt(FConfig.Strings[7]);
    Form2.kbps.Position := StrToInt(FConfig.Strings[8]);
    Form2.Prioridade.Position := StrToInt(FConfig.Strings[9]);
    if not DirectoryExists(FConfig.Strings[0]) then
      MkDir(FConfig.Strings[0]);
    DirectoryListBox2.Directory := FConfig.Strings[0];
  end
  else
  begin
    DefaultDir := ExtractFileDrive(Application.ExeName) + '\MP3';
    FConfig.Add(DefaultDir);
    FConfig.Add('0');
    FConfig.Add(iif(False, 'Sim', 'N' + #227 + 'o'));
    FConfig.Add(iif(False, 'Sim', 'N' + #227 + 'o'));
    FConfig.Add(iif(False, 'Sim', 'N' + #227 + 'o'));
    FConfig.Add(iif(False, 'Sim', 'N' + #227 + 'o'));
    FConfig.Add(IntToStr(1));
    FConfig.Add(IntToStr(0));
    FConfig.Add(IntToStr(8));
    FConfig.Add(IntToStr(0));
    FConfig.SaveToFile(ConfigPath);
    FConfig.LoadFromFile(ConfigPath);
    Form2.Edit1.Text := FConfig.Strings[0];
    Form2.cbPrivado.Checked := iif(FConfig.Strings[2] = 'Sim', True, False);
    Form2.cbCRC.Checked := iif(FConfig.Strings[3] = 'Sim', True, False);
    Form2.cbCopy.Checked := iif(FConfig.Strings[4] = 'Sim', True, False);
    Form2.cbOriginal.Checked := iif(FConfig.Strings[5] = 'Sim', True, False);
    Form2.ComboBox2.ItemIndex := StrToInt(FConfig.Strings[6]);
    Form2.RadioGroup1.ItemIndex := StrToInt(FConfig.Strings[7]);
    Form2.kbps.Position := StrToInt(FConfig.Strings[8]);
    Form2.Prioridade.Position := StrToInt(FConfig.Strings[9]);
    if not DirectoryExists(FConfig.Strings[0]) then
      MkDir(FConfig.Strings[0]);
    DirectoryListBox2.Directory := FConfig.Strings[0];
  end;
end;

procedure TForm1.SaveConfig(Sender: TObject);
var
  ConfigPath: string;
begin
  ConfigPath := ExtractFilePath(Application.ExeName) + 'Configura.txt';
  FConfig.Clear;
  DirectoryListBox2.Directory := Form2.Edit1.Text;
  FConfig.Add(Form2.Edit1.Text);
  FConfig.Add(IntToStr(Form2.ComboBox1.ItemIndex));
  FConfig.Add(iif(Form2.cbPrivado.Checked, 'Sim', 'N' + #227 + 'o'));
  FConfig.Add(iif(Form2.cbCRC.Checked, 'Sim', 'N' + #227 + 'o'));
  FConfig.Add(iif(Form2.cbCopy.Checked, 'Sim', 'N' + #227 + 'o'));
  FConfig.Add(iif(Form2.cbOriginal.Checked, 'Sim', 'N' + #227 + 'o'));
  FConfig.Add(IntToStr(Form2.ComboBox2.ItemIndex));
  FConfig.Add(IntToStr(Form2.RadioGroup1.ItemIndex));
  FConfig.Add(IntToStr(Form2.kbps.Position));
  FConfig.Add(IntToStr(Form2.Prioridade.Position));
  FConfig.SaveToFile(ConfigPath);
end;

procedure TForm1.OnEncoderChanged(Sender: TObject);
begin
  FThreadConverter.EncoderIndex := Form2.ComboBox1.ItemIndex;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i: Integer;
  OutputDir, DirCaption, LastChar: string;
  EncoderConfig: TEncoderConfig;
  DirLen: Integer;
begin
  ShowMessage('Button1Click started.'); // DIAGNOSTIC
  FCancelled := False;
  FWavList.Clear;

  EncoderConfig := Form2.GetEncoderConfig;

  if FileListBox1.Items.Count - 1 >= 0 then
  begin
    if not DirectoryExists(Form2.Edit1.Text) then
    begin
      CreateDir(Form2.Edit1.Text);
      DirectoryListBox2.Directory := Form2.Edit1.Text;
    end
    else
      DirectoryListBox2.Directory := Form2.Edit1.Text;

    for i := 0 to FileListBox1.Items.Count - 1 do
      if FileListBox1.Selected[i] then
      begin
        DirLen := Length(LBDirWav.Caption);
        LastChar := Copy(LBDirWav.Caption, DirLen, 1);
        if LastChar = '\' then
          FWavList.Add(ExtractFileDir(LBDirWav.Caption) + FileListBox1.Items[i])
        else
          FWavList.Add(LBDirWav.Caption + '\' + FileListBox1.Items[i]);
      end;

    if FWavList.Count - 1 >= 0 then
    begin
      Button4.Enabled := False;
      Button5.Enabled := False;
      Button1.Enabled := False;

      DirLen := Length(LBDirMp3.Caption);
      DirCaption := Copy(LBDirMp3.Caption, DirLen, 1);
      if DirCaption = '\' then
        OutputDir := ExtractFileDir(LBDirMp3.Caption)
      else
        OutputDir := LBDirMp3.Caption + '\';

      Form3 := TForm3.Create(Application);
      Form3.CancelFlag := @FCancelled;
      Form3.Show;
      ShowMessage('Form3 shown. FWavList count: ' + IntToStr(FWavList.Count)); // DIAGNOSTIC

      FThreadConverter.OnComplete := OnConversionComplete;
      FThreadConverter.EncoderIndex := Form2.GetEncoderIndex;
      FThreadConverter.ConvertWavToMP3(
        Form2.ThreadPriority,
        FWavList,
        OutputDir,
        EncoderConfig,
        Form3.LBArquivos,
        Form3.ProgressBar1,
        @FCancelled);
    end;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FCancelled := True;
  FThreadConverter.Free;
  FWavList.Free;
  FConfig.Free;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  i: Integer;
begin
  if FileListBox1.Items.Count - 1 >= 0 then
    for i := 0 to FileListBox1.Items.Count - 1 do
      FileListBox1.Selected[i] := False;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  NewFolderName, DirPath, LastChar: string;
  DirLen: Integer;
begin
  NewFolderName := InputBox('Nova Pasta', 'Nome da Pasta:', '');
  if NewFolderName <> '' then
  begin
    DirLen := Length(LBDirMp3.Caption);
    LastChar := Copy(LBDirMp3.Caption, DirLen, 1);
    if LastChar = '\' then
      DirPath := ExtractFileDir(LBDirMp3.Caption)
    else
      DirPath := LBDirMp3.Caption + '\';
    MkDir(DirPath + NewFolderName);
    DirectoryListBox2.Directory := DirPath + NewFolderName;
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  Versions: TStrings;
begin
  LoadConfig;
  FileListBox1.ItemIndex := 0;
  Form2.ComboBox1.Clear;
  Versions := FThreadConverter.GetEncoderVersions;
  try
    Form2.ComboBox1.Items := Versions;
  finally
    Versions.Free;
  end;
  Form2.ComboBox1.ItemIndex := StrToInt(FConfig.Strings[1]);
  if Form2.ComboBox1.ItemIndex = -1 then
    Form2.ComboBox1.ItemIndex := 0;
  Form2.ShowModal;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
var
  i: Integer;
begin
  if FileListBox1.Items.Count - 1 >= 0 then
    for i := 0 to FileListBox1.Items.Count - 1 do
      FileListBox1.Selected[i] := True;
end;

procedure TForm1.OnConversionComplete(Sender: TObject);
begin
  if Form3 <> nil then
  begin
    Form3.Release;
    Form3 := nil;
  end;
  Button4.Enabled := True;
  Button5.Enabled := True;
  Button1.Enabled := True;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  DefaultDir: string;
  Versions: TStrings;
begin
  Form2 := TForm2.Create(Application);
  Form2.OnSaveConfig := SaveConfig;
  Form2.OnEncoderChanged := OnEncoderChanged;
  FConfig := TStringList.Create;
  FWavList := TStringList.Create;
  FCancelled := False;

  Self.AutoSize := True;
  Self.Position := poScreenCenter;

  DefaultDir := ExtractFileDrive(Application.ExeName) + '\';
  DirectoryListBox1.Directory := DefaultDir;

  LoadConfig;
  FThreadConverter := TThreadConverter.Create;

  Versions := FThreadConverter.GetEncoderVersions;
  try
    Form2.ComboBox1.Items := Versions;
  finally
    Versions.Free;
  end;
  if Form2.ComboBox1.Items.Count > 0 then
  begin
    Form2.ComboBox1.ItemIndex := StrToInt(FConfig.Strings[1]);
    if Form2.ComboBox1.ItemIndex < 0 then
      Form2.ComboBox1.ItemIndex := 0;
  end;
  FThreadConverter.EncoderIndex := Form2.ComboBox1.ItemIndex;
end;

end.
