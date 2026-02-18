unit UTelaBP;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Gauges;

type
  TForm3 = class(TForm)
    Panel1: TPanel;
    LBArquivos: TLabel;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    procedure Button1Click(Sender: TObject);
  private
    FCancelFlag: PBoolean;
  public
    property CancelFlag: PBoolean read FCancelFlag write FCancelFlag;
  end;

var
  Form3: TForm3;

implementation

{$R *.DFM}

procedure TForm3.Button1Click(Sender: TObject);
begin
  if FCancelFlag <> nil then
    FCancelFlag^ := True;
  Close;
end;

end.
