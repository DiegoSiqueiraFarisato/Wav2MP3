program WavpraMp3;

uses
  Forms,
  UEncoderTypes in 'UEncoderTypes.pas',
  BladeEnc in 'BladeEnc.pas',
  LameEnc in 'LameEnc.pas',
  Uprincipal in 'Uprincipal.pas' {Form1},
  UThreadConverter in 'UThreadConverter.pas',
  UConfigura in 'UConfigura.pas' {Form2},
  UTelaBP in 'UTelaBP.pas' {Form3};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
