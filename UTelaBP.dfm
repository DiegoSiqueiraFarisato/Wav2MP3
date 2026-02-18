object Form3: TForm3
  Left = 307
  Top = 337
  BorderStyle = bsNone
  ClientHeight = 130
  ClientWidth = 316
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 316
    Height = 130
    Align = alClient
    BevelInner = bvLowered
    TabOrder = 0
    object LBArquivos: TLabel
      Left = 5
      Top = 16
      Width = 2
      Height = 13
      Caption = 'l'
    end
    object Button1: TButton
      Left = 225
      Top = 85
      Width = 86
      Height = 31
      Caption = 'Cancelar'
      TabOrder = 0
      OnClick = Button1Click
    end
    object ProgressBar1: TProgressBar
      Left = 5
      Top = 46
      Width = 306
      Height = 20
      Min = 0
      Max = 100
      TabOrder = 1
    end
  end
end
