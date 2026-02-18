object Form2: TForm2
  Left = 349
  Top = 199
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Configura'#231#245'es'
  ClientHeight = 372
  ClientWidth = 476
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 461
    Height = 333
    TabOrder = 0
    object PageControl1: TPageControl
      Left = 1
      Top = 1
      Width = 385
      Height = 331
      ActivePage = TabSheet2
      Align = alLeft
      TabOrder = 0
      object TabSheet2: TTabSheet
        Caption = 'Encoder'
        ImageIndex = 1
        object Panel2: TPanel
          Left = 0
          Top = 0
          Width = 377
          Height = 66
          Align = alTop
          BevelInner = bvLowered
          TabOrder = 0
          object Label1: TLabel
            Left = 20
            Top = 10
            Width = 92
            Height = 13
            Caption = 'Tipos de Encoders:'
          end
          object ComboBox1: TComboBox
            Left = 20
            Top = 25
            Width = 341
            Height = 21
            Style = csDropDownList
            TabOrder = 0
            OnChange = ComboBox1Change
          end
        end
        object GroupBox1: TGroupBox
          Left = 20
          Top = 82
          Width = 180
          Height = 81
          Caption = 'Qualidade:'
          TabOrder = 1
          object Lkbp: TLabel
            Left = 93
            Top = 15
            Width = 44
            Height = 14
            Alignment = taRightJustify
            Caption = '128 kbps'
            Font.Charset = ANSI_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            Transparent = True
          end
          object kbps: TTrackBar
            Left = 8
            Top = 30
            Width = 133
            Height = 32
            Max = 13
            Position = 8
            TabOrder = 0
            ThumbLength = 15
            TickMarks = tmBoth
            OnChange = kbpsChange
          end
        end
        object GroupBox2: TGroupBox
          Left = 205
          Top = 82
          Width = 155
          Height = 81
          Caption = 'Prioridade:'
          TabOrder = 2
          object LPrioridade: TLabel
            Left = 113
            Top = 15
            Width = 27
            Height = 14
            Alignment = taRightJustify
            Caption = 'Baixa'
            Font.Charset = ANSI_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            Transparent = True
          end
          object Prioridade: TTrackBar
            Left = 8
            Top = 30
            Width = 133
            Height = 32
            Max = 4
            TabOrder = 0
            ThumbLength = 15
            TickMarks = tmBoth
            OnChange = PrioridadeChange
          end
        end
        object Panel3: TPanel
          Left = 0
          Top = 175
          Width = 377
          Height = 128
          Align = alBottom
          BevelInner = bvLowered
          TabOrder = 3
          object Label2: TLabel
            Left = 20
            Top = 10
            Width = 119
            Height = 13
            Caption = 'Samples / Segundo (Hz):'
          end
          object ComboBox2: TComboBox
            Left = 20
            Top = 25
            Width = 180
            Height = 19
            Style = csOwnerDrawFixed
            ItemHeight = 13
            TabOrder = 0
            Items.Strings = (
              '48000'
              '44100'
              '32000')
          end
          object RadioGroup1: TRadioGroup
            Left = 20
            Top = 50
            Width = 180
            Height = 75
            Caption = 'Canais:'
            ItemIndex = 0
            Items.Strings = (
              'Est'#233'reo'
              'Mono')
            TabOrder = 1
          end
          object GroupBox3: TGroupBox
            Left = 205
            Top = 20
            Width = 156
            Height = 105
            TabOrder = 2
            object cbPrivado: TCheckBox
              Left = 9
              Top = 20
              Width = 97
              Height = 17
              Caption = 'Privado'
              TabOrder = 0
            end
            object cbCRC: TCheckBox
              Left = 9
              Top = 40
              Width = 97
              Height = 17
              Caption = 'CRC'
              TabOrder = 1
            end
            object cbCopy: TCheckBox
              Left = 9
              Top = 60
              Width = 137
              Height = 17
              Caption = 'Direito Autoral (Copyright)'
              TabOrder = 2
            end
            object cbOriginal: TCheckBox
              Left = 9
              Top = 80
              Width = 97
              Height = 17
              Caption = 'Original'
              TabOrder = 3
            end
          end
        end
      end
      object TabSheet1: TTabSheet
        Caption = 'Diret'#243'rios'
        ImageIndex = 1
        object Panel4: TPanel
          Left = 0
          Top = 0
          Width = 377
          Height = 81
          Align = alTop
          BevelInner = bvLowered
          TabOrder = 0
          object GroupBox4: TGroupBox
            Left = 10
            Top = 5
            Width = 356
            Height = 66
            Caption = 'Configura'#231#227'o do Diret'#243'rio de Destino'
            TabOrder = 0
            object StaticText1: TStaticText
              Left = 8
              Top = 15
              Width = 198
              Height = 17
              BorderStyle = sbsSunken
              Caption = 'Diret'#243'rio de Destino das Faixas de Audio:'
              TabOrder = 0
            end
            object Edit1: TEdit
              Left = 8
              Top = 35
              Width = 306
              Height = 21
              CharCase = ecUpperCase
              Color = clBtnFace
              ReadOnly = True
              TabOrder = 1
            end
            object Button2: TButton
              Left = 325
              Top = 35
              Width = 21
              Height = 21
              Caption = '...'
              TabOrder = 2
              OnClick = Button2Click
            end
          end
        end
      end
    end
    object Button1: TButton
      Left = 390
      Top = 20
      Width = 65
      Height = 25
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 1
      OnClick = Button1Click
    end
  end
end
