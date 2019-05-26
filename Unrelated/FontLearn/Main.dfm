object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Font Learn'
  ClientHeight = 158
  ClientWidth = 519
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lbQuestion: TLabel
    Left = 0
    Top = 44
    Width = 519
    Height = 66
    Align = alClient
    Alignment = taCenter
    AutoSize = False
    Caption = '...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -40
    Font.Name = 'Default'
    Font.Style = []
    ParentFont = False
    Layout = tlCenter
    ExplicitLeft = -3
    ExplicitTop = 0
    ExplicitWidth = 354
    ExplicitHeight = 65
  end
  object edtInput: TEdit
    AlignWithMargins = True
    Left = 3
    Top = 113
    Width = 513
    Height = 42
    Align = alBottom
    Alignment = taCenter
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -28
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnKeyPress = edtInputKeyPress
    ExplicitWidth = 348
  end
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 513
    Height = 38
    Align = alTop
    TabOrder = 1
    ExplicitWidth = 348
    object lbStatistics: TLabel
      Left = 414
      Top = 11
      Width = 12
      Height = 13
      Alignment = taRightJustify
      Caption = '...'
    end
    object cbMode: TComboBox
      Left = 175
      Top = 8
      Width = 89
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'Letter'
      OnChange = cbModeChange
      Items.Strings = (
        'Letter'
        'Word')
    end
    object btnStatistics: TButton
      Left = 432
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Statistics'
      TabOrder = 1
      OnClick = btnStatisticsClick
    end
    object cbFont: TComboBox
      Left = 11
      Top = 8
      Width = 158
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 2
      Text = 'Standard Galactic Alphabet'
      OnChange = cbFontChange
      Items.Strings = (
        'Standard Galactic Alphabet'
        'Futurama Alien Alphabet One'
        'Futurama Alien Alphabet Two')
    end
  end
end
