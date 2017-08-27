object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Lua Test'
  ClientHeight = 362
  ClientWidth = 452
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottom: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 320
    Width = 446
    Height = 39
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      446
      39)
    object lbError: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 227
      Height = 31
      Align = alLeft
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      WordWrap = True
      ExplicitWidth = 235
      ExplicitHeight = 22
    end
    object Label1: TLabel
      Left = 237
      Top = 7
      Width = 66
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Timeout (ms):'
      ExplicitLeft = 245
    end
    object btnRun: TButton
      AlignWithMargins = True
      Left = 375
      Top = 4
      Width = 66
      Height = 22
      Anchors = [akTop, akRight]
      Caption = 'Run'
      TabOrder = 0
      OnClick = btnRunClick
    end
    object seTimeout: TSpinEdit
      AlignWithMargins = True
      Left = 309
      Top = 4
      Width = 61
      Height = 22
      Anchors = [akTop, akRight]
      MaxValue = 10000
      MinValue = 1
      TabOrder = 1
      Value = 100
    end
  end
  object seCode: TSynEdit
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 446
    Height = 311
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Consolas'
    Font.Pitch = fpFixed
    Font.Style = []
    TabOrder = 1
    Gutter.AutoSize = True
    Gutter.DigitCount = 2
    Gutter.Font.Charset = ANSI_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -16
    Gutter.Font.Name = 'Consolas'
    Gutter.Font.Style = []
    Gutter.ShowLineNumbers = True
    Lines.Strings = (
      'x = {1, 2, 3, 4, 5}'
      ''
      'for i, v in ipairs(x) do'
      '  print(i .. ": " .. v)'
      'end')
    Options = [eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoTabsToSpaces]
    TabWidth = 2
    WantTabs = True
    OnChange = seCodeChange
    FontSmoothing = fsmClearType
    RemovedKeystrokes = <>
    AddedKeystrokes = <
      item
        Command = ecDeleteWord
        ShortCut = 16430
      end>
  end
end
