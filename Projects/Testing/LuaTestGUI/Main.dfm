object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Lua Test'
  ClientHeight = 383
  ClientWidth = 504
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
    Top = 341
    Width = 498
    Height = 39
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      498
      39)
    object lbError: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 279
      Height = 31
      Align = alLeft
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      WordWrap = True
      ExplicitWidth = 235
      ExplicitHeight = 22
    end
    object btnRun: TButton
      AlignWithMargins = True
      Left = 427
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
      Left = 361
      Top = 4
      Width = 61
      Height = 22
      Anchors = [akTop, akRight]
      MaxValue = 60000
      MinValue = 1
      TabOrder = 1
      Value = 100
    end
    object cbTimeout: TCheckBox
      Left = 269
      Top = 6
      Width = 86
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Timeout (ms):'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = cbTimeoutClick
    end
  end
  object seCode: TSynEdit
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 498
    Height = 332
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
      'x = {'
      '  7,'
      '  6,'
      '  8,'
      '  8,'
      '  9,'
      '  9,'
      '  5,'
      '  3,'
      '  6,'
      '  7,'
      '  9,'
      '  8,'
      '  7,'
      '  9,'
      '  3,'
      '  1,'
      '  6,'
      '  7,'
      '  9,'
      '  2'
      '}'
      ''
      'i = 0'
      ''
      'table.sort(x, function(a, b) i = i + 1 return a < b end)'
      ''
      'print(i .. " comparissions")'
      ''
      'print(table.concat(x, ", "))')
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
