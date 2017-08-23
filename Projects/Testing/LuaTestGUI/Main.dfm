object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Lua Test'
  ClientHeight = 342
  ClientWidth = 460
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
    Top = 298
    Width = 454
    Height = 41
    Align = alBottom
    TabOrder = 0
    object lbError: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 374
      Height = 33
      Align = alClient
      AutoSize = False
      WordWrap = True
      ExplicitWidth = 3
      ExplicitHeight = 13
    end
    object btnRun: TButton
      AlignWithMargins = True
      Left = 384
      Top = 4
      Width = 66
      Height = 33
      Align = alRight
      Caption = 'Run'
      TabOrder = 0
      OnClick = btnRunClick
    end
  end
  object seCode: TSynEdit
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 454
    Height = 289
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
      '')
    Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoTabsToSpaces]
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
