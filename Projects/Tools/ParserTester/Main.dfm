object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Parser Tester'
  ClientHeight = 454
  ClientWidth = 666
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
  object synEditor: TSynEdit
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 660
    Height = 343
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Consolas'
    Font.Pitch = fpFixed
    Font.Style = []
    TabOrder = 0
    CodeFolding.GutterShapeSize = 11
    CodeFolding.CollapsedLineColor = clGrayText
    CodeFolding.FolderBarLinesColor = clGrayText
    CodeFolding.IndentGuidesColor = clGray
    CodeFolding.IndentGuides = True
    CodeFolding.ShowCollapsedLine = False
    CodeFolding.ShowHintMark = True
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    OnChange = synEditorChange
    FontSmoothing = fsmNone
  end
  object pnlSettings: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 352
    Width = 660
    Height = 99
    Align = alBottom
    TabOrder = 1
    object cbParsers: TComboBox
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 652
      Height = 21
      Align = alTop
      AutoDropDown = True
      Style = csDropDownList
      TabOrder = 0
      OnChange = cbParsersChange
    end
    object memLog: TMemo
      AlignWithMargins = True
      Left = 4
      Top = 31
      Width = 652
      Height = 64
      Align = alClient
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 1
    end
  end
end
