object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Brigadier Testing'
  ClientHeight = 344
  ClientWidth = 498
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lbError: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 300
    Width = 492
    Height = 13
    Margins.Top = 0
    Margins.Bottom = 0
    Align = alBottom
    WordWrap = True
    ExplicitWidth = 3
  end
  object synEdit: TSynEdit
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 492
    Height = 294
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Consolas'
    Font.Pitch = fpFixed
    Font.Style = []
    TabOrder = 0
    Gutter.Font.Charset = ANSI_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -16
    Gutter.Font.Name = 'Consolas'
    Gutter.Font.Style = []
    Gutter.ShowLineNumbers = True
    Lines.Strings = (
      '@e[type=zombie, x=1, y=56, z=100, distance=1..]'
      '@p[name="Oninoni", nbt={Cool:1b}]'
      ''
      '@s[name="Possseidon", scores={PowerLevel=9001..}]')
    OnChange = synEditChange
    FontSmoothing = fsmNone
  end
  object btnAutoFormat: TButton
    AlignWithMargins = True
    Left = 3
    Top = 316
    Width = 492
    Height = 25
    Align = alBottom
    Caption = 'Auto Format'
    TabOrder = 1
    OnClick = btnAutoFormatClick
  end
  object synCompletion: TSynCompletionProposal
    Options = [scoUseInsertList, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter]
    ClSelect = 16744319
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Consolas'
    Font.Style = [fsBold]
    TitleFont.Charset = ANSI_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -16
    TitleFont.Name = 'Consolas'
    TitleFont.Style = [fsBold]
    Columns = <>
    OnExecute = synCompletionExecute
    ShortCut = 16416
    Editor = synEdit
    Left = 152
    Top = 160
  end
end
