object DebugConsole: TDebugConsole
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Debug Console'
  ClientHeight = 373
  ClientWidth = 759
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object memConsole: TMemo
    Left = 0
    Top = 0
    Width = 759
    Height = 344
    Align = alClient
    Color = clBlack
    DoubleBuffered = True
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    ParentDoubleBuffered = False
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitHeight = 338
  end
  object Panel1: TPanel
    Left = 0
    Top = 344
    Width = 759
    Height = 29
    Align = alBottom
    TabOrder = 1
    object cbPaused: TCheckBox
      AlignWithMargins = True
      Left = 696
      Top = 4
      Width = 59
      Height = 21
      Align = alRight
      Caption = 'Paused'
      TabOrder = 0
    end
  end
end
