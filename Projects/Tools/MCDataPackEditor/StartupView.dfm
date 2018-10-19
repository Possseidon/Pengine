object frmStartup: TfrmStartup
  Left = 0
  Top = 0
  Cursor = crHourGlass
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Starting up MCDataPackEditor'
  ClientHeight = 148
  ClientWidth = 480
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 474
    Height = 13
    Cursor = crHourGlass
    Align = alTop
    Alignment = taCenter
    Caption = 'Loading...'
    ExplicitWidth = 49
  end
  object pbProgress: TProgressBar
    AlignWithMargins = True
    Left = 3
    Top = 105
    Width = 474
    Height = 17
    Cursor = crHourGlass
    Align = alBottom
    TabOrder = 0
  end
  object pbProgressSecondary: TProgressBar
    AlignWithMargins = True
    Left = 3
    Top = 128
    Width = 474
    Height = 17
    Align = alBottom
    TabOrder = 1
  end
  object memProgress: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 22
    Width = 474
    Height = 77
    Align = alClient
    Lines.Strings = (
      '')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
end
