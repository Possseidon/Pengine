object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'NeoPixel'
  ClientHeight = 119
  ClientWidth = 229
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnStart: TButton
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 223
    Height = 57
    Align = alTop
    Caption = 'Start'
    TabOrder = 0
    OnClick = btnStartClick
  end
  object pnlColor: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 66
    Width = 223
    Height = 50
    Align = alClient
    Color = clRed
    ParentBackground = False
    TabOrder = 1
  end
end