object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Physics 2D'
  ClientHeight = 336
  ClientWidth = 505
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object tmrUpdate: TTimer
    Interval = 16
    OnTimer = tmrUpdateTimer
    Left = 24
    Top = 24
  end
end
