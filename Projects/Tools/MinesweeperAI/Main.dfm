object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Minesweeper AI'
  ClientHeight = 509
  ClientWidth = 1079
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseDown = FormMouseDown
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object tmrUpdate: TTimer
    Interval = 1
    OnTimer = tmrUpdateTimer
    Left = 16
    Top = 8
  end
end
