object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Minesweeper AI'
  ClientHeight = 331
  ClientWidth = 391
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Gill Sans Ultra Bold'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnMouseDown = FormMouseDown
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 18
  object tmrAutoEvolve: TTimer
    Enabled = False
    Interval = 20
    OnTimer = tmrAutoEvolveTimer
    Left = 184
    Top = 168
  end
end
