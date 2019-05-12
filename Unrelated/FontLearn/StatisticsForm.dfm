object frmStatistics: TfrmStatistics
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Statistics'
  ClientHeight = 533
  ClientWidth = 660
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Chart1: TChart
    Left = 0
    Top = 0
    Width = 660
    Height = 533
    Legend.Title.Text.Strings = (
      'Characters')
    LeftAxis.Inverted = True
    View3D = False
    Align = alClient
    TabOrder = 0
    ExplicitLeft = 8
    ExplicitTop = -2
    ExplicitWidth = 358
    ExplicitHeight = 294
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 13
    object Series1: THorizBarSeries
      BarBrush.Gradient.Direction = gdLeftRight
      ColorEachPoint = True
      ConePercent = 100
      Marks.Visible = False
      Dark3D = False
      Gradient.Direction = gdLeftRight
      GradientRelative = True
      MultiBar = mbNone
      Sides = 83
      XValues.Name = 'Bar'
      XValues.Order = loNone
      YValues.Name = 'Y'
      YValues.Order = loNone
      Data = {
        050600000000000000000014400000000000907540FF01000000660000000000
        0010400000000000E06F40FF010000006500000000000008400000000000B068
        40FF010000006400000000000000400000000000506940FF0100000063000000
        000000F03F0000000000B06840FF010000006200000000000000000000000000
        005940FF0100000061}
      Detail = {0000000000}
    end
  end
end
