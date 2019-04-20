object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'ScribbleBot'
  ClientHeight = 216
  ClientWidth = 236
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnEditDrawArea: TButton
    Left = 8
    Top = 8
    Width = 129
    Height = 25
    Caption = 'Edit Drawing Area'
    TabOrder = 0
    OnClick = btnEditDrawAreaClick
  end
  object btnEditPaletteArea: TButton
    Left = 8
    Top = 39
    Width = 129
    Height = 25
    Caption = 'Edit Platette Area'
    TabOrder = 1
    OnClick = btnEditPaletteAreaClick
  end
end
