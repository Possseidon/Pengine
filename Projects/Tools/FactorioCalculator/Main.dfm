object Form9: TForm9
  Left = 0
  Top = 0
  Caption = 'Factorio Calculator'
  ClientHeight = 435
  ClientWidth = 690
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pbFactory: TPaintBox
    Left = 0
    Top = 0
    Width = 690
    Height = 435
    Align = alClient
    OnPaint = pbFactoryPaint
    ExplicitLeft = 192
    ExplicitTop = 208
    ExplicitWidth = 105
    ExplicitHeight = 105
  end
  object mmMenu: TMainMenu
    Left = 24
    Top = 16
    object File1: TMenuItem
      Caption = 'File'
      object New1: TMenuItem
        Caption = 'New'
      end
      object Open1: TMenuItem
        Caption = 'Open...'
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Save1: TMenuItem
        Caption = 'Save'
      end
      object Saveas1: TMenuItem
        Caption = 'Save as...'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
      end
    end
  end
end
