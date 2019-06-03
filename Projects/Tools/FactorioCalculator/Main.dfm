object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Factorio Calculator'
  ClientHeight = 435
  ClientWidth = 690
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  inline frmFactory: TfrmFactory
    Left = 0
    Top = 0
    Width = 690
    Height = 435
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 690
    ExplicitHeight = 435
  end
  object mmMain: TMainMenu
    Left = 24
    Top = 8
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
