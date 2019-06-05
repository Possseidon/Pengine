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
        Action = actNew
      end
      object Open1: TMenuItem
        Action = actOpen
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Save1: TMenuItem
        Action = actSave
      end
      object Saveas1: TMenuItem
        Action = actSaveAs
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = actExit
      end
    end
  end
  object alMain: TActionList
    Left = 24
    Top = 64
    object actNew: TAction
      Category = 'File'
      Caption = 'New'
      ShortCut = 16462
      OnExecute = actNewExecute
    end
    object actSaveAs: TAction
      Category = 'File'
      Caption = 'Save as...'
      ShortCut = 24659
      OnExecute = actSaveAsExecute
    end
    object actSave: TAction
      Category = 'File'
      Caption = 'Save'
      ShortCut = 16467
      OnExecute = actSaveExecute
    end
    object actOpen: TAction
      Category = 'File'
      Caption = 'Open...'
      ShortCut = 16463
      OnExecute = actOpenExecute
    end
    object actExit: TAction
      Category = 'File'
      Caption = 'Exit'
      ShortCut = 32883
      OnExecute = actExitExecute
    end
  end
  object odOpen: TOpenDialog
    DefaultExt = '.json'
    Filter = 'Factory Configuration|*.json'
    Options = [ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 96
    Top = 8
  end
  object sdSave: TSaveDialog
    DefaultExt = '.json'
    Filter = 'Factory Configuration|*.json'
    Left = 96
    Top = 64
  end
end
