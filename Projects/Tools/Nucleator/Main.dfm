object frmMain: TfrmMain
  Left = 0
  Top = 0
  AlphaBlend = True
  Caption = 'Nucleator'
  ClientHeight = 346
  ClientWidth = 332
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inline frmPreview: TfrmPreview
    AlignWithMargins = True
    Left = 260
    Top = 3
    Width = 69
    Height = 340
    Align = alClient
    TabOrder = 0
    Visible = False
    ExplicitLeft = 260
    ExplicitTop = 3
    ExplicitWidth = 69
    ExplicitHeight = 340
  end
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 257
    Height = 346
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
  end
  object alActions: TActionList
    Left = 496
    Top = 72
    object actExit: TAction
      Category = 'File'
      Caption = 'Exit'
      OnExecute = actExitExecute
    end
    object actPreview: TAction
      Category = 'View'
      Caption = 'Preview'
      OnExecute = actPreviewExecute
      OnUpdate = actPreviewUpdate
    end
  end
  object mmMain: TMainMenu
    Left = 496
    Top = 16
    object File1: TMenuItem
      Caption = 'File'
      object Exit1: TMenuItem
        Action = actExit
      end
    end
    object View1: TMenuItem
      Caption = 'View'
      object Preview1: TMenuItem
        Action = actPreview
      end
    end
  end
end
