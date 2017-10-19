object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Botania Dandelifeon Simulator'
  ClientHeight = 575
  ClientWidth = 500
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
  PixelsPerInch = 96
  TextHeight = 13
  object pbGame: TPaintBox
    Left = 0
    Top = 0
    Width = 500
    Height = 500
    Align = alClient
    OnMouseDown = pbGameMouseDown
    OnMouseMove = pbGameMouseMove
    OnMouseUp = pbGameMouseUp
    OnPaint = pbGamePaint
    ExplicitLeft = 8
    ExplicitTop = -6
  end
  object pnlControl: TPanel
    Left = 0
    Top = 500
    Width = 500
    Height = 75
    Align = alBottom
    TabOrder = 0
    object lbGenerations: TLabel
      Left = 16
      Top = 6
      Width = 77
      Height = 13
      Caption = 'Generations: ...'
    end
    object lbMana: TLabel
      Left = 16
      Top = 25
      Width = 45
      Height = 13
      Caption = 'Mana: ...'
    end
    object lbCellCount: TLabel
      Left = 269
      Top = 6
      Width = 59
      Height = 13
      Caption = 'CellCount: 0'
    end
    object lbManaPerCell: TLabel
      Left = 16
      Top = 44
      Width = 95
      Height = 13
      Caption = 'Mana/CellCount: ...'
    end
    object btnSimulate: TButton
      Left = 421
      Top = 46
      Width = 75
      Height = 25
      Caption = 'Simulate'
      TabOrder = 0
      OnClick = btnSimulateClick
    end
    object btnShowStart: TButton
      Left = 340
      Top = 46
      Width = 75
      Height = 25
      Caption = 'Show Start'
      TabOrder = 1
      OnClick = btnShowStartClick
    end
    object btnRandom: TButton
      Left = 259
      Top = 46
      Width = 75
      Height = 25
      Caption = 'Random'
      TabOrder = 2
      OnClick = btnRandomClick
    end
  end
end
