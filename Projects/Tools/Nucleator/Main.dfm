object frmMain: TfrmMain
  Left = 0
  Top = 0
  AlphaBlend = True
  Caption = 'Nucleator'
  ClientHeight = 478
  ClientWidth = 900
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = False
  OnCanResize = FormCanResize
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inline frmPreview: TfrmPreview
    AlignWithMargins = True
    Left = 444
    Top = 3
    Width = 453
    Height = 472
    Align = alClient
    TabOrder = 0
    Visible = False
    ExplicitLeft = 444
    ExplicitTop = 3
    ExplicitWidth = 373
    ExplicitHeight = 480
  end
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 441
    Height = 478
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitHeight = 486
    object gbEvolution: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 435
      Height = 246
      Align = alTop
      Caption = 'Evolution'
      TabOrder = 0
      DesignSize = (
        435
        246)
      object Label6: TLabel
        Left = 10
        Top = 21
        Width = 57
        Height = 13
        Caption = 'Generation:'
      end
      object Label7: TLabel
        Left = 136
        Top = 46
        Width = 42
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Worst'
      end
      object Label8: TLabel
        Left = 184
        Top = 46
        Width = 42
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Average'
      end
      object Label9: TLabel
        Left = 232
        Top = 46
        Width = 42
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Best'
      end
      object Label10: TLabel
        Left = 10
        Top = 68
        Width = 38
        Height = 13
        Caption = 'Fitness:'
      end
      object Label1: TLabel
        Left = 10
        Top = 95
        Width = 50
        Height = 13
        Caption = 'Efficiency:'
      end
      object Label2: TLabel
        Left = 10
        Top = 122
        Width = 90
        Height = 13
        Caption = 'Power Generation:'
      end
      object Label3: TLabel
        Left = 10
        Top = 149
        Width = 83
        Height = 13
        Caption = 'Heat Generation:'
      end
      object btnSingleStep: TButton
        Left = 136
        Top = 180
        Width = 138
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Single Step'
        TabOrder = 0
        ExplicitTop = 196
      end
      object btnStartStop: TButton
        Left = 136
        Top = 211
        Width = 138
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Start'
        TabOrder = 1
        ExplicitTop = 227
      end
      object GroupBox2: TGroupBox
        AlignWithMargins = True
        Left = 290
        Top = 18
        Width = 140
        Height = 223
        Align = alRight
        Caption = 'Current Population'
        TabOrder = 2
        ExplicitHeight = 239
        object lbPopulation: TListBox
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 130
          Height = 169
          Align = alClient
          ItemHeight = 13
          PopupMenu = pmPopulation
          TabOrder = 0
          ExplicitHeight = 185
        end
        object btnInspect: TButton
          AlignWithMargins = True
          Left = 5
          Top = 193
          Width = 130
          Height = 25
          Align = alBottom
          Caption = 'Inspect'
          TabOrder = 1
          ExplicitTop = 209
        end
      end
      object seGeneration: TSpinEdit
        Left = 136
        Top = 18
        Width = 138
        Height = 22
        Enabled = False
        MaxValue = 0
        MinValue = 0
        TabOrder = 3
        Value = 0
      end
      object edtFitnessWorst: TEdit
        Left = 136
        Top = 65
        Width = 42
        Height = 21
        ReadOnly = True
        TabOrder = 4
        Text = '-'
      end
      object edtFitnessAverage: TEdit
        Left = 184
        Top = 65
        Width = 42
        Height = 21
        ReadOnly = True
        TabOrder = 5
        Text = '-'
      end
      object edtFitnessBest: TEdit
        Left = 232
        Top = 65
        Width = 42
        Height = 21
        ReadOnly = True
        TabOrder = 6
        Text = '-'
      end
      object edtEfficiencyWorst: TEdit
        Left = 136
        Top = 92
        Width = 42
        Height = 21
        ReadOnly = True
        TabOrder = 7
        Text = '-'
      end
      object edtEfficiencyAverage: TEdit
        Left = 184
        Top = 92
        Width = 42
        Height = 21
        ReadOnly = True
        TabOrder = 8
        Text = '-'
      end
      object edtEfficiencyBest: TEdit
        Left = 232
        Top = 92
        Width = 42
        Height = 21
        ReadOnly = True
        TabOrder = 9
        Text = '-'
      end
      object edtPowerGenerationWorst: TEdit
        Left = 136
        Top = 119
        Width = 42
        Height = 21
        ReadOnly = True
        TabOrder = 10
        Text = '-'
      end
      object edtPowerGenerationAverage: TEdit
        Left = 184
        Top = 119
        Width = 42
        Height = 21
        ReadOnly = True
        TabOrder = 11
        Text = '-'
      end
      object edtPowerGenerationBest: TEdit
        Left = 232
        Top = 119
        Width = 42
        Height = 21
        ReadOnly = True
        TabOrder = 12
        Text = '-'
      end
      object edtHeatGenerationWorst: TEdit
        Left = 136
        Top = 146
        Width = 42
        Height = 21
        ReadOnly = True
        TabOrder = 13
        Text = '-'
      end
      object edtHeatGenerationAverage: TEdit
        Left = 184
        Top = 146
        Width = 42
        Height = 21
        ReadOnly = True
        TabOrder = 14
        Text = '-'
      end
      object edtHeatGenerationBest: TEdit
        Left = 232
        Top = 146
        Width = 42
        Height = 21
        ReadOnly = True
        TabOrder = 15
        Text = '-'
      end
      object aiEvolving: TActivityIndicator
        Left = 46
        Top = 184
        Anchors = [akLeft, akBottom]
        IndicatorSize = aisLarge
        ExplicitTop = 200
      end
    end
    object gbGraph: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 255
      Width = 435
      Height = 220
      Align = alClient
      Caption = 'Statistics'
      TabOrder = 1
      ExplicitTop = 271
      ExplicitHeight = 212
      object pbGraph: TPaintBox
        AlignWithMargins = True
        Left = 5
        Top = 18
        Width = 425
        Height = 197
        Align = alClient
        OnPaint = pbGraphPaint
        ExplicitLeft = 120
        ExplicitWidth = 310
        ExplicitHeight = 137
      end
    end
  end
  object alActions: TActionList
    Left = 528
    Top = 24
    object actPreview: TAction
      Category = 'View'
      Caption = '3D Preview'
      OnExecute = actPreviewExecute
      OnUpdate = actPreviewUpdate
    end
    object actSingleStep: TAction
      Category = 'Evolution'
      Caption = 'actSingleStep'
    end
    object actNew: TAction
      Category = 'File'
      Caption = 'New'
    end
    object actOpen: TAction
      Category = 'File'
      Caption = 'Open...'
    end
    object actSave: TAction
      Category = 'File'
      Caption = 'actSave'
    end
    object actSaveAs: TAction
      Category = 'File'
      Caption = 'actSaveAs'
    end
    object actExit: TAction
      Category = 'File'
      Caption = 'Exit'
      OnExecute = actExitExecute
    end
    object actCurrentSettings: TAction
      Category = 'View'
      Caption = 'Current Settings...'
    end
    object actStartStop: TAction
      Category = 'Evolution'
      Caption = 'actStartStop'
    end
  end
  object mmMain: TMainMenu
    Left = 472
    Top = 24
    object File1: TMenuItem
      Caption = 'File'
      object actNew1: TMenuItem
        Action = actNew
        Caption = 'New...'
      end
      object Open1: TMenuItem
        Action = actOpen
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object actSave1: TMenuItem
        Action = actSave
        Caption = 'Save'
      end
      object actSaveAs1: TMenuItem
        Action = actSaveAs
        Caption = 'Save as...'
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = actExit
      end
    end
    object View1: TMenuItem
      Caption = 'View'
      object ShowSettings1: TMenuItem
        Action = actCurrentSettings
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Preview1: TMenuItem
        Action = actPreview
      end
    end
    object Evolution1: TMenuItem
      Caption = 'Evolution'
      object actSingleStep1: TMenuItem
        Action = actSingleStep
      end
      object actStartStop1: TMenuItem
        Action = actStartStop
      end
      object N4: TMenuItem
        Caption = '-'
      end
    end
  end
  object pmPopulation: TPopupMenu
    Left = 350
    Top = 87
    object Inspect1: TMenuItem
      Caption = 'Inspect'
    end
    object InstactAll1: TMenuItem
      Caption = 'Inspect All'
    end
  end
end
