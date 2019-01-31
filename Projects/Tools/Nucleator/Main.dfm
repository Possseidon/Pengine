object frmMain: TfrmMain
  Left = 0
  Top = 0
  AlphaBlend = True
  Caption = 'Nucleator'
  ClientHeight = 478
  ClientWidth = 959
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
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  inline frmPreview: TfrmPreview
    AlignWithMargins = True
    Left = 532
    Top = 3
    Width = 424
    Height = 472
    Align = alClient
    TabOrder = 0
    Visible = False
    ExplicitLeft = 532
    ExplicitTop = 3
    ExplicitWidth = 365
    ExplicitHeight = 472
  end
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 529
    Height = 478
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object gbEvolution: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 523
      Height = 246
      Align = alTop
      Caption = 'Evolution'
      TabOrder = 0
      DesignSize = (
        523
        246)
      object lbGeneration: TLabel
        Left = 10
        Top = 21
        Width = 57
        Height = 13
        Caption = 'Generation:'
      end
      object lbWorst: TLabel
        Left = 136
        Top = 46
        Width = 42
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Worst'
      end
      object lbAverage: TLabel
        Left = 184
        Top = 46
        Width = 42
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Average'
      end
      object lbBest: TLabel
        Left = 232
        Top = 46
        Width = 42
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Best'
      end
      object lbFitness: TLabel
        Left = 10
        Top = 68
        Width = 38
        Height = 13
        Caption = 'Fitness:'
      end
      object lbEfficiency: TLabel
        Left = 10
        Top = 95
        Width = 50
        Height = 13
        Caption = 'Efficiency:'
      end
      object lbPowerGeneration: TLabel
        Left = 10
        Top = 122
        Width = 90
        Height = 13
        Caption = 'Power Generation:'
      end
      object lbNetHeatGeneration: TLabel
        Left = 10
        Top = 149
        Width = 103
        Height = 13
        Caption = 'Net Heat Generation:'
      end
      object btnSingleStep: TButton
        Left = 136
        Top = 180
        Width = 138
        Height = 25
        Action = actSingleStep
        Anchors = [akLeft, akBottom]
        TabOrder = 0
      end
      object btnStartStop: TButton
        Left = 136
        Top = 211
        Width = 138
        Height = 25
        Action = actStartStop
        Anchors = [akLeft, akBottom]
        TabOrder = 1
      end
      object gbPopulation: TGroupBox
        AlignWithMargins = True
        Left = 280
        Top = 18
        Width = 238
        Height = 223
        Align = alRight
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Population'
        TabOrder = 2
        object lvPopulation: TListView
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 228
          Height = 200
          Align = alClient
          Columns = <
            item
              Caption = 'Fitness'
            end
            item
              Caption = 'Efficiency'
              Width = 60
            end
            item
              Caption = 'RF/t'
            end
            item
              Caption = 'H/t'
              Width = -2
              WidthType = (
                -2)
            end>
          GridLines = True
          ReadOnly = True
          PopupMenu = pmPopulation
          SortType = stData
          TabOrder = 0
          ViewStyle = vsReport
          OnColumnClick = lvPopulationColumnClick
          OnCompare = lvPopulationCompare
        end
      end
      object seGeneration: TSpinEdit
        Left = 136
        Top = 18
        Width = 138
        Height = 22
        MaxValue = 1
        MinValue = 1
        ReadOnly = True
        TabOrder = 3
        Value = 1
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
      object edtNetHeatGenerationWorst: TEdit
        Left = 136
        Top = 146
        Width = 42
        Height = 21
        ReadOnly = True
        TabOrder = 13
        Text = '-'
      end
      object edtNetHeatGenerationAverage: TEdit
        Left = 184
        Top = 146
        Width = 42
        Height = 21
        ReadOnly = True
        TabOrder = 14
        Text = '-'
      end
      object edtNetHeatGenerationBest: TEdit
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
        FrameDelay = 40
        IndicatorSize = aisLarge
      end
    end
    object gbGraph: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 255
      Width = 523
      Height = 220
      Align = alClient
      Caption = 'Statistics'
      TabOrder = 1
      object pbGraph: TPaintBox
        AlignWithMargins = True
        Left = 5
        Top = 18
        Width = 513
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
    Left = 648
    Top = 80
    object actPreview: TAction
      Category = 'View'
      Caption = '3D Preview'
      OnExecute = actPreviewExecute
      OnUpdate = actPreviewUpdate
    end
    object actSingleStep: TAction
      Category = 'Evolution'
      Caption = 'Single Step'
      OnExecute = actSingleStepExecute
      OnUpdate = actSingleStepUpdate
    end
    object actNew: TAction
      Category = 'File'
      Caption = 'New'
      OnExecute = actNewExecute
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
      Caption = 'Start'
      OnExecute = actStartStopExecute
      OnUpdate = actStartStopUpdate
    end
  end
  object mmMain: TMainMenu
    Left = 656
    Top = 144
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
    Left = 374
    Top = 119
    object Inspect1: TMenuItem
      Caption = 'Inspect'
    end
    object InstactAll1: TMenuItem
      Caption = 'Inspect All'
    end
  end
end
