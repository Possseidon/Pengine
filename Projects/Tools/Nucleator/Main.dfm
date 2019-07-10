object frmMain: TfrmMain
  Left = 0
  Top = 0
  AlphaBlend = True
  Caption = 'Nucleator'
  ClientHeight = 518
  ClientWidth = 800
  Color = clBtnFace
  Constraints.MinHeight = 576
  Constraints.MinWidth = 816
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object spltChart: TSplitter
    Left = 0
    Top = 245
    Width = 800
    Height = 5
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
    Beveled = True
    MinSize = 250
    ResizeStyle = rsUpdate
    ExplicitLeft = -8
  end
  object gbChart: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 253
    Width = 794
    Height = 262
    Align = alBottom
    Caption = 'Chart'
    ParentBackground = False
    TabOrder = 0
    object tcStatistics: TChart
      Left = 2
      Top = 15
      Width = 790
      Height = 245
      Legend.LegendStyle = lsSeries
      Legend.TopPos = 0
      Legend.Visible = False
      Title.Font.Name = 'Tahoma'
      Title.Text.Strings = (
        'Fitness')
      Title.Visible = False
      BottomAxis.LabelsFormat.Font.Name = 'Tahoma'
      BottomAxis.Title.Caption = 'Generation'
      BottomAxis.Title.Font.Name = 'Tahoma'
      BottomAxis.Title.Font.OutLine.Color = clDefault
      Chart3DPercent = 9
      LeftAxis.LabelsFormat.Font.Name = 'Tahoma'
      LeftAxis.Title.Caption = 'Fitness'
      LeftAxis.Title.Font.Name = 'Tahoma'
      Panning.MouseWheel = pmwNone
      View3D = False
      Zoom.Animated = True
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      DefaultCanvas = 'TGDIPlusCanvas'
      ColorPaletteIndex = 2
      object seMinValues: TFastLineSeries
        Legend.Text = 'Fitness'
        LegendTitle = 'Fitness'
        SeriesColor = 223
        Title = 'Min'
        LinePen.Color = 223
        XValues.Name = 'X'
        XValues.Order = loAscending
        YValues.Name = 'Y'
        YValues.Order = loNone
        Data = {
          001900000053B81E85EB768C40E5A59BC4A0D98A402D8716D94E548A40E37A14
          AE476C8B405E8FC2F5A8998940A89BC420B08188400AAC1C5A640C8740FA53E3
          A51B1F8540112DB29D6F54854078931804D6B98240D2F753E3258F83405B39B4
          C8763C84401C04560E2D77824070E7FBA97104854055B81E856BF485408A16D9
          CE771484401E2FDD2486E481405F8FC2F528178340B7C876BE1F7F8440A99BC4
          2030FF814030B29DEF273F83402A3108AC1CF7844091976E12835C824089EB51
          B81EA78440608FC2F528178340}
        Detail = {0000000000}
      end
      object seAvgValues: TFastLineSeries
        Legend.Text = 'Fitness'
        LegendTitle = 'Fitness'
        SeriesColor = clPurple
        Title = 'Avg'
        LinePen.Color = clPurple
        XValues.Name = 'X'
        XValues.Order = loAscending
        YValues.Name = 'Y'
        YValues.Order = loNone
        Data = {
          0019000000736891EDFCA59340C1CAA145F6A99440273108AC9C5095408D976E
          1243F79540560E2DB29D2895408195438BEC25964083C0CAA14593954091ED7C
          3FF5D194403F355EBA49F39440726891EDFCA593402CB29DEFA7989340E82631
          08ACF89240AA1C5A64FBE1934035894160A5FC93405639B4C8F6959440716891
          EDFCA59340FEFFFFFFFF2D93404C621058F9319440E8263108ACF892403E355E
          BA09B291406BE7FBA9B11C9240CF22DBF9FE55934006AC1C5AA42494408DC2F5
          289C64954002560E2DF2499540}
        Detail = {0000000000}
      end
      object seMaxValues: TFastLineSeries
        Legend.Text = 'Fitness'
        LegendTitle = 'Fitness'
        SeriesColor = clGreen
        Title = 'Max'
        LinePen.Color = clGreen
        XValues.Name = 'X'
        XValues.Order = loAscending
        YValues.Name = 'Y'
        YValues.Order = loNone
        Data = {
          0019000000A85CE15D7E498840AAFB00A4A6598740AB4885B155AE8640AB4885
          B155AE8640AF86C43DA6CE84402F5BEB8B84BD84402B1DACFF339D86402B1DAC
          FF339D86402B745E6377BF864028DA55481916884029D53E1D4FAF8740A5703D
          0AA7E48940A5703D0AA7E4894028DA554819168840A56B26DFDC7D8940A9A965
          6B2D9E8740A4755435714B8A40A2D634EF483B8B40A761F88848B08840A761F8
          8848B08840A60F5D50CFF4884028DA554819168840283108AC5C3888402983A3
          E4D5F38740A6C2D84220A08940}
        Detail = {0000000000}
      end
      object seAllValues: TPointSeries
        SeriesColor = 10485760
        Title = 'All'
        ClickableLine = False
        Pointer.HorizSize = 3
        Pointer.InflateMargins = False
        Pointer.Pen.Visible = False
        Pointer.Style = psCircle
        Pointer.VertSize = 3
        XValues.Name = 'X'
        XValues.Order = loAscending
        YValues.Name = 'Y'
        YValues.Order = loNone
        Data = {
          00190000003DDA3862C5679040BC7EC16E6C4E90401A096D39F7E28F40E45300
          8C471390409BCE4E06A72191406E8B321B68F790409660713843BC9040414816
          3029CD9040BDCB457CC75B8F400808E6E819808E403CDA3862C56790404A24D1
          CBF0979140124E0B5E3870904058B26323D0098E404FD6A887083F8D40C23923
          4A2BC18F40BB7EC16E6C4E9040B5A679C7B1C38E40A913D04438C68D4010E4A0
          84E14A8F4065FCFB8CFB398F409D8026C2BEC88C404DD6A887083F8D408F368E
          5893988B4012F758FAA84B8840}
        Detail = {0000000000}
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 800
    Height = 245
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
    object gbEvolution: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 523
      Height = 239
      Align = alLeft
      Caption = 'Evolution'
      ParentBackground = False
      TabOrder = 0
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
        Top = 173
        Width = 138
        Height = 25
        Action = actSingleStep
        TabOrder = 0
      end
      object btnStartStop: TButton
        Left = 136
        Top = 204
        Width = 138
        Height = 25
        Action = actStartStop
        TabOrder = 1
      end
      object gbPopulation: TGroupBox
        AlignWithMargins = True
        Left = 280
        Top = 18
        Width = 238
        Height = 216
        Align = alRight
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Population'
        ParentBackground = False
        TabOrder = 2
        object lvPopulation: TListView
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 228
          Height = 193
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
          FullDrag = True
          ReadOnly = True
          RowSelect = True
          SortType = stData
          TabOrder = 0
          ViewStyle = vsReport
          OnColumnClick = lvPopulationColumnClick
          OnCompare = lvPopulationCompare
          OnSelectItem = lvPopulationSelectItem
        end
      end
      object seGeneration: TSpinEdit
        Left = 136
        Top = 18
        Width = 138
        Height = 22
        MaxValue = 1
        MinValue = 1
        TabOrder = 3
        Value = 1
        OnChange = seGenerationChange
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
    end
    object gb3DPreview: TGroupBox
      Left = 529
      Top = 0
      Width = 271
      Height = 245
      Align = alClient
      Caption = '3D Preview'
      ParentBackground = False
      TabOrder = 1
      inline frmPreview: TfrmPreview
        AlignWithMargins = True
        Left = 5
        Top = 18
        Width = 261
        Height = 222
        Align = alClient
        Color = clBlack
        ParentBackground = False
        ParentColor = False
        TabOrder = 0
        ExplicitLeft = 5
        ExplicitTop = 18
        ExplicitWidth = 261
        ExplicitHeight = 222
      end
    end
  end
  object alActions: TActionList
    Left = 304
    Top = 144
    object actSingleStep: TAction
      Category = 'Evolution'
      Caption = 'Single Step'
      OnExecute = actSingleStepExecute
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
    end
  end
  object mmMain: TMainMenu
    Left = 360
    Top = 80
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
  object aeAppEvents: TApplicationEvents
    OnIdle = aeAppEventsIdle
    Left = 304
    Top = 80
  end
end
