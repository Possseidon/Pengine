object frmMain: TfrmMain
  Left = 0
  Top = 0
  AlphaBlend = True
  Caption = 'Nucleator'
  ClientHeight = 491
  ClientWidth = 630
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
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object spltChart: TSplitter
    Left = 0
    Top = 218
    Width = 630
    Height = 5
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
    Beveled = True
    MinSize = 250
    ResizeStyle = rsUpdate
    ExplicitLeft = -8
    ExplicitTop = 245
    ExplicitWidth = 800
  end
  object gbChart: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 226
    Width = 624
    Height = 262
    Align = alBottom
    Caption = 'Chart'
    ParentBackground = False
    TabOrder = 0
    object tcStatistics: TChart
      Left = 2
      Top = 15
      Width = 620
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
      object seBestValues: TFastLineSeries
        Legend.Text = 'Fitness'
        LegendTitle = 'Fitness'
        SeriesColor = clGreen
        Title = 'Best'
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
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 630
    Height = 218
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
    object gbEvolution: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 390
      Height = 212
      Align = alLeft
      Caption = 'Evolution'
      ParentBackground = False
      TabOrder = 0
      DesignSize = (
        390
        212)
      object lbGeneration: TLabel
        Left = 10
        Top = 20
        Width = 57
        Height = 13
        Caption = 'Generation:'
      end
      object btnSingleStep: TButton
        Left = 280
        Top = 15
        Width = 105
        Height = 25
        Action = actSingleStep
        Anchors = [akTop, akRight]
        TabOrder = 0
      end
      object btnStartStop: TButton
        Left = 169
        Top = 15
        Width = 105
        Height = 25
        Action = actStartStop
        Anchors = [akTop, akRight]
        TabOrder = 1
      end
      object lvBreakthroughs: TListView
        AlignWithMargins = True
        Left = 5
        Top = 46
        Width = 380
        Height = 161
        Align = alBottom
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Generation'
            Width = -2
            WidthType = (
              -2)
          end
          item
            Caption = 'Fitness'
            Width = -2
            WidthType = (
              -2)
          end
          item
            Caption = 'Efficiency'
            Width = -2
            WidthType = (
              -2)
          end
          item
            Caption = 'RF/t'
          end
          item
            Caption = 'H/t'
          end
          item
            Caption = 'Cell Count'
            Width = -2
            WidthType = (
              -2)
          end>
        FullDrag = True
        ReadOnly = True
        RowSelect = True
        SortType = stData
        TabOrder = 2
        ViewStyle = vsReport
        OnColumnClick = lvBreakthroughsColumnClick
        OnCompare = lvBreakthroughsCompare
        OnSelectItem = lvBreakthroughsSelectItem
      end
      object edtGeneration: TEdit
        Left = 88
        Top = 17
        Width = 65
        Height = 21
        Alignment = taRightJustify
        ReadOnly = True
        TabOrder = 3
      end
    end
    object gb3DPreview: TGroupBox
      Left = 396
      Top = 0
      Width = 234
      Height = 218
      Align = alClient
      Caption = '3D Preview'
      ParentBackground = False
      TabOrder = 1
      inline frmPreview: TfrmPreview
        AlignWithMargins = True
        Left = 5
        Top = 18
        Width = 224
        Height = 195
        Align = alClient
        Color = clBlack
        ParentBackground = False
        ParentColor = False
        TabOrder = 0
        ExplicitLeft = 5
        ExplicitTop = 18
        ExplicitWidth = 224
        ExplicitHeight = 195
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
