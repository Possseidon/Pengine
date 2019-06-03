object frmRecipes: TfrmRecipes
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Configure Machine-Array'
  ClientHeight = 377
  ClientWidth = 266
  Color = clBtnFace
  Constraints.MinWidth = 200
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 13
  object gbCraftingMachineType: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 260
    Height = 172
    Align = alTop
    Caption = 'Machine-Type'
    TabOrder = 0
    object pbCraftingMachine: TPaintBox
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 250
      Height = 38
      Align = alTop
      OnMouseDown = pbCraftingMachineMouseDown
      OnPaint = pbCraftingMachinePaint
    end
    object pnlMachineArray: TPanel
      Left = 2
      Top = 59
      Width = 256
      Height = 111
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitTop = 57
      ExplicitHeight = 113
      DesignSize = (
        256
        111)
      object lbCount: TLabel
        Left = 6
        Top = 53
        Width = 33
        Height = 13
        Caption = 'Count:'
      end
      object lbPerformance: TLabel
        Left = 6
        Top = 81
        Width = 65
        Height = 13
        Caption = 'Performance:'
      end
      object lbPerformanceUnit: TLabel
        Left = 236
        Top = 81
        Width = 11
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '%'
      end
      object lbMachineName: TLabel
        Left = 0
        Top = 0
        Width = 256
        Height = 13
        Align = alTop
        Alignment = taCenter
        AutoSize = False
        Caption = '[machine-name]'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitLeft = 3
        ExplicitTop = 3
        ExplicitWidth = 176
      end
      object seCount: TSpinEdit
        Left = 166
        Top = 50
        Width = 83
        Height = 22
        Anchors = [akTop, akRight]
        MaxValue = 2147483647
        MinValue = 1
        TabOrder = 0
        Value = 1
        OnChange = seCountChange
      end
      object edtPerformance: TEdit
        Left = 166
        Top = 78
        Width = 64
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 1
        OnChange = edtPerformanceChange
      end
      object btnRemove: TButton
        Left = 6
        Top = 19
        Width = 83
        Height = 25
        Caption = 'Remove'
        TabOrder = 2
        OnClick = btnRemoveClick
      end
    end
  end
  object gbRecipe: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 181
    Width = 260
    Height = 193
    Align = alClient
    Caption = 'Recipe'
    TabOrder = 1
    object pbRecipe: TPaintBox
      AlignWithMargins = True
      Left = 5
      Top = 111
      Width = 250
      Height = 58
      Align = alClient
      OnMouseDown = pbRecipeMouseDown
      OnPaint = pbRecipePaint
      ExplicitLeft = 7
      ExplicitWidth = 178
      ExplicitHeight = 37
    end
    object pbGroup: TPaintBox
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 250
      Height = 68
      Align = alTop
      OnMouseDown = pbGroupMouseDown
      OnPaint = pbGroupPaint
      ExplicitLeft = 3
      ExplicitWidth = 178
    end
    object lbGroupName: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 92
      Width = 250
      Height = 13
      Align = alTop
      Alignment = taCenter
      AutoSize = False
      Caption = '[group-name]'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitLeft = 2
      ExplicitTop = 95
      ExplicitWidth = 182
    end
    object lbRecipeName: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 175
      Width = 250
      Height = 13
      Align = alBottom
      Alignment = taCenter
      AutoSize = False
      Caption = '[recipe-name]'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitLeft = 2
      ExplicitTop = 95
      ExplicitWidth = 182
    end
  end
  object tmrResize: TTimer
    Enabled = False
    Interval = 20
    OnTimer = tmrResizeTimer
    Left = 115
    Top = 123
  end
end
