object frmRecipes: TfrmRecipes
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Configure Machine-Array'
  ClientHeight = 323
  ClientWidth = 194
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
    Width = 188
    Height = 140
    Align = alTop
    Caption = 'Machine-Type'
    TabOrder = 0
    object pbCraftingMachine: TPaintBox
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 178
      Height = 36
      Align = alClient
      OnMouseDown = pbCraftingMachineMouseDown
      OnPaint = pbCraftingMachinePaint
      ExplicitHeight = 32
    end
    object pnlMachineArray: TPanel
      Left = 2
      Top = 57
      Width = 184
      Height = 81
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitTop = 64
      ExplicitWidth = 182
      DesignSize = (
        184
        81)
      object lbCount: TLabel
        Left = 8
        Top = 28
        Width = 33
        Height = 13
        Caption = 'Count:'
      end
      object lbPerformance: TLabel
        Left = 8
        Top = 56
        Width = 65
        Height = 13
        Caption = 'Performance:'
      end
      object lbPerformanceUnit: TLabel
        Left = 164
        Top = 56
        Width = 11
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '%'
        ExplicitLeft = 162
      end
      object lbMachineName: TLabel
        Left = 0
        Top = 0
        Width = 184
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
        Left = 96
        Top = 25
        Width = 83
        Height = 22
        Anchors = [akTop, akRight]
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 0
        ExplicitLeft = 94
      end
      object edtPerformance: TEdit
        Left = 96
        Top = 53
        Width = 64
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 1
        ExplicitLeft = 94
      end
    end
  end
  object gbRecipe: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 149
    Width = 188
    Height = 171
    Align = alClient
    Caption = 'Recipe'
    TabOrder = 1
    ExplicitLeft = 56
    ExplicitTop = 168
    ExplicitWidth = 185
    ExplicitHeight = 105
    object pbRecipe: TPaintBox
      AlignWithMargins = True
      Left = 5
      Top = 111
      Width = 178
      Height = 36
      Align = alClient
      OnMouseDown = pbRecipeMouseDown
      OnPaint = pbRecipePaint
      ExplicitLeft = 7
      ExplicitHeight = 37
    end
    object pbGroup: TPaintBox
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 178
      Height = 68
      Align = alTop
      OnMouseDown = pbGroupMouseDown
      OnPaint = pbGroupPaint
      ExplicitLeft = 3
    end
    object lbGroupName: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 92
      Width = 178
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
      Top = 153
      Width = 178
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
end
