object frmSettings: TfrmSettings
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Settings'
  ClientHeight = 291
  ClientWidth = 484
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object gbReactorBlocks: TGroupBox
    AlignWithMargins = True
    Left = 290
    Top = 3
    Width = 191
    Height = 285
    Align = alClient
    Caption = 'Reactor Blocks'
    TabOrder = 0
    object clbReactorBlocks: TCheckListBox
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 181
      Height = 262
      OnClickCheck = clbReactorBlocksClickCheck
      Align = alClient
      ItemHeight = 13
      PopupMenu = pmReactorBlocks
      TabOrder = 0
    end
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 287
    Height = 291
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      287
      291)
    object btnGenerate: TButton
      Left = 137
      Top = 258
      Width = 138
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Generate'
      ModalResult = 1
      TabOrder = 2
    end
    object gbReactor: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 281
      Height = 104
      Align = alTop
      Caption = 'Reactor'
      TabOrder = 0
      object lbReactorSize: TLabel
        Left = 12
        Top = 21
        Width = 64
        Height = 13
        Caption = 'Reactor Size:'
      end
      object lbFuelBasePower: TLabel
        Left = 12
        Top = 49
        Width = 83
        Height = 13
        Caption = 'Fuel Base Power:'
      end
      object lbFuelBaseHeat: TLabel
        Left = 12
        Top = 76
        Width = 76
        Height = 13
        Caption = 'Fuel Base Heat:'
      end
      object seReactorX: TSpinEdit
        Left = 134
        Top = 18
        Width = 42
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 0
      end
      object seReactorY: TSpinEdit
        Left = 182
        Top = 18
        Width = 42
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 1
        Value = 0
      end
      object seReactorZ: TSpinEdit
        Left = 230
        Top = 18
        Width = 42
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 2
        Value = 0
      end
      object edtFuelBasePower: TEdit
        Left = 134
        Top = 46
        Width = 138
        Height = 21
        TabOrder = 3
        Text = '0'
      end
      object edtFuelBaseHeat: TEdit
        Left = 134
        Top = 73
        Width = 138
        Height = 21
        TabOrder = 4
        Text = '0'
      end
    end
    object gbEvolution: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 113
      Width = 281
      Height = 139
      Align = alTop
      Caption = 'Evolution'
      TabOrder = 1
      object lbPopulationSize: TLabel
        Left = 12
        Top = 25
        Width = 76
        Height = 13
        Caption = 'Population Size:'
      end
      object lbGeneratorFunction: TLabel
        Left = 12
        Top = 53
        Width = 97
        Height = 13
        Caption = 'Generator Function:'
      end
      object lbMutationFunction: TLabel
        Left = 12
        Top = 80
        Width = 90
        Height = 13
        Caption = 'Mutation Function:'
      end
      object lbFitnessFunction: TLabel
        Left = 12
        Top = 107
        Width = 82
        Height = 13
        Caption = 'Fitness Function:'
      end
      object sePopulationSize: TSpinEdit
        Left = 134
        Top = 22
        Width = 138
        Height = 22
        MaxValue = 2147483647
        MinValue = 1
        TabOrder = 0
        Value = 20
      end
      object cbFitnessFunction: TComboBox
        Left = 134
        Top = 104
        Width = 106
        Height = 21
        Style = csDropDownList
        Sorted = True
        TabOrder = 5
      end
      object btnGeneratorFunctionSettings: TButton
        Left = 246
        Top = 50
        Width = 26
        Height = 21
        Action = actShowGeneratorSettings
        TabOrder = 6
      end
      object btnMutationFunctionSettings: TButton
        Left = 246
        Top = 77
        Width = 26
        Height = 21
        Action = actShowMutationSettings
        TabOrder = 4
      end
      object cbMutationFunction: TComboBox
        Left = 134
        Top = 77
        Width = 106
        Height = 21
        Style = csDropDownList
        Sorted = True
        TabOrder = 3
      end
      object cbGeneratorFunction: TComboBox
        Left = 134
        Top = 50
        Width = 106
        Height = 21
        Style = csDropDownList
        Sorted = True
        TabOrder = 1
        OnChange = cbGeneratorFunctionChange
      end
      object btnFitnessFunctionSettings: TButton
        Left = 246
        Top = 104
        Width = 26
        Height = 21
        Action = actShowFitnessSettings
        TabOrder = 2
      end
    end
  end
  object pmReactorBlocks: TPopupMenu
    Left = 336
    Top = 39
    object EnableAll1: TMenuItem
      Action = actEnableAllBlocks
    end
    object DisableAll1: TMenuItem
      Action = actDisableAllBlocks
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object EnableCoolers1: TMenuItem
      Action = actEnabelCoolers
    end
    object DisableCoolers1: TMenuItem
      Action = actDisableCoolers
    end
  end
  object alSettings: TActionList
    Left = 338
    Top = 91
    object actShowGeneratorSettings: TAction
      Category = 'Settings'
      Caption = '...'
      OnExecute = actShowGeneratorSettingsExecute
      OnUpdate = actShowGeneratorSettingsUpdate
    end
    object actShowMutationSettings: TAction
      Category = 'Settings'
      Caption = '...'
    end
    object actShowFitnessSettings: TAction
      Category = 'Settings'
      Caption = '...'
    end
    object actEnableAllBlocks: TAction
      Category = 'Reactor Blocks'
      Caption = 'Enable All'
      OnExecute = actEnableAllBlocksExecute
    end
    object actDisableAllBlocks: TAction
      Category = 'Reactor Blocks'
      Caption = 'Disable All'
      OnExecute = actDisableAllBlocksExecute
    end
    object actEnabelCoolers: TAction
      Category = 'Reactor Blocks'
      Caption = 'Enable Coolers'
      OnExecute = actEnabelCoolersExecute
    end
    object actDisableCoolers: TAction
      Category = 'Reactor Blocks'
      Caption = 'Disable Coolers'
      OnExecute = actDisableCoolersExecute
    end
  end
end
