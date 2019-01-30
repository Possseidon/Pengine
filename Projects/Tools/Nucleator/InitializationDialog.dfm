object frmInitialization: TfrmInitialization
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Initialization'
  ClientHeight = 263
  ClientWidth = 484
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object gbReactorBlocks: TGroupBox
    AlignWithMargins = True
    Left = 290
    Top = 3
    Width = 191
    Height = 257
    Align = alClient
    Caption = 'Reactor Blocks'
    TabOrder = 0
    ExplicitLeft = 278
    ExplicitWidth = 157
    ExplicitHeight = 191
    object clbReactorBlocks: TCheckListBox
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 181
      Height = 234
      Align = alClient
      ItemHeight = 13
      PopupMenu = pmReactorBlocks
      TabOrder = 0
      ExplicitWidth = 130
      ExplicitHeight = 158
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 287
    Height = 263
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 56
    ExplicitTop = -24
    ExplicitWidth = 291
    DesignSize = (
      287
      263)
    object Button1: TButton
      Left = 137
      Top = 230
      Width = 138
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Generate'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object GroupBox1: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 281
      Height = 104
      Align = alTop
      Caption = 'Reactor'
      TabOrder = 1
      object Label1: TLabel
        Left = 12
        Top = 21
        Width = 64
        Height = 13
        Caption = 'Reactor Size:'
      end
      object Label4: TLabel
        Left = 12
        Top = 49
        Width = 83
        Height = 13
        Caption = 'Fuel Base Power:'
      end
      object Label6: TLabel
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
    object GroupBox2: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 113
      Width = 281
      Height = 107
      Align = alTop
      Caption = 'Evolution'
      TabOrder = 2
      object Label2: TLabel
        Left = 12
        Top = 25
        Width = 54
        Height = 13
        Caption = 'Population:'
      end
      object Label3: TLabel
        Left = 12
        Top = 53
        Width = 82
        Height = 13
        Caption = 'Fitness Function:'
      end
      object Label5: TLabel
        Left = 12
        Top = 80
        Width = 90
        Height = 13
        Caption = 'Mutation Function:'
      end
      object sePopulation: TSpinEdit
        Left = 134
        Top = 22
        Width = 138
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 0
      end
      object cbFitnessFunction: TComboBox
        Left = 134
        Top = 50
        Width = 106
        Height = 21
        Style = csDropDownList
        Enabled = False
        TabOrder = 1
      end
      object btnFitnessFunctionSettings: TButton
        Left = 246
        Top = 50
        Width = 26
        Height = 21
        Caption = '...'
        Enabled = False
        TabOrder = 2
      end
      object btnMutationFunctionSettings: TButton
        Left = 246
        Top = 77
        Width = 26
        Height = 21
        Caption = '...'
        Enabled = False
        TabOrder = 3
      end
      object cbMutationFunction: TComboBox
        Left = 134
        Top = 77
        Width = 106
        Height = 21
        Style = csDropDownList
        Enabled = False
        TabOrder = 4
      end
    end
  end
  object pmReactorBlocks: TPopupMenu
    Left = 336
    Top = 39
    object EnableAll1: TMenuItem
      Caption = 'Enable All'
    end
    object DisableAll1: TMenuItem
      Caption = 'Disable All'
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object EnableCoolers1: TMenuItem
      Caption = 'Enable Coolers'
    end
    object DisableCoolers1: TMenuItem
      Caption = 'Disable Coolers'
    end
  end
end
