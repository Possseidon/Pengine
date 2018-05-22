object frmPool: TfrmPool
  Left = 0
  Top = 0
  Width = 268
  Height = 482
  TabOrder = 0
  object gbPool: TGroupBox
    Left = 0
    Top = 0
    Width = 268
    Height = 482
    Align = alClient
    Caption = 'Pool'
    TabOrder = 0
    inline frmRolls: TfrmHelperIntBounds
      AlignWithMargins = True
      Left = 5
      Top = 43
      Width = 258
      Height = 22
      Hint = 'How often this Pool gets evaluated.'
      Align = alTop
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      ExplicitLeft = 5
      ExplicitTop = 43
      ExplicitWidth = 258
      inherited lbName: TLabel
        Width = 26
        Caption = 'Rolls:'
        ExplicitWidth = 26
      end
      inherited cbRanged: TCheckBox
        Left = 156
        ExplicitLeft = 156
      end
      inherited seMin: TSpinEdit
        Left = 101
        ExplicitLeft = 101
      end
      inherited seMax: TSpinEdit
        Left = 208
        ExplicitLeft = 208
      end
    end
    inline frmBonusRolls: TfrmHelperBounds
      AlignWithMargins = True
      Left = 5
      Top = 71
      Width = 258
      Height = 22
      Hint = 'Amount of Bonus Rolls for each Level of Looting.'
      Align = alTop
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      ExplicitLeft = 5
      ExplicitTop = 71
      ExplicitWidth = 258
      inherited lbName: TLabel
        Width = 58
        Caption = 'Bonus Rolls:'
        ExplicitWidth = 58
      end
      inherited cbRanged: TCheckBox
        Left = 156
        ExplicitLeft = 156
      end
      inherited edtMin: TEdit
        Left = 101
        ExplicitLeft = 101
      end
      inherited edtMax: TEdit
        Left = 208
        ExplicitLeft = 208
      end
    end
    object Panel7: TPanel
      Left = 2
      Top = 96
      Width = 264
      Height = 31
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object Button2: TButton
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 78
        Height = 25
        Align = alLeft
        Caption = 'Add Entry'
        TabOrder = 0
      end
      object cbEntries: TComboBox
        AlignWithMargins = True
        Left = 87
        Top = 5
        Width = 174
        Height = 21
        Margins.Top = 5
        Margins.Bottom = 5
        Align = alClient
        Style = csDropDownList
        TabOrder = 1
      end
    end
    object PageControl1: TPageControl
      AlignWithMargins = True
      Left = 5
      Top = 130
      Width = 258
      Height = 347
      Align = alClient
      TabOrder = 1
    end
    object Panel1: TPanel
      Left = 2
      Top = 15
      Width = 264
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 4
      object Button1: TButton
        Left = 135
        Top = -1
        Width = 91
        Height = 25
        Caption = 'Conditions...'
        TabOrder = 0
      end
      object Button3: TButton
        Left = 48
        Top = 0
        Width = 81
        Height = 25
        Caption = 'Remove'
        TabOrder = 1
      end
    end
  end
end
