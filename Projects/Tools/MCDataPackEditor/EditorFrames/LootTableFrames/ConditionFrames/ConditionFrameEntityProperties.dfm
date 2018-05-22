object frmConditionEntityProperties: TfrmConditionEntityProperties
  Left = 0
  Top = 0
  Width = 238
  Height = 75
  TabOrder = 0
  DesignSize = (
    238
    75)
  object lbTarget: TLabel
    Left = 3
    Top = 6
    Width = 67
    Height = 13
    Caption = 'Target Entity:'
  end
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 33
    Width = 232
    Height = 39
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Properties'
    TabOrder = 0
    ExplicitLeft = 0
    ExplicitTop = 30
    ExplicitWidth = 382
    ExplicitHeight = 185
    object cbOnFire: TCheckBox
      Left = 16
      Top = 18
      Width = 97
      Height = 17
      AllowGrayed = True
      Caption = 'Entity is on Fire'
      State = cbGrayed
      TabOrder = 0
    end
  end
  object cbTarget: TComboBox
    Left = 76
    Top = 3
    Width = 159
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
end
