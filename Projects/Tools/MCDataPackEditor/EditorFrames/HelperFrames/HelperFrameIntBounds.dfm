object frmHelperIntBounds: TfrmHelperIntBounds
  Left = 0
  Top = 0
  Width = 221
  Height = 22
  TabOrder = 0
  DesignSize = (
    221
    22)
  object lbName: TLabel
    Left = 0
    Top = 3
    Width = 27
    Height = 13
    Caption = 'Name'
  end
  object cbRanged: TCheckBox
    Left = 119
    Top = 2
    Width = 46
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'up to'
    TabOrder = 0
    OnClick = cbRangedClick
  end
  object seMin: TSpinEdit
    Left = 64
    Top = 0
    Width = 49
    Height = 22
    Anchors = [akTop, akRight]
    MaxValue = 0
    MinValue = 0
    TabOrder = 1
    Value = 0
    OnChange = seMinChange
  end
  object seMax: TSpinEdit
    Left = 171
    Top = 0
    Width = 49
    Height = 22
    Anchors = [akTop, akRight]
    MaxValue = 0
    MinValue = 0
    TabOrder = 2
    Value = 0
    Visible = False
    OnChange = seMaxChange
  end
end
