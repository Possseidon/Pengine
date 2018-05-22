object frmHelperBounds: TfrmHelperBounds
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
  object edtMin: TEdit
    Left = 64
    Top = 0
    Width = 49
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 1
    Text = '0'
    OnChange = edtMinChange
  end
  object edtMax: TEdit
    Left = 171
    Top = 0
    Width = 49
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 2
    Text = '0'
    Visible = False
    OnChange = edtMaxChange
  end
end
