object frmEditorRecipes: TfrmEditorRecipes
  Left = 0
  Top = 0
  Width = 542
  Height = 464
  TabOrder = 0
  object Label1: TLabel
    Left = 132
    Top = 6
    Width = 64
    Height = 13
    Caption = 'Recipe-Type:'
  end
  object Label2: TLabel
    Left = 132
    Top = 33
    Width = 33
    Height = 13
    Caption = 'Group:'
  end
  object pbRecipe: TPaintBox
    Left = 0
    Top = 57
    Width = 542
    Height = 407
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnPaint = pbRecipePaint
  end
  object cbRecipeType: TComboBox
    Left = 227
    Top = 3
    Width = 163
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = cbRecipeTypeChange
  end
  object edtGroup: TEdit
    Left = 227
    Top = 30
    Width = 163
    Height = 21
    TabOrder = 1
  end
end
