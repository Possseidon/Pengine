object frmHighlighterAttributes: TfrmHighlighterAttributes
  Left = 0
  Top = 0
  Width = 484
  Height = 23
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  DesignSize = (
    484
    23)
  object lbTitle: TLabel
    Left = 3
    Top = 3
    Width = 20
    Height = 13
    Caption = 'Title'
  end
  object cbColor: TColorBox
    Left = 125
    Top = 0
    Width = 156
    Height = 22
    Selected = clDefault
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = cbColorChange
  end
  object cbBold: TCheckBox
    Left = 301
    Top = 2
    Width = 50
    Height = 17
    AllowGrayed = True
    Anchors = [akTop, akRight]
    Caption = 'Bold'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    State = cbGrayed
    TabOrder = 1
    OnClick = cbBoldClick
    ExplicitLeft = 258
  end
  object cbItalic: TCheckBox
    Left = 357
    Top = 2
    Width = 51
    Height = 17
    AllowGrayed = True
    Anchors = [akTop, akRight]
    Caption = 'Italic'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsItalic]
    ParentFont = False
    State = cbGrayed
    TabOrder = 2
    OnClick = cbItalicClick
    ExplicitLeft = 314
  end
  object cbUnderline: TCheckBox
    Left = 414
    Top = 3
    Width = 67
    Height = 17
    AllowGrayed = True
    Anchors = [akTop, akRight]
    Caption = 'Underline'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    State = cbGrayed
    TabOrder = 3
    OnClick = cbUnderlineClick
    ExplicitLeft = 371
  end
end
