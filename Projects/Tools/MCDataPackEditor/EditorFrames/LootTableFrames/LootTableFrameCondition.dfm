object frmCondition: TfrmCondition
  Left = 0
  Top = 0
  Width = 268
  Height = 58
  TabOrder = 0
  object gbMain: TGroupBox
    Left = 0
    Top = 0
    Width = 268
    Height = 58
    Align = alClient
    Caption = 'Condition'
    TabOrder = 0
    object Panel1: TPanel
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 258
      Height = 35
      Align = alTop
      BevelKind = bkFlat
      BevelOuter = bvNone
      TabOrder = 0
      object btnMoveLeft: TButton
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 25
        Height = 25
        Align = alLeft
        Caption = '/\'
        TabOrder = 0
      end
      object btnMoveRight: TButton
        AlignWithMargins = True
        Left = 226
        Top = 3
        Width = 25
        Height = 25
        Align = alRight
        Caption = '\/'
        TabOrder = 1
      end
      object btnRemove: TButton
        AlignWithMargins = True
        Left = 34
        Top = 3
        Width = 186
        Height = 25
        Align = alClient
        Caption = 'Remove'
        TabOrder = 2
      end
    end
  end
end
