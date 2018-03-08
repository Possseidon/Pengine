object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'MCDataPackEditor'
  ClientHeight = 695
  ClientWidth = 1090
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 185
    Top = 0
    Height = 695
    ResizeStyle = rsUpdate
    ExplicitLeft = 193
    ExplicitTop = 8
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 695
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object tvFiles: TTreeView
      AlignWithMargins = True
      Left = 3
      Top = 35
      Width = 179
      Height = 657
      Align = alClient
      Indent = 19
      TabOrder = 0
    end
    object ToolBar1: TToolBar
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 179
      Height = 26
      AutoSize = True
      Caption = 'ToolBar1'
      EdgeBorders = [ebLeft, ebTop, ebRight, ebBottom]
      TabOrder = 1
      object ToolButton1: TToolButton
        Left = 0
        Top = 0
        Caption = 'ToolButton1'
        ImageIndex = 0
      end
      object ToolButton2: TToolButton
        Left = 23
        Top = 0
        Width = 8
        Caption = 'ToolButton2'
        ImageIndex = 1
        Style = tbsSeparator
      end
      object ToolButton3: TToolButton
        Left = 31
        Top = 0
        Caption = 'ToolButton3'
        ImageIndex = 1
      end
      object ToolButton4: TToolButton
        Left = 54
        Top = 0
        Caption = 'ToolButton4'
        ImageIndex = 2
      end
      object ToolButton5: TToolButton
        Left = 77
        Top = 0
        Caption = 'ToolButton5'
        ImageIndex = 3
      end
      object ToolButton6: TToolButton
        Left = 100
        Top = 0
        Width = 8
        Caption = 'ToolButton6'
        ImageIndex = 4
        Style = tbsSeparator
      end
      object ToolButton7: TToolButton
        Left = 108
        Top = 0
        Caption = 'ToolButton7'
        ImageIndex = 4
      end
    end
  end
  object tcFiles: TTabControl
    AlignWithMargins = True
    Left = 191
    Top = 3
    Width = 896
    Height = 689
    Align = alClient
    TabOrder = 1
    Tabs.Strings = (
      'Unnamed')
    TabIndex = 0
    inline frmMCFunction1: TfrmSynEdit
      Left = 4
      Top = 24
      Width = 888
      Height = 661
      Align = alClient
      TabOrder = 0
      ExplicitLeft = 4
      ExplicitTop = 24
      ExplicitWidth = 888
      ExplicitHeight = 661
      inherited synEdit: TSynEdit
        Width = 882
        Height = 655
        ExplicitWidth = 882
        ExplicitHeight = 655
      end
    end
  end
  object alActions: TActionList
    Left = 16
    Top = 104
  end
  object mmMain: TMainMenu
    Left = 56
    Top = 88
    object File1: TMenuItem
      Caption = 'File'
      object NewDatapack1: TMenuItem
        Caption = 'New'
        object Datapack1: TMenuItem
          Caption = 'Datapack...'
        end
        object N2: TMenuItem
          Caption = '-'
        end
        object Namespace1: TMenuItem
          Caption = 'Namespace'
        end
        object N1: TMenuItem
          Caption = 'Function'
        end
        object advancement1: TMenuItem
          Caption = 'Advancement'
        end
        object LootTable1: TMenuItem
          Caption = 'Loot Table'
        end
        object Recipe1: TMenuItem
          Caption = 'Recipe'
        end
        object Structure1: TMenuItem
          Caption = 'Structure'
        end
        object ag1: TMenuItem
          Caption = 'Tag'
        end
      end
      object OpenDatapack1: TMenuItem
        Caption = 'Open..'
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
      end
    end
  end
end
