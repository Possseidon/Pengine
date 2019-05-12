object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'MCDataPackEditor'
  ClientHeight = 579
  ClientWidth = 1076
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Menu = mmMain
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object splView: TSplitter
    Left = 237
    Top = 0
    Height = 560
    AutoSnap = False
    MinSize = 237
    ResizeStyle = rsUpdate
    ExplicitLeft = 209
    ExplicitHeight = 506
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 237
    Height = 560
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object tbView: TToolBar
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 231
      Height = 30
      AutoSize = True
      BorderWidth = 1
      EdgeBorders = [ebLeft, ebTop, ebRight, ebBottom]
      Images = ilIcons
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      object ToolButton2: TToolButton
        Left = 0
        Top = 0
        Action = actRefresh
      end
      object ToolButton3: TToolButton
        Left = 23
        Top = 0
        Width = 8
        Caption = 'ToolButton3'
        ImageIndex = 1
        Style = tbsSeparator
      end
      object tbNewDirectory: TToolButton
        Left = 31
        Top = 0
        Action = actNewDirectory
      end
      object tbNewNamespace: TToolButton
        Left = 54
        Top = 0
        Action = actNewNamespace
      end
      object ToolButton1: TToolButton
        Left = 77
        Top = 0
        Width = 8
        ImageIndex = 0
        Style = tbsSeparator
      end
    end
    object tvDatapacks: TTreeView
      AlignWithMargins = True
      Left = 3
      Top = 39
      Width = 231
      Height = 518
      Align = alClient
      HideSelection = False
      Images = ilIcons
      Indent = 19
      MultiSelect = True
      MultiSelectStyle = [msControlSelect, msShiftSelect]
      PopupMenu = pmView
      TabOrder = 1
      OnContextPopup = tvDatapacksContextPopup
      OnMouseDown = tvDatapacksMouseDown
      OnMouseUp = tvDatapacksMouseUp
    end
  end
  object sbMain: TStatusBar
    Left = 0
    Top = 560
    Width = 1076
    Height = 19
    Panels = <>
  end
  object pcTabs: TPageControl
    AlignWithMargins = True
    Left = 243
    Top = 3
    Width = 830
    Height = 554
    Align = alClient
    Images = ilIcons
    ParentShowHint = False
    PopupMenu = pmTabs
    ShowHint = False
    TabOrder = 2
    OnDragDrop = pcTabsDragDrop
    OnDragOver = pcTabsDragOver
    OnMouseDown = pcTabsMouseDown
    OnMouseEnter = pcTabsMouseEnter
    OnMouseLeave = pcTabsMouseLeave
    OnMouseMove = pcTabsMouseMove
    OnMouseUp = pcTabsMouseUp
    OnStartDrag = pcTabsStartDrag
  end
  object alActions: TActionList
    Images = ilIcons
    Left = 48
    Top = 48
    object actNewDatapack: TAction
      Category = 'File'
      Caption = 'New Datapack...'
      ShortCut = 16462
      OnExecute = actNewDatapackExecute
    end
    object actOpenDatapack: TAction
      Category = 'File'
      Caption = 'Open Datapack...'
      ShortCut = 16463
      OnExecute = actOpenDatapackExecute
    end
    object actSave: TAction
      Category = 'File'
      Caption = 'Save'
      ShortCut = 16467
      OnExecute = actSaveExecute
      OnUpdate = actSaveUpdate
    end
    object actSaveAll: TAction
      Category = 'File'
      Caption = 'Save all'
      ShortCut = 24659
      OnExecute = actSaveAllExecute
      OnUpdate = actSaveAllUpdate
    end
    object actRefresh: TAction
      Category = 'Datapack'
      Caption = 'Refresh'
      ImageIndex = 9
      ShortCut = 116
      OnExecute = actRefreshExecute
      OnUpdate = actRefreshUpdate
    end
    object actNewNamespace: TAction
      Category = 'Datapack'
      Caption = 'Namespace'
      ImageIndex = 6
      ShortCut = 16461
      OnExecute = actNewNamespaceExecute
      OnUpdate = actNewNamespaceUpdate
    end
    object actCopyPath: TAction
      Category = 'View'
      Caption = 'Copy Filepath'
      ShortCut = 24643
      OnExecute = actCopyPathExecute
      OnUpdate = actCopyPathUpdate
    end
    object actCopyName: TAction
      Category = 'View'
      Caption = 'Copy Name'
      ShortCut = 16451
      OnExecute = actCopyNameExecute
      OnUpdate = actCopyNameUpdate
    end
    object actExit: TAction
      Category = 'File'
      Caption = 'Exit'
      ShortCut = 32883
      OnExecute = actExitExecute
    end
    object actCollapseNamespaces: TAction
      Category = 'View'
      Caption = 'Collapse Namespaces'
    end
    object actCollapseTypes: TAction
      Category = 'View'
      Caption = 'Collapse Types'
    end
    object actCollapseAll: TAction
      Category = 'View'
      Caption = 'Collapse All'
      OnExecute = actCollapseAllExecute
    end
    object actExpandAll: TAction
      Category = 'View'
      Caption = 'Expand All'
      OnExecute = actExpandAllExecute
    end
    object actExpandNamespaces: TAction
      Category = 'View'
      Caption = 'Expand Namespaces'
    end
    object actExpandTypes: TAction
      Category = 'View'
      Caption = 'Expand Types'
    end
    object actEdit: TAction
      Category = 'View'
      Caption = 'Edit'
      OnExecute = actEditExecute
    end
    object actNewDirectory: TAction
      Category = 'Datapack'
      Caption = 'Directory'
      ImageIndex = 8
      ShortCut = 24654
      OnExecute = actNewDirectoryExecute
      OnUpdate = actNewDirectoryUpdate
    end
    object actOpenInExplorer: TAction
      Category = 'View'
      Caption = 'Show in Explorer...'
      ShortCut = 16453
      OnExecute = actOpenInExplorerExecute
      OnUpdate = actOpenInExplorerUpdate
    end
    object actRename: TAction
      Category = 'View'
      Caption = 'Rename'
      ShortCut = 113
      OnExecute = actRenameExecute
    end
    object actDelete: TAction
      Category = 'View'
      Caption = 'Delete'
      ShortCut = 46
      OnExecute = actDeleteExecute
      OnUpdate = actDeleteUpdate
    end
    object actFormatCurrent: TAction
      Category = 'Functions'
      Caption = 'Format Current File'
      ShortCut = 16452
      OnExecute = actFormatCurrentExecute
      OnUpdate = actFormatCurrentUpdate
    end
    object actFormatAll: TAction
      Category = 'Functions'
      Caption = 'Format Multiple...'
    end
    object actFunctionPreferences: TAction
      Category = 'Functions'
      Caption = 'Preferences...'
      OnExecute = actFunctionPreferencesExecute
    end
    object actCloseTab: TAction
      Category = 'Tabs'
      Caption = 'Close tab'
      ShortCut = 16499
      OnExecute = actCloseTabExecute
    end
    object actCloseAllOtherTabs: TAction
      Category = 'Tabs'
      Caption = 'Close all other tabs'
      ShortCut = 24691
      OnExecute = actCloseAllOtherTabsExecute
    end
    object actOpenVanilla: TAction
      Category = 'File'
      Caption = 'Open Vanilla'
    end
    object actCloseDatapack: TAction
      Category = 'View'
      Caption = 'Close Datapack'
      OnExecute = actCloseDatapackExecute
    end
  end
  object mmMain: TMainMenu
    Images = ilIcons
    Left = 48
    Top = 96
    object miFile: TMenuItem
      Caption = 'File'
      object Datapack1: TMenuItem
        Action = actNewDatapack
      end
      object miOpen: TMenuItem
        Action = actOpenDatapack
      end
      object OpenVanilla1: TMenuItem
        Action = actOpenVanilla
      end
      object RecentlyUsed1: TMenuItem
        Caption = 'Recently Used'
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object Save1: TMenuItem
        Action = actSave
      end
      object Saveall1: TMenuItem
        Action = actSaveAll
      end
      object miFileDiv1: TMenuItem
        Caption = '-'
      end
      object miExit: TMenuItem
        Action = actExit
      end
    end
    object Datapack2: TMenuItem
      Caption = 'Datapack'
      object Refresh1: TMenuItem
        Action = actRefresh
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object miAdd: TMenuItem
        Caption = 'Add'
        object Namespace1: TMenuItem
          Action = actNewNamespace
        end
        object Directory1: TMenuItem
          Action = actNewDirectory
        end
        object N2: TMenuItem
          Caption = '-'
        end
      end
    end
    object ools1: TMenuItem
      Caption = 'Tools'
      object Formatmcfunction1: TMenuItem
        Caption = 'Functions'
        object FormatcurrentFile1: TMenuItem
          Action = actFormatCurrent
        end
        object FormatAllFiles1: TMenuItem
          Action = actFormatAll
        end
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object Preferences1: TMenuItem
        Action = actFunctionPreferences
      end
    end
  end
  object dlgOpenDatapack: TOpenDialog
    DefaultExt = '.mcmeta'
    FileName = 'pack.mcmeta'
    Filter = 'Minecraft Datapack|*.mcmeta'
    Options = [ofHideReadOnly, ofNoChangeDir, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 48
    Top = 144
  end
  object ilIcons: TImageList
    ColorDepth = cd32Bit
    DrawingStyle = dsTransparent
    Left = 48
    Top = 192
    Bitmap = {
      494C01010B000D00040010001000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
      000000000000000000000000000000000000000000000000000047C7EBFF47C6
      EAFF47C6EAFF46C5EAFF46C4E9FF46C4E9FF45C3E8FF45C2E8FF45C2E7FF45C1
      E7FF44C1E6FF45C1E7FF3CA9CAEF00000000AA7231FF402B129F000000000000
      0000170F05605C3C15BFB27226FFB77425FFB77323FFB27020FF5C390EBF170E
      036000000000000000000000000000000000385D72FF385D72FF385D72FF3D61
      7BFF325368FF325368FF3D617BFF325368FF385D72FF385D72FF385D72FF3253
      68FF253C4BFF325368FF325368FF385D72FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000048C8EBFF8FE7
      FFFF8EE6FFFF8CE6FFFF8BE6FFFF89E5FFFF88E5FFFF86E4FFFF85E4FFFF83E4
      FFFF45C1E7FF83E4FFFF45C1E7FF00000000B77A32FFCF8931FF402A129F6D47
      1CCFC3802DFFE09331FFE89832FFE89832FFE89832FFE89832FFE0922FFFC37B
      23FF6D420ECF0201002000000000000000006198BCFF073587FF062F76FF0427
      62FF1515BCFF101091FF8E5A30FF0C715DFF193040FF099BA8FF077D89FF1010
      91FF8E5A30FF794D29FF6A8E10FF6198BCFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000048C8ECFF91E7
      FFFF8FE7FFFF8EE6FFFF8CE6FFFF8BE6FFFF89E5FFFF88E5FFFF86E4FFFF85E4
      FFFF45C2E7FF85E4FFFF45C2E7FF00000000B77A32FFE89934FFCF8931FFD88E
      32FFE89934FFE09332FFCB852CFFC7812AFFC78129FFCB8329FFE09331FFE899
      34FFD88C2CFF955A13EF02010020000000005990B4FF073587FF062F76FF0427
      62FF1515BCFF101091FF8E5A30FF0C715DFF142935FF099BA8FF077D89FF1010
      91FF8E5A30FF794D29FF6A8E10FF5990B4FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000048C9ECFF92E7
      FFFF91E7FFFF8FE7FFFF8EE6FFFF8CE6FFFF8BE6FFFF89E5FFFF88E5FFFF86E4
      FFFF45C2E8FF86E4FFFF45C2E8FF00000000B77A33FFE89935FFE89935FFE899
      35FFCB862FFF83541EDF170F05600A0602400A060240170E04607F4D14DFCB83
      28FFE89935FFD88C2DFF6D400CCF000000006198BCFF073587FF062F76FF0427
      62FF1515BCFF101091FF8E5A30FF142935FF0C715DFFB5B5B5FF898989FF1010
      91FF8E5A30FF794D29FF6A8E10FF6198BCFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000048C9EDFF94E8
      FFFF92E7FFFF91E7FFFF8FE7FFFF8EE6FFFF8CE6FFFF8BE6FFFF89E5FFFF88E5
      FFFF45C3E8FF88E5FFFF45C3E8FF00000000B77A33FFE89A37FFE89A37FFE89A
      37FFCF8931FF40290E9F00000000000000000000000000000000000000104027
      099FCB8328FFE89A37FFC37A21FF170D0260385D72FF073587FF062F76FF0427
      62FF1515BCFF101091FF142935FF193040FF0C715DFFB5B5B5FF898989FF1010
      91FF8E5A30FF794D29FF6A8E10FF385D72FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000049CAEDFF96E8
      FFFF94E8FFFF92E7FFFF91E7FFFF8FE7FFFF8EE6FFFF8CE6FFFF8BE6FFFF89E5
      FFFF46C4E9FF81E1FCFF46C4E9FF00000000B77A33FFE89A38FFE89A38FFE89A
      38FFE89A38FFCF8931FF40290E9F000000000000000000000000000000000000
      00107F4B10DFE09333FFE09333FF5C3608BF6198BCFF101D26FF142935FF1429
      35FF1515BCFF101091FF172D3EFF172D3EFF142935FF099BA8FF077D89FF1221
      2BFF8E5A30FF794D29FF142935FF4C849FFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000049CBEEFF97E8
      FFFF96E8FFFF94E8FFFF92E7FFFF91E7FFFF8FE7FFFF8EE6FFFF8CE6FFFF8BE6
      FFFF57CCEFFF4EC8ECFF276E83BF00000000AE7532FFC78534FFC78533FFC784
      32FFC78431FFC7832FFFB27327FF40280D9F0000000000000000000000000000
      0000170E0360BB741FFFC77E25FFAA6411FF4C849FFF101D26FF101D26FF101D
      26FF101D26FF101D26FF101D26FF12212BFF12212BFF12212BFF101D26FF101D
      26FF8E5A30FF794D29FF12212BFF4C849FFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000049CBEEFF99E9
      FFFF97E8FFFF96E8FFFF94E8FFFF92E7FFFF91E7FFFF8FE7FFFF8EE6FFFF8CE6
      FFFF87E4FEFF46C4E9FF00000010000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000006198BCFF4C849FFF4C849FFF4C84
      9FFF4C849FFF6198BCFF5990B4FF5990B4FF4C849FFF4C849FFF4C849FFF385D
      72FF6198BCFF4C849FFF4C849FFF4C849FFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004ACCEFFF9AE9
      FFFF99E9FFFF97E8FFFF96E8FFFF94E8FFFF92E7FFFF91E7FFFF8FE7FFFF8EE6
      FFFF8CE6FFFF46C5EAFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000385D72FF385D72FF325368FF3253
      68FF325368FF325368FF325368FF385D72FF385D72FF385D72FF325368FF3253
      68FF325368FF325368FF385D72FF385D72FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004ACDEFFF9CEA
      FFFF9AE9FFFF99E9FFFF97E8FFFF96E8FFFF94E8FFFF92E7FFFF91E7FFFF8FE7
      FFFF8EE6FFFF47C6EAFF0000000000000000AA7232FFC78534FFBB7C30FF170F
      06600000000000000000000000000000000040280B9FAA6A1EFFC78129FFC780
      28FFC78027FFC77E26FFC77E25FFB76F19FF6198BCFF1515BCFF101091FF6A8E
      10FF8E5A30FF794D29FF108E74FF0C715DFF172D3EFF1515BCFF101091FF108E
      74FF0C715DFF6A8E10FF4F6A0BFF6198BCFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004ACDEFFF9DEA
      FFFF9CEAFFFF9AE9FFFF99E9FFFF97E8FFFF96E8FFFF94E8FFFF92E7FFFF91E7
      FFFF8FE7FFFF47C6EAFF00000000000000005C3F1BBFE09537FFE09536FF7F53
      21DF000000100000000000000000000000000000000040280B9FBF7925FFE89A
      38FFE89A38FFE89A38FFE89A38FFC77D23FF4C849FFF1515BCFF101091FF6A8E
      10FF8E5A30FF794D29FF484848FF3E3E3EFF193040FF1515BCFF101091FF108E
      74FF0C715DFF6A8E10FF4F6A0BFF4C849FFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004BCEF0FF9FEA
      FFFF9DEAFFFF9CEAFFFF9AE9FFFF99E9FFFF97E8FFFF96E8FFFF94E8FFFF92E7
      FFFF91E7FFFF47C7EBFF0000000000000000170F0660C38233FFE89A36FFCB86
      31FF402A109F000000100000000000000000000000000000000040270A9FBF78
      24FFE89A36FFE89A36FFE89A36FFC77D22FF6198BCFF1515BCFF101091FF6A8E
      10FF8E5A30FF794D29FF484848FF3E3E3EFF1515BCFF101091FF172D3EFF108E
      74FF0C715DFFB5B5B5FF898989FF6198BCFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004BCFF0FFA0EB
      FFFF9FEAFFFF9DEAFFFF9CEAFFFF9AE9FFFF99E9FFFF97E8FFFF96E8FFFF94E8
      FFFF92E7FFFF48C8EBFF0000000000000000000000006D491FCFD88F33FFE899
      35FFCB862FFF7F511DDF170F05600A0602400A060240170E04607F4E14DFCB83
      28FFE89935FFE89935FFE89935FFC77C21FF325368FFB5B5B5FF898989FF6A8E
      10FF8E5A30FF794D29FF108E74FF0C715DFF1515BCFF101091FF142935FF108E
      74FF0C715DFF6A8E10FF4F6A0BFF385D72FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004BCFF1FFA2EB
      FFFFA0EBFFFF9FEAFFFF9DEAFFFF9CEAFFFF9AE9FFFF99E9FFFF97E8FFFF96E8
      FFFF94E8FFFF48C8ECFF00000000000000000000000002010020956328EFD88E
      31FFE89934FFE09332FFCB842CFFC7812AFFC78029FFCB8329FFE09230FFE899
      34FFD88C2CFFC37A22FFE89934FFC77C21FF6198BCFF1515BCFF101091FF1221
      2BFF8E5A30FF794D29FF108E74FF0C715DFF142935FF142935FF142935FF108E
      74FF0C715DFF172D3EFF193040FF4C849FFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004BD0F1FFA3EB
      FFFFA2EBFFFFA0EBFFFF9FEAFFFF9DEAFFFF9CEAFFFF9AE9FFFF99E9FFFF97E8
      FFFF96E8FFFF48C9ECFF00000000000000000000000000000000020100206D47
      1CCFC3802DFFE09330FFE89832FFE89832FFE89832FFE89832FFE0922FFFC37B
      23FF6D420FCF4026079FBF761DFFC77C20FF6198BCFF12212BFF101D26FF101D
      26FF101D26FF101D26FF108E74FF0C715DFF12212BFF12212BFF101D26FF101D
      26FF101D26FF101D26FF12212BFF4C849FFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004CD1F2FF4BD0
      F1FF4BCFF1FF4BCFF0FF4BCEF0FF4ACDEFFF4ACDEFFF4ACCEFFF49CBEEFF49CB
      EEFF49CAEDFF2F849CCF00000000000000000000000000000000000000000000
      0000170F05605C3C15BFB27226FFB77425FFB77323FFB27020FF5C390EBF170E
      036000000000000000004026069FAA6411FF6198BCFF6198BCFF6198BCFF5990
      B4FF6198BCFF6198BCFF6198BCFF4C849FFF4C849FFF4C849FFF385D72FF6198
      BCFF6198BCFF6198BCFF6198BCFF4C849FFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000323232FF343534FF291C25FF291C
      25FF291C25FF291C25FF291C25FF291C25FF291C25FF291C25FF291C25FF291C
      25FF291C25FF291C25FF343534FF323232FF00000000000000000F2836FF0F28
      36FF0F2836FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000343534FF0F0F0FFF0F0F0FFF0F0F
      0FFF0F0F0FFF0F0F0FFF967796FFB493B4FFB493B4FF967796FF0F0F0FFF0F0F
      0FFF0F0F0FFF0F0F0FFF0F0F0FFF343534FF000000000F2836FF87BBD7FF6170
      79FF87BBD7FF0F2836FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000040F15FF040F15FF040F15FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000291C25FF0F0F0FFF291C25FF0F0F
      0FFF967796FF967796FF291C25FF0F0F0FFF0F0F0FFF291C25FF967796FF9677
      96FF0F0F0FFF291C25FF0F0F0FFF291C25FF0F2836FF87BBD7FF8CC7E6FF6170
      79FF8CC7E6FF87BBD7FF0F2836FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000040F15FF123D53FF0B2737FF0B2737FF040F15FF040F15FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000505050FF505050FF000000000000000000000000000000000000
      000000000000000000000000000000000000291C25FF0F0F0FFF0F0F0FFF9677
      96FFC2A0C2FFC2A0C2FF291C25FF967796FF967796FF291C25FFC2A0C2FFC2A0
      C2FF967796FF0F0F0FFF0F0F0FFF291C25FF0F2836FF8CC7E6FF617079FF8CC7
      E6FF87BBD7FF8CC7E6FF87BBD7FF0F2836FF0000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000040F
      15FF123D53FFD6D6D6FF999999FF999999FF0B2737FF0B2737FF040F15FF040F
      15FF000000000000000000000000000000000000000000000000000000000000
      0000505050FFD6D6D6FFD6D6D6FF505050FF505050FF00000000000000000000
      000000000000000000000000000000000000291C25FF0F0F0FFF967796FFC2A0
      C2FFD7C2D7FFD7B3D7FF967796FFD7C2D7FFD7C2D7FF967796FFD7B3D7FFD7C2
      D7FFC2A0C2FF967796FF0F0F0FFF291C25FF0F2836FF87BBD7FF8CC7E6FF87BB
      D7FF617079FF617079FF8CC7E6FF87BBD7FF0F2836FF00000000000000000000
      0000000000000000000000000000000000000000000000000000040F15FF123D
      53FFD6D6D6FFB7B7B7FFB7B7B7FFB7B7B7FF999999FFB7B7B7FF0B2737FF0B27
      37FF040F15FF040F15FF00000000000000000000000000000000000000005050
      50FFEAEAEAFFEAEAEAFFD6D6D6FFD6D6D6FFD6D6D6FF505050FF505050FF0000
      000000000000000000000000000000000000291C25FF0F0F0FFF967796FFC2A0
      C2FFD7B3D7FFD7C2D7FFD7C2D7FF291C25FF291C25FFD7C2D7FFD7C2D7FFD7B3
      D7FFC2A0C2FF967796FF776377FF291C25FF000000000F2836FF87BBD7FF6170
      79FF8CC7E6FF617079FF8CC7E6FF8CC7E6FF87BBD7FF0F2836FF000000000000
      00000000000000000000000000000000000000000000040F15FF123D53FFD6D6
      D6FFB7B7B7FF123D53FF0B2737FF0B2737FFB7B7B7FF999999FF999999FFB7B7
      B7FF0B2737FF0B2737FF040F15FF040F15FF0000000000000000505050FFD6D6
      D6FFD6D6D6FFD6D6D6FFEAEAEAFFD6D6D6FFD6D6D6FFD6D6D6FFEAEAEAFF5050
      50FF00000000000000000000000000000000291C25FF967796FF291C25FF291C
      25FF967796FFD7C2D7FF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFFD7B3D7FF9677
      96FF291C25FF291C25FF967796FF291C25FF00000000000000000F2836FF87BB
      D7FF8CC7E6FF8CC7E6FF8CC7E6FF617079FF8CC7E6FF87BBD7FF0F2836FF0000
      000000000000000000000000000000000000040F15FF123D53FFD6D6D6FFB7B7
      B7FF123D53FF164A64FF164A64FF164A64FF0B2737FF0B2737FFB7B7B7FF9999
      99FFB7B7B7FF999999FF0B2737FF040F15FF00000000505050FFD6D6D6FFD6D6
      D6FFD6D6D6FFD6D6D6FFEAEAEAFFD6D6D6FFD6D6D6FFD6D6D6FFEAEAEAFFEAEA
      EAFF505050FF000000000000000000000000291C25FFB493B4FF0F0F0FFF9677
      96FFC2A0C2FF291C25FF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFF291C25FFC2A0
      C2FF967796FF0F0F0FFFB493B4FF291C25FF0000000000000000000000000F28
      36FF87BBD7FF8CC7E6FF617079FF617079FF617079FF8CC7E6FF87BBD7FF0F28
      36FFF7F7F7FF000000000000000000000000071A25FF123D53FFB7B7B7FF123D
      53FF164A64FF164A64FF164A64FF164A64FF164A64FF164A64FF0B2737FF0B27
      37FF999999FFB7B7B7FF999999FF5A5A5AFF505050FFD6D6D6FFD6D6D6FFD6D6
      D6FFD6D6D6FFEAEAEAFFD6D6D6FFD6D6D6FFD6D6D6FFD6D6D6FFEAEAEAFFD6D6
      D6FFD6D6D6FF505050FF505050FF00000000291C25FFB493B4FF0F0F0FFF9677
      96FFC2A0C2FF291C25FF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFF291C25FFC2A0
      C2FF967796FF0F0F0FFFB493B4FF291C25FF0000000000000000000000000000
      00000F2836FF87BBD7FF8CC7E6FF5F6E79FFF7F7F7FFF7F7F7FF87BBD7FF0F28
      36FF434343FFF7F7F7FF0000000000000000071A25FF123D53FF123D53FF164A
      64FF164A64FF164A64FF164A64FF164A64FF164A64FF164A64FF164A64FF164A
      64FF0B2737FF0B2737FFB7B7B7FF5A5A5AFFEAEAEAFFEAEAEAFFD6D6D6FFD6D6
      D6FFD6D6D6FFD6D6D6FFD6D6D6FFD6D6D6FFEAEAEAFFD6D6D6FFD6D6D6FFD6D6
      D6FFD6D6D6FFD6D6D6FFD6D6D6FF00000000291C25FF967796FF291C25FF291C
      25FF967796FFD7B3D7FF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFFD7C2D7FF9677
      96FF291C25FF291C25FF967796FF291C25FF0000000000000000000000000000
      0000000000000F2836FF87BBD7FF90C9E7FF434343FFF7F7F7FF87BBD7FF0F28
      36FF00000000434343FFF7F7F7FF00000000071A25FF123D53FF164A64FF164A
      64FF164A64FF164A64FF0B2737FF164A64FF164A64FF164A64FF164A64FF164A
      64FF164A64FF164A64FF0B2737FF040F15FF0000000000000000EAEAEAFFEAEA
      EAFFD6D6D6FFD6D6D6FFD6D6D6FFEAEAEAFFD6D6D6FFD6D6D6FFD6D6D6FFD6D6
      D6FFEAEAEAFFEAEAEAFF0000000000000000291C25FF0F0F0FFF967796FFC2A0
      C2FFD7B3D7FFD7C2D7FFD7C2D7FF291C25FF291C25FFD7C2D7FFD7C2D7FFD7B3
      D7FFC2A0C2FF967796FF776377FF291C25FF0000000000000000000000000000
      000000000000000000000F2836FF87BBD7FF434343FFF7F7F7FF0F2836FF0000
      000000000000434343FFF7F7F7FF00000000071A25FF071A25FF164A64FF164A
      64FF164A64FF0B2737FF164A64FF164A64FF164A64FF0B2737FF0B2737FF0B27
      37FF164A64FF164A64FF071A25FF000000000000000000000000000000000000
      0000EAEAEAFFEAEAEAFFD6D6D6FFEAEAEAFFD6D6D6FFD6D6D6FFD6D6D6FFEAEA
      EAFFEAEAEAFF000000000000000000000000291C25FF0F0F0FFF967796FFC2A0
      C2FFD7C2D7FFD7B3D7FF967796FFD7C2D7FFD7C2D7FF967796FFD7B3D7FFD7C2
      D7FFC2A0C2FF967796FF0F0F0FFF291C25FF0000000000000000000000000000
      00000000000000000000000000000F2836FF434343FFF7F7F7FF000000000000
      000000000000434343FFF7F7F7FF000000000000000000000000071A25FF071A
      25FF164A64FF164A64FF164A64FF164A64FF0B2737FF0B2737FF0B2737FF164A
      64FF164A64FF071A25FF00000000000000000000000000000000000000000000
      00000000000000000000EAEAEAFFEAEAEAFFD6D6D6FFD6D6D6FFEAEAEAFFEAEA
      EAFF00000000000000000000000000000000291C25FF0F0F0FFF0F0F0FFF9677
      96FFC2A0C2FFC2A0C2FF291C25FF967796FF967796FF291C25FFC2A0C2FFC2A0
      C2FF967796FF0F0F0FFF0F0F0FFF291C25FF0000000000000000000000000000
      00000000000000000000000000000000000000000000434343FFF7F7F7FF0000
      000000000000434343FFF7F7F7FF000000000000000000000000000000000000
      0000071A25FF071A25FF164A64FF164A64FF0B2737FF164A64FF164A64FF164A
      64FF071A25FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000EAEAEAFFEAEAEAFFEAEAEAFFEAEAEAFF0000
      000000000000000000000000000000000000291C25FF0F0F0FFF291C25FF0F0F
      0FFF967796FF967796FF291C25FF0F0F0FFF0F0F0FFF291C25FF967796FF9677
      96FF0F0F0FFF291C25FF0F0F0FFF291C25FF0000000000000000000000000000
      00000000000000000000000000000000000000000000434343FFDBDBDBFFF7F7
      F7FFDBDBDBFFF7F7F7FFDBDBDBFF000000000000000000000000000000000000
      00000000000000000000071A25FF071A25FF164A64FF164A64FF164A64FF071A
      25FF000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000EAEAEAFFEAEAEAFF000000000000
      000000000000000000000000000000000000343534FF0F0F0FFF0F0F0FFF0F0F
      0FFF0F0F0FFF0F0F0FFF967796FFB493B4FFB493B4FF967796FF0F0F0FFF0F0F
      0FFF0F0F0FFF0F0F0FFF0F0F0FFF343534FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000434343FFDBDB
      DBFFDBDBDBFFDBDBDBFF00000000000000000000000000000000000000000000
      000000000000000000000000000000000000071A25FF071A25FF071A25FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000323232FF343534FF291C25FF291C
      25FF291C25FF291C25FF291C25FF291C25FF291C25FF291C25FF291C25FF291C
      25FF291C25FF291C25FF343534FF343534FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004F7EC7004F7EC7004A77BC003654
      80007496CC0036548000668DC9004F7EC7004F7EC7004F7EC7004A77BC003654
      80007496CC0036548000668DC9004F7EC7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000B1318FF0C1419FF050A0DFF0C14
      19FF0B1318FF15254FFF122048FF15254FFF15254FFF15254FFF15254FFF0B13
      18FF050A0DFF050A0DFF050A0DFF0B1318FF00000000000000001D4D67FF0A1D
      27FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004F7EC7004A77BC00365480008BA6
      CF008BA6CF007496CC0036548000577BB2004771B100436CA900365480008BA6
      CF008BA6CF007496CC0036548000668DC9000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000080D11FF5990B4FF5990B4FF1422
      2AFF4571AFFF4773B1FF416DABFF416DABFF4571AFFF4571AFFF406BA9FF4571
      AFFF14222AFF5990B4FF5990B4FF0B1318FF0000000000000000143548FF2666
      89FF0A1D27FF0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000668DC900365480008BA6CF007991
      B5003C485A003C485A00324159001724370026354D00172437003C485A003C48
      5A003C485A007991B5007496CC00365480000000000000000000000000000000
      000000000000000000002C2C2CFF2C2C2CFF2C2C2CFF2C2C2CFF000000000000
      000000000000000000000000000000000000050A0DFF5990B4FF101D24FF3D6A
      A2FF406DA5FF3D6AA2FF3D6AA2FF3D6AA2FF3A679FFF3E6AA3FF3D6AA2FF406C
      A4FF426FA7FF122026FF578EB2FF050A0DFF0000000000000000000000001435
      48FF1D4D67FF0A1D27FF00000000000000000000000000000000000000000000
      000000000000000000000000000000000000365480008BA6CF008BA6CF003C48
      5A00CDC2C500AFA5A800A79D9F00998F91008074770080747700807477008074
      7700807477003C485A008BA6CF007496CC000000000000000000000000000000
      00002C2C2CFF2C2C2CFF484848FF484848FF484848FF484848FF2C2C2CFF2C2C
      2CFF00000000000000000000000000000000050A0DFF14222AFF3D6AA2FF2337
      54FF223753FF172A4AFF223653FF223753FF223753FF223753FF233754FF2237
      53FF223653FF3D6AA2FF15222BFF050A0DFF0000000000000000000000000000
      0000143548FF266689FF0A1D27FF000000000000000000000000000000000000
      000000000000434343FF00000000000000003F639A00365480008BA6CF003C48
      5A00CDC2C500B9B0B300B9B0B300B9B0B300B9B0B300B9B0B300B9B0B300B9B0
      B300807477003C485A008BA6CF00365480000000000000000000000000002C2C
      2CFF484848FF484848FF717171FF717171FF353535FF484848FF484848FF5757
      57FF2C2C2CFF000000000000000000000000050A0DFF4470AEFF3D6AA2FF2237
      53FF31589EFF4672B0FF203451FF4470AEFF4571AFFF203451FF32599EFF4874
      B2FF223753FF406DA5FF4571AFFF0A1217FF0000000000000000000000000000
      000000000000143548FF1D4D67FF0A1D27FF0000000000000000000000000000
      0000434343FFFFFFFFFF434343FF000000004F7EC7003F639A00365480003C48
      5A00CDC2C500B9B0B30023258200B9B0B300777ACE00B9B0B3006769AC00B9B0
      B300807477003C485A00365480004A77BC0000000000000000002C2C2CFF4848
      48FF717171FF8A8A8AFF717171FF717171FF717171FF484848FF484848FF4848
      48FF484848FF2C2C2CFF00000000000000000A1217FF436FADFF3D6AA2FF172A
      4AFF32599EFF3D6AA2FF1C314BFF3D6AA2FF2C4F8DFF142441FF3D6AA2FF4874
      B2FF223753FF3D6AA2FF4571AFFF0B1318FF0000000000000000000000000000
      00000000000000000000143548FF266689FF0A1D27FF00000000000000000000
      0000434343FFD8D8D8FF434343FF000000004F7EC7004F7EC7003F639A001724
      3700D5CACD00B9B0B300B9B0B300B9B0B300B9B0B300B9B0B300B9B0B300B9B0
      B30080747700172437004A77BC004F7EC70000000000000000002C2C2CFF7171
      71FF535353FF717171FF717171FF717171FF717171FF484848FF484848FF3535
      35FF484848FF2C2C2CFF00000000000000000B1318FF4773B1FF2B4C8BFF182C
      4CFF1F3450FF1B3049FF223753FF223753FF182C4CFF223753FF1C314BFF2034
      51FF223753FF3C68A1FF4571AFFF0B1318FF0000000000000000000000000000
      0000000000000000000000000000143548FF1D4D67FF0A1D27FF000000000000
      0000434343FFC1C1C1FF434343FF000000004F7EC7004F7EC7004F7EC7001A29
      3F00D5CACD00B9B0B3003E87C200B9B0B300777ACE00B9B0B300363ACD00B9B0
      B3008074770026354D004F7EC7004F7EC70000000000000000002C2C2CFF7171
      71FF717171FF717171FF535353FF717171FF8A8A8AFF484848FF484848FF4848
      48FF484848FF2C2C2CFF00000000000000000B1318FF4571AFFF325A98FF2237
      53FF4571AFFF3D6AA2FF223653FF4571AFFF4571AFFF223753FF3D69A1FF4571
      AFFF223753FF3B68A0FF4873B1FF0B1318FF0000000000000000000000000000
      000000000000000000000000000000000000143548FF266689FF0A1D27FF0000
      0000434343FFC1C1C1FF434343FF000000004F7EC7004F7EC7004F7EC7001724
      3700DACFD200B9B0B300B9B0B300B9B0B300B9B0B300B9B0B300B9B0B300B9B0
      B300998F910017243700668DC9004F7EC7000000000000000000000000002C2C
      2CFF717171FF717171FF717171FF717171FF717171FF484848FF353535FF4848
      48FF2C2C2CFF000000000000000000000000050A0DFF4672AFFF3D6AA2FF263A
      57FF4571AFFF3D6AA2FF223753FF4571AFFF4571AFFF223653FF3E6BA3FF416D
      ABFF223753FF3D6AA2FF4571AFFF0E161BFF0000000000000000000000000000
      00000000000000000000000000000000000000000000143548FF1D4D67FF0A1D
      27FFC1C1C1FFD8D8D8FF434343FF000000004F7EC7004F7EC700365480003C48
      5A00D6CBCE00B9B0B3005154A800B9B0B3002C2EA200B9B0B300326C4E00B9B0
      B300A79D9F003241590036548000668DC9000000000000000000000000000000
      00002C2C2CFF717171FF717171FF717171FF717171FF484848FF484848FF2C2C
      2CFF00000000000000000000000000000000050A0DFF4571AFFF3D6AA2FF2236
      53FF203551FF1B304AFF223653FF223653FF192D4DFF223753FF1C314BFF1729
      49FF213552FF3D6AA2FF4571AFFF050A0DFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000143548FFD8D8
      D8FFC1C1C1FF434343FF0000000000000000668DC900365480008BA6CF003C48
      5A00D6CBCE00B9B0B300B9B0B300B9B0B300B9B0B300B9B0B300B9B0B300B9B0
      B300AFA5A8003C485A007496CC00365480000000000000000000000000000000
      0000000000002C2C2CFF8A8A8AFF717171FF535353FF484848FF2C2C2CFF0000
      000000000000000000000000000000000000050A0DFF4571AFFF3B68A0FF2237
      53FF4571AFFF3D6AA2FF152642FF2E5391FF3D6AA2FF1C314BFF2C4F8DFF3259
      9EFF213652FF3B67A0FF4571AFFF0B1318FF0000000000000000000000000000
      00000000000000000000434343FF434343FF434343FF434343FFC1C1C1FFC1C1
      C1FF266689FF0A1D27FF0000000000000000365480008BA6CF008BA6CF003C48
      5A00DDD2D500DACFD200DACFD200DACFD200D5CACD00CDC2C500CDC2C500CDC2
      C500CDC2C5003C485A008BA6CF007496CC000000000000000000000000000000
      000000000000000000002C2C2CFF717171FF717171FF2C2C2CFF000000000000
      000000000000000000000000000000000000080D11FF4470AEFF3C68A1FF2237
      53FF32599EFF335DA3FF203451FF4571AFFF4571AFFF172A4AFF32599EFF4873
      B1FF223753FF3D6AA2FF4571AFFF0B1318FF0000000000000000000000000000
      000000000000434343FFFFFFFFFFD8D8D8FFC1C1C1FFC1C1C1FFD8D8D8FF4343
      43FF143548FF1D4D67FF00000000000000003F639A00365480008BA6CF007991
      B5003C485A003C485A003C485A00172437001B2B4300172437003C485A003C48
      5A003C485A007991B5008BA6CF00365480000000000000000000000000000000
      00000000000000000000000000002C2C2CFF2C2C2CFF00000000000000000000
      0000000000000000000000000000000000000B1318FF14222AFF406CA4FF2135
      52FF182C4CFF223753FF223753FF223753FF182C4CFF172A4AFF223753FF2136
      52FF223753FF3D6AA2FF14222AFF050A0DFF0000000000000000000000000000
      00000000000000000000434343FF434343FF434343FF434343FF434343FF0000
      0000000000000000000000000000000000004F7EC7003F639A00365480008BA6
      CF008BA6CF008BA6CF00365480004A77BC004F7EC7003F639A00365480008BA6
      CF008BA6CF008BA6CF00365480004A77BC000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000050A0DFF5B92B6FF111E25FF3D69
      A1FF3D6AA2FF38659DFF3D6AA2FF3B67A0FF426FA7FF3D6AA2FF3D6AA2FF3D6A
      A2FF3D6AA2FF121F26FF5A91B5FF0B1318FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004F7EC7004F7EC7003F639A003654
      80008BA6CF00365480004A77BC004F7EC7004F7EC7004F7EC7003F639A003654
      80008BA6CF00365480004A77BC004F7EC7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000B1318FF5990B4FF568DB1FF1422
      2AFF4571AFFF4571AFFF4571AFFF4571AFFF416DABFF436FADFF4571AFFF4571
      AFFF16232BFF5F96BAFF5990B4FF0A1217FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004F7EC7004F7EC7004F7EC7003F63
      9A0036548000668DC9004F7EC7004F7EC7004F7EC7004F7EC7004F7EC7003F63
      9A0036548000668DC9004F7EC7004F7EC7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000B1318FF0B1318FF050A0DFF0B13
      18FF0B1318FF0A1218FF050A0DFF050A0DFF050A0DFF0B1318FF0B1318FF050A
      0DFF0C1419FF0B1318FF0C1419FF0C1419FF424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000}
  end
  object pmView: TPopupMenu
    Images = ilIcons
    Left = 48
    Top = 240
    object actEdit1: TMenuItem
      Action = actEdit
      Default = True
    end
    object Rename1: TMenuItem
      Action = actRename
    end
    object Delete1: TMenuItem
      Action = actDelete
    end
    object N8: TMenuItem
      Caption = '-'
    end
    object miAdd2: TMenuItem
      Caption = 'Add'
      object Directory2: TMenuItem
        Action = actNewDirectory
      end
      object Namespace2: TMenuItem
        Action = actNewNamespace
      end
      object N9: TMenuItem
        Caption = '-'
      end
    end
    object Close1: TMenuItem
      Action = actCloseDatapack
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object Copynametoclipboard1: TMenuItem
      Action = actCopyName
    end
    object Copyfullpath1: TMenuItem
      Action = actCopyPath
    end
    object OpeninExplorer1: TMenuItem
      Action = actOpenInExplorer
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Expand1: TMenuItem
      Caption = 'Visibility'
      object actExpandAll1: TMenuItem
        Action = actExpandAll
      end
      object actExpandNamespaces1: TMenuItem
        Action = actExpandNamespaces
      end
      object actExpandTypes1: TMenuItem
        Action = actExpandTypes
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object actCollapseAll1: TMenuItem
        Action = actCollapseAll
      end
      object actCollapseAll2: TMenuItem
        Action = actCollapseNamespaces
      end
      object actCollapseTypes1: TMenuItem
        Action = actCollapseTypes
      end
    end
  end
  object pmTabs: TPopupMenu
    Left = 272
    Top = 32
    object Closetab1: TMenuItem
      Action = actCloseTab
    end
    object Closeallothertabs1: TMenuItem
      Action = actCloseAllOtherTabs
    end
  end
end
