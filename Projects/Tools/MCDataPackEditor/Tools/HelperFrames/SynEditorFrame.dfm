object frmSynEditor: TfrmSynEditor
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object synEditor: TSynEdit
    Left = 0
    Top = 0
    Width = 320
    Height = 240
    Align = alClient
    ActiveLineColor = 15138810
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Consolas'
    Font.Pitch = fpFixed
    Font.Style = []
    ParentShowHint = False
    ShowHint = False
    TabOrder = 0
    OnKeyPress = synEditorKeyPress
    CodeFolding.GutterShapeSize = 11
    CodeFolding.CollapsedLineColor = clGrayText
    CodeFolding.FolderBarLinesColor = clGrayText
    CodeFolding.IndentGuidesColor = clGray
    CodeFolding.IndentGuides = True
    CodeFolding.ShowCollapsedLine = False
    CodeFolding.ShowHintMark = True
    UseCodeFolding = False
    Gutter.DigitCount = 3
    Gutter.Font.Charset = ANSI_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -16
    Gutter.Font.Name = 'Consolas'
    Gutter.Font.Style = []
    Gutter.LeftOffset = 18
    Gutter.ShowLineNumbers = True
    Gutter.Gradient = True
    MaxScrollWidth = 256
    MaxUndo = 65536
    Options = [eoAltSetsColumnMode, eoAutoSizeMaxScrollWidth, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoTabsToSpaces, eoTrimTrailingSpaces]
    RightEdge = 0
    TabWidth = 2
    WantTabs = True
    FontSmoothing = fsmClearType
    RemovedKeystrokes = <>
    AddedKeystrokes = <
      item
        Command = ecDeleteWord
        ShortCut = 16430
      end>
  end
end
