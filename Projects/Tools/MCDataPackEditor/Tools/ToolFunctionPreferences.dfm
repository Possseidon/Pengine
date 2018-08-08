object frmFunctionPreferences: TfrmFunctionPreferences
  Left = 0
  Top = 0
  Caption = 'Function Highlighting'
  ClientHeight = 557
  ClientWidth = 484
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 500
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object gbBasic: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 39
    Width = 478
    Height = 161
    Align = alTop
    Caption = 'Basic'
    TabOrder = 0
    ExplicitLeft = 8
    ExplicitTop = 63
    DesignSize = (
      478
      161)
    object lbBackground: TLabel
      Left = 6
      Top = 21
      Width = 56
      Height = 13
      Caption = 'Background'
    end
    object lbCurrentLineBackground: TLabel
      Left = 6
      Top = 49
      Width = 59
      Height = 13
      Caption = 'Current Line'
    end
    object lbTextDefault: TLabel
      Left = 6
      Top = 77
      Width = 60
      Height = 13
      Caption = 'Text Default'
    end
    object lbComment: TLabel
      Left = 6
      Top = 105
      Width = 45
      Height = 13
      Caption = 'Comment'
    end
    object lbError: TLabel
      Left = 6
      Top = 133
      Width = 24
      Height = 13
      Caption = 'Error'
    end
    object cbBackground: TColorBox
      Left = 128
      Top = 18
      Width = 145
      Height = 22
      Selected = clWhite
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object cbCurrentLine: TColorBox
      Left = 128
      Top = 46
      Width = 145
      Height = 22
      Selected = 15138810
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
    end
    object btnSelectFont: TButton
      Left = 292
      Top = 31
      Width = 175
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Select Default Font...'
      TabOrder = 1
      OnClick = btnSelectFontClick
    end
    object cbTextDefault: TColorBox
      Left = 128
      Top = 74
      Width = 145
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
    end
    object cbTextDefaultBold: TCheckBox
      Left = 292
      Top = 76
      Width = 50
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Bold'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 4
    end
    object cbTextItalic: TCheckBox
      Left = 348
      Top = 76
      Width = 51
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Italic'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsItalic]
      ParentFont = False
      TabOrder = 5
    end
    object cbTextUndeline: TCheckBox
      Left = 405
      Top = 76
      Width = 67
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Underline'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
      TabOrder = 6
    end
    object cbCommentBold: TCheckBox
      Left = 292
      Top = 104
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
      TabOrder = 7
    end
    object cbCommentItalic: TCheckBox
      Left = 348
      Top = 104
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
      TabOrder = 8
    end
    object cbCommentUnderline: TCheckBox
      Left = 405
      Top = 104
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
      TabOrder = 9
    end
    object cbComment: TColorBox
      Left = 128
      Top = 102
      Width = 145
      Height = 22
      Selected = clGreen
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 10
    end
    object cbErrorBold: TCheckBox
      Left = 292
      Top = 132
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
      TabOrder = 11
    end
    object cbErrorItalic: TCheckBox
      Left = 348
      Top = 132
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
      TabOrder = 12
    end
    object cbErrorUnderline: TCheckBox
      Left = 405
      Top = 132
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
      TabOrder = 13
    end
    object cbError: TColorBox
      Left = 128
      Top = 130
      Width = 145
      Height = 22
      Selected = clRed
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 14
    end
  end
  object pnlPreset: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 478
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    DesignSize = (
      478
      30)
    object lbLoadPreset: TLabel
      Left = 6
      Top = 10
      Width = 61
      Height = 13
      Caption = 'Load Preset:'
    end
    object btnLoadTheme: TButton
      Left = 383
      Top = 5
      Width = 84
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Load Theme...'
      TabOrder = 0
    end
    object btnSaveTheme: TButton
      Left = 292
      Top = 5
      Width = 84
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Save Theme...'
      TabOrder = 1
    end
    object cbPreset: TComboBox
      Left = 128
      Top = 7
      Width = 145
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
    end
  end
  object gbSyntaxHighlighting: TGroupBox
    Left = 0
    Top = 203
    Width = 484
    Height = 323
    Align = alClient
    Caption = 'Syntax Highlighting'
    TabOrder = 2
    ExplicitTop = 198
    ExplicitHeight = 309
    object pnlElementSettings: TPanel
      Left = 2
      Top = 48
      Width = 480
      Height = 273
      Align = alClient
      BevelEdges = [beTop]
      BevelKind = bkTile
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitHeight = 259
      object splPreview: TSplitter
        Left = 0
        Top = 162
        Width = 480
        Height = 3
        Cursor = crVSplit
        Align = alBottom
        ResizeStyle = rsUpdate
        ExplicitTop = 0
        ExplicitWidth = 171
      end
      inline phElementHighlighting: TfrmParserHighlighting
        Left = 0
        Top = 0
        Width = 480
        Height = 162
        VertScrollBar.Tracking = True
        Align = alClient
        AutoScroll = True
        TabOrder = 0
        ExplicitWidth = 480
        ExplicitHeight = 148
      end
      inline frmSynPreview: TfrmSynEditor
        Left = 0
        Top = 165
        Width = 480
        Height = 106
        Align = alBottom
        TabOrder = 1
        ExplicitTop = 151
        ExplicitWidth = 480
        ExplicitHeight = 106
        inherited synEditor: TSynEdit
          Width = 480
          Height = 106
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 480
          ExplicitHeight = 106
        end
      end
    end
    object pnlElement: TPanel
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 474
      Height = 27
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        474
        27)
      object lbElemenr: TLabel
        Left = 4
        Top = 6
        Width = 38
        Height = 13
        Caption = 'Element'
      end
      object cbElement: TComboBox
        Left = 126
        Top = 3
        Width = 145
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = cbElementChange
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 526
    Width = 484
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 505
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 406
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
      ExplicitHeight = 23
    end
    object btnOk: TButton
      AlignWithMargins = True
      Left = 325
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
      ExplicitHeight = 23
    end
  end
  object FontDialog: TFontDialog
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Consolas'
    Font.Style = []
    Options = [fdEffects, fdFixedPitchOnly]
    Left = 442
    Top = 70
  end
end
