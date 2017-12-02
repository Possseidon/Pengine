object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 506
  ClientWidth = 859
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnOpen: TButton
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 853
    Height = 25
    Align = alTop
    Caption = 'Open source file...'
    TabOrder = 0
    OnClick = btnOpenClick
  end
  object pnlMain: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 34
    Width = 853
    Height = 469
    Align = alClient
    TabOrder = 1
    OnResize = pnlMainResize
    object spltMain: TSplitter
      Left = 436
      Top = 1
      Width = 5
      Height = 467
      AutoSnap = False
      Color = clBtnFace
      MinSize = 100
      ParentColor = False
      ResizeStyle = rsUpdate
      OnMoved = spltMainMoved
      ExplicitLeft = 189
      ExplicitTop = 0
      ExplicitHeight = 343
    end
    object gbInterface: TGroupBox
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 429
      Height = 461
      Align = alLeft
      Caption = 'Interface Methods'
      TabOrder = 0
      object lbInterface: TListBox
        Left = 2
        Top = 15
        Width = 425
        Height = 444
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object gbImplementation: TGroupBox
      AlignWithMargins = True
      Left = 444
      Top = 4
      Width = 405
      Height = 461
      Align = alClient
      Caption = 'Implementation Methods'
      TabOrder = 1
      object lbImplementation: TListBox
        Left = 2
        Top = 15
        Width = 401
        Height = 444
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
      end
    end
  end
  object odOpenFile: TOpenDialog
    Filter = 'Pascal Source File|*.pas'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 387
    Top = 318
  end
end
