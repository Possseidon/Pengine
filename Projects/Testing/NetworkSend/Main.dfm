object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Network Testing'
  ClientHeight = 249
  ClientWidth = 317
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  DesignSize = (
    317
    249)
  PixelsPerInch = 96
  TextHeight = 13
  object lblAddress: TLabel
    Left = 10
    Top = 11
    Width = 57
    Height = 13
    Caption = 'Connect to:'
  end
  object edtAddress: TEdit
    Left = 73
    Top = 8
    Width = 74
    Height = 21
    TabOrder = 0
    Text = 'localhost'
    OnExit = edtAddressExit
  end
  object memMessages: TMemo
    Left = 8
    Top = 35
    Width = 301
    Height = 179
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    TabOrder = 1
  end
  object edtInput: TEdit
    Left = 8
    Top = 220
    Width = 247
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 2
    OnKeyDown = edtInputKeyDown
    OnKeyPress = edtInputKeyPress
    ExplicitTop = 262
    ExplicitWidth = 257
  end
  object btnSend: TButton
    Left = 261
    Top = 220
    Width = 48
    Height = 21
    Action = actSend
    Anchors = [akRight, akBottom]
    TabOrder = 3
    ExplicitLeft = 271
    ExplicitTop = 262
  end
  object btnConnect: TButton
    Left = 153
    Top = 8
    Width = 75
    Height = 21
    Action = actConnect
    TabOrder = 4
  end
  object btnStartServer: TButton
    Left = 234
    Top = 8
    Width = 75
    Height = 21
    Action = actStartServer
    TabOrder = 5
  end
  object socClient: TClientSocket
    Active = False
    Address = 'localhost'
    ClientType = ctNonBlocking
    Host = 'localhost'
    Port = 42424
    OnRead = socClientRead
    Left = 160
    Top = 72
  end
  object socServer: TServerSocket
    Active = False
    Port = 42424
    ServerType = stNonBlocking
    OnClientConnect = socServerClientConnect
    OnClientDisconnect = socServerClientDisconnect
    OnClientRead = socServerClientRead
    Left = 72
    Top = 72
  end
  object alActions: TActionList
    Left = 72
    Top = 136
    object actSend: TAction
      Caption = 'Send'
      OnExecute = actSendExecute
      OnUpdate = actSendUpdate
    end
    object actConnect: TAction
      Caption = '[Connect]'
      OnExecute = actConnectExecute
      OnUpdate = actConnectUpdate
    end
    object actStartServer: TAction
      Caption = '[Start Server]'
      OnExecute = actStartServerExecute
      OnUpdate = actStartServerUpdate
    end
  end
end
