unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.Win.ScktComp, System.Actions, Vcl.ActnList;

type

  TConnectionType = (
    ctDisconnected,
    ctServer,
    ctClient
    );

  TfrmMain = class(TForm)
    edtAddress: TEdit;
    lblAddress: TLabel;
    memMessages: TMemo;
    edtInput: TEdit;
    btnSend: TButton;
    socClient: TClientSocket;
    socServer: TServerSocket;
    btnConnect: TButton;
    btnStartServer: TButton;
    alActions: TActionList;
    actSend: TAction;
    actConnect: TAction;
    actStartServer: TAction;
    procedure edtInputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure actSendUpdate(Sender: TObject);
    procedure actSendExecute(Sender: TObject);
    procedure actConnectUpdate(Sender: TObject);
    procedure actStartServerUpdate(Sender: TObject);
    procedure actConnectExecute(Sender: TObject);
    procedure actStartServerExecute(Sender: TObject);
    procedure edtAddressExit(Sender: TObject);
    procedure socClientRead(Sender: TObject; Socket: TCustomWinSocket);
    procedure socServerClientRead(Sender: TObject; Socket: TCustomWinSocket);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure socServerClientConnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure socServerClientDisconnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure edtInputKeyPress(Sender: TObject; var Key: Char);
  private
    FConnectionType: TConnectionType;

    procedure SetConnectionType(const Value: TConnectionType);
    property ConnectionType: TConnectionType read FConnectionType write SetConnectionType;

    procedure AppendText(AText: string);
    procedure ServerSendAll(AText: string);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.actConnectExecute(Sender: TObject);
begin
  if ConnectionType = ctClient then
    ConnectionType := ctDisconnected
  else
    ConnectionType := ctClient;
end;

procedure TfrmMain.actConnectUpdate(Sender: TObject);
begin
  if ConnectionType = ctClient then
    actConnect.Caption := 'Disconnect'
  else
    actConnect.Caption := 'Connect';
  actConnect.Enabled := ConnectionType <> ctServer;
end;

procedure TfrmMain.actSendExecute(Sender: TObject);
var
  I, J: Integer;
  Connection: TCustomWinSocket;
  S: UTF8String;
  Stream: TStringStream;
begin
  case ConnectionType of
    ctServer:
      begin
        ServerSendAll('Server: ' + edtInput.Text);
      end;
    ctClient:
      begin
        
        for I := 0 to 360 do
        begin
          S := '';
          for J := 0 to Round((Sin(I / 18 * Pi) + 1) * 40) do
            S := S + '#';                  
          socClient.Socket.SendText(S + #10);
        end;
        
        S := edtInput.Text + #10;
        socClient.Socket.SendBuf(S[1], Length(S) + 1);
      end;
  end;
  edtInput.Text := '';
end;

procedure TfrmMain.actSendUpdate(Sender: TObject);
begin
  actSend.Enabled := ConnectionType <> ctDisconnected;
end;

procedure TfrmMain.actStartServerExecute(Sender: TObject);
begin
  if ConnectionType = ctServer then
    ConnectionType := ctDisconnected
  else
    ConnectionType := ctServer;
end;

procedure TfrmMain.actStartServerUpdate(Sender: TObject);
begin
  if ConnectionType = ctServer then
    actStartServer.Caption := 'Stop Server'
  else
    actStartServer.Caption := 'Start Server';
  actStartServer.Enabled := ConnectionType <> ctClient;
end;

procedure TfrmMain.AppendText(AText: string);
begin
  memMessages.Lines.Add(AText);
end;

procedure TfrmMain.edtAddressExit(Sender: TObject);
begin
  socClient.Host := edtAddress.Text;
end;

procedure TfrmMain.edtInputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      begin
        actSend.Execute;
      end;
  end;
end;

procedure TfrmMain.edtInputKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    #13:
      Key := #0;  
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ConnectionType := ctDisconnected;
end;

procedure TfrmMain.ServerSendAll(AText: string);
var
  I: Integer;
begin
  for I := 0 to socServer.Socket.ActiveConnections - 1 do
    socServer.Socket.Connections[I].SendText(AText);     
  AppendText(AText);
end;

procedure TfrmMain.SetConnectionType(const Value: TConnectionType);
begin                                              
  FConnectionType := Value;
  try
    socServer.Active := FConnectionType = ctServer;
    socClient.Active := FConnectionType = ctClient;
    if FConnectionType = ctDisconnected then
      socClient.Close;
  except
    on E: ESocketError do
    begin
      ShowMessage(E.Message);
      socServer.Active := False;
      socClient.Active := False;
      FConnectionType := ctDisconnected;
    end;
  end;                                  
end;

procedure TfrmMain.socClientRead(Sender: TObject; Socket: TCustomWinSocket);
begin
  AppendText(Socket.ReceiveText);
end;

procedure TfrmMain.socServerClientConnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  ServerSendAll('Client connected');
end;

procedure TfrmMain.socServerClientDisconnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  ServerSendAll('Client disconnected');
end;

procedure TfrmMain.socServerClientRead(Sender: TObject; Socket: TCustomWinSocket);
var
  I: Integer;
  Connection: TCustomWinSocket;
  S: string;
  J: Integer;
begin
  for I := 0 to socServer.Socket.ActiveConnections - 1 do
  begin
    Connection := socServer.Socket.Connections[I];
    if Connection.ReceiveLength > 0 then
    begin
      ServerSendAll(Format('Client %d: %s', [I, Connection.ReceiveText]));
    end;
  end;
end;

end.
