unit SocketDefine;

interface

uses
  WinSock2, SysUtils, Classes, Lists;

type

  TSocketType = (
    stStream = SOCK_STREAM,
    stDatagram,
    stRaw,
    stRDM,
    stSeqPacket
  );

  TProtocolType = (
    ptIP = IPPROTO_IP,
    ptICMP,
    ptIGMP,
    ptGGP,
    ptTCP = IPPROTO_TCP,
    ptPUP = IPPROTO_PUP,
    ptUDP = IPPROTO_UDP,
    ptIDP = IPPROTO_IDP,
    ptND = IPPROTO_ND,
    ptRaw = IPPROTO_RAW
  );

  { TBasicSocket }

  TBasicSocket = class
  private
    FHandle: TSocket;

    class var
      FWSAData: TWSAData;

  public
    class constructor Create;
    class destructor Destroy;

    class property WSAData: TWSAData read FWSAData;

    constructor Create(ASocketType: TSocketType; AProtocolType: TProtocolType);
    destructor Destroy; override;

    procedure Bind(AAdress: String; APort: Word); overload;
    procedure Bind(APort: Word); overload;

    property Handle: TSocket read FHandle;
  end;

  { TConnection }

  TConnection = class (TThread)
  private
    FSocketHandle: TSocket;
  protected
    procedure Execute; override;
  public

  end;

  { TServer }

  TServer = class (TThread)
  private
    FListenerSocket: TBasicSocket;
    FConnections: TObjectList;
  protected
    procedure DoTerminate; override;
    procedure Execute; override;
  public
    constructor Create(APort: Word);
    destructor Destroy; override;
  end;

  { TClient }

  TClient = class
  public

  end;

implementation

{ TServer }

procedure TServer.DoTerminate;
begin
  inherited DoTerminate;
  // Kick all clients
end;

procedure TServer.Execute;
var
  SockAddr: TSockAddrIn;
  SockLen: Integer;
  DataBuffer: array [0 .. 255] of Char;
begin
  while not Terminated do
  begin
    FillChar(DataBuffer, 256, #0);
    SockLen := SizeOf(SockAddr);
    if RecvFrom(FListenerSocket.Handle, DataBuffer, 256, 0, SockAddr, SockLen) = SOCKET_ERROR then
      RaiseLastOSError
    else
    begin
      WriteLn(Format('New connection from %s:%d with Message: %s',
        [inet_ntoa(SockAddr.sin_addr), ntohs(SockAddr.sin_port), String(DataBuffer)]));
    end;
  end;
end;

constructor TServer.Create(APort: Word);
begin
  inherited Create(True);
  FListenerSocket := TBasicSocket.Create(stStream, ptTCP);
  FListenerSocket.Bind(APort);

  FConnections := TObjectList.Create;
end;

destructor TServer.Destroy;
begin
  FConnections.Free;
  inherited Destroy;
end;

{ TConnection }

procedure TConnection.Execute;
begin

end;

{ TBasicSocket }

class constructor TBasicSocket.Create;
begin
  if WSAStartup($0101, FWSAData) = SOCKET_ERROR then
    RaiseLastOSError;
end;

class destructor TBasicSocket.Destroy;
begin
  if WSACleanup = SOCKET_ERROR then
    RaiseLastOSError;
end;

constructor TBasicSocket.Create(ASocketType: TSocketType; AProtocolType: TProtocolType);
begin
  FHandle := Socket(AF_INET, Ord(ASocketType), Ord(AProtocolType));
  if FHandle = INVALID_SOCKET then
    RaiseLastOSError;
end;

destructor TBasicSocket.Destroy;
begin
  if CloseSocket(Handle) = SOCKET_ERROR then
    RaiseLastOSError;
  inherited Destroy;
end;

procedure TBasicSocket.Bind(AAdress: String; APort: Word);
var
  SockAddr: TSockAddrIn;
begin
  SockAddr.sin_addr.S_addr := inet_addr(PChar(AAdress));
  SockAddr.sin_family := AF_INET;
  SockAddr.sin_port := htons(APort);
  if WinSock2.Bind(Handle, SockAddr, SizeOf(SockAddr)) = SOCKET_ERROR then
    RaiseLastOSError;
end;

procedure TBasicSocket.Bind(APort: Word);
var
  SockAddr: TSockAddrIn;
begin
  SockAddr.sin_addr.S_addr := 0;
  SockAddr.sin_family := AF_INET;
  SockAddr.sin_port := htons(APort);
  if WinSock2.Bind(Handle, SockAddr, SizeOf(SockAddr)) = SOCKET_ERROR then
    RaiseLastOSError;
end;


end.

