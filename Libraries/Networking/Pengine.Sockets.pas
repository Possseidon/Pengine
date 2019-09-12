unit Pengine.Sockets;

interface

uses
  System.SysUtils,
  System.Classes,

  Winapi.Windows,
  Winapi.WinSock,

  Pengine.ICollections;

type

  ESocketError = class(Exception);

  TAddressFamily = (
    afUnknown = -1,
    afUnspecified = 0,
    afUnix = 1,
    afInterNetwork = 2,
    afImpLink = 3,
    afPup = 4,
    afChaos = 5,
    afIpx = 6,
    afNS = 6,
    afIso = 7,
    afOsi = 7,
    afEcma = 8,
    afDataKit = 9,
    afCcitt = 10,
    afSna = 11,
    afDecNet = 12,
    afDataLink = 13,
    afLat = 14,
    afHyperChannel = 15,
    afAppleTalk = 16,
    afNetBios = 17,
    afVoiceView = 18,
    afFireFox = 19,
    afBanyan = 21,
    afAtm = 22,
    afInterNetworkV6 = 23,
    afCluster = 24,
    afIeee12844 = 25,
    afIrda = 26,
    afNetworkDesigners = 28,
    afMax = 29
    );

  TSocketType = (
    stUnknown = -1,
    stStream = 1,
    stDgram = 2,
    stRaw = 3,
    stRdm = 4,
    stSeqpacket = 5
    );

  TProtocolType = (
    ptUnknown = -1,
    ptIP = 0,
    ptIPv6HopByHopOptions = 0,
    ptUnspecified = 0,
    ptIcmp = 1,
    ptIgmp = 2,
    ptGgp = 3,
    ptIPv4 = 4,
    ptTcp = 6,
    ptPup = 12,
    ptUdp = 17,
    ptIdp = 22,
    ptIPv6 = 41,
    ptIPv6RoutingHeader = 43,
    ptIPv6FragmentHeader = 44,
    ptIPSecEncapsulatingSecurityPayload = 50,
    ptIPSecAuthenticationHeader = 51,
    ptIcmpV6 = 58,
    ptIPv6NoNextHeader = 59,
    ptIPv6DestinationOptions = 60,
    ptND = 77,
    ptRaw = 255,
    ptIpx = 1000,
    ptSpx = 1256,
    ptSpxII = 1257
    );

  TSocket = class
  private
    FHandle: Winapi.WinSock.TSocket;
    FAddressFamily: TAddressFamily;
    FSocketType: TSocketType;
    FProtocolType: TProtocolType;
    FBlocking: Boolean;

    class function MakeSockAddr(AAddressFamily: TAddressFamily; AAddress: Integer; APort: Word): TSockAddr; static;

    procedure SetBlocking(const Value: Boolean);

  protected
    constructor Create(AHandle: Winapi.WinSock.TSocket); overload;
    constructor Create(AAddressFamily: TAddressFamily; ASocketType: TSocketType; AProtocolType: TProtocolType);
      overload;

  public
    class constructor Create;
    class destructor Destroy;

    destructor Destroy; override;

    class function FormatError(AError: Integer): string; static;
    class procedure RaiseLastError; static;

    property Handle: Winapi.WinSock.TSocket read FHandle;

    procedure GetSockName(out AAddress: Integer; out APort: Word); overload;
    procedure GetSockName(out AAddress: string; out APort: Word); overload;

    property Blocking: Boolean read FBlocking write SetBlocking;
    property AddressFamily: TAddressFamily read FAddressFamily;
    property SocketType: TSocketType read FSocketType;
    property ProtocolType: TProtocolType read FProtocolType;

  end;

  TServerSocket = class;

  TClientSocket = class(TSocket)
  private
    FParent: TServerSocket;
    FDisconnected: Boolean;

    function GetAvailable: Integer;
    function GetDisconnected: Boolean;

  public
    constructor Create; overload;
    constructor Create(AParent: TServerSocket; AHandle: Winapi.WinSock.TSocket); overload;

    procedure Connect(AAddress: Integer; APort: Word = 0); overload;
    procedure Connect(AAddress: string; APort: Word = 0); overload;

    procedure GetPeerName(out AAddress: Integer; out APort: Word); overload;
    procedure GetPeerName(out AAddress: string; out APort: Word); overload;

    procedure SendBuffer(const AData; ASize: Integer);
    procedure Send<T>(const AData: T);
    procedure SendBytes(ABytes: TBytes);

    function CanReceive: Boolean;
    property Available: Integer read GetAvailable;
    procedure ReceiveBuffer(var AData; ASize: Integer);
    procedure Receive<T>(var AData: T); overload;
    function Receive<T>: T; overload;
    function ReceiveAll: TBytes;
    function ReceiveBytes(ASize: Integer): TBytes; inline;

    property Disconnected: Boolean read GetDisconnected;

  end;

  TServerSocket = class(TSocket)
  private
    FClients: IObjectList<TClientSocket>;

    function GetClients: IReadonlyList<TClientSocket>;

  public
    constructor Create;

    property Clients: IReadonlyList<TClientSocket> read GetClients;

    procedure Bind(AAddress: Integer; APort: Word = 0); overload;
    procedure Bind(AAddress: string; APort: Word = 0); overload;

    procedure Listen(ABacklog: Integer = SOMAXCONN);

    function CanAccept: Boolean;
    function Accept: TClientSocket;
    function AcceptAll: Integer;

    procedure CleanupDisconnectedClients;

  end;
  
  TSocketStream = class(TStream)
  private
    FClient: TClientSocket;
  
  public
    constructor Create(AClient: TClientSocket);
 
    function Read(var Buffer; Count: Integer): Integer; override;
    function Write(const Buffer; Count: Integer): Integer; override;
    function Seek(Offset: Integer; Origin: Word): Integer; override;

    property Client: TClientSocket read FClient;
    
  end;

implementation

{ TSocket }

class function TSocket.MakeSockAddr(AAddressFamily: TAddressFamily; AAddress: Integer; APort: Word): TSockAddr;
begin
  Result.sin_family := Ord(AAddressFamily);
  Result.sin_port := htons(APort);
  Result.sin_addr.S_addr := AAddress;
  FillChar(Result.sin_zero[0], SizeOf(Result.sin_zero), 0);
end;

procedure TSocket.SetBlocking(const Value: Boolean);
var
  Arg: Integer;
begin
  if Blocking = Value then
    Exit;
  FBlocking := Value;
  if Value then
    Arg := 0
  else
    Arg := 1;
  if ioctlsocket(Handle, FIONBIO, Arg) = SOCKET_ERROR then
    RaiseLastError;
end;

constructor TSocket.Create(AHandle: Winapi.WinSock.TSocket);
begin
  FHandle := AHandle;
  FBlocking := True;
end;

constructor TSocket.Create(AAddressFamily: TAddressFamily; ASocketType: TSocketType; AProtocolType: TProtocolType);
begin
  FAddressFamily := AAddressFamily;
  FSocketType := ASocketType;
  FProtocolType := AProtocolType;
  Create(socket(Ord(AddressFamily), Ord(SocketType), Ord(ProtocolType)));
  if Handle = INVALID_SOCKET then
    RaiseLastError;
end;

class constructor TSocket.Create;
var
  Data: WSAData;
  Err: Integer;
  Msg: string;
begin
  Err := WSAStartup($101, Data);
  if Err = SOCKET_ERROR then
  begin
    Msg := FormatError(Err);
    MessageBox(0, @Msg[1], 'Socket Error', MB_OK or MB_ICONERROR or MB_TASKMODAL);
    Halt(1);
  end;
end;

class destructor TSocket.Destroy;
begin
  WSACleanup;
end;

destructor TSocket.Destroy;
begin
  closesocket(Handle);
end;

class function TSocket.FormatError(AError: Integer): string;
var
  Len: Cardinal;
begin
  SetLength(Result, 260);
  Len := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, AError, 0, @Result[1], Length(Result), nil);
  SetLength(Result, Len);
end;

class procedure TSocket.RaiseLastError;
begin
  raise ESocketError.Create(FormatError(WSAGetLastError));
end;

procedure TSocket.GetSockName(out AAddress: Integer; out APort: Word);
var
  SockAddr: TSockAddr;
  NameLen: Integer;
begin
  if Winapi.WinSock.GetSockName(Handle, SockAddr, NameLen) = SOCKET_ERROR then
    RaiseLastError;
  Assert(NameLen = SizeOf(SockAddr));
  AAddress := SockAddr.sin_addr.S_addr;
  APort := htons(SockAddr.sin_port);
end;

procedure TSocket.GetSockName(out AAddress: string; out APort: Word);
var
  Address: TInAddr;
begin
  GetSockName(Address.S_addr, APort);
  AAddress := string(inet_ntoa(Address));
end;

{ TClientSocket }

function TClientSocket.GetAvailable: Integer;
begin
  ioctlsocket(Handle, FIONREAD, Result);
end;

function TClientSocket.GetDisconnected: Boolean;
var
  Buffer: Byte;
begin
  if FDisconnected then
    Exit(True);
  if not CanReceive then
    Exit(False);
  Result := Winapi.WinSock.recv(Handle, Buffer, 1, MSG_PEEK) = 0;
  FDisconnected := Result;
end;

constructor TClientSocket.Create;
begin
  inherited Create(afInterNetwork, stStream, ptTcp);
end;

constructor TClientSocket.Create(AParent: TServerSocket; AHandle: Winapi.WinSock.TSocket);
begin
  inherited Create(AHandle);
  FParent := AParent;
end;

procedure TClientSocket.Connect(AAddress: Integer; APort: Word);
var
  SockAddr: TSockAddr;
begin
  SockAddr := MakeSockAddr(AddressFamily, AAddress, APort);
  if Winapi.WinSock.Connect(Handle, SockAddr, SizeOf(SockAddr)) = SOCKET_ERROR then
    RaiseLastError;
end;

procedure TClientSocket.Connect(AAddress: string; APort: Word);
begin
  Connect(inet_addr(PAnsiChar(AnsiString(AAddress))), APort);
end;

procedure TClientSocket.GetPeerName(out AAddress: Integer; out APort: Word);
var
  SockAddr: TSockAddr;
  NameLen: Integer;
begin
  if Winapi.WinSock.GetPeerName(Handle, SockAddr, NameLen) = SOCKET_ERROR then
    RaiseLastError;
  Assert(NameLen = SizeOf(SockAddr));
  AAddress := SockAddr.sin_addr.S_addr;
  APort := htons(SockAddr.sin_port);
end;

procedure TClientSocket.GetPeerName(out AAddress: string; out APort: Word);
var
  Address: TInAddr;
begin
  GetPeerName(Address.S_addr, APort);
  AAddress := string(inet_ntoa(Address));
end;

procedure TClientSocket.SendBuffer(const AData; ASize: Integer);
var
  Sent, Offset: Integer;
begin
  Offset := 0;
  while Offset < ASize do
  begin
    Sent := Winapi.WinSock.Send(Handle, (PByte(@AData) + Offset)^, ASize, 0);
    if Sent = SOCKET_ERROR then
      RaiseLastError;
    Inc(Offset, Sent);
  end;
end;

procedure TClientSocket.Send<T>(const AData: T);
begin
  Assert(not IsManagedType(T));
  SendBuffer(AData, SizeOf(T));
end;

procedure TClientSocket.SendBytes(ABytes: TBytes);
begin
  SendBuffer(ABytes[0], Length(ABytes));
end;

function TClientSocket.CanReceive: Boolean;
const
  Timeout: TTimeVal = (tv_sec: 0; tv_usec: 0);
var
  FDSet: TFDSet;
begin
  FDSet.fd_count := 1;
  FDSet.fd_array[0] := Handle;
  Result := Winapi.WinSock.select(0, @FDSet, nil, nil, @Timeout) <> 0;
end;

procedure TClientSocket.ReceiveBuffer(var AData; ASize: Integer);
var
  Err: Integer;
begin
  Err := Winapi.WinSock.recv(Handle, AData, ASize, 0);
  if (Err = 0) and (ASize > 0) then
    FDisconnected := True
  else if Err = SOCKET_ERROR then
    RaiseLastError;
end;

procedure TClientSocket.Receive<T>(var AData: T);
begin                          
  Assert(not IsManagedType(T));
  ReceiveBuffer(AData, SizeOf(AData));
end;

function TClientSocket.Receive<T>: T;
begin
  Receive(Result);
end;

function TClientSocket.ReceiveAll: TBytes;
begin
  if CanReceive then
    Result := ReceiveBytes(Available)
  else
    Result := nil;
end;

function TClientSocket.ReceiveBytes(ASize: Integer): TBytes;
begin
  SetLength(Result, ASize);
  ReceiveBuffer(Result[0], ASize);
end;

{ TServerSocket }

function TServerSocket.GetClients: IReadonlyList<TClientSocket>;
begin
  Result := FClients.ReadonlyList;
end;

constructor TServerSocket.Create;
begin
  inherited Create(afInterNetwork, stStream, ptTcp);
  FClients := TObjectList<TClientSocket>.Create;
end;

procedure TServerSocket.Bind(AAddress: Integer; APort: Word);
var
  SockAddr: TSockAddr;
begin
  SockAddr := MakeSockAddr(AddressFamily, AAddress, APort);
  if Winapi.WinSock.Bind(Handle, SockAddr, SizeOf(SockAddr)) = SOCKET_ERROR then
    RaiseLastError;
end;

procedure TServerSocket.Bind(AAddress: string; APort: Word);
begin
  Bind(inet_addr(PAnsiChar(AnsiString(AAddress))), APort);
end;

procedure TServerSocket.Listen(ABacklog: Integer);
begin
  if Winapi.WinSock.Listen(Handle, ABacklog) = SOCKET_ERROR then
    RaiseLastError;
end;

function TServerSocket.CanAccept: Boolean;
const
  Timeout: TTimeVal = (tv_sec: 0; tv_usec: 0);
var
  FDSet: TFDSet;
begin
  FDSet.fd_count := 1;
  FDSet.fd_array[0] := Handle;
  Result := Winapi.WinSock.select(0, @FDSet, nil, nil, @Timeout) <> 0;
end;

procedure TServerSocket.CleanupDisconnectedClients;
var
  I: Integer;
begin
  for I := FClients.MaxIndex downto 0 do
    if FClients[I].Disconnected then
      FClients.RemoveAt(I);
end;

function TServerSocket.Accept: TClientSocket;
var
  Address: TSockAddr;
  AcceptedSocket: Winapi.WinSock.TSocket;
begin
  AcceptedSocket := Winapi.WinSock.Accept(Handle, @Address, nil);
  if AcceptedSocket = INVALID_SOCKET then
    RaiseLastError;
  Result := TClientSocket.Create(AcceptedSocket);
  Result.FAddressFamily := AddressFamily;
  Result.FSocketType := SocketType;
  Result.FProtocolType := ProtocolType;
  FClients.Add(Result);
end;

function TServerSocket.AcceptAll: Integer;
begin
  Result := 0;
  while CanAccept do
  begin
    Accept;
    Inc(Result);
  end;
end;

{ TSocketStream }

constructor TSocketStream.Create(AClient: TClientSocket);
begin
  FClient := AClient;
end;

function TSocketStream.Read(var Buffer; Count: Integer): Integer;
begin
  FClient.ReceiveBuffer(Buffer, Count);
  Result := Count;
end;

function TSocketStream.Write(const Buffer; Count: Integer): Integer;
begin
  FClient.SendBuffer(Buffer, Count);
  Result := Count;
end;

function TSocketStream.Seek(Offset: Integer; Origin: Word): Integer;
begin
  Result := 0;
end;

end.
