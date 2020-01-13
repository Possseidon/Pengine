unit Pengine.Sockets;

interface

uses
  System.SysUtils,
  System.Classes,

  Winapi.Windows,
  Winapi.WinSock,

  Pengine.EventHandling,
  Pengine.ICollections;

type

  TGetNameFunc = function(s: TSocket; var name: TSockAddr; var namelen: Integer): Integer; stdcall;

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

    class var
      FWSAData: WSAData;

    procedure SetBlocking(const Value: Boolean);

  protected
    constructor Create(AHandle: Winapi.WinSock.TSocket); overload;
    constructor Create(AAddressFamily: TAddressFamily; ASocketType: TSocketType; AProtocolType: TProtocolType);
      overload;

    procedure GetName(AFunc: TGetNameFunc; var AAddress, AService: AnsiString; AFlags: Integer); overload;
    procedure GetName(AFunc: TGetNameFunc; var AAddress, AService: string; AFlags: Integer); overload;

  public
    class property WSAData: WSAData read FWSAData;

    class constructor Create;
    class destructor Destroy;

    destructor Destroy; override;

    class function FormatError(AError: Integer): string; static;
    class procedure RaiseLastError; static;

    property Handle: Winapi.WinSock.TSocket read FHandle;

    property Blocking: Boolean read FBlocking write SetBlocking;
    property AddressFamily: TAddressFamily read FAddressFamily;
    property SocketType: TSocketType read FSocketType;
    property ProtocolType: TProtocolType read FProtocolType;

    procedure GetSockName(var AAddress, AService: string); overload;
    procedure GetSockName(var AAddress: string; var APort: Word); overload;
    procedure GetSockName(var AAddress, AService: AnsiString); overload;
    procedure GetSockName(var AAddress: AnsiString; var APort: Word); overload;
    function FormatSockNameAnsi: AnsiString;
    function FormatSockName: string;

    function CanRead: Boolean;
    function CanWrite: Boolean;

  end;

  TServerSocket = class;

  TClientSocket = class(TSocket)
  private
    FServer: TServerSocket;
    FDisconnected: Boolean;
    FDisconnectError: Integer;

    function GetAvailable: Integer;
    function GetDisconnected: Boolean;

  public
    constructor Create; overload;
    constructor Create(AServer: TServerSocket; AHandle: Winapi.WinSock.TSocket); overload;

    property Server: TServerSocket read FServer;

    procedure Connect(AAddress: string; APort: Word); overload;
    procedure Connect(AAddress: AnsiString; APort: Word); overload;
    procedure Connect(AAddress, AService: string); overload;
    procedure Connect(AAddress, AService: AnsiString); overload;

    procedure GetPeerName(var AAddress, AService: AnsiString); overload;
    procedure GetPeerName(var AAddress: AnsiString; var APort: Word); overload;
    procedure GetPeerName(var AAddress, AService: string); overload;
    procedure GetPeerName(var AAddress: string; var APort: Word); overload;
    function FormatPeerNameAnsi: AnsiString;
    function FormatPeerName: string;

    procedure SendBuffer(const AData; ASize: Integer);
    procedure Send<T>(const AData: T);
    procedure SendBytes(ABytes: TBytes);

    function CanReceive: Boolean; inline;
    property Available: Integer read GetAvailable;
    procedure ReceiveBuffer(var AData; ASize: Integer);
    procedure Receive<T>(var AData: T); overload;
    function Receive<T>: T; overload;
    function ReceiveAll: TBytes;
    function ReceiveBytes(ASize: Integer): TBytes; inline;

    /// <summary>Wether the client disconnected.</summary>
    /// <remarks>Does NOT raise an exception. On non-graceful disconnect DisconnectError is set instead.</remarks>
    property Disconnected: Boolean read GetDisconnected;
    /// <summary>The error, which caused the client to disconnect or zero if none.</summary>
    property DisconnectError: Integer read FDisconnectError;

  end;

  TServerSocket = class(TSocket)
  private
    FClients: IObjectList<TClientSocket>;
    FOnConnect: TEvent<TClientSocket>;
    FOnDisconnect: TEvent<TClientSocket>;

    function GetClients: IReadonlyList<TClientSocket>;
    function GetOnConnect: TEvent<TClientSocket>.TAccess;
    function GetOnDisconnect: TEvent<TClientSocket>.TAccess;

  public
    constructor Create;

    property Clients: IReadonlyList<TClientSocket> read GetClients;

    procedure Bind(AAddress: string; APort: Word); overload;
    procedure Bind(AAddress: AnsiString; APort: Word); overload;
    procedure Bind(AAddress: string = ''; AService: string = '0'); overload;
    procedure Bind(AAddress: AnsiString; AService: AnsiString = '0'); overload;

    procedure Listen(ABacklog: Integer = SOMAXCONN);

    function CanAccept: Boolean; inline;
    function Accept: TClientSocket;
    function AcceptAll: Integer;

    procedure CleanupDisconnectedClients;

    property OnConnect: TEvent<TClientSocket>.TAccess read GetOnConnect;
    property OnDisconnect: TEvent<TClientSocket>.TAccess read GetOnDisconnect;

  end;

  TSocketStream = class(TStream)
  private
    FWriteBuffer: TMemoryStream;
    FReadBuffer: TMemoryStream;
    FClient: TClientSocket;

  public
    constructor Create(AClient: TClientSocket);
    destructor Destroy; override;

    procedure Update;
    procedure Flush;

    function Read(var Buffer; Count: Integer): Integer; override;
    function Write(const Buffer; Count: Integer): Integer; override;
    function Seek(Offset: Integer; Origin: Word): Integer; override;

    property Client: TClientSocket read FClient;

  end;

implementation

// Missing external methods in Winapi.WinSock

type

  PAddrInfoA = ^TAddrInfoA;

  PPAddrInfoA = ^PAddrInfoA;

  TAddrInfoA = packed record
    ai_flags: Integer; // AI_PASSIVE, AI_CANONNAME, AI_NUMERICHOST
    ai_family: Integer; // PF_xxx
    ai_socktype: Integer; // SOCK_xxx
    ai_protocol: Integer; // 0 or IPPROTO_xxx for IPv4 and IPv6
    ai_addrlen: NativeUInt; // Length of ai_addr
    ai_canonname: PAnsiChar; // Canonical name for nodename
    ai_addr: PSockAddr; // Binary address
    ai_next: PAddrInfoA; // Next structure in linked list
  end;

  PAddrInfoW = ^TAddrInfoW;

  PPAddrInfoW = ^PAddrInfoW;

  TAddrInfoW = packed record
    ai_flags: Integer; // AI_PASSIVE, AI_CANONNAME, AI_NUMERICHOST
    ai_family: Integer; // PF_xxx
    ai_socktype: Integer; // SOCK_xxx
    ai_protocol: Integer; // 0 or IPPROTO_xxx for IPv4 and IPv6
    ai_addrlen: NativeUInt; // Length of ai_addr
    ai_canonname: PChar; // Canonical name for nodename
    ai_addr: PSockAddr; // Binary address
    ai_next: PAddrInfoW; // Next structure in linked list
  end;

const
  // winsocket = 'wsock32.dll';
  winsocket2 = 'ws2_32.dll';

  // Flags used in "hints" argument to getaddrinfo()
  // - AI_ADDRCONFIG is supported starting with Vista
  // - default is AI_ADDRCONFIG ON whether the flag is set or not
  // because the performance penalty in not having ADDRCONFIG in
  // the multi-protocol stack environment is severe;
  // this defaulting may be disabled by specifying the AI_ALL flag,
  // in that case AI_ADDRCONFIG must be EXPLICITLY specified to
  // enable ADDRCONFIG behavior
  AI_PASSIVE = $00000001; // Socket address will be used in bind() call
  AI_CANONNAME = $00000002; // Return canonical name in first ai_canonname
  AI_NUMERICHOST = $00000004; // Nodename must be a numeric address string
  AI_NUMERICSERV = $00000008; // Servicename must be a numeric port number
  AI_DNS_ONLY = $00000010; // Restrict queries to unicast DNS only (no LLMNR, netbios, etc.)

  AI_ALL = $00000100; // Query both IP6 and IP4 with AI_V4MAPPED
  AI_ADDRCONFIG = $00000400; // Resolution only if global address configured
  AI_V4MAPPED = $00000800; // On v6 failure, query v4 and convert to V4MAPPED format

  AI_NON_AUTHORITATIVE = $00004000; // LUP_NON_AUTHORITATIVE
  AI_SECURE = $00008000; // LUP_SECURE
  AI_RETURN_PREFERRED_NAMES = $00010000; // LUP_RETURN_PREFERRED_NAMES

  AI_FQDN = $00020000; // Return the FQDN in ai_canonname
  AI_FILESERVER = $00040000; // Resolving fileserver name resolution
  AI_DISABLE_IDN_ENCODING = $00080000; // Disable Internationalized Domain Names handling
  AI_EXTENDED = $80000000; // Indicates this is extended ADDRINFOEX(2/..) struct
  AI_RESOLUTION_HANDLE = $40000000; // Request resolution handle

  // Flags for getnameinfo()
  NI_NOFQDN = $01; // Only return nodename portion for local hosts
  NI_NUMERICHOST = $02; // Return numeric form of the host's address
  NI_NAMEREQD = $04; // Error if the host's name not in DNS
  NI_NUMERICSERV = $08; // Return numeric form of the service (port #)
  NI_DGRAM = $10; // Service is a datagram service

  NI_MAXHOST = 1025; // Max size of a fully-qualified domain name
  NI_MAXSERV = 32; // Max size of a service name

function GetAddrInfoA(
  pNodeName: PAnsiChar;
  pServiceName: PAnsiChar;
  pHints: PAddrInfoA;
  ppResult: PPAddrInfoA): Integer; stdcall; external winsocket2 name 'getaddrinfo';

function GetAddrInfoW(
  pNodeName: PChar;
  pServiceName: PChar;
  pHints: PAddrInfoW;
  ppResult: PPAddrInfoW): Integer; stdcall; external winsocket2;

procedure FreeAddrInfoA(pAddrInfo: PAddrInfoA); stdcall; external winsocket2 name 'freeaddrinfo';
procedure FreeAddrInfoW(pAddrInfo: PAddrInfoW); stdcall; external winsocket2;

function GetNameInfoA(
  pSockaddr: PSOCKADDR;
  SockaddrLength: Integer;
  pNodeBuffer: PAnsiChar;
  NodeBufferSize: Cardinal;
  pServiceBuffer: PAnsiChar;
  ServiceBufferSize: Cardinal;
  Flags: Integer): Integer; stdcall; external winsocket2 name 'getnameinfo';

function GetNameInfoW(
  pSockaddr: PSOCKADDR;
  SockaddrLength: Integer;
  pNodeBuffer: PChar;
  NodeBufferSize: Cardinal;
  pServiceBuffer: PChar;
  ServiceBufferSize: Cardinal;
  Flags: Integer): Integer; stdcall; external winsocket2;

{ TSocket }

procedure TSocket.SetBlocking(const Value: Boolean);
var
  Arg: Integer;
begin
  if Blocking = Value then
    Exit;
  FBlocking := Value;
  Arg := Ord(Value);
  if Winapi.WinSock.ioctlsocket(Handle, FIONBIO, Arg) = SOCKET_ERROR then
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
  Create(Winapi.WinSock.socket(Ord(AddressFamily), Ord(SocketType), Ord(ProtocolType)));
  if Handle = INVALID_SOCKET then
    RaiseLastError;
end;

procedure TSocket.GetName(AFunc: TGetNameFunc; var AAddress, AService: AnsiString; AFlags: Integer);
var
  Name: TSockAddr;
  NameLen: Integer;
begin
  NameLen := SizeOf(Name);
  if AFunc(Handle, Name, NameLen) <> 0 then
    RaiseLastError;
  SetLength(AAddress, NI_MAXHOST);
  SetLength(AService, NI_MAXSERV);
  if GetNameInfoA(@Name, NameLen, @AAddress[1], NI_MAXHOST, @AService[1], NI_MAXSERV, AFlags) <> 0 then
    RaiseLastError;
  SetLength(AAddress, Length(PAnsiChar(AAddress)));
  SetLength(AService, Length(PAnsiChar(AService)));
end;

procedure TSocket.GetName(AFunc: TGetNameFunc; var AAddress, AService: string; AFlags: Integer);
var
  Name: TSockAddr;
  NameLen: Integer;
begin
  NameLen := SizeOf(Name);
  if AFunc(Handle, Name, NameLen) <> 0 then
    RaiseLastError;
  SetLength(AAddress, NI_MAXHOST);
  SetLength(AService, NI_MAXSERV);
  if GetNameInfoW(@Name, NameLen, @AAddress[1], NI_MAXHOST, @AService[1], NI_MAXSERV, AFlags) <> 0 then
    RaiseLastError;
  SetLength(AAddress, Length(PChar(AAddress)));
  SetLength(AService, Length(PChar(AService)));
end;

class constructor TSocket.Create;
var
  Err: Integer;
  Msg: string;
begin
  Err := WSAStartup($202, FWSAData);
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

procedure TSocket.GetSockName(var AAddress, AService: string);
begin
  GetName(@Winapi.Winsock.getsockname, AAddress, AService, 0);
end;

procedure TSocket.GetSockName(var AAddress: string; var APort: Word);
var
  Service: string;
begin
  GetName(@Winapi.Winsock.getsockname, AAddress, Service, NI_NUMERICSERV);
  APort := Service.ToInteger;
end;

procedure TSocket.GetSockName(var AAddress, AService: AnsiString);
begin
  GetName(@Winapi.Winsock.getsockname, AAddress, AService, 0);
end;

procedure TSocket.GetSockName(var AAddress: AnsiString; var APort: Word);
var
  Service: AnsiString;
begin
  GetName(@Winapi.Winsock.getsockname, AAddress, Service, NI_NUMERICSERV);
  APort := StrToInt(string(Service));
end;

function TSocket.FormatSockNameAnsi: AnsiString;
var
  Address, Service: AnsiString;
begin
  GetName(@Winapi.Winsock.getsockname, Address, Service, 0);
  Result := AnsiString(Format('%s:%s', [Address, Service]));
end;

function TSocket.FormatSockName: string;
var
  Address, Service: string;
begin
  GetName(@Winapi.Winsock.getsockname, Address, Service, 0);
  Result := Format('%s:%s', [Address, Service]);
end;

function TSocket.CanRead: Boolean;
const
  Timeout: TTimeVal = (tv_sec: 0; tv_usec: 0);
var
  FDSet: TFDSet;
begin
  FD_ZERO(FDSet);
  FD_SET(Handle, FDSet);
  Result := Winapi.WinSock.select(0, @FDSet, nil, nil, @Timeout) <> 0;
end;

function TSocket.CanWrite: Boolean;
const
  Timeout: TTimeVal = (tv_sec: 0; tv_usec: 0);
var
  FDSet: TFDSet;
begin
  FD_ZERO(FDSet);
  FD_SET(Handle, FDSet);
  Result := Winapi.WinSock.select(0, nil, @FDSet, nil, @Timeout) <> 0;
end;

{ TClientSocket }

function TClientSocket.GetAvailable: Integer;
begin
  Winapi.WinSock.ioctlsocket(Handle, FIONREAD, Result);
end;

function TClientSocket.GetDisconnected: Boolean;
var
  Buffer: Byte;
  Err: Integer;
begin
  if FDisconnected then
    Exit(True);
  if not CanReceive then
    Exit(False);
  // SOCKET_ERROR (-1) on error, 0 on graceful disconnect
  Err := Winapi.WinSock.recv(Handle, Buffer, 1, MSG_PEEK);
  // Do not raise in this getter, just set the disconnection error, which can be analyzed manually
  if Err = SOCKET_ERROR then
  begin
    FDisconnectError := WSAGetLastError;
    FDisconnected := True;
    Exit(True);
  end;
  Result := Err = 0;
  FDisconnected := Result;
end;

constructor TClientSocket.Create;
begin
  inherited Create(afInterNetwork, stStream, ptTcp);
end;

constructor TClientSocket.Create(AServer: TServerSocket; AHandle: Winapi.WinSock.TSocket);
begin
  inherited Create(AHandle);
  FServer := AServer;
end;

procedure TClientSocket.Connect(AAddress: string; APort: Word);
begin
  Connect(AAddress, APort.ToString);
end;

procedure TClientSocket.Connect(AAddress: AnsiString; APort: Word);
begin
  Connect(AAddress, AnsiString(APort.ToString));
end;

procedure TClientSocket.Connect(AAddress, AService: string);
var
  Hints: TAddrInfoW;
  AddrInfo: PAddrInfoW;
  Err: Integer;
begin
  FillChar(Hints, SizeOf(Hints), 0);
  Hints.ai_family := Ord(AddressFamily);
  Hints.ai_socktype := Ord(SocketType);
  Hints.ai_protocol := Ord(ProtocolType);

  Err := GetAddrInfoW(PChar(AAddress), PChar(AService), @Hints, @AddrInfo);
  if Err <> 0 then
    raise ESocketError.Create(FormatError(Err));

  try
    if Winapi.WinSock.connect(Handle, AddrInfo.ai_addr^, AddrInfo.ai_addrlen) <> 0 then
      RaiseLastError;
  finally
    FreeAddrInfoW(AddrInfo);
  end;
end;

procedure TClientSocket.Connect(AAddress, AService: AnsiString);
var
  Hints: TAddrInfoA;
  AddrInfo: PAddrInfoA;
  Err: Integer;
begin
  FillChar(Hints, SizeOf(Hints), 0);
  Hints.ai_family := Ord(AddressFamily);
  Hints.ai_socktype := Ord(SocketType);
  Hints.ai_protocol := Ord(ProtocolType);

  Err := GetAddrInfoA(PAnsiChar(AAddress), PAnsiChar(AService), @Hints, @AddrInfo);
  if Err <> 0 then
    raise ESocketError.Create(FormatError(Err));

  try
    if Winapi.WinSock.connect(Handle, AddrInfo.ai_addr^, AddrInfo.ai_addrlen) <> 0 then
      RaiseLastError;
  finally
    FreeAddrInfoA(AddrInfo);
  end;
end;

procedure TClientSocket.GetPeerName(var AAddress, AService: AnsiString);
begin
  GetName(@Winapi.Winsock.getpeername, AAddress, AService, 0);
end;

procedure TClientSocket.GetPeerName(var AAddress: AnsiString; var APort: Word);
var
  Service: AnsiString;
begin
  GetName(@Winapi.Winsock.getpeername, AAddress, Service, NI_NUMERICSERV);
  APort := StrToInt(string(Service));
end;

procedure TClientSocket.GetPeerName(var AAddress, AService: string);
begin
  GetName(@Winapi.Winsock.getpeername, AAddress, AService, 0);
end;

procedure TClientSocket.GetPeerName(var AAddress: string; var APort: Word);
var
  Service: string;
begin
  GetName(@Winapi.Winsock.getpeername, AAddress, Service, NI_NUMERICSERV);
  APort := Service.ToInteger;
end;

function TClientSocket.FormatPeerNameAnsi: AnsiString;
var
  Address, Service: AnsiString;
begin
  GetName(@Winapi.Winsock.getpeername, Address, Service, 0);
  Result := AnsiString(Format('%s:%s', [Address, Service]));
end;

function TClientSocket.FormatPeerName: string;
var
  Address, Service: string;
begin
  GetName(@Winapi.Winsock.getpeername, Address, Service, 0);
  Result := Format('%s:%s', [Address, Service]);
end;

procedure TClientSocket.SendBuffer(const AData; ASize: Integer);
var
  Sent, Offset: Integer;
begin
  Offset := 0;
  while Offset < ASize do
  begin
    Sent := Winapi.WinSock.send(Handle, PByte(@AData)[Offset], ASize, 0);
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
begin
  Result := CanRead;
end;

procedure TClientSocket.ReceiveBuffer(var AData; ASize: Integer);
var
  Received, Offset: Integer;
begin
  Offset := 0;
  while Offset < ASize do
  begin
    Received := Winapi.WinSock.recv(Handle, PByte(@AData)[Offset], ASize, 0);
    if (Received = 0) and (ASize > 0) then
    begin
      FDisconnected := True;
      Break;
    end
    else if Received = SOCKET_ERROR then
      RaiseLastError;
    Inc(Offset, Received);
  end;
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

function TServerSocket.GetOnConnect: TEvent<TClientSocket>.TAccess;
begin
  Result := FOnConnect.Access;
end;

function TServerSocket.GetOnDisconnect: TEvent<TClientSocket>.TAccess;
begin
  Result := FOnDisconnect.Access;
end;

constructor TServerSocket.Create;
begin
  inherited Create(afInterNetwork, stStream, ptTcp);
  FClients := TObjectList<TClientSocket>.Create;
end;

procedure TServerSocket.Listen(ABacklog: Integer);
begin
  if Winapi.WinSock.listen(Handle, ABacklog) = SOCKET_ERROR then
    RaiseLastError;
end;

function TServerSocket.CanAccept: Boolean;
begin
  Result := CanRead;
end;

procedure TServerSocket.CleanupDisconnectedClients;
var
  I: Integer;
begin
  for I := FClients.MaxIndex downto 0 do
  begin
    if FClients[I].Disconnected then
    begin
      FOnDisconnect.Execute(FClients[I]);
      FClients.RemoveAt(I);
    end;
  end;
end;

function TServerSocket.Accept: TClientSocket;
var
  AcceptedSocket: Winapi.WinSock.TSocket;
begin
  AcceptedSocket := Winapi.WinSock.accept(Handle, nil, nil);
  if AcceptedSocket = INVALID_SOCKET then
    RaiseLastError;
  Result := TClientSocket.Create(AcceptedSocket);
  Result.FAddressFamily := AddressFamily;
  Result.FSocketType := SocketType;
  Result.FProtocolType := ProtocolType;
  FClients.Add(Result);
  FOnConnect.Execute(Result);
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

procedure TServerSocket.Bind(AAddress: string; APort: Word);
begin
  Bind(AAddress, APort.ToString);
end;

procedure TServerSocket.Bind(AAddress: AnsiString; APort: Word);
begin
  Bind(AAddress, AnsiString(APort.ToString));
end;

procedure TServerSocket.Bind(AAddress, AService: string);
var
  Hints: TAddrInfoW;
  AddrInfo: PAddrInfoW;
  Err: Integer;
begin
  FillChar(Hints, SizeOf(Hints), 0);
  Hints.ai_flags := AI_PASSIVE;
  Hints.ai_family := Ord(AddressFamily);
  Hints.ai_socktype := Ord(SocketType);
  Hints.ai_protocol := Ord(ProtocolType);

  Err := GetAddrInfoW(PChar(AAddress), PChar(AService), @Hints, @AddrInfo);
  if Err <> 0 then
    raise ESocketError.Create(FormatError(Err));

  try
    if Winapi.WinSock.bind(Handle, AddrInfo.ai_addr^, AddrInfo.ai_addrlen) <> 0 then
      RaiseLastError;
  finally
    FreeAddrInfoW(AddrInfo);
  end;
end;

procedure TServerSocket.Bind(AAddress, AService: AnsiString);
var
  Hints: TAddrInfoA;
  AddrInfo: PAddrInfoA;
  Err: Integer;
begin
  FillChar(Hints, SizeOf(Hints), 0);
  Hints.ai_flags := AI_PASSIVE;
  Hints.ai_family := Ord(AddressFamily);
  Hints.ai_socktype := Ord(SocketType);
  Hints.ai_protocol := Ord(ProtocolType);

  Err := GetAddrInfoA(PAnsiChar(AAddress), PAnsiChar(AService), @Hints, @AddrInfo);
  if Err <> 0 then
    raise ESocketError.Create(FormatError(Err));

  try
    if Winapi.WinSock.bind(Handle, AddrInfo.ai_addr^, AddrInfo.ai_addrlen) <> 0 then
      RaiseLastError;
  finally
    FreeAddrInfoA(AddrInfo);
  end;
end;

{ TSocketStream }

constructor TSocketStream.Create(AClient: TClientSocket);
begin
  FWriteBuffer := TMemoryStream.Create;
  FReadBuffer := TMemoryStream.Create;
  FClient := AClient;
end;

destructor TSocketStream.Destroy;
begin
  FWriteBuffer.Free;
  FReadBuffer.Free;
  inherited;
end;

procedure TSocketStream.Update;
var
  Bytes: TBytes;
  OldPosition: Int64;
begin
  Bytes := FClient.ReceiveAll;
  OldPosition := FReadBuffer.Position;
  FReadBuffer.Seek(0, soEnd);
  FReadBuffer.Write(Bytes, Length(Bytes));
  FReadBuffer.Position := OldPosition;
end;

procedure TSocketStream.Flush;
begin
  FClient.SendBuffer(FWriteBuffer.Memory^, FWriteBuffer.Size);
  FWriteBuffer.Clear;
end;

function TSocketStream.Read(var Buffer; Count: Integer): Integer;
var
  Received: Integer;
begin
  Result := 0;
  repeat
    Received := FReadBuffer.Read(PByte(@Buffer)[Result], Count);
    Inc(Result, Received);
    Dec(Count, Received);
    if Count = 0 then
      Break;
    Update;
  until FClient.Disconnected;
  if FReadBuffer.Position = FReadBuffer.Size then
    FReadBuffer.Clear;
end;

function TSocketStream.Write(const Buffer; Count: Integer): Integer;
begin
  Result := FWriteBuffer.Write(Buffer, Count);
end;

function TSocketStream.Seek(Offset: Integer; Origin: Word): Integer;
begin
  Result := 0;
end;

end.
