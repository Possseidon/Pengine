unit SocketDefine;

interface

uses
  System.SysUtils,

  Winapi.Windows,
  Winapi.WinSock;

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

    class function MakeSockAddr(AAddressFamily: TAddressFamily; AAddress: Integer; APort: Word): TSockAddr; static;

    function GetAvailable: Integer;

  public
    class constructor Create;
    class destructor Destroy;

    constructor Create(AAddressFamily: TAddressFamily; ASocketType: TSocketType; AProtocolType: TProtocolType);
    destructor Destroy; override;

    class function FormatError(AError: Integer): string; static;
    class procedure RaiseLastError; static;

    procedure Listen(ABacklog: Integer = SOMAXCONN);

    procedure Bind(AAddress: Integer; APort: Word = 0); overload;
    procedure Bind(AAddress: string; APort: Word = 0); overload;

    procedure Connect(AAddress: Integer; APort: Word = 0); overload;
    procedure Connect(AAddress: string; APort: Word = 0); overload;

    procedure GetSockName(out AAddress: Integer; out APort: Word); overload;
    procedure GetSockName(out AAddress: string; out APort: Word); overload;

    property Available: Integer read GetAvailable;
    
    procedure SendBuffer(const AData; ASize: Integer); 
    procedure Send<T>(const AData: T); 
    procedure SendBytes(ABytes: TBytes);

    procedure ReceiveBuffer(var AData; ASize: Integer);
    procedure Receive<T>(var AData: T); 
    function ReceiveAll(ABlocking: Boolean = False): TBytes; 
    function ReceiveBytes(ASize: Integer): TBytes; inline;
    
    property Handle: Winapi.WinSock.TSocket read FHandle;

    property AddressFamily: TAddressFamily read FAddressFamily;
    property SocketType: TSocketType read FSocketType;
    property ProtocolType: TProtocolType read FProtocolType;

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

function TSocket.GetAvailable: Integer;
begin
  ioctlsocket(Handle, FIONREAD, Result);
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

destructor TSocket.Destroy;
begin
  closesocket(Handle);
end;

constructor TSocket.Create(AAddressFamily: TAddressFamily; ASocketType: TSocketType; AProtocolType: TProtocolType);
begin
  FAddressFamily := AAddressFamily;
  FSocketType := ASocketType;
  FProtocolType := AProtocolType;
  FHandle := socket(Ord(AddressFamily), Ord(SocketType), Ord(ProtocolType));
  if Handle = INVALID_SOCKET then
    RaiseLastError;
end;

class destructor TSocket.Destroy;
begin
  WSACleanup;
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

procedure TSocket.Listen(ABacklog: Integer);
begin
  if Winapi.WinSock.Listen(Handle, ABacklog) = SOCKET_ERROR then
    RaiseLastError;
end;

procedure TSocket.Bind(AAddress: Integer; APort: Word);
var
  SockAddr: TSockAddr;
begin
  SockAddr := MakeSockAddr(AddressFamily, AAddress, APort);
  if Winapi.WinSock.Bind(Handle, SockAddr, SizeOf(SockAddr)) = SOCKET_ERROR then
    RaiseLastError;
end;

procedure TSocket.Bind(AAddress: string; APort: Word);
begin
  Bind(inet_addr(PAnsiChar(AnsiString(AAddress))), APort);
end;

procedure TSocket.Connect(AAddress: Integer; APort: Word);
var
  SockAddr: TSockAddr;
begin
  SockAddr := MakeSockAddr(AddressFamily, AAddress, APort);
  if Winapi.WinSock.Connect(Handle, SockAddr, SizeOf(SockAddr)) = SOCKET_ERROR then
    RaiseLastError;
end;

procedure TSocket.Connect(AAddress: string; APort: Word);
begin
  Connect(inet_addr(PAnsiChar(AnsiString(AAddress))), APort);
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
  AddressInt: in_addr;
begin
  GetSockName(AddressInt.S_addr, APort);
  AAddress := string(inet_ntoa(AddressInt));
end;

procedure TSocket.SendBuffer(const AData; ASize: Integer);
var
  Sent, Offset: Integer;
begin
  Offset := 0;
  while Offset < ASize do
  begin
    Sent := Winapi.WinSock.send(Handle, (PByte(@AData) + Offset)^, ASize, 0);
    if Sent = SOCKET_ERROR then
      RaiseLastError;
    Inc(Offset, Sent);
  end;
end;

procedure TSocket.SendBytes(ABytes: TBytes);
begin
  SendBuffer(ABytes[0], Length(ABytes));
end;

procedure TSocket.Send<T>(const AData: T);
begin
  SendBuffer(AData, SizeOf(T));
end;

procedure TSocket.ReceiveBuffer(var AData; ASize: Integer);
begin
  Winapi.WinSock.recv(Handle, AData, ASize, 0);
end;

function TSocket.ReceiveAll(ABlocking: Boolean): TBytes;
var
  AvailableData: Integer;
begin
  AvailableData := Available;
  if AvailableData = 0 then
  begin
    if not ABlocking then
      Exit(nil);
    repeat
      AvailableData := Available;
    until AvailableData > 0;
  end;
  Result := ReceiveBytes(AvailableData);
end;

procedure TSocket.Receive<T>(var AData: T);
begin
  ReceiveBuffer(AData, SizeOf(AData));
end;

function TSocket.ReceiveBytes(ASize: Integer): TBytes;
begin
  SetLength(Result, ASize);
  ReceiveBuffer(Result[0], ASize);
end;

end.
