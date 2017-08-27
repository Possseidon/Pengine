unit Lua;

interface

uses
  LuaHeader, Lists, LuaConf, SyncObjs, Classes, Windows, TimeManager, Dialogs;

type

  { TLua }

  TLua = class
  private type

    { TLuaThread }

    TLuaThread = class(TThread)
    private
      FLua: TLua;
      FParams: Integer;
      FResults: Integer;
      FError: TLuaPCallError;
    protected
      procedure Execute; override;
    public
      constructor Create(ALua: TLua; AParams, AResults: Integer);
      procedure ForceKill;
      property Error: TLuaPCallError read FError;
    end;

    { TAllocationList }

    TAllocationList = class(TSet<Pointer>)
    protected
      function GetKeyHash(AKey: Pointer): Cardinal; override;
      class function CantIndex(AKey: Pointer): Boolean; override;
      class function KeysEqual(AKey1, AKey2: Pointer): Boolean; override;
    public
      destructor Destroy; override;

      procedure PerformCleanup;
    end;

  private
    FL: TLuaState;
    FAllocations: TAllocationList;
    FMemoryLimit: Integer;
    FMemoryUsage: Integer;
    FLock: TCriticalSection;

    class function Alloc(ud, ptr: Pointer; osize, nsize: NativeUInt): Pointer; static; cdecl;

    procedure MakeLuaState;

  public
    constructor Create;
    class function FromState(AL: TLuaState): TLua; static;
    destructor Destroy; override;

    property L: TLuaState read FL;
    property MemoryLimit: Integer read FMemoryLimit write FMemoryLimit;
    property MemoryUsage: Integer read FMemoryUsage;

    procedure Interlock;
    procedure Unlock;

    function LCall(AParams, AResults: Integer; ATimeout: Single; out AError: TLuaPCallError): Boolean;

  end;

implementation

{ TLua }

class function TLua.Alloc(ud, ptr: Pointer; osize, nsize: NativeUInt): Pointer;
var
  Self: TLua;
begin
  Self := TLua(ud);
  Self.Interlock;
  if nsize = 0 then
  begin
    if ptr <> nil then
      Self.FAllocations.Del(ptr);
    FreeMemory(ptr);
    Result := nil;
  end
  else
  begin
    Result := ReallocMemory(ptr, nsize);
    if Result <> nil then
    begin
      if ptr <> nil then
        Self.FAllocations.Del(ptr);
      Self.FAllocations.Add(Result);
    end;
  end;
  Self.Unlock;
end;

constructor TLua.Create;
begin
  FLock := TCriticalSection.Create;
  FAllocations := TAllocationList.Create(12289);
  MakeLuaState;
end;

destructor TLua.Destroy;
begin
  L.Close;
  FAllocations.Free;
  FLock.Free;
  inherited;
end;

class function TLua.FromState(AL: TLuaState): TLua;
begin
  Result := TLua(AL.GetExtraSpace^);
end;

function TLua.LCall(AParams, AResults: Integer; ATimeout: Single; out AError: TLuaPCallError): Boolean;
var
  Thread: TLuaThread;
  StopWatch: TStopWatch;
begin
  Result := False;
  StopWatch.Start;
  Thread := TLuaThread.Create(Self, AParams, AResults);
  try
    while not Thread.Started do TThread.Yield;
    // Result := WaitForSingleObject(Thread.Handle, ATimeout) = WAIT_OBJECT_0;
    while StopWatch.Time < ATimeOut do
    begin
      TThread.Yield;
      if Thread.Finished then
      begin
        AError := Thread.Error;
        Exit(True);
      end;
    end;

    Thread.ForceKill;
    FAllocations.PerformCleanup;
    MakeLuaState;

  finally
    Thread.Free;
  end;
end;

procedure TLua.MakeLuaState;
begin
  FL := NewLuaState(Alloc, Self);
  PPointer(FL.GetExtraSpace)^ := Self;
end;

procedure TLua.Interlock;
begin
  if not FLock.TryEnter then
    while True do
      TThread.Yield;
end;

procedure TLua.Unlock;
begin
  FLock.Leave;
end;

{ TLua.TAllocationList }

class function TLua.TAllocationList.CantIndex(AKey: Pointer): Boolean;
begin
  Result := AKey = nil;
end;

destructor TLua.TAllocationList.Destroy;
begin
  PerformCleanup;
  inherited;
end;

function TLua.TAllocationList.GetKeyHash(AKey: Pointer): Cardinal;
begin
  Result := NativeUInt(AKey) mod FInternalSize;
end;

class function TLua.TAllocationList.KeysEqual(AKey1, AKey2: Pointer): Boolean;
begin
  Result := AKey1 = AKey2;
end;

procedure TLua.TAllocationList.PerformCleanup;
var
  P: Pointer;
begin
  for P in Self do
    FreeMemory(P);
  Clear;
end;

{ TLua.TLuaThread }

constructor TLua.TLuaThread.Create(ALua: TLua; AParams, AResults: Integer);
begin
  inherited Create;
  FLua := ALua;
  FParams := AParams;
  FResults := AResults;
end;

procedure TLua.TLuaThread.Execute;
var
  Err: TLuaPCallError;
begin
  Err := FLua.L.PCall(FParams, FResults, 0);
  FLua.Interlock;
  FError := Err;
  FLua.Unlock;
end;

procedure TLua.TLuaThread.ForceKill;
begin
  FLua.FLock.Enter;
  TerminateThread(Handle, 0);
  FLua.FLock.Leave;
end;

end.
