unit Pengine.Lua;

interface

uses
  Winapi.Windows,

  System.SysUtils,
  System.SyncObjs,
  System.Classes,
  System.IOUtils,

  Pengine.LuaHeader,
  Pengine.Collections,
  Pengine.Hasher,
  Pengine.HashCollections,
  Pengine.LuaConf,
  Pengine.TimeManager,
  Pengine.DebugConsole;

type

  TLuaLib = class abstract
  protected type

    TTableEntry = class;

    { TEntry }

    TEntry = class abstract
    private
      FName: TLuaString;
      FParent: TTableEntry;

      procedure PushValue(AL: TLuaState);

    public
      constructor Create(AName: TLuaString);

      property Name: TLuaString read FName;

      procedure RegisterEntry(AL: TLuaState); virtual;
      procedure UnregisterEntry(AL: TLuaState); virtual;
    end;

    { TFunctionEntry }

    TFunctionEntry = class(TEntry)
    private
      FFunc: TLuaCFunction;

      class function WrapperFunc(L: TLuaState): Integer; static; cdecl;

    public
      constructor Create(AName: TLuaString; AFunc: TLuaCFunction);

      property Func: TLuaCFunction read FFunc;

      procedure RegisterEntry(AL: TLuaState); override;

      class procedure PushWrappedFunction(AL: TLuaState; AFunc: TLuaCFunction); static;

    end;

    { TStringEntry }

    TStringEntry = class(TEntry)
    private
      FValue: TLuaString;
    public
      constructor Create(AName, AValue: TLuaString);

      property Value: TLuaString read FValue;

      procedure RegisterEntry(AL: TLuaState); override;
    end;

    { TNumberEntry }

    TNumberEntry = class(TEntry)
    private
      FValue: TLuaNumber;
    public
      constructor Create(AName: TLuaString; AValue: TLuaNumber);

      property Value: TLuaNumber read FValue;

      procedure RegisterEntry(AL: TLuaState); override;
    end;

    { TIntegerEntry }

    TIntegerEntry = class(TEntry)
    private
      FValue: TLuaInteger;
    public
      constructor Create(AName: TLuaString; AValue: TLuaInteger);

      property Value: TLuaInteger read FValue;

      procedure RegisterEntry(AL: TLuaState); override;
    end;

    TRecursiveTableEntry = class;

    { TTableEntry }

    TTableEntry = class(TEntry)
    private
      FEntries: TRefArray<TEntry>;
        function GetEntry(AIndex: Integer): TEntry;

    public
      constructor Create(AName: TLuaString = '');
      destructor Destroy; override;

      function EntryCount: Integer;
      property Entries[AIndex: Integer]: TEntry read GetEntry;

      procedure Add(AEntry: TEntry); overload;
      procedure Add(AName: TLuaString; AFunc: TLuaCFunction); overload;
      procedure Add(AName: TLuaString; AString: TLuaString); overload;
      procedure Add(AName: TLuaString; ANumber: TLuaNumber); overload;
      procedure Add(AName: TLuaString; AInteger: TLuaInteger); overload;
      function Add(AName: TLuaString): TTableEntry; overload;

      procedure AddRecursion(AName: AnsiString; ATable: TTableEntry);

      procedure RegisterEntry(AL: TLuaState); override;
      procedure UnregisterEntry(AL: TLuaState); override;
    end;

    { TRecursiveTableEntry }

    TRecursiveTableEntry = class(TEntry)
    private
      FTable: TTableEntry;
    public
      constructor Create(AName: AnsiString; ATable: TTableEntry);

      procedure RegisterEntry(AL: TLuaState); override;
    end;

  private
    FL: TLuaState;
    FEntry: TTableEntry;

  protected
    property L: TLuaState read FL;

    class function LuaNotImplemented(L: TLuaState): Integer; static; cdecl;

    class procedure CreateEntry(AEntry: TTableEntry); virtual; abstract;

  public
    constructor Create(AL: TLuaState); virtual;
    destructor Destroy; override;

    procedure ChangeLuaState(AL: TLuaState); virtual;

    class procedure PushFunc(AL: TLuaState; AFunc: TLuaCFunction); static;

  end;

  TLuaLibClass = class of TLuaLib;

  { TLibNotLoaded }

  ELibNotLoaded = class(Exception)
  public
    constructor Create(ALib: TLuaLibClass);
  end;

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
      FDone: Boolean;
      FStartWorkEvent: TEvent;
    protected
      procedure Execute; override;
    public
      constructor Create(ALua: TLua);
      destructor Destroy; override;

      procedure Start(AParams, AResults: Integer);
      procedure ForceKill;
      property Error: TLuaPCallError read FError;
      property Done: Boolean read FDone;
    end;

    { TAllocationList }

    TAllocationList = class(TSet<Pointer, TPointerHasher>)
    private
      FOwnsPointers: Boolean;
      FStoredOwnsPointers: Boolean;

    protected
      procedure UnownObjects; override;
      procedure ReownObjects; override;

    public
      // don't override, so that copy will be referenced
      constructor Create(AHashMode: THashMode = hmAuto); reintroduce;
      destructor Destroy; override;

      procedure PerformCleanup;

    end;

  private
    FL: TLuaState;
    FAllocations: TAllocationList;
    FMemoryLimit: NativeUInt;
    FMemoryUsage: NativeUInt;
    FLock: TCriticalSection;
    FLockCounter: Integer;
    FThread: TLuaThread;
    FLibs: TClassRefMap<TLuaLib>;

    class function Alloc(ud, ptr: Pointer; osize, nsize: NativeUInt): Pointer; static; cdecl;

    procedure MakeLuaState;

  public
    constructor Create;
    class function FromState(AL: TLuaState): TLua; static;
    destructor Destroy; override;

    property L: TLuaState read FL;
    property MemoryLimit: NativeUInt read FMemoryLimit write FMemoryLimit;
    property MemoryUsage: NativeUInt read FMemoryUsage;

    function Lib<T: TLuaLib>: T;

    procedure Interlock;
    procedure Unlock;
    function ShouldTerminate: Boolean;

    function CallTimeout(AParams, AResults: Integer; ATimeout: Single; out AError: TLuaPCallError): Boolean;
    procedure CallUnlocked(AParams, AResults: Integer); inline;

    procedure AddLib(ALib: TLuaLibClass);
    procedure DelLib(ALib: TLuaLibClass);

  end;

implementation

{ TLua }

procedure TLua.AddLib(ALib: TLuaLibClass);
begin
  if FLibs.KeyExists(ALib) then
    raise Exception.Create('LuaLib is registered already');
  FLibs[ALib] := ALib.Create(L);
end;

class function TLua.Alloc(ud, ptr: Pointer; osize, nsize: NativeUInt): Pointer;
var
  Self: TLua;
begin
  Self := TLua(ud);
  Self.Interlock;
  try
    if nsize = 0 then
    begin
      if ptr <> nil then
      begin
        Self.FAllocations.Del(ptr);
        Dec(Self.FMemoryUsage, osize);
        FreeMemory(ptr);
      end;
      Result := nil;
    end
    else
    begin
      if Self.FMemoryLimit <> 0 then
      begin
        if ptr = nil then
        begin
          if Self.FMemoryUsage + nsize > Self.FMemoryLimit then
            Exit(nil);
        end
        else
        begin
          if Self.FMemoryUsage - osize + nsize > Self.FMemoryLimit then
            Exit(nil);
        end;
      end;

      Result := ReallocMemory(ptr, nsize);
      if Result <> nil then
      begin
        if ptr <> nil then
        begin
          Self.FAllocations.Del(ptr);
          Dec(Self.FMemoryUsage, osize);
        end;
        Self.FAllocations.Add(Result);
        Inc(Self.FMemoryUsage, nsize);
      end;
    end;
  finally
    Self.Unlock;
  end;
end;

constructor TLua.Create;
begin
  FLock := TCriticalSection.Create;
  FAllocations := TAllocationList.Create;
  FThread := TLuaThread.Create(Self);
  MakeLuaState;
  FLibs := TClassRefMap<TLuaLib>.Create(True);
end;

procedure TLua.DelLib(ALib: TLuaLibClass);
begin
  FLibs.Del(ALib);
end;

destructor TLua.Destroy;
begin
  FLibs.Free;
  L.Close;
  FThread.Free;
  FAllocations.Free;
  FLock.Free;
  inherited;
end;

class function TLua.FromState(AL: TLuaState): TLua;
begin
  Result := TLua(AL.GetExtraSpace^);
end;

function TLua.CallTimeout(AParams, AResults: Integer; ATimeout: Single; out AError: TLuaPCallError): Boolean;
var
  StopWatch: TStopWatch;
  Pair: TPair<TClass, TLuaLib>;
begin
  StopWatch.Start;
  Result := False;
  while not FThread.Started do TThread.Yield;
  FThread.Start(AParams, AResults);
  while StopWatch.Time < ATimeOut do
  begin
    if FThread.Done then
    begin
      AError := FThread.Error;
      Exit(True);
    end;
    TThread.Yield;
  end;

  FThread.ForceKill;
  FAllocations.PerformCleanup;
  FMemoryUsage := 0;

  FThread.Free;
  FThread := TLuaThread.Create(Self);

  MakeLuaState;

  for Pair in FLibs do
    Pair.Value.ChangeLuaState(L);
end;

procedure TLua.CallUnlocked(AParams, AResults: Integer);
begin
  Unlock;
  L.Call(AParams, AResults);
  Interlock;
end;

procedure TLua.MakeLuaState;
begin
  FL := NewLuaState(Alloc, Self);
  PPointer(FL.GetExtraSpace)^ := Self;
end;

function TLua.ShouldTerminate: Boolean;
begin
  Result := FThread.Terminated;
end;

function TLua.Lib<T>: T;
var
  Lib: TLuaLib;
begin
  if FLibs.Get(T, Lib) then
    Exit(T(Lib));
  raise ELibNotLoaded.Create(T);
end;

procedure TLua.Interlock;
begin
  if (FLockCounter = 0) and not FLock.TryEnter then
    Sleep(INFINITE);
  Inc(FLockCounter);
end;

procedure TLua.Unlock;
begin
  Dec(FLockCounter);
  if FLockCounter = 0 then
  begin
    FLock.Leave;
    if ShouldTerminate then
      Sleep(INFINITE);
  end;
end;

{ TLua.TAllocationList }

procedure TLua.TAllocationList.UnownObjects;
begin
  FStoredOwnsPointers := FOwnsPointers;
  FOwnsPointers := False;
end;

procedure TLua.TAllocationList.ReownObjects;
begin
  FOwnsPointers := FStoredOwnsPointers;
end;

constructor TLua.TAllocationList.Create(AHashMode: THashMode);
begin
  inherited;
  FOwnsPointers := True;
end;

destructor TLua.TAllocationList.Destroy;
begin
  if FOwnsPointers then
    PerformCleanup;
  inherited;
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

constructor TLua.TLuaThread.Create(ALua: TLua);
begin
  inherited Create;
  FLua := ALua;
  FStartWorkEvent := TEvent.Create;
end;

destructor TLua.TLuaThread.Destroy;
begin
  Terminate;
  FStartWorkEvent.SetEvent;
  FStartWorkEvent.Free;
  inherited;
end;

procedure TLua.TLuaThread.Execute;
var
  Err: TLuaPCallError;
begin
  while True do
  begin
    FStartWorkEvent.WaitFor;
    if Terminated then
      Break;
    FStartWorkEvent.ResetEvent;

    Err := FLua.L.PCall(FParams, FResults, 0);

    FLua.Interlock;
    FError := Err;
    FDone := True;
    FLua.Unlock;
  end;
end;

procedure TLua.TLuaThread.ForceKill;
begin
  Terminate;
  FLua.FLock.Enter;
  TerminateThread(Handle, 0);
  FLua.FLock.Leave;
end;

procedure TLua.TLuaThread.Start(AParams, AResults: Integer);
begin
  FParams := AParams;
  FResults := AResults;
  FDone := False;
  FStartWorkEvent.SetEvent;
end;

{ TLuaLib }

procedure TLuaLib.ChangeLuaState(AL: TLuaState);
begin
  FL := AL;
  L.PushGlobalTable;
  FEntry.RegisterEntry(L);
  L.Pop;
end;

class function TLuaLib.LuaNotImplemented(L: TLuaState): Integer;
begin
  Result := L.Error('The function is not implemented!');
end;

constructor TLuaLib.Create(AL: TLuaState);
begin
  FEntry := TTableEntry.Create;
  CreateEntry(FEntry);
  ChangeLuaState(AL);
end;

destructor TLuaLib.Destroy;
begin
  L.PushGlobalTable;
  FEntry.UnregisterEntry(L);
  L.Pop;
  FEntry.Free;
  inherited;
end;

class procedure TLuaLib.PushFunc(AL: TLuaState; AFunc: TLuaCFunction);
begin
  TFunctionEntry.PushWrappedFunction(AL, @AFunc);
end;

{ TLuaLib.TEntry }

constructor TLuaLib.TEntry.Create(AName: AnsiString);
begin
  FName := AName;
end;

procedure TLuaLib.TEntry.PushValue(AL: TLuaState);
begin
  if FParent <> nil then
    FParent.PushValue(AL)
  else
    AL.PushGlobalTable;
  if Name <> '' then
  begin
    AL.GetField(PPAnsiChar(@Name)^);
    AL.Remove(-2);
  end;
end;

procedure TLuaLib.TEntry.RegisterEntry(AL: TLuaState);
begin
  AL.PushString(PPAnsiChar(@Name)^);
end;

procedure TLuaLib.TEntry.UnregisterEntry(AL: TLuaState);
begin
  AL.PushString(PPAnsiChar(@Name)^);
  AL.PushNil;
  AL.SetTable(-3);
end;

{ TLuaLib.TFunctionEntry }

constructor TLuaLib.TFunctionEntry.Create(AName: AnsiString; AFunc: TLuaCFunction);
begin
  inherited Create(AName);
  FFunc := AFunc;
end;

class procedure TLuaLib.TFunctionEntry.PushWrappedFunction(AL: TLuaState; AFunc: TLuaCFunction);
begin
  AL.PushLightuserdata(@AFunc);
  AL.PushCClosure(WrapperFunc, 1);
end;

procedure TLuaLib.TFunctionEntry.RegisterEntry(AL: TLuaState);
begin
  inherited;
  PushWrappedFunction(AL, @Func);
  AL.SetTable(-3);
end;

class function TLuaLib.TFunctionEntry.WrapperFunc(L: TLuaState): Integer;
var
  Lua: TLua;
begin
  Lua := TLua.FromState(L);
  Lua.Interlock;
  try
    Result := TLuaCFunction(L.ToUserdata(L.UpvalueIndex(1)))(L);
  finally
    Lua.Unlock;
  end;
end;

{ TLuaLib.TTableEntry }

procedure TLuaLib.TTableEntry.Add(AEntry: TEntry);
begin
  FEntries.Add(AEntry);
  AEntry.FParent := Self;
end;

procedure TLuaLib.TTableEntry.Add(AName, AString: TLuaString);
begin
  Add(TStringEntry.Create(AName, AString));
end;

procedure TLuaLib.TTableEntry.Add(AName: TLuaString; AFunc: TLuaCFunction);
begin
  Add(TFunctionEntry.Create(AName, AFunc));
end;

procedure TLuaLib.TTableEntry.Add(AName: TLuaString; ANumber: TLuaNumber);
begin
  Add(TNumberEntry.Create(AName, ANumber));
end;

function TLuaLib.TTableEntry.Add(AName: TLuaString): TTableEntry;
begin
  Result := TTableEntry.Create(AName);
  Add(Result);
end;

procedure TLuaLib.TTableEntry.Add(AName: TLuaString; AInteger: TLuaInteger);
begin
  Add(TIntegerEntry.Create(AName, AInteger));
end;

procedure TLuaLib.TTableEntry.AddRecursion(AName: AnsiString; ATable: TTableEntry);
begin
  Add(TRecursiveTableEntry.Create(AName, ATable));
end;

constructor TLuaLib.TTableEntry.Create(AName: AnsiString);
begin
  inherited;
  FEntries := TRefArray<TEntry>.Create(True);
end;

destructor TLuaLib.TTableEntry.Destroy;
begin
  FEntries.Free;
  inherited;
end;

function TLuaLib.TTableEntry.EntryCount: Integer;
begin
  Result := FEntries.Count;
end;

function TLuaLib.TTableEntry.GetEntry(AIndex: Integer): TEntry;
begin
  Result := FEntries[AIndex];
end;

procedure TLuaLib.TTableEntry.RegisterEntry(AL: TLuaState);
var
  Entry: TEntry;
begin
  if Name <> '' then
  begin
    AL.NewTable;
    AL.PushValue;
    AL.SetField(PPAnsiChar(@Name)^, -3);
  end;
  for Entry in FEntries do
    Entry.RegisterEntry(AL);
  if Name <> '' then
    AL.Pop;
end;

procedure TLuaLib.TTableEntry.UnregisterEntry(AL: TLuaState);
var
  Entry: TEntry;
begin
  if Name <> '' then
  begin
    inherited;
  end
  else
  begin
    for Entry in FEntries do
      Entry.UnregisterEntry(AL);
  end;
end;

{ TLuaLib.TRecursiveTableEntry }

constructor TLuaLib.TRecursiveTableEntry.Create(AName: AnsiString; ATable: TTableEntry);
begin
  inherited Create(AName);
  FTable := ATable;
end;

procedure TLuaLib.TRecursiveTableEntry.RegisterEntry(AL: TLuaState);
begin
  inherited;
  FTable.PushValue(AL);
  AL.SetTable(-3);
end;

{ TLuaLib.TStringEntry }

constructor TLuaLib.TStringEntry.Create(AName, AValue: AnsiString);
begin
  inherited Create(AName);
  FValue := AValue;
end;

procedure TLuaLib.TStringEntry.RegisterEntry(AL: TLuaState);
begin
  inherited;
  AL.PushString(PPAnsiChar(@Value)^);
  AL.SetTable(-3);
end;

{ TLuaLib.TNumberEntry }

constructor TLuaLib.TNumberEntry.Create(AName: AnsiString; AValue: TLuaNumber);
begin
  inherited Create(AName);
  FValue := AValue;
end;

procedure TLuaLib.TNumberEntry.RegisterEntry(AL: TLuaState);
begin
  inherited;
  AL.PushNumber(Value);
  AL.SetTable(-3);
end;

{ TLuaLib.TIntegerEntry }

constructor TLuaLib.TIntegerEntry.Create(AName: AnsiString; AValue: TLuaInteger);
begin
  inherited Create(AName);
  FValue := AValue;
end;

procedure TLuaLib.TIntegerEntry.RegisterEntry(AL: TLuaState);
begin
  inherited;
  AL.PushInteger(Value);
  AL.SetTable(-3);
end;

{ ELibNotLoaded }

constructor ELibNotLoaded.Create(ALib: TLuaLibClass);
begin
  inherited Create(ALib.ClassName);
end;

end.
