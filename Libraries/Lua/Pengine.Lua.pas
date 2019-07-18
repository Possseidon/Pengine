unit Pengine.Lua;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.TypInfo,

  Pengine.Lua.Conf,
  Pengine.Lua.Header,
  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.TimeManager;

type

  TLuaLib = class abstract
  protected type

    TTableEntry = class;

    TEntry = class abstract
    private
      FName: TLuaString;
      FParent: TTableEntry;

      procedure PushValue(L: TLuaState);

    public
      constructor Create(AName: TLuaString);

      property Name: TLuaString read FName;

      procedure RegisterEntry(L: TLuaState); virtual;
      procedure UnregisterEntry(L: TLuaState); virtual;

    end;

    TFunctionEntry = class(TEntry)
    private
      FFunc: TLuaCFunction;

    public
      constructor Create(AName: TLuaString; AFunc: TLuaCFunction);

      property Func: TLuaCFunction read FFunc;

      procedure RegisterEntry(L: TLuaState); override;

    end;

    TStringEntry = class(TEntry)
    private
      FValue: TLuaString;
    public
      constructor Create(AName, AValue: TLuaString);

      property Value: TLuaString read FValue;

      procedure RegisterEntry(L: TLuaState); override;

    end;

    TNumberEntry = class(TEntry)
    private
      FValue: TLuaNumber;
    public
      constructor Create(AName: TLuaString; AValue: TLuaNumber);

      property Value: TLuaNumber read FValue;

      procedure RegisterEntry(L: TLuaState); override;

    end;

    TIntegerEntry = class(TEntry)
    private
      FValue: TLuaInteger;

    public
      constructor Create(AName: TLuaString; AValue: TLuaInteger);

      property Value: TLuaInteger read FValue;

      procedure RegisterEntry(L: TLuaState); override;

    end;

    TRecursiveTableEntry = class;

    TTableEntry = class(TEntry)
    private
      FEntries: TRefArray<TEntry>;
      FHasMetatable: Boolean;
      FMetatable: array [TLuaMetatableEvent] of TEntry;

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

      procedure AddMeta(AEvent: TLuaMetatableEvent; AFunc: TLuaCFunction); overload;
      procedure AddMeta(AEvent: TLuaMetatableEvent; AString: TLuaString); overload;

      procedure AddPublished(AClass: TClass);

      procedure RegisterEntry(L: TLuaState); override;
      procedure UnregisterEntry(L: TLuaState); override;

    end;

    TRecursiveTableEntry = class(TEntry)
    private
      FTable: TTableEntry;
    public
      constructor Create(AName: AnsiString; ATable: TTableEntry);

      procedure RegisterEntry(L: TLuaState); override;

    end;

  private
    FL: TLuaState;
    FEntry: TTableEntry;

  protected
    property L: TLuaState read FL;

    class function LuaNotImplemented(L: TLuaState): Integer; static; cdecl;

    class procedure CreateEntry(AEntry: TTableEntry); virtual;
    class procedure InitLua(L: TLuaState); virtual;

  public
    constructor Create(L: TLuaState); virtual;
    destructor Destroy; override;

    procedure ChangeLuaState(L: TLuaState); virtual;

  end;

  TLuaLibClass = class of TLuaLib;

  ELibNotLoaded = class(Exception)
  public
    constructor Create(ALib: TLuaLibClass);
  end;

  TLua = class
  public type

    TLibs = TClassObjectMap<TLuaLibClass, TLuaLib>;

  private
    FLuaState: TLuaState;
    FMemoryLimit: NativeUInt;
    FMemoryUsage: NativeUInt;
    FCallTimer: TStopWatch;
    FCallTimeout: Single;
    FLibs: TLibs;

    class function Alloc(ud, ptr: Pointer; osize, nsize: NativeUInt): Pointer; static; cdecl;
    class procedure LuaTimeoutHook(L: TLuaState; ar: Plua_Debug); static; cdecl;

    procedure MakeLuaState;

  public
    constructor Create;
    class function FromState(L: TLuaState): TLua; static;
    destructor Destroy; override;

    property L: TLuaState read FLuaState;
    property MemoryLimit: NativeUInt read FMemoryLimit write FMemoryLimit;
    property MemoryUsage: NativeUInt read FMemoryUsage;

    procedure CheckTimeout;

    function CallTimeout(AParams, AResults: Integer; ATimeout: Single): TLuaPCallError;

    function AddLib<T: TLuaLib>: T; overload;
    function AddLib(ALib: TLuaLibClass): TLuaLib; overload;
    procedure RemoveLib(ALib: TLuaLibClass);

    function Lib<T: TLuaLib>: T;

  end;

implementation

{ TLua }

function TLua.AddLib(ALib: TLuaLibClass): TLuaLib;
begin
  if FLibs.KeyExists(ALib) then
    raise Exception.Create('LuaLib is registered already');
  Result := ALib.Create(L);
  FLibs[ALib] := Result;
end;

function TLua.AddLib<T>: T;
begin
  Result := T(AddLib(T));
end;

class function TLua.Alloc(ud, ptr: Pointer; osize, nsize: NativeUInt): Pointer;
var
  Self: TLua;
begin
  Self := TLua(ud);

  // if ptr is nil, osize defines what is allocated
  if ptr = nil then
    osize := 0;

  if (Self.MemoryLimit <> 0) and (Self.MemoryUsage - osize + nsize > Self.MemoryLimit) then
    Exit(nil);

  if nsize <> 0 then
    Result := ReallocMemory(ptr, nsize)
  else
  begin
    FreeMemory(ptr);
    Result := nil;
  end;

  Inc(Self.FMemoryUsage, nsize - osize);
end;

constructor TLua.Create;
begin
  MakeLuaState;
  FLibs := TLibs.Create;
end;

procedure TLua.RemoveLib(ALib: TLuaLibClass);
begin
  FLibs.Remove(ALib);
end;

procedure TLua.CheckTimeout;
begin
  if (FCallTimeout <> 0) and (FCallTimer.Time > FCallTimeout) then
    L.ErrorFmt('timeout after %s', [FCallTimer.Format]);
end;

destructor TLua.Destroy;
begin
  FLibs.Free;
  L.Close;
  inherited;
end;

class function TLua.FromState(L: TLuaState): TLua;
begin
  Result := TLua(L.GetExtraSpace^);
end;

function TLua.CallTimeout(AParams, AResults: Integer; ATimeout: Single): TLuaPCallError;
begin
  FCallTimeout := ATimeout;
  L.SetHook(LuaTimeoutHook, LUA_MASKCOUNT, 100);
  FCallTimer.Start;
  Result := L.PCall(AParams, AResults, 0);
  L.SetHook(nil, 0, 0);
  FCallTimeout := 0;
end;

procedure TLua.MakeLuaState;
begin
  FLuaState := NewLuaState(Alloc, Self);
  PPointer(FLuaState.GetExtraSpace)^ := Self;
end;

function TLua.Lib<T>: T;
var
  Lib: TLuaLib;
begin
  if FLibs.Get(T, Lib) then
    Exit(T(Lib));
  raise ELibNotLoaded.Create(T);
end;

class procedure TLua.LuaTimeoutHook(L: TLuaState; ar: Plua_Debug);
begin
  TLua.FromState(L).CheckTimeout;
end;

{ TLuaLib }

procedure TLuaLib.ChangeLuaState(L: TLuaState);
begin
  FL := L;
  L.PushGlobalTable;
  FEntry.RegisterEntry(L);
  InitLua(L);
  L.Pop;
end;

class function TLuaLib.LuaNotImplemented(L: TLuaState): Integer;
begin
  Result := L.Error('The function is not implemented!');
end;

constructor TLuaLib.Create(L: TLuaState);
begin
  FEntry := TTableEntry.Create;
  CreateEntry(FEntry);
  ChangeLuaState(L);
end;

class procedure TLuaLib.CreateEntry(AEntry: TTableEntry);
begin
  // nothing
end;

destructor TLuaLib.Destroy;
begin
  L.PushGlobalTable;
  FEntry.UnregisterEntry(L);
  L.Pop;
  FEntry.Free;
  inherited;
end;

class procedure TLuaLib.InitLua(L: TLuaState);
begin
  // nothing
end;

{ TLuaLib.TEntry }

constructor TLuaLib.TEntry.Create(AName: AnsiString);
begin
  FName := AName;
end;

procedure TLuaLib.TEntry.PushValue(L: TLuaState);
begin
  if FParent <> nil then
    FParent.PushValue(L)
  else
    L.PushGlobalTable;
  if Name <> '' then
  begin
    L.GetField(PAnsiChar(Name));
    L.Remove(-2);
  end;
end;

procedure TLuaLib.TEntry.RegisterEntry(L: TLuaState);
begin
  L.PushString(PAnsiChar(Name));
end;

procedure TLuaLib.TEntry.UnregisterEntry(L: TLuaState);
begin
  L.PushString(PAnsiChar(Name));
  L.PushNil;
  L.SetTable(-3);
end;

{ TLuaLib.TFunctionEntry }

constructor TLuaLib.TFunctionEntry.Create(AName: AnsiString; AFunc: TLuaCFunction);
begin
  inherited Create(AName);
  FFunc := AFunc;
end;

procedure TLuaLib.TFunctionEntry.RegisterEntry(L: TLuaState);
begin
  inherited;
  L.PushCFunction(Func);
  L.SetTable(-3);
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

procedure TLuaLib.TTableEntry.AddMeta(AEvent: TLuaMetatableEvent; AString: TLuaString);
begin
  FHasMetatable := True;
  FMetatable[AEvent] := TStringEntry.Create(LuaMetatableEventNames[AEvent], AString);
end;

procedure TLuaLib.TTableEntry.AddPublished(AClass: TClass);
var
  MetaEvent: TLuaMetatableEvent;
  Func: TLuaCFunction;
  RttiMethod: TRttiMethod;
begin
  for MetaEvent := Low(TLuaMetatableEvent) to High(TLuaMetatableEvent) do
  begin
    Func := AClass.MethodAddress(string(LuaMetatableEventNames[MetaEvent]));
    if Assigned(Func) then
      AddMeta(MetaEvent, Func);
  end;

  for RttiMethod in TRttiContext.Create.GetType(AClass).GetMethods do
  begin
    if RttiMethod.Visibility <> mvPublished then
      Continue;
    if not RttiMethod.Name.StartsWith('Lua_') then
      Continue;
    Add(AnsiString(RttiMethod.Name.Substring(4)), RttiMethod.CodeAddress);
  end;
end;

procedure TLuaLib.TTableEntry.AddMeta(AEvent: TLuaMetatableEvent; AFunc: TLuaCFunction);
begin
  FHasMetatable := True;
  FMetatable[AEvent] := TFunctionEntry.Create(LuaMetatableEventNames[AEvent], AFunc);
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
var
  MetaEntry: TEntry;
begin
  for MetaEntry in FMetatable do
    MetaEntry.Free;
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

procedure TLuaLib.TTableEntry.RegisterEntry(L: TLuaState);
var
  Entry: TEntry;
  MetaEvent: TLuaMetatableEvent;
begin
  if Name <> '' then
  begin
    L.NewTable;
    L.PushValue;
    L.SetField(PAnsiChar(Name), -3);
  end;

  for Entry in FEntries do
    Entry.RegisterEntry(L);

  if FHasMetatable then
  begin
    L.NewTable;
    for MetaEvent := Low(TLuaMetatableEvent) to High(TLuaMetatableEvent) do
    begin
      if FMetatable[MetaEvent] <> nil then
        FMetatable[MetaEvent].RegisterEntry(L);
    end;
    L.SetMetatable(-2);
  end;

  if Name <> '' then
    L.Pop;
end;

procedure TLuaLib.TTableEntry.UnregisterEntry(L: TLuaState);
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
      Entry.UnregisterEntry(L);
  end;
end;

{ TLuaLib.TRecursiveTableEntry }

constructor TLuaLib.TRecursiveTableEntry.Create(AName: AnsiString; ATable: TTableEntry);
begin
  inherited Create(AName);
  FTable := ATable;
end;

procedure TLuaLib.TRecursiveTableEntry.RegisterEntry(L: TLuaState);
begin
  inherited;
  FTable.PushValue(L);
  L.SetTable(-3);
end;

{ TLuaLib.TStringEntry }

constructor TLuaLib.TStringEntry.Create(AName, AValue: AnsiString);
begin
  inherited Create(AName);
  FValue := AValue;
end;

procedure TLuaLib.TStringEntry.RegisterEntry(L: TLuaState);
begin
  inherited;
  L.PushString(PAnsiChar(Value));
  L.SetTable(-3);
end;

{ TLuaLib.TNumberEntry }

constructor TLuaLib.TNumberEntry.Create(AName: AnsiString; AValue: TLuaNumber);
begin
  inherited Create(AName);
  FValue := AValue;
end;

procedure TLuaLib.TNumberEntry.RegisterEntry(L: TLuaState);
begin
  inherited;
  L.PushNumber(Value);
  L.SetTable(-3);
end;

{ TLuaLib.TIntegerEntry }

constructor TLuaLib.TIntegerEntry.Create(AName: AnsiString; AValue: TLuaInteger);
begin
  inherited Create(AName);
  FValue := AValue;
end;

procedure TLuaLib.TIntegerEntry.RegisterEntry(L: TLuaState);
begin
  inherited;
  L.PushInteger(Value);
  L.SetTable(-3);
end;

{ ELibNotLoaded }

constructor ELibNotLoaded.Create(ALib: TLuaLibClass);
begin
  inherited Create(ALib.ClassName);
end;

end.
