unit LuaDefine;

interface

uses
  LuaHeader, Lists, LuaConf, SyncObjs, Classes, Windows, TimeManager, SysUtils;

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
      FEntries: TObjectArray<TEntry>;
      FEntryReader: TRefArrayReader<TEntry>;

    public
      constructor Create(AName: TLuaString = '');
      destructor Destroy; override;

      property Entries: TRefArrayReader<TEntry> read FEntryReader;

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

    class procedure CreateEntry(AEntry: TTableEntry); virtual; abstract;

  public
    constructor Create(AL: TLuaState); virtual;
    destructor Destroy; override;

    procedure ChangeLuaState(AL: TLuaState); virtual;

    class procedure PushFunc(AL: TLuaState; AFunc: TLuaCFunction); static;

  end;

  TLuaLibClass = class of TLuaLib;

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
    FMemoryLimit: NativeUInt;
    FMemoryUsage: NativeUInt;
    FLock: TCriticalSection;
    FThread: TLuaThread;
    FLibs: TClassObjectMap<TLuaLib>;

    class function Alloc(ud, ptr: Pointer; osize, nsize: NativeUInt): Pointer; static; cdecl;

    procedure MakeLuaState;

  public
    constructor Create;
    class function FromState(AL: TLuaState): TLua; static;
    destructor Destroy; override;

    property L: TLuaState read FL;
    property MemoryLimit: NativeUInt read FMemoryLimit write FMemoryLimit;
    property MemoryUsage: NativeUInt read FMemoryUsage;

    procedure Interlock;
    procedure Unlock;
    function ShouldTerminate: Boolean;

    function LCall(AParams, AResults: Integer; ATimeout: Single; out AError: TLuaPCallError): Boolean;

    procedure AddLib(ALib: TLuaLibClass);
    procedure DelLib(ALib: TLuaLibClass);

  end;

implementation

{ TLua }

procedure TLua.AddLib(ALib: TLuaLibClass);
begin
  if FLibs.HasKey(ALib) then
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
      end;
      FreeMemory(ptr);
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
  FAllocations := TAllocationList.Create(12289);
  MakeLuaState;
  FThread := TLuaThread.Create(Self);
  FLibs := TClassObjectMap<TLuaLib>.Create;
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

function TLua.LCall(AParams, AResults: Integer; ATimeout: Single; out AError: TLuaPCallError): Boolean;
var
  StopWatch: TStopWatch;
  Lib: TPair<TClass, TLuaLib>;
begin
  StopWatch.Start;
  Result := False;
  while not FThread.Started do TThread.Yield;
  FThread.Start(AParams, AResults);
  while StopWatch.Time < ATimeOut do
  begin
    TThread.Yield;
    if FThread.Done then
    begin
      AError := FThread.Error;
      Exit(True);
    end;
  end;

  FThread.ForceKill;
  FAllocations.PerformCleanup;
  FMemoryUsage := 0;
  MakeLuaState;

  for Lib in FLibs do
    Lib.Data.ChangeLuaState(L);

  FThread.Free;
  FThread := TLuaThread.Create(Self);
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
  FEntries := TObjectArray<TEntry>.Create;
  FEntryReader := TRefArrayReader<TEntry>.Create(FEntries);
end;

destructor TLuaLib.TTableEntry.Destroy;
begin
  FEntryReader.Free;
  FEntries.Free;
  inherited;
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

end.
