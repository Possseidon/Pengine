unit LuaState;

interface

uses
  LuaHeader;

type

  { TLuaState2}
  /// <summary>Wraps the whole LuaHeader into an object</summary>
  TLuaState2 = class
  private
    FL: TLuaState;
    FReference: Boolean;

    function GetTop: Integer;
    procedure SetTop(AValue: Integer);

  public

    // --- state manipulation ---

    constructor Create; overload;
    constructor Create(AL: TLuaState; AReference: Boolean = True); overload;
    destructor Destroy; override;
    property L: TLuaState read FL;
    function NewThread: TLuaState2;

    // --- various ---

    /// <summary>The function gets called, when an error outside of a pcall happenes</summary>
    /// <remarks>Returns the old function</remarks>
    function AtPanic(AFunc: TLuaCFunction): TLuaCFunction; inline;
    /// <summary>
    function Version: TLuaNumber; inline;

    // --- basic stack manipulation ---

    /// <summary>Convert an acceptable Stack Index into an absolute Index</summary>
    function AbsIndex(AIndex: Integer): Integer; inline;
    /// <summary>Returns the size of the Stack</summary>
    property Top: Integer read GetTop write SetTop;
    /// <summary>Pushes a copy of the specified index on the stack</summary>
    procedure PushValue(AIndex: Integer); inline;
    /// <summary>Moves everything between AIndex and Top by AAmount</summary>
    procedure Rotate(AIndex, AAmount: Integer); inline;
    /// <summary>Copies the element from AFromIndex to AToIndex</summary>
    procedure Copy(AFromIndex, AToIndex: Integer); inline;
    /// <summary>Checks, if AAmount Values can be pushed onto the stack</summary>
    function CheckStack(AAmount: Integer): Boolean; inline;
    /// <summary>Pops AAmount elements and pushes them on ADest</summary>
    procedure XMove(ADest: TLuaState2; AAmount: Integer); inline;

    // --- access functions (stack -> C (Delphi)) ---

    function IsNumber(AIndex: Integer): Boolean; inline;
    function IsString(AIndex: Integer): Boolean; inline;
    function IsCFunction(AIndex: Integer): Boolean; inline;
    function IsInteger(AIndex: Integer): Boolean; inline;
    function IsUserdata(AIndex: Integer): Boolean; inline;
    function GetType(AIndex: Integer): Integer; inline;
    function TypeName(AType: Integer): TLuaString; inline;

    function ToNumberX(AIndex: Integer; AIsNum: PBoolean): TLuaNumber; inline;
    function ToIntegerX(AIndex: Integer; AIsNum: PBoolean): TLuaInteger; inline;
    function ToBoolean(AIndex: Integer): Integer; inline;
    function RawLen(AIndex: Integer): NativeUInt; inline;
    function ToCFunction(AIndex: Integer): TLuaCFunction; inline;
    function ToUserdata(AIndex: Integer): Pointer; inline;
    function ToThread(AIndex: Integer): TLuaState2; inline;
    function ToPointer(AIndex: Integer): Pointer; inline;

    // --- Comparison and arithmetic functions ---

    procedure Arith(AOperation: TLuaArithOp); inline;

    function RawEqual(AIndex1, AIndex2: Integer): Boolean; inline;
    function Compare(AIndex1, AIndex2: Integer; AOperation: TLuaCompareOp): Boolean; inline;

    // --- push functions (C (Delphi) -> stack) ---

    procedure PushNil; inline;
    procedure PushNumber(ANumber: TLuaNumber); inline;
    procedure PushInteger(AInteger: TLuaInteger); inline;
    procedure PushString(AString: TLuaString); inline;
    procedure PushCClosure(AFunc: TLuaCFunction; AAmount: Integer); inline;
    procedure PushBoolean(ABool: Boolean); inline;
    procedure PushLightUserdata(AData: Pointer); inline;
    function PushThread: Integer; inline;

    // --- get functions (Lua -> stack) ---

    function GetGlobal(AName: TLuaString): Integer; inline;
    function GetTable(AIndex: Integer): Integer; inline;
    function GetField(AIndex: Integer; AName: TLuaString): Integer; inline;
    function GetI(AIndex: Integer; ATableIndex: TLuaInteger): Integer; inline;
    function RawGet(AIndex: Integer): Integer; inline;
    function RawGetI(AIndex: Integer; ATableIndex: TLuaInteger): Integer; inline;
    function RawGetP(AIndex: Integer; APointer: Pointer): Integer; inline;

    procedure CreateTable(AArrSize, ARecSize: Integer); inline;
    function NewUserdata(ASize: NativeUInt): Pointer; inline;
    function GetMetatable(AObjIndex: Integer): Boolean; inline;
    function GetUservalue(AIndex: Integer): Integer; inline;

    // --- set functions (stack -> Lua) ---

    procedure SetGlobal(AName: TLuaString); inline;
    procedure SetTable(AIndex: Integer); inline;
    procedure SetField(AIndex: Integer; AName: TLuaString); inline;
    procedure SetI(AIndex: Integer; ATableIndex: TLuaInteger); inline;
    procedure RawSet(AIndex: Integer); inline;
    procedure RawSetI(AIndex: Integer; ATableIndex: TLuaInteger); inline;
    procedure RawSetP(AIndex: Integer; APointer: Pointer); inline;
    function SetMetatable(AObjIndex: Integer): Integer; inline;
    procedure SetUservalue(AIndex: Integer); inline;

    // --- 'load' and 'call' functions (load and run Lua code) ---
    procedure CallK(AArgs, AResults: Integer; AContext: TLuaKContext; AKFunc: TLuaKFunction); inline;
    procedure Call(AArgs, AResults: Integer); inline;
    function PCallK(AArgs, AResults, AErrFuncIndex: Integer; AContext: TLuaKContext; AKFunc: TLuaKFunction): TLuaPCallError; inline;
    function PCall(AArgs, AResults, AErrFuncIndex: Integer): TLuaPCallError; inline;
    function Load(AReader: TLuaReader; AData: Pointer; AChunkName: TLuaString; AMode: PLuaString = nil): Integer; inline;
    function Dump(AWriter: TLuaWriter; AData: Pointer; AStrip: Integer): Integer; inline;

    // --- coroutine functions ---
    function YieldK(AResults: Integer; AContext: TLuaKContext; AKFunc: TLuaKFunction): Integer; inline;
    function Resume(AFrom: TLuaState2; AArgs: Integer): Integer; inline;
    function Status: Integer; inline;
    function IsYieldable: Integer; inline;
    function Yield(AResults: Integer): Integer; inline;

    // --- garbage-collection function ---
    function GarbageCollector(AWhat: TLuaGCOption; AData: Integer): Integer; inline;

    // --- miscellaneous functions ---
    class function Error(L: TLuaState): Integer; inline; static;
    function Next(AIndex: Integer): LongBool; inline;
    procedure Concat(AAmount: Integer); inline;
    procedure Len(AIndex: Integer); inline;
    function StringToNumber(AString: TLuaString): Boolean; inline;

    function GetAllocF(var AUserdata: Pointer): TLuaAlloc; inline;
    procedure SetAllocF(AFunc: TLuaAlloc; AUserdata: Pointer); inline;

    // --- some useful macros ---
    function GetExtraSpace: Pointer; inline;
    function ToNumber(AIndex: Integer): TLuaNumber; inline;
    function ToInteger(AIndex: Integer): TLuaInteger; inline;

    procedure Pop(AAmount: Integer); inline;

    procedure NewTable; inline;

    procedure RegisterFunc(AName: TLuaString; AFunc: TLuaCFunction); inline;

    procedure PushCFunction(AFunc: TLuaCFunction); inline;

    function IsFunction(AIndex: Integer): Boolean; inline;
    function IsTable(AIndex: Integer): Boolean; inline;
    function IsLightUserdata(AIndex: Integer): Boolean; inline;
    function IsNil(AIndex: Integer): Boolean; inline;
    function IsBoolean(AIndex: Integer): Boolean; inline;
    function IsThread(AIndex: Integer): Boolean; inline;
    function IsNone(AIndex: Integer): Boolean; inline;
    function IsNoneOrNil(AIndex: Integer): Boolean; inline;

    procedure PushGlobalTable; inline;

    function ToString(AIndex: Integer): TLuaString; reintroduce; inline;

    procedure Insert(AIndex: Integer); inline;

    procedure Remove(AIndex: Integer); inline;

    procedure Replace(AIndex: Integer); inline;

    // --- Debug API ---

    function GetStack(ALevel: Integer; var AActRec: TLuaDebug): Boolean; inline;
    function GetInfo(AWhat: TLuaString; var AActRec: TLuaDebug): Integer; inline;
    function GetLocal(var AActRec: TLuaDebug; AIndex: Integer): TLuaString; inline;
    function SetLocal(var AActRec: TLuaDebug; AIndex: Integer): TLuaString; inline;
    function GetUpvalue(AFuncIndex, AIndex: Integer): TLuaString; inline;
    function SetUpvalue(AFuncIndex, AIndex: Integer): TLuaString; inline;

    function UpvalueID(AFuncIndex, AIndex: Integer): Pointer; inline;
    procedure UpvalueJoin(AFuncIndex1, AIndex1, AFuncIndex2, AIndex2: Integer); inline;

    procedure SetHook(AFunc: TLuaHook; AMask, ACount: Integer); inline;
    function GetHook: TLuaHook; inline;
    function GetHookMask: Integer; inline;
    function GetHookCount: Integer; inline;

  end;

implementation

{ TLuaState2 }

constructor TLuaState2.Create(AL: TLuaState; AReference: Boolean);
begin
  FL := AL;
  FReference := AReference;
end;

function TLuaState2.GetTop: Integer;
begin
  Result := lua_gettop(L);
end;

procedure TLuaState2.SetTop(AValue: Integer);
begin
  lua_settop(L, AValue);
end;

constructor TLuaState2.Create;
begin
end;

destructor TLuaState2.Destroy;
begin
  if not FReference then
    lua_close(L);
  inherited Destroy;
end;

function TLuaState2.NewThread: TLuaState2;
begin
  Result := TLuaState2.Create(lua_newthread(L), False);
end;

function TLuaState2.AtPanic(AFunc: TLuaCFunction): TLuaCFunction;
begin
  Result := lua_atpanic(L, AFunc);
end;

function TLuaState2.Version: TLuaNumber;
begin
  Result := lua_version(L)^;
end;

function TLuaState2.AbsIndex(AIndex: Integer): Integer;
begin
  Result := lua_absindex(L, AIndex)
end;

procedure TLuaState2.PushValue(AIndex: Integer);
begin
  lua_pushvalue(L, AIndex);
end;

procedure TLuaState2.Rotate(AIndex, AAmount: Integer);
begin
  lua_rotate(L, AIndex, AAmount);
end;

procedure TLuaState2.Copy(AFromIndex, AToIndex: Integer);
begin
  lua_copy(L, AFromIndex, AToIndex);
end;

function TLuaState2.CheckStack(AAmount: Integer): Boolean;
begin
  Result := lua_checkstack(L, AAmount);
end;

procedure TLuaState2.XMove(ADest: TLuaState2; AAmount: Integer);
begin
  lua_xmove(L, ADest.L, AAmount);
end;

function TLuaState2.IsNumber(AIndex: Integer): Boolean;
begin
  Result := lua_isnumber(L, AIndex);
end;

function TLuaState2.IsString(AIndex: Integer): Boolean;
begin
  Result := lua_isstring(L, AIndex);
end;

function TLuaState2.IsCFunction(AIndex: Integer): Boolean;
begin
  Result := lua_iscfunction(L, AIndex);
end;

function TLuaState2.IsInteger(AIndex: Integer): Boolean;
begin
  Result := lua_isinteger(L, AIndex);
end;

function TLuaState2.IsUserdata(AIndex: Integer): Boolean;
begin
  Result := lua_isuserdata(L, AIndex)
end;

function TLuaState2.GetType(AIndex: Integer): Integer;
begin
  Result := lua_type(L, AIndex);
end;

function TLuaState2.TypeName(AType: Integer): TLuaString;
begin
  Result := lua_typename(L, AType);
end;

function TLuaState2.ToNumberX(AIndex: Integer; AIsNum: PBoolean): TLuaNumber;
var
  B: Integer;
begin
  Result := lua_tonumberx(L, AIndex, @B);
  AIsNum^ := B <> 0;
end;

function TLuaState2.ToIntegerX(AIndex: Integer; AIsNum: PBoolean): TLuaInteger;
var
  B: Integer;
begin
  Result := lua_tointegerx(L, AIndex, @B);
  AIsNum^ := B <> 0;
end;

function TLuaState2.ToBoolean(AIndex: Integer): Integer;
begin
  Result := lua_toboolean(L, AIndex);
end;

function TLuaState2.RawLen(AIndex: Integer): NativeUInt;
begin
  Result := lua_rawlen(L, AIndex);
end;

function TLuaState2.ToCFunction(AIndex: Integer): TLuaCFunction;
begin
  Result := lua_tocfunction(L, AIndex);
end;

function TLuaState2.ToUserdata(AIndex: Integer): Pointer;
begin
  Result := lua_touserdata(L, AIndex);
end;

function TLuaState2.ToThread(AIndex: Integer): TLuaState2;
begin
  Result := TLuaState2(lua_tothread(L, AIndex));
end;

function TLuaState2.ToPointer(AIndex: Integer): Pointer;
begin
  Result := lua_topointer(L, AIndex);
end;

procedure TLuaState2.Arith(AOperation: TLuaArithOp);
begin
  lua_arith(L, Ord(AOperation));
end;

function TLuaState2.RawEqual(AIndex1, AIndex2: Integer): Boolean;
begin
  Result := lua_rawequal(L, AIndex1, AIndex2);
end;

function TLuaState2.Compare(AIndex1, AIndex2: Integer; AOperation: TLuaCompareOp): Boolean;
begin
  Result := lua_compare(L, AIndex1, AIndex2, Ord(AOperation));
end;

procedure TLuaState2.PushNil;
begin
  lua_pushnil(L);
end;

procedure TLuaState2.PushNumber(ANumber: TLuaNumber);
begin
  lua_pushnumber(L, ANumber);
end;

procedure TLuaState2.PushInteger(AInteger: TLuaInteger);
begin
  lua_pushinteger(L, AInteger);
end;

procedure TLuaState2.PushString(AString: TLuaString);
begin
  lua_pushstring(L, @AString[1]);
end;

procedure TLuaState2.PushCClosure(AFunc: TLuaCFunction; AAmount: Integer);
begin
  lua_pushcclosure(L, AFunc, AAmount);
end;

procedure TLuaState2.PushBoolean(ABool: Boolean);
begin
  lua_pushboolean(L, ABool);
end;

procedure TLuaState2.PushLightUserdata(AData: Pointer);
begin
  lua_pushlightuserdata(L, AData)
end;

function TLuaState2.PushThread: Integer;
begin
  Result := lua_pushthread(L);
end;

function TLuaState2.GetGlobal(AName: TLuaString): Integer;
begin
  Result := lua_getglobal(L, @AName[1])
end;

function TLuaState2.GetTable(AIndex: Integer): Integer;
begin
  Result := lua_gettable(L, AIndex);
end;

function TLuaState2.GetField(AIndex: Integer; AName: TLuaString): Integer;
begin
  Result := lua_getfield(L, AIndex, @AName[1]);
end;

function TLuaState2.GetI(AIndex: Integer; ATableIndex: TLuaInteger): Integer;
begin
  Result := lua_geti(L, AIndex, ATableIndex);
end;

function TLuaState2.RawGet(AIndex: Integer): Integer;
begin
  Result := lua_rawget(L, AIndex);
end;

function TLuaState2.RawGetI(AIndex: Integer; ATableIndex: TLuaInteger): Integer;
begin
  Result := lua_rawgeti(L, AIndex, ATableIndex);
end;

function TLuaState2.RawGetP(AIndex: Integer; APointer: Pointer): Integer;
begin
  Result := lua_rawgetp(L, AIndex, APointer);
end;

procedure TLuaState2.CreateTable(AArrSize, ARecSize: Integer);
begin
  lua_createtable(L, AArrSize, ARecSize);
end;

function TLuaState2.NewUserdata(ASize: NativeUInt): Pointer;
begin
  Result := lua_newuserdata(L, ASize);
end;

function TLuaState2.GetMetatable(AObjIndex: Integer): Boolean;
begin
  Result := lua_getmetatable(L, AObjIndex);
end;

function TLuaState2.GetUservalue(AIndex: Integer): Integer;
begin
  Result := lua_getuservalue(L, AIndex);
end;

procedure TLuaState2.SetGlobal(AName: TLuaString);
begin
  lua_setglobal(L, @AName[1]);
end;

procedure TLuaState2.SetTable(AIndex: Integer);
begin
  lua_settable(L, AIndex);
end;

procedure TLuaState2.SetField(AIndex: Integer; AName: TLuaString);
begin
  lua_setfield(L, AIndex, @AName[1]);
end;

procedure TLuaState2.SetI(AIndex: Integer; ATableIndex: TLuaInteger);
begin
  lua_seti(L, AIndex, ATableIndex);
end;

procedure TLuaState2.RawSet(AIndex: Integer);
begin
  lua_rawset(L, AIndex);
end;

procedure TLuaState2.RawSetI(AIndex: Integer; ATableIndex: TLuaInteger);
begin
  lua_rawseti(L, AIndex, ATableIndex);
end;

procedure TLuaState2.RawSetP(AIndex: Integer; APointer: Pointer);
begin
  lua_rawsetp(L, AIndex, APointer);
end;

function TLuaState2.SetMetatable(AObjIndex: Integer): Integer;
begin
  Result := lua_setmetatable(L, AObjIndex);
end;

procedure TLuaState2.SetUservalue(AIndex: Integer);
begin
  lua_setuservalue(L, AIndex);
end;

procedure TLuaState2.CallK(AArgs, AResults: Integer; AContext: TLuaKContext; AKFunc: TLuaKFunction);
begin
  lua_callk(L, AArgs, AResults, AContext, AKFunc);
end;

procedure TLuaState2.Call(AArgs, AResults: Integer);
begin
  lua_call(L, AArgs, AResults);
end;

function TLuaState2.PCallK(AArgs, AResults, AErrFuncIndex: Integer; AContext: TLuaKContext; AKFunc: TLuaKFunction): TLuaPCallError;
begin
  Result := TLuaPCallError(lua_pcallk(L, AArgs, AResults, AErrFuncIndex, AContext, AKFunc));
end;

function TLuaState2.PCall(AArgs, AResults, AErrFuncIndex: Integer): TLuaPCallError;
begin
  Result := TLuaPCallError(lua_pcall(L, AArgs, AResults, AErrFuncIndex));
end;

function TLuaState2.Load(AReader: TLuaReader; AData: Pointer; AChunkName: TLuaString; AMode: PLuaString): Integer;
begin
  Result := lua_load(L, AReader, AData, @AChunkName[1], AMode);
end;

function TLuaState2.Dump(AWriter: TLuaWriter; AData: Pointer; AStrip: Integer): Integer;
begin
  Result := lua_dump(L, AWriter, Adata, AStrip);
end;

function TLuaState2.YieldK(AResults: Integer; AContext: TLuaKContext; AKFunc: TLuaKFunction): Integer;
begin
  Result := lua_yieldk(L, AResults, AContext, AKFunc);
end;

function TLuaState2.Resume(AFrom: TLuaState2; AArgs: Integer): Integer;
begin
  Result := lua_resume(L, AFrom.L, AArgs);
end;

function TLuaState2.Status: Integer;
begin
  Result := lua_status(L);
end;

function TLuaState2.IsYieldable: Integer;
begin
  Result := lua_isyieldable(L);
end;

function TLuaState2.Yield(AResults: Integer): Integer;
begin
  Result := lua_yield(L, AResults);
end;

function TLuaState2.GarbageCollector(AWhat: TLuaGCOption; AData: Integer): Integer;
begin
  Result := lua_gc(L, Ord(AWhat), AData);
end;

class function TLuaState2.Error(L: TLuaState): Integer;
begin
  Result := lua_error(L);
end;

function TLuaState2.Next(AIndex: Integer): LongBool;
begin
  Result := lua_next(L, AIndex);
end;

procedure TLuaState2.Concat(AAmount: Integer);
begin
  lua_concat(L, AAmount);
end;

procedure TLuaState2.Len(AIndex: Integer);
begin
  lua_len(L, AIndex);
end;

function TLuaState2.StringToNumber(AString: TLuaString): Boolean;
begin
  Result := lua_stringtonumber(L, @AString[1]) <> 0;
end;

function TLuaState2.GetAllocF(var AUserdata: Pointer): TLuaAlloc;
begin
  Result := lua_getallocf(L, @AUserdata);
end;

procedure TLuaState2.SetAllocF(AFunc: TLuaAlloc; AUserdata: Pointer);
begin
  lua_setallocf(L, AFunc, AUserdata);
end;

function TLuaState2.GetExtraSpace: Pointer;
begin
  Result := lua_getextraspace(L);
end;

function TLuaState2.ToNumber(AIndex: Integer): TLuaNumber;
begin
  Result := lua_tonumber(L, AIndex);
end;

function TLuaState2.ToInteger(AIndex: Integer): TLuaInteger;
begin
  Result := lua_tointeger(L, AIndex);
end;

procedure TLuaState2.Pop(AAmount: Integer);
begin
  lua_pop(L, AAmount);
end;

procedure TLuaState2.NewTable;
begin
  lua_newtable(L);
end;

procedure TLuaState2.RegisterFunc(AName: TLuaString; AFunc: TLuaCFunction);
begin
  lua_register(L, @AName[1], AFunc);
end;

procedure TLuaState2.PushCFunction(AFunc: TLuaCFunction);
begin
  lua_pushcfunction(L, AFunc);
end;

function TLuaState2.IsFunction(AIndex: Integer): Boolean;
begin
  Result := lua_isfunction(L, AIndex);
end;

function TLuaState2.IsTable(AIndex: Integer): Boolean;
begin
  Result := lua_istable(L, AIndex);
end;

function TLuaState2.IsLightUserdata(AIndex: Integer): Boolean;
begin
  Result := lua_islightuserdata(L, AIndex);
end;

function TLuaState2.IsNil(AIndex: Integer): Boolean;
begin
  Result := lua_isnil(L, AIndex);
end;

function TLuaState2.IsBoolean(AIndex: Integer): Boolean;
begin
  Result := lua_isboolean(L, AIndex);
end;

function TLuaState2.IsThread(AIndex: Integer): Boolean;
begin
  Result := lua_isthread(L, AIndex);
end;

function TLuaState2.IsNone(AIndex: Integer): Boolean;
begin
  Result := lua_isnone(L, AIndex);
end;

function TLuaState2.IsNoneOrNil(AIndex: Integer): Boolean;
begin
  Result := lua_isnoneornil(L, AIndex);
end;

procedure TLuaState2.PushGlobalTable;
begin
  lua_pushglobaltable(L);
end;

function TLuaState2.ToString(AIndex: Integer): TLuaString;
begin
  Result := lua_tostring(L, AIndex);
end;

procedure TLuaState2.Insert(AIndex: Integer);
begin
  lua_insert(L, AIndex);
end;

procedure TLuaState2.Remove(AIndex: Integer);
begin
  lua_remove(L, AIndex);
end;

procedure TLuaState2.Replace(AIndex: Integer);
begin
  lua_replace(L, AIndex)
end;

function TLuaState2.GetStack(ALevel: Integer; var AActRec: TLuaDebug): Boolean;
begin
  Result := lua_getstack(L, ALevel, @AActRec);
end;

function TLuaState2.GetInfo(AWhat: TLuaString; var AActRec: TLuaDebug): Integer;
begin
  Result := lua_getinfo(L, @AWhat[1], @AActRec);
end;

function TLuaState2.GetLocal(var AActRec: TLuaDebug; AIndex: Integer): TLuaString;
begin
  Result := lua_getlocal(L, @AActRec, AIndex);
end;

function TLuaState2.SetLocal(var AActRec: TLuaDebug; AIndex: Integer): TLuaString;
begin
  Result := lua_setlocal(L, @AActRec, AIndex);
end;

function TLuaState2.GetUpvalue(AFuncIndex, AIndex: Integer): TLuaString;
begin
  Result := lua_getupvalue(L, AFuncIndex, AIndex);
end;

function TLuaState2.SetUpvalue(AFuncIndex, AIndex: Integer): TLuaString;
begin
  Result := lua_setupvalue(L, AFuncIndex, AIndex);
end;

function TLuaState2.UpvalueID(AFuncIndex, AIndex: Integer): Pointer;
begin
  Result := lua_upvalueid(L, AFuncIndex, AIndex);
end;

procedure TLuaState2.UpvalueJoin(AFuncIndex1, AIndex1, AFuncIndex2, AIndex2: Integer);
begin
  lua_upvaluejoin(L, AFuncIndex1, AIndex1, AFuncIndex2, AIndex2);
end;

procedure TLuaState2.SetHook(AFunc: TLuaHook; AMask, ACount: Integer);
begin
  lua_sethook(L, AFunc, AMask, ACount);
end;

function TLuaState2.GetHook: TLuaHook;
begin
  Result := lua_gethook(L);
end;

function TLuaState2.GetHookMask: Integer;
begin
  Result := lua_gethookmask(L);
end;

function TLuaState2.GetHookCount: Integer;
begin
  Result := lua_gethookcount(L);
end;

end.
