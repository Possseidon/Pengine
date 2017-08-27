unit LuaHeader;

interface

uses
  LuaConf, SysUtils, AnsiStrings;

const

  LuaDLL = 'lua53.dll';

  // mark for precompiled code ('<esc>Lua')
  LUA_SIGNATURE = #26'Lua';

  // option for multiple returns in 'lua_pcall' and 'lua_call'
  LUA_MULTRET = -1;

  // Pseudo-indices
  // (-LUAI_MAXSTACK is the minimum valid index; we keep some free empty
  // space after that to help overflow detection)
  LUA_REGISTRYINDEX = (-LUAI_MAXSTACK - 1000);
  // [ lua_upvalueindex ]

  // thread status
  LUA_OK = 0;
  LUA_YIELD_STATUS = 1; // name conflict with lua_yield function
  LUA_ERRRUN = 2;
  LUA_ERRSYNTAX = 3;
  LUA_ERRMEM = 4;
  LUA_ERRGCMM = 5;
  LUA_ERRERR = 6;

  // basic types
  LUA_TNONE = -1;

  LUA_TNIL = 0;
  LUA_TBOOLEAN = 1;
  LUA_TLIGHTUSERDATA = 2;
  LUA_TNUMBER = 3;
  LUA_TSTRING = 4;
  LUA_TTABLE = 5;
  LUA_TFUNCTION = 6;
  LUA_TUSERDATA = 7;
  LUA_TTHREAD = 8;

  LUA_NUMTAGS = 9;

  // minimum Lua stack available to a C function
  LUA_MINSTACK = 20;

  // predefined values in the registry
  LUA_RIDX_MAINTHREAD = 1;
  LUA_RIDX_GLOBALS = 2;
  LUA_RIDX_LAST = LUA_RIDX_GLOBALS;

  // Comparison and arithmetic functions
  LUA_OPADD = 0;
  LUA_OPSUB = 1;
  LUA_OPMUL = 2;
  LUA_OPMOD = 3;
  LUA_OPPOW = 4;
  LUA_OPDIV = 5;
  LUA_OPIDIV = 6;
  LUA_OPBAND = 7;
  LUA_OPBOR = 8;
  LUA_OPBXOR = 9;
  LUA_OPSHL = 10;
  LUA_OPSHR = 11;
  LUA_OPUNM = 12;
  LUA_OPBNOT = 13;

  LUA_OPEQ = 0;
  LUA_OPLT = 1;
  LUA_OPLE = 2;

  // garbage-collection options
  LUA_GCSTOP = 0;
  LUA_GCRESTART = 1;
  LUA_GCCOLLECT = 2;
  LUA_GCCOUNT = 3;
  LUA_GCCOUNTB = 4;
  LUA_GCSTEP = 5;
  LUA_GCSETPAUSE = 6;
  LUA_GCSETSTEPMUL = 7;
  LUA_GCISRUNNING = 9;

  // Debug API

  // Event Codes
  LUA_HOOKCALL = 0;
  LUA_HOOKRET = 1;
  LUA_HOOKLINE = 2;
  LUA_HOOKCOUNT = 3;
  LUA_HOOKTAILCALL = 4;

  // Event Mask
  LUA_MASKCALL = 1 shl LUA_HOOKCALL;
  LUA_MASKRET = 1 shl LUA_HOOKRET;
  LUA_MASKLINE = 1 shl LUA_HOOKLINE;
  LUA_MASKCOUNT = 1 shl LUA_HOOKCOUNT;
  // NO, LUA_MASKTAILCALL is not missing, according to the original C-Header file

type

  lua_Number = LuaConf.lua_Number;
  lua_Integer = LuaConf.lua_Integer;
  lua_Unsigned = LuaConf.lua_Unsigned;
  lua_KContext = LuaConf.lua_KContext;

  TLuaState = ^TLuaStateRec;

  Plua_Debug = ^lua_Debug;

  lua_Debug = record
  public
    event: Integer;
    name: PAnsiChar; // (n)
    namewhat: PAnsiChar; // (n) 'global', 'local', 'field', 'method'
    what: PAnsiChar; // (S) 'Lua', 'C', 'main', 'tail'
    source: PAnsiChar; // (S)
    currentline: Integer; // (l)
    linedefined: Integer; // (S)
    lastlinedefined: Integer; // (S)
    nups: Byte; // (u) number of upvalues
    nparams: Byte; // (u) number of parameters
    isvararg: ByteBool; // (u)
    istailcall: ByteBool; // (t)
    short_src: array [0 .. LUA_IDSIZE - 1] of AnsiChar; // (S)
    i_ci: Pointer; // active function
  end;

  lua_Hook = procedure(L: TLuaState; ar: Plua_Debug); cdecl;

  // Type for C (Delphi) functions registered with Lua
  lua_CFunction = function(L: TLuaState): Integer; cdecl;
  // Type for continuation functions
  lua_KFunction = function(L: TLuaState; status: Integer; ctx: lua_KContext): Integer; cdecl;

  // Type for functions that read/write blocks when loading/dumping Lua chunks
  lua_Reader = function(L: TLuaState; ud: Pointer; sz: PNativeUInt): PAnsiChar; cdecl;
  lua_Writer = function(L: TLuaState; p: Pointer; sz: NativeUInt; ud: Pointer): Integer; cdecl;

  // Type for memory-allocation functions
  lua_Alloc = function(ud, ptr: Pointer; osize, nsize: NativeUInt): Pointer; cdecl;

  lua_Ident = PAnsiChar;

  TLuaStatus = (
    lstOk = LUA_OK,
    lstYield,
    lstErrorRun,
    lstErrorSyntax,
    lstErrorMemory,
    lstErrorGCMM,
    lstErrorError
    );

  TLuaLoadError = (
    lleOK = LUA_OK,
    lleErrorSyntax = LUA_ERRSYNTAX,
    lleErrorMemory = LUA_ERRMEM,
    lleErrorGCMM = LUA_ERRGCMM
    );

  TLuaPCallError = (
    lceOK = LUA_OK,
    lceErrorRun = LUA_ERRRUN,
    lceErrorMemory = LUA_ERRMEM,
    lceErrorGCMM = LUA_ERRGCMM,
    lceErrorError = LUA_ERRERR
    );

  TLuaBasicTypes = (
    lbtNone = LUA_TNONE,
    lbtBoolean,
    lbtLightUserdata,
    lbtNumber,
    lbtString,
    lbtTable,
    lbtFunction,
    lbtUserdata,
    lbtThread,
    lbtNumTags
    );

  TLuaArithOp = (
    laoAdd = LUA_OPADD,
    laoSub,
    laoMul,
    laoMod,
    laoPow,
    laoDiv,
    laoIDiv,
    laoBAnd,
    laoBOr,
    laoBXor,
    laoShl,
    laoShr,
    laoUnMinus,
    laoBNot
    );

  TLuaCompareOp = (
    lcoEqual = LUA_OPEQ,
    lcoLessThan,
    lcoLessEqual
    );

  TLuaGCOption = (
    lgcStop = LUA_GCSTOP,
    lgcRestart,
    lgcCollect,
    lgcCount,
    lgcCountB,
    lgcStep,
    lgcSetPause,
    lgcSetStepMul,
    lgcIsRunning = LUA_GCISRUNNING
    );

  TLuaType = (
    ltNone = LUA_TNONE,
    ltNil,
    ltBoolean,
    ltLightUserdata,
    ltNumber,
    ltString,
    ltTable,
    ltFunction,
    ltUserdata,
    ltThread
    );

  TLuaTypeNoNone = ltNil .. ltThread;

  TLuaTypes = set of TLuaTypeNoNone;

  TLuaNumber = lua_Number;
  TLuaString = AnsiString;
  PLuaString = PAnsiChar;
  TLuaInteger = lua_Integer;
  TLuaCFunction = lua_CFunction;

  TLuaAlloc = lua_Alloc;

  TLuaReader = lua_Reader;
  TLuaWriter = lua_Writer;

  TLuaKContext = lua_KContext;
  TLuaKFunction = lua_KFunction;

  TLuaDebug = lua_Debug;
  TLuaHook = lua_Hook;

  { TLuaStateRec }

  TLuaStateRec = record
  private type
    PReaderRec = ^TReaderRec;

    TReaderRec = record
      Done: Boolean;
      Data: PAnsiChar;
    end;

  private
    class function Reader(L: TLuaState; ud: Pointer; size: PNativeUInt): PAnsiChar; cdecl; static;

  public
    // state manipulation
    procedure Close; inline;
    function NewThread: TLuaState; inline;

    function AtPanic(panicf: lua_CFunction): lua_CFunction; inline;

    function Version: Plua_Number; inline;

    // basic stack manipulation
    function AbsIndex(idx: Integer): Integer; inline;
    function GetTop: Integer; inline;
    procedure SetTop(index: Integer); inline;
    procedure PushValue(index: Integer = -1); inline;
    procedure Rotate(idx, n: Integer); inline;
    procedure Copy(fromidx, toidx: Integer); inline;
    function CheckStack(n: Integer): LongBool; inline;

    procedure XMove(&to: TLuaState; n: Integer); inline;

    // access functions (stack -> C (Delphi))
    function IsNumber(index: Integer = -1): LongBool; inline;
    function IsString(index: Integer = -1): LongBool; inline;
    function IsCFunction(index: Integer = -1): LongBool; inline;
    function IsInteger(index: Integer = -1): LongBool; inline;
    function IsUserdata(index: Integer = -1): LongBool; inline;
    function Type_X(index: Integer = -1): Integer; inline;
    function &Type(index: Integer = -1): TLuaType; inline;
    function TypeName_X(tp: Integer = -1): PAnsiChar; inline;
    function TypeName(tp: TLuaType): PAnsiChar; inline;

    function ToNumberX(isnum: PInteger; index: Integer = -1): lua_Number; inline;
    function ToIntegerX(isnum: PInteger; index: Integer = -1): lua_Integer; inline;
    function ToBoolean(index: Integer = -1): LongBool; inline;
    function ToLString(len: PNativeUInt; index: Integer = -1): PAnsiChar; inline;
    function RawLen(index: Integer = -1): NativeUInt; inline;
    function ToCFunction(index: Integer = -1): lua_CFunction; inline;
    function ToUserdata(index: Integer = -1): Pointer; inline;
    function ToThread(index: Integer = -1): TLuaState; inline;
    function ToPointer(index: Integer = -1): Pointer; inline;

    // Comparison and arithmetic functions
    procedure Arith(op: Integer); inline;

    function RawEqual(index1, index2: Integer): LongBool; inline;
    function Compare(index1, index2, op: Integer): LongBool; inline;

    // push functions (C (Delphi) -> stack)
    procedure PushNil; inline;
    procedure PushNumber(n: lua_Number); inline;
    procedure PushInteger(n: lua_Integer); inline;
    function PushLString(s: PAnsiChar; len: NativeUInt): PAnsiChar; inline;
    function PushVFString(fmt, argp: PAnsiChar): PAnsiChar; inline;
    function PushFString(fmt: PAnsiChar; args: array of const): PAnsiChar;
    function PushString(s: PAnsiChar): PAnsiChar; inline;
    procedure PushCClosure(fn: lua_CFunction; n: Integer); inline;
    procedure PushBoolean(b: LongBool); inline;
    procedure PushLightuserdata(p: Pointer); inline;
    function PushThread: LongBool; inline;

    // get functions (Lua -> stack)
    function GetGlobal_X(name: PAnsiChar): Integer; inline;
    function GetGlobal(name: PAnsiChar): TLuaType; inline;
    function GetTable(index: Integer = -1): Integer; inline;
    function GetField_X(k: PAnsiChar; index: Integer = -1): Integer; inline;
    function GetField(k: PAnsiChar; index: Integer = -1): TLuaType; inline;
    function GetI_X(i: lua_Integer; index: Integer = -1): Integer; inline;
    function GetI(i: lua_Integer; index: Integer = -1): TLuaType; inline;
    function RawGet(index: Integer = -1): Integer; inline;
    function RawGetI(i: lua_Integer; index: Integer = -1): Integer; inline;
    function RawGetP(p: Pointer; index: Integer = -1): Integer; inline;

    procedure CreateTable(narr, nrec: Integer); inline;
    function NewUserdata(size: NativeUInt): Pointer; inline;
    function GetMetatable(index: Integer = -1): LongBool; inline;
    function GetUservalue(index: Integer = -1): Integer; inline;

    // set functions (stack -> Lua)
    procedure SetGlobal(name: PAnsiChar); inline;
    procedure SetTable(index: Integer); inline;
    procedure SetField(k: PAnsiChar; index: Integer); inline;
    procedure SetI(i: lua_Integer; index: Integer); inline;
    procedure RawSet(index: Integer); inline;
    procedure RawSetI(i: lua_Integer; index: Integer); inline;
    procedure RawSetP(p: Pointer; index: Integer); inline;
    procedure SetMetatable(index: Integer); inline;
    procedure SetUservalue(index: Integer); inline;

    // 'load' and 'call' functions (load and run Lua code)
    procedure CallK(nargs, nresults: Integer; ctx: lua_KContext; k: lua_KFunction); inline;
    procedure Call(nargs, nresults: Integer); inline;
    function PCallK_X(nargs, nresults, msgh: Integer; ctx: lua_KContext; k: lua_KFunction): Integer; inline;
    function PCallK(nargs, nresults, msgh: Integer; ctx: lua_KContext; k: lua_KFunction): TLuaPCallError; inline;
    function PCall_X(nargs, nresults, msgh: Integer): Integer; inline;
    function PCall(nargs, nresults, msgh: Integer): TLuaPCallError; inline;
    function Load_X(Reader: lua_Reader; Data: Pointer; chunkname, mode: PAnsiChar): Integer; inline;
    function Load(Reader: TLuaReader; Data: Pointer; chunkname, mode: PAnsiChar): TLuaLoadError; inline;
    function Dump(writer: lua_Writer; Data: Pointer; strip: Integer): Integer; inline;

    // coroutine functions
    function YieldK(nresults: Integer; ctx: lua_KContext; k: lua_KFunction): Integer; inline;
    function Resume(from: TLuaState; nargs: Integer): Integer; inline;
    function Status: Integer; inline;
    function IsYieldable: LongBool; inline;
    function Yield(nresults: Integer): Integer; inline;

    // garbage-collection function
    function GC(what, Data: Integer): Integer; inline;

    // miscellaneous functions
    function Error_X: Integer; inline;
    function Next(index: Integer): LongBool; inline;
    procedure Concat(n: Integer); inline;
    procedure Len(index: Integer); inline;
    function StringToNumber(s: PAnsiChar): NativeUInt; inline;

    function GetAllocF(ud: PPointer): lua_Alloc; inline;
    procedure SetAllocF(f: lua_Alloc; ud: Pointer); inline;

    // some useful macros
    function GetExtraSpace: Pointer; inline;
    function ToNumber(index: Integer = -1): lua_Number; inline;
    function ToInteger(index: Integer = -1): lua_Integer; inline;

    procedure Pop(n: Integer = 1); inline;

    procedure NewTable; inline;

    procedure &Register(name: PAnsiChar; f: lua_CFunction); inline;

    procedure PushCFunction(f: lua_CFunction); inline;

    function IsFunction(index: Integer = -1): LongBool; inline;
    function IsTable(index: Integer = -1): LongBool; inline;
    function IsLightuserdata(index: Integer = -1): LongBool; inline;
    function IsNil(index: Integer = -1): LongBool; inline;
    function IsBoolean(index: Integer = -1): LongBool; inline;
    function IsThread(index: Integer = -1): LongBool; inline;
    function IsNone(index: Integer = -1): LongBool; inline;
    function IsNoneOrNil(index: Integer = -1): LongBool; inline;

    function PushLiteral(s: PAnsiChar): PAnsiChar; inline;

    procedure PushGlobalTable; inline;

    function ToString_X(index: Integer = -1): PAnsiChar; inline;
    function ToString(index: Integer = -1): PAnsiChar;

    procedure Insert(index: Integer); inline;

    procedure Remove(index: Integer); inline;

    procedure Replace(index: Integer); inline;

    // Debug API

    function GetStack(level: Integer; ar: Plua_Debug): LongBool; inline;
    function GetInfo(what: PAnsiChar; ar: Plua_Debug): LongBool; inline;
    function GetLocal(ar: Plua_Debug; n: Integer): PAnsiChar; inline;
    function SetLocal(ar: Plua_Debug; n: Integer): PAnsiChar; inline;
    function GetUpvalue(funcindex, n: Integer): PAnsiChar; inline;
    function SetUpvalue(funcindex, n: Integer): PAnsiChar; inline;

    function UpvalueID(funcindex, n: Integer): Pointer; inline;
    procedure UpvalueJoin(funcindex1, n1, funcindex2, n2: Integer); inline;

    procedure SetHook(f: lua_Hook; mask, count: Integer); inline;
    function GetHook: lua_Hook; inline;
    function GetHookMask: Integer; inline;
    function GetHookCount: Integer; inline;

    class function UpvalueIndex(i: Integer): Integer; static; inline;

    // --- Custom Functions ---

    property Top: Integer read GetTop write SetTop;
    function TypeNameAt(index: Integer = -1): PAnsiChar; inline;

    function LoadString(AString: AnsiString; AChunkName: AnsiString = ''): TLuaLoadError;

    procedure Where(ALevel: Integer);

    function Error(AMessage: AnsiString; ALevel: Integer = 1): Integer;
    function ErrorFmt(AFmt: AnsiString; AArgs: array of const; ALevel: Integer = 1): Integer;

    function FormatStack: AnsiString;

    function FormatTypes(ATypes: TLuaTypes; ANone: Boolean = False): AnsiString;

    procedure CheckType(AIndex: Integer; AType: TLuaType); overload;
    procedure CheckType(AIndex: Integer; ATypes: TLuaTypes; ANone: Boolean = False); overload;
    function CheckAny(AIndex: Integer): TLuaType; overload;
    procedure CheckEnd(AIndex: Integer); overload; inline;

    function CheckOrDefault(AIndex: Integer; ADefault: TLuaInteger): TLuaInteger; overload;
    function CheckOrDefault(AIndex: Integer; ADefault: TLuaNumber): TLuaNumber; overload;
    function CheckOrDefault(AIndex: Integer; ADefault: TLuaString): TLuaString; overload;
    function CheckOrDefault(AIndex: Integer; ADefault: Boolean): Boolean; overload;

  end;

function LuaDefaultAlloc(ud, ptr: Pointer; osize, nsize: NativeUInt): Pointer; cdecl;
function NewLuaState(AAllocFunc: TLuaAlloc; AUserData: Pointer = nil): TLuaState;

// --- DLL ---

// state manipulation
function lua_newstate(f: lua_Alloc; ud: Pointer): TLuaState; cdecl; external LuaDLL;
procedure lua_close(L: TLuaState); cdecl; external LuaDLL;
function lua_newthread(L: TLuaState): TLuaState; cdecl; external LuaDLL;

function lua_atpanic(L: TLuaState; panicf: lua_CFunction): lua_CFunction; cdecl; external LuaDLL;

function lua_version(L: TLuaState): Plua_Number; cdecl; external LuaDLL;

// basic stack manipulation
function lua_absindex(L: TLuaState; idx: Integer): Integer; cdecl; external LuaDLL;
function lua_gettop(L: TLuaState): Integer; cdecl; external LuaDLL;
procedure lua_settop(L: TLuaState; index: Integer); cdecl; external LuaDLL;
procedure lua_pushvalue(L: TLuaState; index: Integer); cdecl; external LuaDLL;
procedure lua_rotate(L: TLuaState; idx, n: Integer); cdecl; external LuaDLL;
procedure lua_copy(L: TLuaState; fromidx, toidx: Integer); cdecl; external LuaDLL;
function lua_checkstack(L: TLuaState; n: Integer): LongBool; cdecl; external LuaDLL;

procedure lua_xmove(from, &to: TLuaState; n: Integer); cdecl; external LuaDLL;

// access functions (stack -> C (Delphi))
function lua_isnumber(L: TLuaState; index: Integer): LongBool; cdecl; external LuaDLL;
function lua_isstring(L: TLuaState; index: Integer): LongBool; cdecl; external LuaDLL;
function lua_iscfunction(L: TLuaState; index: Integer): LongBool; cdecl; external LuaDLL;
function lua_isinteger(L: TLuaState; index: Integer): LongBool; cdecl; external LuaDLL;
function lua_isuserdata(L: TLuaState; index: Integer): LongBool; cdecl; external LuaDLL;
function lua_type(L: TLuaState; index: Integer): Integer; cdecl; external LuaDLL;
function lua_typename(L: TLuaState; tp: Integer): PAnsiChar; cdecl; external LuaDLL;

function lua_tonumberx(L: TLuaState; index: Integer; isnum: PInteger): lua_Number; cdecl; external LuaDLL;
function lua_tointegerx(L: TLuaState; index: Integer; isnum: PInteger): lua_Integer; cdecl; external LuaDLL;
function lua_toboolean(L: TLuaState; index: Integer): LongBool; cdecl; external LuaDLL;
function lua_tolstring(L: TLuaState; index: Integer; len: PNativeUInt): PAnsiChar; cdecl; external LuaDLL;
function lua_rawlen(L: TLuaState; index: Integer): NativeUInt; cdecl; external LuaDLL;
function lua_tocfunction(L: TLuaState; index: Integer): lua_CFunction; cdecl; external LuaDLL;
function lua_touserdata(L: TLuaState; index: Integer): Pointer; cdecl; external LuaDLL;
function lua_tothread(L: TLuaState; index: Integer): TLuaState; cdecl; external LuaDLL;
function lua_topointer(L: TLuaState; index: Integer): Pointer; cdecl; external LuaDLL;

// Comparison and arithmetic functions
procedure lua_arith(L: TLuaState; op: Integer); cdecl; external LuaDLL;

function lua_rawequal(L: TLuaState; index1, index2: Integer): LongBool; cdecl; external LuaDLL;
function lua_compare(L: TLuaState; index1, index2, op: Integer): LongBool; cdecl; external LuaDLL;

// push functions (C (Delphi) -> stack)
procedure lua_pushnil(L: TLuaState); cdecl; external LuaDLL;
procedure lua_pushnumber(L: TLuaState; n: lua_Number); cdecl; external LuaDLL;
procedure lua_pushinteger(L: TLuaState; n: lua_Integer); cdecl; external LuaDLL;
function lua_pushlstring(L: TLuaState; s: PAnsiChar; len: NativeUInt): PAnsiChar; cdecl; external LuaDLL;
function lua_pushstring(L: TLuaState; s: PAnsiChar): PAnsiChar; cdecl; external LuaDLL;
function lua_pushvfstring(L: TLuaState; fmt, argp: PAnsiChar): PAnsiChar; cdecl; external LuaDLL;
function lua_pushfstring(L: TLuaState; fmt: PAnsiChar): PAnsiChar; varargs; cdecl; cdecl; external LuaDLL;
procedure lua_pushcclosure(L: TLuaState; fn: lua_CFunction; n: Integer); cdecl; external LuaDLL;
procedure lua_pushboolean(L: TLuaState; b: LongBool); cdecl; external LuaDLL;
procedure lua_pushlightuserdata(L: TLuaState; p: Pointer); cdecl; external LuaDLL;
function lua_pushthread(L: TLuaState): LongBool; cdecl; external LuaDLL;

// get functions (Lua -> stack)
function lua_getglobal(L: TLuaState; name: PAnsiChar): Integer; cdecl; external LuaDLL;
function lua_gettable(L: TLuaState; index: Integer): Integer; cdecl; external LuaDLL;
function lua_getfield(L: TLuaState; index: Integer; k: PAnsiChar): Integer; cdecl; external LuaDLL;
function lua_geti(L: TLuaState; index: Integer; i: lua_Integer): Integer; cdecl; external LuaDLL;
function lua_rawget(L: TLuaState; index: Integer): Integer; cdecl; external LuaDLL;
function lua_rawgeti(L: TLuaState; index: Integer; i: lua_Integer): Integer; cdecl; external LuaDLL;
function lua_rawgetp(L: TLuaState; index: Integer; p: Pointer): Integer; cdecl; external LuaDLL;

procedure lua_createtable(L: TLuaState; narr, nrec: Integer); cdecl; external LuaDLL;
function lua_newuserdata(L: TLuaState; size: NativeUInt): Pointer; cdecl; external LuaDLL;
function lua_getmetatable(L: TLuaState; index: Integer): LongBool; cdecl; external LuaDLL;
function lua_getuservalue(L: TLuaState; index: Integer): Integer; cdecl; external LuaDLL;

// set functions (stack -> Lua)
procedure lua_setglobal(L: TLuaState; name: PAnsiChar); cdecl; external LuaDLL;
procedure lua_settable(L: TLuaState; index: Integer); cdecl; external LuaDLL;
procedure lua_setfield(L: TLuaState; index: Integer; k: PAnsiChar); cdecl; external LuaDLL;
procedure lua_seti(L: TLuaState; index: Integer; i: lua_Integer); cdecl; external LuaDLL;
procedure lua_rawset(L: TLuaState; index: Integer); cdecl; external LuaDLL;
procedure lua_rawseti(L: TLuaState; index: Integer; i: lua_Integer); cdecl; external LuaDLL;
procedure lua_rawsetp(L: TLuaState; index: Integer; p: Pointer); cdecl; external LuaDLL;
function lua_setmetatable(L: TLuaState; index: Integer): Integer; cdecl; external LuaDLL;
procedure lua_setuservalue(L: TLuaState; index: Integer); cdecl; external LuaDLL;

// 'load' and 'call' functions (load and run Lua code)
procedure lua_callk(L: TLuaState; nargs, nresults: Integer; ctx: lua_KContext; k: lua_KFunction); cdecl;
  external LuaDLL;
// [ lua_call ]
function lua_pcallk(L: TLuaState; nargs, nresults, msgh: Integer; ctx: lua_KContext; k: lua_KFunction): Integer;
  cdecl; external LuaDLL;
// [ lua_pcall ]
function lua_load(L: TLuaState; Reader: lua_Reader; Data: Pointer; chunkname, mode: PAnsiChar): Integer;
  cdecl; external LuaDLL;
function lua_dump(L: TLuaState; writer: lua_Writer; Data: Pointer; strip: Integer): Integer; cdecl; external LuaDLL;

// coroutine functions
function lua_yieldk(L: TLuaState; nresults: Integer; ctx: lua_KContext; k: lua_KFunction): Integer; cdecl;
  external LuaDLL;
function lua_resume(L, from: TLuaState; narg: Integer): Integer; cdecl; external LuaDLL;
function lua_status(L: TLuaState): Integer; cdecl; external LuaDLL;
function lua_isyieldable(L: TLuaState): LongBool; cdecl; external LuaDLL;
// [ lua_yield ]

// garbage-collection function
function lua_gc(L: TLuaState; what, Data: Integer): Integer; cdecl; external LuaDLL;

// miscellaneous functions
function lua_error(L: TLuaState): Integer; cdecl; external LuaDLL;
function lua_next(L: TLuaState; index: Integer): LongBool; cdecl; external LuaDLL;
procedure lua_concat(L: TLuaState; n: Integer); cdecl; external LuaDLL;
procedure lua_len(L: TLuaState; index: Integer); cdecl; external LuaDLL;
function lua_stringtonumber(L: TLuaState; s: PAnsiChar): NativeUInt; cdecl; external LuaDLL;

function lua_getallocf(L: TLuaState; ud: PPointer): lua_Alloc; cdecl; external LuaDLL;
procedure lua_setallocf(L: TLuaState; f: lua_Alloc; ud: Pointer); cdecl; external LuaDLL;

// macro help functions
function lua_upvalueindex(i: Integer): Integer; inline;
procedure lua_call(L: TLuaState; nargs, nresults: Integer); inline;
function lua_pcall(L: TLuaState; nargs, nresults, msgh: Integer): Integer; inline;
function lua_yield(L: TLuaState; nresults: Integer): Integer; inline;
// alternative
// function lua_pushfstring(L: TLuaState; fmt: PAnsiChar; args: array of const): PAnsiChar;

// some useful macros
function lua_getextraspace(L: TLuaState): Pointer; inline;
function lua_tonumber(L: TLuaState; index: Integer): lua_Number; inline;
function lua_tointeger(L: TLuaState; index: Integer): lua_Integer; inline;

procedure lua_pop(L: TLuaState; n: Integer); inline;

procedure lua_newtable(L: TLuaState); inline;

procedure lua_register(L: TLuaState; name: PAnsiChar; f: lua_CFunction); inline;

procedure lua_pushcfunction(L: TLuaState; f: lua_CFunction); inline;

function lua_isfunction(L: TLuaState; index: Integer): LongBool; inline;
function lua_istable(L: TLuaState; index: Integer): LongBool; inline;
function lua_islightuserdata(L: TLuaState; index: Integer): LongBool; inline;
function lua_isnil(L: TLuaState; index: Integer): LongBool; inline;
function lua_isboolean(L: TLuaState; index: Integer): LongBool; inline;
function lua_isthread(L: TLuaState; index: Integer): LongBool; inline;
function lua_isnone(L: TLuaState; index: Integer): LongBool; inline;
function lua_isnoneornil(L: TLuaState; index: Integer): LongBool; inline;

function lua_pushliteral(L: TLuaState; s: PAnsiChar): PAnsiChar; inline;

procedure lua_pushglobaltable(L: TLuaState); inline;

function lua_tostring(L: TLuaState; index: Integer): PAnsiChar; inline;

procedure lua_insert(L: TLuaState; index: Integer); inline;

procedure lua_remove(L: TLuaState; index: Integer); inline;

procedure lua_replace(L: TLuaState; index: Integer); inline;

// Debug API

function lua_getstack(L: TLuaState; level: Integer; ar: Plua_Debug): LongBool; cdecl; external LuaDLL;
function lua_getinfo(L: TLuaState; what: PAnsiChar; ar: Plua_Debug): LongBool; cdecl; external LuaDLL;
function lua_getlocal(L: TLuaState; ar: Plua_Debug; n: Integer): PAnsiChar; cdecl; external LuaDLL;
function lua_setlocal(L: TLuaState; ar: Plua_Debug; n: Integer): PAnsiChar; cdecl; external LuaDLL;
function lua_getupvalue(L: TLuaState; funcindex, n: Integer): PAnsiChar; cdecl; external LuaDLL;
function lua_setupvalue(L: TLuaState; funcindex, n: Integer): PAnsiChar; cdecl; external LuaDLL;

function lua_upvalueid(L: TLuaState; funcindex, n: Integer): Pointer; cdecl; external LuaDLL;
procedure lua_upvaluejoin(L: TLuaState; funcindex1, n1, funcindex2, n2: Integer); cdecl; external LuaDLL;

procedure lua_sethook(L: TLuaState; f: lua_Hook; mask, count: Integer); cdecl; external LuaDLL;
function lua_gethook(L: TLuaState): lua_Hook; cdecl; external LuaDLL;
function lua_gethookmask(L: TLuaState): Integer; cdecl; external LuaDLL;
function lua_gethookcount(L: TLuaState): Integer; cdecl; external LuaDLL;

implementation

// helper functions

function lua_upvalueindex(i: Integer): Integer;
begin
  Result := LUA_REGISTRYINDEX - i;
end;

procedure lua_call(L: TLuaState; nargs, nresults: Integer);
begin
  lua_callk(L, nargs, nresults, 0, nil);
end;

function lua_pcall(L: TLuaState; nargs, nresults, msgh: Integer): Integer;
begin
  Result := lua_pcallk(L, nargs, nresults, msgh, 0, nil);
end;

function lua_yield(L: TLuaState; nresults: Integer): Integer;
begin
  Result := lua_yieldk(L, nresults, 0, nil);
end;

function lua_getextraspace(L: TLuaState): Pointer;
begin
  Result := Pointer(PAnsiChar(L) - LUA_EXTRASPACE);
end;

function lua_tonumber(L: TLuaState; index: Integer): lua_Number;
begin
  Result := lua_tonumberx(L, index, nil);
end;

function lua_tointeger(L: TLuaState; index: Integer): lua_Integer;
begin
  Result := lua_tointegerx(L, index, nil);
end;

procedure lua_pop(L: TLuaState; n: Integer);
begin
  lua_settop(L, -n - 1);
end;

procedure lua_newtable(L: TLuaState);
begin
  lua_createtable(L, 0, 0);
end;

procedure lua_register(L: TLuaState; name: PAnsiChar; f: lua_CFunction);
begin
  lua_pushcfunction(L, f);
  lua_setglobal(L, name);
end;

procedure lua_pushcfunction(L: TLuaState; f: lua_CFunction);
begin
  lua_pushcclosure(L, f, 0);
end;

function lua_isfunction(L: TLuaState; index: Integer): LongBool;
begin
  Result := lua_type(L, index) = LUA_TFUNCTION;
end;

function lua_istable(L: TLuaState; index: Integer): LongBool;
begin
  Result := lua_type(L, index) = LUA_TTABLE;
end;

function lua_islightuserdata(L: TLuaState; index: Integer): LongBool;
begin
  Result := lua_type(L, index) = LUA_TLIGHTUSERDATA;
end;

function lua_isnil(L: TLuaState; index: Integer): LongBool;
begin
  Result := lua_type(L, index) = LUA_TNIL;
end;

function lua_isboolean(L: TLuaState; index: Integer): LongBool;
begin
  Result := lua_type(L, index) = LUA_TBOOLEAN;
end;

function lua_isthread(L: TLuaState; index: Integer): LongBool;
begin
  Result := lua_type(L, index) = LUA_TTHREAD;
end;

function lua_isnone(L: TLuaState; index: Integer): LongBool;
begin
  Result := lua_type(L, index) = LUA_TNONE;
end;

function lua_isnoneornil(L: TLuaState; index: Integer): LongBool;
begin
  Result := lua_type(L, index) <= 0;
end;

function lua_pushliteral(L: TLuaState; s: PAnsiChar): PAnsiChar;
begin
  Result := lua_pushstring(L, s);
end;

procedure lua_pushglobaltable(L: TLuaState);
begin
  lua_rawgeti(L, LUA_REGISTRYINDEX, LUA_RIDX_GLOBALS);
end;

function lua_tostring(L: TLuaState; index: Integer): PAnsiChar;
begin
  Result := lua_tolstring(L, index, nil);
end;

procedure lua_insert(L: TLuaState; index: Integer);
begin
  lua_rotate(L, index, 1);
end;

procedure lua_remove(L: TLuaState; index: Integer);
begin
  lua_rotate(L, index, -1);
  lua_pop(L, 1);
end;

procedure lua_replace(L: TLuaState; index: Integer);
begin
  lua_copy(L, -1, index);
  lua_pop(L, 1);
end;

function LuaDefaultAlloc(ud, ptr: Pointer; osize, nsize: NativeUInt): Pointer; cdecl;
begin
  if nsize = 0 then
  begin
    FreeMemory(ptr);
    Exit(nil);
  end;
  Result := ReallocMemory(ptr, nsize);
end;

{ TLuaStateRec }

function NewLuaState(AAllocFunc: TLuaAlloc; AUserData: Pointer): TLuaState;
begin
  Result := lua_newstate(AAllocFunc, AUserData);
end;

class function TLuaStateRec.Reader(L: TLuaState; ud: Pointer; size: PNativeUInt): PAnsiChar;
var
  R: PReaderRec;
begin
  R := PReaderRec(ud);
  if R.Done then
    Exit(nil);
  R.Done := True;
  size^ := Length(R.Data);
  Result := R.Data;
end;

procedure TLuaStateRec.Close;
begin
  lua_close(@Self);
end;

function TLuaStateRec.NewThread: TLuaState;
begin
  Result := lua_newthread(@Self);
end;

function TLuaStateRec.AtPanic(panicf: lua_CFunction): lua_CFunction;
begin
  Result := lua_atpanic(@Self, panicf);
end;

function TLuaStateRec.Version: Plua_Number;
begin
  Result := lua_version(@Self);
end;

function TLuaStateRec.AbsIndex(idx: Integer): Integer;
begin
  Result := lua_absindex(@Self, idx);
end;

function TLuaStateRec.GetTop: Integer;
begin
  Result := lua_gettop(@Self);
end;

procedure TLuaStateRec.SetTop(index: Integer);
begin
  lua_settop(@Self, index);
end;

procedure TLuaStateRec.PushValue(index: Integer);
begin
  lua_pushvalue(@Self, index);
end;

procedure TLuaStateRec.Rotate(idx, n: Integer);
begin
  lua_rotate(@Self, idx, n);
end;

procedure TLuaStateRec.Copy(fromidx, toidx: Integer);
begin
  lua_copy(@Self, fromidx, toidx);
end;

function TLuaStateRec.CheckStack(n: Integer): LongBool;
begin
  Result := lua_checkstack(@Self, n);
end;

procedure TLuaStateRec.XMove(&to: TLuaState; n: Integer);
begin
  lua_xmove(@Self, &to, n);
end;

function TLuaStateRec.IsNumber(index: Integer): LongBool;
begin
  Result := lua_isnumber(@Self, index);
end;

function TLuaStateRec.IsString(index: Integer): LongBool;
begin
  Result := lua_isstring(@Self, index);
end;

function TLuaStateRec.IsCFunction(index: Integer): LongBool;
begin
  Result := lua_iscfunction(@Self, index);
end;

function TLuaStateRec.IsInteger(index: Integer): LongBool;
begin
  Result := lua_isinteger(@Self, index);
end;

function TLuaStateRec.IsUserdata(index: Integer): LongBool;
begin
  Result := lua_isuserdata(@Self, index);
end;

function TLuaStateRec.Type_X(index: Integer): Integer;
begin
  Result := lua_type(@Self, index);
end;

function TLuaStateRec.&Type(index: Integer): TLuaType;
begin
  Result := TLuaType(Type_X(index));
end;

function TLuaStateRec.TypeName_X(tp: Integer): PAnsiChar;
begin
  Result := lua_typename(@Self, tp);
end;

function TLuaStateRec.TypeName(tp: TLuaType): PAnsiChar;
begin
  Result := TypeName_X(Ord(tp));
end;

function TLuaStateRec.TypeNameAt(index: Integer): PAnsiChar;
begin
  Result := TypeName_X(Type_X(index));
end;

function TLuaStateRec.LoadString(AString: AnsiString; AChunkName: AnsiString): TLuaLoadError;
var
  R: TReaderRec;
begin
  R.Done := False;
  R.Data := PPAnsiChar(@AString)^;
  Result := Load(Reader, @R, PPAnsiChar(@AChunkName)^, 't');
end;

procedure TLuaStateRec.Where(ALevel: Integer);
var
  ActRec: lua_Debug;
begin
  if GetStack(ALevel, @ActRec) then
  begin
    GetInfo('Sl', @ActRec);
    if ActRec.currentline > 0 then
    begin
      PushFString('%s:%d: ', [ActRec.short_src, ActRec.currentline]);
      Exit;
    end;
  end;
  PushFString('', []);
end;

function TLuaStateRec.Error(AMessage: AnsiString; ALevel: Integer): Integer;
begin
  Where(ALevel);
  PushString(PPAnsiChar(@AMessage)^);
  Concat(2);
  Result := Error_X;
end;

function TLuaStateRec.ErrorFmt(AFmt: AnsiString; AArgs: array of const; ALevel: Integer): Integer;
begin
  Result := Error(Format(AFmt, AArgs), ALevel);
end;

function TLuaStateRec.FormatStack: AnsiString;
var
  I: Integer;
begin
  Result := '';
  for I := Top downto 1 do
  begin
    PushValue;
    Result := Result + AnsiStrings.Format('[%d/-%d] %s', [I, Top - I, ToString]) + #10;
    Pop;
  end;
end;

function TLuaStateRec.FormatTypes(ATypes: TLuaTypes; ANone: Boolean): AnsiString;
var
  T: TLuaTypeNoNone;
  First: Boolean;
begin
  First := not ANone;
  if ANone then
    Result := TypeName(ltNone);
  for T in ATypes do
  begin
    if First then
    begin
      Result := TypeName(T);
      First := False;
      Continue;
    end;
    Result := Result + '/' + TypeName(T);
  end;
end;

procedure TLuaStateRec.CheckType(AIndex: Integer; AType: TLuaType);
var
  T: TLuaType;
begin
  T := &Type(AIndex);
  if T <> AType then
    ErrorFmt('arg #%d: %s expected, got %s', [AIndex, TypeName(AType), TypeNameAt(AIndex)]);
end;

procedure TLuaStateRec.CheckType(AIndex: Integer; ATypes: TLuaTypes; ANone: Boolean);
var
  T: TLuaType;
begin
  T := &Type(AIndex);
  if (T = ltNone) and not ANone or (T <> ltNone) and not(T in ATypes) then
    ErrorFmt('arg #%d: %s expected, got %s', [AIndex, FormatTypes(ATypes, ANone), TypeName(T)]);
end;

function TLuaStateRec.CheckAny(AIndex: Integer): TLuaType;
begin
  Result := &Type(AIndex);
  if Result = ltNone then
    ErrorFmt('arg #%d: argument expected, got %s', [AIndex, TypeName(Result)]);
end;

procedure TLuaStateRec.CheckEnd(AIndex: Integer);
begin
  CheckType(AIndex, ltNone);
end;

function TLuaStateRec.CheckOrDefault(AIndex: Integer; ADefault: TLuaInteger): TLuaInteger;
var
  isnum: LongBool;
begin
  CheckType(AIndex, [ltNil, ltNumber], True);
  if IsNoneOrNil(AIndex) then
    Exit(ADefault);
  Result := ToIntegerX(@isnum, AIndex);
  if not isnum then
    ErrorFmt('arg #%d: number must be an integer', [AIndex]);
end;

function TLuaStateRec.CheckOrDefault(AIndex: Integer; ADefault: TLuaNumber): TLuaNumber;
begin
  CheckType(AIndex, [ltNil, ltNumber], True);
  if IsNoneOrNil(AIndex) then
    Exit(ADefault);
  Result := ToNumber(AIndex);
end;

function TLuaStateRec.CheckOrDefault(AIndex: Integer; ADefault: TLuaString): TLuaString;
begin
  CheckType(AIndex, [ltNil, ltString], True);
  if IsNoneOrNil(AIndex) then
    Exit(ADefault);
  Result := ToString_X(AIndex);
end;

function TLuaStateRec.CheckOrDefault(AIndex: Integer; ADefault: Boolean): Boolean;
begin
  CheckType(AIndex, [ltNil, ltBoolean], True);
  if IsNoneOrNil(AIndex) then
    Exit(ADefault);
  Result := ToBoolean(AIndex);
end;

function TLuaStateRec.ToNumberX(isnum: PInteger; index: Integer): lua_Number;
begin
  Result := lua_tonumberx(@Self, index, isnum);
end;

function TLuaStateRec.ToIntegerX(isnum: PInteger; index: Integer): lua_Integer;
begin
  Result := lua_tointegerx(@Self, index, isnum);
end;

function TLuaStateRec.ToBoolean(index: Integer): LongBool;
begin
  Result := lua_toboolean(@Self, index);
end;

function TLuaStateRec.ToLString(len: PNativeUInt; index: Integer): PAnsiChar;
begin
  Result := lua_tolstring(@Self, index, len);
end;

function TLuaStateRec.RawLen(index: Integer): NativeUInt;
begin
  Result := lua_rawlen(@Self, index);
end;

function TLuaStateRec.ToCFunction(index: Integer): lua_CFunction;
begin
  Result := lua_tocfunction(@Self, index);
end;

function TLuaStateRec.ToUserdata(index: Integer): Pointer;
begin
  Result := lua_touserdata(@Self, index);
end;

function TLuaStateRec.ToThread(index: Integer): TLuaState;
begin
  Result := lua_tothread(@Self, index);
end;

function TLuaStateRec.ToPointer(index: Integer): Pointer;
begin
  Result := lua_topointer(@Self, index);
end;

procedure TLuaStateRec.Arith(op: Integer);
begin
  lua_arith(@Self, op);
end;

function TLuaStateRec.RawEqual(index1, index2: Integer): LongBool;
begin
  Result := lua_rawequal(@Self, index1, index2);
end;

function TLuaStateRec.Compare(index1, index2, op: Integer): LongBool;
begin
  Result := lua_compare(@Self, index1, index2, op);
end;

procedure TLuaStateRec.PushNil;
begin
  lua_pushnil(@Self);
end;

procedure TLuaStateRec.PushNumber(n: lua_Number);
begin
  lua_pushnumber(@Self, n);
end;

procedure TLuaStateRec.PushInteger(n: lua_Integer);
begin
  lua_pushinteger(@Self, n);
end;

function TLuaStateRec.PushLString(s: PAnsiChar; len: NativeUInt): PAnsiChar;
begin
  Result := lua_pushlstring(@Self, s, len);
end;

function TLuaStateRec.PushVFString(fmt, argp: PAnsiChar): PAnsiChar;
begin
  Result := lua_pushvfstring(@Self, fmt, argp);
end;

function TLuaStateRec.PushString(s: PAnsiChar): PAnsiChar;
begin
  Result := lua_pushstring(@Self, s);
end;

procedure TLuaStateRec.PushCClosure(fn: lua_CFunction; n: Integer);
begin
  lua_pushcclosure(@Self, fn, n);
end;

procedure TLuaStateRec.PushBoolean(b: LongBool);
begin
  lua_pushboolean(@Self, b);
end;

procedure TLuaStateRec.PushLightuserdata(p: Pointer);
begin
  lua_pushlightuserdata(@Self, p);
end;

function TLuaStateRec.PushThread: LongBool;
begin
  Result := lua_pushthread(@Self);
end;

function TLuaStateRec.GetGlobal(name: PAnsiChar): TLuaType;
begin
  Result := TLuaType(GetGlobal_X(name));
end;

function TLuaStateRec.GetGlobal_X(name: PAnsiChar): Integer;
begin
  Result := lua_getglobal(@Self, name);
end;

function TLuaStateRec.GetTable(index: Integer): Integer;
begin
  Result := lua_gettable(@Self, index);
end;

function TLuaStateRec.GetField_X(k: PAnsiChar; index: Integer): Integer;
begin
  Result := lua_getfield(@Self, index, k);
end;

function TLuaStateRec.GetField(k: PAnsiChar; index: Integer): TLuaType;
begin
  Result := TLuaType(GetField_X(k, index));
end;

function TLuaStateRec.GetI_X(i: lua_Integer; index: Integer): Integer;
begin
  Result := lua_geti(@Self, index, i);
end;

function TLuaStateRec.GetI(i: lua_Integer; index: Integer): TLuaType;
begin
  Result := TLuaType(GetI_X(i, index));
end;

function TLuaStateRec.RawGet(index: Integer): Integer;
begin
  Result := lua_rawget(@Self, index);
end;

function TLuaStateRec.RawGetI(i: lua_Integer; index: Integer): Integer;
begin
  Result := lua_rawgeti(@Self, index, i);
end;

function TLuaStateRec.RawGetP(p: Pointer; index: Integer): Integer;
begin
  Result := lua_rawgetp(@Self, index, p);
end;

procedure TLuaStateRec.CreateTable(narr, nrec: Integer);
begin
  lua_createtable(@Self, narr, nrec);
end;

function TLuaStateRec.NewUserdata(size: NativeUInt): Pointer;
begin
  Result := lua_newuserdata(@Self, size);
end;

function TLuaStateRec.GetMetatable(index: Integer): LongBool;
begin
  Result := lua_getmetatable(@Self, index);
end;

function TLuaStateRec.GetUservalue(index: Integer): Integer;
begin
  Result := lua_getuservalue(@Self, index);
end;

procedure TLuaStateRec.SetGlobal(name: PAnsiChar);
begin
  lua_setglobal(@Self, name);
end;

procedure TLuaStateRec.SetTable(index: Integer);
begin
  lua_settable(@Self, index);
end;

procedure TLuaStateRec.SetField(k: PAnsiChar; index: Integer);
begin
  lua_setfield(@Self, index, k);
end;

procedure TLuaStateRec.SetI(i: lua_Integer; index: Integer);
begin
  lua_seti(@Self, index, i);
end;

procedure TLuaStateRec.RawSet(index: Integer);
begin
  lua_rawset(@Self, index);
end;

procedure TLuaStateRec.RawSetI(i: lua_Integer; index: Integer);
begin
  lua_rawseti(@Self, index, i);
end;

procedure TLuaStateRec.RawSetP(p: Pointer; index: Integer);
begin
  lua_rawsetp(@Self, index, p);
end;

procedure TLuaStateRec.SetMetatable(index: Integer);
begin
  lua_setmetatable(@Self, index);
end;

procedure TLuaStateRec.SetUservalue(index: Integer);
begin
  lua_setuservalue(@Self, index);
end;

procedure TLuaStateRec.CallK(nargs, nresults: Integer; ctx: lua_KContext;
  k: lua_KFunction);
begin
  lua_callk(@Self, nargs, nresults, ctx, k);
end;

procedure TLuaStateRec.Call(nargs, nresults: Integer);
begin
  lua_call(@Self, nargs, nresults);
end;

function TLuaStateRec.PCallK_X(nargs, nresults, msgh: Integer; ctx: lua_KContext;
  k: lua_KFunction): Integer;
begin
  Result := lua_pcallk(@Self, nargs, nresults, msgh, ctx, k);
end;

function TLuaStateRec.PCallK(nargs, nresults, msgh: Integer; ctx: lua_KContext; k: lua_KFunction): TLuaPCallError;
begin
  Result := TLuaPCallError(PCallK_X(nargs, nresults, msgh, ctx, k));
end;

function TLuaStateRec.PCall_X(nargs, nresults, msgh: Integer): Integer;
begin
  Result := lua_pcall(@Self, nargs, nresults, msgh);
end;

function TLuaStateRec.PCall(nargs, nresults, msgh: Integer): TLuaPCallError;
begin
  Result := TLuaPCallError(PCall_X(nargs, nresults, msgh));
end;

function TLuaStateRec.Load_X(Reader: lua_Reader; Data: Pointer; chunkname, mode: PAnsiChar): Integer;
begin
  Result := lua_load(@Self, Reader, Data, chunkname, mode);
end;

function TLuaStateRec.Load(Reader: TLuaReader; Data: Pointer; chunkname, mode: PAnsiChar): TLuaLoadError;
begin
  Result := TLuaLoadError(Load_X(Reader, Data, chunkname, mode));
end;

function TLuaStateRec.Dump(writer: lua_Writer; Data: Pointer; strip: Integer): Integer;
begin
  Result := lua_dump(@Self, writer, Data, strip);
end;

function TLuaStateRec.YieldK(nresults: Integer; ctx: lua_KContext; k: lua_KFunction): Integer;
begin
  Result := lua_yieldk(@Self, nresults, ctx, k);
end;

function TLuaStateRec.Resume(from: TLuaState; nargs: Integer): Integer;
begin
  Result := lua_resume(@Self, from, nargs);
end;

function TLuaStateRec.Status: Integer;
begin
  Result := lua_status(@Self);
end;

function TLuaStateRec.IsYieldable: LongBool;
begin
  Result := lua_isyieldable(@Self);
end;

function TLuaStateRec.Yield(nresults: Integer): Integer;
begin
  Result := lua_yield(@Self, nresults);
end;

function TLuaStateRec.GC(what, Data: Integer): Integer;
begin
  Result := lua_gc(@Self, what, Data);
end;

function TLuaStateRec.Error_X: Integer;
begin
  Result := lua_error(@Self);
end;

function TLuaStateRec.Next(index: Integer): LongBool;
begin
  Result := lua_next(@Self, index);
end;

procedure TLuaStateRec.Concat(n: Integer);
begin
  lua_concat(@Self, n);
end;

procedure TLuaStateRec.Len(index: Integer);
begin
  lua_len(@Self, index);
end;

function TLuaStateRec.StringToNumber(s: PAnsiChar): NativeUInt;
begin
  Result := lua_stringtonumber(@Self, s);
end;

function TLuaStateRec.GetAllocF(ud: PPointer): lua_Alloc;
begin
  Result := lua_getallocf(@Self, ud);
end;

procedure TLuaStateRec.SetAllocF(f: lua_Alloc; ud: Pointer);
begin
  lua_setallocf(@Self, f, ud);
end;

function TLuaStateRec.GetExtraSpace: Pointer;
begin
  Result := lua_getextraspace(@Self);
end;

function TLuaStateRec.ToNumber(index: Integer): lua_Number;
begin
  Result := lua_tonumber(@Self, index);
end;

function TLuaStateRec.ToInteger(index: Integer): lua_Integer;
begin
  Result := lua_tointeger(@Self, index);
end;

procedure TLuaStateRec.Pop(n: Integer);
begin
  lua_pop(@Self, n);
end;

procedure TLuaStateRec.NewTable;
begin
  lua_newtable(@Self);
end;

procedure TLuaStateRec.&Register(name: PAnsiChar; f: lua_CFunction);
begin
  lua_register(@Self, name, f);
end;

procedure TLuaStateRec.PushCFunction(f: lua_CFunction);
begin
  lua_pushcfunction(@Self, f);
end;

function TLuaStateRec.PushFString(fmt: PAnsiChar;
  args: array of const): PAnsiChar;
var
  list: array of PAnsiChar;
  i, L: Integer;
  s: AnsiString;
begin
  L := Length(args);
  if L > 0 then
  begin
    SetLength(list, L);
    for i := 0 to L - 1 do
    begin
      case args[i].VType of
        vtInteger:
          list[i] := PAnsiChar(args[i].VInteger);
        // vtBoolean: if args[i].VBoolean then list[i] := 'TRUE' else list[i] := 'FALSE';
        vtChar:
          list[i] := PAnsiChar(args[i].VChar);
        // vtExtended: list[i] := PAnsiChar(args[i].VExtended^);
        vtString:
          begin
            s := args[i].VString^;
            list[i] := PPAnsiChar(@s)^;
          end;
        vtPointer:
          list[i] := PAnsiChar(args[i].VPointer);
        vtPChar:
          list[i] := args[i].VPChar;
        // vtObject: list[i] := PAnsiChar(args[i].VObject);
        // vtClass: list[i] := PAnsiChar(args[i].VClass);
        vtWideChar:
          list[i] := PAnsiChar(args[i].VWideChar);
        vtPWideChar:
          begin
            s := AnsiString(WideString(args[i].VPWideChar));
            list[i] := PPAnsiChar(@s)^;
          end;
        vtAnsiString:
          list[i] := args[i].VAnsiString;
        // vtCurrency: list[i] := PAnsiChar(args[i].VCurrency);
        // vtVariant: list[i] := PAnsiChar(args[i].VVariant);
        // vtInterface: list[i] := PAnsiChar(args[i].VInterface);
        vtWideString:
          begin
            s := AnsiString(WideString(args[i].VWideString));
            list[i] := PPAnsiChar(@s)^;
          end;
        vtInt64:
          list[i] := PAnsiChar(args[i].VInt64^);
        vtUnicodeString:
          begin
            s := AnsiString(UnicodeString(args[i].VUnicodeString));
            list[i] := PPAnsiChar(@s)^;
          end;
      else
        raise ENotSupportedException.CreateFmt('Unsupported Formatting VariantType: %d', [args[i].VType]);
      end;
    end;
    Result := PushVFString(fmt, @list[0]);
  end
  else
    Result := PushVFString(fmt, nil);
end;

function TLuaStateRec.IsFunction(index: Integer): LongBool;
begin
  Result := lua_isfunction(@Self, index);
end;

function TLuaStateRec.IsTable(index: Integer): LongBool;
begin
  Result := lua_istable(@Self, index);
end;

function TLuaStateRec.IsLightuserdata(index: Integer): LongBool;
begin
  Result := lua_islightuserdata(@Self, index);
end;

function TLuaStateRec.IsNil(index: Integer): LongBool;
begin
  Result := lua_isnil(@Self, index);
end;

function TLuaStateRec.IsBoolean(index: Integer): LongBool;
begin
  Result := lua_isboolean(@Self, index);
end;

function TLuaStateRec.IsThread(index: Integer): LongBool;
begin
  Result := lua_isthread(@Self, index);
end;

function TLuaStateRec.IsNone(index: Integer): LongBool;
begin
  Result := lua_isnone(@Self, index);
end;

function TLuaStateRec.IsNoneOrNil(index: Integer): LongBool;
begin
  Result := lua_isnoneornil(@Self, index);
end;

function TLuaStateRec.PushLiteral(s: PAnsiChar): PAnsiChar;
begin
  Result := lua_pushliteral(@Self, s);
end;

procedure TLuaStateRec.PushGlobalTable;
begin
  lua_pushglobaltable(@Self);
end;

function TLuaStateRec.ToString_X(index: Integer): PAnsiChar;
begin
  Result := lua_tostring(@Self, index);
end;

function TLuaStateRec.ToString(index: Integer): PAnsiChar;
var
  T: TLuaType;
begin
  index := AbsIndex(index);
  T := &Type(index);
  case T of
    ltNone:
      PushString('none');
    ltNil:
      PushString('nil');
    ltBoolean:
      if ToBoolean(index) then
        PushString('true')
      else
        PushString('false');
    ltLightUserdata:
      PushFString('lightuserdata: %p', [ToPointer(index)]);
    // ltNumber: ; // default
    // ltString: ; // default
    ltTable .. ltThread:
      PushFString('%s: %p', [TypeName(T), ToPointer(index)]);
  else
    Exit(ToString_X(index));
  end;
  Replace(index);
  Result := ToString_X(index);
end;

procedure TLuaStateRec.Insert(index: Integer);
begin
  lua_insert(@Self, index);
end;

procedure TLuaStateRec.Remove(index: Integer);
begin
  lua_remove(@Self, index);
end;

procedure TLuaStateRec.Replace(index: Integer);
begin
  lua_replace(@Self, index);
end;

function TLuaStateRec.GetStack(level: Integer; ar: Plua_Debug): LongBool;
begin
  Result := lua_getstack(@Self, level, ar);
end;

function TLuaStateRec.GetInfo(what: PAnsiChar; ar: Plua_Debug): LongBool;
begin
  Result := lua_getinfo(@Self, what, ar);
end;

function TLuaStateRec.GetLocal(ar: Plua_Debug; n: Integer): PAnsiChar;
begin
  Result := lua_getlocal(@Self, ar, n);
end;

function TLuaStateRec.SetLocal(ar: Plua_Debug; n: Integer): PAnsiChar;
begin
  Result := lua_setlocal(@Self, ar, n);
end;

function TLuaStateRec.GetUpvalue(funcindex, n: Integer): PAnsiChar;
begin
  Result := lua_getupvalue(@Self, funcindex, n);
end;

function TLuaStateRec.SetUpvalue(funcindex, n: Integer): PAnsiChar;
begin
  Result := lua_setupvalue(@Self, funcindex, n);
end;

function TLuaStateRec.UpvalueID(funcindex, n: Integer): Pointer;
begin
  Result := lua_upvalueid(@Self, funcindex, n);
end;

procedure TLuaStateRec.UpvalueJoin(funcindex1, n1, funcindex2, n2: Integer);
begin
  lua_upvaluejoin(@Self, funcindex1, n1, funcindex2, n2);
end;

procedure TLuaStateRec.SetHook(f: lua_Hook; mask, count: Integer);
begin
  lua_sethook(@Self, f, mask, count);
end;

function TLuaStateRec.GetHook: lua_Hook;
begin
  Result := lua_gethook(@Self);
end;

function TLuaStateRec.GetHookMask: Integer;
begin
  Result := lua_gethookmask(@Self);
end;

function TLuaStateRec.GetHookCount: Integer;
begin
  Result := lua_gethookcount(@Self);
end;

class function TLuaStateRec.UpvalueIndex(i: Integer): Integer;
begin
  Result := lua_upvalueindex(i);
end;

end.
