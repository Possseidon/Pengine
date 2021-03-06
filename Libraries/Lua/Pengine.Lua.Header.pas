unit Pengine.Lua.Header;

interface

uses
  System.SysUtils,
  System.AnsiStrings,

  Pengine.Utility,
  Pengine.Lua.Conf;

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
  LUA_YIELDED = 1; // name conflict with lua_yield function
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

  // Auxiliary Library

  LUA_ERRFILE = LUA_ERRERR + 1;
  LUA_LOADED_TABLE = '_LOADED';
  LUA_PRELOAD_TABLE = '_PRELOAD';

  LUA_NOREF = -2;
  LUA_REFNIL = -1;

  // Lua Libraries

  LUA_VERSUFFIX = '_' + LUA_VERSION_MAJOR + '_' + LUA_VERSION_MINOR;

  LUA_COLIBNAME = 'coroutine';
  LUA_TABLIBNAME = 'table';
  LUA_IOLIBNAME = 'io';
  LUA_OSLIBNAME = 'os';
  LUA_STRLIBNAME = 'string';
  LUA_UTF8LIBNAME = 'utf8';
  LUA_BITLIBNAME = 'bit32';
  LUA_MATHLIBNAME = 'math';
  LUA_DBLIBNAME = 'debug';
  LUA_LOADLIBNAME = 'package';

type

  lua_Number = Pengine.Lua.Conf.lua_Number;
  lua_Integer = Pengine.Lua.Conf.lua_Integer;
  lua_Unsigned = Pengine.Lua.Conf.lua_Unsigned;
  lua_KContext = Pengine.Lua.Conf.lua_KContext;

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
  TLuaUnsigned = lua_Unsigned;
  TLuaCFunction = lua_CFunction;

  TLuaMetatableEvent = (
    // General
    mtIndex,
    mtNewIndex,
    mtMode,
    mtCall,
    mtMetatable,
    mtToString,
    mtLen,
    mtPairs,
    mtIPairs,
    mtGC,

    // Math
    mtUnm,
    mtAdd,
    mtSub,
    mtMul,
    mtDiv,
    mtIDiv,
    mtMod,
    mtPow,
    mtConcat,

    // Bitwise
    mtBAnd,
    mtBOr,
    mtBXOr,
    mtBNot,
    mtShl,
    mtShr,

    // Equivalence
    mtEq,
    mtLt,
    mtLe
    );

  TLuaMetatableEvents = set of TLuaMetatableEvent;

  TLuaAlloc = lua_Alloc;

  TLuaReader = lua_Reader;
  TLuaWriter = lua_Writer;

  TLuaKContext = lua_KContext;
  TLuaKFunction = lua_KFunction;

  TLuaDebug = lua_Debug;
  TLuaHook = lua_Hook;

  PLuaLReg = ^TLuaLReg;
  TLuaLReg = record
    name: PAnsiChar;
    Func: TLuaCFunction;
  end;

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

    function AtPanic(panicf: lua_CFunction): TLuaCFunction; inline;

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
    function TypeName_X(tp: Integer): PAnsiChar; inline;
    function TypeName(tp: TLuaType): PAnsiChar; inline;

    function ToNumberX(isnum: PLongBool; index: Integer = -1): TLuaNumber; inline;
    function ToIntegerX(isnum: PLongBool; index: Integer = -1): TLuaInteger; overload; inline;
    function ToIntegerX(var AResult: TLuaInteger; index: Integer = -1): Boolean; overload; inline;
    function ToBoolean(index: Integer = -1): LongBool; inline;
    function ToLString(len: PNativeUInt; index: Integer = -1): PAnsiChar; inline;
    function RawLen(index: Integer = -1): NativeUInt; inline;
    function ToCFunction(index: Integer = -1): TLuaCFunction; inline;
    function ToUserdata(index: Integer = -1): Pointer; inline;
    function ToThread(index: Integer = -1): TLuaState; inline;
    function ToPointer(index: Integer = -1): Pointer; inline;

    // Comparison and arithmetic functions
    procedure Arith(op: Integer); inline;

    function RawEqual(index1, index2: Integer): LongBool; inline;
    function CompareX(index1, index2, op: Integer): LongBool; inline;
    function Compare(index1, index2: Integer; op: TLuaCompareOp): LongBool; inline;

    // push functions (C (Delphi) -> stack)
    procedure PushNil; inline;
    procedure PushNumber(n: lua_Number); inline;
    procedure PushInteger(n: lua_Integer); inline;
    function PushLString(s: PAnsiChar; len: NativeUInt): PAnsiChar; inline;
    function PushVFString(fmt, argp: PAnsiChar): PAnsiChar; inline;
    function PushFString(fmt: PAnsiChar; args: array of const): PAnsiChar;
    function PushString(s: PAnsiChar): PAnsiChar; overload; inline;
    function PushString(s: AnsiString): PAnsiChar; overload; inline;
    procedure PushCClosure(fn: lua_CFunction; n: Integer); inline;
    procedure PushBoolean(b: LongBool); inline;
    procedure PushLightuserdata(p: Pointer); inline;
    function PushThread: LongBool; inline;

    // get functions (Lua -> stack)
    function GetGlobal_X(name: PAnsiChar): Integer; inline;
    function GetGlobal(name: PAnsiChar): TLuaType; inline;
    function GetTable_X(index: Integer): Integer; inline;
    function GetTable(index: Integer): TLuaType; inline;
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
    function GetUservalue_X(index: Integer = -1): Integer; inline;
    function GetUservalue(index: Integer = -1): TLuaType; inline;

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
    procedure CallK(nargs, nresults: Integer; ctx: TLuaKContext; k: TLuaKFunction); inline;
    procedure Call(nargs, nresults: Integer); inline;
    function PCallK_X(nargs, nresults, msgh: Integer; ctx: TLuaKContext; k: TLuaKFunction): Integer; inline;
    function PCallK(nargs, nresults, msgh: Integer; ctx: TLuaKContext; k: TLuaKFunction): TLuaPCallError; inline;
    function PCall_X(nargs, nresults, msgh: Integer): Integer; inline;
    function PCall(nargs, nresults, msgh: Integer): TLuaPCallError; inline;
    function Load_X(Reader: lua_Reader; Data: Pointer; chunkname, mode: PAnsiChar): Integer; inline;
    function Load(Reader: TLuaReader; Data: Pointer; chunkname, mode: PAnsiChar): TLuaLoadError; inline;
    function Dump(writer: lua_Writer; Data: Pointer; strip: Integer): Integer; inline;

    // coroutine functions
    function YieldK(nresults: Integer; ctx: TLuaKContext; k: TLuaKFunction): Integer; inline;
    function Resume_X(from: TLuaState; nargs: Integer): Integer; inline;
    function Resume(from: TLuaState; nargs: Integer): TLuaStatus; inline;
    function Status_X: Integer; inline;
    function status: TLuaStatus; inline;
    function IsYieldable: LongBool; inline;
    function Yield(nresults: Integer): Integer; inline;

    // garbage-collection function
    function GC(what, Data: Integer): Integer; inline;

    // miscellaneous functions
    function Error_X: Integer; inline;
    function Next(index: Integer): LongBool; inline;
    procedure Concat(n: Integer); inline;
    procedure len(index: Integer); inline;
    function StringToNumber(s: PAnsiChar): NativeUInt; inline;

    function GetAllocF(ud: PPointer): TLuaAlloc; inline;
    procedure SetAllocF(f: TLuaAlloc; ud: Pointer); inline;

    // some useful macros
    function GetExtraSpace: Pointer; inline;
    function ToNumber(out AValue: TLuaNumber; index: Integer = -1): LongBool; overload; inline;
    function ToNumber(index: Integer = -1): TLuaNumber; overload; inline;
    function ToInteger(index: Integer = -1): TLuaInteger; inline;

    procedure Pop(n: Integer = 1); inline;

    procedure NewTable; inline;

    procedure &Register(name: PAnsiChar; f: TLuaCFunction); inline;

    procedure PushCFunction(f: TLuaCFunction); inline;

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

    // Auxiliary Library

    procedure LCheckVersion_(ver: TLuaNumber; sz: NativeUInt); inline;
    procedure LCheckVersion; inline;

    function LGetMetafield(obj: Integer; e: PAnsiChar): Integer; inline;
    function LCallMeta(obj: Integer; e: PAnsiChar): Integer; inline;
    function LToLString(idx: Integer; len: PNativeUInt): PAnsiChar; inline;
    function LArgError(arg: Integer; extramsg: PAnsiChar): Integer; inline;
    function LCheckLString(arg: Integer; len: PNativeUInt): PAnsiChar; inline;
    function LOptLString(arg: Integer; def: PAnsiChar; len: PNativeUInt): PAnsiChar; inline;
    function LCheckNumber(arg: Integer): TLuaNumber; inline;
    function LOptNumber(arg: Integer; def: TLuaNumber): TLuaNumber; inline;

    function LCheckInteger(arg: Integer): TLuaInteger; inline;
    function LOptInteger(arg: Integer; def: TLuaInteger): TLuaInteger; inline;

    procedure LCheckStack(sz: Integer; msg: PAnsiChar); inline;
    procedure LCheckType(arg: Integer; t: Integer); inline;
    procedure LCheckAny(arg: Integer); inline;

    function LNewMetatable(tname: PAnsiChar): Integer; inline;
    procedure LSetMetatable(tname: PAnsiChar); inline;
    function LTestUData(ud: Integer; tname: PAnsiChar): Pointer; inline;
    function LCheckUData(ud: Integer; tname: PAnsiChar): Pointer; inline;

    procedure LWhere(lvl: Integer); inline;

    function LCheckOption(arg: Integer; def: PAnsiChar; lst: PPAnsiChar): Integer; inline;

    function LFileResult(stat: Integer; fname: PAnsiChar): Integer; inline;
    function LExecResult(stat: Integer): Integer; inline;

    function LRef(t: Integer): Integer; inline;
    procedure LUnref(t, ref: Integer); inline;

    function LLoadFileX(filename, mode: PAnsiChar): Integer; inline;

    function LLoadFile(filename: PAnsiChar): Integer; inline;

    function LLoadBufferX(buff: PAnsiChar; sz: NativeUInt; name, mode: PAnsiChar): Integer; inline;
    function LLoadString(s: PAnsiChar): Integer; inline;

    class function LNewState: TLuaState; static; inline;

    function LLen(idx: Integer): TLuaInteger; inline;

    function LGSub(s, p, r: PAnsiChar): PAnsiChar; inline;

    procedure LSetFuncs(libs: PLuaLReg; nup: Integer); inline;

    function LGetSubTable(idx: Integer; fname: PAnsiChar): Integer; inline;

    procedure LTraceback(L1: TLuaState; msg: PAnsiChar; level: Integer); inline;

    procedure LRequireF(modname: PAnsiChar; openf: TLuaCFunction; glb: Integer); inline;

    // Some useful Macros

    // procedure luaL_newlibtable(); inline;

    // procedure luaL_newlib(); inline;

    procedure LArgCheck(cond: Boolean; arg: Integer; extramsg: PAnsiChar); inline;
    function LCheckString(n: Integer): PAnsiChar; inline;
    function LOptString(n: Integer; d: PAnsiChar): PAnsiChar; inline;

    function LTypeName(i: Integer): PAnsiChar; inline;

    function LDoFile(fn: PAnsiChar): Boolean; inline;

    function LDoString(s: PAnsiChar): Boolean; inline;

    function LGetMetatable(n: PAnsiChar): Integer; inline;

    // function luaL_opt(): ; inline;

    function LLoadBuffer(s: PAnsiChar; sz: NativeUInt; n: PAnsiChar): Integer; inline;

    // Lua Libraries

    function OpenBase: Integer; inline;
    function OpenCoroutine: Integer; inline;
    function OpenTable: Integer; inline;
    function OpenIO: Integer; inline;
    function OpenOS: Integer; inline;
    function OpenString: Integer; inline;
    function OpenUTF8: Integer; inline;
    function OpenBit32: Integer; inline;
    function OpenMath: Integer; inline;
    function OpenDebug: Integer; inline;
    function OpenPackage: Integer; inline;

    function LOpenLibs: Integer; inline;

    // --- Custom Functions ---

    property Top: Integer read GetTop write SetTop;
    function TypeNameAt(index: Integer = -1): PAnsiChar;
    procedure SetName(AName: AnsiString; index: Integer = -1);
    procedure SetNameDebug(AName: AnsiString; index: Integer = -1); inline;

    function LoadString(AString: AnsiString; AChunkName: AnsiString): TLuaLoadError; overload;
    function LoadString(AString: AnsiString): TLuaLoadError; overload;

    procedure Where(ALevel: Integer);

    function Error(AMessage: AnsiString; ALevel: Integer = 1): Integer;
    function ErrorFmt(AFmt: AnsiString; AArgs: array of const; ALevel: Integer = 1): Integer;

    function FormatStack: AnsiString;

    function FormatTypes(ATypes: TLuaTypes; ANone: Boolean = False): AnsiString;

    // Generates error strings
    function BadTypeString(AExpected, AGot: TLuaString): TLuaString;
    function BadArgString(AArg: Integer; AExpected, AGot: TLuaString): TLuaString;

    // Tests a stackposition and uses BadTypeString
    procedure CheckType(AIndex: Integer; AType: TLuaType); overload;
    function CheckType(AIndex: Integer; ATypes: TLuaTypes; ANone: Boolean = False): TLuaType; overload;
    function CheckAny(AIndex: Integer): TLuaType;

    function CheckNumber(AIndex: Integer): TLuaNumber;
    function CheckInteger(AIndex: Integer): TLuaInteger;
    function CheckBoolean(AIndex: Integer): Boolean;
    function CheckString(AIndex: Integer): TLuaString;

    function CheckOrDefault(AIndex: Integer; ADefault: TLuaNumber): TLuaNumber; overload;
    function CheckOrDefault(AIndex: Integer; ADefault: TLuaInteger): TLuaInteger; overload;
    function CheckOrDefault(AIndex: Integer; ADefault: Boolean): Boolean; overload;
    function CheckOrDefault(AIndex: Integer; ADefault: TLuaString): TLuaString; overload;

    // Tests a stackposition and uses BadArgString
    procedure CheckArg(AIndex: Integer; AType: TLuaType); overload;
    function CheckArg(AIndex: Integer; ATypes: TLuaTypes; ANone: Boolean = False): TLuaType; overload;
    function CheckArgAny(AIndex: Integer): TLuaType;

    function CheckArgNumber(AIndex: Integer): TLuaNumber;
    function CheckArgInteger(AIndex: Integer): TLuaInteger;
    function CheckArgBoolean(AIndex: Integer): Boolean;
    function CheckArgString(AIndex: Integer): TLuaString;

    function CheckArgOrDefault(AIndex: Integer; ADefault: TLuaNumber): TLuaNumber; overload;
    function CheckArgOrDefault(AIndex: Integer; ADefault: TLuaInteger): TLuaInteger; overload;
    function CheckArgOrDefault(AIndex: Integer; ADefault: Boolean): Boolean; overload;
    function CheckArgOrDefault(AIndex: Integer; ADefault: TLuaString): TLuaString; overload;

  end;

const

  LuaMetatableEventNames: array [TLuaMetatableEvent] of AnsiString = (
    // General
    '__index',
    '__newindex',
    '__mode',
    '__call',
    '__metatable',
    '__tostring',
    '__len',
    '__pairs',
    '__ipairs',
    '__gc',

    // Math
    '__unm',
    '__add',
    '__sub',
    '__mul',
    '__div',
    '__idiv',
    '__mod',
    '__pow',
    '__concat',

    // Bitwise
    '__band',
    '__bor',
    '__bxor',
    '__bnot',
    '__shl',
    '__shr',

    // Equivalence
    '__eq',
    '__lt',
    '__le'
    );

  // Auxiliary Library

  LUAL_NUMSIZES = SizeOf(TLuaInteger) * 16 + SizeOf(TLuaNumber);

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

function lua_tonumberx(L: TLuaState; index: Integer; isnum: PLongBool): lua_Number; cdecl; external LuaDLL;
function lua_tointegerx(L: TLuaState; index: Integer; isnum: PLongBool): lua_Integer; cdecl; external LuaDLL;
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
function lua_pushfstring(L: TLuaState; fmt: PAnsiChar): PAnsiChar; cdecl; varargs; external LuaDLL;
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

// Auxiliary Library

procedure luaL_checkversion_(L: TLuaState; ver: TLuaNumber; sz: NativeUInt); cdecl; external LuaDLL;
procedure luaL_checkversion(L: TLuaState); inline;

function luaL_getmetafield(L: TLuaState; obj: Integer; e: PAnsiChar): Integer; cdecl; external LuaDLL;
function luaL_callmeta(L: TLuaState; obj: Integer; e: PAnsiChar): Integer; cdecl; external LuaDLL;
function luaL_tolstring(L: TLuaState; idx: Integer; len: PNativeUInt): PAnsiChar; cdecl; external LuaDLL;
function luaL_argerror(L: TLuaState; arg: Integer; extramsg: PAnsiChar): Integer; cdecl; external LuaDLL;
function luaL_checklstring(L: TLuaState; arg: Integer; len: PNativeUInt): PAnsiChar; cdecl; external LuaDLL;
function luaL_optlstring(L: TLuaState; arg: Integer; def: PAnsiChar; len: PNativeUInt): PAnsiChar; cdecl; external LuaDLL;
function luaL_checknumber(L: TLuaState; arg: Integer): TLuaNumber; cdecl; external LuaDLL;
function luaL_optnumber(L: TLuaState; arg: Integer; def: TLuaNumber): TLuaNumber; cdecl; external LuaDLL;

function luaL_checkinteger(L: TLuaState; arg: Integer): TLuaInteger; cdecl; external LuaDLL;
function luaL_optinteger(L: TLuaState; arg: Integer; def: TLuaInteger): TLuaInteger; cdecl; external LuaDLL;

procedure luaL_checkstack(L: TLuaState; sz: Integer; msg: PAnsiChar); cdecl; external LuaDLL;
procedure luaL_checktype(L: TLuaState; arg: Integer; t: Integer); cdecl; external LuaDLL;
procedure luaL_checkany(L: TLuaState; arg: Integer); cdecl; external LuaDLL;

function luaL_newmetatable(L: TLuaState; tname: PAnsiChar): Integer; cdecl; external LuaDLL;
procedure luaL_setmetatable(L: TLuaState; tname: PAnsiChar); cdecl; external LuaDLL;
function luaL_testudata(L: TLuaState; ud: Integer; tname: PAnsiChar): Pointer; cdecl; external LuaDLL;
function luaL_checkudata(L: TLuaState; ud: Integer; tname: PAnsiChar): Pointer; cdecl; external LuaDLL;

procedure luaL_where(L: TLuaState; lvl: Integer); cdecl; external LuaDLL;
function luaL_error(L: TLuaState; fmt: PAnsiChar): Integer; cdecl; varargs; external LuaDLL;

function luaL_checkoption(L: TLuaState; arg: Integer; def: PAnsiChar; lst: PPAnsiChar): Integer; cdecl; external LuaDLL;

function luaL_fileresult(L: TLuaState; stat: Integer; fname: PAnsiChar): Integer; cdecl; external LuaDLL;
function luaL_execresult(L: TLuaState; stat: Integer): Integer; cdecl; external LuaDLL;

function luaL_ref(L: TLuaState; t: Integer): Integer; cdecl; external LuaDLL;
procedure luaL_unref(L: TLuaState; t, ref: Integer); cdecl; external LuaDLL;

function luaL_loadfilex(L: TLuaState; filename, mode: PAnsiChar): Integer; cdecl; external LuaDLL;

function luaL_loadfile(L: TLuaState; filename: PAnsiChar): Integer; inline;

function luaL_loadbufferx(L: TLuaState; buff: PAnsiChar; sz: NativeUInt; name, mode: PAnsiChar): Integer; cdecl; external LuaDLL;
function luaL_loadstring(L: TLuaState; s: PAnsiChar): Integer; cdecl; external LuaDLL;

function luaL_newstate: TLuaState; cdecl; external LuaDLL;

function luaL_len(L: TLuaState; idx: Integer): TLuaInteger; cdecl; external LuaDLL;

function luaL_gsub(L: TLuaState; s, p, r: PAnsiChar): PAnsiChar; cdecl; external LuaDLL;

procedure luaL_setfuncs(L: TLuaState; libs: PLuaLReg; nup: Integer); cdecl; external LuaDLL;

function luaL_getsubtable(L: TLuaState; idx: Integer; fname: PAnsiChar): Integer; cdecl; external LuaDLL;

procedure luaL_traceback(L, L1: TLuaState; msg: PAnsiChar; level: Integer); cdecl; external LuaDLL;

procedure luaL_requiref(L: TLuaState; modname: PAnsiChar; openf: TLuaCFunction; glb: Integer); cdecl; external LuaDLL;

// Some useful Macros

// procedure luaL_newlibtable(L: TLuaState); inline;

// procedure luaL_newlib(L: TLuaState); inline;

procedure luaL_argcheck(L: TLuaState; cond: Boolean; arg: Integer; extramsg: PAnsiChar); inline;
function luaL_checkstring(L: TLuaState; n: Integer): PAnsiChar; inline;
function luaL_optstring(L: TLuaState; n: Integer; d: PAnsiChar): PAnsiChar; inline;

function luaL_typename(L: TLuaState; i: Integer): PAnsiChar; inline;

function luaL_dofile(L: TLuaState; fn: PAnsiChar): Boolean; inline;

function luaL_dostring(L: TLuaState; s: PAnsiChar): Boolean; inline;

function luaL_getmetatable(L: TLuaState; n: PAnsiChar): Integer; inline;

// function luaL_opt(L: TLuaState; ): ; inline;

function luaL_loadbuffer(L: TLuaState; s: PAnsiChar; sz: NativeUInt; n: PAnsiChar): Integer; inline;

(* TODO Generic Buffer Manipulation
typedef struct luaL_Buffer {
  char *b;  /* buffer address */
  size_t size;  /* buffer size */
  size_t n;  /* number of characters in buffer */
  lua_State *L;
  char initb[LUAL_BUFFERSIZE];  /* initial buffer */
} luaL_Buffer;

#define luaL_addchar(B,c) \
  ((void)((B)->n < (B)->size || luaL_prepbuffsize((B), 1)), \
   ((B)->b[(B)->n++] = (c)))

#define luaL_addsize(B,s)       ((B)->n += (s))

LUALIB_API void (luaL_buffinit) (lua_State *L, luaL_Buffer *B);
LUALIB_API char *(luaL_prepbuffsize) (luaL_Buffer *B, size_t sz);
LUALIB_API void (luaL_addlstring) (luaL_Buffer *B, const char *s, size_t l);
LUALIB_API void (luaL_addstring) (luaL_Buffer *B, const char *s);
LUALIB_API void (luaL_addvalue) (luaL_Buffer *B);
LUALIB_API void (luaL_pushresult) (luaL_Buffer *B);
LUALIB_API void (luaL_pushresultsize) (luaL_Buffer *B, size_t sz);
LUALIB_API char *(luaL_buffinitsize) (lua_State *L, luaL_Buffer *B, size_t sz);

#define luaL_prepbuffer(B)      luaL_prepbuffsize(B, LUAL_BUFFERSIZE)
*)

(* TODO: File handles for IO library

/*
** A file handle is a userdata with metatable 'LUA_FILEHANDLE' and
** initial structure 'luaL_Stream' (it may contain other fields
** after that initial structure).
*/

#define LUA_FILEHANDLE          "FILE*"

typedef struct luaL_Stream {
  FILE *f;  /* stream (NULL for incompletely created streams) */
  lua_CFunction closef;  /* to close stream (NULL for closed streams) */
} luaL_Stream;
*)

// Lua Libraries

function luaopen_base(L: TLuaState): Integer; cdecl; external LuaDLL;
function luaopen_coroutine(L: TLuaState): Integer; cdecl; external LuaDLL;
function luaopen_table(L: TLuaState): Integer; cdecl; external LuaDLL;
function luaopen_io(L: TLuaState): Integer; cdecl; external LuaDLL;
function luaopen_os(L: TLuaState): Integer; cdecl; external LuaDLL;
function luaopen_string(L: TLuaState): Integer; cdecl; external LuaDLL;
function luaopen_utf8(L: TLuaState): Integer; cdecl; external LuaDLL;
function luaopen_bit32(L: TLuaState): Integer; cdecl; external LuaDLL;
function luaopen_math(L: TLuaState): Integer; cdecl; external LuaDLL;
function luaopen_debug(L: TLuaState): Integer; cdecl; external LuaDLL;
function luaopen_package(L: TLuaState): Integer; cdecl; external LuaDLL;

function luaL_openlibs(L: TLuaState): Integer; cdecl; external LuaDLL;

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

// Auxiliary Functions

procedure luaL_checkversion(L: TLuaState);
begin
  luaL_checkversion_(L, LUA_VERSION_NUM, LUAL_NUMSIZES);
end;

function luaL_loadfile(L: TLuaState; filename: PAnsiChar): Integer;
begin
  Result := luaL_loadfilex(L, filename, nil);
end;

procedure luaL_argcheck(L: TLuaState; cond: Boolean; arg: Integer; extramsg: PAnsiChar);
begin
  if not cond then
    luaL_argerror(L, arg, extramsg);
end;

function luaL_checkstring(L: TLuaState; n: Integer): PAnsiChar;
begin
  Result := luaL_checklstring(L, n, nil);
end;

function luaL_optstring(L: TLuaState; n: Integer; d: PAnsiChar): PAnsiChar;
begin
  Result := luaL_optlstring(L, n, d, nil);
end;

function luaL_typename(L: TLuaState; i: Integer): PAnsiChar;
begin
  Result := lua_typename(L, lua_type(L, i));
end;

function luaL_dofile(L: TLuaState; fn: PAnsiChar): Boolean;
begin
  Result := (luaL_loadfile(L, fn) <> 0) or (lua_pcall(L, 0, LUA_MULTRET, 0) <> 0);
end;

function luaL_dostring(L: TLuaState; s: PAnsiChar): Boolean;
begin
  Result := (luaL_loadstring(L, s) <> 0) or (lua_pcall(L, 0, LUA_MULTRET, 0) <> 0);
end;

function luaL_getmetatable(L: TLuaState; n: PAnsiChar): Integer;
begin
  Result := lua_getfield(L, LUA_REGISTRYINDEX, n);
end;

function luaL_loadbuffer(L: TLuaState; s: PAnsiChar; sz: NativeUInt; n: PAnsiChar): Integer;
begin
  Result := luaL_loadbufferx(L, s, sz, n, nil);
end;

function NewLuaState(AAllocFunc: TLuaAlloc; AUserData: Pointer): TLuaState;
begin
  Result := lua_newstate(AAllocFunc, AUserData);
end;

{ TLuaStateRec }

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

function TLuaStateRec.AtPanic(panicf: lua_CFunction): TLuaCFunction;
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

function TLuaStateRec.ToNumberX(isnum: PLongBool; index: Integer = -1): TLuaNumber;
begin
  Result := lua_tonumberx(@Self, index, isnum);
end;

function TLuaStateRec.ToIntegerX(isnum: PLongBool; index: Integer = -1): TLuaInteger;
begin
  Result := lua_tointegerx(@Self, index, isnum);
end;

function TLuaStateRec.ToIntegerX(var AResult: TLuaInteger; index: Integer): Boolean;
begin
  AResult := ToIntegerX(@Result, index);
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

function TLuaStateRec.ToCFunction(index: Integer = -1): TLuaCFunction;
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

function TLuaStateRec.CompareX(index1, index2, op: Integer): LongBool;
begin
  Result := lua_compare(@Self, index1, index2, op);
end;

function TLuaStateRec.Compare(index1, index2: Integer; op: TLuaCompareOp): LongBool;
begin
  Result := lua_compare(@Self, index1, index2, Ord(op));
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

function TLuaStateRec.PushFString(fmt: PAnsiChar; args: array of const): PAnsiChar;
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
            list[i] := PAnsiChar(s);
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
            list[i] := PAnsiChar(s);
          end;
        vtAnsiString:
          list[i] := args[i].VAnsiString;
        // vtCurrency: list[i] := PAnsiChar(args[i].VCurrency);
        // vtVariant: list[i] := PAnsiChar(args[i].VVariant);
        // vtInterface: list[i] := PAnsiChar(args[i].VInterface);
        vtWideString:
          begin
            s := AnsiString(WideString(args[i].VWideString));
            list[i] := PAnsiChar(s);
          end;
        vtInt64:
          list[i] := PAnsiChar(args[i].VInt64^);
        vtUnicodeString:
          begin
            s := AnsiString(UnicodeString(args[i].VUnicodeString));
            list[i] := PAnsiChar(s);
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

function TLuaStateRec.PushString(s: PAnsiChar): PAnsiChar;
begin
  Result := lua_pushstring(@Self, s);
end;

function TLuaStateRec.PushString(s: AnsiString): PAnsiChar;
begin
  Result := PushString(PAnsiChar(s));
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

function TLuaStateRec.GetGlobal_X(name: PAnsiChar): Integer;
begin
  Result := lua_getglobal(@Self, name);
end;

function TLuaStateRec.GetGlobal(name: PAnsiChar): TLuaType;
begin
  Result := TLuaType(GetGlobal_X(name));
end;

function TLuaStateRec.GetTable_X(index: Integer): Integer;
begin
  Result := lua_gettable(@Self, index);
end;

function TLuaStateRec.GetTable(index: Integer): TLuaType;
begin
  Result := TLuaType(GetTable_X(index));
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

function TLuaStateRec.GetUservalue_X(index: Integer): Integer;
begin
  Result := lua_getuservalue(@Self, index);
end;

function TLuaStateRec.GetUservalue(index: Integer): TLuaType;
begin
  Result := TLuaType(GetUservalue_X(index));
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

procedure TLuaStateRec.CallK(nargs, nresults: Integer; ctx: TLuaKContext; k: TLuaKFunction);
begin
  lua_callk(@Self, nargs, nresults, ctx, k);
end;

procedure TLuaStateRec.Call(nargs, nresults: Integer);
begin
  lua_call(@Self, nargs, nresults);
end;

function TLuaStateRec.PCallK_X(nargs, nresults, msgh: Integer; ctx: TLuaKContext; k: TLuaKFunction): Integer;
begin
  Result := lua_pcallk(@Self, nargs, nresults, msgh, ctx, k);
end;

function TLuaStateRec.PCallK(nargs, nresults, msgh: Integer; ctx: TLuaKContext; k: TLuaKFunction): TLuaPCallError;
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

function TLuaStateRec.YieldK(nresults: Integer; ctx: TLuaKContext; k: TLuaKFunction): Integer;
begin
  Result := lua_yieldk(@Self, nresults, ctx, k);
end;

function TLuaStateRec.Resume_X(from: TLuaState; nargs: Integer): Integer;
begin
  Result := lua_resume(@Self, from, nargs);
end;

function TLuaStateRec.Resume(from: TLuaState; nargs: Integer): TLuaStatus;
begin
  Result := TLuaStatus(Resume_X(from, nargs));
end;

function TLuaStateRec.Status_X: Integer;
begin
  Result := lua_status(@Self);
end;

function TLuaStateRec.status: TLuaStatus;
begin
  Result := TLuaStatus(Status_X);
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

procedure TLuaStateRec.len(index: Integer);
begin
  lua_len(@Self, index);
end;

function TLuaStateRec.StringToNumber(s: PAnsiChar): NativeUInt;
begin
  Result := lua_stringtonumber(@Self, s);
end;

function TLuaStateRec.GetAllocF(ud: PPointer): TLuaAlloc;
begin
  Result := lua_getallocf(@Self, ud);
end;

procedure TLuaStateRec.SetAllocF(f: TLuaAlloc; ud: Pointer);
begin
  lua_setallocf(@Self, f, ud);
end;

function TLuaStateRec.GetExtraSpace: Pointer;
begin
  Result := lua_getextraspace(@Self);
end;

function TLuaStateRec.ToNumber(out AValue: TLuaNumber; index: Integer): LongBool;
begin
  AValue := lua_tonumberx(@Self, index, @Result);
end;

function TLuaStateRec.ToNumber(index: Integer = -1): TLuaNumber;
begin
  Result := lua_tonumber(@Self, index);
end;

function TLuaStateRec.ToInteger(index: Integer = -1): TLuaInteger;
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

procedure TLuaStateRec.&Register(name: PAnsiChar; f: TLuaCFunction);
begin
  lua_register(@Self, name, f);
end;

procedure TLuaStateRec.PushCFunction(f: TLuaCFunction);
begin
  lua_pushcfunction(@Self, f);
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
    ltTable, ltUserdata:
      if GetMetatable(index) then
      begin
        if GetField('__tostring') <> ltNil then
        begin
          PushValue(index);
          Call(1, 1);
        end
        else
        begin
          PushFString('%s: %p', [TypeNameAt(index), ToPointer(index)]);
          Remove(-2); // remove __tostring
        end;
        Remove(-2); // remove metatable
      end
      else
        PushFString('%s: %p', [TypeNameAt(index), ToPointer(index)]);
    ltFunction, ltThread:
      PushFString('%s: %p', [TypeNameAt(index), ToPointer(index)]);
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

procedure TLuaStateRec.LCheckVersion_(ver: TLuaNumber; sz: NativeUInt);
begin
  luaL_checkversion_(@Self, ver, sz);
end;

procedure TLuaStateRec.LCheckVersion;
begin
  luaL_checkversion(@Self);
end;

function TLuaStateRec.LGetMetafield(obj: Integer; e: PAnsiChar): Integer;
begin
  Result := luaL_getmetafield(@Self, obj, e);
end;

function TLuaStateRec.LCallMeta(obj: Integer; e: PAnsiChar): Integer;
begin
  Result := luaL_callmeta(@Self, obj, e);
end;

function TLuaStateRec.LToLString(idx: Integer; len: PNativeUInt): PAnsiChar;
begin
  Result := luaL_tolstring(@Self, idx, len);
end;

function TLuaStateRec.LArgError(arg: Integer; extramsg: PAnsiChar): Integer;
begin
  Result := luaL_argerror(@Self, arg, extramsg);
end;

function TLuaStateRec.LCheckLString(arg: Integer; len: PNativeUInt): PAnsiChar;
begin
  Result := luaL_checklstring(@Self, arg, len);
end;

function TLuaStateRec.LOptLString(arg: Integer; def: PAnsiChar; len: PNativeUInt): PAnsiChar;
begin
  Result := luaL_optlstring(@Self, arg, def, len);
end;

function TLuaStateRec.LCheckNumber(arg: Integer): TLuaNumber;
begin
  Result := luaL_checknumber(@Self, arg);
end;

function TLuaStateRec.LOptNumber(arg: Integer; def: TLuaNumber): TLuaNumber;
begin
  Result := luaL_optnumber(@Self, arg, def);
end;

function TLuaStateRec.LCheckInteger(arg: Integer): TLuaInteger;
begin
  Result := luaL_checkinteger(@Self, arg);
end;

function TLuaStateRec.LOptInteger(arg: Integer; def: TLuaInteger): TLuaInteger;
begin
  Result := luaL_optinteger(@Self, arg, def);
end;

procedure TLuaStateRec.LCheckStack(sz: Integer; msg: PAnsiChar);
begin
  luaL_checkstack(@Self, sz, msg);
end;

procedure TLuaStateRec.LCheckType(arg, t: Integer);
begin
  luaL_checktype(@Self, arg, t);
end;

procedure TLuaStateRec.LCheckAny(arg: Integer);
begin
  luaL_checkany(@Self, arg);
end;

function TLuaStateRec.LNewMetatable(tname: PAnsiChar): Integer;
begin
  Result := luaL_newmetatable(@Self, tname);
end;

procedure TLuaStateRec.LSetMetatable(tname: PAnsiChar);
begin
  luaL_setmetatable(@Self, tname);
end;

function TLuaStateRec.LTestUData(ud: Integer; tname: PAnsiChar): Pointer;
begin
  Result := luaL_testudata(@Self, ud, tname);
end;

function TLuaStateRec.LCheckUData(ud: Integer; tname: PAnsiChar): Pointer;
begin
  Result := luaL_checkudata(@Self, ud, tname);
end;

procedure TLuaStateRec.LWhere(lvl: Integer);
begin
  luaL_where(@Self, lvl);
end;

function TLuaStateRec.LCheckOption(arg: Integer; def: PAnsiChar; lst: PPAnsiChar): Integer;
begin
  Result := luaL_checkoption(@Self, arg, def, lst);
end;

function TLuaStateRec.LFileResult(stat: Integer; fname: PAnsiChar): Integer;
begin
  Result := luaL_fileresult(@Self, stat, fname);
end;

function TLuaStateRec.LExecResult(stat: Integer): Integer;
begin
  Result := luaL_execresult(@Self, stat);
end;

function TLuaStateRec.LRef(t: Integer): Integer;
begin
  Result := luaL_ref(@Self, t);
end;

procedure TLuaStateRec.LUnref(t, ref: Integer);
begin
  luaL_unref(@Self, t, ref);
end;

function TLuaStateRec.LLoadFileX(filename, mode: PAnsiChar): Integer;
begin
  Result := luaL_loadfilex(@Self, filename, mode);
end;

function TLuaStateRec.LLoadFile(filename: PAnsiChar): Integer;
begin
  Result := luaL_loadfile(@Self, filename);
end;

function TLuaStateRec.LLoadBufferX(buff: PAnsiChar; sz: NativeUInt; name, mode: PAnsiChar): Integer;
begin
  Result := luaL_loadbufferx(@Self, buff, sz, name, mode);
end;

function TLuaStateRec.LLoadString(s: PAnsiChar): Integer;
begin
  Result := luaL_loadstring(@Self, s);
end;

class function TLuaStateRec.LNewState: TLuaState;
begin
  Result := luaL_newstate;
end;

function TLuaStateRec.LLen(idx: Integer): TLuaInteger;
begin
  Result := luaL_len(@Self, idx);
end;

function TLuaStateRec.LGSub(s, p, r: PAnsiChar): PAnsiChar;
begin
  Result := luaL_gsub(@Self, s, p, r);
end;

procedure TLuaStateRec.LSetFuncs(libs: PLuaLReg; nup: Integer);
begin
  luaL_setfuncs(@Self, libs, nup);
end;

function TLuaStateRec.LGetSubTable(idx: Integer; fname: PAnsiChar): Integer;
begin
  Result := luaL_getsubtable(@Self, idx, fname);
end;

procedure TLuaStateRec.LTraceback(L1: TLuaState; msg: PAnsiChar; level: Integer);
begin
  luaL_traceback(@Self, L1, msg, level);
end;

procedure TLuaStateRec.LRequireF(modname: PAnsiChar; openf: TLuaCFunction; glb: Integer);
begin
  luaL_requiref(@Self, modname, openf, glb);
end;

procedure TLuaStateRec.LArgCheck(cond: Boolean; arg: Integer; extramsg: PAnsiChar);
begin
  luaL_argcheck(@Self, cond, arg, extramsg);
end;

function TLuaStateRec.LCheckString(n: Integer): PAnsiChar;
begin
  Result := luaL_checkstring(@Self, n);
end;

function TLuaStateRec.LOptString(n: Integer; d: PAnsiChar): PAnsiChar;
begin
  Result := luaL_optstring(@Self, n, d);
end;

function TLuaStateRec.LTypeName(i: Integer): PAnsiChar;
begin
  Result := luaL_typename(@Self, i);
end;

function TLuaStateRec.LDoFile(fn: PAnsiChar): Boolean;
begin
  Result := luaL_dofile(@Self, fn);
end;

function TLuaStateRec.LDoString(s: PAnsiChar): Boolean;
begin
  Result := luaL_dostring(@Self, s);
end;

function TLuaStateRec.LGetMetatable(n: PAnsiChar): Integer;
begin
  Result := luaL_getmetatable(@Self, n);
end;

function TLuaStateRec.LLoadBuffer(s: PAnsiChar; sz: NativeUInt; n: PAnsiChar): Integer;
begin
  Result := luaL_loadbuffer(@Self, s, sz, n);
end;

function TLuaStateRec.OpenBase: Integer;
begin
  Result := luaopen_base(@Self);
end;

function TLuaStateRec.OpenCoroutine: Integer;
begin
  Result := luaopen_coroutine(@Self);
end;

function TLuaStateRec.OpenTable: Integer;
begin
  Result := luaopen_table(@Self);
end;

function TLuaStateRec.OpenIO: Integer;
begin
  Result := luaopen_io(@Self);
end;

function TLuaStateRec.OpenOS: Integer;
begin
  Result := luaopen_os(@Self);
end;

function TLuaStateRec.OpenString: Integer;
begin
  Result := luaopen_string(@Self);
end;

function TLuaStateRec.OpenUTF8: Integer;
begin
  Result := luaopen_utf8(@Self);
end;

function TLuaStateRec.OpenBit32: Integer;
begin
  Result := luaopen_bit32(@Self);
end;

function TLuaStateRec.OpenMath: Integer;
begin
  Result := luaopen_math(@Self);
end;

function TLuaStateRec.OpenDebug: Integer;
begin
  Result := luaopen_debug(@Self);
end;

function TLuaStateRec.OpenPackage: Integer;
begin
  Result := luaopen_package(@Self);
end;

function TLuaStateRec.LOpenLibs: Integer;
begin
  Result := luaL_openlibs(@Self);
end;

function TLuaStateRec.TypeNameAt(index: Integer): PAnsiChar;
begin
  if GetMetatable(index) then
  begin
    GetField('__name');
    if IsString then
      Result := ToString_X
    else
      Result := TypeName_X(Type_X(index));
    Pop(2);
  end
  else
    Result := TypeName_X(Type_X(index));
end;

procedure TLuaStateRec.SetName(AName: AnsiString; index: Integer);
begin
  if index <> LUA_REGISTRYINDEX then
    index := AbsIndex(index);
  if not GetMetatable(index) then
    NewTable;
  PushString(AName);
  SetField('__name', -2);
  SetMetatable(index);
end;

procedure TLuaStateRec.SetNameDebug(AName: AnsiString; index: Integer);
begin
  {$IFDEF DEBUG}
  SetName(AName, index);
  {$ENDIF}
end;

function TLuaStateRec.LoadString(AString: AnsiString; AChunkName: AnsiString): TLuaLoadError;
var
  R: TReaderRec;
begin
  R.Done := False;
  R.Data := PAnsiChar(AString);
  Result := Load(Reader, @R, PAnsiChar(AChunkName), 't');
end;

function TLuaStateRec.LoadString(AString: AnsiString): TLuaLoadError;
var
  R: TReaderRec;
begin
  R.Done := False;
  R.Data := PAnsiChar(AString);
  Result := Load(Reader, @R, nil, 't');
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
  PushString(AMessage);
  Concat(2);
  Result := Error_X;
end;

function TLuaStateRec.ErrorFmt(AFmt: AnsiString; AArgs: array of const; ALevel: Integer): Integer;
begin
  Result := Error(Format(AFmt, AArgs), ALevel);
end;

function TLuaStateRec.FormatStack: AnsiString;
var
  i: Integer;
begin
  Result := '';
  for i := Top downto 1 do
  begin
    PushValue(i);
    Result := Result + System.AnsiStrings.Format('[%d/-%d] %s', [i, Top - i, ToString]) + #10;
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

function TLuaStateRec.BadTypeString(AExpected, AGot: TLuaString): TLuaString;
begin
  Result := Format(TLuaString('bad type (%s expected, got %s)'), [AExpected, AGot]);
end;

function TLuaStateRec.BadArgString(AArg: Integer; AExpected, AGot: TLuaString): TLuaString;
begin
  Result := Format(TLuaString('bad argument #%d (%s expected, got %s)'), [AArg, AExpected, AGot]);
end;

procedure TLuaStateRec.CheckType(AIndex: Integer; AType: TLuaType);
begin
  if &Type(AIndex) <> AType then
    Error(BadTypeString(TypeName(AType), TypeNameAt(AIndex)));
end;

function TLuaStateRec.CheckType(AIndex: Integer; ATypes: TLuaTypes; ANone: Boolean): TLuaType;
begin
  Result := &Type(AIndex);
  if (Result = ltNone) and not ANone or (Result <> ltNone) and not(Result in ATypes) then
    Error(BadTypeString(FormatTypes(ATypes), TypeName(Result)));
end;

function TLuaStateRec.CheckAny(AIndex: Integer): TLuaType;
begin
  Result := &Type(AIndex);
  if Result = ltNone then
    Error(BadTypeString('any', 'none'));
end;

function TLuaStateRec.CheckNumber(AIndex: Integer): TLuaNumber;
var
  isnum: LongBool;
begin
  Result := ToNumberX(@isnum, AIndex);
  if not isnum then
    Error(BadTypeString(TypeName(ltNumber), TypeNameAt(AIndex)));
end;

function TLuaStateRec.CheckInteger(AIndex: Integer): TLuaInteger;
var
  IsInt: LongBool;
begin
  Result := ToIntegerX(@IsInt, AIndex);
  if not IsInt then
    Error(BadTypeString('integer', TypeNameAt(AIndex)));
end;

function TLuaStateRec.CheckBoolean(AIndex: Integer): Boolean;
begin
  CheckType(AIndex, ltBoolean);
  Result := ToBoolean(AIndex);
end;

function TLuaStateRec.CheckString(AIndex: Integer): TLuaString;
begin
  CheckType(AIndex, [ltString, ltNumber]);
  Result := ToString(AIndex);
end;

function TLuaStateRec.CheckOrDefault(AIndex: Integer; ADefault: TLuaNumber): TLuaNumber;
begin
  if IsNoneOrNil(AIndex) then
    Exit(ADefault);
  Result := CheckNumber(AIndex);
end;

function TLuaStateRec.CheckOrDefault(AIndex: Integer; ADefault: TLuaInteger): TLuaInteger;
begin
  if IsNoneOrNil(AIndex) then
    Exit(ADefault);
  Result := CheckInteger(AIndex);
end;

function TLuaStateRec.CheckOrDefault(AIndex: Integer; ADefault: Boolean): Boolean;
begin
  if IsNoneOrNil(AIndex) then
    Exit(ADefault);
  Result := CheckBoolean(AIndex);
end;

function TLuaStateRec.CheckOrDefault(AIndex: Integer; ADefault: TLuaString): TLuaString;
begin
  if IsNoneOrNil(AIndex) then
    Exit(ADefault);
  Result := CheckString(AIndex);
end;

procedure TLuaStateRec.CheckArg(AIndex: Integer; AType: TLuaType);
begin
  if &Type(AIndex) <> AType then
    Error(BadArgString(AIndex, TypeName(AType), TypeNameAt(AIndex)));
end;

function TLuaStateRec.CheckArg(AIndex: Integer; ATypes: TLuaTypes; ANone: Boolean): TLuaType;
begin
  Result := &Type(AIndex);
  if (Result = ltNone) and not ANone or (Result <> ltNone) and not(Result in ATypes) then
    Error(BadArgString(AIndex, FormatTypes(ATypes), TypeName(Result)));
end;

function TLuaStateRec.CheckArgAny(AIndex: Integer): TLuaType;
begin
  Result := &Type(AIndex);
  if Result = ltNone then
    Error(BadArgString(AIndex, 'any', 'none'));
end;

function TLuaStateRec.CheckArgNumber(AIndex: Integer): TLuaNumber;
begin
  CheckArg(AIndex, ltNumber);
  Result := ToNumber(AIndex);
end;

function TLuaStateRec.CheckArgInteger(AIndex: Integer): TLuaInteger;
var
  IsInt: LongBool;
begin
  Result := ToIntegerX(@IsInt, AIndex);
  if not IsInt then
    Error(BadArgString(AIndex, 'integer', TypeNameAt(AIndex)));
end;

function TLuaStateRec.CheckArgBoolean(AIndex: Integer): Boolean;
begin
  CheckType(AIndex, ltBoolean);
  Result := ToBoolean(AIndex);
end;

function TLuaStateRec.CheckArgString(AIndex: Integer): TLuaString;
begin
  CheckArg(AIndex, [ltString, ltNumber]);
  Result := ToString(AIndex);
end;

function TLuaStateRec.CheckArgOrDefault(AIndex: Integer; ADefault: TLuaNumber): TLuaNumber;
begin
  if IsNoneOrNil(AIndex) then
    Exit(ADefault);
  Result := CheckNumber(AIndex);
end;

function TLuaStateRec.CheckArgOrDefault(AIndex: Integer; ADefault: TLuaInteger): TLuaInteger;
begin
  if IsNoneOrNil(AIndex) then
    Exit(ADefault);
  Result := CheckArgInteger(AIndex);
end;

function TLuaStateRec.CheckArgOrDefault(AIndex: Integer; ADefault: Boolean): Boolean;
begin
  if IsNoneOrNil(AIndex) then
    Exit(ADefault);
  Result := CheckArgBoolean(AIndex);
end;

function TLuaStateRec.CheckArgOrDefault(AIndex: Integer; ADefault: TLuaString): TLuaString;
begin
  if IsNoneOrNil(AIndex) then
    Exit(ADefault);
  Result := CheckArgString(AIndex);
end;

end.
