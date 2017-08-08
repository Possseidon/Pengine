unit LuaHeader;

interface

uses
  LuaConf, SysUtils;

const

  LuaDLL = 'lua53.dll';

  // mark for precompiled code ('<esc>Lua')
  LUA_SIGNATURE = #27'Lua';

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

  TLuaThreadStatus = (
    ltsOk = LUA_OK,
    ltsYield,
    ltsErrorRun,
    ltsErrorSyntax,
    ltsErrorMemory,
    ltsErrorGCMM,
    ltsErrorError
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

  TLuaOperation = (
    lopAdd = LUA_OPADD,
    lopSub,
    lopMul,
    lopMod,
    lopPow,
    lopDiv,
    lopIDiv,
    lopBAnd,
    lopBOr,
    lopBXor,
    lopShl,
    lopShr,
    lopUnMinus,
    lopBNot
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

  lua_Number = LuaConf.LUA_NUMBER;
  lua_Integer = LuaConf.LUA_INTEGER;
  lua_Unsigned = LuaConf.LUA_UNSIGNED;
  lua_KContext = LuaConf.LUA_KCONTEXT;

  Plua_State = ^lua_State;
  lua_State = record
  end;

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
    short_src: array [0 .. LUA_IDSIZE - 1] of Char; // (S)
  private
    {%H-}i_ci: Pointer;  // active function
  end;

  lua_Hook = procedure(L: Plua_State; ar: Plua_Debug);

  // Type for C (Delphi) functions registered with Lua
  lua_CFunction = function(L: Plua_State): Integer;
  // Type for continuation functions
  lua_KFunction = function(L: Plua_State; status: Integer; ctx: lua_KContext): Integer;

  // Type for functions that read/write blocks when loading/dumping Lua chunks
  lua_Reader = function(L: Plua_State; ud: Pointer; sz: PNativeUInt): PAnsiChar;
  lua_Writer = function(L: Plua_State; p: Pointer; sz: NativeUInt; ud: Pointer): Integer;

  // Type for memory-allocation functions
  lua_Alloc = function(ud, ptr: Pointer; osize, nsize: NativeUInt): Pointer;

  lua_Ident = PAnsiChar;

// --- DLL ---

// state manipulation
function lua_newstate(f: lua_Alloc; ud: Pointer): Plua_State; external LuaDLL;
procedure lua_close(L: Plua_State); external LuaDLL;
function lua_newthread(L: Plua_State): Plua_State; external LuaDLL;

function lua_atpanic(L: Plua_State; panicf: lua_CFunction): lua_CFunction; external LuaDLL;

function lua_version(L: Plua_State): Plua_Number; external LuaDLL;

// basic stack manipulation
function lua_absindex(L: Plua_State; idx: Integer): Integer; external LuaDLL;
function lua_gettop(L: Plua_State): Integer; external LuaDLL;
procedure lua_settop(L: Plua_State; idx: Integer); external LuaDLL;
procedure lua_pushvalue(L: Plua_State; idx: Integer); external LuaDLL;
procedure lua_rotate(L: Plua_State; idx, n: Integer); external LuaDLL;
procedure lua_copy(L: Plua_State; fromidx, toidx: Integer); external LuaDLL;
function lua_checkstack(L: Plua_State; n: Integer): LongBool; external LuaDLL;

procedure lua_xmove(from, &to: Plua_State; n: Integer); external LuaDLL;

// access functions (stack -> C (Delphi))
function lua_isnumber(L: Plua_State; idx: Integer): LongBool; external LuaDLL;
function lua_isstring(L: Plua_State; idx: Integer): LongBool; external LuaDLL;
function lua_iscfunction(L: Plua_State; idx: Integer): LongBool; external LuaDLL;
function lua_isinteger(L: Plua_State; idx: Integer): LongBool; external LuaDLL;
function lua_isuserdata(L: Plua_State; idx: Integer): LongBool; external LuaDLL;
function lua_type(L: Plua_State; idx: Integer): Integer; external LuaDLL;
function lua_typename(L: Plua_State; tp: Integer): PAnsiChar; external LuaDLL;

function lua_tonumberx(L: Plua_State; idx: Integer; isnum: PInteger): lua_Number; external LuaDLL;
function lua_tointegerx(L: Plua_State; idx: Integer; isnum: PInteger): lua_Integer; external LuaDLL;
function lua_toboolean(L: Plua_State; idx: Integer): Integer; external LuaDLL;
function lua_tolstring(L: Plua_State; idx: Integer; len: PNativeUInt): PAnsiChar; external LuaDLL;
function lua_rawlen(L: Plua_State; idx: Integer): NativeUInt; external LuaDLL;
function lua_tocfunction(L: Plua_State; idx: Integer): lua_CFunction; external LuaDLL;
function lua_touserdata(L: Plua_State; idx: Integer): Pointer; external LuaDLL;
function lua_tothread(L: Plua_State; idx: Integer): Plua_State; external LuaDLL;
function lua_topointer(L: Plua_State; idx: Integer): Pointer; external LuaDLL;

// Comparison and arithmetic functions
procedure lua_arith(L: Plua_State; op: Integer); external LuaDLL;

function lua_rawequal(L: Plua_State; idx1, idx2: Integer): LongBool; external LuaDLL;
function lua_compare(L: Plua_State; idx1, idx2, op: Integer): Integer; external LuaDLL;

// push functions (C (Delphi) -> stack)
procedure lua_pushnil(L: Plua_State); external LuaDLL;
procedure lua_pushnumber(L: Plua_State; n: lua_Number); external LuaDLL;
procedure lua_pushinteger(L: Plua_State; n: lua_Integer); external LuaDLL;
function lua_pushlstring(L: Plua_State; s: PAnsiChar; len: NativeUInt): PAnsiChar; external LuaDLL;
function lua_pushstring(L: Plua_State; s: PAnsiChar): PAnsiChar; external LuaDLL;
// not usable
// function lua_pushvfstring(L: Plua_State; fmt, argp: PAnsichar): PAnsiChar; external LuaDLL;
function lua_pushfstring(L: Plua_State; fmt: PAnsiChar): PAnsiChar; varargs; cdecl; external LuaDLL;
procedure lua_pushcclosure(L: Plua_State; fn: lua_CFunction; n: Integer); external LuaDLL;
procedure lua_pushboolean(L: Plua_State; b: LongBool); external LuaDLL;
procedure lua_pushlightuserdata(L: Plua_State; p: Pointer); external LuaDLL;
function lua_pushthread(L: Plua_State): Integer; external LuaDLL;

// get functions (Lua -> stack)
function lua_getglobal(L: Plua_State; name: PAnsiChar): Integer; external LuaDLL;
function lua_gettable(L: Plua_State; idx: Integer): Integer; external LuaDLL;
function lua_getfield(L: Plua_State; idx: Integer; k: PAnsiChar): Integer; external LuaDLL;
function lua_geti(L: Plua_State; idx: Integer; n: lua_Integer): Integer; external LuaDLL;
function lua_rawget(L: Plua_State; idx: Integer): Integer; external LuaDLL;
function lua_rawgeti(L: Plua_State; idx: Integer; n: lua_Integer): Integer; external LuaDLL;
function lua_rawgetp(L: Plua_State; idx: Integer; p: Pointer): Integer; external LuaDLL;

procedure lua_createtable(L: Plua_State; narr, nrec: Integer); external LuaDLL;
function lua_newuserdata(L: Plua_State; sz: NativeUInt): Pointer; external LuaDLL;
function lua_getmetatable(L: Plua_State; objindex: Integer): LongBool; external LuaDLL;
function lua_getuservalue(L: Plua_State; idx: Integer): Integer; external LuaDLL;

// set functions (stack -> Lua)
procedure lua_setglobal(L: Plua_State; name: PAnsiChar); external LuaDLL;
procedure lua_settable(L: Plua_State; idx: Integer); external LuaDLL;
procedure lua_setfield(L: Plua_State; idx: Integer; k: PAnsiChar); external LuaDLL;
procedure lua_seti(L: Plua_State; idx: Integer; n: lua_Integer); external LuaDLL;
procedure lua_rawset(L: Plua_State; idx: Integer); external LuaDLL;
procedure lua_rawseti(L: Plua_State; idx: Integer; n: lua_Integer); external LuaDLL;
procedure lua_rawsetp(L: Plua_State; idx: Integer; p: Pointer); external LuaDLL;
function lua_setmetatable(L: Plua_State; objindex: Integer): Integer; external LuaDLL;
procedure lua_setuservalue(L: Plua_State; idx: Integer); external LuaDLL;

// 'load' and 'call' functions (load and run Lua code)
procedure lua_callk(L: Plua_State; nargs, nresults: Integer; ctx: lua_KContext; k: lua_KFunction); external LuaDLL;
// [ lua_call ]
function lua_pcallk(L: Plua_State; nargs, nresults, errfunc: Integer; ctx: lua_KContext; k: lua_KFunction): Integer; external LuaDLL;
// [ lua_pcall ]
function lua_load(L: Plua_State; reader: lua_Reader; dt: Pointer; chunkname, mode: PAnsiChar): Integer; external LuaDLL;
function lua_dump(L: Plua_State; writer: lua_Writer; data: Pointer; strip: Integer): Integer; external LuaDLL;

// coroutine functions
function lua_yieldk(L: Plua_State; nresults: Integer; ctx: lua_KContext; k: lua_KFunction): Integer; external LuaDLL;
function lua_resume(L, from: Plua_State; narg: Integer): Integer; external LuaDLL;
function lua_status(L: Plua_State): Integer; external LuaDLL;
function lua_isyieldable(L: Plua_State): Integer; external LuaDLL;
// [ lua_yield ]

// garbage-collection function
function lua_gc(L: Plua_State; what, data: Integer): Integer; external LuaDLL;

// miscellaneous functions
function lua_error(L: Plua_State): Integer; external LuaDLL;
function lua_next(L: Plua_State; idx: Integer): LongBool; external LuaDLL;
procedure lua_concat(L: Plua_State; n: Integer); external LuaDLL;
procedure lua_len(L: Plua_State; idx: Integer); external LuaDLL;
function lua_stringtonumber(L: Plua_State; s: PAnsiChar): NativeUInt; external LuaDLL;

function lua_getallocf(L: Plua_State; ud: PPointer): lua_Alloc; external LuaDLL;
procedure lua_setallocf(L: Plua_State; f: lua_Alloc; ud: Pointer); external LuaDLL;

// macro help functions
function lua_upvalueindex(i: Integer): Integer; inline;
procedure lua_call(L: Plua_State; nargs, nresults: Integer); inline;
function lua_pcall(L: Plua_State; nargs, nresults, errfunc: Integer): Integer; inline;
function lua_yield(L: Plua_State; nresults: Integer): Integer; inline;
// alternative
// function lua_pushfstring(L: Plua_State; fmt: PAnsiChar; args: array of const): PAnsiChar;

// some useful macros
function lua_getextraspace(L: Plua_State): Pointer; inline;
function lua_tonumber(L: Plua_State; idx: Integer): lua_Number; inline;
function lua_tointeger(L: Plua_State; idx: Integer): lua_Integer; inline;

procedure lua_pop(L: Plua_State; n: Integer); inline;

procedure lua_newtable(L: Plua_State); inline;

procedure lua_register(L: Plua_State; name: PAnsiChar; fn: lua_CFunction); inline;

procedure lua_pushcfunction(L: Plua_State; fn: lua_CFunction); inline;

function lua_isfunction(L: Plua_State; idx: Integer): Boolean; inline;
function lua_istable(L: Plua_State; idx: Integer): Boolean; inline;
function lua_islightuserdata(L: Plua_State; idx: Integer): Boolean; inline;
function lua_isnil(L: Plua_State; idx: Integer): Boolean; inline;
function lua_isboolean(L: Plua_State; idx: Integer): Boolean; inline;
function lua_isthread(L: Plua_State; idx: Integer): Boolean; inline;
function lua_isnone(L: Plua_State; idx: Integer): Boolean; inline;
function lua_isnoneornil(L: Plua_State; idx: Integer): Boolean; inline;

function lua_pushliteral(L: Plua_State; s: PAnsiChar): PAnsiChar; inline;

procedure lua_pushglobaltable(L: Plua_State); inline;

function lua_tostring(L: Plua_State; idx: Integer): PAnsiChar; inline;

procedure lua_insert(L: Plua_State; idx: Integer); inline;

procedure lua_remove(L: Plua_State; idx: Integer); inline;

procedure lua_replace(L: Plua_State; idx: Integer); inline;

// Debug API

function lua_getstack(L: Plua_State; level: Integer; ar: Plua_Debug): LongBool; external LuaDLL;
function lua_getinfo(L: Plua_State; what: PAnsiChar; ar: Plua_Debug): Integer; external LuaDLL;
function lua_getlocal(L: Plua_State; ar: Plua_Debug; n: Integer): PAnsiChar; external LuaDLL;
function lua_setlocal(L: Plua_State; ar: Plua_Debug; n: Integer): PAnsiChar; external LuaDLL;
function lua_getupvalue(L: Plua_State; funcindex, n: Integer): PAnsiChar; external LuaDLL;
function lua_setupvalue(L: Plua_State; funcindex, n: Integer): PAnsiChar; external LuaDLL;

function lua_upvalueid(L: Plua_State; fidx, n: Integer): Pointer; external LuaDLL;
procedure lua_upvaluejoin(L: Plua_State; fidx1, n1, fidx2, n2: Integer); external LuaDLL;

procedure lua_sethook(L: Plua_State; func: lua_Hook; mask, count: Integer); external LuaDLL;
function lua_gethook(L: Plua_State): lua_Hook; external LuaDLL;
function lua_gethookmask(L: Plua_State): Integer; external LuaDLL;
function lua_gethookcount(L: Plua_State): Integer; external LuaDLL;

implementation

// helper functions

function lua_upvalueindex(i: Integer): Integer;
begin
  Result := LUA_REGISTRYINDEX - i;
end;

procedure lua_call(L: Plua_State; nargs, nresults: Integer);
begin
  lua_callk(L, nargs, nresults, 0, nil);
end;

function lua_pcall(L: Plua_State; nargs, nresults, errfunc: Integer): Integer;
begin
  Result := lua_pcallk(L, nargs, nresults, errfunc, 0, nil);
end;

function lua_yield(L: Plua_State; nresults: Integer): Integer;
begin
  Result := lua_yieldk(L, nresults, 0, nil);
end;

function lua_getextraspace(L: Plua_State): Pointer;
begin
  Result := Pointer(PAnsiChar(L) - LUA_EXTRASPACE);
end;

function lua_tonumber(L: Plua_State; idx: Integer): lua_Number;
begin
  Result := lua_tonumberx(L, idx, nil);
end;

function lua_tointeger(L: Plua_State; idx: Integer): lua_Integer;
begin
  Result := lua_tointegerx(L, idx, nil);
end;

procedure lua_pop(L: Plua_State; n: Integer);
begin
  lua_settop(L, -n - 1);
end;

procedure lua_newtable(L: Plua_State);
begin
  lua_createtable(L, 0, 0);
end;

procedure lua_register(L: Plua_State; name: PAnsiChar; fn: lua_CFunction);
begin
  lua_pushcfunction(L, fn);
  lua_setglobal(L, name)
end;

procedure lua_pushcfunction(L: Plua_State; fn: lua_CFunction);
begin
  lua_pushcclosure(L, fn, 0);
end;

function lua_isfunction(L: Plua_State; idx: Integer): Boolean;
begin
  Result := lua_type(L, idx) = LUA_TFUNCTION;
end;

function lua_istable(L: Plua_State; idx: Integer): Boolean;
begin
  Result := lua_type(L, idx) = LUA_TTABLE;
end;

function lua_islightuserdata(L: Plua_State; idx: Integer): Boolean;
begin
  Result := lua_type(L, idx) = LUA_TLIGHTUSERDATA;
end;

function lua_isnil(L: Plua_State; idx: Integer): Boolean;
begin
  Result := lua_type(L, idx) = LUA_TNIL;
end;

function lua_isboolean(L: Plua_State; idx: Integer): Boolean;
begin
  Result := lua_type(L, idx) = LUA_TBOOLEAN;
end;

function lua_isthread(L: Plua_State; idx: Integer): Boolean;
begin
  Result := lua_type(L, idx) = LUA_TTHREAD;
end;

function lua_isnone(L: Plua_State; idx: Integer): Boolean;
begin
  Result := lua_type(L, idx) = LUA_TNONE;
end;

function lua_isnoneornil(L: Plua_State; idx: Integer): Boolean;
begin
  Result := lua_type(L, idx) <= 0;
end;

function lua_pushliteral(L: Plua_State; s: PAnsiChar): PAnsiChar;
begin
  Result := lua_pushstring(L, s);
end;

procedure lua_pushglobaltable(L: Plua_State);
begin
  lua_rawgeti(L, LUA_REGISTRYINDEX, LUA_RIDX_GLOBALS);
end;

function lua_tostring(L: Plua_State; idx: Integer): PAnsiChar;
begin
  Result := lua_tolstring(L, idx, nil);
end;

procedure lua_insert(L: Plua_State; idx: Integer);
begin
  lua_rotate(L, idx, 1);
end;

procedure lua_remove(L: Plua_State; idx: Integer);
begin
  lua_rotate(L, idx, -1);
  lua_pop(L, 1);
end;

procedure lua_replace(L: Plua_State; idx: Integer);
begin
  lua_copy(L, -1, idx);
  lua_pop(L, 1);
end;

end.
