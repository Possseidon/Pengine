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

  lua_Number = LuaConf.LUA_NUMBER;
  lua_Integer = LuaConf.LUA_INTEGER;
  lua_Unsigned = LuaConf.LUA_UNSIGNED;
  lua_KContext = LuaConf.LUA_KCONTEXT;

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
    short_src: array [0 .. LUA_IDSIZE - 1] of Char; // (S)
  private
    {%H-}i_ci: Pointer;  // active function
  end;

  lua_Hook = procedure(L: TLuaState; ar: Plua_Debug);

  // Type for C (Delphi) functions registered with Lua
  lua_CFunction = function(L: TLuaState): Integer;
  // Type for continuation functions
  lua_KFunction = function(L: TLuaState; status: Integer; ctx: lua_KContext): Integer;

  // Type for functions that read/write blocks when loading/dumping Lua chunks
  lua_Reader = function(L: TLuaState; ud: Pointer; sz: PNativeUInt): PAnsiChar;
  lua_Writer = function(L: TLuaState; p: Pointer; sz: NativeUInt; ud: Pointer): Integer;

  // Type for memory-allocation functions
  lua_Alloc = function(ud, ptr: Pointer; osize, nsize: NativeUInt): Pointer;

  lua_Ident = PAnsiChar;

  { TLuaStateRec }

  // TODO: Add _ut to untyped things like "TLuaStateRec.type" and add a typed one

  TLuaStateRec = record
    // state manipulation
    procedure close; inline;
    function newthread: TLuaState; inline;

    function atpanic(panicf: lua_CFunction): lua_CFunction; inline;

    function version: Plua_Number; inline;

    // basic stack manipulation
    function absindex(idx: Integer): Integer; inline;
    function gettop: Integer; inline;
    procedure settop(index: Integer); inline;
    procedure pushvalue(index: Integer); inline;
    procedure rotate(idx, n: Integer); inline;
    procedure copy(fromidx, toidx: Integer); inline;
    function checkstack(n: Integer): LongBool; inline;

    procedure xmove(&to: TLuaState; n: Integer); inline;

    // access functions (stack -> C (Delphi))
    function isnumber(index: Integer): LongBool; inline;
    function isstring(index: Integer): LongBool; inline;
    function iscfunction(index: Integer): LongBool; inline;
    function isinteger(index: Integer): LongBool; inline;
    function isuserdata(index: Integer): LongBool; inline;
    function &type(index: Integer): Integer; inline;
    function typename(tp: Integer): PAnsiChar; inline;

    function tonumberx(index: Integer; isnum: PInteger): lua_Number; inline;
    function tointegerx(index: Integer; isnum: PInteger): lua_Integer; inline;
    function toboolean(index: Integer): LongBool; inline;
    function tolstring(index: Integer; len: PNativeUInt): PAnsiChar; inline;
    function rawlen(index: Integer): NativeUInt; inline;
    function tocfunction(index: Integer): lua_CFunction; inline;
    function touserdata(index: Integer): Pointer; inline;
    function tothread(index: Integer): TLuaState; inline;
    function topointer(index: Integer): Pointer; inline;

    // Comparison and arithmetic functions
    procedure arith(op: Integer); inline;

    function rawequal(index1, index2: Integer): LongBool; inline;
    function compare(index1, index2, op: Integer): LongBool; inline;

    // push functions (C (Delphi) -> stack)
    procedure pushnil; inline;
    procedure pushnumber(n: lua_Number); inline;
    procedure pushinteger(n: lua_Integer); inline;
    function pushlstring(s: PAnsiChar; len: NativeUInt): PAnsiChar; inline;
    function pushstring(s: PAnsiChar): PAnsiChar; inline;
    procedure pushcclosure(fn: lua_CFunction; n: Integer); inline;
    procedure pushboolean(b: LongBool); inline;
    procedure pushlightuserdata(p: Pointer); inline;
    function pushthread: LongBool; inline;

    // get functions (Lua -> stack)
    function getglobal(name: PAnsiChar): Integer; inline;
    function gettable(index: Integer): Integer; inline;
    function getfield(index: Integer; k: PAnsiChar): Integer; inline;
    function geti(index: Integer; i: lua_Integer): Integer; inline;
    function rawget(index: Integer): Integer; inline;
    function rawgeti(index: Integer; i: lua_Integer): Integer; inline;
    function rawgetp(index: Integer; p: Pointer): Integer; inline;

    procedure createtable(narr, nrec: Integer); inline;
    function newuserdata(size: NativeUInt): Pointer; inline;
    function getmetatable(index: Integer): LongBool; inline;
    function getuservalue(index: Integer): Integer; inline;

    // set functions (stack -> Lua)
    procedure setglobal(name: PAnsiChar); inline;
    procedure settable(index: Integer); inline;
    procedure setfield(index: Integer; k: PAnsiChar); inline;
    procedure seti(index: Integer; i: lua_Integer); inline;
    procedure rawset(index: Integer); inline;
    procedure rawseti(index: Integer; i: lua_Integer); inline;
    procedure rawsetp(index: Integer; p: Pointer); inline;
    procedure setmetatable(index: Integer); inline;
    procedure setuservalue(index: Integer); inline;

    // 'load' and 'call' functions (load and run Lua code)
    procedure callk(nargs, nresults: Integer; ctx: lua_KContext; k: lua_KFunction); inline;
    procedure call(nargs, nresults: Integer); inline;
    function pcallk(nargs, nresults, msgh: Integer; ctx: lua_KContext; k: lua_KFunction): Integer; inline;
    function pcall(nargs, nresults, msgh: Integer): Integer; inline;
    function load(reader: lua_Reader; data: Pointer; chunkname, mode: PAnsiChar): Integer; inline;
    function dump(writer: lua_Writer; data: Pointer; strip: Integer): Integer; inline;

    // coroutine functions
    function yieldk(nresults: Integer; ctx: lua_KContext; k: lua_KFunction): Integer; inline;
    function resume(from: TLuaState; nargs: Integer): Integer; inline;
    function status: Integer; inline;
    function isyieldable: LongBool; inline;
    function yield(nresults: Integer): Integer; inline;

    // garbage-collection function
    function gc(what, data: Integer): Integer; inline;

    // miscellaneous functions
    function error: Integer; inline;
    function next(index: Integer): LongBool; inline;
    procedure concat(n: Integer); inline;
    procedure len(index: Integer); inline;
    function stringtonumber(s: PAnsiChar): NativeUInt; inline;

    function getallocf(ud: PPointer): lua_Alloc; inline;
    procedure setallocf(f: lua_Alloc; ud: Pointer); inline;

    // some useful macros
    function getextraspace: Pointer; inline;
    function tonumber(index: Integer): lua_Number; inline;
    function tointeger(index: Integer): lua_Integer; inline;

    procedure pop(n: Integer); inline;

    procedure newtable; inline;

    procedure &register(name: PAnsiChar; f: lua_CFunction); inline;

    procedure pushcfunction(f: lua_CFunction); inline;

    function isfunction(index: Integer): LongBool; inline;
    function istable(index: Integer): LongBool; inline;
    function islightuserdata(index: Integer): LongBool; inline;
    function isnil(index: Integer): LongBool; inline;
    function isboolean(index: Integer): LongBool; inline;
    function isthread(index: Integer): LongBool; inline;
    function isnone(index: Integer): LongBool; inline;
    function isnoneornil(index: Integer): LongBool; inline;

    function pushliteral(s: PAnsiChar): PAnsiChar; inline;

    procedure pushglobaltable; inline;

    function tostring(index: Integer): PAnsiChar; inline;

    procedure insert(index: Integer); inline;

    procedure remove(index: Integer); inline;

    procedure replace(index: Integer); inline;

    // Debug API

    function getstack(level: Integer; ar: Plua_Debug): LongBool; inline;
    function getinfo(what: PAnsiChar; ar: Plua_Debug): LongBool; inline;
    function getlocal(ar: Plua_Debug; n: Integer): PAnsiChar; inline;
    function setlocal(ar: Plua_Debug; n: Integer): PAnsiChar; inline;
    function getupvalue(funcindex, n: Integer): PAnsiChar; inline;
    function setupvalue(funcindex, n: Integer): PAnsiChar; inline;

    function upvalueid(funcindex, n: Integer): Pointer; inline;
    procedure upvaluejoin(funcindex1, n1, funcindex2, n2: Integer); inline;

    procedure sethook(f: lua_Hook; mask, count: Integer); inline;
    function gethook: lua_Hook; inline;
    function gethookmask: Integer; inline;
    function gethookcount: Integer; inline;

  end;

function LuaDefaultAlloc({%H-}ud, ptr: Pointer; {%H-}osize, nsize: NativeUInt): Pointer;
function NewLuaState: TLuaState;

// --- DLL ---

// state manipulation
function lua_newstate(f: lua_Alloc; ud: Pointer): TLuaState; external LuaDLL;
procedure lua_close(L: TLuaState); external LuaDLL;
function lua_newthread(L: TLuaState): TLuaState; external LuaDLL;

function lua_atpanic(L: TLuaState; panicf: lua_CFunction): lua_CFunction; external LuaDLL;

function lua_version(L: TLuaState): Plua_Number; external LuaDLL;

// basic stack manipulation
function lua_absindex(L: TLuaState; idx: Integer): Integer; external LuaDLL;
function lua_gettop(L: TLuaState): Integer; external LuaDLL;
procedure lua_settop(L: TLuaState; index: Integer); external LuaDLL;
procedure lua_pushvalue(L: TLuaState; index: Integer); external LuaDLL;
procedure lua_rotate(L: TLuaState; idx, n: Integer); external LuaDLL;
procedure lua_copy(L: TLuaState; fromidx, toidx: Integer); external LuaDLL;
function lua_checkstack(L: TLuaState; n: Integer): LongBool; external LuaDLL;

procedure lua_xmove(from, &to: TLuaState; n: Integer); external LuaDLL;

// access functions (stack -> C (Delphi))
function lua_isnumber(L: TLuaState; index: Integer): LongBool; external LuaDLL;
function lua_isstring(L: TLuaState; index: Integer): LongBool; external LuaDLL;
function lua_iscfunction(L: TLuaState; index: Integer): LongBool; external LuaDLL;
function lua_isinteger(L: TLuaState; index: Integer): LongBool; external LuaDLL;
function lua_isuserdata(L: TLuaState; index: Integer): LongBool; external LuaDLL;
function lua_type(L: TLuaState; index: Integer): Integer; external LuaDLL;
function lua_typename(L: TLuaState; tp: Integer): PAnsiChar; external LuaDLL;

function lua_tonumberx(L: TLuaState; index: Integer; isnum: PInteger): lua_Number; external LuaDLL;
function lua_tointegerx(L: TLuaState; index: Integer; isnum: PInteger): lua_Integer; external LuaDLL;
function lua_toboolean(L: TLuaState; index: Integer): LongBool; external LuaDLL;
function lua_tolstring(L: TLuaState; index: Integer; len: PNativeUInt): PAnsiChar; external LuaDLL;
function lua_rawlen(L: TLuaState; index: Integer): NativeUInt; external LuaDLL;
function lua_tocfunction(L: TLuaState; index: Integer): lua_CFunction; external LuaDLL;
function lua_touserdata(L: TLuaState; index: Integer): Pointer; external LuaDLL;
function lua_tothread(L: TLuaState; index: Integer): TLuaState; external LuaDLL;
function lua_topointer(L: TLuaState; index: Integer): Pointer; external LuaDLL;

// Comparison and arithmetic functions
procedure lua_arith(L: TLuaState; op: Integer); external LuaDLL;

function lua_rawequal(L: TLuaState; index1, index2: Integer): LongBool; external LuaDLL;
function lua_compare(L: TLuaState; index1, index2, op: Integer): LongBool; external LuaDLL;

// push functions (C (Delphi) -> stack)
procedure lua_pushnil(L: TLuaState); external LuaDLL;
procedure lua_pushnumber(L: TLuaState; n: lua_Number); external LuaDLL;
procedure lua_pushinteger(L: TLuaState; n: lua_Integer); external LuaDLL;
function lua_pushlstring(L: TLuaState; s: PAnsiChar; len: NativeUInt): PAnsiChar; external LuaDLL;
function lua_pushstring(L: TLuaState; s: PAnsiChar): PAnsiChar; external LuaDLL;
// not usable
// function lua_pushvfstring(L: TLuaState; fmt, argp: PAnsichar): PAnsiChar; external LuaDLL;
function lua_pushfstring(L: TLuaState; fmt: PAnsiChar): PAnsiChar; varargs; cdecl; external LuaDLL;
procedure lua_pushcclosure(L: TLuaState; fn: lua_CFunction; n: Integer); external LuaDLL;
procedure lua_pushboolean(L: TLuaState; b: LongBool); external LuaDLL;
procedure lua_pushlightuserdata(L: TLuaState; p: Pointer); external LuaDLL;
function lua_pushthread(L: TLuaState): LongBool; external LuaDLL;

// get functions (Lua -> stack)
function lua_getglobal(L: TLuaState; name: PAnsiChar): Integer; external LuaDLL;
function lua_gettable(L: TLuaState; index: Integer): Integer; external LuaDLL;
function lua_getfield(L: TLuaState; index: Integer; k: PAnsiChar): Integer; external LuaDLL;
function lua_geti(L: TLuaState; index: Integer; i: lua_Integer): Integer; external LuaDLL;
function lua_rawget(L: TLuaState; index: Integer): Integer; external LuaDLL;
function lua_rawgeti(L: TLuaState; index: Integer; i: lua_Integer): Integer; external LuaDLL;
function lua_rawgetp(L: TLuaState; index: Integer; p: Pointer): Integer; external LuaDLL;

procedure lua_createtable(L: TLuaState; narr, nrec: Integer); external LuaDLL;
function lua_newuserdata(L: TLuaState; size: NativeUInt): Pointer; external LuaDLL;
function lua_getmetatable(L: TLuaState; index: Integer): LongBool; external LuaDLL;
function lua_getuservalue(L: TLuaState; index: Integer): Integer; external LuaDLL;

// set functions (stack -> Lua)
procedure lua_setglobal(L: TLuaState; name: PAnsiChar); external LuaDLL;
procedure lua_settable(L: TLuaState; index: Integer); external LuaDLL;
procedure lua_setfield(L: TLuaState; index: Integer; k: PAnsiChar); external LuaDLL;
procedure lua_seti(L: TLuaState; index: Integer; i: lua_Integer); external LuaDLL;
procedure lua_rawset(L: TLuaState; index: Integer); external LuaDLL;
procedure lua_rawseti(L: TLuaState; index: Integer; i: lua_Integer); external LuaDLL;
procedure lua_rawsetp(L: TLuaState; index: Integer; p: Pointer); external LuaDLL;
function lua_setmetatable(L: TLuaState; index: Integer): Integer; external LuaDLL;
procedure lua_setuservalue(L: TLuaState; index: Integer); external LuaDLL;

// 'load' and 'call' functions (load and run Lua code)
procedure lua_callk(L: TLuaState; nargs, nresults: Integer; ctx: lua_KContext; k: lua_KFunction); external LuaDLL;
// [ lua_call ]
function lua_pcallk(L: TLuaState; nargs, nresults, msgh: Integer; ctx: lua_KContext; k: lua_KFunction): Integer; external LuaDLL;
// [ lua_pcall ]
function lua_load(L: TLuaState; reader: lua_Reader; data: Pointer; chunkname, mode: PAnsiChar): Integer; external LuaDLL;
function lua_dump(L: TLuaState; writer: lua_Writer; data: Pointer; strip: Integer): Integer; external LuaDLL;

// coroutine functions
function lua_yieldk(L: TLuaState; nresults: Integer; ctx: lua_KContext; k: lua_KFunction): Integer; external LuaDLL;
function lua_resume(L, from: TLuaState; narg: Integer): Integer; external LuaDLL;
function lua_status(L: TLuaState): Integer; external LuaDLL;
function lua_isyieldable(L: TLuaState): LongBool; external LuaDLL;
// [ lua_yield ]

// garbage-collection function
function lua_gc(L: TLuaState; what, data: Integer): Integer; external LuaDLL;

// miscellaneous functions
function lua_error(L: TLuaState): Integer; external LuaDLL;
function lua_next(L: TLuaState; index: Integer): LongBool; external LuaDLL;
procedure lua_concat(L: TLuaState; n: Integer); external LuaDLL;
procedure lua_len(L: TLuaState; index: Integer); external LuaDLL;
function lua_stringtonumber(L: TLuaState; s: PAnsiChar): NativeUInt; external LuaDLL;

function lua_getallocf(L: TLuaState; ud: PPointer): lua_Alloc; external LuaDLL;
procedure lua_setallocf(L: TLuaState; f: lua_Alloc; ud: Pointer); external LuaDLL;

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

function lua_getstack(L: TLuaState; level: Integer; ar: Plua_Debug): LongBool; external LuaDLL;
function lua_getinfo(L: TLuaState; what: PAnsiChar; ar: Plua_Debug): LongBool; external LuaDLL;
function lua_getlocal(L: TLuaState; ar: Plua_Debug; n: Integer): PAnsiChar; external LuaDLL;
function lua_setlocal(L: TLuaState; ar: Plua_Debug; n: Integer): PAnsiChar; external LuaDLL;
function lua_getupvalue(L: TLuaState; funcindex, n: Integer): PAnsiChar; external LuaDLL;
function lua_setupvalue(L: TLuaState; funcindex, n: Integer): PAnsiChar; external LuaDLL;

function lua_upvalueid(L: TLuaState; funcindex, n: Integer): Pointer; external LuaDLL;
procedure lua_upvaluejoin(L: TLuaState; funcindex1, n1, funcindex2, n2: Integer); external LuaDLL;

procedure lua_sethook(L: TLuaState; f: lua_Hook; mask, count: Integer); external LuaDLL;
function lua_gethook(L: TLuaState): lua_Hook; external LuaDLL;
function lua_gethookmask(L: TLuaState): Integer; external LuaDLL;
function lua_gethookcount(L: TLuaState): Integer; external LuaDLL;

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
  lua_setglobal(L, name)
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

function LuaDefaultAlloc(ud, ptr: Pointer; osize, nsize: NativeUInt): Pointer;
begin
  if nsize = 0 then
  begin
    FreeMemory(ptr);
    Exit(nil);
  end;
  Result := ReallocMemory(ptr, nsize);
end;

{ TLuaStateRec }

function NewLuaState: TLuaState;
begin
  Result := lua_newstate(LuaDefaultAlloc, nil);
end;

procedure TLuaStateRec.close;
begin
  lua_close(@Self);
end;

function TLuaStateRec.newthread: TLuaState;
begin
  Result := lua_newthread(@Self);
end;

function TLuaStateRec.atpanic(panicf: lua_CFunction): lua_CFunction;
begin
  Result := lua_atpanic(@Self, panicf);
end;

function TLuaStateRec.version: Plua_Number;
begin
  Result := lua_version(@Self);
end;

function TLuaStateRec.absindex(idx: Integer): Integer;
begin
  Result := lua_absindex(@Self, idx);
end;

function TLuaStateRec.gettop: Integer;
begin
  Result := lua_gettop(@Self);
end;

procedure TLuaStateRec.settop(index: Integer);
begin
  lua_settop(@Self, index);
end;

procedure TLuaStateRec.pushvalue(index: Integer);
begin
  lua_pushvalue(@Self, index);
end;

procedure TLuaStateRec.rotate(idx, n: Integer);
begin
  lua_rotate(@Self, idx, n);
end;

procedure TLuaStateRec.copy(fromidx, toidx: Integer);
begin
  lua_copy(@Self, fromidx, toidx);
end;

function TLuaStateRec.checkstack(n: Integer): LongBool;
begin
  Result := lua_checkstack(@Self, n);
end;

procedure TLuaStateRec.xmove(&to: TLuaState; n: Integer);
begin
  lua_xmove(@Self, &to, n);
end;

function TLuaStateRec.isnumber(index: Integer): LongBool;
begin
  Result := lua_isnumber(@Self, index);
end;

function TLuaStateRec.isstring(index: Integer): LongBool;
begin
  Result := lua_isstring(@Self, index);
end;

function TLuaStateRec.iscfunction(index: Integer): LongBool;
begin
  Result := lua_iscfunction(@Self, index);
end;

function TLuaStateRec.isinteger(index: Integer): LongBool;
begin
  Result := lua_isinteger(@Self, index);
end;

function TLuaStateRec.isuserdata(index: Integer): LongBool;
begin
  Result := lua_isuserdata(@Self, index);
end;

function TLuaStateRec.&type(index: Integer): Integer;
begin
  Result := lua_type(@Self, index);
end;

function TLuaStateRec.typename(tp: Integer): PAnsiChar;
begin
  Result := lua_typename(@Self, tp);
end;

function TLuaStateRec.tonumberx(index: Integer; isnum: PInteger): lua_Number;
begin
  Result := lua_tonumberx(@Self, index, isnum);
end;

function TLuaStateRec.tointegerx(index: Integer; isnum: PInteger): lua_Integer;
begin
  Result := lua_tointegerx(@Self, index, isnum);
end;

function TLuaStateRec.toboolean(index: Integer): LongBool;
begin
  Result := lua_toboolean(@Self, index);
end;

function TLuaStateRec.tolstring(index: Integer; len: PNativeUInt): PAnsiChar;
begin
  Result := lua_tolstring(@Self, index, len);
end;

function TLuaStateRec.rawlen(index: Integer): NativeUInt;
begin
  Result := lua_rawlen(@Self, index);
end;

function TLuaStateRec.tocfunction(index: Integer): lua_CFunction;
begin
  Result := lua_tocfunction(@Self, index);
end;

function TLuaStateRec.touserdata(index: Integer): Pointer;
begin
  Result := lua_touserdata(@Self, index);
end;

function TLuaStateRec.tothread(index: Integer): TLuaState;
begin
  Result := lua_tothread(@Self, index);
end;

function TLuaStateRec.topointer(index: Integer): Pointer;
begin
  Result := lua_topointer(@Self, index);
end;

procedure TLuaStateRec.arith(op: Integer);
begin
  lua_arith(@Self, op);
end;

function TLuaStateRec.rawequal(index1, index2: Integer): LongBool;
begin
  Result := lua_rawequal(@Self, index1, index2);
end;

function TLuaStateRec.compare(index1, index2, op: Integer): LongBool;
begin
  Result := lua_compare(@Self, index1, index2, op);
end;

procedure TLuaStateRec.pushnil;
begin
  lua_pushnil(@Self);
end;

procedure TLuaStateRec.pushnumber(n: lua_Number);
begin
  lua_pushnumber(@Self, n);
end;

procedure TLuaStateRec.pushinteger(n: lua_Integer);
begin
  lua_pushinteger(@Self, n);
end;

function TLuaStateRec.pushlstring(s: PAnsiChar; len: NativeUInt): PAnsiChar;
begin
  Result := lua_pushlstring(@Self, s, len);
end;

function TLuaStateRec.pushstring(s: PAnsiChar): PAnsiChar;
begin
  Result := lua_pushstring(@Self, s);
end;

procedure TLuaStateRec.pushcclosure(fn: lua_CFunction; n: Integer);
begin
  lua_pushcclosure(@Self, fn, n);
end;

procedure TLuaStateRec.pushboolean(b: LongBool);
begin
  lua_pushboolean(@Self, b);
end;

procedure TLuaStateRec.pushlightuserdata(p: Pointer);
begin
  lua_pushlightuserdata(@Self, p);
end;

function TLuaStateRec.pushthread: LongBool;
begin
  Result := lua_pushthread(@Self);
end;

function TLuaStateRec.getglobal(name: PAnsiChar): Integer;
begin
  Result := lua_getglobal(@Self, name);
end;

function TLuaStateRec.gettable(index: Integer): Integer;
begin
  Result := lua_gettable(@Self, index);
end;

function TLuaStateRec.getfield(index: Integer; k: PAnsiChar): Integer;
begin
  Result := lua_getfield(@Self, index, k);
end;

function TLuaStateRec.geti(index: Integer; i: lua_Integer): Integer;
begin
  Result := lua_geti(@Self, index, i);
end;

function TLuaStateRec.rawget(index: Integer): Integer;
begin
  Result := lua_rawget(@Self, index);
end;

function TLuaStateRec.rawgeti(index: Integer; i: lua_Integer): Integer;
begin
  Result := lua_rawgeti(@Self, index, i);
end;

function TLuaStateRec.rawgetp(index: Integer; p: Pointer): Integer;
begin
  Result := lua_rawgetp(@Self, index, p);
end;

procedure TLuaStateRec.createtable(narr, nrec: Integer);
begin
  lua_createtable(@Self, narr, nrec);
end;

function TLuaStateRec.newuserdata(size: NativeUInt): Pointer;
begin
  Result := lua_newuserdata(@Self, size);
end;

function TLuaStateRec.getmetatable(index: Integer): LongBool;
begin
  Result := lua_getmetatable(@Self, index);
end;

function TLuaStateRec.getuservalue(index: Integer): Integer;
begin
  Result := lua_getuservalue(@Self, index);
end;

procedure TLuaStateRec.setglobal(name: PAnsiChar);
begin
  lua_setglobal(@Self, name);
end;

procedure TLuaStateRec.settable(index: Integer);
begin
  lua_settable(@Self, index);
end;

procedure TLuaStateRec.setfield(index: Integer; k: PAnsiChar);
begin
  lua_setfield(@Self, index, k);
end;

procedure TLuaStateRec.seti(index: Integer; i: lua_Integer);
begin
  lua_seti(@Self, index, i);
end;

procedure TLuaStateRec.rawset(index: Integer);
begin
  lua_rawset(@Self, index);
end;

procedure TLuaStateRec.rawseti(index: Integer; i: lua_Integer);
begin
  lua_rawseti(@Self, index, i);
end;

procedure TLuaStateRec.rawsetp(index: Integer; p: Pointer);
begin
  lua_rawsetp(@Self, index, p);
end;

procedure TLuaStateRec.setmetatable(index: Integer);
begin
  lua_setmetatable(@Self, index);
end;

procedure TLuaStateRec.setuservalue(index: Integer);
begin
  lua_setuservalue(@Self, index);
end;

procedure TLuaStateRec.callk(nargs, nresults: Integer; ctx: lua_KContext; k: lua_KFunction);
begin
  lua_callk(@Self, nargs, nresults, ctx, k);
end;

procedure TLuaStateRec.call(nargs, nresults: Integer);
begin
  lua_call(@Self, nargs, nresults);
end;

function TLuaStateRec.pcallk(nargs, nresults, msgh: Integer; ctx: lua_KContext; k: lua_KFunction): Integer;
begin
  Result := lua_pcallk(@Self, nargs, nresults, msgh, ctx, k);
end;

function TLuaStateRec.pcall(nargs, nresults, msgh: Integer): Integer;
begin
  Result := lua_pcall(@Self, nargs, nresults, msgh);
end;

function TLuaStateRec.load(reader: lua_Reader; data: Pointer; chunkname, mode: PAnsiChar): Integer;
begin
  Result := lua_load(@Self, reader, data, chunkname, mode);
end;

function TLuaStateRec.dump(writer: lua_Writer; data: Pointer; strip: Integer): Integer;
begin
  Result := lua_dump(@Self, writer, data, strip);
end;

function TLuaStateRec.yieldk(nresults: Integer; ctx: lua_KContext; k: lua_KFunction): Integer;
begin
  Result := lua_yieldk(@Self, nresults, ctx, k);
end;

function TLuaStateRec.resume(from: TLuaState; nargs: Integer): Integer;
begin
  Result := lua_resume(@Self, from, nargs);
end;

function TLuaStateRec.status: Integer;
begin
  Result := lua_status(@Self);
end;

function TLuaStateRec.isyieldable: LongBool;
begin
  Result := lua_isyieldable(@Self);
end;

function TLuaStateRec.yield(nresults: Integer): Integer;
begin
  Result := lua_yield(@Self, nresults);
end;

function TLuaStateRec.gc(what, data: Integer): Integer;
begin
  Result := lua_gc(@Self, what, data);
end;

function TLuaStateRec.error: Integer;
begin
  Result := lua_error(@Self);
end;

function TLuaStateRec.next(index: Integer): LongBool;
begin
  Result := lua_next(@Self, index);
end;

procedure TLuaStateRec.concat(n: Integer);
begin
  lua_concat(@Self, n);
end;

procedure TLuaStateRec.len(index: Integer);
begin
  lua_len(@Self, index);
end;

function TLuaStateRec.stringtonumber(s: PAnsiChar): NativeUInt;
begin
  Result := lua_stringtonumber(@Self, s);
end;

function TLuaStateRec.getallocf(ud: PPointer): lua_Alloc;
begin
  Result := lua_getallocf(@Self, ud);
end;

procedure TLuaStateRec.setallocf(f: lua_Alloc; ud: Pointer);
begin
  lua_setallocf(@Self, f, ud);
end;

function TLuaStateRec.getextraspace: Pointer;
begin
  Result := lua_getextraspace(@Self);
end;

function TLuaStateRec.tonumber(index: Integer): lua_Number;
begin
  Result := lua_tonumber(@Self, index);
end;

function TLuaStateRec.tointeger(index: Integer): lua_Integer;
begin
  Result := lua_tointeger(@Self, index);
end;

procedure TLuaStateRec.pop(n: Integer);
begin
  lua_pop(@Self, n);
end;

procedure TLuaStateRec.newtable;
begin
  lua_newtable(@Self);
end;

procedure TLuaStateRec.&register(name: PAnsiChar; f: lua_CFunction);
begin
  lua_register(@Self, name, f);
end;

procedure TLuaStateRec.pushcfunction(f: lua_CFunction);
begin
  lua_pushcfunction(@Self, f);
end;

function TLuaStateRec.isfunction(index: Integer): LongBool;
begin
  Result := lua_isfunction(@Self, index);
end;

function TLuaStateRec.istable(index: Integer): LongBool;
begin
  Result := lua_istable(@Self, index);
end;

function TLuaStateRec.islightuserdata(index: Integer): LongBool;
begin
  Result := lua_islightuserdata(@Self, index);
end;

function TLuaStateRec.isnil(index: Integer): LongBool;
begin
  Result := lua_isnil(@Self, index);
end;

function TLuaStateRec.isboolean(index: Integer): LongBool;
begin
  Result := lua_isboolean(@Self, index);
end;

function TLuaStateRec.isthread(index: Integer): LongBool;
begin
  Result := lua_isthread(@Self, index);
end;

function TLuaStateRec.isnone(index: Integer): LongBool;
begin
  Result := lua_isnone(@Self, index);
end;

function TLuaStateRec.isnoneornil(index: Integer): LongBool;
begin
  Result := lua_isnoneornil(@Self, index);
end;

function TLuaStateRec.pushliteral(s: PAnsiChar): PAnsiChar;
begin
  Result := lua_pushliteral(@Self, s);
end;

procedure TLuaStateRec.pushglobaltable;
begin
  lua_pushglobaltable(@Self);
end;

function TLuaStateRec.tostring(index: Integer): PAnsiChar;
begin
  Result := lua_tostring(@Self, index);
end;

procedure TLuaStateRec.insert(index: Integer);
begin
  lua_insert(@Self, index);
end;

procedure TLuaStateRec.remove(index: Integer);
begin
  lua_remove(@Self, index);
end;

procedure TLuaStateRec.replace(index: Integer);
begin
  lua_replace(@Self, index);
end;

function TLuaStateRec.getstack(level: Integer; ar: Plua_Debug): LongBool;
begin
  Result := lua_getstack(@Self, level, ar);
end;

function TLuaStateRec.getinfo(what: PAnsiChar; ar: Plua_Debug): LongBool;
begin
  Result := lua_getinfo(@Self, what, ar);
end;

function TLuaStateRec.getlocal(ar: Plua_Debug; n: Integer): PAnsiChar;
begin
  Result := lua_getlocal(@Self, ar, n);
end;

function TLuaStateRec.setlocal(ar: Plua_Debug; n: Integer): PAnsiChar;
begin
  Result := lua_setlocal(@Self, ar, n);
end;

function TLuaStateRec.getupvalue(funcindex, n: Integer): PAnsiChar;
begin
  Result := lua_getupvalue(@Self, funcindex, n);
end;

function TLuaStateRec.setupvalue(funcindex, n: Integer): PAnsiChar;
begin
  Result := lua_setupvalue(@Self, funcindex, n);
end;

function TLuaStateRec.upvalueid(funcindex, n: Integer): Pointer;
begin
  Result := lua_upvalueid(@Self, funcindex, n);
end;

procedure TLuaStateRec.upvaluejoin(funcindex1, n1, funcindex2, n2: Integer);
begin
  lua_upvaluejoin(@Self, funcindex1, n1, funcindex2, n2);
end;

procedure TLuaStateRec.sethook(f: lua_Hook; mask, count: Integer);
begin
  lua_sethook(@Self, f, mask, count);
end;

function TLuaStateRec.gethook: lua_Hook;
begin
  Result := lua_gethook(@Self);
end;

function TLuaStateRec.gethookmask: Integer;
begin
  Result := lua_gethookmask(@Self);
end;

function TLuaStateRec.gethookcount: Integer;
begin
  Result := lua_gethookcount(@Self);
end;

end.
