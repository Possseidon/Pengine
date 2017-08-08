unit LuaAuxLib;

interface

uses
  LuaConf, LuaHeader, SysUtils, Windows;

const

  // extra error code for 'luaL_loadfilex'
  LUA_ERRFILE = LUA_ERRERR + 1;

  // key, in the registry, for table of loaded modules
  LUA_LOADED_TABLE = '_LOADED';

  // key, in the registry, for table of preloaded loaders
  LUA_PRELOAD_TABLE = '_PRELOAD';

  LUAL_NUMSIZES = SizeOf(lua_Integer) * 16 + SizeOf(lua_Number);

  // predefined references
  LUA_NOREF = -2;
  LUA_REFNIL = -1;

  LUAL_BUFFERSIZE = 512;

  LUA_FILEHANDLE = 'FILE*';

type

  PluaL_Reg = ^luaL_Reg;
  luaL_Reg = record
    name: PAnsiChar;
    func: lua_CFunction;
  end;

  PluaL_Buffer = ^luaL_Buffer;
  luaL_Buffer = record
    b: PAnsiChar;
    size: NativeUInt;
    n: NativeUInt;
    L: Plua_State;
    initb: array [0 .. LUAL_BUFFERSIZE - 1] of AnsiChar;
  end;

  PluaL_Stream = ^luaL_Stream;
  luaL_Stream = record
    f: ^file;
    closef: lua_CFunction;
  end;

procedure luaL_checkversion_(L: Plua_State; ver: lua_Number; sz: NativeUInt);
procedure luaL_checkversion(L: Plua_State);

function luaL_getmetafield(L: Plua_State; obj: Integer; e: PAnsiChar): Integer;
function luaL_callmeta(L: Plua_State; obj: Integer; e: PAnsiChar): Integer;
function luaL_tolstring(L: Plua_State; idx: Integer; len: PNativeUInt): PAnsiChar;
function luaL_argerror(L: Plua_State; arg: Integer; extramsg: PAnsiChar): Integer;
function luaL_checklstring(L: Plua_State; arg: Integer; len: PNativeUInt): PAnsiChar;
function luaL_optlstring(L: Plua_State; arg: Integer; def: PAnsiChar; len: PNativeUInt): PAnsiChar;
function luaL_checknumber(L: Plua_State; arg: Integer): lua_Number;
function luaL_optnumber(L: Plua_State; arg: Integer; def: lua_Number): lua_Number;

function luaL_checkinteger(L: Plua_State; arg: Integer): lua_Integer;
function luaL_optinteger(L: Plua_State; arg: Integer; def: lua_Integer): lua_Integer;

procedure luaL_checkstack(L: Plua_State; space: Integer; msg: PAnsiChar);
procedure luaL_checktype(L: Plua_State; arg, t: Integer);
procedure luaL_checkany(L: Plua_State; arg: Integer);

function luaL_newmetatable(L: Plua_State; tname: PAnsiChar): LongBool;
procedure luaL_setmetatable(L: Plua_State; tname: PAnsiChar);
function luaL_testudata(L: Plua_State; ud: Integer; tname: PAnsiChar): Pointer;
function luaL_checkudata(L: Plua_State; ud: Integer; tname: PAnsiChar): Pointer;

procedure luaL_where(L: Plua_State; level: Integer);
function luaL_error(L: Plua_State; fmt: PAnsiChar; a: array of const): Integer; overload;
function luaL_error(L: Plua_State; str: PAnsiChar): Integer; overload;

function luaL_checkoption(L: Plua_State; arg: Integer; def: PAnsiChar; lst: array of PAnsiChar): Integer;

function luaL_fileresult(L: Plua_State; stat: Integer; fname: PAnsiChar): Integer;
function luaL_execresult(L: Plua_State; stat: Integer): Integer;

function luaL_ref(L: Plua_State; t: Integer): Integer;
procedure luaL_unref(L: Plua_State; t, ref: Integer);

function luaL_loadfilex(L: Plua_State; filename, mode: PAnsiChar): LongBool;
function luaL_loadfile(L: Plua_State; filename: PAnsiChar): LongBool;

function luaL_loadbufferx(L: Plua_State; buff: PAnsiChar; sz: NativeUInt; name, mode: PAnsiChar): Integer;
function luaL_loadstring(L: Plua_State; s: PAnsiChar): Integer;

function luaL_newstate: Plua_State;

function luaL_len(L: Plua_State; idx: Integer): lua_Integer;

function luaL_gsub(L: Plua_State; s, p, r: PAnsiChar): PAnsiChar;

procedure luaL_setfuncs(L: Plua_state; lib: PluaL_Reg; nup: Integer);

function luaL_getsubtable(L: Plua_State; idx: Integer; fname: PAnsiChar): Integer;

procedure luaL_traceback(L, L1: Plua_State; msg: PAnsiChar; level: Integer);

procedure luaL_requiref(L: Plua_State; modname: PAnsiChar; openf: lua_CFunction; glb: Integer);

// --- some useful macros ---
procedure luaL_newlibtable(L: Plua_State; lib: PluaL_Reg); inline;

procedure luaL_newlib(L: Plua_State; lib: PluaL_Reg); inline;

procedure luaL_argcheck(L: Plua_State; cond: Boolean; arg: Integer; extramsg: PAnsiChar); inline;
function luaL_checkstring(L: Plua_State; arg: Integer): PAnsiChar; inline;
function luaL_optstring(L: Plua_State; arg: Integer; def: PAnsiChar): PAnsiChar; inline;

function luaL_typename(L: Plua_State; idx: Integer): PAnsiChar; inline;

function luaL_dofile(L: Plua_State; fn: lua_CFunction): LongBool; inline;

function luaL_dostring(L: Plua_State; s: PAnsiChar): LongBool; inline;

function luaL_getmetatable(L: Plua_State; e: PAnsiChar): Integer; inline;

// not convertible
// function luaL_opt(L: Plua_State; f, n, d); inline;

function luaL_loadbuffer(L: Plua_State; buff: PAnsiChar; sz: NativeUInt; name: PAnsiChar): Integer; inline;

// --- generic buffer manipulation ---
procedure luaL_addchar(B: PluaL_Buffer; c: AnsiChar); inline;

procedure luaL_addsize(B: PluaL_Buffer; s: Integer); inline;

procedure luaL_buffinit(L: Plua_state; B: PluaL_Buffer);
function luaL_prepbuffsize(B: PluaL_Buffer; sz: NativeUInt): PAnsiChar;
procedure luaL_addlstring(B: PluaL_Buffer; s: PAnsiChar; len: NativeUInt);
procedure luaL_addstring(B: PluaL_Buffer; s: PAnsiChar);
procedure luaL_adddvalue(B: PluaL_Buffer);
procedure luaL_pushresult(B: PluaL_Buffer);
procedure luaL_pushresultsize(B: PluaL_Buffer; sz: NativeUInt);
function luaL_buffinitsize(L: Plua_state; B: PluaL_Buffer; sz: NativeUInt): PAnsiChar;

function luaL_prepbuffer(B: PluaL_Buffer): PAnsiChar; inline;

// "Abstraction Layer" for basic report of messages and errors

procedure lua_writestring(s: PAnsiChar; len: NativeUInt);
procedure lua_writenewline;
procedure lua_writestringerror(s: PAnsiChar; fmt: array of const);

implementation

const
  LEVELS1 = 10;
  LEVELS2 = 11;

  // index for free-list header
  freelist = 0;

type

  PUBox = ^UBox;
  UBox = record
    box: Pointer;
    bsize: NativeUInt;
  end;

  PLoadF = ^LoadF;
  LoadF = record
    n: Integer;
    f: ^file;
    buff: array [0 .. LUAL_BUFFERSIZE - 1] of AnsiChar;
  end;

// search for 'objidx' in table at index -1.
// return 1 + string at top if find a good name.
function findfield(L: Plua_State; objidx, level: Integer): LongBool;
begin
  if (level = 0) or not lua_istable(L, -1) then
    Exit(False);
  lua_pushnil(L);
  while lua_next(L, -2) do
  begin
    if lua_type(L, -2) = LUA_TSTRING then
    begin
      if lua_rawequal(L, objidx, -1) then
      begin
        lua_pop(L, 1);
        Exit(True);
      end
      else if findfield(L, objidx, level - 1) then
      begin
        lua_remove(L, -2);
        lua_pushliteral(L, '');
        lua_insert(L, -2);
        lua_concat(L, 3);
        Exit(True);
      end;
    end;
    lua_pop(L, 1);
  end;
  Result := False;
end;

// Search for a name for a function in all loaded modules
function pushglobalfuncname(L: Plua_State; ar: Plua_Debug): LongBool;
var
  top: Integer;
  name: AnsiString;
begin
  top := lua_gettop(L);
  lua_getinfo(L, 'f', ar);
  lua_getfield(L, LUA_REGISTRYINDEX, LUA_LOADED_TABLE);
  if findfield(L, top + 1, 2) then
  begin
    name := lua_tostring(L, -1);
    if name.StartsWith('_G.') then
    begin
      lua_pushstring(L, @name[1]);
      lua_remove(L, -2);
    end;
    lua_copy(L, -1, top + 1);
    lua_pop(L, 2);
    Exit(True);
  end;
  lua_settop(L, top);
  Result := False;
end;

procedure pushfuncname(L: Plua_State; ar: Plua_Debug);
begin
  if pushglobalfuncname(L, ar) then
  begin
    lua_pushfstring(L, 'function ''%s''', lua_tostring(L, -1));
    lua_remove(L, -2);
  end
  else if ar^.namewhat^ <> #0 then
    lua_pushfstring(L, '%s ''%s''', ar^.namewhat, ar^.name)
  else if ar^.what^ = 'm' then
    lua_pushliteral(L, 'main chunk')
  else if ar^.what^ = 'C' then
    lua_pushfstring(L, 'function <%s:%d>', ar^.short_src, ar^.linedefined)
  else
    lua_pushliteral(L, '?');
end;

function lastlevel(L: Plua_State): Integer;
var
  li, le, m: Integer;
  ar: lua_Debug;
begin
  li := 1;
  le := 1;
  while lua_getstack(L, le, @ar) do
  begin
    li := le;
    le := le shl 1; // x2
  end;
  while li < le do
  begin
    m := (li + le) div 2;
    if lua_getstack(L, m, @ar) then
      li := m + 1
    else
      le := m;
  end;
  Result := le - 1;
end;

function typeerror(L: Plua_State; arg: Integer; tname: PAnsiChar): Integer;
var
  msg, typearg: PAnsiChar;
begin
  if luaL_getmetafield(L, arg, '__name') = LUA_TSTRING then
    typearg := lua_tostring(L, -1)
  else if lua_type(L, arg) = LUA_TLIGHTUSERDATA then
    typearg := 'light userdata'
  else
    typearg := luaL_typename(L, arg);
  msg := lua_pushfstring(L, '%s expected, got %s', tname, typearg);
  Result := luaL_argerror(L, arg, msg);
end;

procedure tag_error(L: Plua_State; arg, tag: Integer);
begin
  typeerror(L, arg, lua_typename(L, tag));
end;

procedure interror(L: Plua_State; arg: Integer);
begin
  if lua_isnumber(L, arg) then
    luaL_argerror(L, arg, 'number has no integer representation')
  else
    tag_error(L, arg, LUA_TNUMBER);
end;

function resizebox(L: Plua_State; idx: Integer; newsize: NativeUInt): Pointer;
var
  ud, temp: Pointer;
  allocf: lua_Alloc;
  box: PUBox;
begin
  allocf := lua_getallocf(L, &ud);
  box := PUBox(lua_touserdata(L, idx));
  temp := allocf(ud, box^.box, box^.bsize, newsize);
  if (temp = nil) and (newsize > 0) then
  begin
    resizebox(L, idx, 0);
    luaL_error(L, 'not enough memory for buffer allocation');
  end;
  box^.box := temp;
  box^.bsize := newsize;
  Result := temp;
end;

function boxgc(L: Plua_State): Integer;
begin
  resizebox(L, 1, 0);
  Result := 0;
end;

function newbox(L: Plua_State; newsize: NativeUInt): Pointer;
var
  box: PUBox;
begin
  box := PUBox(lua_newuserdata(L, SizeOf(UBox)));
  box^.box := nil;
  box^.bsize := 0;
  if luaL_newmetatable(L, 'LUABOX') then
  begin
    lua_pushcfunction(L, boxgc);
    lua_setfield(L, -2, '__gc');
  end;
  lua_setmetatable(L, -2);
  Result := resizebox(L, -1, newsize);
end;

function buffonstack(B: PluaL_Buffer): LongBool;
begin
  Result := B^.b <> @B^.initb[0];
end;

function getF(L: Plua_State; ud: Pointer; size: PNativeUInt): PAnsiChar;
var
  lf: PLoadF;
  s: Integer;
begin
  lf := PLoadF(ud);
  if lf^.n > 0 then
  begin
    size^ := lf^.n;
    lf^.n := 0;
  end
  else
  begin
    if EOF(lf^.f^) then
      Exit(nil);
    BlockRead(lf^.f^, lf^.buff[0], Length(lf^.buff), s{%H-});
    size^ := s;
  end;
  Result := @lf^.buff[0];
end;

function errfile(L: Plua_State; what: PAnsiChar; fnameindex: Integer): Integer;
var
  serr, filename: PAnsiChar;
begin
  serr := @AnsiString(SysErrorMessage(GetLastOSError))[1];
  filename := lua_tostring(L, fnameindex) + 1;
  lua_pushfstring(L, 'cannot %s %s: %s', what, filename, serr);
  lua_remove(L, fnameindex);
  Result := LUA_ERRFILE;
end;

function skipBOM(lf: PLoadF): Integer;
var
  p: PAnsiChar;
  c: Integer;
begin
  p := #239#187#191;
  lf^.n := 0;
  repeat
    c := getc(lf^.f);
    if (AnsiChar(c) = ^Z) or (AnsiChar(c) <> p^) then
      Exit(c);
    Inc(p);
    lf^.buff[lf^.n] := c;
    Inc(lf^.n);
  until p = #0;
  lf^.n = 0;
  Result := getc(lf->f);
end;


// --- API ---

procedure luaL_checkversion_(L: Plua_State; ver: lua_Number; sz: NativeUInt);
begin
  // TODO: function
end;

procedure luaL_checkversion(L: Plua_State);
begin
  // TODO: function
end;

function luaL_getmetafield(L: Plua_State; obj: Integer; e: PAnsiChar): Integer;
begin
  // TODO: function
end;

function luaL_callmeta(L: Plua_State; obj: Integer; e: PAnsiChar): Integer;
begin
  // TODO: function
end;

function luaL_tolstring(L: Plua_State; idx: Integer; len: PNativeUInt): PAnsiChar;
begin
  // TODO: function
end;

function luaL_argerror(L: Plua_State; arg: Integer; extramsg: PAnsiChar): Integer;
var
  ar: lua_Debug;
begin
  if not lua_getstack(L, 0, @ar) then
    Exit(luaL_error(L, 'bad argument #%d (%s)', [arg, extramsg]));
  lua_getinfo(L, 'n', @ar);
  if ar.namewhat = 'method' then
  begin
    Dec(arg);
    if arg = 0 then
      Exit(luaL_error(L, 'calling ''%s'' on bad self (%s)', [ar.name, extramsg]));
  end;
  if ar.name = nil then
  begin
    if pushglobalfuncname(L, @ar) then
      ar.name := lua_tostring(L, -1)
    else
      ar.name := '?';
  end;
  Result := luaL_error(L, 'bad argument #%d to ''%s'' (%s)', [arg, ar.name, extramsg]);
end;

function luaL_checklstring(L: Plua_State; arg: Integer; len: PNativeUInt): PAnsiChar;
var
  s: PAnsiChar;
begin
  s := lua_tolstring(L, arg, len);
  if s = nil then
    tag_error(L, arg, LUA_TSTRING);
  Result := s;
end;

function luaL_optlstring(L: Plua_State; arg: Integer; def: PAnsiChar; len: PNativeUInt): PAnsiChar;
begin
  if lua_isnoneornil(L, arg) then
  begin
    if len <> nil then
    begin
      if def <> nil then
        len^ := Length(def)
      else
        len^ := 0;
    end;
    Exit(def);
  end;
  Result := luaL_checklstring(L, arg, len);
end;

function luaL_checknumber(L: Plua_State; arg: Integer): lua_Number;
var
  isnum: LongBool;
  d: lua_Number;
begin
  d := lua_tonumberx(L, arg, @isnum);
  if not isnum then
    tag_error(L, arg, LUA_TNUMBER);
  Result := d;
end;

function luaL_optnumber(L: Plua_State; arg: Integer; def: lua_Number): lua_Number;
begin
  if lua_isnoneornil(L, arg) then
    Result := def
  else
    Result := luaL_checknumber(L, arg);
end;

function luaL_checkinteger(L: Plua_State; arg: Integer): lua_Integer;
var
  isnum: LongBool;
  d: lua_Integer;
begin
  d := lua_tointegerx(L, arg, @isnum);
  if not isnum then
    interror(L, arg);
  Result := d;
end;

function luaL_optinteger(L: Plua_State; arg: Integer; def: lua_Integer): lua_Integer;
begin
  if lua_isnoneornil(L, arg) then
    Result := def
  else
    Result := luaL_checkinteger(L, arg);
end;

procedure luaL_checkstack(L: Plua_State; space: Integer; msg: PAnsiChar);
begin
  if  not lua_checkstack(L, space) then
  begin
    if msg <> nil then
      luaL_error(L, 'stack overflow (%s)', [msg])
    else
      luaL_error(L, 'stack overflow');
  end;
end;

procedure luaL_checktype(L: Plua_State; arg, t: Integer);
begin
  if lua_type(L, arg) <> t then
    tag_error(L, arg, t);
end;

procedure luaL_checkany(L: Plua_State; arg: Integer);
begin
  if lua_type(L, arg) = LUA_TNONE then
    luaL_argerror(L, arg, 'value expected');
end;

function luaL_newmetatable(L: Plua_State; tname: PAnsiChar): LongBool;
begin
  if luaL_getmetatable(L, tname) <> LUA_TNIL then
    Exit(False);
  lua_pop(L, 1);
  lua_createtable(L, 0, 2);
  lua_pushstring(L, tname);
  lua_setfield(L, -2, '__name');
  lua_pushvalue(L, -1);
  lua_setfield(L, LUA_REGISTRYINDEX, tname);
  Result := True;
end;

procedure luaL_setmetatable(L: Plua_State; tname: PAnsiChar);
begin
  luaL_getmetatable(L, tname);
  lua_setmetatable(L, -2);
end;

function luaL_testudata(L: Plua_State; ud: Integer; tname: PAnsiChar): Pointer;
var
  p: Pointer;
begin
  p := lua_touserdata(L, ud);
  if p <> nil then
  begin
    if lua_getmetatable(L, ud) then
    begin
      luaL_getmetatable(L, tname);
      if not lua_rawequal(L, -1, -2) then
        p := nil;
      lua_pop(L, 2);
      Exit(p);
    end;
  end;
  Result := nil;
end;

function luaL_checkudata(L: Plua_State; ud: Integer; tname: PAnsiChar): Pointer;
var
  p: Pointer;
begin
  p := luaL_testudata(L, ud, tname);
  if p = nil then
    typeerror(L, ud, tname);
  Result := p;
end;

procedure luaL_where(L: Plua_State; level: Integer);
var
  ar: lua_Debug;
begin
  if lua_getstack(L, level, @ar) then
  begin
    lua_getinfo(L, 'Sl', @ar);
    if ar.currentline > 0 then
    begin
      lua_pushfstring(L, '%s:%d', ar.short_src, ar.currentline);
      Exit;
    end;
  end;
  lua_pushfstring(L, '');
end;

function luaL_error(L: Plua_State; fmt: PAnsiChar; a: array of const): Integer;
begin
  luaL_where(L, 1);
  // yes, I hate my life...
  case Length(a) of
    1:  lua_pushfstring(L, fmt, a[0]);
    2:  lua_pushfstring(L, fmt, a[0], a[1]);
    3:  lua_pushfstring(L, fmt, a[0], a[1], a[2]);
    4:  lua_pushfstring(L, fmt, a[0], a[1], a[2], a[3]);
    5:  lua_pushfstring(L, fmt, a[0], a[1], a[2], a[3], a[4]);
    6:  lua_pushfstring(L, fmt, a[0], a[1], a[2], a[3], a[4], a[5]);
    7:  lua_pushfstring(L, fmt, a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
    8:  lua_pushfstring(L, fmt, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
    9:  lua_pushfstring(L, fmt, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8]);
    10: lua_pushfstring(L, fmt, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9]);
    11: lua_pushfstring(L, fmt, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10]);
    12: lua_pushfstring(L, fmt, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11]);
    13: lua_pushfstring(L, fmt, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12]);
    14: lua_pushfstring(L, fmt, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13]);
    15: lua_pushfstring(L, fmt, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14]);
    16: lua_pushfstring(L, fmt, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15]);
    0: lua_pushfstring(L, fmt);
  else
    raise Exception.Create('Too many VarArgs');
  end;
  lua_concat(L, 2);
  Result := lua_error(L);
end;

function luaL_error(L: Plua_State; str: PAnsiChar): Integer;
begin
  luaL_where(L, 1);
  lua_pushfstring(L, str);
  lua_concat(L, 2);
  Result := lua_error(L);
end;

function luaL_checkoption(L: Plua_State; arg: Integer; def: PAnsiChar; lst: array of PAnsiChar): Integer;
var
  name: PAnsiChar;
  I: Integer;
begin
  if def <> nil then
    name := luaL_optstring(L, arg, def)
  else
    name := luaL_checkstring(L, arg);
  for I := 0 to Length(lst) - 1 do
    if lst[I] = name then
      Exit(I);
  Result := luaL_argerror(L, arg, lua_pushfstring(L, 'invalid option ''%s''', name));
end;

function luaL_fileresult(L: Plua_State; stat: Integer; fname: PAnsiChar): Integer;
var
  en: Integer;
begin
  en := GetLastOSError;
  if stat <> 0 then
  begin
    lua_pushboolean(L, True);
    Exit(1);
  end;
  lua_pushnil(L);
  if fname <> nil then
    lua_pushfstring(L, '%s: %s', fname, @AnsiString(SysErrorMessage(en))[1])
  else
    lua_pushfstring(L, '%s', @AnsiString(SysErrorMessage(en))[1]);
  lua_pushinteger(L, en);
  Result := 3;
end;

function luaL_execresult(L: Plua_State; stat: Integer): Integer;
var
  what: PAnsiChar;
begin
  what := 'exit';
  if stat = -1 then
    Exit(luaL_fileresult(L, 0, nil));
  if (what[1] = 'e') and (stat = 0) then
    lua_pushboolean(L, True)
  else
    lua_pushnil(L);
  lua_pushstring(L, what);
  lua_pushinteger(L, stat);
  Result := 3;
end;

function luaL_ref(L: Plua_State; t: Integer): Integer;
var
  ref: Integer;
begin
  if lua_isnil(L, -1) then
  begin
    lua_pop(L, 1);
    Exit(LUA_REFNIL);
  end;
  t := lua_absindex(L, t);
  lua_rawgeti(L, t, freelist);
  ref := lua_tointeger(L, -1);
  lua_pop(L, 1);
  if ref <> 0 then
  begin
    lua_rawgeti(L, t, ref);
    lua_rawseti(L, t, freelist);
  end
  else
    ref := lua_rawlen(L, t) + 1;
  lua_rawseti(L, t, ref);
  Result := ref;
end;

procedure luaL_unref(L: Plua_State; t, ref: Integer);
begin
  if ref >= 0 then
  begin
    t := lua_absindex(L, t);
    lua_rawgeti(L, t, freelist);
    lua_rawseti(L, t, ref);
    lua_pushinteger(L, ref);
    lua_rawseti(L, t, freelist);
  end;
end;

function luaL_loadfilex(L: Plua_State; filename, mode: PAnsiChar): LongBool;
begin
  // TODO: function
end;

function luaL_loadfile(L: Plua_State; filename: PAnsiChar): LongBool;
begin
  // TODO: function
end;

function luaL_loadbufferx(L: Plua_State; buff: PAnsiChar; sz: NativeUInt; name, mode: PAnsiChar): Integer;
begin
  // TODO: function
end;

function luaL_loadstring(L: Plua_State; s: PAnsiChar): Integer;
begin
  // TODO: function
end;

function luaL_newstate: Plua_State;
begin
  // TODO: function
end;

function luaL_len(L: Plua_State; idx: Integer): lua_Integer;
begin
  // TODO: function
end;

function luaL_gsub(L: Plua_State; s, p, r: PAnsiChar): PAnsiChar;
begin
  // TODO: function
end;

procedure luaL_setfuncs(L: Plua_state; lib: PluaL_Reg; nup: Integer);
begin
  // TODO: function
end;

function luaL_getsubtable(L: Plua_State; idx: Integer; fname: PAnsiChar): Integer;
begin
  // TODO: function
end;

procedure luaL_traceback(L, L1: Plua_State; msg: PAnsiChar; level: Integer);
var
  ar: lua_Debug;
  top, last, n1: Integer;
begin
  top := lua_gettop(L);
  last := lastlevel(L);
  if last - level > LEVELS1 + LEVELS2 then
    n1 := LEVELS1
  else
    n1 := -1;
  if msg <> nil then
    lua_pushfstring(L, '%s'#10, msg);
  luaL_checkstack(L, 10, nil);
  lua_pushliteral(L, 'stack traceback:');
  while lua_getstack(L1, level, @ar) do
  begin
    Inc(level);
    Dec(n1);
    if n1 = 0 then
    begin
      lua_pushliteral(L, #10#9'...');
      level := last - LEVELS2 + 1;
    end
    else
    begin
      lua_getinfo(L1, 'Slnt', @ar);
      lua_pushfstring(L, #10#9'%s:', ar.short_src);
      if ar.currentline > 0 then
        lua_pushfstring(L, '%d:', ar.currentline);
      lua_pushliteral(L, ' in ');
      pushfuncname(L, @ar);
      if ar.istailcall then
        lua_pushliteral(L, #10#9'(...tail calls...)');
      lua_concat(L, lua_gettop(L) - top);
    end;
  end;
  lua_concat(L, lua_gettop(L) - top);
end;

procedure luaL_requiref(L: Plua_State; modname: PAnsiChar; openf: lua_CFunction; glb: Integer);
begin
  // TODO: function
end;

procedure luaL_newlibtable(L: Plua_State; lib: PluaL_Reg);
begin
  // TODO: function
end;

procedure luaL_newlib(L: Plua_State; lib: PluaL_Reg);
begin
  // TODO: function
end;

procedure luaL_argcheck(L: Plua_State; cond: Boolean; arg: Integer; extramsg: PAnsiChar);
begin
  // TODO: function
end;

function luaL_checkstring(L: Plua_State; arg: Integer): PAnsiChar;
begin
  // TODO: function
end;

function luaL_optstring(L: Plua_State; arg: Integer; def: PAnsiChar): PAnsiChar;
begin
  // TODO: function
end;

function luaL_typename(L: Plua_State; idx: Integer): PAnsiChar;
begin
  // TODO: function
end;

function luaL_dofile(L: Plua_State; fn: lua_CFunction): LongBool;
begin
  // TODO: function
end;

function luaL_dostring(L: Plua_State; s: PAnsiChar): LongBool;
begin
  // TODO: function
end;

function luaL_getmetatable(L: Plua_State; e: PAnsiChar): Integer;
begin
  // TODO: function
end;

function luaL_loadbuffer(L: Plua_State; buff: PAnsiChar; sz: NativeUInt; name: PAnsiChar): Integer;
begin
  // TODO: function
end;

procedure luaL_addchar(B: PluaL_Buffer; c: AnsiChar);
begin
  // TODO: function
end;

procedure luaL_addsize(B: PluaL_Buffer; s: Integer);
begin
  // TODO: function
end;

procedure luaL_buffinit(L: Plua_state; B: PluaL_Buffer);
begin
  B^.L := L;
  B^.b := B^.initb;
  B^.n := 0;
  B^.size := LUAL_BUFFERSIZE;
end;

function luaL_prepbuffsize(B: PluaL_Buffer; sz: NativeUInt): PAnsiChar;
var
  L: Plua_State;
  newbuff: PAnsiChar;
  newsize: NativeUInt;
begin
  L := B^.L;
  if B^.size - B^.n < sz then
  begin
    newsize := B^.size * 2;
    if newsize - B^.n < sz then
      newsize := B^.n + sz;
    if (newsize < B^.n) or (newsize - B^.n < sz) then
      luaL_error(L, 'buffer too large');
    if buffonstack(B) then
      newbuff := PAnsiChar(resizebox(L, -1, newsize))
    else
    begin
      newbuff := PAnsiChar(newbox(L, newsize));
      Move(B^.b^, newbuff^, B^.n * sizeof(AnsiChar));
    end;
    B^.b := newbuff;
    B^.size := newsize;
  end;
  Result := @B^.b[B^.n];
end;

procedure luaL_addlstring(B: PluaL_Buffer; s: PAnsiChar; len: NativeUInt);
var
  buff: PAnsiChar;
begin
  if len > 0 then
  begin
    buff := luaL_prepbuffsize(B, len);
    Move(s^, buff^, len * sizeof(AnsiChar));
    luaL_addsize(B, len);
  end;
end;

procedure luaL_addstring(B: PluaL_Buffer; s: PAnsiChar);
begin
  luaL_addlstring(B, s, Length(s));
end;

procedure luaL_adddvalue(B: PluaL_Buffer);
var
  L: Plua_State;
  len: NativeUInt;
  s: PAnsiChar;
begin
  L := B^.L;
  s := lua_tolstring(L, -1, @len);
  if buffonstack(B) then
    lua_insert(L, -2);
  luaL_addlstring(B, s, len);
  if buffonstack(B) then
    lua_remove(L, -2)
  else
    lua_remove(L, -1);
end;

procedure luaL_pushresult(B: PluaL_Buffer);
var
  L: Plua_State;
begin
  L := B^.L;
  lua_pushlstring(L, B^.b, B^.n);
  if buffonstack(B) then
  begin
    resizebox(L, -2, 0);
    lua_remove(L, -2);
  end;
end;

procedure luaL_pushresultsize(B: PluaL_Buffer; sz: NativeUInt);
begin
  luaL_addsize(B, sz);
  luaL_pushresult(B);
end;

function luaL_buffinitsize(L: Plua_state; B: PluaL_Buffer; sz: NativeUInt): PAnsiChar;
begin
  luaL_buffinit(L, B);
  Result := luaL_prepbuffsize(B, sz);
end;

function luaL_prepbuffer(B: PluaL_Buffer): PAnsiChar;
begin
  Result := luaL_prepbuffsize(B, LUAL_BUFFERSIZE);
end;

procedure lua_writestring(s: PAnsiChar; len: NativeUInt);
begin
  // TODO: function
end;

procedure lua_writenewline;
begin
  // TODO: function
end;

procedure lua_writestringerror(s: PAnsiChar; fmt: array of const);
begin
  // TODO: function
end;

end.
