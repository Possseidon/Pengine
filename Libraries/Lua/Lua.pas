unit Lua;

interface

uses
  LuaConf;

const

  // Version
  LuaDLLName = 'lua53.dll';

  LUA_VERSION_MAJOR = '5';
  LUA_VERSION_MINOR = '3';
  LUA_VERSION_NUM = '503';
  LUA_VERSION_RELEASE = '4';

  LUA_VERSION = 'Lua ' + LUA_VERSION_MAJOR + '.' + LUA_VERSION_MINOR;
  LUA_RELEASE = LUA_VERSION + '.' + LUA_VERSION_RELEASE;
  LUA_COPYRIGHT = LUA_RELEASE + '  Copyright (C) 1994-2017 Lua.org, PUC-Rio';
  LUA_AUTHORS = 'R. Ierusalimschy, L. H. de Figueiredo, W. Celes';

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
  LUA_YIELD = 1;
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
  LUA_RIDX_LAST	= LUA_RIDX_GLOBALS;

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

  lua_State = record
  end;

  Plua_State = ^lua_State;

  lua_Alloc =
    function(ud, ptr: Pointer; osize, nsize: NativeUInt): Pointer;

// DLL
function lua_newstate(f: lua_Alloc; ud: Pointer): Plua_State; external LuaDLLName;
procedure lua_close(L: Plua_State); external LuaDLLName;

// helper functions
function lua_upvalueindex(i: Integer): Integer; inline;

implementation

// helper functions

function lua_upvalueindex(i: Integer): Integer;
begin
  Result := LUA_REGISTRYINDEX - i;
end;

end.
