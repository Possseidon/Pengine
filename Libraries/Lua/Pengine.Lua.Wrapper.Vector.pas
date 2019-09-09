unit Pengine.Lua.Wrapper.Vector;

interface

uses
  Pengine.Vector,
  Pengine.Lua.Wrapper;

type

  TLuaLibVector3 = class(TLuaLib)
  protected
    class procedure CreateEntry(AEntry: TLuaLib.TTableEntry); override;

  published
    class function __call(L: TLuaState): Integer; static; cdecl;

  end;

  TLuaVector3 = class(TLuaWrapper<TVector3>)
  public
    class function LuaName: AnsiString; override;

  protected
    class function TryConvertType(L: TLuaState; out AData: TLuaWrapper<TVector3>.PData; AIndex: Integer): Boolean; override;

  published
    class function __index(L: TLuaState): Integer; static; cdecl;
    class function __newindex(L: TLuaState): Integer; static; cdecl;

    class function __tostring(L: TLuaState): Integer; static; cdecl;

    class function __add(L: TLuaState): Integer; static; cdecl;
    class function __sub(L: TLuaState): Integer; static; cdecl;
    class function __mul(L: TLuaState): Integer; static; cdecl;
    class function __div(L: TLuaState): Integer; static; cdecl;

    class function __neg(L: TLuaState): Integer; static; cdecl;

    class function __eq(L: TLuaState): Integer; static; cdecl;
    class function __lt(L: TLuaState): Integer; static; cdecl;
    class function __le(L: TLuaState): Integer; static; cdecl;

    class function __len(L: TLuaState): Integer; static; cdecl;

    class function LuaGet_volume(L: TLuaState): Integer; static; cdecl;

    class function Lua_normalize(L: TLuaState): Integer; static; cdecl;
    class function Lua_dot(L: TLuaState): Integer; static; cdecl;
    class function Lua_sqrdot(L: TLuaState): Integer; static; cdecl;
    class function Lua_cross(L: TLuaState): Integer; static; cdecl;

    class function Lua_cosAngleTo(L: TLuaState): Integer; static; cde
    class function Lua_angleRadTo(L: TLuaState): Integer; static; cdecl;cl;
    class function Lua_angleTo(L: TLuaState): Integer; static; cdecl;

    class function Lua_rotateRad(L: TLuaState): Integer; static; cdecl;
    class function Lua_rotate(L: TLuaState): Integer; static; cdecl;
    class function Lua_reflect(L: TLuaState): Integer; static; cdecl;

    class function Lua_abs(L: TLuaState): Integer; static; cdecl;
    class function Lua_floor(L: TLuaState): Integer; static; cdecl;
    class function Lua_ceil(L: TLuaState): Integer; static; cdecl;

    class function Lua_min(L: TLuaState): Integer; static; cdecl;
    class function Lua_max(L: TLuaState): Integer; static; cdecl;

    class function Lua_offset(L: TLuaState): Integer; static; cdecl;

    class function LuaGet_dirs(L: TLuaState): Integer; static; cdecl;
    class function LuaGet_longestAxis(L: TLuaState): Integer; static; cdecl;

  end;

implementation

end.
