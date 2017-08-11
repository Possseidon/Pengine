unit Lua;

interface

uses
  LuaHeader;

type

  { TLua }

  TLua = class
  private
    FState: TLuaState;

    class function CallLuaFunc(AL: Plua_State): Integer; static;

  public
    constructor Create;
    destructor Destroy; override;

    property State: TLuaState read FState;

    procedure PushFunc(AFunc: TLuaFunc);

  end;

implementation

{ TLuaList }

procedure TLuaList.Read(L: Plua_State);
begin
  lua_tonumber(L, lua_gettop)
end;

procedure TLuaList.Push(L: Plua_State);
begin

end;

{ TLua }

class function TLua.CallLuaFunc(AL: Plua_State): Integer;
var
  Func: TLuaFunc;
  Params: TLuaParams;
  Results: TLuaResults;
  L: TLuaState;
begin
  L := TLuaState.Create(AL);
  Func := PLuaFunc(L.ToUserdata(L.Top))^;
  Params := TLuaParams.Create(L);
  L.Free;
  Results := TLuaResults.Create;

  // could do lua_error which does a long jump
  Func(Params, Results);

  L := TLuaState.Create(AL);

  L.Free;
end;

constructor TLua.Create;
begin
  FState := TLuaState.Create;
end;

destructor TLua.Destroy;
begin
  State.Free;
  inherited Destroy;
end;

procedure TLua.PushFunc(AFunc: TLuaFunc);
begin
  State.PushLightUserdata(@AFunc);
  State.PushCFunction(CallLuaFunc);
end;

end.

