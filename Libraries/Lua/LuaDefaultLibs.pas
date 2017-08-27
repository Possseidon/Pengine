unit LuaDefaultLibs;

interface

uses
  LuaDefine, LuaHeader, SysUtils;

type

  TLuaLibBasic = class(TLuaLib)
  private
    class function LuaError(L: TLuaState): Integer; static; cdecl;
    class function LuaNext(L: TLuaState): Integer; static; cdecl;
    class function LuaToString(L: TLuaState): Integer; static; cdecl;
    class function LuaType(L: TLuaState): Integer; static; cdecl;

  protected
    class function CreateEntry: TLuaLib.TEntry; override;
  end;

implementation

{ TLuaLibBasic }

class function TLuaLibBasic.CreateEntry: TLuaLib.TEntry;
var
  Env: TTableEntry absolute Result;
begin
  Env := TTableEntry.Create;
  with Env do
  begin
    Add('error', LuaError);
    Add('next', LuaNext);
    Add('tostring', LuaToString);
    Add('type', LuaType);
  end;
end;

class function TLuaLibBasic.LuaError(L: TLuaState): Integer;
var
  Level: Integer;
begin
  if L.IsNone(1) then
  begin
    Level := 1;
    L.PushNil
  end
  else
  begin
    Level := L.CheckOrDefault(2, 1);
    L.CheckEnd(3);
  end;
  Result := L.Error(L.ToString(1), Level);
end;

class function TLuaLibBasic.LuaNext(L: TLuaState): Integer;
begin
  L.CheckType(1, ltTable);
  L.Top := 2;
  L.CheckEnd(3);
  if L.Next(1) then
    Result := 2
  else
    Result := 0;
end;

class function TLuaLibBasic.LuaToString(L: TLuaState): Integer;
begin
  L.CheckAny(1);
  L.CheckEnd(2);
  L.ToString;
  Result := 1;
end;

class function TLuaLibBasic.LuaType(L: TLuaState): Integer;
var
  T: TLuaType;
begin
  T := L.CheckAny(1);
  L.CheckEnd(2);
  L.PushString(L.TypeName(T));
  Result := 1;
end;

end.
