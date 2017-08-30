unit LuaDefaultLibs;

interface

uses
  LuaDefine, LuaHeader, SysUtils;

type

  { TLuaLibBasic }

  TLuaLibBasic = class(TLuaLib)
  private
    class function LuaAssert(L: TLuaState): Integer; static; cdecl;
    class function LuaError(L: TLuaState): Integer; static; cdecl;
    class function LuaGetMetatable(L: TLuaState): Integer; static; cdecl;
    class function LuaIPairs(L: TLuaState): Integer; static; cdecl;
    class function LuaNext(L: TLuaState): Integer; static; cdecl;
    class function LuaPairs(L: TLuaState): Integer; static; cdecl;
    class function LuaSetMetatable(L: TLuaState): Integer; static; cdecl;
    class function LuaToString(L: TLuaState): Integer; static; cdecl;
    class function LuaType(L: TLuaState): Integer; static; cdecl;

    class function LuaINext(L: TLuaState): Integer; static; cdecl;

  protected
    class procedure CreateEntry(AEntry: TLuaLib.TTableEntry); override;
  end;

  { TLuaLibTable }

  TLuaLibTable = class(TLuaLib)
  private
    class function LuaConcat(L: TLuaState): Integer; static; cdecl;
    class function LuaInsert(L: TLuaState): Integer; static; cdecl;
    class function LuaMove(L: TLuaState): Integer; static; cdecl;
    class function LuaPack(L: TLuaState): Integer; static; cdecl;
    class function LuaUnpack(L: TLuaState): Integer; static; cdecl;

  protected
    class procedure CreateEntry(AEntry: TLuaLib.TTableEntry); override;
  end;

implementation

{ TLuaLibBasic }

class procedure TLuaLibBasic.CreateEntry(AEntry: TLuaLib.TTableEntry);
begin
  with AEntry do
  begin
    Add('assert', LuaAssert);
    Add('error', LuaError);
    Add('getmetatable', LuaGetMetatable);
    Add('ipairs', LuaIPairs);
    Add('next', LuaNext);
    Add('pairs', LuaPairs);
    Add('setmetatable', LuaSetMetatable);
    Add('tostring', LuaToString);
    Add('type', LuaType);
    AddRecursion('_G', AEntry);
  end;
end;

class function TLuaLibBasic.LuaAssert(L: TLuaState): Integer;
begin
  L.CheckAny(1);
  if L.ToBoolean(1) then
  begin
    Result := L.Top;
  end
  else
  begin
    L.Remove(1);
    L.Top := 1;
    Result := L.Error_X;
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

class function TLuaLibBasic.LuaGetMetatable(L: TLuaState): Integer;
begin
  L.CheckAny(1);
  L.CheckEnd(2);
  if L.GetMetatable(1) then
  begin
    if L.GetField('__metatable', 2) = ltNil then
      L.Top := 2;
  end
  else
    L.PushNil;
  Result := 1;
end;

class function TLuaLibBasic.LuaINext(L: TLuaState): Integer;
var
  I: TLuaInteger;
begin
  L.CheckType(1, ltTable);
  L.Top := 2;
  L.CheckEnd(3);
  I := L.CheckOrDefault(2, 0) + 1;
  L.Top := 1;
  L.PushInteger(I);
  L.GetI(I, 1);
  if L.IsNoneOrNil then
  begin
    L.PushNil;
    Result := 1
  end
  else
    Result := 2;
end;

class function TLuaLibBasic.LuaIPairs(L: TLuaState): Integer;
begin
  L.CheckType(1, ltTable);
  L.CheckEnd(2);
  PushFunc(L, LuaINext);
  L.Insert(1);
  L.PushInteger(0);
  Result := 3;
end;

class function TLuaLibBasic.LuaNext(L: TLuaState): Integer;
begin
  L.CheckType(1, ltTable);
  L.Top := 2;
  L.CheckEnd(3);
  if L.Next(1) then
    Result := 2
  else
  begin
    L.PushNil;
    Result := 1;
  end;
end;

class function TLuaLibBasic.LuaPairs(L: TLuaState): Integer;
begin
  L.CheckType(1, ltTable);
  L.CheckEnd(2);
  L.PushCFunction(LuaNext);
  L.Insert(1);
  Result := 2;
end;

class function TLuaLibBasic.LuaSetMetatable(L: TLuaState): Integer;
begin
  L.CheckType(1, ltTable);
  L.CheckType(2, [ltTable, ltNil]);
  L.CheckEnd(3);
  if L.GetMetatable(1) then
  begin
    if L.GetField('__metatable', 3) <> ltNil then
      L.Error('cannot change a protected metatable');
    L.Top := 2;
  end;
  L.SetMetatable(1);
  Result := 1;
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

{ TLuaLibTable }

class procedure TLuaLibTable.CreateEntry(AEntry: TLuaLib.TTableEntry);
begin
  with AEntry.Add('table') do
  begin
    Add('concat', LuaConcat);
    Add('insert', LuaInsert);
    Add('move', LuaMove);
    Add('pack', LuaPack);
    Add('remove', LuaNotImplemented); // TODO: implement
    Add('sort', LuaNotImplemented); // TODO: implement
    Add('unpack', LuaUnpack);
  end;
end;

class function TLuaLibTable.LuaConcat(L: TLuaState): Integer;
var
  Lua: TLua;
  Sep: AnsiString;
  A, B, I: TLuaInteger;
begin
  Lua := TLua.FromState(L);
  L.CheckType(1, ltTable);
  L.CheckEnd(5);
  L.Top := 4;
  L.Len(1);
  if L.IsNoneOrNil(2) then
  begin
    L.PushString('');
    L.Insert(2);
  end
  else
    L.CheckType(2, ltString);
  A := L.CheckOrDefault(3, 1);
  B := L.CheckOrDefault(4, L.ToInteger);
  L.PushString('');
  for I := A to B do
  begin
    L.GetI(I, 1);
    if not (L.&Type in [ltNumber, ltString]) then
      Exit(L.ErrorFmt('table element #%d: number or string expected, got %s', [I, L.TypeNameAt]));
    if I < B then
    begin
      L.PushValue(2);
      L.Concat(3)
    end
    else
      L.Concat(2);
    if Lua.ShouldTerminate then
      Exit(0);
  end;
  Result := 1;
end;

class function TLuaLibTable.LuaInsert(L: TLuaState): Integer;
var
  Lua: TLua;
  Pos, Len, I: TLuaInteger;
begin
  Lua := TLua.FromState(L);
  L.CheckType(1, ltTable);
  L.Len(1);
  Len := L.ToInteger;
  L.Pop;
  if L.Top <= 2 then
  begin
    Pos := Len + 1;
    L.Top := 2;
  end
  else
  begin
    L.CheckType(2, ltNumber);
    Pos := L.CheckInteger(2);
    if (Pos < 1) or (Pos > Len + 1) then
      L.ErrorFmt('arg #2: pos %d out of bounds, allowed [1 - %d]', [Pos, Len + 1]);
    L.CheckEnd(4);
  end;
  L.CheckAny(L.Top);
  for I := Len downto Pos do
  begin
    L.GetI(I, 1);
    L.SetI(I + 1, 1);
    if Lua.ShouldTerminate then
      Exit(0);
  end;
  L.SetI(Pos, 1);
  Result := 0;
end;

class function TLuaLibTable.LuaMove(L: TLuaState): Integer;
var
  Lua: TLua;
  F, E, T, I: TLuaInteger;
begin
  Lua := TLua.FromState(L);
  L.CheckType(1, ltTable);
  L.CheckType(2, ltNumber);
  F := L.CheckInteger(2);
  L.CheckType(3, ltNumber);
  E := L.CheckInteger(3);
  L.CheckType(4, ltNumber);
  T := L.CheckInteger(4);
  if L.Top = 4 then
    L.PushValue(1)
  else
  begin
    L.CheckType(5, ltTable);
    L.CheckEnd(6);
  end;
  for I := 0 to E - F do
  begin
    L.GetI(F + I, 1);
    L.SetI(T + I, 5);
    if Lua.ShouldTerminate then
      Exit(0);
  end;
  Result := 1;
end;

class function TLuaLibTable.LuaPack(L: TLuaState): Integer;
var
  Lua: TLua;
  I: TLuaInteger;
begin
  Lua := TLua.FromState(L);
  L.CreateTable(L.Top, 0);
  L.Insert(1);
  for I := L.Top - 1 downto 1 do
  begin
    L.SetI(I, 1);
    if Lua.ShouldTerminate then
      Exit(0);
  end;
  Result := 1;
end;

class function TLuaLibTable.LuaUnpack(L: TLuaState): Integer;
var
  Lua: TLua;
  A, B, I: TLuaInteger;
begin
  Lua := TLua.FromState(L);
  L.CheckType(1, ltTable);
  L.CheckEnd(4);
  L.Top := 3;
  L.Len(1);
  A := L.CheckOrDefault(2, 1);
  B := L.CheckOrDefault(3, L.ToInteger);
  for I := A to B do
  begin
    L.GetI(I, 1);
    if Lua.ShouldTerminate then
      Exit(0);
  end;
  Result := B - A + 1;
end;

end.
