unit LuaDefaultLibs;

interface

uses
  LuaDefine, LuaHeader, SysUtils, Sorting, Math;

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
  private type

    TLuaSorter = class(TQuickSorter)
    private
      FParent: TLuaLibTable;
      FLua: TLua;

      function GetL: TLuaState;

      property L: TLuaState read GetL;

    protected
      function Low: Integer; override;
      function High: Integer; override;

      procedure SavePivot(I: Integer); override;
      procedure DiscardPivot; override;
      function CompareToPivot(I: Integer; ADir: TQuickSorter.TCompareDirection): Boolean; override;

      procedure Swap(A, B: Integer); override;

    public
      constructor Create(AParent: TLuaLibTable);

    end;

  private
    FSorter: TLuaSorter;

    class function LuaConcat(L: TLuaState): Integer; static; cdecl;
    class function LuaInsert(L: TLuaState): Integer; static; cdecl;
    class function LuaMove(L: TLuaState): Integer; static; cdecl;
    class function LuaPack(L: TLuaState): Integer; static; cdecl;
    class function LuaRemove(L: TLuaState): Integer; static; cdecl;
    class function LuaSort(L: TLuaState): Integer; static; cdecl;
    class function LuaUnpack(L: TLuaState): Integer; static; cdecl;

    class function LuaDefaultCompare(L: TLuaState): Integer; static; cdecl;

  protected
    class procedure CreateEntry(AEntry: TLuaLib.TTableEntry); override;

  public
    constructor Create(AL: TLuaState); override;
    destructor Destroy; override;

  end;

  { TLuaLibMath }

  TLuaLibMath = class(TLuaLib)
  private
    // class function LuaAbs(L: TLuaState): Integer; static; cdecl;
    // class function LuaArcCos(L: TLuaState): Integer; static; cdecl;
    // class function LuaArcSin(L: TLuaState): Integer; static; cdecl;
    // class function LuaArcTan(L: TLuaState): Integer; static; cdecl;
    // class function LuaCeil(L: TLuaState): Integer; static; cdecl;
    // class function LuaCos(L: TLuaState): Integer; static; cdecl;
    // class function LuaDeg(L: TLuaState): Integer; static; cdecl;
    // class function LuaExp(L: TLuaState): Integer; static; cdecl;
    // class function LuaFloor(L: TLuaState): Integer; static; cdecl;
    // class function LuaFMod(L: TLuaState): Integer; static; cdecl;
    // class function LuaLog(L: TLuaState): Integer; static; cdecl;
    // class function LuaMax(L: TLuaState): Integer; static; cdecl;
    // class function LuaMin(L: TLuaState): Integer; static; cdecl;
    // class function LuaModF(L: TLuaState): Integer; static; cdecl;
    // class function LuaRad(L: TLuaState): Integer; static; cdecl;
    // class function LuaRandom(L: TLuaState): Integer; static; cdecl;
    // class function LuaRandomSeed(L: TLuaState): Integer; static; cdecl;
    // class function LuaSin(L: TLuaState): Integer; static; cdecl;
    // class function LuaSqrt(L: TLuaState): Integer; static; cdecl;
    // class function LuaTan(L: TLuaState): Integer; static; cdecl;
    // class function LuaToInteger(L: TLuaState): Integer; static; cdecl;
    // class function LuaType(L: TLuaState): Integer; static; cdecl;
    // class function LuaUlt(L: TLuaState): Integer; static; cdecl;

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
    if L.Top = 1 then
      Exit(L.Error('assertion failed'))
    else
    begin
      Result := L.Error(L.ToString(2));
    end;
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
    Add('remove', LuaRemove);
    Add('sort', LuaSort);
    Add('unpack', LuaUnpack);
  end;
end;

constructor TLuaLibTable.Create(AL: TLuaState);
begin
  inherited;
  FSorter := TLuaSorter.Create(Self);
end;

destructor TLuaLibTable.Destroy;
begin
  FSorter.Free;
  inherited;
end;

class function TLuaLibTable.LuaConcat(L: TLuaState): Integer;
var
  Lua: TLua;
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
      Exit(L.ErrorFmt('arg #2: pos %d out of bounds, allowed [1 - %d]', [Pos, Len + 1]));
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

class function TLuaLibTable.LuaRemove(L: TLuaState): Integer;
var
  Pos, Len, I: TLuaInteger;
begin
  L.CheckType(1, ltTable);
  L.CheckEnd(3);
  L.Len(1);
  Len := L.ToInteger;
  Pos := L.CheckOrDefault(2, Len);
  if Len = 0 then
  begin
    if not (Pos in [0 .. 1]) then
    begin
      Exit(L.ErrorFmt('arg #2: pos %d out of bounds, allowed [0 - 1]', [Pos]));
    end;
  end
  else
  begin
    if (Pos < 1) or (Pos > Len + 1) then
    begin
      Exit(L.ErrorFmt('arg #2: pos %d out of bounds, allowed [1 - %d]', [Pos, Len + 1]));
    end;
  end;
  L.GetI(Pos, 1);
  for I := Pos to Len do
  begin
    L.GetI(I + 1, 1);
    L.SetI(I, 1);
  end;
  Result := 1;
end;

class function TLuaLibTable.LuaSort(L: TLuaState): Integer;
var
  TableLib: TLuaLibTable;
begin
  TableLib := TLua.FromState(L).Lib<TLuaLibTable>;
  L.CheckType(1, ltTable);
  if L.Top > 1 then
  begin
    L.CheckType(2, ltFunction);
    L.CheckEnd(3);
  end
  else
  begin
    L.PushCFunction(LuaDefaultCompare);
  end;

  if not TableLib.FSorter.Sort then
    L.Error('invalid sort function');

  Result := 0;
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

class function TLuaLibTable.LuaDefaultCompare(L: TLuaState): Integer;
begin
  L.CheckEnd(3);
  L.PushBoolean(L.Compare(1, 2, lcoLessThan));
  Result := 1;
end;

{ TLuaLibTable.TLuaSorter }

function TLuaLibTable.TLuaSorter.GetL: TLuaState;
begin
  Result := FParent.L;
end;

function TLuaLibTable.TLuaSorter.Low: Integer;
begin
  Result := 1;
end;

procedure TLuaLibTable.TLuaSorter.DiscardPivot;
begin
  L.Pop;
end;

function TLuaLibTable.TLuaSorter.High: Integer;
begin
  L.Len(1);
  Result := L.ToInteger;
  L.Pop;
end;

function TLuaLibTable.TLuaSorter.CompareToPivot(I: Integer; ADir: TQuickSorter.TCompareDirection): Boolean;
begin
  L.PushValue(2); // function
  if ADir = cdBeforePivot then
  begin
    L.GetI(I, 1);   // tested element
    L.PushValue(3); // pivot element
  end
  else
  begin
    L.PushValue(3); // pivot element
    L.GetI(I, 1);   // tested element
  end;
  FLua.Unlock;
  L.Call(2, 1);
  FLua.Interlock;
  Result := L.ToBoolean;
  L.Pop;
end;

procedure TLuaLibTable.TLuaSorter.SavePivot(I: Integer);
begin
  L.GetI(I, 1);
end;

constructor TLuaLibTable.TLuaSorter.Create(AParent: TLuaLibTable);
begin
  FParent := AParent;
  FLua := TLua.FromState(L);
end;

procedure TLuaLibTable.TLuaSorter.Swap(A, B: Integer);
begin
  L.GetI(A, 1);
  L.GetI(B, 1);
  L.SetI(A, 1);
  L.SetI(B, 1);
end;

{ TLuaLibMath }

class procedure TLuaLibMath.CreateEntry(AEntry: TLuaLib.TTableEntry);
begin
  with AEntry.Add('math') do
  begin
    Add('abs', LuaNotImplemented);
    Add('acos', LuaNotImplemented);
    Add('asin', LuaNotImplemented);
    Add('atan', LuaNotImplemented);
    Add('ceil', LuaNotImplemented);
    Add('cos', LuaNotImplemented);
    Add('deg', LuaNotImplemented);
    Add('exp', LuaNotImplemented);
    Add('floor', LuaNotImplemented);
    Add('fmod', LuaNotImplemented);
    Add('huge', Infinity);
    Add('log', LuaNotImplemented);
    Add('max', LuaNotImplemented);
    Add('maxinteger', TLuaInteger.MaxValue);
    Add('min', LuaNotImplemented);
    Add('mininteger', TLuaInteger.MinValue);
    Add('modf', LuaNotImplemented);
    Add('pi', Pi);
    Add('rad', LuaNotImplemented);
    Add('random', LuaNotImplemented);
    Add('randomseed', LuaNotImplemented);
    Add('sin', LuaNotImplemented);
    Add('sqrt', LuaNotImplemented);
    Add('tan', LuaNotImplemented);
    Add('tointeger', LuaNotImplemented);
    Add('type', LuaNotImplemented);
    Add('ult', LuaNotImplemented);
  end;
end;

end.
