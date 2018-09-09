unit Pengine.LuaDefaultLibs;

interface

uses
  System.Math,
  System.AnsiStrings,
  System.SysUtils,

  Pengine.Lua,
  Pengine.LuaHeader,
  Pengine.Sorting,
  Pengine.IntMaths;

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
    class function LuaSelect(L: TLuaState): Integer; static; cdecl;
    class function LuaSetMetatable(L: TLuaState): Integer; static; cdecl;
    class function LuaToNumber(L: TLuaState): Integer; static; cdecl;
    class function LuaToString(L: TLuaState): Integer; static; cdecl;
    class function LuaType(L: TLuaState): Integer; static; cdecl;
    // TODO: pcall

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
      function Bounds: TIntBounds1; override;

      procedure SavePivot(I: Integer); override;
      procedure DiscardPivot; override;

      function BeforePivot(I: Integer): Boolean; override;
      function AfterPivot(I: Integer): Boolean; override;

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

    // custom functions
    class function LuaAggregate(L: TLuaState): Integer; static; cdecl;
    class function LuaCopy(L: TLuaState): Integer; static; cdecl;
    class function LuaCopyIf(L: TLuaState): Integer; static; cdecl;
    class function LuaZip(L: TLuaState): Integer; static; cdecl;

  protected
    class procedure CreateEntry(AEntry: TLuaLib.TTableEntry); override;

  public
    constructor Create(AL: TLuaState); override;
    destructor Destroy; override;

  end;

  { TLuaLibMath }

  TLuaLibMath = class(TLuaLib)
  private
    class function LuaAbs(L: TLuaState): Integer; static; cdecl;
    class function LuaArcCos(L: TLuaState): Integer; static; cdecl;
    class function LuaArcSin(L: TLuaState): Integer; static; cdecl;
    class function LuaArcTan(L: TLuaState): Integer; static; cdecl;
    class function LuaCeil(L: TLuaState): Integer; static; cdecl;
    class function LuaCos(L: TLuaState): Integer; static; cdecl;
    class function LuaDeg(L: TLuaState): Integer; static; cdecl;
    class function LuaExp(L: TLuaState): Integer; static; cdecl;
    class function LuaFloor(L: TLuaState): Integer; static; cdecl;
    class function LuaFMod(L: TLuaState): Integer; static; cdecl;
    class function LuaLog(L: TLuaState): Integer; static; cdecl;
    class function LuaMax(L: TLuaState): Integer; static; cdecl;
    class function LuaMin(L: TLuaState): Integer; static; cdecl;
    class function LuaModF(L: TLuaState): Integer; static; cdecl;
    class function LuaRad(L: TLuaState): Integer; static; cdecl;
    // class function LuaRandom(L: TLuaState): Integer; static; cdecl;
    // class function LuaRandomSeed(L: TLuaState): Integer; static; cdecl;
    class function LuaSin(L: TLuaState): Integer; static; cdecl;
    class function LuaSqrt(L: TLuaState): Integer; static; cdecl;
    class function LuaTan(L: TLuaState): Integer; static; cdecl;
    class function LuaToInteger(L: TLuaState): Integer; static; cdecl;
    class function LuaType(L: TLuaState): Integer; static; cdecl;
    class function LuaUlt(L: TLuaState): Integer; static; cdecl;

  protected
    class procedure CreateEntry(AEntry: TLuaLib.TTableEntry); override;

  end;

  { TLuaLibCoroutine }

  TLuaLibCoroutine = class(TLuaLib)
  private
    class function LuaCreate(L: TLuaState): Integer; static; cdecl;
    class function LuaIsYieldable(L: TLuaState): Integer; static; cdecl;
    class function LuaResume(L: TLuaState): Integer; static; cdecl;
    class function LuaRunning(L: TLuaState): Integer; static; cdecl;
    class function LuaStatus(L: TLuaState): Integer; static; cdecl;
    class function LuaWrap(L: TLuaState): Integer; static; cdecl;
    class function LuaYield(L: TLuaState): Integer; static; cdecl;

    class function LuaResumeHelper(L, C: TLuaState; AArgs: Integer): Integer;
    class function LuaWrapHelper(L: TLuaState): Integer; static; cdecl;

  protected
    class procedure CreateEntry(AEntry: TLuaLib.TTableEntry); override;

  end;

var
  Counter: Int64;

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
    Add('select', LuaSelect);
    Add('setmetatable', LuaSetMetatable);
    Add('tonumber', LuaToNumber);
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

class function TLuaLibBasic.LuaSelect(L: TLuaState): Integer;
var
  I: TLuaInteger;
begin
  if L.CheckType(1, [ltNumber, ltString]) = ltNumber then
  begin
    I := L.CheckInteger(1);
    if (I = 0) or (I <= -L.Top) then
      Exit(L.ErrorFmt('arg #1: index must not be zero or less than -%d', [L.Top - 1]));
    if I < 0 then
      Exit(-I);
    Exit(Max(L.Top - I, 0));
  end;
  if L.ToString[1] = '#' then
  begin
    L.PushInteger(L.Top - 1);
    Exit(1);
  end;
  Result := L.Error('string can only be "#" to get argument count');
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

class function TLuaLibBasic.LuaToNumber(L: TLuaState): Integer;
var
  S: AnsiString;
  INum, Base, AddBase, Add: TLuaInteger;
  I: Integer;
begin
  if L.Top = 1 then
  begin
    if L.&Type(1) = ltNumber then
      Exit(1);
    L.CheckType(1, [ltNumber, ltString]);
    if L.StringToNumber(L.ToString(1)) = 0 then
      L.PushNil;
    Exit(1);
  end;
  L.CheckType(1, ltString);
  Base := L.CheckInteger(2);
  L.CheckEnd(3);
  S := System.AnsiStrings.StrUpper(L.ToString(1));
  INum := 0;
  AddBase := 1;
  if not InRange(Base, 2, 36) then
    L.Error('bad argument #2 to ''tonumber'' (base out of range)');
  if Base > 10 then
  begin
    for I := Length(S) downto 1 do
    begin
      if (S[I] >= '0') and (S[I] <= '9') then
        Add := Ord(S[I]) - Ord('0')
      else if (S[I] >= 'A') and (Ord(S[I]) < Ord('A') + Base - 10) then
        Add := 10 + Ord(S[I]) - Ord('A')
      else
      begin
        L.PushNil;
        Exit(1);
      end;
      Inc(INum, AddBase * Add);
      AddBase := AddBase * Base;
    end;
  end
  else
  begin
    for I := Length(S) downto 1 do
    begin
      if (S[I] >= '0') and (Ord(S[I]) < Ord('0') + Base) then
        Add := Ord(S[I]) - Ord('0')
      else
      begin
        L.PushNil;
        Exit(1);
      end;
      Inc(INum, AddBase * Add);
      AddBase := AddBase * Base;
    end;
  end;
  L.PushInteger(INum);
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
    // custom
    Add('aggregate', LuaAggregate);
    Add('copy', LuaCopy);
    Add('copyif', LuaCopyIf);
    Add('zip', LuaZip);
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
  S: AnsiString;
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
  S := '';
  for I := A to B do
  begin
    L.GetI(I, 1);
    if not (L.&Type in [ltNumber, ltString]) then
      Exit(L.ErrorFmt('table element #%d: number or string expected, got %s', [I, L.TypeNameAt]));
    S := S + L.ToString_X;
    L.Pop;
    if I < B then
      S := S + L.ToString_X(2);
    Lua.CheckTimeout;
    Inc(Counter);
  end;
  if S = '' then
    L.PushLiteral('')
  else
    L.PushString(PPAnsiChar(@S)^);
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
    Lua.CheckTimeout;
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
    Lua.CheckTimeout;
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
    Lua.CheckTimeout;
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

  if not TableLib.FSorter.TrySort then
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
  if not L.CheckStack(B - A) then
    L.Error('too many results to unpack');
  for I := A to B do
  begin
    L.GetI(I, 1);
    Lua.CheckTimeout;
  end;
  Result := B - A + 1;
end;

class function TLuaLibTable.LuaDefaultCompare(L: TLuaState): Integer;
begin
  L.CheckEnd(3);
  L.PushBoolean(L.Compare(1, 2, lcoLessThan));
  Result := 1;
end;

class function TLuaLibTable.LuaAggregate(L: TLuaState): Integer;
var
  Lua: TLua;
  I, Len: TLuaInteger;
begin
  Lua := TLua.FromState(L);
  L.CheckType(1, ltTable);
  L.CheckType(2, ltFunction);
  L.CheckEnd(3);
  L.Len(1);
  Len := L.ToInteger;
  if Len = 0 then
  begin
    L.PushNil;
    Exit(1);
  end;
  L.GetI(1, 1);  
  for I := 2 to Len do
  begin          
    L.PushValue(2);
    L.Insert(4);
    L.GetI(I, 1);
    L.Call(2, 1);
    Lua.CheckTimeout;
  end;
  Result := 1;
end;
                          
class function TLuaLibTable.LuaCopy(L: TLuaState): Integer;
var
  A, B, I, J: TLuaInteger;
begin
  L.CheckType(1, ltTable);
  if L.Top <= 2 then
  begin                  
    L.Len(1); 
    A := 1;
    B := L.CheckOrDefault(2, L.ToInteger);
  end
  else
  begin
    L.CheckType(2, ltNumber);
    L.CheckType(3, ltNumber);
    L.CheckEnd(4);
    A := L.ToInteger(2);
    B := L.ToInteger(3);
  end;
  L.CreateTable(B - A + 1, 0); // 5
  J := 1;
  for I := A to B do
  begin
    L.GetI(I, 1);
    L.SetI(J, -2);
    Inc(J);
  end;
  Result := 1;
end;

class function TLuaLibTable.LuaCopyIf(L: TLuaState): Integer;
var
  A, B, I, J: Integer;
  Add: Boolean;
begin
  L.CheckType(1, ltTable);
  L.CheckType(2, ltFunction);
  if L.Top <= 3 then
  begin                  
    L.Len(1); 
    A := 1;
    B := L.CheckOrDefault(3, L.ToInteger);
  end
  else
  begin
    L.CheckType(3, ltNumber);
    L.CheckType(4, ltNumber);
    L.CheckEnd(5);
    A := L.ToInteger(3);
    B := L.ToInteger(4);
  end;
  L.CreateTable(B - A + 1, 0); // 5
  J := 1;
  for I := A to B do
  begin            
    L.GetI(I, 1); // 6
    L.PushValue(2); // function
    L.PushValue(6); // value
    L.Call(1, 1);
    Add := L.ToBoolean;
    L.Pop;
    if Add then
    begin
      L.SetI(J, 5);
      Inc(J);
    end
    else
      L.Pop;
  end;
  Result := 1;
end;

class function TLuaLibTable.LuaZip(L: TLuaState): Integer;
var
  Lua: TLua;
  LenA, LenB, Len: TLuaInteger;
  I: TLuaInteger;
begin
  Lua := TLua.FromState(L);
  L.CheckType(1, ltTable);
  L.CheckType(2, ltTable);
  L.CheckType(3, ltFunction);
  L.CheckEnd(4);
  L.Len(1); // stack 4
  LenA := L.ToInteger;
  L.Len(2); // stack 5
  LenB := L.ToInteger;
  Len := Max(LenA, LenB);
  L.CreateTable(Len, 0); // stack 6
  for I := 1 to Len do
  begin  
    L.PushValue(3);
    L.GetI(I, 1);
    L.GetI(I, 2);
    L.Call(2, 1);
    L.SetI(I, 6);
    Lua.CheckTimeout;
  end;
  Result := 1; 
end;

{ TLuaLibTable.TLuaSorter }

function TLuaLibTable.TLuaSorter.GetL: TLuaState;
begin
  Result := FParent.L;
end;

function TLuaLibTable.TLuaSorter.Bounds: TIntBounds1;
begin
  L.Len(1);
  Result := IBounds1(1, L.ToInteger);
  L.Pop;
end;

procedure TLuaLibTable.TLuaSorter.DiscardPivot;
begin
  L.Pop;
end;

function TLuaLibTable.TLuaSorter.BeforePivot(I: Integer): Boolean;
begin
  L.PushValue(2); // function
  L.GetI(I, 1);   // tested element
  L.PushValue(3); // pivot element
  L.Call(2, 1);
  Result := L.ToBoolean;
  L.Pop;
end;

function TLuaLibTable.TLuaSorter.AfterPivot(I: Integer): Boolean;
begin
  L.PushValue(2); // function
  L.PushValue(3); // pivot element
  L.GetI(I, 1);   // tested element
  L.Call(2, 1);
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
    Add('abs', LuaAbs);
    Add('acos', LuaArcCos);
    Add('asin', LuaArcSin);
    Add('atan', LuaArcTan);
    Add('ceil', LuaCeil);
    Add('cos', LuaCos);
    Add('deg', LuaDeg);
    Add('exp', LuaExp);
    Add('floor', LuaFloor);
    Add('fmod', LuaFMod);
    Add('huge', Infinity);
    Add('log', LuaLog);
    Add('max', LuaMax);
    Add('maxinteger', TLuaInteger.MaxValue);
    Add('min', LuaMin);
    Add('mininteger', TLuaInteger.MinValue);
    Add('modf', LuaModF);
    Add('pi', Pi);
    Add('rad', LuaRad);
    Add('random', LuaNotImplemented);
    Add('randomseed', LuaNotImplemented);
    Add('sin', LuaSin);
    Add('sqrt', LuaSqrt);
    Add('tan', LuaTan);
    Add('tointeger', LuaToInteger);
    Add('type', LuaType);
    Add('ult', LuaUlt);
  end;
end;

class function TLuaLibMath.LuaAbs(L: TLuaState): Integer;
var
  IsInt: LongBool;
  X: TLuaInteger;
begin
  L.CheckType(1, ltNumber);
  L.CheckEnd(2);
  X := L.ToIntegerX(@IsInt, 1);
  if IsInt then
    L.PushInteger(Abs(X))
  else
    L.PushNumber(Abs(L.ToNumber(1)));
  Result := 1;
end;

class function TLuaLibMath.LuaArcCos(L: TLuaState): Integer;
begin
  L.CheckType(1, ltNumber);
  L.CheckEnd(2);
  L.PushNumber(ArcCos(L.ToNumber(1)));
  Result := 1;
end;

class function TLuaLibMath.LuaArcSin(L: TLuaState): Integer;
begin
  L.CheckType(1, ltNumber);
  L.CheckEnd(2);
  L.PushNumber(ArcSin(L.ToNumber(1)));
  Result := 1;
end;

class function TLuaLibMath.LuaArcTan(L: TLuaState): Integer;
var
  X, Y: TLuaNumber;
begin
  L.CheckType(1, ltNumber);
  Y := L.CheckOrDefault(2, 1.0);
  L.CheckEnd(3);
  X := L.ToNumber(1);
  if Y = 0 then
  begin
    if X > 0 then
      L.PushNumber(Pi / 2)
    else if X = 0 then
      L.PushNumber(NaN)
    else
      L.PushNumber(-Pi / 2);
  end
  else
  begin
    L.PushNumber(ArcTan(X / Y));
  end;
  Result := 1;
end;

class function TLuaLibMath.LuaCeil(L: TLuaState): Integer;
var
  X: TLuaNumber;
begin
  L.CheckType(1, ltNumber);
  L.CheckEnd(2);
  X := L.ToNumber(1);
  if X <= TLuaNumber.MinValue - 1 then
    Exit(L.ErrorFmt('%f is too small to get ceiled', [X]));
  if X > TLuaNumber.MaxValue then
    Exit(L.ErrorFmt('%f is too big to get ceiled', [X]));
  L.PushInteger(Ceil(L.ToNumber(1)));
  Result := 1;
end;

class function TLuaLibMath.LuaCos(L: TLuaState): Integer;
begin
  L.CheckType(1, ltNumber);
  L.CheckEnd(2);
  L.PushNumber(Cos(L.ToNumber(1)));
  Result := 1;
end;

class function TLuaLibMath.LuaDeg(L: TLuaState): Integer;
begin
  L.CheckType(1, ltNumber);
  L.CheckEnd(2);
  L.PushNumber(RadToDeg(L.ToNumber(1)));
  Result := 1;
end;

class function TLuaLibMath.LuaExp(L: TLuaState): Integer;
begin
  L.CheckType(1, ltNumber);
  L.CheckEnd(2);
  L.PushNumber(Exp(L.ToNumber(1)));
  Result := 1;
end;

class function TLuaLibMath.LuaFloor(L: TLuaState): Integer;
var
  X: TLuaNumber;
begin
  L.CheckType(1, ltNumber);
  L.CheckEnd(2);
  X := L.ToNumber(1);
  if X <= TLuaNumber.MinValue then
    Exit(L.ErrorFmt('%f is too small to get floored', [X]));
  if X > TLuaNumber.MaxValue + 1 then
    Exit(L.ErrorFmt('%f is too big to get floored', [X]));
  L.PushInteger(Ceil(L.ToNumber(1)));
  Result := 1;
end;

class function TLuaLibMath.LuaFMod(L: TLuaState): Integer;
var
  X, Y: TLuaInteger;
  XIsInt, YIsInt: LongBool;
begin
  L.CheckType(1, ltNumber);
  L.CheckType(2, ltNumber);
  L.CheckEnd(3);
  X := L.ToIntegerX(@XIsInt, 1);
  if XIsInt then
  begin
    Y := L.ToIntegerX(@YIsInt, 2);
    if YIsInt then
    begin
      L.PushInteger(X mod Y);
      Exit(1);
    end;
  end;
  L.PushNumber(FMod(L.ToNumber(1), L.ToNumber(2)));
  Result := 1;
end;

class function TLuaLibMath.LuaLog(L: TLuaState): Integer;
begin
  L.CheckType(1, ltNumber);
  if L.Top = 1 then // arg 2 is e, use ln instead of log
  begin
    L.PushNumber(Ln(L.ToNumber(1)));
  end
  else
  begin
    L.CheckType(2, ltNumber);
    L.CheckEnd(3);
    L.PushNumber(LogN(L.ToNumber(2), L.ToNumber(1))); // note that base and X are swapped
  end;
  Result := 1;
end;

class function TLuaLibMath.LuaMax(L: TLuaState): Integer;
var
  Lua: TLua;
  I: TLuaInteger;
  MaxIndex: TLuaInteger;
begin
  Lua := TLua.FromState(L);
  L.CheckAny(1);
  MaxIndex := 1;
  for I := 2 to L.Top do
  begin
    if L.Compare(MaxIndex, I, lcoLessThan) then
      MaxIndex := I;
    Lua.CheckTimeout;
  end;
  L.PushValue(MaxIndex);
  Result := 1;
end;

class function TLuaLibMath.LuaMin(L: TLuaState): Integer;
var
  Lua: TLua;
  I: TLuaInteger;
  MinIndex: TLuaInteger;
begin
  Lua := TLua.FromState(L);
  L.CheckAny(1);
  MinIndex := 1;
  for I := 2 to L.Top do
  begin
    if L.Compare(I, MinIndex, lcoLessThan) then
      MinIndex := I;
    Lua.CheckTimeout;
  end;
  L.PushValue(MinIndex);
  Result := 1;
end;

class function TLuaLibMath.LuaModF(L: TLuaState): Integer;
var
  X: lua_Number;
  IsInt: LongBool;
  IntPart: TLuaInteger;
begin
  L.CheckType(1, ltNumber);
  L.CheckEnd(2);
  X := L.ToNumber(1);
  L.PushNumber(Int(X));
  IntPart := L.ToIntegerX(@IsInt, 2);
  if IsInt then
    L.PushInteger(IntPart);
  L.PushNumber(Frac(X));
  Result := 2;
end;

class function TLuaLibMath.LuaRad(L: TLuaState): Integer;
begin
  L.CheckType(1, ltNumber);
  L.CheckEnd(2);
  L.PushNumber(DegToRad(L.ToNumber(1)));
  Result := 1;
end;

class function TLuaLibMath.LuaSin(L: TLuaState): Integer;
begin
  L.CheckType(1, ltNumber);
  L.CheckEnd(2);
  L.PushNumber(Sin(L.ToNumber(1)));
  Result := 1;
end;

class function TLuaLibMath.LuaSqrt(L: TLuaState): Integer;
begin
  L.CheckType(1, ltNumber);
  L.CheckEnd(2);
  L.PushNumber(Sqrt(L.ToNumber(1)));
  Result := 1;
end;

class function TLuaLibMath.LuaTan(L: TLuaState): Integer;
begin
  L.CheckType(1, ltNumber);
  L.CheckEnd(2);
  L.PushNumber(Tan(L.ToNumber(1)));
  Result := 1;
end;

class function TLuaLibMath.LuaToInteger(L: TLuaState): Integer;
var
  IsInt: LongBool;
  I: TLuaInteger;
begin
  L.CheckType(1, ltNumber);
  L.CheckEnd(2);
  I := L.ToIntegerX(@IsInt, 1);
  if IsInt then
    L.PushInteger(I)
  else
    L.PushNil;
  Result := 1;
end;

class function TLuaLibMath.LuaType(L: TLuaState): Integer;
begin
  L.CheckAny(1);
  L.CheckEnd(2);
  if L.IsInteger(1) then
    L.PushString('integer')
  else if L.IsNumber(1) then
    L.PushString('float')
  else
    L.PushNil;
  Result := 1;
end;

class function TLuaLibMath.LuaUlt(L: TLuaState): Integer;
begin
  L.CheckType(1, ltNumber);
  L.CheckType(2, ltNumber);
  L.CheckEnd(3);
  L.PushBoolean(TLuaUnsigned(L.CheckInteger(1)) < TLuaUnsigned(L.CheckInteger(2)));
  Result := 1;
end;

{ TLuaLibCoroutine }

class procedure TLuaLibCoroutine.CreateEntry(AEntry: TLuaLib.TTableEntry);
begin
  with AEntry.Add('coroutine') do
  begin
    Add('create', LuaCreate);
    Add('isyieldable', LuaIsYieldable);
    Add('resume', LuaResume);
    Add('running', LuaRunning);
    Add('status', LuaStatus);
    Add('wrap', LuaWrap);
    Add('yield', LuaYield);
  end;
end;

class function TLuaLibCoroutine.LuaCreate(L: TLuaState): Integer;
var
  Thread: TLuaState;
begin
  L.CheckType(1, ltFunction);
  L.CheckEnd(2);
  Thread := L.NewThread;
  L.PushValue(1);
  L.XMove(Thread, 1);
  Result := 1;
end;

class function TLuaLibCoroutine.LuaIsYieldable(L: TLuaState): Integer;
begin
  L.CheckEnd(1);
  L.PushBoolean(L.IsYieldable);
  Result := 1;
end;

class function TLuaLibCoroutine.LuaResume(L: TLuaState): Integer;
var
  R: Integer;
begin
  L.CheckType(1, ltThread);
  R := LuaResumeHelper(L, L.ToThread(1), L.Top - 1);
  if R < 0 then
  begin
    L.PushBoolean(False);
    L.Insert(-2);
    Result := 2;
  end
  else
  begin
    L.PushBoolean(True);
    L.Insert(-R - 1);
    Result := R + 1;
  end;
end;

class function TLuaLibCoroutine.LuaRunning(L: TLuaState): Integer;
begin
  L.CheckEnd(1);
  L.PushBoolean(L.PushThread);
  Result := 2;
end;

class function TLuaLibCoroutine.LuaStatus(L: TLuaState): Integer;
var
  C: TLuaState;
  AR: TLuaDebug;
begin
  L.CheckType(1, ltThread);
  L.CheckEnd(2);
  C := L.ToThread(1);

  if L = C then
    L.PushLiteral('running')
  else
  begin
    case C.Status of
      lstYield:
        L.PushLiteral('suspended');
      lstOk:
        begin
          if C.GetStack(0, @AR) then
            L.PushLiteral('normal')
          else if C.Top = 0 then
            L.PushLiteral('dead')
          else
            L.PushLiteral('suspended');
        end;
    else
      L.PushLiteral('dead');
    end;
  end;
  Result := 1;
end;

class function TLuaLibCoroutine.LuaWrap(L: TLuaState): Integer;
begin
  LuaCreate(L);
  L.PushCClosure(LuaWrapHelper, 1);
  Result := 1;
end;

class function TLuaLibCoroutine.LuaYield(L: TLuaState): Integer;
begin
  Result := L.Yield(L.Top);
end;

class function TLuaLibCoroutine.LuaResumeHelper(L, C: TLuaState; AArgs: Integer): Integer;
var
  Status: TLuaStatus;
  ResultCount: Integer;
begin
  if not C.CheckStack(AArgs) then
  begin
    L.PushString('too many arguments to resume');
    Exit(-1);
  end;
  if (C.Status = lstOk) and (L.Top = 0) then
  begin
    L.PushString('cannot resume dead coroutine');
    Exit(-1);
  end;
  L.XMove(C, AArgs);
  Status := C.Resume(L, AArgs);
  if Status in [lstOk, lstYield] then
  begin
    ResultCount := C.Top;
    if not L.CheckStack(ResultCount + 1) then
    begin
      C.Pop(ResultCount);
      C.PushLiteral('too many results to resume');
      Exit(-1);
    end;
    C.XMove(L, ResultCount);
    Exit(ResultCount);
  end;
  C.XMove(L, 1);
  Result := -1;
end;

class function TLuaLibCoroutine.LuaWrapHelper(L: TLuaState): Integer;
var
  C: TLuaState;
  R: Integer;
begin
  C := L.ToThread(L.UpvalueIndex(1));
  R := LuaResumeHelper(L, C, L.Top);
  if R < 0 then
  begin
    if L.&Type = ltString then
    begin
      L.Where(1);
      L.Insert(-2);
      L.Concat(2);
    end;
    Exit(L.Error_X);
  end;
  Result := R;
end;

end.
