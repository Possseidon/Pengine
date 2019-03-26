unit Pengine.LuaDefaultLibs;

{$M+}

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

  TLuaLibBasic = class(TLuaLib)
  protected
    class procedure CreateEntry(AEntry: TLuaLib.TTableEntry); override;

  published
    class function Lua_assert(L: TLuaState): Integer; static; cdecl;
    class function Lua_error(L: TLuaState): Integer; static; cdecl;
    class function Lua_getmetatable(L: TLuaState): Integer; static; cdecl;
    class function Lua_ipairs(L: TLuaState): Integer; static; cdecl;
    class function Lua_next(L: TLuaState): Integer; static; cdecl;
    class function Lua_pairs(L: TLuaState): Integer; static; cdecl;
    class function Lua_select(L: TLuaState): Integer; static; cdecl;
    class function Lua_setmetatable(L: TLuaState): Integer; static; cdecl;
    class function Lua_tonumber(L: TLuaState): Integer; static; cdecl;
    class function Lua_tostring(L: TLuaState): Integer; reintroduce; static; cdecl;
    class function Lua_type(L: TLuaState): Integer; static; cdecl;
    // TODO: pcall

    class function inext(L: TLuaState): Integer; static; cdecl;

  end;

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

    class function LuaDefaultCompare(L: TLuaState): Integer; static; cdecl;

  protected
    class procedure CreateEntry(AEntry: TLuaLib.TTableEntry); override;

  public
    constructor Create(AL: TLuaState); override;
    destructor Destroy; override;

  published
    class function Lua_concat(L: TLuaState): Integer; static; cdecl;
    class function Lua_insert(L: TLuaState): Integer; static; cdecl;
    class function Lua_move(L: TLuaState): Integer; static; cdecl;
    class function Lua_pack(L: TLuaState): Integer; static; cdecl;
    class function Lua_remove(L: TLuaState): Integer; static; cdecl;
    class function Lua_sort(L: TLuaState): Integer; static; cdecl;
    class function Lua_unpack(L: TLuaState): Integer; static; cdecl;

    // custom functions
    class function Lua_aggregate(L: TLuaState): Integer; static; cdecl;
    class function Lua_copy(L: TLuaState): Integer; static; cdecl;
    class function Lua_copyif(L: TLuaState): Integer; static; cdecl;
    class function Lua_zip(L: TLuaState): Integer; static; cdecl;

  end;

  TLuaLibMath = class(TLuaLib)
  protected
    class procedure CreateEntry(AEntry: TLuaLib.TTableEntry); override;

  published
    class function Lua_abs(L: TLuaState): Integer; static; cdecl;
    class function Lua_arccos(L: TLuaState): Integer; static; cdecl;
    class function Lua_arcsin(L: TLuaState): Integer; static; cdecl;
    class function Lua_arctan(L: TLuaState): Integer; static; cdecl;
    class function Lua_ceil(L: TLuaState): Integer; static; cdecl;
    class function Lua_cos(L: TLuaState): Integer; static; cdecl;
    class function Lua_deg(L: TLuaState): Integer; static; cdecl;
    class function Lua_exp(L: TLuaState): Integer; static; cdecl;
    class function Lua_floor(L: TLuaState): Integer; static; cdecl;
    class function Lua_fmod(L: TLuaState): Integer; static; cdecl;
    class function Lua_log(L: TLuaState): Integer; static; cdecl;
    class function Lua_max(L: TLuaState): Integer; static; cdecl;
    class function Lua_min(L: TLuaState): Integer; static; cdecl;
    class function Lua_modf(L: TLuaState): Integer; static; cdecl;
    class function Lua_rad(L: TLuaState): Integer; static; cdecl;
    // class function Lua_random(L: TLuaState): Integer; static; cdecl;
    // class function Lua_randomseed(L: TLuaState): Integer; static; cdecl;
    class function Lua_sin(L: TLuaState): Integer; static; cdecl;
    class function Lua_sqrt(L: TLuaState): Integer; static; cdecl;
    class function Lua_tan(L: TLuaState): Integer; static; cdecl;
    class function Lua_tointeger(L: TLuaState): Integer; static; cdecl;
    class function Lua_type(L: TLuaState): Integer; static; cdecl;
    class function Lua_ult(L: TLuaState): Integer; static; cdecl;

  end;

  TLuaLibCoroutine = class(TLuaLib)
  private
    class function LuaResumeHelper(L, C: TLuaState; AArgs: Integer): Integer;
    class function LuaWrapHelper(L: TLuaState): Integer; static; cdecl;

  protected
    class procedure CreateEntry(AEntry: TLuaLib.TTableEntry); override;

  published
    class function Lua_create(L: TLuaState): Integer; static; cdecl;
    class function Lua_isyieldable(L: TLuaState): Integer; static; cdecl;
    class function Lua_resume(L: TLuaState): Integer; static; cdecl;
    class function Lua_running(L: TLuaState): Integer; static; cdecl;
    class function Lua_status(L: TLuaState): Integer; static; cdecl;
    class function Lua_wrap(L: TLuaState): Integer; static; cdecl;
    class function Lua_yield(L: TLuaState): Integer; static; cdecl;

  end;

  // TODO: string

implementation

{ TLuaLibBasic }

class procedure TLuaLibBasic.CreateEntry(AEntry: TLuaLib.TTableEntry);
begin
  with AEntry do
  begin
    AddPublished(Self);
    AddRecursion('_G', AEntry);
  end;
end;

class function TLuaLibBasic.Lua_assert(L: TLuaState): Integer;
begin
  L.CheckArgAny(1);
  if L.ToBoolean(1) then
  begin
    Result := L.Top;
  end
  else
  begin
    if L.Top = 1 then
      Exit(L.error('assertion failed'))
    else
    begin
      Result := L.error(L.tostring(2));
    end;
  end;
end;

class function TLuaLibBasic.Lua_error(L: TLuaState): Integer;
var
  Level: Integer;
begin
  if L.IsNone(1) then
  begin
    Level := 1;
    L.PushNil;
  end
  else
    Level := L.CheckArgOrDefault(2, 1);
  Result := L.error(L.tostring(1), Level);
end;

class function TLuaLibBasic.Lua_getmetatable(L: TLuaState): Integer;
begin
  L.CheckArgAny(1);
  if L.getmetatable(1) then
  begin
    if L.GetField('__metatable', 2) = ltNil then
      L.Top := 2;
  end
  else
    L.PushNil;
  Result := 1;
end;

class function TLuaLibBasic.inext(L: TLuaState): Integer;
var
  I: TLuaInteger;
begin
  L.CheckArg(1, ltTable);
  L.Top := 2;
  I := L.CheckArgOrDefault(2, 0) + 1;
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

class function TLuaLibBasic.Lua_ipairs(L: TLuaState): Integer;
begin
  L.CheckArg(1, ltTable);
  L.PushCFunction(inext);
  L.insert(1);
  L.PushInteger(0);
  Result := 3;
end;

class function TLuaLibBasic.Lua_next(L: TLuaState): Integer;
begin
  L.CheckArg(1, ltTable);
  L.Top := 2;
  if L.next(1) then
    Result := 2
  else
  begin
    L.PushNil;
    Result := 1;
  end;
end;

class function TLuaLibBasic.Lua_pairs(L: TLuaState): Integer;
begin
  L.CheckArg(1, ltTable);
  L.PushCFunction(Lua_next);
  L.insert(1);
  Result := 2;
end;

class function TLuaLibBasic.Lua_select(L: TLuaState): Integer;
var
  I: TLuaInteger;
begin
  if L.CheckArg(1, [ltNumber, ltString]) = ltNumber then
  begin
    I := L.CheckArgInteger(1);
    if (I = 0) or (I <= -L.Top) then
      Exit(L.ErrorFmt('arg #1: index must not be zero or less than -%d', [L.Top - 1]));
    if I < 0 then
      Exit(-I);
    Exit(max(L.Top - I, 0));
  end;
  if L.ToString(1)[0] = '#' then
  begin
    L.PushInteger(L.Top - 1);
    Exit(1);
  end;
  Result := L.error('string can only be "#" to get argument count');
end;

class function TLuaLibBasic.Lua_setmetatable(L: TLuaState): Integer;
begin
  L.CheckArg(1, ltTable);
  L.CheckArg(2, [ltTable, ltNil]);
  if L.getmetatable(1) then
  begin
    if L.GetField('__metatable', 3) <> ltNil then
      L.error('cannot change a protected metatable');
    L.Top := 2;
  end;
  L.setmetatable(1);
  Result := 1;
end;

class function TLuaLibBasic.Lua_tonumber(L: TLuaState): Integer;
var
  S: AnsiString;
  INum, Base, AddBase, Add: TLuaInteger;
  I: Integer;
begin
  if L.Top = 1 then
  begin
    if L.&type(1) = ltNumber then
      Exit(1);
    L.CheckArg(1, [ltNumber, ltString]);
    if L.StringToNumber(L.tostring(1)) = 0 then
      L.PushNil;
    Exit(1);
  end;
  L.CheckArg(1, ltString);
  Base := L.CheckArgInteger(2);
  S := System.AnsiStrings.StrUpper(L.tostring(1));
  INum := 0;
  AddBase := 1;
  if not InRange(Base, 2, 36) then
    L.error('bad argument #2 to ''tonumber'' (base out of range)');
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

class function TLuaLibBasic.Lua_tostring(L: TLuaState): Integer;
begin
  L.CheckArgAny(1);
  L.tostring;
  Result := 1;
end;

class function TLuaLibBasic.Lua_type(L: TLuaState): Integer;
var
  T: TLuaType;
begin
  T := L.CheckArgAny(1);
  L.PushString(L.TypeName(T));
  Result := 1;
end;

{ TLuaLibTable }

class procedure TLuaLibTable.CreateEntry(AEntry: TLuaLib.TTableEntry);
begin
  AEntry.Add('table').AddPublished(Self);
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

class function TLuaLibTable.Lua_concat(L: TLuaState): Integer;
var
  Lua: TLua;
  A, B, I: TLuaInteger;
  S: AnsiString;
begin
  Lua := TLua.FromState(L);
  L.CheckArg(1, ltTable);
  L.Top := 4;
  L.Len(1);
  if L.IsNoneOrNil(2) then
  begin
    L.PushString('');
    L.insert(2);
  end
  else
    L.CheckArg(2, ltString);
  A := L.CheckArgOrDefault(3, 1);
  B := L.CheckArgOrDefault(4, L.tointeger);
  S := '';
  for I := A to B do
  begin
    L.GetI(I, 1);
    if not(L.&type in [ltNumber, ltString]) then
      Exit(L.ErrorFmt('table element #%d: number or string expected, got %s', [I, L.TypeNameAt]));
    S := S + L.ToString_X;
    L.Pop;
    if I < B then
      S := S + L.ToString_X(2);
    Lua.CheckTimeout;
  end;
  if S = '' then
    L.PushLiteral('')
  else
    L.PushString(PPAnsiChar(@S)^);
  Result := 1;
end;

class function TLuaLibTable.Lua_insert(L: TLuaState): Integer;
var
  Lua: TLua;
  Pos, Len, I: TLuaInteger;
begin
  Lua := TLua.FromState(L);
  L.CheckArg(1, ltTable);
  L.Len(1);
  Len := L.tointeger;
  L.Pop;
  if L.Top <= 2 then
  begin
    Pos := Len + 1;
    L.Top := 2;
  end
  else
  begin
    L.CheckArg(2, ltNumber);
    Pos := L.CheckArgInteger(2);
    if (Pos < 1) or (Pos > Len + 1) then
      Exit(L.ErrorFmt('arg #2: pos %d out of bounds, allowed [1 - %d]', [Pos, Len + 1]));
  end;
  L.CheckArgAny(L.Top);
  for I := Len downto Pos do
  begin
    L.GetI(I, 1);
    L.SetI(I + 1, 1);
    Lua.CheckTimeout;
  end;
  L.SetI(Pos, 1);
  Result := 0;
end;

class function TLuaLibTable.Lua_move(L: TLuaState): Integer;
var
  Lua: TLua;
  F, E, T, I: TLuaInteger;
begin
  Lua := TLua.FromState(L);
  L.CheckArg(1, ltTable);
  L.CheckArg(2, ltNumber);
  F := L.CheckArgInteger(2);
  L.CheckArg(3, ltNumber);
  E := L.CheckArgInteger(3);
  L.CheckArg(4, ltNumber);
  T := L.CheckArgInteger(4);
  if L.Top = 4 then
    L.PushValue(1)
  else
  begin
    L.CheckArg(5, ltTable);
  end;
  for I := 0 to E - F do
  begin
    L.GetI(F + I, 1);
    L.SetI(T + I, 5);
    Lua.CheckTimeout;
  end;
  Result := 1;
end;

class function TLuaLibTable.Lua_pack(L: TLuaState): Integer;
var
  Lua: TLua;
  I: TLuaInteger;
begin
  Lua := TLua.FromState(L);
  L.CreateTable(L.Top, 0);
  L.insert(1);
  for I := L.Top - 1 downto 1 do
  begin
    L.SetI(I, 1);
    Lua.CheckTimeout;
  end;
  Result := 1;
end;

class function TLuaLibTable.Lua_remove(L: TLuaState): Integer;
var
  Pos, Len, I: TLuaInteger;
begin
  L.CheckArg(1, ltTable);
  L.Len(1);
  Len := L.tointeger;
  Pos := L.CheckArgOrDefault(2, Len);
  if Len = 0 then
  begin
    if not(Pos in [0 .. 1]) then
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

class function TLuaLibTable.Lua_sort(L: TLuaState): Integer;
var
  TableLib: TLuaLibTable;
begin
  TableLib := TLua.FromState(L).Lib<TLuaLibTable>;
  L.CheckArg(1, ltTable);
  if L.Top > 1 then
  begin
    L.CheckArg(2, ltFunction);
  end
  else
  begin
    L.PushCFunction(LuaDefaultCompare);
  end;

  if not TableLib.FSorter.TrySort then
    L.error('invalid sort function');

  Result := 0;
end;

class function TLuaLibTable.Lua_unpack(L: TLuaState): Integer;
var
  Lua: TLua;
  A, B, I: TLuaInteger;
begin
  Lua := TLua.FromState(L);
  L.CheckArg(1, ltTable);
  L.Top := 3;
  L.Len(1);
  A := L.CheckArgOrDefault(2, 1);
  B := L.CheckArgOrDefault(3, L.tointeger);
  if not L.CheckStack(B - A) then
    L.error('too many results to unpack');
  for I := A to B do
  begin
    L.GetI(I, 1);
    Lua.CheckTimeout;
  end;
  Result := B - A + 1;
end;

class function TLuaLibTable.LuaDefaultCompare(L: TLuaState): Integer;
begin
  L.PushBoolean(L.Compare(1, 2, lcoLessThan));
  Result := 1;
end;

class function TLuaLibTable.Lua_aggregate(L: TLuaState): Integer;
var
  Lua: TLua;
  I, Len: TLuaInteger;
begin
  Lua := TLua.FromState(L);
  L.CheckArg(1, ltTable);
  L.CheckArg(2, ltFunction);
  L.Len(1);
  Len := L.tointeger;
  if Len = 0 then
  begin
    L.PushNil;
    Exit(1);
  end;
  L.GetI(1, 1);
  for I := 2 to Len do
  begin
    L.PushValue(2);
    L.insert(4);
    L.GetI(I, 1);
    L.Call(2, 1);
    Lua.CheckTimeout;
  end;
  Result := 1;
end;

class function TLuaLibTable.Lua_copy(L: TLuaState): Integer;
var
  A, B, I, J: TLuaInteger;
begin
  L.CheckArg(1, ltTable);
  if L.Top <= 2 then
  begin
    L.Len(1);
    A := 1;
    B := L.CheckArgOrDefault(2, L.tointeger);
  end
  else
  begin
    L.CheckArg(2, ltNumber);
    L.CheckArg(3, ltNumber);
    A := L.tointeger(2);
    B := L.tointeger(3);
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

class function TLuaLibTable.Lua_copyif(L: TLuaState): Integer;
var
  A, B, I, J: Integer;
  Add: Boolean;
begin
  L.CheckArg(1, ltTable);
  L.CheckArg(2, ltFunction);
  if L.Top <= 3 then
  begin
    L.Len(1);
    A := 1;
    B := L.CheckArgOrDefault(3, L.tointeger);
  end
  else
  begin
    L.CheckArg(3, ltNumber);
    L.CheckArg(4, ltNumber);
    A := L.tointeger(3);
    B := L.tointeger(4);
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

class function TLuaLibTable.Lua_zip(L: TLuaState): Integer;
var
  Lua: TLua;
  LenA, LenB, Len: TLuaInteger;
  I: TLuaInteger;
begin
  Lua := TLua.FromState(L);
  L.CheckArg(1, ltTable);
  L.CheckArg(2, ltTable);
  L.CheckArg(3, ltFunction);
  L.Len(1); // stack 4
  LenA := L.tointeger;
  L.Len(2); // stack 5
  LenB := L.tointeger;
  Len := max(LenA, LenB);
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
  Result := IBounds1(1, L.tointeger);
  L.Pop;
end;

procedure TLuaLibTable.TLuaSorter.DiscardPivot;
begin
  L.Pop;
end;

function TLuaLibTable.TLuaSorter.BeforePivot(I: Integer): Boolean;
begin
  L.PushValue(2); // function
  L.GetI(I, 1); // tested element
  L.PushValue(3); // pivot element
  L.Call(2, 1);
  Result := L.ToBoolean;
  L.Pop;
end;

function TLuaLibTable.TLuaSorter.AfterPivot(I: Integer): Boolean;
begin
  L.PushValue(2); // function
  L.PushValue(3); // pivot element
  L.GetI(I, 1); // tested element
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
    AddPublished(Self);
    Add('huge', Infinity);
    Add('maxinteger', TLuaInteger.MaxValue);
    Add('mininteger', TLuaInteger.MinValue);
    Add('pi', Pi);
    Add('random', LuaNotImplemented);
    Add('randomseed', LuaNotImplemented);
  end;
end;

class function TLuaLibMath.Lua_abs(L: TLuaState): Integer;
var
  F: TLuaNumber;
  I: TLuaInteger;
  IsInt: LongBool;
begin
  F := L.CheckArgNumber(1);
  I := L.ToIntegerX(@IsInt, 1);
  if IsInt then
    L.PushInteger(Abs(I))
  else
    L.PushNumber(Abs(F));
  Result := 1;
end;

class function TLuaLibMath.Lua_arccos(L: TLuaState): Integer;
begin
  L.PushNumber(ArcCos(L.CheckArgNumber(1)));
  Result := 1;
end;

class function TLuaLibMath.Lua_arcsin(L: TLuaState): Integer;
begin
  L.PushNumber(ArcSin(L.CheckArgNumber(1)));
  Result := 1;
end;

class function TLuaLibMath.Lua_arctan(L: TLuaState): Integer;
var
  X, Y: TLuaNumber;
begin
  X := L.CheckArgNumber(1);
  Y := L.CheckArgOrDefault(2, 1.0);
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

class function TLuaLibMath.Lua_ceil(L: TLuaState): Integer;
var
  X: TLuaNumber;
begin
  X := L.CheckArgNumber(1);
  {
    if X <= TLuaNumber.MinValue - 1 then
    Exit(L.ErrorFmt('%f is too small to get ceiled', [X]));
    if X > TLuaNumber.MaxValue then
    Exit(L.ErrorFmt('%f is too big to get ceiled', [X]));
  }
  // Big number already "rounded"
  if (X > TLuaNumber.MinValue - 1) and (X <= TLuaNumber.MaxValue) then
    L.PushInteger(Ceil(X));
  Result := 1;
end;

class function TLuaLibMath.Lua_cos(L: TLuaState): Integer;
begin
  L.PushNumber(Cos(L.CheckArgNumber(1)));
  Result := 1;
end;

class function TLuaLibMath.Lua_deg(L: TLuaState): Integer;
begin
  L.PushNumber(RadToDeg(L.CheckArgNumber(1)));
  Result := 1;
end;

class function TLuaLibMath.Lua_exp(L: TLuaState): Integer;
begin
  L.PushNumber(Exp(L.CheckArgNumber(1)));
  Result := 1;
end;

class function TLuaLibMath.Lua_floor(L: TLuaState): Integer;
var
  X: TLuaNumber;
begin
  X := L.CheckArgNumber(1);
  {
    if X <= TLuaNumber.MinValue then
    Exit(L.ErrorFmt('%f is too small to get floored', [X]));
    if X > TLuaNumber.MaxValue + 1 then
    Exit(L.ErrorFmt('%f is too big to get floored', [X]));
  }
  // Big number already "rounded"
  if (X > TLuaNumber.MinValue) and (X <= TLuaNumber.MaxValue + 1) then
    L.PushInteger(Floor(L.tonumber(1)));
  Result := 1;
end;

class function TLuaLibMath.Lua_fmod(L: TLuaState): Integer;
var
  A, B: TLuaNumber;
  X, Y: TLuaInteger;
  XIsInt, YIsInt: LongBool;
begin
  A := L.CheckArgNumber(1);
  B := L.CheckArgNumber(2);
  X := L.ToIntegerX(@XIsInt, 1);
  if XIsInt then
  begin
    Y := L.ToIntegerX(@YIsInt, 2);
    if YIsInt then
    begin
      if Y = 0 then
        L.error('bad argument #2 (zero)');
      L.PushInteger(X mod Y);
      Exit(1);
    end;
  end;
  L.PushNumber(FMod(A, B));
  Result := 1;
end;

class function TLuaLibMath.Lua_log(L: TLuaState): Integer;
var
  A: TLuaNumber;
begin
  A := L.CheckArgNumber(1);
  if L.Top = 1 then // arg 2 is e, use ln instead of log
    L.PushNumber(Ln(A))
  else
    L.PushNumber(LogN(L.CheckArgNumber(2), A)); // note that base and X are swapped
  Result := 1;
end;

class function TLuaLibMath.Lua_max(L: TLuaState): Integer;
var
  Lua: TLua;
  I: TLuaInteger;
  MaxIndex: TLuaInteger;
begin
  Lua := TLua.FromState(L);
  L.CheckArgAny(1);
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

class function TLuaLibMath.Lua_min(L: TLuaState): Integer;
var
  Lua: TLua;
  I: TLuaInteger;
  MinIndex: TLuaInteger;
begin
  Lua := TLua.FromState(L);
  L.CheckArgAny(1);
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

class function TLuaLibMath.Lua_modf(L: TLuaState): Integer;
var
  X: lua_Number;
  IsInt: LongBool;
  IntPart: TLuaInteger;
begin
  X := L.CheckArgNumber(1);
  L.PushNumber(Int(X));
  IntPart := L.ToIntegerX(@IsInt, 2);
  if IsInt then
    L.PushInteger(IntPart);
  L.PushNumber(Frac(X));
  Result := 2;
end;

class function TLuaLibMath.Lua_rad(L: TLuaState): Integer;
begin
  L.PushNumber(DegToRad(L.CheckArgNumber(1)));
  Result := 1;
end;

class function TLuaLibMath.Lua_sin(L: TLuaState): Integer;
begin
  L.PushNumber(Sin(L.CheckArgNumber(1)));
  Result := 1;
end;

class function TLuaLibMath.Lua_sqrt(L: TLuaState): Integer;
begin
  L.PushNumber(Sqrt(L.CheckArgNumber(1)));
  Result := 1;
end;

class function TLuaLibMath.Lua_tan(L: TLuaState): Integer;
begin
  L.PushNumber(Tan(L.CheckArgNumber(1)));
  Result := 1;
end;

class function TLuaLibMath.Lua_tointeger(L: TLuaState): Integer;
var
  IsInt: LongBool;
  I: TLuaInteger;
begin
  I := L.ToIntegerX(@IsInt, 1);
  if IsInt then
    L.PushInteger(I)
  else
    L.PushNil;
  Result := 1;
end;

class function TLuaLibMath.Lua_type(L: TLuaState): Integer;
begin
  L.CheckArgAny(1);
  if L.IsInteger(1) then
    L.PushString('integer')
  else if L.IsNumber(1) then
    L.PushString('float')
  else
    L.PushNil;
  Result := 1;
end;

class function TLuaLibMath.Lua_ult(L: TLuaState): Integer;
begin
  L.PushBoolean(TLuaUnsigned(L.CheckArgInteger(1)) < TLuaUnsigned(L.CheckArgInteger(2)));
  Result := 1;
end;

{ TLuaLibCoroutine }

class procedure TLuaLibCoroutine.CreateEntry(AEntry: TLuaLib.TTableEntry);
begin
  AEntry.Add('coroutine').AddPublished(Self);
end;

class function TLuaLibCoroutine.Lua_create(L: TLuaState): Integer;
var
  Thread: TLuaState;
begin
  L.CheckArg(1, ltFunction);
  Thread := L.NewThread;
  L.PushValue(1);
  L.XMove(Thread, 1);
  Result := 1;
end;

class function TLuaLibCoroutine.Lua_isyieldable(L: TLuaState): Integer;
begin
  L.PushBoolean(L.isyieldable);
  Result := 1;
end;

class function TLuaLibCoroutine.Lua_resume(L: TLuaState): Integer;
var
  R: Integer;
begin
  L.CheckArg(1, ltThread);
  R := LuaResumeHelper(L, L.ToThread(1), L.Top - 1);
  if R < 0 then
  begin
    L.PushBoolean(False);
    L.insert(-2);
    Result := 2;
  end
  else
  begin
    L.PushBoolean(True);
    L.insert(-R - 1);
    Result := R + 1;
  end;
end;

class function TLuaLibCoroutine.Lua_running(L: TLuaState): Integer;
begin
  L.PushBoolean(L.PushThread);
  Result := 2;
end;

class function TLuaLibCoroutine.Lua_status(L: TLuaState): Integer;
var
  C: TLuaState;
  AR: TLuaDebug;
begin
  L.CheckArg(1, ltThread);
  C := L.ToThread(1);

  if L = C then
    L.PushLiteral('running')
  else
  begin
    case C.status of
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

class function TLuaLibCoroutine.Lua_wrap(L: TLuaState): Integer;
begin
  Lua_create(L);
  L.PushCClosure(LuaWrapHelper, 1);
  Result := 1;
end;

class function TLuaLibCoroutine.Lua_yield(L: TLuaState): Integer;
begin
  Result := L.yield(L.Top);
end;

class function TLuaLibCoroutine.LuaResumeHelper(L, C: TLuaState; AArgs: Integer): Integer;
var
  status: TLuaStatus;
  ResultCount: Integer;
begin
  if not C.CheckStack(AArgs) then
  begin
    L.PushString('too many arguments to resume');
    Exit(-1);
  end;
  if (C.status = lstOk) and (C.Top = 0) then
  begin
    L.PushString('cannot resume dead coroutine');
    Exit(-1);
  end;
  L.XMove(C, AArgs);
  status := C.resume(L, AArgs);
  if status in [lstOk, lstYield] then
  begin
    ResultCount := C.Top;
    if not L.CheckStack(ResultCount + 1) then
    begin
      C.Pop(ResultCount);
      L.PushLiteral('too many results to resume');
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
    if L.&type = ltString then
    begin
      L.Where(1);
      L.insert(-2);
      L.concat(2);
    end;
    Exit(L.Error_X);
  end;
  Result := R;
end;

end.
