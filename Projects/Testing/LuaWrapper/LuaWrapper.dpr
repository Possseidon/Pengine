program LuaWrapper;

{$APPTYPE CONSOLE}

{$R *.res}

{$M+}


uses
  System.SysUtils,

  Pengine.TimeManager,
  Pengine.DebugConsole,
  Pengine.Lua,
  Pengine.LuaDefaultLibs,
  Pengine.LuaHeader,
  Pengine.LuaWrapper,
  Pengine.Vector,
  Pengine.EventHandling;

type
  TLuaVector3 = class(TLuaWrapper<TVector3>)
  public
    class function RecordName: AnsiString; override;

  published
    class function Lua_dot(L: TLuaState): Integer; static; cdecl;
    class function Lua_cross(L: TLuaState): Integer; static; cdecl;
    class function Lua_normalize(L: TLuaState): Integer; static; cdecl;

    class function __index(L: TLuaState): Integer; static; cdecl;
    class function __newindex(L: TLuaState): Integer; static; cdecl;
    class function __tostring(L: TLuaState): Integer; static; cdecl;
    class function __add(L: TLuaState): Integer; static; cdecl;
    class function __sub(L: TLuaState): Integer; static; cdecl;
    class function __len(L: TLuaState): Integer; static; cdecl;

    class function __eq(L: TLuaState): Integer; static; cdecl;
    class function __lt(L: TLuaState): Integer; static; cdecl;
    class function __le(L: TLuaState): Integer; static; cdecl;

  end;

  TLuaLocation3 = class(TLuaWrapper<TLocation3>)
  public type

    TFunctionWrapper = class
    private
      FL: TLuaState;
      FFunction: Pointer;

    public
      constructor Create(AL: TLuaState);

      procedure Call(AInfo: TLocation3.TChangeEventInfo);

      property L: TLuaState read FL;

    end;

    TLuaChangeEvent = class(TLuaWrapper<TLocation3.TChangeEvent.TAccess>)
    published
      class function Lua_add(L: TLuaState): Integer; static; cdecl;

    end;

  published
    class function Lua_reset(L: TLuaState): Integer; static; cdecl;

    // class function __index(L: TLuaState): Integer; static; cdecl;
    // class function __newindex(L: TLuaState): Integer; static; cdecl;

    class function LuaGet_pos(L: TLuaState): Integer; static; cdecl;
    class function LuaSet_pos(L: TLuaState): Integer; static; cdecl;

    class function LuaGet_onChange(L: TLuaState): Integer; static; cdecl;

  end;

  TLuaLibVector3 = class(TLuaLib)
  protected
    class procedure CreateEntry(AEntry: TLuaLib.TTableEntry); override;

  published
    class function __call(L: TLuaState): Integer; static; cdecl;

  end;

  TLuaLibPrint = class(TLuaLib)
  private
    class function LuaPrint(L: TLuaState): Integer; static; cdecl;

  protected
    class procedure CreateEntry(AEntry: TLuaLib.TTableEntry); override;

  end;

  { TLuaLibDebug }

class procedure TLuaLibPrint.CreateEntry(AEntry: TLuaLib.TTableEntry);
begin
  AEntry.Add('print', LuaPrint);
end;

class function TLuaLibPrint.LuaPrint(L: TLuaState): Integer;
var
  I: Integer;
begin
  Write(L.ToString(1));
  for I := 2 to L.Top do
    Write(#9, L.ToString(I));
  Writeln;
  Result := 0;
end;

class function TLuaVector3.RecordName: AnsiString;
begin
  Result := 'vec3';
end;

class function TLuaVector3.Lua_dot(L: TLuaState): Integer;
var
  Vec: PData;
begin
  Vec := CheckArg(L, 1);
  L.PushNumber(Vec.Dot(CheckArg(L, 2)^));
  Result := 1;
end;

{ TLuaVector3 }

class function TLuaVector3.Lua_cross(L: TLuaState): Integer;
var
  Vec: PData;
begin
  Vec := CheckArg(L, 1);
  Push(L, Vec.Cross(CheckArg(L, 2)^));
  Result := 1;
end;

class function TLuaVector3.Lua_normalize(L: TLuaState): Integer;
begin
  Push(L, CheckArg(L).Normalize);
  Result := 1;
end;

class function TLuaVector3.__index(L: TLuaState): Integer;
var
  Vector: PData;
  Index: AnsiString;
begin
  Vector := CheckArg(L);
  Index := L.CheckArgString(2);
  if Index = 'x' then
    L.PushNumber(Vector.X)
  else if Index = 'y' then
    L.PushNumber(Vector.Y)
  else if Index = 'z' then
    L.PushNumber(Vector.Z)
  else
    Exit(0);
  Result := 1;
end;

class function TLuaVector3.__newindex(L: TLuaState): Integer;
var
  Vector: PData;
  Index: AnsiString;
  Value: TLuaNumber;
begin
  Vector := CheckArg(L);
  Index := L.CheckArgString(2);
  Value := L.CheckArgNumber(3);
  if Index = 'x' then
    Vector.X := Value
  else if Index = 'y' then
    Vector.Y := Value
  else if Index = 'z' then
    Vector.Z := Value
  else
    L.Error('invalid property');
  Result := 0;
end;

class function TLuaVector3.__tostring(L: TLuaState): Integer;
begin
  L.PushString(AnsiString(CheckArg(L).ToString));
  Result := 1;
end;

class function TLuaVector3.__add(L: TLuaState): Integer;
begin
  Push(L, CheckArg(L, 1)^ + CheckArg(L, 2)^);
  Result := 1;
end;

class function TLuaVector3.__sub(L: TLuaState): Integer;
begin
  Push(L, CheckArg(L, 1)^ - CheckArg(L, 2)^);
  Result := 1;
end;

class function TLuaVector3.__len(L: TLuaState): Integer;
begin
  L.PushNumber(CheckArg(L).Length);
  Result := 1;
end;

class function TLuaVector3.__eq(L: TLuaState): Integer;
begin
  L.PushBoolean(CheckArg(L, 1)^ = CheckArg(L, 2)^);
  Result := 1;
end;

class function TLuaVector3.__lt(L: TLuaState): Integer;
begin
  L.PushBoolean(CheckArg(L, 1)^ < CheckArg(L, 2)^);
  Result := 1;
end;

class function TLuaVector3.__le(L: TLuaState): Integer;
begin
  L.PushBoolean(CheckArg(L, 1)^ <= CheckArg(L, 2)^);
  Result := 1;
end;

{ TLuaLibVector3 }

class procedure TLuaLibVector3.CreateEntry(AEntry: TLuaLib.TTableEntry);
begin
  AEntry.Add('vec3').AddPublished(Self);
end;

class function TLuaLibVector3.__call(L: TLuaState): Integer;
begin
  // Ignore self
  case L.Top of
    1:
      TLuaVector3.Push(L, 0);
    2:
      TLuaVector3.Push(L, Vec3(L.CheckArgNumber(2)));
    4:
      TLuaVector3.Push(L, Vec3(L.CheckArgNumber(2), L.CheckArgNumber(3), L.CheckArgNumber(4)));
  else
    Exit(L.Error('expected 0, 1 or 3 arguments'));
  end;
  Result := 1;
end;

{ TLuaLocation3 }

class function TLuaLocation3.LuaGet_onChange(L: TLuaState): Integer;
begin
  TLuaChangeEvent.Push(L, Check(L).OnChanged);
  Result := 1;
end;

class function TLuaLocation3.LuaGet_pos(L: TLuaState): Integer;
begin
  TLuaVector3.Push(L, Check(L).Pos);
  Result := 1;
end;

class function TLuaLocation3.LuaSet_pos(L: TLuaState): Integer;
begin
  Check(L).Pos := TLuaVector3.Check(L, 2)^;
  Result := 0;
end;

class function TLuaLocation3.Lua_reset(L: TLuaState): Integer;
begin
  CheckArg(L).Reset;
  Result := 0;
end;

{ TLuaLocation3.TLuaChangeEvent }

class function TLuaLocation3.TLuaChangeEvent.Lua_add(L: TLuaState): Integer;
var
  Data: PData;
  Wrapper: TFunctionWrapper;
begin
  Data := Check(L);
  L.CheckArg(2, ltFunction);
  Wrapper := TFunctionWrapper.Create(L);
  Data.Add(Wrapper.Call);
  Result := 0;
end;

{ TLuaLocation3.TFunctionWrapper }

procedure TLuaLocation3.TFunctionWrapper.Call(AInfo: TLocation3.TChangeEventInfo);
begin
  // TODO: Push Function
  TLuaLocation3.Push(L, AInfo.Sender);
  L.Call(1, 0);
end;

constructor TLuaLocation3.TFunctionWrapper.Create(AL: TLuaState);
begin
  FL := AL;
end;

var
  Lua: TLua;
  Code: string;

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    Lua := TLua.Create;

    Lua.AddLib<TLuaLibBasic>;
    Lua.AddLib<TLuaLibTable>;
    Lua.AddLib<TLuaLibMath>;
    Lua.AddLib<TLuaLibCoroutine>;
    Lua.AddLib<TLuaLibPrint>;
    Lua.AddLib<TLuaLibVector3>;

    TLuaVector3.Push(Lua.L, Vec3(1, 2, 3));
    Lua.L.SetGlobal('a');

    TLuaVector3.Push(Lua.L, Vec3(5, 1, 2));
    Lua.L.SetGlobal('b');

    TLuaLocation3.Push(Lua.L, TLocation3.Create);
    Lua.L.SetGlobal('l');

    while True do
    begin
      Readln(Code);
      if Lua.L.LoadString(AnsiString(Code)) <> lleOK then
      begin
        Writeln(Lua.L.ToString);
        Continue;
      end;

      if Lua.CallTimeout(0, 0, 1) <> lceOK then
        Writeln(Lua.L.ToString);

    end;

    Lua.Free;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
