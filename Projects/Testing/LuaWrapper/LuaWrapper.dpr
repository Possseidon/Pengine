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
            {
    PFunctionWrapper = ^TFunctionWrapper;

    TFunctionWrapper = record
    public
      L: TLuaState;

      class function Remove(L: TLuaState): Integer; static; cdecl;

      procedure Call(AInfo: TLocation3.TChangeEventInfo);

    end;

    TLuaChangeEvent = class(TLuaWrapper<TLocation3.TChangeEvent.TAccess>)
    published
      class function Lua_add(L: TLuaState): Integer; static; cdecl;
      class function Lua_remove(L: TLuaState): Integer; static; cdecl;

    end;
         }

         {
    TLuaChangeEventInfo = class(TLuaEventInfo<TLocation3.TChangeEventInfo>)

    end;
         }

    TLuaChangeEvent = class(TLuaEvent<TLocation3.TChangeEvent.TAccess, TLocation3.TChangeEventInfo>)
    protected
      class procedure PushParams(L: TLuaState; AInfo: TLocation3.TChangeEventInfo); override;

    end;

  published
    class function Lua_reset(L: TLuaState): Integer; static; cdecl;

    // class function __index(L: TLuaState): Integer; static; cdecl;
    // class function __newindex(L: TLuaState): Integer; static; cdecl;

    class function LuaGet_pos(L: TLuaState): Integer; static; cdecl;
    class function LuaSet_pos(L: TLuaState): Integer; static; cdecl;

    class function LuaGet_onChange(L: TLuaState): Integer; static; cdecl;
    class function LuaSet_onChange(L: TLuaState): Integer; static; cdecl;

  end;

  TLuaLibVector3 = class(TLuaLib)
  protected
    class procedure CreateEntry(AEntry: TLuaLib.TTableEntry); override;

  published
    class function __call(L: TLuaState): Integer; static; cdecl;

  end;

  TLuaLibPrint = class(TLuaLib)
  protected
    class procedure CreateEntry(AEntry: TLuaLib.TTableEntry); override;

  published
    class function Lua_print(L: TLuaState): Integer; static; cdecl;
    class function Lua_gc(L: TLuaState): Integer; static; cdecl;

  end;

  { TLuaLibDebug }

class procedure TLuaLibPrint.CreateEntry(AEntry: TLuaLib.TTableEntry);
begin
  AEntry.AddPublished(Self);
end;

class function TLuaLibPrint.Lua_gc(L: TLuaState): Integer;
begin
  L.GC(LUA_GCCOLLECT, 0);
  Result := 0;
end;

class function TLuaLibPrint.Lua_print(L: TLuaState): Integer;
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
  Push(L, CheckArg(L, 1).Normalize);
  Result := 1;
end;

class function TLuaVector3.__index(L: TLuaState): Integer;
var
  Vector: PData;
  Index: AnsiString;
begin
  Vector := CheckArg(L, 1);
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
  Vector := CheckArg(L, 1);
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
  L.PushString(AnsiString(CheckArg(L, 1).ToString));
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
  L.PushNumber(CheckArg(L, 1).Length);
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
  case L.Top - 1 of
    0:
      TLuaVector3.Push(L, 0);
    1:
      TLuaVector3.Push(L, Vec3(L.CheckArgNumber(2)));
    3:
      TLuaVector3.Push(L, Vec3(L.CheckArgNumber(2), L.CheckArgNumber(3), L.CheckArgNumber(4)));
  else
    Exit(L.Error('expected 0, 1 or 3 arguments'));
  end;
  Result := 1;
end;

{ TLuaLocation3 }

class function TLuaLocation3.LuaGet_onChange(L: TLuaState): Integer;
begin
  TLuaChangeEvent.Push(L, Check(L, 1).OnChanged);
  Result := 1;
end;

class function TLuaLocation3.LuaGet_pos(L: TLuaState): Integer;
begin
  TLuaVector3.Push(L, Check(L, 1).Pos);
  Result := 1;
end;

class function TLuaLocation3.LuaSet_onChange(L: TLuaState): Integer;
begin
  L.PushCFunction(TLuaChangeEvent.Lua_add);
  TLuaChangeEvent.Push(L, Check(L, 1).OnChanged);
  L.PushValue(2);
  L.Call(2, 0);
  Result := 0;
end;

class function TLuaLocation3.LuaSet_pos(L: TLuaState): Integer;
begin
  Check(L, 1).Pos := TLuaVector3.Check(L, 2)^;
  Result := 0;
end;

class function TLuaLocation3.Lua_reset(L: TLuaState): Integer;
begin
  CheckArg(L, 1).Reset;
  Result := 0;
end;

{ TLuaLocation3.TLuaChangeEvent }
                      {
class function TLuaLocation3.TLuaChangeEvent.Lua_add(L: TLuaState): Integer;
var
  Data: PData;
  Wrapper: PFunctionWrapper;
begin
  Data := Check(L, 1);
  L.CheckArg(2, ltFunction);

  L.PushValue(2);
  if L.GetTable(LUA_REGISTRYINDEX) <> ltNil then
    L.Error('event handler exists already');

  Wrapper := L.NewUserdata(SizeOf(TFunctionWrapper));
  Wrapper.L := L;

  Data.Add(Wrapper.Call);

  L.NewTable;
  L.PushValue(1);
  L.PushValue(-3);
  L.PushCClosure(Wrapper.Remove, 2);
  L.SetField('__gc', -2);
  L.SetMetatable(-2);

  L.PushLightuserdata(Wrapper);
  L.PushValue(2);
  L.SetTable(LUA_REGISTRYINDEX);

  L.PushValue(2);
  L.PushValue(4);
  L.SetTable(LUA_REGISTRYINDEX);

  Result := 0;
end;

class function TLuaLocation3.TLuaChangeEvent.Lua_remove(L: TLuaState): Integer;
var
  Data: PData;
  Wrapper: PFunctionWrapper;
begin
  Data := Check(L, 1);
  L.CheckArg(2, ltFunction);
  L.PushValue(2);

  if L.GetTable(LUA_REGISTRYINDEX) <> ltNil then
  begin
    Wrapper := L.ToUserdata;
    Data.Remove(Wrapper.Call);

    Assert(L.GetMetatable);
    L.PushNil;
    L.SetField('__gc', -2);
    L.Pop;

    L.PushNil;
    L.SetTable(LUA_REGISTRYINDEX);
    L.PushNil;
    L.SetTable(LUA_REGISTRYINDEX);
  end
  else
    L.Error('event handler not found');

  Result := 0;
end;

{ TLuaLocation3.TFunctionWrapper }
    {
procedure TLuaLocation3.TFunctionWrapper.Call(AInfo: TLocation3.TChangeEventInfo);
begin
  L.PushLightuserdata(@Self);
  L.GetTable(LUA_REGISTRYINDEX);
  TLuaLocation3.Push(L, AInfo.Sender);
  TLuaVector3.Push(L, AInfo.Sender.Pos);
  TLuaVector3.Push(L, AInfo.Sender.Offset);
  L.Call(3, 0);
end;

class function TLuaLocation3.TFunctionWrapper.Remove(L: TLuaState): Integer;
var
  Data: TLuaChangeEvent.PData;
  Self: PFunctionWrapper;
begin
  Data := L.ToUserdata(L.UpvalueIndex(1));
  Self := L.ToUserdata(L.UpvalueIndex(2));
  Data.Remove(Self.Call);
  Result := 0;
end;
     }
{ TLuaLocation3.TLuaChangeEvent2 }

class procedure TLuaLocation3.TLuaChangeEvent.PushParams(L: TLuaState; AInfo: TLocation3.TChangeEventInfo);
begin
  TLuaLocation3.Push(L, AInfo.Sender);
  TLuaVector3.Push(L, AInfo.Sender.Pos);
  TLuaVector3.Push(L, AInfo.Sender.Offset);
end;

var
  Lua: TLua;
  Location: TLocation3;
  Code: string;
  I: Integer;
  OldTop: Integer;
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

    Location := TLocation3.Create;
    TLuaLocation3.Push(Lua.L, Location);
    Lua.L.SetGlobal('l');

    while True do
    begin
      Readln(Code);
      if Code = 'exit' then
        Break;

      case Lua.L.LoadString('return ' + AnsiString(Code)) of
        lleErrorSyntax:
          if Lua.L.LoadString(AnsiString(Code)) <> lleOK then
          begin
            Writeln(Lua.L.ToString);
            Lua.L.Pop;
            Continue;
          end;

        lleErrorMemory, lleErrorGCMM:
          begin
            Writeln(Lua.L.ToString);
            Lua.L.Pop;
            Continue;
          end;
      end;

      OldTop := Lua.L.Top;
      if Lua.CallTimeout(0, LUA_MULTRET, 1) <> lceOK then
      begin
        Writeln(Lua.L.ToString);
        Lua.L.Pop;
        Continue;
      end;

      if OldTop <> Lua.L.Top + 1 then
      begin
        Lua.L.PushCFunction(TLuaLibPrint.Lua_print);
        Lua.L.Insert(OldTop);
        Lua.L.Call(Lua.L.Top - OldTop, 0);
      end;

    end;

    Lua.Free;

    Location.Pos := 42;
    Location.Free;

    Writeln('Done');

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
