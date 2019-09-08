program LuaWrapper;

{$APPTYPE CONSOLE}

{$R *.res}

{$M+}


uses
  System.SysUtils,

  Pengine.TimeManager,
  Pengine.DebugConsole,
  Pengine.Vector,
  Pengine.EventHandling,
  Pengine.Lua,
  Pengine.Lua.Header,
  Pengine.Lua.Wrapper;

type

  TLuaVector3 = class(TLuaWrapper<TVector3>)
  public
    class function LuaName: AnsiString; override;

  protected
    class function TryConvertType(L: TLuaState; out AData: TLuaWrapper<TVector3>.PData; AIndex: Integer): Boolean; override;

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

  TTest = class
  public type

    TEvent = TEvent<TTest>;

    TState = (
      &stOn,
      &stOff,
      &stUndefined
      );

  private
    FText: string;
    FVector: TVector3;
    FState: TState;
    FOnTextChange: TEvent;
    FOnVectorChange: TEvent;

    function GetOnTextChange: TEvent.TAccess;
    function GetOnVectorChange: TEvent.TAccess;

    procedure SetText(const Value: string);
    procedure SetVector(const Value: TVector3);

  public
    property Text: string read FText write SetText;
    property Vector: TVector3 read FVector write SetVector;
    property State: TState read FState write FState;

    property OnTextChange: TEvent.TAccess read GetOnTextChange;
    property OnVectorChange: TEvent.TAccess read GetOnVectorChange;

  end;

  TLuaTest = class(TLuaWrapper<TTest>)
  public type

    TLuaEvent = class(TLuaEvent<TTest>)
    protected
      class procedure PushParams(L: TLuaState; AInfo: TTest); override;

    public
      class function LuaName: AnsiString; override;

    end;

  public
    class function LuaName: AnsiString; override;

  published
    class function LuaGet_text(L: TLuaState): Integer; static; cdecl;
    class function LuaSet_text(L: TLuaState): Integer; static; cdecl;

    class function LuaGet_vector(L: TLuaState): Integer; static; cdecl;
    class function LuaSet_vector(L: TLuaState): Integer; static; cdecl;

    class function LuaGet_state(L: TLuaState): Integer; static; cdecl;
    class function LuaSet_state(L: TLuaState): Integer; static; cdecl;

    class function LuaGet_onTextChange(L: TLuaState): Integer; static; cdecl;
    class function LuaSet_onTextChange(L: TLuaState): Integer; static; cdecl;

    class function LuaGet_onVectorChange(L: TLuaState): Integer; static; cdecl;
    class function LuaSet_onVectorChange(L: TLuaState): Integer; static; cdecl;

  end;

  TLuaLocation3 = class(TLuaClassWrapper<TLocation3>)
  public type

    TLuaChangeEvent = class(TLuaEvent<TLocation3.TChangeEventInfo>)
    protected
      class procedure PushParams(L: TLuaState; AInfo: TLocation3.TChangeEventInfo); override;

    public
      class function LuaName: AnsiString; override;

    end;

  public
    class function LuaName: AnsiString; override;

  published
    class function Lua_reset(L: TLuaState): Integer; static; cdecl;

    class function LuaGet_parent(L: TLuaState): Integer; static; cdecl;
    class function LuaSet_parent(L: TLuaState): Integer; static; cdecl;

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

  TLuaLibLocation3 = class(TLuaLib)
  protected
    class procedure CreateEntry(AEntry: TLuaLib.TTableEntry); override;

  published
    class function __call(L: TLuaState): Integer; static; cdecl;

  end;

  TManagedRecord = record
    Text: string;
  end;

  TLuaManagedRecord = class(TLuaWrapper<TManagedRecord>)
  published
    class function LuaGet_text(L: TLuaState): Integer; static; cdecl;
    class function LuaSet_text(L: TLuaState): Integer; static; cdecl;

  end;

  ITest = interface
    procedure Test;
  end;

  TLuaInterfaceTest = class(TLuaWrapper<ITest>)
  published
    class function Lua_test(L: TLuaState): Integer; static; cdecl;

  end;

  TInterfaceTest = class(TInterfacedObject, ITest)
  public
    destructor Destroy; override;

    procedure Test;

  end;

{ TLuaVector3 }

class function TLuaVector3.LuaName: AnsiString;
begin
  Result := 'vec3';
end;

class function TLuaVector3.TryConvertType(L: TLuaState; out AData: TLuaWrapper<TVector3>.PData; AIndex: Integer): Boolean;
var
  Value: TLuaNumber;
begin
  if L.ToNumber(Value, AIndex) then
  begin
    TLuaVector3.Push(L, Value);
    L.Replace(AIndex);
  end;
  Result := inherited;
end;

class function TLuaVector3.Lua_dot(L: TLuaState): Integer;
var
  Vec: PData;
begin
  Vec := CheckArg(L, 1);
  L.PushNumber(Vec.Dot(CheckArg(L, 2)^));
  Result := 1;
end;

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

class function TLuaLocation3.LuaGet_parent(L: TLuaState): Integer;
begin
  TLuaLocation3.Push(L, Check(L, 1).Parent);
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

class function TLuaLocation3.LuaSet_parent(L: TLuaState): Integer;
var
  Self, Other, Current: TLocation3;
begin
  Self := Check(L, 1)^;
  Other := Check(L, 2)^;
  Current := Other;
  while Current <> nil do
  begin
    if Current = Self then
      L.Error('cannot set recursive location3 parents');
    Current := Current.Parent;
  end;
  Self.Parent := Other;
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

class function TLuaLocation3.LuaName: AnsiString;
begin
  Result := 'location3';
end;

{ TLuaLocation3.TLuaChangeEvent }

class procedure TLuaLocation3.TLuaChangeEvent.PushParams(L: TLuaState; AInfo: TLocation3.TChangeEventInfo);
begin
  TLuaLocation3.Push(L, AInfo.Location);
  TLuaVector3.Push(L, AInfo.Location.Pos);
  TLuaVector3.Push(L, AInfo.Location.Offset);
end;

class function TLuaLocation3.TLuaChangeEvent.LuaName: AnsiString;
begin
  Result := 'location3 change event';
end;

{ TTest }

function TTest.GetOnTextChange: TTest.TEvent.TAccess;
begin
  Result := FOnTextChange.Access;
end;

function TTest.GetOnVectorChange: TTest.TEvent.TAccess;
begin
  Result := FOnVectorChange.Access;
end;

procedure TTest.SetText(const Value: string);
begin
  if Text = Value then
    Exit;
  FText := Value;
  FOnTextChange.Execute(Self);
end;

procedure TTest.SetVector(const Value: TVector3);
begin
  if Vector = Value then
    Exit;
  FVector := Value;
  FOnVectorChange.Execute(Self);
end;

{ TLuaTest.TLuaEvent }

class procedure TLuaTest.TLuaEvent.PushParams(L: TLuaState; AInfo: TTest);
begin
  inherited;
  TLuaTest.Push(L, AInfo);
end;

class function TLuaTest.TLuaEvent.LuaName: AnsiString;
begin
  Result := 'test change event';
end;

{ TLuaTest }

class function TLuaTest.LuaGet_onTextChange(L: TLuaState): Integer;
begin
  TLuaEvent.Push(L, Check(L, 1).OnTextChange);
  Result := 1;
end;

class function TLuaTest.LuaGet_onVectorChange(L: TLuaState): Integer;
begin
  TLuaEvent.Push(L, Check(L, 1).OnVectorChange);
  Result := 1;
end;

class function TLuaTest.LuaGet_state(L: TLuaState): Integer;
begin
  TLuaEnumWrapper<TTest.TState>.Push(L, Check(L, 1).State);
  Result := 1;
end;

class function TLuaTest.LuaGet_text(L: TLuaState): Integer;
begin
  L.PushString(AnsiString(Check(L, 1).Text));
  Result := 1;
end;

class function TLuaTest.LuaGet_vector(L: TLuaState): Integer;
begin
  TLuaVector3.Push(L, Check(L, 1).Vector);
  Result := 1;
end;

class function TLuaTest.LuaSet_onTextChange(L: TLuaState): Integer;
begin
  L.PushCFunction(TLuaEvent.Lua_add);
  TLuaEvent.Push(L, Check(L, 1).OnTextChange);
  L.PushValue(2);
  L.Call(2, 0);
  Result := 0;
end;

class function TLuaTest.LuaSet_onVectorChange(L: TLuaState): Integer;
begin
  L.PushCFunction(TLuaEvent.Lua_add);
  TLuaEvent.Push(L, Check(L, 1).OnVectorChange);
  L.PushValue(2);
  L.Call(2, 0);
  Result := 0;
end;

class function TLuaTest.LuaSet_state(L: TLuaState): Integer;
begin
  Check(L, 1).State := TLuaEnumWrapper<TTest.TState>.Check(L, 2)^;
  Result := 0;
end;

class function TLuaTest.LuaSet_text(L: TLuaState): Integer;
begin
  Check(L, 1).Text := string(L.CheckString(2));
  Result := 0;
end;

class function TLuaTest.LuaSet_vector(L: TLuaState): Integer;
begin
  Check(L, 1).Vector := TLuaVector3.Check(L, 2)^;
  Result := 0;
end;

class function TLuaTest.LuaName: AnsiString;
begin
  Result := 'test';
end;

{ TLuaManagedRecord }

class function TLuaManagedRecord.LuaGet_text(L: TLuaState): Integer;
begin
  L.PushString(AnsiString(Check(L, 1).Text));
  Result := 1;
end;

class function TLuaManagedRecord.LuaSet_text(L: TLuaState): Integer;
begin
  Check(L, 1).Text := string(L.CheckString(2));
  Result := 0;
end;

{ TLuaLibLocation3 }

class procedure TLuaLibLocation3.CreateEntry(AEntry: TLuaLib.TTableEntry);
begin
  AEntry.Add('TLocation3').AddPublished(Self);
end;

class function TLuaLibLocation3.__call(L: TLuaState): Integer;
begin
  TLuaLocation3.PushOwned(L, TLocation3.Create(L.CheckArgOrDefault(2, False)));
  Result := 1;
end;

{ TInterfaceTest }

destructor TInterfaceTest.Destroy;
begin
  Writeln('Goodbye!');
  inherited;
end;

procedure TInterfaceTest.Test;
begin
  Writeln('This is test!');
end;

{ TLuaInterfaceTest }

class function TLuaInterfaceTest.Lua_test(L: TLuaState): Integer;
begin
  Check(L, 1).Test;
  Result := 0;
end;

var
  Lua: TLua;
  Location: TLocation3;
  Test, Test2: TTest;
  Code: string;
  OldTop: Integer;
  ManagedRec: TManagedRecord;

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    Lua := TLua.Create;
    Lua.L.LOpenLibs;
    Lua.AddLib<TLuaLibVector3>;
    Lua.AddLib<TLuaLibLocation3>;
    Lua.AddLib<TLuaLibEnum<TTest.TState>>;

    TLuaVector3.Push(Lua.L, Vec3(1, 2, 3));
    Lua.L.SetGlobal('a');

    TLuaVector3.Push(Lua.L, Vec3(5, 1, 2));
    Lua.L.SetGlobal('b');

    Location := TLocation3.Create;
    TLuaLocation3.Push(Lua.L, Location);
    Lua.L.SetGlobal('l');

    TLuaLocation3.PushOwned(Lua.L, TLocation3.Create);
    Lua.L.SetGlobal('l2');

    Test := TTest.Create;
    TLuaTest.Push(Lua.L, Test);
    Lua.L.SetGlobal('test');

    Test2 := TTest.Create;
    TLuaTest.Push(Lua.L, Test2);
    Lua.L.SetGlobal('test2');

    ManagedRec.Text := 'test';
    TLuaManagedRecord.Push(Lua.L, ManagedRec);
    Lua.L.SetGlobal('rec');

    TLuaInterfaceTest.Push(Lua.L, TInterfaceTest.Create);
    Lua.L.SetGlobal('intf');

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
        Lua.L.GetGlobal('print');
        Lua.L.Insert(OldTop);
        Lua.L.Call(Lua.L.Top - OldTop, 0);
      end;

    end;

    Lua.Free;

    Location.Pos := 39485798;
    Location.Free;

    Test.Text := 'asdfasfasdf';
    Test.Vector := 437958374;
    Test.Free;
    Test2.Free;

    Writeln('Done');

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
