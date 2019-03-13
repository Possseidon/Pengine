unit Pengine.LuaWrapper;

interface

uses
  System.Rtti,
  System.TypInfo,
  System.SysUtils,

  Pengine.LuaHeader;

type

  TLuaFunction = function(L: TLuaState): Integer of object;

  TLuaWrapper<T> = class
  public type

    PData = ^T;

  private
    class function Index(L: TLuaState): Integer; static; cdecl;

  public
    class procedure Push(L: TLuaState; AValue: T);

    class function Check(L: TLuaState; AIndex: Integer = 1): PData;
    class function CheckArg(L: TLuaState; AIndex: Integer = 1): PData;
    class function RecordName: AnsiString; virtual;

  end;

implementation

{ TLuaWrapper<T> }

class function TLuaWrapper<T>.Check(L: TLuaState; AIndex: Integer): PData;
begin
  if not L.IsUserdata(AIndex) then
    L.Error(L.BadTypeString(RecordName, L.TypeNameAt(AIndex)));

  L.GetUservalue(AIndex);
  if (L.ToUserdata <> Self) then
    L.Error(L.BadTypeString(RecordName, L.TypeNameAt(AIndex)));

  Result := L.ToUserdata(AIndex);
  L.Pop;
end;

class function TLuaWrapper<T>.CheckArg(L: TLuaState; AIndex: Integer): PData;
begin
  if not L.IsUserdata(AIndex) then
    L.Error(L.BadArgString(AIndex, RecordName, L.TypeNameAt(AIndex)));

  L.GetUservalue(AIndex);
  if (L.ToUserdata <> Self) then
    L.Error(L.BadArgString(AIndex, RecordName, L.TypeNameAt(AIndex)));

  Result := L.ToUserdata(AIndex);
  L.Pop;
end;

class function TLuaWrapper<T>.Index(L: TLuaState): Integer;
var
  CustomIndex, IndexTable: Integer;
begin
  // Check table
  L.PushValue(L.UpvalueIndex(2));
  L.PushValue(-2);
  L.GetTable(-2);
  if not L.IsNil then
    Exit(1);

  // If nil, call index metamethod if not nil
  if L.IsNil(L.UpvalueIndex(1)) then
    Exit(0);
  L.PushValue(L.UpvalueIndex(1));
  L.PushValue(1);
  L.PushValue(2);
  L.Call(2, 1);
  Result := 1;
end;

class function TLuaWrapper<T>.RecordName: AnsiString;
begin
  Result := AnsiString(ClassName);
end;

class procedure TLuaWrapper<T>.Push(L: TLuaState; AValue: T);
var
  MetaEvent: TLuaMetatableEvent;
  Func: TLuaCFunction;
  RttiMethod: TRttiMethod;
begin
  // (1) Create record userdata
  T(L.NewUserdata(SizeOf(T))^) := AValue; // +

  // (2) Push the registry table
  L.PushValue(LUA_REGISTRYINDEX); // +

  // (3) Push class type for uservalue
  L.PushLightuserdata(Self); // +

  // Set uservalue of userdata to class type
  L.PushValue;
  L.SetUservalue(-4);

  // (4) Get the metatable for the classtype
  L.PushValue; // +
  L.GetTable(-3); // -uservalue +metatable

  if L.IsNil then
  begin
    // Pop the nil value
    L.Pop; // -

    // (5) Create the metatable
    L.NewTable; // +

    for MetaEvent := Low(TLuaMetatableEvent) to High(TLuaMetatableEvent) do
    begin
      Func := MethodAddress(string(LuaMetatableEventNames[MetaEvent]));
      if Assigned(Func) then
      begin
        L.PushCFunction(Func);
        L.SetField(PAnsiChar(LuaMetatableEventNames[MetaEvent]), -2);
      end;
    end;

    L.PushString(RecordName);
    L.SetField('__name', -2);

    // Decorate the __index function, to first check for methods
    L.GetField('__index');
    L.NewTable;

    for RttiMethod in TRttiContext.Create.GetType(Self).GetMethods do
    begin
      if RttiMethod.Visibility <> mvPublished then
        Continue;
      if not RttiMethod.Name.StartsWith('Lua_') then
        Continue;
      L.PushCFunction(RttiMethod.CodeAddress);
      L.SetField(PAnsiChar(AnsiString(RttiMethod.Name.Substring(4))), -2);
    end;

    L.PushCClosure(Index, 2);
    L.SetField('__index', -2);

    // Save the metatable in the registry
    // key uservalue
    L.PushValue(-2); // +
    // value metatable
    L.PushValue(-2); // +

    L.SetTable(-5); // -2
  end;

  // Set the metatable
  L.SetMetatable(-4); // -

  // Pop uservalue and registry
  L.Pop(2); // -2
end;

end.
