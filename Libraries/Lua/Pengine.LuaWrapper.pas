unit Pengine.LuaWrapper;

interface

uses
  System.Rtti,
  System.TypInfo,
  System.SysUtils,

  Pengine.LuaHeader;

type

  TLuaWrapper<T> = class
  public type

    PData = ^T;

  private
    class function Index(L: TLuaState): Integer; static; cdecl;
    class function NewIndex(L: TLuaState): Integer; static; cdecl;

  protected
    class function TryConvertType(L: TLuaState; out AData: PData; AIndex: Integer = 1): Boolean; virtual;

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
  if not TryConvertType(L, Result, AIndex) then
    L.Error(L.BadTypeString(RecordName, L.TypeNameAt(AIndex)));
end;

class function TLuaWrapper<T>.CheckArg(L: TLuaState; AIndex: Integer): PData;
begin
  if not TryConvertType(L, Result, AIndex) then
    L.Error(L.BadArgString(AIndex, RecordName, L.TypeNameAt(AIndex)));
end;

class function TLuaWrapper<T>.Index(L: TLuaState): Integer;
var
  CustomIndex, IndexTable: Integer;
begin
  L.PushValue(L.UpvalueIndex(1));

  // Check table
  L.PushValue(-2);
  if L.GetTable(-2) <> ltNil then
    Exit(1);
  L.Pop;

  // Check get table
  L.GetField('_get');
  L.PushValue(-3);
  if L.GetTable(-2) <> ltNil then
  begin
    L.PushValue(1);
    L.PushValue(2);
    L.Call(2, 1);
    Exit(1);
  end;
  L.Pop(2);

  // If nil, call index metamethod if not nil
  if L.IsNil(L.UpvalueIndex(2)) then
    Exit(0);
  L.PushValue(L.UpvalueIndex(2));
  L.PushValue(1);
  L.PushValue(2);
  L.Call(2, 1);
  Result := 1;
end;

class function TLuaWrapper<T>.NewIndex(L: TLuaState): Integer;
begin
  L.PushValue(L.UpvalueIndex(1));

  // Check set table
  L.GetField('_set');
  L.PushValue(-4);
  if L.GetTable(-2) <> ltNil then
  begin
    L.PushValue(1);
    L.PushValue(3);
    L.Call(2, 0);
    Exit(0);
  end;
  L.Pop;

  // If nil, call newindex metamethod if not nil
  if L.IsNil(L.UpvalueIndex(2)) then
    L.ErrorFmt('invalid property "%s"', [L.ToString(2)]);
  L.PushValue(L.UpvalueIndex(2));
  L.PushValue(1);
  L.PushValue(2);
  L.PushValue(3);
  L.Call(3, 0);
  Result := 0;
end;

class function TLuaWrapper<T>.RecordName: AnsiString;
begin
  Result := AnsiString(ClassName);
end;

class function TLuaWrapper<T>.TryConvertType(L: TLuaState; out AData: PData; AIndex: Integer = 1): Boolean;
begin
  if not L.IsUserdata(AIndex) then
    Exit(False);
  if L.GetUservalue(AIndex) = ltNil then
  begin
    L.Pop;
    Exit(False);
  end;
  Result := L.ToUserdata = Self;
  L.Pop;
  if Result then
    AData := L.ToUserdata(AIndex);
end;

class procedure TLuaWrapper<T>.Push(L: TLuaState; AValue: T);
var
  MetaEvent: TLuaMetatableEvent;
  Func: TLuaCFunction;
  RttiMethod: TRttiMethod;
begin
  // (1) Create record userdata
  T(L.NewUserdata(SizeOf(T))^) := AValue; // +
  L.SetNameDebug(RecordName);

  // (2) Push the registry table
  L.PushValue(LUA_REGISTRYINDEX); // +
  L.SetNameDebug('registry');

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
    L.SetNameDebug(RecordName + ' metatable');

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

    // Method table for __index and __newindex functions
    L.NewTable;
    L.SetNameDebug('method table');

    // getter table
    L.NewTable;
    L.SetNameDebug('getter table');
    // setter table
    L.NewTable;
    L.SetNameDebug('setter table');

    for RttiMethod in TRttiContext.Create.GetType(Self).GetMethods do
    begin
      if RttiMethod.Visibility <> mvPublished then
        Continue;
      if RttiMethod.Name.StartsWith('Lua_') then
      begin
        L.PushCFunction(RttiMethod.CodeAddress);
        L.SetField(PAnsiChar(AnsiString(RttiMethod.Name.Substring(4))), -4);
      end
      else if RttiMethod.Name.StartsWith('LuaGet_') then
      begin
        L.PushCFunction(RttiMethod.CodeAddress);
        L.SetField(PAnsiChar(AnsiString(RttiMethod.Name.Substring(7))), -3);
      end
      else if RttiMethod.Name.StartsWith('LuaSet_') then
      begin
        L.PushCFunction(RttiMethod.CodeAddress);
        L.SetField(PAnsiChar(AnsiString(RttiMethod.Name.Substring(7))), -2);
      end;
    end;

    L.SetField('_set', -3);
    L.SetField('_get', -2);

    L.PushValue;
    L.GetField('__index', -3);
    L.PushCClosure(Index, 2);
    L.SetField('__index', -3);

    L.GetField('__newindex', -2);
    L.PushCClosure(NewIndex, 2);
    L.SetField('__newindex', -2);

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
