unit Pengine.Lua.Wrapper;

interface

uses
  System.Rtti,
  System.TypInfo,
  System.SysUtils,

  Pengine.Lua.Header,
  Pengine.Lua,
  Pengine.EventHandling;

type

  TLuaWrapperClass = class of TLuaWrapper;

  TLuaWrapper = class
  private
    class procedure PushParamsX(L: TLuaState; const AInfo); virtual; abstract;

  public
    class function GetClass(L: TLuaState; AIndex: Integer = -1): TLuaWrapperClass;

    class procedure PushX(L: TLuaState; AValue: Pointer); virtual; abstract;

    class function CheckX(L: TLuaState; AIndex: Integer): Pointer; virtual; abstract;
    class function CheckArgX(L: TLuaState; AIndex: Integer): Pointer; virtual; abstract;

    class function LuaName: AnsiString; virtual;

  end;

  TLuaWrapper<T> = class(TLuaWrapper)
  public type

    PData = ^T;

  private
    class function Index(L: TLuaState): Integer; static; cdecl;
    class function NewIndex(L: TLuaState): Integer; static; cdecl;

    class function GC(L: TLuaState): Integer; static; cdecl;

  protected
    class function TryConvertType(L: TLuaState; out AData: PData; AIndex: Integer): Boolean; virtual;

    class procedure CustomizeMetatable(L: TLuaState; AValue: T); virtual;

  public
    class procedure PushX(L: TLuaState; AValue: Pointer); override;
    class procedure Push(L: TLuaState; AValue: T); virtual;

    class function CheckX(L: TLuaState; AIndex: Integer): Pointer; override;
    class function CheckArgX(L: TLuaState; AIndex: Integer): Pointer; override;

    class function Check(L: TLuaState; AIndex: Integer): PData;
    class function CheckArg(L: TLuaState; AIndex: Integer): PData;

  end;

  TLuaClassWrapper<T: class> = class(TLuaWrapper<T>)
  public
    class procedure PushOwned(L: TLuaState; AValue: T);

  end;

  {$M+}

  TLuaEvent<T> = class(TLuaWrapper<TEvent<T>.TAccess>)
  private
    class procedure PushParamsX(L: TLuaState; const AInfo); override;

    class function RemoveSubscription(L: TLuaState): Integer; cdecl; static;

  protected
    class procedure PushParams(L: TLuaState; AInfo: T); virtual; abstract;

  public
    class function LuaName: AnsiString; override;

  published
    class function Lua_add(L: TLuaState): Integer; static; cdecl;

  end;

  TLuaEnumWrapper<T: record > = class(TLuaWrapper<T>)
  public
    class constructor Create;

    class procedure Push(L: TLuaState; AValue: T); override;

    class function EnumToInt(AValue: T): Integer;
    class function IntToEnum(AInteger: Integer): T;

    class function LuaName: AnsiString; override;

  published
    class function __tostring(L: TLuaState): Integer; static; cdecl;
    class function __eq(L: TLuaState): Integer; static; cdecl;
    class function __len(L: TLuaState): Integer; static; cdecl;

  end;

  TLuaLibEnum<T: record > = class(TLuaLib)
  protected
    class procedure CreateEntry(AEntry: TLuaLib.TTableEntry); override;

  published
    class function __len(L: TLuaState): Integer; static; cdecl;
    class function __index(L: TLuaState): Integer; static; cdecl;

  end;

  {$M-}

implementation

{ TLuaWrapper<T> }

class function TLuaWrapper<T>.Check(L: TLuaState; AIndex: Integer): PData;
begin
  if not TryConvertType(L, Result, AIndex) then
    L.Error(L.BadTypeString(LuaName, L.TypeNameAt(AIndex)));
end;

class function TLuaWrapper<T>.CheckArg(L: TLuaState; AIndex: Integer): PData;
begin
  if not TryConvertType(L, Result, AIndex) then
    L.Error(L.BadArgString(AIndex, LuaName, L.TypeNameAt(AIndex)));
end;

class function TLuaWrapper<T>.CheckArgX(L: TLuaState; AIndex: Integer): Pointer;
begin
  Result := CheckArg(L, AIndex);
end;

class function TLuaWrapper<T>.CheckX(L: TLuaState; AIndex: Integer): Pointer;
begin
  Result := Check(L, AIndex);
end;

class procedure TLuaWrapper<T>.CustomizeMetatable(L: TLuaState; AValue: T);
begin
  // nothing
end;

class function TLuaWrapper<T>.GC(L: TLuaState): Integer;
begin
  Finalize(PData(L.ToUserdata(1))^);
  if GetTypeKind(T) = tkClass then
  begin
    L.GetUservalue;
    if L.GetField('owned') = ltBoolean then
      TObject(L.ToUserdata(1)^).Free;
    Result := 0;
  end;
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

class function TLuaWrapper<T>.TryConvertType(L: TLuaState; out AData: PData; AIndex: Integer): Boolean;
var
  ClassType: TLuaWrapperClass;
begin
  ClassType := GetClass(L, AIndex);
  Result := (ClassType <> nil) and ClassType.InheritsFrom(Self);
  if Result then
    AData := L.ToUserdata(AIndex);
end;

class procedure TLuaWrapper<T>.Push(L: TLuaState; AValue: T);
var
  DataPointer: PData;
  MetaEvent: TLuaMetatableEvent;
  Func: TLuaCFunction;
  RttiMethod: TRttiMethod;
begin
  // (1) Create record userdata
  // use move to avoid problems with managed records
  DataPointer := L.NewUserdata(SizeOf(T));
  Initialize(DataPointer^);
  DataPointer^ := AValue;
  L.SetNameDebug(LuaName);

  // DebugName the registry table
  // L.PushValue(LUA_REGISTRYINDEX); // +
  L.SetNameDebug('registry', LUA_REGISTRYINDEX);

  // (2) Push class type
  L.PushLightuserdata(Self); // +

  // Create uservalue table
  L.NewTable; // +
  L.PushValue(-2); // +
  L.SetField('class', -2); // -
  L.SetUservalue(-3); // -

  // (3) Get the metatable for the classtype
  L.PushValue; // +
  L.GetTable(LUA_REGISTRYINDEX); // -uservalue +metatable

  if L.IsNil then
  begin
    // Pop the nil value
    L.Pop; // -

    // (5) Create the metatable
    L.NewTable; // +
    L.SetNameDebug(LuaName + ' metatable');

    L.PushBoolean(False);
    L.SetField('__metatable', -2);

    for MetaEvent := Low(TLuaMetatableEvent) to High(TLuaMetatableEvent) do
    begin
      Func := MethodAddress(string(LuaMetatableEventNames[MetaEvent]));
      if Assigned(Func) then
      begin
        L.PushCFunction(Func);
        L.SetField(PAnsiChar(LuaMetatableEventNames[MetaEvent]), -2);
      end;
    end;

    L.PushString(LuaName);
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

    if (GetTypeKind(T) = tkClass) or IsManagedType(T) then
    begin
      L.PushCFunction(GC);
      L.SetField('__gc',  -2);
    end;

    // Save the metatable in the registry
    // key uservalue
    L.PushValue(-2); // +
    // value metatable
    L.PushValue(-2); // +

    L.SetTable(LUA_REGISTRYINDEX); // -2

    CustomizeMetatable(L, AValue);
  end;

  // Set the metatable
  L.SetMetatable(-3); // -

  // Pop uservalue
  L.Pop; // -
end;

class procedure TLuaWrapper<T>.PushX(L: TLuaState; AValue: Pointer);
begin
  Push(L, T(AValue^));
end;

{ TLuaEvent<T> }

class procedure TLuaEvent<T>.PushParamsX(L: TLuaState; const AInfo);
begin
  PushParams(L, T(AInfo));
end;

class function TLuaEvent<T>.LuaName: AnsiString;
begin
  Result := 'event';
end;

class function TLuaEvent<T>.RemoveSubscription(L: TLuaState): Integer;
var
  Exists: Boolean;
begin
  L.PushValue(1);
  Exists := L.GetTable(LUA_REGISTRYINDEX) = ltBoolean;
  if Exists then
  begin
    L.PushValue(1);
    L.PushNil;
    L.SetTable(LUA_REGISTRYINDEX);
    IInterface(L.ToUserdata(L.UpvalueIndex(1)))._Release;
  end;
  L.PushBoolean(Exists);
  Result := 1;
end;

class function TLuaEvent<T>.Lua_add(L: TLuaState): Integer;
var
  Self: TLuaWrapperClass;
  Data: PData;
  Subscription: IEventSubscription;
  Handler: TEvent<T>.THandler;
  HandlerRef: Pointer;
begin
  Self := GetClass(L, 1);
  if (Self = nil) or not Self.InheritsFrom(TLuaEvent<T>) then
    L.Error(L.BadArgString(1, LuaName, L.TypeNameAt(1)));

  Data := Self.CheckX(L, 1);
  L.CheckArg(2, ltFunction);

  Handler := procedure(const AInfo: T)
    var
      Top: Integer;
    begin
      L.PushLightuserdata(TEvent<T>.THandler(HandlerRef));
      L.GetTable(LUA_REGISTRYINDEX);
      Top := L.Top;
      Self.PushParamsX(L, AInfo);
      L.Call(L.Top - Top, 0);
    end;
  HandlerRef := @Handler;

  L.PushLightuserdata(@Handler);
  L.PushValue(2);
  L.SetTable(LUA_REGISTRYINDEX);

  Subscription := Data.Subscribe(Handler);
  Subscription._AddRef;

  // Create a subscription table
  // __gc of this table removes the subcscription
  // It gets saved as a key to true in the registry to keep it alive, if it is not used
  // It also contains a remove function, to unsubscribe manually
  // and not on LuaState (to be exact, registry) destruction
  L.NewTable; // create subscription table
  L.PushLightuserdata(Subscription);
  L.PushCClosure(RemoveSubscription, 1); // create closure
  L.PushValue; // duplicate closure
  L.SetField('remove', -3); // set remove method in subscrition table

  L.NewTable; // create metatable
  L.PushString('eventsubscription');
  L.SetField('__name', -2);
  L.PushBoolean(False);
  L.SetField('__metatable', -2);
  L.PushValue(-2); // copy closure
  L.SetField('__gc', -2); // set __gc metamethod
  L.SetMetatable(-3); // set metatable of subscription table
  L.Pop; // pop closure
  L.PushValue; // duplicate subscription table, to return it at the end
  L.PushBoolean(True);
  L.SetTable(LUA_REGISTRYINDEX); // create reference in registry

  Result := 1;
end;

{ TLuaWrapper }

class function TLuaWrapper.GetClass(L: TLuaState; AIndex: Integer): TLuaWrapperClass;
begin
  if not L.IsUserdata(AIndex) then
    Exit(nil);
  if L.GetUservalue(AIndex) <> ltTable then // +
  begin
    L.Pop; // -
    Exit(nil);
  end;
  L.GetField('class'); // +
  Result := L.ToUserdata;
  L.Pop(2); // -2
end;

class function TLuaWrapper.LuaName: AnsiString;
begin
  Result := AnsiString(ClassName);
end;

{ TLuaEnumWrapper<T> }

class function TLuaEnumWrapper<T>.EnumToInt(AValue: T): Integer;
begin
  case SizeOf(T) of
    1:
      Result := PByte(@AValue)^;
    2:
      Result := PWord(@AValue)^;
    4:
      Result := PCardinal(@AValue)^;
  else
    Assert(False);
  end;
end;

class constructor TLuaEnumWrapper<T>.Create;
begin
  Assert(GetTypeKind(T) = tkEnumeration);
  Assert(TypeInfo(T) <> nil, 'Enumeration must start at zero and be continuous.');
end;

class function TLuaEnumWrapper<T>.IntToEnum(AInteger: Integer): T;
begin
  case SizeOf(T) of
    1:
      PByte(@Result)^ := AInteger;
    2:
      PWord(@Result)^ := AInteger;
    4:
      PCardinal(@Result)^ := AInteger;
  else
    Assert(False);
  end;
end;

class procedure TLuaEnumWrapper<T>.Push(L: TLuaState; AValue: T);
var
  Names: TArray<string>;
  I: Integer;
begin
  inherited;
  Assert(L.GetMetatable);
  L.NewTable;

  Names := TRttiEnumerationType(TRttiContext.Create.GetType(TypeInfo(T))).GetNames;
  for I := 0 to Length(Names) - 1 do
  begin
    L.PushString(AnsiString(Names[I]));
    L.SetI(I, -2);
    L.PushInteger(I);
    L.SetField(PAnsiChar(AnsiString(Names[I])), -2);
  end;

  L.SetField('lookup', -2);
  L.Pop;
end;

class function TLuaEnumWrapper<T>.LuaName: AnsiString;
begin
  Result := AnsiString(GetTypeName(TypeInfo(T)));
end;

class function TLuaEnumWrapper<T>.__eq(L: TLuaState): Integer;
begin
  L.PushBoolean(CompareMem(Check(L, 1), Check(L, 2), SizeOf(T)));
  Result := 1;
end;

class function TLuaEnumWrapper<T>.__len(L: TLuaState): Integer;
begin
  L.PushInteger(EnumToInt(Check(L, 1)^) + 1);
  Result := 1;
end;

class function TLuaEnumWrapper<T>.__tostring(L: TLuaState): Integer;
var
  Value: PData;
begin
  Value := Check(L, 1);
  Assert(L.GetMetatable);
  Assert(L.GetField('lookup') = ltTable);
  L.PushString(AnsiString(GetEnumName(TypeInfo(T), EnumToInt(Value^))));
  Result := 1;
end;

{ TLuaLibEnum<T> }

class procedure TLuaLibEnum<T>.CreateEntry(AEntry: TLuaLib.TTableEntry);
var
  TypeName: string;
begin
  TypeName := GetTypeName(TypeInfo(T));
  TypeName := TypeName.Substring(TypeName.LastIndexOf('.') + 1);
  AEntry.Add(AnsiString(TypeName)).AddPublished(Self);
end;

class function TLuaLibEnum<T>.__index(L: TLuaState): Integer;
var
  Value: Integer;
begin
  case L.CheckType(2, [ltNumber, ltString]) of
    ltNumber:
      Value := L.CheckInteger(2) - 1;
    ltString:
      Value := GetEnumValue(TypeInfo(T), string(L.ToString));
  end;
  if (Value >= 0) and (Value <= GetTypeData(TypeInfo(T)).MaxValue) then
    TLuaEnumWrapper<T>.Push(L, Value)
  else
    L.PushNil;
  Result := 1;
end;

class function TLuaLibEnum<T>.__len(L: TLuaState): Integer;
begin
  L.PushInteger(GetTypeData(TypeInfo(T)).MaxValue + 1);
  Result := 1;
end;

{ TLuaClassWrapper<T> }

class procedure TLuaClassWrapper<T>.PushOwned(L: TLuaState; AValue: T);
begin
  Push(L, AValue);
  L.GetUservalue;
  L.PushBoolean(True);
  L.SetField('owned', -2);
  L.Pop;
end;

end.
