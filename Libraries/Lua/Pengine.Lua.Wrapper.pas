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
    class procedure PushParamsX(L: TLuaState; AInfo: TObject); virtual; abstract;

  public
    class function GetClass(L: TLuaState; AIndex: Integer = -1): TLuaWrapperClass;

    class procedure PushX(L: TLuaState; AValue: Pointer); virtual; abstract;

    class function CheckX(L: TLuaState; AIndex: Integer): Pointer; virtual; abstract;
    class function CheckArgX(L: TLuaState; AIndex: Integer): Pointer; virtual; abstract;

    class function RecordName: AnsiString; virtual;

  end;

  TLuaWrapper<T> = class(TLuaWrapper)
  public type

    PData = ^T;

  private
    class function Index(L: TLuaState): Integer; static; cdecl;
    class function NewIndex(L: TLuaState): Integer; static; cdecl;

  protected
    class function TryConvertType(L: TLuaState; out AData: PData; AIndex: Integer): Boolean; virtual;

  public
    class procedure PushX(L: TLuaState; AValue: Pointer); override;
    class procedure Push(L: TLuaState; AValue: T); virtual;

    class function CheckX(L: TLuaState; AIndex: Integer): Pointer; override;
    class function CheckArgX(L: TLuaState; AIndex: Integer): Pointer; override;

    class function Check(L: TLuaState; AIndex: Integer): PData;
    class function CheckArg(L: TLuaState; AIndex: Integer): PData;

  end;

{$M+}

  TLuaEvent<TAccess: record; TInfo: TEventInfo> = class(TLuaWrapper<TAccess>)
  private type

    TPushParamsFunc = procedure(L: TLuaState; AInfo: TObject) of object;

    PFunctionWrapper = ^TFunctionWrapper;

    TFunctionWrapper = record
    public
      L: TLuaState;
      Event: Pointer;
      PushParamsFunc: TPushParamsFunc;

      class function Remove(L: TLuaState): Integer; static; cdecl;

      procedure Call(AInfo: TInfo);

    end;

  class procedure PushParamsX(L: TLuaState; AInfo: TObject); override;

  protected
    class procedure PushParams(L: TLuaState; AInfo: TInfo); virtual; abstract;

  public
    class function RecordName: AnsiString; override;

    class procedure ClearEvents(L: TLuaState; AEvent: TAccess);

  published
    class function Lua_add(L: TLuaState): Integer; static; cdecl;
    class function Lua_remove(L: TLuaState): Integer; static; cdecl;

  end;

  TLuaEnumWrapper<T: record > = class(TLuaWrapper<T>)
  public
    class constructor Initialize;

    class procedure Push(L: TLuaState; AValue: T); override;

    class function EnumToInt(AValue: T): Integer;
    class function IntToEnum(AInteger: Integer): T;

    class function RecordName: AnsiString; override;

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
    L.Error(L.BadTypeString(RecordName, L.TypeNameAt(AIndex)));
end;

class function TLuaWrapper<T>.CheckArg(L: TLuaState; AIndex: Integer): PData;
begin
  if not TryConvertType(L, Result, AIndex) then
    L.Error(L.BadArgString(AIndex, RecordName, L.TypeNameAt(AIndex)));
end;

class function TLuaWrapper<T>.CheckArgX(L: TLuaState; AIndex: Integer): Pointer;
begin
  Result := CheckArg(L, AIndex);
end;

class function TLuaWrapper<T>.CheckX(L: TLuaState; AIndex: Integer): Pointer;
begin
  Result := Check(L, AIndex);
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
  MetaEvent: TLuaMetatableEvent;
  Func: TLuaCFunction;
  RttiMethod: TRttiMethod;
begin
  // (1) Create record userdata
  PData(L.NewUserdata(SizeOf(T)))^ := AValue; // +
  L.SetNameDebug(RecordName);

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
    L.SetNameDebug(RecordName + ' metatable');

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

    L.SetTable(LUA_REGISTRYINDEX); // -2
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

class procedure TLuaEvent<TAccess, TInfo>.PushParamsX(L: TLuaState; AInfo: TObject);
begin
  PushParams(L, TInfo(AInfo));
end;

class function TLuaEvent<TAccess, TInfo>.RecordName: AnsiString;
begin
  Result := 'event';
end;

class procedure TLuaEvent<TAccess, TInfo>.ClearEvents(L: TLuaState; AEvent: TAccess);
begin
  L.PushLightuserdata(PPointer(@AEvent)^);

  L.PushValue;
  if L.GetTable(LUA_REGISTRYINDEX) <> ltNil then
  begin
    L.PushNil;
    L.SetMetatable(-2);
  end;

  L.PushNil;
  L.SetTable(LUA_REGISTRYINDEX);
end;

class function TLuaEvent<TAccess, TInfo>.Lua_add(L: TLuaState): Integer;
var
  Data: PData;
  Wrapper: PFunctionWrapper;
  Handler: TEvent<TInfo>.THandler;
  Self: TLuaWrapperClass;
begin
  Self := GetClass(L, 1);
  if (Self = nil) or not(Self.InheritsFrom(TLuaEvent<TAccess, TInfo>)) then
    L.Error(L.BadArgString(1, RecordName, L.TypeNameAt(1)));

  Data := Self.CheckX(L, 1);
  L.CheckArg(2, ltFunction);

  // use the actual event inside of the access
  L.PushLightuserdata(PPointer(Data)^);
  if L.GetTable(LUA_REGISTRYINDEX) = ltNil then
  begin
    L.Pop;
    L.NewTable;
    L.SetNameDebug('event handler list');
    L.PushLightuserdata(PPointer(Data)^);
    L.PushValue(-2);

    L.NewTable;
    L.SetNameDebug('event handler list metatable');
    L.PushValue(1);
    // L.PushValue(-3);
    L.PushCClosure(Wrapper.Remove, 1);
    L.SetField('__gc', -2);
    L.SetMetatable(-2);

    L.SetTable(LUA_REGISTRYINDEX);

  end;

  L.PushValue(2);
  if L.GetTable(-2) <> ltNil then
    L.Error('event handler exists already');
  L.Pop;

  Wrapper := L.NewUserdata(SizeOf(TFunctionWrapper));
  Wrapper.L := L;
  Wrapper.PushParamsFunc := Self.PushParamsX;
  Wrapper.Event := PPointer(Data)^;

  Handler := Wrapper.Call;
  TEvent<TInfo>.TAccess(Pointer(Data)^).Add(Handler);

  L.PushLightuserdata(Wrapper);
  L.PushValue;
  L.PushValue(2);
  L.SetTable(3);

  L.PushValue(2);
  L.PushValue(4);
  L.SetTable(3);

  Result := 0;
end;

class function TLuaEvent<TAccess, TInfo>.Lua_remove(L: TLuaState): Integer;
var
  Data: PData;
  Wrapper: PFunctionWrapper;
  Handler: TEvent<TInfo>.THandler;
  Self: TLuaWrapperClass;
begin
  Self := GetClass(L, 1);
  if (Self = nil) or not(Self.InheritsFrom(TLuaEvent<TAccess, TInfo>)) then
    L.Error(L.BadArgString(1, RecordName, L.TypeNameAt(1)));

  Data := Self.CheckX(L, 1);
  L.CheckArg(2, ltFunction);

  L.PushLightuserdata(PPointer(Data)^);
  if L.GetTable(LUA_REGISTRYINDEX) <> ltNil then
  begin
    L.PushValue(2);
    if L.GetTable(-2) <> ltNil then
    begin
      Wrapper := L.ToUserdata;

      Handler := Wrapper.Call;
      TEvent<TInfo>.TAccess(Pointer(Data)^).Remove(Handler);

      L.PushLightuserdata(Wrapper);
      L.PushNil;
      L.SetTable(3);

      L.PushValue(2);
      L.PushNil;
      L.SetTable(3);

      L.PushNil;
      if not L.Next(3) then
      begin
        L.PushNil;
        L.SetMetatable(3);

        L.PushLightuserdata(PPointer(Data)^);
        L.PushNil;
        L.SetTable(LUA_REGISTRYINDEX);
      end;

      Exit(0);
    end;
  end;

  Exit(L.Error('event handler not found'));
end;

{ TLuaEvent<T>.TFunctionWrapper }

procedure TLuaEvent<TAccess, TInfo>.TFunctionWrapper.Call(AInfo: TInfo);
var
  OldTop: Integer;
begin
  L.PushLightuserdata(Event);
  L.GetTable(LUA_REGISTRYINDEX);

  L.PushLightuserdata(@Self);
  L.GetTable(-2);

  OldTop := L.Top;
  PushParamsFunc(L, AInfo);
  L.Call(L.Top - OldTop, 0);
end;

class function TLuaEvent<TAccess, TInfo>.TFunctionWrapper.Remove(L: TLuaState): Integer;
var
  Data: ^TEvent<TInfo>.TAccess;
  Self: PFunctionWrapper;
  Handler: TEvent<TInfo>.THandler;
begin
  Data := L.ToUserdata(L.UpvalueIndex(1));
  L.PushNil;
  while L.Next(1) do
  begin
    if L.IsUserdata then
    begin
      Self := L.ToUserdata;
      Handler := Self.Call;
      Data^.Remove(Handler);
    end;
    L.Pop;
  end;

  Result := 0;
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

class function TLuaWrapper.RecordName: AnsiString;
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

class constructor TLuaEnumWrapper<T>.Initialize;
begin
  Assert(GetTypeKind(T) = tkEnumeration);
  Assert(TypeInfo(T) <> nil, 'enumeration must start at zero and be continuous');
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

class function TLuaEnumWrapper<T>.RecordName: AnsiString;
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
  begin
    TLuaEnumWrapper<T>.Push(L, Value);
    Result := 1;
  end
  else
    Result := 0;
end;

class function TLuaLibEnum<T>.__len(L: TLuaState): Integer;
begin
  L.PushInteger(GetTypeData(TypeInfo(T)).MaxValue + 1);
  Result := 1;
end;

end.
