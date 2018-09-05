unit Pengine.LuaObject;

interface

uses
  System.RTTI,
  System.SysUtils,
  System.TypInfo,

  Pengine.LuaHeader,
  Pengine.Lua;

type

{$M+}

  TLuaObject = class
  public type

    PObject = ^TObject;

  private
    class var

      R: TRttiContext;

  private
    class function LuaMetaIndex(L: TLuaState): Integer; static; cdecl;
    class function LuaMetaNewIndex(L: TLuaState): Integer; static; cdecl;
    class function LuaMetaToString(L: TLuaState): Integer; static; cdecl;

    class function PushValue(L: TLuaState; AValue: TValue; AName: string): Integer; static;
    class function Index(Self: TObject; L: TLuaState; AProperty: TRttiProperty): Integer; overload; static;
    class function Index(Self: TObject; L: TLuaState; AMethods: TArray<TRttiMethod>): Integer; overload; static;
    class function Index(Self: TObject; L: TLuaState; AIndexedPropery: TRttiIndexedProperty): Integer; overload; static;
    class function Index(Self: TObject; L: TLuaState; AField: TRttiField): Integer; overload; static;

    class function NewIndex(Self: TObject; L: TLuaState; AProperty: TRttiProperty): Integer; overload; static;
    class function NewIndex(Self: TObject; L: TLuaState; AMethods: TArray<TRttiMethod>): Integer; overload; static;
    class function NewIndex(Self: TObject; L: TLuaState; AIndexedPropery: TRttiIndexedProperty): Integer;
      overload; static;
    class function NewIndex(Self: TObject; L: TLuaState; AField: TRttiField): Integer; overload; static;

  public
    class constructor Create;

    class procedure Push(L: TLuaState; AObject: TObject);

  end;

implementation

{ TLuaObject }

class procedure TLuaObject.Push(L: TLuaState; AObject: TObject);
var
  SelfPointer: ^TObject;
begin
  SelfPointer := L.NewUserdata(SizeOf(Pointer));
  SelfPointer^ := AObject;

  L.CreateTable(0, 4);
  L.PushCFunction(LuaMetaIndex);
  L.SetField('__index', -2);
  L.PushCFunction(LuaMetaNewIndex);
  L.SetField('__newindex', -2);
  L.PushBoolean(False);
  L.SetField('__metatable', -2);
  L.PushCFunction(LuaMetaToString);
  L.SetField('__tostring', -2);

  L.SetMetatable(-2);
end;

class function TLuaObject.PushValue(L: TLuaState; AValue: TValue; AName: string): Integer;
begin
  case AValue.Kind of
    tkInteger:
      L.PushInteger(AValue.AsInteger);
    tkInt64:
      L.PushInteger(AValue.AsInt64);
    tkFloat:
      L.PushNumber(AValue.AsType<Double>);
    tkString, tkLString, tkWString, tkUString:
      // TODO: Handle non-ansi strings differently
      L.PushString(PAnsiChar(AValue.AsType<AnsiString>));
    tkPointer:
      L.PushLightuserdata(AValue.AsType<Pointer>);
    tkEnumeration:
      if AValue.IsType<Boolean> then
        L.PushBoolean(AValue.AsBoolean)
      else
        // TODO: enums
        Exit(L.ErrorFmt('Can''t read %s, as the enumtype %s is not supported.', [AName, AValue.TypeInfo.Name]));
    tkClass:
      TLuaObject.Push(L, AValue.AsObject);
    // tkChar: ;
    // tkSet: ;
    // tkMethod: ;
    // tkWChar: ;
    // tkVariant: ;
    // tkArray: ;
    // tkRecord: ;
    // tkInterface: ;
    // tkDynArray: ;
    // tkClassRef: ;
    // tkProcedure: ;
  else
    Exit(L.ErrorFmt('Can''t read %s, as the type %s is not supported.', [AName, AValue.TypeInfo.Name]));
  end;
  Result := 1;
end;

class function TLuaObject.Index(Self: TObject; L: TLuaState; AProperty: TRttiProperty): Integer;
begin
  if not AProperty.IsReadable then
    Exit(L.ErrorFmt('Property "%s" is not readable.', [AProperty.Name]));
  Result := PushValue(L, AProperty.GetValue(Self), 'property "' + AProperty.Name + '"');
end;

class function TLuaObject.Index(Self: TObject; L: TLuaState; AMethods: TArray<TRttiMethod>): Integer;
begin
  Exit(L.Error('Indexing of methods not implemented.'));
end;

class function TLuaObject.Index(Self: TObject; L: TLuaState; AIndexedPropery: TRttiIndexedProperty): Integer;
begin
  Exit(L.Error('Indexing of indexed properties not implemented.'));
end;

class constructor TLuaObject.Create;
begin
  R := TRttiContext.Create;
end;

class function TLuaObject.Index(Self: TObject; L: TLuaState; AField: TRttiField): Integer;
begin
  Result := PushValue(L, AField.GetValue(Self), 'field "' + AField.Name + '"');
end;

class function TLuaObject.LuaMetaIndex(L: TLuaState): Integer;
var
  Self: TObject;
  Name: string;
  SelfType: TRttiType;
  Prop: TRttiProperty;
  Methods: TArray<TRttiMethod>;
  IndexedProp: TRttiIndexedProperty;
  Field: TRttiField;
  I: Integer;
begin
  TLua.FromState(L).Interlock;
  try
    L.CheckType(2, ltString);
    Name := string(L.ToString_X);
    Self := PObject(L.ToUserdata(1))^;

    SelfType := R.GetType(Self.ClassInfo);

    Prop := SelfType.GetProperty(Name);
    if (Prop <> nil) and (Prop.Visibility = mvPublished) then
      Exit(Index(Self, L, Prop));

    Methods := SelfType.GetMethods(Name);
    for I := Length(Methods) - 1 downto 0 do
      if Methods[I].Visibility <> mvPublished then
        Delete(Methods, I, 1);
    if Length(Methods) <> 0 then
      Exit(Index(Self, L, Methods));

    IndexedProp := SelfType.GetIndexedProperty(Name);
    if IndexedProp <> nil then
      Exit(Index(Self, L, IndexedProp));

    Field := SelfType.GetField(Name);
    if Field <> nil then
      Exit(Index(Self, L, Field));

    Exit(L.ErrorFmt('Invalid class member "%s"', [Name]));

  finally
    TLua.FromState(L).Unlock;
  end;
end;

class function TLuaObject.LuaMetaNewIndex(L: TLuaState): Integer;
var
  Self: TObject;
  Name: string;
  SelfType: TRttiType;
  Prop: TRttiProperty;
  Methods: TArray<TRttiMethod>;
  IndexedProp: TRttiIndexedProperty;
  Field: TRttiField;
  I: Integer;
begin
  TLua.FromState(L).Interlock;
  try
    L.CheckType(2, ltString);
    Name := string(L.ToString_X(2));
    Self := PObject(L.ToUserdata(1))^;

    SelfType := R.GetType(Self.ClassInfo);

    Prop := SelfType.GetProperty(Name);
    if (Prop <> nil) and (Prop.Visibility = mvPublished) then
      Exit(NewIndex(Self, L, Prop));

    Methods := SelfType.GetMethods(Name);
    for I := Length(Methods) - 1 downto 0 do
      if Methods[I].Visibility <> mvPublished then
        Delete(Methods, I, 1);
    if Length(Methods) <> 0 then
      Exit(NewIndex(Self, L, Methods));

    IndexedProp := SelfType.GetIndexedProperty(Name);
    if IndexedProp <> nil then
      Exit(NewIndex(Self, L, IndexedProp));

    Field := SelfType.GetField(Name);
    if Field <> nil then
      Exit(NewIndex(Self, L, Field));

    Exit(L.ErrorFmt('Invalid class member "%s"', [Name]));

  finally
    TLua.FromState(L).Unlock;
  end;
end;

class function TLuaObject.LuaMetaToString(L: TLuaState): Integer;
var
  Self: TObject;
begin
  Self := PObject(L.ToUserdata(1))^;
  L.PushString(PAnsiChar(AnsiString(Self.ClassName)));
  Result := 1;
end;

class function TLuaObject.NewIndex(Self: TObject; L: TLuaState; AProperty: TRttiProperty): Integer;
var
  ErrorFlag: Boolean;
begin
  if not AProperty.IsWritable then
    Exit(L.ErrorFmt('Property "%s" is not writable.', [AProperty.Name]));

  ErrorFlag := False;
  try
    case L.&Type of
      ltBoolean:
        AProperty.SetValue(Self, L.ToBoolean);
      ltNumber:
        if L.IsInteger then
          AProperty.SetValue(Self, L.ToInteger)
        else
          AProperty.SetValue(Self, L.ToNumber);
      ltString:
        AProperty.SetValue(Self, TValue.From<AnsiString>(L.ToString));
    else
      ErrorFlag := True;
    end;
  except
    on E: EInvalidCast do
      ErrorFlag := True;
  end;

  if ErrorFlag then
    Exit(L.ErrorFmt('Can''t write %s to property "%s" of type %s',
      [L.TypeNameAt, AProperty.Name, AProperty.PropertyType.Name]));

  Result := 0;
end;

class function TLuaObject.NewIndex(Self: TObject; L: TLuaState; AMethods: TArray<TRttiMethod>): Integer;
begin
  Exit(L.Error('Cannot change a method.'));
end;

class function TLuaObject.NewIndex(Self: TObject; L: TLuaState; AIndexedPropery: TRttiIndexedProperty): Integer;
begin
  Exit(L.Error('Assigning of indexed properties not implemented.'));
end;

class function TLuaObject.NewIndex(Self: TObject; L: TLuaState; AField: TRttiField): Integer;
begin
  Exit(L.Error('Assigning of fields not implemented.'));
end;

end.
