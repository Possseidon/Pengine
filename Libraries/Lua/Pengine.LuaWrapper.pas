unit Pengine.LuaWrapper;

interface

uses
  System.RTTI,
  System.SysUtils,
  System.TypInfo,

  Pengine.LuaHeader,
  Pengine.Lua,
  System.Math;

type

  TLuaWrapper = class
  public type

    PData = ^TData;

    TData = packed record
      Self: TObject;
      Visibility: TMemberVisibility;

      class operator Equal(A, B: TData): Boolean;

    end;

    PEnum = ^TEnum;

    TEnum = packed record
      EnumType: PTypeInfo;

      class operator Equal(A, B: TEnum): Boolean;

    end;

    PEnumValue = ^TEnumValue;

    TEnumValue = packed record
      EnumType: PTypeInfo;
      Value: Integer;

      class operator Equal(A, B: TEnumValue): Boolean;

    end;

    PFunction = ^TFunction;

    TFunction = packed record
      Func: TRttiMethod;
      Self: TObject;
    end;

  private
    class var

      R: TRttiContext;

  private
    class procedure AddFunc(L: TLuaState; AName: PAnsiChar; AFunc: TLuaCFunction); static;
    class function CheckUserdata(L: TLuaState; AEqFunc: TLuaCFunction): Boolean; static;

    class function LuaObjectIndex(L: TLuaState): Integer; static; cdecl;
    class function LuaObjectNewIndex(L: TLuaState): Integer; static; cdecl;
    class function LuaObjectToString(L: TLuaState): Integer; static; cdecl;

    class function LuaEnumToString(L: TLuaState): Integer; static; cdecl;
    class function LuaEnumIndex(L: TLuaState): Integer; static; cdecl;
    class function LuaEnumLen(L: TLuaState): Integer; static; cdecl;
    class function LuaEnumEq(L: TLuaState): Integer; static; cdecl;

    class function LuaEnumValueIndex(L: TLuaState): Integer; static; cdecl;
    class function LuaEnumValueToString(L: TLuaState): Integer; static; cdecl;
    class function LuaEnumValueEq(L: TLuaState): Integer; static; cdecl;

    class function LuaFunctionToString(L: TLuaState): Integer; static; cdecl;

    class function PushValue(AData: PData; L: TLuaState; AValue: TValue; AName: string): Integer; static;
    class function Index(AData: PData; L: TLuaState; AProperty: TRttiProperty): Integer; overload; static;
    class function Index(AData: PData; L: TLuaState; AMethods: TArray<TRttiMethod>): Integer; overload; static;
    class function Index(AData: PData; L: TLuaState; AIndexedPropery: TRttiIndexedProperty): Integer; overload; static;
    class function Index(AData: PData; L: TLuaState; AField: TRttiField): Integer; overload; static;

    class function NewIndex(AData: PData; L: TLuaState; AProperty: TRttiProperty): Integer; overload; static;
    class function NewIndex(AData: PData; L: TLuaState; AMethods: TArray<TRttiMethod>): Integer; overload; static;
    class function NewIndex(AData: PData; L: TLuaState; AIndexedPropery: TRttiIndexedProperty): Integer;
      overload; static;
    class function NewIndex(AData: PData; L: TLuaState; AField: TRttiField): Integer; overload; static;

  public
    class constructor Create;

    class procedure PushObject(L: TLuaState; AObject: TObject; AVisiblity: TMemberVisibility = mvPublished);
    class procedure PushEnum(L: TLuaState; AEnum: PTypeInfo); overload;
    class procedure PushEnum<T>(L: TLuaState); overload;
    class procedure PushEnumValue(L: TLuaState; AEnum: PTypeInfo; AValue: Integer); overload;
    class procedure PushEnumValue<T>(L: TLuaState; AValue: Integer); overload;
    class procedure PushFunction(L: TLuaState; AFunc: TRttiMethod; AObject: TObject);

  end;

implementation

{ TLuaObject }

class procedure TLuaWrapper.PushObject(L: TLuaState; AObject: TObject; AVisiblity: TMemberVisibility);
var
  Data: PData;
begin
  Data := L.NewUserdata(SizeOf(TData));
  Data.Self := AObject;
  Data.Visibility := AVisiblity;

  L.CreateTable(0, 4);

  L.PushBoolean(False);
  L.SetField('__metatable', -2);

  AddFunc(L, '__tostring', LuaObjectToString);
  AddFunc(L, '__index', LuaObjectIndex);
  AddFunc(L, '__newindex', LuaObjectNewIndex);

  L.SetMetatable(-2);
end;

class procedure TLuaWrapper.PushEnum(L: TLuaState; AEnum: PTypeInfo);
var
  Data: PEnum;
begin
  Assert((AEnum <> nil) and (AEnum.Kind = tkEnumeration), 'Enum expected.');

  Data := L.NewUserdata(SizeOf(TEnum));
  Data.EnumType := AEnum;

  L.CreateTable(0, 5);

  L.PushBoolean(False);
  L.SetField('__metatable', -2);

  AddFunc(L, '__tostring', LuaEnumToString);
  AddFunc(L, '__index', LuaEnumIndex);
  AddFunc(L, '__len', LuaEnumLen);
  AddFunc(L, '__eq', LuaEnumEq);

  L.SetMetatable(-2);
end;

class procedure TLuaWrapper.PushEnum<T>(L: TLuaState);
begin
  PushEnum(L, TypeInfo(T));
end;

class procedure TLuaWrapper.PushEnumValue(L: TLuaState; AEnum: PTypeInfo; AValue: Integer);
var
  Data: PEnumValue;
begin
  Assert(AEnum.Kind = tkEnumeration, 'Regular enum expected.');

  Data := L.NewUserdata(SizeOf(TEnumValue));
  Data.EnumType := AEnum;
  Data.Value := AValue;

  L.CreateTable(0, 4);

  L.PushBoolean(False);
  L.SetField('__metatable', -2);

  AddFunc(L, '__tostring', LuaEnumValueToString);
  AddFunc(L, '__eq', LuaEnumValueEq);
  AddFunc(L, '__index', LuaEnumValueIndex);

  L.SetMetatable(-2);
end;

class procedure TLuaWrapper.PushEnumValue<T>(L: TLuaState; AValue: Integer);
begin
  PushEnumValue(L, TypeInfo(T), AValue);
end;

class procedure TLuaWrapper.PushFunction(L: TLuaState; AFunc: TRttiMethod; AObject: TObject);
var
  Data: PFunction;
begin
  Data := L.NewUserdata(SizeOf(TFunction));
  Data.Func := AFunc;
  Data.Self := AObject;

  L.CreateTable(0, 2);

  L.PushBoolean(False);
  L.SetField('__metatable', -2);

  AddFunc(L, '__tostring', LuaFunctionToString);

  L.SetMetatable(-2);
end;

class procedure TLuaWrapper.AddFunc(L: TLuaState; AName: PAnsiChar; AFunc: TLuaCFunction);
begin
  L.PushCFunction(AFunc);
  L.SetField(AName, -2);
end;

class function TLuaWrapper.PushValue(AData: PData; L: TLuaState; AValue: TValue; AName: string): Integer;
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
      L.PushString(AValue.AsType<AnsiString>);
    tkPointer:
      L.PushLightuserdata(AValue.AsType<Pointer>);
    tkEnumeration:
      if AValue.IsType<Boolean> then
        L.PushBoolean(AValue.AsBoolean)
      else
        PushEnumValue(L, AValue.TypeInfo, AValue.AsOrdinal);
    tkClass:
      TLuaWrapper.PushObject(L, AValue.AsObject, AData.Visibility);
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
    Exit(L.ErrorFmt('can''t read %s, as the type %s is not supported', [AName, AValue.TypeInfo.Name]));
  end;
  Result := 1;
end;

class function TLuaWrapper.Index(AData: PData; L: TLuaState; AProperty: TRttiProperty): Integer;
begin
  if not AProperty.IsReadable then
    Exit(L.ErrorFmt('property "%s" is not readable', [AProperty.Name]));
  Result := PushValue(AData, L, AProperty.GetValue(AData.Self), 'property "' + AProperty.Name + '"');
end;

class function TLuaWrapper.Index(AData: PData; L: TLuaState; AMethods: TArray<TRttiMethod>): Integer;
begin
  if Length(AMethods) > 1 then
    Exit(L.Error('overloaded methods are not yet supported'));

  PushFunction(L, AMethods[0], AData.Self);
  Result := 1;
end;

class function TLuaWrapper.Index(AData: PData; L: TLuaState; AIndexedPropery: TRttiIndexedProperty): Integer;
begin
  Exit(L.Error('indexing of indexed properties not implemented'));
end;

class function TLuaWrapper.CheckUserdata(L: TLuaState; AEqFunc: TLuaCFunction): Boolean;
begin
  L.GetMetatable;
  L.GetField('__eq');
  Result := @L.ToCFunction(-1) = @AEqFunc;
  L.Pop(2);
end;

class constructor TLuaWrapper.Create;
begin
  R := TRttiContext.Create;
end;

class function TLuaWrapper.Index(AData: PData; L: TLuaState; AField: TRttiField): Integer;
begin
  Result := PushValue(AData, L, AField.GetValue(AData.Self), 'field "' + AField.Name + '"');
end;

class function TLuaWrapper.LuaEnumEq(L: TLuaState): Integer;
var
  A, B: PEnum;
begin
  A := L.ToUserdata(1);
  B := L.ToUserdata(2);
  L.PushBoolean(A^ = B^);
  Result := 1;
end;

class function TLuaWrapper.LuaEnumIndex(L: TLuaState): Integer;
var
  Name: string;
  Data: PEnum;
  Value: Integer;
  EnumTypeData: PTypeData;
begin
  Data := L.ToUserdata(1);
  if L.IsInteger(2) then
  begin
    Value := L.ToInteger;
    EnumTypeData := Data.EnumType.TypeData;
    if (Value < 0) or (Value > EnumTypeData.MaxValue) then
      Exit(L.ErrorFmt('%s enum only has values from 0 to %d, got %d',
        [GetTypeName(Data.EnumType), EnumTypeData.MaxValue, Value]));
  end
  else if L.&Type = ltString then
  begin
    Name := string(L.ToString_X);
    Value := GetEnumValue(Data.EnumType, Name);
    if Value = -1 then
      Exit(L.ErrorFmt('"%s" is not an enum value of %s', [Name, GetTypeName(Data.EnumType)]));
  end
  else
    Exit(L.ErrorFmt('enums can only be indexed by integer or string, got %s', [L.TypeNameAt]));
  PushEnumValue(L, Data.EnumType, Value);
  Result := 1;
end;

class function TLuaWrapper.LuaEnumLen(L: TLuaState): Integer;
var
  Data: PEnum;
begin
  Data := L.ToUserdata(1);
  L.PushInteger(Data.EnumType.TypeData.MaxValue);
  Result := 1;
end;

class function TLuaWrapper.LuaEnumToString(L: TLuaState): Integer;
var
  Data: PEnum;
begin
  Data := L.ToUserdata(1);
  L.PushString(AnsiString(GetTypeName(Data.EnumType)));
  Result := 1;
end;

class function TLuaWrapper.LuaEnumValueEq(L: TLuaState): Integer;
var
  A, B: PEnumValue;
begin
  A := L.ToUserdata(1);
  B := L.ToUserdata(2);
  L.PushBoolean(A^ = B^);
  Result := 1;
end;

class function TLuaWrapper.LuaEnumValueIndex(L: TLuaState): Integer;
var
  Data: PEnumValue;
  Name: string;
begin
  Data := L.ToUserdata(1);
  L.CheckType(2, ltString);
  Name := string(L.ToString(2));
  if Name = 'index' then
    L.PushInteger(Data.Value)
  else if Name = 'name' then
    L.PushString(AnsiString(GetEnumName(Data.EnumType, Data.Value)))
  else if Name = 'type' then
    PushEnum(L, Data.EnumType)
  else
    Exit(L.ErrorFmt('expected "name", "index" or "type", got "%s"', [Name]));
  Result := 1;
end;

class function TLuaWrapper.LuaEnumValueToString(L: TLuaState): Integer;
var
  Data: PEnumValue;
begin
  Data := L.ToUserdata(1);
  L.PushString(AnsiString(GetEnumName(Data.EnumType, Data.Value)));
  Result := 1;
end;

class function TLuaWrapper.LuaFunctionToString(L: TLuaState): Integer;
var
  Func: PFunction;
begin
  Func := L.ToUserdata;
  L.PushString(AnsiString(Func.Func.ToString));
  Result := 1;
end;

class function TLuaWrapper.LuaObjectIndex(L: TLuaState): Integer;
var
  Data: PData;
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
    Data := L.ToUserdata(1);

    SelfType := R.GetType(Data.Self.ClassInfo);

    Prop := SelfType.GetProperty(Name);
    if (Prop <> nil) and (Prop.Visibility >= Data.Visibility) then
      Exit(Index(Data, L, Prop));

    Methods := SelfType.GetMethods(Name);
    for I := Length(Methods) - 1 downto 0 do
      if Methods[I].Visibility < Data.Visibility then
        Delete(Methods, I, 1);
    if Length(Methods) <> 0 then
      Exit(Index(Data, L, Methods));

    IndexedProp := SelfType.GetIndexedProperty(Name);
    if (IndexedProp <> nil) and (IndexedProp.Visibility >= Data.Visibility) then
      Exit(Index(Data, L, IndexedProp));

    Field := SelfType.GetField(Name);
    if (Field <> nil) and (Field.Visibility >= Data.Visibility) then
      Exit(Index(Data, L, Field));

    Exit(L.ErrorFmt('invalid class member "%s"', [Name]));

  finally
    TLua.FromState(L).Unlock;
  end;
end;

class function TLuaWrapper.LuaObjectNewIndex(L: TLuaState): Integer;
var
  Data: PData;
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
    Data := L.ToUserdata(1);

    SelfType := R.GetType(Data.Self.ClassInfo);

    Prop := SelfType.GetProperty(Name);
    if (Prop <> nil) and (Prop.Visibility >= Data.Visibility) then
      Exit(NewIndex(Data, L, Prop));

    Methods := SelfType.GetMethods(Name);
    for I := Length(Methods) - 1 downto 0 do
      if Methods[I].Visibility < Data.Visibility then
        Delete(Methods, I, 1);
    if Length(Methods) <> 0 then
      Exit(NewIndex(Data, L, Methods));

    IndexedProp := SelfType.GetIndexedProperty(Name);
    if IndexedProp <> nil then
      Exit(NewIndex(Data, L, IndexedProp));

    Field := SelfType.GetField(Name);
    if Field <> nil then
      Exit(NewIndex(Data, L, Field));

    Exit(L.ErrorFmt('invalid class member "%s"', [Name]));

  finally
    TLua.FromState(L).Unlock;
  end;
end;

class function TLuaWrapper.LuaObjectToString(L: TLuaState): Integer;
var
  Data: PData;
begin
  Data := L.ToUserdata(1);
  L.PushString(AnsiString(Data.Self.ClassName));
  Result := 1;
end;

class function TLuaWrapper.NewIndex(AData: PData; L: TLuaState; AProperty: TRttiProperty): Integer;
var
  Value: TValue;
  I, MinValue, MaxValue: TLuaInteger;
  PropType: TRttiType;
  EnumValue: PEnumValue;
  Name: PAnsiChar;
begin
  if not AProperty.IsWritable then
    Exit(L.ErrorFmt('property "%s" is not writable', [AProperty.Name]));

  PropType := AProperty.PropertyType;

  case AProperty.PropertyType.TypeKind of
    tkInteger, tkInt64:
      begin
        if not L.ToIntegerX(I) then
          Exit(L.ErrorFmt('integer expected, got %s', [L.TypeNameAt]));
        if AProperty.PropertyType.TypeKind = tkInt64 then
        begin
          MinValue := TRttiInt64Type(PropType).MinValue;
          MaxValue := TRttiInt64Type(PropType).MaxValue;
        end
        else
        begin
          MinValue := TRttiOrdinalType(PropType).MinValue;
          MaxValue := TRttiOrdinalType(PropType).MaxValue;
        end;
        if InRange(I, MinValue, MaxValue) then
          Value := I
        else
          Exit(L.ErrorFmt('value %d must be in range [%d, %d]', [I, MinValue, MaxValue]))
      end;
    // tkChar: ;
    tkEnumeration:
      case L.&Type of
        ltUserdata:
          begin
            if not CheckUserdata(L, LuaEnumValueEq) then
              Exit(L.Error('value is not an enum value'));
            EnumValue := L.ToUserdata;
            if PropType.Handle <> EnumValue.EnumType then
              Exit(L.ErrorFmt('incompatible enum types %s and %s',
                [GetTypeName(PropType.Handle), GetTypeName(EnumValue.EnumType)]));
            Value := TValue.FromOrdinal(PropType.Handle, EnumValue.Value);
          end;
        ltString:
          begin
            Name := L.ToString;
            I := GetEnumValue(PropType.Handle, string(Name));
            if I = -1 then
              Exit(L.ErrorFmt('"%s" is not a valid value for enum %s', [Name, GetTypeName(PropType.Handle)]));
            Value := TValue.FromOrdinal(PropType.Handle, I)
          end;
        ltNumber:
          if L.ToIntegerX(I) then
          begin
            MinValue := PropType.Handle.TypeData.MinValue;
            MaxValue := PropType.Handle.TypeData.MaxValue;
            if InRange(I, MinValue, MaxValue) then
              Value := TValue.FromOrdinal(PropType.Handle, I)
            else
              Exit(L.ErrorFmt('value %d must be in range [%d, %d]', [I, MinValue, MaxValue]))
          end
          else
            Exit(L.Error('number must be an integer'));
        ltBoolean:
          if PropType.Handle = TypeInfo(Boolean) then
            Value := L.ToBoolean
          else
            Exit(L.Error('boolean can only be assigned to boolean property'));
      else
        Exit(L.Error('enum property can only be assigned by integer, string or enum value'));
      end;
    tkFloat:
      Value := L.ToNumber;
    tkString, tkLString, tkWString, tkUString:
      Value := string(L.ToString);
    // tkSet: ;
    // tkClass: ;
    // tkMethod: ;
    // tkWChar: ;
    // tkVariant: ;
    // tkArray: ;
    // tkRecord: ;
    // tkInterface: ;
    // tkDynArray: ;
    // tkClassRef: ;
    // tkPointer: ;
    // tkProcedure: ;
  else
    Exit(L.ErrorFmt('cannot assign value to property of type %s', [AProperty.PropertyType.Name]));
  end;

  AProperty.SetValue(AData.Self, Value);
  Result := 0;
end;

class function TLuaWrapper.NewIndex(AData: PData; L: TLuaState; AMethods: TArray<TRttiMethod>): Integer;
begin
  Exit(L.Error('cannot change a method'));
end;

class function TLuaWrapper.NewIndex(AData: PData; L: TLuaState; AIndexedPropery: TRttiIndexedProperty): Integer;
begin
  Exit(L.Error('assigning of indexed properties not implemented'));
end;

class function TLuaWrapper.NewIndex(AData: PData; L: TLuaState; AField: TRttiField): Integer;
begin
  Exit(L.Error('assigning of fields not implemented'));
end;

{ TLuaWrapper.TData }

class operator TLuaWrapper.TData.Equal(A, B: TData): Boolean;
begin
  Result := A.Self = B.Self;
end;

{ TLuaWrapper.TEnum }

class operator TLuaWrapper.TEnum.Equal(A, B: TEnum): Boolean;
begin
  Result := A.EnumType = B.EnumType;
end;

{ TLuaWrapper.TEnumValue }

class operator TLuaWrapper.TEnumValue.Equal(A, B: TEnumValue): Boolean;
begin
  Result := (A.EnumType = B.EnumType) and (A.Value = B.Value);
end;

end.
