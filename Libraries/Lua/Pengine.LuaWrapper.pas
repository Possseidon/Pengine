unit Pengine.LuaWrapper;

{$POINTERMATH ON}

interface

uses
  System.RTTI,
  System.SysUtils,
  System.TypInfo,

  Pengine.LuaHeader,
  Pengine.Lua,
  System.Math;

// TODO: More interlocking

type

  TLuaWrapper = class
  public const

    Visibility = mvPublic;

  public type

    PObjectRec = ^TObjectRec;

    TObjectRec = packed record
      Self: TObject;

      class operator Equal(A, B: TObjectRec): Boolean;

    end;

    PRecordRec = ^TRecordRec;

    TRecordRec = packed record
      TypeInfo: PTypeInfo;
      // [ record data ]
      function Data: Pointer;

    end;

    PEnumRec = ^TEnumRec;

    TEnumRec = packed record
      EnumType: PTypeInfo;

      class operator Equal(A, B: TEnumRec): Boolean;

    end;

    PEnumValueRec = ^TEnumValueRec;

    TEnumValueRec = packed record
      EnumType: PTypeInfo;
      Value: Integer;

      class operator Equal(A, B: TEnumValueRec): Boolean;

    end;

    PFunctionRec = ^TFunctionRec;

    TFunctionRec = packed record
      Func: TRttiMethod;
      Self: TValue;
    end;

    PEventRec = ^TEventRec;

    TEventRec = packed record
      Method: TValue;
    end;

  private
    class var

      R: TRttiContext;

  private
    class procedure HideMeta(L: TLuaState); static;
    class procedure AddFunc(L: TLuaState; AName: PAnsiChar; AFunc: TLuaCFunction); static;
    class function CheckUserdata(L: TLuaState; AEqFunc: TLuaCFunction; Index: Integer = -1): Boolean; static;
    class function ConvertArgs(L: TLuaState; AParameters: TArray<TRttiParameter>; AOffset: Integer)
      : TArray<TValue>; static;

    class function LuaObjectIndex(L: TLuaState): Integer; static; cdecl;
    class function LuaObjectNewIndex(L: TLuaState): Integer; static; cdecl;
    class function LuaObjectToString(L: TLuaState): Integer; static; cdecl;

    class function LuaRecordToString(L: TLuaState): Integer; static; cdecl;
    class function LuaRecordIndex(L: TLuaState): Integer; static; cdecl;
    class function LuaRecordNewIndex(L: TLuaState): Integer; static; cdecl;
    class function LuaRecordEq(L: TLuaState): Integer; static; cdecl;

    class function LuaEnumToString(L: TLuaState): Integer; static; cdecl;
    class function LuaEnumIndex(L: TLuaState): Integer; static; cdecl;
    class function LuaEnumLen(L: TLuaState): Integer; static; cdecl;
    class function LuaEnumEq(L: TLuaState): Integer; static; cdecl;

    class function LuaEnumValueIndex(L: TLuaState): Integer; static; cdecl;
    class function LuaEnumValueToString(L: TLuaState): Integer; static; cdecl;
    class function LuaEnumValueEq(L: TLuaState): Integer; static; cdecl;

    class function LuaFunctionToString(L: TLuaState): Integer; static; cdecl;
    class function LuaFunctionCall(L: TLuaState): Integer; static; cdecl;

    class function LuaEventToString(L: TLuaState): Integer; static; cdecl;
    class function LuaEventCall(L: TLuaState): Integer; static; cdecl;

    class function PushValue(L: TLuaState; AValue: TValue): Integer; static;
    class function GetValue(L: TLuaState; ATypeInfo: PTypeInfo; Index: Integer = -1): TValue; static;

    class function Index(L: TLuaState; AProperty: TRttiProperty; AInstance: TObject): Integer; overload; static;
    class function Index(L: TLuaState; AMethod: TRttiMethod; AInstance: TValue): Integer; overload; static;
    class function Index(L: TLuaState; AIndexedProperty: TRttiIndexedProperty; AInstance: TObject): Integer;
      overload; static;
    class function Index(L: TLuaState; AField: TRttiField; AInstance: TObject): Integer; overload; static;

    class function NewIndex(L: TLuaState; AProperty: TRttiProperty; AInstance: TObject): Integer; overload; static;
    class function NewIndex(L: TLuaState; AIndexedPropery: TRttiIndexedProperty; AInstance: TObject): Integer;
      overload; static;
    class function NewIndex(L: TLuaState; AField: TRttiField; AInstance: TObject): Integer; overload; static;

  public
    class constructor Create;

    class procedure PushObject(L: TLuaState; AObject: TObject);
    class procedure PushRecord(L: TLuaState; const ARecord; ATypeInfo: PTypeInfo); overload;
    class procedure PushRecord<T: record >(L: TLuaState; ARecord: T); overload;

    class procedure PushEnum(L: TLuaState; AEnum: PTypeInfo); overload;
    class procedure PushEnum<T>(L: TLuaState); overload;
    class procedure PushEnumValue(L: TLuaState; AEnum: PTypeInfo; AValue: Integer); overload;
    class procedure PushEnumValue<T>(L: TLuaState; AValue: Integer); overload;

    class procedure PushFunction(L: TLuaState; AFunc: TRttiMethod; AObject: TValue); overload;
    class procedure PushFunction(L: TLuaState; AFunc: string; AObject: TObject); overload;
    class procedure PushEvent(L: TLuaState; AType: TRttiInvokableType; AMethod: TValue);

  end;

implementation

{ TLuaObject }

class procedure TLuaWrapper.PushObject(L: TLuaState; AObject: TObject);
var
  Data: PObjectRec;
begin
  Data := L.NewUserdata(SizeOf(TObjectRec));
  Data.Self := AObject;

  L.CreateTable(0, 4);

  HideMeta(L);
  AddFunc(L, '__tostring', LuaObjectToString);
  AddFunc(L, '__index', LuaObjectIndex);
  AddFunc(L, '__newindex', LuaObjectNewIndex);

  L.SetMetatable(-2);
end;

class procedure TLuaWrapper.PushRecord(L: TLuaState; const ARecord; ATypeInfo: PTypeInfo);
var
  RecSize: Integer;
  Data: PRecordRec;
begin
  RecSize := ATypeInfo.TypeData.RecSize;
  Data := L.NewUserdata(SizeOf(TRecordRec) + RecSize);

  Data.TypeInfo := ATypeInfo;
  Move(PByte(ARecord), (Data.Data)^, RecSize);

  L.CreateTable(0, 1);
  AddFunc(L, '__tostring', LuaRecordToString);
  AddFunc(L, '__eq', LuaRecordEq);
  AddFunc(L, '__index', LuaRecordIndex);
  AddFunc(L, '__newindex', LuaRecordNewIndex);

  HideMeta(L);

  L.SetMetatable(-2);
end;

class procedure TLuaWrapper.PushRecord<T>(L: TLuaState; ARecord: T);
begin
  PushRecord(L, ARecord, TypeInfo(T));
end;

class procedure TLuaWrapper.PushEnum(L: TLuaState; AEnum: PTypeInfo);
var
  Data: PEnumRec;
begin
  Assert((AEnum <> nil) and (AEnum.Kind = tkEnumeration), 'Enum expected.');

  Data := L.NewUserdata(SizeOf(TEnumRec));
  Data.EnumType := AEnum;

  L.CreateTable(0, 5);

  HideMeta(L);
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
  Data: PEnumValueRec;
begin
  Assert(AEnum.Kind = tkEnumeration, 'Regular enum expected.');

  Data := L.NewUserdata(SizeOf(TEnumValueRec));
  Data.EnumType := AEnum;
  Data.Value := AValue;

  L.CreateTable(0, 4);

  HideMeta(L);
  AddFunc(L, '__tostring', LuaEnumValueToString);
  AddFunc(L, '__eq', LuaEnumValueEq);
  AddFunc(L, '__index', LuaEnumValueIndex);

  L.SetMetatable(-2);
end;

class procedure TLuaWrapper.PushEnumValue<T>(L: TLuaState; AValue: Integer);
begin
  PushEnumValue(L, TypeInfo(T), AValue);
end;

class procedure TLuaWrapper.PushEvent(L: TLuaState; AType: TRttiInvokableType; AMethod: TValue);
var
  Data: PEventRec;
begin
  Data := L.NewUserdata(SizeOf(TEventRec));
  Initialize(Data^);
  Data.Method := AMethod;

  L.CreateTable(0, 3);

  HideMeta(L);
  AddFunc(L, '__tostring', LuaEventToString);
  AddFunc(L, '__call', LuaEventCall);

  L.SetMetatable(-2);
end;

class procedure TLuaWrapper.PushFunction(L: TLuaState; AFunc: string; AObject: TObject);
var
  Method: TRttiMethod;
begin
  Method := R.GetType(AObject.ClassType).GetMethod(AFunc);
  Assert(Method <> nil, Format('Unknown function "%s.%s".', [AObject.ClassName, AFunc]));
  PushFunction(L, Method, AObject);
end;

class procedure TLuaWrapper.PushFunction(L: TLuaState; AFunc: TRttiMethod; AObject: TValue);
var
  Data: PFunctionRec;
begin
  Data := L.NewUserdata(SizeOf(TFunctionRec));
  Initialize(Data^);
  Data.Func := AFunc;
  Data.Self := AObject;

  L.CreateTable(0, 3);

  HideMeta(L);
  AddFunc(L, '__tostring', LuaFunctionToString);
  AddFunc(L, '__call', LuaFunctionCall);

  L.SetMetatable(-2);
end;

class procedure TLuaWrapper.AddFunc(L: TLuaState; AName: PAnsiChar; AFunc: TLuaCFunction);
begin
  L.PushCFunction(AFunc);
  L.SetField(AName, -2);
end;

class function TLuaWrapper.PushValue(L: TLuaState; AValue: TValue): Integer;
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
      if AValue.AsObject = nil then
        L.PushNil
      else
        TLuaWrapper.PushObject(L, AValue.AsObject);
    // tkChar: ;
    // tkSet: ;
    tkMethod:
      TLuaWrapper.PushEvent(L, TRttiInvokableType(R.GetType(AValue.TypeInfo)), AValue);
    // tkWChar: ;
    // tkVariant: ;
    // tkArray: ;
    tkRecord:
      TLuaWrapper.PushRecord(L, AValue.GetReferenceToRawData^, AValue.TypeInfo);
    // tkInterface: ;
    // tkDynArray: ;
    // tkClassRef: ;
    // tkProcedure: ;
  else
    Exit(L.ErrorFmt('the type %s is not supported', [AValue.TypeInfo.Name]));
  end;
  Result := 1;
end;

class function TLuaWrapper.Index(L: TLuaState; AProperty: TRttiProperty; AInstance: TObject): Integer;
begin
  if not AProperty.IsReadable then
    Exit(L.ErrorFmt('property "%s" is not readable', [AProperty.Name]));
  Result := PushValue(L, AProperty.GetValue(AInstance));
end;

class function TLuaWrapper.Index(L: TLuaState; AMethod: TRttiMethod; AInstance: TValue): Integer;
begin
  PushFunction(L, AMethod, AInstance);
  Result := 1;
end;

class function TLuaWrapper.Index(L: TLuaState; AIndexedProperty: TRttiIndexedProperty; AInstance: TObject): Integer;
begin
  Exit(L.Error('indexing of indexed properties not implemented'));
end;

class function TLuaWrapper.CheckUserdata(L: TLuaState; AEqFunc: TLuaCFunction; Index: Integer): Boolean;
begin
  if L.GetMetatable(Index) then
  begin
    L.GetField('__eq');
    Result := @L.ToCFunction(-1) = @AEqFunc;
    L.Pop(2);
  end
  else
  begin
    L.Pop(1);
    Result := False;
  end;
end;

class function TLuaWrapper.ConvertArgs(L: TLuaState; AParameters: TArray<TRttiParameter>; AOffset: Integer)
  : TArray<TValue>;
var
  I: Integer;
begin
  SetLength(Result, Length(AParameters));
  if L.Top - AOffset + 1 <> Length(AParameters) then
    L.ErrorFmt('expected %d arguments, got %d', [Length(AParameters), L.Top - AOffset + 1]);
  for I := 0 to Length(AParameters) - 1 do
  begin
    if AParameters[I].Flags - [pfConst, pfAddress, pfReference] <> [] then
      L.Error('unsupported function parameter flags');
    Result[I] := GetValue(L, AParameters[I].ParamType.Handle, AOffset + I);
  end;
end;

class constructor TLuaWrapper.Create;
begin
  R := TRttiContext.Create;
end;

class function TLuaWrapper.GetValue(L: TLuaState; ATypeInfo: PTypeInfo; Index: Integer): TValue;
var
  I, MinValue, MaxValue: TLuaInteger;
  EnumValue: PEnumValueRec;
  Name: PAnsiChar;
  Rec: PRecordRec;
begin
  case ATypeInfo.Kind of
    tkInteger, tkInt64:
      begin
        if not L.ToIntegerX(I, Index) then
          Exit(L.ErrorFmt('integer expected, got %s', [L.TypeNameAt(Index)]));
        if ATypeInfo.Kind = tkInt64 then
        begin
          MinValue := ATypeInfo.TypeData.MinInt64Value;
          MaxValue := ATypeInfo.TypeData.MaxInt64Value;
        end
        else
        begin
          MinValue := ATypeInfo.TypeData.MinValue;
          MaxValue := ATypeInfo.TypeData.MaxValue;
        end;
        if InRange(I, MinValue, MaxValue) then
          Result := I
        else
          Exit(L.ErrorFmt('value %d must be in range [%d, %d]', [I, MinValue, MaxValue]))
      end;
    // tkChar: ;
    tkEnumeration:
      case L.&Type(Index) of
        ltUserdata:
          begin
            if not CheckUserdata(L, LuaEnumValueEq, Index) then
              Exit(L.Error('value is not an enum value'));
            EnumValue := L.ToUserdata(Index);
            if ATypeInfo <> EnumValue.EnumType then
              Exit(L.ErrorFmt('incompatible enum types %s and %s',
                [GetTypeName(ATypeInfo), GetTypeName(EnumValue.EnumType)]));
            Result := TValue.FromOrdinal(ATypeInfo, EnumValue.Value);
          end;
        ltString:
          begin
            Name := L.ToString(Index);
            I := GetEnumValue(ATypeInfo, string(Name));
            if I = -1 then
              Exit(L.ErrorFmt('"%s" is not a valid value for enum %s', [Name, GetTypeName(ATypeInfo)]));
            Result := TValue.FromOrdinal(ATypeInfo, I)
          end;
        ltNumber:
          if L.ToIntegerX(I, Index) then
          begin
            MinValue := ATypeInfo.TypeData.MinValue;
            MaxValue := ATypeInfo.TypeData.MaxValue;
            if InRange(I, MinValue, MaxValue) then
              Result := TValue.FromOrdinal(ATypeInfo, I)
            else
              Exit(L.ErrorFmt('value %d must be in range [%d, %d]', [I, MinValue, MaxValue]))
          end
          else
            Exit(L.Error('number must be an integer'));
        ltBoolean:
          if ATypeInfo = TypeInfo(Boolean) then
            Result := L.ToBoolean(Index)
          else
            Exit(L.Error('boolean can only be assigned to boolean property'));
      else
        Exit(L.Error('enum can only be assigned by integer, string or enum value'));
      end;
    tkFloat:
      Result := L.ToNumber(Index);
    tkString, tkLString, tkWString, tkUString:
      Result := string(L.ToString(Index));
    // tkSet: ;
    // tkClass: ;
    // tkMethod: ;
    // tkWChar: ;
    // tkVariant: ;
    // tkArray: ;
    tkRecord:
      begin
        case L.&Type of
          ltUserdata:
            begin
              if not CheckUserdata(L, LuaRecordEq, Index) then
                Exit(L.Error('value is not a record value'));
              Rec := L.ToUserdata(Index);
              if ATypeInfo <> Rec.TypeInfo then
                Exit(L.ErrorFmt('incompatible record types %s and %s',
                  [GetTypeName(ATypeInfo), GetTypeName(Rec.TypeInfo)]));
              TValue.Make(Rec.Data, ATypeInfo, Result);
            end;
          // TODO: From table, init by zero and then set each value
        else
          Exit(L.Error('record can only be assigned with another record'));
        end;
      end;
    // tkInterface: ;
    // tkDynArray: ;
    // tkClassRef: ;
    // tkPointer: ;
    // tkProcedure: ;
  else
    Exit(L.ErrorFmt('cannot convert value for type %s', [GetTypeName(ATypeInfo)]));
  end;
end;

class procedure TLuaWrapper.HideMeta(L: TLuaState);
begin
  L.PushBoolean(False);
  L.SetField('__metatable', -2);
end;

class function TLuaWrapper.Index(L: TLuaState; AField: TRttiField; AInstance: TObject): Integer;
begin
  Result := PushValue(L, AField.GetValue(AInstance));
end;

class function TLuaWrapper.LuaEnumEq(L: TLuaState): Integer;
var
  A, B: PEnumRec;
begin
  A := L.ToUserdata(1);
  B := L.ToUserdata(2);
  L.PushBoolean(A^ = B^);
  Result := 1;
end;

class function TLuaWrapper.LuaEnumIndex(L: TLuaState): Integer;
var
  Name: string;
  Data: PEnumRec;
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
  Data: PEnumRec;
begin
  Data := L.ToUserdata(1);
  L.PushInteger(Data.EnumType.TypeData.MaxValue);
  Result := 1;
end;

class function TLuaWrapper.LuaEnumToString(L: TLuaState): Integer;
var
  Data: PEnumRec;
begin
  Data := L.ToUserdata(1);
  L.PushString(AnsiString(GetTypeName(Data.EnumType)));
  Result := 1;
end;

class function TLuaWrapper.LuaEnumValueEq(L: TLuaState): Integer;
var
  A, B: PEnumValueRec;
begin
  A := L.ToUserdata(1);
  B := L.ToUserdata(2);
  L.PushBoolean(A^ = B^);
  Result := 1;
end;

class function TLuaWrapper.LuaEnumValueIndex(L: TLuaState): Integer;
var
  Data: PEnumValueRec;
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
  Data: PEnumValueRec;
begin
  Data := L.ToUserdata(1);
  L.PushString(AnsiString(GetEnumName(Data.EnumType, Data.Value)));
  Result := 1;
end;

class function TLuaWrapper.LuaEventCall(L: TLuaState): Integer;
var
  Event: PEventRec;
  FuncResult: TValue;
  FuncType: TRttiInvokableType;
begin
  Event := L.ToUserdata(1);

  FuncType := TRttiInvokableType(R.GetType(Event.Method.TypeInfo));
  FuncResult := FuncType.Invoke(Event.Method, ConvertArgs(L, FuncType.GetParameters, 2));

  if FuncType.ReturnType = nil then
    L.PushNil
  else
    PushValue(L, FuncResult);

  Result := 1;
end;

class function TLuaWrapper.LuaEventToString(L: TLuaState): Integer;
var
  Event: PEventRec;
  FuncType: TRttiInvokableType;
begin
  Event := L.ToUserdata;
  FuncType := TRttiInvokableType(R.GetType(Event.Method.TypeInfo));
  L.PushString(AnsiString(FuncType.ToString));
  Result := 1;
end;

class function TLuaWrapper.LuaFunctionCall(L: TLuaState): Integer;
var
  Func: PFunctionRec;
  FuncResult: TValue;
begin
  Func := L.ToUserdata(1);

  if Func.Func.HasExtendedInfo and (Func.Func.MethodKind in [mkConstructor, mkDestructor]) then
    Exit(L.Error('calling a constructor or destructor is not allowed'));

  FuncResult := Func.Func.Invoke(Func.Self, ConvertArgs(L, Func.Func.GetParameters, 2));

  if Func.Func.ReturnType = nil then
    L.PushNil
  else
    PushValue(L, FuncResult);

  Result := 1;
end;

class function TLuaWrapper.LuaFunctionToString(L: TLuaState): Integer;
var
  Func: PFunctionRec;
begin
  Func := L.ToUserdata;
  L.PushString(AnsiString(Func.Func.ToString));
  Result := 1;
end;

class function TLuaWrapper.LuaObjectIndex(L: TLuaState): Integer;
var
  Data: PObjectRec;
  Name: string;
  SelfType: TRttiType;
  Prop: TRttiProperty;
  Method: TRttiMethod;
  IndexedProp: TRttiIndexedProperty;
  Field: TRttiField;
begin
  TLua.FromState(L).Interlock;
  try
    L.CheckType(2, ltString);
    Name := string(L.ToString_X);
    Data := L.ToUserdata(1);

    SelfType := R.GetType(Data.Self.ClassInfo);

    Prop := SelfType.GetProperty(Name);
    if (Prop <> nil) and (Prop.Visibility >= Visibility) then
      Exit(Index(L, Prop, Data.Self));

    Method := SelfType.GetMethod(Name);
    if (Method <> nil) and (Method.Visibility >= Visibility) then
      Exit(Index(L, Method, Data.Self));

    IndexedProp := SelfType.GetIndexedProperty(Name);
    if (IndexedProp <> nil) and (IndexedProp.Visibility >= Visibility) then
      Exit(Index(L, IndexedProp, Data.Self));

    Field := SelfType.GetField(Name);
    if (Field <> nil) and (Field.Visibility >= Visibility) then
      Exit(Index(L, Field, Data.Self));

    Exit(L.ErrorFmt('invalid class member "%s"', [Name]));

  finally
    TLua.FromState(L).Unlock;
  end;
end;

class function TLuaWrapper.LuaObjectNewIndex(L: TLuaState): Integer;
var
  Data: PObjectRec;
  Name: string;
  SelfType: TRttiType;
  Prop: TRttiProperty;
  IndexedProp: TRttiIndexedProperty;
  Field: TRttiField;
begin
  TLua.FromState(L).Interlock;
  try
    L.CheckType(2, ltString);
    Name := string(L.ToString_X(2));
    Data := L.ToUserdata(1);

    SelfType := R.GetType(Data.Self.ClassInfo);

    Prop := SelfType.GetProperty(Name);
    if (Prop <> nil) and (Prop.Visibility >= Visibility) then
      Exit(NewIndex(L, Prop, Data.Self));

    IndexedProp := SelfType.GetIndexedProperty(Name);
    if IndexedProp <> nil then
      Exit(NewIndex(L, IndexedProp, Data.Self));

    Field := SelfType.GetField(Name);
    if Field <> nil then
      Exit(NewIndex(L, Field, Data.Self));

    Exit(L.ErrorFmt('invalid class member "%s"', [Name]));

  finally
    TLua.FromState(L).Unlock;
  end;
end;

class function TLuaWrapper.LuaObjectToString(L: TLuaState): Integer;
var
  Data: PObjectRec;
begin
  Data := L.ToUserdata(1);
  L.PushString(AnsiString(Data.Self.ClassName));
  Result := 1;
end;

class function TLuaWrapper.LuaRecordEq(L: TLuaState): Integer;
begin
  Exit(L.Error('record equal is not implemented'));
end;

class function TLuaWrapper.LuaRecordIndex(L: TLuaState): Integer;
var
  Name: string;
  Data: PRecordRec;
  RecType: TRttiType;
  Prop: TRttiProperty;
  Method: TRttiMethod;
  IndexedProp: TRttiIndexedProperty;
  Field: TRttiField;
begin
  L.CheckType(2, ltString);
  Name := string(L.ToString_X);
  Data := L.ToUserdata(1);

  RecType := R.GetType(Data.TypeInfo);

  // RTTI doesn't contain record properties...

  Method := RecType.GetMethod(Name);
  if (Method <> nil) and (Method.Visibility >= Visibility) then
    Exit(Index(L, Method, TValue.From<Pointer>(Data.Data)));

  IndexedProp := RecType.GetIndexedProperty(Name);
  if (IndexedProp <> nil) and (IndexedProp.Visibility >= Visibility) then
    Exit(Index(L, IndexedProp, Data.Data));

  Field := RecType.GetField(Name);
  if (Field <> nil) and (Field.Visibility >= Visibility) then
    Exit(Index(L, Field, Data.Data));

  Exit(L.ErrorFmt('invalid class member "%s"', [Name]));

end;

class function TLuaWrapper.LuaRecordNewIndex(L: TLuaState): Integer;
var
  Name: string;
  Data: PRecordRec;
  SelfType: TRttiType;
  Prop: TRttiProperty;
  IndexedProp: TRttiIndexedProperty;
  Field: TRttiField;
begin
  L.CheckType(2, ltString);
  Name := string(L.ToString_X(2));
  Data := L.ToUserdata(1);

  SelfType := R.GetType(Data.TypeInfo);

  Prop := SelfType.GetProperty(Name);
  if (Prop <> nil) and (Prop.Visibility >= Visibility) then
    Exit(NewIndex(L, Prop, Data.Data));

  IndexedProp := SelfType.GetIndexedProperty(Name);
  if IndexedProp <> nil then
    Exit(NewIndex(L, IndexedProp, Data.Data));

  Field := SelfType.GetField(Name);
  if Field <> nil then
    Exit(NewIndex(L, Field, Data.Data));

  Exit(L.ErrorFmt('invalid class member "%s"', [Name]));

end;

class function TLuaWrapper.LuaRecordToString(L: TLuaState): Integer;
var
  Data: PRecordRec;
  Method: TRttiMethod;
begin
  Data := L.ToUserdata(1);
  Method := R.GetType(Data.TypeInfo).GetMethod('ToString');
  if (Method <> nil) and (Length(Method.GetParameters) = 0) then
    L.PushString(Method.Invoke(TValue.From<Pointer>(Data.Data), []).AsType<AnsiString>)
  else
    L.PushString(AnsiString(GetTypeName(Data.TypeInfo)));
  Result := 1;
end;

class function TLuaWrapper.NewIndex(L: TLuaState; AProperty: TRttiProperty; AInstance: TObject): Integer;
begin
  if not AProperty.IsWritable then
    Exit(L.ErrorFmt('property "%s" is not writable', [AProperty.Name]));

  AProperty.SetValue(AInstance, GetValue(L, AProperty.PropertyType.Handle));
  Result := 0;
end;

class function TLuaWrapper.NewIndex(L: TLuaState; AIndexedPropery: TRttiIndexedProperty; AInstance: TObject): Integer;
begin
  Exit(L.Error('assigning of indexed properties not implemented'));
end;

class function TLuaWrapper.NewIndex(L: TLuaState; AField: TRttiField; AInstance: TObject): Integer;
begin
  AField.SetValue(AInstance, GetValue(L, AField.FieldType.Handle));
  Result := 0;
end;

{ TLuaWrapper.TData }

class operator TLuaWrapper.TObjectRec.Equal(A, B: TObjectRec): Boolean;
begin
  Result := A.Self = B.Self;
end;

{ TLuaWrapper.TEnum }

class operator TLuaWrapper.TEnumRec.Equal(A, B: TEnumRec): Boolean;
begin
  Result := A.EnumType = B.EnumType;
end;

{ TLuaWrapper.TEnumValue }

class operator TLuaWrapper.TEnumValueRec.Equal(A, B: TEnumValueRec): Boolean;
begin
  Result := (A.EnumType = B.EnumType) and (A.Value = B.Value);
end;

{ TLuaWrapper.TRecordRec }

function TLuaWrapper.TRecordRec.Data: Pointer;
begin
  Result := PRecordRec(@Self) + 1;
end;

end.
