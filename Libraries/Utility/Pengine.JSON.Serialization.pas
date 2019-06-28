unit Pengine.JSON.Serialization;

{$POINTERMATH ON}

interface

uses
  System.SysUtils,
  System.TypInfo,

  Pengine.Collections,
  Pengine.ICollections,
  Pengine.JSON,
  Pengine.Utility;

type

  EJSerialization = class(Exception);

  TJSerializer = class;

  IJSerializable = interface
    function GetJVersion: Integer;
    procedure DefineJStorage(ASerializer: TJSerializer);

  end;

  TJStaticSerializerClass = class of TJStaticSerializer;

  TJStaticSerializer = class(TObject);

  TJStaticSerializer<T> = class(TJStaticSerializer)
  public
    class function Serialize(AValue: T): TJValue; virtual; abstract;
    class function Unserialize(AJValue: TJValue): T; virtual; abstract;

  end;

  TJSerializer = class
  public type

    TMode = (smSerialize, smUnserialize);

  private
    FMode: TMode;
    FValue: TJObject;
    FVersion: Integer;

    constructor Create(AValue: TJObject); overload;
    constructor Create(AVersion: Integer); overload;

  public
    destructor Destroy; override;

    class function Serialize(AObject: IJSerializable; AVersion: Integer = -1): TJObject;
    class procedure Unserialize(AObject: IJSerializable; AValue: TJObject);

    property Mode: TMode read FMode;
    function IsStoring: Boolean;
    function IsLoading: Boolean;

    property Value: TJObject read FValue;
    function OwnValue: TJObject;

    property Version: Integer read FVersion;

    // Primitives
    procedure Define(AName: string; var ANumber: Int64); overload;
    procedure Define(AName: string; var ANumber: Integer); overload;
    procedure Define(AName: string; var ANumber: Single); overload;
    procedure Define(AName: string; var ANumber: Double); overload;
    procedure Define(AName: string; var AText: string); overload;
    procedure Define(AName: string; var AValue: Boolean); overload;

    // Nullable Primitives
    procedure DefineNullable(AName: string; var ANumber: TOpt<Int64>); overload;
    procedure DefineNullable(AName: string; var ANumber: TOpt<Integer>); overload;
    procedure DefineNullable(AName: string; var ANumber: TOpt<Single>); overload;
    procedure DefineNullable(AName: string; var ANumber: TOpt<Double>); overload;
    procedure DefineNullable(AName: string; var AText: TOpt<string>); overload;
    procedure DefineNullable(AName: string; var AValue: TOpt<Boolean>); overload;

    // Primitive Arrays
    procedure DefineArray(AName: string; AArray: TArray<Integer>); overload;
    procedure DefineArray(AName: string; AArray: TArray<Int64>); overload;
    procedure DefineArray(AName: string; AArray: TArray<Single>); overload;
    procedure DefineArray(AName: string; AArray: TArray<Double>); overload;
    procedure DefineArray(AName: string; AArray: TArray<string>); overload;
    procedure DefineArray(AName: string; AArray: TArray<Boolean>); overload;

    // Primitive Collections
    procedure DefineCollection(AName: string; ACollection: ICollection<Integer>); overload;
    procedure DefineCollection(AName: string; ACollection: ICollection<Int64>); overload;
    procedure DefineCollection(AName: string; ACollection: ICollection<Single>); overload;
    procedure DefineCollection(AName: string; ACollection: ICollection<Double>); overload;
    procedure DefineCollection(AName: string; ACollection: ICollection<string>); overload;
    procedure DefineCollection(AName: string; ACollection: ICollection<Boolean>); overload;

    // Serializable Objects
    procedure Define(AName: string; AObject: IJSerializable); overload;
    procedure DefineNullable<T: constructor, IJSerializable>(AName: string; var AObject: T); overload;
    procedure DefineNullable<T: IJSerializable>(AName: string; var AObject: T; AInstantiator: TFunc<T>); overload;

    // Static Serializer
    procedure DefineCustom<T; S: TJStaticSerializer<T> //
      >(AName: string; var AValue: T); overload;

    // Serializable Arrays
    procedure DefineArray<T: constructor, IJSerializable>(AName: string; AArray: TArray<T>); overload;
    procedure DefineArray<T: IJSerializable>(AName: string; AArray: TArray<T>; AInstantiator: TFunc<T>); overload;

    // Serializable Collections
    procedure DefineCollection<T: constructor, IJSerializable>(AName: string; ACollection: ICollection<T>); overload;
    procedure DefineCollection<T: IJSerializable>(AName: string; ACollection: ICollection<T>;
      AInstantiator: TFunc<T>); overload;

    // TODO: ICollection<TPair<string, T>> primitive and serializable

    // Enum
    procedure DefineEnum<T: record >(AName: string; var AEnum: T; AEnumToName: TFunc<T, string>;
      ANameToEnum: TFunc<string, T>); overload;
    procedure DefineEnum<T: record >(AName: string; var AEnum: T; const ANames: array of string); overload;

    // Set
    procedure DefineSet<T>(AName: string; var ASet: T; AEnumToName: TFunc<Byte, string>;
      ANameToEnum: TFunc<string, Byte>); overload;
    procedure DefineSet<T>(AName: string; var ASet: T; const ANames: array of string); overload;

  end;

implementation

{ TJSerializer }

constructor TJSerializer.Create(AValue: TJObject);
begin
  FMode := smUnserialize;
  FValue := AValue;
  FVersion := Value['_VERSION'] or 0;
end;

constructor TJSerializer.Create(AVersion: Integer);
begin
  FMode := smSerialize;
  FValue := TJObject.Create;
  FVersion := AVersion;
  if Version <> 0 then
    Value['_VERSION'] := Version;
end;

destructor TJSerializer.Destroy;
begin
  if IsStoring then
    FValue.Free;
  inherited;
end;

class function TJSerializer.Serialize(AObject: IJSerializable; AVersion: Integer): TJObject;
var
  Serializer: TJSerializer;
begin
  if AVersion = -1 then
    AVersion := AObject.GetJVersion;
  Serializer := TJSerializer.Create(AVersion);
  try
    AObject.DefineJStorage(Serializer);
    Result := Serializer.OwnValue;

  finally
    Serializer.Free;

  end;
end;

class procedure TJSerializer.Unserialize(AObject: IJSerializable; AValue: TJObject);
var
  Serializer: TJSerializer;
begin
  Serializer := TJSerializer.Create(AValue);
  try
    AObject.DefineJStorage(Serializer);

  finally
    Serializer.Free;

  end;
end;

function TJSerializer.IsStoring: Boolean;
begin
  Result := Mode = smSerialize;
end;

function TJSerializer.IsLoading: Boolean;
begin
  Result := Mode = smUnserialize;
end;

function TJSerializer.OwnValue: TJObject;
begin
  Result := FValue;
  FValue := nil;
end;

procedure TJSerializer.Define(AName: string; var ANumber: Int64);
begin
  case Mode of
    smSerialize:
      Value[AName] := ANumber;
    smUnserialize:
      ANumber := Value[AName];
  end;
end;

procedure TJSerializer.Define(AName: string; var ANumber: Integer);
begin
  case Mode of
    smSerialize:
      Value[AName] := ANumber;
    smUnserialize:
      ANumber := Value[AName];
  end;
end;

procedure TJSerializer.Define(AName: string; var ANumber: Single);
begin
  case Mode of
    smSerialize:
      Value[AName] := ANumber;
    smUnserialize:
      ANumber := Value[AName];
  end;
end;

procedure TJSerializer.Define(AName: string; var ANumber: Double);
begin
  case Mode of
    smSerialize:
      Value[AName] := ANumber;
    smUnserialize:
      ANumber := Value[AName];
  end;
end;

procedure TJSerializer.Define(AName: string; var AText: string);
begin
  case Mode of
    smSerialize:
      Value[AName] := AText;
    smUnserialize:
      AText := Value[AName];
  end;
end;

procedure TJSerializer.Define(AName: string; var AValue: Boolean);
begin
  case Mode of
    smSerialize:
      Value[AName] := AValue;
    smUnserialize:
      AValue := Value[AName];
  end;
end;

procedure TJSerializer.DefineNullable(AName: string; var ANumber: TOpt<Int64>);
begin
  raise ENotImplemented.Create('Not implemented!');
end;

procedure TJSerializer.DefineNullable(AName: string; var ANumber: TOpt<Integer>);
begin
  raise ENotImplemented.Create('Not implemented!');
end;

procedure TJSerializer.DefineNullable(AName: string; var ANumber: TOpt<Single>);
begin
  raise ENotImplemented.Create('Not implemented!');
end;

procedure TJSerializer.DefineNullable(AName: string; var ANumber: TOpt<Double>);
begin
  raise ENotImplemented.Create('Not implemented!');
end;

procedure TJSerializer.DefineNullable(AName: string; var AText: TOpt<string>);
begin
  raise ENotImplemented.Create('Not implemented!');
end;

procedure TJSerializer.DefineNullable(AName: string; var AValue: TOpt<Boolean>);
begin
  raise ENotImplemented.Create('Not implemented!');
end;

procedure TJSerializer.DefineArray(AName: string; AArray: TArray<Integer>);
var
  JArray: TJArray;
  JValue: TJValue;
  Item: Integer;
begin
  case Mode of
    smSerialize:
      begin
        JArray := Value.AddArray(AName);
        for Item in AArray do
          JArray.Add(Item);
      end;
    smUnserialize:
      begin
        AArray.Clear;
        for JValue in Value[AName].AsArray do
          AArray.Add(JValue);
      end;
  end;
end;

procedure TJSerializer.DefineArray(AName: string; AArray: TArray<Int64>);
var
  JArray: TJArray;
  JValue: TJValue;
  Item: Int64;
begin
  case Mode of
    smSerialize:
      begin
        JArray := Value.AddArray(AName);
        for Item in AArray do
          JArray.Add(Item);
      end;
    smUnserialize:
      begin
        AArray.Clear;
        for JValue in Value[AName].AsArray do
          AArray.Add(JValue);
      end;
  end;
end;

procedure TJSerializer.DefineArray(AName: string; AArray: TArray<Single>);
var
  JArray: TJArray;
  JValue: TJValue;
  Item: Single;
begin
  case Mode of
    smSerialize:
      begin
        JArray := Value.AddArray(AName);
        for Item in AArray do
          JArray.Add(Item);
      end;
    smUnserialize:
      begin
        AArray.Clear;
        for JValue in Value[AName].AsArray do
          AArray.Add(JValue);
      end;
  end;
end;

procedure TJSerializer.DefineArray(AName: string; AArray: TArray<Double>);
var
  JArray: TJArray;
  JValue: TJValue;
  Item: Double;
begin
  case Mode of
    smSerialize:
      begin
        JArray := Value.AddArray(AName);
        for Item in AArray do
          JArray.Add(Item);
      end;
    smUnserialize:
      begin
        AArray.Clear;
        for JValue in Value[AName].AsArray do
          AArray.Add(JValue);
      end;
  end;
end;

procedure TJSerializer.DefineArray(AName: string; AArray: TArray<string>);
var
  JArray: TJArray;
  JValue: TJValue;
  Item: string;
begin
  case Mode of
    smSerialize:
      begin
        JArray := Value.AddArray(AName);
        for Item in AArray do
          JArray.Add(Item);
      end;
    smUnserialize:
      begin
        AArray.Clear;
        for JValue in Value[AName].AsArray do
          AArray.Add(JValue);
      end;
  end;
end;

procedure TJSerializer.DefineArray(AName: string; AArray: TArray<Boolean>);
begin
  raise ENotImplemented.Create('Not implemented!');
end;

procedure TJSerializer.DefineCollection(AName: string; ACollection: ICollection<Integer>);
var
  JArray: TJArray;
  Item: Integer;
  JValue: TJValue;
begin
  case Mode of
    smSerialize:
      begin
        JArray := Value.AddArray(AName);
        for Item in ACollection do
          JArray.Add(Item);
      end;
    smUnserialize:
      begin
        ACollection.Clear;
        for JValue in Value[AName].AsArray do
          ACollection.Add(JValue);
      end;
  end;
end;

procedure TJSerializer.DefineCollection(AName: string; ACollection: ICollection<Int64>);
begin
  raise ENotImplemented.Create('Not implemented!');
end;

procedure TJSerializer.DefineCollection(AName: string; ACollection: ICollection<Single>);
begin
  raise ENotImplemented.Create('Not implemented!');
end;

procedure TJSerializer.DefineCollection(AName: string; ACollection: ICollection<Double>);
begin
  raise ENotImplemented.Create('Not implemented!');
end;

procedure TJSerializer.DefineCollection(AName: string; ACollection: ICollection<string>);
begin
  raise ENotImplemented.Create('Not implemented!');
end;

procedure TJSerializer.DefineCollection(AName: string; ACollection: ICollection<Boolean>);
begin
  raise ENotImplemented.Create('Not implemented!');
end;

procedure TJSerializer.Define(AName: string; AObject: IJSerializable);
var
  JObject: TJObject;
begin
  case Mode of
    smSerialize:
      Value[AName] := Serialize(AObject, Version);
    smUnserialize:
      if Value.Get(AName, JObject) then
        Unserialize(AObject, JObject);
  end;
end;

procedure TJSerializer.DefineNullable<T>(AName: string; var AObject: T);
begin
  DefineNullable<T>(AName, AObject,
    function: T
    begin
      Result := T.Create;
    end);
end;

procedure TJSerializer.DefineNullable<T>(AName: string; var AObject: T; AInstantiator: TFunc<T>);
var
  JValue: TJValue;
begin
  case Mode of
    smSerialize:
      if AObject <> nil then
        Value[AName] := Serialize(AObject);
    smUnserialize:
      if Value.Get(AName, JValue) then
      begin
        AObject := AInstantiator;
        Unserialize(AObject, JValue);
      end;
  end;
end;

procedure TJSerializer.DefineCustom<T, S>(AName: string; var AValue: T);
var
  JValue: TJValue;
begin
  case Mode of
    smSerialize:
      Value[AName] := S.Serialize(AValue);
    smUnserialize:
      if Value.Get(AName, JValue) then
        AValue := S.Unserialize(JValue);
  end;
end;

procedure TJSerializer.DefineArray<T>(AName: string; AArray: TArray<T>);
begin
  DefineArray<T>(AName, AArray,
    function: T
    begin
      Result := T.Create;
    end);
end;

procedure TJSerializer.DefineArray<T>(AName: string; AArray: TArray<T>; AInstantiator: TFunc<T>);
var
  JArray: TJArray;
  Item: T;
  JItem: TJObject;
begin
  case Mode of
    smSerialize:
      begin
        JArray := Value.AddArray(AName);
        for Item in AArray do
          JArray.Add(TJSerializer.Serialize(Item));
      end;
    smUnserialize:
      if Value.Get(AName, JArray) then
      begin
        AArray.Clear;
        for JItem in JArray do
        begin
          Item := AInstantiator;
          TJSerializer.Unserialize(Item, JItem);
          AArray.Add(Item);
        end;
      end;
  end;
end;

procedure TJSerializer.DefineCollection<T>(AName: string; ACollection: ICollection<T>);
begin
  DefineCollection<T>(AName, ACollection,
    function: T
    begin
      Result := T.Create;
    end);
end;

procedure TJSerializer.DefineCollection<T>(AName: string; ACollection: ICollection<T>; AInstantiator: TFunc<T>);
var
  JArray: TJArray;
  Item: T;
  JValue: TJValue;
begin
  case Mode of
    smSerialize:
      begin
        JArray := Value.AddArray(AName);
        for Item in ACollection do
          JArray.Add(TJSerializer.Serialize(Item));
      end;
    smUnserialize:
      begin
        ACollection.Clear;
        for JValue in Value[AName].AsArray do
        begin
          Item := AInstantiator;
          TJSerializer.Unserialize(Item, JValue);
          ACollection.Add(Item);
        end;
      end;
  end;
end;

procedure TJSerializer.DefineEnum<T>(AName: string; var AEnum: T; AEnumToName: TFunc<T, string>;
ANameToEnum: TFunc<string, T>);
var
  EnumValue: Integer;
begin
  if GetTypeKind(T) <> tkEnumeration then
    raise EJSerialization.Create('Enumeration type expected.');

  case Mode of
    smSerialize:
      Value[AName] := AEnumToName(AEnum);
    smUnserialize:
      AEnum := ANameToEnum(Value[AName]);
  end;
end;

procedure TJSerializer.DefineEnum<T>(AName: string; var AEnum: T; const ANames: array of string);
var
  Names: PString;
begin
  if GetTypeKind(T) <> tkEnumeration then
    raise EJSerialization.Create('Enumeration type expected.');

  // ok, as the anonymous methods don't leave this scope
  Names := @ANames[0];
  DefineEnum<T>(AName, AEnum,
    function(AEnum: T): string
    var
      Index: Integer;
    begin
      case SizeOf(T) of
        1:
          Index := PByte(@AEnum)^;
        2:
          Index := PWord(@AEnum)^;
        4:
          Index := PInteger(@AEnum)^;
      end;

      Result := Names[Index];
    end,
    function(AName: string): T
    var
      Index: Integer;
      Found: Boolean;
    begin
      Found := False;
      for Index := GetTypeData(TypeInfo(T)).MinValue to GetTypeData(TypeInfo(T)).MaxValue do
      begin
        if AName = Names[Index] then
        begin
          Found := True;
          Break;
        end;
      end;

      if not Found then
        raise EJSerialization.Create('Invalid enum value.');

      case SizeOf(T) of
        1:
          PByte(@Result)^ := Index;
        2:
          PWord(@Result)^ := Index;
        4:
          PInteger(@Result)^ := Index;
      end;
    end
    );
end;

procedure TJSerializer.DefineSet<T>(AName: string; var ASet: T; AEnumToName: TFunc<Byte, string>;
ANameToEnum: TFunc<string, Byte>);
var
  JArray: TJArray;
  EnumType: PTypeInfo;
  B, SetOffset: Byte;
  JValue: TJValue;
begin
  if GetTypeKind(T) <> tkSet then
    raise EJSerialization.Create('Set type expected.');

  EnumType := GetTypeData(TypeInfo(T)).CompType^;

  // A set of a subrange will remove empty bytes at the start
  // Example: 10 .. 12 will not have 0 .. 9 be empty, but offset everything to omit 0 .. 7
  SetOffset := GetTypeData(EnumType).MinValue div 8;

  case Mode of
    smSerialize:
      begin
        JArray := Value.AddArray(AName);
        for B := GetTypeData(EnumType).MinValue to GetTypeData(EnumType).MaxValue do
        begin
          if PByte(@ASet)[B div 8 - SetOffset] shr (B mod 8) and 1 = 1 then
            JArray.Add(AEnumToName(B));
        end;
      end;
    smUnserialize:
      begin
        for JValue in Value[AName].AsArray do
        begin
          B := ANameToEnum(JValue);
          PByte(@ASet)[B div 8 - SetOffset] := PByte(@ASet)[B div 8 - SetOffset] or (1 shl (B mod 8));
        end;
      end;
  end;
end;

procedure TJSerializer.DefineSet<T>(AName: string; var ASet: T; const ANames: array of string);
var
  EnumType: PTypeInfo;
  Names: PString;
begin
  if GetTypeKind(T) <> tkSet then
    raise EJSerialization.Create('Set type expected.');

  EnumType := GetTypeData(TypeInfo(T)).CompType^;

  // ok, as the anonymous methods don't leave this scope
  Names := @ANames[0];
  DefineSet(AName, ASet,
    function(AEnum: Byte): string
    begin
      Result := Names[AEnum];
    end,
    function(AName: string): Byte
    begin
      for Result := GetTypeData(EnumType).MinValue to GetTypeData(EnumType).MaxValue do
        if AName = Names[Result] then
          Exit;
      raise EJSerialization.Create('Invalid set value.');
    end
    );
end;

end.
