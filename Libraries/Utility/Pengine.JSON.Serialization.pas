unit Pengine.JSON.Serialization;

{$POINTERMATH ON}

interface

uses
  System.SysUtils,
  System.TypInfo,

  Pengine.Collections,
  Pengine.ICollections,
  Pengine.JSON;

type

  EJSerialization = class(Exception);

  TJSerializer = class;

  IJSerializable = interface
    function GetJVersion: Integer;
    procedure DefineJStorage(ASerializer: TJSerializer);

  end;

  TJArraySerializer = class
  public
    procedure Serialize(AValue: TJArray); virtual; abstract;
    procedure Unserialize(AValue: TJArray); virtual; abstract;

  end;

  TJArraySerializer<T> = class(TJArraySerializer)
  private
    FData: T;

  protected
    property Data: T read FData;

  public
    constructor Create(AData: T);

  end;

  TJArrayRefSerializer = class
  public
    procedure Serialize(AValue: TJArray); virtual; abstract;
    procedure Unserialize(AValue: TJArray); virtual; abstract;

  end;

  TJArrayRefSerializer<T> = class(TJArrayRefSerializer)
  public type

    PT = ^T;

  private
    FData: PT;

  protected
    property Data: PT read FData;

  public
    constructor Create(AData: PT);

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

    // Normal Values
    procedure Define(AName: string; var ANumber: Int64); overload;
    procedure Define(AName: string; var ANumber: Integer); overload;
    procedure Define(AName: string; var ANumber: Single); overload;
    procedure Define(AName: string; var ANumber: Double); overload;
    procedure Define(AName: string; var AText: string); overload;
    procedure Define(AName: string; var AValue: Boolean); overload;

    // Serializable Objects
    procedure Define(AName: string; AObject: IJSerializable; AVersion: Integer = -1); overload;
    procedure Define(AName: string; ASerializer: TJArraySerializer); overload;
    procedure Define(AName: string; ASerializer: TJArrayRefSerializer); overload;

    // Arrays
    procedure DefineArray(AName: string; AArray: TArray<Integer>); overload;
    procedure DefineArray(AName: string; AArray: TArray<Int64>); overload;
    procedure DefineArray(AName: string; AArray: TArray<Single>); overload;
    procedure DefineArray(AName: string; AArray: TArray<Double>); overload;
    procedure DefineArray(AName: string; AArray: TArray<string>); overload;

    // Arrays of Serializable
    procedure DefineArray<T: constructor, IJSerializable>(AName: string; AArray: TArray<T>); overload;
    procedure DefineArray<T: IJSerializable>(AName: string; AArray: TArray<T>; AInstantiator: TFunc<T>); overload;

    procedure DefineList<T: constructor, IJSerializable>(AName: string; AList: IListBase<T>); overload;
    procedure DefineList<T: IJSerializable>(AName: string; AList: IListBase<T>; AInstantiator: TFunc<T>); overload;

    // Enum
    procedure DefineEnum<T: record >(AName: string; var AEnum: T; AEnumToName: TFunc<T, string>;
      ANameToEnum: TFunc<string, T>); overload;
    procedure DefineEnum<T: record; N>(AName: string; var AEnum: T; const ANames: N); overload;

    // Set
    procedure DefineSet<T>(AName: string; var ASet: T; AEnumToName: TFunc<Byte, string>;
      ANameToEnum: TFunc<string, Byte>); overload;
    procedure DefineSet<T, N>(AName: string; var ASet: T; const ANames: N); overload;

    property Mode: TMode read FMode;
    function IsStoring: Boolean;
    function IsLoading: Boolean;

    property Value: TJObject read FValue;
    function OwnValue: TJObject;

    property Version: Integer read FVersion;

  end;

implementation

{ TJSerializer }

destructor TJSerializer.Destroy;
begin
  if IsStoring then
    FValue.Free;
  inherited;
end;

function TJSerializer.IsLoading: Boolean;
begin
  Result := Mode = smUnserialize;
end;

function TJSerializer.IsStoring: Boolean;
begin
  Result := Mode = smSerialize;
end;

function TJSerializer.OwnValue: TJObject;
begin
  Result := FValue;
  FValue := nil;
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

procedure TJSerializer.Define(AName: string; var ANumber: Single);
var
  JValue: TJValue;
begin
  case Mode of
    smSerialize:
      Value[AName] := ANumber;
    smUnserialize:
      if Value.Get(AName, JValue) then
        ANumber := JValue;
  end;
end;

procedure TJSerializer.Define(AName: string; var ANumber: Double);
var
  JValue: TJValue;
begin
  case Mode of
    smSerialize:
      Value[AName] := ANumber;
    smUnserialize:
      if Value.Get(AName, JValue) then
        ANumber := JValue;
  end;
end;

procedure TJSerializer.Define(AName: string; var ANumber: Int64);
var
  JValue: TJValue;
begin
  case Mode of
    smSerialize:
      Value[AName] := ANumber;
    smUnserialize:
      if Value.Get(AName, JValue) then
        ANumber := JValue;
  end;
end;

procedure TJSerializer.Define(AName: string; var ANumber: Integer);
var
  JValue: TJValue;
begin
  case Mode of
    smSerialize:
      Value[AName] := ANumber;
    smUnserialize:
      if Value.Get(AName, JValue) then
        ANumber := JValue;
  end;
end;

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

procedure TJSerializer.Define(AName: string; AObject: IJSerializable; AVersion: Integer);
var
  JValue: TJValue;
begin
  if AObject = nil then
    Exit;
  case Mode of
    smSerialize:
      Value[AName] := TJSerializer.Serialize(AObject, AVersion);
    smUnserialize:
      if Value.Get(AName, JValue) then
        TJSerializer.Unserialize(AObject, JValue);
  end;
end;

procedure TJSerializer.Define(AName: string; var AValue: Boolean);
var
  JValue: TJValue;
begin
  case Mode of
    smSerialize:
      Value[AName] := AValue;
    smUnserialize:
      if Value.Get(AName, JValue) then
        AValue := JValue;
  end;
end;

procedure TJSerializer.Define(AName: string; var AText: string);
var
  JValue: TJValue;
begin
  case Mode of
    smSerialize:
      Value[AName] := AText;
    smUnserialize:
      if Value.Get(AName, JValue) then
        AText := JValue;
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

procedure TJSerializer.Define(AName: string; ASerializer: TJArraySerializer);
var
  V: TJValue;
begin
  try
    case Mode of
      smSerialize:
        begin
          ASerializer.Serialize(V.AddArray(AName));
        end;
      smUnserialize:
        if Value.Get(AName, V) then
          ASerializer.Unserialize(V.AsArray);
    end;

  finally
    ASerializer.Free;

  end;
end;

procedure TJSerializer.Define(AName: string; ASerializer: TJArrayRefSerializer);
var
  JValue: TJValue;
begin
  try
    case Mode of
      smSerialize:
        ASerializer.Serialize(Value.AddArray(AName));
      smUnserialize:
        if Value.Get(AName, JValue) then
          ASerializer.Unserialize(JValue);
    end;

  finally
    ASerializer.Free;

  end;
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

procedure TJSerializer.DefineEnum<T, N>(AName: string; var AEnum: T; const ANames: N);
var
  Names: PString;
begin
  if GetTypeKind(T) <> tkEnumeration then
    raise EJSerialization.Create('Enumeration type expected.');
  if (GetTypeKind(N) <> tkArray) or
    (GetTypeData(TypeInfo(N)).ArrayData.ElType^ <> TypeInfo(string)) then
    raise EJSerialization.Create('Static array of string expected.');

  Names := @ANames;
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

procedure TJSerializer.DefineList<T>(AName: string; AList: IListBase<T>);
begin
  DefineList<T>(AName, AList,
    function: T
    begin
      Result := T.Create;
    end);
end;

procedure TJSerializer.DefineList<T>(AName: string; AList: IListBase<T>; AInstantiator: TFunc<T>);
var
  JArray: TJArray;
  Item: T;
  JValue: TJValue;
begin
  case Mode of
    smSerialize:
      begin
        JArray := Value.AddArray(AName);
        for Item in AList do
          JArray.Add(TJSerializer.Serialize(Item));
      end;
    smUnserialize:
      begin
        AList.Clear;
        for JValue in Value[AName].AsArray do
        begin
          Item := AInstantiator;
          TJSerializer.Unserialize(Item, JValue);
          AList.Add(Item);
        end;
      end;
  end;
end;

procedure TJSerializer.DefineSet<T, N>(AName: string; var ASet: T; const ANames: N);
var
  Names: PString;
  EnumType: PTypeInfo;
begin
  if GetTypeKind(T) <> tkSet then
    raise EJSerialization.Create('Set type expected.');
  if (GetTypeKind(N) <> tkArray) or
    (GetTypeData(TypeInfo(N)).ArrayData.ElType^ <> TypeInfo(string)) then
    raise EJSerialization.Create('Static array of string expected.');

  EnumType := GetTypeData(TypeInfo(T)).CompType^;

  Names := @ANames;
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

procedure TJSerializer.DefineArray<T>(AName: string; AArray: TArray<T>);
begin
  DefineArray<T>(AName, AArray,
    function: T
    begin
      Result := T.Create;
    end);
end;

{ TJArraySerializer<T> }

constructor TJArraySerializer<T>.Create(AData: T);
begin
  FData := AData;
end;

{ TJArrayRefSerializer<T> }

constructor TJArrayRefSerializer<T>.Create(AData: PT);
begin
  FData := AData;
end;

end.
