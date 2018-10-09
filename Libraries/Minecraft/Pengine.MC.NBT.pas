unit Pengine.MC.NBT;

{$POINTERMATH ON}

interface

uses
  System.SysUtils,
  System.Classes,
  System.ZLib,
  System.Character,

  Pengine.CollectionInterfaces,
  Pengine.Collections,
  Pengine.Hasher,
  Pengine.HashCollections,
  Pengine.Parser,
  Pengine.Utility,

  Pengine.MC.General;

type

  // TODO: Format NBT with TStringBuilder

  ENBTListItemWrongType = class(Exception)
  public
    constructor Create;
  end;

  ENBTCompoundExpected = class(Exception)
  public
    constructor Create;
  end;

  ENBTDuplicateTag = class(Exception)
  public
    constructor Create;
  end;

  /// <summary>A list of all in minecraft currently available NBT-Tags.</summary>
  /// <remarks>The ordinal value of each type is used in parsing of nbt data.</remarks>
  TNBTType = (
    nbtEnd,
    nbtByte,
    nbtShort,
    nbtInt,
    nbtLong,
    nbtFloat,
    nbtDouble,
    nbtByteArray,
    nbtString,
    nbtList,
    nbtCompound,
    nbtIntArray,
    nbtLongArray
    );

  TNBTIntegerType = nbtByte .. nbtLong;

  TNBTFloatType = nbtFloat .. nbtDouble;

  TNBTNumberType = nbtByte .. nbtDouble;

  TNBTTagClass = class of TNBTTag;

  /// <summary>The abstract base class for any NBT-Tag.</summary>
  TNBTTag = class abstract
  private
    class function ParseBigEndian<T>(AStream: TStream): T; static;
    class function ParseString(AStream: TStream): string; static;

  public
    constructor Create; overload; virtual;
    constructor Create(AStream: TStream); overload; virtual;

    /// <returns>A parsed tag instance or nil if TAG_End was found.</returns>
    class function Parse(AStream: TStream; out AName: string): TNBTTag;
    class function GetType: TNBTType; virtual; abstract;

    function Format: string; virtual; abstract;

    function Equals(Obj: TObject): Boolean; override;

  end;

  /// <summary>A generic tag class, used only for number types.</summary>
  TNBTTag<T> = class abstract(TNBTTag)
  private
    FValue: T;

  public
    constructor Create(AStream: TStream); override;

    property Value: T read FValue write FValue;

    function Equals(Obj: TObject): Boolean; override;

  end;

  /// <summary>A tag for a signed number with 1 byte of storage.</summary>
  TNBTByte = class(TNBTTag<ShortInt>)
  public
    class function GetType: TNBTType; override;
    function Format: string; override;

  end;

  /// <summary>A tag for a signed number with 2 bytes of storage.</summary>
  TNBTShort = class(TNBTTag<SmallInt>)
  public
    class function GetType: TNBTType; override;
    function Format: string; override;

  end;

  /// <summary>A tag for a signed number with 4 bytes of storage.</summary>
  TNBTInt = class(TNBTTag<Integer>)
  public
    class function GetType: TNBTType; override;
    function Format: string; override;

  end;

  /// <summary>A tag for a signed number with 8 bytes of storage.</summary>
  TNBTLong = class(TNBTTag<Int64>)
  public
    class function GetType: TNBTType; override;
    function Format: string; override;

  end;

  /// <summary>A tag for a floating point number with 4 bytes of storage.</summary>
  TNBTFloat = class(TNBTTag<Single>)
  public
    class function GetType: TNBTType; override;
    function Format: string; override;

  end;

  /// <summary>A tag for a floating point number with 8 bytes of storage.</summary>
  TNBTDouble = class(TNBTTag<Double>)
  public
    class function GetType: TNBTType; override;
    function Format: string; override;

  end;

  /// <summary>A generic base class for number array types.</summary>
  TNBTArray<T> = class abstract(TNBTTag)
  public type

    TItems = TArray<T>;

  private
    FItems: TItems;

  protected
    class function FormatItem(AItem: T): string; virtual; abstract;

  public
    constructor Create; overload; override;
    constructor Create(AStream: TStream); overload; override;
    destructor Destroy; override;

    class function GetNumberType: TNBTType; virtual; abstract;

    property Items: TItems read FItems;

    function Format: string; override;

    function Equals(Obj: TObject): Boolean; override;

  end;

  /// <summary>A tag for an array of signed 1 byte numbers.</summary>
  TNBTByteArray = class(TNBTArray<ShortInt>)
  protected
    class function FormatItem(AItem: ShortInt): string; override;

  public
    class function GetType: TNBTType; override;
    class function GetNumberType: TNBTType; override;

  end;

  /// <summary>A tag for an array of strings.</summary>
  TNBTString = class(TNBTTag)
  private
    FText: string;

  public
    constructor Create(AStream: TStream); override;

    class function GetType: TNBTType; override;

    property Text: string read FText write FText;

    function Format: string; override;

    class function EscapeRequired(AText: string): Boolean;
    class function Escape(AText: string): string;

    function Equals(Obj: TObject): Boolean; override;

  end;

  /// <summary>A tag for an array of any other tag type.</summary>
  TNBTList = class(TNBTTag)
  public type

    TItems = TObjectArray<TNBTTag>;

  private
    FType: TNBTType;
    FItems: TItems;

    function GetItem(AIndex: Integer): TNBTTag;

    procedure AddCheck(AType: TNBTType);
    procedure RemoveCheck;

  public
    constructor Create; override;
    constructor Create(AStream: TStream); override;
    destructor Destroy; override;

    class function GetType: TNBTType; override;

    property Items[AIndex: Integer]: TNBTTag read GetItem; default;

    procedure Add(ATag: TNBTTag);
    procedure Insert(ATag: TNBTTag; AIndex: Integer = 0);
    procedure RemoveAt(AIndex: Integer);
    procedure Clear;

    function Format: string; override;

    function Equals(Obj: TObject): Boolean; override;

  end;

  /// <summary>A tag for an array of signed 4 byte numbers.</summary>
  TNBTIntArray = class(TNBTArray<Integer>)
  protected
    class function FormatItem(AItem: Integer): string; override;

  public
    class function GetType: TNBTType; override;
    class function GetNumberType: TNBTType; override;

  end;

  /// <summary>A tag for an array of signed 8 byte numbers.</summary>
  TNBTLongArray = class(TNBTArray<Int64>)
  protected
    class function FormatItem(AItem: Int64): string; override;

  public
    class function GetType: TNBTType; override;
    class function GetNumberType: TNBTType; override;

  end;

  /// <summary>A tag containing more named tags.</summary>
  TNBTCompound = class(TNBTTag)
  public type

    TMap = TToObjectMap<string, TNBTTag, TStringHasher>;
    TOrder = TArray<TMap.TPair>;

  private
    FMap: TMap;
    FOrder: TOrder;

    function GetTag(AName: string): TNBTTag; overload;
    function GetTag(AIndex: Integer): TNBTTag; overload;
    procedure AddTag(AName: string; const Value: TNBTTag);

  public
    constructor Create; overload; override;
    constructor Create(AStream: TStream); overload; override;
    destructor Destroy; override;

    class function GetType: TNBTType; override;

    property Tags[AName: string]: TNBTTag read GetTag write AddTag; default;
    property Tags[AIndex: Integer]: TNBTTag read GetTag; default;
    function Get(AName: string; out ATag: TNBTTag): Boolean;
    function Count: Integer;
    procedure Remove(AName: string);
    procedure Clear;
    function Empty: Boolean;
    function TagExists(AName: string): Boolean;

    function GetEnumerator: IIterator<TMap.TPair>;

    function Format: string; override;

    class function LoadFromStream(AStream: TStream): TNBTCompound;
    class function LoadFromFile(AFileName: TFileName): TNBTCompound;

    function Equals(Obj: TObject): Boolean; override;

  end;

  TNBTDataParser = class(TObjectParser<TNBTTag>)
  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
    class function IgnoreContext: Boolean; override;

  end;

  TNBTParserCompound = class(TNBTDataParser)
  public type

    TOptionalMode = (
      omReturnNil,
      omReturnEmpty
      );

  public const

    TokenBracket = 1;
    TokenTag = 2;
    TokenColon = 3;
    TokenComma = 4;

    TokenNames: array [TokenBracket .. TokenComma] of string = (
      'Brackets',
      'Tag',
      'Colon',
      'Comma'
      );

  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
    class function GetTokenCount: Integer; override;
    class function GetTokenName(AIndex: Integer): string; override;

    function ParseResult: TNBTCompound;
    function OwnParseResult: TNBTCompound;
    class function Require(AInfo: TParseInfo): TNBTCompound;
    class function Optional(AInfo: TParseInfo; AMode: TOptionalMode): TNBTCompound;

  end;

  TNBTParserListOrArray = class(TNBTDataParser)
  public const

    TokenBracket = 1;
    TokenComma = 2;
    TokenArrayType = 3;
    TokenArraySeperator = 4;

    TokenNames: array [TokenBracket .. TokenArraySeperator] of string = (
      'Brackets',
      'Comma',         
      'Array-Type',
      'Array-Seperator'
      );

  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
    class function GetTokenCount: Integer; override;
    class function GetTokenName(AIndex: Integer): string; override;

  end;

  TNBTParserString = class(TNBTDataParser)
  protected
    function Parse: Boolean; override;
    function ParseResult: TNBTString;

  public
    class function GetResultName: string; override;
    class function IgnoreContext: Boolean; override;

  end;

  TNBTParserNumber = class(TNBTDataParser)
  public const

    TokenNumber = 1;
    TokenSuffix = 2;

    TokenNames: array [TokenNumber .. TokenSuffix] of string = (
      'Number',
      'Suffix'
      );

  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
    class function GetTokenCount: Integer; override;
    class function GetTokenName(AIndex: Integer): string; override;

  end;

  TNBTParserClass = class of TNBTDataParser;

  TNBTPath = class
  public type

    TKey = class
    public
      function Format: string; virtual; abstract;

    end;

    TCompoundKey = class(TKey)
    private
      FKey: string;

    public
      constructor Create(AKey: string);

      property Key: string read FKey write FKey;

      function Format: string; override;

    end;

    TArrayIndex = class(TKey)
    private
      FIndex: Integer;

    public
      constructor Create(AIndex: Integer);

      property Index: Integer read FIndex write FIndex;

      function Format: string; override;

    end;

    TKeys = TObjectArray<TKey>;

  private
    FKeys: TKeys;

  public
    constructor Create;
    destructor Destroy; override;

    property Keys: TKeys read FKeys;

    function Format: string;

  end;

  TNBTPathParser = class(TObjectParser<TNBTPath>)
  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;

  end;

const

  NBTClasses: array [TNBTType] of TNBTTagClass = (
    nil,
    TNBTByte,
    TNBTShort,
    TNBTInt,
    TNBTLong,
    TNBTFloat,
    TNBTDouble,
    TNBTByteArray,
    TNBTString,
    TNBTList,
    TNBTCompound,
    TNBTIntArray,
    TNBTLongArray
    );

  NBTNames: array [TNBTType] of string = (
    'End',
    'Byte',
    'Short',
    'Int',
    'Long',
    'Float',
    'Double',
    'Byte-Array',
    'String',
    'List',
    'Compound',
    'Int-Array',
    'Long-Array'
    );

  NBTNumberSuffixes: array [TNBTNumberType] of Char = (
    'b',
    's',
    'i',
    'l',
    'f',
    'd'
    );

  NBTArrayable = [nbtByte, nbtInt, nbtLong];

implementation

{ TNBTTag }

class function TNBTTag.ParseBigEndian<T>(AStream: TStream): T;
var
  BigEndian: T;
begin
  if SizeOf(T) = 1 then
  begin
    AStream.Read(Result, 1);
    Exit;
  end;
  AStream.Read(BigEndian, SizeOf(T));
  case SizeOf(T) of
    2:
      begin
        (PByte(@Result) + 0)^ := (PByte(@BigEndian) + 1)^;
        (PByte(@Result) + 1)^ := (PByte(@BigEndian) + 0)^;
      end;
    4:
      begin
        (PByte(@Result) + 0)^ := (PByte(@BigEndian) + 3)^;
        (PByte(@Result) + 1)^ := (PByte(@BigEndian) + 2)^;
        (PByte(@Result) + 2)^ := (PByte(@BigEndian) + 1)^;
        (PByte(@Result) + 3)^ := (PByte(@BigEndian) + 0)^;
      end;
    8:
      begin
        (PByte(@Result) + 0)^ := (PByte(@BigEndian) + 7)^;
        (PByte(@Result) + 1)^ := (PByte(@BigEndian) + 6)^;
        (PByte(@Result) + 2)^ := (PByte(@BigEndian) + 5)^;
        (PByte(@Result) + 3)^ := (PByte(@BigEndian) + 4)^;
        (PByte(@Result) + 4)^ := (PByte(@BigEndian) + 3)^;
        (PByte(@Result) + 5)^ := (PByte(@BigEndian) + 2)^;
        (PByte(@Result) + 6)^ := (PByte(@BigEndian) + 1)^;
        (PByte(@Result) + 7)^ := (PByte(@BigEndian) + 0)^;
      end;
  end;
end;

class function TNBTTag.ParseString(AStream: TStream): string;
var
  Len: Word;
  Name: UTF8String;
begin
  Len := ParseBigEndian<Word>(AStream);
  SetLength(Name, Len);
  if Len <> 0 then
  begin
    AStream.ReadBuffer(Name[1], Len);
    Result := string(Name);
  end
  else
    Result := '';
end;

constructor TNBTTag.Create;
begin
  // nothing by default
end;

constructor TNBTTag.Create(AStream: TStream);
begin
  Create;
end;

function TNBTTag.Equals(Obj: TObject): Boolean;
begin
  Result := Obj.ClassType = ClassType;
end;

class function TNBTTag.Parse(AStream: TStream; out AName: string): TNBTTag;
var
  TagType: TNBTType;
begin
  AStream.ReadData(TagType);
  if TagType = nbtEnd then
    Exit(nil);
  AName := ParseString(AStream);
  Result := NBTClasses[TagType].Create(AStream);
end;

{ TNBTTag<T> }

constructor TNBTTag<T>.Create(AStream: TStream);
begin
  inherited;
  FValue := ParseBigEndian<T>(AStream);
end;

function TNBTTag<T>.Equals(Obj: TObject): Boolean;
begin
  Result := inherited and CompareMem(@TNBTTag<T>(Obj).FValue, @FValue, SizeOf(T));
end;

{ TNBTArray<T> }

constructor TNBTArray<T>.Create(AStream: TStream);
var
  C, I: Integer;
begin
  inherited;
  C := ParseBigEndian<Integer>(AStream);
  FItems.Capacity := C;
  FItems.ForceCount(C);
  for I := 0 to C - 1 do
    FItems[I] := ParseBigEndian<T>(AStream);
end;

constructor TNBTArray<T>.Create;
begin
  FItems := TItems.Create;
end;

destructor TNBTArray<T>.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TNBTArray<T>.Equals(Obj: TObject): Boolean;
begin
  Result := inherited and FItems.Equals(TNBTArray<T>(Obj).FItems);
end;

function TNBTArray<T>.Format: string;
var
  I: Integer;
begin
  Result := '[' + NBTNumberSuffixes[GetNumberType].ToUpper + '; ';
  for I := 0 to FItems.MaxIndex do
  begin
    Result := Result + FormatItem(Items[I]);
    if I < FItems.MaxIndex then
      Result := Result + ', ';
  end;
  Result := Result + ']';
end;

{ TNBTByte }

function TNBTByte.Format: string;
begin
  Result := IntToStr(Value) + NBTNumberSuffixes[GetType];
end;

class function TNBTByte.GetType: TNBTType;
begin
  Result := nbtByte;
end;

{ TNBTShort }

function TNBTShort.Format: string;
begin
  Result := IntToStr(Value) + NBTNumberSuffixes[GetType];
end;

class function TNBTShort.GetType: TNBTType;
begin
  Result := nbtShort;
end;

{ TNBTInt }

function TNBTInt.Format: string;
begin
  Result := IntToStr(Value);
end;

class function TNBTInt.GetType: TNBTType;
begin
  Result := nbtInt;
end;

{ TNBTLong }

function TNBTLong.Format: string;
begin
  Result := IntToStr(Value) + NBTNumberSuffixes[GetType];
end;

class function TNBTLong.GetType: TNBTType;
begin
  Result := nbtLong;
end;

{ TNBTFloat }

function TNBTFloat.Format: string;
begin
  Result := PrettyFloat(Value) + NBTNumberSuffixes[GetType];
end;

class function TNBTFloat.GetType: TNBTType;
begin
  Result := nbtFloat;
end;

{ TNBTDouble }

function TNBTDouble.Format: string;
begin
  Result := PrettyFloat(Value) + NBTNumberSuffixes[GetType];
end;

class function TNBTDouble.GetType: TNBTType;
begin
  Result := nbtDouble;
end;

{ TNBTByteArray }

class function TNBTByteArray.FormatItem(AItem: ShortInt): string;
begin
  Result := IntToStr(AItem) + NBTNumberSuffixes[GetNumberType];
end;

class function TNBTByteArray.GetNumberType: TNBTType;
begin
  Result := nbtByte;
end;

class function TNBTByteArray.GetType: TNBTType;
begin
  Result := nbtByteArray;
end;

{ TNBTString }

constructor TNBTString.Create(AStream: TStream);
begin
  inherited;
  FText := ParseString(AStream);
end;

function TNBTString.Equals(Obj: TObject): Boolean;
begin
  Result := inherited and (TNBTString(Obj).Text = Text);
end;

class function TNBTString.Escape(AText: string): string;
begin
  Result := '"' + AText.Replace('\', '\\').Replace('"', '\"') + '"';
end;

class function TNBTString.EscapeRequired(AText: string): Boolean;
var
  C: Char;
begin
  for C in AText do
    if not CharInSet(C, IdentChars) then
      Exit(True);
  Result := False;
end;

function TNBTString.Format: string;
begin
  Result := Escape(Text);
end;

class function TNBTString.GetType: TNBTType;
begin
  Result := nbtString;
end;

{ TNBTList }

procedure TNBTList.Add(ATag: TNBTTag);
begin
  AddCheck(ATag.GetType);
  FItems.Add(ATag);
end;

procedure TNBTList.AddCheck(AType: TNBTType);
begin
  if FType = nbtEnd then
    FType := AType
  else if FType <> AType then
    raise ENBTListItemWrongType.Create;
end;

procedure TNBTList.Clear;
begin
  FType := nbtEnd;
  FItems.Clear;
end;

constructor TNBTList.Create(AStream: TStream);
var
  Count: Integer;
  I: Integer;
begin
  inherited;
  AStream.ReadData(FType);
  Count := ParseBigEndian<Integer>(AStream);
  FItems.Capacity := Count;
  FItems.ForceCount(Count);
  for I := 0 to Count - 1 do
    FItems[I] := NBTClasses[FType].Create(AStream);
end;

constructor TNBTList.Create;
begin
  inherited;
  FItems := TItems.Create;
end;

procedure TNBTList.RemoveAt(AIndex: Integer);
begin
  FItems.RemoveAt(AIndex);
  RemoveCheck;
end;

procedure TNBTList.RemoveCheck;
begin
  if FItems.Empty then
    FType := nbtEnd;
end;

destructor TNBTList.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TNBTList.Equals(Obj: TObject): Boolean;
begin
  Result := inherited and TNBTList(Obj).FItems.Equals(FItems);
end;

function TNBTList.Format: string;
var
  I: Integer;
begin
  Result := '[';
  for I := 0 to FItems.MaxIndex do
  begin
    Result := Result + Items[I].Format;
    if I < FItems.MaxIndex then
      Result := Result + ', ';
  end;
  Result := Result + ']';
end;

function TNBTList.GetItem(AIndex: Integer): TNBTTag;
begin
  Result := FItems[AIndex];
end;

class function TNBTList.GetType: TNBTType;
begin
  Result := nbtList;
end;

procedure TNBTList.Insert(ATag: TNBTTag; AIndex: Integer);
begin
  AddCheck(ATag.GetType);
  FItems.Insert(ATag, AIndex);
end;

{ TNBTIntArray }

class function TNBTIntArray.FormatItem(AItem: Integer): string;
begin
  Result := IntToStr(AItem);
end;

class function TNBTIntArray.GetNumberType: TNBTType;
begin
  Result := nbtInt;
end;

class function TNBTIntArray.GetType: TNBTType;
begin
  Result := nbtIntArray;
end;

{ TNBTLongArray }

class function TNBTLongArray.FormatItem(AItem: Int64): string;
begin
  Result := IntToStr(AItem) + NBTNumberSuffixes[GetNumberType];
end;

class function TNBTLongArray.GetNumberType: TNBTType;
begin
  Result := nbtLong;
end;

class function TNBTLongArray.GetType: TNBTType;
begin
  Result := nbtLongArray;
end;

{ TNBTCompound }

procedure TNBTCompound.AddTag(AName: string; const Value: TNBTTag);
begin
  if FMap.KeyExists(AName) then
    raise ENBTDuplicateTag.Create;
  FMap[AName] := Value;
  FOrder.Add(TMap.TPair.Create(AName, Value));
end;

procedure TNBTCompound.Clear;
begin
  if Count = 0 then
    Exit;
  FMap.Clear;
  FOrder.Clear;
end;

function TNBTCompound.Count: Integer;
begin
  Result := FMap.Count;
end;

constructor TNBTCompound.Create(AStream: TStream);
var
  Tag: TNBTTag;
  Name: string;
begin
  inherited;
  while True do
  begin
    Tag := TNBTTag.Parse(AStream, Name);
    if Tag = nil then
      Break;
    AddTag(Name, Tag);
  end;
end;

constructor TNBTCompound.Create;
begin
  inherited;
  FMap := TMap.Create;
  FOrder := TOrder.Create;
end;

procedure TNBTCompound.Remove(AName: string);
begin
  FMap.Remove(AName);
  FOrder.RemoveAt(FOrder.FindFirstIndex(
    function(APair: TMap.TPair): Boolean
    begin
      Result := APair.Key = AName;
    end));
end;

destructor TNBTCompound.Destroy;
begin
  FMap.Free;
  FOrder.Free;
  inherited;
end;

function TNBTCompound.Empty: Boolean;
begin
  Result := FMap.Empty;
end;

function TNBTCompound.Equals(Obj: TObject): Boolean;
begin
  Result := inherited and TNBTCompound(Obj).FMap.Equals(FMap);
end;

function TNBTCompound.Format: string;
var
  Pair: TMap.TPair;
  First: Boolean;
begin
  Result := '{';
  if not Empty then
  begin
    First := True;
    for Pair in FOrder do
    begin
      if not First then
        Result := Result + ', '
      else
        First := False;
      if TNBTString.EscapeRequired(Pair.Key) then
        Result := Result + TNBTString.Escape(Pair.Key)
      else
        Result := Result + Pair.Key;
      Result := Result + ': ' + Pair.Value.Format;
    end;
  end;
  Result := Result + '}';
end;

function TNBTCompound.GetEnumerator: IIterator<TMap.TPair>;
begin
  Result := FOrder.GetEnumerator;
end;

function TNBTCompound.GetTag(AName: string): TNBTTag;
begin
  Result := FMap[AName];
end;

function TNBTCompound.GetTag(AIndex: Integer): TNBTTag;
begin
  Result := FOrder[AIndex].Value;
end;

class function TNBTCompound.GetType: TNBTType;
begin
  Result := nbtCompound;
end;

class function TNBTCompound.LoadFromStream(AStream: TStream): TNBTCompound;
var
  Name: string;
  Tag: TNBTTag;
begin
  Tag := Parse(AStream, Name);
  if Tag is TNBTCompound then
    Exit(TNBTCompound(Tag));
  raise ENBTCompoundExpected.Create;
end;

function TNBTCompound.TagExists(AName: string): Boolean;
begin
  Result := FMap.KeyExists(AName);
end;

function TNBTCompound.Get(AName: string; out ATag: TNBTTag): Boolean;
begin
  Result := FMap.Get(AName, ATag);
end;

class function TNBTCompound.LoadFromFile(AFileName: TFileName): TNBTCompound;
var
  Stream: TStream;
  DecompressionStream: TZDecompressionStream;
begin
  try
    Stream := TFileStream.Create(AFileName, fmOpenRead);
    DecompressionStream := TDecompressionStream.Create(Stream, 15 + 16, True);
    try
      Result := LoadFromStream(DecompressionStream);
    finally
      DecompressionStream.Free;
    end;
  except
    on EZDecompressionError do
    begin
      Stream := TFileStream.Create(AFileName, fmOpenRead);
      try
        Result := LoadFromStream(Stream);
      finally
        Stream.Free;
      end;
    end;
  end;
end;

{ TNBTParserCompound }

class function TNBTParserCompound.GetResultName: string;
begin
  Result := 'NBT-Compound';
end;

class function TNBTParserCompound.GetTokenCount: Integer;
begin
  Result := Length(TokenNames);
end;

class function TNBTParserCompound.GetTokenName(AIndex: Integer): string;
begin
  Result := TokenNames[AIndex];
end;

class function TNBTParserCompound.Optional(AInfo: TParseInfo; AMode: TOptionalMode): TNBTCompound;
begin
  with TNBTParserCompound.Create(AInfo, False) do
  begin
    if Success then
      Result := OwnParseResult
    else
    begin
      case AMode of
        omReturnNil:
          Result := nil;
        omReturnEmpty:
          Result := TNBTCompound.Create;
      else
        raise ENotImplemented.Create('Unknown Mode.');
      end
    end;
    Free;
  end;
end;

function TNBTParserCompound.OwnParseResult: TNBTCompound;
begin
  Result := TNBTCompound(inherited OwnParseResult);
end;

function TNBTParserCompound.Parse: Boolean;
var
  TagName: string;
  DataParser: TNBTDataParser;
begin
  Token := TokenBracket;

  if not StartsWith('{') then
    Exit(False);

  ResetToken;

  SkipWhitespace;

  SetParseResult(TNBTCompound.Create);

  Token := TokenBracket;

  while not StartsWith('}') do
  begin
    Token := TokenTag;

    TagName := TStringOrIdentParser.Require(Info);
    if TagName = '' then
      raise EParseError.Create('Expected tag name.');

    if ParseResult.TagExists(TagName) then
      raise EParseError.CreateFmt('Duplicate key "%s" in compound.', [TagName]);

    ResetToken;

    SkipWhitespace;

    Token := TokenColon;

    if not StartsWith(':') then
      raise EParseError.Create('Expected ":" followed by tag data.');

    ResetToken;

    SkipWhitespace;

    DataParser := TNBTDataParser.Create(Info, True);
    try
      ParseResult[TagName] := DataParser.OwnParseResult;
    finally
      DataParser.Free;
    end;

    SkipWhitespace;

    Token := TokenComma;

    if StartsWith(',') then
    begin
      SkipWhitespace;
      Continue;
    end;

    Token := TokenBracket;

    if not StartsWith('}', False) then
      raise EParseError.Create('Expected "," or "}" to close the compound.');

  end;

  Result := True;
end;

function TNBTParserCompound.ParseResult: TNBTCompound;
begin
  Result := TNBTCompound(inherited ParseResult);
end;

class function TNBTParserCompound.Require(AInfo: TParseInfo): TNBTCompound;
begin
  Result := TNBTCompound(inherited Require(AInfo));
end;

{ TNBTParserListOrArray }

class function TNBTParserListOrArray.GetResultName: string;
begin
  Result := 'NBT-Array';
end;

class function TNBTParserListOrArray.GetTokenCount: Integer;
begin
  Result := Length(TokenNames);
end;

class function TNBTParserListOrArray.GetTokenName(AIndex: Integer): string;
begin
  Result := TokenNames[AIndex];
end;

function TNBTParserListOrArray.Parse: Boolean;
var
  DataParser: TNBTDataParser;
  IsArrayType: Boolean;
  FirstType, ArrayType: TNBTType;
  Marker: TLogMarker;
begin
  Token := TokenBracket;

  if not StartsWith('[') then
    Exit(False);

  FirstType := nbtEnd;
  IsArrayType := False;

  for ArrayType in NBTArrayable do
  begin
    if StartsWith(NBTNumberSuffixes[ArrayType].ToUpper + ';', False) then
    begin
      FirstType := ArrayType;
      Token := TokenArrayType;
      Advance;
      Token := TokenArraySeperator;
      Advance;
      case FirstType of
        nbtByte:
          SetParseResult(TNBTByteArray.Create);
        nbtInt:
          SetParseResult(TNBTIntArray.Create);
        nbtLong:
          SetParseResult(TNBTLongArray.Create);
      else
        Assert(False);
      end;
      IsArrayType := True;
    end;
  end;

  ResetToken;

  SkipWhitespace;

  Token := TokenBracket;

  while not StartsWith(']') do
  begin
    Marker := GetMarker;
    DataParser := TNBTDataParser.Create(Info, True);
    try
      if FirstType = nbtEnd then
      begin
        FirstType := DataParser.ParseResult.GetType;
        SetParseResult(TNBTList.Create);
      end;

      if DataParser.ParseResult.GetType <> FirstType then
      begin
        Log(Marker, 'Cannot add %s into list of type %s.',
          [NBTNames[DataParser.ParseResult.GetType], NBTNames[FirstType]], elFatal);
      end
      else
      begin
        if IsArrayType then
        begin
          case FirstType of
            nbtByte:
              TNBTByteArray(ParseResult).Items.Add(TNBTByte(DataParser.ParseResult).Value);
            nbtInt:
              TNBTIntArray(ParseResult).Items.Add(TNBTInt(DataParser.ParseResult).Value);
            nbtLong:
              TNBTLongArray(ParseResult).Items.Add(TNBTLong(DataParser.ParseResult).Value);
          end;
        end
        else
          TNBTList(ParseResult).Add(DataParser.OwnParseResult);
      end;

    finally
      DataParser.Free;
    end;

    SkipWhitespace;

    Token := TokenComma;

    if StartsWith(',') then
    begin
      SkipWhitespace;
      Continue;
    end;

    Token := TokenBracket;

    if not StartsWith(']', False) then
      raise EParseError.Create('Expected "," or "]" to close the list.');

  end;

  if ParseResult = nil then
    SetParseResult(TNBTList.Create);

  Result := True;
end;

{ TNBTDataParser }

class function TNBTDataParser.GetResultName: string;
begin
  Result := 'NBT-Data';
end;

class function TNBTDataParser.IgnoreContext: Boolean;
begin
  Result := Self = TNBTDataParser;
end;

function TNBTDataParser.Parse: Boolean;
const
  ParserClasses: array [0 .. 3] of TNBTParserClass = (
    TNBTParserCompound,
    TNBTParserListOrArray,
    TNBTParserNumber,
    TNBTParserString
    );

var
  ParserClass: TNBTParserClass;
  Parser: TNBTDataParser;
begin
  for ParserClass in ParserClasses do
  begin
    Parser := ParserClass.Create(Info, False);
    try
      if Parser.Success then
      begin
        SetParseResult(Parser.OwnParseResult);
        Exit(True);
      end;
    finally
      Parser.Free;
    end;
  end;
  Result := False;
end;

{ ENBTListItemWrongType }

constructor ENBTListItemWrongType.Create;
begin
  inherited Create('NBT-List items must be of the same type.');
end;

{ ENBTCompoundExpected }

constructor ENBTCompoundExpected.Create;
begin
  inherited Create('Expected a compound nbt tag as root.');
end;

{ TNBTParserString }

class function TNBTParserString.GetResultName: string;
begin
  Result := 'NBT-String';
end;

class function TNBTParserString.IgnoreContext: Boolean;
begin
  Result := True;
end;

function TNBTParserString.Parse: Boolean;
var
  Parser: TStringOrIdentParser;
begin
  Parser := TStringOrIdentParser.Create(Info, True);
  Result := not Parser.IsIdent or not Parser.ParseResult.IsEmpty;
  if Result then
  begin
    SetParseResult(TNBTString.Create);
    ParseResult.Text := Parser.ParseResult;
  end;
  Parser.Free;
end;

function TNBTParserString.ParseResult: TNBTString;
begin
  Result := TNBTString(inherited ParseResult);
end;

{ TNBTParserNumber }

class function TNBTParserNumber.GetResultName: string;
begin
  Result := 'NBT-Number';
end;

class function TNBTParserNumber.GetTokenCount: Integer;
begin
  Result := High(TokenNames);
end;

class function TNBTParserNumber.GetTokenName(AIndex: Integer): string;
begin
  Result := TokenNames[AIndex];
end;

function TNBTParserNumber.Parse: Boolean;
const
  IntegerChars = ['+', '-', '0' .. '9'];
  FloatChars = IntegerChars + ['.', 'e', 'E'];
var
  NumberType: TNBTNumberType;
  Parsed: string;
  C: Char;
  IsFloat, FoundType: Boolean;
  NumInt: Int64;
  NumFloat: Double;
begin
  Token := TokenNumber;

  IsFloat := False;
  for C in Info do
  begin
    if not CharInSet(C, FloatChars) then
      Break;
    if C = '.' then
      IsFloat := True;
    Parsed := Parsed + C;
  end;

  Token := TokenSuffix;

  FoundType := False;
  for NumberType := Low(TNBTNumberType) to High(TNBTNumberType) do
  begin
    if NumberType = nbtInt then
      Continue;
    if First.ToLower = NBTNumberSuffixes[NumberType] then
    begin
      FoundType := True;
      if IsFloat and (NumberType <= High(TNBTIntegerType)) then
        Exit(False);
      if not IsFloat and (NumberType > High(TNBTIntegerType)) then
        IsFloat := True;
      Advance;
      Break;
    end;
  end;

  if not FoundType then
  begin
    if IsFloat then
      NumberType := nbtDouble
    else
      NumberType := nbtInt;
  end;

  SetParseResult(NBTClasses[NumberType].Create);

  if IsFloat then
  begin
    if Double.TryParse(Parsed, NumFloat, FormatSettings.Invariant) then
    begin
      case NumberType of
        nbtFloat:
          TNBTFloat(ParseResult).Value := NumFloat;
        nbtDouble:
          TNBTDouble(ParseResult).Value := NumFloat;
      end;
    end
    else
      Exit(False);
  end
  else
  begin
    if Int64.TryParse(Parsed, NumInt) then
    begin
      case NumberType of
        nbtByte:
          TNBTByte(ParseResult).Value := NumInt;
        nbtShort:
          TNBTShort(ParseResult).Value := NumInt;
        nbtInt:
          TNBTInt(ParseResult).Value := NumInt;
        nbtLong:
          TNBTLong(ParseResult).Value := NumInt;
      end;
    end
    else
      Exit(False);
  end;

  Result := True;
end;

{ ENBTDuplicateTag }

constructor ENBTDuplicateTag.Create;
begin
  inherited Create('Duplicate tag names in compound.');
end;

{ TNBTPath.TCompoundKey }

constructor TNBTPath.TCompoundKey.Create(AKey: string);
begin
  FKey := AKey;
end;

function TNBTPath.TCompoundKey.Format: string;
begin
  if Key.IsEmpty or (Key.IndexOfAny(['"', ' ', '.', '[', ']']) <> -1) then
    Exit(DblQuoted(Key));
  Result := Key;
end;

{ TNBTPath.TArrayIndex }

constructor TNBTPath.TArrayIndex.Create(AIndex: Integer);
begin
  FIndex := AIndex;
end;

function TNBTPath.TArrayIndex.Format: string;
begin
  Result := '[' + Index.ToString + ']';
end;

{ TNBTPath }

constructor TNBTPath.Create;
begin
  FKeys := TKeys.Create;
end;

destructor TNBTPath.Destroy;
begin
  FKeys.Free;
  inherited;
end;

function TNBTPath.Format: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Keys.MaxIndex do
  begin
    Result := Result + Keys[I].Format;
    if (I <> Keys.MaxIndex) and (Keys[I + 1] is TCompoundKey) then
      Result := Result + '.';
  end;
end;

{ TNBTPathParser }

class function TNBTPathParser.GetResultName: string;
begin
  Result := 'NBT-Path';
end;

function TNBTPathParser.Parse: Boolean;
var
  Key: string;
  Marker: TLogMarker;
  Index: Integer;
begin
  if ReachedEnd or First.IsWhitespace then
    Exit(False);

  SetParseResult(TNBTPath.Create);
  while not First.IsWhitespace do
  begin
    if StartsWith('[') then
    begin
      Marker := GetMarker;
      Key := ReadWhile(['0' .. '9', '-']);
      if Key.IsEmpty then
        raise EParseError.Create('Expected integer.');
      if not TryStrToInt(Key, Index) then
        Log(Marker, '"%s" is not a valid integer.', [Key])
      else
      begin
        if Index < 0 then
          Log(Marker, 'The index cannot be less than zero.');
        ParseResult.Keys.Add(TNBTPath.TArrayIndex.Create(Index));
      end;
      if not StartsWith(']') then
        raise EParseError.Create('Expected "]".');

    end
    else
    begin
      if First = '"' then
      begin
        Key := TStringParser.Require(Info);
      end
      else
      begin
        Key := ReadWhile(
          function(C: Char): Boolean
          begin
            Result := not C.IsWhitespace and not CharInSet(C, ['.', '[', '"']);
          end);
        if Key.IsEmpty then
          raise EParseError.Create('Expected quoted tag, unquoted tag or array index.');
      end;
      ParseResult.Keys.Add(TNBTPath.TCompoundKey.Create(Key));

    end;

    if ReachedEnd or First.IsWhitespace then
      Break;

    if StartsWith('.') or StartsWith('[', False) then
      Continue;

    raise EParseError.Create('Expected "." or "["', 1);

    Break;
  end;
  Result := True;
end;

end.
