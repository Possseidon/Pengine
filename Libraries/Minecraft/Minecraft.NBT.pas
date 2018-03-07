unit Minecraft.NBT;

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
  Pengine.Parser;

type

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
  end;

  /// <summary>A tag foTNBTTypearray of strings.</summary>
  TNBTString = class(TNBTTag)
  private
    FText: string;

  public
    constructor Create(AStream: TStream); override;

    class function GetType: TNBTType; override;

    property Text: string read FText write FText;

    function Format: string; override;

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
    procedure DelCheck;

  public
    constructor Create; override;
    constructor Create(AStream: TStream); override;
    destructor Destroy; override;

    class function GetType: TNBTType; override;

    property Items[AIndex: Integer]: TNBTTag read GetItem; default;

    procedure Add(ATag: TNBTTag);
    procedure Insert(ATag: TNBTTag; AIndex: Integer = 0);
    procedure DelAt(AIndex: Integer);
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
  end;

  /// <summary>A tag foTNBTTypearray of signed 8 byte numbers.</summary>
  TNBTLongArray = class(TNBTArray<Int64>)
  protected
    class function FormatItem(AItem: Int64): string; override;
  public
    class function GetType: TNBTType; override;
  end;

  /// <summary>A tag coTNBTTypeing more named tags.</summary>
  TNBTCompound = class(TNBTTag)
  public type

    TMap = TToObjectMap<string, TNBTTag, TStringHasher>;
    TOrder = TArray<TMap.TPair>;

  private
    FMap: TMap;
    FOrder: TOrder;

    procedure AddTag(AName: string; const Value: TNBTTag);
    function GetTag(AName: string): TNBTTag;

  public
    constructor Create; overload; override;
    constructor Create(AStream: TStream); overload; override;
    destructor Destroy; override;

    class function GetType: TNBTType; override;

    property Tags[AName: string]: TNBTTag read GetTag write AddTag; default;
    function Count: Integer;
    procedure Del(AName: string);
    procedure Clear;
    function Empty: Boolean;
    function TagExists(AName: string): Boolean;

    function GetEnumerator: IIterator<TMap.TPair>;

    function Format: string; override;

    class function LoadFromStream(AStream: TStream): TNBTCompound;
    class function LoadFromFile(AFileName: TFileName): TNBTCompound;

    function Equals(Obj: TObject): Boolean; override;
    
  end;

  TNBTDataParser = class(TParser<TNBTTag>)
  protected
    function Parse: Boolean; override;

    procedure Cleanup; override;
    
  end;

  TNBTParserCompound = class(TNBTDataParser)
  protected
    function Parse: Boolean; override;

  public
    function ParseResult: TNBTCompound;

  end;

  TNBTParserListOrArray = class(TNBTDataParser)
  protected
    function Parse: Boolean; override;

  end;

  TNBTParserString = class(TNBTDataParser)
  protected
    function Parse: Boolean; override;
    function ParseResult: TNBTString;

  end;

  TNBTParserNumber = class(TNBTDataParser)
  protected
    function Parse: Boolean; override;

  end;

  TNBTParserClass = class of TNBTDataParser;

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
    'end',
    'byte',
    'short',
    'int',
    'long',
    'float',
    'double',
    'byte_array',
    'string',
    'list',
    'compound',
    'int_array',
    'long_array'
    );

  NBTNumberSuffixes: array [TNBTNumberType] of Char = (
    'b',
    's',
    #0,
    'l',
    'f',
    'd'
  );

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
  Result := '[';
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
  Result := IntToStr(Value) + 'b';
end;

class function TNBTByte.GetType: TNBTType;
begin
  Result := nbtByte;
end;

{ TNBTShort }

function TNBTShort.Format: string;
begin
  Result := IntToStr(Value) + 's';
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
  Result := IntToStr(Value) + 'l';
end;

class function TNBTLong.GetType: TNBTType;
begin
  Result := nbtLong;
end;

{ TNBTFloat }

function TNBTFloat.Format: string;
begin
  Result := Value.ToString(ffGeneral, 7, 0, TFormatSettings.Invariant) + 'f';
end;

class function TNBTFloat.GetType: TNBTType;
begin
  Result := nbtFloat;
end;

{ TNBTDouble }

function TNBTDouble.Format: string;
begin
  Result := Value.ToString(ffGeneral, 15, 0, TFormatSettings.Invariant) + 'd';
end;

class function TNBTDouble.GetType: TNBTType;
begin
  Result := nbtDouble;
end;

{ TNBTByteArray }

class function TNBTByteArray.FormatItem(AItem: ShortInt): string;
begin
  Result := IntToStr(AItem);
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

function TNBTString.Format: string;
begin
  Result := '"' + Text.Replace('\', '\\').Replace('"', '\"') + '"';
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

procedure TNBTList.DelAt(AIndex: Integer);
begin
  FItems.DelAt(AIndex);
  DelCheck;
end;

procedure TNBTList.DelCheck;
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

class function TNBTIntArray.GetType: TNBTType;
begin
  Result := nbtIntArray;
end;

{ TNBTLongArray }

class function TNBTLongArray.FormatItem(AItem: Int64): string;
begin
  Result := IntToStr(AItem);
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
    Self[Name] := Tag;
  end;
end;

constructor TNBTCompound.Create;
begin
  inherited;
  FMap := TMap.Create;
  FOrder := TOrder.Create;
end;

procedure TNBTCompound.Del(AName: string);
begin
  FMap.Del(AName);
  FOrder.DelAt(FOrder.FindFirstIndex(
    function (APair: TMap.TPair): Boolean
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
    for Pair in FMap do
    begin
      if not First then
        Result := Result + ', '
      else
        First := False;
      Result := Result + Pair.Key + ':' + Pair.Value.Format;
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

class function TNBTCompound.LoadFromFile(AFileName: TFileName): TNBTCompound;
var
  Stream: TStream;
  DecompressionStream: TZDecompressionStream;
begin
  try
    Stream := TFileStream.Create(AFileName, fmOpenRead);
    try
      DecompressionStream := TDecompressionStream.Create(Stream, 15 + 16);
      try
        Result := LoadFromStream(DecompressionStream);
      finally
        DecompressionStream.Free;
      end;
    finally
      Stream.Free;
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

function TNBTParserCompound.Parse: Boolean;
const
  TagNameChars = ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '_'];
var
  TagName: string;
  DataParser: TNBTDataParser;
  C: Char;
begin
  Info.SkipWhitespace;

  if not Info.StartsWith('{') then
    Exit(False);

  Info.SkipWhitespace;
    
  SetParseResult(TNBTCompound.Create);

  while not Info.StartsWith('}') do
  begin
    Info.SkipWhitespace;

    TagName := '';
    for C in Info do
    begin
      if not CharInSet(C, TagNameChars) then
        Break;
      TagName := TagName + C;
    end;
    if TagName = '' then
      raise EParseError.Create('Expected tag name.');
    
    if ParseResult.TagExists(TagName) then
      raise EParseError.CreateFmt('Duplicate key "%s" in compound.', [TagName]);

    Info.SkipWhitespace;

    if not Info.StartsWith(':') then
      raise EParseError.Create('Expected ":" followed by tag data.');

    Info.SkipWhitespace;

    DataParser := TNBTDataParser.Create(Info, True);
    ParseResult[TagName] := DataParser.ParseResult;
    DataParser.Free;

    Info.SkipWhitespace;

    if Info.StartsWith(',') then
    begin
      Info.SkipWhitespace;
      Continue;
    end;

    if not Info.StartsWith('}', False) then
      raise EParseError.Create('Expected "," or "}" to close the compound.');

  end;

  Result := True;
end;

function TNBTParserCompound.ParseResult: TNBTCompound;
begin
  Result := TNBTCompound(inherited ParseResult);
end;

{ TNBTParserListOrArray }

function TNBTParserListOrArray.Parse: Boolean;
var
  DataParser: TNBTDataParser;
  FirstType: TNBTType;
begin
  Info.SkipWhitespace;

  if not Info.StartsWith('[') then
    Exit(False);

  Info.SkipWhitespace;

  FirstType := nbtEnd;
  while not Info.StartsWith(']') do
  begin
    Info.SkipWhitespace;

    DataParser := TNBTDataParser.Create(Info, True);
    try
      if FirstType = nbtEnd then
      begin
        FirstType := DataParser.ParseResult.GetType;
        case FirstType of
          nbtByte:
            SetParseResult(TNBTByteArray.Create);
          nbtInt:
            SetParseResult(TNBTIntArray.Create);
          nbtLongArray:
            SetParseResult(TNBTLongArray.Create);
        else
          SetParseResult(TNBTList.Create);
        end;
      end;

      if DataParser.ParseResult.GetType <> FirstType then
      begin
        DataParser.ParseResult.Free;
        raise EParseError.Create('Cannot add ' + NBTNames[DataParser.ParseResult.GetType] +
          ' into list of type ' + NBTNames[FirstType] + '.');
      end;

      case FirstType of
        nbtByte:
          begin
            TNBTByteArray(ParseResult).Items.Add(TNBTByte(DataParser.ParseResult).Value);
            DataParser.ParseResult.Free;
          end;
        nbtInt:
          begin
            TNBTIntArray(ParseResult).Items.Add(TNBTInt(DataParser.ParseResult).Value);
            DataParser.ParseResult.Free;
          end;
        nbtLongArray:
          begin
            TNBTLongArray(ParseResult).Items.Add(TNBTLong(DataParser.ParseResult).Value);
            DataParser.ParseResult.Free;
          end
      else
        TNBTList(ParseResult).Add(DataParser.ParseResult);
      end;
    finally
      DataParser.Free;
    end;

    Info.SkipWhitespace;

    if Info.StartsWith(',') then
    begin
      Info.SkipWhitespace;
      Continue;
    end;

    if not Info.StartsWith(']', False) then
      raise EParseError.Create('Expected "," or "]" to close the list.');
  end;

  if ParseResult = nil then
    SetParseResult(TNBTList.Create);

  Result := True;
end;

{ TNBTDataParser }

procedure TNBTDataParser.Cleanup;
begin
  inherited;
  ParseResult.Free;
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
    Parser := ParserClass.Create(Info, True);
    try
      if Parser.Success then
      begin
        SetParseResult(Parser.ParseResult);
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

function TNBTParserString.Parse: Boolean;
const
  AlphaNum = ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '_'];
var
  C: Char;
begin
  SetParseResult(TNBTString.Create);

  if Info.StartsWith('"') then
  begin
    for C in Info do
    begin
      case C of
        '"':
          begin
            Info.Advance;
            Exit(True);
          end;
        '\':
          Info.Advance;
      end;
      ParseResult.Text := ParseResult.Text + Info.First;
    end;
    raise EParseError.Create('Found unterminated string.');
  end
  else
  begin
    for C in Info do
    begin
      if not CharInSet(C, AlphaNum) then
        Break;
      ParseResult.Text := ParseResult.Text + C;
    end;
    Result := not ParseResult.Text.IsEmpty;
  end;
end;

function TNBTParserString.ParseResult: TNBTString;
begin
  Result := TNBTString(inherited ParseResult);
end;

{ TNBTParserNumber }

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
  StartPos: Integer;
begin
  StartPos := Info.Pos;

  IsFloat := False;
  for C in Info do
  begin
    if not CharInSet(C, FloatChars) then
      Break;
    if C = '.' then
      IsFloat := True;
    Parsed := Parsed + C;
  end;

  FoundType := False;  
  for NumberType := Low(TNBTNumberType) to High(TNBTNumberType) do
  begin
    if NBTNumberSuffixes[NumberType] = #0 then
      Continue;
    if Info.First = NBTNumberSuffixes[NumberType] then
    begin
      FoundType := True;
      if IsFloat and (NumberType <= High(TNBTIntegerType)) then
        Exit(False);
      if not IsFloat and (NumberType > High(TNBTIntegerType)) then
        IsFloat := True;
      Info.Advance;
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
    begin
      Info.Pos := StartPos;
      Exit(False);
    end;
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
    begin    
      Info.Pos := StartPos;
      Exit(False);
    end;
  end;
  
  Result := True;
end;

{ ENBTDuplicateTag }

constructor ENBTDuplicateTag.Create;
begin
  inherited Create('Duplicate tag names in compound.');
end;

end.
