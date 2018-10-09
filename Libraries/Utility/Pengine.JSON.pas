unit Pengine.JSON;

interface

uses
  System.SysUtils,

  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.Hasher,
  System.Math,
  Pengine.Parser,
  Pengine.CollectionInterfaces,
  Pengine.Utility;

type

  EJSONError = class(Exception);

  TJValue = class;
  TJObject = class;
  TJArray = class;
  TJString = class;
  TJNumber = class;
  TJBool = class;
  TJNull = class;

  TJPair = TPair<string, TJValue>;

  /// <summary>Any json value.</summary>
  TJValue = class
  public type

    TNumber = record
    public
      class operator Implicit(ANumber: TNumber): Double; static;
      class operator Implicit(ANumber: Int64): TNumber; static;
      class operator Implicit(ANumber: Double): TNumber; static;

    private
      case IsFloat: Boolean of
        False: (AsInt: Int64);
        True: (AsFloat: Double);
    end;

    TParser = class(TObjectParser<TJValue>)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  private
    function GetAsObject: TJObject;
    function GetAsArray: TJArray;
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    function GetAsNumber: TNumber;
    procedure SetAsNumber(const Value: TNumber);
    function GetAsInt: Int64;
    procedure SetAsInt(const Value: Int64);
    function GetAsFloat: Double;
    procedure SetAsFloat(const Value: Double);
    function GetAsBool: Boolean;
    procedure SetAsBool(const Value: Boolean);
    function GetValue(AKey: string): TJValue; overload;
    procedure SetValue(AKey: string; const Value: TJValue); overload;
    function GetValue(AIndex: Integer): TJValue; overload;
    procedure SetValue(AIndex: Integer; const Value: TJValue); overload;

  protected
    procedure FormatInternal(ABuilder: TStringBuilder); virtual; abstract;

  public
    class function GetTypeName: string; virtual;
    function Format: string;

    function Cast<T: TJValue>: T; inline;

    property AsObject: TJObject read GetAsObject;
    property AsArray: TJArray read GetAsArray;
    property AsString: string read GetAsString write SetAsString;
    property AsNumber: TNumber read GetAsNumber write SetAsNumber;
    property AsInt: Int64 read GetAsInt write SetAsInt;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsBool: Boolean read GetAsBool write SetAsBool;

    function ObjectOrNil: TJObject;
    function ArrayOrNil: TJArray;
    function StringOrDefault(ADefault: string = ''): string;
    function NumberOrDefault(ADefault: TNumber): TNumber;
    function IntOrDefault(ADefault: Int64 = 0): Int64;
    function FloatOrDefault(ADefault: Double = 0): Double;
    function BoolOrDefault(ADefault: Boolean = False): Boolean;

    /// <returns>True, if the value is null or actually nil.</returns>
    function IsNull: Boolean; inline;
    /// <returns>True, if the valus it not null or actually nil.</returns>
    function Exists: Boolean; inline;

    property Values[AKey: string]: TJValue read GetValue write SetValue; default;
    property Values[AIndex: Integer]: TJValue read GetValue write SetValue; default;

    function GetEnumerator: IIterator<TJPair>;

  end;

  TJValues = TRefArray<TJValue>;

  TJPairHelper = record helper for TJPair
  private
    function GetAsObject: TJObject; inline;
    function GetAsArray: TJArray; inline;
    function GetAsString: string; inline;
    procedure SetAsString(const Value: string); inline;
    function GetAsNumber: TJValue.TNumber; inline;
    procedure SetAsNumber(const Value: TJValue.TNumber); inline;
    function GetAsInt: Int64; inline;
    procedure SetAsInt(const Value: Int64); inline;
    function GetAsFloat: Double; inline;
    procedure SetAsFloat(const Value: Double); inline;
    function GetAsBool: Boolean; inline;
    procedure SetAsBool(const Value: Boolean); inline;

  public
    property AsObject: TJObject read GetAsObject;
    property AsArray: TJArray read GetAsArray;
    property AsString: string read GetAsString write SetAsString;
    property AsNumber: TJValue.TNumber read GetAsNumber write SetAsNumber;
    property AsInt: Int64 read GetAsInt write SetAsInt;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsBool: Boolean read GetAsBool write SetAsBool;

    function ObjectOrNil: TJObject; inline;
    function ArrayOrNil: TJArray; inline;
    function StringOrDefault(ADefault: string = ''): string; inline;
    function NumberOrDefault(ADefault: TJValue.TNumber): TJValue.TNumber; inline;
    function IntOrDefault(ADefault: Int64 = 0): Int64; inline;
    function FloatOrDefault(ADefault: Double = 0): Double; inline;
    function BoolOrDefault(ADefault: Boolean = False): Boolean; inline;

  end;

  /// <summary>
  /// <p>A string-key to value map.</p>
  /// <p>Example:</p>
  /// <code>
  /// {<p/>
  /// "a": 42,<p/>
  /// "b": true,<p/>
  /// "c": "hello world"<p/>
  /// }
  /// </code>
  /// </summary>
  TJObject = class(TJValue)
  public type

    TEmptyIterator = class(TInterfacedObject, IIterator<TJPair>)
    public
      function MoveNext: Boolean;
      function GetCurrent: TJPair;

    end;

    TParser = class(TObjectParser<TJObject>)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

    TOrder = TToObjectPairArray<string, TJValue>;

    TMap = TToRefMap<string, TJValue, TStringHasher>;

  private
    FOrder: TOrder;
    FMap: TMap;

    function GetOrder: TOrder.TReader;
    function GetValue(AKey: string): TJValue;
    procedure SetValue(AKey: string; const Value: TJValue); overload;

  protected
    procedure FormatInternal(ABuilder: TStringBuilder); override;

  public
    constructor Create;
    destructor Destroy; override;

    class function GetTypeName: string; override;

    /// <summary>Gets the json-value to the given key.</summary>
    /// <returns>True, if the key exists.</returns>
    function Get(AKey: string; out AValue: TJValue): Boolean; overload;
    /// <summary>Gets the json-value to the given key as the specified type.</summary>
    /// <returns>True, if the key exists and matches the given type.</returns>
    function Get<T: TJValue>(AKey: string; out AValue: T): Boolean; overload;
    /// <summary>Gets the json-value to the given key as the specified type.</summary>
    /// <returns><c>nil</c>, if the value doesn't match the given type or doesn't exist in the first place.</returns>
    function Get<T: TJValue>(AKey: string): T; overload;

    /// <summary>Grants read and write access to all values.</summary>
    /// <remarks>The getter will return nil for non-existent keys or if the object itself is nil.</remarks>
    property Values[AKey: string]: TJValue read GetValue write SetValue; default;

    procedure Add(AKey: string; AValue: string); overload;
    procedure Add(AKey: string; AValue: TJValue.TNumber); overload;
    procedure Add(AKey: string; AValue: Boolean); overload;
    function AddObject(AKey: string): TJObject;
    function AddArray(AKey: string): TJArray;

    procedure Remove(AKey: string);

    property Order: TOrder.TReader read GetOrder;

    function GetEnumerator: IIterator<TJPair>;

    class function Parse(AText: string): TJObject;

  end;

  /// <summary>
  /// <p>An array of values.</p>
  /// <p>Example:</p>
  /// <code>
  /// [<p/>
  /// 42,<p/>
  /// true,<p/>
  /// "hello world"<p/>
  /// ]
  /// </code>
  /// </summary>
  TJArray = class(TJValue)
  public type

    TEmptyIterator = class(TInterfacedObject, IIterator<TJValue>)
    public
      function MoveNext: Boolean;
      function GetCurrent: TJValue;

    end;

    TParser = class(TObjectParser<TJArray>)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

    TValues = TObjectArray<TJValue>;

  private
    FValues: TValues;

  protected
    procedure FormatInternal(ABuilder: TStringBuilder); override;

  public
    constructor Create;
    destructor Destroy; override;

    class function GetTypeName: string; override;

    property Values: TValues read FValues;

    function GetEnumerator: IIterator<TJValue>;

  end;

  /// <summary>
  /// A double quoted string with backslash escaping.
  /// Example: <c>"Hello\nWorld"</c>
  /// </summary>
  TJString = class(TJValue)
  public type

    TStringParser = class(TParser<string>)
    public const

      HexChars = ['0' .. '9', 'a' .. 'f', 'A' .. 'F'];

    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

    TParser = class(TParser<TJString>)
    protected
      function Parse: Boolean; override;

    end;

  private
    FText: string;

    function GetQuoted: string;

  protected
    procedure FormatInternal(ABuilder: TStringBuilder); override;

  public
    constructor Create(AText: string); overload;

    class function GetTypeName: string; override;

    property Text: string read FText write FText;
    property Quoted: string read GetQuoted;

    class function Quote(AText: string): string;

  end;

  /// <summary>A general type for both integer and floating point numbers.</summary>
  TJNumber = class(TJValue)
  public const

    ValidInitialChars = ['0' .. '9', '-', '+', '.'];

    ValidChars = ValidInitialChars + ['e', 'E'];

  public type

    TParser = class(TParser<TJNumber>)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  private
    FNumber: TJValue.TNumber;

  protected
    procedure FormatInternal(ABuilder: TStringBuilder); override;

  public
    constructor Create(ANumber: TJValue.TNumber); overload;

    class function GetTypeName: string; override;

    property Number: TJValue.TNumber read FNumber write FNumber;

    function TryGetInt(out AValue: Int64): Boolean;

  end;

  /// <summary>A boolean, <c>true</c> or <c>false</c>.</summary>
  TJBool = class(TJValue)
  public const

    BoolStrings: array [Boolean] of string = (
      'false',
      'true'
      );

  public type

    TParser = class(TParser<TJBool>)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  private
    FValue: Boolean;

  protected
    procedure FormatInternal(ABuilder: TStringBuilder); override;

  public
    constructor Create(AValue: Boolean); overload;

    class function GetTypeName: string; override;

    property Value: Boolean read FValue write FValue;

  end;

  /// <summary>The identifier <c>null</c>.</summary>
  TJNull = class(TJValue)
  public const

    NullString = 'null';

  public type

    TParser = class(TParser<TJNull>)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  protected
    procedure FormatInternal(ABuilder: TStringBuilder); override;

  public
    class function GetTypeName: string; override;

  end;

implementation

{ TJValue }

function TJValue.GetAsArray: TJArray;
begin
  Result := Cast<TJArray>;
end;

function TJValue.GetAsBool: Boolean;
begin
  Result := Cast<TJBool>.Value;
end;

function TJValue.GetAsFloat: Double;
begin
  Result := Cast<TJNumber>.Number;
end;

function TJValue.GetAsInt: Int64;
begin
  if not Cast<TJNumber>.TryGetInt(Result) then
    raise EJSONError.Create('Number does not have an integer representation.');
end;

function TJValue.GetAsNumber: TNumber;
begin
  Result := Cast<TJNumber>.Number;
end;

function TJValue.GetAsObject: TJObject;
begin
  Result := Cast<TJObject>;
end;

function TJValue.GetAsString: string;
begin
  Result := Cast<TJString>.Text;
end;

function TJValue.GetEnumerator: IIterator<TJPair>;
begin
  Result := AsObject.GetEnumerator;
end;

function TJValue.ArrayOrNil: TJArray;
begin
  if Exists then
    Exit(AsArray);
  Result := nil;
end;

function TJValue.BoolOrDefault(ADefault: Boolean): Boolean;
begin
  if Exists then
    Exit(AsBool);
  Result := ADefault;
end;

function TJValue.Cast<T>: T;
begin
  if Self is T then
    Exit(T(Self));
  if IsNull then
    raise EJSONError.CreateFmt('Expected %s got null.', [T.GetTypeName]);
  raise EJSONError.CreateFmt('Expected %s, got %s.', [T.GetTypeName, GetTypeName]);
end;

function TJValue.Exists: Boolean;
begin
  Result := not IsNull;
end;

function TJValue.FloatOrDefault(ADefault: Double): Double;
begin
  if Exists then
    Exit(AsFloat);
  Result := ADefault;
end;

function TJValue.Format: string;
var
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create;
  try
    FormatInternal(Builder);
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

class function TJValue.GetTypeName: string;
begin
  Result := 'JSON-Value';
end;

function TJValue.GetValue(AIndex: Integer): TJValue;
begin
  Result := ArrayOrNil[AIndex];
end;

function TJValue.GetValue(AKey: string): TJValue;
begin
  Result := ObjectOrNil[AKey];
end;

function TJValue.IntOrDefault(ADefault: Int64): Int64;
begin
  if Exists then
    Exit(AsInt);
  Result := ADefault;
end;

function TJValue.IsNull: Boolean;
begin
  Result := (Self = nil) or (ClassType = TJNull);
end;

function TJValue.NumberOrDefault(ADefault: TNumber): TNumber;
begin
  if Exists then
    Exit(AsNumber);
  Result := ADefault;
end;

function TJValue.ObjectOrNil: TJObject;
begin
  if Exists then
    Exit(AsObject);
  Result := nil;
end;

procedure TJValue.SetAsBool(const Value: Boolean);
begin
  Cast<TJBool>.Value := Value;
end;

procedure TJValue.SetAsFloat(const Value: Double);
begin
  Cast<TJNumber>.Number := Value;
end;

procedure TJValue.SetAsInt(const Value: Int64);
begin
  Cast<TJNumber>.Number := Value;
end;

procedure TJValue.SetAsNumber(const Value: TNumber);
begin
  Cast<TJNumber>.Number := Value;
end;

procedure TJValue.SetAsString(const Value: string);
begin
  Cast<TJString>.Text := Value;
end;

procedure TJValue.SetValue(AIndex: Integer; const Value: TJValue);
begin
  AsArray[AIndex] := Value;
end;

procedure TJValue.SetValue(AKey: string; const Value: TJValue);
begin
  AsObject[AKey] := Value;
end;

function TJValue.StringOrDefault(ADefault: string): string;
begin
  if Exists then
    Exit(AsString);
  Result := ADefault;
end;

{ TJObject }

procedure TJObject.Add(AKey, AValue: string);
begin
  Values[AKey] := TJString.Create(AValue);
end;

procedure TJObject.Add(AKey: string; AValue: TJValue.TNumber);
begin
  Values[AKey] := TJNumber.Create(AValue);
end;

procedure TJObject.Add(AKey: string; AValue: Boolean);
begin
  Values[AKey] := TJBool.Create(AValue);
end;

function TJObject.AddArray(AKey: string): TJArray;
begin
  Result := TJArray.Create;
  Values[AKey] := Result;
end;

function TJObject.AddObject(AKey: string): TJObject;
begin
  Result := TJObject.Create;
  Values[AKey] := Result;
end;

constructor TJObject.Create;
begin
  FOrder := TOrder.Create;
  FMap := TMap.Create;
end;

destructor TJObject.Destroy;
begin
  FMap.Free;
  FOrder.Free;
  inherited;
end;

procedure TJObject.FormatInternal(ABuilder: TStringBuilder);

  procedure AppendIndex(I: Integer);
  begin
    ABuilder.Append(TJString.Quote(Order[I].Key));
    ABuilder.Append(': ');
    Order[I].Value.FormatInternal(ABuilder);
  end;

var
  I: Integer;
begin
  ABuilder.Append('{');
  if not Order.Empty then
  begin
    AppendIndex(0);
    for I := 1 to Order.MaxIndex do
    begin
      ABuilder.Append(', ');
      AppendIndex(I);
    end;
  end;
  ABuilder.Append('}');
end;

function TJObject.Get(AKey: string; out AValue: TJValue): Boolean;
begin
  Result := FMap.Get(AKey, AValue);
end;

function TJObject.Get<T>(AKey: string): T;
var
  Value: TJValue;
begin
  if FMap.Get(AKey, Value) and (Value is T) then
    Exit(T(Value));
  Result := nil;
end;

function TJObject.Get<T>(AKey: string; out AValue: T): Boolean;
var
  Value: TJValue;
begin
  Result := FMap.Get(AKey, Value);
  if Result and (Value is T) then
    AValue := T(Value)
  else
    Result := False;
end;

function TJObject.GetEnumerator: IIterator<TJPair>;
begin
  if Exists then
    Result := Order.GetEnumerator
  else
    Result := TEmptyIterator.Create;
end;

function TJObject.GetOrder: TOrder.TReader;
begin
  Result := FOrder.Reader;
end;

class function TJObject.GetTypeName: string;
begin
  Result := 'JSON-Object';
end;

function TJObject.GetValue(AKey: string): TJValue;
begin
  if Self.IsNull or not FMap.Get(AKey, Result) then
    Result := nil;
end;

class function TJObject.Parse(AText: string): TJObject;
var
  Parser: TParser;
begin
  Parser := TParser.Create(AText, False);
  try
    if Parser.Success then
      Result := Parser.OwnParseResult
    else
      raise EJSONError.Create('Could not parse json.');
  finally
    Parser.Free;
  end;
end;

procedure TJObject.Remove(AKey: string);
begin
  FOrder.RemoveAt(Order.FindFirstIndex(
    function(APair: TJPair): Boolean
    begin
      Result := APair.Key = AKey;
    end
    ));
  FMap.Remove(AKey);
end;

procedure TJObject.SetValue(AKey: string; const Value: TJValue);
begin
  FMap[AKey] := Value;
  FOrder.Add(TJPair.Create(AKey, Value));
end;

{ TJArray }

constructor TJArray.Create;
begin
  FValues := TValues.Create;
end;

destructor TJArray.Destroy;
begin
  FValues.Free;
  inherited;
end;

procedure TJArray.FormatInternal(ABuilder: TStringBuilder);
var
  I: Integer;
begin
  ABuilder.Append('[');
  if not Values.Empty then
  begin
    Values[0].FormatInternal(ABuilder);
    for I := 1 to Values.MaxIndex do
    begin
      ABuilder.Append(', ');
      Values[I].FormatInternal(ABuilder);
    end;
  end;
  ABuilder.Append(']');
end;

function TJArray.GetEnumerator: IIterator<TJValue>;
begin
  if Exists then
    Result := Values.GetEnumerator
  else
    Result := TEmptyIterator.Create;
end;

class function TJArray.GetTypeName: string;
begin
  Result := 'JSON-Array';
end;

{ TJNull }

procedure TJNull.FormatInternal(ABuilder: TStringBuilder);
begin
  ABuilder.Append(NullString);
end;

class function TJNull.GetTypeName: string;
begin
  Result := 'JSON-Null';
end;

{ TJBool }

constructor TJBool.Create(AValue: Boolean);
begin
  FValue := AValue;
end;

procedure TJBool.FormatInternal(ABuilder: TStringBuilder);
begin
  ABuilder.Append(BoolStrings[Value]);
end;

class function TJBool.GetTypeName: string;
begin
  Result := 'JSON-Bool';
end;

{ TJString }

constructor TJString.Create(AText: string);
begin
  FText := AText;
end;

procedure TJString.FormatInternal(ABuilder: TStringBuilder);
begin
  ABuilder.Append(Quoted);
end;

function TJString.GetQuoted: string;
begin
  Result := Quote(Text);
end;

class function TJString.GetTypeName: string;
begin
  Result := 'JSON-String';
end;

class function TJString.Quote(AText: string): string;
var
  Builder: TStringBuilder;
  LastSplit, Current, Len: Integer;
begin
  Builder := TStringBuilder.Create('"');
  LastSplit := 1;
  Current := 1;
  Len := AText.Length;
  while Current <= Len do
  begin
    if CharInSet(AText[Current], ['\', '"', #8, #9, #10, #12, #13]) then
    begin
      Builder.Append(AText, LastSplit - 1, Current - LastSplit);
      Builder.Append('\');
      case AText[Current] of
        '\', '"':
          Builder.Append(AText[Current]);
        #8: // backspace
          Builder.Append('b');
        #9: // horizontal tab
          Builder.Append('t');
        #10: // line feed
          Builder.Append('n');
        #12: // form feed
          Builder.Append('f');
        #13: // carriage return
          Builder.Append('r');
      end;
      LastSplit := Current + 1;
    end
    else if (AText[Current] < #$20) or (AText[Current] > #$FF) or
      (AText[Current] > #$7E) and (AText[Current] < #$A0) then
    begin
      Builder.Append(AText, LastSplit - 1, Current - LastSplit);
      Builder.Append('\u');
      Builder.Append(IntToHex(Word(AText[Current]), 4));
      LastSplit := Current + 1;
    end;
    Inc(Current);
  end;
  Builder.Append(AText, LastSplit - 1, Current - LastSplit);
  Builder.Append('"');
  Result := Builder.ToString;
  Builder.Free;
end;

{ TJNumber }

constructor TJNumber.Create(ANumber: TJValue.TNumber);
begin
  FNumber := ANumber;
end;

procedure TJNumber.FormatInternal(ABuilder: TStringBuilder);
begin
  if Number.IsFloat then
    ABuilder.Append(PrettyFloat(Number.AsFloat))
  else
    ABuilder.Append(Number.AsInt);
end;

class function TJNumber.GetTypeName: string;
begin
  Result := 'JSON-Number';
end;

function TJNumber.TryGetInt(out AValue: Int64): Boolean;
begin
  if Number.IsFloat then
  begin
    if Frac(Number.AsFloat) <> 0 then
      Exit(False);
    AValue := Trunc(Number.AsFloat);
  end
  else
    AValue := Number.AsInt;
  Result := True;
end;

{ TJValue.TNumber }

class operator TJValue.TNumber.Implicit(ANumber: TNumber): Double;
begin
  if ANumber.IsFloat then
    Result := ANumber.AsFloat
  else
    Result := ANumber.AsInt;
end;

class operator TJValue.TNumber.Implicit(ANumber: Int64): TNumber;
begin
  Result.IsFloat := False;
  Result.AsInt := ANumber;
end;

class operator TJValue.TNumber.Implicit(ANumber: Double): TNumber;
begin
  Result.IsFloat := True;
  Result.AsFloat := ANumber;
end;

{ TJObject.TParser }

class function TJObject.TParser.GetResultName: string;
begin
  Result := TJObject.GetTypeName;
end;

function TJObject.TParser.Parse: Boolean;
var
  Key: string;
begin
  if not StartsWith('{') then
    Exit(False);

  SkipWhitespace;

  SetParseResult(TJObject.Create);

  if StartsWith('}') then
    Exit(True);

  while True do
  begin
    Key := TJString.TStringParser.Require(Info);

    SkipWhitespace;

    if not StartsWith(':') then
      raise EParseError.Create('Expected ":" followed by JSON-Value.');

    SkipWhitespace;

    ParseResult[Key] := TJValue.TParser.Require(Info);

    SkipWhitespace;

    if StartsWith('}') then
      Break;
    if not StartsWith(',') then
      raise EParseError.Create('Expected "," followed by key or "}" to close the object.');

    SkipWhitespace;

  end;
  Result := True;
end;

{ TJString.TStringParser }

class function TJString.TStringParser.GetResultName: string;
begin
  Result := TJString.GetTypeName;
end;

function TJString.TStringParser.Parse: Boolean;
var
  Builder: TStringBuilder;
  Escaped: Char;
  Marker: TLogMarker;
  HexString: string;
begin
  if not StartsWith('"') then
    Exit(False);

  Builder := TStringBuilder.Create;
  try
    while not StartsWith('"') do
    begin
      if ReachedEnd then
      begin
        raise EParseError.Create('Found unterminated string.');
      end;

      Marker := GetMarker;
      if StartsWith('\') then
      begin
        Escaped := First;
        Advance;
        case Escaped of
          '\', '"', '/':
            Builder.Append(Escaped);
          'b': // backspace
            Builder.Append(#8);
          't': // horizontal tab
            Builder.Append(#9);
          'n': // line feed
            Builder.Append(#10);
          'f': // form feed
            Builder.Append(#12);
          'r': // carriage return
            Builder.Append(#13);
          'u':
            begin
              HexString := ReadWhile(HexChars, 4);
              if HexString.Length <> 4 then
                Log(Marker, 'Expected 4 hexadecimal digits.', elFatal)
              else
                Builder.Append(Char(StrToInt('$' + HexString)));
            end;
        else
          Log(Marker, 'Invalid escape sequence: \%s', [Escaped], elFatal);
        end;
      end
      else
      begin
        Builder.Append(First);
        Advance;
      end;

    end;

    SetParseResult(Builder.ToString);
    Result := True;

  finally
    Builder.Free;

  end;
end;

{ TJString.TParser }

function TJString.TParser.Parse: Boolean;
begin
  SetParseResult(TJString.Create(TStringParser.Require(Info)));
  Result := True;
end;

{ TJValue.TParser }

class function TJValue.TParser.GetResultName: string;
begin
  Result := TJValue.GetTypeName;
end;

function TJValue.TParser.Parse: Boolean;
var
  ParserClass: TParserClass;
  Parser: TObjectParser<TJValue>;
begin
  case First of
    '"':
      ParserClass := TJString.TParser;
    '0' .. '9', '-', '+', '.':
      ParserClass := TJNumber.TParser;
    '{':
      ParserClass := TJObject.TParser;
    '[':
      ParserClass := TJArray.TParser;
    't', 'f':
      ParserClass := TJBool.TParser;
    'n':
      ParserClass := TJNull.TParser;
  else
    Exit(False);
  end;
  Parser := TObjectParser<TJValue>(ParserClass.Create(Info, False));
  Result := Parser.Success;
  if Result then
    SetParseResult(Parser.OwnParseResult);
  Parser.Free;
end;

{ TJNumber.TParser }

class function TJNumber.TParser.GetResultName: string;
begin
  Result := TJNumber.GetTypeName;
end;

function TJNumber.TParser.Parse: Boolean;
var
  NumText: string;
  IntValue: Integer;
  FloatValue: Double;
begin
  if not CharInSet(First, ValidInitialChars) then
    Exit(False);
  NumText := ReadWhile(ValidChars);
  if TryStrToInt(NumText, IntValue) then
    SetParseResult(TJNumber.Create(IntValue))
  else if TryStrToFloat(NumText, FloatValue, TFormatSettings.Invariant) then
    SetParseResult(TJNumber.Create(FloatValue))
  else
    Exit(False);
  Result := True;
end;

{ TJBool.TParser }

class function TJBool.TParser.GetResultName: string;
begin
  Result := TJBool.GetTypeName;
end;

function TJBool.TParser.Parse: Boolean;
var
  Bool: Boolean;
begin
  for Bool := Low(Boolean) to High(Boolean) do
    if StartsWith(BoolStrings[Bool]) then
    begin
      SetParseResult(TJBool.Create(Bool));
      Exit(True);
    end;
  Result := False;
end;

{ TJNull.TParser }

class function TJNull.TParser.GetResultName: string;
begin
  Result := TJNull.GetTypeName;
end;

function TJNull.TParser.Parse: Boolean;
begin
  if not StartsWith(NullString) then
    Exit(False);
  SetParseResult(TJNull.Create);
  Result := True;
end;

{ TJArray.TParser }

class function TJArray.TParser.GetResultName: string;
begin
  Result := TJArray.GetTypeName;
end;

function TJArray.TParser.Parse: Boolean;
begin
  if not StartsWith('[') then
    Exit(False);

  SkipWhitespace;

  SetParseResult(TJArray.Create);

  if StartsWith(']') then
    Exit(True);

  while True do
  begin

    ParseResult.Values.Add(TJValue.TParser.Require(Info));

    SkipWhitespace;

    if StartsWith(']') then
      Break;
    if not StartsWith(',') then
      raise EParseError.Create('Expected "," followed by key.');

    SkipWhitespace;

  end;
  Result := True;
end;

{ TJPairHelper }

function TJPairHelper.ArrayOrNil: TJArray;
begin
  Result := Value.ArrayOrNil;
end;

function TJPairHelper.BoolOrDefault(ADefault: Boolean): Boolean;
begin
  Result := Value.BoolOrDefault(ADefault);
end;

function TJPairHelper.FloatOrDefault(ADefault: Double): Double;
begin
  Result := Value.FloatOrDefault(ADefault);
end;

function TJPairHelper.GetAsArray: TJArray;
begin
  Result := Value.GetAsArray;
end;

function TJPairHelper.GetAsBool: Boolean;
begin
  Result := Value.GetAsBool;
end;

function TJPairHelper.GetAsFloat: Double;
begin
  Result := Value.GetAsFloat;
end;

function TJPairHelper.GetAsInt: Int64;
begin
  Result := Value.GetAsInt;
end;

function TJPairHelper.GetAsNumber: TJValue.TNumber;
begin
  Result := Value.GetAsNumber;
end;

function TJPairHelper.GetAsObject: TJObject;
begin
  Result := Value.GetAsObject;
end;

function TJPairHelper.GetAsString: string;
begin
  Result := Value.GetAsString;
end;

function TJPairHelper.IntOrDefault(ADefault: Int64): Int64;
begin
  Result := Value.IntOrDefault(ADefault);
end;

function TJPairHelper.NumberOrDefault(ADefault: TJValue.TNumber): TJValue.TNumber;
begin
  Result := Value.NumberOrDefault(ADefault);
end;

function TJPairHelper.ObjectOrNil: TJObject;
begin
  Result := Value.ObjectOrNil;
end;

procedure TJPairHelper.SetAsBool(const Value: Boolean);
begin
  Self.Value.SetAsBool(Value);
end;

procedure TJPairHelper.SetAsFloat(const Value: Double);
begin
  Self.Value.SetAsFloat(Value);
end;

procedure TJPairHelper.SetAsInt(const Value: Int64);
begin
  Self.Value.SetAsInt(Value);
end;

procedure TJPairHelper.SetAsNumber(const Value: TJValue.TNumber);
begin
  Self.Value.SetAsNumber(Value);
end;

procedure TJPairHelper.SetAsString(const Value: string);
begin
  Self.Value.SetAsString(Value);
end;

function TJPairHelper.StringOrDefault(ADefault: string): string;
begin
  Result := Value.StringOrDefault(ADefault);
end;

{ TJArray.TEmptyIterator }

function TJArray.TEmptyIterator.GetCurrent: TJValue;
begin
  // never actually called
  Result := nil;
end;

function TJArray.TEmptyIterator.MoveNext: Boolean;
begin
  Result := False;
end;

{ TJObject.TEmptyIterator }

function TJObject.TEmptyIterator.GetCurrent: TJPair;
begin
  // never actually called
  Result.Create('', nil);
end;

function TJObject.TEmptyIterator.MoveNext: Boolean;
begin
  Result := False;
end;

end.
