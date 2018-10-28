unit Pengine.JSON;

interface

uses
  System.SysUtils,
  System.Math,

  Pengine.IntMaths,
  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.Hasher,
  Pengine.Parser,
  Pengine.CollectionInterfaces,
  Pengine.Utility,
  Pengine.Interfaces,
  System.IOUtils;

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

  TJValueClass = class of TJValue;

  /// <summary>Any json value.</summary>
  TJValue = class
  public type

    /// <summary>A number, which is either an integer or a floating point number.</summary>
    TNumber = record
    public type

      TType = (
        ntInt,
        ntSingle,
        ntDouble
        );

    public
      class operator Implicit(ANumber: TNumber): Double; static;
      class operator Implicit(ANumber: Int64): TNumber; static;
      class operator Implicit(ANumber: Single): TNumber; static;
      class operator Implicit(ANumber: Double): TNumber; static;

      class operator Equal(A, B: TNumber): Boolean; static;
      class operator NotEqual(A, B: TNumber): Boolean; static;

      /// <returns>True, if the current type is a floating point type.</returns>
      function IsFloat: Boolean;

    private
      case NumType: TType of
        ntInt:
          (AsInt: Int64);
        ntSingle, ntDouble:
          (AsFloat: Double);
    end;

    /// <summary>A wrapper, that allows implicit conversion from primitive types to TJValue.</summary>
    TJWrapper = record
    private
      FValue: TJValue;

      function GetAsObject: TJObject; inline;
      function GetAsArray: TJArray; inline;
      function GetAsString: string; inline;
      function GetAsNumber: TNumber; inline;
      function GetAsInt: Int64; inline;
      function GetAsFloat: Double; inline;
      function GetAsBool: Boolean; inline;
      procedure SetAsString(const Value: string); inline;
      procedure SetAsNumber(const Value: TNumber); inline;
      procedure SetAsInt(const Value: Int64); inline;
      procedure SetAsFloat(const Value: Double); inline;
      procedure SetAsBool(const Value: Boolean); inline;

      function GetValue(AKey: string): TJWrapper; overload; inline;
      function GetValue(AIndex: Integer): TJWrapper; overload; inline;

    public
      // Wrapper <-> TJValue
      class operator Implicit(AWrapper: TJWrapper): TJValue; static; inline;
      class operator Implicit(AValue: TJValue): TJWrapper; static; inline;

      // Primitive -> Wrapper
      class operator Implicit(AText: string): TJWrapper; static; inline;
      class operator Implicit(ANumber: TNumber): TJWrapper; static; inline;
      class operator Implicit(ANumber: Double): TJWrapper; static; inline;
      class operator Implicit(ANumber: Single): TJWrapper; static; inline;
      class operator Implicit(ANumber: Int64): TJWrapper; static; inline;
      class operator Implicit(AValue: Boolean): TJWrapper; static; inline;

      // Wrapper -> Primitve
      class operator Implicit(AWrapper: TJWrapper): string; static; inline;
      class operator Implicit(AWrapper: TJWrapper): TNumber; static; inline;
      class operator Implicit(AWrapper: TJWrapper): Double; static; inline;
      class operator Implicit(AWrapper: TJWrapper): Single; static; inline;
      class operator Implicit(AWrapper: TJWrapper): Int64; static; inline;
      class operator Implicit(AWrapper: TJWrapper): Boolean; static; inline;

      // TJValue relay functions
      function Copy: TJValue; inline;

      // or default operator
      class operator LogicalOr(AWrapper: TJWrapper; ADefault: string): string; static; inline;
      class operator LogicalOr(AWrapper: TJWrapper; ADefault: Boolean): Boolean; static; inline;
      class operator LogicalOr(AWrapper: TJWrapper; ADefault: Int64): Int64; static; inline;
      class operator LogicalOr(AWrapper: TJWrapper; ADefault: Single): Single; static; inline;
      class operator LogicalOr(AWrapper: TJWrapper; ADefault: Double): Double; static; inline;

      function GetTypeName: string; inline;
      function Format(APretty: Boolean = True; AIndentWidth: Integer = 2): string; inline;

      function Cast<T: TJValue>: T; inline;

      function IsNull: Boolean; inline;
      function Exists: Boolean; inline;

      property Value: TJValue read FValue;
      property AsObject: TJObject read GetAsObject;
      property AsArray: TJArray read GetAsArray;
      property AsString: string read GetAsString write SetAsString;
      property AsNumber: TNumber read GetAsNumber write SetAsNumber;
      property AsInt: Int64 read GetAsInt write SetAsInt;
      property AsFloat: Double read GetAsFloat write SetAsFloat;
      property AsBool: Boolean read GetAsBool write SetAsBool;

      function StringOrDefault(ADefault: string = ''): string; inline;
      function NumberOrDefault(ADefault: TNumber): TNumber; inline;
      function IntOrDefault(ADefault: Int64 = 0): Int64; inline;
      function FloatOrDefault(ADefault: Double = 0): Double; inline;
      function BoolOrDefault(ADefault: Boolean = False): Boolean; inline;

      /// <summary>Interprets as TJObject and gets or sets the value at the specified key.</summary>
      /// <remarks>Returns nil for null or none existent keys.</remarks>
      property Values[AKey: string]: TJWrapper read GetValue; default;
      /// <summary>Interprets as TJArray and gets or sets the value at the specified index.</summary>
      /// <remarks>Returns nil for null or none existent indices.</remarks>
      property Values[AIndex: Integer]: TJWrapper read GetValue; default;

      procedure Assign(AFrom: TJValue); inline;

      function GetEnumerator: IIterator<TJPair>; inline;
      function ToString: string; inline;

    end;

    TParser = class(TObjectParser<TJValue>)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  protected type

    TJStringBuilder = class(TStringBuilder)
    private
      FPretty: Boolean;
      FArrayWrapThreshold: Integer;
      FIndentWidth: Integer;
      FIndentLevel: Integer;

    public
      constructor Create(APretty: Boolean; AIndentWidth: Integer; AArrayWrapTheshold: Integer);

      property Pretty: Boolean read FPretty;
      property IndentWith: Integer read FIndentWidth;
      property ArrayWrapTheshold: Integer read FArrayWrapThreshold;

      procedure Indent; inline;
      procedure Unindent; inline;
      procedure AddIndentation; inline;
      procedure NewLine; inline;
      property IndentLevel: Integer read FIndentLevel;

    end;

  private
    function GetAsObject: TJObject;
    function GetAsArray: TJArray;
    function GetAsString: string;
    function GetAsNumber: TNumber;
    function GetAsInt: Int64;
    function GetAsFloat: Double;
    function GetAsBool: Boolean;
    procedure SetAsString(const Value: string);
    procedure SetAsNumber(const Value: TNumber);
    procedure SetAsInt(const Value: Int64);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsBool(const Value: Boolean);

    function GetValue(AKey: string): TJWrapper; overload;
    function GetValue(AIndex: Integer): TJWrapper; overload;

  protected
    procedure FormatInternal(ABuilder: TJStringBuilder); virtual; abstract;

  public
    constructor Create; virtual;
    function Copy: TJValue;

    class function GetTypeName: string; virtual;
    function Format(APretty: Boolean = True; AIndentWidth: Integer = 2; AArrayWrapThreshold: Integer = 3): string;

    /// <returns>True, if the value is null or nil.</returns>
    function IsNull: Boolean; inline;
    /// <returns>True, if the valus it not null or nil.</returns>
    function Exists: Boolean; inline;

    /// <returns>The TJValue as the given type, or nil, if the value is null or nil.</returns>
    function Cast<T: TJValue>: T; inline;

    property AsObject: TJObject read GetAsObject;
    property AsArray: TJArray read GetAsArray;
    property AsString: string read GetAsString write SetAsString;
    property AsNumber: TNumber read GetAsNumber write SetAsNumber;
    property AsInt: Int64 read GetAsInt write SetAsInt;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsBool: Boolean read GetAsBool write SetAsBool;

    function StringOrDefault(ADefault: string = ''): string;
    function NumberOrDefault(ADefault: TNumber): TNumber;
    function IntOrDefault(ADefault: Int64 = 0): Int64;
    function FloatOrDefault(ADefault: Double = 0): Double;
    function BoolOrDefault(ADefault: Boolean = False): Boolean;

    /// <summary>Interprets as TJObject and gets or sets the value at the specified key.</summary>
    /// <remarks>Returns nil for null or none existent keys.</remarks>
    property Values[AKey: string]: TJWrapper read GetValue; default;
    /// <summary>Interprets as TJArray and gets or sets the value at the specified index.</summary>
    /// <remarks>Returns nil for null or none existent indices.</remarks>
    property Values[AIndex: Integer]: TJWrapper read GetValue; default;

    procedure Assign(AFrom: TJValue); virtual;

    function GetEnumerator: IIterator<TJPair>;
    function ToString: string; override;

    class function Parse(AText: string): TJValue;
    class function CreateFromFile(APath: string): TJValue;

  end;

  TJValues = TRefArray<TJValue>;

  TJWrapper = TJValue.TJWrapper;

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
    function IsNull: Boolean; inline;
    function Exists: Boolean; inline;

    property AsObject: TJObject read GetAsObject;
    property AsArray: TJArray read GetAsArray;
    property AsString: string read GetAsString write SetAsString;
    property AsNumber: TJValue.TNumber read GetAsNumber write SetAsNumber;
    property AsInt: Int64 read GetAsInt write SetAsInt;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsBool: Boolean read GetAsBool write SetAsBool;

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
    public const

      TokenBrackets = 1;
      TokenColon = 2;
      TokenComma = 3;

      TokenNames: array [TokenBrackets .. TokenComma] of string = (
        'Brackets',
        'Colon',
        'Comma'
        );

    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;
      class function GetTokenCount: Integer; override;
      class function GetTokenName(AIndex: Integer): string; override;

    end;

    TOrder = TToObjectPairArray<string, TJValue>;

    TMap = TToRefMap<string, TJValue, TStringHasher>;

  private
    FOrder: TOrder;
    FMap: TMap;

    function GetOrder: TOrder.TReader;
    function GetValue(AKey: string): TJValue.TJWrapper;
    procedure SetValue(AKey: string; const Value: TJValue.TJWrapper); overload;
    function GetCount: Integer;

  protected
    procedure FormatInternal(ABuilder: TJValue.TJStringBuilder); override;

  public
    constructor Create; override;
    destructor Destroy; override;

    function Copy: TJObject;

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
    property Values[AKey: string]: TJValue.TJWrapper read GetValue write SetValue; default;

    property Count: Integer read GetCount;
    procedure Clear;

    procedure Add(AKey: string; AValue: TJValue.TJWrapper);
    function AddObject(AKey: string): TJObject;
    function AddArray(AKey: string): TJArray;
    function AddNull(AKey: string): TJNull;

    procedure Remove(AKey: string);
    function Extract(AKey: string): TJValue; overload;
    function Extract<T: TJValue>(AKey: string): T; overload;

    property Order: TOrder.TReader read GetOrder;

    function GetEnumerator: IIterator<TJPair>;
    function Equals(AObject: TObject): Boolean; override;

    procedure Assign(AFrom: TJValue); override;

    class function Parse(AText: string): TJObject;
    class function CreateFromFile(APath: string): TJObject;

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
    public const

      TokenBrackets = 1;
      TokenComma = 2;

      TokenNames: array [TokenBrackets .. TokenComma] of string = (
        'Brackets',
        'Comma'
        );

    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;
      class function GetTokenCount: Integer; override;
      class function GetTokenName(AIndex: Integer): string; override;

    end;

    TValues = TObjectArray<TJValue>;

  private
    FValues: TValues;

    function GetCount: Integer;
    function GetMaxIndex: Integer;
    function GetValue(AIndex: Integer): TJValue.TJWrapper;
    procedure SetValue(AIndex: Integer; const Value: TJValue.TJWrapper);

  protected
    procedure FormatInternal(ABuilder: TJValue.TJStringBuilder); override;

  public
    constructor Create; override;
    destructor Destroy; override;

    function Copy: TJArray;

    class function GetTypeName: string; override;

    property Values[AIndex: Integer]: TJValue.TJWrapper read GetValue write SetValue; default;
    property Count: Integer read GetCount;
    property MaxIndex: Integer read GetMaxIndex;
    function Empty: Boolean;

    procedure Add(AValue: TJValue.TJWrapper);
    function AddObject: TJObject;
    function AddArray: TJArray;
    function AddNull: TJNull;

    function Extract(AIndex: Integer): TJValue; overload;
    function Extract<T: TJValue>(AIndex: Integer): T; overload;

    function GetEnumerator: IIterator<TJValue>;
    function Equals(AObject: TObject): Boolean; override;

    procedure Assign(AFrom: TJValue); override;

    class function Parse(AText: string): TJArray;
    class function CreateFromFile(APath: string): TJArray;

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

      TokenQuotes = 1;
      TokenContent = 2;
      TokenBackslash = 3;
      TokenEscaped = 4;
      TokenUnicodeHex = 5;

      TokenNames: array [TokenQuotes .. TokenUnicodeHex] of string = (
        'Quotes',
        'Content',
        'Backslash',
        'Escaped',
        'Unicode-Hex'
        );

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
    procedure FormatInternal(ABuilder: TJValue.TJStringBuilder); override;

  public
    constructor Create(AText: string); reintroduce; overload;

    class function GetTypeName: string; override;

    property Text: string read FText write FText;
    property Quoted: string read GetQuoted;

    class function Quote(AText: string): string;

    function Equals(AObject: TObject): Boolean; override;

    procedure Assign(AValue: TJValue); override;

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
    procedure FormatInternal(ABuilder: TJValue.TJStringBuilder); override;

  public
    constructor Create(ANumber: TJValue.TNumber); reintroduce; overload;

    class function GetTypeName: string; override;

    property Number: TJValue.TNumber read FNumber write FNumber;

    function TryGetInt(out AValue: Int64): Boolean;

    function Equals(AObject: TObject): Boolean; override;

    procedure Assign(AFrom: TJValue); override;

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
    public const

      TokenFalse = 1;
      TokenTrue = 2;

      TokenNames: array [TokenFalse .. TokenTrue] of string = (
        'False',
        'True'
        );

    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;
      class function GetTokenCount: Integer; override;
      class function GetTokenName(AIndex: Integer): string; override;

    end;

  private
    FValue: Boolean;

  protected
    procedure FormatInternal(ABuilder: TJValue.TJStringBuilder); override;

  public
    constructor Create(AValue: Boolean); reintroduce; overload;

    class function GetTypeName: string; override;

    property Value: Boolean read FValue write FValue;

    function Equals(AObject: TObject): Boolean; override;

    procedure Assign(AFrom: TJValue); override;

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
    procedure FormatInternal(ABuilder: TJValue.TJStringBuilder); override;

  public
    class function GetTypeName: string; override;

    function Equals(AObject: TObject): Boolean; override;

  end;

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

    TMode = (
      smSerialize,
      smUnserialize
      );

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

    procedure Define(AName: string; var ANumber: Int64); overload;
    procedure Define(AName: string; var ANumber: Integer); overload;
    procedure Define(AName: string; var ANumber: Single); overload;
    procedure Define(AName: string; var ANumber: Double); overload;
    procedure Define(AName: string; var AText: string); overload;
    procedure Define(AName: string; var AValue: Boolean); overload;
    procedure Define(AName: string; AObject: IJSerializable; AVersion: Integer = -1); overload;
    procedure Define(AName: string; ASerializer: TJArraySerializer); overload;
    procedure Define(AName: string; ASerializer: TJArrayRefSerializer); overload;

    property Mode: TMode read FMode;
    function IsStoring: Boolean;

    property Value: TJObject read FValue;
    function OwnValue: TJObject;

    property Version: Integer read FVersion;

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

procedure TJValue.Assign(AFrom: TJValue);
begin
  // nothing by default
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
    Exit(nil); // raise EJSONError.CreateFmt('Expected %s got null.', [T.GetTypeName]);
  raise EJSONError.CreateFmt('Expected %s, got %s.', [T.GetTypeName, GetTypeName]);
end;

function TJValue.Copy: TJValue;
begin
  if Self = nil then
    Exit(nil);
  Result := TJValueClass(ClassType).Create;
  Result.Assign(Self);
end;

constructor TJValue.Create;
begin
  // nothing by default
end;

class function TJValue.CreateFromFile(APath: string): TJValue;
begin
  Result := Parse(TFile.ReadAllText(APath));
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

function TJValue.Format(APretty: Boolean; AIndentWidth, AArrayWrapThreshold: Integer): string;
var
  Builder: TJStringBuilder;
begin
  if Self = nil then
    Exit(TJNull.NullString);
  Builder := TJStringBuilder.Create(APretty, AIndentWidth, AArrayWrapThreshold);
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

function TJValue.GetValue(AIndex: Integer): TJWrapper;
begin
  Result := AsArray[AIndex];
end;

function TJValue.GetValue(AKey: string): TJWrapper;
begin
  Result := AsObject[AKey];
end;

function TJValue.IntOrDefault(ADefault: Int64): Int64;
begin
  if Exists then
    Exit(AsInt);
  Result := ADefault;
end;

function TJValue.IsNull: Boolean;
begin
  Result := (Self = nil) or (Self is TJNull);
end;

function TJValue.NumberOrDefault(ADefault: TNumber): TNumber;
begin
  if Exists then
    Exit(AsNumber);
  Result := ADefault;
end;

class function TJValue.Parse(AText: string): TJValue;
var
  Parser: TParser;
begin
  Parser := TParser.Create(AText, False);
  Result := Parser.OwnParseResult;
  Parser.Free;
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

function TJValue.StringOrDefault(ADefault: string): string;
begin
  if Exists then
    Exit(AsString);
  Result := ADefault;
end;

function TJValue.ToString: string;
begin
  Result := Format(False);
end;

{ TJObject }

procedure TJObject.Add(AKey: string; AValue: TJValue.TJWrapper);
begin
  Values[AKey] := AValue;
end;

function TJObject.AddArray(AKey: string): TJArray;
begin
  Result := TJArray.Create;
  Values[AKey] := Result;
end;

function TJObject.AddNull(AKey: string): TJNull;
begin
  Result := TJNull.Create;
  Values[AKey] := Result;
end;

function TJObject.AddObject(AKey: string): TJObject;
begin
  Result := TJObject.Create;
  Values[AKey] := Result;
end;

procedure TJObject.Assign(AFrom: TJValue);
var
  JPair: TJPair;
begin
  if not(AFrom is TJObject) then
    raise EJSONError.Create('Cannot assign value to object.');
  Clear;
  for JPair in TJObject(AFrom).Order do
    Self[JPair.Key] := JPair.Value.Copy;
end;

procedure TJObject.Clear;
begin
  FOrder.Clear;
  FMap.Clear;
end;

function TJObject.Copy: TJObject;
begin
  Result := TJObject(inherited Copy);
end;

constructor TJObject.Create;
begin
  FOrder := TOrder.Create;
  FMap := TMap.Create;
end;

class function TJObject.CreateFromFile(APath: string): TJObject;
begin
  Result := Parse(TFile.ReadAllText(APath));
end;

destructor TJObject.Destroy;
begin
  FMap.Free;
  FOrder.Free;
  inherited;
end;

function TJObject.Equals(AObject: TObject): Boolean;
var
  JPair: TJPair;
begin
  if not(AObject is TJObject) then
    Exit(inherited);
  if Count <> TJObject(AObject).Count then
    Exit(False);
  for JPair in Self do
    if not JPair.Value.Equals(TJObject(AObject)[JPair.Key]) then
      Exit(False);
  Result := True;
end;

function TJObject.Extract(AKey: string): TJValue;
begin
  Result := Self[AKey];
  FOrder.OwnsValues := False;
  Remove(AKey);
  FOrder.OwnsValues := True;
end;

function TJObject.Extract<T>(AKey: string): T;
begin
  Result := Extract(AKey).Cast<T>;
end;

procedure TJObject.FormatInternal(ABuilder: TJValue.TJStringBuilder);

  procedure AppendIndex(I: Integer);
  begin
    ABuilder.Append(TJString.Quote(Order[I].Key));
    ABuilder.Append(': ');
    if Order[I].Value = nil then
      ABuilder.Append(TJNull.NullString)
    else
      Order[I].Value.FormatInternal(ABuilder);
  end;

var
  I: Integer;
begin
  ABuilder.Append('{');
  if not Order.Empty then
  begin
    ABuilder.Indent;
    AppendIndex(0);
    for I := 1 to Order.MaxIndex do
    begin
      ABuilder.Append(',');
      if not ABuilder.Pretty then
        ABuilder.Append(' ')
      else
        ABuilder.NewLine;
      AppendIndex(I);
    end;
    ABuilder.Unindent;
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

function TJObject.GetCount: Integer;
begin
  Result := Order.Count;
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

function TJObject.GetValue(AKey: string): TJValue.TJWrapper;
begin
  if Self.IsNull or not FMap.Get(AKey, Result.FValue) then
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

procedure TJObject.SetValue(AKey: string; const Value: TJValue.TJWrapper);
var
  I: Integer;
begin
  if FMap.KeyExists(AKey) then
    for I := 0 to FOrder.MaxIndex do
      if AKey = FOrder[I].Key then
      begin
        FOrder[I] := TJPair.Create(FOrder[I].Key, Value);
        Exit;
      end;
  FOrder.Add(TJPair.Create(AKey, Value));
  FMap[AKey] := Value;
end;

{ TJArray }

procedure TJArray.Add(AValue: TJValue.TJWrapper);
begin
  FValues.Add(AValue);
end;

function TJArray.AddArray: TJArray;
begin
  Result := TJArray.Create;
  FValues.Add(Result);
end;

function TJArray.AddNull: TJNull;
begin
  Result := TJNull.Create;
  FValues.Add(Result);
end;

function TJArray.AddObject: TJObject;
begin
  Result := TJObject.Create;
  FValues.Add(Result);
end;

procedure TJArray.Assign(AFrom: TJValue);
var
  I: Integer;
begin
  if not(AFrom is TJArray) then
    raise EJSONError.Create('Cannot assign value to array.');
  FValues.Clear;
  FValues.Capacity := TJArray(AFrom).Count;
  FValues.ForceCount(FValues.Capacity);
  for I := 0 to TJArray(AFrom).FValues.MaxIndex do
    Values[I] := TJArray(AFrom).Values[I].Copy;
end;

function TJArray.Copy: TJArray;
begin
  Result := TJArray(inherited Copy);
end;

constructor TJArray.Create;
begin
  FValues := TValues.Create;
end;

class function TJArray.CreateFromFile(APath: string): TJArray;
begin
  Result := Parse(TFile.ReadAllText(APath));
end;

destructor TJArray.Destroy;
begin
  FValues.Free;
  inherited;
end;

function TJArray.Empty: Boolean;
begin
  Result := FValues.Empty;
end;

function TJArray.Equals(AObject: TObject): Boolean;
var
  I: Integer;
begin
  if not(AObject is TJArray) then
    Exit(inherited);
  if Count <> TJArray(AObject).Count then
    Exit(False);
  for I := 0 to MaxIndex do
    if not FValues[I].Equals(TJArray(AObject)[I]) then
      Exit(False);
  Result := True;
end;

function TJArray.Extract(AIndex: Integer): TJValue;
begin
  Result := Values[AIndex];
  FValues.OwnsObjects := False;
  FValues.RemoveAt(AIndex);
  FValues.OwnsObjects := True;
end;

function TJArray.Extract<T>(AIndex: Integer): T;
begin
  Result := Extract(AIndex).Cast<T>;
end;

procedure TJArray.FormatInternal(ABuilder: TJValue.TJStringBuilder);
var
  Wrapped: Boolean;
  I: Integer;
begin
  Wrapped := ABuilder.Pretty;
  if Wrapped then
  begin
    Wrapped := Count > ABuilder.ArrayWrapTheshold;
    if not Wrapped then
      for I := 0 to ABuilder.ArrayWrapTheshold - 1 do
        if (Values[I].Value is TJObject) or (Values[I].Value is TJArray) then
        begin
          Wrapped := True;
          Break;
        end;
  end;
  ABuilder.Append('[');
  if Wrapped then
    ABuilder.Indent;
  if not Empty then
  begin
    FValues[0].FormatInternal(ABuilder);
    for I := 1 to MaxIndex do
    begin
      ABuilder.Append(',');
      if not Wrapped then
        ABuilder.Append(' ')
      else if Wrapped then
        ABuilder.NewLine;
      FValues[I].FormatInternal(ABuilder);
    end;
  end;
  if Wrapped then
    ABuilder.Unindent;
  ABuilder.Append(']');
end;

function TJArray.GetCount: Integer;
begin
  Result := FValues.Count;
end;

function TJArray.GetEnumerator: IIterator<TJValue>;
begin
  if Exists then
    Result := FValues.GetEnumerator
  else
    Result := TEmptyIterator.Create;
end;

function TJArray.GetMaxIndex: Integer;
begin
  Result := FValues.MaxIndex;
end;

class function TJArray.GetTypeName: string;
begin
  Result := 'JSON-Array';
end;

function TJArray.GetValue(AIndex: Integer): TJValue.TJWrapper;
begin
  if FValues.RangeCheck(AIndex) then
    Result := FValues[AIndex]
  else
    Result := nil;
end;

class function TJArray.Parse(AText: string): TJArray;
var
  Parser: TParser;
begin
  Parser := TParser.Create(AText, False);
  Result := Parser.OwnParseResult;
  Parser.Free;
end;

procedure TJArray.SetValue(AIndex: Integer; const Value: TJValue.TJWrapper);
begin
  if FValues.RangeCheck(AIndex) then
    FValues[AIndex] := Value
  else
  begin
    while MaxIndex < AIndex - 1 do
      AddNull;
    Add(Value);
  end;
end;

{ TJNull }

function TJNull.Equals(AObject: TObject): Boolean;
begin
  Result := (AObject = nil) or (AObject is TJNull);
end;

procedure TJNull.FormatInternal(ABuilder: TJValue.TJStringBuilder);
begin
  ABuilder.Append(NullString);
end;

class function TJNull.GetTypeName: string;
begin
  Result := 'JSON-Null';
end;

{ TJBool }

procedure TJBool.Assign(AFrom: TJValue);
begin
  if not(AFrom is TJBool) then
    raise EJSONError.Create('Cannot assign value to bool.');
  Value := TJBool(AFrom).Value;
end;

constructor TJBool.Create(AValue: Boolean);
begin
  FValue := AValue;
end;

function TJBool.Equals(AObject: TObject): Boolean;
begin
  if not(AObject is TJBool) then
    Exit(False);
  Result := Value = TJBool(AObject).Value;
end;

procedure TJBool.FormatInternal(ABuilder: TJValue.TJStringBuilder);
begin
  ABuilder.Append(BoolStrings[Value]);
end;

class function TJBool.GetTypeName: string;
begin
  Result := 'JSON-Bool';
end;

{ TJString }

procedure TJString.Assign(AValue: TJValue);
begin
  if not(AValue is TJString) then
    raise EJSONError.Create('Cannot assign value to string.');
  Text := TJString(AValue).Text;
end;

constructor TJString.Create(AText: string);
begin
  FText := AText;
end;

function TJString.Equals(AObject: TObject): Boolean;
begin
  if not(AObject is TJString) then
    Exit(inherited);
  Result := Text = TJString(AObject).Text;
end;

procedure TJString.FormatInternal(ABuilder: TJValue.TJStringBuilder);
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

procedure TJNumber.Assign(AFrom: TJValue);
begin
  if not(AFrom is TJNumber) then
    raise EJSONError.Create('Cannot assign value to number.');
  Number := TJNumber(AFrom).Number;
end;

constructor TJNumber.Create(ANumber: TJValue.TNumber);
begin
  FNumber := ANumber;
end;

function TJNumber.Equals(AObject: TObject): Boolean;
begin
  if not(AObject is TJNumber) then
    Exit(False);
  Result := Number = TJNumber(AObject).Number;
end;

procedure TJNumber.FormatInternal(ABuilder: TJValue.TJStringBuilder);
begin
  case Number.NumType of
    ntInt:
      ABuilder.Append(Number.AsInt);
    ntSingle:
      ABuilder.Append(PrettyFloat(Single(Number.AsFloat)));
    ntDouble:
      ABuilder.Append(PrettyFloat(Number.AsFloat));
  end;
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
  Result.NumType := ntInt;
  Result.AsInt := ANumber;
end;

class operator TJValue.TNumber.Equal(A, B: TNumber): Boolean;
begin
  if A.IsFloat or B.IsFloat then
    Result := Double(A) = Double(B)
  else
    Result := A.AsInt = B.AsInt;
end;

class operator TJValue.TNumber.Implicit(ANumber: Double): TNumber;
begin
  Result.NumType := ntDouble;
  Result.AsFloat := ANumber;
end;

function TJValue.TNumber.IsFloat: Boolean;
begin
  Result := NumType <> ntInt;
end;

class operator TJValue.TNumber.NotEqual(A, B: TNumber): Boolean;
begin
  Result := not(A = B);
end;

class operator TJValue.TNumber.Implicit(ANumber: Single): TNumber;
begin
  Result.NumType := ntSingle;
  Result.AsFloat := ANumber;
end;

{ TJObject.TParser }

class function TJObject.TParser.GetResultName: string;
begin
  Result := TJObject.GetTypeName;
end;

class function TJObject.TParser.GetTokenCount: Integer;
begin
  Result := Length(TokenNames);
end;

class function TJObject.TParser.GetTokenName(AIndex: Integer): string;
begin
  Result := TokenNames[AIndex];
end;

function TJObject.TParser.Parse: Boolean;
var
  Key: string;
begin
  Token := TokenBrackets;
  if not StartsWith('{') then
    Exit(False);
  ResetToken;

  SkipWhitespace;

  SetParseResult(TJObject.Create);

  Token := TokenBrackets;
  if StartsWith('}') then
    Exit(True);
  ResetToken;

  while True do
  begin
    Key := TJString.TStringParser.Require(Info);

    SkipWhitespace;

    Token := TokenColon;
    if not StartsWith(':') then
      Log(1, 'Expected ":".', elError);
    ResetToken;

    SkipWhitespace;

    ParseResult[Key] := TJValue.TParser.Require(Info);

    SkipWhitespace;

    Token := TokenBrackets;
    if StartsWith('}') then
      Break;
    Token := TokenComma;
    if not StartsWith(',') then
      Log(1, 'Expected "," or "}" to close the object.', elError);
    ResetToken;

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
  Token := TokenQuotes;
  if not StartsWith('"') then
    Exit(False);

  Builder := TStringBuilder.Create;
  try
    while not StartsWith('"') do
    begin
      if ReachedEnd then
        raise EParseError.Create('Found unterminated string.');

      Marker := GetMarker;
      Token := TokenBackslash;
      if StartsWith('\') then
      begin
        Escaped := First;
        Token := TokenEscaped;
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
              Token := TokenUnicodeHex;
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
        Token := TokenContent;
        Advance;
      end;

      Token := TokenQuotes;
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

class function TJBool.TParser.GetTokenCount: Integer;
begin
  Result := Length(TokenNames);
end;

class function TJBool.TParser.GetTokenName(AIndex: Integer): string;
begin
  Result := TokenNames[AIndex];
end;

function TJBool.TParser.Parse: Boolean;
const
  BoolTokenIndex: array [Boolean] of Integer = (TokenFalse, TokenTrue);
var
  Bool: Boolean;
begin
  for Bool := Low(Boolean) to High(Boolean) do
  begin
    Token := BoolTokenIndex[Bool];
    if StartsWith(BoolStrings[Bool]) then
    begin
      SetParseResult(TJBool.Create(Bool));
      Exit(True);
    end;
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

class function TJArray.TParser.GetTokenCount: Integer;
begin
  Result := Length(TokenNames);
end;

class function TJArray.TParser.GetTokenName(AIndex: Integer): string;
begin
  Result := TokenNames[AIndex];
end;

function TJArray.TParser.Parse: Boolean;
begin
  Token := TokenBrackets;
  if not StartsWith('[') then
    Exit(False);
  ResetToken;

  SkipWhitespace;

  SetParseResult(TJArray.Create);

  Token := TokenBrackets;
  if StartsWith(']') then
    Exit(True);
  ResetToken;

  while True do
  begin

    ParseResult.FValues.Add(TJValue.TParser.Require(Info));

    SkipWhitespace;

    Token := TokenBrackets;
    if StartsWith(']') then
      Break;
    Token := TokenComma;
    if not StartsWith(',') then
      Log(1, 'Expected "," or "]" to close array.', elError);
    ResetToken;

    SkipWhitespace;

  end;
  Result := True;
end;

{ TJPairHelper }

function TJPairHelper.BoolOrDefault(ADefault: Boolean): Boolean;
begin
  Result := Value.BoolOrDefault(ADefault);
end;

function TJPairHelper.Exists: Boolean;
begin
  Result := Value.Exists;
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

function TJPairHelper.IsNull: Boolean;
begin
  Result := Value.IsNull;
end;

function TJPairHelper.NumberOrDefault(ADefault: TJValue.TNumber): TJValue.TNumber;
begin
  Result := Value.NumberOrDefault(ADefault);
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

{ TJValue.TJStringBuilder }

procedure TJValue.TJStringBuilder.AddIndentation;
begin
  Append(' ', IndentLevel * IndentWith);
end;

constructor TJValue.TJStringBuilder.Create(APretty: Boolean; AIndentWidth: Integer; AArrayWrapTheshold: Integer);
begin
  inherited Create;
  FPretty := APretty;
  FArrayWrapThreshold := AArrayWrapTheshold;
  FIndentWidth := AIndentWidth;
end;

procedure TJValue.TJStringBuilder.Indent;
begin
  Inc(FIndentLevel);
  NewLine;
end;

procedure TJValue.TJStringBuilder.NewLine;
begin
  if Pretty then
  begin
    AppendLine;
    AddIndentation;
  end;
end;

procedure TJValue.TJStringBuilder.Unindent;
begin
  Dec(FIndentLevel);
  NewLine;
end;

{ TJValue.TWrapper }

class operator TJValue.TJWrapper.Implicit(AText: string): TJWrapper;
begin
  Result.FValue := TJString.Create(AText);
end;

class operator TJValue.TJWrapper.Implicit(AWrapper: TJWrapper): TJValue;
begin
  Result := AWrapper.FValue;
end;

class operator TJValue.TJWrapper.Implicit(AValue: TJValue): TJWrapper;
begin
  Result.FValue := AValue;
end;

class operator TJValue.TJWrapper.Implicit(ANumber: TNumber): TJWrapper;
begin
  Result := TJNumber.Create(ANumber);
end;

class operator TJValue.TJWrapper.Implicit(AValue: Boolean): TJWrapper;
begin
  Result := TJBool.Create(AValue);
end;

class operator TJValue.TJWrapper.Implicit(ANumber: Int64): TJWrapper;
begin
  Result := TNumber(ANumber);
end;

class operator TJValue.TJWrapper.Implicit(AWrapper: TJWrapper): string;
begin
  Result := AWrapper.AsString;
end;

class operator TJValue.TJWrapper.Implicit(ANumber: Double): TJWrapper;
begin
  Result := TNumber(ANumber);
end;

class operator TJValue.TJWrapper.Implicit(ANumber: Single): TJWrapper;
begin
  Result := TNumber(ANumber);
end;

class operator TJValue.TJWrapper.Implicit(AWrapper: TJWrapper): TNumber;
begin
  Result := AWrapper.AsNumber;
end;

class operator TJValue.TJWrapper.Implicit(AWrapper: TJWrapper): Int64;
begin
  Result := AWrapper.AsInt;
end;

procedure TJValue.TJWrapper.Assign(AFrom: TJValue);
begin
  FValue.Assign(AFrom);
end;

function TJValue.TJWrapper.BoolOrDefault(ADefault: Boolean): Boolean;
begin
  Result := FValue.BoolOrDefault(ADefault);
end;

function TJValue.TJWrapper.Cast<T>: T;
begin
  Result := FValue.Cast<T>;
end;

function TJValue.TJWrapper.Copy: TJValue;
begin
  Result := FValue.Copy;
end;

function TJValue.TJWrapper.Exists: Boolean;
begin
  Result := FValue.Exists;
end;

function TJValue.TJWrapper.FloatOrDefault(ADefault: Double): Double;
begin
  Result := FValue.FloatOrDefault(ADefault);
end;

function TJValue.TJWrapper.Format(APretty: Boolean; AIndentWidth: Integer): string;
begin
  Result := FValue.Format(APretty, AIndentWidth);
end;

function TJValue.TJWrapper.GetAsArray: TJArray;
begin
  Result := FValue.AsArray;
end;

function TJValue.TJWrapper.GetAsBool: Boolean;
begin
  Result := FValue.AsBool;
end;

function TJValue.TJWrapper.GetAsFloat: Double;
begin
  Result := FValue.AsFloat;
end;

function TJValue.TJWrapper.GetAsInt: Int64;
begin
  Result := FValue.AsInt;
end;

function TJValue.TJWrapper.GetAsNumber: TNumber;
begin
  Result := FValue.AsNumber;
end;

function TJValue.TJWrapper.GetAsObject: TJObject;
begin
  Result := FValue.AsObject;
end;

function TJValue.TJWrapper.GetAsString: string;
begin
  Result := FValue.AsString;
end;

function TJValue.TJWrapper.GetEnumerator: IIterator<TJPair>;
begin
  Result := FValue.GetEnumerator;
end;

function TJValue.TJWrapper.GetTypeName: string;
begin
  Result := FValue.GetTypeName;
end;

function TJValue.TJWrapper.GetValue(AKey: string): TJWrapper;
begin
  Result := FValue.Values[AKey];
end;

function TJValue.TJWrapper.GetValue(AIndex: Integer): TJWrapper;
begin
  Result := FValue.Values[AIndex];
end;

class operator TJValue.TJWrapper.Implicit(AWrapper: TJWrapper): Boolean;
begin
  Result := AWrapper.AsBool;
end;

function TJValue.TJWrapper.IntOrDefault(ADefault: Int64): Int64;
begin
  Result := FValue.IntOrDefault(ADefault);
end;

function TJValue.TJWrapper.IsNull: Boolean;
begin
  Result := FValue.IsNull;
end;

class operator TJValue.TJWrapper.LogicalOr(AWrapper: TJWrapper; ADefault: Boolean): Boolean;
begin
  if AWrapper.IsNull then
    Exit(ADefault);
  Result := AWrapper;
end;

class operator TJValue.TJWrapper.LogicalOr(AWrapper: TJWrapper; ADefault: string): string;
begin
  if AWrapper.IsNull then
    Exit(ADefault);
  Result := AWrapper;
end;

function TJValue.TJWrapper.NumberOrDefault(ADefault: TNumber): TNumber;
begin
  Result := FValue.NumberOrDefault(ADefault);
end;

procedure TJValue.TJWrapper.SetAsBool(const Value: Boolean);
begin
  FValue.AsBool := Value;
end;

procedure TJValue.TJWrapper.SetAsFloat(const Value: Double);
begin
  FValue.AsFloat := Value;
end;

procedure TJValue.TJWrapper.SetAsInt(const Value: Int64);
begin
  FValue.AsInt := Value;
end;

procedure TJValue.TJWrapper.SetAsNumber(const Value: TNumber);
begin
  FValue.AsNumber := Value;
end;

procedure TJValue.TJWrapper.SetAsString(const Value: string);
begin
  FValue.AsString := Value;
end;

function TJValue.TJWrapper.StringOrDefault(ADefault: string): string;
begin
  Result := FValue.StringOrDefault(ADefault);
end;

function TJValue.TJWrapper.ToString: string;
begin
  Result := FValue.ToString;
end;

class operator TJValue.TJWrapper.Implicit(AWrapper: TJWrapper): Double;
begin
  Result := AWrapper.AsFloat;
end;

class operator TJValue.TJWrapper.Implicit(AWrapper: TJWrapper): Single;
begin
  Result := AWrapper.AsFloat;
end;

class operator TJValue.TJWrapper.LogicalOr(AWrapper: TJWrapper; ADefault: Int64): Int64;
begin
  if AWrapper.IsNull then
    Exit(ADefault);
  Result := AWrapper;
end;

class operator TJValue.TJWrapper.LogicalOr(AWrapper: TJWrapper; ADefault: Single): Single;
begin
  if AWrapper.IsNull then
    Exit(ADefault);
  Result := AWrapper;
end;

class operator TJValue.TJWrapper.LogicalOr(AWrapper: TJWrapper; ADefault: Double): Double;
begin
  if AWrapper.IsNull then
    Exit(ADefault);
  Result := AWrapper;
end;

{ TJSerializer }

destructor TJSerializer.Destroy;
begin
  if IsStoring then
    FValue.Free;
  inherited;
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
  V: TJValue;
begin
  case Mode of
    smSerialize:
      Value[AName] := ANumber;
    smUnserialize:
      if Value.Get(AName, V) then
        ANumber := V.AsNumber;
  end;
end;

procedure TJSerializer.Define(AName: string; var ANumber: Double);
var
  V: TJValue;
begin
  case Mode of
    smSerialize:
      Value[AName] := ANumber;
    smUnserialize:
      if Value.Get(AName, V) then
        ANumber := V.AsNumber;
  end;
end;

procedure TJSerializer.Define(AName: string; var ANumber: Int64);
var
  V: TJValue;
begin
  case Mode of
    smSerialize:
      Value[AName] := ANumber;
    smUnserialize:
      if Value.Get(AName, V) then
        ANumber := V.AsInt;
  end;
end;

procedure TJSerializer.Define(AName: string; var ANumber: Integer);
var
  V: TJValue;
begin
  case Mode of
    smSerialize:
      Value[AName] := ANumber;
    smUnserialize:
      if Value.Get(AName, V) then
        ANumber := V.AsInt;
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
  V: TJValue;
begin
  if AObject = nil then
    Exit;
  case Mode of
    smSerialize:
      Value[AName] := TJSerializer.Serialize(AObject, AVersion);
    smUnserialize:
      if Value.Get(AName, V) then
        TJSerializer.Unserialize(AObject, Value[AName].AsObject);
  end;
end;

procedure TJSerializer.Define(AName: string; var AValue: Boolean);
var
  V: TJValue;
begin
  case Mode of
    smSerialize:
      Value[AName] := AValue;
    smUnserialize:
      if Value.Get(AName, V) then
        AValue := V.AsBool;
  end;
end;

procedure TJSerializer.Define(AName: string; var AText: string);
var
  V: TJValue;
begin
  case Mode of
    smSerialize:
      Value[AName] := AText;
    smUnserialize:
      if Value.Get(AName, V) then
        AText := V.AsString;
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
        ASerializer.Serialize(Value.AddArray(AName));
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
  V: TJValue;
begin
  try
    case Mode of
      smSerialize:
        ASerializer.Serialize(Value.AddArray(AName));
      smUnserialize:
        if Value.Get(AName, V) then
          ASerializer.Unserialize(V.AsArray);
    end;

  finally
    ASerializer.Free;

  end;
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
