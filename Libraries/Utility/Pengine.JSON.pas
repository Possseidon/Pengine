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

  TJBase = class;
  TJParent = class;
  TJObject = class;
  TJArray = class;
  TJString = class;
  TJNumber = class;
  TJBool = class;
  TJNull = class;

  TJBaseClass = class of TJBase;

  /// <summary>Any json value.</summary>
  TJBase = class
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

    /// <summary>A wrapper for any json value with implicit conversion and path information.</summary>
    TJValue = record
    public type

      TJPair = TPair<string, TJValue>;

    private
      FValue: TJBase;

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

      function GetValue(AKey: string): TJValue; overload;
      function GetValue(AIndex: Integer): TJValue; overload;
      procedure SetValue(AKey: string; const Value: TJValue); overload;
      procedure SetValue(AIndex: Integer; const Value: TJValue); overload;
      function GetIndex: Integer;
      function GetName: string;
      function GetParent: TJParent;
      function GetRoot: TJBase;
      procedure SetIndex(const Value: Integer);

    public
      // Value -> TJBase
      class operator Implicit(AValue: TJValue): TJBase; static; inline;
      class operator Implicit(AValue: TJValue): TJParent; static; inline;
      class operator Implicit(AValue: TJValue): TJObject; static; inline;
      class operator Implicit(AValue: TJValue): TJArray; static; inline;
      class operator Implicit(AValue: TJValue): TJString; static; inline;
      class operator Implicit(AValue: TJValue): TJNumber; static; inline;
      class operator Implicit(AValue: TJValue): TJBool; static; inline;

      // TJBase -> Value
      class operator Implicit(AValue: TJBase): TJValue; static; inline;

      // Primitive -> Value
      class operator Implicit(AText: string): TJValue; static; inline;
      class operator Implicit(ANumber: TNumber): TJValue; static; inline;
      class operator Implicit(ANumber: Double): TJValue; static; inline;
      class operator Implicit(ANumber: Single): TJValue; static; inline;
      class operator Implicit(ANumber: Int64): TJValue; static; inline;
      class operator Implicit(AValue: Boolean): TJValue; static; inline;

      // Value -> Primitve
      class operator Implicit(AValue: TJValue): string; static; inline;
      class operator Implicit(AValue: TJValue): TNumber; static; inline;
      class operator Implicit(AValue: TJValue): Double; static; inline;
      class operator Implicit(AValue: TJValue): Single; static; inline;
      class operator Implicit(AValue: TJValue): Int64; static; inline;
      class operator Implicit(AValue: TJValue): Boolean; static; inline;

      // or default operator
      class operator LogicalOr(AValue: TJValue; ADefault: string): string; static; inline;
      class operator LogicalOr(AValue: TJValue; ADefault: Boolean): Boolean; static; inline;
      class operator LogicalOr(AValue: TJValue; ADefault: Int64): Int64; static; inline;
      class operator LogicalOr(AValue: TJValue; ADefault: Single): Single; static; inline;
      class operator LogicalOr(AValue: TJValue; ADefault: Double): Double; static; inline;
      class operator LogicalOr(AValue1, AValue2: TJValue): TJValue; static; inline;

      /// <summary>Interprets as TJObject and gets or sets the value at the specified key.</summary>
      /// <remarks>Returns nil for null or none existent keys.</remarks>
      property Values[AKey: string]: TJValue read GetValue write SetValue; default;
      /// <summary>Interprets as TJArray and gets or sets the value at the specified index.</summary>
      /// <remarks>Returns nil for null or none existent indices.</remarks>
      property Values[AIndex: Integer]: TJValue read GetValue write SetValue; default;

      function AddObject(AKey: string): TJObject;
      function AddArray(AKey: string): TJArray;

      function Cast<T: TJBase>: T; overload;
      function Cast(out AValue: TJObject): Boolean; overload;
      function Cast(out AValue: TJArray): Boolean; overload;
      function Cast(out AValue: TJString): Boolean; overload;
      function Cast(out AValue: TJNumber): Boolean; overload;
      function Cast(out AValue: TJBool): Boolean; overload;

      function IsNull: Boolean; inline;
      function Exists: Boolean; inline;

      function IsObject: Boolean; inline;
      function IsArray: Boolean; inline;
      function IsString: Boolean; inline;
      function IsNumber: Boolean; inline;
      function IsBool: Boolean; inline;

      property Value: TJBase read FValue;

      property AsObject: TJObject read GetAsObject;
      property AsArray: TJArray read GetAsArray;
      property AsString: string read GetAsString write SetAsString;
      property AsNumber: TNumber read GetAsNumber write SetAsNumber;
      property AsInt: Int64 read GetAsInt write SetAsInt;
      property AsFloat: Double read GetAsFloat write SetAsFloat;
      property AsBool: Boolean read GetAsBool write SetAsBool;

      // TJBase relay functions
      procedure Free;

      function Copy: TJBase; inline;

      property Parent: TJParent read GetParent;
      property Root: TJBase read GetRoot;
      property Index: Integer read GetIndex write SetIndex;
      property Name: string read GetName;
      function Path(AFrom: TJParent = nil): string;

      function GetTypeName: string; inline;
      function Format(APretty: Boolean = True; AIndentWidth: Integer = 2): string; inline;

      function Equals(AOther: TJBase): Boolean; inline;
      procedure Assign(AFrom: TJBase); inline;

      function ToString: string; inline;

    end;

    TParser = class(TObjectParser<TJBase>)
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
    FParent: TJParent;
    FIndex: Integer;

    procedure SetIndex(const Value: Integer);
    function GetName: string;
    function GetRoot: TJBase;

  protected
    procedure FormatInternal(ABuilder: TJStringBuilder); virtual; abstract;

  public
    constructor Create; overload; virtual;

    function Copy: TJBase;

    property Parent: TJParent read FParent;
    property Root: TJBase read GetRoot;
    property Index: Integer read FIndex write SetIndex;
    property Name: string read GetName;
    function Path(AFrom: TJParent = nil): string;

    class function GetTypeName: string; virtual;
    function Format(APretty: Boolean = True; AIndentWidth: Integer = 2; AArrayWrapThreshold: Integer = 3): string;

    procedure Assign(AFrom: TJBase); virtual;

    function ToString: string; override;

    class function Parse(AText: string): TJBase;
    class function CreateFromFile(APath: string): TJBase;

  end;

  TJValue = TJBase.TJValue;

  TJValues = TArray<TJValue>;

  TJPair = TJValue.TJPair;

  TJParent = class(TJBase)
  public
    function NameOf(AIndex: Integer): string; virtual; abstract;
    procedure ChangeIndex(AFrom, ATo: Integer); virtual; abstract;

    class function GetTypeName: string; override;

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
  TJObject = class(TJParent)
  public type

    TEmptyIterator = class(TInterfacedObject, IIterator<TJPair>)
    private
      function GetCurrent: TJPair;
      function MoveNext: Boolean;

    end;

    TIterator = class(TInterfacedObject, IIterator<TJPair>)
    private
      FJObject: TJObject;
      FCurrent: Integer;

      function GetCurrent: TJPair;

    public
      constructor Create(AJObject: TJObject);

      function MoveNext: Boolean;
      property Current: TJPair read GetCurrent;

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

    TPair = TPair<string, TJBase>;

    TOrder = TToObjectPairArray<string, TJBase>;

    TMap = TMap<string, TJBase, TStringHasher>;

  private
    FOrder: TOrder;
    FMap: TMap;

    function GetOrder: TOrder.TReader;
    function GetValue(AKey: string): TJValue;
    procedure SetValue(AKey: string; const Value: TJValue); overload;
    function GetCount: Integer;

  protected
    procedure FormatInternal(ABuilder: TJBase.TJStringBuilder); override;

  public
    constructor Create; override;
    destructor Destroy; override;

    function NameOf(AIndex: Integer): string; override;
    procedure ChangeIndex(AFrom, ATo: Integer); override;

    function Copy: TJObject;

    class function GetTypeName: string; override;

    /// <summary>Gets the json-value to the given key.</summary>
    /// <returns>True, if the key exists.</returns>
    function Get(AKey: string; out AValue: TJBase): Boolean; overload;
    function Get(AKey: string; out AValue: TJValue): Boolean; overload;
    /// <summary>Gets the json-value to the given key and check for a given type.</summary>
    /// <returns>True, if the key exists.</returns>
    function Get(AKey: string; out AValue: TJObject): Boolean; overload;
    function Get(AKey: string; out AValue: TJArray): Boolean; overload;
    function Get(AKey: string; out AValue: TJString): Boolean; overload;
    function Get(AKey: string; out AValue: TJNumber): Boolean; overload;
    function Get(AKey: string; out AValue: TJBool): Boolean; overload;

    /// <summary>Grants read and write access to all values.</summary>
    /// <remarks>The getter will return nil for non-existent keys or if the object itself is nil.</remarks>
    property Values[AKey: string]: TJValue read GetValue write SetValue; default;

    property Count: Integer read GetCount;
    procedure Clear;

    procedure Add(AKey: string; AValue: TJBase.TJValue);
    function AddObject(AKey: string): TJObject;
    function AddArray(AKey: string): TJArray;
    function AddNull(AKey: string): TJNull;

    function TryRemove(AKey: string): Boolean;
    procedure Remove(AKey: string);
    function Extract(AKey: string): TJValue; overload;
    function Extract<T: TJBase>(AKey: string): T; overload;

    property Order: TOrder.TReader read GetOrder;

    function GetEnumerator: IIterator<TJPair>;
    function Equals(AObject: TObject): Boolean; override;

    procedure Assign(AFrom: TJBase); override;

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
  TJArray = class(TJParent)
  public type

    TEmptyIterator = class(TInterfacedObject, IIterator<TJValue>)
    private
      function GetCurrent: TJValue;
      function MoveNext: Boolean;

    end;

    TIterator = class(TInterfacedObject, IIterator<TJValue>)
    private
      FJArray: TJArray;
      FCurrent: Integer;

      function GetCurrent: TJValue;

    public
      constructor Create(AJArray: TJArray);

      function MoveNext: Boolean;
      property Current: TJValue read GetCurrent;

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

    TValues = TObjectArray<TJBase>;

  private
    FValues: TValues;

    function GetCount: Integer;
    function GetMaxIndex: Integer;
    function GetValue(AIndex: Integer): TJValue;
    procedure SetValue(AIndex: Integer; const Value: TJValue);

  protected
    procedure FormatInternal(ABuilder: TJBase.TJStringBuilder); override;

  public
    constructor Create; override;
    destructor Destroy; override;

    function NameOf(AIndex: Integer): string; override;
    procedure ChangeIndex(AFrom, ATo: Integer); override;

    function Copy: TJArray;

    class function GetTypeName: string; override;

    property Values[AIndex: Integer]: TJValue read GetValue write SetValue; default;
    property Count: Integer read GetCount;
    property MaxIndex: Integer read GetMaxIndex;
    function Empty: Boolean;

    procedure Add(AValue: TJValue);
    function AddObject: TJObject;
    function AddArray: TJArray;
    function AddNull: TJNull;

    procedure Remove(AIndex: Integer);

    function Extract(AIndex: Integer): TJValue; overload;
    function Extract<T: TJBase>(AIndex: Integer): T; overload;

    function GetEnumerator: IIterator<TJValue>;
    function Equals(AObject: TObject): Boolean; override;

    procedure Assign(AFrom: TJBase); override;

    class function Parse(AText: string): TJArray;
    class function CreateFromFile(APath: string): TJArray;

  end;

  /// <summary>
  /// A double quoted string with backslash escaping.
  /// Example: <c>"Hello\nWorld"</c>
  /// </summary>
  TJString = class(TJBase)
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
    procedure FormatInternal(ABuilder: TJBase.TJStringBuilder); override;

  public
    constructor Create(AText: string); reintroduce; overload;

    class function GetTypeName: string; override;

    property Text: string read FText write FText;
    property Quoted: string read GetQuoted;

    class function Quote(AText: string): string;

    function Equals(AObject: TObject): Boolean; override;

    procedure Assign(AValue: TJBase); override;

  end;

  /// <summary>A general type for both integer and floating point numbers.</summary>
  TJNumber = class(TJBase)
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
    FNumber: TJBase.TNumber;

  protected
    procedure FormatInternal(ABuilder: TJBase.TJStringBuilder); override;

  public
    constructor Create(ANumber: TJBase.TNumber); reintroduce; overload;

    class function GetTypeName: string; override;

    property Number: TJBase.TNumber read FNumber write FNumber;

    function TryGetInt(out AValue: Int64): Boolean;

    function Equals(AObject: TObject): Boolean; override;

    procedure Assign(AFrom: TJBase); override;

  end;

  /// <summary>A boolean, <c>true</c> or <c>false</c>.</summary>
  TJBool = class(TJBase)
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
    procedure FormatInternal(ABuilder: TJBase.TJStringBuilder); override;

  public
    constructor Create(AValue: Boolean); reintroduce; overload;

    class function GetTypeName: string; override;

    property Value: Boolean read FValue write FValue;

    function Equals(AObject: TObject): Boolean; override;

    procedure Assign(AFrom: TJBase); override;

  end;

  /// <summary>The identifier <c>null</c>.</summary>
  TJNull = class(TJBase)
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
    procedure FormatInternal(ABuilder: TJBase.TJStringBuilder); override;

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

{ TJBase }

procedure TJBase.Assign(AFrom: TJBase);
begin
  // nothing by default
end;

function TJBase.Copy: TJBase;
begin
  if Self = nil then
    Exit(nil);
  Result := TJBaseClass(ClassType).Create;
  Result.Assign(Self);
end;

constructor TJBase.Create;
begin
  // nothing by default
end;

class function TJBase.CreateFromFile(APath: string): TJBase;
begin
  Result := Parse(TFile.ReadAllText(APath));
end;

function TJBase.Format(APretty: Boolean; AIndentWidth, AArrayWrapThreshold: Integer): string;
var
  Builder: TJStringBuilder;
begin
  Builder := TJStringBuilder.Create(APretty, AIndentWidth, AArrayWrapThreshold);
  try
    FormatInternal(Builder);
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

function TJBase.GetName: string;
begin
  if Parent = nil then
    Exit('');
  Result := Parent.NameOf(Index);
end;

function TJBase.GetRoot: TJBase;
begin
  if Parent = nil then
    Exit(Self);
  Result := Parent.Root;
end;

class function TJBase.GetTypeName: string;
begin
  Result := 'JSON-Value';
end;

class function TJBase.Parse(AText: string): TJBase;
var
  Parser: TParser;
begin
  Parser := TParser.Create(AText, False);
  Result := Parser.OwnParseResult;
  Parser.Free;
end;

function TJBase.Path(AFrom: TJParent): string;
begin
  if Self = AFrom then
    Exit('');
  if Parent <> nil then
  begin
    Result := Parent.Path(AFrom);
    if (Parent <> AFrom) and (Parent.Parent <> nil) and (Parent is TJObject) then
      Result := Result + '.';
  end;
  Result := Result + Name;
end;

procedure TJBase.SetIndex(const Value: Integer);
begin
  Parent.ChangeIndex(Index, Value);
end;

function TJBase.ToString: string;
begin
  Result := Format(False);
end;

{ TJObject }

procedure TJObject.Add(AKey: string; AValue: TJBase.TJValue);
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

procedure TJObject.Assign(AFrom: TJBase);
var
  JPair: TJPair;
begin
  if not(AFrom is TJObject) then
    raise EJSONError.Create('Cannot assign value to object.');
  Clear;
  for JPair in TJObject(AFrom) do
    Self[JPair.Key] := JPair.Value.Copy;
end;

procedure TJObject.ChangeIndex(AFrom, ATo: Integer);
var
  I: Integer;
begin
  FOrder.SetIndex(AFrom, ATo);
  for I := Min(AFrom, ATo) to Max(AFrom, ATo) do
    FOrder[I].Value.FIndex := I;
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

procedure TJObject.FormatInternal(ABuilder: TJBase.TJStringBuilder);

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

function TJObject.Get(AKey: string; out AValue: TJBase): Boolean;
begin
  Result := FMap.Get(AKey, AValue);
end;

function TJObject.GetCount: Integer;
begin
  Result := Order.Count;
end;

function TJObject.Get(AKey: string; out AValue: TJValue): Boolean;
begin
  Result := Get(AKey, AValue.FValue);
end;

function TJObject.GetEnumerator: IIterator<TJPair>;
begin
  if Self = nil then
    Exit(TEmptyIterator.Create);
  Result := TIterator.Create(Self);
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
  if not FMap.Get(AKey, Result.FValue) then
    Result := AddNull(AKey);
end;

function TJObject.NameOf(AIndex: Integer): string;
begin
  Result := Order[AIndex].Key;
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
  if not TryRemove(AKey) then
    raise EJSONError.Create('JSON-Key doesn''t exist.');
end;

procedure TJObject.SetValue(AKey: string; const Value: TJValue);
var
  I: Integer;
  JValue: TJBase;
begin
  if Value.FValue = nil then
    JValue := TJNull.Create
  else
    JValue := Value.FValue;
  JValue.FParent := Self;
  if FMap.KeyExists(AKey) then
    for I := 0 to FOrder.MaxIndex do
      if AKey = FOrder[I].Key then
      begin
        FOrder[I] := TPair.Create(FOrder[I].Key, JValue);
        JValue.FIndex := I;
        Exit;
      end;
  FOrder.Add(TPair.Create(AKey, JValue));
  JValue.FIndex := FOrder.MaxIndex;
  FMap[AKey] := Value;
end;

function TJObject.TryRemove(AKey: string): Boolean;
var
  RemovedIndex, I: Integer;
begin
  Result := FMap.TryRemove(AKey);
  if Result then
  begin
    RemovedIndex := Order.FindFirstIndex(
      function(APair: TPair): Boolean
      begin
        Result := APair.Key = AKey;
      end);
    FOrder.RemoveAt(RemovedIndex);
    for I := RemovedIndex to Order.MaxIndex do
      FOrder[I].Value.FIndex := I;
  end;
end;

function TJObject.Get(AKey: string; out AValue: TJObject): Boolean;
var
  JValue: TJValue;
begin
  Result := Get(AKey, JValue);
  if Result then
    AValue := JValue;
end;

function TJObject.Get(AKey: string; out AValue: TJArray): Boolean;
var
  JValue: TJValue;
begin
  Result := Get(AKey, JValue);
  if Result then
    AValue := JValue;
end;

function TJObject.Get(AKey: string; out AValue: TJString): Boolean;
var
  JValue: TJValue;
begin
  Result := Get(AKey, JValue);
  if Result then
    AValue := JValue;
end;

function TJObject.Get(AKey: string; out AValue: TJNumber): Boolean;
var
  JValue: TJValue;
begin
  Result := Get(AKey, JValue);
  if Result then
    AValue := JValue;
end;

function TJObject.Get(AKey: string; out AValue: TJBool): Boolean;
var
  JValue: TJValue;
begin
  Result := Get(AKey, JValue);
  if Result then
    AValue := JValue;
end;

{ TJArray }

procedure TJArray.Add(AValue: TJValue);
var
  JValue: TJBase;
begin
  if AValue.FValue = nil then
    JValue := TJNull.Create
  else
    JValue := AValue.FValue;
  JValue.FParent := Self;
  FValues.Add(AValue);
  JValue.FIndex := MaxIndex;
end;

function TJArray.AddArray: TJArray;
begin
  Result := TJArray.Create;
  Add(Result);
end;

function TJArray.AddNull: TJNull;
begin
  Result := TJNull.Create;
  Add(Result);
end;

function TJArray.AddObject: TJObject;
begin
  Result := TJObject.Create;
  Add(Result);
end;

procedure TJArray.Assign(AFrom: TJBase);
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

procedure TJArray.ChangeIndex(AFrom, ATo: Integer);
var
  I: Integer;
begin
  FValues.SetIndex(AFrom, ATo);
  for I := Min(AFrom, ATo) to Max(AFrom, ATo) do
    FValues[I].FIndex := I;
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
    if not TJArray(AObject)[I].Equals(FValues[I]) then
      Exit(False);
  Result := True;
end;

function TJArray.Extract(AIndex: Integer): TJValue;
begin
  Result := Values[AIndex];
  FValues.OwnsObjects := False;
  Remove(AIndex);
  FValues.OwnsObjects := True;
end;

function TJArray.Extract<T>(AIndex: Integer): T;
begin
  Result := Extract(AIndex).Cast<T>;
end;

procedure TJArray.FormatInternal(ABuilder: TJBase.TJStringBuilder);
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
  if Self = nil then
    Exit(TEmptyIterator.Create);
  Result := TIterator.Create(Self);
end;

function TJArray.GetMaxIndex: Integer;
begin
  Result := FValues.MaxIndex;
end;

class function TJArray.GetTypeName: string;
begin
  Result := 'JSON-Array';
end;

function TJArray.GetValue(AIndex: Integer): TJValue;
begin
  if not FValues.RangeCheck(AIndex) then
    Values[AIndex] := TJNull.Create;
  Result := FValues[AIndex];
end;

function TJArray.NameOf(AIndex: Integer): string;
begin
  Result := '[' + AIndex.ToString + ']';
end;

class function TJArray.Parse(AText: string): TJArray;
var
  Parser: TParser;
begin
  Parser := TParser.Create(AText, False);
  Result := Parser.OwnParseResult;
  Parser.Free;
end;

procedure TJArray.Remove(AIndex: Integer);
var
  I: Integer;
begin
  FValues.RemoveAt(AIndex);
  for I := AIndex to MaxIndex do
    FValues[I].FIndex := I;
end;

procedure TJArray.SetValue(AIndex: Integer; const Value: TJValue);
var
  JValue: TJBase;
begin
  if Value.FValue = nil then
    JValue := TJNull.Create
  else
    JValue := Value.FValue;
  JValue.FParent := Self;
  if FValues.RangeCheck(AIndex) then
  begin
    FValues[AIndex] := JValue;
    JValue.FIndex := AIndex;
  end
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

procedure TJNull.FormatInternal(ABuilder: TJBase.TJStringBuilder);
begin
  ABuilder.Append(NullString);
end;

class function TJNull.GetTypeName: string;
begin
  Result := 'JSON-Null';
end;

{ TJBool }

procedure TJBool.Assign(AFrom: TJBase);
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

procedure TJBool.FormatInternal(ABuilder: TJBase.TJStringBuilder);
begin
  ABuilder.Append(BoolStrings[Value]);
end;

class function TJBool.GetTypeName: string;
begin
  Result := 'JSON-Bool';
end;

{ TJString }

procedure TJString.Assign(AValue: TJBase);
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

procedure TJString.FormatInternal(ABuilder: TJBase.TJStringBuilder);
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

procedure TJNumber.Assign(AFrom: TJBase);
begin
  if not(AFrom is TJNumber) then
    raise EJSONError.Create('Cannot assign value to number.');
  Number := TJNumber(AFrom).Number;
end;

constructor TJNumber.Create(ANumber: TJBase.TNumber);
begin
  FNumber := ANumber;
end;

function TJNumber.Equals(AObject: TObject): Boolean;
begin
  if not(AObject is TJNumber) then
    Exit(False);
  Result := Number = TJNumber(AObject).Number;
end;

procedure TJNumber.FormatInternal(ABuilder: TJBase.TJStringBuilder);
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

{ TJBase.TNumber }

class operator TJBase.TNumber.Implicit(ANumber: TNumber): Double;
begin
  if ANumber.IsFloat then
    Result := ANumber.AsFloat
  else
    Result := ANumber.AsInt;
end;

class operator TJBase.TNumber.Implicit(ANumber: Int64): TNumber;
begin
  Result.NumType := ntInt;
  Result.AsInt := ANumber;
end;

class operator TJBase.TNumber.Equal(A, B: TNumber): Boolean;
begin
  if A.IsFloat or B.IsFloat then
    Result := A.AsFloat = B.AsFloat
  else
    Result := A.AsInt = B.AsInt;
end;

class operator TJBase.TNumber.Implicit(ANumber: Double): TNumber;
begin
  Result.NumType := ntDouble;
  Result.AsFloat := ANumber;
end;

function TJBase.TNumber.IsFloat: Boolean;
begin
  Result := NumType <> ntInt;
end;

class operator TJBase.TNumber.NotEqual(A, B: TNumber): Boolean;
begin
  Result := not(A = B);
end;

class operator TJBase.TNumber.Implicit(ANumber: Single): TNumber;
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

    ParseResult[Key] := TJBase.TParser.Require(Info);

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

{ TJBase.TParser }

class function TJBase.TParser.GetResultName: string;
begin
  Result := TJBase.GetTypeName;
end;

function TJBase.TParser.Parse: Boolean;
var
  ParserClass: TParserClass;
  Parser: TObjectParser<TJBase>;
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
  Parser := TObjectParser<TJBase>(ParserClass.Create(Info, False));
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

    ParseResult.Add(TJBase.TParser.Require(Info));

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

{ TJBase.TJStringBuilder }

procedure TJBase.TJStringBuilder.AddIndentation;
begin
  Append(' ', IndentLevel * IndentWith);
end;

constructor TJBase.TJStringBuilder.Create(APretty: Boolean; AIndentWidth: Integer; AArrayWrapTheshold: Integer);
begin
  inherited Create;
  FPretty := APretty;
  FArrayWrapThreshold := AArrayWrapTheshold;
  FIndentWidth := AIndentWidth;
end;

procedure TJBase.TJStringBuilder.Indent;
begin
  Inc(FIndentLevel);
  NewLine;
end;

procedure TJBase.TJStringBuilder.NewLine;
begin
  if Pretty then
  begin
    AppendLine;
    AddIndentation;
  end;
end;

procedure TJBase.TJStringBuilder.Unindent;
begin
  Dec(FIndentLevel);
  NewLine;
end;

{ TJBase.TWrapper }

class operator TJBase.TJValue.Implicit(AText: string): TJValue;
begin
  Result := TJString.Create(AText);
end;

class operator TJBase.TJValue.Implicit(AValue: TJValue): TJBase;
begin
  Result := AValue.FValue;
end;

class operator TJBase.TJValue.Implicit(AValue: TJBase): TJValue;
begin
  Result.FValue := AValue;
end;

class operator TJBase.TJValue.Implicit(ANumber: TNumber): TJValue;
begin
  Result := TJNumber.Create(ANumber);
end;

class operator TJBase.TJValue.Implicit(AValue: Boolean): TJValue;
begin
  Result := TJBool.Create(AValue);
end;

class operator TJBase.TJValue.Implicit(ANumber: Int64): TJValue;
begin
  Result := TNumber(ANumber);
end;

class operator TJBase.TJValue.Implicit(AValue: TJValue): string;
begin
  Result := AValue.AsString;
end;

class operator TJBase.TJValue.Implicit(ANumber: Double): TJValue;
begin
  Result := TNumber(ANumber);
end;

class operator TJBase.TJValue.Implicit(ANumber: Single): TJValue;
begin
  Result := TNumber(ANumber);
end;

class operator TJBase.TJValue.Implicit(AValue: TJValue): TNumber;
begin
  Result := AValue.AsNumber;
end;

class operator TJBase.TJValue.Implicit(AValue: TJValue): Int64;
begin
  Result := AValue.AsInt;
end;

function TJBase.TJValue.AddArray(AKey: string): TJArray;
begin
  Result := AsObject.AddArray(AKey);
end;

function TJBase.TJValue.AddObject(AKey: string): TJObject;
begin
  Result := AsObject.AddObject(AKey);
end;

procedure TJBase.TJValue.Assign(AFrom: TJBase);
begin
  FValue.Assign(AFrom);
end;

function TJBase.TJValue.Cast<T>: T;
begin
  if FValue is T then
    Exit(T(FValue));
  raise EJSONError.CreateFmt('Expected %s to be %s, got %s.', [Path, T.GetTypeName, GetTypeName]);
end;

function TJBase.TJValue.Copy: TJBase;
begin
  Result := FValue.Copy;
end;

function TJBase.TJValue.Equals(AOther: TJBase): Boolean;
begin
  Result := Value.Equals(AOther);
end;

function TJBase.TJValue.Exists: Boolean;
begin
  Result := not IsNull;
end;

function TJBase.TJValue.Format(APretty: Boolean; AIndentWidth: Integer): string;
begin
  Result := FValue.Format(APretty, AIndentWidth);
end;

procedure TJBase.TJValue.Free;
begin
  FreeAndNil(FValue);
end;

function TJBase.TJValue.GetAsArray: TJArray;
begin
  Result := Cast<TJArray>;
end;

function TJBase.TJValue.GetAsBool: Boolean;
begin
  Result := Cast<TJBool>.Value;
end;

function TJBase.TJValue.GetAsFloat: Double;
begin
  Result := Cast<TJNumber>.Number;
end;

function TJBase.TJValue.GetAsInt: Int64;
begin
  if not Cast<TJNumber>.TryGetInt(Result) then
    raise EJSONError.Create('Number does not have an integer representation.');
end;

function TJBase.TJValue.GetAsNumber: TNumber;
begin
  Result := Cast<TJNumber>.Number;
end;

function TJBase.TJValue.GetAsObject: TJObject;
begin
  Result := Cast<TJObject>;
end;

function TJBase.TJValue.GetAsString: string;
begin
  Result := Cast<TJString>.Text;
end;

function TJBase.TJValue.GetIndex: Integer;
begin
  Result := FValue.Index;
end;

function TJBase.TJValue.GetName: string;
begin
  Result := FValue.Name;
end;

function TJBase.TJValue.GetParent: TJParent;
begin
  Result := FValue.Parent;
end;

function TJBase.TJValue.GetRoot: TJBase;
begin
  Result := FValue.Root;
end;

function TJBase.TJValue.GetTypeName: string;
begin
  Result := FValue.GetTypeName;
end;

function TJBase.TJValue.GetValue(AKey: string): TJValue;
begin
  Result := AsObject[AKey];
end;

function TJBase.TJValue.GetValue(AIndex: Integer): TJValue;
begin
  Result := AsArray[AIndex];
end;

class operator TJBase.TJValue.Implicit(AValue: TJValue): Boolean;
begin
  Result := AValue.AsBool;
end;

class operator TJBase.TJValue.Implicit(AValue: TJValue): TJParent;
begin
  Result := AValue.Cast<TJParent>;
end;

class operator TJBase.TJValue.Implicit(AValue: TJValue): TJObject;
begin
  Result := AValue.Cast<TJObject>;
end;

class operator TJBase.TJValue.Implicit(AValue: TJValue): TJArray;
begin
  Result := AValue.Cast<TJArray>;
end;

class operator TJBase.TJValue.Implicit(AValue: TJValue): TJString;
begin
  Result := AValue.Cast<TJString>;
end;

class operator TJBase.TJValue.Implicit(AValue: TJValue): TJNumber;
begin
  Result := AValue.Cast<TJNumber>;
end;

class operator TJBase.TJValue.Implicit(AValue: TJValue): TJBool;
begin
  Result := AValue.Cast<TJBool>;
end;

function TJBase.TJValue.IsArray: Boolean;
begin
  Result := FValue is TJArray;
end;

function TJBase.TJValue.IsBool: Boolean;
begin
  Result := FValue is TJBool;
end;

function TJBase.TJValue.IsNull: Boolean;
begin
  Result := (FValue = nil) or (FValue is TJNull);
end;

function TJBase.TJValue.IsNumber: Boolean;
begin
  Result := FValue is TJNumber;
end;

function TJBase.TJValue.IsObject: Boolean;
begin
  Result := FValue is TJObject;
end;

function TJBase.TJValue.IsString: Boolean;
begin
  Result := FValue is TJString;
end;

class operator TJBase.TJValue.LogicalOr(AValue1, AValue2: TJValue): TJValue;
begin
  if AValue1.Exists then
    Exit(AValue1);
  Result := AValue2;
end;

function TJBase.TJValue.Path(AFrom: TJParent): string;
begin
  Result := FValue.Path(AFrom);
end;

class operator TJBase.TJValue.LogicalOr(AValue: TJValue; ADefault: Boolean): Boolean;
begin
  if AValue.IsNull then
    Exit(ADefault);
  Result := AValue;
end;

class operator TJBase.TJValue.LogicalOr(AValue: TJValue; ADefault: string): string;
begin
  if AValue.IsNull then
    Exit(ADefault);
  Result := AValue;
end;

procedure TJBase.TJValue.SetAsBool(const Value: Boolean);
begin
  Cast<TJBool>.Value := Value;
end;

procedure TJBase.TJValue.SetAsFloat(const Value: Double);
begin
  Cast<TJNumber>.Number := Value;
end;

procedure TJBase.TJValue.SetAsInt(const Value: Int64);
begin
  Cast<TJNumber>.Number := Value;
end;

procedure TJBase.TJValue.SetAsNumber(const Value: TNumber);
begin
  Cast<TJNumber>.Number := Value;
end;

procedure TJBase.TJValue.SetAsString(const Value: string);
begin
  Cast<TJString>.Text := Value;
end;

procedure TJBase.TJValue.SetIndex(const Value: Integer);
begin
  FValue.Index := Value;
end;

procedure TJBase.TJValue.SetValue(AIndex: Integer; const Value: TJValue);
begin
  AsArray[AIndex] := Value;
end;

procedure TJBase.TJValue.SetValue(AKey: string; const Value: TJValue);
begin
  AsObject[AKey] := Value;
end;

function TJBase.TJValue.ToString: string;
begin
  Result := FValue.ToString;
end;

function TJBase.TJValue.Cast(out AValue: TJObject): Boolean;
begin
  Result := FValue is TJObject;
  if Result then
    AValue := TJObject(FValue);
end;

function TJBase.TJValue.Cast(out AValue: TJArray): Boolean;
begin
  Result := FValue is TJArray;
  if Result then
    AValue := TJArray(FValue);
end;

function TJBase.TJValue.Cast(out AValue: TJString): Boolean;
begin
  Result := FValue is TJString;
  if Result then
    AValue := TJString(FValue);
end;

function TJBase.TJValue.Cast(out AValue: TJNumber): Boolean;
begin
  Result := FValue is TJNumber;
  if Result then
    AValue := TJNumber(FValue);
end;

function TJBase.TJValue.Cast(out AValue: TJBool): Boolean;
begin
  Result := FValue is TJBool;
  if Result then
    AValue := TJBool(FValue);
end;

class operator TJBase.TJValue.Implicit(AValue: TJValue): Double;
begin
  Result := AValue.AsFloat;
end;

class operator TJBase.TJValue.Implicit(AValue: TJValue): Single;
begin
  Result := AValue.AsFloat;
end;

class operator TJBase.TJValue.LogicalOr(AValue: TJValue; ADefault: Int64): Int64;
begin
  if AValue.IsNull then
    Exit(ADefault);
  Result := AValue;
end;

class operator TJBase.TJValue.LogicalOr(AValue: TJValue; ADefault: Single): Single;
begin
  if AValue.IsNull then
    Exit(ADefault);
  Result := AValue;
end;

class operator TJBase.TJValue.LogicalOr(AValue: TJValue; ADefault: Double): Double;
begin
  if AValue.IsNull then
    Exit(ADefault);
  Result := AValue;
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

{ TJObject.TIterator }

constructor TJObject.TIterator.Create(AJObject: TJObject);
begin
  inherited Create;
  FJObject := AJObject;
  FCurrent := -1;
end;

function TJObject.TIterator.GetCurrent: TJPair;
var
  Pair: TPair;
begin
  Pair := FJObject.Order[FCurrent];
  Result.Create(Pair.Key, Pair.Value);
end;

function TJObject.TIterator.MoveNext: Boolean;
begin
  Inc(FCurrent);
  Result := FCurrent < FJObject.Count;
end;

{ TJArray.TIterator }

constructor TJArray.TIterator.Create(AJArray: TJArray);
begin
  inherited Create;
  FJArray := AJArray;
  FCurrent := -1;
end;

function TJArray.TIterator.GetCurrent: TJValue;
begin
  Result := FJArray[FCurrent];
end;

function TJArray.TIterator.MoveNext: Boolean;
begin
  Inc(FCurrent);
  Result := FCurrent < FJArray.Count;
end;

{ TJObject.TEmptyIterator }

function TJObject.TEmptyIterator.GetCurrent: TJPair;
begin
  raise EProgrammerNotFound.Create('Oh no!');
end;

function TJObject.TEmptyIterator.MoveNext: Boolean;
begin
  Result := False;
end;

{ TJArray.TEmptyIterator }

function TJArray.TEmptyIterator.GetCurrent: TJValue;
begin
  raise EProgrammerNotFound.Create('Oh no!');
end;

function TJArray.TEmptyIterator.MoveNext: Boolean;
begin
  Result := False;
end;

{ TJParent }

class function TJParent.GetTypeName: string;
begin
  Result := 'JSON-Parent';
end;

end.
