unit Pengine.JSON;

interface

uses
  System.SysUtils,
  System.Math,
  System.IOUtils,

  Pengine.IntMaths,
  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.Hasher,
  Pengine.Parser,
  Pengine.CollectionInterfaces,
  Pengine.Utility,
  Pengine.Interfaces;

type

  // TODO: Change JSON-Formatter to use an interface, similarly as to how I want to do TParser now.

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

  /// <summary>A base class for all JSON-Values.</summary>
  TJBase = class abstract
  public type

    /// <summary>A number, which is either an integer or a floating point number.</summary>
    TNumber = record
    public type

      TType = (ntInt, ntSingle, ntDouble);

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

    IFormatter = interface(IFormatter<TJBase>)
      function GetPretty: Boolean;
      procedure SetPretty(const Value: Boolean);
      function GetIndentWidth: Integer;
      procedure SetIndentWidth(const Value: Integer);
      function GetArrayWrapThreshold: Integer;
      procedure SetArrayWrapThreshold(const Value: Integer);

      property Pretty: Boolean read GetPretty write SetPretty;
      property IndentWidth: Integer read GetIndentWidth write SetIndentWidth;
      property ArrayWrapTheshold: Integer read GetArrayWrapThreshold write SetArrayWrapThreshold;

    end;

    TFormatter = class(TFormatter<TJBase>, IFormatter)
    public const

      DefaultPretty = True;
      DefaultArrayWrapThreshold = 3;
      DefaultIndentWidth = 2;

    private
      FBuilder: TStringBuilder;
      FIndentLevel: Integer;

      // Format-Settings
      FPretty: Boolean;
      FArrayWrapThreshold: Integer;
      FIndentWidth: Integer;

      function GetPretty: Boolean;
      procedure SetPretty(const Value: Boolean);
      function GetIndentWidth: Integer;
      procedure SetIndentWidth(const Value: Integer);
      function GetArrayWrapThreshold: Integer;
      procedure SetArrayWrapThreshold(const Value: Integer);

    public
      constructor Create; override;

      function Format: string; override;

      /// <summary>Wether to add line breaks and indentation.</summary>
      property Pretty: Boolean read GetPretty write SetPretty;
      /// <summary>How many spaces to use for indentation.</summary>
      property IndentWidth: Integer read GetIndentWidth write SetIndentWidth;
      /// <summary>How many primitive JSON-Values are allowed in an array without wrapping.</summary>
      property ArrayWrapTheshold: Integer read GetArrayWrapThreshold write SetArrayWrapThreshold;

      property Builder: TStringBuilder read FBuilder;
      procedure Indent; inline;
      procedure Unindent; inline;
      procedure AddIndentation; inline;
      procedure NewLine; inline;
      property IndentLevel: Integer read FIndentLevel;

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
      procedure SetAsString(const Value: string); inline;
      function GetAsNumber: TNumber; inline;
      procedure SetAsNumber(const Value: TNumber); inline;
      function GetAsInt: Int64; inline;
      procedure SetAsInt(const Value: Int64); inline;
      function GetAsFloat: Double; inline;
      procedure SetAsFloat(const Value: Double); inline;
      function GetAsBool: Boolean; inline;
      procedure SetAsBool(const Value: Boolean); inline;

      function GetParent: TJParent;
      function GetRoot: TJBase;
      function GetIndex: Integer;
      procedure SetIndex(const Value: Integer);
      function GetName: string;

      function GetValue(AKey: string): TJValue; overload; inline;
      procedure SetValue(AKey: string; const Value: TJValue); overload; inline;
      function GetValue(AIndex: Integer): TJValue; overload; inline;
      procedure SetValue(AIndex: Integer; const Value: TJValue); overload; inline;

      function GetCount: Integer;
      function GetMaxIndex: Integer;

      function GetEscaped: string;

    public
      // TJValue -> TJBase and descendents
      class operator Implicit(AValue: TJValue): TJBase; static; inline;
      class operator Implicit(AValue: TJValue): TJParent; static; inline;
      class operator Implicit(AValue: TJValue): TJObject; static; inline;
      class operator Implicit(AValue: TJValue): TJArray; static; inline;
      class operator Implicit(AValue: TJValue): TJString; static; inline;
      class operator Implicit(AValue: TJValue): TJNumber; static; inline;
      class operator Implicit(AValue: TJValue): TJBool; static; inline;

      // TJBase -> TJValue
      class operator Implicit(AValue: TJBase): TJValue; static; inline;

      // Primitives -> TJValue
      class operator Implicit(AText: string): TJValue; static; inline;
      class operator Implicit(ANumber: TNumber): TJValue; static; inline;
      class operator Implicit(ANumber: Double): TJValue; static; inline;
      class operator Implicit(ANumber: Single): TJValue; static; inline;
      class operator Implicit(ANumber: Int64): TJValue; static; inline;
      class operator Implicit(AValue: Boolean): TJValue; static; inline;

      // TJValue -> Primitves
      class operator Implicit(AValue: TJValue): string; static; inline;
      class operator Implicit(AValue: TJValue): TNumber; static; inline;
      class operator Implicit(AValue: TJValue): Double; static; inline;
      class operator Implicit(AValue: TJValue): Single; static; inline;
      class operator Implicit(AValue: TJValue): Int64; static; inline;
      class operator Implicit(AValue: TJValue): Boolean; static; inline;

      // Default or-operator
      class operator LogicalOr(AValue: TJValue; ADefault: string): string; static; inline;
      class operator LogicalOr(AValue: TJValue; ADefault: Boolean): Boolean; static; inline;
      class operator LogicalOr(AValue: TJValue; ADefault: Int64): Int64; static; inline;
      class operator LogicalOr(AValue: TJValue; ADefault: Single): Single; static; inline;
      class operator LogicalOr(AValue: TJValue; ADefault: Double): Double; static; inline;
      class operator LogicalOr(AValue1, AValue2: TJValue): TJValue; static; inline;

      // TJValue
      function IsObject: Boolean; inline;
      function IsArray: Boolean; inline;
      function IsString: Boolean; inline;
      function IsNumber: Boolean; inline;
      function IsBool: Boolean; inline;
      function IsNull: Boolean; inline;
      function Exists: Boolean; inline;

      function Cast<T: TJBase>: T; overload;
      function Cast<T: TJBase>(out AValue: T): Boolean; overload;

      property AsObject: TJObject read GetAsObject;
      property AsArray: TJArray read GetAsArray;
      property AsString: string read GetAsString write SetAsString;
      property AsNumber: TNumber read GetAsNumber write SetAsNumber;
      property AsInt: Int64 read GetAsInt write SetAsInt;
      property AsFloat: Double read GetAsFloat write SetAsFloat;
      property AsBool: Boolean read GetAsBool write SetAsBool;

      property Value: TJBase read FValue;

      // TJBase
      procedure Free; inline;

      function GetTypeName: string; inline;

      function Copy: TJBase; inline;
      procedure Assign(AFrom: TJBase); inline;

      property Parent: TJParent read GetParent;
      property Root: TJBase read GetRoot;
      property Index: Integer read GetIndex write SetIndex;
      property Name: string read GetName;
      function Path(AFrom: TJParent = nil): string; inline;

      function Formatter: IFormatter; inline;
      function Format: string; inline;
      function ToString: string; inline;

      function Equals(AOther: TJBase): Boolean; inline;

      // TJObject and TJArray
      property Values[AKey: string]: TJValue read GetValue write SetValue; default;
      property Values[AIndex: Integer]: TJValue read GetValue write SetValue; default;
      property Count: Integer read GetCount;
      property MaxIndex: Integer read GetMaxIndex;
      function Empty: Boolean; inline;

      function AddObject(AKey: string): TJObject; overload; inline;
      function AddArray(AKey: string): TJArray; overload; inline;
      function AddNull(AKey: string): TJNull; overload; inline;

      function Get(AKey: string; out AValue: TJBase): Boolean; overload; inline;
      function Get(AKey: string; out AValue: TJValue): Boolean; overload; inline;

      function Get(AKey: string; out AValue: TJObject): Boolean; overload; inline;
      function Get(AKey: string; out AValue: TJArray): Boolean; overload; inline;
      function Get(AKey: string; out AValue: TJString): Boolean; overload; inline;
      function Get(AKey: string; out AValue: TJNumber): Boolean; overload; inline;
      function Get(AKey: string; out AValue: TJBool): Boolean; overload; inline;

      function Add(AValue: TJValue): TJValue; inline;
      function AddObject: TJObject; overload; inline;
      function AddArray: TJArray; overload; inline;
      function AddNull: TJNull; overload; inline;
      function Insert(AValue: TJValue; AIndex: Integer = 0): TJValue; inline;
      function InsertObject(AIndex: Integer = 0): TJObject; inline;
      function InsertArray(AIndex: Integer = 0): TJArray; inline;
      function InsertNull(AIndex: Integer = 0): TJNull; inline;

      function TryRemove(AKey: string): Boolean; inline;
      procedure Remove(AKey: string); overload; inline;
      procedure Remove(AIndex: Integer); overload; inline;
      procedure Clear; inline;

      function Extract(AKey: string): TJValue; overload; inline;
      function Extract<T: TJBase>(AKey: string): T; overload; inline;
      function Extract(AIndex: Integer): TJValue; overload; inline;
      function Extract<T: TJBase>(AIndex: Integer): T; overload; inline;

      // TJString
      property Escaped: string read GetEscaped;

      // TJNumber
      function TryGetInt(out AValue: Int64): Boolean;

    end;

    TParser = class(TObjectParser<TJBase>)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  private
    FParent: TJParent;
    FIndex: Integer;

    procedure SetIndex(const Value: Integer);
    function GetRoot: TJBase;
    function GetName: string;

  protected
    procedure FormatInternal(AFormatter: TFormatter); virtual; abstract;

  public
    constructor Create; overload; virtual;
    destructor Destroy; override;

    class function GetTypeName: string; virtual;

    /// <summary>Parses any json value from the given string.</summary>
    class function Parse(AText: string): TJBase;
    /// <summary>Parses any json value from the given file.</summary>
    class function CreateFromFile(APath: string): TJBase;

    /// <summary>Creates an exact copy of the json value.</summary>
    function Copy: TJBase;
    /// <summary>Copies everything from the given value if of the same type.</summary>
    procedure Assign(AFrom: TJBase); virtual;

    /// <summary>The parent of this object or <c>nil</c> for root objects.</summary>
    property Parent: TJParent read FParent;
    /// <summary>The root object of this structure.</summary>
    /// <remarks>Can never be <c>nil</c>, as it returns itself, if the parent is <c>nil</c>.</remarks>
    property Root: TJBase read GetRoot;
    /// <summary>The index of this object in the parent object or 0 if no parent exists.</summary>
    property Index: Integer read FIndex write SetIndex;
    /// <summary>The key of this object if the parent is a JSON-Objects, or the index in <c>[1]</c> form for JSON-Arrays.</summary>
    property Name: string read GetName;
    /// <returns>The path to this specific JSON-Value going from the given parent or root if omitted.</returns>
    function Path(AFrom: TJParent = nil): string;

    /// <returns>A formatter interface with settings.</returns>
    function Formatter: IFormatter;
    /// <summary>Formats into a parseable string with default formatting.</summary>
    function Format: string;
    /// <summary>Formats into a parseable string without linebreaks.</summary>
    function ToString: string; override;

  end;

  TJValue = TJBase.TJValue;

  TJValues = TArray<TJValue>;

  TJPair = TJValue.TJPair;

  /// <summary>A common base class for JSON-Values containing other JSON-Values.</summary>
  TJParent = class abstract(TJBase)
  protected
    procedure RemoveItem(AIndex: Integer); virtual; abstract;
    procedure ChangeIndex(AFrom, ATo: Integer); virtual; abstract;
    function NameOf(AIndex: Integer): string; virtual; abstract;

    function GetCount: Integer; virtual; abstract;
    function GetMaxIndex: Integer;

  public
    class function GetTypeName: string; override;

    /// <summary>The count of JSON-Values in this JSON-Object or JSON-Array.</summary>
    property Count: Integer read GetCount;
    /// <summary>The highest index of this JSON-Object or JSON-Array.</summary>
    property MaxIndex: Integer read GetMaxIndex;
    function Empty: Boolean; virtual; abstract;
    procedure Clear; virtual; abstract;

  end;

  /// <summary>
  /// <p>A JSON-Object.</p>
  /// <p>Example:</p>
  /// <code>{"a": 42, "b": true, "c": "hello world"}</code>
  /// </summary>
  TJObject = class(TJParent)
  public type

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

    TOrder = TArray<TPair>;

    TMap = TMap<string, TJBase, TStringHasher>;

  private
    FOrder: TOrder;
    FMap: TMap;

    function GetValue(AKey: string): TJValue;
    procedure SetValue(AKey: string; const Value: TJValue); overload;
    function GetPair(AIndex: Integer): TJPair;

  protected
    procedure FormatInternal(AFormatter: TJBase.TFormatter); override;

    function GetCount: Integer; override;
    procedure RemoveItem(AIndex: Integer); override;
    procedure ChangeIndex(AFrom, ATo: Integer); override;
    function NameOf(AIndex: Integer): string; override;

  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetTypeName: string; override;

    class function Parse(AText: string): TJObject;
    class function CreateFromFile(APath: string): TJObject;

    function Copy: TJObject;
    procedure Assign(AFrom: TJBase); override;

    /// <summary>Used to read, append or modify the JSON-Values in this JSON-Object.</summary>
    /// <remarks>Never returns nil, but appends JSON-Null objects for non-existent keys.<p/>
    /// Creates a copy if the value is already owned.</remarks>
    property Values[AKey: string]: TJValue read GetValue write SetValue; default;
    /// <summary>Used to read pairs of this JSON-Object by index.</summary>
    property Pairs[AIndex: Integer]: TJPair read GetPair;
    /// <returns>True, if this JSON-Object is empty.</returns>
    function Empty: Boolean; override;

    /// <summary>Adds and returns a new JSON-Object with the given key.</summary>
    function AddObject(AKey: string): TJObject;
    /// <summary>Adds and returns a new JSON-Array with the given key.</summary>
    function AddArray(AKey: string): TJArray;
    /// <summary>Adds and returns a JSON-Null object with the given key.</summary>
    function AddNull(AKey: string): TJNull;

    /// <summary>Attempts to retrieve the json-value of the given key.</summary>
    /// <remarks>Unlike <c>Values</c> this does not create JSON-Null objects and just returns false.</remarks>
    function Get(AKey: string; out AValue: TJBase): Boolean; overload;
    /// <summary>Attempts to retrieve the json-value of the given key.</summary>
    /// <remarks>Unlike <c>Values</c> this does not create JSON-Null objects and just returns false.</remarks>
    function Get(AKey: string; out AValue: TJValue): Boolean; overload;

    /// <summary>Attempts to retrieve the json-value of the given key and check for a given type.</summary>
    function Get(AKey: string; out AValue: TJObject): Boolean; overload;
    /// <summary>Attempts to retrieve the json-value of the given key and check for a given type.</summary>
    function Get(AKey: string; out AValue: TJArray): Boolean; overload;
    /// <summary>Attempts to retrieve the json-value of the given key and check for a given type.</summary>
    function Get(AKey: string; out AValue: TJString): Boolean; overload;
    /// <summary>Attempts to retrieve the json-value of the given key and check for a given type.</summary>
    function Get(AKey: string; out AValue: TJNumber): Boolean; overload;
    /// <summary>Attempts to retrieve the json-value of the given key and check for a given type.</summary>
    function Get(AKey: string; out AValue: TJBool): Boolean; overload;

    /// <summary>Removes a specified key-value pair and returns true if one was found.</summary>
    function TryRemove(AKey: string): Boolean;
    /// <summary>Remoes a specified key-value pair and raises an exception if none was found.</summary>
    procedure Remove(AKey: string);
    /// <summary>Deletes all key-value pairs.</summary>
    procedure Clear; override;

    /// <summary>Removes a specified key-value pair, but unowns and returns it instead of freeing it.</summary>
    function Extract(AKey: string): TJValue; overload;
    /// <summary>Removes a specified key-value pair of a given type, but unowns and returns it instead of freeing it.</summary>
    function Extract<T: TJBase>(AKey: string): T; overload;

    function GetEnumerator: IIterator<TJPair>;
    function Equals(AObject: TObject): Boolean; override;

  end;

  /// <summary>
  /// <p>A JSON-Array.</p>
  /// <p>Example:</p>
  /// <code>[42, true, "hello world"]</code>
  /// </summary>
  TJArray = class(TJParent)
  public type

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

    TValues = TArray<TJBase>;

  private
    FValues: TValues;

    function GetValue(AIndex: Integer): TJValue;
    procedure SetValue(AIndex: Integer; const Value: TJValue);

  protected
    procedure FormatInternal(AFormatter: TJBase.TFormatter); override;

    function GetCount: Integer; override;
    procedure RemoveItem(AIndex: Integer); override;
    procedure ChangeIndex(AFrom, ATo: Integer); override;
    function NameOf(AIndex: Integer): string; override;

  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetTypeName: string; override;

    class function Parse(AText: string): TJArray;
    class function CreateFromFile(APath: string): TJArray;

    function Copy: TJArray;
    procedure Assign(AFrom: TJBase); override;

    /// <summary>Used to read, add or modify JSON-Values in this JSON-Array.</summary>
    /// <remarks>Automatically fills up missing indices with JSON-Null objects.<p/>
    /// Creates a copy if the value is already owned.</remarks>
    property Values[AIndex: Integer]: TJValue read GetValue write SetValue; default;
    /// <returns>True, if this JSON-Array is empty.</returns>
    function Empty: Boolean; override;

    /// <summary>Appends a JSON-Value at the end of the JSON-Array.</summary>
    /// <remarks>Creates a copy if the value is already owned.</remarks>
    function Add(AValue: TJValue): TJValue;
    /// <summary>Appends a JSON-Object at the end of the JSON-Array.</summary>
    function AddObject: TJObject;
    /// <summary>Appends a JSON-Array at the end of the JSON-Array.</summary>
    function AddArray: TJArray;
    /// <summary>Appends a JSON-Null object at the end of the JSON-Array.</summary>
    function AddNull: TJNull;

    /// <summary>Inserts a JSON-Value at a given position or the start if omitted.</summary>
    /// <remarks>Creates a copy if the value is already owned.</remarks>
    function Insert(AValue: TJValue; AIndex: Integer = 0): TJValue;
    /// <summary>Inserts a JSON-Object at a given position or the start if omitted.</summary>
    function InsertObject(AIndex: Integer = 0): TJObject;
    /// <summary>Inserts a JSON-Array at a given position or the start if omitted.</summary>
    function InsertArray(AIndex: Integer = 0): TJArray;
    /// <summary>Inserts a JSON-Null object at a given position or the start if omitted.</summary>
    function InsertNull(AIndex: Integer = 0): TJNull;

    /// <summary>Removes a JSON-Value at a given index.</summary>
    procedure Remove(AIndex: Integer);
    /// <summary>Deletes all JSON-Values.</summary>
    procedure Clear; override;

    /// <summary>Removes a JSON-Value at the given index, but unowns and returns it instead of freeing it.</summary>
    function Extract(AIndex: Integer): TJValue; overload;
    /// <summary>Removes a JSON-Value at the given index of the given type, but unowns and returns it instead of freeing it.</summary>
    function Extract<T: TJBase>(AIndex: Integer): T; overload;

    function GetEnumerator: IIterator<TJValue>;
    function Equals(AObject: TObject): Boolean; override;

  end;

  /// <summary>A double quoted string with backslash escaping.</summary>
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

    function GetEscaped: string;

  protected
    procedure FormatInternal(AFormatter: TJBase.TFormatter); override;

  public
    constructor Create(AText: string); reintroduce; overload;

    class function GetTypeName: string; override;

    procedure Assign(AValue: TJBase); override;

    /// <summary>The text of this JSON-String.</summary>
    property Text: string read FText write FText;
    /// <summary>The text of this JSON-String with quotes and escaped.</summary>
    property Escaped: string read GetEscaped;

    /// <returns>An escaped version of the given string with double-quotes.</returns>
    /// <remarks>
    /// Following simple escapes are used:<p/>
    /// <c>\\ \" \b \t \n \f \r</c><p/>
    /// Unicode escaping is used for everything outside of:<p/>
    /// <c>\u0020</c> to <c>\u007E</c> and <c>\u00A0</c> to <c>\u00FF</c>
    /// </remarks>
    class function Escape(AText: string): string;

    function Equals(AObject: TObject): Boolean; override;

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
    procedure FormatInternal(AFormatter: TJBase.TFormatter); override;

  public
    constructor Create(ANumber: TJBase.TNumber); reintroduce; overload;

    class function GetTypeName: string; override;

    procedure Assign(AFrom: TJBase); override;

    /// <summary>The value of this JSON-Number.</summary>
    property Number: TJBase.TNumber read FNumber write FNumber;

    /// <summary>Attempts to convert the current value into an integer.</summary>
    function TryGetInt(out AValue: Int64): Boolean;

    function Equals(AObject: TObject): Boolean; override;

  end;

  /// <summary>A boolean, <c>true</c> or <c>false</c>.</summary>
  TJBool = class(TJBase)
  public const

    BoolStrings: array [Boolean] of string = ('false', 'true');

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
    procedure FormatInternal(AFormatter: TJBase.TFormatter); override;

  public
    constructor Create(AValue: Boolean); reintroduce; overload;

    class function GetTypeName: string; override;

    procedure Assign(AFrom: TJBase); override;

    /// <summary>The value of this JSON-Bool.</summary>
    property Value: Boolean read FValue write FValue;

    function Equals(AObject: TObject): Boolean; override;

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
    procedure FormatInternal(AFormatter: TJBase.TFormatter); override;

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

destructor TJBase.Destroy;
begin
  if Parent <> nil then
    Parent.RemoveItem(Index);
  inherited;
end;

function TJBase.Format: string;
begin
  Result := Formatter.Format;
end;

function TJBase.Formatter: IFormatter;
begin
  Result := TFormatter.Create(Self);
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
  with Formatter do
  begin
    Pretty := False;
    Result := Format;
  end;
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

{ TJBase.TJValue }

class operator TJBase.TJValue.Implicit(AText: string): TJValue;
begin
  Result := TJString.Create(AText);
end;

class operator TJBase.TJValue.Implicit(AValue: TJValue): TJBase;
begin
  Result := AValue.Value;
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

function TJBase.TJValue.Add(AValue: TJValue): TJValue;
begin
  Result := AsArray.Add(AValue);
end;

function TJBase.TJValue.AddArray(AKey: string): TJArray;
begin
  Result := AsObject.AddArray(AKey);
end;

function TJBase.TJValue.AddNull(AKey: string): TJNull;
begin
  Result := AsObject.AddNull(AKey);
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

procedure TJBase.TJValue.Clear;
begin
  Cast<TJParent>.Clear;
end;

function TJBase.TJValue.Copy: TJBase;
begin
  Result := FValue.Copy;
end;

function TJBase.TJValue.Empty: Boolean;
begin
  Result := Cast<TJParent>.Empty;
end;

function TJBase.TJValue.Equals(AOther: TJBase): Boolean;
begin
  Result := Value.Equals(AOther);
end;

function TJBase.TJValue.Exists: Boolean;
begin
  Result := not IsNull;
end;

function TJBase.TJValue.Extract(AKey: string): TJValue;
begin
  Result := AsObject.Extract(AKey);
end;

function TJBase.TJValue.Extract(AIndex: Integer): TJValue;
begin
  Result := AsArray.Extract(AIndex);
end;

function TJBase.TJValue.Extract<T>(AKey: string): T;
begin
  Result := AsObject.Extract<T>(AKey);
end;

function TJBase.TJValue.Extract<T>(AIndex: Integer): T;
begin
  Result := AsArray.Extract<T>(AIndex);
end;

function TJBase.TJValue.Format: string;
begin
  Result := FValue.Format;
end;

function TJBase.TJValue.Formatter: IFormatter;
begin
  Result := Value.Formatter;
end;

procedure TJBase.TJValue.Free;
begin
  FreeAndNil(FValue);
end;

function TJBase.TJValue.Get(AKey: string; out AValue: TJObject): Boolean;
begin
  Result := AsObject.Get(AKey, AValue);
end;

function TJBase.TJValue.Get(AKey: string; out AValue: TJValue): Boolean;
begin
  Result := AsObject.Get(AKey, AValue);
end;

function TJBase.TJValue.Get(AKey: string; out AValue: TJBase): Boolean;
begin
  Result := AsObject.Get(AKey, AValue);
end;

function TJBase.TJValue.Get(AKey: string; out AValue: TJArray): Boolean;
begin
  Result := AsObject.Get(AKey, AValue);
end;

function TJBase.TJValue.Get(AKey: string; out AValue: TJBool): Boolean;
begin
  Result := AsObject.Get(AKey, AValue);
end;

function TJBase.TJValue.Get(AKey: string; out AValue: TJNumber): Boolean;
begin
  Result := AsObject.Get(AKey, AValue);
end;

function TJBase.TJValue.Get(AKey: string; out AValue: TJString): Boolean;
begin
  Result := AsObject.Get(AKey, AValue);
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
  if not TryGetInt(Result) then
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

function TJBase.TJValue.GetCount: Integer;
begin
  Result := Cast<TJParent>.Count;
end;

function TJBase.TJValue.GetEscaped: string;
begin
  Result := Cast<TJString>.Escaped;
end;

function TJBase.TJValue.GetIndex: Integer;
begin
  Result := FValue.Index;
end;

function TJBase.TJValue.GetMaxIndex: Integer;
begin
  Result := Cast<TJParent>.MaxIndex;
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

function TJBase.TJValue.Insert(AValue: TJValue; AIndex: Integer = 0): TJValue;
begin
  Result := AsArray.Insert(AValue, AIndex);
end;

function TJBase.TJValue.InsertArray(AIndex: Integer): TJArray;
begin
  Result := AsArray.InsertArray(AIndex);
end;

function TJBase.TJValue.InsertNull(AIndex: Integer): TJNull;
begin
  Result := AsArray.InsertNull(AIndex);
end;

function TJBase.TJValue.InsertObject(AIndex: Integer): TJObject;
begin
  Result := AsArray.InsertObject(AIndex);
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
  Result := FValue is TJNull;
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

procedure TJBase.TJValue.Remove(AKey: string);
begin
  AsObject.Remove(AKey);
end;

procedure TJBase.TJValue.Remove(AIndex: Integer);
begin
  AsArray.Remove(AIndex);
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

function TJBase.TJValue.TryGetInt(out AValue: Int64): Boolean;
begin
  Result := Cast<TJNumber>.TryGetInt(AValue);
end;

function TJBase.TJValue.TryRemove(AKey: string): Boolean;
begin
  Result := AsObject.TryRemove(AKey);
end;

function TJBase.TJValue.Cast<T>(out AValue: T): Boolean;
begin
  if Value is T then
  begin
    AValue := T(Value);
    Exit(True);
  end;
  Result := False;
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

function TJBase.TJValue.AddArray: TJArray;
begin
  Result := AsArray.AddArray;
end;

function TJBase.TJValue.AddNull: TJNull;
begin
  Result := AsArray.AddNull;
end;

function TJBase.TJValue.AddObject: TJObject;
begin
  Result := AsArray.AddObject;
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

{ TJBase.TFormatter }

procedure TJBase.TFormatter.AddIndentation;
begin
  Builder.Append(' ', IndentLevel * IndentWidth);
end;

constructor TJBase.TFormatter.Create;
begin
  inherited;
  FPretty := DefaultPretty;
  FArrayWrapThreshold := DefaultArrayWrapThreshold;
  FIndentWidth := DefaultIndentWidth;
end;

function TJBase.TFormatter.Format: string;
begin
  FBuilder := TStringBuilder.Create;
  try
    Value.FormatInternal(Self);
    Result := FBuilder.ToString;

  finally
    Builder.Free;

  end;
end;

function TJBase.TFormatter.GetArrayWrapThreshold: Integer;
begin
  Result := FArrayWrapThreshold;
end;

function TJBase.TFormatter.GetIndentWidth: Integer;
begin
  Result := FIndentWidth;
end;

function TJBase.TFormatter.GetPretty: Boolean;
begin
  Result := FPretty;
end;

procedure TJBase.TFormatter.Indent;
begin
  Inc(FIndentLevel);
  NewLine;
end;

procedure TJBase.TFormatter.NewLine;
begin
  if Pretty then
  begin
    Builder.AppendLine;
    AddIndentation;
  end;
end;

procedure TJBase.TFormatter.SetArrayWrapThreshold(const Value: Integer);
begin
  FArrayWrapThreshold := Value;
end;

procedure TJBase.TFormatter.SetIndentWidth(const Value: Integer);
begin
  FIndentWidth := Value;
end;

procedure TJBase.TFormatter.SetPretty(const Value: Boolean);
begin
  FPretty := Value;
end;

procedure TJBase.TFormatter.Unindent;
begin
  Dec(FIndentLevel);
  NewLine;
end;

{ TJParent }

function TJParent.GetMaxIndex: Integer;
begin
  Result := Count - 1;
end;

class function TJParent.GetTypeName: string;
begin
  Result := 'JSON-Parent';
end;

{ TJObject }

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
    Values[JPair.Key] := JPair.Value.Copy;
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
var
  Pair: TPair;
begin
  for Pair in FOrder do
  begin
    Pair.Value.FParent := nil;
    Pair.Value.Free;
  end;
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
  Clear;
  FMap.Free;
  FOrder.Free;
  inherited;
end;

function TJObject.Empty: Boolean;
begin
  Result := FOrder.Empty;
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
  Result.Value.FParent := nil;
  RemoveItem(Result.Index);
end;

function TJObject.Extract<T>(AKey: string): T;
begin
  Result := Extract(AKey).Cast<T>;
end;

procedure TJObject.FormatInternal(AFormatter: TJBase.TFormatter);

  procedure AppendIndex(I: Integer);
  begin
    AFormatter.Builder.Append(TJString.Escape(FOrder[I].Key));
    AFormatter.Builder.Append(': ');
    FOrder[I].Value.FormatInternal(AFormatter);
  end;

var
  I: Integer;
begin
  AFormatter.Builder.Append('{');
  if not Empty then
  begin
    AFormatter.Indent;
    AppendIndex(0);
    for I := 1 to MaxIndex do
    begin
      AFormatter.Builder.Append(',');
      if not AFormatter.Pretty then
        AFormatter.Builder.Append(' ')
      else
        AFormatter.NewLine;
      AppendIndex(I);
    end;
    AFormatter.Unindent;
  end;
  AFormatter.Builder.Append('}');
end;

function TJObject.Get(AKey: string; out AValue: TJBase): Boolean;
begin
  Result := FMap.Get(AKey, AValue);
end;

function TJObject.GetCount: Integer;
begin
  Result := FOrder.Count;
end;

function TJObject.Get(AKey: string; out AValue: TJValue): Boolean;
begin
  Result := Get(AKey, AValue.FValue);
end;

function TJObject.GetEnumerator: IIterator<TJPair>;
begin
  Result := TIterator.Create(Self);
end;

function TJObject.GetPair(AIndex: Integer): TJPair;
var
  Pair: TPair;
begin
  Pair := FOrder[AIndex];
  Result := TJPair.Create(Pair.Key, Pair.Value);
end;

class function TJObject.GetTypeName: string;
begin
  Result := 'JSON-Object';
end;

function TJObject.GetValue(AKey: string): TJValue;
begin
  if not FMap.Get(AKey, Result.FValue) then
  begin
    Result := TJNull.Create;
    Values[AKey] := Result;
  end;
end;

function TJObject.NameOf(AIndex: Integer): string;
begin
  Result := FOrder[AIndex].Key;
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

procedure TJObject.RemoveItem(AIndex: Integer);
var
  I: Integer;
begin
  FMap.TryRemove(FOrder[AIndex].Key);
  FOrder.RemoveAt(AIndex);
  for I := AIndex to MaxIndex do
    FOrder[I].Value.FIndex := I;
end;

procedure TJObject.SetValue(AKey: string; const Value: TJValue);
var
  JValue, OldValue: TJBase;
begin
  if Value.Parent <> nil then
    JValue := Value.Copy
  else
    JValue := Value.FValue;
  JValue.FParent := Self;
  if FMap.Get(AKey, OldValue) then
  begin
    FOrder[OldValue.Index] := TPair.Create(FOrder[OldValue.Index].Key, JValue);
    FMap[AKey] := JValue;
    JValue.FIndex := OldValue.Index;
    OldValue.FParent := nil;
    OldValue.Free;
    Exit;
  end;
  FOrder.Add(TPair.Create(AKey, JValue));
  JValue.FIndex := FOrder.MaxIndex;
  FMap[AKey] := JValue;
end;

function TJObject.TryRemove(AKey: string): Boolean;
var
  RemovedValue: TJBase;
begin
  Result := FMap.Get(AKey, RemovedValue);
  if Result then
    RemovedValue.Free;
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

{ TJObject.TIterator }

constructor TJObject.TIterator.Create(AJObject: TJObject);
begin
  FJObject := AJObject;
  FCurrent := -1;
end;

function TJObject.TIterator.GetCurrent: TJPair;
begin
  Result := FJObject.Pairs[FCurrent];
end;

function TJObject.TIterator.MoveNext: Boolean;
begin
  Inc(FCurrent);
  Result := FCurrent < FJObject.Count;
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

{ TJArray }

function TJArray.Add(AValue: TJValue): TJValue;
begin
  if AValue.Parent <> nil then
    Result := AValue.Copy
  else
    Result := AValue;
  Result.FValue.FParent := Self;
  FValues.Add(Result);
  Result.FValue.FIndex := MaxIndex;
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
  Value: TJBase;
begin
  if not(AFrom is TJArray) then
    raise EJSONError.Create('Cannot assign value to array.');
  FValues.Clear;
  FValues.Capacity := TJArray(AFrom).Count;
  FValues.ForceCount(FValues.Capacity);
  for I := 0 to TJArray(AFrom).FValues.MaxIndex do
  begin
    Value := TJArray(AFrom).Values[I].Copy;
    Value.FIndex := I;
    Value.FParent := Self;
    FValues[I] := Value;
  end;
end;

procedure TJArray.ChangeIndex(AFrom, ATo: Integer);
var
  I: Integer;
begin
  FValues.SetIndex(AFrom, ATo);
  for I := Min(AFrom, ATo) to Max(AFrom, ATo) do
    FValues[I].FIndex := I;
end;

procedure TJArray.Clear;
var
  Value: TJBase;
begin
  for Value in FValues do
  begin
    Value.FParent := nil;
    Value.Free;
  end;
  FValues.Clear;
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
  Clear;
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
  Result.Value.FParent := nil;
  Remove(AIndex);
end;

function TJArray.Extract<T>(AIndex: Integer): T;
begin
  Result := Extract(AIndex).Cast<T>;
end;

procedure TJArray.FormatInternal(AFormatter: TJBase.TFormatter);
var
  Wrapped: Boolean;
  I: Integer;
begin
  Wrapped := AFormatter.Pretty;
  if Wrapped then
  begin
    Wrapped := Count > AFormatter.ArrayWrapTheshold;
    if not Wrapped then
      for I := 0 to Min(AFormatter.ArrayWrapTheshold - 1, MaxIndex) do
        if Values[I].Value is TJParent then
        begin
          Wrapped := True;
          Break;
        end;
  end;
  AFormatter.Builder.Append('[');
  if Wrapped then
    AFormatter.Indent;
  if not Empty then
  begin
    FValues[0].FormatInternal(AFormatter);
    for I := 1 to MaxIndex do
    begin
      AFormatter.Builder.Append(',');
      if not Wrapped then
        AFormatter.Builder.Append(' ')
      else if Wrapped then
        AFormatter.NewLine;
      FValues[I].FormatInternal(AFormatter);
    end;
  end;
  if Wrapped then
    AFormatter.Unindent;
  AFormatter.Builder.Append(']');
end;

function TJArray.GetCount: Integer;
begin
  Result := FValues.Count;
end;

function TJArray.GetEnumerator: IIterator<TJValue>;
begin
  Result := TIterator.Create(Self);
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

function TJArray.Insert(AValue: TJValue; AIndex: Integer = 0): TJValue;
var
  I: Integer;
begin
  if AValue.Parent <> nil then
    Result := AValue.Copy
  else
    Result := AValue;
  FValues.Insert(Result, AIndex);
  Result.Value.FParent := Self;
  for I := AIndex to FValues.MaxIndex do
    FValues[I].FIndex := I;
end;

function TJArray.InsertArray(AIndex: Integer): TJArray;
begin
  Result := TJArray.Create;
  Insert(Result, AIndex);
end;

function TJArray.InsertNull(AIndex: Integer): TJNull;
begin
  Result := TJNull.Create;
  Insert(Result, AIndex);
end;

function TJArray.InsertObject(AIndex: Integer): TJObject;
begin
  Result := TJObject.Create;
  Insert(Result, AIndex);
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
begin
  FValues[AIndex].Free;
end;

procedure TJArray.RemoveItem(AIndex: Integer);
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
  if Value.Parent <> nil then
    JValue := Value.Copy
  else
    JValue := Value.FValue;
  JValue.FParent := Self;
  if FValues.RangeCheck(AIndex) then
  begin
    FValues[AIndex].FParent := nil;
    FValues[AIndex].Free;
    FValues[AIndex] := JValue;
    JValue.FIndex := AIndex;
  end
  else
  begin
    while MaxIndex < AIndex - 1 do
      AddNull;
    FValues.Add(JValue);
    JValue.FIndex := MaxIndex;
  end;
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

procedure TJString.FormatInternal(AFormatter: TJBase.TFormatter);
begin
  AFormatter.Builder.Append(Escaped);
end;

function TJString.GetEscaped: string;
begin
  Result := Escape(Text);
end;

class function TJString.GetTypeName: string;
begin
  Result := 'JSON-String';
end;

class function TJString.Escape(AText: string): string;
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
    else if (AText[Current] < #$20) or (AText[Current] > #$FF) or (AText[Current] > #$7E) and (AText[Current] < #$A0)
    then
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

procedure TJNumber.FormatInternal(AFormatter: TJBase.TFormatter);
begin
  case Number.NumType of
    ntInt:
      AFormatter.Builder.Append(Number.AsInt);
    ntSingle:
      AFormatter.Builder.Append(PrettyFloat(Single(Number.AsFloat)));
    ntDouble:
      AFormatter.Builder.Append(PrettyFloat(Number.AsFloat));
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

procedure TJBool.FormatInternal(AFormatter: TJBase.TFormatter);
begin
  AFormatter.Builder.Append(BoolStrings[Value]);
end;

class function TJBool.GetTypeName: string;
begin
  Result := 'JSON-Bool';
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

{ TJNull }

function TJNull.Equals(AObject: TObject): Boolean;
begin
  Result := (AObject = nil) or (AObject is TJNull);
end;

procedure TJNull.FormatInternal(AFormatter: TJBase.TFormatter);
begin
  AFormatter.Builder.Append(NullString);
end;

class function TJNull.GetTypeName: string;
begin
  Result := 'JSON-Null';
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
