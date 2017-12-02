unit Pengine.ICollections;

interface

uses
  System.Classes,
  System.Math,
  System.SysUtils,

  Pengine.Sorting,
  Pengine.IntMaths,
  Pengine.Vector;

type

  /// <summary>Raised, if an operation requires the array to have at least one item.</summary>
  EArrayEmpty = class(Exception)
  public
    constructor Create;
  end;

  /// <summary>Raised, if an item could not be found in the array.</summary>
  EArrayItemNotFound = class(Exception)
  public
    constructor Create;
  end;

  /// <summary>Raised, if an array-index was out of bounds.</summary>
  EArrayRangeError = class(Exception)
  public
    constructor Create(AIndex, ACount: Integer);
  end;

  /// <summary>Raised, if an array-item does not have a string-representative.</summary>
  EArrayItemNoStringRepresentative = class(Exception)
  public
    constructor Create;
  end;

  /// <summary>Raised, if a negative Buckets is tried to assigned to an array.</summary>
  EArrayNegativeCapacity = class(Exception)
  public
    constructor Create;
  end;

  // TODO: XmlDoc
  EArrayInvalidGrowAmount = class(Exception)
  public
    constructor Create;
  end;

  // TODO: XmlDoc
  EArrayInvalidShrinkRetain = class(Exception)
  public
    constructor Create;
  end;

  // TODO: XmlDoc
  ETooManyHashBuckets = class(Exception)
  public
    constructor Create;
  end;

  // TODO: XmlDoc
  EInvalidHashBucketCount = class(Exception)
  public
    constructor Create;
  end;

  // TODO: XmlDoc
  EHashBucketRequired = class(Exception)
  public
    constructor Create;
  end;

  // TODO: XmlDoc
  EMapKeyNotFound = class(Exception)
  public
    constructor Create;
  end;

  // TODO: XmlDoc
  ESetKeyNotFound = class(Exception)
  public
    constructor Create;
  end;

  // TODO: XmlDoc
  ESetKeyExistsAlready = class(Exception)
  public
    constructor Create;
  end;

  // TODO: XmlDoc
  EHashEmpty = class(Exception)
  public
    constructor Create;
  end;

  // TODO: XmlDoc
  EHashKeyNotIndexable = class(Exception)
  public
    constructor Create;
  end;

  /// <summary>A generic Interface for an Iterator. Used by <see cref="Pengine.Collections|IIterable`1"/>.</summary>
  IIterator<T> = interface
    ['{01FA5E8D-FB71-4D60-B113-429284B8B8F7}']
    function MoveNext: Boolean;
    function GetCurrent: T;
    property Current: T read GetCurrent;
  end;

  // TODO: XmlDoc
  TIterator<T> = class abstract(TInterfacedObject, IIterator<T>)
  public
    function MoveNext: Boolean; virtual; abstract;
    function GetCurrent: T; virtual; abstract;
    property Current: T read GetCurrent;
  end;

  /// <summary>A generic interface for an iteratable type. Uses <see cref="Pengine.Collections|IIterator`1"/>.</summary>
  IIterable<T> = interface
    ['{86380564-F207-4B73-A40D-F10AD12B5B98}']
    function GetEnumerator: IIterator<T>;
    function Count: Integer;
    function CountOptimized: Boolean;
  end;

  /// <summary>A generic interface, for searchable objects.</summary>
  IFindable<T> = interface
    ['{9FE5998D-6A74-4BC9-A06B-129C4E72D313}']
    function Check(AItem: T): Boolean;
  end;

  TFindFuncStatic<T> = function(AItem: T): Boolean;
  TFindFuncRef<T> = reference to function(AItem: T): Boolean;
  TFindFunc<T> = function(AItem: T): Boolean of object;

  /// <summary>A generic interface for comparable objects.</summary>
  IComparable<T> = interface
    ['{577FBD7F-8894-4012-BAD0-42809985D928}']
    function Compare(ALeft, ARight: T): Boolean;
  end;

  TCompareFuncStatic<T> = function(ALeft, ARight: T): Boolean;
  TCompareFuncRef<T> = reference to function(ALeft, ARight: T): Boolean;
  TCompareFunc<T> = function(ALeft, ARight: T): Boolean of object;

  /// <summary>A generic base class for iteratable types. Implements <see cref="Pengine.Collections|IIterable`1"/>.</summary>
  /// <remarks>The Count function should almost definitly be overriden in the derived class.</remarks>
  TIterable<T> = class(TInterfacedObject, IIterable<T>)
  public
    function GetEnumerator: IIterator<T>; virtual; abstract;

    /// <remarks>This function should almost definitely be overwritten.</remarks>
    function Count: Integer; virtual;
    // TODO: XmlDoc
    function CountOptimized: Boolean; virtual;
  end;

  /// <summary>A generic class, representing a read-only Key-Value-Pair.</summary>
  TPair<K, V> = record
  private
    FKey: K;
    FValue: V;

  public
    constructor Create(AKey: K; AValue: V);

    property Key: K read FKey;
    property Value: V read FValue;
  end;

  IArray = interface;

  IArrayReader = interface
    ['{4653A23E-6426-4E9C-9009-64D2CEFAC8F8}']

    { IArrayReader }

    function GetGrowAmount: Integer;
    function GetShrinkRetain: Integer;
    function GetCapacity: Integer;

    property GrowAmount: Integer read GetGrowAmount;
    property ShrinkRetain: Integer read GetShrinkRetain;
    property Capacity: Integer read GetCapacity;

    function Count: Integer;
    function CountOptimized: Boolean;
    function MaxIndex: Integer;
    function Empty: Boolean;

    procedure RangeCheckException(AIndex: Integer);
    function RangeCheck(AIndex: Integer): Boolean;

    function DataPointer: Pointer;

    function Copy: IArray;

  end;

  IArray = interface(IArrayReader)
    ['{F303D1D9-65CB-4916-9B16-5C96EB94D505}']

    { IArrayReader }

    function GetGrowAmount: Integer;
    function GetShrinkRetain: Integer;
    function GetCapacity: Integer;

    function Count: Integer;
    function CountOptimized: Boolean;
    function MaxIndex: Integer;
    function Empty: Boolean;

    procedure RangeCheckException(AIndex: Integer);
    function RangeCheck(AIndex: Integer): Boolean;

    function DataPointer: Pointer;

    function Copy: IArray;

    { IArray }

    procedure SetGrowAmount(const Value: Integer);
    procedure SetShrinkRetain(const Value: Integer);
    procedure SetCapacity(const Value: Integer);

    property GrowAmount: Integer read GetGrowAmount write SetGrowAmount;
    property ShrinkRetain: Integer read GetShrinkRetain write SetShrinkRetain;
    property Capacity: Integer read GetCapacity write SetCapacity;

    procedure DelAt(AIndex: Integer);
    procedure DelLast;
    procedure Clear;

    procedure Swap(A, B: Integer);
    procedure SwapUnchecked(A, B: Integer);

  end;

  IArrayReader<T> = interface(IArrayReader)
    ['{70C52481-77ED-4519-A31F-0111365D56C8}']

    { IArrayReader }

    function GetGrowAmount: Integer;
    function GetShrinkRetain: Integer;
    function GetCapacity: Integer;

    function Count: Integer;
    function CountOptimized: Boolean;
    function MaxIndex: Integer;
    function Empty: Boolean;

    procedure RangeCheckException(AIndex: Integer);
    function RangeCheck(AIndex: Integer): Boolean;

    function DataPointer: Pointer;

    { IIterable<T> }

    function GetEnumerator: IIterator<T>;

    { IArrayReader<T> }

    property GrowAmount: Integer read GetGrowAmount;
    property ShrinkRetain: Integer read GetShrinkRetain;
    property Capacity: Integer read GetCapacity;

    function FindFirstIndex(AFunc: TFindFuncStatic<T>): Integer; overload;
    function FindFirstIndex(AFunc: TFindFuncRef<T>): Integer; overload;
    function FindFirstIndex(AFunc: TFindFunc<T>): Integer; overload;

    function FindFirst(AFunc: TFindFuncStatic<T>): T; overload;
    function FindFirst(AFunc: TFindFuncRef<T>): T; overload;
    function FindFirst(AFunc: TFindFunc<T>): T; overload;

    function FindLastIndex(AFunc: TFindFuncStatic<T>): Integer; overload;
    function FindLastIndex(AFunc: TFindFuncRef<T>): Integer; overload;
    function FindLastIndex(AFunc: TFindFunc<T>): Integer; overload;

    function FindLast(AFunc: TFindFuncStatic<T>): T; overload;
    function FindLast(AFunc: TFindFuncRef<T>): T; overload;
    function FindLast(AFunc: TFindFunc<T>): T; overload;

    function FindIndexAsArray(AFunc: TFindFuncStatic<T>): IIntArray; overload;
    function FindIndexAsArray(AFunc: TFindFuncRef<T>): IIntArray; overload;
    function FindIndexAsArray(AFunc: TFindFunc<T>): IIntArray; overload;

    function FindAsArray(AFunc: TFindFuncStatic<T>): IArray<T>; overload; virtual;
    function FindAsArray(AFunc: TFindFuncRef<T>): IArray<T>; overload; virtual;
    function FindAsArray(AFunc: TFindFunc<T>): IArray<T>; overload; virtual;

    function Sorted(AFunc: TCompareFuncStatic<T>): Boolean; overload;
    function Sorted(AFunc: TCompareFuncRef<T>): Boolean; overload;
    function Sorted(AFunc: TCompareFunc<T>): Boolean; overload;

    function BinarySearch(AItem: T; AFunc: TCompareFuncStatic<T>): Integer; overload;
    function BinarySearch(AItem: T; AFunc: TCompareFuncRef<T>): Integer; overload;
    function BinarySearch(AItem: T; AFunc: TCompareFunc<T>): Integer; overload;

    function Copy: TArray<T>;

    function GetItem(I: Integer): T;
    property Items[I: Integer]: T read GetItem; default;

    function GetFirst: T;
    property First: T read GetFirst;

    function GetLast: T;
    property Last: T read GetLast;

    function InReverse: TArray<T>.TReverseWrapper;

    // TODO: XmlDoc
    function ToString: string; override;
    // TODO: XmlDoc
    class function ItemToString(AItem: T): string; virtual;

  end;

  TArray = class(TInterfacedObject, IArray, IArrayReader)
  public const

    // TODO: XmlDoc
    NeverShrink = Integer.MaxValue;

  private
    FGrowAmount: Integer;
    FShrinkRetain: Integer;

    procedure SetGrowAmount(const Value: Integer);
    procedure SetShrinkRetain(const Value: Integer);
    function GetGrowAmount: Integer;
    function GetShrinkRetain: Integer;

  protected
    FCount: Integer;

    function CreateSame(AGrowAmount: Integer = 16; AShrinkRetain: Integer = 8): TArray;

    // TODO: XmlDoc
    function ShouldFreeItems: Boolean; virtual;
    // TODO: XmlDoc
    procedure ItemRemoved(AIndex: Integer); virtual;

    // TODO: XmlDoc
    function GetCapacity: Integer; virtual; abstract;
    // TODO: XmlDoc
    procedure SetCapacity(const Value: Integer); virtual; abstract;

    procedure CopyTo(AArray: TArray); virtual; abstract;

    function CreateCopy: TArray; virtual;

  public
    // TODO: XmlDoc
    constructor Create(AGrowAmount: Integer = 16; AShrinkRetain: Integer = 8); virtual;
    // TODO: XmlDoc
    destructor Destroy; override;

    // TODO: XmlDoc
    property GrowAmount: Integer read GetGrowAmount write SetGrowAmount;
    // TODO: XmlDoc
    property ShrinkRetain: Integer read GetShrinkRetain write SetShrinkRetain;

    // TODO: XmlDoc
    function Count: Integer; inline;
    // TODO: XmlDoc
    function CountOptimized: Boolean; inline;
    // TODO: XmlDoc Remarks: returns -1 on empty arrays
    function MaxIndex: Integer; inline;
    // TODO: XmlDoc
    function Empty: Boolean; inline;

    // TODO: XmlDoc
    procedure DelAt(AIndex: Integer); virtual; abstract;
    // TODO: XmlDoc
    procedure DelLast; inline;
    // TODO: XmlDoc
    procedure Clear;

    // TODO: XmlDoc
    property Capacity: Integer read GetCapacity write SetCapacity;

    // TODO: XmlDoc
    procedure RangeCheckException(AIndex: Integer); inline;
    // TODO: XmlDoc
    function RangeCheck(AIndex: Integer): Boolean; inline;

    // TODO: XmlDoc
    procedure Swap(A, B: Integer);
    // TODO: XmlDoc
    procedure SwapUnchecked(A, B: Integer); virtual;

    // TODO: XmlDoc
    function DataPointer: Pointer; virtual; abstract;

    // TODO: XmlDoc
    function Copy: IArray;

  end;

  TArrayClass = class of TArray;

  TIntArray = class;

  // TODO: XmlDoc
  TArray<T> = class(TArray, IIterable<T>)
  public type

    // TODO: XmlDoc
    TIterator = class(TIterator<T>)
    private
      FList: TArray<T>;
      FCurrent: Integer;
      FReversed: Boolean;
      FRemoveFlag: Boolean;

    public
      constructor Create(AList: TArray<T>; AReversed: Boolean = False);

      // TODO: XmlDoc
      function MoveNext: Boolean; override;
      // TODO: XmlDoc
      function GetCurrent: T; override;

      // TODO: XmlDoc
      procedure RemoveCurrent; inline;

    end;

    // TODO: XmlDoc
    TReverseWrapper = record
    private
      FArray: TArray<T>;

    public
      // TODO: XmlDoc
      constructor Create(AArray: TArray<T>);

      // TODO: XmlDoc
      function GetEnumerator(AAutoFree: Boolean = False): TIterator;

      // TODO: XmlDoc
      function Copy: TArray<T>;

    end;

    // TODO: XmlDoc
    TSorterBase = class abstract(TQuickSorter)
    private
      FArray: TArray<T>;
      FPivot: T;

    protected
      function Bounds: TIntBounds1; override;

      procedure SavePivot(I: Integer); override;

      procedure Swap(A: Integer; B: Integer); override;

    public
      // TODO: XmlDoc
      constructor Create(AArray: TArray<T>);

    end;

    // TODO: XmlDoc
    TSorterStatic = class(TSorterBase)
    private
      FFunc: TCompareFuncStatic<T>;
    protected
      function BeforePivot(I: Integer): Boolean; override;
      function AfterPivot(I: Integer): Boolean; override;
    public
      constructor Create(AArray: TArray<T>; AFunc: TCompareFuncStatic<T>);
    end;

    // TODO: XmlDoc
    TSorterRef = class(TSorterBase)
    private
      FFunc: TCompareFuncRef<T>;
    protected
      function BeforePivot(I: Integer): Boolean; override;
      function AfterPivot(I: Integer): Boolean; override;
    public
      constructor Create(AArray: TArray<T>; AFunc: TCompareFuncRef<T>);
    end;

    // TODO: XmlDoc
    TSorter = class(TSorterBase)
    private
      FFunc: TCompareFunc<T>;
    protected
      function BeforePivot(I: Integer): Boolean; override;
      function AfterPivot(I: Integer): Boolean; override;
    public
      constructor Create(AArray: TArray<T>; AFunc: TCompareFunc<T>);
    end;

  private
    FItems: array of T;

    function GetItem(AIndex: Integer): T;
    procedure SetItem(AIndex: Integer; AValue: T);

    function GetFirst: T;
    procedure SetFirst(const Value: T);

    function GetLast: T;
    procedure SetLast(const Value: T);

  protected
    function GetCapacity: Integer; override;
    procedure SetCapacity(const Value: Integer); override;

    procedure CopyTo(AArray: TArray); override;

  public
    // TODO: XmlDoc
    function Add(AItem: T): T; overload;
    // TODO: XmlDoc
    procedure Add(AItems: IIterable<T>); overload;
    // TODO: XmlDoc
    procedure Add(AItems: IEnumerable<T>); overload;
    // TODO: XmlDoc: Remarks: Insert can also add item at the end
    function Insert(AItem: T; AIndex: Integer): T;

    // TODO: XmlDoc
    procedure DelAt(AIndex: Integer); override;

    // TODO: XmlDoc
    procedure SwapUnchecked(A, B: Integer); override;

    {$REGION 'Find Functions'}

    // TODO: XmlDoc
    function FindFirstIndex(AFunc: TFindFuncStatic<T>): Integer; overload;
    // TODO: XmlDoc
    function FindFirstIndex(AFunc: TFindFuncRef<T>): Integer; overload;
    // TODO: XmlDoc
    function FindFirstIndex(AFunc: TFindFunc<T>): Integer; overload;

    // TODO: XmlDoc
    function FindFirst(AFunc: TFindFuncStatic<T>): T; overload;
    // TODO: XmlDoc
    function FindFirst(AFunc: TFindFuncRef<T>): T; overload;
    // TODO: XmlDoc
    function FindFirst(AFunc: TFindFunc<T>): T; overload;

    // TODO: XmlDoc
    function FindLastIndex(AFunc: TFindFuncStatic<T>): Integer; overload;
    // TODO: XmlDoc
    function FindLastIndex(AFunc: TFindFuncRef<T>): Integer; overload;
    // TODO: XmlDoc
    function FindLastIndex(AFunc: TFindFunc<T>): Integer; overload;

    // TODO: XmlDoc
    function FindLast(AFunc: TFindFuncStatic<T>): T; overload;
    // TODO: XmlDoc
    function FindLast(AFunc: TFindFuncRef<T>): T; overload;
    // TODO: XmlDoc
    function FindLast(AFunc: TFindFunc<T>): T; overload;

    // TODO: XmlDoc
    function FindIndexAsArray(AFunc: TFindFuncStatic<T>): TIntArray; overload;
    // TODO: XmlDoc
    function FindIndexAsArray(AFunc: TFindFuncRef<T>): TIntArray; overload;
    // TODO: XmlDoc
    function FindIndexAsArray(AFunc: TFindFunc<T>): TIntArray; overload;

    // TODO: XmlDoc
    function FindAsArray(AFunc: TFindFuncStatic<T>): TArray<T>; overload; virtual;
    // TODO: XmlDoc
    function FindAsArray(AFunc: TFindFuncRef<T>): TArray<T>; overload; virtual;
    // TODO: XmlDoc
    function FindAsArray(AFunc: TFindFunc<T>): TArray<T>; overload; virtual;

    {$ENDREGION} //

    {$REGION 'Sorting'}

    // TODO: XmlDoc
    procedure Sort(AFunc: TCompareFuncStatic<T>); overload; inline;
    // TODO: XmlDoc
    procedure Sort(AFunc: TCompareFuncRef<T>); overload; inline;
    // TODO: XmlDoc
    procedure Sort(AFunc: TCompareFunc<T>); overload; inline;

    // TODO: XmlDoc
    function TrySort(AFunc: TCompareFuncStatic<T>): Boolean; overload; inline;
    // TODO: XmlDoc
    function TrySort(AFunc: TCompareFuncRef<T>): Boolean; overload; inline;
    // TODO: XmlDoc
    function TrySort(AFunc: TCompareFunc<T>): Boolean; overload; inline;

    // TODO: XmlDoc
    function Sorted(AFunc: TCompareFuncStatic<T>): Boolean; overload;
    // TODO: XmlDoc
    function Sorted(AFunc: TCompareFuncRef<T>): Boolean; overload;
    // TODO: XmlDoc
    function Sorted(AFunc: TCompareFunc<T>): Boolean; overload;

    {$ENDREGION}

    // TODO: XmlDoc
    function BinarySearch(AItem: T; AFunc: TCompareFuncStatic<T>): Integer; overload;
    // TODO: XmlDoc
    function BinarySearch(AItem: T; AFunc: TCompareFuncRef<T>): Integer; overload;
    // TODO: XmlDoc
    function BinarySearch(AItem: T; AFunc: TCompareFunc<T>): Integer; overload;

    // TODO: XmlDoc
    function Copy: TArray<T>; reintroduce; inline;

    // TODO: XmlDoc
    property Items[I: Integer]: T read GetItem write SetItem; default;

    // TODO: XmlDoc
    property First: T read GetFirst write SetFirst;
    // TODO: XmlDoc
    property Last: T read GetLast write SetLast;

    // TODO: XmlDoc
    function DataPointer: Pointer; override;

    // TODO: XmlDoc
    function GetEnumerator: IIterator<T>;
    // TODO: XmlDoc
    function InReverse: TReverseWrapper; inline;

    // TODO: XmlDoc
    function ToString: string; override;
    // TODO: XmlDoc
    class function ItemToString(AItem: T): string; virtual;

  end;

  // TODO: XmlDoc
  TIntArray = class(TArray<Integer>)
  public
    // TODO: XmlDoc
    function FindAsIntArray(AFunc: TFindFuncStatic<Integer>): TIntArray; overload; inline;
    // TODO: XmlDoc
    function FindAsIntArray(AFunc: TFindFuncRef<Integer>): TIntArray; overload; inline;
    // TODO: XmlDoc
    function FindAsIntArray(AFunc: TFindFunc<Integer>): TIntArray; overload; inline;

    // TODO: XmlDoc
    function Copy: TIntArray; reintroduce; inline;

    // TODO: XmlDoc
    function Sum: Integer;
    // TODO: XmlDoc
    function Difference: Integer; inline;
    // TODO: XmlDoc
    function Average: Single; inline;
    // TODO: XmlDoc
    function Min: Integer;
    // TODO: XmlDoc
    function Max: Integer;
    // TODO: XmlDoc
    function Bounds: TIntBounds1;

    // TODO: XmlDoc
    class function ItemToString(AItem: Integer): string; override;

  end;

  // TODO: XmlDoc
  TSingleArray = class(TArray<Single>)
  public
    // TODO: XmlDoc
    function FindAsSingleArray(AFunc: TFindFuncStatic<Single>): TSingleArray; overload; inline;
    // TODO: XmlDoc
    function FindAsSingleArray(AFunc: TFindFuncRef<Single>): TSingleArray; overload; inline;
    // TODO: XmlDoc
    function FindAsSingleArray(AFunc: TFindFunc<Single>): TSingleArray; overload; inline;

    // TODO: XmlDoc
    function Copy: TSingleArray; reintroduce; inline;

    // TODO: XmlDoc
    function Sum: Single;
    // TODO: XmlDoc
    function Difference: Single; inline;
    // TODO: XmlDoc
    function Average: Single; inline;
    // TODO: XmlDoc
    function Min: Single;
    // TODO: XmlDoc
    function Max: Single;
    // TODO: XmlDoc
    function Bounds: TBounds1;

    // TODO: XmlDoc
    class function ItemToString(AItem: Single): string; override;

  end;

  // TODO: TIntVector2Array
  // TODO: TIntVector3Array
  // TODO: TVector2Array
  // TODO: TVector3Array

  // TODO: XmlDoc
  TStringArray = class(TArray<string>)
  public
    // TODO: XmlDoc
    function FindAsStringArray(AFunc: TFindFuncStatic<string>): TStringArray; overload; inline;
    // TODO: XmlDoc
    function FindAsStringArray(AFunc: TFindFuncRef<string>): TStringArray; overload; inline;
    // TODO: XmlDoc
    function FindAsStringArray(AFunc: TFindFunc<string>): TStringArray; overload; inline;

    // TODO: XmlDoc
    function Copy: TStringArray; reintroduce; inline;

    // TODO: XmlDoc
    class function ItemToString(AItem: string): string; override;

  end;

  // TODO: XmlDoc
  TAnsiStringArray = class(TArray<AnsiString>)
  public
    // TODO: XmlDoc
    function FindAsAnsiStringArray(AFunc: TFindFuncStatic<AnsiString>): TAnsiStringArray; overload; inline;
    // TODO: XmlDoc
    function FindAsAnsiStringArray(AFunc: TFindFuncRef<AnsiString>): TAnsiStringArray; overload; inline;
    // TODO: XmlDoc
    function FindAsAnsiStringArray(AFunc: TFindFunc<AnsiString>): TAnsiStringArray; overload; inline;

    // TODO: XmlDoc
    function Copy: TAnsiStringArray; reintroduce; inline;

    // TODO: XmlDoc
    class function ItemToString(AItem: AnsiString): string; override;

  end;

  // TODO: XmlDoc
  TRefArray<T: class> = class(TArray<T>)
  public
    // TODO: XmlDoc
    function FindAsRefArray(AFunc: TFindFuncStatic<T>): TRefArray<T>; overload; inline;
    // TODO: XmlDoc
    function FindAsRefArray(AFunc: TFindFuncRef<T>): TRefArray<T>; overload; inline;
    // TODO: XmlDoc
    function FindAsRefArray(AFunc: TFindFunc<T>): TRefArray<T>; overload; inline;

    // TODO: XmlDoc
    function Copy: TRefArray<T>; reintroduce; inline;

    // TODO: XmlDoc
    function Find(AItem: T): Integer; inline;
    // TODO: XmlDoc
    procedure Del(AItem: T);

    // TODO: XmlDoc
    class function ItemToString(AItem: T): string; override;

  end;

  // TODO: XmlDoc
  TObjectArray<T: class> = class(TRefArray<T>)
  protected
    function ShouldFreeItems: Boolean; override;
    procedure ItemRemoved(AIndex: Integer); override;
  public
    function CreateCopy: TArray; override;
  end;

  // TODO: XmlDoc
  TObjectPairArray<K: class; V> = class(TArray<TPair<K, V>>)
  protected
    function ShouldFreeItems: Boolean; override;
    procedure ItemRemoved(AIndex: Integer); override;
  public
    function Copy: TObjectPairArray<K, V>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TToObjectPairArray<K; V: class> = class(TArray<TPair<K, V>>)
  protected
    function ShouldFreeItems: Boolean; override;
    procedure ItemRemoved(AIndex: Integer); override;
  public
    function Copy: TToObjectPairArray<K, V>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TObjectObjectPairArray<K, V: class> = class(TArray<TPair<K, V>>)
  protected
    function ShouldFreeItems: Boolean; override;
    procedure ItemRemoved(AIndex: Integer); override;
  public
    function Copy: TObjectObjectPairArray<K, V>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TInterfaceArray<T: IUnknown> = class(TArray<T>)
  public
    // TODO: XmlDoc
    function FindAsInterfaceArray(AFunc: TFindFuncStatic<T>): TInterfaceArray<T>; overload; inline;
    // TODO: XmlDoc
    function FindAsInterfaceArray(AFunc: TFindFuncRef<T>): TInterfaceArray<T>; overload; inline;
    // TODO: XmlDoc
    function FindAsInterfaceArray(AFunc: TFindFunc<T>): TInterfaceArray<T>; overload; inline;

    // TODO: XmlDoc
    function Copy: TInterfaceArray<T>; reintroduce; inline;

    // TODO: XmlDoc
    function Find(AItem: T): Integer; overload; inline;
    // TODO: XmlDoc
    function Find(AItem: TObject): Integer; overload; inline;
    // TODO: XmlDoc
    procedure Del(AItem: T); overload;
    // TODO: XmlDoc
    procedure Del(AItem: TObject); overload;

    // TODO: XmlDoc
    class function ItemToString(AItem: T): string; override;

  end;

  // TODO: XmlDoc
  TStack = class
  private
    FArray: IArray;

    function GetCapacity: Integer; inline;
    procedure SetCapacity(const Value: Integer); inline;

    function GetGrowAmount: Integer; inline;
    procedure SetGrowAmount(const Value: Integer); inline;

    function GetShrinkRetain: Integer; inline;
    procedure SetShrinkRetain(const Value: Integer); inline;

  protected
    function CreateSame(AGrowAmount: Integer = 16; AShrinkRetain: Integer = 8): TStack;

    // TODO: XmlDoc
    function CreateArray(AGrowAmount: Integer = 16; AShrinkRetain: Integer = 8): TArray; virtual; abstract;

    function CreateCopy: TStack; virtual;

    constructor CreateNoArray; virtual;

  public
    constructor Create(AGrowAmount: Integer = 16; AShrinkRetain: Integer = 8); virtual;

    // TODO: XmlDoc
    procedure Clear; inline;

    // TODO: XmlDoc
    property Capacity: Integer read GetCapacity write SetCapacity;
    // TODO: XmlDoc
    function Count: Integer;
    // TODO: XmlDoc
    property GrowAmount: Integer read GetGrowAmount write SetGrowAmount;
    // TODO: XmlDoc
    property ShrinkRetain: Integer read GetShrinkRetain write SetShrinkRetain;

    // TODO: XmlDoc
    function Copy: TStack;

  end;

  TStackClass = class of TStack;

  // TODO: XmlDoc
  TStack<T> = class(TStack)
  private
    FArray: TArray<T>;

    function GetTop: T; inline;
    procedure SetTop(const Value: T); inline;

  protected
    function CreateArray(AGrowAmount, AShrinkRetain: Integer): TArray; override;

  public
    // TODO: XmlDoc
    property Top: T read GetTop write SetTop;
    // TODO: XmlDoc
    procedure Push(AItem: T); inline;
    // TODO: XmlDoc
    function Pop: T; inline;

    // TODO: XmlDoc
    function Copy: TStack<T>; reintroduce; inline;

  end;

  // TODO: TQueue<T> using a linked list

  // TODO: XmlDoc
  TObjectStack<T: class> = class(TStack<T>)
  protected
    // TODO: XmlDoc
    function CreateArray(AGrowAmount, AShrinkRetain: Integer): TArray; override;

  public
    // TODO: XmlDoc
    function Copy: TObjectStack<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  THashBase = class(TInterfacedObject)
  public const

    HashPrimeOffset = 6;

    HashPrimes: array [0 .. 25] of Integer = (
      53, 97, 193, 389, 769, 1543, 3079, 6151, 12289, 24593, 49157, 98317, 196613, 393241, 786433, 1572869, 3145739,
      6291469, 12582917, 25165843, 50331653, 100663319, 201326611, 402653189, 805306457, 1610612741);

  public type

    // TODO: XmlDoc
    TBucket = TArray;
    // TODO: XmlDoc
    TBuckets = array of TBucket;

    // TODO: XmlDoc
    TIterator<T> = class abstract(TInterfacedObject)
    private
      FIndex: Integer;
      FBucketIndex: Integer;

    protected
      function GetBuckets: Integer; virtual; abstract;
      function GetBucketSize(AIndex: Integer): Integer; virtual; abstract;

      property Index: Integer read FIndex;
      property BucketIndex: Integer read FBucketIndex;

    public
      constructor Create;

      function MoveNext: Boolean;
      function GetCurrent: T; virtual; abstract;

    end;

  private
    FAutoRehash: Boolean;
    FBuckets: TBuckets;

    procedure SetAutoRehash(const Value: Boolean);

  protected
    FCount: Integer;

    function GetBuckets: Integer; virtual; abstract;
    procedure SetBuckets(const Value: Integer); virtual; abstract;
    procedure SetBucketsDirect(Value: Integer); virtual; abstract;

    procedure ClearBuckets; virtual; abstract;
    // TODO: XmlDoc not only copy the reference, but also nil the other reference
    procedure OwnBuckets(AFrom: THashBase); virtual; abstract;
    procedure CopyBuckets(AFrom: THashBase); virtual; abstract;

    // TODO: XmlDoc
    function CreateSame(AAutoRehash: Boolean = True): THashBase; inline;

    procedure CopyTo(AHashBase: THashBase);

    function CreateBucket: TBucket; virtual; abstract;

    function CreateCopy(AAutoRehash: Boolean = True): THashBase; virtual;

  public
    constructor Create(AAutoRehash: Boolean = True); virtual;
    destructor Destroy; override;

    class function GetHashBuckets(ACount: Integer): Integer; static;

    property Buckets: Integer read GetBuckets write SetBuckets;
    property AutoRehash: Boolean read FAutoRehash write SetAutoRehash;

    function Count: Integer;
    function CountOptimized: Boolean;

    procedure Rehash(ABuckets: Integer);

    procedure Clear;
    function Empty: Boolean; inline;

    function Copy(AAutoRehash: Boolean = True): THashBase;

  end;

  THashBaseClass = class of THashBase;

  // TODO: XmlDoc
  THashBase<K> = class abstract(THashBase)
  protected
    class function GetHash(AKey: K): Cardinal; virtual; abstract;

    class function CanIndex(AKey: K): Boolean; virtual;
    class function KeysEqual(AKey1, AKey2: K): Boolean; virtual; abstract;

  public
    // TODO: XmlDoc
    function GetCappedHash(AKey: K): Integer;

    function Copy(AAutoRehash: Boolean = True): THashBase<K>; reintroduce; inline;

  end;

  // TODO: XmlDoc
  TSet<T> = class abstract(THashBase<T>, IIterable<T>)
  public type

    // TODO: XmlDoc
    TBucket = TArray<T>;

    // TODO: XmlDoc
    TIterator = class(THashBase.TIterator<T>, IIterator<T>)
    private
      FSet: TSet<T>;

    protected
      function GetBuckets: Integer; override;
      function GetBucketSize(AIndex: Integer): Integer; override;

    public
      constructor Create(ASet: TSet<T>);

      function GetCurrent: T; override;

    end;

  private
    function GetElement(AElement: T): Boolean;
    procedure SetElement(AElement: T; const Value: Boolean);

  protected
    function GetBuckets: Integer; override;
    procedure SetBuckets(const Value: Integer); override;

    function CreateBucket: THashBase.TBucket; override;

    procedure SetBucketsDirect(Value: Integer); override;

    procedure ClearBuckets; override;
    procedure OwnBuckets(AFrom: THashBase); override;
    procedure CopyBuckets(AFrom: THashBase); override;

  public
    property Elements[AElement: T]: Boolean read GetElement write SetElement; default;

    function TryAdd(AElement: T): Boolean;
    procedure Add(AElement: T); overload;
    procedure Add(ASet: TSet<T>); overload;

    function TryDel(AElement: T): Boolean;
    procedure Del(AElement: T); overload;
    procedure Del(ASet: TSet<T>); overload;

    function GetEnumerator: IIterator<T>;

    function Copy(AAutoRehash: Boolean = True): TSet<T>; reintroduce; inline;

    function Union(ASet: TSet<T>): TSet<T>;
    function Intersection(ASet: TSet<T>): TSet<T>;
    function Remove(ASet: TSet<T>): TSet<T>;

    function IsSupersetOf(ASet: TSet<T>): Boolean;
    function IsSubsetOf(ASet: TSet<T>): Boolean;
    function Equals(ASet: TSet<T>): Boolean; reintroduce;

  end;

  // TODO: XmlDoc
  TMap<K, V> = class abstract(THashBase<K>, IIterable<TPair<K, V>>)
  public type
    // TODO: XmlDoc
    TPair = TPair<K, V>;
    // TODO: XmlDoc
    TBucket = TArray<TPair>;

    // TODO: XmlDoc
    TIterator = class(THashBase.TIterator<TPair>, IIterator<TPair>)
    private
      FMap: TMap<K, V>;

    protected
      function GetBuckets: Integer; override;
      function GetBucketSize(AIndex: Integer): Integer; override;

    public
      constructor Create(AMap: TMap<K, V>);

      function GetCurrent: TPair; override;

    end;

  private
    function GetItem(AKey: K): V;
    procedure SetItem(AKey: K; const Value: V);

  protected
    function GetBuckets: Integer; override;
    procedure SetBuckets(const Value: Integer); override;

    function CreateBucket: THashBase.TBucket; override;

    procedure SetBucketsDirect(Value: Integer); override;

    procedure ClearBuckets; override;
    procedure OwnBuckets(AFrom: THashBase); override;
    procedure CopyBuckets(AFrom: THashBase); override;

  public
    function Get(AKey: K; out AValue: V): Boolean;
    property Items[AKey: K]: V read GetItem write SetItem; default;

    function TryDel(AKey: K): Boolean;
    procedure Del(AKey: K);

    function GetEnumerator: IIterator<TPair>;

    function BucketCounts: TIntArray;

    function KeySet(AAutoRehash: Boolean = False): TSet<K>;

    function Copy(AAutoRehash: Boolean = True): TMap<K, V>; reintroduce; inline;

  end;

  // TODO: XmlDoc
  TValueHasher<K> = class abstract
  public
    class function GetHash(AKey: K): Cardinal; virtual; abstract;
    class function KeysEqual(AKey1, AKey2: K): Boolean; virtual; abstract;
    class function CanIndex(AKey: K): Boolean; virtual;
  end;

  // TODO: XmlDoc
  TValueMap<K, V; H: TValueHasher<K>> = class(TMap<K, V>)
  protected
    class function GetHash(AKey: K): Cardinal; override;
    class function KeysEqual(AKey1, AKey2: K): Boolean; override;
    class function CanIndex(AKey: K): Boolean; override;
  public
    function Copy(AAutoRehash: Boolean = True): TValueMap<K, V, H>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TValueSet<T; H: TValueHasher<T>> = class(TSet<T>)
  protected
    class function GetHash(AKey: T): Cardinal; override;
    class function KeysEqual(AKey1, AKey2: T): Boolean; override;
    class function CanIndex(AKey: T): Boolean; override;
  public
    function Copy(AAutoRehash: Boolean = True): TValueSet<T, H>; reintroduce; inline;
  end;

implementation

{ EEmptyGenericArray }

constructor EArrayEmpty.Create;
begin
  inherited Create('The operation requires the array to contain at least one item.');
end;

{ EArrayItemNotFound }

constructor EArrayItemNotFound.Create;
begin
  inherited Create('The item could not be found in the array.');
end;

{ EArrayRangeError }

constructor EArrayRangeError.Create(AIndex, ACount: Integer);
begin
  inherited CreateFmt('The array-index "%d" is out of bounds, as the collection only has %d items.',
    [AIndex, ACount]);
end;

{ EArrayItemNoStringRepresentative }

constructor EArrayItemNoStringRepresentative.Create;
begin
  inherited Create('Items in array do not have a string representative.');
end;

{ EArrayNegativeCapacity }

constructor EArrayNegativeCapacity.Create;
begin
  inherited Create('Capacity cannot be negative.');
end;

{ EArrayInvalidGrowAmount }

constructor EArrayInvalidGrowAmount.Create;
begin
  inherited Create('The array grow amount must be greater than zero.');
end;

{ EArrayInvalidShrinkRetain }

constructor EArrayInvalidShrinkRetain.Create;
begin
  inherited Create('The array shrink retain must be at least zero.');
end;

{ ETooManyHashBuckets }

constructor ETooManyHashBuckets.Create;
begin
  inherited Create('Hash Buckets cannot exceed 1610612741.');
end;

{ EInvalidHashBucketCount }

constructor EInvalidHashBucketCount.Create;
begin
  inherited Create('At least zero hash buckets are required.');
end;

{ EHashBucketRequired }

constructor EHashBucketRequired.Create;
begin
  inherited Create('At least one hash bucket is required, as the collection contains at least one element.');
end;

{ EMapKeyNotFound }

constructor EMapKeyNotFound.Create;
begin
  inherited Create('The map does not contain the specified key.');
end;

{ ESetKeyNotFound }

constructor ESetKeyNotFound.Create;
begin
  inherited Create('The map does not contain the specified key.');
end;

{ ESetKeyExistsAlready }

constructor ESetKeyExistsAlready.Create;
begin
  inherited Create('The set key exists already.');
end;

{ EHashEmpty }

constructor EHashEmpty.Create;
begin
  inherited Create('The operation requires the hash collection to have at least one item.');
end;

{ EHashKeyNotIndexable }

constructor EHashKeyNotIndexable.Create;
begin
  inherited Create('Hash key is not indexable.');
end;

{ TIterable<T> }

function TIterable<T>.Count: Integer;
var
  Item: T;
begin
  Result := 0;
  for Item in Self do
    Inc(Result);
end;

function TIterable<T>.CountOptimized: Boolean;
begin
  Result := False;
end;

{ TPair<K, V> }

constructor TPair<K, V>.Create(AKey: K; AValue: V);
begin
  FKey := AKey;
  FValue := AValue;
end;

{ TArray }

function TArray.CreateCopy: TArray;
begin
  Result := CreateSame(GrowAmount, ShrinkRetain);
  CopyTo(Result);
end;

function TArray.Copy: IArray;
begin
  Result := CreateCopy;
end;

function TArray.Count: Integer;
begin
  Result := FCount;
end;

function TArray.CountOptimized: Boolean;
begin
  Result := True;
end;

constructor TArray.Create(AGrowAmount, AShrinkRetain: Integer);
begin
  GrowAmount := AGrowAmount;
  ShrinkRetain := AShrinkRetain;
end;

function TArray.CreateSame(AGrowAmount, AShrinkRetain: Integer): TArray;
begin
  Result := TArrayClass(ClassType).Create(AGrowAmount, AShrinkRetain);
end;

destructor TArray.Destroy;
begin
  if ShouldFreeItems then
    Clear;
  inherited;
end;

function TArray.Empty: Boolean;
begin
  Result := Count = 0;
end;

function TArray.GetGrowAmount: Integer;
begin
  Result := FGrowAmount;
end;

function TArray.GetShrinkRetain: Integer;
begin
  Result := FShrinkRetain;
end;

procedure TArray.DelLast;
begin
  DelAt(MaxIndex);
end;

procedure TArray.Clear;
var
  I: Integer;
begin
  if ShouldFreeItems then
    for I := 0 to MaxIndex do
      ItemRemoved(I);
  Capacity := 0;
end;

function TArray.MaxIndex: Integer;
begin
  Result := FCount - 1;
end;

function TArray.RangeCheck(AIndex: Integer): Boolean;
begin
  Result := AIndex in IBounds1(Count);
end;

procedure TArray.Swap(A, B: Integer);
begin
  RangeCheckException(A);
  RangeCheckException(B);
  SwapUnchecked(A, B);
end;

procedure TArray.SwapUnchecked(A, B: Integer);
begin

end;

procedure TArray.RangeCheckException(AIndex: Integer);
begin
  if not RangeCheck(AIndex) then
    raise EArrayRangeError.Create(AIndex, Count);
end;

procedure TArray.SetGrowAmount(const Value: Integer);
begin
  if Value < 0 then
    raise EArrayInvalidGrowAmount.Create;
  FGrowAmount := Value;
end;

procedure TArray.SetShrinkRetain(const Value: Integer);
begin
  if Value < 0 then
    raise EArrayInvalidShrinkRetain.Create;
  FShrinkRetain := Value;
end;

function TArray.ShouldFreeItems: Boolean;
begin
  Result := False;
end;

procedure TArray.ItemRemoved(AIndex: Integer);
begin
  // only called if ShouldFreeItems returns True
end;

{ TArray<T>.TIterator }

constructor TArray<T>.TIterator.Create(AList: TArray<T>; AReversed: Boolean);
begin
  FList := AList;
  FReversed := AReversed;
  if FReversed then
    FCurrent := FList.Count
  else
    FCurrent := -1;
end;

function TArray<T>.TIterator.GetCurrent: T;
begin
  Result := FList[FCurrent];
end;

function TArray<T>.TIterator.MoveNext: Boolean;
begin
  if FRemoveFlag then
  begin
    FList.DelAt(FCurrent);
    FRemoveFlag := False;
  end
  else if not FReversed then
    Inc(FCurrent);

  if FReversed then
  begin
    Dec(FCurrent);
    Result := FCurrent <> -1;
  end
  else
    Result := FCurrent <> FList.Count;
end;

procedure TArray<T>.TIterator.RemoveCurrent;
begin
  FRemoveFlag := True;
end;

{ TArray<T>.TReverseWrapper }

function TArray<T>.TReverseWrapper.Copy: TArray<T>;
var
  Item: T;
begin
  Result := TArray<T>(FArray.CreateSame(FArray.GrowAmount, FArray.ShrinkRetain));
  for Item in Self do
    Result.Add(Item);
end;

constructor TArray<T>.TReverseWrapper.Create(AArray: TArray<T>);
begin
  FArray := AArray;
end;

function TArray<T>.TReverseWrapper.GetEnumerator(AAutoFree: Boolean): TIterator;
begin
  Result := TArray<T>.TIterator.Create(FArray, True);
end;

{ TArray<T>.TSorterBase }

function TArray<T>.TSorterBase.Bounds: TIntBounds1;
begin
  Result := IBounds1(FArray.Count);
end;

procedure TArray<T>.TSorterBase.SavePivot(I: Integer);
begin
  FPivot := FArray[I];
end;

procedure TArray<T>.TSorterBase.Swap(A, B: Integer);
begin
  FArray.Swap(A, B);
end;

constructor TArray<T>.TSorterBase.Create(AArray: TArray<T>);
begin
  FArray := AArray;
end;

{ TArray<T>.TSorterStatic }

function TArray<T>.TSorterStatic.AfterPivot(I: Integer): Boolean;
begin
  Result := FFunc(FPivot, FArray[I]);
end;

function TArray<T>.TSorterStatic.BeforePivot(I: Integer): Boolean;
begin
  Result := FFunc(FArray[I], FPivot);
end;

constructor TArray<T>.TSorterStatic.Create(AArray: TArray<T>; AFunc: TCompareFuncStatic<T>);
begin
  inherited Create(AArray);
  FFunc := AFunc;
end;

{ TArray<T>.TSorterRef }

function TArray<T>.TSorterRef.AfterPivot(I: Integer): Boolean;
begin
  Result := FFunc(FPivot, FArray[I]);
end;

function TArray<T>.TSorterRef.BeforePivot(I: Integer): Boolean;
begin
  Result := FFunc(FArray[I], FPivot);
end;

constructor TArray<T>.TSorterRef.Create(AArray: TArray<T>; AFunc: TCompareFuncRef<T>);
begin
  inherited Create(AArray);
  FFunc := AFunc;
end;

{ TArray<T>.TSorter }

function TArray<T>.TSorter.AfterPivot(I: Integer): Boolean;
begin
  Result := FFunc(FPivot, FArray[I]);
end;

function TArray<T>.TSorter.BeforePivot(I: Integer): Boolean;
begin
  Result := FFunc(FArray[I], FPivot);
end;

constructor TArray<T>.TSorter.Create(AArray: TArray<T>; AFunc: TCompareFunc<T>);
begin
  inherited Create(AArray);
  FFunc := AFunc;
end;

{ TArray<T> }

function TArray<T>.GetCapacity: Integer;
begin
  Result := Length(FItems);
end;

procedure TArray<T>.SetCapacity(const Value: Integer);
begin
  if Value < 0 then
    raise EArrayNegativeCapacity.Create;
  SetLength(FItems, Value);
  FCount := Min(Count, Capacity);
end;

function TArray<T>.GetItem(AIndex: Integer): T;
begin
  RangeCheckException(AIndex);
  Result := FItems[AIndex];
end;

procedure TArray<T>.SetItem(AIndex: Integer; AValue: T);
begin
  RangeCheckException(AIndex);
  FItems[AIndex] := AValue;
end;

function TArray<T>.GetFirst: T;
begin
  if Empty then
    raise EArrayEmpty.Create;
  Result := FItems[0];
end;

procedure TArray<T>.SetFirst(const Value: T);
begin
  if Empty then
    raise EArrayEmpty.Create;
  FItems[0] := Value;
end;

function TArray<T>.GetLast: T;
begin
  if Empty then
    raise EArrayEmpty.Create;
  Result := FItems[MaxIndex];
end;

procedure TArray<T>.SetLast(const Value: T);
begin
  if Empty then
    raise EArrayEmpty.Create;
  FItems[MaxIndex] := Value;
end;

function TArray<T>.Add(AItem: T): T;
begin
  if Count + 1 > Capacity then
    Capacity := Capacity + FGrowAmount;
  FItems[FCount] := AItem;
  Inc(FCount);
  Result := AItem;
end;

function TArray<T>.Insert(AItem: T; AIndex: Integer): T;
begin
  if AIndex = Count then
  begin
    Add(AItem);
    Exit;
  end;
  RangeCheckException(AIndex);
  if Count + 1 > Capacity then
    Capacity := Capacity + FGrowAmount;
  Move(FItems[AIndex], FItems[AIndex + 1], SizeOf(T) * (Count - AIndex));
  FItems[AIndex] := AItem;
  Inc(FCount);
  Result := AItem;
end;

procedure TArray<T>.DelAt(AIndex: Integer);
begin
  RangeCheckException(AIndex);
  if ShouldFreeItems then
    ItemRemoved(AIndex);
  if Count - AIndex > 1 then
    Move(FItems[AIndex + 1], FItems[AIndex], SizeOf(T) * (Count - AIndex - 1));
  Dec(FCount);
  if Count <= Capacity - FGrowAmount - ShrinkRetain then
    Capacity := Capacity - FGrowAmount;
end;

procedure TArray<T>.Add(AItems: IEnumerable<T>);
var
  Item: T;
begin
  for Item in AItems do
    Add(Item);
end;

procedure TArray<T>.SwapUnchecked(A, B: Integer);
var
  Tmp: T;
begin
  Tmp := FItems[A];
  FItems[A] := FItems[B];
  FItems[B] := Tmp;
end;

function TArray<T>.FindFirstIndex(AFunc: TFindFuncStatic<T>): Integer;
var
  I: Integer;
begin
  for I := 0 to MaxIndex do
    if AFunc(FItems[I]) then
      Exit(I);
  Result := -1;
end;

function TArray<T>.FindFirstIndex(AFunc: TFindFuncRef<T>): Integer;
var
  I: Integer;
begin
  for I := 0 to MaxIndex do
    if AFunc(FItems[I]) then
      Exit(I);
  Result := -1;
end;

function TArray<T>.FindFirstIndex(AFunc: TFindFunc<T>): Integer;
var
  I: Integer;
begin
  for I := 0 to MaxIndex do
    if AFunc(FItems[I]) then
      Exit(I);
  Result := -1;
end;

function TArray<T>.FindFirst(AFunc: TFindFuncStatic<T>): T;
var
  I: Integer;
begin
  I := FindFirstIndex(AFunc);
  if I = -1 then
    raise EArrayItemNotFound.Create;
  Result := FItems[I];
end;

function TArray<T>.FindFirst(AFunc: TFindFuncRef<T>): T;
var
  I: Integer;
begin
  I := FindFirstIndex(AFunc);
  if I = -1 then
    raise EArrayItemNotFound.Create;
  Result := FItems[I];
end;

function TArray<T>.FindFirst(AFunc: TFindFunc<T>): T;
var
  I: Integer;
begin
  I := FindFirstIndex(AFunc);
  if I = -1 then
    raise EArrayItemNotFound.Create;
  Result := FItems[I];
end;

function TArray<T>.FindLastIndex(AFunc: TFindFuncStatic<T>): Integer;
var
  I: Integer;
begin
  for I := MaxIndex downto 0 do
    if AFunc(FItems[I]) then
      Exit(I);
  Result := -1;
end;

function TArray<T>.FindLastIndex(AFunc: TFindFuncRef<T>): Integer;
var
  I: Integer;
begin
  for I := MaxIndex downto 0 do
    if AFunc(FItems[I]) then
      Exit(I);
  Result := -1;
end;

function TArray<T>.FindLastIndex(AFunc: TFindFunc<T>): Integer;
var
  I: Integer;
begin
  for I := MaxIndex downto 0 do
    if AFunc(FItems[I]) then
      Exit(I);
  Result := -1;
end;

function TArray<T>.FindLast(AFunc: TFindFuncStatic<T>): T;
var
  I: Integer;
begin
  I := FindLastIndex(AFunc);
  if I = -1 then
    raise EArrayItemNotFound.Create;
  Result := FItems[I];
end;

function TArray<T>.FindLast(AFunc: TFindFuncRef<T>): T;
var
  I: Integer;
begin
  I := FindLastIndex(AFunc);
  if I = -1 then
    raise EArrayItemNotFound.Create;
  Result := FItems[I];
end;

function TArray<T>.FindLast(AFunc: TFindFunc<T>): T;
var
  I: Integer;
begin
  I := FindLastIndex(AFunc);
  if I = -1 then
    raise EArrayItemNotFound.Create;
  Result := FItems[I];
end;

function TArray<T>.FindIndexAsArray(AFunc: TFindFuncStatic<T>): TIntArray;
var
  I: Integer;
begin
  Result := TIntArray.Create;
  for I := 0 to MaxIndex do
    if AFunc(FItems[I]) then
      Result.Add(I);
end;

function TArray<T>.FindIndexAsArray(AFunc: TFindFuncRef<T>): TIntArray;
var
  I: Integer;
begin
  Result := TIntArray.Create;
  for I := 0 to MaxIndex do
    if AFunc(FItems[I]) then
      Result.Add(I);
end;

function TArray<T>.FindIndexAsArray(AFunc: TFindFunc<T>): TIntArray;
var
  I: Integer;
begin
  Result := TIntArray.Create;
  for I := 0 to MaxIndex do
    if AFunc(FItems[I]) then
      Result.Add(I);
end;

function TArray<T>.FindAsArray(AFunc: TFindFuncStatic<T>): TArray<T>;
var
  I: Integer;
begin
  Result := TArray<T>(TArrayClass(ClassType).Create);
  for I := 0 to MaxIndex do
    if AFunc(FItems[I]) then
      Result.Add(FItems[I]);
end;

function TArray<T>.FindAsArray(AFunc: TFindFuncRef<T>): TArray<T>;
var
  I: Integer;
begin
  Result := TArray<T>(TArrayClass(ClassType).Create);
  for I := 0 to MaxIndex do
    if AFunc(FItems[I]) then
      Result.Add(FItems[I]);
end;

function TArray<T>.FindAsArray(AFunc: TFindFunc<T>): TArray<T>;
var
  I: Integer;
begin
  Result := TArray<T>(TArrayClass(ClassType).Create);
  for I := 0 to MaxIndex do
    if AFunc(FItems[I]) then
      Result.Add(FItems[I]);
end;

procedure TArray<T>.Sort(AFunc: TCompareFuncStatic<T>);
begin
  TSorterStatic.Create(Self, AFunc).Sort;
end;

procedure TArray<T>.Sort(AFunc: TCompareFuncRef<T>);
begin
  TSorterRef.Create(Self, AFunc).Sort;
end;

procedure TArray<T>.Sort(AFunc: TCompareFunc<T>);
begin
  TSorter.Create(Self, AFunc).Sort;
end;

function TArray<T>.TrySort(AFunc: TCompareFuncStatic<T>): Boolean;
begin
  Result := TSorterStatic.Create(Self, AFunc).TrySort;
end;

function TArray<T>.TrySort(AFunc: TCompareFuncRef<T>): Boolean;
begin
  Result := TSorterRef.Create(Self, AFunc).TrySort;
end;

function TArray<T>.TrySort(AFunc: TCompareFunc<T>): Boolean;
begin
  Result := TSorter.Create(Self, AFunc).TrySort;
end;

function TArray<T>.Sorted(AFunc: TCompareFuncStatic<T>): Boolean;
var
  I: Integer;
begin
  for I := 1 to MaxIndex do
    if AFunc(Self[I], Self[I - 1]) then
      Exit(False);
  Result := True;
end;

function TArray<T>.Sorted(AFunc: TCompareFuncRef<T>): Boolean;
var
  I: Integer;
begin
  for I := 1 to MaxIndex do
    if AFunc(Self[I], Self[I - 1]) then
      Exit(False);
  Result := True;
end;

function TArray<T>.Sorted(AFunc: TCompareFunc<T>): Boolean;
var
  I: Integer;
begin
  for I := 1 to MaxIndex do
    if AFunc(Self[I], Self[I - 1]) then
      Exit(False);
  Result := True;
end;

function TArray<T>.Copy: TArray<T>;
begin
  Result := TArray<T>(CreateCopy);
end;

procedure TArray<T>.CopyTo(AArray: TArray);
begin
  AArray.Capacity := Capacity;
  Move(FItems[0], TArray<T>(AArray).FItems[0], SizeOf(T) * Count);
end;

function TArray<T>.DataPointer: Pointer;
begin
  Result := FItems;
end;

function TArray<T>.GetEnumerator: IIterator<T>;
begin
  Result := TIterator.Create(Self, False);
end;

function TArray<T>.InReverse: TReverseWrapper;
begin
  Result := TReverseWrapper.Create(Self);
end;

procedure TArray<T>.Add(AItems: IIterable<T>);
var
  Item: T;
begin
  AItems.Count;
  for Item in AItems do
    Add(Item);
end;

function TArray<T>.ToString: string;
var
  I: Integer;
begin
  Result := '[';
  if not Empty then
    Result := Result + ItemToString(Self[0]);
  for I := 1 to MaxIndex do
    Result := Result + ', ' + ItemToString(Self[I]);
  Result := Result + ']';
end;

class function TArray<T>.ItemToString(AItem: T): string;
begin
  raise EArrayItemNoStringRepresentative.Create;
end;

function TArray<T>.BinarySearch(AItem: T; AFunc: TCompareFuncStatic<T>): Integer;
begin
  raise ENotImplemented.Create('BinarySearch not implemented.');
end;

function TArray<T>.BinarySearch(AItem: T; AFunc: TCompareFunc<T>): Integer;
begin
  raise ENotImplemented.Create('BinarySearch not implemented.');
end;

function TArray<T>.BinarySearch(AItem: T; AFunc: TCompareFuncRef<T>): Integer;
begin
  raise ENotImplemented.Create('BinarySearch not implemented.');
end;

{ TIntArray }

function TIntArray.FindAsIntArray(AFunc: TFindFuncStatic<Integer>): TIntArray;
begin
  Result := TIntArray(FindAsArray(AFunc));
end;

function TIntArray.FindAsIntArray(AFunc: TFindFuncRef<Integer>): TIntArray;
begin
  Result := TIntArray(FindAsArray(AFunc));
end;

function TIntArray.FindAsIntArray(AFunc: TFindFunc<Integer>): TIntArray;
begin
  Result := TIntArray(FindAsArray(AFunc));
end;

function TIntArray.Sum: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I in Self do
    Result := Result + I;
end;

function TIntArray.Difference: Integer;
begin
  Result := Bounds.Length;
end;

function TIntArray.Average: Single;
begin
  Result := Sum / Count;
end;

function TIntArray.Min: Integer;
var
  I: Integer;
begin
  if Empty then
    raise EArrayEmpty.Create;
  Result := Self[0];
  for I := 1 to MaxIndex do
    Result := System.Math.Min(Result, Self[I]);
end;

function TIntArray.Max: Integer;
var
  I: Integer;
begin
  if Empty then
    raise EArrayEmpty.Create;
  Result := Self[0];
  for I := 1 to MaxIndex do
    Result := System.Math.Max(Result, Self[I]);
end;

function TIntArray.Bounds: TIntBounds1;
var
  I: Integer;
begin
  if Empty then
    raise EArrayEmpty.Create;
  Result := Self[0];
  for I := 0 to MaxIndex do
  begin
    if Self[I] < Result.Low then
      Result.Low := Self[I]
    else if Self[I] > Result.High then
      Result.High := Self[I];
  end;
end;

function TIntArray.Copy: TIntArray;
begin
  Result := TIntArray(CreateCopy);
end;

class function TIntArray.ItemToString(AItem: Integer): string;
begin
  Result := AItem.ToString;
end;

{ TSingleArray }

function TSingleArray.FindAsSingleArray(AFunc: TFindFuncStatic<Single>): TSingleArray;
begin
  Result := TSingleArray(FindAsArray(AFunc));
end;

function TSingleArray.FindAsSingleArray(AFunc: TFindFuncRef<Single>): TSingleArray;
begin
  Result := TSingleArray(FindAsArray(AFunc));
end;

function TSingleArray.FindAsSingleArray(AFunc: TFindFunc<Single>): TSingleArray;
begin
  Result := TSingleArray(FindAsArray(AFunc));
end;

function TSingleArray.Copy: TSingleArray;
begin
  Result := TSingleArray(CreateCopy);
end;

function TSingleArray.Sum: Single;
var
  I: Single;
begin
  Result := 0;
  for I in Self do
    Result := Result + I;
end;

function TSingleArray.Difference: Single;
begin
  Result := Bounds.Length;
end;

function TSingleArray.Average: Single;
begin
  Result := Sum / Count;
end;

function TSingleArray.Min: Single;
var
  I: Integer;
begin
  if Empty then
    raise EArrayEmpty.Create;
  Result := Self[0];
  for I := 1 to MaxIndex do
    Result := System.Math.Min(Result, Self[I]);
end;

function TSingleArray.Max: Single;
var
  I: Integer;
begin
  if Empty then
    raise EArrayEmpty.Create;
  Result := Self[0];
  for I := 1 to MaxIndex do
    Result := System.Math.Max(Result, Self[I]);
end;

function TSingleArray.Bounds: TBounds1;
var
  I: Integer;
begin
  if Empty then
    raise EArrayEmpty.Create;
  Result := Self[0];
  for I := 0 to MaxIndex do
  begin
    if Self[I] < Result.Low then
      Result.Low := Self[I]
    else if Self[I] > Result.High then
      Result.High := Self[I];
  end;
end;

class function TSingleArray.ItemToString(AItem: Single): string;
begin
  Result := Format('%f', [AItem]);
end;

{ TStringArray }

function TStringArray.FindAsStringArray(AFunc: TFindFuncStatic<string>): TStringArray;
begin
  Result := TStringArray(FindAsArray(AFunc));
end;

function TStringArray.FindAsStringArray(AFunc: TFindFuncRef<string>): TStringArray;
begin
  Result := TStringArray(FindAsArray(AFunc));
end;

function TStringArray.FindAsStringArray(AFunc: TFindFunc<string>): TStringArray;
begin
  Result := TStringArray(FindAsArray(AFunc));
end;

function TStringArray.Copy: TStringArray;
begin
  Result := TStringArray(CreateCopy);
end;

class function TStringArray.ItemToString(AItem: string): string;
begin
  Result := AItem;
end;

{ TAnsiStringArray }

function TAnsiStringArray.FindAsAnsiStringArray(AFunc: TFindFuncStatic<AnsiString>): TAnsiStringArray;
begin
  Result := TAnsiStringArray(FindAsArray(AFunc));
end;

function TAnsiStringArray.FindAsAnsiStringArray(AFunc: TFindFuncRef<AnsiString>): TAnsiStringArray;
begin
  Result := TAnsiStringArray(FindAsArray(AFunc));
end;

function TAnsiStringArray.FindAsAnsiStringArray(AFunc: TFindFunc<AnsiString>): TAnsiStringArray;
begin
  Result := TAnsiStringArray(FindAsArray(AFunc));
end;

function TAnsiStringArray.Copy: TAnsiStringArray;
begin
  Result := TAnsiStringArray(CreateCopy);
end;

class function TAnsiStringArray.ItemToString(AItem: AnsiString): string;
begin
  Result := string(AItem);
end;

{ TRefArray<T> }

function TRefArray<T>.FindAsRefArray(AFunc: TFindFuncStatic<T>): TRefArray<T>;
begin
  Result := TRefArray<T>(FindAsArray(AFunc));
end;

function TRefArray<T>.FindAsRefArray(AFunc: TFindFuncRef<T>): TRefArray<T>;
begin
  Result := TRefArray<T>(FindAsArray(AFunc));
end;

function TRefArray<T>.FindAsRefArray(AFunc: TFindFunc<T>): TRefArray<T>;
begin
  Result := TRefArray<T>(FindAsArray(AFunc));
end;

function TRefArray<T>.Copy: TRefArray<T>;
begin
  Result := TRefArray<T>(CreateCopy);
end;

function TRefArray<T>.Find(AItem: T): Integer;
begin
  Result := FindFirstIndex(
    function(ACurrent: T): Boolean
    begin
      Result := ACurrent = AItem;
    end);
end;

procedure TRefArray<T>.Del(AItem: T);
var
  I: Integer;
begin
  I := Find(AItem);
  if I = -1 then
    raise EArrayItemNotFound.Create;
  DelAt(I);
end;

class function TRefArray<T>.ItemToString(AItem: T): string;
begin
  Result := AItem.ToString;
end;

{ TObjectArray<T> }

function TObjectArray<T>.ShouldFreeItems: Boolean;
begin
  Result := True;
end;

procedure TObjectArray<T>.ItemRemoved(AIndex: Integer);
begin
  Self[AIndex].Free;
end;

function TObjectArray<T>.CreateCopy: TArray;
begin
  Result := TRefArray<T>.Create(GrowAmount, ShrinkRetain);
  CopyTo(Result);
end;

{ TObjectPairArray<K, V> }

function TObjectPairArray<K, V>.ShouldFreeItems: Boolean;
begin
  Result := True;
end;

procedure TObjectPairArray<K, V>.ItemRemoved(AIndex: Integer);
begin
  Self[AIndex].Key.Free;
end;

function TObjectPairArray<K, V>.Copy: TObjectPairArray<K, V>;
begin
  Result := TObjectPairArray<K, V>(CreateCopy);
end;

{ TToObjectPairArray<K, V> }

function TToObjectPairArray<K, V>.ShouldFreeItems: Boolean;
begin
  Result := True;
end;

procedure TToObjectPairArray<K, V>.ItemRemoved(AIndex: Integer);
begin
  Self[AIndex].Value.Free;
end;

function TToObjectPairArray<K, V>.Copy: TToObjectPairArray<K, V>;
begin
  Result := TToObjectPairArray<K, V>(CreateCopy);
end;

{ TObjectObjectPairArray<K, V> }

function TObjectObjectPairArray<K, V>.ShouldFreeItems: Boolean;
begin
  Result := True;
end;

procedure TObjectObjectPairArray<K, V>.ItemRemoved(AIndex: Integer);
begin
  Self[AIndex].Key.Free;
  Self[AIndex].Value.Free;
end;

function TObjectObjectPairArray<K, V>.Copy: TObjectObjectPairArray<K, V>;
begin
  Result := TObjectObjectPairArray<K, V>(CreateCopy);
end;

{ TInterfaceArray<T> }

function TInterfaceArray<T>.FindAsInterfaceArray(AFunc: TFindFuncStatic<T>): TInterfaceArray<T>;
begin
  Result := TInterfaceArray<T>(FindAsArray(AFunc));
end;

function TInterfaceArray<T>.FindAsInterfaceArray(AFunc: TFindFuncRef<T>): TInterfaceArray<T>;
begin
  Result := TInterfaceArray<T>(FindAsArray(AFunc));
end;

function TInterfaceArray<T>.FindAsInterfaceArray(AFunc: TFindFunc<T>): TInterfaceArray<T>;
begin
  Result := TInterfaceArray<T>(FindAsArray(AFunc));
end;

function TInterfaceArray<T>.Copy: TInterfaceArray<T>;
begin
  Result := TInterfaceArray<T>(CreateCopy);
end;

function TInterfaceArray<T>.Find(AItem: T): Integer;
begin
  Result := FindFirstIndex(
    function(ACurrent: T): Boolean
    begin
      Result := (ACurrent as TObject) = (AItem as TObject);
    end);
end;

function TInterfaceArray<T>.Find(AItem: TObject): Integer;
begin
  Result := FindFirstIndex(
    function(ACurrent: T): Boolean
    begin
      Result := (ACurrent as TObject) = AItem;
    end);
end;

procedure TInterfaceArray<T>.Del(AItem: T);
var
  I: Integer;
begin
  I := Find(AItem);
  if I = -1 then
    raise EArrayItemNotFound.Create;
  DelAt(I);
end;

procedure TInterfaceArray<T>.Del(AItem: TObject);
var
  I: Integer;
begin
  I := Find(AItem);
  if I = -1 then
    raise EArrayItemNotFound.Create;
  DelAt(I);
end;

class function TInterfaceArray<T>.ItemToString(AItem: T): string;
begin
  Result := (AItem as TObject).ToString;
end;

{ TStack }

function TStack.GetCapacity: Integer;
begin
  Result := FArray.Capacity;
end;

procedure TStack.SetCapacity(const Value: Integer);
begin
  FArray.Capacity := Value;
end;

function TStack.GetGrowAmount: Integer;
begin
  Result := FArray.GrowAmount;
end;

procedure TStack.SetGrowAmount(const Value: Integer);
begin
  FArray.GrowAmount := Value;
end;

function TStack.GetShrinkRetain: Integer;
begin
  Result := FArray.ShrinkRetain;
end;

procedure TStack.SetShrinkRetain(const Value: Integer);
begin
  FArray.ShrinkRetain := Value;
end;

function TStack.CreateSame(AGrowAmount, AShrinkRetain: Integer): TStack;
begin
  Result := TStackClass(ClassType).Create(AGrowAmount, AShrinkRetain);
end;

constructor TStack.Create(AGrowAmount, AShrinkRetain: Integer);
begin
  FArray := CreateArray(AGrowAmount, AShrinkRetain);
end;

function TStack.CreateCopy: TStack;
begin
  Result := TStack(ClassType).CreateNoArray;
  FArray := Result.FArray.Copy;
end;

constructor TStack.CreateNoArray;
begin
  // do nothing
end;

procedure TStack.Clear;
begin
  FArray.Clear;
end;

function TStack.Count: Integer;
begin
  Result := FArray.Count;
end;

function TStack.Copy: TStack;
begin
  Result := CreateCopy;
end;

{ TStack<T> }

function TStack<T>.GetTop: T;
begin
  Result := FArray.Last;
end;

procedure TStack<T>.SetTop(const Value: T);
begin
  FArray.Last := Value;
end;

function TStack<T>.CreateArray(AGrowAmount, AShrinkRetain: Integer): TArray;
begin
  Result := TArray<T>.Create(AGrowAmount, AShrinkRetain);
end;

procedure TStack<T>.Push(AItem: T);
begin
  FArray.Add(AItem);
end;

function TStack<T>.Pop: T;
begin
  FArray.DelLast;
end;

function TStack<T>.Copy: TStack<T>;
begin
  Result := TStack<T>(CreateCopy);
end;

{ TObjectStack<T> }

function TObjectStack<T>.Copy: TObjectStack<T>;
begin
  Result := TObjectStack<T>(CreateCopy);
end;

function TObjectStack<T>.CreateArray(AGrowAmount, AShrinkRetain: Integer): TArray;
begin
  Result := TObjectArray<T>.Create(AGrowAmount, AShrinkRetain);
end;

{ THashBase.TIterator<T> }

constructor THashBase.TIterator<T>.Create;
begin
  FIndex := 0;
  FBucketIndex := -1;
end;

function THashBase.TIterator<T>.MoveNext: Boolean;
begin
  if GetBuckets = 0 then
    Exit(False);

  Inc(FBucketIndex);

  if GetBucketSize(FIndex) <> 0 then
  begin
    if FBucketIndex < GetBucketSize(FIndex) then
      Exit(True)
    else
      FBucketIndex := 0;
  end;

  repeat
    if FIndex = GetBuckets - 1 then
      Exit(False);
    Inc(FIndex);
  until GetBucketSize(FIndex) <> 0;

  Result := True;
end;

{ THashBase }

procedure THashBase.SetAutoRehash(const Value: Boolean);
begin
  if FAutoRehash = Value then
    Exit;
  FAutoRehash := Value;
  Buckets := Buckets;
end;

procedure THashBase.Clear;
begin
  ClearBuckets;
  FCount := 0;
  Buckets := 0;
end;

function THashBase.Count: Integer;
begin
  Result := FCount;
end;

function THashBase.CountOptimized: Boolean;
begin
  Result := True;
end;

constructor THashBase.Create(AAutoRehash: Boolean);
begin
  FAutoRehash := AAutoRehash;
end;

function THashBase.CreateCopy(AAutoRehash: Boolean): THashBase;
begin
  Result := CreateSame(AAutoRehash);
  CopyTo(Result);
end;

function THashBase.CreateSame(AAutoRehash: Boolean): THashBase;
begin
  Result := THashBaseClass(ClassType).Create(AAutoRehash);
end;

destructor THashBase.Destroy;
begin
  Clear;
  inherited;
end;

function THashBase.Empty: Boolean;
begin
  Result := FCount = 0;
end;

class function THashBase.GetHashBuckets(ACount: Integer): Integer;
var
  F: Single;
begin
  if ACount < 0 then
    raise EInvalidHashBucketCount.Create;
  if ACount = 0 then
    Exit(HashPrimes[0]);
  F := Log2(ACount);
  Result := Max(0, Floor(F - HashPrimeOffset));
  if Result >= Length(HashPrimes) then
    raise ETooManyHashBuckets.Create;
  Result := HashPrimes[Result];
end;

procedure THashBase.Rehash(ABuckets: Integer);
var
  New: THashBase;
begin
  if Count = 0 then
  begin
    SetBucketsDirect(ABuckets);
    Exit;
  end;
  New := CreateSame(False);
  try
    New.SetBucketsDirect(ABuckets);
    New.CopyBuckets(Self);
    Clear;
    FCount := New.Count;
    OwnBuckets(New);
  finally
    New.Free;
  end;
end;

function THashBase.Copy(AAutoRehash: Boolean): THashBase;
begin
  Result := CreateCopy;
end;

procedure THashBase.CopyTo(AHashBase: THashBase);
var
  I: Integer;
begin
  AHashBase.SetBucketsDirect(Buckets);
  for I := 0 to Buckets - 1 do
    if FBuckets[I] <> nil then
      AHashBase.FBuckets[I] := FBuckets[I].CreateCopy;
end;

{ THashBase<K> }

class function THashBase<K>.CanIndex(AKey: K): Boolean;
begin
  Result := True;
end;

function THashBase<K>.Copy(AAutoRehash: Boolean): THashBase<K>;
begin
  Result := THashBase<K>(CreateCopy(AAutoRehash));
end;

function THashBase<K>.GetCappedHash(AKey: K): Integer;
begin
  Result := GetHash(AKey) mod Cardinal(Buckets);
end;

{ TSet<T>.TIterator }

constructor TSet<T>.TIterator.Create(ASet: TSet<T>);
begin
  inherited Create;
  FSet := ASet;
end;

function TSet<T>.TIterator.GetBuckets: Integer;
begin
  Result := FSet.Buckets;
end;

function TSet<T>.TIterator.GetBucketSize(AIndex: Integer): Integer;
begin
  if FSet.FBuckets[AIndex] = nil then
    Exit(0);
  Result := FSet.FBuckets[AIndex].Count;
end;

function TSet<T>.TIterator.GetCurrent: T;
begin
  Result := TBucket(FSet.FBuckets[Index])[BucketIndex];
end;

{ TSet<T> }

procedure TSet<T>.Add(AElement: T);
begin
  if not TryAdd(AElement) then
    raise ESetKeyExistsAlready.Create;
end;

procedure TSet<T>.Add(ASet: TSet<T>);
var
  Element: T;
begin
  for Element in ASet do
    TryAdd(Element);
end;

procedure TSet<T>.ClearBuckets;
var
  I: Integer;
begin
  for I := 0 to Length(FBuckets) - 1 do
    FreeAndNil(FBuckets[I]);
end;

function TSet<T>.Copy(AAutoRehash: Boolean): TSet<T>;
begin
  Result := TSet<T>(CreateCopy(AAutoRehash));
end;

procedure TSet<T>.CopyBuckets(AFrom: THashBase);
var
  Key: T;
begin
  for Key in TSet<T>(AFrom) do
    TryAdd(Key);
end;

function TSet<T>.CreateBucket: THashBase.TBucket;
begin
  Result := TBucket.Create(4, 2);
end;

procedure TSet<T>.Del(AElement: T);
begin
  if not TryDel(AElement) then
    raise ESetKeyNotFound.Create;
end;

procedure TSet<T>.Del(ASet: TSet<T>);
var
  Element: T;
begin
  for Element in ASet do
    TryDel(Element);
end;

function TSet<T>.Equals(ASet: TSet<T>): Boolean;
var
  Element: T;
begin
  if Count <> ASet.Count then
    Exit(False);

  for Element in Self do
    if not ASet[Element] then
      Exit(False);

  Result := True;
end;

function TSet<T>.GetBuckets: Integer;
begin
  Result := Length(FBuckets);
end;

function TSet<T>.GetEnumerator: IIterator<T>;
begin
  Result := TIterator.Create(Self);
end;

function TSet<T>.Intersection(ASet: TSet<T>): TSet<T>;
var
  Element: T;
begin
  Result := TSet<T>(CreateSame(False));
  Result.Buckets := (Buckets + ASet.Buckets) div 2;
  for Element in Self do
    if ASet[Element] then
      Result.TryAdd(Element);
end;

function TSet<T>.IsSubsetOf(ASet: TSet<T>): Boolean;
var
  Element: T;
begin
  for Element in Self do
    if not ASet[Element] then
      Exit(False);
  Result := True;
end;

function TSet<T>.IsSupersetOf(ASet: TSet<T>): Boolean;
var
  Element: T;
begin
  for Element in ASet do
    if not Self[Element] then
      Exit(False);
  Result := True;
end;

function TSet<T>.GetElement(AElement: T): Boolean;
var
  H: Integer;
  Key: T;
begin
  if Empty then
    Exit(False);

  H := GetCappedHash(AElement);
  if FBuckets[H] = nil then
    Exit(False);
  for Key in TBucket(FBuckets[H]) do
    if KeysEqual(Key, AElement) then
      Exit(True);
  Result := False;
end;

procedure TSet<T>.OwnBuckets(AFrom: THashBase);
begin
  FBuckets := TSet<T>(AFrom).FBuckets;
  TSet<T>(AFrom).FBuckets := nil;
end;

function TSet<T>.Remove(ASet: TSet<T>): TSet<T>;
var
  Element: T;
begin
  Result := TSet<T>(CreateSame(False));
  for Element in Self do
    if not ASet[Element] then
      TryAdd(Element);
end;

procedure TSet<T>.SetBuckets(const Value: Integer);
var
  NewCapacity: Integer;
begin
  if Value = 0 then
  begin
    if Count > 0 then
      raise EHashBucketRequired.Create
    else
      SetLength(FBuckets, 0);
    Exit;
  end;

  NewCapacity := GetHashBuckets(Value);
  if NewCapacity > Buckets then
    Rehash(NewCapacity)
  else if NewCapacity < Buckets div 2 then // only shrink if bucket count halved
    Rehash(NewCapacity);
end;

procedure TSet<T>.SetBucketsDirect(Value: Integer);
begin
  SetLength(FBuckets, Value);
end;

procedure TSet<T>.SetElement(AElement: T; const Value: Boolean);
begin
  if Value then
    TryAdd(AElement)
  else
    TryDel(AElement);
end;

function TSet<T>.TryAdd(AElement: T): Boolean;
var
  H, I: Integer;
begin
  if Buckets = 0 then
  begin
    if AutoRehash then
      Buckets := 1
    else
      raise EHashBucketRequired.Create;
  end;

  H := GetCappedHash(AElement);
  if FBuckets[H] = nil then
  begin
    FBuckets[H] := CreateBucket;
    TBucket(FBuckets[H]).Add(AElement);
    Inc(FCount);
    if AutoRehash then
      Buckets := Count;
    Exit;
  end;

  for I := 0 to FBuckets[H].MaxIndex do
    if KeysEqual(TBucket(FBuckets[H])[I], AElement) then
      Exit(False);
  TBucket(FBuckets[H]).Add(AElement);
  Inc(FCount);
  if AutoRehash then
    Buckets := Count;
end;

function TSet<T>.TryDel(AElement: T): Boolean;
var
  H, I: Integer;
begin
  if Empty then
    Exit(False);

  H := GetCappedHash(AElement);
  if FBuckets[H] = nil then
    Exit(False);
  I := TBucket(FBuckets[H]).FindFirstIndex(
    function(ACurrent: T): Boolean
    begin
      Result := KeysEqual(ACurrent, AElement);
    end);
  if I = -1 then
    Exit(False);
  FBuckets[H].DelAt(I);
  Dec(FCount);
  if FBuckets[H].Count = 0 then
    FreeAndNil(FBuckets[H]);
  if AutoRehash then
    Buckets := Count;
  Result := True;
end;

function TSet<T>.Union(ASet: TSet<T>): TSet<T>;
var
  Element: T;
begin
  Result := TSet<T>(CreateSame(False));
  Result.Buckets := (Buckets + ASet.Buckets) div 2;
  Result.Add(Self);
  Result.Add(ASet);
  Result.AutoRehash := True;
end;

{ TMap<K, V>.TIterator }

constructor TMap<K, V>.TIterator.Create(AMap: TMap<K, V>);
begin
  FMap := AMap;
  FIndex := 0;
  FBucketIndex := -1;
end;

function TMap<K, V>.TIterator.GetBuckets: Integer;
begin
  Result := FMap.Buckets;
end;

function TMap<K, V>.TIterator.GetBucketSize(AIndex: Integer): Integer;
begin
  if FMap.FBuckets[AIndex] = nil then
    Exit(0);
  Result := FMap.FBuckets[AIndex].Count;
end;

function TMap<K, V>.TIterator.GetCurrent: TPair;
begin
  Result := TBucket(FMap.FBuckets[Index])[BucketIndex];
end;

{ TMap<K, V> }

function TMap<K, V>.BucketCounts: TIntArray;
var
  I: Integer;
begin
  Result := TIntArray.Create;
  Result.Capacity := Buckets;
  for I := 0 to Buckets - 1 do
    if FBuckets[I] = nil then
      Result.Add(0)
    else
      Result.Add(FBuckets[I].Count);
end;

procedure TMap<K, V>.ClearBuckets;
var
  I: Integer;
begin
  for I := 0 to Length(FBuckets) - 1 do
    FreeAndNil(FBuckets[I]);
end;

function TMap<K, V>.Copy(AAutoRehash: Boolean): TMap<K, V>;
begin
  Result := TMap<K, V>(CreateCopy);
end;

procedure TMap<K, V>.CopyBuckets(AFrom: THashBase);
var
  Pair: TPair;
begin
  for Pair in TMap<K, V>(AFrom) do
    Self[Pair.Key] := Pair.Value;
end;

function TMap<K, V>.CreateBucket: THashBase.TBucket;
begin
  Result := TBucket.Create(4, 2);
end;

procedure TMap<K, V>.Del(AKey: K);
begin
  if not TryDel(AKey) then
    raise EMapKeyNotFound.Create;
end;

function TMap<K, V>.Get(AKey: K; out AValue: V): Boolean;
var
  H: Integer;
  Pair: TPair;
begin
  if Empty then
    Exit(False);

  H := GetCappedHash(AKey);
  if FBuckets[H] = nil then
    Exit(False);
  for Pair in TBucket(FBuckets[H]) do
  begin
    if KeysEqual(Pair.Key, AKey) then
    begin
      AValue := Pair.Value;
      Exit(True);
    end;
  end;
  Result := False;
end;

function TMap<K, V>.GetBuckets: Integer;
begin
  Result := Length(FBuckets);
end;

procedure TMap<K, V>.SetBuckets(const Value: Integer);
var
  NewCapacity: Integer;
begin
  if Value = 0 then
  begin
    if Count > 0 then
      raise EHashBucketRequired.Create
    else
      SetLength(FBuckets, 0);
    Exit;
  end;

  NewCapacity := GetHashBuckets(Value);
  if NewCapacity > Buckets then
    Rehash(NewCapacity)
  else if NewCapacity < Buckets div 2 then // only shrink if bucket count halved
    Rehash(NewCapacity);
end;

procedure TMap<K, V>.SetBucketsDirect(Value: Integer);
begin
  SetLength(FBuckets, Value);
end;

procedure TMap<K, V>.SetItem(AKey: K; const Value: V);
var
  H, I: Integer;
  Pair: TPair;
begin
  if Buckets = 0 then
  begin
    if AutoRehash then
      Buckets := 1
    else
      raise EHashBucketRequired.Create;
  end;

  H := GetCappedHash(AKey);
  if FBuckets[H] = nil then
  begin
    FBuckets[H] := CreateBucket;
    TBucket(FBuckets[H]).Add(TPair.Create(AKey, Value));
    Inc(FCount);
    if AutoRehash then
      Buckets := Count;
    Exit;
  end;

  for I := 0 to FBuckets[H].MaxIndex do
  begin
    if KeysEqual(TBucket(FBuckets[H])[I].Key, AKey) then
    begin
      TBucket(FBuckets[H])[I] := TPair.Create(AKey, Value);
      Exit;
    end;
  end;
  TBucket(FBuckets[H]).Add(TPair.Create(AKey, Value));
  Inc(FCount);
  if AutoRehash then
    Buckets := Count;
end;

function TMap<K, V>.TryDel(AKey: K): Boolean;
var
  H, I: Integer;
begin
  if Empty then
    Exit(False);

  H := GetCappedHash(AKey);
  if FBuckets[H] = nil then
    Exit(False);
  I := TBucket(FBuckets[H]).FindFirstIndex(
    function(APair: TPair): Boolean
    begin
      Result := KeysEqual(APair.Key, AKey);
    end);
  if I = -1 then
    Exit(False);
  FBuckets[H].DelAt(I);
  Dec(FCount);
  if FBuckets[H].Count = 0 then
    FreeAndNil(FBuckets[H]);
  if AutoRehash then
    Buckets := Count;
  Result := True;
end;

function TMap<K, V>.GetEnumerator: IIterator<TPair>;
begin
  Result := TIterator.Create(Self);
end;

function TMap<K, V>.GetItem(AKey: K): V;
begin
  if not Get(AKey, Result) then
    raise EMapKeyNotFound.Create;
end;

procedure TMap<K, V>.OwnBuckets(AFrom: THashBase);
begin
  Self.FBuckets := TMap<K, V>(AFrom).FBuckets;
  TMap<K, V>(AFrom).FBuckets := nil;
end;

function TMap<K, V>.KeySet(AAutoRehash: Boolean): TSet<K>;
var
  Pair: TPair;
begin
  Result := TSet<K>(CreateSame(False));
  Result.Rehash(Buckets);
  for Pair in Self do
    Result.TryAdd(Pair.Key);
  Result.AutoRehash := AAutoRehash;
end;

{ TValueHasher<K> }

class function TValueHasher<K>.CanIndex(AKey: K): Boolean;
begin
  Result := True;
end;

{ TValueMap<K, V, H> }

class function TValueMap<K, V, H>.GetHash(AKey: K): Cardinal;
begin
  Result := H.GetHash(AKey);
end;

class function TValueMap<K, V, H>.KeysEqual(AKey1, AKey2: K): Boolean;
begin
  Result := H.KeysEqual(AKey1, AKey2);
end;

class function TValueMap<K, V, H>.CanIndex(AKey: K): Boolean;
begin
  Result := H.CanIndex(AKey);
end;

function TValueMap<K, V, H>.Copy(AAutoRehash: Boolean): TValueMap<K, V, H>;
begin
  Result := TValueMap<K, V, H>(CreateCopy);
end;

{ TValueSet<T, H> }

class function TValueSet<T, H>.CanIndex(AKey: T): Boolean;
begin
  Result := H.CanIndex(AKey);
end;

function TValueSet<T, H>.Copy(AAutoRehash: Boolean): TValueSet<T, H>;
begin
  Result := TValueSet<T, H>(CreateCopy);
end;

class function TValueSet<T, H>.GetHash(AKey: T): Cardinal;
begin
  Result := H.GetHash(AKey);
end;

class function TValueSet<T, H>.KeysEqual(AKey1, AKey2: T): Boolean;
begin
  Result := H.KeysEqual(AKey1, AKey2);
end;

end.
