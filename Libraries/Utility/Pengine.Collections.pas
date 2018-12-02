unit Pengine.Collections;

interface

uses
  System.Classes,
  System.Math,
  System.SysUtils,

  Pengine.Interfaces,
  Pengine.Sorting,
  Pengine.Vector,
  Pengine.IntMaths,
  Pengine.CollectionInterfaces;

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
    constructor Create;
  end;

  /// <summary>Raised, if an array-item does not have a string-representative.</summary>
  EArrayItemNoStringRepresentative = class(Exception)
  public
    constructor Create;
  end;

  /// <summary>Raised, if a negative capacity is tried to assigned to an array.</summary>
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

  /// <summary>A generic record, representing a read-only Key-Value-Pair.</summary>
  TPair<K, V> = record
  private
    FKey: K;
    FValue: V;

  public
    constructor Create(AKey: K; AValue: V);

    property Key: K read FKey;
    property Value: V read FValue;

  end;

  // TODO: XmlDoc
  TArray = class(TInterfaceBase)
  public type

    TReader = class
    private
      function GetCapacity: Integer; inline;
      function GetGrowAmount: Integer; inline;
      function GetShrinkRetain: Integer; inline;

    public
      property GrowAmount: Integer read GetGrowAmount;
      property ShrinkRetain: Integer read GetShrinkRetain;

      function Count: Integer; inline;
      function CountOptimized: Boolean; inline;
      function MaxIndex: Integer; inline;
      function Empty: Boolean; inline;

      property Capacity: Integer read GetCapacity;

      procedure RangeCheckException(AIndex: Integer); inline;
      function RangeCheck(AIndex: Integer): Boolean; inline;

      function Copy: TArray; inline;

    end;

  public const

    NeverShrink = Integer.MaxValue;

    DefaultGrowAmount = 16;
    DefaultShrinkRetain = 8;

  private
    FGrowAmount: Integer;
    FShrinkRetain: Integer;

    procedure SetGrowAmount(const Value: Integer); virtual;
    procedure SetShrinkRetain(const Value: Integer); virtual;

  protected
    FCount: Integer;

    function CreateSame: TArray;

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

    procedure BeforeSort; virtual;
    procedure AfterSort; virtual;

  public
    // TODO: XmlDoc
    constructor Create; virtual;
    // TODO: XmlDoc
    destructor Destroy; override;

    // TODO: XmlDoc
    property GrowAmount: Integer read FGrowAmount write SetGrowAmount;
    // TODO: XmlDoc
    property ShrinkRetain: Integer read FShrinkRetain write SetShrinkRetain;

    procedure SetGrowShrink(AGrowAmount, AShrinkRetain: Integer);

    // TODO: XmlDoc
    function Count: Integer; inline;
    // use this, when making changes over DataPointer, keep capacity in mind
    procedure ForceCount(ACount: Integer); inline;
    // TODO: XmlDoc
    function CountOptimized: Boolean; inline;
    // TODO: XmlDoc Remarks: returns -1 on empty arrays
    function MaxIndex: Integer; inline;
    // TODO: XmlDoc
    function Empty: Boolean; inline;

    // TODO: XmlDoc
    procedure RemoveAt(AIndex: Integer); virtual; abstract;
    // TODO: XmlDoc
    procedure RemoveLast; inline;
    // TODO: XmlDoc
    procedure Clear(AZeroCapacity: Boolean = True); virtual;

    // TODO: XmlDoc
    property Capacity: Integer read GetCapacity write SetCapacity;

    // TODO: XmlDoc
    procedure RangeCheckException(AIndex: Integer); inline;
    // TODO: XmlDoc
    function RangeCheck(AIndex: Integer): Boolean; inline;

    // TODO: XmlDoc
    procedure Swap(A, B: Integer); virtual;
    // TODO: XmlDoc
    procedure SwapUnchecked(A, B: Integer); virtual; abstract;

    procedure Shuffle;

    // TODO: XmlDoc
    function DataPointer: Pointer; virtual; abstract;

    // TODO: XmlDoc
    function Copy: TArray;

    function Reader: TReader; inline;

  end;

  TArrayClass = class of TArray;

  // TODO: XmlDoc
  TArray<T> = class(TArray, IIterable<T>)
  public type

    TValue = T;

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
      destructor Destroy; override;

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

    TReader = class(TArray.TReader)
    private
      function GetFirst: T;
      function GetItem(I: Integer): T;
      function GetLast: T;
      procedure SetFirst(const Value: T);
      procedure SetItem(I: Integer; const Value: T);
      procedure SetLast(const Value: T);

    public

      {$REGION 'Find Functions'}

      // TODO: XmlDoc
      function FindFirstIndex(AFunc: TFindFuncStatic<T>): Integer; overload; inline;
      // TODO: XmlDoc
      function FindFirstIndex(AFunc: TFindFuncRef<T>): Integer; overload; inline;
      // TODO: XmlDoc
      function FindFirstIndex(AFunc: TFindFunc<T>): Integer; overload; inline;

      // TODO: XmlDoc
      function FindFirst(AFunc: TFindFuncStatic<T>): T; overload; inline;
      // TODO: XmlDoc
      function FindFirst(AFunc: TFindFuncRef<T>): T; overload; inline;
      // TODO: XmlDoc
      function FindFirst(AFunc: TFindFunc<T>): T; overload; inline;

      // TODO: XmlDoc
      function FindLastIndex(AFunc: TFindFuncStatic<T>): Integer; overload; inline;
      // TODO: XmlDoc
      function FindLastIndex(AFunc: TFindFuncRef<T>): Integer; overload; inline;
      // TODO: XmlDoc
      function FindLastIndex(AFunc: TFindFunc<T>): Integer; overload; inline;

      // TODO: XmlDoc
      function FindLast(AFunc: TFindFuncStatic<T>): T; overload;inline;
      // TODO: XmlDoc
      function FindLast(AFunc: TFindFuncRef<T>): T; overload; inline;
      // TODO: XmlDoc
      function FindLast(AFunc: TFindFunc<T>): T; overload; inline;

      // TODO: XmlDoc
      function FindIndexAsArray(AFunc: TFindFuncStatic<T>): TArray<Integer>; overload; inline;
      // TODO: XmlDoc
      function FindIndexAsArray(AFunc: TFindFuncRef<T>): TArray<Integer>; overload; inline;
      // TODO: XmlDoc
      function FindIndexAsArray(AFunc: TFindFunc<T>): TArray<Integer>; overload; inline;

      // TODO: XmlDoc
      function FindAsArray(AFunc: TFindFuncStatic<T>): TArray<T>; overload; inline;
      // TODO: XmlDoc
      function FindAsArray(AFunc: TFindFuncRef<T>): TArray<T>; overload; inline;
      // TODO: XmlDoc
      function FindAsArray(AFunc: TFindFunc<T>): TArray<T>; overload; inline;

      {$ENDREGION} //

      {$REGION 'Sorting'}

      // TODO: XmlDoc
      function Sorted(AFunc: TCompareFuncStatic<T>): Boolean; overload; inline;
      // TODO: XmlDoc
      function Sorted(AFunc: TCompareFuncRef<T>): Boolean; overload; inline;
      // TODO: XmlDoc
      function Sorted(AFunc: TCompareFunc<T>): Boolean; overload; inline;

      {$ENDREGION}

      procedure ForEach(AFunc: TForEachProcStatic<T>); overload;
      procedure ForEach(AFunc: TForEachProcRef<T>); overload;
      procedure ForEach(AFunc: TForEachProc<T>); overload;

      function ForEach<R>(AFunc: TForEachFuncStatic<T, R>): TArray<R>; overload;
      function ForEach<R>(AFunc: TForEachFuncRef<T, R>): TArray<R>; overload;
      function ForEach<R>(AFunc: TForEachFunc<T, R>): TArray<R>; overload;

      // TODO: XmlDoc
      function BinarySearch(AItem: T; AFunc: TCompareFuncStatic<T>): Integer; overload; inline;
      // TODO: XmlDoc
      function BinarySearch(AItem: T; AFunc: TCompareFuncRef<T>): Integer; overload; inline;
      // TODO: XmlDoc
      function BinarySearch(AItem: T; AFunc: TCompareFunc<T>): Integer; overload; inline;

      // TODO: XmlDoc
      function Copy: TArray<T>; reintroduce; inline;

      // TODO: XmlDoc
      property Items[I: Integer]: T read GetItem write SetItem; default;

      // TODO: XmlDoc
      property First: T read GetFirst write SetFirst;
      // TODO: XmlDoc
      property Last: T read GetLast write SetLast;

      // TODO: XmlDoc
      function GetEnumerator: IIterator<T>; inline;
      // TODO: XmlDoc
      function InReverse: TReverseWrapper; inline;

      // TODO: XmlDoc
      function ToString: string; override;
      // TODO: XmlDoc
      class function ItemToString(AItem: T): string; inline;

    end;

  private
    FItems: array of T;

    function GetItem(AIndex: Integer): T;

    function GetFirst: T;
    procedure SetFirst(const Value: T);

    function GetLast: T;
    procedure SetLast(const Value: T);
    
  protected
    procedure SetItem(AIndex: Integer; AValue: T); virtual;

    function GetCapacity: Integer; override;
    procedure SetCapacity(const Value: Integer); override;

    procedure CopyTo(AArray: TArray); override;

  public
    // TODO: XmlDoc
    function Add(AItem: T): T; overload; virtual;
    // TODO: XmlDoc
    procedure Add(AItems: IIterator<T>); overload;
    // TODO: XmlDoc
    procedure Add(AItems: IIterable<T>); overload;
    // TODO: XmlDoc
    procedure Add(AItems: IEnumerable<T>); overload;

    // TODO: XmlDoc: Remarks: Insert can also add item at the end
    function Insert(AItem: T; AIndex: Integer): T; virtual;

    procedure SetIndex(ASource, ADestination: Integer); virtual;

    // TODO: XmlDoc
    procedure RemoveAt(AIndex: Integer); override;

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
    function FindIndexAsArray(AFunc: TFindFuncStatic<T>): TArray<Integer>; overload;
    // TODO: XmlDoc
    function FindIndexAsArray(AFunc: TFindFuncRef<T>): TArray<Integer>; overload;
    // TODO: XmlDoc
    function FindIndexAsArray(AFunc: TFindFunc<T>): TArray<Integer>; overload;

    // TODO: XmlDoc
    function FindAsArray(AFunc: TFindFuncStatic<T>): TArray<T>; overload;
    // TODO: XmlDoc
    function FindAsArray(AFunc: TFindFuncRef<T>): TArray<T>; overload;
    // TODO: XmlDoc
    function FindAsArray(AFunc: TFindFunc<T>): TArray<T>; overload;

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

    procedure ForEach(AFunc: TForEachProcStatic<T>); overload;
    procedure ForEach(AFunc: TForEachProcRef<T>); overload;
    procedure ForEach(AFunc: TForEachProc<T>); overload;

    function ForEach<R>(AFunc: TForEachFuncStatic<T, R>): TArray<R>; overload;
    function ForEach<R>(AFunc: TForEachFuncRef<T, R>): TArray<R>; overload;
    function ForEach<R>(AFunc: TForEachFunc<T, R>): TArray<R>; overload;

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

    function Reader: TReader; reintroduce; inline;

  end;

  // TODO: XmlDoc
  TIntArray = TArray<Integer>;
  TIntArrayHelper = class helper for TIntArray
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
  end;

  TSingleArray = TArray<Single>;
  TSingleArrayHelper = class helper for TSingleArray
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
  end;

  TFindableArray<T> = class abstract(TArray<T>)
  public type

    TReader = class(TArray<T>.TReader)
    public
      function Find(AItem: T): Integer; reintroduce; inline;
      function Contains(AItem: T): Boolean; reintroduce; inline;

    end;

  public
    // TODO: XmlDoc
    function Find(AItem: T): Integer; virtual; abstract;
    function Contains(AItem: T): Boolean;
    // TODO: XmlDoc
    procedure Remove(AItem: T);

    function Reader: TReader; reintroduce; inline;

  end;

  // TODO: XmlDoc
  TBaseRefArray<T: class> = class abstract(TFindableArray<T>)
  protected
    function GetOwnsObjects: Boolean; virtual; abstract;

    function ShouldFreeItems: Boolean; override;
    procedure ItemRemoved(AIndex: Integer); override;

  public
    // TODO: XmlDoc
    function Find(AItem: T): Integer; override;

    // TODO: XmlDoc
    class function ItemToString(AItem: T): string; override;

    property OwnsObjects: Boolean read GetOwnsObjects;

    function Equals(Obj: TObject): Boolean; override;

  end;

  TRefArray<T: class> = class(TBaseRefArray<T>)
  public type

    TReader = class(TFindableArray<T>.TReader)
    public
      function Copy: TRefArray<T>; reintroduce; inline;

    end;

  private
    FOwnsObjects: Boolean;

    procedure SetOwnsObjects(const Value: Boolean);

  protected
    function GetOwnsObjects: Boolean; override;

  public
    constructor Create(AOwnsObjects: Boolean); reintroduce; overload;

    // TODO: XmlDoc
    function FindAsArray(AFunc: TFindFuncStatic<T>): TRefArray<T>; overload; inline;
    // TODO: XmlDoc
    function FindAsArray(AFunc: TFindFuncRef<T>): TRefArray<T>; overload; inline;
    // TODO: XmlDoc
    function FindAsArray(AFunc: TFindFunc<T>): TRefArray<T>; overload; inline;

    // TODO: XmlDoc
    function Copy: TRefArray<T>; reintroduce; inline;

    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;

    function Reader: TReader; reintroduce; inline;

  end;

  /// <summary>The same as <see cref="TRefArray{T}"/> but the constructor defaults to owning the objects.</summary>
  TObjectArray<T: class> = class(TRefArray<T>)
  public
    /// <summary>Creates a <see cref="TObjectArray{T}"/>, which will automatically free all objects on destruction.</summary>
    constructor Create; overload; override;

  end;

  // TODO: XmlDoc DO NOT COPY THIS, IT WON'T KEEP ITS LINK (probably)
  TRefArrayOwnLinked<T: class> = class(TBaseRefArray<T>)
  private
    FOwnsObjectsLink: PBoolean;

  protected
    function GetOwnsObjects: Boolean; override;

  public
    constructor Create(AOwnsObjectsLink: PBoolean); reintroduce;

  end;

  // TODO: XmlDoc
  TBaseRefPairArray<K: class; V> = class abstract(TArray<TPair<K, V>>)
  protected
    function GetOwnsKeys: Boolean; virtual; abstract;

    function ShouldFreeItems: Boolean; override;
    procedure ItemRemoved(AIndex: Integer); override;

  public
    property OwnsKeys: Boolean read GetOwnsKeys;

  end;

  TRefPairArray<K: class; V> = class(TBaseRefPairArray<K, V>)
  private
    FOwnsKeys: Boolean;

    procedure SetOwnsKeys(const Value: Boolean);

  protected
    function GetOwnsKeys: Boolean; override;

  public
    constructor Create(AOwnsKeys: Boolean); reintroduce; overload;

    // TODO: XmlDoc
    function FindAsArray(AFunc: TFindFuncStatic<TPair<K, V>>): TRefPairArray<K, V>; overload; inline;
    // TODO: XmlDoc
    function FindAsArray(AFunc: TFindFuncRef<TPair<K, V>>): TRefPairArray<K, V>; overload; inline;
    // TODO: XmlDoc
    function FindAsArray(AFunc: TFindFunc<TPair<K, V>>): TRefPairArray<K, V>; overload; inline;

    // TODO: XmlDoc
    function Copy: TRefPairArray<K, V>; reintroduce; inline;

    property OwnsKeys: Boolean read GetOwnsKeys write SetOwnsKeys;

  end;

  // TODO: XmlDoc DO NOT COPY THIS, IT WON'T KEEP ITS LINK (probably)
  TRefPairArrayOwnLinked<K: class; V> = class(TBaseRefPairArray<K, V>)
  private
    FOwnsKeysLink: PBoolean;

  protected
    function GetOwnsKeys: Boolean; override;

  public
    constructor Create(AOwnsKeysLink: PBoolean); reintroduce;

  end;

  // TODO: XmlDoc
  TBaseToRefPairArray<K; V: class> = class abstract(TArray<TPair<K, V>>)
  public type

    TPair = TPair<K, V>;

  protected
    function GetOwnsValues: Boolean; virtual; abstract;

    function ShouldFreeItems: Boolean; override;
    procedure ItemRemoved(AIndex: Integer); override;

  public
    property OwnsValues: Boolean read GetOwnsValues;

  end;

  TToRefPairArray<K; V: class> = class(TBaseToRefPairArray<K, V>)
  private
    FOwnsValues: Boolean;

    procedure SetOwnsValues(const Value: Boolean);

  protected
    function GetOwnsValues: Boolean; override;

  public
    constructor Create(AOwnsValues: Boolean); reintroduce; overload;

    // TODO: XmlDoc
    function FindAsArray(AFunc: TFindFuncStatic<TPair<K, V>>): TToRefPairArray<K, V>; overload; inline;
    // TODO: XmlDoc
    function FindAsArray(AFunc: TFindFuncRef<TPair<K, V>>): TToRefPairArray<K, V>; overload; inline;
    // TODO: XmlDoc
    function FindAsArray(AFunc: TFindFunc<TPair<K, V>>): TToRefPairArray<K, V>; overload; inline;

    // TODO: XmlDoc
    function Copy: TToRefPairArray<K, V>; reintroduce; inline;

    property OwnsValues: Boolean read GetOwnsValues write SetOwnsValues;

  end;

  TToObjectPairArray<K; V: class> = class(TToRefPairArray<K, V>)
  public
    constructor Create; overload; override;

  end;

  // TODO: XmlDoc DO NOT COPY THIS, IT WON'T KEEP ITS LINK (probably)
  TToRefPairArrayOwnLinked<K; V: class> = class(TBaseToRefPairArray<K, V>)
  private
    FOwnsValuesLink: PBoolean;

  protected
    function GetOwnsValues: Boolean; override;

  public
    constructor Create(AOwnsValuesLink: PBoolean); reintroduce;

  end;

  // TODO: XmlDoc
  TBaseRefRefPairArray<K, V: class> = class abstract(TArray<TPair<K, V>>)
  protected
    function GetOwnsKeys: Boolean; virtual; abstract;
    function GetOwnsValues: Boolean; virtual; abstract;

    function ShouldFreeItems: Boolean; override;
    procedure ItemRemoved(AIndex: Integer); override;

  public
    // TODO: XmlDoc
    class function ItemToString(AItem: TPair<K, V>): string; override;

    property OwnsKeys: Boolean read GetOwnsKeys;
    property OwnsValues: Boolean read GetOwnsValues;

  end;

  TRefRefPairArray<K, V: class> = class(TBaseRefRefPairArray<K, V>)
  private
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;

    procedure SetOwnsKeys(const Value: Boolean);
    procedure SetOwnsValues(const Value: Boolean);

  protected
    function GetOwnsKeys: Boolean; override;
    function GetOwnsValues: Boolean; override;

  public
    constructor Create(AOwnsKeys, AOwnsValues: Boolean); reintroduce; overload;

    // TODO: XmlDoc
    function FindAsArray(AFunc: TFindFuncStatic<TPair<K, V>>): TRefRefPairArray<K, V>; overload; inline;
    // TODO: XmlDoc
    function FindAsArray(AFunc: TFindFuncRef<TPair<K, V>>): TRefRefPairArray<K, V>; overload; inline;
    // TODO: XmlDoc
    function FindAsArray(AFunc: TFindFunc<TPair<K, V>>): TRefRefPairArray<K, V>; overload; inline;

    // TODO: XmlDoc
    function Copy: TRefRefPairArray<K, V>; reintroduce; inline;

    property OwnsKeys: Boolean read GetOwnsKeys write SetOwnsKeys;
    property OwnsValues: Boolean read GetOwnsValues write SetOwnsValues;

  end;

  // TODO: XmlDoc DO NOT COPY THIS, IT WON'T KEEP ITS LINK (probably)
  TRefRefPairArrayOwnLinked<K, V: class> = class(TBaseRefRefPairArray<K, V>)
  private
    FOwnsKeysLink: PBoolean;
    FOwnsValuesLink: PBoolean;

  protected
    function GetOwnsKeys: Boolean; override;
    function GetOwnsValues: Boolean; override;

  public
    constructor Create(AOwnsKeysLink, AOwnsValuesLink: PBoolean);
      reintroduce;

  end;

  // TODO: XmlDoc
  TInterfaceArray<T: IInterface> = class(TArray<T>)
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
    procedure Remove(AItem: T); overload;
    // TODO: XmlDoc
    procedure Remove(AItem: TObject); overload;

    // TODO: XmlDoc
    class function ItemToString(AItem: T): string; override;

  end;

  // Linked to a TArray<T> but is an iterable of the interface
  TLinkedInterfaceArray<I: IInterface; T: I> = class(TIterable<I>)
  public type

    TIterator = class(TIterator<I>)
    private
      FLinkedArray: TArray<T>;
      FIndex: Integer;

    public
      constructor Create(ALinkedArray: TArray<T>);

      function MoveNext: Boolean; override;
      function GetCurrent: I; override;

    end;

  private
    FLinkedArray: TArray<T>;

  public
    constructor Create(ALinkedArray: TArray<T>);

    function Count: Integer; override;
    function CountOptimized: Boolean; override;
    function GetEnumerator: IIterator<I>; override;

  end;

  // TODO: XmlDoc
  TStack = class
  private
    FArray: TArray;

    function GetCapacity: Integer; inline;
    procedure SetCapacity(const Value: Integer); inline;

    function GetGrowAmount: Integer; inline;
    procedure SetGrowAmount(const Value: Integer); inline;

    function GetShrinkRetain: Integer; inline;
    procedure SetShrinkRetain(const Value: Integer); inline;

  protected
    function CreateSame: TStack;

    // TODO: XmlDoc
    function CreateArray: TArray; virtual; abstract;

    function CreateCopy: TStack; virtual;

  public
    constructor Create; virtual;
    destructor Destroy; override;

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
    function GetTop: T; inline;
    procedure SetTop(const Value: T); inline;

  protected
    function CreateArray: TArray; override;

  public
    // TODO: XmlDoc
    property Top: T read GetTop write SetTop;
    // TODO: XmlDoc
    procedure Push(AItem: T); inline;
    // TODO: XmlDoc
    function Pop: T; inline;

    function Count: Integer; inline;
    function Empty: Boolean; inline;

    // TODO: XmlDoc
    function Copy: TStack<T>; reintroduce; inline;

  end;

  // TODO: TQueue<T> using a linked list

  // TODO: XmlDoc
  TRefStack<T: class> = class(TStack<T>)
  private
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const Value: Boolean);
  protected
    // TODO: XmlDoc
    function CreateArray: TArray; override;

  public
    constructor Create(AOwnsObjects: Boolean); reintroduce; overload;

    // TODO: XmlDoc
    function Copy: TRefStack<T>; reintroduce; inline;

    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;

  end;

  TObjectStack<T: class> = class(TRefStack<T>)
  public
    constructor Create; overload; override;
  end;

  // TODO: XmlDoc
  TValueHasher<K> = class abstract
  public
    class function GetHash(AKey: K): Cardinal; virtual; abstract;
    class function KeysEqual(AKey1, AKey2: K): Boolean; virtual; abstract;
    class function CanIndex(AKey: K): Boolean; virtual;
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

constructor EArrayRangeError.Create;
begin
  inherited Create('The array index is out of bounds.');
end;

{ EArrayItemNoStringRepresentative }

constructor EArrayItemNoStringRepresentative.Create;
begin
  inherited Create('The items in the array do not have a string representative.');
end;

{ EArrayNegativeCapacity }

constructor EArrayNegativeCapacity.Create;
begin
  inherited Create('The array capacity cannot be negative.');
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

{ TPair<K, V> }

constructor TPair<K, V>.Create(AKey: K; AValue: V);
begin
  FKey := AKey;
  FValue := AValue;
end;

{ TArray }

function TArray.CreateCopy: TArray;
begin
  Result := CreateSame;
  CopyTo(Result);
end;

function TArray.Copy: TArray;
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

constructor TArray.Create;
begin
  GrowAmount := DefaultGrowAmount;
  ShrinkRetain := DefaultShrinkRetain;
end;

function TArray.CreateSame: TArray;
begin
  Result := TArrayClass(ClassType).Create;
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

procedure TArray.ForceCount(ACount: Integer);
begin
  FCount := ACount;
end;

procedure TArray.RemoveLast;
begin
  RemoveAt(MaxIndex);
end;

procedure TArray.AfterSort;
begin
  // nothing by default
end;

procedure TArray.BeforeSort;
begin
  // nothing by default
end;

procedure TArray.Clear(AZeroCapacity: Boolean);
var
  I: Integer;
begin
  if ShouldFreeItems then
    for I := 0 to MaxIndex do
      ItemRemoved(I);
  if AZeroCapacity then
    Capacity := 0
  else
    FCount := 0;
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

procedure TArray.RangeCheckException(AIndex: Integer);
begin
  if not RangeCheck(AIndex) then
    raise EArrayRangeError.Create;
end;

function TArray.Reader: TReader;
begin
  Result := TReader(Self);
end;

procedure TArray.SetGrowAmount(const Value: Integer);
begin
  if Value < 1 then
    raise EArrayInvalidGrowAmount.Create;
  FGrowAmount := Value;
end;

procedure TArray.SetGrowShrink(AGrowAmount, AShrinkRetain: Integer);
begin
  GrowAmount := AGrowAmount;
  ShrinkRetain := AShrinkRetain;
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

procedure TArray.Shuffle;
var
  I: Integer;
begin
  for I := MaxIndex downto 0 do
    SwapUnchecked(Random(I + 1), I);
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
    FList.RemoveAt(FCurrent);
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
  Result := TArray<T>(FArray.CreateSame);
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
  FArray.SwapUnchecked(A, B);
end;

constructor TArray<T>.TSorterBase.Create(AArray: TArray<T>);
begin
  FArray := AArray;
  FArray.BeforeSort;
end;

destructor TArray<T>.TSorterBase.Destroy;
begin
  FArray.AfterSort;
  inherited;
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

procedure TArray<T>.SetIndex(ASource, ADestination: Integer);
var
  Tmp: T;
begin
  RangeCheckException(ASource);
  if ASource = ADestination then
    Exit;
  RangeCheckException(ADestination);
  Tmp := Items[ASource];
  if ASource < ADestination then
    Move(FItems[ASource + 1], FItems[ASource], SizeOf(T) * (ADestination - ASource))
  else
    Move(FItems[ADestination], FItems[ADestination + 1], SizeOf(T) * (ASource - ADestination));
  Move(Tmp, FItems[ADestination], SizeOf(T));
end;

procedure TArray<T>.SetItem(AIndex: Integer; AValue: T);
begin
  RangeCheckException(AIndex);
  if ShouldFreeItems then
    ItemRemoved(AIndex);
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
  Finalize(FItems[Count]);
  Move(FItems[AIndex], FItems[AIndex + 1], SizeOf(T) * (Count - AIndex));
  Initialize(FItems[AIndex]);
  FItems[AIndex] := AItem;
  Inc(FCount);
  Result := AItem;
end;

procedure TArray<T>.RemoveAt(AIndex: Integer);
var
  Tmp: T;
begin
  RangeCheckException(AIndex);
  if ShouldFreeItems then
    ItemRemoved(AIndex);
  if Count - AIndex > 1 then
  begin
    // move the deleted element at the end, so that it will get cleaned up correctly (in other words DON'T TOUCH)
    Tmp := FItems[AIndex];
    Move(FItems[AIndex + 1], FItems[AIndex], SizeOf(T) * (Count - AIndex - 1));
    Move(Tmp, FItems[MaxIndex], SizeOf(T));
  end;
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

procedure TArray<T>.ForEach(AFunc: TForEachProcStatic<T>);
var
  AItem: T;
begin
  for AItem in Self do
    AFunc(AItem);
end;

procedure TArray<T>.ForEach(AFunc: TForEachProcRef<T>);
var
  AItem: T;
begin
  for AItem in Self do
    AFunc(AItem);
end;

procedure TArray<T>.ForEach(AFunc: TForEachProc<T>);
var
  AItem: T;
begin
  for AItem in Self do
    AFunc(AItem);
end;

function TArray<T>.ForEach<R>(AFunc: TForEachFuncStatic<T, R>): TArray<R>;
var
  I: Integer;
begin
  Result := TArray<R>.Create;
  Result.Capacity := Count;
  Result.ForceCount(Count);
  for I := 0 to MaxIndex do
    Result[I] := AFunc(Self[I]);
end;

function TArray<T>.ForEach<R>(AFunc: TForEachFuncRef<T, R>): TArray<R>;
var
  I: Integer;
begin
  Result := TArray<R>.Create;
  Result.Capacity := Count;
  Result.ForceCount(Count);
  for I := 0 to MaxIndex do
    Result[I] := AFunc(Self[I]);
end;

function TArray<T>.ForEach<R>(AFunc: TForEachFunc<T, R>): TArray<R>;
var
  I: Integer;
begin
  Result := TArray<R>.Create;
  Result.Capacity := Count;
  Result.ForceCount(Count);
  for I := 0 to MaxIndex do
    Result[I] := AFunc(Self[I]);
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
  Item: T;
begin
  Result := TArray<T>(CreateSame);
  for Item in Self do
    if AFunc(Item) then
      Result.Add(Item);
end;

function TArray<T>.FindAsArray(AFunc: TFindFuncRef<T>): TArray<T>;
var
  Item: T;
begin
  Result := TArray<T>(CreateSame);
  for Item in Self do
    if AFunc(Item) then
      Result.Add(Item);
end;

function TArray<T>.FindAsArray(AFunc: TFindFunc<T>): TArray<T>;
var
  Item: T;
begin
  Result := TArray<T>(CreateSame);
  for Item in Self do
    if AFunc(Item) then
      Result.Add(Item);
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
  AArray.Capacity := Count;
  AArray.FCount := Count;
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
begin
  Add(AItems.GetEnumerator);
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

function TArray<T>.Reader: TReader;
begin
  Result := TReader(Self);
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

procedure TArray<T>.Add(AItems: IIterator<T>);
begin
  while AItems.MoveNext do
    Add(AItems.Current);
end;

{ TIntArrayHelper }

function TIntArrayHelper.Sum: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I in Self do
    Result := Result + I;
end;

function TIntArrayHelper.Difference: Integer;
begin
  Result := Bounds.Length;
end;

function TIntArrayHelper.Average: Single;
begin
  Result := Sum / Count;
end;

function TIntArrayHelper.Min: Integer;
var
  I: Integer;
begin
  if Empty then
    raise EArrayEmpty.Create;
  Result := Self[0];
  for I := 1 to MaxIndex do
    Result := System.Math.Min(Result, Self[I]);
end;

function TIntArrayHelper.Max: Integer;
var
  I: Integer;
begin
  if Empty then
    raise EArrayEmpty.Create;
  Result := Self[0];
  for I := 1 to MaxIndex do
    Result := System.Math.Max(Result, Self[I]);
end;

function TIntArrayHelper.Bounds: TIntBounds1;
var
  I: Integer;
begin
  if Empty then
    raise EArrayEmpty.Create;
  Result := Self[0];
  for I := 0 to MaxIndex do
  begin
    if Self[I] < Result.C1 then
      Result.C1 := Self[I]
    else if Self[I] > Result.C2 then
      Result.C2 := Self[I];
  end;
end;

{ TRefArray<T> }

function TBaseRefArray<T>.Equals(Obj: TObject): Boolean;
var
  I: Integer;
begin
  if Obj.ClassType <> ClassType then
    Exit(False);
  if TBaseRefArray<T>(Obj).Count <> Count then
    Exit(False);
  for I := 0 to MaxIndex do
    if not Self[I].Equals(TBaseRefArray<T>(Obj)[I]) then
      Exit(False);
  Result := True;
end;

function TBaseRefArray<T>.Find(AItem: T): Integer;
begin
  Result := FindFirstIndex(
    function(ACurrent: T): Boolean
    begin
      Result := ACurrent = AItem;
    end);
end;

procedure TBaseRefArray<T>.ItemRemoved(AIndex: Integer);
begin
  Self[AIndex].Free;
end;

class function TBaseRefArray<T>.ItemToString(AItem: T): string;
begin
  Result := AItem.ToString;
end;

function TBaseRefArray<T>.ShouldFreeItems: Boolean;
begin
  Result := OwnsObjects;
end;

{ TRefArray<T> }

function TRefArray<T>.Copy: TRefArray<T>;
begin
  Result := TRefArray<T>(CreateCopy);
end;

constructor TRefArray<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  OwnsObjects := AOwnsObjects;
end;

function TRefArray<T>.FindAsArray(AFunc: TFindFuncStatic<T>): TRefArray<T>;
begin

end;

function TRefArray<T>.FindAsArray(AFunc: TFindFunc<T>): TRefArray<T>;
begin

end;

function TRefArray<T>.FindAsArray(AFunc: TFindFuncRef<T>): TRefArray<T>;
begin

end;

function TRefArray<T>.GetOwnsObjects: Boolean;
begin
  Result := FOwnsObjects;
end;

function TRefArray<T>.Reader: TReader;
begin
  Result := TReader(Self);
end;

procedure TRefArray<T>.SetOwnsObjects(const Value: Boolean);
begin
  FOwnsObjects := Value;
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

procedure TInterfaceArray<T>.Remove(AItem: T);
var
  I: Integer;
begin
  I := Find(AItem);
  if I = -1 then
    raise EArrayItemNotFound.Create;
  RemoveAt(I);
end;

procedure TInterfaceArray<T>.Remove(AItem: TObject);
var
  I: Integer;
begin
  I := Find(AItem);
  if I = -1 then
    raise EArrayItemNotFound.Create;
  RemoveAt(I);
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

function TStack.CreateSame: TStack;
begin
  Result := TStackClass(ClassType).Create;
end;

destructor TStack.Destroy;
begin
  FArray.Free;
  inherited;
end;

constructor TStack.Create;
begin
  FArray := CreateArray;
end;

function TStack.CreateCopy: TStack;
begin
  Result := TStackClass(ClassType).Create;
  FArray.CopyTo(Result.FArray);
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
  Result := TArray<T>(FArray).Last;
end;

procedure TStack<T>.SetTop(const Value: T);
begin
  TArray<T>(FArray).Last := Value;
end;

function TStack<T>.CreateArray: TArray;
begin
  Result := TArray<T>.Create;
end;

procedure TStack<T>.Push(AItem: T);
begin
  TArray<T>(FArray).Add(AItem);
end;

function TStack<T>.Pop: T;
begin
  Result := Top;
  FArray.RemoveLast;
end;

function TStack<T>.Count: Integer;
begin
  Result := FArray.Count;
end;

function TStack<T>.Empty: Boolean;
begin
  Result := FArray.Empty;
end;

function TStack<T>.Copy: TStack<T>;
begin
  Result := TStack<T>(CreateCopy);
end;

{ TValueHasher<K> }

class function TValueHasher<K>.CanIndex(AKey: K): Boolean;
begin
  Result := True;
end;

{ TRefStack<T> }

function TRefStack<T>.Copy: TRefStack<T>;
begin
  Result := TRefStack<T>(CreateCopy);
end;

constructor TRefStack<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  OwnsObjects := AOwnsObjects;
end;

function TRefStack<T>.CreateArray: TArray;
begin
  Result := TRefArray<T>.Create;
end;

function TRefStack<T>.GetOwnsObjects: Boolean;
begin
  Result := TRefArray<T>(FArray).OwnsObjects;
end;

procedure TRefStack<T>.SetOwnsObjects(const Value: Boolean);
begin
  TRefArray<T>(FArray).OwnsObjects := Value;
end;

{ TRefArrayOwnLinked<T> }

constructor TRefArrayOwnLinked<T>.Create(AOwnsObjectsLink: PBoolean);
begin
  inherited Create;
  FOwnsObjectsLink := AOwnsObjectsLink;
end;

function TRefArrayOwnLinked<T>.GetOwnsObjects: Boolean;
begin
  Result := FOwnsObjectsLink^;
end;

{ TFindableArray<T> }

function TFindableArray<T>.Contains(AItem: T): Boolean;
begin
  Result := Find(AItem) <> -1;
end;

function TFindableArray<T>.Reader: TReader;
begin
  Result := TReader(Self);
end;

procedure TFindableArray<T>.Remove(AItem: T);
var
  I: Integer;
begin
  I := Find(AItem);
  if I = -1 then
    raise EArrayItemNotFound.Create;
  RemoveAt(I);
end;

{ TBaseRefPairArray<K, V> }

procedure TBaseRefPairArray<K, V>.ItemRemoved(AIndex: Integer);
begin
  Self[AIndex].Key.Free;
end;

function TBaseRefPairArray<K, V>.ShouldFreeItems: Boolean;
begin
  Result := OwnsKeys;
end;

{ TBaseToRefPairArray<K, V> }

procedure TBaseToRefPairArray<K, V>.ItemRemoved(AIndex: Integer);
begin
  Self[AIndex].Value.Free;
end;

function TBaseToRefPairArray<K, V>.ShouldFreeItems: Boolean;
begin
  Result := OwnsValues;
end;

{ TRefPairArray<K, V> }

function TRefPairArray<K, V>.Copy: TRefPairArray<K, V>;
begin
  Result := TRefPairArray<K, V>(CreateCopy);
end;

constructor TRefPairArray<K, V>.Create(AOwnsKeys: Boolean);
begin
  inherited Create;
  OwnsKeys := AOwnsKeys;
end;

function TRefPairArray<K, V>.FindAsArray(AFunc: TFindFuncStatic<TPair<K, V>>): TRefPairArray<K, V>;
begin
  Result := TRefPairArray<K, V>(inherited FindAsArray(AFunc));
end;

function TRefPairArray<K, V>.FindAsArray(AFunc: TFindFunc<TPair<K, V>>): TRefPairArray<K, V>;
begin
  Result := TRefPairArray<K, V>(inherited FindAsArray(AFunc));
end;

function TRefPairArray<K, V>.FindAsArray(AFunc: TFindFuncRef<TPair<K, V>>): TRefPairArray<K, V>;
begin
  Result := TRefPairArray<K, V>(inherited FindAsArray(AFunc));
end;

function TRefPairArray<K, V>.GetOwnsKeys: Boolean;
begin
  Result := FOwnsKeys;
end;

procedure TRefPairArray<K, V>.SetOwnsKeys(const Value: Boolean);
begin
  FOwnsKeys := Value;
end;

{ TRefPairArrayOwnLinked<K, V> }

constructor TRefPairArrayOwnLinked<K, V>.Create(AOwnsKeysLink: PBoolean);
begin
  inherited Create;
  FOwnsKeysLink := AOwnsKeysLink;
end;

function TRefPairArrayOwnLinked<K, V>.GetOwnsKeys: Boolean;
begin
  Result := FOwnsKeysLink^;
end;

{ TToRefPairArray<K, V> }

function TToRefPairArray<K, V>.Copy: TToRefPairArray<K, V>;
begin
  Result := TToRefPairArray<K, V>(CreateCopy);
end;

constructor TToRefPairArray<K, V>.Create(AOwnsValues: Boolean);
begin
  inherited Create;
  OwnsValues := AOwnsValues;
end;

function TToRefPairArray<K, V>.FindAsArray(AFunc: TFindFuncStatic<TPair<K, V>>): TToRefPairArray<K, V>;
begin
  Result := TToRefPairArray<K, V>(inherited FindAsArray(AFunc));
end;

function TToRefPairArray<K, V>.FindAsArray(AFunc: TFindFunc<TPair<K, V>>): TToRefPairArray<K, V>;
begin
  Result := TToRefPairArray<K, V>(inherited FindAsArray(AFunc));
end;

function TToRefPairArray<K, V>.FindAsArray(AFunc: TFindFuncRef<TPair<K, V>>): TToRefPairArray<K, V>;
begin
  Result := TToRefPairArray<K, V>(inherited FindAsArray(AFunc));
end;

function TToRefPairArray<K, V>.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

procedure TToRefPairArray<K, V>.SetOwnsValues(const Value: Boolean);
begin
  FOwnsValues := Value;
end;

{ TToRefPairArrayOwnLinked<K, V> }

constructor TToRefPairArrayOwnLinked<K, V>.Create(AOwnsValuesLink: PBoolean);
begin
  inherited Create;
  FOwnsValuesLink := AOwnsValuesLink;
end;

function TToRefPairArrayOwnLinked<K, V>.GetOwnsValues: Boolean;
begin
  Result := FOwnsValuesLink^;
end;

{ TBaseRefRefPairArray<K, V> }

procedure TBaseRefRefPairArray<K, V>.ItemRemoved(AIndex: Integer);
begin
  if OwnsKeys then
    Self[AIndex].Key.Free;
  if OwnsValues then
    Self[AIndex].Value.Free;
end;

class function TBaseRefRefPairArray<K, V>.ItemToString(AItem: TPair<K, V>): string;
begin
  Result := AItem.Key.ToString + ' = ' + AItem.Value.ToString;
end;

function TBaseRefRefPairArray<K, V>.ShouldFreeItems: Boolean;
begin
  Result := OwnsKeys or OwnsValues;
end;

{ TRefRefPairArray<K, V> }

function TRefRefPairArray<K, V>.Copy: TRefRefPairArray<K, V>;
begin
  Result := TRefRefPairArray<K, V>(CreateCopy);
end;

constructor TRefRefPairArray<K, V>.Create(AOwnsKeys, AOwnsValues: Boolean);
begin
  inherited Create;
  OwnsKeys := AOwnsKeys;
  OwnsValues := AOwnsValues;
end;

function TRefRefPairArray<K, V>.FindAsArray(AFunc: TFindFuncStatic<TPair<K, V>>): TRefRefPairArray<K, V>;
begin
  Result := TRefRefPairArray<K, V>(inherited FindAsArray(AFunc));
end;

function TRefRefPairArray<K, V>.FindAsArray(AFunc: TFindFunc<TPair<K, V>>): TRefRefPairArray<K, V>;
begin
  Result := TRefRefPairArray<K, V>(inherited FindAsArray(AFunc));
end;

function TRefRefPairArray<K, V>.FindAsArray(AFunc: TFindFuncRef<TPair<K, V>>): TRefRefPairArray<K, V>;
begin
  Result := TRefRefPairArray<K, V>(inherited FindAsArray(AFunc));
end;

function TRefRefPairArray<K, V>.GetOwnsKeys: Boolean;
begin
  Result := FOwnsKeys;
end;

function TRefRefPairArray<K, V>.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

procedure TRefRefPairArray<K, V>.SetOwnsKeys(const Value: Boolean);
begin
  FOwnsKeys := Value;
end;

procedure TRefRefPairArray<K, V>.SetOwnsValues(const Value: Boolean);
begin
  FOwnsValues := Value;
end;

{ TRefRefPairArrayOwnLinked<K, V> }

constructor TRefRefPairArrayOwnLinked<K, V>.Create(AOwnsKeysLink, AOwnsValuesLink: PBoolean);
begin
  inherited Create;
  FOwnsKeysLink := AOwnsKeysLink;
  FOwnsValuesLink := AOwnsValuesLink;
end;

function TRefRefPairArrayOwnLinked<K, V>.GetOwnsKeys: Boolean;
begin
  Result := FOwnsKeysLink^;
end;

function TRefRefPairArrayOwnLinked<K, V>.GetOwnsValues: Boolean;
begin
  Result := FOwnsValuesLink^;
end;

{ TLinkedInterfaceArray<I, T> }

function TLinkedInterfaceArray<I, T>.Count: Integer;
begin
  Result := FLinkedArray.Count;
end;

function TLinkedInterfaceArray<I, T>.CountOptimized: Boolean;
begin
  Result := FLinkedArray.CountOptimized;
end;

constructor TLinkedInterfaceArray<I, T>.Create(ALinkedArray: TArray<T>);
begin
  FLinkedArray := ALinkedArray;
end;

function TLinkedInterfaceArray<I, T>.GetEnumerator: IIterator<I>;
begin
  Result := TIterator.Create(FLinkedArray);
end;

{ TLinkedInterfaceArray<I, T>.TIterator }

constructor TLinkedInterfaceArray<I, T>.TIterator.Create(ALinkedArray: TArray<T>);
begin
  FLinkedArray := ALinkedArray;
  FIndex := -1;
end;

function TLinkedInterfaceArray<I, T>.TIterator.GetCurrent: I;
begin
  Result := FLinkedArray[FIndex];
end;

function TLinkedInterfaceArray<I, T>.TIterator.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := FIndex < FLinkedArray.Count;
end;

{ TObjectArray<T> }

constructor TObjectArray<T>.Create;
begin
  inherited Create(True);
end;

{ TObjectStack<T> }

constructor TObjectStack<T>.Create;
begin
  inherited Create(True);
end;

{ TArray.TReader }

function TArray.TReader.GetCapacity: Integer;
begin
  Result := TArray(Self).Capacity;
end;

function TArray.TReader.GetGrowAmount: Integer;
begin
  Result := TArray(Self).GrowAmount;
end;

function TArray.TReader.GetShrinkRetain: Integer;
begin
  Result := TArray(Self).ShrinkRetain;
end;

function TArray.TReader.Count: Integer;
begin
  Result := TArray(Self).Count;
end;

function TArray.TReader.CountOptimized: Boolean;
begin
  Result := TArray(Self).CountOptimized;
end;

function TArray.TReader.MaxIndex: Integer;
begin
  Result := TArray(Self).MaxIndex;
end;

function TArray.TReader.Empty: Boolean;
begin
  Result := TArray(Self).Empty;
end;

procedure TArray.TReader.RangeCheckException(AIndex: Integer);
begin
  TArray(Self).RangeCheckException(AIndex);
end;

function TArray.TReader.RangeCheck(AIndex: Integer): Boolean;
begin
  Result := TArray(Self).RangeCheck(AIndex);
end;

function TArray.TReader.Copy: TArray;
begin
  Result := TArray(Self).Copy;
end;

{ TArray<T>.TReader }

function TArray<T>.TReader.BinarySearch(AItem: T; AFunc: TCompareFunc<T>): Integer;
begin
  Result := TArray<T>(Self).BinarySearch(AItem, AFunc);
end;

function TArray<T>.TReader.BinarySearch(AItem: T; AFunc: TCompareFuncRef<T>): Integer;
begin
  Result := TArray<T>(Self).BinarySearch(AItem, AFunc);
end;

function TArray<T>.TReader.BinarySearch(AItem: T; AFunc: TCompareFuncStatic<T>): Integer;
begin
  Result := TArray<T>(Self).BinarySearch(AItem, AFunc);
end;

function TArray<T>.TReader.Copy: TArray<T>;
begin
  Result := TArray<T>(Self).Copy;
end;

function TArray<T>.TReader.FindAsArray(AFunc: TFindFuncRef<T>): TArray<T>;
begin
  Result := TArray<T>(Self).FindAsArray(AFunc);
end;

function TArray<T>.TReader.FindAsArray(AFunc: TFindFunc<T>): TArray<T>;
begin
  Result := TArray<T>(Self).FindAsArray(AFunc);
end;

function TArray<T>.TReader.FindAsArray(AFunc: TFindFuncStatic<T>): TArray<T>;
begin
  Result := TArray<T>(Self).FindAsArray(AFunc);
end;

function TArray<T>.TReader.FindFirst(AFunc: TFindFuncStatic<T>): T;
begin
  Result := TArray<T>(Self).FindFirst(AFunc);
end;

function TArray<T>.TReader.FindFirst(AFunc: TFindFuncRef<T>): T;
begin
  Result := TArray<T>(Self).FindFirst(AFunc);
end;

function TArray<T>.TReader.FindFirst(AFunc: TFindFunc<T>): T;
begin
  Result := TArray<T>(Self).FindFirst(AFunc);
end;

function TArray<T>.TReader.FindFirstIndex(AFunc: TFindFuncRef<T>): Integer;
begin
  Result := TArray<T>(Self).FindFirstIndex(AFunc);
end;

function TArray<T>.TReader.FindFirstIndex(AFunc: TFindFuncStatic<T>): Integer;
begin
  Result := TArray<T>(Self).FindFirstIndex(AFunc);
end;

function TArray<T>.TReader.FindFirstIndex(AFunc: TFindFunc<T>): Integer;
begin
  Result := TArray<T>(Self).FindFirstIndex(AFunc);
end;

function TArray<T>.TReader.FindIndexAsArray(AFunc: TFindFunc<T>): TIntArray;
begin
  Result := TArray<T>(Self).FindIndexAsArray(AFunc);
end;

function TArray<T>.TReader.FindIndexAsArray(AFunc: TFindFuncRef<T>): TIntArray;
begin
  Result := TArray<T>(Self).FindIndexAsArray(AFunc);
end;

function TArray<T>.TReader.FindIndexAsArray(AFunc: TFindFuncStatic<T>): TIntArray;
begin
  Result := TArray<T>(Self).FindIndexAsArray(AFunc);
end;

function TArray<T>.TReader.FindLast(AFunc: TFindFunc<T>): T;
begin
  Result := TArray<T>(Self).FindLast(AFunc);
end;

function TArray<T>.TReader.FindLast(AFunc: TFindFuncRef<T>): T;
begin
  Result := TArray<T>(Self).FindLast(AFunc);
end;

function TArray<T>.TReader.FindLast(AFunc: TFindFuncStatic<T>): T;
begin
  Result := TArray<T>(Self).FindLast(AFunc);
end;

function TArray<T>.TReader.FindLastIndex(AFunc: TFindFuncStatic<T>): Integer;
begin
  Result := TArray<T>(Self).FindLastIndex(AFunc);
end;

function TArray<T>.TReader.FindLastIndex(AFunc: TFindFunc<T>): Integer;
begin
  Result := TArray<T>(Self).FindLastIndex(AFunc);
end;

procedure TArray<T>.TReader.ForEach(AFunc: TForEachProcStatic<T>);
begin
  TArray<T>(Self).ForEach(AFunc);
end;

procedure TArray<T>.TReader.ForEach(AFunc: TForEachProcRef<T>);
begin
  TArray<T>(Self).ForEach(AFunc);
end;

procedure TArray<T>.TReader.ForEach(AFunc: TForEachProc<T>);
begin
  TArray<T>(Self).ForEach(AFunc);
end;

function TArray<T>.TReader.ForEach<R>(AFunc: TForEachFuncStatic<T, R>): TArray<R>;
begin
  Result := TArray<T>(Self).ForEach<R>(AFunc);
end;

function TArray<T>.TReader.ForEach<R>(AFunc: TForEachFuncRef<T, R>): TArray<R>;
begin
  Result := TArray<T>(Self).ForEach<R>(AFunc);
end;

function TArray<T>.TReader.ForEach<R>(AFunc: TForEachFunc<T, R>): TArray<R>;
begin
  Result := TArray<T>(Self).ForEach<R>(AFunc);
end;

function TArray<T>.TReader.FindLastIndex(AFunc: TFindFuncRef<T>): Integer;
begin
  Result := TArray<T>(Self).FindLastIndex(AFunc);
end;

function TArray<T>.TReader.GetEnumerator: IIterator<T>;
begin
  Result := TArray<T>(Self).GetEnumerator;
end;

function TArray<T>.TReader.GetFirst: T;
begin
  Result := TArray<T>(Self).First;
end;

function TArray<T>.TReader.GetItem(I: Integer): T;
begin
  Result := TArray<T>(Self)[I];
end;

function TArray<T>.TReader.GetLast: T;
begin
  Result := TArray<T>(Self).Last;
end;

function TArray<T>.TReader.InReverse: TReverseWrapper;
begin
  Result := TArray<T>(Self).InReverse;
end;

class function TArray<T>.TReader.ItemToString(AItem: T): string;
begin
  Result := TArray<T>(Pointer(Self)).ItemToString(AItem);
end;

procedure TArray<T>.TReader.SetFirst(const Value: T);
begin
  TArray<T>(Self).First := Value;
end;

procedure TArray<T>.TReader.SetItem(I: Integer; const Value: T);
begin
  TArray<T>(Self)[I] := Value;
end;

procedure TArray<T>.TReader.SetLast(const Value: T);
begin
  TArray<T>(Self).Last := Value;
end;

function TArray<T>.TReader.Sorted(AFunc: TCompareFunc<T>): Boolean;
begin
  Result := TArray<T>(Self).Sorted(AFunc);
end;

function TArray<T>.TReader.Sorted(AFunc: TCompareFuncRef<T>): Boolean;
begin
  Result := TArray<T>(Self).Sorted(AFunc);
end;

function TArray<T>.TReader.Sorted(AFunc: TCompareFuncStatic<T>): Boolean;
begin
  Result := TArray<T>(Self).Sorted(AFunc);
end;

function TArray<T>.TReader.ToString: string;
begin
  Result := TArray<T>(Self).ToString;
end;

{ TSingleArrayHelper }

function TSingleArrayHelper.Sum: Single;
var
  I: Single;
begin
  Result := 0;
  for I in Self do
    Result := Result + I;
end;

function TSingleArrayHelper.Difference: Single;
begin
  Result := Bounds.Length;
end;

function TSingleArrayHelper.Average: Single;
begin
  Result := Sum / Count;
end;

function TSingleArrayHelper.Min: Single;
var
  I: Integer;
begin
  if Empty then
    raise EArrayEmpty.Create;
  Result := Self[0];
  for I := 1 to MaxIndex do
    Result := System.Math.Min(Result, Self[I]);
end;

function TSingleArrayHelper.Max: Single;
var
  I: Integer;
begin
  if Empty then
    raise EArrayEmpty.Create;
  Result := Self[0];
  for I := 1 to MaxIndex do
    Result := System.Math.Max(Result, Self[I]);
end;

function TSingleArrayHelper.Bounds: TBounds1;
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

{ TFindableArray<T>.TReader }

function TFindableArray<T>.TReader.Contains(AItem: T): Boolean;
begin
  Result := TFindableArray<T>(Self).Contains(AItem);
end;

function TFindableArray<T>.TReader.Find(AItem: T): Integer;
begin
  Result := TFindableArray<T>(Self).Find(AItem);
end;

{ TRefArray<T>.TReader }

function TRefArray<T>.TReader.Copy: TRefArray<T>;
begin
  Result := TRefArray<T>(Self).Copy;
end;

{ TToObjectPairArray<K, V> }

constructor TToObjectPairArray<K, V>.Create;
begin
  inherited Create(True);
end;

end.
