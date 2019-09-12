unit Pengine.ICollections;

interface

uses
  System.SysUtils,
  System.Hash,

  Pengine.HashPrimes;

type

  // --- General ---

  EDefaultError = class(Exception);

  TDefault = class
  private
    class function HashString(A: UnicodeString): Cardinal; overload; static; inline;
    {$IFDEF WINDOWS}
    class function HashString(A: AnsiString): Cardinal; overload; static; inline;
    {$ENDIF}
  public
    class function Equate<T>(A, B: T): Boolean; static; inline;
    class function Compare<T>(A, B: T): Boolean; static; inline;
    class function Hash<T>(A: T): Cardinal; static; inline;

  end;

  TPair<K, V> = record
  private
    FKey: K;
    FValue: V;

  public
    constructor Create(AKey: K; AValue: V);

    property Key: K read FKey;
    property Value: V read FValue;

  end;

  THashValue<T> = record
  private
    FHash: Cardinal;
    FValue: T;

  public
    constructor Create(AHash: Cardinal; AValue: T);

    property Hash: Cardinal read FHash;
    property Value: T read FValue;

  end;

  // --- Collection Interfaces ---

  /// <summary>Defines logic to iterate over items of an IIterable&lt;T&gt;.</summary>
  IIterator<T> = interface
    function GetCurrent: T;

    function MoveNext: Boolean;
    property Current: T read GetCurrent;

  end;

  IIterate<T> = interface;

  /// <summary>Iterable using a for-in loop.</summary>
  IIterable<T> = interface
    function GetEnumerator: IIterator<T>;
    function Iterate: IIterate<T>;

  end;

  IMap<K, V> = interface;

  /// <summary>Serves as a wrapper for iterate operations that require generic parameters.</summary>
  TGenericWrapper<T> = record
  private
    FIterate: IIterate<T>;

  public
    constructor Create(AIterate: IIterate<T>);

    function Map<R>(AFunc: TFunc<T, R>): IIterate<R>;
    function Zip<R>(AIterable: IIterable<T>; AFunc: TFunc<T, T, R>): IIterate<R>; overload;
    function Zip<U, R>(AIterable: IIterable<U>; AFunc: TFunc<T, U, R>): IIterate<R>; overload;
    function ToMapK<V>(AIterable: IIterable<V>): IMap<T, V>;
    function ToMapV<V>(AIterable: IIterable<V>): IMap<V, T>;
    function OfType<R: class>: IIterate<R>;

  end;

  IList<T> = interface;
  ISet<T> = interface;

  /// <summary>Defines various operations that can be performed on an iteration.</summary>
  IIterate<T> = interface(IIterable<T>)
    function GetItem(AIndex: Integer): T;

    /// <summary>Counts all elements in the iteration.</summary>
    function Count: Integer; overload;
    /// <summary>Counts all elements in the iteration, that meet a certain condition.</summary>
    function Count(APredicate: TPredicate<T>): Integer; overload;
    /// <summary>Returns, wether the iteration is empty.</summary>
    function Empty: Boolean;
    /// <summary>Returns, wether the iteration has at least one element.</summary>
    function Any: Boolean; overload;
    /// <summary>Returns true, if at least one element meets a certain condition.</summary>
    function Any(APredicate: TPredicate<T>): Boolean; overload;
    /// <summary>Returns true, if all elements meet a certain condition.</summary>
    function All(APredicate: TPredicate<T>): Boolean;

    /// <summary>Returns true, if the iteration has at least a certain amount of items.</summary>
    function HasAtLeast(ACount: Integer): Boolean; overload;
    /// <summary>Returns true, if the iteration has at least a certain amount of items, that meet a condition.</summary>
    function HasAtLeast(ACount: Integer; APredicate: TPredicate<T>): Boolean; overload;
    /// <summary>Returns true, if the iteration has at most a certain amount of items.</summary>
    function HasAtMost(ACount: Integer): Boolean; overload;
    /// <summary>Returns true, if the iteration has at most a certain amount of items, that meet a condition.</summary>
    function HasAtMost(ACount: Integer; APredicate: TPredicate<T>): Boolean; overload;
    /// <summary>Returns true, if the iteration has exactly as many items, that meet a condition, as specified.</summary>
    function HasExactly(ACount: Integer): Boolean; overload;
    /// <summary>Returns true, if the iteration has exactly as many items, that meet a condition, as specified.</summary>
    function HasExactly(ACount: Integer; APredicate: TPredicate<T>): Boolean; overload;

    /// <summary>Reduces all values in the iteration into a single value using an accumulator.</summary>
    function Reduce(AFunc: TFunc<T, T, T>): T; overload;
    /// <summary>Reduces all values in the iteration into a single value using an accumulator and a seed value.</summary>
    function Reduce(AFunc: TFunc<T, T, T>; ASeed: T): T; overload;

    /// <summary>Returns a single item at the given position from the iteration.</summary>
    property Items[AIndex: Integer]: T read GetItem;
    /// <summary>Returns the very first item of the iteration.</summary>
    function First: T;
    /// <summary>Returns the very last item of the iteration.</summary>
    function Last: T;
    /// <summary>Creates a new iteration over the given range of the current iteration.</summary>
    function Range(AIndex, ACount: Integer): IIterate<T>;
    /// <summary>Creates a new iteration, that only contains the elements, that meet a certain condition.</summary>
    function Where(APredicate: TPredicate<T>): IIterate<T>;
    /// <summary>Creates a new iteration, that stops after the given count of items.</summary>
    function Take(ACount: Integer): IIterate<T>;
    /// <summary>Creates a new iteration, that only contains elements, as long as a certain condition is met.</summary>
    function TakeWhile(APredicate: TPredicate<T>): IIterate<T>;
    /// <summary>Creates a new iteration, that starts after the given count of items.</summary>
    function Skip(ACount: Integer): IIterate<T>;
    /// <summary>Creates a new iteration, that only contains elements, once a certain condition is met.</summary>
    function SkipUntil(APredicate: TPredicate<T>): IIterate<T>;
    /// <summary>Appends the given iterable to the current iteration.</summary>
    function Concat(AIterable: IIterable<T>): IIterate<T>;
    /// <summary>Appends a single item to the iteration.</summary>
    function Append(AItem: T): IIterate<T>;
    /// <summary>Prepends a single item to the iteration.</summary>
    function Prepend(AItem: T): IIterate<T>;
    /// <summary>Maps all elements to a different value using a conversion function.</summary>
    function Map(AFunc: TFunc<T, T>): IIterate<T>;
    /// <summary>Combines the iteration with another iterable and uses a combination function to combine their items.</summary>
    function Zip(AIterable: IIterable<T>; AFunc: TFunc<T, T, T>): IIterate<T>;

    /// <summary>Returns a wrapper for operations, that require additional generic type information.</summary>
    function Generic: TGenericWrapper<T>;

    /// <summary>Creates a new list out of this iteration.</summary>
    function ToList: IList<T>;
    /// <summary>Creates a new set out of this iteration.</summary>
    function ToSet: ISet<T>;

  end;

  /// <summary>A collection, which is readonly.</summary>
  IReadonlyCollection<T> = interface(IIterable<T>)
    function GetCount: Integer;

    property Count: Integer read GetCount;
    function Empty: Boolean;
    function Contains(AItem: T): Boolean;

  end;

  /// <summary>A list, which is readonly.</summary>
  IReadonlyList<T> = interface(IReadonlyCollection<T>)
    function GetItem(AIndex: Integer): T;
    function GetMaxIndex: Integer;
    function GetFirst: T;
    function GetLast: T;

    property Items[AIndex: Integer]: T read GetItem; default;

    property MaxIndex: Integer read GetMaxIndex;
    property First: T read GetFirst;
    property Last: T read GetLast;

    function IndexOf(AItem: T): Integer;

    function Reverse: IIterate<T>;

  end;

  /// <summary>A set, which is readonly.</summary>
  IReadonlySet<T> = interface(IReadonlyCollection<T>)
    function GetItem(AItem: T): Boolean;

    property Items[AItem: T]: Boolean read GetItem; default;

  end;

  /// <summary>A map, which is readonly.</summary>
  IReadonlyMap<K, V> = interface(IReadonlyCollection < TPair < K, V >> )
    function GetItem(AKey: K): V;

    property Items[AKey: K]: V read GetItem; default;

    function Keys: IReadonlyCollection<K>;
    function Values: IReadonlyCollection<V>;

    function ContainsKey(AKey: K): Boolean;
    function ContainsValue(AValue: V): Boolean;
    function Get(AKey: K; out AValue: V): Boolean;
    function GetKey(AValue: V; out AKey: K): Boolean;
    function GetKeys(AValue: V): IIterable<K>;

  end;

  /// <summary>A collection of items, that are not necessarily ordered in any way.</summary>
  ICollection<T> = interface(IIterable<T>)
    function GetCount: Integer;

    property Count: Integer read GetCount;
    function Empty: Boolean;

    function Contains(AItem: T): Boolean;

    function Add(AItem: T): Boolean;
    function Remove(AItem: T): Boolean;
    function Extract(AItem: T): T;
    procedure Clear;

    procedure AddRange(AItems: array of T); overload;
    procedure AddRange(AItems: IIterable<T>); overload;
    procedure AddRange(AItems: IIterator<T>); overload;

    function ReadonlyCollection: IReadonlyCollection<T>;

  end;

  /// <summary>Serves as a base for both IList<T> and ISortedList<T>.</summary>
  IListBase<T> = interface(ICollection<T>)
    function GetCapacity: Integer;
    procedure SetCapacity(const Value: Integer);
    function GetCompare: TFunc<T, T, Boolean>;
    function GetEquate: TFunc<T, T, Boolean>;
    procedure SetCompare(const Value: TFunc<T, T, Boolean>);
    procedure SetEquate(const Value: TFunc<T, T, Boolean>);

    function GetItem(AIndex: Integer): T;
    function GetMaxIndex: Integer;
    function GetFirst: T;
    function GetLast: T;

    property Items[AIndex: Integer]: T read GetItem; default;
    property Capacity: Integer read GetCapacity write SetCapacity;

    property MaxIndex: Integer read GetMaxIndex;
    property First: T read GetFirst;
    property Last: T read GetLast;

    function IndexOf(AItem: T): Integer;

    procedure RemoveAt(AIndex: Integer);
    procedure RemoveRange(AIndex, ACount: Integer);

    function Extract(AIndex: Integer): T;

    function ReadonlyList: IReadonlyList<T>;

    property Equate: TFunc<T, T, Boolean> read GetEquate write SetEquate;
    property Compare: TFunc<T, T, Boolean> read GetCompare write SetCompare;

    function Reverse: IIterate<T>;

    function DataPointer: Pointer;

  end;

  /// <summary>An ordered collection of items.</summary>
  IList<T> = interface(IListBase<T>)
    procedure SetItem(AIndex: Integer; AItem: T);

    property Items[AIndex: Integer]: T read GetItem write SetItem; default;

    procedure Insert(AIndex: Integer; AItem: T);
    procedure InsertRange(AIndex: Integer; AItems: array of T); overload;
    procedure InsertRange(AIndex: Integer; AItems: IIterable<T>); overload;
    procedure InsertRange(AIndex: Integer; AItems: IIterator<T>); overload;

    procedure Move(AFrom, ATo: Integer); overload;
    procedure Move(AItem: T; AIndex: Integer); overload;

    procedure Swap(AIndex1, AIndex2: Integer); overload;
    procedure Swap(AItem: T; AIndex: Integer); overload;
    procedure Swap(AItem1, AItem2: T); overload;

    function TrySort: Boolean;
    procedure Sort;

    function Copy: IList<T>;

  end;

  /// <summary>A list, that by default owns its values and therefore frees them automatically.</summary>
  IObjectList<T> = interface(IList<T>)
    function GetOwnsValues: Boolean;
    procedure SetOwnsValues(AValue: Boolean);

    property OwnsValues: Boolean read GetOwnsValues write SetOwnsValues;

  end;

  /// <summary>A sorted collection, where the order of elements is given by a compare function.</summary>
  ISortedList<T> = interface(IListBase<T>)
    /// <summary>Finds the index, where the item needs to be positioned.</summary>
    /// <remarks>If the exact item is found, gives the index of that item.</remarks>
    function BinarySearch(AItem: T): Integer;

    function Copy: ISortedList<T>;

  end;

  /// <summary>A sorted list, that by default owns its values and therefore frees them automatically.</summary>
  ISortedObjectList<T> = interface(ISortedList<T>)
    function GetOwnsValues: Boolean;
    procedure SetOwnsValues(AValue: Boolean);

    property OwnsValues: Boolean read GetOwnsValues write SetOwnsValues;

  end;

  /// <summary>Provides hashing functionality.</summary>
  IHashCollection<T> = interface
    function GetBucketCount: Integer;
    function GetAutoRehash: Boolean;
    procedure SetAutoRehash(const Value: Boolean);
    function GetHash: TFunc<T, Cardinal>;
    procedure SetHash(const Value: TFunc<T, Cardinal>);
    function GetEquate: TFunc<T, T, Boolean>;
    procedure SetEquate(const Value: TFunc<T, T, Boolean>);

    property BucketCount: Integer read GetBucketCount;
    procedure Rehash(ABuckets: Integer);
    procedure RehashFor(ACount: Integer);
    property AutoRehash: Boolean read GetAutoRehash write SetAutoRehash;

    property Hash: TFunc<T, Cardinal> read GetHash write SetHash;
    property Equate: TFunc<T, T, Boolean> read GetEquate write SetEquate;

  end;

  /// <summary>An unordered set.</summary>
  ISet<T> = interface(ICollection<T>)
    function GetItem(AItem: T): Boolean;
    procedure SetItem(AItem: T; AValue: Boolean);
    function GetHash: TFunc<T, Cardinal>;
    procedure SetHash(const Value: TFunc<T, Cardinal>);
    function GetEquate: TFunc<T, T, Boolean>;
    procedure SetEquate(const Value: TFunc<T, T, Boolean>);

    property Items[AItem: T]: Boolean read GetItem write SetItem; default;

    function ReadonlySet: IReadonlySet<T>;
    function HashCollection: IHashCollection<T>;

    property Hash: TFunc<T, Cardinal> read GetHash write SetHash;
    property Equate: TFunc<T, T, Boolean> read GetEquate write SetEquate;

    function Copy: ISet<T>;

  end;

  /// <summary>A set, that by default owns its values and therefore frees them automatically.</summary>
  IObjectSet<T> = interface(ISet<T>)
    function GetOwnsValues: Boolean;
    procedure SetOwnsValues(AValue: Boolean);

    property OwnsValues: Boolean read GetOwnsValues write SetOwnsValues;

  end;

  /// <summary>A key-value map.</summary>
  IMap<K, V> = interface(ICollection < TPair < K, V >> )
    function GetItem(AKey: K): V;
    procedure SetItem(AKey: K; AValue: V);
    function GetHashKey: TFunc<K, Cardinal>;
    procedure SetHashKey(const Value: TFunc<K, Cardinal>);
    function GetEquateKey: TFunc<K, K, Boolean>;
    procedure SetEquateKey(const Value: TFunc<K, K, Boolean>);
    function GetEquateValue: TFunc<V, V, Boolean>;
    procedure SetEquateValue(const Value: TFunc<V, V, Boolean>);

    property Items[AKey: K]: V read GetItem write SetItem; default;

    function Keys: IReadonlyCollection<K>;
    function Values: IReadonlyCollection<V>;

    function ContainsKey(AKey: K): Boolean;
    function ContainsValue(AValue: V): Boolean;
    function Get(AKey: K; out AValue: V): Boolean;
    function GetKey(AValue: V; out AKey: K): Boolean;
    function GetKeys(AValue: V): IIterable<K>;

    function Add(AKey: K; AValue: V): Boolean; overload;
    function Remove(AKey: K): Boolean; overload;
    function Remove(AKey: K; AValue: V): Boolean; overload;
    function Extract(AKey: K): V; overload;
    function Extract(AKey: K; out AValue: V): Boolean; overload;

    function ReadonlyMap: IReadonlyMap<K, V>;
    function HashCollection: IHashCollection<K>;

    property HashKey: TFunc<K, Cardinal> read GetHashKey write SetHashKey;
    property EquateKey: TFunc<K, K, Boolean> read GetEquateKey write SetEquateKey;
    property EquateValue: TFunc<V, V, Boolean> read GetEquateValue write SetEquateValue;

    function Copy: IMap<K, V>;

  end;

  IObjectMap<K: class; V> = interface(IMap<K, V>)
    function GetOwnsKeys: Boolean;
    procedure SetOwnsKeys(AValue: Boolean);

    property OwnsKeys: Boolean read GetOwnsKeys write SetOwnsKeys;

  end;

  IToObjectMap<K; V: class> = interface(IMap<K, V>)
    function GetOwnsValues: Boolean;
    procedure SetOwnsValues(AValue: Boolean);

    property OwnsValues: Boolean read GetOwnsValues write SetOwnsValues;

  end;

  IObjectObjectMap<K, V: class> = interface(IMap<K, V>)
    function GetOwnsKeys: Boolean;
    procedure SetOwnsKeys(AValue: Boolean);
    function GetOwnsValues: Boolean;
    procedure SetOwnsValues(AValue: Boolean);

    property OwnsKeys: Boolean read GetOwnsKeys write SetOwnsKeys;
    property OwnsValues: Boolean read GetOwnsValues write SetOwnsValues;

  end;

  IStack<T> = interface
    function GetCount: Integer;
    function GetTop: T;

    property Count: Integer read GetCount;
    function Empty: Boolean;

    procedure Push(AItem: T);
    function Pop: T;
    property Top: T read GetTop;

    function PopMany: IIterable<T>;

  end;

  IQueue<T> = interface
    function GetCount: Integer;
    function GetNext: T;

    property Count: Integer read GetCount;
    function Empty: Boolean;

    procedure Enqueue(AItem: T);
    function Dequeue: T;
    property Next: T read GetNext;

    function DequeueMany: IIterable<T>;

  end;

  // --- Collection Implementations ---

  EListError = class(Exception);
  EMapError = class(Exception);
  ESetError = class(Exception);
  EIterateError = class(Exception);

  TListBase<T> = class(TInterfacedObject)
  public const

    ReduceThreshold = 64;

  private
    FItems: array of T;
    FCount: Integer;
    FEquate: TFunc<T, T, Boolean>;
    FCompare: TFunc<T, T, Boolean>;

    procedure ReduceCapacity(ACount: Integer); inline;

  protected
    constructor Create; virtual;

    procedure EnsureCapacity(ACount: Integer); inline;
    procedure RangeCheck(AIndex: Integer); inline;

    procedure DoRemoveAt(AIndex: Integer); inline;
    procedure DoRemoveRange(AIndex, ACount: Integer); inline;

    // ICollection<T>
    function GetCount: Integer;
    function GetMaxIndex: Integer;

    // IListBase<T>
    function GetItem(AIndex: Integer): T;
    function GetCapacity: Integer;
    procedure SetCapacity(const Value: Integer);

    function GetFirst: T;
    function GetLast: T;

    procedure Insert(AIndex: Integer; AItem: T);
    procedure InsertRange(AIndex: Integer; AItems: array of T); overload;
    procedure InsertRange(AIndex: Integer; AItems: IIterable<T>); overload;
    procedure InsertRange(AIndex: Integer; AItems: IIterator<T>); overload;

    function TrySortRange(LBound, RBound: Integer): Boolean;
    function TrySort: Boolean;

    function GetCompare: TFunc<T, T, Boolean>;
    function GetEquate: TFunc<T, T, Boolean>;
    procedure SetCompare(const Value: TFunc<T, T, Boolean>);
    procedure SetEquate(const Value: TFunc<T, T, Boolean>);

  public
    // IIterable<T>
    function GetEnumerator: IIterator<T>;

    // ICollection<T>
    property Count: Integer read GetCount;
    function Empty: Boolean;

    procedure Clear;

    // IListBase<T>
    property Items[AIndex: Integer]: T read GetItem; default;
    property Capacity: Integer read GetCapacity write SetCapacity;

    property MaxIndex: Integer read GetMaxIndex;
    property First: T read GetFirst;
    property Last: T read GetLast;

    procedure RemoveAt(AIndex: Integer);
    procedure RemoveRange(AIndex, ACount: Integer);

    function Extract(AIndex: Integer): T;

    property Equate: TFunc<T, T, Boolean> read GetEquate write SetEquate;
    property Compare: TFunc<T, T, Boolean> read GetCompare write SetCompare;

    function Reverse: IIterate<T>;

    function DataPointer: Pointer;

  end;

  TListBaseIterator<T> = class(TInterfacedObject)
  private
    FList: TListBase<T>;
    FCurrent: Integer;

  protected
    function GetCurrent: T;

  public
    constructor Create(AList: TListBase<T>);

  end;

  TListIterator<T> = class(TListBaseIterator<T>, IIterator<T>)
  public
    constructor Create(AList: TListBase<T>);

    function MoveNext: Boolean;

  end;

  TListReverseIterator<T> = class(TListBaseIterator<T>, IIterator<T>)
  public
    constructor Create(AList: TListBase<T>);

    function MoveNext: Boolean;

  end;

  TList<T> = class(TListBase<T>, IList<T>, IListBase<T>, ICollection<T>, IIterable<T>, IReadonlyList<T>,
    IReadonlyCollection<T>)
  private
    procedure SetItem(AIndex: Integer; AValue: T);

  public
    constructor Create; overload; override;
    constructor Create(AItems: array of T); reintroduce; overload;
    constructor Create(AIterable: IIterable<T>); reintroduce; overload;
    constructor Create(AIterator: IIterator<T>); reintroduce; overload;

    // IIterable<T>
    function GetEnumerator: IIterator<T>;
    function Iterate: IIterate<T>;

    // ICollection<T>
    function ReadonlyCollection: IReadonlyCollection<T>;

    function Contains(AItem: T): Boolean;
    function Remove(AItem: T): Boolean;
    function Extract(AItem: T): T; overload;

    // IListBase<T>
    function ReadonlyList: IReadonlyList<T>;

    // IList<T>
    property Items[AIndex: Integer]: T read GetItem write SetItem; default;

    function Add(AItem: T): Boolean;
    procedure AddRange(AItems: array of T); overload;
    procedure AddRange(AItems: IIterable<T>); overload;
    procedure AddRange(AItems: IIterator<T>); overload;

    function IndexOf(AItem: T): Integer;

    procedure Move(AFrom, ATo: Integer); overload;
    procedure Move(AItem: T; AIndex: Integer); overload;

    procedure Swap(AIndex1, AIndex2: Integer); overload;
    procedure Swap(AItem: T; AIndex: Integer); overload;
    procedure Swap(AItem1, AItem2: T); overload;

    function TrySort: Boolean;
    procedure Sort;

    function Copy: IList<T>;

  end;

  TSortedList<T> = class(TListBase<T>, ISortedList<T>, IListBase<T>, ICollection<T>, IIterable<T>, IReadonlyList<T>,
    IReadonlyCollection<T>)
  private
    function BinarySearchRec(AItem: T; ALeft, ARight: Integer): Integer;

  public
    constructor Create; overload; override;
    constructor Create(AItems: array of T); reintroduce; overload;
    constructor Create(AItems: IIterable<T>); reintroduce; overload;
    constructor Create(AItems: IIterator<T>); reintroduce; overload;

    // IIterable<T>
    function Iterate: IIterate<T>;

    // ICollection<T>
    function Add(AItem: T): Boolean;
    procedure AddRange(AItems: array of T); overload;
    procedure AddRange(AItems: IIterable<T>); overload;
    procedure AddRange(AItems: IIterator<T>); overload;
    function IndexOf(AItem: T): Integer;

    function Contains(AItem: T): Boolean;
    function Remove(AItem: T): Boolean;
    function Extract(AItem: T): T; overload;

    function ReadonlyCollection: IReadonlyCollection<T>;

    // IListBase<T>
    function ReadonlyList: IReadonlyList<T>;

    // ISortedList<T>
    function BinarySearch(AItem: T): Integer;

    function Copy: ISortedList<T>;

  end;

  TObjectList<T: class> = class(TList<T>, IObjectList<T>, IList<T>, IListBase<T>, ICollection<T>, IIterable<T>,
    IReadonlyCollection<T>, IReadonlyList<T>)
  private
    FOwnsValues: Boolean;

    function GetOwnsValues: Boolean;
    procedure SetOwnsValues(AValue: Boolean);

  public
    constructor Create; overload; override;
    destructor Destroy; override;

    property OwnsValues: Boolean read GetOwnsValues write SetOwnsValues;

    procedure Clear;
    function Remove(AItem: T): Boolean;
    procedure RemoveAt(AIndex: Integer);
    procedure RemoveRange(AIndex, ACount: Integer);

  end;

  TSortedObjectList<T: class> = class(TSortedList<T>, ISortedObjectList<T>, ISortedList<T>, IListBase<T>,
    ICollection<T>, IIterable<T>, IReadonlyCollection<T>, IReadonlyList<T>)
  private
    FOwnsValues: Boolean;

    function GetOwnsValues: Boolean;
    procedure SetOwnsValues(AValue: Boolean);

  public
    constructor Create; overload; override;
    destructor Destroy; override;

    property OwnsValues: Boolean read GetOwnsValues write SetOwnsValues;

    procedure Clear;
    function TryRemove(AItem: T): Boolean;
    procedure Remove(AItem: T);
    procedure RemoveAt(AIndex: Integer);
    procedure RemoveRange(AIndex, ACount: Integer);

  end;

  THashBase<T, K> = class(TInterfacedObject)
  private type

    TItems = array of array of THashValue<T>;

    TIteratorBase = class(TInterfacedObject)
    private
      FHashBase: THashBase<T, K>;
      FBucket: Integer;
      FIndex: Integer;

    public
      constructor Create(AHashBase: THashBase<T, K>);

      function MoveNext: Boolean;

    end;

    TIterator = class(TIteratorBase, IIterator<T>)
    protected
      function GetCurrent: T;

    end;

  private
    FItems: TItems;
    FCount: Integer;
    FAutoRehash: Boolean;
    FEquate: TFunc<K, K, Boolean>;
    FHash: TFunc<K, Cardinal>;

  protected
    constructor Create; virtual;

    procedure EnsureCapacity(ACount: Integer);
    procedure ReduceCapacity(ACount: Integer);

    function HashToBucket(AHash: Cardinal): Integer; inline;
    procedure AddHashValue(AValue: THashValue<T>); inline;
    function MakeHashValue(AKey: K; AValue: T): THashValue<T>; inline;
    procedure DoRemove(ABucket, AIndex: Integer);

    // ICollection<T>
    function GetCount: Integer;

    // IHashCollection<T>
    function GetBucketCount: Integer;
    function GetAutoRehash: Boolean;
    procedure SetAutoRehash(const Value: Boolean);

    function GetHash: TFunc<K, Cardinal>;
    procedure SetHash(const Value: TFunc<K, Cardinal>);
    function GetEquate: TFunc<K, K, Boolean>;
    procedure SetEquate(const Value: TFunc<K, K, Boolean>);

  public
    // ICollection<T>
    property Count: Integer read GetCount;
    function Empty: Boolean;

    procedure Clear;

    // IHashCollection<T>
    property BucketCount: Integer read GetBucketCount;
    property AutoRehash: Boolean read GetAutoRehash write SetAutoRehash;
    property Hash: TFunc<K, Cardinal> read GetHash write SetHash;
    property Equate: TFunc<K, K, Boolean> read GetEquate write SetEquate;

    procedure Rehash(ABuckets: Integer);
    procedure RehashFor(ACount: Integer);

  end;

  TSet<T> = class(THashBase<T, T>, IHashCollection<T>, ISet<T>, ICollection<T>, IIterable<T>,
    IReadonlySet<T>, IReadonlyCollection<T>)
  private
    function FindValue(AValue: T; out ABucket, AIndex: Integer): Boolean; overload; inline;
    function FindValue(AValue: T; AHash: Cardinal; out ABucket, AIndex: Integer): Boolean; overload;
    function MakeHashValue(AValue: T): THashValue<T>; inline;

  protected
    // ISet<T>
    function GetItem(AItem: T): Boolean;
    procedure SetItem(AItem: T; AValue: Boolean);

  public
    constructor Create(AItems: array of T); reintroduce; overload;
    constructor Create(AItems: IIterable<T>); reintroduce; overload;
    constructor Create(AItems: IIterator<T>); reintroduce; overload;

    // IIterable<T>
    function GetEnumerator: IIterator<T>;
    function Iterate: IIterate<T>;

    // ICollection<T>
    function Contains(AItem: T): Boolean;

    function Add(AItem: T): Boolean;
    function Remove(AItem: T): Boolean;
    function Extract(AItem: T): T;

    procedure AddRange(AItems: array of T); overload;
    procedure AddRange(AItems: IIterable<T>); overload;
    procedure AddRange(AItems: IIterator<T>); overload;

    function ReadonlyCollection: IReadonlyCollection<T>;

    // ISet<T>
    property Items[AItem: T]: Boolean read GetItem write SetItem; default;

    function ReadonlySet: IReadonlySet<T>;
    function HashCollection: IHashCollection<T>;

    function Copy: ISet<T>;

  end;

  TObjectSet<T: class> = class(TSet<T>, IObjectSet<T>, IHashCollection<T>, ISet<T>, ICollection<T>, IIterable<T>,
    IReadonlySet<T>, IReadonlyCollection<T>)
  private
    FOwnsValues: Boolean;

    function GetOwnsValues: Boolean;
    procedure SetOwnsValues(AValue: Boolean);

    procedure SetItem(AItem: T; AValue: Boolean);

  public
    constructor Create; overload; override;
    destructor Destroy; override;

    property Items[AItem: T]: Boolean read GetItem write SetItem; default;

    function Remove(AItem: T): Boolean;
    procedure Clear;

    property OwnsValues: Boolean read GetOwnsValues write SetOwnsValues;

  end;

  TMap<K, V> = class(THashBase<TPair<K, V>, K>, IHashCollection<K>, IMap<K, V>, ICollection<TPair<K, V>>,
    IIterable<TPair<K, V>>, IReadonlyMap<K, V>, IReadonlyCollection < TPair < K, V >> )
  private type

    TCollection = class(TInterfacedObject)
    private
      FMap: TMap<K, V>;

    protected
      // IReadonlyCollection<K>
      function GetCount: Integer;

    public
      constructor Create(AMap: TMap<K, V>);

      // IReadonlyCollection<K>
      property Count: Integer read GetCount;
      function Empty: Boolean;

    end;

    TKeyIterator = class(TIteratorBase, IIterator<K>)
    protected
      function GetCurrent: K;

    end;

    TValueIterator = class(TIteratorBase, IIterator<V>)
    protected
      function GetCurrent: V;

    end;

    TFindKeysIterator = class(TKeyIterator, IIterator<K>)
    private
      FValue: V;

    public
      constructor Create(AMap: TMap<K, V>; AValue: V);

      function MoveNext: Boolean;

    end;

    TKeyCollection = class(TCollection, IReadonlyCollection<K>, IIterable<K>)
    public
      // Iterable<K>
      function GetEnumerator: IIterator<K>;
      function Iterate: IIterate<K>;

      // IReadonlyCollection<K>
      function Contains(AItem: K): Boolean;

    end;

    TValueCollection = class(TCollection, IReadonlyCollection<V>, IIterable<V>)
    public
      // Iterable<V>
      function GetEnumerator: IIterator<V>;
      function Iterate: IIterate<V>;

      // IReadonlyCollection<V>
      function Contains(AItem: V): Boolean;

    end;

    TFindKeysIterable = class(TInterfacedObject, IIterable<K>)
    private
      FMap: TMap<K, V>;
      FValue: V;

    public
      constructor Create(AMap: TMap<K, V>; AValue: V);

      // Iterable<V>
      function GetEnumerator: IIterator<K>;
      function Iterate: IIterate<K>;

    end;

  private
    FEquateValue: TFunc<V, V, Boolean>;

  protected
    function FindKey(AKey: K; out ABucket, AIndex: Integer): Boolean; overload; inline;
    function FindKey(AKey: K; AHash: Cardinal; out ABucket, AIndex: Integer): Boolean; overload;
    function FindValue(AValue: V; out ABucket, AIndex: Integer): Boolean;

    function MakeHashPair(AKey: K; AValue: V): THashValue<TPair<K, V>>;

    // ICollection<TPair<K, V>>
    function GetCount: Integer;

    // IMap<K, V>
    function GetItem(AKey: K): V;
    procedure SetItem(AKey: K; AValue: V);

    function GetHashKey: TFunc<K, Cardinal>;
    procedure SetHashKey(const Value: TFunc<K, Cardinal>);
    function GetEquateKey: TFunc<K, K, Boolean>;
    procedure SetEquateKey(const Value: TFunc<K, K, Boolean>);
    function GetEquateValue: TFunc<V, V, Boolean>;
    procedure SetEquateValue(const Value: TFunc<V, V, Boolean>);

  public
    constructor Create; overload; override;
    constructor Create(APairs: array of TPair<K, V>); reintroduce; overload;
    constructor Create(APairs: IIterator < TPair < K, V >> ); reintroduce; overload;
    constructor Create(APairs: IIterable < TPair < K, V >> ); reintroduce; overload;

    // IIterable<TPair<K, V>>
    function GetEnumerator: IIterator<TPair<K, V>>;
    function Iterate: IIterate<TPair<K, V>>;

    // ICollection<TPair<K, V>>
    property Count: Integer read GetCount;
    function Empty: Boolean;

    function Contains(APair: TPair<K, V>): Boolean; overload;

    function Add(APair: TPair<K, V>): Boolean; overload;
    function Remove(APair: TPair<K, V>): Boolean; overload;
    function Extract(APair: TPair<K, V>): TPair<K, V>; overload;

    procedure AddRange(APairs: array of TPair<K, V>); overload;
    procedure AddRange(APairs: IIterable < TPair < K, V >> ); overload;
    procedure AddRange(APairs: IIterator < TPair < K, V >> ); overload;

    function ReadonlyCollection: IReadonlyCollection<TPair<K, V>>;

    // IMap<K, V>
    property Items[AKey: K]: V read GetItem write SetItem; default;

    function Keys: IReadonlyCollection<K>;
    function Values: IReadonlyCollection<V>;

    function ContainsKey(AKey: K): Boolean;
    function ContainsValue(AValue: V): Boolean;
    function Get(AKey: K; out AValue: V): Boolean;
    function GetKey(AValue: V; out AKey: K): Boolean;
    function GetKeys(AValue: V): IIterable<K>;

    function Add(AKey: K; AValue: V): Boolean; overload;
    function Remove(AKey: K): Boolean; overload;
    function Remove(AKey: K; AValue: V): Boolean; overload;
    function Extract(AKey: K): V; overload;
    function Extract(AKey: K; out AValue: V): Boolean; overload;

    function ReadonlyMap: IReadonlyMap<K, V>;
    function HashCollection: IHashCollection<K>;

    property HashKey: TFunc<K, Cardinal> read GetHashKey write SetHashKey;
    property EquateKey: TFunc<K, K, Boolean> read GetEquateKey write SetEquateKey;
    property EquateValue: TFunc<V, V, Boolean> read GetEquateValue write SetEquateValue;

    function Copy: IMap<K, V>;

  end;

  TObjectMap<K: class; V> = class(TMap<K, V>, IObjectMap<K, V>, IHashCollection<K>, IMap<K, V>,
    ICollection<TPair<K, V>>, IIterable<TPair<K, V>>, IReadonlyMap<K, V>, IReadonlyCollection < TPair < K, V >> )
  private
    FOwnsKeys: Boolean;

    function GetOwnsKeys: Boolean;
    procedure SetOwnsKeys(AValue: Boolean);

  protected
    procedure SetItem(AKey: K; AValue: V);

  public
    constructor Create; overload; override;
    destructor Destroy; override;

    property Items[AKey: K]: V read GetItem write SetItem; default;
    function Remove(APair: TPair<K, V>): Boolean; overload;
    function Remove(AKey: K): Boolean; overload;
    function Remove(AKey: K; AValue: V): Boolean; overload;
    procedure Clear;

    property OwnsKeys: Boolean read GetOwnsKeys write SetOwnsKeys;

  end;

  TToObjectMap<K; V: class> = class(TMap<K, V>, IToObjectMap<K, V>, IHashCollection<K>, IMap<K, V>,
    ICollection<TPair<K, V>>, IIterable<TPair<K, V>>, IReadonlyMap<K, V>, IReadonlyCollection < TPair < K, V >> )
  private
    FOwnsValues: Boolean;

    function GetOwnsValues: Boolean;
    procedure SetOwnsValues(AValue: Boolean);

  protected
    procedure SetItem(AKey: K; AValue: V);

  public
    constructor Create; overload; override;
    destructor Destroy; override;

    property Items[AKey: K]: V read GetItem write SetItem; default;
    function Remove(APair: TPair<K, V>): Boolean; overload;
    function Remove(AKey: K): Boolean; overload;
    function Remove(AKey: K; AValue: V): Boolean; overload;
    procedure Clear;

    property OwnsValues: Boolean read GetOwnsValues write SetOwnsValues;

  end;

  TObjectObjectMap<K, V: class> = class(TMap<K, V>, IObjectObjectMap<K, V>, IHashCollection<K>, IMap<K, V>,
    ICollection<TPair<K, V>>, IIterable<TPair<K, V>>, IReadonlyMap<K, V>, IReadonlyCollection < TPair < K, V >> )
  private
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;

    function GetOwnsKeys: Boolean;
    procedure SetOwnsKeys(AValue: Boolean);
    function GetOwnsValues: Boolean;
    procedure SetOwnsValues(AValue: Boolean);

  protected
    procedure SetItem(AKey: K; AValue: V);

  public
    constructor Create; overload; override;
    destructor Destroy; override;

    property Items[AKey: K]: V read GetItem write SetItem; default;
    function Remove(APair: TPair<K, V>): Boolean; overload;
    function Remove(AKey: K): Boolean; overload;
    function Remove(AKey: K; AValue: V): Boolean; overload;
    procedure Clear;

    property OwnsKeys: Boolean read GetOwnsKeys write SetOwnsKeys;
    property OwnsValues: Boolean read GetOwnsValues write SetOwnsValues;

  end;

  TStack<T> = class(TList<T>, IStack<T>, IList<T>, IListBase<T>, ICollection<T>, IIterable<T>, IReadonlyList<T>,
    IReadonlyCollection<T>)
  public type

    TPopper = class(TInterfacedObject, IIterator<T>)
    private
      FStack: IStack<T>;
      FCurrent: T;

      function GetCurrent: T;

    public
      constructor Create(AStack: IStack<T>);

      function MoveNext: Boolean;

    end;

  private
    function GetTop: T;

  public
    procedure Push(AItem: T);
    function Pop: T;
    property Top: T read GetTop;

    function GetEnumerator: IIterator<T>;
    function Iterate: IIterate<T>;

    function PopMany: IIterable<T>;

  end;

  TQueue<T> = class(TList<T>, IQueue<T>, IList<T>, IListBase<T>, ICollection<T>, IIterable<T>, IReadonlyList<T>,
    IReadonlyCollection<T>)
  public type

    TDequeuer = class(TInterfacedObject, IIterator<T>)
    private
      FQueue: IQueue<T>;
      FCurrent: T;

      function GetCurrent: T;

    public
      constructor Create(AQueue: IQueue<T>);

      function MoveNext: Boolean;

    end;

  private
    function GetNext: T;

  public
    procedure Enqueue(AItem: T);
    function Dequeue: T;
    property Next: T read GetNext;

    function GetEnumerator: IIterator<T>;
    function Iterate: IIterate<T>;

    function DequeueMany: IIterable<T>;

  end;

  // --- Iterate Implementations ---

  TIterate<T> = class(TInterfacedObject, IIterate<T>, IIterable<T>)
  private
    function GetItem(AIndex: Integer): T;

  public
    // IIterable<T>
    function GetEnumerator: IIterator<T>; virtual; abstract;
    function Iterate: IIterate<T>;

    // IIterate<T>
    function Count: Integer; overload;
    function Count(APredicate: TPredicate<T>): Integer; overload;
    function Empty: Boolean;
    function Any: Boolean; overload;
    function Any(APredicate: TPredicate<T>): Boolean; overload;
    function All(APredicate: TPredicate<T>): Boolean;

    function HasAtLeast(ACount: Integer): Boolean; overload;
    function HasAtLeast(ACount: Integer; APredicate: TPredicate<T>): Boolean; overload;
    function HasAtMost(ACount: Integer): Boolean; overload;
    function HasAtMost(ACount: Integer; APredicate: TPredicate<T>): Boolean; overload;
    function HasExactly(ACount: Integer): Boolean; overload;
    function HasExactly(ACount: Integer; APredicate: TPredicate<T>): Boolean; overload;

    function Reduce(AFunc: TFunc<T, T, T>): T; overload;
    function Reduce(AFunc: TFunc<T, T, T>; ASeed: T): T; overload;

    property Items[AIndex: Integer]: T read GetItem;
    function First: T;
    function Last: T;
    function Range(AIndex, ACount: Integer): IIterate<T>;
    function Where(APredicate: TPredicate<T>): IIterate<T>;
    function Take(ACount: Integer): IIterate<T>;
    function TakeWhile(APredicate: TPredicate<T>): IIterate<T>;
    function Skip(ACount: Integer): IIterate<T>;
    function SkipUntil(APredicate: TPredicate<T>): IIterate<T>;
    function Concat(AIterable: IIterable<T>): IIterate<T>;
    function Append(AItem: T): IIterate<T>;
    function Prepend(AItem: T): IIterate<T>;
    function Map(AFunc: TFunc<T, T>): IIterate<T>;
    function Zip(AIterable: IIterable<T>; AFunc: TFunc<T, T, T>): IIterate<T>;

    function Generic: TGenericWrapper<T>;

    function ToList: IList<T>;
    function ToSet: ISet<T>;

  end;

  TIterableIterate<T> = class(TIterate<T>, IIterable<T>, IIterate<T>)
  private
    FIterable: IIterable<T>;

  public
    constructor Create(AIterable: IIterable<T>);

    // IIterable<T>
    function GetEnumerator: IIterator<T>; override;

  end;

  TWhereIterate<T> = class(TIterate<T>, IIterable<T>, IIterate<T>)
  public type

    TIterator = class(TInterfacedObject, IIterator<T>)
    private
      FIterator: IIterator<T>;
      FPredicate: TPredicate<T>;

      function GetCurrent: T;

    public
      constructor Create(AIterator: IIterator<T>; APredicate: TPredicate<T>);

      function MoveNext: Boolean;

    end;

  private
    FIterable: IIterable<T>;
    FPredicate: TPredicate<T>;

  public
    constructor Create(AIterable: IIterable<T>; APredicate: TPredicate<T>);

    // IIterable<T>
    function GetEnumerator: IIterator<T>; override;

  end;

  TTakeWhileIterate<T> = class(TIterate<T>, IIterable<T>, IIterate<T>)
  public type

    TIterator = class(TInterfacedObject, IIterator<T>)
    private
      FIterator: IIterator<T>;
      FPredicate: TPredicate<T>;

      function GetCurrent: T;

    public
      constructor Create(AIterator: IIterator<T>; APredicate: TPredicate<T>);

      function MoveNext: Boolean;

    end;

  private
    FIterable: IIterable<T>;
    FPredicate: TPredicate<T>;

  public
    constructor Create(AIterable: IIterable<T>; APredicate: TPredicate<T>);

    // IIterable<T>
    function GetEnumerator: IIterator<T>; override;

  end;

  TSkipUntilIterate<T> = class(TIterate<T>, IIterable<T>, IIterate<T>)
  public type

    TIterator = class(TInterfacedObject, IIterator<T>)
    private
      FIterator: IIterator<T>;
      FState: Integer;

      function GetCurrent: T;

    public
      constructor Create(AIterator: IIterator<T>; APredicate: TPredicate<T>);

      function MoveNext: Boolean;

    end;

  private
    FIterable: IIterable<T>;
    FPredicate: TPredicate<T>;

  public
    constructor Create(AIterable: IIterable<T>; APredicate: TPredicate<T>);

    // IIterable<T>
    function GetEnumerator: IIterator<T>; override;

  end;

  TRangeIterate<T> = class(TIterate<T>, IIterable<T>, IIterate<T>)
  public type

    TIterator = class(TInterfacedObject, IIterator<T>)
    private
      FIterator: IIterator<T>;
      FCount: Integer;
      FNotEmpty: Boolean;

      function GetCurrent: T;

    public
      constructor Create(AIterator: IIterator<T>; AIndex, ACount: Integer);

      function MoveNext: Boolean;

    end;

  private
    FIterable: IIterable<T>;
    FIndex: Integer;
    FCount: Integer;

  public
    constructor Create(AIterable: IIterable<T>; AIndex, ACount: Integer);

    // IIterable<T>
    function GetEnumerator: IIterator<T>; override;

  end;

  TMapIterate<T, R> = class(TIterate<R>, IIterable<R>, IIterate<R>)
  public type

    TIterator = class(TInterfacedObject, IIterator<R>)
    private
      FIterator: IIterator<T>;
      FFunc: TFunc<T, R>;

      function GetCurrent: R;

    public
      constructor Create(AIterator: IIterator<T>; AFunc: TFunc<T, R>);

      function MoveNext: Boolean;

    end;

  private
    FIterable: IIterable<T>;
    FFunc: TFunc<T, R>;

  public
    constructor Create(AIterable: IIterable<T>; AFunc: TFunc<T, R>);

    // IIterable<R>
    function GetEnumerator: IIterator<R>; override;

  end;

  TZipIterate<T, U, R> = class(TIterate<R>, IIterable<R>, IIterate<R>)
  public type

    TIterator = class(TInterfacedObject, IIterator<R>)
    private
      FIterator1: IIterator<T>;
      FIterator2: IIterator<U>;
      FFunc: TFunc<T, U, R>;

      function GetCurrent: R;

    public
      constructor Create(AIterator1: IIterator<T>; AIterator2: IIterator<U>; AFunc: TFunc<T, U, R>);

      function MoveNext: Boolean;

    end;

  private
    FIterable1: IIterable<T>;
    FIterable2: IIterable<U>;
    FFunc: TFunc<T, U, R>;

  public
    constructor Create(AIterable1: IIterable<T>; AIterable2: IIterable<U>; AFunc: TFunc<T, U, R>);

    // IIterable<R>
    function GetEnumerator: IIterator<R>; override;

  end;

  TConcatIterate<T> = class(TIterate<T>, IIterable<T>, IIterate<T>)
  public type

    TIterator = class(TInterfacedObject, IIterator<T>)
    private
      FIterator1: IIterator<T>;
      FIterator2: IIterator<T>;

      function GetCurrent: T;

    public
      constructor Create(AIterator1, AIterator2: IIterator<T>);

      function MoveNext: Boolean;

    end;

  private
    FIterable1: IIterable<T>;
    FIterable2: IIterable<T>;

  public
    constructor Create(AIterable1, AIterable2: IIterable<T>);

    // IIterable<T>
    function GetEnumerator: IIterator<T>; override;

  end;

  TAppendIterate<T> = class(TIterate<T>, IIterable<T>, IIterate<T>)
  public type

    TIterator = class(TInterfacedObject, IIterator<T>)
    private
      FIterator: IIterator<T>;
      FItem: T;

      function GetCurrent: T;

    public
      constructor Create(AIterator: IIterator<T>; AItem: T);

      function MoveNext: Boolean;

    end;

  private
    FIterable: IIterable<T>;
    FItem: T;

  public
    constructor Create(AIterable: IIterable<T>; AItem: T);

    // IIterable<T>
    function GetEnumerator: IIterator<T>; override;

  end;

  TPrependIterate<T> = class(TIterate<T>, IIterable<T>, IIterate<T>)
  public type

    TIterator = class(TInterfacedObject, IIterator<T>)
    private
      FIterator: IIterator<T>;
      FItem: T;
      FState: Integer;

      function GetCurrent: T;

    public
      constructor Create(AIterator: IIterator<T>; AItem: T);

      function MoveNext: Boolean;

    end;

  private
    FIterable: IIterable<T>;
    FItem: T;

  public
    constructor Create(AIterable: IIterable<T>; AItem: T);

    // IIterable<T>
    function GetEnumerator: IIterator<T>; override;

  end;

  TListReverseIterate<T> = class(TIterate<T>, IIterable<T>, IIterate<T>)
  private
    FListBase: TListBase<T>;

  public
    constructor Create(AListBase: TListBase<T>);

    // IIterable<T>
    function GetEnumerator: IIterator<T>; override;

  end;

  TIntRangeIterate = class(TIterate<Integer>, IIterable<Integer>, IIterate<Integer>)
  public type

    TIterator = class(TInterfacedObject, IIterator<Integer>)
    private
      FCurrent: Integer;
      FStop: Integer;
      FStep: Integer;

      function GetCurrent: Integer;

    public
      constructor Create(AStart, AStop, AStep: Integer);

      function MoveNext: Boolean;

    end;

  private
    FStart: Integer;
    FStop: Integer;
    FStep: Integer;

  public
    constructor Create(AStart, AStop, AStep: Integer);

    // IIterable<T>
    function GetEnumerator: IIterator<Integer>; override;

  end;

  TEmptyIterator<T> = class(TInterfacedObject, IIterator<T>)
  private
    function GetCurrent: T;

  public
    function MoveNext: Boolean;

  end;

  TEmptyIterable<T> = class(TInterfacedObject, IIterable<T>)
  public
    function GetEnumerator: IIterator<T>;
    function Iterate: IIterate<T>;

  end;

function IntRange(ACount: Integer): IIterate<Integer>; overload;
function IntRange(AStart, AStop: Integer; AStep: Integer = 1): IIterate<Integer>; overload;

implementation

function IntRange(ACount: Integer): IIterate<Integer>;
begin
  Result := TIntRangeIterate.Create(0, ACount, 1);
end;

function IntRange(AStart, AStop, AStep: Integer): IIterate<Integer>;
begin
  Result := TIntRangeIterate.Create(AStart, AStop, AStep);
end;

{ TDefault }

class function TDefault.Equate<T>(A, B: T): Boolean;
begin
  if (GetTypeKind(T) = tkRecord) and IsManagedType(T) then
    raise EDefaultError.Create('Comparing managed records is not supported.');
  case GetTypeKind(T) of
    tkInteger, tkChar, tkEnumeration, tkFloat, tkSet, tkClass, tkMethod, tkWChar, tkRecord, tkInterface, tkInt64,
      tkClassRef, tkPointer:
      case SizeOf(T) of
        1:
          Result := PByte(@A)^ = PByte(@B)^;
        2:
          Result := PWord(@A)^ = PWord(@B)^;
        4:
          Result := PCardinal(@A)^ = PCardinal(@B)^;
        8:
          Result := PUInt64(@A)^ = PUInt64(@B)^;
      else
        Result := CompareMem(@A, @B, SizeOf(T));
      end;
    tkUString:
      Result := PUnicodeString(@A)^ = PUnicodeString(@B)^;
    {$IFDEF WINDOWS}
    tkLString:
      Result := PAnsiString(@A)^ = PAnsiString(@B)^;
    {$ENDIF}
  else
    raise EDefaultError.Create('Unsupported object equation.');
  end;
end;

class function TDefault.Hash<T>(A: T): Cardinal;
var
  I: PByte;
begin
  if (GetTypeKind(T) = tkRecord) and IsManagedType(T) then
    raise EDefaultError.Create('Comparing managed records is not supported.');
  case GetTypeKind(T) of
    tkInteger, tkChar, tkEnumeration, tkFloat, tkSet, tkClass, tkMethod, tkWChar, tkRecord, tkInterface, tkInt64,
      tkClassRef, tkPointer, tkArray:
      Result := THashBobJenkins.GetHashValue(A, SizeOf(T));
    {
      case SizeOf(T) of
      1:
      Result := PByte(@A)^;
      2:
      Result := PWord(@A)^;
      3:
      Result := PWord(@A)^ or ((PByte(@A) + 2)^ shl 16);
      4:
      Result := PCardinal(@A)^;
      8:
      Result := PCardinal(@A)^ xor PCardinal(PByte(@A) + 4)^;
      12:
      Result := PCardinal(@A)^ xor PCardinal(PByte(@A) + 4)^ xor
      PCardinal(PByte(@A) + 8)^;
      16:
      Result := PCardinal(@A)^ xor PCardinal(PByte(@A) + 4)^ xor
      PCardinal(PByte(@A) + 8)^ xor PCardinal(PByte(@A) + 12)^;
      else
      Result := 0;
      I := @A;
      while (I + 4) <= (PByte(@A) + SizeOf(T)) do
      begin
      Result := Result xor PCardinal(I)^;
      Inc(I, 4);
      end;
      if I <> (PByte(@A) + SizeOf(T)) then
      Result := Result xor PCardinal(PByte(@A) + SizeOf(T) - 4)^;

      end;
    }
    tkUString:
      Result := HashString(PUnicodeString(@A)^);
    {$IFDEF WINDOWS}
    tkLString:
      Result := HashString(PAnsiString(@A)^);
    {$ENDIF}
  else
    raise EDefaultError.Create('Unsupported object hashing.');
  end;
end;

class function TDefault.HashString(A: UnicodeString): Cardinal;
begin
  Result := THashBobJenkins.GetHashValue(A[1], Length(A) * SizeOf(WideChar));
end;

{$IFDEF WINDOWS}

class function TDefault.HashString(A: AnsiString): Cardinal;
begin
  Result := THashBobJenkins.GetHashValue(A[1], Length(A) * SizeOf(AnsiChar));
end;
{$ENDIF}


class function TDefault.Compare<T>(A, B: T): Boolean;
type
  PReal = ^Real;
begin
  if (GetTypeKind(T) = tkRecord) and IsManagedType(T) then
    raise EDefaultError.Create('Comparing managed records is not supported.');

  // Enums
  if GetTypeKind(T) = tkEnumeration then
  begin
    case SizeOf(T) of
      1:
        Exit(PByte(@A)^ < PByte(@B)^);
      2:
        Exit(PWord(@A)^ < PWord(@B)^);
      4:
        Exit(PCardinal(@A)^ < PCardinal(@B)^);
    end;
  end
  else
  begin
    // 1 Byte
    if TypeInfo(T) = TypeInfo(ShortInt) then
      Exit(PShortInt(@A)^ < PShortInt(@B)^);
    if TypeInfo(T) = TypeInfo(Byte) then
      Exit(PByte(@A)^ < PByte(@B)^);
    {$IFDEF WINDOWS}
    if TypeInfo(T) = TypeInfo(AnsiChar) then
      Exit(PAnsiChar(@A)^ < PAnsiChar(@B)^);
    {$ENDIF}
    // 2 Byte
    if TypeInfo(T) = TypeInfo(SmallInt) then
      Exit(PSmallInt(@A)^ < PSmallInt(@B)^);
    if TypeInfo(T) = TypeInfo(Word) then
      Exit(PWord(@A)^ < PWord(@B)^);
    if TypeInfo(T) = TypeInfo(WideChar) then
      Exit(PWideChar(@A)^ < PWideChar(@B)^);

    // 4 Byte
    if TypeInfo(T) = TypeInfo(Integer) then
      Exit(PInteger(@A)^ < PInteger(@B)^);
    if TypeInfo(T) = TypeInfo(Cardinal) then
      Exit(PCardinal(@A)^ < PCardinal(@B)^);

    // 8 Byte
    if TypeInfo(T) = TypeInfo(Int64) then
      Exit(PInt64(@A)^ < PInt64(@B)^);
    if TypeInfo(T) = TypeInfo(UInt64) then
      Exit(PUInt64(@A)^ < PUInt64(@B)^);

    // Floats
    if TypeInfo(T) = TypeInfo(Single) then
      Exit(PSingle(@A)^ < PSingle(@B)^);
    if TypeInfo(T) = TypeInfo(Double) then
      Exit(PDouble(@A)^ < PDouble(@B)^);
    if TypeInfo(T) = TypeInfo(Real) then
      Exit(PReal(@A)^ < PReal(@B)^);
    if TypeInfo(T) = TypeInfo(Extended) then
      Exit(PExtended(@A)^ < PExtended(@B)^);

    // strings
    {$IFDEF WINDOWS}
    if TypeInfo(T) = TypeInfo(AnsiString) then
      Exit(PAnsiString(@A)^ < PAnsiString(@B)^);
    {$ENDIF}
    if TypeInfo(T) = TypeInfo(UnicodeString) then
      Exit(PUnicodeString(@A)^ < PUnicodeString(@B)^);
    if TypeInfo(T) = TypeInfo(RawByteString) then
      Exit(PRawByteString(@A)^ < PRawByteString(@B)^);
  end;

  raise EDefaultError.Create('Unsupported object comparision.');
end;

{ TDefault<T> }
{
class constructor TDefault<T>.Create;
begin
  FEquateFunc := TDefault.Equate<T>;
  FCompareFunc := TDefault.Compare<T>;
  FHashFunc := TDefault.Hash<T>;
end;
}
{ TPair<K, V> }

constructor TPair<K, V>.Create(AKey: K; AValue: V);
begin
  FKey := AKey;
  FValue := AValue;
end;

{ THashValue<T> }

constructor THashValue<T>.Create(AHash: Cardinal; AValue: T);
begin
  FHash := AHash;
  FValue := AValue;
end;

{ TGenericWrapper<T> }

constructor TGenericWrapper<T>.Create(AIterate: IIterate<T>);
begin
  FIterate := AIterate;
end;

function TGenericWrapper<T>.Map<R>(AFunc: TFunc<T, R>): IIterate<R>;
begin
  Result := TMapIterate<T, R>.Create(FIterate, AFunc);
end;

function TGenericWrapper<T>.OfType<R>: IIterate<R>;
begin
  if GetTypeKind(T) <> tkClass then
    raise EIterateError.Create('Invalid type for OfType-Iteration.');
  Result := IIterate<R>(FIterate.Where(
    function(Item: T): Boolean
    begin
      Result := Item is R;
    end));
end;

function TGenericWrapper<T>.ToMapK<V>(AIterable: IIterable<V>): IMap<T, V>;
var
  Keys: IIterator<T>;
  Values: IIterator<V>;
begin
  Result := TMap<T, V>.Create;
  Keys := FIterate.GetEnumerator;
  Values := AIterable.GetEnumerator;
  while Keys.MoveNext and Values.MoveNext do
    Result.Add(Keys.Current, Values.Current);
end;

function TGenericWrapper<T>.ToMapV<V>(AIterable: IIterable<V>): IMap<V, T>;
var
  Keys: IIterator<V>;
  Values: IIterator<T>;
begin
  Result := TMap<V, T>.Create;
  Keys := AIterable.GetEnumerator;
  Values := FIterate.GetEnumerator;
  while Keys.MoveNext and Values.MoveNext do
    Result.Add(Keys.Current, Values.Current);
end;

function TGenericWrapper<T>.Zip<R>(AIterable: IIterable<T>; AFunc: TFunc<T, T, R>): IIterate<R>;
begin
  Result := TZipIterate<T, T, R>.Create(FIterate, AIterable, AFunc);
end;

function TGenericWrapper<T>.Zip<U, R>(AIterable: IIterable<U>; AFunc: TFunc<T, U, R>): IIterate<R>;
begin
  Result := TZipIterate<T, U, R>.Create(FIterate, AIterable, AFunc);
end;

{ TListBase<T> }

procedure TListBase<T>.ReduceCapacity(ACount: Integer);
var
  NewCapacity: Integer;
begin
  if ACount = 0 then
    FItems := nil
  else if Length(FItems) >= ACount + ReduceThreshold then
    SetLength(FItems, ACount);
end;

constructor TListBase<T>.Create;
begin
  FEquate := TDefault.Equate<T>;
  FCompare := TDefault.Compare<T>;
end;

procedure TListBase<T>.EnsureCapacity(ACount: Integer);
var
  NewCapacity: Integer;
begin
  NewCapacity := Length(FItems);
  if NewCapacity >= ACount then
    Exit;
  // Inc(NewCapacity, (ACount - NewCapacity + GrowAmount - 1) div GrowAmount * GrowAmount);
  repeat
    if NewCapacity > 64 then
      NewCapacity := (NewCapacity * 3) div 2
    else
    begin
      if NewCapacity > 8 then
        NewCapacity := NewCapacity + 16
      else
        NewCapacity := NewCapacity + 4;
    end;
  until NewCapacity >= ACount;
  SetLength(FItems, NewCapacity);
end;

procedure TListBase<T>.RangeCheck(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EListError.Create('List index out of range.')at ReturnAddress;
end;

function TListBase<T>.DataPointer: Pointer;
begin
  Result := @FItems[0];
end;

procedure TListBase<T>.DoRemoveAt(AIndex: Integer);
var
  Tmp: T;
begin
  if AIndex <> MaxIndex then
  begin
    Tmp := FItems[AIndex];
    Move(FItems[AIndex + 1], FItems[AIndex], SizeOf(T) * (Count - AIndex - 1));
    Move(Tmp, FItems[MaxIndex], SizeOf(T));
  end;
  ReduceCapacity(Count - 1);
  Dec(FCount);
end;

procedure TListBase<T>.DoRemoveRange(AIndex, ACount: Integer);
var
  Tmp: array of T;
begin
  if AIndex <> Count - ACount then
  begin
    SetLength(Tmp, ACount);
    Move(FItems[AIndex], Tmp[0], ACount * SizeOf(T));
    Move(FItems[AIndex + ACount], FItems[AIndex], SizeOf(T) * (Count - AIndex - ACount));
    Move(Tmp, FItems[MaxIndex], ACount * SizeOf(T));
  end;
  ReduceCapacity(Count - ACount);
  Dec(FCount, ACount);
end;

function TListBase<T>.GetCount: Integer;
begin
  Result := FCount;
end;

function TListBase<T>.GetMaxIndex: Integer;
begin
  Result := FCount - 1;
end;

function TListBase<T>.GetItem(AIndex: Integer): T;
begin
  Result := FItems[AIndex];
end;

function TListBase<T>.GetCapacity: Integer;
begin
  Result := Length(FItems);
end;

procedure TListBase<T>.SetCapacity(const Value: Integer);
begin
  if Value < Count then
    raise EListError.Create('List capacity cannot be smaller than element count.');
  SetLength(FItems, Value);
end;

function TListBase<T>.GetFirst: T;
begin
  Result := Items[0];
end;

function TListBase<T>.GetLast: T;
begin
  Result := Items[MaxIndex];
end;

procedure TListBase<T>.Insert(AIndex: Integer; AItem: T);
begin
  EnsureCapacity(Count + 1);
  if AIndex <> Count then
  begin
    RangeCheck(AIndex);
    Finalize(FItems[Count]);
    Move(FItems[AIndex], FItems[AIndex + 1], SizeOf(T) * (Count - AIndex));
    Initialize(FItems[AIndex]);
  end;
  FItems[AIndex] := AItem;
  Inc(FCount);
end;

procedure TListBase<T>.InsertRange(AIndex: Integer; AItems: array of T);
var
  I: Integer;
begin
  for I := 0 to Length(AItems) - 1 do
    Insert(AIndex + I, AItems[I]);
end;

procedure TListBase<T>.InsertRange(AIndex: Integer; AItems: IIterable<T>);
begin
  InsertRange(AIndex, AItems.GetEnumerator);
end;

procedure TListBase<T>.InsertRange(AIndex: Integer; AItems: IIterator<T>);
begin
  while AItems.MoveNext do
  begin
    Insert(AIndex, AItems.Current);
    Inc(AIndex);
  end;
end;

function TListBase<T>.TrySortRange(LBound, RBound: Integer): Boolean;
var
  L, R: Integer;
  Pivot, Tmp: T;
begin
  L := LBound;
  R := RBound;

  Pivot := FItems[(L + R) shr 1];

  repeat
    while Compare(FItems[L], Pivot) do
    begin
      Inc(L);
      if L > RBound then
        Exit(False);
    end;
    while Compare(Pivot, FItems[R]) do
    begin
      Dec(R);
      if R < LBound then
        Exit(False);
    end;
    if L <= R then
    begin
      if L <> R then
      begin
        Tmp := FItems[L];
        FItems[L] := FItems[R];
        FItems[R] := Tmp;
      end;
      Inc(L);
      Dec(R);
    end;
  until L > R;

  Result := (R <= LBound) or TrySortRange(LBound, R);
  Result := Result and ((L >= RBound) or TrySortRange(L, RBound));
end;

function TListBase<T>.TrySort: Boolean;
begin
  Result := (Count <= 1) or TrySortRange(0, MaxIndex);
end;

function TListBase<T>.GetCompare: TFunc<T, T, Boolean>;
begin
  Result := FCompare;
end;

function TListBase<T>.GetEnumerator: IIterator<T>;
begin
  Result := TListIterator<T>.Create(Self);
end;

function TListBase<T>.GetEquate: TFunc<T, T, Boolean>;
begin
  Result := FEquate;
end;

procedure TListBase<T>.SetCompare(const Value: TFunc<T, T, Boolean>);
begin
  if Assigned(Value) then
    FCompare := Value
  else
    FCompare := TDefault.Compare<T>;
end;

procedure TListBase<T>.SetEquate(const Value: TFunc<T, T, Boolean>);
begin
  if Assigned(Value) then
    FEquate := Value
  else
    FEquate := TDefault.Equate<T>;
end;

function TListBase<T>.Empty: Boolean;
begin
  Result := Count = 0;
end;

procedure TListBase<T>.Clear;
begin
  FCount := 0;
  FItems := nil;
end;

procedure TListBase<T>.RemoveAt(AIndex: Integer);
begin
  RangeCheck(AIndex);
  DoRemoveAt(AIndex);
end;

procedure TListBase<T>.RemoveRange(AIndex, ACount: Integer);
begin
  RangeCheck(AIndex);
  RangeCheck(AIndex + ACount - 1);
  DoRemoveRange(AIndex, ACount);
end;

function TListBase<T>.Reverse: IIterate<T>;
begin
  Result := TListReverseIterate<T>.Create(Self);
end;

function TListBase<T>.Extract(AIndex: Integer): T;
begin
  RangeCheck(AIndex);
  Result := Items[AIndex];
  RemoveAt(AIndex);
end;

{ TListBaseIterator<T> }

function TListBaseIterator<T>.GetCurrent: T;
begin
  Result := FList[FCurrent];
end;

constructor TListBaseIterator<T>.Create(AList: TListBase<T>);
begin
  FList := AList;
end;

{ TList<T> }

procedure TList<T>.SetItem(AIndex: Integer; AValue: T);
begin
  FItems[AIndex] := AValue;
end;

constructor TList<T>.Create;
begin
  inherited;
end;

constructor TList<T>.Create(AItems: array of T);
begin
  Create;
  AddRange(AItems);
end;

constructor TList<T>.Create(AIterable: IIterable<T>);
begin
  Create;
  AddRange(AIterable);
end;

constructor TList<T>.Create(AIterator: IIterator<T>);
begin
  Create;
  AddRange(AIterator);
end;

function TList<T>.GetEnumerator: IIterator<T>;
begin
  Result := TListIterator<T>.Create(Self);
end;

function TList<T>.Iterate: IIterate<T>;
begin
  Result := TIterableIterate<T>.Create(Self);
end;

procedure TList<T>.Move(AFrom, ATo: Integer);
begin
  if AFrom = ATo then
    Exit;
  Insert(ATo, Extract(AFrom));
end;

procedure TList<T>.Move(AItem: T; AIndex: Integer);
begin
  Move(IndexOf(AItem), AIndex);
end;

function TList<T>.ReadonlyCollection: IReadonlyCollection<T>;
begin
  Result := Self;
end;

function TList<T>.Contains(AItem: T): Boolean;
var
  Item: T;
begin
  for Item in Self do
    if Equate(Item, AItem) then
      Exit(True);
  Result := False;
end;

function TList<T>.Remove(AItem: T): Boolean;
var
  I: Integer;
begin
  I := IndexOf(AItem);
  Result := I <> -1;
  if Result then
    DoRemoveAt(I);
end;

function TList<T>.Extract(AItem: T): T;
begin
  if not Remove(AItem) then
    raise EListError.Create('Item not found.');
  Result := AItem;
end;

function TList<T>.ReadonlyList: IReadonlyList<T>;
begin
  Result := Self;
end;

function TList<T>.Add(AItem: T): Boolean;
begin
  Insert(Count, AItem);
  Result := True;
end;

procedure TList<T>.AddRange(AItems: array of T);
var
  Item: T;
begin
  for Item in AItems do
    Add(Item);
end;

procedure TList<T>.AddRange(AItems: IIterable<T>);
begin
  AddRange(AItems.GetEnumerator);
end;

procedure TList<T>.AddRange(AItems: IIterator<T>);
begin
  while AItems.MoveNext do
    Add(AItems.Current);
end;

function TList<T>.IndexOf(AItem: T): Integer;
var
  I: Integer;
begin
  for I := 0 to MaxIndex do
    if Equate(AItem, FItems[I]) then
      Exit(I);
  Result := -1;
end;

function TList<T>.TrySort: Boolean;
begin
  Result := inherited;
end;

procedure TList<T>.Sort;
begin
  if not TrySort then
    raise EListError.Create('Invalid sort function.');
end;

procedure TList<T>.Swap(AIndex1, AIndex2: Integer);
var
  Tmp: T;
begin
  if AIndex1 = AIndex2 then
    Exit;
  Tmp := FItems[AIndex1];
  FItems[AIndex1] := FItems[AIndex2];
  FItems[AIndex2] := Tmp;
end;

procedure TList<T>.Swap(AItem: T; AIndex: Integer);
begin
  Swap(IndexOf(AItem), AIndex);
end;

procedure TList<T>.Swap(AItem1, AItem2: T);
begin
  Swap(IndexOf(AItem1), IndexOf(AItem2));
end;

function TList<T>.Copy: IList<T>;
begin
  Result := TList<T>.Create(Self);
  Result.Compare := Compare;
  Result.Equate := Equate;
end;

{ TSortedList<T> }

function TSortedList<T>.BinarySearchRec(AItem: T; ALeft, ARight: Integer): Integer;
var
  Center: Integer;
  Pivot: T;
begin
  if ARight <= ALeft then
    Exit(ALeft);
  Center := (ALeft + ARight - 1) div 2;
  Pivot := FItems[Center];
  if Compare(Pivot, AItem) then
    Result := BinarySearchRec(AItem, Center + 1, ARight)
  else
    Result := BinarySearchRec(AItem, ALeft, Center);
end;

constructor TSortedList<T>.Create;
begin
  inherited;
end;

constructor TSortedList<T>.Create(AItems: array of T);
begin
  Create;
  AddRange(AItems);
end;

constructor TSortedList<T>.Create(AItems: IIterable<T>);
begin
  Create;
  AddRange(AItems);
end;

constructor TSortedList<T>.Create(AItems: IIterator<T>);
begin
  Create;
  AddRange(AItems);
end;

function TSortedList<T>.Iterate: IIterate<T>;
begin
  Result := TIterableIterate<T>.Create(Self);
end;

function TSortedList<T>.Add(AItem: T): Boolean;
begin
  Insert(BinarySearch(AItem), AItem);
  Result := True;
end;

procedure TSortedList<T>.AddRange(AItems: array of T);
var
  Item: T;
begin
  for Item in AItems do
    Insert(Count, Item);
  if not TrySort then
    raise EListError.Create('Invalid sort function.');
end;

procedure TSortedList<T>.AddRange(AItems: IIterable<T>);
begin
  AddRange(AItems.GetEnumerator);
end;

procedure TSortedList<T>.AddRange(AItems: IIterator<T>);
begin
  while AItems.MoveNext do
    Insert(Count, AItems.Current);
  if not TrySort then
    raise EListError.Create('Invalid sort function.');
end;

function TSortedList<T>.IndexOf(AItem: T): Integer;
begin
  Result := BinarySearch(AItem);
  repeat
    if Equate(AItem, FItems[Result]) then
      Exit;
    if Compare(AItem, FItems[Result]) then
      Break;
    Inc(Result);
  until Result > MaxIndex;
  Result := -1;
end;

function TSortedList<T>.Contains(AItem: T): Boolean;
begin
  Result := IndexOf(AItem) <> -1;
end;

function TSortedList<T>.Remove(AItem: T): Boolean;
var
  I: Integer;
begin
  I := IndexOf(AItem);
  Result := I <> -1;
  if Result then
    DoRemoveAt(I);
end;

function TSortedList<T>.Extract(AItem: T): T;
begin
  if not Remove(AItem) then
    raise EListError.Create('Item not found.');
  Result := AItem;
end;

function TSortedList<T>.ReadonlyCollection: IReadonlyCollection<T>;
begin
  Result := Self;
end;

function TSortedList<T>.ReadonlyList: IReadonlyList<T>;
begin
  Result := Self;
end;

function TSortedList<T>.BinarySearch(AItem: T): Integer;
begin
  Result := BinarySearchRec(AItem, 0, Count);
end;

function TSortedList<T>.Copy: ISortedList<T>;
begin
  Result := TSortedList<T>.Create;
  Result.Compare := Compare;
  Result.Equate := Equate;
  Result.AddRange(Self);
end;

{ TObjectList<T> }

function TObjectList<T>.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

procedure TObjectList<T>.SetOwnsValues(AValue: Boolean);
begin
  FOwnsValues := AValue;
end;

constructor TObjectList<T>.Create;
begin
  inherited;
  FOwnsValues := True;
end;

destructor TObjectList<T>.Destroy;
var
  Item: T;
begin
  if OwnsValues then
    for Item in Reverse do
      Item.Free;
  inherited;
end;

procedure TObjectList<T>.Clear;
var
  Item: T;
begin
  if OwnsValues then
    for Item in Reverse do
      Item.Free;
  inherited;
end;

function TObjectList<T>.Remove(AItem: T): Boolean;
var
  I: Integer;
begin
  I := IndexOf(AItem);
  Result := I <> -1;
  if Result then
  begin
    if OwnsValues then
      Items[I].Free;
    DoRemoveAt(I);
  end;
end;

procedure TObjectList<T>.RemoveAt(AIndex: Integer);
begin
  RangeCheck(AIndex);
  if OwnsValues then
    Items[AIndex].Free;
  DoRemoveAt(AIndex);
end;

procedure TObjectList<T>.RemoveRange(AIndex, ACount: Integer);
var
  I: Integer;
begin
  RangeCheck(AIndex);
  RangeCheck(AIndex + ACount - 1);
  if OwnsValues then
    for I := AIndex + ACount - 1 downto AIndex do
      Items[AIndex].Free;
  DoRemoveRange(AIndex, ACount);
end;

{ TSortedObjectList<T> }

function TSortedObjectList<T>.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

procedure TSortedObjectList<T>.SetOwnsValues(AValue: Boolean);
begin
  FOwnsValues := AValue;
end;

constructor TSortedObjectList<T>.Create;
begin
  inherited;
  FOwnsValues := True;
end;

destructor TSortedObjectList<T>.Destroy;
var
  Item: T;
begin
  if OwnsValues then
    for Item in Reverse do
      Item.Free;
  inherited;
end;

procedure TSortedObjectList<T>.Clear;
var
  Item: T;
begin
  if OwnsValues then
    for Item in Reverse do
      Item.Free;
  inherited;
end;

function TSortedObjectList<T>.TryRemove(AItem: T): Boolean;
var
  I: Integer;
begin
  I := IndexOf(AItem);
  Result := I <> -1;
  if Result then
  begin
    if OwnsValues then
      Items[I].Free;
    DoRemoveAt(I);
  end;
end;

procedure TSortedObjectList<T>.Remove(AItem: T);
begin
  if not TryRemove(AItem) then
    raise EListError.Create('Item not found.');
end;

procedure TSortedObjectList<T>.RemoveAt(AIndex: Integer);
begin
  RangeCheck(AIndex);
  if OwnsValues then
    Items[AIndex].Free;
  DoRemoveAt(AIndex);
end;

procedure TSortedObjectList<T>.RemoveRange(AIndex, ACount: Integer);
var
  I: Integer;
begin
  RangeCheck(AIndex);
  RangeCheck(AIndex + ACount - 1);
  if OwnsValues then
    for I := AIndex + ACount - 1 downto AIndex do
      Items[AIndex].Free;
  DoRemoveRange(AIndex, ACount);
end;

{ THashBase<T, K>.TIteratorBase }

constructor THashBase<T, K>.TIteratorBase.Create(AHashBase: THashBase<T, K>);
begin
  FHashBase := AHashBase;
  FIndex := -1;
end;

function THashBase<T, K>.TIteratorBase.MoveNext: Boolean;
begin
  if FHashBase.BucketCount = 0 then
    Exit(False);
  Inc(FIndex);
  if FIndex > High(FHashBase.FItems[FBucket]) then
  begin
    FIndex := 0;
    repeat
      Inc(FBucket);
      if FBucket >= Length(FHashBase.FItems) then
        Exit(False);
    until Length(FHashBase.FItems[FBucket]) > 0;
  end;
  Result := True;
end;

{ THashBase<T, K>.TIterator }

function THashBase<T, K>.TIterator.GetCurrent: T;
begin
  Result := FHashBase.FItems[FBucket, FIndex].Value;
end;

{ THashBase<T, K> }

procedure THashBase<T, K>.Clear;
begin
  FItems := nil;
  FCount := 0;
end;

constructor THashBase<T, K>.Create;
begin
  FAutoRehash := True;
  FHash := TDefault.Hash<K>;
  FEquate := TDefault.Equate<K>;
end;

function THashBase<T, K>.Empty: Boolean;
begin
  Result := FCount = 0;
end;

function THashBase<T, K>.GetAutoRehash: Boolean;
begin
  Result := FAutoRehash;
end;

function THashBase<T, K>.GetBucketCount: Integer;
begin
  Result := Length(FItems);
end;

function THashBase<T, K>.GetCount: Integer;
begin
  Result := FCount;
end;

function THashBase<T, K>.GetEquate: TFunc<K, K, Boolean>;
begin
  Result := FEquate;
end;

function THashBase<T, K>.GetHash: TFunc<K, Cardinal>;
begin
  Result := FHash;
end;

function THashBase<T, K>.HashToBucket(AHash: Cardinal): Integer;
begin
  Result := AHash mod Cardinal(BucketCount);
end;

function THashBase<T, K>.MakeHashValue(AKey: K; AValue: T): THashValue<T>;
begin
  Result.Create(Hash(AKey), AValue);
end;

procedure THashBase<T, K>.SetAutoRehash(const Value: Boolean);
begin
  FAutoRehash := Value;
end;

procedure THashBase<T, K>.SetEquate(const Value: TFunc<K, K, Boolean>);
begin
  if Assigned(Value) then
    FEquate := Value
  else
    FEquate := TDefault.Equate<K>;
end;

procedure THashBase<T, K>.SetHash(const Value: TFunc<K, Cardinal>);
begin
  if Assigned(Value) then
    FHash := Value
  else
    FHash := TDefault.Hash<K>;
end;

procedure THashBase<T, K>.AddHashValue(AValue: THashValue<T>);
var
  Bucket: Integer;
begin
  Bucket := HashToBucket(AValue.Hash);
  Insert(AValue, FItems[Bucket], Length(FItems[Bucket]));
end;

procedure THashBase<T, K>.Rehash(ABuckets: Integer);
var
  OldItems: TItems;
  I, J: Integer;
begin
  OldItems := FItems;
  FItems := nil;
  SetLength(FItems, ABuckets);
  for I := 0 to High(OldItems) do
    for J := 0 to High(OldItems[I]) do
      AddHashValue(OldItems[I, J]);
end;

procedure THashBase<T, K>.RehashFor(ACount: Integer);
begin
  Rehash(ChooseHashPrime(ACount));
end;

procedure THashBase<T, K>.EnsureCapacity(ACount: Integer);
var
  NewBucketCount: Integer;
begin
  if not AutoRehash or (ACount <= Count) then
    Exit;
  NewBucketCount := ChooseHashPrime(ACount);
  if NewBucketCount > BucketCount then
    Rehash(NewBucketCount);
end;

procedure THashBase<T, K>.ReduceCapacity(ACount: Integer);
var
  NewBucketCount: Integer;
begin
  if not AutoRehash then
    Exit;
  NewBucketCount := ChooseHashPrime(ACount);
  // only shrink if bucket count halved
  if NewBucketCount < BucketCount div 2 then
    Rehash(NewBucketCount);
end;

procedure THashBase<T, K>.DoRemove(ABucket, AIndex: Integer);
begin
  Delete(FItems[ABucket], AIndex, 1);
  Dec(FCount);
  ReduceCapacity(Count);
end;

{ TSet<T> }

function TSet<T>.FindValue(AValue: T; out ABucket, AIndex: Integer): Boolean;
begin
  Result := FindValue(AValue, Hash(AValue), ABucket, AIndex);
end;

function TSet<T>.FindValue(AValue: T; AHash: Cardinal; out ABucket, AIndex: Integer): Boolean;
var
  BucketLen: Integer;
begin
  if BucketCount = 0 then
    Exit(False);
  ABucket := HashToBucket(AHash);
  AIndex := 0;
  if Count = 0 then
    Exit(False);
  BucketLen := Length(FItems[ABucket]);
  while AIndex < BucketLen do
  begin
    if (AHash = FItems[ABucket, AIndex].Hash) and Equate(AValue, FItems[ABucket, AIndex].Value) then
      Exit(True);
    Inc(AIndex);
  end;
  Result := False;
end;

function TSet<T>.MakeHashValue(AValue: T): THashValue<T>;
begin
  Result := inherited MakeHashValue(AValue, AValue);
end;

function TSet<T>.GetItem(AItem: T): Boolean;
var
  Bucket, Index: Integer;
begin
  Result := FindValue(AItem, Bucket, Index);
end;

procedure TSet<T>.SetItem(AItem: T; AValue: Boolean);
begin
  if AValue then
    Add(AItem)
  else
    Remove(AItem);
end;

constructor TSet<T>.Create(AItems: array of T);
begin
  Create;
  AddRange(AItems);
end;

constructor TSet<T>.Create(AItems: IIterable<T>);
begin
  Create;
  AddRange(AItems);
end;

constructor TSet<T>.Create(AItems: IIterator<T>);
begin
  Create;
  AddRange(AItems);
end;

function TSet<T>.GetEnumerator: IIterator<T>;
begin
  Result := TIterator.Create(Self);
end;

function TSet<T>.Iterate: IIterate<T>;
begin
  Result := TIterableIterate<T>.Create(Self);
end;

function TSet<T>.Contains(AItem: T): Boolean;
var
  Bucket, Index: Integer;
begin
  Result := FindValue(AItem, Bucket, Index);
end;

function TSet<T>.Add(AItem: T): Boolean;
var
  Value: THashValue<T>;
  Bucket, Index: Integer;
begin
  EnsureCapacity(Count + 1);
  Value := MakeHashValue(AItem);
  Result := not FindValue(AItem, Value.Hash, Bucket, Index);
  if Result then
  begin
    Insert(Value, FItems[Bucket], Index);
    Inc(FCount);
  end;
end;

function TSet<T>.Remove(AItem: T): Boolean;
var
  Bucket, Index: Integer;
begin
  Result := FindValue(AItem, Bucket, Index);
  if Result then
    DoRemove(Bucket, Index);
end;

function TSet<T>.Extract(AItem: T): T;
begin
  if not Remove(AItem) then
    raise ESetError.Create('Item not found.');
  Result := AItem;
end;

procedure TSet<T>.AddRange(AItems: array of T);
var
  Item: T;
begin
  EnsureCapacity(Count + Length(AItems));
  for Item in AItems do
    Add(Item);
end;

procedure TSet<T>.AddRange(AItems: IIterable<T>);
begin
  AddRange(AItems.GetEnumerator);
end;

procedure TSet<T>.AddRange(AItems: IIterator<T>);
begin
  while AItems.MoveNext do
    Add(AItems.Current);
end;

function TSet<T>.ReadonlyCollection: IReadonlyCollection<T>;
begin
  Result := Self;
end;

function TSet<T>.ReadonlySet: IReadonlySet<T>;
begin
  Result := Self;
end;

function TSet<T>.HashCollection: IHashCollection<T>;
begin
  Result := Self;
end;

function TSet<T>.Copy: ISet<T>;
begin
  Result := TSet<T>.Create;
  Result.Hash := Hash;
  Result.Equate := Equate;
  Result.HashCollection.RehashFor(Count);
  Result.AddRange(Self);
end;

{ TObjectSet<T> }

function TObjectSet<T>.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

procedure TObjectSet<T>.SetOwnsValues(AValue: Boolean);
begin
  FOwnsValues := AValue;
end;

procedure TObjectSet<T>.SetItem(AItem: T; AValue: Boolean);
begin
  if AValue then
    Add(AItem)
  else
    Remove(AItem);
end;

constructor TObjectSet<T>.Create;
begin
  inherited;
  FOwnsValues := True;
end;

destructor TObjectSet<T>.Destroy;
var
  Item: T;
begin
  if OwnsValues then
    for Item in Self do
      Item.Free;
  inherited;
end;

function TObjectSet<T>.Remove(AItem: T): Boolean;
begin
  Result := inherited;
  if Result and OwnsValues then
    AItem.Free;
end;

procedure TObjectSet<T>.Clear;
var
  Item: T;
begin
  if OwnsValues then
    for Item in Self do
      Item.Free;
  inherited;
end;

{ TMap<K, V>.TCollection }

function TMap<K, V>.TCollection.Empty: Boolean;
begin
  Result := FMap.Empty;
end;

function TMap<K, V>.TCollection.GetCount: Integer;
begin
  Result := FMap.Count;
end;

constructor TMap<K, V>.TCollection.Create(AMap: TMap<K, V>);
begin
  FMap := AMap;
end;

{ TMap<K, V>.TKeyIterator }

function TMap<K, V>.TKeyIterator.GetCurrent: K;
begin
  Result := FHashBase.FItems[FBucket, FIndex].Value.Key;
end;

{ TMap<K, V>.TValueIterator }

function TMap<K, V>.TValueIterator.GetCurrent: V;
begin
  Result := FHashBase.FItems[FBucket, FIndex].Value.Value;
end;

{ TMap<K, V>.TFindKeysIterator }

constructor TMap<K, V>.TFindKeysIterator.Create(AMap: TMap<K, V>; AValue: V);
begin
  inherited Create(AMap);
  FValue := AValue;
end;

function TMap<K, V>.TFindKeysIterator.MoveNext: Boolean;
begin
  repeat
    Result := inherited MoveNext;
  until not Result or TMap<K, V>(FHashBase).EquateValue(FHashBase.FItems[FBucket, FIndex].Value.Value, FValue);
end;

{ TMap<K, V>.TKeyCollection }

function TMap<K, V>.TKeyCollection.GetEnumerator: IIterator<K>;
begin
  Result := TKeyIterator.Create(FMap);
end;

function TMap<K, V>.TKeyCollection.Iterate: IIterate<K>;
begin
  Result := TIterableIterate<K>.Create(Self);
end;

function TMap<K, V>.TKeyCollection.Contains(AItem: K): Boolean;
begin
  Result := FMap.ContainsKey(AItem);
end;

{ TMap<K, V>.TValueCollection }

function TMap<K, V>.TValueCollection.GetEnumerator: IIterator<V>;
begin
  Result := TValueIterator.Create(FMap);
end;

function TMap<K, V>.TValueCollection.Iterate: IIterate<V>;
begin
  Result := TIterableIterate<V>.Create(Self);
end;

function TMap<K, V>.TValueCollection.Contains(AItem: V): Boolean;
begin
  Result := FMap.ContainsValue(AItem);
end;

{ TMap<K, V>.TFindKeysIterable }

constructor TMap<K, V>.TFindKeysIterable.Create(AMap: TMap<K, V>; AValue: V);
begin
  FMap := AMap;
  FValue := AValue;
end;

function TMap<K, V>.TFindKeysIterable.GetEnumerator: IIterator<K>;
begin
  Result := TFindKeysIterator.Create(FMap, FValue);
end;

function TMap<K, V>.TFindKeysIterable.Iterate: IIterate<K>;
begin
  Result := TIterableIterate<K>.Create(Self);
end;

{ TMap<K, V> }

function TMap<K, V>.GetCount: Integer;
begin
  Result := FCount;
end;

function TMap<K, V>.GetItem(AKey: K): V;
begin
  if not Get(AKey, Result) then
    raise EMapError.Create('Key not found.');
end;

function TMap<K, V>.GetKey(AValue: V; out AKey: K): Boolean;
var
  Bucket, Index: Integer;
begin
  Result := FindValue(AValue, Bucket, Index);
  if Result then
    AKey := FItems[Bucket, Index].Value.Key;
end;

function TMap<K, V>.GetKeys(AValue: V): IIterable<K>;
begin
  Result := TFindKeysIterable.Create(Self, AValue);
end;

function TMap<K, V>.HashCollection: IHashCollection<K>;
begin
  Result := Self;
end;

procedure TMap<K, V>.SetItem(AKey: K; AValue: V);
var
  Bucket, Index: Integer;
  Pair: THashValue<TPair<K, V>>;
begin
  EnsureCapacity(Count + 1);
  Pair := MakeHashPair(AKey, AValue);
  if FindKey(AKey, Pair.Hash, Bucket, Index) then
    FItems[Bucket, Index] := Pair
  else
  begin
    Insert(Pair, FItems[Bucket], Index);
    Inc(FCount);
  end;
end;

function TMap<K, V>.GetEquateKey: TFunc<K, K, Boolean>;
begin
  Result := Equate;
end;

function TMap<K, V>.GetEquateValue: TFunc<V, V, Boolean>;
begin
  Result := FEquateValue;
end;

function TMap<K, V>.GetHashKey: TFunc<K, Cardinal>;
begin
  Result := Hash;
end;

procedure TMap<K, V>.SetEquateKey(const Value: TFunc<K, K, Boolean>);
begin
  Equate := Value;
end;

procedure TMap<K, V>.SetEquateValue(const Value: TFunc<V, V, Boolean>);
begin
  if Assigned(Value) then
    FEquateValue := Value
  else
    FEquateValue := TDefault.Equate<V>;
end;

procedure TMap<K, V>.SetHashKey(const Value: TFunc<K, Cardinal>);
begin
  Hash := Value;
end;

constructor TMap<K, V>.Create;
begin
  inherited;
  FEquateValue := TDefault.Equate<V>;
end;

constructor TMap<K, V>.Create(APairs: array of TPair<K, V>);
begin
  Create;
  AddRange(APairs);
end;

constructor TMap<K, V>.Create(APairs: IIterator < TPair < K, V >> );
begin
  Create;
  AddRange(APairs);
end;

constructor TMap<K, V>.Create(APairs: IIterable < TPair < K, V >> );
begin
  Create;
  AddRange(APairs);
end;

function TMap<K, V>.GetEnumerator: IIterator<TPair<K, V>>;
begin
  Result := TIterator.Create(Self);
end;

function TMap<K, V>.Iterate: IIterate<TPair<K, V>>;
begin
  Result := TIterableIterate < TPair < K, V >>.Create(Self);
end;

function TMap<K, V>.Empty: Boolean;
begin
  Result := FCount = 0;
end;

function TMap<K, V>.Extract(AKey: K): V;
begin
  if not Extract(AKey, Result) then
    raise EMapError.Create('Key not found.');
end;

function TMap<K, V>.Extract(AKey: K; out AValue: V): Boolean;
var
  Bucket, Index: Integer;
begin
  Result := FindKey(AKey, Bucket, Index);
  if Result then
  begin
    AValue := FItems[Bucket, Index].Value.Value;
    DoRemove(Bucket, Index);
  end;
end;

function TMap<K, V>.Extract(APair: TPair<K, V>): TPair<K, V>;
begin
  if not Remove(APair) then
    raise EMapError.Create('Pair not found.');
  Result := APair;
end;

function TMap<K, V>.FindKey(AKey: K; AHash: Cardinal; out ABucket, AIndex: Integer): Boolean;
var
  BucketLen: Integer;
begin
  if BucketCount = 0 then
    Exit(False);
  ABucket := HashToBucket(AHash);
  AIndex := 0;
  if Count = 0 then
    Exit(False);
  BucketLen := Length(FItems[ABucket]);
  while AIndex < BucketLen do
  begin
    if (AHash = FItems[ABucket, AIndex].Hash) and EquateKey(AKey, FItems[ABucket, AIndex].Value.Key) then
      Exit(True);
    Inc(AIndex);
  end;
  Result := False;
end;

function TMap<K, V>.FindValue(AValue: V; out ABucket, AIndex: Integer): Boolean;
var
  Bucket, Index: Integer;
begin
  for Bucket := 0 to High(FItems) do
    for Index := 0 to High(FItems[Bucket]) do
      if EquateValue(FItems[Bucket, Index].Value.Value, AValue) then
        Exit(True);
  Result := False;
end;

function TMap<K, V>.FindKey(AKey: K; out ABucket, AIndex: Integer): Boolean;
begin
  Result := FindKey(AKey, HashKey(AKey), ABucket, AIndex);
end;

function TMap<K, V>.Contains(APair: TPair<K, V>): Boolean;
var
  Value: V;
begin
  Result := Get(APair.Key, Value) and EquateValue(APair.Value, Value);
end;

function TMap<K, V>.Add(APair: TPair<K, V>): Boolean;
begin
  Result := Add(APair.Key, APair.Value);
end;

function TMap<K, V>.Remove(APair: TPair<K, V>): Boolean;
begin
  Result := Remove(APair.Key, APair.Value);
end;

procedure TMap<K, V>.AddRange(APairs: array of TPair<K, V>);
var
  Pair: TPair<K, V>;
begin
  EnsureCapacity(Count + Length(APairs));
  for Pair in APairs do
    Add(Pair);
end;

procedure TMap<K, V>.AddRange(APairs: IIterable < TPair < K, V >> );
begin
  AddRange(APairs.GetEnumerator);
end;

function TMap<K, V>.Add(AKey: K; AValue: V): Boolean;
var
  Pair: THashValue<TPair<K, V>>;
  Bucket, Index: Integer;
begin
  EnsureCapacity(Count + 1);
  Pair := MakeHashPair(AKey, AValue);
  Result := not FindKey(AKey, Pair.Hash, Bucket, Index);
  if Result then
  begin
    Insert(Pair, FItems[Bucket], Index);
    Inc(FCount);
  end;
end;

procedure TMap<K, V>.AddRange(APairs: IIterator < TPair < K, V >> );
begin
  while APairs.MoveNext do
    Add(APairs.Current);
end;

function TMap<K, V>.ReadonlyCollection: IReadonlyCollection<TPair<K, V>>;
begin
  Result := Self;
end;

function TMap<K, V>.Keys: IReadonlyCollection<K>;
begin
  Result := TKeyCollection.Create(Self);
end;

function TMap<K, V>.MakeHashPair(AKey: K; AValue: V): THashValue<TPair<K, V>>;
begin
  Result := MakeHashValue(AKey, TPair<K, V>.Create(AKey, AValue));
end;

function TMap<K, V>.Values: IReadonlyCollection<V>;
begin
  Result := TValueCollection.Create(Self);
end;

function TMap<K, V>.ContainsKey(AKey: K): Boolean;
var
  Bucket, Index: Integer;
begin
  Result := FindKey(AKey, Bucket, Index);
end;

function TMap<K, V>.ContainsValue(AValue: V): Boolean;
var
  Bucket, Index: Integer;
begin
  Result := FindValue(AValue, Bucket, Index);
end;

function TMap<K, V>.Copy: IMap<K, V>;
begin
  Result := TMap<K, V>.Create;
  Result.HashKey := HashKey;
  Result.EquateKey := EquateKey;
  Result.EquateValue := EquateValue;
  Result.HashCollection.RehashFor(Count);
  Result.AddRange(Self);
end;

function TMap<K, V>.Get(AKey: K; out AValue: V): Boolean;
var
  Bucket, Index: Integer;
begin
  Result := FindKey(AKey, Bucket, Index);
  if Result then
    AValue := FItems[Bucket, Index].Value.Value;
end;

function TMap<K, V>.Remove(AKey: K): Boolean;
var
  Bucket, Index: Integer;
begin
  Result := FindKey(AKey, Bucket, Index);
  if Result then
    DoRemove(Bucket, Index);
end;

function TMap<K, V>.ReadonlyMap: IReadonlyMap<K, V>;
begin
  Result := Self;
end;

function TMap<K, V>.Remove(AKey: K; AValue: V): Boolean;
var
  Bucket, Index: Integer;
begin
  Result := FindKey(AKey, Bucket, Index) and EquateValue(FItems[Bucket, Index].Value.Value, AValue);
  if Result then
    DoRemove(Bucket, Index);
end;

{ TObjectMap<K, V> }

function TObjectMap<K, V>.GetOwnsKeys: Boolean;
begin
  Result := FOwnsKeys;
end;

procedure TObjectMap<K, V>.SetOwnsKeys(AValue: Boolean);
begin
  FOwnsKeys := AValue;
end;

procedure TObjectMap<K, V>.SetItem(AKey: K; AValue: V);
var
  Bucket, Index: Integer;
  Pair: THashValue<TPair<K, V>>;
begin
  EnsureCapacity(Count + 1);
  Pair := MakeHashPair(AKey, AValue);
  if FindKey(AKey, Pair.Hash, Bucket, Index) then
  begin
    if OwnsKeys then
      FItems[Bucket, Index].Value.Key.Free;
    FItems[Bucket, Index] := Pair;
  end
  else
  begin
    Insert(Pair, FItems[Bucket], Index);
    Inc(FCount);
  end;
end;

constructor TObjectMap<K, V>.Create;
begin
  inherited;
  FOwnsKeys := True;
end;

destructor TObjectMap<K, V>.Destroy;
var
  Key: K;
begin
  if OwnsKeys then
    for Key in Keys do
      Key.Free;
  inherited;
end;

function TObjectMap<K, V>.Remove(APair: TPair<K, V>): Boolean;
begin
  Result := inherited;
  if Result and OwnsKeys then
    APair.Key.Free;
end;

function TObjectMap<K, V>.Remove(AKey: K): Boolean;
begin
  Result := inherited;
  if Result and OwnsKeys then
    AKey.Free;
end;

function TObjectMap<K, V>.Remove(AKey: K; AValue: V): Boolean;
begin
  Result := inherited;
  if Result and OwnsKeys then
    AKey.Free;
end;

procedure TObjectMap<K, V>.Clear;
var
  Key: K;
begin
  if OwnsKeys then
    for Key in Keys do
      Key.Free;
  inherited;
end;

{ TToObjectMap<K, V> }

function TToObjectMap<K, V>.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

procedure TToObjectMap<K, V>.SetOwnsValues(AValue: Boolean);
begin
  FOwnsValues := AValue;
end;

procedure TToObjectMap<K, V>.SetItem(AKey: K; AValue: V);
var
  Bucket, Index: Integer;
  Pair: THashValue<TPair<K, V>>;
begin
  EnsureCapacity(Count + 1);
  Pair := MakeHashPair(AKey, AValue);
  if FindKey(AKey, Pair.Hash, Bucket, Index) then
  begin
    if OwnsValues then
      FItems[Bucket, Index].Value.Value.Free;
    FItems[Bucket, Index] := Pair;
  end
  else
  begin
    Insert(Pair, FItems[Bucket], Index);
    Inc(FCount);
  end;
end;

constructor TToObjectMap<K, V>.Create;
begin
  inherited;
  FOwnsValues := True;
end;

destructor TToObjectMap<K, V>.Destroy;
var
  Value: V;
begin
  if OwnsValues then
    for Value in Values do
      Value.Free;
  inherited;
end;

function TToObjectMap<K, V>.Remove(APair: TPair<K, V>): Boolean;
begin
  Result := inherited;
  if inherited and OwnsValues then
    APair.Value.Free;
end;

function TToObjectMap<K, V>.Remove(AKey: K): Boolean;
var
  Value: V;
begin
  if OwnsValues then
  begin
    Result := Extract(AKey, Value);
    if Result then
      Value.Free;
  end
  else
    Result := inherited;
end;

function TToObjectMap<K, V>.Remove(AKey: K; AValue: V): Boolean;
begin
  Result := inherited;
  if Result and OwnsValues then
    AValue.Free;
end;

procedure TToObjectMap<K, V>.Clear;
var
  Value: V;
begin
  if OwnsValues then
    for Value in Values do
      Value.Free;
  inherited;
end;

{ TObjectObjectMap<K, V> }

function TObjectObjectMap<K, V>.GetOwnsKeys: Boolean;
begin
  Result := FOwnsKeys;
end;

procedure TObjectObjectMap<K, V>.SetOwnsKeys(AValue: Boolean);
begin
  FOwnsKeys := AValue;
end;

function TObjectObjectMap<K, V>.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

procedure TObjectObjectMap<K, V>.SetOwnsValues(AValue: Boolean);
begin
  FOwnsValues := AValue;
end;

procedure TObjectObjectMap<K, V>.SetItem(AKey: K; AValue: V);
var
  Bucket, Index: Integer;
  Pair: THashValue<TPair<K, V>>;
begin
  EnsureCapacity(Count + 1);
  Pair := MakeHashPair(AKey, AValue);
  if FindKey(AKey, Pair.Hash, Bucket, Index) then
  begin
    if OwnsKeys then
      FItems[Bucket, Index].Value.Key.Free;
    if OwnsValues then
      FItems[Bucket, Index].Value.Value.Free;
    FItems[Bucket, Index] := Pair;
  end
  else
  begin
    Insert(Pair, FItems[Bucket], Index);
    Inc(FCount);
  end;
end;

constructor TObjectObjectMap<K, V>.Create;
begin
  inherited;
  FOwnsKeys := True;
  FOwnsValues := True;
end;

destructor TObjectObjectMap<K, V>.Destroy;
var
  Key: K;
  Value: V;
begin
  if OwnsKeys then
    for Key in Keys do
      Key.Free;
  if OwnsValues then
    for Value in Values do
      Value.Free;
  inherited;
end;

function TObjectObjectMap<K, V>.Remove(APair: TPair<K, V>): Boolean;
begin
  Result := inherited;
  if Result then
  begin
    if OwnsKeys then
      APair.Key.Free;
    if OwnsValues then
      APair.Value.Free;
  end;
end;

function TObjectObjectMap<K, V>.Remove(AKey: K): Boolean;
var
  Value: V;
begin
  if OwnsValues then
  begin
    Result := Extract(AKey, Value);
    if Result then
    begin
      if OwnsKeys then
        AKey.Free;
      Value.Free;
    end;
  end
  else
  begin
    Result := inherited;
    if Result and OwnsKeys then
      AKey.Free;
  end;
end;

function TObjectObjectMap<K, V>.Remove(AKey: K; AValue: V): Boolean;
begin
  Result := inherited;
  if Result then
  begin
    if OwnsKeys then
      AKey.Free;
    if OwnsValues then
      AValue.Free;
  end;
end;

procedure TObjectObjectMap<K, V>.Clear;
var
  Key: K;
  Value: V;
begin
  if OwnsKeys then
    for Key in Keys do
      Key.Free;
  if OwnsValues then
    for Value in Values do
      Value.Free;
  inherited;
end;

{ TIterate<T> }

function TIterate<T>.GetItem(AIndex: Integer): T;
begin
  if AIndex >= 0 then
  begin
    for Result in Self do
    begin
      if AIndex = 0 then
        Exit;
      Dec(AIndex);
    end;
  end;
  raise EIterateError.Create('The iteration index is out of bounds.');
end;

function TIterate<T>.Iterate: IIterate<T>;
begin
  Result := Self;
end;

function TIterate<T>.Count: Integer;
var
  Item: T;
begin
  Result := 0;
  for Item in Self do
    Inc(Result);
end;

function TIterate<T>.Count(APredicate: TPredicate<T>): Integer;
var
  Item: T;
begin
  Result := 0;
  for Item in Self do
    if APredicate(Item) then
      Inc(Result);
end;

function TIterate<T>.Empty: Boolean;
var
  Item: T;
begin
  for Item in Self do
    Exit(False);
  Result := True;
end;

function TIterate<T>.Any: Boolean;
var
  Item: T;
begin
  for Item in Self do
    Exit(True);
  Result := False;
end;

function TIterate<T>.Any(APredicate: TPredicate<T>): Boolean;
var
  Item: T;
begin
  for Item in Self do
    if APredicate(Item) then
      Exit(True);
  Result := False;
end;

function TIterate<T>.All(APredicate: TPredicate<T>): Boolean;
var
  Item: T;
begin
  for Item in Self do
    if not APredicate(Item) then
      Exit(False);
  Result := True;
end;

function TIterate<T>.HasAtLeast(ACount: Integer): Boolean;
var
  Item: T;
begin
  for Item in Self do
  begin
    if ACount <= 0 then
      Break;
    Dec(ACount);
  end;
  Result := ACount <= 0;
end;

function TIterate<T>.HasAtLeast(ACount: Integer; APredicate: TPredicate<T>): Boolean;
var
  Item: T;
begin
  for Item in Self do
  begin
    if ACount <= 0 then
      Break;
    if APredicate(Item) then
      Dec(ACount);
  end;
  Result := ACount <= 0;
end;

function TIterate<T>.HasAtMost(ACount: Integer): Boolean;
var
  Item: T;
begin
  for Item in Self do
  begin
    if ACount < 0 then
      Break;
    Dec(ACount);
  end;
  Result := ACount >= 0;
end;

function TIterate<T>.HasAtMost(ACount: Integer; APredicate: TPredicate<T>): Boolean;
var
  Item: T;
begin
  for Item in Self do
  begin
    if ACount < 0 then
      Break;
    if APredicate(Item) then
      Dec(ACount);
  end;
  Result := ACount >= 0;
end;

function TIterate<T>.HasExactly(ACount: Integer): Boolean;
var
  Item: T;
begin
  for Item in Self do
  begin
    if ACount < 0 then
      Break;
    Dec(ACount);
  end;
  Result := ACount = 0;
end;

function TIterate<T>.HasExactly(ACount: Integer; APredicate: TPredicate<T>): Boolean;
var
  Item: T;
begin
  for Item in Self do
  begin
    if ACount < 0 then
      Break;
    if APredicate(Item) then
      Dec(ACount);
  end;
  Result := ACount = 0;
end;

function TIterate<T>.Reduce(AFunc: TFunc<T, T, T>): T;
var
  Iterator: IIterator<T>;
begin
  Iterator := GetEnumerator;
  if not Iterator.MoveNext then
    raise EIterateError.Create('Operation requires at least one element in iterable.');
  Result := Iterator.Current;
  while Iterator.MoveNext do
    Result := AFunc(Result, Iterator.Current);
end;

function TIterate<T>.Reduce(AFunc: TFunc<T, T, T>; ASeed: T): T;
var
  Iterator: IIterator<T>;
begin
  Iterator := GetEnumerator;
  Result := ASeed;
  while Iterator.MoveNext do
    Result := AFunc(Result, Iterator.Current);
end;

function TIterate<T>.First: T;
begin
  for Result in Self do
    Exit;
  raise EIterateError.Create('Iterate is empty.');
end;

function TIterate<T>.Last: T;
var
  Iterator: IIterator<T>;
begin
  Iterator := GetEnumerator;
  if not Iterator.MoveNext then
    raise EIterateError.Create('Iterate is empty.');
  while Iterator.MoveNext do
    Result := Iterator.Current;
end;

function TIterate<T>.Range(AIndex, ACount: Integer): IIterate<T>;
begin
  Result := TRangeIterate<T>.Create(Self, AIndex, ACount);
end;

function TIterate<T>.Where(APredicate: TPredicate<T>): IIterate<T>;
begin
  Result := TWhereIterate<T>.Create(Self, APredicate);
end;

function TIterate<T>.Take(ACount: Integer): IIterate<T>;
begin
  Result := TRangeIterate<T>.Create(Self, 0, ACount);
end;

function TIterate<T>.TakeWhile(APredicate: TPredicate<T>): IIterate<T>;
begin
  Result := TTakeWhileIterate<T>.Create(Self, APredicate);
end;

function TIterate<T>.Skip(ACount: Integer): IIterate<T>;
begin
  Result := TRangeIterate<T>.Create(Self, ACount, Integer.MaxValue);
end;

function TIterate<T>.SkipUntil(APredicate: TPredicate<T>): IIterate<T>;
begin
  Result := TSkipUntilIterate<T>.Create(Self, APredicate);
end;

function TIterate<T>.Concat(AIterable: IIterable<T>): IIterate<T>;
begin
  Result := TConcatIterate<T>.Create(Self, AIterable);
end;

function TIterate<T>.Append(AItem: T): IIterate<T>;
begin
  Result := TAppendIterate<T>.Create(Self, AItem);
end;

function TIterate<T>.Prepend(AItem: T): IIterate<T>;
begin
  Result := TPrependIterate<T>.Create(Self, AItem);
end;

function TIterate<T>.Map(AFunc: TFunc<T, T>): IIterate<T>;
begin
  Result := TMapIterate<T, T>.Create(Self, AFunc);
end;

function TIterate<T>.Zip(AIterable: IIterable<T>; AFunc: TFunc<T, T, T>): IIterate<T>;
begin
  Result := TZipIterate<T, T, T>.Create(Self, AIterable, AFunc);
end;

function TIterate<T>.Generic: TGenericWrapper<T>;
begin
  Result.Create(Self);
end;

function TIterate<T>.ToList: IList<T>;
begin
  Result := TList<T>.Create(Self);
end;

function TIterate<T>.ToSet: ISet<T>;
begin
  Result := TSet<T>.Create(Self);
end;

{ TIterableIterate<T> }

constructor TIterableIterate<T>.Create(AIterable: IIterable<T>);
begin
  FIterable := AIterable;
end;

function TIterableIterate<T>.GetEnumerator: IIterator<T>;
begin
  Result := FIterable.GetEnumerator;
end;

{ TWhereIterate<T>.TIterator }

constructor TWhereIterate<T>.TIterator.Create(AIterator: IIterator<T>; APredicate: TPredicate<T>);
begin
  FIterator := AIterator;
  FPredicate := APredicate;
end;

function TWhereIterate<T>.TIterator.GetCurrent: T;
begin
  Result := FIterator.Current;
end;

function TWhereIterate<T>.TIterator.MoveNext: Boolean;
begin
  repeat
    Result := FIterator.MoveNext;
  until not Result or FPredicate(FIterator.Current);
end;

{ TWhereIterate<T> }

constructor TWhereIterate<T>.Create(AIterable: IIterable<T>; APredicate: TPredicate<T>);
begin
  FIterable := AIterable;
  FPredicate := APredicate;
end;

function TWhereIterate<T>.GetEnumerator: IIterator<T>;
begin
  Result := TIterator.Create(FIterable.GetEnumerator, FPredicate);
end;

{ TTakeWhileIterate<T>.TIterator }

constructor TTakeWhileIterate<T>.TIterator.Create(AIterator: IIterator<T>; APredicate: TPredicate<T>);
begin
  FIterator := AIterator;
  FPredicate := APredicate;
end;

function TTakeWhileIterate<T>.TIterator.GetCurrent: T;
begin
  Result := FIterator.Current;
end;

function TTakeWhileIterate<T>.TIterator.MoveNext: Boolean;
begin
  Result := FIterator.MoveNext and FPredicate(FIterator.Current);
end;

{ TTakeWhileIterate<T> }

constructor TTakeWhileIterate<T>.Create(AIterable: IIterable<T>; APredicate: TPredicate<T>);
begin
  FIterable := AIterable;
  FPredicate := APredicate;
end;

function TTakeWhileIterate<T>.GetEnumerator: IIterator<T>;
begin
  Result := TIterator.Create(FIterable.GetEnumerator, FPredicate);
end;

{ TSkipUntilIterate<T>.TIterator }

constructor TSkipUntilIterate<T>.TIterator.Create(AIterator: IIterator<T>; APredicate: TPredicate<T>);
begin
  FIterator := AIterator;
  if not FIterator.MoveNext then
    Exit;
  while not APredicate(FIterator.Current) do
    if not FIterator.MoveNext then
      Exit;
  FState := 1;
end;

function TSkipUntilIterate<T>.TIterator.GetCurrent: T;
begin
  Result := FIterator.Current;
end;

function TSkipUntilIterate<T>.TIterator.MoveNext: Boolean;
begin
  case FState of
    0:
      Result := False;
    1:
      begin
        Inc(FState);
        Result := True;
      end;
    2:
      Result := FIterator.MoveNext;
  end;
end;

{ TSkipUntilIterate<T> }

constructor TSkipUntilIterate<T>.Create(AIterable: IIterable<T>; APredicate: TPredicate<T>);
begin
  FIterable := AIterable;
  FPredicate := APredicate;
end;

function TSkipUntilIterate<T>.GetEnumerator: IIterator<T>;
begin
  Result := TIterator.Create(FIterable.GetEnumerator, FPredicate);
end;

{ TRangeIterate<T>.TIterator }

constructor TRangeIterate<T>.TIterator.Create(AIterator: IIterator<T>; AIndex, ACount: Integer);
begin
  FIterator := AIterator;
  FCount := ACount;
  FNotEmpty := True;
  while (AIndex > 0) and FNotEmpty do
  begin
    FNotEmpty := FIterator.MoveNext;
    Dec(AIndex);
  end;
end;

function TRangeIterate<T>.TIterator.GetCurrent: T;
begin
  Result := FIterator.Current;
end;

function TRangeIterate<T>.TIterator.MoveNext: Boolean;
begin
  Dec(FCount);
  Result := FNotEmpty and (FCount >= 0) and FIterator.MoveNext;
end;

{ TRangeIterate<T> }

constructor TRangeIterate<T>.Create(AIterable: IIterable<T>; AIndex, ACount: Integer);
begin
  FIterable := AIterable;
  FIndex := AIndex;
  FCount := ACount;
end;

function TRangeIterate<T>.GetEnumerator: IIterator<T>;
begin
  Result := TIterator.Create(FIterable.GetEnumerator, FIndex, FCount);
end;

{ TMapIterate<T, R>.TIterator }

constructor TMapIterate<T, R>.TIterator.Create(AIterator: IIterator<T>; AFunc: TFunc<T, R>);
begin
  FIterator := AIterator;
  FFunc := AFunc;
end;

function TMapIterate<T, R>.TIterator.GetCurrent: R;
begin
  Result := FFunc(FIterator.Current);
end;

function TMapIterate<T, R>.TIterator.MoveNext: Boolean;
begin
  Result := FIterator.MoveNext;
end;

{ TMapIterate<T, R> }

constructor TMapIterate<T, R>.Create(AIterable: IIterable<T>; AFunc: TFunc<T, R>);
begin
  FIterable := AIterable;
  FFunc := AFunc;
end;

function TMapIterate<T, R>.GetEnumerator: IIterator<R>;
begin
  Result := TIterator.Create(FIterable.GetEnumerator, FFunc);
end;

{ TZipIterate<T, U, R>.TIterator }

constructor TZipIterate<T, U, R>.TIterator.Create(AIterator1: IIterator<T>; AIterator2: IIterator<U>;
AFunc: TFunc<T, U, R>);
begin
  FIterator1 := AIterator1;
  FIterator2 := AIterator2;
  FFunc := AFunc;
end;

function TZipIterate<T, U, R>.TIterator.GetCurrent: R;
begin
  Result := FFunc(FIterator1.Current, FIterator2.Current);
end;

function TZipIterate<T, U, R>.TIterator.MoveNext: Boolean;
begin
  Result := FIterator1.MoveNext and FIterator2.MoveNext;
end;

{ TZipIterate<T, U, R> }

constructor TZipIterate<T, U, R>.Create(AIterable1: IIterable<T>; AIterable2: IIterable<U>; AFunc: TFunc<T, U, R>);
begin
  FIterable1 := AIterable1;
  FIterable2 := AIterable2;
  FFunc := AFunc;
end;

function TZipIterate<T, U, R>.GetEnumerator: IIterator<R>;
begin
  Result := TIterator.Create(FIterable1.GetEnumerator, FIterable2.GetEnumerator, FFunc);
end;

{ TConcatIterate<T>.TIterator }

constructor TConcatIterate<T>.TIterator.Create(AIterator1, AIterator2: IIterator<T>);
begin
  FIterator1 := AIterator1;
  FIterator2 := AIterator2;
end;

function TConcatIterate<T>.TIterator.GetCurrent: T;
begin
  if FIterator1 = nil then
    Exit(FIterator2.Current);
  Result := FIterator1.Current;
end;

function TConcatIterate<T>.TIterator.MoveNext: Boolean;
begin
  if FIterator1 = nil then
    Exit(FIterator2.MoveNext);
  Result := FIterator1.MoveNext;
  if not Result then
    FIterator1 := nil;
end;

{ TConcatIterate<T> }

constructor TConcatIterate<T>.Create(AIterable1, AIterable2: IIterable<T>);
begin
  FIterable1 := AIterable1;
  FIterable2 := AIterable2;
end;

function TConcatIterate<T>.GetEnumerator: IIterator<T>;
begin
  Result := TIterator.Create(FIterable1.GetEnumerator, FIterable2.GetEnumerator);
end;

{ TAppendIterate<T>.TIterator }

constructor TAppendIterate<T>.TIterator.Create(AIterator: IIterator<T>; AItem: T);
begin
  FIterator := AIterator;
  FItem := AItem;
end;

function TAppendIterate<T>.TIterator.GetCurrent: T;
begin
  if FIterator = nil then
    Exit(FItem);
  Result := FIterator.Current;
end;

function TAppendIterate<T>.TIterator.MoveNext: Boolean;
begin
  if FIterator = nil then
    Exit(False);
  if not FIterator.MoveNext then
    FIterator := nil;
  Result := True;
end;

{ TAppendIterate<T> }

constructor TAppendIterate<T>.Create(AIterable: IIterable<T>; AItem: T);
begin
  FIterable := AIterable;
  FItem := AItem;
end;

function TAppendIterate<T>.GetEnumerator: IIterator<T>;
begin
  Result := TIterator.Create(FIterable.GetEnumerator, FItem);
end;

{ TPrependIterate<T>.TIterator }

constructor TPrependIterate<T>.TIterator.Create(AIterator: IIterator<T>; AItem: T);
begin
  FIterator := AIterator;
  FItem := AItem;
end;

function TPrependIterate<T>.TIterator.GetCurrent: T;
begin
  if FState < 2 then
    Exit(FItem);
  Result := FIterator.Current;
end;

function TPrependIterate<T>.TIterator.MoveNext: Boolean;
begin
  if FState < 1 then
    Result := True
  else
    Result := FIterator.MoveNext;
  if FState < 2 then
    Inc(FState);
end;

{ TPrependIterate<T> }

constructor TPrependIterate<T>.Create(AIterable: IIterable<T>; AItem: T);
begin
  FIterable := AIterable;
  FItem := AItem;
end;

function TPrependIterate<T>.GetEnumerator: IIterator<T>;
begin
  Result := TIterator.Create(FIterable.GetEnumerator, FItem);
end;

{ TIntRangeIterate.TIterator }

constructor TIntRangeIterate.TIterator.Create(AStart, AStop, AStep: Integer);
begin
  FCurrent := AStart - AStep;
  FStop := AStop;
  FStep := AStep;
end;

function TIntRangeIterate.TIterator.GetCurrent: Integer;
begin
  Result := FCurrent;
end;

function TIntRangeIterate.TIterator.MoveNext: Boolean;
begin
  Inc(FCurrent, FStep);
  Result := FCurrent < FStop;
end;

{ TIntRangeIterate }

constructor TIntRangeIterate.Create(AStart, AStop, AStep: Integer);
begin
  FStart := AStart;
  FStop := AStop;
  FStep := AStep;
end;

function TIntRangeIterate.GetEnumerator: IIterator<Integer>;
begin
  Result := TIterator.Create(FStart, FStop, FStep);
end;

{ TListReverseIterator<T> }

constructor TListReverseIterator<T>.Create(AList: TListBase<T>);
begin
  inherited;
  FCurrent := FList.Count;
end;

function TListReverseIterator<T>.MoveNext: Boolean;
begin
  Dec(FCurrent);
  Result := FCurrent >= 0;
end;

{ TListReverseIterate<T> }

constructor TListReverseIterate<T>.Create(AListBase: TListBase<T>);
begin
  FListBase := AListBase;
end;

function TListReverseIterate<T>.GetEnumerator: IIterator<T>;
begin
  Result := TListReverseIterator<T>.Create(FListBase);
end;

{ TListIterator<T> }

constructor TListIterator<T>.Create(AList: TListBase<T>);
begin
  inherited;
  FCurrent := -1;
end;

function TListIterator<T>.MoveNext: Boolean;
begin
  Inc(FCurrent);
  Result := FCurrent < FList.Count;
end;

{ TEmptyIterator<T> }

function TEmptyIterator<T>.GetCurrent: T;
begin
  Exit(Default (T));
end;

function TEmptyIterator<T>.MoveNext: Boolean;
begin
  Result := False;
end;

{ TEmptyIterable<T> }

function TEmptyIterable<T>.GetEnumerator: IIterator<T>;
begin
  Result := TEmptyIterator<T>.Create;
end;

function TEmptyIterable<T>.Iterate: IIterate<T>;
begin
  Result := TIterableIterate<T>.Create(Self);
end;

{ TStack<T> }

function TStack<T>.GetEnumerator: IIterator<T>;
begin
  Result := TPopper.Create(Self);
end;

function TStack<T>.GetTop: T;
begin
  Result := Last;
end;

function TStack<T>.Iterate: IIterate<T>;
begin
  Result := TIterableIterate<T>.Create(Self);
end;

function TStack<T>.Pop: T;
begin
  Result := Last;
  RemoveAt(MaxIndex);
end;

function TStack<T>.PopMany: IIterable<T>;
begin
  Result := Self;
end;

procedure TStack<T>.Push(AItem: T);
begin
  Add(AItem);
end;

{ TQueue<T> }

function TQueue<T>.Dequeue: T;
begin
  Result := Items[0];
  RemoveAt(0);
end;

function TQueue<T>.DequeueMany: IIterable<T>;
begin
  Result := Self;
end;

procedure TQueue<T>.Enqueue(AItem: T);
begin
  Add(AItem);
end;

function TQueue<T>.GetEnumerator: IIterator<T>;
begin
  Result := TDequeuer.Create(Self);
end;

function TQueue<T>.GetNext: T;
begin
  Result := First;
end;

function TQueue<T>.Iterate: IIterate<T>;
begin
  Result := TIterableIterate<T>.Create(Self);
end;

{ TStack<T>.TPopper }

constructor TStack<T>.TPopper.Create(AStack: IStack<T>);
begin
  FStack := AStack;
end;

function TStack<T>.TPopper.GetCurrent: T;
begin
  Result := FCurrent;
end;

function TStack<T>.TPopper.MoveNext: Boolean;
begin
  Result := not FStack.Empty;
  if Result then
    FCurrent := FStack.Pop;
end;

{ TQueue<T>.TDequeuer }

constructor TQueue<T>.TDequeuer.Create(AQueue: IQueue<T>);
begin
  FQueue := AQueue;
end;

function TQueue<T>.TDequeuer.GetCurrent: T;
begin
  Result := FCurrent;
end;

function TQueue<T>.TDequeuer.MoveNext: Boolean;
begin
  Result := not FQueue.Empty;
  if Result then
    FCurrent := FQueue.Dequeue;
end;

end.
