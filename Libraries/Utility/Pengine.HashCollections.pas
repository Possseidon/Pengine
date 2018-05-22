unit Pengine.HashCollections;

interface

uses
  System.SysUtils,
  System.Math,

  Pengine.CollectionInterfaces,
  Pengine.Interfaces,
  Pengine.Collections,
  Pengine.Equaller,
  Pengine.Hasher,
  Pengine.Vector,
  Pengine.IntMaths;

type

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

  EKeyNotHashable = class(Exception)
  public
    constructor Create;
  end;

  // TODO: XmlDoc
  THashMode = (
    hmManual,
    hmAuto
    );

  // TODO: XmlDoc
  THashBase = class(TInterfaceBase)
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
    TIterator<T> = class abstract(TInterfacedObject, IIterator<T>)
    private
      FHashBase: THashBase;
      FIndex: Integer;
      FBucketIndex: Integer;

      function GetBucketSize(AIndex: Integer): Integer;

    protected
      property HashBase: THashBase read FHashBase;
      property Index: Integer read FIndex;
      property BucketIndex: Integer read FBucketIndex;

    public
      constructor Create(AHashBase: THashBase);

      function MoveNext: Boolean;
      function GetCurrent: T; virtual; abstract;

      property Current: T read GetCurrent;

    end;

    TReader = class
    private
      function GetBuckets: Integer;
      function GetHashMode: THashMode;

    public
      property Buckets: Integer read GetBuckets;
      property HashMode: THashMode read GetHashMode;

      function Count: Integer;
      function CountOptimized: Boolean;

      function Empty: Boolean; inline;

      function Copy(AHashMode: THashMode = hmAuto): THashBase;

      function BucketCounts: TIntArray;

    end;

  private
    FHashMode: THashMode;
    FBuckets: TBuckets;

    procedure SetHashMode(const Value: THashMode);
    
  protected
    FCount: Integer;

    function GetBuckets: Integer; virtual; abstract;
    procedure SetBuckets(const Value: Integer); virtual; abstract;
    procedure SetBucketsDirect(Value: Integer); virtual; abstract;

    procedure UnownObjects; virtual;
    procedure ReownObjects; virtual;
    procedure ClearBuckets;
    procedure CopyBuckets(AFrom: THashBase); virtual; abstract;

    // TODO: XmlDoc
    function CreateSame(AHashMode: THashMode = hmAuto): THashBase; inline;

    function CreateBucket: TBucket; virtual; abstract;

    function CreateCopy(AHashMode: THashMode): THashBase; virtual;

  public
    constructor Create(AHashMode: THashMode = hmAuto); virtual;
    destructor Destroy; override;

    class function GetHashBuckets(ACount: Integer): Integer; static;

    property Buckets: Integer read GetBuckets write SetBuckets;
    property HashMode: THashMode read FHashMode write SetHashMode;

    function Count: Integer;
    function CountOptimized: Boolean;

    procedure Rehash(ABuckets: Integer);

    procedure Clear;
    function Empty: Boolean; inline;
    function Copy(AHashMode: THashMode = hmAuto): THashBase;

    function BucketCounts: TIntArray;

    function Reader: TReader; inline;

    procedure Assign(AHashBase: THashBase); virtual;

  end;

  THashBaseClass = class of THashBase;

  // TODO: XmlDoc
  THashBase<K; H: THasher<K>> = class abstract(THashBase)
  public type

    TReader = class(THashBase.TReader)
    public
      function GetCappedHash(AKey: K): Integer;

    end;

  protected
    class function KeysEqual(const AKey1, AKey2: K): Boolean;
    class function GetHash(const AKey: K): Cardinal;
    class function CanIndex(const AKey: K): Boolean;

  public
    // TODO: XmlDoc
    function GetCappedHash(AKey: K): Integer;

    function Copy(AHashMode: THashMode = hmAuto): THashBase<K, H>; reintroduce; inline;

    function Reader: TReader; reintroduce; inline;

  end;

  // TODO: XmlDoc
  TSet<T; H: THasher<T>> = class abstract(THashBase<T, H>, IIterable<T>)
  public type

    // TODO: XmlDoc
    TBucket = TArray<T>;

    // TODO: XmlDoc
    TIterator = class(THashBase.TIterator<T>)
    public
      constructor Create(ASet: TSet<T, H>);

      function GetCurrent: T; override;

    end;

    TReader = class(THashBase<T, H>.TReader)
    private
      function GetElement(AElement: T): Boolean;

    public
      property Elements[AElement: T]: Boolean read GetElement; default;

      function GetEnumerator: IIterator<T>;

      function Copy(AHashMode: THashMode = hmAuto): TSet<T, H>; reintroduce; inline;

      function Union(ASet: TSet<T, H>): TSet<T, H>;
      function Intersection(ASet: TSet<T, H>): TSet<T, H>;
      function Without(ASet: TSet<T, H>): TSet<T, H>;

      function IsSupersetOf(ASet: TSet<T, H>): Boolean;
      function IsSubsetOf(ASet: TSet<T, H>): Boolean;
      function Equals(ASet: TSet<T, H>): Boolean; reintroduce;

    end;

  private
    function GetElement(AElement: T): Boolean;
    procedure SetElement(AElement: T; const Value: Boolean);

  protected
    function GetBuckets: Integer; override;
    procedure SetBuckets(const Value: Integer); override;

    function CreateBucket: THashBase.TBucket; override;

    procedure SetBucketsDirect(Value: Integer); override;

    procedure CopyBuckets(AFrom: THashBase); override;

  public
    property Elements[AElement: T]: Boolean read GetElement write SetElement; default;

    function TryAdd(AElement: T): Boolean;
    procedure Add(AElement: T); overload;
    procedure Add(ASet: TSet<T, H>); overload;

    function TryRemove(AElement: T): Boolean;
    procedure Remove(AElement: T); overload;
    procedure Remove(ASet: TSet<T, H>); overload;

    function GetEnumerator: IIterator<T>;

    function Copy(AHashMode: THashMode = hmAuto): TSet<T, H>; reintroduce; inline;

    function Union(ASet: TSet<T, H>): TSet<T, H>;
    function Intersection(ASet: TSet<T, H>): TSet<T, H>;
    function Without(ASet: TSet<T, H>): TSet<T, H>;

    function IsSupersetOf(ASet: TSet<T, H>): Boolean;
    function IsSubsetOf(ASet: TSet<T, H>): Boolean;
    function Equals(Obj: TObject): Boolean; overload; override;
    function Equals(ASet: TSet<T, H>): Boolean; reintroduce; overload;

    function Reader: TReader; reintroduce; inline;

  end;

  // TODO: XmlDoc
  TMap<K, V; H: THasher<K>> = class abstract(THashBase<K, H>, IIterable<TPair<K, V>>)
  public type
    // TODO: XmlDoc
    TPair = TPair<K, V>;
    // TODO: XmlDoc
    TBucket = TArray<TPair>;

    // TODO: XmlDoc
    TIterator = class(THashBase.TIterator<TPair>)
    public
      function GetCurrent: TPair; override;
    end;

    TKeyIterator = class(THashBase.TIterator<K>)
    public
      function GetCurrent: K; override;
    end;

    TValueIterator = class(THashBase.TIterator<V>)
    public
      function GetCurrent: V; override;
    end;

    TReader = class(THashBase<K, H>.TReader)
    private
      function GetActualKeyE(AKey: K): K;
      function GetPair(AKey: K): TPair;
      function GetValue(AKey: K): V;

    public
      function Get(AKey: K; out AValue: V): Boolean; overload;
      property Values[AKey: K]: V read GetValue; default;
      function Get(AKey: K; out APair: TPair): Boolean; overload;
      property Pairs[AKey: K]: TPair read GetPair;

      function GetActualKey(AKey: K; out AActualKey: K): Boolean;
      property ActualKeys[AKey: K]: K read GetActualKeyE;

      function KeyExists(AKey: K): Boolean;

      function GetEnumerator: IIterator<TPair>;

      function KeySet(AHashMode: THashMode = hmManual): TSet<K, H>;

      function Copy(AHashMode: THashMode = hmAuto): TMap<K, V, H>; reintroduce; inline;

    end;

    TKeysWrapper = record
    private
      FMap: TMap<K, V, H>;

    public
      constructor Create(AMap: TMap<K, V, H>);

      function GetEnumerator: IIterator<K>;

      function ToArray(AGrowAmount: Integer = 16; AShrinkRetain: Integer = 8): TArray<K>;
      function ToSet(AHashMode: THashMode = hmAuto): TSet<K, H>;

    end;

    TValuesWrapper = record
    private
      FMap: TMap<K, V, H>;
    public
      constructor Create(AMap: TMap<K, V, H>);

      function GetEnumerator: IIterator<V>;

      function ToArray(AGrowAmount: Integer = 16; AShrinkRetain: Integer = 8): TArray<V>;

    end;

  private
    function GetValue(AKey: K): V;
    procedure SetValue(AKey: K; const Value: V);

    function GetActualKeyE(AKey: K): K;
    procedure SetActualKeyE(AKey: K; const Value: K);
    function GetPair(AKey: K): TPair;

  protected
    function GetBuckets: Integer; override;
    procedure SetBuckets(const Value: Integer); override;

    function CreateBucket: THashBase.TBucket; override;

    procedure SetBucketsDirect(Value: Integer); override;

    procedure CopyBuckets(AFrom: THashBase); override;

  public
    function Get(AKey: K; out AValue: V): Boolean; overload;
    property Value[AKey: K]: V read GetValue write SetValue; default;
    function Get(AKey: K; out APair: TPair): Boolean; overload;
    property Pair[AKey: K]: TPair read GetPair;

    function TryRemove(AKey: K): Boolean;
    procedure Remove(AKey: K);

    function GetActualKey(AKey: K; out AActualKey: K): Boolean;
    function SetActualKey(AKey, AActualKey: K): Boolean;
    property ActualKeys[AKey: K]: K read GetActualKeyE write SetActualKeyE;

    function KeyExists(AKey: K): Boolean;

    function GetEnumerator: IIterator<TPair>;
    function Keys: TKeysWrapper; inline;
    function Values: TValuesWrapper; inline;

    function KeySet(AHashMode: THashMode = hmAuto): TSet<K, H>;

    function Copy(AHashMode: THashMode = hmAuto): TMap<K, V, H>; reintroduce; inline;

    function Reader: TReader; reintroduce; inline;

    function Equals(Obj: TObject): Boolean; overload; override;
    function Equals(AOther: TMap<K, V, H>): Boolean; reintroduce; overload;

  end;

  // TODO: XmlDoc
  TRefSet<T: class; H: THasher<T>> = class(TSet<T, H>)
  private
    FOwnsObjects: Boolean;
    FStoredOwnsObjects: Boolean;

  protected
    function CreateBucket: THashBase.TBucket; override;

    procedure UnownObjects; override;
    procedure ReownObjects; override;

  public
    constructor Create(AHashMode: THashMode = hmAuto); overload; override;
    constructor Create(AOwnsObjects: Boolean; AHashMode: THashMode = hmAuto); reintroduce; overload;

    function Copy(AHashMode: THashMode = hmAuto): TRefSet<T, H>; reintroduce; inline;

    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;

  end;

  TRefSet<T: class> = class(TRefSet<T, TRefHasher<T>>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TRefSet<T>; reintroduce; inline;
  end;

  TObjectSet<T: class; H: THasher<T>> = class(TRefSet<T, H>)
  public
    constructor Create(AHashMode: THashMode = hmAuto); override;
  end;

  TObjectSet<T: class> = class(TObjectSet<T, TRefHasher<T>>);

  // TODO: XmlDoc
  TRefMap<K: class; V; H: THasher<K>> = class(TMap<K, V, H>)
  private
    FOwnsKeys: Boolean;
    FStoredOwnsKeys: Boolean;

  protected
    function CreateBucket: THashBase.TBucket; override;

    procedure UnownObjects; override;
    procedure ReownObjects; override;

  public
    constructor Create(AHashMode: THashMode = hmAuto); overload; override;
    constructor Create(AOwnsKeys: Boolean; AHashMode: THashMode = hmAuto); reintroduce; overload;

    function Copy(AHashMode: THashMode = hmAuto): TRefMap<K, V, H>; reintroduce; inline;

    property OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;

  end;

  TRefMap<K: class; V> = class(TRefMap<K, V, TRefHasher<K>>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TRefMap<K, V>; reintroduce; inline;
  end;

  TObjectMap<K: class; V; H: THasher<K>> = class(TRefMap<K, V, H>)
  public
    constructor Create(AHashMode: THashMode = hmAuto); override;
  end;

  TObjectMap<K: class; V> = class(TObjectMap<K, V, TRefHasher<K>>);

  // TODO: XmlDoc
  TRefRefMap<K, V: class; H: THasher<K>> = class(TRefMap<K, V, H>)
  private
    FOwnsValues: Boolean;
    FStoredOwnsValues: Boolean;

  protected
    function CreateBucket: THashBase.TBucket; override;

    procedure UnownObjects; override;
    procedure ReownObjects; override;

  public
    constructor Create(AHashMode: THashMode = hmAuto); overload; override;
    constructor Create(AOwnsObjects: Boolean; AHashMode: THashMode = hmAuto); reintroduce; overload;
    constructor Create(AOwnsKeys, AOwnsValues: Boolean; AHashMode: THashMode = hmAuto); reintroduce; overload;

    function Copy(AHashMode: THashMode = hmAuto): TRefRefMap<K, V, H>; reintroduce; inline;

    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;
            
    function Equals(Obj: TObject): Boolean; overload; override;
    function Equals(AOther: TRefRefMap<K, V, H>): Boolean; reintroduce; overload;

  end;

  TRefRefMap<K, V: class> = class(TRefRefMap<K, V, TRefHasher<K>>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TRefRefMap<K, V>; reintroduce; inline;
  end;

  TObjectRefMap<K, V: class; H: THasher<K>> = class(TRefRefMap<K, V, H>)
  protected
    function CreateCopy(AHashMode: THashMode): THashBase; override;
  public
    constructor Create(AHashMode: THashMode = hmAuto); overload; override;
    constructor Create(AOwnsValues: Boolean; AHashMode: THashMode = hmAuto); reintroduce; overload;
  end;

  TObjectRefMap<K, V: class> = class(TObjectRefMap<K, V, TRefHasher<K>>);

  TObjectObjectMap<K, V: class; H: THasher<K>> = class(TRefRefMap<K, V, H>)
  protected
    function CreateCopy(AHashMode: THashMode): THashBase; override;
  public
    constructor Create(AHashMode: THashMode = hmAuto); override;
  end;

  TObjectObjectMap<K, V: class> = class(TObjectObjectMap<K, V, TRefHasher<K>>);

  TRefObjectMap<K, V: class; H: THasher<K>> = class(TRefRefMap<K, V, H>)
  protected
    function CreateCopy(AHashMode: THashMode): THashBase; override;
  public
    constructor Create(AHashMode: THashMode = hmAuto); overload; override;
    constructor Create(AOwnsKeys: Boolean; AHashMode: THashMode = hmAuto); reintroduce; overload;
  end;

  TRefObjectMap<K, V: class> = class(TRefObjectMap<K, V, TRefHasher<K>>);

  // TODO: XmlDoc
  TToRefMap<K; V: class; H: THasher<K>> = class(TMap<K, V, H>)
  private
    FOwnsValues: Boolean;
    FStoredOwnsValues: Boolean;

  protected
    function CreateBucket: THashBase.TBucket; override;

    procedure UnownObjects; override;
    procedure ReownObjects; override;

  public
    constructor Create(AHashMode: THashMode = hmAuto); overload; override;
    constructor Create(AOwnsValues: Boolean; AHashMode: THashMode = hmAuto); reintroduce; overload;

    function Copy(AHashMode: THashMode = hmAuto): TToRefMap<K, V, H>; reintroduce; inline;

    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;
           
    function Equals(Obj: TObject): Boolean; overload; override;
    function Equals(AOther: TToRefMap<K, V, H>): Boolean; reintroduce; overload;

  end;

  TToObjectMap<K; V: class; H: THasher<K>> = class(TToRefMap<K, V, H>)
  protected
    function CreateCopy(AHashMode: THashMode): THashBase; override;
  public
    constructor Create(AHashMode: THashMode = hmAuto); override;
  end;

  TClassSet = TSet<TClass, TClassHasher>;

  TClassMap<T> = class(TMap<TClass, T, TClassHasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TClassMap<T>; reintroduce; inline;
  end;

  TClassRefMap<T: class> = class(TToRefMap<TClass, T, TClassHasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TClassRefMap<T>; reintroduce; inline;
  end;

  TClassObjectMap<T: class> = class(TToObjectMap<TClass, T, TClassHasher>);

implementation

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

{ THashBase.TIterator<T> }

constructor THashBase.TIterator<T>.Create(AHashBase: THashBase);
begin
  FHashBase := AHashBase;
  FIndex := 0;
  FBucketIndex := -1;
end;

function THashBase.TIterator<T>.GetBucketSize(AIndex: Integer): Integer;
begin
  if HashBase.FBuckets[AIndex] = nil then
    Exit(0);
  Result := HashBase.FBuckets[AIndex].Count;
end;

function THashBase.TIterator<T>.MoveNext: Boolean;
begin
  if HashBase.Buckets = 0 then
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
    if FIndex = HashBase.Buckets - 1 then
      Exit(False);
    Inc(FIndex);
  until GetBucketSize(FIndex) <> 0;

  Result := True;
end;

{ THashBase }

procedure THashBase.SetHashMode(const Value: THashMode);
begin
  if FHashMode = Value then
    Exit;
  FHashMode := Value;
  Buckets := Buckets;
end;

procedure THashBase.UnownObjects;
begin
  // nothing to unown by default
end;

procedure THashBase.Clear;
begin
  ClearBuckets;
  FCount := 0;
  SetBucketsDirect(0);
end;

function THashBase.Count: Integer;
begin
  Result := FCount;
end;

function THashBase.CountOptimized: Boolean;
begin
  Result := True;
end;

constructor THashBase.Create(AHashMode: THashMode);
begin
  FHashMode := AHashMode;
end;

function THashBase.CreateCopy(AHashMode: THashMode): THashBase;
begin
  Result := CreateSame(AHashMode);
  Result.Assign(Self);
end;

function THashBase.CreateSame(AHashMode: THashMode): THashBase;
begin
  Result := THashBaseClass(ClassType).Create(AHashMode);
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

function THashBase.Reader: TReader;
begin
  Result := TReader(Self);
end;

procedure THashBase.Rehash(ABuckets: Integer);
var
  Tmp: THashBase;
begin
  if Count = 0 then
  begin
    SetBucketsDirect(ABuckets);
    Exit;
  end;
  Tmp := CreateSame(hmManual);
  Tmp.FBuckets := FBuckets;
  Tmp.FCount := Count;
  FBuckets := nil;
  FCount := 0;
  SetBucketsDirect(ABuckets);
  if HashMode = hmAuto then
  begin
    FHashMode := hmManual;
    CopyBuckets(Tmp);
    FHashMode := hmAuto; // use FHashMode, it doesn't test for rehashing
  end
  else
    CopyBuckets(Tmp);
  UnownObjects; // also linked to references in Tmp
  Tmp.Free;
  ReownObjects;
end;

procedure THashBase.ReownObjects;
begin
  // nothing to reown by default
end;

procedure THashBase.ClearBuckets;
var
  I: Integer;
begin
  if not Empty then
    for I := 0 to Length(FBuckets) - 1 do
      FreeAndNil(FBuckets[I]);
end;

function THashBase.Copy(AHashMode: THashMode): THashBase;
begin
  Result := CreateCopy(AHashMode);
end;

procedure THashBase.Assign(AHashBase: THashBase);
var
  I: Integer;
begin
  Clear;
  SetBucketsDirect(Buckets);
  for I := 0 to Buckets - 1 do
    if FBuckets[I] <> nil then
      FBuckets[I] := AHashBase.FBuckets[I].Copy;
end;

function THashBase.BucketCounts: TIntArray;
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

{ THashBase<K, H> }

class function THashBase<K, H>.KeysEqual(const AKey1, AKey2: K): Boolean;
begin
  Result := H.Equal(AKey1, AKey2);
end;

function THashBase<K, H>.Reader: TReader;
begin
  Result := TReader(Self);
end;

class function THashBase<K, H>.GetHash(const AKey: K): Cardinal;
begin
  Result := H.GetHash(AKey);
end;

class function THashBase<K, H>.CanIndex(const AKey: K): Boolean;
begin
  Result := H.CanIndex(AKey);
end;

function THashBase<K, H>.GetCappedHash(AKey: K): Integer;
begin
  if CanIndex(AKey) then
    Exit(Integer(GetHash(AKey) mod Cardinal(Buckets)));
  raise EKeyNotHashable.Create;
end;

function THashBase<K, H>.Copy(AHashMode: THashMode): THashBase<K, H>;
begin
  Result := THashBase<K, H>(CreateCopy(AHashMode));
end;

{ TSet<T, H>.TIterator }

constructor TSet<T, H>.TIterator.Create(ASet: TSet<T, H>);
begin
  inherited Create(ASet);
end;

function TSet<T, H>.TIterator.GetCurrent: T;
begin
  Result := TBucket(HashBase.FBuckets[Index])[BucketIndex];
end;

{ TSet<T, H> }

procedure TSet<T, H>.Add(AElement: T);
begin
  if not TryAdd(AElement) then
    raise ESetKeyExistsAlready.Create;
end;

procedure TSet<T, H>.Add(ASet: TSet<T, H>);
var
  Element: T;
begin
  for Element in ASet do
    TryAdd(Element);
end;

function TSet<T, H>.Copy(AHashMode: THashMode): TSet<T, H>;
begin
  Result := TSet<T, H>(CreateCopy(AHashMode));
end;

procedure TSet<T, H>.CopyBuckets(AFrom: THashBase);
var
  Key: T;
begin
  for Key in TSet<T, H>(AFrom) do
    TryAdd(Key);
end;

function TSet<T, H>.CreateBucket: THashBase.TBucket;
begin
  Result := TBucket.Create(4, 2);
end;

procedure TSet<T, H>.Remove(AElement: T);
begin
  if not TryRemove(AElement) then
    raise ESetKeyNotFound.Create;
end;

procedure TSet<T, H>.Remove(ASet: TSet<T, H>);
var
  Element: T;
begin
  for Element in ASet do
    TryRemove(Element);
end;

function TSet<T, H>.Equals(Obj: TObject): Boolean;
begin
  if Obj.ClassType <> ClassType then
    Exit(False);
  Result := Equals(TSet<T, H>(Obj));
end;

function TSet<T, H>.Equals(ASet: TSet<T, H>): Boolean;
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

function TSet<T, H>.GetBuckets: Integer;
begin
  Result := Length(FBuckets);
end;

function TSet<T, H>.GetEnumerator: IIterator<T>;
begin
  Result := TIterator.Create(Self);
end;

function TSet<T, H>.Intersection(ASet: TSet<T, H>): TSet<T, H>;
var
  Element: T;
begin
  Result := TSet<T, H>(CreateSame(hmManual));
  Result.Buckets := (Buckets + ASet.Buckets) div 2;
  for Element in Self do
    if ASet[Element] then
      Result.TryAdd(Element);
end;

function TSet<T, H>.IsSubsetOf(ASet: TSet<T, H>): Boolean;
var
  Element: T;
begin
  for Element in Self do
    if not ASet[Element] then
      Exit(False);
  Result := True;
end;

function TSet<T, H>.IsSupersetOf(ASet: TSet<T, H>): Boolean;
var
  Element: T;
begin
  for Element in ASet do
    if not Self[Element] then
      Exit(False);
  Result := True;
end;

function TSet<T, H>.GetElement(AElement: T): Boolean;
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

function TSet<T, H>.Reader: TReader;
begin
  Result := TReader(Self);
end;

function TSet<T, H>.Without(ASet: TSet<T, H>): TSet<T, H>;
var
  Element: T;
begin
  Result := TSet<T, H>(CreateSame(hmManual));
  for Element in Self do
    if not ASet[Element] then
      TryAdd(Element);
end;

procedure TSet<T, H>.SetBuckets(const Value: Integer);
var
  NewBuckets: Integer;
begin
  if Value = 0 then
  begin
    if Count > 0 then
      raise EHashBucketRequired.Create
    else
      SetLength(FBuckets, 0);
    Exit;
  end;

  NewBuckets := GetHashBuckets(Value);
  if NewBuckets > Buckets then
    Rehash(NewBuckets)
  else if NewBuckets < Buckets div 2 then // only shrink if bucket count halved
    Rehash(NewBuckets);
end;

procedure TSet<T, H>.SetBucketsDirect(Value: Integer);
begin
  SetLength(FBuckets, Value);
end;

procedure TSet<T, H>.SetElement(AElement: T; const Value: Boolean);
begin
  if Value then
    TryAdd(AElement)
  else
    TryRemove(AElement);
end;

function TSet<T, H>.TryAdd(AElement: T): Boolean;
var
  H, I: Integer;
begin
  if Buckets = 0 then
  begin
    if HashMode = hmAuto then
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
    if HashMode = hmAuto then
      Buckets := Count;
    Exit(True);
  end;

  for I := 0 to FBuckets[H].MaxIndex do
    if KeysEqual(TBucket(FBuckets[H])[I], AElement) then
      Exit(False);
  TBucket(FBuckets[H]).Add(AElement);
  Inc(FCount);
  if HashMode = hmAuto then
    Buckets := Count;
  Result := True;
end;

function TSet<T, H>.TryRemove(AElement: T): Boolean;
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
  FBuckets[H].RemoveAt(I);
  Dec(FCount);
  if FBuckets[H].Count = 0 then
    FreeAndNil(FBuckets[H]);
  if HashMode = hmAuto then
    Buckets := Count;
  Result := True;
end;

function TSet<T, H>.Union(ASet: TSet<T, H>): TSet<T, H>;
var
  Element: T;
begin
  Result := TSet<T, H>(CreateSame(hmManual));
  Result.Buckets := (Buckets + ASet.Buckets) div 2;
  Result.Add(Self);
  Result.Add(ASet);
  Result.HashMode := hmAuto;
end;

{ TMap<K, V, H>.TIterator }

function TMap<K, V, H>.TIterator.GetCurrent: TPair;
begin
  Result := TBucket(HashBase.FBuckets[Index])[BucketIndex];
end;

{ TMap<K, V, H>.TKeyIterator }

function TMap<K, V, H>.TKeyIterator.GetCurrent: K;
begin
  Result := TBucket(HashBase.FBuckets[Index])[BucketIndex].Key;
end;

{ TMap<K, V, H>.TValueIterator }

function TMap<K, V, H>.TValueIterator.GetCurrent: V;
begin
  Result := TBucket(HashBase.FBuckets[Index])[BucketIndex].Value;
end;

{ TMap<K, V, H> }

function TMap<K, V, H>.Copy(AHashMode: THashMode): TMap<K, V, H>;
begin
  Result := TMap<K, V, H>(CreateCopy(AHashMode));
end;

procedure TMap<K, V, H>.CopyBuckets(AFrom: THashBase);
var
  Pair: TPair;
begin
  for Pair in TMap<K, V, H>(AFrom) do
    Self[Pair.Key] := Pair.Value;
end;

function TMap<K, V, H>.CreateBucket: THashBase.TBucket;
begin
  Result := TBucket.Create(4, 2);
end;

procedure TMap<K, V, H>.Remove(AKey: K);
begin
  if not TryRemove(AKey) then
    raise EMapKeyNotFound.Create;
end;

function TMap<K, V, H>.Equals(Obj: TObject): Boolean;
begin
  if Obj.ClassType <> ClassType then
    Exit(False);
  Result := Equals(TMap<K, V, H>(Obj));
end;

function TMap<K, V, H>.Equals(AOther: TMap<K, V, H>): Boolean;
var
  Pair: TPair<K, V>;
  Value: V;
begin
  if Count <> AOther.Count then
    Exit(False);

  for Pair in Self do
  begin
    if not AOther.Get(Pair.Key, Value) then
      Exit(False);
    if CompareMem(@Value, @Pair.Value, SizeOf(V)) then
      Exit(False);
  end;
  Result := True;
end;

function TMap<K, V, H>.Get(AKey: K; out AValue: V): Boolean;
var
  H: Integer;
  Pair: TPair;
begin
  if Empty or not CanIndex(AKey) then
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

function TMap<K, V, H>.Get(AKey: K; out APair: TPair): Boolean;
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
      APair := Pair;
      Exit(True);
    end;
  end;
  Result := False;
end;

function TMap<K, V, H>.GetActualKey(AKey: K; out AActualKey: K): Boolean;
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
      AActualKey := Pair.Key;
      Exit(True);
    end;
  end;
  Result := False;
end;

function TMap<K, V, H>.GetActualKeyE(AKey: K): K;
begin
  if not GetActualKey(AKey, Result) then
    raise EMapKeyNotFound.Create;
end;

function TMap<K, V, H>.GetBuckets: Integer;
begin
  Result := Length(FBuckets);
end;

function TMap<K, V, H>.SetActualKey(AKey, AActualKey: K): Boolean;
var
  H, I: Integer;
begin
  if Empty then

    H := GetCappedHash(AKey);
  if FBuckets[H] = nil then
    Exit(False);
  for I := 0 to FBuckets[H].MaxIndex do
  begin
    if KeysEqual(TBucket(FBuckets[H])[I].Key, AKey) then
    begin
      TBucket(FBuckets[H])[I] := TPair.Create(AActualKey, TBucket(FBuckets[H])[I].Value);
      Exit(True);
    end;
  end;

  Result := False;
end;

procedure TMap<K, V, H>.SetActualKeyE(AKey: K; const Value: K);
begin
  if not SetActualKey(AKey, Value) then
    raise EMapKeyNotFound.Create;
end;

procedure TMap<K, V, H>.SetBuckets(const Value: Integer);
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

procedure TMap<K, V, H>.SetBucketsDirect(Value: Integer);
begin
  SetLength(FBuckets, Value);
end;

procedure TMap<K, V, H>.SetValue(AKey: K; const Value: V);
var
  H, I: Integer;
  Pair: TPair;
begin
  if Buckets = 0 then
  begin
    if HashMode = hmAuto then
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
    if HashMode = hmAuto then
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
  if HashMode = hmAuto then
    Buckets := Count;
end;

function TMap<K, V, H>.TryRemove(AKey: K): Boolean;
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
  FBuckets[H].RemoveAt(I);
  Dec(FCount);
  if FBuckets[H].Count = 0 then
    FreeAndNil(FBuckets[H]);
  if HashMode = hmAuto then
    Buckets := Count;
  Result := True;
end;

function TMap<K, V, H>.Values: TValuesWrapper;
begin
  Result.Create(Self);
end;

function TMap<K, V, H>.GetEnumerator: IIterator<TPair>;
begin
  Result := TIterator.Create(Self);
end;

function TMap<K, V, H>.GetValue(AKey: K): V;
begin
  if not Get(AKey, Result) then
    raise EMapKeyNotFound.Create;
end;

function TMap<K, V, H>.GetPair(AKey: K): TPair;
begin
  if not Get(AKey, Result) then
    raise EMapKeyNotFound.Create;
end;

function TMap<K, V, H>.KeyExists(AKey: K): Boolean;
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
    if KeysEqual(Pair.Key, AKey) then
      Exit(True);

  Result := False;
end;

function TMap<K, V, H>.Keys: TKeysWrapper;
begin
  Result.Create(Self);
end;

function TMap<K, V, H>.KeySet(AHashMode: THashMode): TSet<K, H>;
begin
  Result := Keys.ToSet(AHashMode);
end;

function TMap<K, V, H>.Reader: TReader;
begin
  Result := TReader(Self);
end;

{ TRefSet<T, H> }

function TRefSet<T, H>.CreateBucket: THashBase.TBucket;
begin
  Result := TRefArrayOwnLinked<T>.Create(@FOwnsObjects, 4, 2);
end;

procedure TRefSet<T, H>.UnownObjects;
begin
  FStoredOwnsObjects := OwnsObjects;
  OwnsObjects := False;
end;

procedure TRefSet<T, H>.ReownObjects;
begin
  OwnsObjects := FStoredOwnsObjects;
end;

constructor TRefSet<T, H>.Create(AHashMode: THashMode);
begin
  inherited;
end;

constructor TRefSet<T, H>.Create(AOwnsObjects: Boolean; AHashMode: THashMode);
begin
  inherited Create(AHashMode);
  OwnsObjects := AOwnsObjects;
end;

function TRefSet<T, H>.Copy(AHashMode: THashMode): TRefSet<T, H>;
begin
  Result := TRefSet<T, H>(CreateCopy(AHashMode));
end;

{ TRefMap<K, V, H> }

function TRefMap<K, V, H>.CreateBucket: THashBase.TBucket;
begin
  Result := TRefPairArrayOwnLinked<K, V>.Create(@FOwnsKeys, 4, 2);
end;

procedure TRefMap<K, V, H>.UnownObjects;
begin
  FStoredOwnsKeys := OwnsKeys;
  OwnsKeys := False;
end;

procedure TRefMap<K, V, H>.ReownObjects;
begin
  OwnsKeys := FStoredOwnsKeys;
end;

constructor TRefMap<K, V, H>.Create(AHashMode: THashMode);
begin
  inherited;
end;

constructor TRefMap<K, V, H>.Create(AOwnsKeys: Boolean; AHashMode: THashMode);
begin
  inherited Create(AHashMode);
  OwnsKeys := AOwnsKeys;
end;

function TRefMap<K, V, H>.Copy(AHashMode: THashMode): TRefMap<K, V, H>;
begin
  Result := TRefMap<K, V, H>(CreateCopy(AHashMode));
end;

{ TRefRefMap<K, V, H> }

function TRefRefMap<K, V, H>.CreateBucket: THashBase.TBucket;
begin
  Result := TRefRefPairArrayOwnLinked<K, V>.Create(@FOwnsKeys, @FOwnsValues, 4, 2);
end;

function TRefRefMap<K, V, H>.Equals(Obj: TObject): Boolean;
begin
  if Obj.ClassType <> ClassType then
    Exit(False);
  Result := Equals(TRefRefMap<K, V, H>(Obj));
end;

function TRefRefMap<K, V, H>.Equals(AOther: TRefRefMap<K, V, H>): Boolean;
var
  Pair: TPair<K, V>;
  Value: V;
begin
  if Count <> AOther.Count then
    Exit(False);

  for Pair in Self do
  begin
    if not AOther.Get(Pair.Key, Value) then
      Exit(False);
    if Pair.Value.Equals(Value) then
      Exit(False);
  end;
  Result := True;   
end;

procedure TRefRefMap<K, V, H>.UnownObjects;
begin
  inherited;
  FStoredOwnsValues := OwnsValues;
  OwnsValues := False;
end;

procedure TRefRefMap<K, V, H>.ReownObjects;
begin
  inherited;
  OwnsValues := FStoredOwnsValues;
end;

constructor TRefRefMap<K, V, H>.Create(AHashMode: THashMode);
begin
  inherited;
end;

constructor TRefRefMap<K, V, H>.Create(AOwnsObjects: Boolean; AHashMode: THashMode);
begin
  inherited Create(AOwnsObjects, AHashMode);
  OwnsValues := AOwnsObjects;
end;

constructor TRefRefMap<K, V, H>.Create(AOwnsKeys, AOwnsValues: Boolean; AHashMode: THashMode);
begin
  inherited Create(AOwnsKeys, AHashMode);
  OwnsValues := AOwnsValues;
end;

function TRefRefMap<K, V, H>.Copy(AHashMode: THashMode): TRefRefMap<K, V, H>;
begin
  Result := TRefRefMap<K, V, H>(CreateCopy(AHashMode));
end;

{ TToRefMap<K, V, H> }

function TToRefMap<K, V, H>.CreateBucket: THashBase.TBucket;
begin
  Result := TToRefPairArrayOwnLinked<K, V>.Create(@FOwnsValues, 4, 2);
end;

function TToRefMap<K, V, H>.Equals(Obj: TObject): Boolean;
begin
  if Obj.ClassType <> ClassType then
    Exit(False);
  Result := Equals(TToRefMap<K, V, H>(Obj));  
end;

function TToRefMap<K, V, H>.Equals(AOther: TToRefMap<K, V, H>): Boolean;
var
  Pair: TPair<K, V>;
  Value: V;
begin
  if Count <> AOther.Count then
    Exit(False);

  for Pair in Self do
  begin
    if not AOther.Get(Pair.Key, Value) then
      Exit(False);
    if Pair.Value.Equals(Value) then
      Exit(False);
  end;
  Result := True;       
end;

procedure TToRefMap<K, V, H>.UnownObjects;
begin
  FStoredOwnsValues := OwnsValues;
  OwnsValues := False;
end;

procedure TToRefMap<K, V, H>.ReownObjects;
begin
  OwnsValues := FStoredOwnsValues;
end;

constructor TToRefMap<K, V, H>.Create(AHashMode: THashMode);
begin
  inherited;
end;

constructor TToRefMap<K, V, H>.Create(AOwnsValues: Boolean; AHashMode: THashMode);
begin
  inherited Create(AHashMode);
  OwnsValues := AOwnsValues;
end;

function TToRefMap<K, V, H>.Copy(AHashMode: THashMode): TToRefMap<K, V, H>;
begin
  Result := TToRefMap<K, V, H>(CreateCopy(AHashMode));
end;

{ TToObjectMap<K, V, H> }

constructor TToObjectMap<K, V, H>.Create(AHashMode: THashMode);
begin
  inherited Create(True, AHashMode);
end;

function TToObjectMap<K, V, H>.CreateCopy(AHashMode: THashMode): THashBase;
begin
  Result := TToRefMap<K, V, H>.Create(AHashMode);
  Result.Assign(Self);
end;

{ TClassMap<T> }

function TClassMap<T>.Copy(AHashMode: THashMode): TClassMap<T>;
begin
  Result := TClassMap<T>(CreateCopy(AHashMode));
end;

{ THashBase.TReader }

function THashBase.TReader.GetBuckets: Integer;
begin
  Result := THashBase(Self).Buckets;
end;

function THashBase.TReader.GetHashMode: THashMode;
begin
  Result := THashBase(Self).HashMode;
end;

function THashBase.TReader.Count: Integer;
begin
  Result := THashBase(Self).Count;
end;

function THashBase.TReader.CountOptimized: Boolean;
begin
  Result := THashBase(Self).CountOptimized;
end;

function THashBase.TReader.Empty: Boolean;
begin
  Result := THashBase(Self).Empty;
end;

function THashBase.TReader.BucketCounts: TIntArray;
begin
  Result := THashBase(Self).BucketCounts;
end;

function THashBase.TReader.Copy(AHashMode: THashMode): THashBase;
begin
  Result := THashBase(Self).Copy(AHashMode);
end;

{ THashBase<K, H>.TReader }

function THashBase<K, H>.TReader.GetCappedHash(AKey: K): Integer;
begin
  Result := THashBase<K, H>(Self).GetCappedHash(AKey);
end;


{ TSet<T, H>.TReader }

function TSet<T, H>.TReader.GetElement(AElement: T): Boolean;
begin
  Result := TSet<T, H>(Self)[AElement];
end;

function TSet<T, H>.TReader.GetEnumerator: IIterator<T>;
begin
  Result := TSet<T, H>(Self).GetEnumerator;
end;
function TSet<T, H>.TReader.Copy(AHashMode: THashMode): TSet<T, H>;
begin
  Result := TSet<T, H>(Self).Copy(AHashMode);
end;

function TSet<T, H>.TReader.Union(ASet: TSet<T, H>): TSet<T, H>;
begin
  Result := TSet<T, H>(Self).Union(ASet);
end;

function TSet<T, H>.TReader.Intersection(ASet: TSet<T, H>): TSet<T, H>;
begin
  Result := TSet<T, H>(Self).Intersection(ASet);
end;

function TSet<T, H>.TReader.Without(ASet: TSet<T, H>): TSet<T, H>;
begin
  Result := TSet<T, H>(Self).Without(ASet);
end;

function TSet<T, H>.TReader.IsSupersetOf(ASet: TSet<T, H>): Boolean;
begin
  Result := TSet<T, H>(Self).IsSupersetOf(ASet);
end;

function TSet<T, H>.TReader.IsSubsetOf(ASet: TSet<T, H>): Boolean;
begin
  Result := TSet<T, H>(Self).IsSubsetOf(ASet);
end;

function TSet<T, H>.TReader.Equals(ASet: TSet<T, H>): Boolean;
begin
  Result := TSet<T, H>(Self).Equals(ASet);
end;

{ TMap<K, V, H>.TReader }

function TMap<K, V, H>.TReader.GetActualKeyE(AKey: K): K;
begin
  Result := TMap<K, V, H>(Self).ActualKeys[AKey];
end;

function TMap<K, V, H>.TReader.GetPair(AKey: K): TPair;
begin
  Result := TMap<K, V, H>(Self).Pair[AKey];
end;

function TMap<K, V, H>.TReader.GetValue(AKey: K): V;
begin
  Result := TMap<K, V, H>(Self)[AKey];
end;

function TMap<K, V, H>.TReader.Get(AKey: K; out AValue: V): Boolean;
begin
  Result := TMap<K, V, H>(Self).Get(AKey, AValue);
end;

function TMap<K, V, H>.TReader.Get(AKey: K; out APair: TPair): Boolean;
begin
  Result := TMap<K, V, H>(Self).Get(AKey, APair);
end;

function TMap<K, V, H>.TReader.GetActualKey(AKey: K; out AActualKey: K): Boolean;
begin
  Result := TMap<K, V, H>(Self).GetActualKey(AKey, AActualKey);
end;

function TMap<K, V, H>.TReader.KeyExists(AKey: K): Boolean;
begin
  Result := TMap<K, V, H>(Self).KeyExists(AKey);
end;

function TMap<K, V, H>.TReader.GetEnumerator: IIterator<TPair>;
begin
  Result := TMap<K, V, H>(Self).GetEnumerator;
end;

function TMap<K, V, H>.TReader.KeySet(AHashMode: THashMode): TSet<K, H>;
begin
  Result := TMap<K, V, H>(Self).KeySet(AHashMode);
end;

function TMap<K, V, H>.TReader.Copy(AHashMode: THashMode): TMap<K, V, H>;
begin
  Result := TMap<K, V, H>(Self).Copy(AHashMode);
end;

{ TMap<K, V, H>.TKeysWrapper }

constructor TMap<K, V, H>.TKeysWrapper.Create(AMap: TMap<K, V, H>);
begin
  FMap := AMap;
end;

function TMap<K, V, H>.TKeysWrapper.GetEnumerator: IIterator<K>;
begin
  Result := TKeyIterator.Create(FMap);
end;

function TMap<K, V, H>.TKeysWrapper.ToArray(AGrowAmount, AShrinkRetain: Integer): TArray<K>;
var
  Key: K;
begin
  Result := TArray<K>.Create(AGrowAmount, AShrinkRetain);
  Result.Capacity := FMap.Count;
  for Key in Self do
    Result.Add(Key);
end;

function TMap<K, V, H>.TKeysWrapper.ToSet(AHashMode: THashMode): TSet<K, H>;
var
  Key: K;
begin
  Result := TSet<K, H>.Create(hmManual);
  Result.Buckets := FMap.Count;
  for Key in Self do
    Result.Add(Key);
  Result.HashMode := AHashMode;  
end;

{ TMap<K, V, H>.TValuesWrapper }

constructor TMap<K, V, H>.TValuesWrapper.Create(AMap: TMap<K, V, H>);
begin
  FMap := AMap;
end;

function TMap<K, V, H>.TValuesWrapper.GetEnumerator: IIterator<V>;
begin
  Result := TValueIterator.Create(FMap);
end;

function TMap<K, V, H>.TValuesWrapper.ToArray(AGrowAmount, AShrinkRetain: Integer): TArray<V>;
var
  Value: V;
begin
  Result := TArray<V>.Create(AGrowAmount, AShrinkRetain);
  Result.Capacity := FMap.Count;
  for Value in Self do
    Result.Add(Value);  
end;

{ TClassRefMap<T> }

function TClassRefMap<T>.Copy(AHashMode: THashMode): TClassRefMap<T>;
begin
  Result := TClassRefMap<T>(CreateCopy(AHashMode));
end;

{ TRefSet<T> }

function TRefSet<T>.Copy(AHashMode: THashMode): TRefSet<T>;
begin
  Result := TRefSet<T>(CreateCopy(AHashMode));
end;

{ TObjectSet<T, H> }

constructor TObjectSet<T, H>.Create(AHashMode: THashMode);
begin
  inherited Create(True, AHashMode);
end;

{ TRefMap<K, V> }

function TRefMap<K, V>.Copy(AHashMode: THashMode): TRefMap<K, V>;
begin
  Result := TRefMap<K, V>(CreateCopy(AHashMode));
end;

{ TObjectMap<K, V, H> }

constructor TObjectMap<K, V, H>.Create(AHashMode: THashMode);
begin
  inherited Create(True, AHashMode);
end;

{ TRefRefMap<K, V> }

function TRefRefMap<K, V>.Copy(AHashMode: THashMode): TRefRefMap<K, V>;
begin
  Result := TRefRefMap<K, V>(CreateCopy(AHashMode));
end;

{ TObjectRefMap<K, V, H> }

constructor TObjectRefMap<K, V, H>.Create(AHashMode: THashMode);
begin
  inherited Create(True, False, AHashMode);
end;

constructor TObjectRefMap<K, V, H>.Create(AOwnsValues: Boolean; AHashMode: THashMode);
begin
  inherited Create(True, AOwnsValues, AHashMode);
end;

function TObjectRefMap<K, V, H>.CreateCopy(AHashMode: THashMode): THashBase;
begin
  Result := TRefRefMap<K, V, H>.Create(AHashMode);
  Result.Assign(Self);
end;

{ TObjectObjectMap<K, V, H> }

constructor TObjectObjectMap<K, V, H>.Create(AHashMode: THashMode);
begin
  inherited Create(True, AHashMode);
end;

function TObjectObjectMap<K, V, H>.CreateCopy(AHashMode: THashMode): THashBase;
begin
  Result := TRefRefMap<K, V>.Create(AHashMode);
  Result.Assign(Self);
end;

{ TRefObjectMap<K, V, H> }

constructor TRefObjectMap<K, V, H>.Create(AHashMode: THashMode);
begin
  inherited Create(False, True, AHashMode);
end;

constructor TRefObjectMap<K, V, H>.Create(AOwnsKeys: Boolean; AHashMode: THashMode);
begin
  inherited Create(AOwnsKeys, True, AHashMode);
end;

function TRefObjectMap<K, V, H>.CreateCopy(AHashMode: THashMode): THashBase;
begin
  Result := TRefRefMap<K, V, H>.Create(AHashMode);
  Result.Assign(Self);
end;

{ EKeyNotHashable }

constructor EKeyNotHashable.Create;
begin
  inherited Create('The given key is not hashable.');
end;

end.
