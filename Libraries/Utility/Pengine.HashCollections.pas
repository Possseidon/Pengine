unit Pengine.HashCollections;

interface

uses
  System.SysUtils,

  Pengine.CollectionInterfaces,
  Pengine.Interfaces,
  Pengine.Collections,
  Pengine.ValueHasher;

type

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

    procedure CopyTo(AHashBase: THashBase);

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

    function Copy(AHashMode: THashMode = hmAuto): THashBase<K>; reintroduce; inline;

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

    function Copy(AHashMode: THashMode = hmAuto): TSet<T>; reintroduce; inline;

    function Union(ASet: TSet<T>): TSet<T>;
    function Intersection(ASet: TSet<T>): TSet<T>;
    function Remove(ASet: TSet<T>): TSet<T>;

    function IsSupersetOf(ASet: TSet<T>): Boolean;
    function IsSubsetOf(ASet: TSet<T>): Boolean;
    function Equals(ASet: TSet<T>): Boolean; reintroduce;

  end;

  // TODO: XmlDoc
  TValueSet<T; H: TValueHasher<T>> = class(TSet<T>)
  protected
    class function GetHash(AKey: T): Cardinal; override;
    class function KeysEqual(AKey1, AKey2: T): Boolean; override;
    class function CanIndex(AKey: T): Boolean; override;
  public
    function Copy(AHashMode: THashMode = hmAuto): TValueSet<T, H>; reintroduce; inline;
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
    property Values[AKey: K]: V read GetValue write SetValue; default;
    function Get(AKey: K; out APair: TPair): Boolean; overload;
    property Pairs[AKey: K]: TPair read GetPair;

    function TryDel(AKey: K): Boolean;
    procedure Del(AKey: K);

    function GetActualKey(AKey: K; out AActualKey: K): Boolean;
    function SetActualKey(AKey, AActualKey: K): Boolean;
    property ActualKeys[AKey: K]: K read GetActualKeyE write SetActualKeyE;

    function KeyExists(AKey: K): Boolean;

    function GetEnumerator: IIterator<TPair>;

    function BucketCounts: TIntArray;

    function KeySet(AHashMode: THashMode = hmManual): TSet<K>;

    function Copy(AHashMode: THashMode = hmAuto): TMap<K, V>; reintroduce; inline;

  end;

  // TODO: XmlDoc
  TValueMap<K, V; H: TValueHasher<K>> = class(TMap<K, V>)
  protected
    class function GetHash(AKey: K): Cardinal; override;
    class function KeysEqual(AKey1, AKey2: K): Boolean; override;
    class function CanIndex(AKey: K): Boolean; override;
  public
    function Copy(AHashMode: THashMode = hmAuto): TValueMap<K, V, H>; reintroduce; inline;
  end;

implementation

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
  CopyTo(Result);
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

function THashBase<K>.Copy(AHashMode: THashMode): THashBase<K>;
begin
  Result := THashBase<K>(CreateCopy(AHashMode));
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

function TSet<T>.Copy(AHashMode: THashMode): TSet<T>;
begin
  Result := TSet<T>(CreateCopy(AHashMode));
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
  Result := TSet<T>(CreateSame(hmManual));
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

function TSet<T>.Remove(ASet: TSet<T>): TSet<T>;
var
  Element: T;
begin
  Result := TSet<T>(CreateSame(hmManual));
  for Element in Self do
    if not ASet[Element] then
      TryAdd(Element);
end;

procedure TSet<T>.SetBuckets(const Value: Integer);
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
    Exit;
  end;

  for I := 0 to FBuckets[H].MaxIndex do
    if KeysEqual(TBucket(FBuckets[H])[I], AElement) then
      Exit(False);
  TBucket(FBuckets[H]).Add(AElement);
  Inc(FCount);
  if HashMode = hmAuto then
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
  if HashMode = hmAuto then
    Buckets := Count;
  Result := True;
end;

function TSet<T>.Union(ASet: TSet<T>): TSet<T>;
var
  Element: T;
begin
  Result := TSet<T>(CreateSame(hmManual));
  Result.Buckets := (Buckets + ASet.Buckets) div 2;
  Result.Add(Self);
  Result.Add(ASet);
  Result.HashMode := hmAuto;
end;

{ TValueSet<T, H> }

class function TValueSet<T, H>.CanIndex(AKey: T): Boolean;
begin
  Result := H.CanIndex(AKey);
end;

function TValueSet<T, H>.Copy(AHashMode: THashMode): TValueSet<T, H>;
begin
  Result := TValueSet<T, H>(CreateCopy(AHashMode));
end;

class function TValueSet<T, H>.GetHash(AKey: T): Cardinal;
begin
  Result := H.GetHash(AKey);
end;

class function TValueSet<T, H>.KeysEqual(AKey1, AKey2: T): Boolean;
begin
  Result := H.KeysEqual(AKey1, AKey2);
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

function TMap<K, V>.Copy(AHashMode: THashMode): TMap<K, V>;
begin
  Result := TMap<K, V>(CreateCopy(AHashMode));
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

function TMap<K, V>.Get(AKey: K; out APair: TPair): Boolean;
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

function TMap<K, V>.GetActualKey(AKey: K; out AActualKey: K): Boolean;
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

function TMap<K, V>.GetActualKeyE(AKey: K): K;
begin
  if not GetActualKey(AKey, Result) then
    raise EMapKeyNotFound.Create;
end;

function TMap<K, V>.GetBuckets: Integer;
begin
  Result := Length(FBuckets);
end;

function TMap<K, V>.SetActualKey(AKey, AActualKey: K): Boolean;
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

procedure TMap<K, V>.SetActualKeyE(AKey: K; const Value: K);
begin
  if not SetActualKey(AKey, Value) then
    raise EMapKeyNotFound.Create;
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

procedure TMap<K, V>.SetValue(AKey: K; const Value: V);
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
  if HashMode = hmAuto then
    Buckets := Count;
  Result := True;
end;

function TMap<K, V>.GetEnumerator: IIterator<TPair>;
begin
  Result := TIterator.Create(Self);
end;

function TMap<K, V>.GetValue(AKey: K): V;
begin
  if not Get(AKey, Result) then
    raise EMapKeyNotFound.Create;
end;

function TMap<K, V>.GetPair(AKey: K): TPair;
begin
  if not Get(AKey, Result) then
    raise EMapKeyNotFound.Create;
end;

function TMap<K, V>.KeyExists(AKey: K): Boolean;
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

function TMap<K, V>.KeySet(AHashMode: THashMode): TSet<K>;
var
  Pair: TPair;
begin
  Result := TSet<K>(CreateSame(hmManual));
  Result.Rehash(Buckets);
  for Pair in Self do
    Result.TryAdd(Pair.Key);
  Result.HashMode := AHashMode;
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

function TValueMap<K, V, H>.Copy(AHashMode: THashMode): TValueMap<K, V, H>;
begin
  Result := TValueMap<K, V, H>(CreateCopy(AHashMode));
end;

end.
