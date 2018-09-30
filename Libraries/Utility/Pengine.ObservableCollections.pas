unit Pengine.ObservableCollections;

interface

uses
  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.Hasher,
  Pengine.EventHandling;

type

  /// <summary>An array with events for changes.</summary>
  TObservableArray<T> = class(TArray<T>)
  public type

    TSelf = TObservableArray<T>;

    TEventInfo = TSenderEventInfo<TSelf>;

    TEvent = TEvent<TEventInfo>;

    TAddEventInfo = class(TEventInfo)
    private
      FIndex: Integer;
      FItem: T;

    public
      constructor Create(ASender: TSelf; AIndex: Integer; AItem: T);

      property Index: Integer read FIndex;
      property Item: T read FItem;

    end;

    TAddEvent = TEvent<TAddEventInfo>;

    TItemChangeEventInfo = class(TEventInfo)
    private
      FIndex: Integer;
      FNewItem: T;

      function GetOldItem: T;

    public
      constructor Create(ASender: TSelf; AIndex: Integer; ANewItem: T);

      property Index: Integer read FIndex;
      property OldItem: T read GetOldItem;
      property NewItem: T read FNewItem;

    end;

    TItemChangeEvent = TEvent<TItemChangeEventInfo>;

    TRemoveEventInfo = class(TEventInfo)
    private
      FIndex: Integer;

      function GetItem: T;

    public
      constructor Create(ASender: TSelf; AIndex: Integer);

      property Item: T read GetItem;
      property Index: Integer read FIndex;

    end;

    TRemoveEvent = TEvent<TRemoveEventInfo>;

    TChangeIndexEventInfo = class(TEventInfo)
    private
      FIndex: Integer;
      FDestination: Integer;

      function GetItem: T;

    public
      constructor Create(ASender: TSelf; AIndex, ADestination: Integer);

      property Item: T read GetItem;
      property Index: Integer read FIndex;
      property Destination: Integer read FDestination;

    end;

    TChangeIndexEvent = TEvent<TChangeIndexEventInfo>;

    TSwapEventInfo = class(TEventInfo)
    private
      FIndex1: Integer;
      FIndex2: Integer;

      function GetItem1: T;
      function GetItem2: T;

    public
      constructor Create(ASender: TSelf; AIndex1, AIndex2: Integer);

      property Item1: T read GetItem1;
      property Index1: Integer read FIndex1;
      property Item2: T read GetItem2;
      property Index2: Integer read FIndex2;

    end;

    TSwapEvent = TEvent<TSwapEventInfo>;

  private
    FOnChange: TEvent;
    FOnAdd: TAddEvent;
    FOnItemChange: TItemChangeEvent;
    FOnRemove: TRemoveEvent;
    FOnClear: TEvent;
    FOnChangeIndex: TChangeIndexEvent;
    FOnSwap: TSwapEvent;
    FOnSort: TEvent;

  protected
    procedure SetItem(AIndex: Integer; AValue: T); override;

    procedure BeforeSort; override;
    procedure AfterSort; override;

    function CreateCopy: TArray; override;

  public
    function Add(AItem: T): T; override;
    function Insert(AItem: T; AIndex: Integer): T; override;
    procedure RemoveAt(AIndex: Integer); override;
    procedure Clear(AZeroCapacity: Boolean = True); override;
    procedure SetIndex(ASource, ADestination: Integer); override;
    procedure Swap(A, B: Integer); override;

    function Copy: TArray<T>; reintroduce; inline;

    /// <summary>Called after every other event, that changes the array.</summary>
    function OnChange: TEvent.TAccess;
    /// <summary>Called before an new item gets added or inserted.</summary>
    function OnAdd: TAddEvent.TAccess;
    /// <summary>Called before an item changes its value.</summary>
    function OnItemChange: TItemChangeEvent.TAccess;
    /// <summary>Called before an item gets removed, except for clear.</summary>
    function OnRemove: TRemoveEvent.TAccess;
    /// <summary>Called before an item changes its position and everything else gets moved according to it.</summary>
    function OnChangeIndex: TChangeIndexEvent.TAccess;
    /// <summary>Called before two items are swapped. Does not react to SwapUnchecked and therefore sorting.</summary>
    function OnSwap: TSwapEvent.TAccess;
    /// <summary>Called right after the list got sorted.</summary>
    function OnSort: TEvent.TAccess;
    /// <summary>Called, before the array gets cleared.</summary>
    function OnClear: TEvent.TAccess;

  end;

  TObservableFindableArray<T> = class(TObservableArray<T>)
  public
    function Find(AItem: T): Integer; virtual; abstract;
    procedure Remove(AItem: T);

  end;

  TObservableRefArray<T: class> = class(TObservableFindableArray<T>)
  private
    FOwnsObjects: Boolean;

  protected
    function ShouldFreeItems: Boolean; override;
    procedure ItemRemoved(AIndex: Integer); override;

  public
    constructor Create(AOwnsObjects: Boolean); reintroduce; overload;

    function Find(AItem: T): Integer; override;

    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;

  end;

  TObservableObjectArray<T: class> = class(TObservableRefArray<T>)
  public
    constructor Create; overload; override;

  end;

  // TODO: TObservableSet<T>

  /// <summary>A map with events for changes.</summary>
  TObservableMap<K, V; H: THasher<K>> = class(TMap<K, V, H>)
  public type

    TSelf = TObservableMap<K, V, H>;

    TEventInfo = TSenderEventInfo<TSelf>;

    TEvent = TEvent<TEventInfo>;

    TAddEventInfo = class(TEventInfo)
    private
      FKey: K;
      FValue: V;

    public
      constructor Create(ASender: TSelf; AKey: K; AValue: V);

      property Key: K read FKey;
      property Value: V read FValue;

    end;

    TAddEvent = TEvent<TAddEventInfo>;

    TValueChangeEventInfo = class(TEventInfo)
    private
      FKey: K;
      FNewValue: V;

      function GetOldValue: V;

    public
      constructor Create(ASender: TSelf; AKey: K; ANewValue: V);

      property Key: K read FKey;
      property OldValue: V read GetOldValue;
      property NewValue: V read FNewValue;

    end;

    TValueChangeEvent = TEvent<TValueChangeEventInfo>;

    TRemoveEventInfo = class(TEventInfo)
    private
      FKey: K;

      function GetValue: V;

    public
      constructor Create(ASender: TSelf; AKey: K);

      property Key: K read FKey;
      property Value: V read GetValue;

    end;

    TRemoveEvent = TEvent<TRemoveEventInfo>;

    TReader = class(TMap<K, V, H>.TReader)
    public
      function Copy: TMap<K, V, H>; reintroduce; inline;

      function OnChange: TEvent.TAccess;
      function OnAdd: TAddEvent.TAccess;
      function OnValueChange: TValueChangeEvent.TAccess;
      function OnRemove: TRemoveEvent.TAccess;
      function OnClear: TEvent.TAccess;

    end;

  private
    FOnChange: TEvent;
    FOnAdd: TAddEvent;
    FOnValueChange: TValueChangeEvent;
    FOnRemove: TRemoveEvent;
    FOnClear: TEvent;

  protected
    procedure BeforeValueAdd(AKey: K; AValue: V); override;
    procedure BeforeValueChange(AKey: K; AValue: V); override;
    procedure BeforeRemove(AKey: K); override;
    procedure AfterChange; override;

    function CreateCopy: THashBase; override;

  public
    procedure Clear; override;

    function Copy: TMap<K, V, H>; reintroduce; inline;

    function OnChange: TEvent.TAccess;
    function OnAdd: TAddEvent.TAccess;
    function OnValueChange: TValueChangeEvent.TAccess;
    function OnRemove: TRemoveEvent.TAccess;
    function OnClear: TEvent.TAccess;

    function Reader: TReader; reintroduce; inline;

  end;

  /// <summary>A generic map where the key is an object reference.</summary>
  TObservableRefMap<K: class; V; H: THasher<K>> = class(TObservableMap<K, V, H>)
  private
    FOwnsKeys: Boolean;
    FStoredOwnsKeys: Boolean;

  protected
    function CreateCopy: THashBase; override;

    function CreateBucket: THashBase.TBucket; override;

    procedure UnownObjects; override;
    procedure ReownObjects; override;

  public
    constructor Create(AOwnsKeys: Boolean); reintroduce; overload;

    property OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;

    function Copy: TRefMap<K, V, H>; reintroduce; inline;

  end;

  /// <summary>A generic map where the key is an object reference, that uses the default reference hashing algorithm.</summary>
  TObservableRefMap<K: class; V> = class(TObservableRefMap<K, V, TRefHasher<K>>)
  protected
    function CreateCopy: THashBase; override;

  public
    function Copy: TRefMap<K, V>; reintroduce; inline;

  end;

  /// <summary>A generic map where the key is an object.</summary>
  TObservableObjectMap<K: class; V; H: THasher<K>> = class(TObservableRefMap<K, V, H>)
  public
    constructor Create; override;

  end;

  /// <summary>A generic map where the key is an object, that uses the default reference hashing algorithm.</summary>
  TObservableObjectMap<K: class; V> = class(TObservableObjectMap<K, V, TRefHasher<K>>);

  /// <summary>A generic map from and to an object reference.</summary>
  TObservableRefRefMap<K, V: class; H: THasher<K>> = class(TObservableRefMap<K, V, H>)
  private
    FOwnsValues: Boolean;
    FStoredOwnsValues: Boolean;

  protected
    function CreateCopy: THashBase; override;

    function CreateBucket: THashBase.TBucket; override;

    procedure UnownObjects; override;
    procedure ReownObjects; override;

  public
    constructor Create(AOwnsObjects: Boolean); reintroduce; overload;
    constructor Create(AOwnsKeys, AOwnsValues: Boolean); reintroduce; overload;

    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;

    function Equals(Obj: TObject): Boolean; overload; override;
    function Equals(AOther: TRefRefMap<K, V, H>): Boolean; reintroduce; overload;

    function Copy: TRefRefMap<K, V, H>; reintroduce; inline;

  end;

  /// <summary>A generic map from and to an object referece, using the default reference hashing algorithm.</summary>
  TObservableRefRefMap<K, V: class> = class(TObservableRefRefMap<K, V, TRefHasher<K>>)
  public
    function Copy: TRefRefMap<K, V>; reintroduce; inline;

  end;

  /// <summary>A generic map from object to object reference.</summary>
  TObservableObjectRefMap<K, V: class; H: THasher<K>> = class(TObservableRefRefMap<K, V, H>)
  public
    constructor Create; overload; override;
    constructor Create(AOwnsValues: Boolean); reintroduce; overload;

  end;

  /// <summary>A generic map from object to object reference, using the default reference hashing algorithm.</summary>
  TObservableObjectRefMap<K, V: class> = class(TObservableObjectRefMap<K, V, TRefHasher<K>>);

  /// <summary>A generic map from and to an object.</summary>
  TObservableObjectObjectMap<K, V: class; H: THasher<K>> = class(TObservableRefRefMap<K, V, H>)
  public
    constructor Create; override;

  end;

  /// <summary>A generic map from and to an object, using the default reference hashing algorithm.</summary>
  TObservableObjectObjectMap<K, V: class> = class(TObservableObjectObjectMap<K, V, TRefHasher<K>>);

  /// <summary>A generic map from an object reference to an object.</summary>
  TObservableRefObjectMap<K, V: class; H: THasher<K>> = class(TObservableRefRefMap<K, V, H>)
  public
    constructor Create; overload; override;
    constructor Create(AOwnsKeys: Boolean); reintroduce; overload;

  end;

  /// <summary>A generic map from an object reference to an object, using the default reference hashing algorithm.</summary>
  TObservableRefObjectMap<K, V: class> = class(TObservableRefObjectMap<K, V, TRefHasher<K>>);

  /// <summary>A generic map to an object reference.</summary>
  TObservableToRefMap<K; V: class; H: THasher<K>> = class(TObservableMap<K, V, H>)
  private
    FOwnsValues: Boolean;
    FStoredOwnsValues: Boolean;

  protected
    function CreateCopy: THashBase; override;

    function CreateBucket: THashBase.TBucket; override;

    procedure UnownObjects; override;
    procedure ReownObjects; override;

  public
    constructor Create(AOwnsValues: Boolean); reintroduce; overload;

    function Copy: TToRefMap<K, V, H>; reintroduce; inline;

    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;

    function Equals(Obj: TObject): Boolean; overload; override;
    function Equals(AOther: TToRefMap<K, V, H>): Boolean; reintroduce; overload;

  end;

  /// <summary>A generic map to an object.</summary>
  TObservableToObjectMap<K; V: class; H: THasher<K>> = class(TObservableToRefMap<K, V, H>)
  public
    constructor Create; override;

  end;

implementation

{ TObservableArray<T>.TAddEventInfo }

constructor TObservableArray<T>.TAddEventInfo.Create(ASender: TSelf; AIndex: Integer; AItem: T);
begin
  inherited Create(ASender);
  FIndex := AIndex;
  FItem := AItem;
end;

{ TObservableArray<T>.TRemoveEventInfo }

constructor TObservableArray<T>.TRemoveEventInfo.Create(ASender: TSelf; AIndex: Integer);
begin
  inherited Create(ASender);
  FIndex := AIndex;
end;

function TObservableArray<T>.TRemoveEventInfo.GetItem: T;
begin
  Result := Sender[Index];
end;

{ TObservableArray<T>.TChangeIndexEventInfo }

constructor TObservableArray<T>.TChangeIndexEventInfo.Create(ASender: TSelf; AIndex, ADestination: Integer);
begin
  inherited Create(ASender);
  FIndex := AIndex;
  FDestination := ADestination;
end;

function TObservableArray<T>.TChangeIndexEventInfo.GetItem: T;
begin
  Result := Sender[Index];
end;

{ TObservableArray<T>.TSwapEventInfo }

constructor TObservableArray<T>.TSwapEventInfo.Create(ASender: TSelf; AIndex1, AIndex2: Integer);
begin
  inherited Create(ASender);
  FIndex1 := AIndex1;
  FIndex2 := AIndex2;
end;

function TObservableArray<T>.TSwapEventInfo.GetItem1: T;
begin
  Result := Sender[Index1];
end;

function TObservableArray<T>.TSwapEventInfo.GetItem2: T;
begin
  Result := Sender[Index2];
end;

{ TObservableArray<T> }

function TObservableArray<T>.Add(AItem: T): T;
begin
  FOnAdd.Execute(TAddEventInfo.Create(Self, Count, AItem));
  Result := inherited;
  FOnChange.Execute(TEventInfo.Create(Self));
end;

procedure TObservableArray<T>.AfterSort;
begin
  FOnChange.Execute(TEventInfo.Create(Self));
end;

procedure TObservableArray<T>.BeforeSort;
begin
  FOnSort.Execute(TEventInfo.Create(Self));
end;

procedure TObservableArray<T>.Clear(AZeroCapacity: Boolean);
begin
  if not Empty then
  begin
    FOnClear.Execute(TEventInfo.Create(Self));
    inherited;
    FOnChange.Execute(TEventInfo.Create(Self));
  end
  else if Capacity > 0 then
    inherited;
end;

function TObservableArray<T>.Copy: TArray<T>;
begin
  Result := TArray<T>(CreateCopy);
end;

function TObservableArray<T>.CreateCopy: TArray;
begin
  Result := TArray<T>.Create;
  CopyTo(Result);
end;

function TObservableArray<T>.Insert(AItem: T; AIndex: Integer): T;
begin
  FOnAdd.Execute(TAddEventInfo.Create(Self, AIndex, AItem));
  Result := inherited;
  FOnChange.Execute(TEventInfo.Create(Self));
end;

function TObservableArray<T>.OnAdd: TAddEvent.TAccess;
begin
  Result := FOnAdd.Access;
end;

function TObservableArray<T>.OnChange: TEvent.TAccess;
begin
  Result := FOnChange.Access;
end;

function TObservableArray<T>.OnChangeIndex: TChangeIndexEvent.TAccess;
begin
  Result := FOnChangeIndex.Access;
end;

function TObservableArray<T>.OnClear: TEvent.TAccess;
begin
  Result := FOnClear.Access;
end;

function TObservableArray<T>.OnItemChange: TItemChangeEvent.TAccess;
begin
  Result := FOnItemChange.Access;
end;

function TObservableArray<T>.OnRemove: TRemoveEvent.TAccess;
begin
  Result := FOnRemove.Access;
end;

function TObservableArray<T>.OnSort: TEvent.TAccess;
begin
  Result := FOnSort.Access;
end;

function TObservableArray<T>.OnSwap: TSwapEvent.TAccess;
begin
  Result := FOnSwap.Access;
end;

procedure TObservableArray<T>.RemoveAt(AIndex: Integer);
begin
  FOnRemove.Execute(TRemoveEventInfo.Create(Self, AIndex));
  inherited;
  FOnChange.Execute(TEventInfo.Create(Self));
end;

procedure TObservableArray<T>.SetIndex(ASource, ADestination: Integer);
begin
  if ASource = ADestination then
    Exit;
  FOnChangeIndex.Execute(TChangeIndexEventInfo.Create(Self, ASource, ADestination));
  inherited;
  FOnChange.Execute(TEventInfo.Create(Self));
end;

procedure TObservableArray<T>.SetItem(AIndex: Integer; AValue: T);
begin
  FOnItemChange.Execute(TItemChangeEventInfo.Create(Self, AIndex, AValue));
  inherited;
  FOnChange.Execute(TEventInfo.Create(Self));
end;

procedure TObservableArray<T>.Swap(A, B: Integer);
begin
  if A = B then
    Exit;
  FOnSwap.Execute(TSwapEventInfo.Create(Self, A, B));
  inherited;
  FOnChange.Execute(TEventInfo.Create(Self));
end;

{ TObservableFindableArray<T> }

procedure TObservableFindableArray<T>.Remove(AItem: T);
begin
  RemoveAt(Find(AItem));
end;

{ TObservableRefArray<T> }

constructor TObservableRefArray<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
end;

function TObservableRefArray<T>.Find(AItem: T): Integer;
begin
  Result := FindFirstIndex(
    function(ACurrent: T): Boolean
    begin
      Result := ACurrent = AItem;
    end);
end;

procedure TObservableRefArray<T>.ItemRemoved(AIndex: Integer);
begin
  Self[AIndex].Free;
end;

function TObservableRefArray<T>.ShouldFreeItems: Boolean;
begin
  Result := True;
end;

{ TObservableObjectArray<T> }

constructor TObservableObjectArray<T>.Create;
begin
  inherited Create(True);
end;

{ TObservableArray<T>.TItemChangeEventInfo }

constructor TObservableArray<T>.TItemChangeEventInfo.Create(ASender: TSelf; AIndex: Integer; ANewItem: T);
begin
  inherited Create(ASender);
  FIndex := AIndex;
  FNewItem := ANewItem;
end;

function TObservableArray<T>.TItemChangeEventInfo.GetOldItem: T;
begin
  Result := Sender[Index];
end;

{ TObservableMap<K, V, H>.TAddEventInfo }

constructor TObservableMap<K, V, H>.TAddEventInfo.Create(ASender: TSelf; AKey: K; AValue: V);
begin
  inherited Create(ASender);
  FKey := AKey;
  FValue := AValue;
end;

{ TObservableMap<K, V, H>.TValueChangeEventInfo }

constructor TObservableMap<K, V, H>.TValueChangeEventInfo.Create(ASender: TSelf; AKey: K; ANewValue: V);
begin
  inherited Create(ASender);
  FKey := AKey;
  FNewValue := ANewValue;
end;

function TObservableMap<K, V, H>.TValueChangeEventInfo.GetOldValue: V;
begin
  Result := Sender[Key];
end;

{ TObservableMap<K, V, H>.TRemoveEventInfo }

constructor TObservableMap<K, V, H>.TRemoveEventInfo.Create(ASender: TSelf; AKey: K);
begin
  inherited Create(ASender);
  FKey := AKey;
end;

function TObservableMap<K, V, H>.TRemoveEventInfo.GetValue: V;
begin
  Result := Sender[Key];
end;

{ TObservableMap<K, V, H> }

procedure TObservableMap<K, V, H>.AfterChange;
begin
  inherited;
  FOnChange.Execute(TEventInfo.Create(Self));
end;

procedure TObservableMap<K, V, H>.BeforeRemove(AKey: K);
begin
  FOnRemove.Execute(TRemoveEventInfo.Create(Self, AKey));
end;

procedure TObservableMap<K, V, H>.BeforeValueAdd(AKey: K; AValue: V);
begin
  FOnAdd.Execute(TAddEventInfo.Create(Self, AKey, AValue));
end;

procedure TObservableMap<K, V, H>.BeforeValueChange(AKey: K; AValue: V);
begin
  FOnValueChange.Execute(TValueChangeEventInfo.Create(Self, AKey, AValue));
end;

procedure TObservableMap<K, V, H>.Clear;
begin
  FOnClear.Execute(TEventInfo.Create(Self));
  inherited;
  FOnChange.Execute(TEventInfo.Create(Self));
end;

function TObservableMap<K, V, H>.Copy: TMap<K, V, H>;
begin
  Result := TMap<K, V, H>(CreateCopy);
end;

function TObservableMap<K, V, H>.CreateCopy: THashBase;
begin
  Result := TMap<K, V, H>.Create;
  Result.Assign(Self);
end;

function TObservableMap<K, V, H>.OnAdd: TAddEvent.TAccess;
begin
  Result := FOnAdd.Access;
end;

function TObservableMap<K, V, H>.OnChange: TEvent.TAccess;
begin
  Result := FOnChange.Access;
end;

function TObservableMap<K, V, H>.OnClear: TEvent.TAccess;
begin
  Result := FOnClear.Access;
end;

function TObservableMap<K, V, H>.OnRemove: TRemoveEvent.TAccess;
begin
  Result := FOnRemove.Access;
end;

function TObservableMap<K, V, H>.OnValueChange: TValueChangeEvent.TAccess;
begin
  Result := FOnValueChange.Access;
end;

function TObservableMap<K, V, H>.Reader: TReader;
begin
  Result := TReader(Self);
end;

{ TObservableRefMap<K, V, H> }

function TObservableRefMap<K, V, H>.Copy: TRefMap<K, V, H>;
begin
  Result := TRefMap<K, V, H>(CreateCopy);
end;

constructor TObservableRefMap<K, V, H>.Create(AOwnsKeys: Boolean);
begin
  inherited Create;
  FOwnsKeys := AOwnsKeys;
end;

function TObservableRefMap<K, V, H>.CreateBucket: THashBase.TBucket;
begin
  Result := TRefPairArrayOwnLinked<K, V>.Create(@FOwnsKeys);
  Result.SetGrowShrink(4, 2);
end;

function TObservableRefMap<K, V, H>.CreateCopy: THashBase;
begin
  Result := TRefMap<K, V, H>.Create;
  Result.Assign(Self);
end;

procedure TObservableRefMap<K, V, H>.ReownObjects;
begin
  OwnsKeys := FStoredOwnsKeys;
end;

procedure TObservableRefMap<K, V, H>.UnownObjects;
begin
  FStoredOwnsKeys := OwnsKeys;
  OwnsKeys := False;
end;

{ TObservableRefMap<K, V> }

function TObservableRefMap<K, V>.Copy: TRefMap<K, V>;
begin
  Result := TRefMap<K, V>(CreateCopy);
end;

function TObservableRefMap<K, V>.CreateCopy: THashBase;
begin
  Result := TRefMap<K, V>.Create;
  Result.Assign(Self);
end;

{ TObservableObjectMap<K, V, H> }

constructor TObservableObjectMap<K, V, H>.Create;
begin
  inherited Create(True);
end;

{ TObservableRefRefMap<K, V, H> }

function TObservableRefRefMap<K, V, H>.Copy: TRefRefMap<K, V, H>;
begin
  Result := TRefRefMap<K, V, H>(CreateCopy);
end;

constructor TObservableRefRefMap<K, V, H>.Create(AOwnsKeys, AOwnsValues: Boolean);
begin
  inherited Create(AOwnsKeys);
  FOwnsValues := AOwnsValues;
end;

constructor TObservableRefRefMap<K, V, H>.Create(AOwnsObjects: Boolean);
begin
  inherited Create(AOwnsObjects);
  FOwnsValues := AOwnsObjects;
end;

function TObservableRefRefMap<K, V, H>.CreateBucket: THashBase.TBucket;
begin
  Result := TRefRefPairArrayOwnLinked<K, V>.Create(@FOwnsKeys, @FOwnsValues);
  Result.SetGrowShrink(4, 2);
end;

function TObservableRefRefMap<K, V, H>.CreateCopy: THashBase;
begin
  Result := TRefRefMap<K, V, H>.Create;
  Result.Assign(Self);
end;

function TObservableRefRefMap<K, V, H>.Equals(AOther: TRefRefMap<K, V, H>): Boolean;
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

function TObservableRefRefMap<K, V, H>.Equals(Obj: TObject): Boolean;
begin
  if Obj.ClassType <> ClassType then
    Exit(False);
  Result := Equals(TRefRefMap<K, V, H>(Obj));
end;

procedure TObservableRefRefMap<K, V, H>.ReownObjects;
begin
  FOwnsValues := FStoredOwnsValues;
  inherited;
end;

procedure TObservableRefRefMap<K, V, H>.UnownObjects;
begin
  inherited;
  FStoredOwnsValues := FOwnsValues;
  FOwnsValues := False;
end;

{ TObservableRefRefMap<K, V> }

function TObservableRefRefMap<K, V>.Copy: TRefRefMap<K, V>;
begin
  Result := TRefRefMap<K, V>(CreateCopy);
end;

{ TObservableObjectRefMap<K, V, H> }

constructor TObservableObjectRefMap<K, V, H>.Create;
begin
  inherited Create(True, False);
end;

constructor TObservableObjectRefMap<K, V, H>.Create(AOwnsValues: Boolean);
begin
  inherited Create(True, AOwnsValues);
end;

{ TObservableObjectObjectMap<K, V, H> }

constructor TObservableObjectObjectMap<K, V, H>.Create;
begin
  inherited Create(True);
end;

{ TObservableRefObjectMap<K, V, H> }

constructor TObservableRefObjectMap<K, V, H>.Create;
begin
  inherited Create(False, True);
end;

constructor TObservableRefObjectMap<K, V, H>.Create(AOwnsKeys: Boolean);
begin
  inherited Create(AOwnsKeys, True);
end;

{ TObservableToRefMap<K, V, H> }

function TObservableToRefMap<K, V, H>.Copy: TToRefMap<K, V, H>;
begin
  Result := TToRefMap<K, V, H>.Create;
  Result.Assign(Self);
end;

constructor TObservableToRefMap<K, V, H>.Create(AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
end;

function TObservableToRefMap<K, V, H>.CreateBucket: THashBase.TBucket;
begin
  Result := TToRefPairArrayOwnLinked<K, V>.Create(@FOwnsValues);
  Result.SetGrowShrink(4, 2);
end;

function TObservableToRefMap<K, V, H>.CreateCopy: THashBase;
begin
  Result := TToRefMap<K, V, H>.Create;
  Result.Assign(Self);
end;

function TObservableToRefMap<K, V, H>.Equals(AOther: TToRefMap<K, V, H>): Boolean;
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

function TObservableToRefMap<K, V, H>.Equals(Obj: TObject): Boolean;
begin
  if Obj.ClassType <> ClassType then
    Exit(False);
  Result := Equals(TToRefMap<K, V, H>(Obj));
end;

procedure TObservableToRefMap<K, V, H>.ReownObjects;
begin
  FOwnsValues := FStoredOwnsValues;
end;

procedure TObservableToRefMap<K, V, H>.UnownObjects;
begin
  FStoredOwnsValues := FOwnsValues;
  FOwnsValues := False;
end;

{ TObservableToObjectMap<K, V, H> }

constructor TObservableToObjectMap<K, V, H>.Create;
begin
  inherited Create(True);
end;

{ TObservableMap<K, V, H>.TReader }

function TObservableMap<K, V, H>.TReader.Copy: TMap<K, V, H>;
begin
  Result := TSelf(Self).Copy;
end;

function TObservableMap<K, V, H>.TReader.OnAdd: TAddEvent.TAccess;
begin
  Result := TSelf(Self).OnAdd;
end;

function TObservableMap<K, V, H>.TReader.OnChange: TEvent.TAccess;
begin
  Result := TSelf(Self).OnChange;
end;

function TObservableMap<K, V, H>.TReader.OnClear: TEvent.TAccess;
begin
  Result := TSelf(Self).OnClear;
end;

function TObservableMap<K, V, H>.TReader.OnRemove: TRemoveEvent.TAccess;
begin
  Result := TSelf(Self).OnRemove;
end;

function TObservableMap<K, V, H>.TReader.OnValueChange: TValueChangeEvent.TAccess;
begin
  Result := TSelf(Self).OnValueChange;
end;

end.
