unit Pengine.ICollections;

interface

uses
  System.SysUtils,

  Pengine.EventHandling;

type

  EListError = class(Exception);

  EIterateError = class(Exception);

  /// <summary>Defines logic to iterate over items of an IIterable&lt;T&gt;.</summary>
  IIterator<T> = interface
    function GetCurrent: T;

    function MoveNext: Boolean;
    property Current: T read GetCurrent;

  end;

  IIterate<T> = interface;

  IList<T> = interface;

  /// <summary>Iterable using a for-in loop.</summary>
  IIterable<T> = interface
    function GetEnumerator: IIterator<T>;
    function Iterate: IIterate<T>;

  end;

  /// <summary>Serves as a wrapper for iterate operations that require generic parameters.</summary>
  TGenericWrapper<T> = record
  private
    FIterate: IIterate<T>;

  public
    constructor Create(AIterate: IIterate<T>);

    function Map<R>(AFunc: TFunc<T, R>): IIterate<R>;
    function Zip<R>(AIterable: IIterable<T>; AFunc: TFunc<T, T, R>): IIterate<R>; overload;
    function Zip<U, R>(AIterable: IIterable<U>; AFunc: TFunc<T, U, R>): IIterate<R>; overload;

  end;

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

  end;

  /// <summary>A collection, which is readonly.</summary>
  IReadonlyCollection<T> = interface(IIterable<T>)
    function GetCount: Integer;

    property Count: Integer read GetCount;
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
    function IndexOfLast(AItem: T): Integer;

  end;

  /// <summary>A collection of items, that are not necessarily ordered in any way.</summary>
  ICollection<T> = interface(IIterable<T>)
    function GetCount: Integer;

    property Count: Integer read GetCount;
    function Contains(AItem: T): Boolean;

    procedure Add(AItem: T);
    procedure Remove(AItem: T);
    function TryRemove(AItem: T): Boolean;
    procedure Clear;

    procedure AddRange(AItems: array of T); overload;
    procedure AddRange(AItems: IIterable<T>); overload;
    procedure AddRange(AItems: IIterator<T>); overload;

    function ReadonlyCollection: IReadonlyCollection<T>;

  end;

  /// <summary>An ordered collection of items.</summary>
  IList<T> = interface(ICollection<T>)
    function GetItem(AIndex: Integer): T;
    procedure SetItem(AIndex: Integer; AItem: T);
    function GetMaxIndex: Integer;
    function GetFirst: T;
    function GetLast: T;

    property Items[AIndex: Integer]: T read GetItem write SetItem; default;

    property MaxIndex: Integer read GetMaxIndex;
    property First: T read GetFirst;
    property Last: T read GetLast;

    function IndexOf(AItem: T): Integer;
    function IndexOfLast(AItem: T): Integer;

    procedure Insert(AIndex: Integer; AItem: T);
    procedure RemoveAt(AIndex: Integer);
    procedure RemoveRange(AIndex, ACount: Integer);
    procedure RemoveLast(AItem: T);

    function Extract(AIndex: Integer): T;

    procedure InsertRange(AIndex: Integer; AItems: array of T); overload;
    procedure InsertRange(AIndex: Integer; AItems: IIterable<T>); overload;
    procedure InsertRange(AIndex: Integer; AItems: IIterator<T>); overload;

    function ReadonlyList: IReadonlyList<T>;

    function Copy: IList<T>;
    procedure Assign(AFrom: IList<T>);

  end;

  TList<T> = class(TInterfacedObject, IIterable<T>, IReadonlyCollection<T>, IReadonlyList<T>, ICollection<T>, IList<T>)
  public const

    GrowAmount = 16;

  private
    FItems: array of T;
    FCount: Integer;

    procedure EnsureCapacity(ACount: Integer);
    procedure ReduceCapacity(ACount: Integer);

    function GetCount: Integer;
    function GetMaxIndex: Integer;

    function GetItem(AIndex: Integer): T;
    procedure SetItem(AIndex: Integer; AValue: T);

    function GetFirst: T;
    function GetLast: T;

    function ItemsEqual(A, B: T): Boolean; virtual;

  protected
    procedure RangeCheck(AIndex: Integer); inline;

    procedure DoRemoveAt(AIndex: Integer);
    procedure DoRemoveRange(AIndex, ACount: Integer);

  public
    constructor Create; overload; virtual;
    constructor Create(AItems: array of T); overload;
    constructor Create(AIterable: IIterable<T>); overload;
    constructor Create(AIterator: IIterator<T>); overload;

    // IIterable<T>
    function GetEnumerator: IIterator<T>;
    function Iterate: IIterate<T>;

    // ICollection<T>
    property Count: Integer read GetCount;
    function Contains(AItem: T): Boolean;

    procedure Add(AItem: T);
    procedure Remove(AItem: T);
    function TryRemove(AItem: T): Boolean;
    procedure Clear;

    procedure AddRange(AItems: array of T); overload;
    procedure AddRange(AItems: IIterable<T>); overload;
    procedure AddRange(AItems: IIterator<T>); overload;

    function ReadonlyCollection: IReadonlyCollection<T>;

    // IList<T>
    property Items[AIndex: Integer]: T read GetItem write SetItem; default;

    property MaxIndex: Integer read GetMaxIndex;
    property First: T read GetFirst;
    property Last: T read GetLast;

    function IndexOf(AItem: T): Integer;
    function IndexOfLast(AItem: T): Integer;

    procedure Insert(AIndex: Integer; AItem: T);
    procedure RemoveAt(AIndex: Integer);
    procedure RemoveRange(AIndex, ACount: Integer);
    procedure RemoveLast(AItem: T);

    function Extract(AIndex: Integer): T;

    procedure InsertRange(AIndex: Integer; AItems: array of T); overload;
    procedure InsertRange(AIndex: Integer; AItems: IIterable<T>); overload;
    procedure InsertRange(AIndex: Integer; AItems: IIterator<T>); overload;

    function ReadonlyList: IReadonlyList<T>;

    function Copy: IList<T>;
    procedure Assign(AFrom: IList<T>);

  end;

  TListIterator<T> = class(TInterfacedObject, IIterator<T>)
  private
    FList: TList<T>;
    FCurrent: Integer;

    function GetCurrent: T;

  public
    constructor Create(AList: TList<T>);

    function MoveNext: Boolean;

  end;

  IObjectList<T> = interface(IList<T>)
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(AValue: Boolean);

    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;

  end;

  TObjectList<T: class> = class(TList<T>, IIterable<T>, IReadonlyCollection<T>, IReadonlyList<T>, ICollection<T>,
    IList<T>, IObjectList<T>)
  private
    FOwnsObjects: Boolean;

    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(AValue: Boolean);

  public
    constructor Create; overload; override;
    destructor Destroy; override;

    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;

    procedure Clear;
    procedure Remove(AItem: T);
    procedure RemoveAt(AIndex: Integer);
    procedure RemoveRange(AIndex, ACount: Integer);
    procedure RemoveLast(AItem: T);

  end;

  TIterate<T> = class(TInterfacedObject, IIterable<T>, IIterate<T>)
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

  end;

  TIterableIterate<T> = class(TIterate<T>, IIterable<T>, IIterate<T>)
  private
    FIterable: IIterable<T>;

  public
    constructor Create(AIterable: IIterable<T>);

    // IIterable<T>
    function GetEnumerator: IIterator<T>; override;

  end;

  TWhereIterator<T> = class(TInterfacedObject, IIterator<T>)
  private
    FIterator: IIterator<T>;
    FPredicate: TPredicate<T>;

    function GetCurrent: T;

  public
    constructor Create(AIterator: IIterator<T>; APredicate: TPredicate<T>);

    function MoveNext: Boolean;

  end;

  TWhereIterate<T> = class(TIterate<T>, IIterable<T>, IIterate<T>)
  private
    FIterator: IIterator<T>;
    FPredicate: TPredicate<T>;

  public
    constructor Create(AIterator: IIterator<T>; APredicate: TPredicate<T>);

    // IIterable<T>
    function GetEnumerator: IIterator<T>; override;

  end;

  TTakeWhileIterator<T> = class(TInterfacedObject, IIterator<T>)
  private
    FIterator: IIterator<T>;
    FPredicate: TPredicate<T>;

    function GetCurrent: T;

  public
    constructor Create(AIterator: IIterator<T>; APredicate: TPredicate<T>);

    function MoveNext: Boolean;

  end;

  TTakeWhileIterate<T> = class(TIterate<T>, IIterable<T>, IIterate<T>)
  private
    FIterator: IIterator<T>;
    FPredicate: TPredicate<T>;

  public
    constructor Create(AIterator: IIterator<T>; APredicate: TPredicate<T>);

    // IIterable<T>
    function GetEnumerator: IIterator<T>; override;

  end;

  TSkipUntilIterator<T> = class(TInterfacedObject, IIterator<T>)
  private
    FIterator: IIterator<T>;
    FState: Integer;

    function GetCurrent: T;

  public
    constructor Create(AIterator: IIterator<T>; APredicate: TPredicate<T>);

    function MoveNext: Boolean;

  end;

  TSkipUntilIterate<T> = class(TIterate<T>, IIterable<T>, IIterate<T>)
  private
    FIterator: IIterator<T>;
    FPredicate: TPredicate<T>;

  public
    constructor Create(AIterator: IIterator<T>; APredicate: TPredicate<T>);

    // IIterable<T>
    function GetEnumerator: IIterator<T>; override;

  end;

  TRangeIterator<T> = class(TInterfacedObject, IIterator<T>)
  private
    FIterator: IIterator<T>;
    FCount: Integer;
    FNotEmpty: Boolean;

    function GetCurrent: T;

  public
    constructor Create(AIterator: IIterator<T>; AIndex, ACount: Integer);

    function MoveNext: Boolean;

  end;

  TRangeIterate<T> = class(TIterate<T>, IIterable<T>, IIterate<T>)
  private
    FIterator: IIterator<T>;
    FIndex: Integer;
    FCount: Integer;

  public
    constructor Create(AIterator: IIterator<T>; AIndex, ACount: Integer);

    // IIterable<T>
    function GetEnumerator: IIterator<T>; override;

  end;

  TMapIterator<T, R> = class(TInterfacedObject, IIterator<R>)
  private
    FIterator: IIterator<T>;
    FFunc: TFunc<T, R>;

    function GetCurrent: R;

  public
    constructor Create(AIterator: IIterator<T>; AFunc: TFunc<T, R>);

    function MoveNext: Boolean;

  end;

  TMapIterate<T, R> = class(TIterate<R>, IIterable<R>, IIterate<R>)
  private
    FIterator: IIterator<T>;
    FFunc: TFunc<T, R>;

  public
    constructor Create(AIterator: IIterator<T>; AFunc: TFunc<T, R>);

    // IIterable<R>
    function GetEnumerator: IIterator<R>; override;

  end;

  TZipIterator<T, U, R> = class(TInterfacedObject, IIterator<R>)
  private
    FIterator1: IIterator<T>;
    FIterator2: IIterator<U>;
    FFunc: TFunc<T, U, R>;

    function GetCurrent: R;

  public
    constructor Create(AIterator1: IIterator<T>; AIterator2: IIterator<U>; AFunc: TFunc<T, U, R>);

    function MoveNext: Boolean;

  end;

  TZipIterate<T, U, R> = class(TIterate<R>, IIterable<R>, IIterate<R>)
  private
    FIterator1: IIterator<T>;
    FIterator2: IIterator<U>;
    FFunc: TFunc<T, U, R>;

  public
    constructor Create(AIterator1: IIterator<T>; AIterator2: IIterator<U>; AFunc: TFunc<T, U, R>);

    // IIterable<R>
    function GetEnumerator: IIterator<R>; override;

  end;

  TConcatIterator<T> = class(TInterfacedObject, IIterator<T>)
  private
    FIterator1: IIterator<T>;
    FIterator2: IIterator<T>;

    function GetCurrent: T;

  public
    constructor Create(AIterator1, AIterator2: IIterator<T>);

    function MoveNext: Boolean;

  end;

  TConcatIterate<T> = class(TIterate<T>, IIterable<T>, IIterate<T>)
  private
    FIterator1: IIterator<T>;
    FIterator2: IIterator<T>;

  public
    constructor Create(AIterator1, AIterator2: IIterator<T>);

    // IIterable<T>
    function GetEnumerator: IIterator<T>; override;

  end;

  TAppendIterator<T> = class(TInterfacedObject, IIterator<T>)
  private
    FIterator: IIterator<T>;
    FItem: T;

    function GetCurrent: T;

  public
    constructor Create(AIterator: IIterator<T>; AItem: T);

    function MoveNext: Boolean;

  end;

  TAppendIterate<T> = class(TIterate<T>, IIterable<T>, IIterate<T>)
  private
    FIterator: IIterator<T>;
    FItem: T;

  public
    constructor Create(AIterator: IIterator<T>; AItem: T);

    // IIterable<T>
    function GetEnumerator: IIterator<T>; override;

  end;
       
  TPrependIterator<T> = class(TInterfacedObject, IIterator<T>)
  private
    FIterator: IIterator<T>;
    FItem: T;
    FState: Integer;

    function GetCurrent: T;

  public
    constructor Create(AIterator: IIterator<T>; AItem: T);

    function MoveNext: Boolean;

  end;

  TPrependIterate<T> = class(TIterate<T>, IIterable<T>, IIterate<T>)
  private
    FIterator: IIterator<T>;
    FItem: T;

  public
    constructor Create(AIterator: IIterator<T>; AItem: T);

    // IIterable<T>
    function GetEnumerator: IIterator<T>; override;

  end;

implementation

{ TList<T> }

procedure TList<T>.EnsureCapacity(ACount: Integer);
var
  Capacity: Integer;
begin
  Capacity := Length(FItems);
  if Capacity >= ACount then
    Exit;
  Inc(Capacity, (ACount - Capacity + GrowAmount - 1) div GrowAmount * GrowAmount);
  SetLength(FItems, Capacity);
end;

function TList<T>.Extract(AIndex: Integer): T;
begin
  RangeCheck(AIndex);
  Result := Items[AIndex];
  RemoveAt(AIndex);
end;

procedure TList<T>.ReduceCapacity(ACount: Integer);
var
  Capacity: Integer;
begin
  Capacity := Length(FItems);
  if Capacity - ACount >= GrowAmount then
    Exit;
  Dec(Capacity, (Capacity - ACount) div GrowAmount * GrowAmount);
  SetLength(FItems, Capacity);
end;

function TList<T>.GetCount: Integer;
begin
  Result := FCount;
end;

function TList<T>.GetMaxIndex: Integer;
begin
  Result := FCount - 1;
end;

function TList<T>.GetItem(AIndex: Integer): T;
begin
  Result := FItems[AIndex];
end;

procedure TList<T>.SetItem(AIndex: Integer; AValue: T);
begin
  FItems[AIndex] := AValue;
end;

function TList<T>.TryRemove(AItem: T): Boolean;
var
  I: Integer;
begin
  I := IndexOf(AItem);
  if I = -1 then
    Exit(False);
  RemoveAt(I);
  Result := True;
end;

function TList<T>.GetFirst: T;
begin
  Result := Items[0];
end;

function TList<T>.GetLast: T;
begin
  Result := Items[MaxIndex];
end;

function TList<T>.ItemsEqual(A, B: T): Boolean;
begin
  if (GetTypeKind(T) = tkRecord) and IsManagedType(T) then
    raise EListError.Create('Comparing managed records is not supported.');
  case GetTypeKind(T) of
    tkInteger, tkChar, tkEnumeration, tkFloat, tkSet, tkClass, tkMethod, tkWChar, tkRecord, tkInterface, tkInt64,
      tkClassRef, tkPointer:
      begin
        Result := CompareMem(@A, @B, SizeOf(T));
      end;
    tkString:
      Result := PString(@A)^ = PString(@B)^;
    tkWString:
      Result := PWideString(@A)^ = PWideString(@B)^;
    tkUString:
      Result := PUnicodeString(@A)^ = PUnicodeString(@B)^;
    tkAnsiString:
      Result := PAnsiString(@A)^ = PAnsiString(@B)^;
  else
    raise EListError.Create('Unsupported object comparision.');
  end;
end;

function TList<T>.Iterate: IIterate<T>;
begin
  Result := TIterableIterate<T>.Create(Self);
end;

constructor TList<T>.Create;
begin
  // nothing
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

function TList<T>.Contains(AItem: T): Boolean;
begin
  Result := IndexOf(AItem) <> -1;
end;

function TList<T>.Copy: IList<T>;
begin
  Result := TList<T>.Create(Self);
end;

procedure TList<T>.Add(AItem: T);
begin
  Insert(Count, AItem);
end;

procedure TList<T>.Remove(AItem: T);
begin
  if not TryRemove(AItem) then
    raise EListError.Create('Item not found.');
end;

procedure TList<T>.Clear;
begin
  FCount := 0;
  SetLength(FItems, 0);
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

procedure TList<T>.Assign(AFrom: IList<T>);
begin
  Clear;
  AddRange(AFrom);
end;

procedure TList<T>.RangeCheck(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EListError.Create('List index out of range.')at ReturnAddress;
end;

function TList<T>.ReadonlyCollection: IReadonlyCollection<T>;
begin
  Result := Self;
end;

function TList<T>.IndexOf(AItem: T): Integer;
var
  I: Integer;
begin
  for I := 0 to MaxIndex do
    if ItemsEqual(AItem, FItems[I]) then
      Exit(I);
  Result := -1;
end;

function TList<T>.IndexOfLast(AItem: T): Integer;
var
  I: Integer;
begin
  for I := MaxIndex downto 0 do
    if ItemsEqual(AItem, FItems[I]) then
      Exit(I);
  Result := -1;
end;

procedure TList<T>.Insert(AIndex: Integer; AItem: T);
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

procedure TList<T>.RemoveAt(AIndex: Integer);
begin
  RangeCheck(AIndex);
  DoRemoveAt(AIndex);
end;

procedure TList<T>.RemoveLast(AItem: T);
begin
  RemoveAt(IndexOfLast(AItem));
end;

procedure TList<T>.RemoveRange(AIndex, ACount: Integer);
begin
  RangeCheck(AIndex);
  RangeCheck(AIndex + ACount - 1);
  DoRemoveRange(AIndex, ACount);
end;

procedure TList<T>.InsertRange(AIndex: Integer; AItems: array of T);
var
  I: Integer;
begin
  for I := 0 to Length(AItems) - 1 do
    Insert(AIndex + I, AItems[I]);
end;

procedure TList<T>.InsertRange(AIndex: Integer; AItems: IIterable<T>);
begin
  InsertRange(AIndex, AItems.GetEnumerator);
end;

procedure TList<T>.InsertRange(AIndex: Integer; AItems: IIterator<T>);
begin
  while AItems.MoveNext do
  begin
    Insert(AIndex, AItems.Current);
    Inc(AIndex);
  end;
end;

function TList<T>.ReadonlyList: IReadonlyList<T>;
begin
  Result := Self;
end;

procedure TList<T>.DoRemoveAt(AIndex: Integer);
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

procedure TList<T>.DoRemoveRange(AIndex, ACount: Integer);
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

{ TListIterator<T> }

constructor TListIterator<T>.Create(AList: TList<T>);
begin
  FList := AList;
  FCurrent := -1;
end;

function TListIterator<T>.GetCurrent: T;
begin
  Result := FList[FCurrent];
end;

function TListIterator<T>.MoveNext: Boolean;
begin
  Inc(FCurrent);
  Result := FCurrent <> FList.Count;
end;

{ TObjectList<T> }

procedure TObjectList<T>.Clear;
var
  Item: T;
begin
  if OwnsObjects then
    for Item in Self do
      Item.Free;
  inherited;
end;

constructor TObjectList<T>.Create;
begin
  inherited;
  FOwnsObjects := True;
end;

destructor TObjectList<T>.Destroy;
var
  Item: T;
begin
  if OwnsObjects then
    for Item in Self do
      Item.Free;
  inherited;
end;

function TObjectList<T>.GetOwnsObjects: Boolean;
begin
  Result := FOwnsObjects;
end;

procedure TObjectList<T>.Remove(AItem: T);
begin
  RemoveAt(IndexOf(AItem));
end;

procedure TObjectList<T>.RemoveAt(AIndex: Integer);
begin
  RangeCheck(AIndex);
  if OwnsObjects then
    Items[AIndex].Free;
  DoRemoveAt(AIndex);
end;

procedure TObjectList<T>.RemoveLast(AItem: T);
begin
  RemoveAt(IndexOfLast(AItem));
end;

procedure TObjectList<T>.RemoveRange(AIndex, ACount: Integer);
var
  I: Integer;
begin
  RangeCheck(AIndex);
  RangeCheck(AIndex + ACount - 1);
  if OwnsObjects then
    for I := AIndex to AIndex + ACount - 1 do
      Items[AIndex].Free;
  DoRemoveRange(AIndex, ACount);
end;

procedure TObjectList<T>.SetOwnsObjects(AValue: Boolean);
begin
  FOwnsObjects := AValue;
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

{ TIterate<T> }

function TIterate<T>.GetItem(AIndex: Integer): T;
var
  Item: T;
begin
  if AIndex >= 0 then
  begin
    for Item in Self do
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
  Result := TRangeIterate<T>.Create(GetEnumerator, AIndex, ACount);
end;

function TIterate<T>.Where(APredicate: TPredicate<T>): IIterate<T>;
begin
  Result := TWhereIterate<T>.Create(GetEnumerator, APredicate);
end;

function TIterate<T>.Zip(AIterable: IIterable<T>; AFunc: TFunc<T, T, T>): IIterate<T>;
begin
  Result := TZipIterate<T, T, T>.Create(GetEnumerator, AIterable.GetEnumerator, AFunc);
end;

function TIterate<T>.Take(ACount: Integer): IIterate<T>;
begin
  Result := TRangeIterate<T>.Create(GetEnumerator, 0, ACount);
end;

function TIterate<T>.TakeWhile(APredicate: TPredicate<T>): IIterate<T>;
begin
  Result := TTakeWhileIterate<T>.Create(GetEnumerator, APredicate);
end;

function TIterate<T>.Skip(ACount: Integer): IIterate<T>;
begin                                                         
  Result := TRangeIterate<T>.Create(GetEnumerator, ACount, Integer.MaxValue);
end;

function TIterate<T>.SkipUntil(APredicate: TPredicate<T>): IIterate<T>;
begin
  Result := TSkipUntilIterate<T>.Create(GetEnumerator, APredicate);
end;

function TIterate<T>.Concat(AIterable: IIterable<T>): IIterate<T>;
begin
  Result := TConcatIterate<T>.Create(GetEnumerator, AIterable.GetEnumerator);
end;

function TIterate<T>.Append(AItem: T): IIterate<T>;
begin
  Result := TAppendIterate<T>.Create(GetEnumerator, AItem);
end;

function TIterate<T>.Prepend(AItem: T): IIterate<T>;
begin
  Result := TPrependIterate<T>.Create(GetEnumerator, AItem);
end;

function TIterate<T>.Map(AFunc: TFunc<T, T>): IIterate<T>;
begin
  Result := TMapIterate<T, T>.Create(GetEnumerator, AFunc);
end;

function TIterate<T>.Generic: TGenericWrapper<T>;
begin
  Result.Create(Self);
end;

function TIterate<T>.ToList: IList<T>;
begin
  Result := TList<T>.Create(Self);
end;

{ TWhereIterator<T> }

constructor TWhereIterator<T>.Create(AIterator: IIterator<T>; APredicate: TPredicate<T>);
begin
  FIterator := AIterator;
  FPredicate := APredicate;
end;

function TWhereIterator<T>.GetCurrent: T;
begin
  Result := FIterator.Current;
end;

function TWhereIterator<T>.MoveNext: Boolean;
begin
  repeat
    Result := FIterator.MoveNext;
  until not Result or FPredicate(FIterator.Current);
end;

{ TWhereIterate<T> }

constructor TWhereIterate<T>.Create(AIterator: IIterator<T>; APredicate: TPredicate<T>);
begin
  FIterator := AIterator;
  FPredicate := APredicate;
end;

function TWhereIterate<T>.GetEnumerator: IIterator<T>;
begin
  Result := TWhereIterator<T>.Create(FIterator, FPredicate);
end;

{ TTakeWhileIterate<T> }

constructor TTakeWhileIterate<T>.Create(AIterator: IIterator<T>; APredicate: TPredicate<T>);
begin
  FIterator := AIterator;
  FPredicate := APredicate;
end;

function TTakeWhileIterate<T>.GetEnumerator: IIterator<T>;
begin
  Result := TTakeWhileIterator<T>.Create(FIterator, FPredicate);
end;

{ TTakeWhileIterator<T> }

constructor TTakeWhileIterator<T>.Create(AIterator: IIterator<T>; APredicate: TPredicate<T>);
begin
  FIterator := AIterator;
  FPredicate := APredicate;
end;

function TTakeWhileIterator<T>.GetCurrent: T;
begin
  Result := FIterator.Current;
end;

function TTakeWhileIterator<T>.MoveNext: Boolean;
begin
  Result := FIterator.MoveNext and FPredicate(FIterator.Current);
end;

{ TSkipUntilIterator<T> }

constructor TSkipUntilIterator<T>.Create(AIterator: IIterator<T>; APredicate: TPredicate<T>);
begin
  FIterator := AIterator;
  if not FIterator.MoveNext then
    Exit;
  while not APredicate(FIterator.Current) do
    if not FIterator.MoveNext then
      Exit;
  FState := 1;
end;

function TSkipUntilIterator<T>.GetCurrent: T;
begin
  Result := FIterator.Current;
end;

function TSkipUntilIterator<T>.MoveNext: Boolean;
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

constructor TSkipUntilIterate<T>.Create(AIterator: IIterator<T>; APredicate: TPredicate<T>);
begin
  FIterator := AIterator;
  FPredicate := APredicate;
end;

function TSkipUntilIterate<T>.GetEnumerator: IIterator<T>;
begin
  Result := TSkipUntilIterator<T>.Create(FIterator, FPredicate);
end;

{ TGenericWrapper<T> }

constructor TGenericWrapper<T>.Create(AIterate: IIterate<T>);
begin
  FIterate := AIterate;
end;

function TGenericWrapper<T>.Map<R>(AFunc: TFunc<T, R>): IIterate<R>;
begin
  Result := TMapIterate<T, R>.Create(FIterate.GetEnumerator, AFunc);
end;

function TGenericWrapper<T>.Zip<R>(AIterable: IIterable<T>; AFunc: TFunc<T, T, R>): IIterate<R>;
begin                                                                                                                           
  Result := TZipIterate<T, T, R>.Create(FIterate.GetEnumerator, AIterable.GetEnumerator, AFunc);
end;

function TGenericWrapper<T>.Zip<U, R>(AIterable: IIterable<U>; AFunc: TFunc<T, U, R>): IIterate<R>;
begin
  Result := TZipIterate<T, U, R>.Create(FIterate.GetEnumerator, AIterable.GetEnumerator, AFunc);
end;

{ TRangeIterator<T> }

constructor TRangeIterator<T>.Create(AIterator: IIterator<T>; AIndex, ACount: Integer);
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

function TRangeIterator<T>.GetCurrent: T;
begin
  Result := FIterator.Current;
end;

function TRangeIterator<T>.MoveNext: Boolean;
begin
  Dec(FCount);
  Result := FNotEmpty and FIterator.MoveNext and (FCount >= 0);
end;

{ TRangeIterate<T> }

constructor TRangeIterate<T>.Create(AIterator: IIterator<T>; AIndex, ACount: Integer);
begin
  FIterator := AIterator;
  FIndex := AIndex;
  FCount := ACount;
end;

function TRangeIterate<T>.GetEnumerator: IIterator<T>;
begin
  Result := TRangeIterator<T>.Create(FIterator, FIndex, FCount);
end;

{ TMapIterator<T, R> }

constructor TMapIterator<T, R>.Create(AIterator: IIterator<T>; AFunc: TFunc<T, R>);
begin
  FIterator := AIterator;
  FFunc := AFunc;
end;

function TMapIterator<T, R>.GetCurrent: R;
begin
  Result := FFunc(FIterator.Current);
end;

function TMapIterator<T, R>.MoveNext: Boolean;
begin
  Result := FIterator.MoveNext;
end;

{ TMapIterate<T, R> }

constructor TMapIterate<T, R>.Create(AIterator: IIterator<T>; AFunc: TFunc<T, R>);
begin
  FIterator := AIterator;
  FFunc := AFunc;
end;

function TMapIterate<T, R>.GetEnumerator: IIterator<R>;
begin
  Result := TMapIterator<T, R>.Create(FIterator, FFunc);
end;

{ TZipIterator<T, U, R> }

constructor TZipIterator<T, U, R>.Create(AIterator1: IIterator<T>; AIterator2: IIterator<U>; AFunc: TFunc<T, U, R>);
begin
  FIterator1 := AIterator1;
  FIterator2 := AIterator2;
  FFunc := AFunc;
end;

function TZipIterator<T, U, R>.GetCurrent: R;
begin
  Result := FFunc(FIterator1.Current, FIterator2.Current);
end;

function TZipIterator<T, U, R>.MoveNext: Boolean;
begin
  Result := FIterator1.MoveNext and FIterator2.MoveNext;
end;

{ TZipIterate<T, U, R> }

constructor TZipIterate<T, U, R>.Create(AIterator1: IIterator<T>; AIterator2: IIterator<U>; AFunc: TFunc<T, U, R>);
begin
  FIterator1 := AIterator1;
  FIterator2 := AIterator2;
  FFunc := AFunc;
end;

function TZipIterate<T, U, R>.GetEnumerator: IIterator<R>;
begin
  Result := TZipIterator<T, U, R>.Create(FIterator1, FIterator2, FFunc);
end;

{ TConcatIterator<T> }

constructor TConcatIterator<T>.Create(AIterator1, AIterator2: IIterator<T>);
begin
  FIterator1 := AIterator1;
  FIterator2 := AIterator2;
end;

function TConcatIterator<T>.GetCurrent: T;
begin
  if FIterator1 = nil then
    Exit(FIterator2.Current);
  Result := FIterator1.Current;
end;

function TConcatIterator<T>.MoveNext: Boolean;
begin
  if FIterator1 = nil then
    Exit(FIterator2.MoveNext);
  Result := FIterator1.MoveNext;
  if not Result then
    FIterator1 := nil;
end;

{ TConcatIterate<T> }

constructor TConcatIterate<T>.Create(AIterator1, AIterator2: IIterator<T>);
begin
  FIterator1 := AIterator1;
  FIterator2 := AIterator2;
end;

function TConcatIterate<T>.GetEnumerator: IIterator<T>;
begin
  Result := TConcatIterator<T>.Create(FIterator1, FIterator2);
end;
         
{ TAppendIterator<T> }

constructor TAppendIterator<T>.Create(AIterator: IIterator<T>; AItem: T);
begin
  FIterator := AIterator;
  FItem := AItem;
end;

function TAppendIterator<T>.GetCurrent: T;
begin
  if FIterator = nil then
    Exit(FItem);
  Result := FIterator.Current;
end;

function TAppendIterator<T>.MoveNext: Boolean;
begin
  if FIterator = nil then
    Exit(False);
  if not FIterator.MoveNext then
    FIterator := nil;
  Result := True;
end;

{ TAppendIterate<T> }

constructor TAppendIterate<T>.Create(AIterator: IIterator<T>; AItem: T);
begin
  FIterator := AIterator;
  FItem := AItem;
end;

function TAppendIterate<T>.GetEnumerator: IIterator<T>;
begin
  Result := TAppendIterator<T>.Create(FIterator, FItem);
end;

{ TPrependIterator<T> }

constructor TPrependIterator<T>.Create(AIterator: IIterator<T>; AItem: T);
begin
  FIterator := AIterator;
  FItem := AItem;
end;

function TPrependIterator<T>.GetCurrent: T;
begin
  if FState < 2 then
    Exit(FItem);
  Result := FIterator.Current;
end;

function TPrependIterator<T>.MoveNext: Boolean;
begin
  if FState < 1 then
    Result := True
  else
    Result := FIterator.MoveNext;
  if FState < 2 then
    Inc(FState);
end;

{ TPrependIterate<T> }

constructor TPrependIterate<T>.Create(AIterator: IIterator<T>; AItem: T);
begin
  FIterator := AIterator;
  FItem := AItem;
end;

function TPrependIterate<T>.GetEnumerator: IIterator<T>;
begin
  Result := TPrependIterator<T>.Create(FIterator, FItem);
end;

end.
