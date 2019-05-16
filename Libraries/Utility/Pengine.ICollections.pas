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

  /// <summary>Iterable using a for-in loop.</summary>
  IIterable<T> = interface
    function GetEnumerator: IIterator<T>;
    function Iterate: IIterate<T>;

  end;

  /// <summary>Defines various filtering operations for lazy iteration.</summary>
  IIterate<T> = interface(IIterable<T>)
    function Any(APredicate: TPredicate<T>): Boolean;
    function All(APredicate: TPredicate<T>): Boolean;
    function Reduce(AFunc: TFunc<T, T, T>): T; overload;
    function Reduce(AFunc: TFunc<T, T, T>; ADefault: T): T; overload;

    function Filter(APredicate: TPredicate<T>): IIterate<T>;
    function TakeWhile(APredicate: TPredicate<T>): IIterate<T>;
    function Range(AIndex, ACount: Integer): IIterate<T>;

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
  public
    // IIterable<T>
    function GetEnumerator: IIterator<T>; virtual; abstract;
    function Iterate: IIterate<T>; virtual; abstract;

    // IIterate<T>
    function Any(APredicate: TPredicate<T>): Boolean;
    function All(APredicate: TPredicate<T>): Boolean;
    function Reduce(AFunc: TFunc<T, T, T>): T; overload;
    function Reduce(AFunc: TFunc<T, T, T>; ADefault: T): T; overload;

    function Filter(APredicate: TPredicate<T>): IIterate<T>;
    function TakeWhile(APredicate: TPredicate<T>): IIterate<T>;
    function Range(AIndex, ACount: Integer): IIterate<T>;

  end;

  TIterableIterate<T> = class(TIterate<T>, IIterable<T>, IIterate<T>)
  private
    FIterable: IIterable<T>;

  public
    constructor Create(AIterable: IIterable<T>);

    // IIterable<T>
    function GetEnumerator: IIterator<T>; override;
    function Iterate: IIterate<T>; override;

  end;

  TFilterIterator<T> = class(TInterfacedObject, IIterator<T>)
  private
    FIterator: IIterator<T>;
    FPredicate: TPredicate<T>;

    function GetCurrent: T;

  public
    constructor Create(AIterator: IIterator<T>; APredicate: TPredicate<T>);

    function MoveNext: Boolean;

  end;

  TFilterIterate<T> = class(TIterate<T>, IIterable<T>, IIterate<T>)
  private
    FIterator: IIterator<T>;
    FPredicate: TPredicate<T>;

  public
    constructor Create(AIterator: IIterator<T>; APredicate: TPredicate<T>);

    // IIterable<T>
    function GetEnumerator: IIterator<T>; override;
    function Iterate: IIterate<T>; override;

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
    raise Exception.Create('Comparing managed records is not supported.');
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
    raise Exception.Create('Unsupported object comparision.');
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

procedure TList<T>.Add(AItem: T);
begin
  Insert(Count, AItem);
end;

procedure TList<T>.Remove(AItem: T);
begin
  RemoveAt(IndexOf(AItem));
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

function TIterableIterate<T>.Iterate: IIterate<T>;
begin
  Result := FIterable.Iterate;
end;

{ TIterate<T> }

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

function TIterate<T>.Reduce(AFunc: TFunc<T, T, T>; ADefault: T): T;
var
  Iterator: IIterator<T>;
begin
  Iterator := GetEnumerator;
  Result := ADefault;
  while Iterator.MoveNext do
    Result := AFunc(Result, Iterator.Current);
end;

function TIterate<T>.Filter(APredicate: TPredicate<T>): IIterate<T>;
begin
  Result := TFilterIterate<T>.Create(GetEnumerator, APredicate);
end;

function TIterate<T>.TakeWhile(APredicate: TPredicate<T>): IIterate<T>;
begin

end;

function TIterate<T>.Range(AIndex, ACount: Integer): IIterate<T>;
begin

end;

{ TFilterIterator<T> }

constructor TFilterIterator<T>.Create(AIterator: IIterator<T>; APredicate: TPredicate<T>);
begin
  FIterator := AIterator;
  FPredicate := APredicate;
end;

function TFilterIterator<T>.GetCurrent: T;
begin
  Result := FIterator.Current;
end;

function TFilterIterator<T>.MoveNext: Boolean;
begin
  repeat
    Result := FIterator.MoveNext;
  until not Result or FPredicate(FIterator.Current);
end;

{ TFilterIterate<T> }

constructor TFilterIterate<T>.Create(AIterator: IIterator<T>; APredicate: TPredicate<T>);
begin
  FIterator := AIterator;
  FPredicate := APredicate;
end;

function TFilterIterate<T>.GetEnumerator: IIterator<T>;
begin
  Result := TFilterIterator<T>.Create(FIterator, FPredicate);
end;

function TFilterIterate<T>.Iterate: IIterate<T>;
begin
  Result := Self;
end;

end.
