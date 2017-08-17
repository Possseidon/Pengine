unit Lists;

interface

uses
  Classes, SysUtils, IntegerMaths, Math, IntfBase;

type

  { IIterator<T> }

  IIterator<T> = interface
  ['{01FA5E8D-FB71-4D60-B113-429284B8B8F7}']
    function MoveNext: Boolean;
    function GetCurrent: T;

    property Current: T read GetCurrent;
  end;

  TIterator<T> = class(TInterfacedObject, IIterator<T>)
  public
    function MoveNext: Boolean; virtual; abstract;
    function GetCurrent: T; virtual; abstract;

    property Current: T read GetCurrent;
  end;

  { IIterable<T> }

  IIterable<T> = interface
  ['{86380564-F207-4B73-A40D-F10AD12B5B98}']
    function GetEnumerator: IIterator<T>;

    function Count: Integer;
  end;

  TRefCountedIterable<T> = class(TInterfacedObject, IIterable<T>)
  public
    function GetEnumerator: IIterator<T>; virtual; abstract;

    /// <remarks>WARNING! This function should DEFINITLY be overwritten!</remarks>
    function Count: Integer; virtual;
  end;

  EGenericArrayEmpty = class(Exception)
  public
    constructor Create;
  end;

  EGenericArrayItemNotFound = class(Exception)
  public
    constructor Create;
  end;

  EGenericArrayRangeError = class(Exception)
  public
    constructor Create(AIndex, ACount: Integer);
  end;

  { TFindFunctionClass }

  TFindFunctionClass<T> = class abstract
  protected
    function Find(AElement: T): Boolean; virtual; abstract;
  end;

  TCompareFunction<T> = function(A, B: T): Boolean;
  TCompareFunctionOfObject<T> = function(A, B: T): Boolean of object;

  TFindFunctionStatic<T> = function(A: T): Boolean;
  TFindFunctionOfObject<T> = function(A: T): Boolean of object;

  { TPair }

  TPair<TKey, TData> = record
  private
    FKey: TKey;
    FData: TData;
  public
    constructor Create(AKey: TKey; AData: TData);

    property Key: TKey read FKey;
    property Data: TData read FData;
  end;

  { TGenericArray<T> }

  TIntArray = class;

  TGenericArray<T> = class(TInterfaceBase, IIterable<T>)
  private
    FItems: array of T;
    FSizeSteps: Integer;
    FCount: Integer;

    procedure SortLR(ACompareFunc: TCompareFunction<T>; ALeft, ARight: Integer); overload;
    procedure SortLR(ACompareFunc: TCompareFunctionOfObject<T>; ALeft, ARight: Integer); overload;

  public type

    { TIterator }

    TIterator = class(TIterator<T>)
    private
      FList: TGenericArray<T>;

      FCurrent: Integer;
      FReversed: Boolean;

      FRemoveFlag: Boolean;

    public
      constructor Create(AList: TGenericArray<T>; AReversed: Boolean);

      function MoveNext: Boolean; override;
      function GetCurrent: T; override;

      procedure RemoveCurrent; inline;
    end;

    TReverseWrapper = record
    private
      FGenericArray: TGenericArray<T>;
    public
      constructor Create(AGenericArray: TGenericArray<T>);
      function GetEnumerator(AAutoFree: Boolean = False): TIterator;
    end;

  protected
    function GetItem(AIndex: Integer): T;
    procedure SetItem(AIndex: Integer; AValue: T);

  public
    constructor Create(ASizeSteps: Integer = 16);
    destructor Destroy; override;

    function Add(AElement: T): T;
    function Insert(AElement: T; AIndex: Integer): T;

    procedure DelAt(AIndex: Integer); virtual;
    procedure DelLast; inline;
    procedure DelAll; virtual;

    procedure Swap(A, B: Integer);

    {$REGION 'Find Functions'}

    function FindFirstIndex(AFunc: TFindFunctionStatic<T>): Integer; overload;
    function FindFirstIndex(AFunc: TFindFunctionOfObject<T>): Integer; overload;
    function FindFirstIndex(AFunc: TFindFunctionClass<T>; ADoFree: Boolean = True): Integer; overload;

    function FindFirst(AFunc: TFindFunctionStatic<T>): T; overload;
    function FindFirst(AFunc: TFindFunctionOfObject<T>): T; overload;
    function FindFirst(AFunc: TFindFunctionClass<T>; ADoFree: Boolean = True): T; overload;

    function FindLastIndex(AFunc: TFindFunctionStatic<T>): Integer; overload;
    function FindLastIndex(AFunc: TFindFunctionOfObject<T>): Integer; overload;
    function FindLastIndex(AFunc: TFindFunctionClass<T>; ADoFree: Boolean = True): Integer; overload;

    function FindLast(AFunc: TFindFunctionStatic<T>): T; overload;
    function FindLast(AFunc: TFindFunctionOfObject<T>): T; overload;
    function FindLast(AFunc: TFindFunctionClass<T>; ADoFree: Boolean = True): T; overload;

    function FindIndexAsArray(AFunc: TFindFunctionStatic<T>): TIntArray; overload;
    function FindIndexAsArray(AFunc: TFindFunctionOfObject<T>): TIntArray; overload;
    function FindIndexAsArray(AFunc: TFindFunctionClass<T>; ADoFree: Boolean = True): TIntArray; overload;

    function FindAsArray(AFunc: TFindFunctionStatic<T>): TGenericArray<T>; overload;
    function FindAsArray(AFunc: TFindFunctionOfObject<T>): TGenericArray<T>; overload;
    function FindAsArray(AFunc: TFindFunctionClass<T>; ADoFree: Boolean = True): TGenericArray<T>; overload;

    {$ENDREGION}

    procedure Sort(AFunc: TCompareFunction<T>); overload;
    procedure Sort(AFunc: TCompareFunctionOfObject<T>); overload;

    function Copy: TGenericArray<T>; virtual;

    function Count: Integer;
    function Empty: Boolean; inline;

    property Items[I: Integer]: T read GetItem write SetItem; default;

    function First: T; inline;
    function Last: T; inline;

    function DataPointer: Pointer; inline;

    function GetEnumerator: IIterator<T>;
    function IterReversed: TReverseWrapper; inline;

    procedure RangeCheckException(AIndex: Integer); inline;
    function RangeCheck(AIndex: Integer): Boolean; inline;

    procedure Append(AItems: IIterable<T>);

  end;

  { TIntArray }

  TIntArray = class(TGenericArray<Integer>)
  public
    function Sum: Integer;
    function Difference: Integer;
    function Min: Integer;
    function Max: Integer;
    function Bounds: TIntBounds1;

    function ToString: string; override;
  end;

  { TRefArray<T> }

  TRefArray<T: class> = class(TGenericArray<T>)
  public
    function FindAsRefArray(AFunc: TFindFunctionStatic<T>): TRefArray<T>; overload;
    function FindAsRefArray(AFunc: TFindFunctionOfObject<T>): TRefArray<T>; overload;
    function FindAsRefArray(AFunc: TFindFunctionClass<T>; ADoFree: Boolean = True): TRefArray<T>; overload;

    function Find(AData: T): Integer;
    procedure Del(AData: T);

    function Copy: TGenericArray<T>; override;
    function CopyAsRefArray: TRefArray<T>;

    function ToString: string; override;
  end;

  { TObjectArray<T> }

  TObjectArray<T: class> = class(TRefArray<T>)
  public
    procedure DelAt(AIndex: Integer); override;
    procedure DelAll; override;
  end;

  { TInterfaceArray<T> }

  TInterfaceArray<T: IInterface> = class(TGenericArray<T>)
  public
    function Find(AData: T): Integer;
    procedure Del(AData: T);

    function Copy: TGenericArray<T>; override;
    function CopyAsInterfaceArray: TInterfaceArray<T>;
  end;

  { TObjectStack<T> }

  TObjectStack<T: class> = class
  private
    type

      { TItem }

    TItem = class
    public
      Prev: TItem;
      Data: T;

      constructor Create(AData: T; APrev: TItem);
    end;

  private
    FTop: TItem;
    FReferenceList: Boolean;

  public
    constructor Create(AReferenceList: Boolean = False);
    destructor Destroy; override;

    function Push(AElement: T): T;
    function Pop: Boolean;
    function Top: T;

    function Copy: TObjectStack<T>;
  end;

  { THashBase }

  THashBase<TKey> = class abstract
  protected
    FCount: Integer;
    FInternalSize: Integer;

    function GetKeyHash(AKey: TKey): Integer; virtual; abstract;
    class function CantIndex({%H-}AKey: TKey): Boolean; virtual;
    class function KeysEqual(AKey1, AKey2: TKey): Boolean; virtual; abstract;
    class function CopyKey(AKey: TKey): TKey; virtual;

  public
    constructor Create(AInternalSize: Integer);
  end;

  { TMap }

  TMap<TKey, TData> = class abstract(THashBase<TKey>)
  private type

    { THashEntry }

    THashEntry = class
    public
      Key: TKey;
      Data: TData;
      Next: THashEntry;
    end;

  public type

    { TIterator }

    TIterator = class
    private
      FList: TMap<TKey, TData>;
      FIndex: Integer;
      FEntry: THashEntry;

      function GetCurrent: TPair<TKey, TData>;

    public
      constructor Create(AList: TMap<TKey, TData>);

      function MoveNext: Boolean;
      property Current: TPair<TKey, TData> read GetCurrent;

    end;

  private
    FData: array of THashEntry;

  protected
    property InternalSize: Integer read FInternalSize;

    function GetEntry(AKey: TKey): TData;
    procedure SetEntry(AKey: TKey; AValue: TData); virtual;

    procedure FreeKey(AKey: TKey); virtual;
    procedure FreeData(AData: TData); virtual;

  public
    constructor Create(AInternalSize: Integer = 193);
    destructor Destroy; override;

    function Get(AKey: TKey; out AData: TData): Boolean;
    function HasKey(AKey: TKey): Boolean;
    procedure Del(AKey: TKey);

    property Data[AKey: TKey]: TData read GetEntry write SetEntry; default;

    function NextKeyCheck(AKey: TKey; out AOut: TKey): Boolean; overload;
    function NextKeyCheck(var AKey: TKey): Boolean; overload;

    function PrevKeyCheck(AKey: TKey; out AOut: TKey): Boolean; overload;
    function PrevKeyCheck(var AKey: TKey): Boolean; overload;

    function HasNextKey(AKey: TKey): Boolean;
    function HasPrevKey(AKey: TKey): Boolean;

    function NextKey(AKey: TKey): TKey;
    function PrevKey(AKey: TKey): TKey;

    function FirstKeyCheck(out AOut: TKey): Boolean;
    function LastKeyCheck(out AOut: TKey): Boolean;

    function FirstKey: TKey;
    function LastKey: TKey;

    function NextData(AKey: TKey): TData;
    function PrevData(AKey: TKey): TData;

    procedure DelAll;

    function GetEnumerator: TIterator;
    property Count: Integer read FCount;

  end;

  { TClassMap }

  TClassMap<T> = class(TMap<TClass, T>)
  protected
    function GetKeyHash(AKey: TClass): Integer; override;
    class function CantIndex(AKey: TClass): Boolean; override;
    class function KeysEqual(AKey1, AKey2: TClass): Boolean; override;
  end;

  { TClassRefMap }

  TClassRefMap<T: class> = class(TClassMap<T>)
  public
    function GetOrNil(AKey: TClass): T;
  end;

  { TClassObjectMap }

  TClassObjectMap<T: class> = class(TClassRefMap<T>)
  protected
    procedure FreeData(AData: T); override;
  end;

  { TRefMap<TKey, TData> }

  TRefMap<TKey: class; TData> = class(TMap<TKey, TData>)
  protected
    function GetKeyHash(AKey: TKey): Integer; override;
    class function CantIndex(AKey: TKey): Boolean; override;
    class function KeysEqual(AKey1, AKey2: TKey): Boolean; override;

    function GetActualKey(AKey: TKey; out AActualKey: TKey): Boolean;
    function ActualKey(AKey: TKey): TKey;
  end;

  { TRefRefMap<TKey, TData> }

  TRefRefMap<TKey, TData: class> = class(TRefMap<TKey, TData>)
  public
    function GetOrNil(AKey: TKey): TData;
  end;

  { TRefObjectMap<TKey, TData> }

  TRefObjectMap<TKey, TData: class> = class(TRefRefMap<TKey, TData>)
  protected
    procedure FreeData(AData: TData); override;
  end;

  { TObjectMap }

  TObjectMap<TKey: class; TData> = class(TRefMap<TKey,TData>)
  protected
    procedure FreeKey(AKey: TKey); override;
  end;

  { TObjectRefMap }

  TObjectRefMap<TKey, TData: class> = class(TObjectMap<TKey,TData>)
  protected
    function GetOrNil(AKey: TKey): TData;
  end;

  { TObjectObjectMap }

  TObjectObjectMap<TKey, TData: class> = class(TObjectRefMap<TKey,TData>)
  protected
    procedure FreeData(AData: TData); override;
  end;

  { TStringMap<TData> }

  TStringMap<TData> = class(TMap<string, TData>)
  protected
    function GetKeyHash(AKey: string): Integer; override;
    class function CantIndex(AKey: string): Boolean; override;
    class function KeysEqual(AKey1, AKey2: string): Boolean; override;
  end;

  { TAnsiStringMap<TData> }

  TAnsiStringMap<TData> = class(TMap<AnsiString, TData>)
  protected
    function GetKeyHash(AKey: AnsiString): Integer; override;
    class function CantIndex(AKey: AnsiString): Boolean; override;
    class function KeysEqual(AKey1, AKey2: AnsiString): Boolean; override;
  end;

  { TStringObjectMap }

  TStringObjectMap<TData: class> = class(TStringMap<TData>)
  protected
    procedure FreeData(AData: TData); override;
  end;

  { TAnsiStringObjectMap<TData> }

  TAnsiStringObjectMap<TData: class> = class(TAnsiStringMap<TData>)
  protected
    procedure FreeData(AData: TData); override;
  end;

  { TSet<T> }

  TSet<T> = class abstract(THashBase<T>)
  private type

    { TEntry }

    TEntry = class
    public
      Data: T;
      Next: TEntry;
    end;

    { TIterator }

    TIterator = class
    private
      FList: TSet<T>;
      FIndex: Integer;
      FEntry: TEntry;
      function GetCurrent: T;
    public
      constructor Create(AList: TSet<T>);

      function MoveNext: Boolean;
      property Current: T read GetCurrent;
    end;

  private
    FTags: array of TEntry;
    FCount: Integer;

  protected
    function GetElement(S: T): Boolean; virtual;
    procedure SetElement(S: T; AValue: Boolean); virtual;

    procedure FreeData(const {%H-}AData: T); virtual;

  public
    constructor Create(AInternalSize: Integer = 193);
    destructor Destroy; override;

    property Elements[S: T]: Boolean read GetElement write SetElement; default;
    procedure Add(S: T);
    procedure Del(S: T);

    property Count: Integer read FCount;

    procedure Clear;

    procedure Assign(ATagList: TSet<T>);

    function GetEnumerator: TIterator;
  end;

  { TRefSet }

  TRefSet<T: class> = class(TSet<T>)
  protected
    function GetKeyHash(AKey: T): Integer; override;
    class function CantIndex(AKey: T): Boolean; override;
    class function KeysEqual(AKey1, AKey2: T): Boolean; override;
  end;

  { TObjectSet }

  TObjectSet<T: class> = class(TRefSet<T>)
  protected
    procedure FreeData(const AData: T); override;
  end;

  { TTags }

  TTags = class(TSet<string>)
  protected
    function GetKeyHash(AKey: string): Integer; override;
    class function CantIndex(AKey: string): Boolean; override;
    class function KeysEqual(AKey1, AKey2: string): Boolean; override;
  end;

  { TCardinalSet }

  TCardinalSet = class(TSet<Cardinal>)
  protected
    function GetKeyHash(AKey: Cardinal): Integer; override;
    class function CantIndex({%H-}AKey: Cardinal): Boolean; override;
    class function KeysEqual(AKey1, AKey2: Cardinal): Boolean; override;
  end;

  // --- Reader Wrappers ---
  // Prevent the modification of the List
  // Of course, elements can still be accessed and changed if they aren't read only

  { TGenericArrayReader<T> }

  TGenericArrayReader<T> = class(TInterfaceBase, IIterable<T>)
  private
    function GetItem(I: Integer): T;

  protected
    FGenericArray: TGenericArray<T>;
    
  public
    constructor Create(AGenericArray: TGenericArray<T>);

    {$REGION 'Find Functions'}

    function FindFirstIndex(AFunc: TFindFunctionStatic<T>): Integer; overload; inline;
    function FindFirstIndex(AFunc: TFindFunctionOfObject<T>): Integer; overload; inline;
    function FindFirstIndex(AFunc: TFindFunctionClass<T>; ADoFree: Boolean = True): Integer; overload; inline;

    function FindFirst(AFunc: TFindFunctionStatic<T>): T; overload; inline;
    function FindFirst(AFunc: TFindFunctionOfObject<T>): T; overload; inline;
    function FindFirst(AFunc: TFindFunctionClass<T>; ADoFree: Boolean = True): T; overload; inline;

    function FindLastIndex(AFunc: TFindFunctionStatic<T>): Integer; overload; inline;
    function FindLastIndex(AFunc: TFindFunctionOfObject<T>): Integer; overload; inline;
    function FindLastIndex(AFunc: TFindFunctionClass<T>; ADoFree: Boolean = True): Integer; overload; inline;

    function FindLast(AFunc: TFindFunctionStatic<T>): T; overload; inline;
    function FindLast(AFunc: TFindFunctionOfObject<T>): T; overload; inline;
    function FindLast(AFunc: TFindFunctionClass<T>; ADoFree: Boolean = True): T; overload; inline;

    function FindIndexAsArray(AFunc: TFindFunctionStatic<T>): TIntArray; overload; inline;
    function FindIndexAsArray(AFunc: TFindFunctionOfObject<T>): TIntArray; overload; inline;
    function FindIndexAsArray(AFunc: TFindFunctionClass<T>; ADoFree: Boolean = True): TIntArray; overload; inline;

    function FindAsArray(AFunc: TFindFunctionStatic<T>): TGenericArray<T>; overload; inline;
    function FindAsArray(AFunc: TFindFunctionOfObject<T>): TGenericArray<T>; overload; inline;
    function FindAsArray(AFunc: TFindFunctionClass<T>; ADoFree: Boolean = True): TGenericArray<T>; overload; inline;

    {$ENDREGION}

    function Copy: TGenericArray<T>; inline;

    function Count: Integer;
    function Empty: Boolean; inline;

    property Items[I: Integer]: T read GetItem; default;

    function First: T; inline;
    function Last: T; inline;

    function GetEnumerator: IIterator<T>; inline;
    function IterReversed: TGenericArray<T>.TReverseWrapper; inline;

    procedure RangeCheckException(AIndex: Integer); inline;
    function RangeCheck(AIndex: Integer): Boolean; inline;

    function ToString: string; override;

  end;

  { TIntArrayReader }

  TIntArrayReader = class(TGenericArrayReader<Integer>)
  public
    constructor Create(AIntArray: TIntArray);

    function Sum: Integer; inline;
    function Difference: Integer; inline;
    function Min: Integer; inline;
    function Max: Integer; inline;
    function Bounds: TIntBounds1; inline;
  end;

  { TRefArrayReader<T> }

  TRefArrayReader<T: class> = class(TGenericArrayReader<T>)
  public
    constructor Create(ARefArray: TRefArray<T>);

    function FindAsRefArray(AFunc: TFindFunctionStatic<T>): TRefArray<T>; overload; inline;
    function FindAsRefArray(AFunc: TFindFunctionOfObject<T>): TRefArray<T>; overload; inline;
    function FindAsRefArray(AFunc: TFindFunctionClass<T>; ADoFree: Boolean = True): TRefArray<T>; overload; inline;

    function Find(AData: T): Integer; inline;
    
    function CopyAsRefArray: TRefArray<T>; inline;
  end;

  { TInterfaceArrayReader<T> }

  TInterfaceArrayReader<T: IInterface> = class(TGenericArrayReader<T>)
  public
    constructor Create(AInterfaceArray: TInterfaceArray<T>);

    function Find(AData: T): Integer;
    
    function CopyAsInterfaceArray: TInterfaceArray<T>;
  end;

function GetHash(AObject: TObject; ARange: Integer): Integer; overload; inline;
function GetHash(AString: WideString; ARange: Integer): Integer; overload; inline;
function GetHash(AString: AnsiString; ARange: Integer): Integer; overload; inline;
function GetHash(ASingle: Single; ARange: Integer): Integer; overload; inline;
function GetHash(AInteger: Integer; ARange: Integer): Integer; overload; inline;
function GetHash(AIntVector: TIntVector2; ARange: Integer): Integer; overload; inline;
function GetHash(AIntVector: TIntVector3; ARange: Integer): Integer; overload; inline;

implementation

function GetHash(AObject: TObject; ARange: Integer): Integer;
var
  I: Integer;
begin
{$IFDEF CPUX64}
  I := Integer(Pointer(AObject)) xor Integer(NativeUInt(Pointer(AObject)) shr 32);
{$ELSE}
  I := Integer(Pointer(AObject));
{$ENDIF}
  Result := (I xor Integer(I shl 3) xor (I shr 7)) mod ARange;
end;

function GetHash(AString: WideString; ARange: Integer): Integer;
begin
  if AString = '' then
    Exit(0);
  Result := Integer(Length(AString) *
    (Byte(AString[1]) or Byte(AString[(Length(AString) + 1) div 2]) shl 8 or Byte(AString[Length(AString)]) shl 16));
  Result := Result mod ARange;
end;

function GetHash(AString: AnsiString; ARange: Integer): Integer;
begin
  if AString = '' then
    Exit(0);
  Result := Integer(Length(AString) *
    (Byte(AString[1]) or Byte(AString[(Length(AString) + 1) div 2]) shl 8 or Byte(AString[Length(AString)]) shl 16));
  Result := Result mod ARange;
end;

function GetHash(ASingle: Single; ARange: Integer): Integer;
begin
  Result := PInteger(@ASingle)^ mod ARange;
end;

function GetHash(AInteger: Integer; ARange: Integer): Integer; overload;
begin
  Result := (AInteger - Low(Integer)) mod ARange;
end;

function GetHash(AIntVector: TIntVector2; ARange: Integer): Integer; overload; inline;
begin
  Result :=
    (GetHash(AIntVector.X, High(Integer)) xor
    GetHash(AIntVector.Y, High(Integer))) mod ARange;
end;

function GetHash(AIntVector: TIntVector3; ARange: Integer): Integer; overload; inline;
begin
  Result :=
    (GetHash(AIntVector.X, High(Integer)) xor
    GetHash(AIntVector.Y, High(Integer)) xor
    GetHash(AIntVector.Z, High(Integer))) mod ARange;
end;

{ TRefCountedIterable<T> }

function TRefCountedIterable<T>.Count: Integer;
var
  Element: T;
begin
  Result := 0;
  for Element in Self do
    Inc(Result);
end;

{ TObjectStack<T>.TItem }

constructor TObjectStack<T>.TItem.Create(AData: T; APrev: TItem);
begin
  Data := AData;
  Prev := APrev;
end;

{ TPair<TKey, TData> }

constructor TPair<TKey, TData>.Create(AKey: TKey; AData: TData);
begin
  FKey := AKey;
  FData := AData;
end;

{ THashTable<TKey, TData> }

function TMap<TKey, TData>.GetEntry(AKey: TKey): TData;
begin
  if not Get(AKey, Result) then
    raise Exception.Create('HashTable-Key missing');
end;

procedure TMap<TKey, TData>.SetEntry(AKey: TKey; AValue: TData);
var
  Entry: THashEntry;
  Hash: Integer;
begin
  if CantIndex(AKey) then
    raise Exception.Create('Invalid HashTable-Index');

  Hash := GetKeyHash(AKey);
  if FData[Hash] = nil then
  begin
    // create new base entry
    FData[Hash] := THashEntry.Create;
    FData[Hash].Key := CopyKey(AKey);
    FData[Hash].Data := AValue;
    Inc(FCount);
    Exit;
  end;

  Entry := FData[Hash];
  // find key in list
  while not KeysEqual(Entry.Key, AKey) do
  begin
    if Entry.Next = nil then // not found > add entry
    begin
      Entry.Next := THashEntry.Create;
      Entry.Next.Key := CopyKey(AKey);
      Entry.Next.Data := AValue;
      Inc(FCount);
      Exit;
    end;
    Entry := Entry.Next;
  end;

  // update Data
  FreeData(Entry.Data);
  Entry.Data := AValue;
end;

procedure TMap<TKey, TData>.FreeKey(AKey: TKey);
begin
  // nothing by default
end;

procedure TMap<TKey, TData>.FreeData(AData: TData);
begin
  // nothing by default
end;

constructor TMap<TKey, TData>.Create(AInternalSize: Integer);
begin
  inherited Create(AInternalSize);
  SetLength(FData, FInternalSize);
end;

destructor TMap<TKey, TData>.Destroy;
begin
  DelAll;
  inherited Destroy;
end;

function TMap<TKey, TData>.Get(AKey: TKey; out AData: TData): Boolean;
var
  Entry: THashEntry;
  Hash: Integer;
begin
  if CantIndex(AKey) then
    raise Exception.Create('Invalid HashTable-Index');

  Hash := GetKeyHash(AKey);
  if FData[Hash] = nil then // base entry doesn't exist > not found
    Exit(False);

  Entry := FData[Hash];
  while not KeysEqual(Entry.Key, AKey) do
  begin
    if Entry.Next = nil then // end reached > not found
      Exit(False);
    Entry := Entry.Next;
  end;
  // found
  AData := Entry.Data;
  Result := True;
end;

function TMap<TKey, TData>.HasKey(AKey: TKey): Boolean;
var
  _: TData;
begin
  Result := Get(AKey, _);
end;

procedure TMap<TKey, TData>.Del(AKey: TKey);
var
  Hash: Integer;
  Entry, PrevEntry: THashEntry;
begin
  Hash := GetKeyHash(AKey);
  Entry := FData[Hash];
  if Entry = nil then
    Exit; // already nil

  PrevEntry := nil;
  // find key in list
  while not KeysEqual(Entry.Key, AKey) do
  begin
    if Entry.Next = nil then // not found
      Exit;
    PrevEntry := Entry;
    Entry := Entry.Next;
  end;

  FreeKey(Entry.Key);
  FreeData(Entry.Data);

  if PrevEntry <> nil then
    PrevEntry.Next := Entry.Next
  else
    FData[Hash] := Entry.Next;

  Entry.Free;
  Dec(FCount);
end;

function TMap<TKey, TData>.NextKeyCheck(AKey: TKey; out AOut: TKey): Boolean;
var
  Hash: Integer;
  Current: THashEntry;
begin
  Hash := GetKeyHash(AKey);
  // Find Entry
  Current := FData[Hash];
  while not KeysEqual(Current.Key, AKey) do
  begin
    Current := Current.Next;
    if Current = nil then
      Exit(False);
  end;

  // Find Next
  if Current.Next <> nil then
  begin
    AOut := Current.Next.Key;
    Exit(True);
  end;
  repeat
    Inc(Hash);
  until (Hash = FInternalSize) or (FData[Hash] <> nil);
  if Hash = FInternalSize then
    Exit(False);
  AOut := FData[Hash].Key;
  Result := True;
end;

function TMap<TKey, TData>.PrevKeyCheck(AKey: TKey; out AOut: TKey): Boolean;
var
  Hash: Integer;
  Current: THashEntry;
begin
  Hash := GetKeyHash(AKey);
  // Find Entry
  if FData[Hash] = nil then
    Exit(False);
  Current := FData[Hash];
  while not KeysEqual(Current.Key, AKey) do
  begin
    if (Current.Next <> nil) and KeysEqual(Current.Next.Key, AKey) then
    begin
      AOut := Current.Key;
      Exit(True)
    end
    else if Current.Next = nil then
      Exit(False);
    Current := Current.Next;
  end;

  // Find Prev
  while Hash > 0 do
  begin
    Dec(Hash);
    if FData[Hash] <> nil then
    begin
      Current := FData[Hash];
      while Current.Next <> nil do
        Current := Current.Next;
      AOut := Current.Key;
      Exit(True);
    end;
  end;
  Result := False;
end;

function TMap<TKey, TData>.NextKeyCheck(var AKey: TKey): Boolean;
var
  Tmp: TKey;
begin
  Tmp := AKey;
  Result := NextKeyCheck(Tmp, AKey);
  if not Result then
    AKey := Tmp;
end;

function TMap<TKey, TData>.PrevKeyCheck(var AKey: TKey): Boolean;
var
  Tmp: TKey;
begin
  Tmp := AKey;
  Result := PrevKeyCheck(Tmp, AKey);
  if not Result then
    AKey := Tmp;
end;

function TMap<TKey, TData>.HasNextKey(AKey: TKey): Boolean;
var
  _: TKey;
begin
  Result := NextKeyCheck(AKey, _);
end;

function TMap<TKey, TData>.HasPrevKey(AKey: TKey): Boolean;
var
  _: TKey;
begin
  Result := PrevKeyCheck(AKey, _);
end;

function TMap<TKey, TData>.NextKey(AKey: TKey): TKey;
begin
  if not NextKeyCheck(AKey, Result) then
    raise Exception.Create('No next Key!');
end;

function TMap<TKey, TData>.PrevKey(AKey: TKey): TKey;
begin
  if not NextKeyCheck(AKey, Result) then
    raise Exception.Create('No previous Key!');
end;

function TMap<TKey, TData>.FirstKeyCheck(out AOut: TKey): Boolean;
var
  I: Integer;
begin
  if Count = 0 then
    Exit(False);
  Result := True;
  for I := 0 to FInternalSize - 1 do
    if FData[I] <> nil then
    begin
      AOut := FData[I].Key;
      Exit;
    end;
end;

function TMap<TKey, TData>.FirstKey: TKey;
begin
  if not FirstKeyCheck(Result) then
    raise Exception.Create('No first Key!');
end;

function TMap<TKey, TData>.LastKey: TKey;
begin
  if not LastKeyCheck(Result) then
    raise Exception.Create('No last Key!');
end;

function TMap<TKey, TData>.LastKeyCheck(out AOut: TKey): Boolean;
var
  I: Integer;
  Current: THashEntry;
begin
  if Count = 0 then
    Exit(False);
  Result := True;
  for I := FInternalSize - 1 downto 0 do
  begin
    Current := FData[I];
    if Current <> nil then
    begin
      while Current.Next <> nil do
        Current := Current.Next;
      AOut := Current.Key;
      Exit;
    end;
  end;
end;

function TMap<TKey, TData>.NextData(AKey: TKey): TData;
begin
  Result := Data[NextKey(AKey)];
end;

function TMap<TKey, TData>.PrevData(AKey: TKey): TData;
begin
  Result := Data[PrevKey(AKey)];
end;

procedure TMap<TKey, TData>.DelAll;
var
  Next: THashEntry;
  I: Integer;
begin
  for I := 0 to FInternalSize - 1 do
  begin
    while FData[I] <> nil do
    begin
      Next := FData[I].Next;
      FreeKey(FData[I].Key);
      FreeData(FData[I].Data);
      FData[I].Free;
      FData[I] := Next;
    end;
  end;
  FCount := 0;
end;

function TMap<TKey, TData>.GetEnumerator: TIterator;
begin
  Result := TIterator.Create(Self);
end;

{ THashTable<TKey, TData>.TIterator }

function TMap<TKey, TData>.TIterator.GetCurrent: TPair<TKey, TData>;
begin
  Result := TPair<TKey, TData>.Create(FEntry.Key, FEntry.Data);
end;

constructor TMap<TKey, TData>.TIterator.Create(AList: TMap<TKey, TData>);
begin
  FList := AList;
  FIndex := -1;
  FEntry := nil;
end;

function TMap<TKey, TData>.TIterator.MoveNext: Boolean;
begin
  if (FIndex = -1) or (FEntry.Next = nil) then
  begin
    // Move to next list
    repeat
      FIndex := FIndex + 1;
      if Integer(FIndex) = FList.FInternalSize then
        Exit(False);
      FEntry := FList.FData[FIndex];
    until (FEntry <> nil);
  end
  else
  begin
    FEntry := FEntry.Next;
  end;
  Result := True;
end;

{ TStringHashTable<TData> }

function TStringMap<TData>.GetKeyHash(AKey: string): Integer;
begin
  Result := GetHash(AKey, InternalSize);
end;

class function TStringMap<TData>.CantIndex(AKey: string): Boolean;
begin
  Result := AKey = '';
end;

class function TStringMap<TData>.KeysEqual(AKey1, AKey2: string): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TStringObjectHashTable<TData> }

procedure TStringObjectMap<TData>.FreeData(AData: TData);
begin
  AData.Free;
end;

{ THashBase<TKey> }

constructor THashBase<TKey>.Create(AInternalSize: Integer);
begin
  if AInternalSize = 0 then
    raise Exception.Create('Internal Size for HashTable must be at least 1');
  FInternalSize := AInternalSize;
end;

class function THashBase<TKey>.CantIndex(AKey: TKey): Boolean;
begin
  Result := False;
end;

class function THashBase<TKey>.CopyKey(AKey: TKey): TKey;
begin
  Result := AKey;
end;

{ TRefMap<TKey, TData> }

function TRefMap<TKey, TData>.GetActualKey(AKey: TKey; out AActualKey: TKey): Boolean;
var
  Entry: THashEntry;
  Hash: Integer;
begin
  if CantIndex(AKey) then
    raise Exception.Create('Invalid HashTable-Index');

  Hash := GetKeyHash(AKey);
  if FData[Hash] = nil then // base entry doesn't exist > not found
    Exit(False);

  Entry := FData[Hash];
  while not KeysEqual(Entry.Key, AKey) do
  begin
    if Entry.Next = nil then // end reached > not found
      Exit(False);
    Entry := Entry.Next;
  end;
  // found
  AActualKey := Entry.Key;
  Result := True;
end;

function TRefMap<TKey, TData>.GetKeyHash(AKey: TKey): Integer;
begin
  Result := GetHash(TObject(AKey), FInternalSize);
end;

function TRefMap<TKey, TData>.ActualKey(AKey: TKey): TKey;
begin
  if not GetActualKey(AKey, Result) then
    raise Exception.Create('HashTable-Key not found');
end;

class function TRefMap<TKey, TData>.CantIndex(AKey: TKey): Boolean;
begin
  Result := AKey = nil;
end;

class function TRefMap<TKey, TData>.KeysEqual(AKey1, AKey2: TKey): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TRefRefMap<TKey, TData> }

function TRefRefMap<TKey, TData>.GetOrNil(AKey: TKey): TData;
begin
  if not Get(AKey, Result) then
    Result := nil;
end;

{ TRefSet<T> }

function TRefSet<T>.GetKeyHash(AKey: T): Integer;
begin
  Result := GetHash(TObject(AKey), FInternalSize);
end;

class function TRefSet<T>.CantIndex(AKey: T): Boolean;
begin
  Result := AKey = nil;
end;

class function TRefSet<T>.KeysEqual(AKey1, AKey2: T): Boolean;
begin
  Result := Pointer(AKey1) = Pointer(AKey2);
end;

{ TObjectSet<T> }

procedure TObjectSet<T>.FreeData(const AData: T);
begin
  AData.Free;
end;

{ TArrayList<T> }

function TGenericArray<T>.GetItem(AIndex: Integer): T;
begin
  RangeCheckException(AIndex);
  Result := FItems[AIndex];
end;

procedure TGenericArray<T>.SetItem(AIndex: Integer; AValue: T);
begin
  RangeCheckException(AIndex);
  FItems[AIndex] := AValue;
end;

procedure TGenericArray<T>.Sort(AFunc: TCompareFunction<T>);
begin
  if Count > 1 then
    SortLR(AFunc, 0, Count - 1);
end;

procedure TGenericArray<T>.Sort(AFunc: TCompareFunctionOfObject<T>);
begin
  if Count > 1 then
    SortLR(AFunc, 0, Count - 1);
end;

procedure TGenericArray<T>.SortLR(ACompareFunc: TCompareFunctionOfObject<T>; ALeft, ARight: Integer);
var
  Pivot: T;
  L, R: Integer;
begin
  Pivot := FItems[(ALeft + ARight) div 2];
  L := ALeft;
  R := ARight;
  repeat
    while ACompareFunc(Pivot, FItems[L]) do
      Inc(L);
    while ACompareFunc(FItems[R], Pivot) do
      Dec(R);
    if L <= R then
    begin
      Swap(L, R);
      Inc(L);
      Dec(R);
    end;
  until L > R;
  if R > ALeft then
    SortLR(ACompareFunc, ALeft, R);
  if L < ARight then
    SortLR(ACompareFunc, L, ARight);
end;

procedure TGenericArray<T>.SortLR(ACompareFunc: TCompareFunction<T>; ALeft, ARight: Integer);
var
  Pivot: T;
  L, R: Integer;
begin
  Pivot := FItems[(ALeft + ARight) div 2];
  L := ALeft;
  R := ARight;
  repeat
    while ACompareFunc(Pivot, FItems[L]) do
      Inc(L);
    while ACompareFunc(FItems[R], Pivot) do
      Dec(R);
    if L <= R then
    begin
      Swap(L, R);
      Inc(L);
      Dec(R);
    end;
  until L > R;
  if R > ALeft then
    SortLR(ACompareFunc, ALeft, R);
  if L < ARight then
    SortLR(ACompareFunc, L, ARight);
end;

constructor TGenericArray<T>.Create(ASizeSteps: Integer);
begin
  FSizeSteps := ASizeSteps;
end;

function TGenericArray<T>.Add(AElement: T): T;
begin
  if Count + 1 > Length(FItems) then
    SetLength(FItems, Length(FItems) + FSizeSteps);
  FItems[FCount] := AElement;
  Inc(FCount);
  Result := AElement;
end;

function TGenericArray<T>.Insert(AElement: T; AIndex: Integer): T;
begin
  if Count + 1 > Length(FItems) then
    SetLength(FItems, Length(FItems) + FSizeSteps);
  Move(FItems[AIndex], FItems[AIndex + 1], SizeOf(T) * (Count - AIndex));
  FItems[AIndex] := AElement;
  Inc(FCount);
  Result := AElement;
end;

procedure TGenericArray<T>.DelLast;
begin
  DelAt(Count - 1);
end;

destructor TGenericArray<T>.Destroy;
begin
  DelAll;
  inherited;
end;

procedure TGenericArray<T>.DelAll;
begin
  FCount := 0;
  SetLength(FItems, 0);
end;

procedure TGenericArray<T>.DelAt(AIndex: Integer);
begin
  RangeCheckException(AIndex);
  if Count - AIndex > 1 then
    Move(FItems[AIndex + 1], FItems[AIndex], SizeOf(T) * (Count - AIndex - 1));
  Dec(FCount);
  if Length(FItems) - FSizeSteps >= FCount then
    SetLength(FItems, Length(FItems) - FSizeSteps);
end;

procedure TGenericArray<T>.Swap(A, B: Integer);
var
  Tmp: T;
begin
  RangeCheckException(A);
  RangeCheckException(B);
  Tmp := FItems[A];
  FItems[A] := FItems[B];
  FItems[B] := Tmp;
end;

function TGenericArray<T>.FindFirstIndex(AFunc: TFindFunctionStatic<T>): Integer;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if AFunc(FItems[I]) then
      Exit(I);
  Result := -1;
end;

function TGenericArray<T>.FindFirstIndex(AFunc: TFindFunctionOfObject<T>): Integer;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if AFunc(FItems[I]) then
      Exit(I);
  Result := -1;
end;

function TGenericArray<T>.FindFirstIndex(AFunc: TFindFunctionClass<T>; ADoFree: Boolean): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if AFunc.Find(FItems[I]) then
      Result := I;
  if ADoFree then
    AFunc.Free;
end;

function TGenericArray<T>.FindFirst(AFunc: TFindFunctionStatic<T>): T;
var
  I: Integer;
begin
  I := FindFirstIndex(AFunc);
  if I = -1 then
    raise EGenericArrayItemNotFound.Create;
  Result := FItems[I];
end;

function TGenericArray<T>.FindFirst(AFunc: TFindFunctionOfObject<T>): T;
var
  I: Integer;
begin
  I := FindFirstIndex(AFunc);
  if I = -1 then
    raise EGenericArrayItemNotFound.Create;
  Result := FItems[I];
end;

function TGenericArray<T>.FindFirst(AFunc: TFindFunctionClass<T>; ADoFree: Boolean): T;
var
  I: Integer;
begin
  I := FindFirstIndex(AFunc, ADoFree);
  if I = -1 then
    raise EGenericArrayItemNotFound.Create;
  Result := FItems[I];
end;

function TGenericArray<T>.FindLastIndex(AFunc: TFindFunctionStatic<T>): Integer;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if AFunc(FItems[I]) then
      Exit(I);
  Result := -1;
end;

function TGenericArray<T>.FindLastIndex(AFunc: TFindFunctionOfObject<T>): Integer;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if AFunc(FItems[I]) then
      Exit(I);
  Result := -1;
end;

function TGenericArray<T>.FindLastIndex(AFunc: TFindFunctionClass<T>; ADoFree: Boolean): Integer;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if AFunc.Find(FItems[I]) then
      Exit(I);
  Result := -1;
  if ADoFree then
    AFunc.Free;
end;

function TGenericArray<T>.FindLast(AFunc: TFindFunctionStatic<T>): T;
var
  I: Integer;
begin
  I := FindLastIndex(AFunc);
  if I = -1 then
    raise EGenericArrayItemNotFound.Create;
  Result := FItems[I];
end;

function TGenericArray<T>.FindLast(AFunc: TFindFunctionOfObject<T>): T;
var
  I: Integer;
begin
  I := FindLastIndex(AFunc);
  if I = -1 then
    raise EGenericArrayItemNotFound.Create;
  Result := FItems[I];
end;

function TGenericArray<T>.FindLast(AFunc: TFindFunctionClass<T>; ADoFree: Boolean): T;
var
  I: Integer;
begin
  I := FindLastIndex(AFunc, ADoFree);
  if I = -1 then
    raise EGenericArrayItemNotFound.Create;
  Result := FItems[I];
end;

function TGenericArray<T>.FindIndexAsArray(AFunc: TFindFunctionStatic<T>): TIntArray;
var
  I: Integer;
begin
  Result := TIntArray.Create;
  for I := 0 to Count - 1 do
    if AFunc(FItems[I]) then
      Result.Add(I);
end;

function TGenericArray<T>.FindIndexAsArray(AFunc: TFindFunctionOfObject<T>): TIntArray;
var
  I: Integer;
begin
  Result := TIntArray.Create;
  for I := 0 to Count - 1 do
    if AFunc(FItems[I]) then
      Result.Add(I);
end;

function TGenericArray<T>.FindIndexAsArray(AFunc: TFindFunctionClass<T>; ADoFree: Boolean): TIntArray;
var
  I: Integer;
begin
  Result := TIntArray.Create;
  for I := 0 to Count - 1 do
    if AFunc.Find(FItems[I]) then
      Result.Add(I);
  if ADoFree then
    AFunc.Free;
end;

function TGenericArray<T>.FindAsArray(AFunc: TFindFunctionStatic<T>): TGenericArray<T>;
var
  I: Integer;
begin
  Result := TGenericArray<T>.Create;
  for I := 0 to Count - 1 do
    if AFunc(FItems[I]) then
      Result.Add(FItems[I]);
end;

function TGenericArray<T>.FindAsArray(AFunc: TFindFunctionOfObject<T>): TGenericArray<T>;
var
  I: Integer;
begin
  Result := TGenericArray<T>.Create;
  for I := 0 to Count - 1 do
    if AFunc(FItems[I]) then
      Result.Add(FItems[I]);
end;

function TGenericArray<T>.FindAsArray(AFunc: TFindFunctionClass<T>; ADoFree: Boolean): TGenericArray<T>;
var
  I: Integer;
begin
  Result := TGenericArray<T>.Create;
  for I := 0 to Count - 1 do
    if AFunc.Find(FItems[I]) then
      Result.Add(FItems[I]);
  if ADoFree then
    AFunc.Free;
end;

function TGenericArray<T>.Copy: TGenericArray<T>;
var
  I: Integer;
begin
  Result := TGenericArray<T>.Create(FSizeSteps);
  for I := 0 to Count - 1 do
    Result.Add(FItems[I]);
end;

function TGenericArray<T>.Count: Integer;
begin
  Result := FCount;
end;

function TGenericArray<T>.Empty: Boolean;
begin
  Result := Count = 0;
end;

function TGenericArray<T>.First: T;
begin
  if Count = 0 then
    raise EGenericArrayEmpty.Create;
  Result := FItems[0];
end;

function TGenericArray<T>.Last: T;
begin
  if Count = 0 then
    raise EGenericArrayEmpty.Create;
  Result := FItems[Count - 1];
end;

function TGenericArray<T>.DataPointer: Pointer;
begin
  Result := FItems;
end;

function TGenericArray<T>.GetEnumerator: IIterator<T>;
begin
  Result := TIterator.Create(Self, False);
end;

function TGenericArray<T>.IterReversed: TReverseWrapper;
begin
  Result := TReverseWrapper.Create(Self);
end;

procedure TGenericArray<T>.RangeCheckException(AIndex: Integer);
begin
  if not RangeCheck(AIndex) then
    EGenericArrayRangeError.Create(AIndex, Count);
end;

function TGenericArray<T>.RangeCheck(AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < Count);
end;

procedure TGenericArray<T>.Append(AItems: IIterable<T>);
var
  AddCount: Integer;
  Item: T;
begin
  AddCount := AItems.Count;
  if Count + AddCount > Length(FItems) then
    SetLength(FItems, FSizeSteps * Ceil((Count + AddCount) / FSizeSteps));
  for Item in AItems do
  begin
    FItems[FCount] := Item;
    Inc(FCount);
  end;
end;

{ TClassMap }

function TClassMap<T>.GetKeyHash(AKey: TClass): Integer;
begin
  Result := GetHash(TObject(AKey), FInternalSize);
end;

class function TClassMap<T>.CantIndex(AKey: TClass): Boolean;
begin
  Result := AKey = nil;
end;

class function TClassMap<T>.KeysEqual(AKey1, AKey2: TClass): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TClassRefMap }

function TClassRefMap<T>.GetOrNil(AKey: TClass): T;
begin
  if not Get(AKey, Result) then
    Result := nil;
end;

{ TClassObjectMap }

procedure TClassObjectMap<T>.FreeData(AData: T);
begin
  AData.Free;
end;

{ TCardinalSet }

function TCardinalSet.GetKeyHash(AKey: Cardinal): Integer;
begin
  Result := Integer(AKey) mod FInternalSize;
end;

class function TCardinalSet.CantIndex(AKey: Cardinal): Boolean;
begin
  Result := False;
end;

class function TCardinalSet.KeysEqual(AKey1, AKey2: Cardinal): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TIntArray }

function TIntArray.Bounds: TIntBounds1;
begin
  Result := Range1(Min, Max);
end;

function TIntArray.Difference: Integer;
begin
  Result := Bounds.Length;
end;

function TIntArray.Max: Integer;
var
  I: Integer;
begin
  Result := Result.MinValue;
  for I in Self do
    Result := Math.Max(Result, I);
end;

function TIntArray.Min: Integer;
var
  I: Integer;
begin
  Result := Result.MaxValue;
  for I in Self do
    Result := Math.Min(Result, I);
end;

function TIntArray.Sum: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I in Self do
    Result := Result + I;
end;

function TIntArray.ToString: string;
var
  I: Integer;
begin
  if Count = 0 then
    Exit('Empty');
  Result := IntToStr(FItems[0]);
  for I := 1 to Count - 1 do
    Result := Result + ', ' + IntToStr(FItems[I]);
end;

{ TTags }

function TTags.GetKeyHash(AKey: string): Integer;
begin
  Result := GetHash(AKey, FInternalSize);
end;

class function TTags.CantIndex(AKey: string): Boolean;
begin
  Result := AKey = '';
end;

class function TTags.KeysEqual(AKey1, AKey2: string): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TSet<T>.TIterator }

function TSet<T>.TIterator.GetCurrent: T;
begin
  Result := FEntry.Data;
end;

constructor TSet<T>.TIterator.Create(AList: TSet<T>);
begin
  FList := AList;
  FIndex := -1;
  FEntry := nil;
end;

function TSet<T>.TIterator.MoveNext: Boolean;
begin
  if (FIndex = -1) or (FEntry.Next = nil) then
  begin
    // Move to next list
    repeat
      FIndex := FIndex + 1;
      if FIndex = FList.FInternalSize then
        Exit(False);
      FEntry := FList.FTags[FIndex];
    until (FEntry <> nil);
  end
  else
  begin
    FEntry := FEntry.Next;
  end;
  Result := True;
end;

{ TSet<T> }

function TSet<T>.GetElement(S: T): Boolean;
var
  Entry: TEntry;
begin
  if CantIndex(S) then
    raise Exception.Create('Invalid Set-Index');

  Entry := FTags[GetKeyHash(S)];
  while Entry <> nil do
  begin
    if KeysEqual(Entry.Data, S) then
      Exit(True);
    Entry := Entry.Next;
  end;
  Result := False;
end;

procedure TSet<T>.SetElement(S: T; AValue: Boolean);
var
  Hash: Integer;
  Entry, EntryToDelete: TEntry;
begin
  if CantIndex(S) then
    raise Exception.Create('Invalid Set-Index');

  Hash := GetKeyHash(S);
  if FTags[Hash] = nil then
  begin
    if AValue then
    begin
      // create new base entry
      FTags[Hash] := TEntry.Create;
      FTags[Hash].Data := S;
      Inc(FCount);
    end;
    // else doesn't exist in the first place
  end
  else
  begin
    // first
    if KeysEqual(FTags[Hash].Data, S) then
    begin
      if not AValue then
      begin
        // delete first
        Entry := FTags[Hash].Next;
        FreeData(FTags[Hash].Data);
        FTags[Hash].Free;
        FTags[Hash] := Entry;
        Dec(FCount);
      end;
      Exit;
    end;
    // rest
    Entry := FTags[Hash];
    while Entry.Next <> nil do
    begin
      if KeysEqual(Entry.Next.Data, S) then
      begin
        if not AValue then
        begin
          // delete in rest
          EntryToDelete := Entry.Next;
          Entry.Next := Entry.Next.Next;
          FreeData(EntryToDelete.Data);
          EntryToDelete.Free;
          Dec(FCount);
        end;
        // else exists already
        Exit;
      end;
      Entry := Entry.Next;
    end;
    // not found
    if AValue then
    begin
      // add
      Entry.Next := TEntry.Create;
      Entry.Next.Data := S;
      Inc(FCount);
    end;
    // else doesn't exist in the first place
  end;
end;

procedure TSet<T>.FreeData(const AData: T);
begin
  // might not do anything depending on generic Data Type
end;

constructor TSet<T>.Create(AInternalSize: Integer);
begin
  inherited Create(AInternalSize);
  SetLength(FTags, FInternalSize);
end;

destructor TSet<T>.Destroy;
begin
  Clear;
  inherited;
end;

procedure TSet<T>.Add(S: T);
begin
  SetElement(S, True);
end;

procedure TSet<T>.Del(S: T);
begin
  SetElement(S, False);
end;

procedure TSet<T>.Clear;
var
  I: Integer;
  Next: TEntry;
begin
  for I := 0 to FInternalSize - 1 do
  begin
    if FCount = 0 then
      Exit;
    while FTags[I] <> nil do
    begin
      Next := FTags[I].Next;
      FTags[I].Free;
      FTags[I] := Next;
      Dec(FCount);
    end;
  end;
end;

procedure TSet<T>.Assign(ATagList: TSet<T>);
var
  S: T;
begin
  Clear;
  for S in ATagList do
    Self[S] := True;
end;

function TSet<T>.GetEnumerator: TIterator;
begin
  Result := TIterator.Create(Self);
end;

{ TObjectStack }

constructor TObjectStack<T>.Create(AReferenceList: Boolean);
begin
  FReferenceList := AReferenceList;
end;

destructor TObjectStack<T>.Destroy;
begin
  while Pop do;
  inherited Destroy;
end;

function TObjectStack<T>.Push(AElement: T): T;
begin
  FTop := TItem.Create(AElement, FTop);
  Result := FTop.Data;
end;

function TObjectStack<T>.Pop: Boolean;
var
  Old: TItem;
begin
  if FTop = nil then
    raise Exception.Create('Cannot pop empty stack');
  Old := FTop;
  if not FReferenceList then
    FTop.Data.Free;
  FTop := FTop.Prev;
  Result := FTop <> nil;
  Old.Free;
end;

function TObjectStack<T>.Top: T;
begin
  Result := FTop.Data;
end;

function TObjectStack<T>.Copy: TObjectStack<T>;
var
  A, B: TItem;
begin
  Result := TObjectStack<T>.Create(True);
  if FTop = nil then
    Exit;
  A := TItem.Create(FTop.Data, nil);
  Result.FTop := A;
  B := FTop;
  while B.Prev <> nil do
  begin
    B := B.Prev;
    A.Prev := TItem.Create(B.Data, nil);
    A := A.Prev;
  end;
end;

{ TObjectArray }

function TRefArray<T>.CopyAsRefArray: TRefArray<T>;
var
  I: Integer;
begin
  Result := TRefArray<T>.Create;
  for I := 0 to Count - 1 do
    Result.Add(FItems[I]);
end;

function TRefArray<T>.FindAsRefArray(AFunc: TFindFunctionStatic<T>): TRefArray<T>;
var
  I: Integer;
begin
  Result := TRefArray<T>.Create;
  for I := 0 to Count - 1 do
    if AFunc(FItems[I]) then
      Result.Add(FItems[I]);
end;

function TRefArray<T>.FindAsRefArray(AFunc: TFindFunctionOfObject<T>): TRefArray<T>;
var
  I: Integer;
begin
  Result := TRefArray<T>.Create;
  for I := 0 to Count - 1 do
    if AFunc(FItems[I]) then
      Result.Add(FItems[I]);
end;

function TRefArray<T>.FindAsRefArray(AFunc: TFindFunctionClass<T>; ADoFree: Boolean = True): TRefArray<T>;
var
  I: Integer;
begin
  Result := TRefArray<T>.Create;
  for I := 0 to Count - 1 do
    if AFunc.Find(FItems[I]) then
      Result.Add(FItems[I]);
  if ADoFree then
    AFunc.Free;
end;

function TRefArray<T>.Find(AData: T): Integer;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Pointer(AData) = Pointer(FItems[I]) then
      Exit(I);
  Result := -1;
end;

procedure TRefArray<T>.Del(AData: T);
begin
  DelAt(Find(AData));
end;

function TRefArray<T>.Copy: TGenericArray<T>;
begin
  Result := CopyAsRefArray;
end;

function TRefArray<T>.ToString: string;
var
  I: Integer;
begin
  if Count = 0 then
    Exit('Empty');
  Result := T(FItems[0]).ToString; // casts only necessary for delphi quick-syntaxcheck
  for I := 1 to Count - 1 do
    Result := Result + ', ' + T(FItems[I]).ToString;
end;

{ TAnsiStringObjectMap<TData> }

procedure TAnsiStringObjectMap<TData>.FreeData(AData: TData);
begin
  AData.Free;
end;

{ TObjectMap<TKey, TData> }

procedure TObjectMap<TKey, TData>.FreeKey(AKey: TKey);
begin
  AKey.Free;
end;

{ TAnsiStringMap<TData> }

class function TAnsiStringMap<TData>.CantIndex(AKey: AnsiString): Boolean;
begin
  Result := AKey = '';
end;

function TAnsiStringMap<TData>.GetKeyHash(AKey: AnsiString): Integer;
begin
  Result := GetHash(AKey, FInternalSize);
end;

class function TAnsiStringMap<TData>.KeysEqual(AKey1, AKey2: AnsiString): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TArrayList<T>.TIterator }

constructor TGenericArray<T>.TIterator.Create(AList: TGenericArray<T>; AReversed: Boolean);
begin
  FList := AList;
  FReversed := AReversed;
  if FReversed then
    FCurrent := FList.Count
  else
    FCurrent := -1;
end;

function TGenericArray<T>.TIterator.GetCurrent: T;
begin
  Result := FList[FCurrent];
end;

function TGenericArray<T>.TIterator.MoveNext: Boolean;
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

procedure TGenericArray<T>.TIterator.RemoveCurrent;
begin
  FRemoveFlag := True;
end;

{ EEmptyGenericArray }

constructor EGenericArrayEmpty.Create;
begin
  inherited Create('The GenericArray does not have a first/last Element as it is empty');
end;

{ EGenericArrayRangeError }

constructor EGenericArrayRangeError.Create(AIndex, ACount: Integer);
begin
  inherited CreateFmt('GenericArray Index %d out of bounds [0 - %d]', [AIndex, ACount - 1]);
end;

{ EGenericArrayItemNotFound }

constructor EGenericArrayItemNotFound.Create;
begin
  inherited Create('Could not find the Element in the GenericArray');
end;

{ TGenericArray<T>.TReverseWrapper }

constructor TGenericArray<T>.TReverseWrapper.Create(AGenericArray: TGenericArray<T>);
begin
  FGenericArray := AGenericArray;
end;

function TGenericArray<T>.TReverseWrapper.GetEnumerator(AAutoFree: Boolean): TIterator;
begin
  Result := TGenericArray<T>.TIterator.Create(FGenericArray, True);
end;

{ TObjectArray<T> }

procedure TObjectArray<T>.DelAll;
var
  Item: T;
begin
  for Item in Self do
    Item.Free;
  inherited;
end;

procedure TObjectArray<T>.DelAt(AIndex: Integer);
begin
  Items[AIndex].Free;
  inherited;
end;

{ TInterfaceArray<T> }

function TInterfaceArray<T>.Copy: TGenericArray<T>;
begin
  Result := CopyAsInterfaceArray;
end;

function TInterfaceArray<T>.CopyAsInterfaceArray: TInterfaceArray<T>;
var
  Item: T;
begin
  Result := TInterfaceArray<T>.Create;
  for Item in Self do
    Result.Add(Item);
end;

procedure TInterfaceArray<T>.Del(AData: T);
begin
  DelAt(Find(AData));
end;

function TInterfaceArray<T>.Find(AData: T): Integer;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if IInterface(Items[I]) = IInterface(AData) then
      Exit(I);
  Result := -1;
end;

{ TGenericArrayReader<T> }

function TGenericArrayReader<T>.Copy: TGenericArray<T>;
begin
  Result := FGenericArray.Copy;  
end;

constructor TGenericArrayReader<T>.Create(AGenericArray: TGenericArray<T>);
begin
  FGenericArray := AGenericArray;
end;

function TGenericArrayReader<T>.Empty: Boolean;
begin
  Result := FGenericArray.Empty;
end;

function TGenericArrayReader<T>.FindAsArray(AFunc: TFindFunctionStatic<T>): TGenericArray<T>;
begin
  Result := FGenericArray.FindAsArray(AFunc);
end;

function TGenericArrayReader<T>.FindAsArray(AFunc: TFindFunctionClass<T>; ADoFree: Boolean): TGenericArray<T>;
begin
  Result := FGenericArray.FindAsArray(AFunc, ADoFree);
end;

function TGenericArrayReader<T>.FindAsArray(AFunc: TFindFunctionOfObject<T>): TGenericArray<T>;
begin
  Result := FGenericArray.FindAsArray(AFunc);
end;

function TGenericArrayReader<T>.FindFirst(AFunc: TFindFunctionOfObject<T>): T;
begin                                        
  Result := FGenericArray.FindFirst(AFunc);
end;

function TGenericArrayReader<T>.FindFirst(AFunc: TFindFunctionClass<T>; ADoFree: Boolean): T;
begin                              
  Result := FGenericArray.FindFirst(AFunc, ADoFree);
end;

function TGenericArrayReader<T>.FindFirst(AFunc: TFindFunctionStatic<T>): T;
begin                               
  Result := FGenericArray.FindFirst(AFunc);
end;

function TGenericArrayReader<T>.FindFirstIndex(AFunc: TFindFunctionOfObject<T>): Integer;
begin                               
  Result := FGenericArray.FindFirstIndex(AFunc);
end;

function TGenericArrayReader<T>.FindFirstIndex(AFunc: TFindFunctionClass<T>; ADoFree: Boolean): Integer;
begin                                     
  Result := FGenericArray.FindFirstIndex(AFunc, ADoFree);
end;

function TGenericArrayReader<T>.FindFirstIndex(AFunc: TFindFunctionStatic<T>): Integer;
begin
  Result := FGenericArray.FindFirstIndex(AFunc);
end;

function TGenericArrayReader<T>.FindIndexAsArray(AFunc: TFindFunctionClass<T>; ADoFree: Boolean): TIntArray;
begin                                           
  Result := FGenericArray.FindIndexAsArray(AFunc, True);
end;

function TGenericArrayReader<T>.FindIndexAsArray(AFunc: TFindFunctionOfObject<T>): TIntArray;
begin                                             
  Result := FGenericArray.FindIndexAsArray(AFunc);
end;

function TGenericArrayReader<T>.FindIndexAsArray(AFunc: TFindFunctionStatic<T>): TIntArray;
begin                                              
  Result := FGenericArray.FindIndexAsArray(AFunc);
end;

function TGenericArrayReader<T>.FindLast(AFunc: TFindFunctionClass<T>; ADoFree: Boolean): T;
begin                                  
  Result := FGenericArray.FindLast(AFunc, ADoFree);
end;

function TGenericArrayReader<T>.FindLast(AFunc: TFindFunctionOfObject<T>): T;
begin                                         
  Result := FGenericArray.FindLast(AFunc);
end;

function TGenericArrayReader<T>.FindLast(AFunc: TFindFunctionStatic<T>): T;
begin                             
  Result := FGenericArray.FindLast(AFunc);
end;

function TGenericArrayReader<T>.FindLastIndex(AFunc: TFindFunctionOfObject<T>): Integer;
begin                                                
  Result := FGenericArray.FindLastIndex(AFunc);
end;

function TGenericArrayReader<T>.FindLastIndex(AFunc: TFindFunctionClass<T>; ADoFree: Boolean): Integer;
begin                                                
  Result := FGenericArray.FindLastIndex(AFunc, ADoFree);
end;

function TGenericArrayReader<T>.FindLastIndex(AFunc: TFindFunctionStatic<T>): Integer;
begin                                                       
  Result := FGenericArray.FindLastIndex(AFunc);
end;

function TGenericArrayReader<T>.First: T;
begin                              
  Result := FGenericArray.First;
end;

function TGenericArrayReader<T>.Count: Integer;
begin
  Result := FGenericArray.Count;
end;

function TGenericArrayReader<T>.GetItem(I: Integer): T;
begin
  Result := FGenericArray.Items[I];
end;

function TGenericArrayReader<T>.GetEnumerator: IIterator<T>;
begin
  Result := FGenericArray.GetEnumerator;
end;

function TGenericArrayReader<T>.IterReversed: TGenericArray<T>.TReverseWrapper;
begin
  Result := FGenericArray.IterReversed;
end;

function TGenericArrayReader<T>.Last: T;
begin
  Result := FGenericArray.Last;
end;

function TGenericArrayReader<T>.RangeCheck(AIndex: Integer): Boolean;
begin
  Result := FGenericArray.RangeCheck(AIndex);
end;

procedure TGenericArrayReader<T>.RangeCheckException(AIndex: Integer);
begin                                        
  FGenericArray.RangeCheckException(AIndex);
end;

function TGenericArrayReader<T>.ToString: string;
begin
  Result := FGenericArray.ToString;
end;

{ TIntArrayReader }

function TIntArrayReader.Bounds: TIntBounds1;
begin
  Result := TIntArray(FGenericArray).Bounds;
end;

constructor TIntArrayReader.Create(AIntArray: TIntArray);
begin
  inherited Create(AIntArray);
end;

function TIntArrayReader.Difference: Integer;
begin
  Result := TIntArray(FGenericArray).Difference;
end;

function TIntArrayReader.Max: Integer;
begin                                           
  Result := TIntArray(FGenericArray).Max;
end;

function TIntArrayReader.Min: Integer;
begin                                   
  Result := TIntArray(FGenericArray).Min;
end;

function TIntArrayReader.Sum: Integer;
begin                                   
  Result := TIntArray(FGenericArray).Sum;
end;

{ TRefArrayReader<T> }

function TRefArrayReader<T>.CopyAsRefArray: TRefArray<T>;
begin
  Result := TRefArray<T>(FGenericArray).CopyAsRefArray;
end;

constructor TRefArrayReader<T>.Create(ARefArray: TRefArray<T>);
begin
  inherited Create(ARefArray);
end;

function TRefArrayReader<T>.Find(AData: T): Integer;
begin
  Result := TRefArray<T>(FGenericArray).Find(AData);
end;

function TRefArrayReader<T>.FindAsRefArray(AFunc: TFindFunctionClass<T>; ADoFree: Boolean): TRefArray<T>;
begin                                    
  Result := TRefArray<T>(FGenericArray).FindAsRefArray(AFunc, ADoFree);
end;

function TRefArrayReader<T>.FindAsRefArray(AFunc: TFindFunctionOfObject<T>): TRefArray<T>;
begin                                                                  
  Result := TRefArray<T>(FGenericArray).FindAsRefArray(AFunc);
end;

function TRefArrayReader<T>.FindAsRefArray(AFunc: TFindFunctionStatic<T>): TRefArray<T>;
begin                                                       
  Result := TRefArray<T>(FGenericArray).FindAsRefArray(AFunc);
end;

{ TInterfaceArrayReader<T> }

function TInterfaceArrayReader<T>.CopyAsInterfaceArray: TInterfaceArray<T>;
begin                    
  Result := TInterfaceArray<T>(FGenericArray).CopyAsInterfaceArray;
end;

constructor TInterfaceArrayReader<T>.Create(AInterfaceArray: TInterfaceArray<T>);
begin
  inherited Create(AInterfaceArray);
end;

function TInterfaceArrayReader<T>.Find(AData: T): Integer;
begin                                                                    
  Result := TInterfaceArray<T>(FGenericArray).Find(AData);
end;

{ TRefObjectMap<TKey, TData> }

procedure TRefObjectMap<TKey, TData>.FreeData(AData: TData);
begin
  AData.Free;
end;

{ TObjectRefMap<TKey, TData> }

function TObjectRefMap<TKey, TData>.GetOrNil(AKey: TKey): TData;
begin
  if not Get(AKey, Result) then
    Result := nil;
end;

{ TObjectObjectMap<TKey, TData> }

procedure TObjectObjectMap<TKey, TData>.FreeData(AData: TData);
begin
  AData.Free;
end;

end.
