unit Lists;

// TODO: GENERIC FTW FUCK THE CODETOOLS!
//       try to derive from TObjectList and and make the TObject be IDoublyLinked

interface

uses
  Classes, SysUtils, VectorGeometry, BasicTypeClasses;

type

  TWithEachProcedureBasic = procedure (AElement: TObject);
  TWithEachProcedureObject = procedure (AElement: TObject) of object;

  TCompareFunction = function(A, B: TObject): Boolean;

  TWithEachProcedure = class abstract
  protected
    procedure WithEach(AElement: TObject); virtual; abstract;
  end;

  TFindFunction = class abstract
  protected
    function Find(AElement: TObject): Boolean; virtual; abstract;
  end;

  { TObjectList }

  TObjectList = class
  private
    type
      TItem = class
      public
        Data: TObject;
        Next, Prev: TItem;
      end;

  private
    FFirst: TItem;
    FLast: TItem;
    FReferenceList: Boolean;
    FCount: Integer;

    FIterReversed: Boolean;

    function Find(AElement: TObject): TItem;
    procedure Del(AItem: TItem); overload;
    function GetItem(I: Integer): TObject;

    procedure Sort(ACompare: TCompareFunction; ALeft, ARight: TItem; ACount: Integer); overload;
  public
    type

      { TIterator }

      TIterator = class
      private
        FList: TObjectList;

        FCurrent: TItem;
        FNext: TItem;
        FReversed: Boolean;
        FAutoFree: Boolean;

        FRemoveFlag: Boolean;

      public
        constructor Create(AList: TObjectList; AReversed, AAutoFree: Boolean);

        function MoveNext: Boolean;
        property Current: TObject read FCurrent.Data;

        procedure RemoveCurrent;
      end;

  public
    constructor Create(AReferenceList: Boolean = False);
    destructor Destroy; override;

    function Add(AElement: TObject): TObject;
    procedure Del(AElement: TObject); overload;
    procedure DelAll;

    procedure WithEach(AFunc: TWithEachProcedure; AFreeFunction: Boolean = True); overload;
    procedure WithEach(AFunc: TWithEachProcedureBasic); overload;
    procedure WithEach(AFunc: TWithEachProcedureObject); overload;

    function FindFirst(AFunc: TFindFunction; AFreeFunction: Boolean = True): TObject;
    function FindLast(AFunc: TFindFunction; AFreeFunction: Boolean = True): TObject;
    function FindAll(AFunc: TFindFunction; AFreeFunction: Boolean = True): TObjectList;

    function Exists(AElement: TObject): Boolean;

    procedure Sort(ACompareFunc: TCompareFunction); overload;

    function Copy: TObjectList;

    function GetEnumerator(AAutoFree: Boolean = False): TIterator;
    function IterReversed: TObjectList;

    property First: TObject read FFirst.Data;
    property Last: TObject read FLast.Data;

    property Item[I: Integer]: TObject read GetItem; default;

    property Count: Integer read FCount;
  end;

  { TObjectArray }

  TObjectArray = class
  private
    FItems: array of TObject;
    FReferenceList: Boolean;
    FIterReversed: Boolean;

    function GetCount: Integer;

    procedure RangeCheck(AIndex: Integer);

    procedure Sort(ACompareFunc: TCompareFunction; ALeft, ARight: Integer); overload;

  public
    type

      { TIterator }

      TIterator = class
      private
        FList: TObjectArray;

        FCurrent: Integer;
        FReversed: Boolean;
        FAutoFree: Boolean;

        FRemoveFlag: Boolean;

        function GetCurrent: TObject;
      public
        constructor Create(AList: TObjectArray; AReversed, AAutoFree: Boolean);

        function MoveNext: Boolean;
        property Current: TObject read GetCurrent;

        procedure RemoveCurrent;
      end;

  protected
    function GetItem(I: Integer): TObject;
    procedure SetItem(I: Integer; AValue: TObject);
  public
    constructor Create(AReferenceList: Boolean = False);
    destructor Destroy; override;

    function Add(AElement: TObject): TObject;
    procedure DelLast;
    procedure DelAll;

    procedure Insert(AElement: TObject; AIndex: Integer);
    procedure Del(AElement: TObject); overload;
    procedure Del(AIndex: Integer); overload;

    function Swap(I, J: Integer): Boolean;

    procedure WithEach(AFunc: TWithEachProcedure; AFreeFunction: Boolean = True); overload;
    procedure WithEach(AFunc: TWithEachProcedureBasic); overload;
    function FindFirstIndex(AFunc: TFindFunction; AFreeFunction: Boolean = True): Integer;
    function FindLastIndex(AFunc: TFindFunction; AFreeFunction: Boolean = True): Integer;

    function FindFirst(AFunc: TFindFunction; AFreeFunction: Boolean = True): TObject;
    function FindLast(AFunc: TFindFunction; AFreeFunction: Boolean = True): TObject;

    function FindAsList(AFunc: TFindFunction; AFreeFunction: Boolean = True): TObjectList;
    function FindAsArray(AFunc: TFindFunction; AFreeFunction: Boolean = True): TObjectArray;

    function Copy: TObjectArray;

    procedure Sort(ACompareFunc: TCompareFunction); overload;

    function Find(AElement: TObject): Integer;

    property Count: Integer read GetCount;
    property Item[I: Integer]: TObject read GetItem write SetItem; default;

    function First: TObject;
    function Last: TObject;

    function GetEnumerator(AAutoFree: Boolean = False): TIterator;
    function IterReversed: TObjectArray;
  end;

  { TObjectStack }

  TObjectStack = class
  private
    FItems: array of TObject;
    FCount: Integer;
    FReferenceList: Boolean;
    function GetCount: Integer;
  public
    constructor Create(AReferenceList: Boolean = False);
    destructor Destroy; override;

    function Push(AElement: TObject): TObject;
    function Pop: TObject;
    function Top: TObject;

    function Copy: TObjectStack;
  end;

  { TObjectHashTable }

  TObjectHashTable = class
  private
    type

      { THashEntry }

      THashEntry = class
      public
        Key, Data: TObject;
        Next: THashEntry;
      end;

  private
    FData: array of THashEntry;
    FReferenceList: Boolean;
    FInternalSize: Cardinal;
    FCount: Cardinal;

    function GetEntry(Key: TObject): TObject;
    procedure SetEntry(Key: TObject; AValue: TObject);

  public
    constructor Create(AReferenceList: Boolean = False; AInternalSize: Cardinal = 256);
    destructor Destroy; override;

    property Data[Key: TObject]: TObject read GetEntry write SetEntry; default;
  end;

  { TStringHashTable }

  TStringHashTable = class
  private
    type

      { THashEntry }

      THashEntry = class
      public
        Key: String;
        Data: TObject;
        Next: THashEntry;
      end;

  public
    type

    { TPair }

      TPair = record
        Key: String;
        Data: TObject;
      end;

    { TIterator }

      TIterator = class
      private
        FList: TStringHashTable;
        FIndex: Integer;
        FEntry: THashEntry;
        function GetCurrent: TPair;
      public
        constructor Create(AList: TStringHashTable);

        function MoveNext: Boolean;
        property Current: TPair read GetCurrent;
      end;

  private
    FData: array of THashEntry;
    FReferenceList: Boolean;
    FInternalSize: Cardinal;
    FCount: Cardinal;

    function GetNext(Key: String): TObject;
    function GetPrev(Key: String): TObject;

  protected
    function GetEntry(Key: String): TObject;
    procedure SetEntry(Key: String; AValue: TObject);

  public
    constructor Create(AReferenceList: Boolean = False; AInternalSize: Cardinal = 256);
    destructor Destroy; override;

    property Data[Key: String]: TObject read GetEntry write SetEntry; default;

    function NextKey(Key: String): String;
    function PrevKey(Key: String): String;

    function FirstKey: String;
    function LastKey: String;

    function NextData(Key: String): TObject;
    function PrevData(Key: String): TObject;

    procedure DelAll;

    function GetEnumerator: TIterator;
    property Count: Cardinal read FCount;
  end;

  { TIntVectorHashTable }

  TIntVectorHashTable = class
  private
    type

      { THashEntry }

      THashEntry = class
      public
        Key: TIntVector;
        Data: TObject;
        Next: THashEntry;
      end;

  private
    FData: array of THashEntry;
    FReferenceList: Boolean;
    FInternalSize: Cardinal;
    FCount: Cardinal;

    function GetEntry(Key: TIntVector): TObject;
    procedure SetEntry(Key: TIntVector; AValue: TObject);

  public
    constructor Create(AReferenceList: Boolean = False; AInternalSize: Cardinal = 256);
    destructor Destroy; override;

    property Data[Key: TIntVector]: TObject read GetEntry write SetEntry; default;

    property Count: Cardinal read FCount;
  end;

  { TStringArray }

  TStringArray = class (TObjectArray)
  private
    function GetString(I: Integer): String;
  protected
    procedure SetString(I: Integer; AValue: String); virtual;
  public
    property Strings[I: Integer]: String read GetString write SetString; default;

    procedure Add(AString: String);
  end;

  { TNotifyStringArray }

  TNotifyStringArray = class (TStringArray)
  private
    FChanged: Boolean;
  protected
    procedure SetString(I: Integer; AValue: String); override;
  public
    procedure NotifyChanges;
    property Changed: Boolean read FChanged;
  end;

  { TTagList }
  // using hashed Strings
  TTagList = class
  private
    type

      { TEntry }

      TEntry = class
      public
        Data: String;
        Next: TEntry;
      end;

      { TIterator }

      TIterator = class
      private
        FList: TTagList;
        FIndex: Integer;
        FEntry: TEntry;
        function GetCurrent: String;
      public
        constructor Create(AList: TTagList);

        function MoveNext: Boolean;
        property Current: String read GetCurrent;
      end;

  private
    FTags: array of TEntry;
    FCount: Cardinal;
    FInternalSize: Cardinal;
    function GetTag(S: String): Boolean;
    procedure SetTag(S: String; AValue: Boolean);
  public
    constructor Create(AInternalSize: Cardinal = 256);
    destructor Destroy; override;

    property Tag[S: String]: Boolean read GetTag write SetTag; default;
    property Count: Cardinal read FCount;

    procedure Clear;

    procedure Assign(ATagList: TTagList);

    function GetEnumerator: TIterator;
  end;

function GetHash(AObject: TObject; ARange: Cardinal): Cardinal; overload; inline;
function GetHash(AString: String; ARange: Cardinal): Cardinal; overload; inline;
function GetHash(AIntVector: TIntVector; ARange: Cardinal): Cardinal; overload; inline;

implementation

function GetHash(AObject: TObject; ARange: Cardinal): Cardinal;
var
  I: UInt64;
begin
  I := UInt64(Pointer(AObject));
  Result := (I xor Cardinal(I shl 3) xor (I shr 7)) mod ARange;
end;

function GetHash(AString: String; ARange: Cardinal): Cardinal;
var
  C: Char;
begin
  Result := 0;
  for C in AString do
    Result := Cardinal((Result + Ord(C)) xor Ord(C) * Ord(C));
  Result := Result mod ARange;
end;

function GetHash(AIntVector: TIntVector; ARange: Cardinal): Cardinal;
var
  X, Y, Z: PCardinal;
begin
  AIntVector.X := Integer(AIntVector.X * 53);
  AIntVector.Y := Integer(AIntVector.Y * 97);
  AIntVector.Z := Integer(AIntVector.Z * 193);
  X := @AIntVector.X;
  Y := @AIntVector.Y;
  Z := @AIntVector.Z;
  Result := (X^ xor Y^ xor Z^) mod ARange;
end;

{ TTagList.TIterator }

function TTagList.TIterator.GetCurrent: String;
begin
  Result := FEntry.Data;
end;

constructor TTagList.TIterator.Create(AList: TTagList);
begin
  FList := AList;
  FIndex := -1;
  FEntry := nil;
end;

function TTagList.TIterator.MoveNext: Boolean;
begin
  if (FIndex = -1) or (FEntry.Next = nil) then
  begin
    // Move to next list
    repeat
      FIndex := FIndex + 1;
      if Int64(FIndex) = FList.FInternalSize then
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

{ TTagList }

function TTagList.GetTag(S: String): Boolean;
var
  Entry: TEntry;
begin
  Entry := FTags[GetHash(S, FInternalSize)];
  while Entry <> nil do
  begin
    if Entry.Data = S then
      Exit(True);
    Entry := Entry.Next;
  end;
  Result := False;
end;

procedure TTagList.SetTag(S: String; AValue: Boolean);
var
  Hash: Cardinal;
  Entry, EntryToDelete: TEntry;
begin
  Hash := GetHash(S, FInternalSize);
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
    if FTags[Hash].Data = S then
    begin
      if not AValue then
      begin
        // delete first
        Entry := FTags[Hash].Next;
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
      if Entry.Next.Data = S then
      begin
        if not AValue then
        begin
          // delete in rest
          EntryToDelete := Entry.Next;
          Entry.Next := Entry.Next.Next;
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

constructor TTagList.Create(AInternalSize: Cardinal);
begin
  SetLength(FTags, AInternalSize);
  FInternalSize := AInternalSize;
end;

destructor TTagList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TTagList.Clear;
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

procedure TTagList.Assign(ATagList: TTagList);
var
  S: String;
begin
  Clear;
  for S in ATagList do
    Self[S] := True;
end;

function TTagList.GetEnumerator: TIterator;
begin
  Result := TIterator.Create(Self);
end;

{ TNotifyStringArray }

procedure TNotifyStringArray.SetString(I: Integer; AValue: String);
begin
  inherited SetString(I, AValue);
  FChanged := True;
end;

procedure TNotifyStringArray.NotifyChanges;
begin
  FChanged := False;
end;

{ TStringArray }

function TStringArray.GetString(I: Integer): String;
var
  S: TString;
begin
  S := GetItem(I) as TString;
  if S = nil then
    Result := EmptyStr
  else
    Result := S.Text;
end;

procedure TStringArray.SetString(I: Integer; AValue: String);
begin
  if AValue = EmptyStr then
    SetItem(I, nil)
  else
    SetItem(I, TString.Create(AValue));
end;

procedure TStringArray.Add(AString: String);
begin
  inherited Add(TString.Create(AString));
end;

{ TIntVectorHashTable }

function TIntVectorHashTable.GetEntry(Key: TIntVector): TObject;
var
  Entry: THashEntry;
  Hash: Cardinal;
begin
  Hash := GetHash(Key, FInternalSize);
  if FData[Hash] = nil then // base entry doesn't exist yet > not found
    Exit(nil);

  Entry := FData[Hash];
  while Entry.Key <> Key do
  begin
    if Entry.Next = nil then // end reached > not found
      Exit(nil);
    Entry := Entry.Next;
  end;
  // found
  Result := Entry.Data;
end;

procedure TIntVectorHashTable.SetEntry(Key: TIntVector; AValue: TObject);
var
  Entry, PrevEntry: THashEntry;
  Hash: Cardinal;
begin
  Hash := GetHash(Key, FInternalSize);
  if FData[Hash] = nil then
  begin
    // create new base entry
    FData[Hash] := THashEntry.Create;
    FData[Hash].Key := Key;
    FData[Hash].Data := AValue;
    Inc(FCount);
    Exit;
  end;

  Entry := FData[Hash];
  PrevEntry := nil;
  // find key in list
  while Entry.Key <> Key do
  begin
    if Entry.Next = nil then // not found > add entry
    begin
      Entry.Next := THashEntry.Create;
      Entry.Next.Key := Key;
      Entry.Next.Data := AValue;
      Inc(FCount);
      Exit;
    end;
    PrevEntry := Entry;
    Entry := Entry.Next;
  end;
  if AValue = nil then // delete entry
  begin
    if Entry.Data = nil then // already nil
      Exit;

    if not FReferenceList then
      Entry.Data.Free;
    if PrevEntry <> nil then
      PrevEntry.Next := Entry.Next
    else
      FData[Hash] := Entry.Next;

    Entry.Free;
    Dec(FCount);
  end
  else // change entry
  begin
    if not FReferenceList then
      Entry.Data.Free;
    Entry.Data := AValue;
  end;
end;

constructor TIntVectorHashTable.Create(AReferenceList: Boolean; AInternalSize: Cardinal);
begin
  if AInternalSize = 0 then
    raise Exception.Create('Listsize must be greater than zero!'); // TODO: exception
  SetLength(FData, AInternalSize);
  FInternalSize := AInternalSize;
  FReferenceList := AReferenceList;
end;

destructor TIntVectorHashTable.Destroy;
var
  I: Integer;
  Next: THashEntry;
begin
  for I := 0 to FInternalSize - 1 do
  begin
    if FData[I] = nil then
      Continue;
    repeat
      Next := FData[I].Next;
      if not FReferenceList then
        FData[I].Data.Free;
      FData[I].Free;
      FData[I] := Next;
    until Next = nil;
  end;
  inherited Destroy;
end;

{ TStringHashTable.TIterator }

function TStringHashTable.TIterator.GetCurrent: TPair;
begin
  Result.Key := FEntry.Key;
  Result.Data := FEntry.Data;
end;

constructor TStringHashTable.TIterator.Create(AList: TStringHashTable);
begin
  FList := AList;
  FIndex := -1;
  FEntry := nil;
end;

function TStringHashTable.TIterator.MoveNext: Boolean;
begin
  if (FIndex = -1) or (FEntry.Next = nil) then
  begin
    // Move to next list
    repeat
      FIndex := FIndex + 1;
      if Int64(FIndex) = FList.FInternalSize then
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

{ TStringHashTable }

function TStringHashTable.GetEntry(Key: String): TObject;
var
  Entry: THashEntry;
  Hash: Cardinal;
begin
  if Key = '' then
    Exit(nil);

  Hash := GetHash(Key, FInternalSize);
  if FData[Hash] = nil then // base entry doesn't exist yet > not found
    Exit(nil);

  Entry := FData[Hash];
  while Entry.Key <> Key do
  begin
    if Entry.Next = nil then // end reached > not found
      Exit(nil);
    Entry := Entry.Next;
  end;
  // found
  Result := Entry.Data;
end;

function TStringHashTable.GetNext(Key: String): TObject;
var
  Hash: Cardinal;
  Current: THashEntry;
begin
  Hash := GetHash(Key, FInternalSize);
  // Find Entry
  Current := FData[Hash];
  while Current.Key <> Key do
  begin
    Current := Current.Next;
    if Current = nil then
      Exit(nil); // given entry not in list
  end;

  // Find Next
  if Current.Next <> nil then
    Exit(Current.Next.Data);
  repeat
    Inc(Hash);
  until (Hash = FInternalSize) or (FData[Hash] <> nil);
  if Hash = FInternalSize then
    Exit(nil);
  Result := FData[Hash].Data;
end;

function TStringHashTable.GetPrev(Key: String): TObject;
var
  Hash: Cardinal;
  Current: THashEntry;
begin
  raise Exception.Create('THIS IS A COPY OF NEXT!');

  Hash := GetHash(Key, FInternalSize);
  // Find Entry
  Current := FData[Hash];
  while Current.Key <> Key do
  begin
    Current := Current.Next;
    if Current = nil then
      Exit(nil); // given entry not in list
  end;

  // Find Next
  if Current.Next <> nil then
    Exit(Current.Next.Data);
  repeat
    Inc(Hash);
  until (Hash = FInternalSize) or (FData[Hash] <> nil);
  if Hash = FInternalSize then
    Exit(nil);
  Result := FData[Hash].Data;
end;

procedure TStringHashTable.SetEntry(Key: String; AValue: TObject);
var
  Entry, PrevEntry: THashEntry;
  Hash: Cardinal;
begin
  if Key = '' then
    raise Exception.Create('Empty String for TStringHashTable is not allowed!');

  Hash := GetHash(Key, FInternalSize);
  if FData[Hash] = nil then
  begin
    // create new base entry
    FData[Hash] := THashEntry.Create;
    FData[Hash].Key := Key;
    FData[Hash].Data := AValue;
    Inc(FCount);
    Exit;
  end;

  Entry := FData[Hash];
  PrevEntry := nil;
  // find key in list
  while Entry.Key <> Key do
  begin
    if Entry.Next = nil then // not fount > add entry
    begin
      Entry.Next := THashEntry.Create;
      Entry.Next.Key := Key;
      Entry.Next.Data := AValue;
      Inc(FCount);
      Exit;
    end;
    PrevEntry := Entry;
    Entry := Entry.Next;
  end;
  if AValue = nil then // delete entry
  begin
    if Entry.Data = nil then // already nil
      Exit;

    if not FReferenceList then
      Entry.Data.Free;

    if PrevEntry <> nil then
      PrevEntry.Next := Entry.Next
    else
      FData[Hash] := Entry.Next;

    Entry.Free;
    Dec(FCount);
  end
  else // change entry
  begin
    if not FReferenceList then
      Entry.Data.Free;
    Entry.Data := AValue;
  end;
end;

constructor TStringHashTable.Create(AReferenceList: Boolean; AInternalSize: Cardinal);
begin
  if AInternalSize = 0 then
    raise Exception.Create('Listsize must be greater than zero!'); // TODO: exception
  SetLength(FData, AInternalSize);
  FInternalSize := AInternalSize;
  FReferenceList := AReferenceList;
end;

destructor TStringHashTable.Destroy;
begin
  DelAll;
  inherited Destroy;
end;

function TStringHashTable.NextKey(Key: String): String;
var
  Hash: Cardinal;
  Current: THashEntry;
begin
  Hash := GetHash(Key, FInternalSize);
  // Find Entry
  Current := FData[Hash];
  while Current.Key <> Key do
  begin
    Current := Current.Next;
    if Current = nil then
      Exit(''); // given entry not in list
  end;

  // Find Next
  if Current.Next <> nil then
    Exit(Current.Next.Key);
  repeat
    Inc(Hash);
  until (Hash = FInternalSize) or (FData[Hash] <> nil);
  if Hash = FInternalSize then
    Exit('');
  Result := FData[Hash].Key;
end;

function TStringHashTable.PrevKey(Key: String): String;
var
  Hash: Cardinal;
  Current: THashEntry;
begin
  Hash := GetHash(Key, FInternalSize);
  // Find Entry
  if FData[Hash] = nil then
    Exit('');
  Current := FData[Hash];
  while Current.Key <> Key do
  begin
    if (Current.Next <> nil) and (Current.Next.Key = Key) then
      Exit(Current.Key)
    else if Current.Next = nil then
      Exit(''); // not in list
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
      Exit(Current.Key);
    end;
  end;
  Result := '';
end;

function TStringHashTable.FirstKey: String;
var
  Current: THashEntry;
  I: Integer;
begin
  if Count = 0 then
    Exit('');
  for I := 0 to FInternalSize - 1 do
  begin
    Current := FData[I];
    while Current <> nil do
    begin
      Exit(Current.Key);
      Current := Current.Next;
    end;
  end;
end;

function TStringHashTable.LastKey: String;
var
  FoundOne: Boolean;
  I: Cardinal;
  Current: THashEntry;
begin
    if Count = 0 then
    Exit('');
  FoundOne := False;
  for I := FInternalSize - 1 downto 0 do
  begin
    Current := FData[I];
    while Current <> nil do
    begin
      FoundOne := True;
      Result := Current.Key;
      Current := Current.Next;
    end;
    if FoundOne then
      Exit;
  end;
end;

function TStringHashTable.NextData(Key: String): TObject;
begin
  Result := Data[NextKey(Key)];
end;

function TStringHashTable.PrevData(Key: String): TObject;
begin
  Result := Data[PrevKey(Key)];
end;

procedure TStringHashTable.DelAll;
var
  Next: THashEntry;
  I: Integer;
begin
  for I := 0 to FInternalSize - 1 do
  begin
    while FData[I] <> nil do
    begin
      Next := FData[I].Next;
      if not FReferenceList then
        FData[I].Data.Free;
      FData[I].Free;
      FData[I] := Next;
    end;
  end;
end;

function TStringHashTable.GetEnumerator: TIterator;
begin
  Result := TIterator.Create(Self);
end;

{ TObjectHashTable }

function TObjectHashTable.GetEntry(Key: TObject): TObject;
var
  Entry: THashEntry;
  Hash: Cardinal;
begin
  Hash := GetHash(Key, FInternalSize);
  if FData[Hash] = nil then // base entry doesn't exist yet > not found
    Exit(nil);

  Entry := FData[Hash];
  while Pointer(Entry.Key) <> Pointer(Key) do
  begin
    if Entry.Next = nil then // end reached > not found
      Exit(nil);
    Entry := Entry.Next;
  end;
  // found
  Result := Entry.Data;
end;

procedure TObjectHashTable.SetEntry(Key: TObject; AValue: TObject);
var
  Entry, PrevEntry: THashEntry;
  Hash: Cardinal;
begin
  Hash := GetHash(Key, FInternalSize);
  if FData[Hash] = nil then
  begin
    // create new base entry
    FData[Hash] := THashEntry.Create;
    FData[Hash].Key := Key;
    FData[Hash].Data := AValue;
    Inc(FCount);
    Exit;
  end;

  Entry := FData[Hash];
  PrevEntry := nil;
  // find key in list
  while Pointer(Entry.Key) <> Pointer(Key) do
  begin
    if Entry.Next = nil then // not fount > add entry
    begin
      Entry.Next := THashEntry.Create;
      Entry.Next.Key := Key;
      Entry.Next.Data := AValue;
      Inc(FCount);
      Exit;
    end;
    PrevEntry := Entry;
    Entry := Entry.Next;
  end;
  if AValue = nil then // delete entry
  begin
    if Entry.Data = nil then // already nil
      Exit;

    if not FReferenceList then
      Entry.Data.Free;
    if PrevEntry <> nil then
      PrevEntry.Next := Entry.Next
    else
      FData[Hash] := Entry.Next;

    Entry.Free;
    Dec(FCount);
  end
  else // change entry
    Entry.Data := AValue;
end;

constructor TObjectHashTable.Create(AReferenceList: Boolean; AInternalSize: Cardinal);
begin
  if AInternalSize = 0 then
    raise Exception.Create('Listsize must be greater than zero!'); // TODO: exception
  SetLength(FData, AInternalSize);
  FInternalSize := AInternalSize;
  FReferenceList := AReferenceList;
end;

destructor TObjectHashTable.Destroy;
var
  Next: THashEntry;
  I: Integer;
begin
  for I := 0 to FInternalSize - 1 do
  begin
    if FData[I] = nil then
      Continue;
    repeat
      Next := FData[I].Next;
      if not FReferenceList then
        FData[I].Data.Free;
      FData[I].Free;
      FData[I] := Next;
    until Next = nil;
  end;
  inherited Destroy;
end;

{ TObjectStack }

function TObjectStack.GetCount: Integer;
begin
  Result := FCount;
end;

constructor TObjectStack.Create(AReferenceList: Boolean);
begin
  FReferenceList := AReferenceList;
end;

destructor TObjectStack.Destroy;
begin
  if not FReferenceList then
    while FCount > 0 do
      Pop.Free;
  inherited Destroy;
end;

function TObjectStack.Push(AElement: TObject): TObject;
begin
  SetLength(FItems, Length(FItems) + 1);
  FItems[Length(FItems) - 1] := AElement;
  Inc(FCount);
  Result := AElement;
end;

function TObjectStack.Pop: TObject;
begin
  if FCount = 0 then
    Exit(nil);
  Dec(FCount);
  Result := FItems[FCount]; // "count - 1" but already decremented
  SetLength(FItems, FCount);
end;

function TObjectStack.Top: TObject;
begin
  Result := FItems[FCount - 1];
end;

function TObjectStack.Copy: TObjectStack;
var
  I: Integer;
begin
  Result := TObjectStack.Create;
  for I := 0 to FCount - 1 do
    Result.Push(FItems[I]);
end;

{ TObjectArray.TIterator }

function TObjectArray.TIterator.GetCurrent: TObject;
begin
  Result := FList[FCurrent];
end;

constructor TObjectArray.TIterator.Create(AList: TObjectArray; AReversed, AAutoFree: Boolean);
begin
  FList := AList;
  FReversed := AReversed;
  FAutoFree := AAutoFree;
  if FReversed then
    FCurrent := FList.Count
  else
    FCurrent := -1;
end;

function TObjectArray.TIterator.MoveNext: Boolean;
begin
  if FRemoveFlag then
  begin
    FList.Del(FCurrent);
    FRemoveFlag := False;
  end
  else if not FReversed then
  begin
    Inc(FCurrent);
    Result := FCurrent <> FList.Count;
  end;

  if FReversed then
  begin
    Dec(FCurrent);
    Result := FCurrent <> -1;
  end;

  if not Result and FAutoFree then
    Free;
end;

procedure TObjectArray.TIterator.RemoveCurrent;
begin
  FRemoveFlag := True;
end;

{ TObjectArray }

function TObjectArray.GetItem(I: Integer): TObject;
begin
  RangeCheck(I);
  Result := FItems[I];
end;

procedure TObjectArray.RangeCheck(AIndex: Integer);
begin
  if (AIndex >= Count) or (AIndex < 0) then
    raise Exception.Create(Format('Item out of bounds! (%d >= %d)', [AIndex, Count])); // TODO: ObjectArray Get Item Exception
end;

procedure TObjectArray.Sort(ACompareFunc: TCompareFunction; ALeft, ARight: Integer);
var
  Pivot, Tmp: TObject;
  L, R: Integer;
begin
  Pivot := FItems[ALeft];
  L := ALeft;
  R := ARight;
  repeat
    while ACompareFunc(Pivot, FItems[L]) do
      Inc(L);
    while ACompareFunc(FItems[R], Pivot) do
      Dec(R);
    if L <= R then
    begin
      Tmp := FItems[L];
      FItems[L] := FItems[R];
      FItems[R] := Tmp;
      Inc(L);
      Dec(R);
    end;
  until L > R;
  if R > ALeft then
    Sort(ACompareFunc, ALeft, R);
  if L < ARight then
    Sort(ACompareFunc, L, ARight);
end;

procedure TObjectArray.SetItem(I: Integer; AValue: TObject);
begin
  RangeCheck(I);
  if not FReferenceList then
    FItems[I].Free;
  FItems[I] := AValue;
end;

constructor TObjectArray.Create(AReferenceList: Boolean);
begin
  FReferenceList := AReferenceList;
end;

destructor TObjectArray.Destroy;
var
  I: Integer;
begin
  if not FReferenceList then
    for I := 0 to Count - 1 do with FItems[I] do
      Free;
  inherited;
end;

function TObjectArray.Add(AElement: TObject): TObject;
begin
  SetLength(FItems, Count + 1);
  FItems[Count - 1] := AElement;
  Result := AElement;
end;

procedure TObjectArray.DelLast;
begin
  if not FReferenceList then
    FItems[Count - 1].Free;
  SetLength(FItems, Count - 1);
end;

procedure TObjectArray.DelAll;
var
  I: Integer;
begin
  if not FReferenceList then
    for I := 0 to Count - 1 do
      FItems[I].Free;
  SetLength(FItems, 0);
end;

procedure TObjectArray.Insert(AElement: TObject; AIndex: Integer);
begin
  SetLength(FItems, Count + 1);
  Move(FItems[AIndex], FItems[AIndex + 1], SizeOf(FItems[AIndex]) * (Count - AIndex));
  FItems[AIndex] := AElement;
end;

procedure TObjectArray.Del(AElement: TObject);
begin
  Del(Find(AElement));
end;

procedure TObjectArray.Del(AIndex: Integer);
begin
  if not FReferenceList then
    FItems[AIndex].Free;
  if Count - AIndex - 1 > 0 then
    Move(FItems[AIndex + 1], FItems[AIndex], SizeOf(FItems[AIndex]) * (Count - AIndex - 1));
  SetLength(FItems, Count - 1);
end;

function TObjectArray.Swap(I, J: Integer): Boolean;
var
  Tmp: TObject;
begin
  Result := (I >= 0) and (I < Count) and (J >= 0) and (J < Count);
  if Result and (I <> J) then
  begin
    Tmp := FItems[I];
    FItems[I] := FItems[J];
    FItems[J] := Tmp;
  end;
end;

procedure TObjectArray.WithEach(AFunc: TWithEachProcedure; AFreeFunction: Boolean);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    AFunc.WithEach(FItems[I]);
  if AFreeFunction then
    AFunc.Free;
end;

procedure TObjectArray.WithEach(AFunc: TWithEachProcedureBasic);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    AFunc(FItems[I]);
end;

function TObjectArray.FindFirstIndex(AFunc: TFindFunction; AFreeFunction: Boolean): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if AFunc.Find(FItems[I]) then
    begin
      Result := I;
      Break;
    end;
  if AFreeFunction then
    AFunc.Free;
end;

function TObjectArray.FindLastIndex(AFunc: TFindFunction; AFreeFunction: Boolean): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Count - 1 downto 0 do
    if AFunc.Find(FItems[I]) then
    begin
      Result := I;
      Break;
    end;
  if AFreeFunction then
    AFunc.Free;
end;

function TObjectArray.FindFirst(AFunc: TFindFunction; AFreeFunction: Boolean): TObject;
var
  I: Integer;
begin
  I := FindFirstIndex(AFunc, AFreeFunction);
  if I = -1 then
    Result := nil
  else
    Result := FItems[I];
end;

function TObjectArray.FindLast(AFunc: TFindFunction; AFreeFunction: Boolean): TObject;
var
  I: Integer;
begin
  I := FindLastIndex(AFunc, AFreeFunction);
  if I = -1 then
    Result := nil
  else
    Result := FItems[I];
end;

function TObjectArray.FindAsList(AFunc: TFindFunction; AFreeFunction: Boolean): TObjectList;
var
  I: Integer;
begin
  Result := TObjectList.Create(True);
  for I := 0 to Count - 1 do
    if AFunc.Find(FItems[I]) then
      Result.Add(FItems[I]);
  if AFreeFunction then
    AFunc.Free;
end;

function TObjectArray.FindAsArray(AFunc: TFindFunction; AFreeFunction: Boolean): TObjectArray;
var
  I: Integer;
begin
  Result := TObjectArray.Create(True);
  for I := 0 to Count - 1 do
    if AFunc.Find(FItems[I]) then
      Result.Add(FItems[I]);
  if AFreeFunction then
    AFunc.Free;
end;

function TObjectArray.Find(AElement: TObject): Integer;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do with FItems[I] do
    if Pointer(AElement) = Pointer(FItems[I]) then
      Exit(I);
  Result := -1;
end;

function TObjectArray.First: TObject;
begin
  if Count > 0 then
    Exit(Self[0]);
  Result := nil;
end;

function TObjectArray.Last: TObject;
begin
  if Count > 0 then
    Exit(Self[Count - 1]);
  Result := nil;
end;

function TObjectArray.GetEnumerator(AAutoFree: Boolean): TIterator;
begin
  Result := TIterator.Create(Self, FIterReversed, AAutoFree);
  FIterReversed := False;
end;

function TObjectArray.IterReversed: TObjectArray;
begin
  FIterReversed := True;
  Result := Self;
end;

function TObjectArray.Copy: TObjectArray;
var
  I: Integer;
begin
  Result := TObjectArray.Create(True);
  for I := 0 to Count - 1 do
    Result.Add(FItems[I]);
end;

procedure TObjectArray.Sort(ACompareFunc: TCompareFunction);
begin
  if Count > 1 then
    Sort(ACompareFunc, 0, Count - 1);
end;

function TObjectArray.GetCount: Integer;
begin
  Result := Length(FItems);
end;

{ TObjectList.TIterator }

constructor TObjectList.TIterator.Create(AList: TObjectList; AReversed, AAutoFree: Boolean);
begin
  FList := AList;
  FReversed := AReversed;
  FAutoFree := AAutoFree;
  if FReversed then
    FNext := AList.FLast
  else
    FNext := AList.FFirst;
end;

function TObjectList.TIterator.MoveNext: Boolean;
begin
  // check remove flag
  if FRemoveFlag and (FCurrent <> nil) then
  begin
    FList.Del(FCurrent);
    FRemoveFlag := False;
  end;
  // move to next
  Result := FNext <> nil;
  if Result then
  begin
    FCurrent := FNext;
    if FReversed then
      FNext := FNext.Prev
    else
      FNext := FNext.Next;
  end
  else if FAutoFree then
    Self.Free;
end;

procedure TObjectList.TIterator.RemoveCurrent;
begin
  FRemoveFlag := True;
end;

{ TObjectList }

function TObjectList.Find(AElement: TObject): TItem;
var
  I: TItem;
begin
  I := FFirst;
  while (I <> nil) and (Pointer(I.Data) <> Pointer(AElement)) do
    I := I.Next;
  if I = nil then
    raise Exception.Create('TODO! Could not find Element in List');
  Result := I;
end;

procedure TObjectList.Del(AItem: TItem);
begin
  with AItem do
  begin
    if Prev = nil then
      FFirst := Next
    else
      Prev.Next := Next;
    if Next = nil then
      FLast := Prev
    else
      Next.Prev := Prev;

    if not FReferenceList then
      Data.Free;
    Free;
  end;
  FCount := FCount - 1;
end;

function TObjectList.GetItem(I: Integer): TObject;
var
  FPos: TItem;
begin
  if (I >= Count) or (I < 0) then
    raise Exception.Create('Item out of bounds'); // TODO: ObjectList Get Item Exception
  FPos := FFirst;
  while I > 0 do
  begin
    FPos := FPos.Next;
    Dec(I);
  end;
  Result := FPos.Data;
end;

procedure TObjectList.Sort(ACompare: TCompareFunction; ALeft, ARight: TItem; ACount: Integer);
var
  Pivot, Tmp: TObject;
  L, R: TItem;
  LMove, RMove: Integer;
begin
  Pivot := ALeft.Data;
  L := ALeft;
  R := ARight;
  LMove := 0;
  RMove := 0;
  repeat
    // Move to first greater/equal than Pivot on left side
    while ACompare(Pivot, L.Data) do
    begin
      L := L.Next;
      Inc(LMove);
    end;
    // Move to first less/equal than Pivot on right side
    while ACompare(R.Data, Pivot) do
    begin
      R := R.Prev;
      Inc(RMove);
    end;

    if LMove + RMove < ACount then
    begin
      // Swap the data
      Tmp := L.Data;
      L.Data := R.Data;
      R.Data := Tmp;
      // Move further
      L := L.Next;
      Inc(LMove);
      R := R.Prev;
      Inc(RMove);
    end
  until LMove + RMove >= ACount;
  // Sort left
  if ACount - RMove > 1 then
    Sort(ACompare, ALeft, R, ACount - RMove);
  // Sort right
  if ACount - LMove > 1 then
    Sort(ACompare, L, ARight, ACount - LMove);
end;

constructor TObjectList.Create(AReferenceList: Boolean);
begin
  FReferenceList := AReferenceList;
end;

destructor TObjectList.Destroy;
begin
  DelAll;
  inherited Destroy;
end;

function TObjectList.Add(AElement: TObject): TObject;
begin
  if FLast = nil then
  begin
    FFirst := TItem.Create;
    FLast := FFirst;
    FFirst.Data := AElement;
  end
  else
  begin
    FLast.Next := TItem.Create;
    FLast.Next.Prev := FLast;
    FLast := FLast.Next;
    FLast.Data := AElement;
  end;
  FCount := FCount + 1;
  Result := AElement;
end;

procedure TObjectList.Del(AElement: TObject);
begin
  Del(Find(AElement));
end;

procedure TObjectList.DelAll;
begin
  while FCount > 0 do
    Del(FLast);
end;

procedure TObjectList.WithEach(AFunc: TWithEachProcedure; AFreeFunction: Boolean);
var
  Current: TObject;
begin
  for Current in Self do
    AFunc.WithEach(Current);
  if AFreeFunction then
    AFunc.Free;
end;

procedure TObjectList.WithEach(AFunc: TWithEachProcedureBasic);
var
  Current: TObject;
begin
  for Current in Self do
    AFunc(Current);
end;

procedure TObjectList.WithEach(AFunc: TWithEachProcedureObject);
var
  Current: TObject;
begin
  for Current in Self do
    AFunc(Current);
end;

function TObjectList.FindFirst(AFunc: TFindFunction; AFreeFunction: Boolean): TObject;
var
  Current: TObject;
begin
  for Current in Self do
    if AFunc.Find(Current) then
    begin
      Result := Current;
      Break;
    end;
  if AFreeFunction then
    AFunc.Free;
end;

function TObjectList.FindLast(AFunc: TFindFunction; AFreeFunction: Boolean): TObject;
var
  Current: TObject;
begin
  Result := nil;
  for Current in IterReversed do
    if AFunc.Find(Current) then
    begin
      Result := Current;
      Break;
    end;
  if AFreeFunction then
    AFunc.Free;
end;

function TObjectList.FindAll(AFunc: TFindFunction; AFreeFunction: Boolean): TObjectList;
var
  Current: TObject;
begin
  Result := TObjectList.Create(True);
  for Current in Self do
    if AFunc.Find(Current) then
      Result.Add(Current);
  if AFreeFunction then
    AFunc.Free;
end;

function TObjectList.Exists(AElement: TObject): Boolean;
var
  Current: TObject;
begin
  for Current in Self do
    if Pointer(Current) = Pointer(AElement) then
      Exit(True);
  Result := False;
end;

procedure TObjectList.Sort(ACompareFunc: TCompareFunction);
begin
  if Count > 1 then
    Sort(ACompareFunc, FFirst, FLast, Count);
end;

function TObjectList.Copy: TObjectList;
var
  Current: TObject;
begin
  Result := TObjectList.Create(True);
  for Current in Self do
    Result.Add(Current);
end;

function TObjectList.GetEnumerator(AAutoFree: Boolean): TIterator;
begin
  Result := TIterator.Create(Self, FIterReversed, AAutoFree);
  FIterReversed := False;
end;

function TObjectList.IterReversed: TObjectList;
begin
  FIterReversed := True;
  Result := Self;
end;

end.

