unit Pengine.ObservableCollections;

interface

uses
  Pengine.Collections,
  Pengine.EventHandling;

type

  TObservableArray<T> = class(TArray<T>)
  public type

    TEventInfo = TSenderEventInfo<TObservableArray<T>>;

    TEvent = TEvent<TEventInfo>;

    TAddEventInfo = class(TEventInfo)
    private
      FItem: T;
      FIndex: Integer;

    public
      constructor Create(ASender: TObservableArray<T>; AIndex: Integer; AItem: T);

      property Item: T read FItem;
      property Index: Integer read FIndex;

    end;

    TAddEvent = TEvent<TAddEventInfo>;

    TRemoveEventInfo = class(TEventInfo)
    private
      FIndex: Integer;

      function GetItem: T;

    public
      constructor Create(ASender: TObservableArray<T>; AIndex: Integer);

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
      constructor Create(ASender: TObservableArray<T>; AIndex, ADestination: Integer);

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
      constructor Create(ASender: TObservableArray<T>; AIndex1, AIndex2: Integer);

      property Item1: T read GetItem1;
      property Index1: Integer read FIndex1;
      property Item2: T read GetItem2;
      property Index2: Integer read FIndex2;

    end;

    TSwapEvent = TEvent<TSwapEventInfo>;

  private
    FOnChange: TEvent;
    FOnAdd: TAddEvent;
    FOnRemove: TRemoveEvent;
    FOnClear: TEvent;
    FOnChangeIndex: TChangeIndexEvent;
    FOnSwap: TSwapEvent;
    FOnSort: TEvent;

  protected
    procedure BeforeSort; override;
    procedure AfterSort; override;

  public
    function Add(AItem: T): T; override;
    function Insert(AItem: T; AIndex: Integer): T; override;
    procedure RemoveAt(AIndex: Integer); override;
    procedure Clear(AZeroCapacity: Boolean = True); override;
    procedure SetIndex(ASource, ADestination: Integer); override;
    procedure Swap(A, B: Integer); override;

    function Copy: TObservableArray<T>; reintroduce; inline;

    /// <summary>Called after every other event, that changes the array.</summary>
    function OnChange: TEvent.TAccess;
    /// <summary>Called before an new item gets added or inserted.</summary>
    function OnAdd: TAddEvent.TAccess;
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
    constructor Create(AGrowAmount: Integer = 16; AShrinkRetain: Integer = 8); overload; override;
    constructor Create(AOwnsObjects: Boolean; AGrowAmount: Integer = 16; AShrinkRetain: Integer = 8); reintroduce; overload;

    function Find(AItem: T): Integer; override;

    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;

  end;

  TObservableObjectArray<T: class> = class(TObservableRefArray<T>)

  end;

implementation

{ TObservableArray<T>.TAddEventInfo }

constructor TObservableArray<T>.TAddEventInfo.Create(ASender: TObservableArray<T>; AIndex: Integer; AItem: T);
begin
  inherited Create(ASender);
  FIndex := AIndex;
  FItem := AItem;
end;

{ TObservableArray<T>.TRemoveEventInfo }

constructor TObservableArray<T>.TRemoveEventInfo.Create(ASender: TObservableArray<T>; AIndex: Integer);
begin
  inherited Create(ASender);
  FIndex := AIndex;
end;

function TObservableArray<T>.TRemoveEventInfo.GetItem: T;
begin
  Result := Sender[Index];
end;

{ TObservableArray<T>.TChangeIndexEventInfo }

constructor TObservableArray<T>.TChangeIndexEventInfo.Create(ASender: TObservableArray<T>; AIndex,
  ADestination: Integer);
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

constructor TObservableArray<T>.TSwapEventInfo.Create(ASender: TObservableArray<T>; AIndex1, AIndex2: Integer);
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
  inherited;
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

function TObservableArray<T>.Copy: TObservableArray<T>;
begin
  Result := TObservableArray<T>(CreateCopy);
end;

function TObservableArray<T>.Insert(AItem: T; AIndex: Integer): T;
begin
  FOnAdd.Execute(TAddEventInfo.Create(Self, AIndex, AItem));
  inherited;
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

constructor TObservableRefArray<T>.Create(AGrowAmount, AShrinkRetain: Integer);
begin
  inherited;
end;

constructor TObservableRefArray<T>.Create(AOwnsObjects: Boolean; AGrowAmount, AShrinkRetain: Integer);
begin
  inherited Create(AGrowAmount, AShrinkRetain);
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

end.
