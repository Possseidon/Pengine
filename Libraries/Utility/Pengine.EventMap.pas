unit Pengine.EventMap;

interface

uses
  Pengine.HashCollections,
  Pengine.Hasher,
  Pengine.EventHandling,
  Pengine.Collections;

type

  TEventMap<T; H: THasher<T>> = class
  private
    FMap: TMap<T, Pointer, H>;

    function GetAccess(AKey: T): TEvent.TAccess;

  public
    constructor Create;
    destructor Destroy; override;

    property Access[AKey: T]: TEvent.TAccess read GetAccess;

    procedure Execute;

  end;

  TEventMap<T; E: TEventInfo; H: THasher<T>> = class
  private
    FMap: TMap<T, Pointer, H>;

    function GetAccess(AKey: T): TEvent<E>.TAccess;

  public
    constructor Create;
    destructor Destroy; override;

    property Access[AKey: T]: TEvent<E>.TAccess read GetAccess;

    procedure Execute(AInfo: E; AFreeInfo: Boolean = True);

  end;

implementation


{ TEventMap<T, H> }

constructor TEventMap<T, H>.Create;
begin
  FMap := TMap<T, Pointer, H>.Create;
end;

destructor TEventMap<T, H>.Destroy;
var
  P: Pointer;
begin
  for P in FMap.Values do
    FreeMem(P);
  FMap.Free;
  inherited;
end;

procedure TEventMap<T, H>.Execute;
var
  Pair: TPair<T, Pointer>;
  Element: T;
  RemoveHandler: TArray<T>;
begin
  if FMap.Empty then
    Exit;
  RemoveHandler := TArray<T>.Create;
  try
    for Pair in FMap do
    begin
      if TEvent(Pair.Value^).HasHandler then
        TEvent(Pair.Value^).Execute
      else
      begin
        FreeMem(Pair.Value);
        RemoveHandler.Add(Pair.Key);
      end;
    end;
    for Element in RemoveHandler do
    begin
      FMap.Del(Element);  
    end;
  finally
    RemoveHandler.Free;
  end;
end;

function TEventMap<T, H>.GetAccess(AKey: T): TEvent.TAccess;
var
  P: Pointer;
begin
  if FMap.Get(AKey, P) then
    Exit(TEvent(P^).Access);
  P := AllocMem(SizeOf(TEvent));
  FMap[AKey] := P;
  Result := TEvent(P^).Access;
end;

{ TEventMap<T, E, H> }

constructor TEventMap<T, E, H>.Create;
begin
  FMap := TMap<T, Pointer, H>.Create;
end;

destructor TEventMap<T, E, H>.Destroy;
begin
  FMap.Free;
  inherited;
end;

procedure TEventMap<T, E, H>.Execute(AInfo: E; AFreeInfo: Boolean);
var
  Pair: TPair<T, Pointer>;
  Element: T;
  RemoveHandler: TArray<T>;
begin
  if FMap.Empty then
    Exit;
  RemoveHandler := TArray<T>.Create;
  try
    for Pair in FMap do
    begin
      if TEvent<E>(Pair.Value^).HasHandler then
        TEvent<E>(Pair.Value^).Execute(AInfo)
      else
      begin
        FreeMem(Pair.Value);
        RemoveHandler.Add(Pair.Key);
      end;
    end;
    for Element in RemoveHandler do
    begin
      FMap.Del(Element);  
    end;
  finally
    RemoveHandler.Free;
    if AFreeInfo then
      AInfo.Free;
  end;
end;

function TEventMap<T, E, H>.GetAccess(AKey: T): TEvent<E>.TAccess;
var
  P: Pointer;
begin
  if FMap.Get(AKey, P) then
    Exit(TEvent<E>(P^).Access);
  P := AllocMem(SizeOf(TEvent));
  FMap[AKey] := P;
  Result := TEvent<E>(P^).Access;
end;

end.

