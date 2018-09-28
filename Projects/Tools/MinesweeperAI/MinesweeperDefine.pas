unit MinesweeperDefine;

interface

uses
  System.SysUtils,

  Pengine.IntMaths,
  Pengine.Bitfield,
  Pengine.Utility,
  Pengine.EventHandling,
  Pengine.Collections,
  System.Math;

type

  EMinesweeper = class(Exception);

  TAdjacentMines = 0 .. 8;

  TRevealResult = (
    rrZeroMines,
    rrSomeMines,
    rrMine
    );

  TMinesweeperBase = class
  private
    function GetSize: TIntVector2; virtual; abstract;

  public
    property Size: TIntVector2 read GetSize;

    function Reveal(APos: TIntVector2): TRevealResult; virtual; abstract;
    function IsRevealed(APos: TIntVector2): Boolean; virtual; abstract;
    function AdjacentMines(APos: TIntVector2): TAdjacentMines; virtual; abstract;

  end;

  TMinesweeper = class(TMinesweeperBase)
  public type

    TEventInfo = TSenderEventInfo<TMinesweeper>;

    TEvent = TEvent<TEventInfo>;

    TRevealEventInfo = class(TEventInfo)
    public type

      TFields = TArray<TIntVector2>;

    private
      FFields: TFields;

      function GetFields: TFields.TReader;

    public
      constructor Create(ASender: TMinesweeper; AFields: TFields);
      destructor Destroy; override;

      property Fields: TFields.TReader read GetFields;

    end;

    TRevealEvent = TEvent<TRevealEventInfo>;

  private
    FOnReveal: TEvent;
    FWrapAround: Boolean;
    FRevealedFields: Integer;
    FMineCount: Integer;
    FMines: TBitfieldArray;
    FRevealed: TBitfieldArray;

    function GetSize: TIntVector2; override;
    function GetFields: Integer;
    function GetNoneMineFields: Integer;
    function GetRevealedPercentage: Single;

  public
    constructor Create(ASize: TIntVector2);
    destructor Destroy; override;

    procedure Clear;
    procedure Generate(AMines: Integer); overload;
    procedure Generate(AMines: Integer; APreventMineAt: TIntVector2); overload;

    property Mines: Integer read FMineCount;
    property Fields: Integer read GetFields;
    property NoneMineFields: Integer read GetNoneMineFields;
    property RevealedFields: Integer read FRevealedFields;
    property RevealedPercentage: Single read GetRevealedPercentage;
    function IsFullyRevealed: Boolean;

    function IsMine(APos: TIntVector2): Boolean; inline;
    function IsRevealed(APos: TIntVector2): Boolean; override;

    function AdjacentMines(APos: TIntVector2): TAdjacentMines; override;
    function HasAdjacentMines(APos: TIntVector2): Boolean;
    function Reveal(APos: TIntVector2): TRevealResult; override;

    function CheckBounds(APos: TIntVector2): TIntBounds2;

    property WrapAround: Boolean read FWrapAround write FWrapAround;

  end;

implementation

{ TMinesweeper }

function TMinesweeper.AdjacentMines(APos: TIntVector2): TAdjacentMines;
var
  Pos: TIntVector2;
begin
  Result := 0;
  for Pos in CheckBounds(APos) do
    if IsMine(Pos) then
      Inc(Result);
end;

function TMinesweeper.CheckBounds(APos: TIntVector2): TIntBounds2;
begin
  Result := APos + IBounds2I(-1, +1);
  if not WrapAround then
    Result := IBounds2(Size).Clamp(Result);
end;

procedure TMinesweeper.Clear;
begin
  if Mines = 0 then
    Exit;
  FRevealedFields := 0;
  FMineCount := 0;
  FMines.Clear;
  FRevealed.Clear;
end;

constructor TMinesweeper.Create(ASize: TIntVector2);
begin
  FMines := TBitfieldArray.Create(ASize);
  FRevealed := TBitfieldArray.Create(ASize);
end;

destructor TMinesweeper.Destroy;
begin
  FMines.Free;
  FRevealed.Free;
  inherited;
end;

procedure TMinesweeper.Generate(AMines: Integer);
begin
  Generate(AMines, -1);
end;

procedure TMinesweeper.Generate(AMines: Integer; APreventMineAt: TIntVector2);

  function FieldIndexToPos(AIndex: Integer): TIntVector2;
  begin
    Result.Create(AIndex mod Size.X, AIndex div Size.X);
  end;

  function PosToFieldIndex(APos: TIntVector2): Integer;
  begin
    Result := APos.X + APos.Y * Size.X;
  end;

var
  Indices: TIntArray;
  I, Selected: Integer;
  Pos: TIntVector2;
begin
  if AMines < 0 then
    raise EMinesweeper.Create('Invalid number of mines.');

  Clear;

  if AMines = 0 then
    Exit;

  FMineCount := AMines;

  Indices := TIntArray.Create;
  Indices.Capacity := Fields;
  Indices.ForceCount(Indices.Capacity);

  for I := 0 to Indices.MaxIndex do
    Indices[I] := I;

  I := Indices.MaxIndex;

  if APreventMineAt <> -1 then
  begin
    for Pos in CheckBounds(APreventMineAt).InReverse do
    begin
      Indices[PosToFieldIndex(Pos)] := Indices[I];
      Dec(I);
    end;
  end;

  for I := I downto I - AMines + 1 do
  begin
    Selected := Random(I + 1);
    FMines[FieldIndexToPos(Indices[Selected])] := True;
    Indices[Selected] := Indices[I];
  end;

  Indices.Free;
end;

function TMinesweeper.GetFields: Integer;
begin
  Result := Size.X * Size.Y;
end;

function TMinesweeper.GetNoneMineFields: Integer;
begin
  Result := Fields - Mines;
end;

function TMinesweeper.GetRevealedPercentage: Single;
begin
  Result := RevealedFields / NoneMineFields;
end;

function TMinesweeper.GetSize: TIntVector2;
begin
  Result := FMines.Size;
end;

function TMinesweeper.HasAdjacentMines(APos: TIntVector2): Boolean;
var
  Pos: TIntVector2;
begin
  for Pos in CheckBounds(APos) do
    if IsMine(Pos) then
      Exit(True);
  Result := False;
end;

function TMinesweeper.IsFullyRevealed: Boolean;
begin
  Result := RevealedFields = NoneMineFields;
end;

function TMinesweeper.IsMine(APos: TIntVector2): Boolean;
begin
  if WrapAround then
    Result := FMines[IBounds2(Size).RangedMod(APos)]
  else
    Result := (APos in Size) and FMines[APos];
end;

function TMinesweeper.IsRevealed(APos: TIntVector2): Boolean;
begin
  if WrapAround then
    Result := FRevealed[IBounds2(Size).RangedMod(APos)]
  else
    Result := (APos in Size) and FRevealed[APos];
end;

function TMinesweeper.Reveal(APos: TIntVector2): TRevealResult;
var
  Pos: TIntVector2;
begin
  if FRevealed[APos] then
    Exit;

  if IsMine(APos) then
    Exit(rrMine);

  FRevealed[APos] := True;
  Inc(FRevealedFields);

  if HasAdjacentMines(APos) then
    Exit(rrSomeMines);

  Result := rrZeroMines;

  for Pos in CheckBounds(APos) do
    Reveal(Pos);
end;

{ TMinesweeper.TRevealEventInfo }

constructor TMinesweeper.TRevealEventInfo.Create(ASender: TMinesweeper; AFields: TFields);
begin
  inherited Create(ASender);
  FFields := AFields;
end;

destructor TMinesweeper.TRevealEventInfo.Destroy;
begin
  FFields.Free;
  inherited;
end;

function TMinesweeper.TRevealEventInfo.GetFields: TFields.TReader;
begin
  Result := FFields.Reader;
end;

end.
