unit ReactorDefine;

interface

uses
  System.SysUtils,

  Pengine.IntMaths;

type

  // Performance Optimizations:
  // - Cache Adjacent Cell Count of Reactor Cells

  TReactor = class;

  TReactorBlockClass = class of TReactorBlock;

  TReactorBlock = class abstract
  public type

    TType = (
      rbReactorCell,
      rbModeratorBlock,
      rbWaterCooler,
      rbRedstoneCooler,
      rbQuartzCooler,
      rbGoldCooler,
      rbGlowstoneCooler,
      rbLapisCooler,
      rbDiamondCooler,
      rbLiquidHeliumCooler,
      rbEnderiumCooler,
      rbCryotheumCooler,
      rbIronCooler,
      rbEmeralsCooler,
      rbCopperCooler,
      rbTinCooler,
      rbMagnesiumCooler
      );

    TCoolerType = rbWaterCooler .. rbMagnesiumCooler;

    TData = record
      Name: string;
      DisplayName: string;
      Description: string;
      CoolerValue: Single;
    end;

  public const

    Data: array [TType] of TData = (
      // Reactor Cell
      (Name: 'Reactor Cell'),

      // Moderator Block
      (Name: 'Moderator Block'),

      // Cooler
      (Name: 'Water Cooler'; CoolerValue: 60),
      (Name: 'Redstone Cooler'; CoolerValue: 90),
      (Name: 'Quartz Cooler'; CoolerValue: 90),
      (Name: 'Gold Cooler'; CoolerValue: 120),
      (Name: 'Glowstone Cooler'; CoolerValue: 130),
      (Name: 'Lapis Cooler'; CoolerValue: 120),
      (Name: 'Diamond Cooler'; CoolerValue: 150),
      (Name: 'Liquid Helium Cooler'; CoolerValue: 140),
      (Name: 'Enderium Cooler'; CoolerValue: 120),
      (Name: 'Cryotheum Cooler'; CoolerValue: 160),
      (Name: 'Iron Cooler'; CoolerValue: 80),
      (Name: 'Emerald Cooler'; CoolerValue: 160),
      (Name: 'Copper Cooler'; CoolerValue: 80),
      (Name: 'Tin Cooler'; CoolerValue: 120),
      (Name: 'Magnesium Cooler'; CoolerValue: 110)
      );

    {
      Water Cooler 	        60 H/t    Must touch at least one Reactor Cell or active moderator block.
      Redstone Cooler 	    90 H/t   	Must touch at least one Reactor Cell.
      Quartz Cooler 	      90 H/t 	  Must touch at least one active moderator block.
      Gold Cooler 	        120 H/t 	Must touch at least one active Water Cooler and one active Redstone Cooler.
      Glowstone Cooler 	    130 H/t 	Must touch at least two active moderator blocks.
      Lapis Cooler 	        120 H/t 	Must touch at least one Reactor Cell and one Reactor Casing.
      Diamond Cooler 	      150 H/t 	Must touch at least one active Water Cooler and one active Quartz Cooler.
      Liquid Helium Cooler 	140 H/t 	Must touch exactly one active Redstone Cooler and at least one Reactor Casing.
      Enderium Cooler       120 H/t 	Must touch exactly three Reactor Casings at exactly one vertex.
      Cryotheum Cooler 	    160 H/t 	Must touch at least two Reactor Cells.
      Iron Cooler 	        80 H/t 	  Must touch at least one active Gold Cooler.
      Emerald Cooler 	      160 H/t 	Must touch at least one active moderator block and one Reactor Cell.
      Copper Cooler 	      80 H/t 	  Must touch at least one active Glowstone Cooler.
      Tin Cooler 	          120 H/t 	Must be at least between two active Lapis Coolers along the same axis.
      Magnesium Cooler 	    110 H/t 	Must touch at least one Reactor Casing and one active moderator block.
    }

  private
    FReactor: TReactor;
    FPos: TIntVector3;

    function GetAdjacentBlock(ADir: TBasicDir3): TReactorBlock;

  public
    constructor Create(AReactor: TReactor; APos: TIntVector3); virtual;

    class function ReactorBlockClass: TReactorBlockClass;

    class function GetType: TType; virtual; abstract;
    class function GetName: string;

    property Reactor: TReactor read FReactor;
    property Pos: TIntVector3 read FPos;

    function CalculateHeat(ABaseHeat: Single): Single; virtual; abstract;
    function CalculateRF(ABaseRF: Single): Single; virtual; abstract;

    function WallsTouching: Integer;

    function IsCorner: Boolean;
    function IsEdge: Boolean;
    function IsWall: Boolean;

    property AdjacentBlock[ADir: TBasicDir3]: TReactorBlock read GetAdjacentBlock;
    function CountAdjacent(AType: TType): Integer;

  end;

  TReactor = class
  private
    FBlocks: array of array of array of TReactorBlock;

    function GetSize: TIntVector3;
    procedure SetSize(const Value: TIntVector3);
    function GetBlock(APos: TIntVector3): TReactorBlock;

    procedure AssertPos(APos: TIntVector3);

  public                        
    constructor Create; overload;
    constructor Create(ASize: TIntVector3); overload;
    destructor Destroy; override;

    procedure Assign(AFrom: TReactor);
    function Copy: TReactor;

    property Size: TIntVector3 read GetSize write SetSize;
    property Blocks[APos: TIntVector3]: TReactorBlock read GetBlock; default;

    function Add(AReactorBlockClass: TReactorBlockClass; APos: TIntVector3): TReactorBlock; overload;
    function Add<T: TReactorBlock>(APos: TIntVector3): T; overload;
    procedure RemoveBlock(APos: TIntVector3);
    procedure Clear;

    function CalculateTotalHeat(ABaseHeat: Single): Single;
    function CalculateTotalRF(ABaseRF: Single): Single;

  end;

  TReactorCell = class(TReactorBlock)
  private
    /// <returns>How many cells are connected either direct or indirect over up to 4 moderator blocks.</returns>
    function CountRelevantCells: Integer;
  
  public
    class function GetType: TReactorBlock.TType; override;
  
    function CalculateEfficency: Single;
    function CalculateHeat(ABaseHeat: Single): Single; override;
    function CalculateRF(ABaseRF: Single): Single; override;

  end;

  TModeratorBlock = class(TReactorBlock)
  public
    class function GetType: TReactorBlock.TType; override;
  
    function CalculateHeat(ABaseHeat: Single): Single; override;
    function CalculateRF(ABaseRF: Single): Single; override;

  end;

  TCooler = class(TReactorBlock)
  public
    function CalculateHeat(ABaseHeat: Single): Single; override;
    function CalculateRF(ABaseRF: Single): Single; override;

  end;

implementation

{ TReactorBlock }

function TReactorBlock.CountAdjacent(AType: TType): Integer;
var
  Dir: TBasicDir3;
  Block: TReactorBlock;
begin
  Result := 0;
  for Dir := Low(TBasicDir3) to High(TBasicDir3) do
  begin
    Block := AdjacentBlock[Dir];
    if (Block <> nil) and (Block.GetType = AType) then
      Inc(Result);
  end;
end;

constructor TReactorBlock.Create(AReactor: TReactor; APos: TIntVector3);
begin
  FReactor := AReactor;
  FPos := APos;
end;

function TReactorBlock.GetAdjacentBlock(ADir: TBasicDir3): TReactorBlock;
var
  AdjacentPos: TIntVector3;
begin
  AdjacentPos := Pos + Vec3Dir[ADir];
  if AdjacentPos in IBounds3(Reactor.Size) then
    Exit(Reactor[AdjacentPos]);
  Result := nil;
end;

class function TReactorBlock.GetName: string;
begin
  Result := Data[GetType].Name;
end;

function TReactorBlock.IsCorner: Boolean;
begin
  Result := WallsTouching = 3;
end;

function TReactorBlock.IsEdge: Boolean;
begin
  Result := WallsTouching >= 2;
end;

function TReactorBlock.IsWall: Boolean;
begin
  Result := WallsTouching >= 1;
end;

class function TReactorBlock.ReactorBlockClass: TReactorBlockClass;
begin
  Result := Self;
end;

function TReactorBlock.WallsTouching: Integer;
var
  Axis: TCoordAxis3;
begin
  Result := 0;
  for Axis := Low(TCoordAxis3) to High(TCoordAxis3) do
    if (Pos[Axis] = 0) or (Pos[Axis] = Reactor.Size[Axis] - 1) then
      Inc(Result);
end;

{ TReactor }

function TReactor.GetBlock(APos: TIntVector3): TReactorBlock;
begin
  Result := FBlocks[APos.X, APos.Y, APos.Z];
end;

function TReactor.GetSize: TIntVector3;
begin
  Result.X := Length(FBlocks);
  if Result.X = 0 then
    Exit(0);
  Result.Y := Length(FBlocks[0]);
  Result.Z := Length(FBlocks[0, 0]);
end;

procedure TReactor.SetSize(const Value: TIntVector3);
begin
  if Size = Value then
    Exit;
  if Value in IBounds3I(1, 24) then
  begin
    Clear;
    SetLength(FBlocks, Value.X, Value.Y, Value.Z);
  end
  else
    raise EArgumentException.Create('Reactor size must be between 1x1x1 and 24x24x24.');
end;

function TReactor.Add(AReactorBlockClass: TReactorBlockClass; APos: TIntVector3): TReactorBlock;
begin
  RemoveBlock(APos);
  Result := AReactorBlockClass.Create(Self, APos);
  FBlocks[APos.X, APos.Y, APos.Z] := Result;         
end;

function TReactor.Add<T>(APos: TIntVector3): T;
begin
  Result := T(Add(T, APos));
end;

procedure TReactor.RemoveBlock(APos: TIntVector3);
begin
  AssertPos(APos);
  FreeAndNil(FBlocks[APos.X, APos.Y, APos.Z]);
end;

procedure TReactor.Clear;
var
  Pos: TIntVector3;
begin
  for Pos in Size do
    FreeAndNil(FBlocks[Pos.X, Pos.Y, Pos.Z]);
end;

function TReactor.Copy: TReactor;
begin
  Result := TReactor.Create;
  Result.Assign(Self);
end;

constructor TReactor.Create;
begin
  // nothing
end;

constructor TReactor.Create(ASize: TIntVector3);
begin
  Size := ASize;
end;

destructor TReactor.Destroy;
begin
  Clear;
  inherited;
end;

procedure TReactor.AssertPos(APos: TIntVector3);
begin
  if not(APos in IBounds3(Size)) then
    raise EArgumentOutOfRangeException.Create('Reactor block position out of range.');
end;

procedure TReactor.Assign(AFrom: TReactor);
var
  Pos: TIntVector3;
begin
  Size := AFrom.Size;
  for Pos in Size do
    Add(AFrom[Pos].ReactorBlockClass, Pos);
end;

function TReactor.CalculateTotalHeat(ABaseHeat: Single): Single;
var
  Pos: TIntVector3;
  Block: TReactorBlock;
begin
  Result := 0;
  for Pos in Size do
  begin
    Block := Blocks[Pos];
    if Block <> nil then
      Result := Result + Blocks[Pos].CalculateHeat(ABaseHeat);
  end;
end;

function TReactor.CalculateTotalRF(ABaseRF: Single): Single;
var
  Pos: TIntVector3;
  Block: TReactorBlock;
begin
  Result := 0;
  for Pos in Size do
  begin
    Block := Blocks[Pos];
    if Block <> nil then
      Result := Result + Blocks[Pos].CalculateRF(ABaseRF);
  end;
end;

{ TReactorCell }

function TReactorCell.CalculateEfficency: Single;
begin
  Result := 1 + CountRelevantCells;
end;

function TReactorCell.CalculateHeat(ABaseHeat: Single): Single;
var
  N: Integer;
begin
  N := CountAdjacent(rbReactorCell);
  Result := ABaseHeat * (N + 1) * (N + 2) / 2;
end;

function TReactorCell.CalculateRF(ABaseRF: Single): Single;
begin
  Result := ABaseRF * CalculateEfficency;
end;

function TReactorCell.CountRelevantCells: Integer;
var
  Dir: TBasicDir3;
  I: Integer;
  BlockPos: TIntVector3;
  Block: TReactorBlock;
begin
  Result := 0;
  for Dir := Low(TBasicDir3) to High(TBasicDir3) do
  begin  
    BlockPos := Pos;
    for I := 1 to 5 do
    begin
      BlockPos := Vec3Dir[Dir];
      if not (BlockPos in IBounds3(Reactor.Size)) then
        Break;
      Block := Reactor[BlockPos];
      if Block is TReactorCell then
      begin
        Inc(Result);
        Break;
      end;
      if (I <> 5) and not (Block is TModeratorBlock) then
        Break;
    end;
  end;
end;

class function TReactorCell.GetType: TReactorBlock.TType;
begin
  Result := rbReactorCell;
end;

{ TModeratorBlock }

function TModeratorBlock.CalculateHeat(ABaseHeat: Single): Single;
var
  Dir: TBasicDir3;
  Adjacent: TReactorBlock;
  HasCell: Boolean;
begin
  Result := 0;
  HasCell := False;
  for Dir := Low(TBasicDir3) to High(TBasicDir3) do
  begin
    Adjacent := AdjacentBlock[Dir];
    if Adjacent is TReactorCell then
    begin
      HasCell := True;
      Result := Result + ABaseHeat * (TReactorCell(Adjacent).CalculateEfficency / 3);
    end;
  end;
  if not HasCell then
    Result := ABaseHeat;
end;

function TModeratorBlock.CalculateRF(ABaseRF: Single): Single;
var
  Dir: TBasicDir3;
  Adjacent: TReactorBlock;
begin
  Result := 0;
  for Dir := Low(TBasicDir3) to High(TBasicDir3) do
  begin
    Adjacent := AdjacentBlock[Dir];
    if Adjacent is TReactorCell then
      Result := Result + ABaseRF * (TReactorCell(Adjacent).CalculateEfficency / 6);
  end;
end;

class function TModeratorBlock.GetType: TReactorBlock.TType;
begin
  Result := rbModeratorBlock;
end;

{ TCooler }

function TCooler.CalculateHeat(ABaseHeat: Single): Single;
begin
  Result := -Data[GetType].CoolerValue;
end;

function TCooler.CalculateRF(ABaseRF: Single): Single;
begin
  Result := 0;
end;

end.
