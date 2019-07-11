unit ReactorDefine;

interface

uses
  System.SysUtils,

  Pengine.IntMaths;

type

  EReactor = class(Exception);

  TReactor = class
  public type

    TBlockType = (
      rbAir,
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

    TBlockTypes = set of TBlockType;

    TBlockData = record
      Name: string;
      DisplayName: string;
      Description: string;
      TextureName: string;
      CoolerValue: Single;
    end;

    TCalculation = class
    public type

      TCellEfficiency = 0 .. 7;
      TModeratorState = (msUnprocessed, msNoCell, msAdjacentCell);
      TCoolerState = (csUnprocessed, csInactive, csActive);

      // Meaning of actual value of TData:
      // Any: 0 -> unprocessed
      // Air: 1 -> processed
      // Cell: 1 .. 7 -> total connected cells via up to 4 moderator blocks in a straight line
      // Moderator: 1 .. 2 -> no ajdacent cell / has adjacent cell
      // Cooler: 1 .. 2 -> inactive / active
      PData = ^TData;

      TData = record
        case Integer of
          0:
            (Processed: ByteBool);
          1:
            (CellEfficiency: TCellEfficiency);
          2:
            (ModeratorState: TModeratorState);
          3:
            (CoolerState: TCoolerState);
      end;

    private
      FReactor: TReactor;
      FCalculated: Boolean;
      FData: array of TData;
      FCellCount: Integer;
      FEfficiency: Single;
      FHeatFactor: Single;
      FCoolingRate: Single;

      function CountRelevantCells(APos: TIntVector3): Integer;
      function CountWalls(APos: TIntVector3): Integer;
      function CountCells(APos: TIntVector3): Integer;
      function CountActive(APos: TIntVector3; ABlockType: TBlockType): Integer;
      function LapisCheck(APos: TIntVector3): Boolean;

      procedure ProcessReactorCell(APos: TIntVector3; AData: PData);
      procedure ProcessModeratorBlock(APos: TIntVector3; AData: PData);
      procedure ProcessCooler(APos: TIntVector3; AData: PData; ACondition: Boolean);

      function GetData(APos: TIntVector3): PData; // inline;

      property Data[APos: TIntVector3]: PData read GetData;

      procedure Calculate;

      function GetCellCount: Integer;
      function GetCoolingRate: Single;
      function GetEfficiency: Single;
      function GetHeatFactor: Single;

    public
      constructor Create(AReactor: TReactor);

      function IsInactiveCooler(APos: TIntVector3): Boolean;

      property Reactor: TReactor read FReactor;

      property CellCount: Integer read GetCellCount;
      property Efficiency: Single read GetEfficiency;
      property HeatFactor: Single read GetHeatFactor;
      property CoolingRate: Single read GetCoolingRate;

      function PowerGeneration(ABasePower: Single): Single;
      function HeatGeneration(ABaseHeat: Single): Single;
      function NetHeatGeneration(ABaseHeat: Single): Single;

    end;

  public const

    SizeLimits: TIntBounds3 = (
      C1: (X: 1; Y: 1; Z: 1);
      C2: (X: 24; Y: 24; Z: 24)
      );

    BlockTypes = [Low(TBlockType) .. High(TBlockType)];
    BlockTypesNoAir = [rbReactorCell .. High(TBlockType)];
    CoolerTypes = [rbWaterCooler .. High(TBlockType)];

    BlockData: array [TBlockType] of TBlockData = (
      // Air
      (DisplayName: 'Air';
      Description: 'An empty space inside of the reactor.'),

      // Reactor Cell
      (DisplayName: 'Reactor Cell';
      Description: 'Generates energy and produces heat.';
      TextureName: 'CELL_BLOCK'),

      // Moderator Block
      (DisplayName: 'Moderator Block';
      Description: 'Increases efficiency when placed besides reactor cells.';
      TextureName: 'INGOT_BLOCK_GRAPHITE'),

      // Cooler
      (DisplayName: 'Water Cooler';
      Description: 'Must touch at least one Reactor Cell or active moderator block.';
      TextureName: 'COOLER_WATER';
      CoolerValue: 20),

      (DisplayName: 'Redstone Cooler';
      Description: 'Must touch at least one Reactor Cell.';
      TextureName: 'COOLER_REDSTONE';
      CoolerValue: 80),

      (DisplayName: 'Quartz Cooler';
      Description: 'Must touch at least one active moderator block.';
      TextureName: 'COOLER_QUARTZ';
      CoolerValue: 80),

      (DisplayName: 'Gold Cooler';
      Description: 'Must touch at least one active Water Cooler and one active Redstone Cooler.';
      TextureName: 'COOLER_GOLD';
      CoolerValue: 120),

      (DisplayName: 'Glowstone Cooler';
      Description: 'Must touch at least two active moderator blocks.';
      TextureName: 'COOLER_GLOWSTONE';
      CoolerValue: 120),

      (DisplayName: 'Lapis Cooler';
      Description: 'Must touch at least one Reactor Cell and one Reactor Casing.';
      TextureName: 'COOLER_LAPIS';
      CoolerValue: 100),

      (DisplayName: 'Diamond Cooler';
      Description: 'Must touch at least one active Water Cooler and one active Quartz Cooler.';
      TextureName: 'COOLER_DIAMOND';
      CoolerValue: 120),

      (DisplayName: 'Liquid Helium Cooler';
      Description: 'Must touch exactly one active Redstone Cooler and at least one Reactor Casing.';
      TextureName: 'COOLER_HELIUM';
      CoolerValue: 120),

      (DisplayName: 'Enderium Cooler';
      Description: 'Must touch exactly three Reactor Casings at exactly one vertex.';
      TextureName: 'COOLER_ENDERIUM';
      CoolerValue: 140),

      (DisplayName: 'Cryotheum Cooler';
      Description: 'Must touch at least two Reactor Cells.';
      TextureName: 'COOLER_CRYOTHEUM';
      CoolerValue: 140),

      (DisplayName: 'Iron Cooler';
      Description: 'Must touch at least one active Gold Cooler.';
      TextureName: 'COOLER_IRON';
      CoolerValue: 60),

      (DisplayName: 'Emerald Cooler';
      Description: 'Must touch at least one active moderator block and one Reactor Cell.';
      TextureName: 'COOLER_EMERALD';
      CoolerValue: 140),

      (DisplayName: 'Copper Cooler';
      Description: 'Must touch at least one active Glowstone Cooler.';
      TextureName: 'COOLER_COPPER';
      CoolerValue: 60),

      (DisplayName: 'Tin Cooler';
      Description: 'Must be at least between two active Lapis Coolers along the same axis.';
      TextureName: 'COOLER_TIN';
      CoolerValue: 80),

      (DisplayName: 'Magnesium Cooler';
      Description: 'Must touch at least one Reactor Casing and one active moderator block.';
      TextureName: 'COOLER_MAGNESIUM';
      CoolerValue: 100)
      );

  private
    FBlocks: array of TBlockType;
    FSize: TIntVector3;
    FCalculation: TCalculation;

    function PosToIndex(const APos: TIntVector3): Integer; inline;
    function IndexToPos(AIndex: Integer): TIntVector3; inline;

    procedure SetSize(const Value: TIntVector3);
    function GetBlock(const APos: TIntVector3): TBlockType;
    procedure SetBlock(const APos: TIntVector3; const Value: TBlockType);

    procedure PosCheck(APos: TIntVector3); inline;

    function GetCalculation: TCalculation;

  public
    constructor Create(ASize: TIntVector3);
    destructor Destroy; override;

    procedure Assign(AFrom: TReactor);
    function Copy: TReactor;

    property Size: TIntVector3 read FSize write SetSize;
    property Blocks[const APos: TIntVector3]: TBlockType read GetBlock write SetBlock; default;
    procedure Clear;

    procedure RemoveInactiveCoolers;

    property Calculation: TCalculation read GetCalculation;

  end;

implementation

{ TReactor }

function TReactor.GetBlock(const APos: TIntVector3): TBlockType;
begin
  if APos in Size then
    Exit(FBlocks[PosToIndex(APos)]);
  Result := rbAir;
end;

function TReactor.GetCalculation: TCalculation;
begin
  if FCalculation = nil then
    FCalculation := TCalculation.Create(Self);
  Result := FCalculation;
end;

function TReactor.IndexToPos(AIndex: Integer): TIntVector3;
begin
  Result.Create(
    AIndex mod Size.X,
    AIndex div Size.X mod Size.Y,
    AIndex div (Size.X * Size.Y) mod Size.Z
    );
end;

procedure TReactor.SetBlock(const APos: TIntVector3; const Value: TBlockType);
begin
  PosCheck(APos);
  FreeAndNil(FCalculation);
  FBlocks[PosToIndex(APos)] := Value;
end;

procedure TReactor.SetSize(const Value: TIntVector3);
begin
  if Size = Value then
    Exit;
  if Value in SizeLimits then
  begin
    FSize := Value;
    SetLength(FBlocks, Size.Volume);
    Clear;
  end
  else
    raise EReactor.CreateFmt('Reactor size must be in range %s.', [SizeLimits.ToString]);
end;

procedure TReactor.Clear;
var
  I: Integer;
begin
  FreeAndNil(FCalculation);
  for I := 0 to Length(FBlocks) - 1 do
    FBlocks[I] := rbAir;
end;

procedure TReactor.TCalculation.ProcessCooler(APos: TIntVector3; AData: PData; ACondition: Boolean);
begin
  if not ACondition then
  begin
    AData.CoolerState := csInactive;
    Exit;
  end;
  AData.CoolerState := csActive;
  FCoolingRate := FCoolingRate + BlockData[Reactor[APos]].CoolerValue;
end;

procedure TReactor.TCalculation.ProcessModeratorBlock(APos: TIntVector3; AData: PData);
var
  Dir: TBasicDir3;
  CheckPos: TIntVector3;
  CellEfficiency: Byte;
begin
  AData.ModeratorState := msNoCell;
  for Dir := Low(TBasicDir3) to High(TBasicDir3) do
  begin
    CheckPos := APos + Vec3Dir[Dir];
    if Reactor[CheckPos] = rbReactorCell then
    begin
      CellEfficiency := Data[CheckPos].CellEfficiency;
      FEfficiency := FEfficiency + CellEfficiency / 6;
      FHeatFactor := FHeatFactor + CellEfficiency / 3;
      AData.ModeratorState := msAdjacentCell;
    end;
  end;
  if AData.ModeratorState = msNoCell then
    FHeatFactor := FHeatFactor + 1;
end;

procedure TReactor.TCalculation.ProcessReactorCell(APos: TIntVector3; AData: PData);
begin
  AData.CellEfficiency := CountRelevantCells(APos) + 1;
  FEfficiency := FEfficiency + AData.CellEfficiency;
  FHeatFactor := FHeatFactor + (Sqr(AData.CellEfficiency) + AData.CellEfficiency) div 2;
  Inc(FCellCount);
end;

function TReactor.Copy: TReactor;
begin
  Result := TReactor.Create(Self.Size);
  Result.Assign(Self);
end;

constructor TReactor.Create(ASize: TIntVector3);
begin
  Size := ASize;
end;

destructor TReactor.Destroy;
begin
  FCalculation.Free;
  inherited;
end;

procedure TReactor.PosCheck(APos: TIntVector3);
begin
  if not(APos in Size) then
    raise EReactor.Create('Reactor block position out of range.');
end;

function TReactor.PosToIndex(const APos: TIntVector3): Integer;
begin
  Result := APos.X + Size.X * (APos.Y + APos.Z * Size.Y);
end;

procedure TReactor.RemoveInactiveCoolers;
var
  Pos: TIntVector3;
  Calc: TCalculation;
begin
  Calc := Calculation;
  for Pos in Size do
    if Calc.IsInactiveCooler(Pos) then
      FBlocks[PosToIndex(Pos)] := rbAir;
  FreeAndNil(FCalculation);
end;

procedure TReactor.Assign(AFrom: TReactor);
var
  I: Integer;
begin
  FreeAndNil(FCalculation);
  Size := AFrom.Size;
  for I := 0 to Length(FBlocks) - 1 do
    FBlocks[I] := AFrom.FBlocks[I];
end;

{ TReactor.TCalculation }

procedure TReactor.TCalculation.Calculate;
var
  Pos: TIntVector3;
begin
  for Pos in Reactor.Size do
    GetData(Pos);

  if FCellCount > 0 then
  begin
    FEfficiency := FEfficiency / FCellCount;
    FHeatFactor := FHeatFactor / FCellCount;
  end
  else
  begin
    FEfficiency := 0;
    FHeatFactor := 0;
  end;
  FCalculated := True;
end;

function TReactor.TCalculation.CountActive(APos: TIntVector3; ABlockType: TBlockType): Integer;
var
  Dir: TBasicDir3;
  CheckPos: TIntVector3;
begin
  Result := 0;
  for Dir := Low(TBasicDir3) to High(TBasicDir3) do
  begin
    CheckPos := APos + Vec3Dir[Dir];
    if (Reactor[CheckPos] = ABlockType) and (Data[CheckPos].CoolerState = csActive) then
      Inc(Result);
  end;
end;

function TReactor.TCalculation.CountCells(APos: TIntVector3): Integer;
var
  Dir: TBasicDir3;
begin
  Result := 0;
  for Dir := Low(TBasicDir3) to High(TBasicDir3) do
    if Reactor[APos + Vec3Dir[Dir]] = rbReactorCell then
      Inc(Result);
end;

function TReactor.TCalculation.CountRelevantCells(APos: TIntVector3): Integer;
var
  Dir: TBasicDir3;
  I: Integer;
  CurrentPos: TIntVector3;
begin
  Result := 0;
  for Dir := Low(TBasicDir3) to High(TBasicDir3) do
  begin
    CurrentPos := APos + Vec3Dir[Dir];
    for I := 1 to 4 do
    begin
      if Reactor[CurrentPos] <> rbModeratorBlock then
        Break;
      CurrentPos := CurrentPos + Vec3Dir[Dir];
    end;
    if Reactor[CurrentPos] = rbReactorCell then
      Inc(Result);
  end;
end;

function TReactor.TCalculation.CountWalls(APos: TIntVector3): Integer;
var
  Axis: TCoordAxis3;
begin
  Result := 0;
  for Axis := Low(TCoordAxis3) to High(TCoordAxis3) do
    if (APos[Axis] = 0) or (APos[Axis] = Reactor.Size[Axis] - 1) then
      Inc(Result);
end;

constructor TReactor.TCalculation.Create(AReactor: TReactor);
begin
  FReactor := AReactor;
  SetLength(FData, Reactor.Size.Volume);
  FillChar(FData[0], SizeOf(TData) * Length(FData), 0);
end;

function TReactor.TCalculation.GetCellCount: Integer;
begin
  if not FCalculated then
    Calculate;
  Result := FCellCount;
end;

function TReactor.TCalculation.GetCoolingRate: Single;
begin
  if not FCalculated then
    Calculate;
  Result := FCoolingRate;
end;

function TReactor.TCalculation.GetData(APos: TIntVector3): PData;
begin
  Assert(APos in Reactor.Size);
  Result := @FData[Reactor.PosToIndex(APos)];
  if not Result.Processed then
  begin
    case Reactor[APos] of
      rbAir:
        Result.Processed := True;
      rbReactorCell:
        ProcessReactorCell(APos, Result);
      rbModeratorBlock:
        ProcessModeratorBlock(APos, Result);
      rbWaterCooler:
        ProcessCooler(APos, Result,
          (CountCells(APos) >= 1) or
          (CountActive(APos, rbModeratorBlock) >= 1));
      rbRedstoneCooler:
        ProcessCooler(APos, Result,
          (CountCells(APos) >= 1));
      rbQuartzCooler:
        ProcessCooler(APos, Result,
          (CountActive(APos, rbModeratorBlock) >= 1));
      rbGoldCooler:
        ProcessCooler(APos, Result,
          (CountActive(APos, rbWaterCooler) >= 1) and
          (CountActive(APos, rbRedstoneCooler) >= 1));
      rbGlowstoneCooler:
        ProcessCooler(APos, Result,
          (CountActive(APos, rbModeratorBlock) >= 2));
      rbLapisCooler:
        ProcessCooler(APos, Result,
          (CountCells(APos) >= 1) and
          (CountWalls(APos) >= 1));
      rbDiamondCooler:
        ProcessCooler(APos, Result,
          (CountActive(APos, rbWaterCooler) >= 1) and
          (CountActive(APos, rbQuartzCooler) >= 1));
      rbLiquidHeliumCooler:
        ProcessCooler(APos, Result,
          (CountActive(APos, rbRedstoneCooler) = 1) and
          (CountWalls(APos) >= 1));
      rbEnderiumCooler:
        ProcessCooler(APos, Result,
          (CountWalls(APos) = 3));
      rbCryotheumCooler:
        ProcessCooler(APos, Result,
          (CountCells(APos) >= 2));
      rbIronCooler:
        ProcessCooler(APos, Result,
          (CountActive(APos, rbGoldCooler) >= 1));
      rbEmeralsCooler:
        ProcessCooler(APos, Result,
          (CountActive(APos, rbModeratorBlock) >= 1) and
          (CountCells(APos) >= 1));
      rbCopperCooler:
        ProcessCooler(APos, Result,
          (CountActive(APos, rbGlowstoneCooler) >= 1));
      rbTinCooler:
        ProcessCooler(APos, Result,
          (LapisCheck(APos)));
      rbMagnesiumCooler:
        ProcessCooler(APos, Result,
          (CountWalls(APos) >= 1) and
          (CountActive(APos, rbModeratorBlock) >= 1));
    else
      raise ENotImplemented.Create('Reactor Block Type not yet implemented.');
    end;
  end;
end;

function TReactor.TCalculation.GetEfficiency: Single;
begin
  if not FCalculated then
    Calculate;
  Result := FEfficiency;
end;

function TReactor.TCalculation.GetHeatFactor: Single;
begin
  if not FCalculated then
    Calculate;
  Result := FHeatFactor;
end;

function TReactor.TCalculation.HeatGeneration(ABaseHeat: Single): Single;
begin
  if not FCalculated then
    Calculate;
  Result := ABaseHeat * CellCount * HeatFactor;
end;

function TReactor.TCalculation.IsInactiveCooler(APos: TIntVector3): Boolean;
begin
  Result := (Reactor[APos] in CoolerTypes) and (Data[APos].CoolerState = csInactive);
end;

function TReactor.TCalculation.LapisCheck(APos: TIntVector3): Boolean;
var
  Axis: TCoordAxis3;
  A, B: TIntVector3;
begin
  for Axis := Low(TCoordAxis3) to High(TCoordAxis3) do
  begin
    A := APos + Vec3Axis[Axis];
    B := APos - Vec3Axis[Axis];
    if (Reactor[A] = rbLapisCooler) and (Data[A].CoolerState = csActive) and
      (Reactor[B] = rbLapisCooler) and (Data[B].CoolerState = csActive) then
      Exit(True);
  end;
  Result := False;
end;

function TReactor.TCalculation.NetHeatGeneration(ABaseHeat: Single): Single;
begin
  if not FCalculated then
    Calculate;
  Result := HeatGeneration(ABaseHeat) - CoolingRate;
end;

function TReactor.TCalculation.PowerGeneration(ABasePower: Single): Single;
begin
  if not FCalculated then
    Calculate;
  Result := ABasePower * CellCount * Efficiency;
end;

end.
