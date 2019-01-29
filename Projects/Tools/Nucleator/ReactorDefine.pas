unit ReactorDefine;

interface

uses
  System.SysUtils,

  Pengine.IntMaths;

type

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

    TCoolerType = rbWaterCooler .. rbMagnesiumCooler;

    TBlockData = record
      // Name: string;
      DisplayName: string;
      // Description: string;
      CoolerValue: Single;
    end;

  public const

    Coolers = [Low(TCoolerType) .. High(TCoolerType)];

    BlockData: array [TBlockType] of TBlockData = (
      // Air
      (DisplayName: 'Air'),

      // Reactor Cell
      (DisplayName: 'Reactor Cell'),

      // Moderator Block
      (DisplayName: 'Moderator Block'),

      // Cooler
      (DisplayName: 'Water Cooler'; CoolerValue: 20),
      (DisplayName: 'Redstone Cooler'; CoolerValue: 80),
      (DisplayName: 'Quartz Cooler'; CoolerValue: 80),
      (DisplayName: 'Gold Cooler'; CoolerValue: 120),
      (DisplayName: 'Glowstone Cooler'; CoolerValue: 120),
      (DisplayName: 'Lapis Cooler'; CoolerValue: 100),
      (DisplayName: 'Diamond Cooler'; CoolerValue: 120),
      (DisplayName: 'Liquid Helium Cooler'; CoolerValue: 120),
      (DisplayName: 'Enderium Cooler'; CoolerValue: 140),
      (DisplayName: 'Cryotheum Cooler'; CoolerValue: 140),
      (DisplayName: 'Iron Cooler'; CoolerValue: 60),
      (DisplayName: 'Emerald Cooler'; CoolerValue: 140),
      (DisplayName: 'Copper Cooler'; CoolerValue: 60),
      (DisplayName: 'Tin Cooler'; CoolerValue: 80),
      (DisplayName: 'Magnesium Cooler'; CoolerValue: 100)
      );

    {
      Water Cooler 	         20 h/t   Must touch at least one Reactor Cell or active moderator block.
      Redstone Cooler 	     80 h/t  	Must touch at least one Reactor Cell.
      Quartz Cooler 	       80 h/t	  Must touch at least one active moderator block.
      Gold Cooler 	         120 h/t 	Must touch at least one active Water Cooler and one active Redstone Cooler.
      Glowstone Cooler 	     120 h/t 	Must touch at least two active moderator blocks.
      Lapis Cooler 	         100 h/t 	Must touch at least one Reactor Cell and one Reactor Casing.
      Diamond Cooler 	       120 h/t 	Must touch at least one active Water Cooler and one active Quartz Cooler.
      Liquid Helium Cooler 	 120 h/t 	Must touch exactly one active Redstone Cooler and at least one Reactor Casing.
      Enderium Cooler        140 h/t 	Must touch exactly three Reactor Casings at exactly one vertex.
      Cryotheum Cooler 	     140 h/t 	Must touch at least two Reactor Cells.
      Iron Cooler 	         60 h/t	  Must touch at least one active Gold Cooler.
      Emerald Cooler 	       140 h/t 	Must touch at least one active moderator block and one Reactor Cell.
      Copper Cooler 	       60 h/t	  Must touch at least one active Glowstone Cooler.
      Tin Cooler 	           80 h/t 	Must be at least between two active Lapis Coolers along the same axis.
      Magnesium Cooler 	     100 h/t 	Must touch at least one Reactor Casing and one active moderator block.
    }

  private
    FBlocks: array of array of array of TBlockType;
    FCellCount: Integer;
    FCalculated: Boolean;
    FEfficiency: Single;
    FHeatFactor: Single;
    FCoolingRate: Single;

    function GetSize: TIntVector3;
    procedure SetSize(const Value: TIntVector3);
    function GetBlock(APos: TIntVector3): TBlockType;
    procedure SetBlock(APos: TIntVector3; const Value: TBlockType);

    procedure AssertPos(APos: TIntVector3);

    function GetCellCount: Integer;
    function GetEfficiency: Single;
    function GetHeatFactor: Single;
    function GetCoolingRate: Single;

    // Calculation
    procedure Calculate;
    procedure EnsureCalculated; inline;

  public
    constructor Create(ASize: TIntVector3);

    procedure Assign(AFrom: TReactor);
    function Copy: TReactor;

    property Size: TIntVector3 read GetSize write SetSize;
    property Blocks[APos: TIntVector3]: TBlockType read GetBlock write SetBlock; default;
    procedure Clear;

    property CellCount: Integer read GetCellCount;
    property Efficiency: Single read GetEfficiency;
    property HeatFactor: Single read GetHeatFactor;
    property CoolingRate: Single read GetCoolingRate;

    function PowerGeneration(ABasePower: Single): Single;
    function HeatGeneration(ABaseHeat: Single): Single;

  end;

implementation

{ TReactor }

function TReactor.GetBlock(APos: TIntVector3): TBlockType;
begin
  if APos in Size then
    Exit(FBlocks[APos.X, APos.Y, APos.Z]);
  Result := rbAir;
end;

function TReactor.GetCellCount: Integer;
begin
  EnsureCalculated;
  Result := FCellCount;
end;

function TReactor.GetCoolingRate: Single;
begin
  EnsureCalculated;
  Result := FCoolingRate;
end;

function TReactor.GetEfficiency: Single;
begin
  EnsureCalculated;
  Result := FEfficiency;
end;

function TReactor.GetHeatFactor: Single;
begin
  EnsureCalculated;
  Result := FHeatFactor;
end;

function TReactor.GetSize: TIntVector3;
begin
  Result.X := Length(FBlocks);
  if Result.X = 0 then
    Exit(0);
  Result.Y := Length(FBlocks[0]);
  Result.Z := Length(FBlocks[0, 0]);
end;

function TReactor.HeatGeneration(ABaseHeat: Single): Single;
begin
  Result := ABaseHeat * CellCount * HeatFactor - CoolingRate;
end;

function TReactor.PowerGeneration(ABasePower: Single): Single;
begin
  Result := ABasePower * CellCount * Efficiency;
end;

procedure TReactor.SetBlock(APos: TIntVector3; const Value: TBlockType);
begin
  AssertPos(APos);
  FBlocks[APos.X, APos.Y, APos.Z] := Value;
  FCalculated := False;
end;

procedure TReactor.SetSize(const Value: TIntVector3);
begin
  if Size = Value then
    Exit;
  if Value in IBounds3I(1, 24) then
  begin
    FCalculated := False;
    SetLength(FBlocks, Value.X, Value.Y, Value.Z);
  end
  else
    raise EArgumentException.Create('Reactor size must be between 1x1x1 and 24x24x24.');
end;

procedure TReactor.EnsureCalculated;
begin
  if FCalculated then
    Exit;
  Calculate;
end;

procedure TReactor.Calculate;
var
  Data: array of array of array of Byte;
  ResultHeatFactor, ResultEfficiency, ResultCoolingRate: Single;
  ResultCellCount: Integer;
  Size: TIntVector3;

  function CountRelevantCells(APos: TIntVector3): Integer;
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
        if Blocks[CurrentPos] <> rbModeratorBlock then
          Break;
        CurrentPos := CurrentPos + Vec3Dir[Dir];
      end;
      if Blocks[CurrentPos] = rbReactorCell then
        Inc(Result);
    end;
  end;

  function ProcessCell(const APos: TIntVector3): Byte;
  begin
    Result := CountRelevantCells(APos) + 1;
    Data[APos.X, APos.Y, APos.Z] := Result;
    ResultEfficiency := ResultEfficiency + Result;
    ResultHeatFactor := ResultHeatFactor + (Sqr(Result) + Result) div 2;
    Inc(ResultCellCount);
  end;

  function GetCell(const APos: TIntVector3): Byte;
  begin
    if not(APos in Size) then
      Exit(0);
    Result := Data[APos.X, APos.Y, APos.Z];
    if Result = 0 then
      Result := ProcessCell(APos);
  end;

  function ProcessModerator(const APos: TIntVector3): Byte;
  var
    Dir: TBasicDir3;
    CheckPos: TIntVector3;
    CellEfficiency: Byte;
  begin
    Result := 1;
    for Dir := Low(TBasicDir3) to High(TBasicDir3) do
    begin
      CheckPos := APos + Vec3Dir[Dir];
      if Blocks[CheckPos] = rbReactorCell then
      begin
        CellEfficiency := GetCell(CheckPos);
        ResultEfficiency := ResultEfficiency + CellEfficiency / 6;
        ResultHeatFactor := ResultHeatFactor + CellEfficiency / 3;
        Result := 2;
      end;
    end;
    if Result = 1 then
      ResultHeatFactor := ResultHeatFactor + 1;
    Data[APos.X, APos.Y, APos.Z] := Result;
  end;

  function GetModerator(const APos: TIntVector3): Byte;
  begin
    if not(APos in Size) then
      Exit(0);
    Result := Data[APos.X, APos.Y, APos.Z];
    if Result = 0 then
      Result := ProcessModerator(APos);
  end;

  function Process(const APos: TIntVector3): Byte;

    function Get(const APos: TIntVector3): Byte;
    begin
      if not(APos in Size) then
        Exit(0);
      Result := Data[APos.X, APos.Y, APos.Z];
      if Result = 0 then
        Result := Process(APos);
    end;

    function CountWalls(APos: TIntVector3): Integer;
    var
      Axis: TCoordAxis3;
    begin
      Result := 0;
      for Axis := Low(TCoordAxis3) to High(TCoordAxis3) do
        if APos[Axis] mod (Size[Axis] - 1) = 0 then
          Inc(Result);
    end;

    function CountCells(APos: TIntVector3): Integer;
    var
      Dir: TBasicDir3;
    begin
      Result := 0;
      for Dir := Low(TBasicDir3) to High(TBasicDir3) do
        if Blocks[APos + Vec3Dir[Dir]] = rbReactorCell then
          Inc(Result);
    end;

    function CountActive(APos: TIntVector3; ABlockType: TBlockType): Integer;
    var
      Dir: TBasicDir3;
      CheckPos: TIntVector3;
    begin
      Result := 0;
      for Dir := Low(TBasicDir3) to High(TBasicDir3) do
      begin
        CheckPos := APos + Vec3Dir[Dir];
        if (Blocks[CheckPos] = ABlockType) and (Get(CheckPos) = 2) then
          Inc(Result);
      end;
    end;

    function CoolerCheck(ACondition: Boolean): Byte; inline;
    begin
      if ACondition then
        Exit(2);
      Result := 1;
    end;

    function CheckLapisAxis(APos: TIntVector3): Boolean;
    var
      Axis: TCoordAxis3;
      A, B: TIntVector3;
    begin
      for Axis := Low(TCoordAxis3) to High(TCoordAxis3) do
      begin
        A := APos + Vec3Axis[Axis];
        B := APos - Vec3Axis[Axis];
        if (A in Size) and (B in Size) and
          (Blocks[A] = rbLapisCooler) and (Get(A) = 2) and
          (Blocks[B] = rbLapisCooler) and (Get(B) = 2) then
          Exit(True);
      end;
      Result := False;
    end;

  var
    BlockType: TBlockType;
  begin
    BlockType := Blocks[APos];
    case BlockType of
      rbReactorCell:
        Exit(GetCell(APos));
      rbModeratorBlock:
        Exit(GetModerator(APos));
      rbWaterCooler:
        Result := CoolerCheck((CountCells(APos) >= 1) or (CountActive(APos, rbModeratorBlock) >= 1));
      rbRedstoneCooler:
        Result := CoolerCheck(CountCells(APos) >= 1);
      rbQuartzCooler:
        Result := CoolerCheck(CountActive(APos, rbModeratorBlock) >= 1);
      rbGoldCooler:
        Result := CoolerCheck((CountActive(APos, rbWaterCooler) >= 1) and (CountActive(APos, rbRedstoneCooler) >= 1));
      rbGlowstoneCooler:
        Result := CoolerCheck(CountActive(APos, rbModeratorBlock) >= 2);
      rbLapisCooler:
        Result := CoolerCheck((CountCells(APos) >= 1) and (CountWalls(APos) >= 1));
      rbDiamondCooler:
        Result := CoolerCheck((CountActive(APos, rbWaterCooler) >= 1) and (CountActive(APos, rbQuartzCooler) >= 1));
      rbLiquidHeliumCooler:
        Result := CoolerCheck((CountActive(APos, rbRedstoneCooler) = 1) and (CountWalls(APos) >= 1));
      rbEnderiumCooler:
        Result := CoolerCheck(CountWalls(APos) = 3);
      rbCryotheumCooler:
        Result := CoolerCheck(CountCells(APos) >= 2);
      rbIronCooler:
        Result := CoolerCheck(CountActive(APos, rbGoldCooler) >= 1);
      rbEmeralsCooler:
        Result := CoolerCheck((CountActive(APos, rbModeratorBlock) >= 1) and (CountCells(APos) >= 1));
      rbCopperCooler:
        Result := CoolerCheck(CountActive(APos, rbGlowstoneCooler) >= 1);
      rbTinCooler:
        Result := CoolerCheck(CheckLapisAxis(APos));
      rbMagnesiumCooler:
        Result := CoolerCheck((CountWalls(APos) >= 1) and (CountActive(APos, rbModeratorBlock) >= 1));
    else
      // rbAir
      Exit(0);
    end;
    Data[APos.X, APos.Y, APos.Z] := Result;
    if Result = 2 then
      ResultCoolingRate := ResultCoolingRate + BlockData[BlockType].CoolerValue;
  end;

var
  Pos: TIntVector3;
begin
  ResultCellCount := 0;
  ResultEfficiency := 0;
  ResultHeatFactor := 0;

  Size := Self.Size;
  SetLength(Data, Size.X, Size.Y, Size.Z);

  for Pos in Size do
    Process(Pos);

  FCellCount := ResultCellCount;
  FEfficiency := ResultEfficiency / ResultCellCount;
  FHeatFactor := ResultHeatFactor / ResultCellCount;
  FCoolingRate := ResultCoolingRate;

  FCalculated := True;
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
  Result := TReactor.Create(Self.Size);
  Result.Assign(Self);
end;

constructor TReactor.Create(ASize: TIntVector3);
begin
  Size := ASize;
end;

procedure TReactor.AssertPos(APos: TIntVector3);
begin
  if not(APos in Size) then
    raise EArgumentOutOfRangeException.Create('Reactor block position out of range.');
end;

procedure TReactor.Assign(AFrom: TReactor);
var
  Pos: TIntVector3;
begin
  Size := AFrom.Size;
  for Pos in Size do
    Self[Pos] := AFrom[Pos];

  FCalculated := AFrom.FCalculated;
  FEfficiency := AFrom.FEfficiency;
  FHeatFactor := AFrom.FHeatFactor;
end;

end.
