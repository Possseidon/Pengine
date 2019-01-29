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

    TCoolerCondition = function(AReactor: TReactor; APos: TIntVector3): Boolean;

    TBlockData = record
      // Name: string;
      DisplayName: string;
      // Description: string;
      CoolerValue: Single;
      Condition: TCoolerCondition;
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
      (DisplayName: 'Water Cooler'; CoolerValue: 60),
      (DisplayName: 'Redstone Cooler'; CoolerValue: 90),
      (DisplayName: 'Quartz Cooler'; CoolerValue: 90),
      (DisplayName: 'Gold Cooler'; CoolerValue: 120),
      (DisplayName: 'Glowstone Cooler'; CoolerValue: 130),
      (DisplayName: 'Lapis Cooler'; CoolerValue: 120),
      (DisplayName: 'Diamond Cooler'; CoolerValue: 150),
      (DisplayName: 'Liquid Helium Cooler'; CoolerValue: 140),
      (DisplayName: 'Enderium Cooler'; CoolerValue: 120),
      (DisplayName: 'Cryotheum Cooler'; CoolerValue: 160),
      (DisplayName: 'Iron Cooler'; CoolerValue: 80),
      (DisplayName: 'Emerald Cooler'; CoolerValue: 160),
      (DisplayName: 'Copper Cooler'; CoolerValue: 80),
      (DisplayName: 'Tin Cooler'; CoolerValue: 120),
      (DisplayName: 'Magnesium Cooler'; CoolerValue: 110)
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
    FBlocks: array of array of array of TBlockType;

    function GetSize: TIntVector3;
    procedure SetSize(const Value: TIntVector3);
    function GetBlock(APos: TIntVector3): TBlockType;
    procedure SetBlock(APos: TIntVector3; const Value: TBlockType);

    procedure AssertPos(APos: TIntVector3);

  public
    constructor Create(ASize: TIntVector3);

    procedure Assign(AFrom: TReactor);
    function Copy: TReactor;

    property Size: TIntVector3 read GetSize write SetSize;
    property Blocks[APos: TIntVector3]: TBlockType read GetBlock write SetBlock; default;
    procedure Clear;

  end;

  TReactorCalculator = class
  private type

    TBlockData = record
      Efficiency: Single;
      GeneratedHeat: Single;
      GeneratedRF: Single;
    end;

  private
    FReactor: TReactor;
    FBlockData: array of array of array of TBlockData;

  public
    constructor Create(AReactor: TReactor);

  end;

implementation

{ TReactor }

function TReactor.GetBlock(APos: TIntVector3): TBlockType;
begin
  if APos in Size then
    Exit(FBlocks[APos.X, APos.Y, APos.Z]);
  Result := rbAir;
end;

function TReactor.GetSize: TIntVector3;
begin
  Result.X := Length(FBlocks);
  if Result.X = 0 then
    Exit(0);
  Result.Y := Length(FBlocks[0]);
  Result.Z := Length(FBlocks[0, 0]);
end;

procedure TReactor.SetBlock(APos: TIntVector3; const Value: TBlockType);
begin
  AssertPos(APos);
  FBlocks[APos.X, APos.Y, APos.Z] := Value;
end;

procedure TReactor.SetSize(const Value: TIntVector3);
begin
  if Size = Value then
    Exit;
  if Value in IBounds3I(1, 24) then
    SetLength(FBlocks, Value.X, Value.Y, Value.Z)
  else
    raise EArgumentException.Create('Reactor size must be between 1x1x1 and 24x24x24.');
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
  if not(APos in IBounds3(Size)) then
    raise EArgumentOutOfRangeException.Create('Reactor block position out of range.');
end;

procedure TReactor.Assign(AFrom: TReactor);
var
  Pos: TIntVector3;
begin
  Size := AFrom.Size;
  for Pos in Size do
    Self[Pos] := AFrom[Pos];
end;

end.
