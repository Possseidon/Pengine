unit ReactorEvolutionDefine;

interface

uses
  System.SysUtils,
  System.Math,

  Pengine.ICollections,
  Pengine.IntMaths,

  ReactorDefine;

type

  IEvolutionSettings = interface;

  TReactorRating = class
  private
    FReactor: TReactor;
    FGeneration: Integer;
    FSettings: IEvolutionSettings;
    FPowerGeneration: Single;
    FHeatGeneration: Single;
    FNetHeatGeneration: Single;
    FFitness: Single;

    function GetCellCount: Integer;
    function GetCoolingRate: Single;
    function GetEfficiency: Single;
    function GetHeatFactor: Single;

  public
    constructor Create(AReactor: TReactor; AGeneration: Integer; ASettings: IEvolutionSettings);
    destructor Destroy; override;

    property Reactor: TReactor read FReactor;
    property Settings: IEvolutionSettings read FSettings;
    property Generation: Integer read FGeneration;

    property CellCount: Integer read GetCellCount;
    property Efficiency: Single read GetEfficiency;
    property HeatFactor: Single read GetHeatFactor;
    property CoolingRate: Single read GetCoolingRate;

    property PowerGeneration: Single read FPowerGeneration;
    property HeatGeneration: Single read FHeatGeneration;
    property NetHeatGeneration: Single read FNetHeatGeneration;

    property Fitness: Single read FFitness;

    function Copy: TReactorRating;

  end;

  TEvolutionGeneration = class
  public type

    TStats = record
      Best: Single;
      Worst: Single;
      Average: Single;
    end;

  private
    FSettings: IEvolutionSettings;
    FIndex: Integer;
    FReactorRatings: ISortedObjectList<TReactorRating>;

    function GetPopulationSize: Integer;

    function GetBest: TReactorRating;
    function GetWorst: TReactorRating;

    function GetFitnessStats: TStats;
    function GetEfficiencyStats: TStats;
    function GetPowerGenerationStats: TStats;
    function GetNetHeatGenerationStats: TStats;

    function CreateStats(AValueExtractor: TFunc<TReactorRating, Single>; ALowerBetter: Boolean = False): TStats;
    function GetSettings: IEvolutionSettings;
    function GetReactorRatings: IReadonlyList<TReactorRating>;

  public
    constructor Create(ASettings: IEvolutionSettings; AParentGeneration: TEvolutionGeneration = nil); overload;

    property Index: Integer read FIndex;
    property Settings: IEvolutionSettings read GetSettings;

    /// <summary>Ordered by fitness from best to worst.</summary>
    property ReactorRatings: IReadonlyList<TReactorRating> read GetReactorRatings;
    /// <summary>The reactor count.</summary>
    property PopulationSize: Integer read GetPopulationSize;

    /// <summary>The reactor with the best fitness of this generation.</summary>
    property Best: TReactorRating read GetBest;
    /// <summary>The reactor with the worst fitness of this generation.</summary>
    property Worst: TReactorRating read GetWorst;

    /// <summary>Worst, average and best fitnesses of this generation.</summary>
    property FitnessStats: TStats read GetFitnessStats;
    /// <summary>Worst, average and best efficiency of this generation.</summary>
    property EfficiencyStats: TStats read GetEfficiencyStats;
    /// <summary>Worst, average and best power generation of this generation.</summary>
    property PowerGenerationStats: TStats read GetPowerGenerationStats;
    /// <summary>Worst, average and best heat generation of this generation.</summary>
    property NetHeatGenerationStats: TStats read GetNetHeatGenerationStats;

  end;

  TEvolutionFunctionClass = class of TEvolutionFunction;

  IEvolutionFunction = interface
    ['{C2A9C568-DB20-492F-95BD-E27A89BDA8CD}']
    function GetDisplayName: string;

    function FunctionClass: TEvolutionFunctionClass;

    procedure Assign(AFrom: IEvolutionFunction);
    function Copy(ASettings: IEvolutionSettings): IEvolutionFunction;

  end;

  IFitnessFunction = interface(IEvolutionFunction)
    ['{CBC54E5B-6AB4-47A6-9A1A-B245A4297997}']
    function Calculate(AReactor: TReactor; ASettings: IEvolutionSettings): Single;

    function Copy(ASettings: IEvolutionSettings): IFitnessFunction;

  end;

  IGeneratorFunction = interface(IEvolutionFunction)
    ['{3CC45E2C-4779-4FE4-AC75-51F3C59BD7A4}']
    function Generator: IIterable<TReactor>;

    function Copy(ASettings: IEvolutionSettings): IGeneratorFunction;

  end;

  IMutationFunction = interface(IGeneratorFunction)
    ['{3662408E-35BE-4076-AD04-BDC5B29B0824}']
    function GetParentGeneration: TEvolutionGeneration;
    procedure SetParentGeneration(const Value: TEvolutionGeneration);
    property ParentGeneration: TEvolutionGeneration read GetParentGeneration write SetParentGeneration;

    function Copy(ASettings: IEvolutionSettings): IMutationFunction;

  end;

  TGeneratorFunctionClass = class of TGeneratorFunction;
  TMutationFunctionClass = class of TMutationFunction;
  TFitnessFunctionClass = class of TFitnessFunction;

  IEvolutionSettings = interface
    ['{A5E4086D-B8FB-41D6-858A-21D51E286790}']
    function GetReactorSize: TIntVector3;
    procedure SetReactorSize(const Value: TIntVector3);
    function GetReactorSizeX: Integer;
    procedure SetReactorSizeX(const Value: Integer);
    function GetReactorSizeY: Integer;
    procedure SetReactorSizeY(const Value: Integer);
    function GetReactorSizeZ: Integer;
    procedure SetReactorSizeZ(const Value: Integer);

    function GetFuelBasePower: Single;
    procedure SetFuelBasePower(const Value: Single);
    function GetFuelBaseHeat: Single;
    procedure SetFuelBaseHeat(const Value: Single);

    function GetBlockTypes: TReactor.TBlockTypes;
    procedure SetBlockTypes(const Value: TReactor.TBlockTypes);
    function GetBlockType(ABlockType: TReactor.TBlockType): Boolean;
    procedure SetBlockType(ABlockType: TReactor.TBlockType; const Value: Boolean);
    function GetBlockTypeList: IList<TReactor.TBlockType>;

    function GetPopulationSize: Integer;
    procedure SetPopulationSize(const Value: Integer);
    function GetGeneratorFunction: IGeneratorFunction;
    function GetMutationFunction: IMutationFunction;
    function GetFitnessFunction: IFitnessFunction;

    property ReactorSize: TIntVector3 read GetReactorSize write SetReactorSize;
    property ReactorSizeX: Integer read GetReactorSizeX write SetReactorSizeX;
    property ReactorSizeY: Integer read GetReactorSizeY write SetReactorSizeY;
    property ReactorSizeZ: Integer read GetReactorSizeZ write SetReactorSizeZ;

    property FuelBasePower: Single read GetFuelBasePower write SetFuelBasePower;
    property FuelBaseHeat: Single read GetFuelBaseHeat write SetFuelBaseHeat;

    property BlockTypes: TReactor.TBlockTypes read GetBlockTypes write SetBlockTypes;
    property HasBlockType[ABlockType: TReactor.TBlockType]: Boolean read GetBlockType write SetBlockType;
    property BlockTypeList: IList<TReactor.TBlockType> read GetBlockTypeList;

    property PopulationSize: Integer read GetPopulationSize write SetPopulationSize;
    property GeneratorFunction: IGeneratorFunction read GetGeneratorFunction;
    property MutationFunction: IMutationFunction read GetMutationFunction;
    property FitnessFunction: IFitnessFunction read GetFitnessFunction;

    procedure SetGeneratorFunction(AClass: TGeneratorFunctionClass);
    procedure SetMutationFunction(AClass: TMutationFunctionClass);
    procedure SetFitnessFunction(AClass: TFitnessFunctionClass);

    procedure Assign(AFrom: IEvolutionSettings);
    function Copy: IEvolutionSettings;

  end;

  TEvolutionFunction = class abstract(TInterfacedObject, IEvolutionFunction)
  private
    [Weak]
    FSettings: IEvolutionSettings;

    function GetDisplayNameI: string;
    function IEvolutionFunction.GetDisplayName = GetDisplayNameI;

    function FunctionClassI: TEvolutionFunctionClass;
    function IEvolutionFunction.FunctionClass = FunctionClassI;

  public
    constructor Create(ASettings: IEvolutionSettings); virtual;

    class function GetDisplayName: string; virtual; abstract;

    property Settings: IEvolutionSettings read FSettings;

    class function FunctionClass: TEvolutionFunctionClass;

    procedure Assign(AFrom: IEvolutionFunction); virtual;
    function Copy(ASettings: IEvolutionSettings): IEvolutionFunction;

  end;

  TFitnessFunction = class abstract(TEvolutionFunction, IFitnessFunction)
  public
    function Calculate(AReactor: TReactor; ASettings: IEvolutionSettings): Single; virtual; abstract;

    function Copy(ASettings: IEvolutionSettings): IFitnessFunction;

  end;

  TGeneratorFunction = class abstract(TEvolutionFunction, IGeneratorFunction, IIterable<TReactor>)
  public
    function Generator: IIterable<TReactor>;

    function GetEnumerator: IIterator<TReactor>; virtual; abstract;
    function Iterate: IIterate<TReactor>;

    function Copy(ASettings: IEvolutionSettings): IGeneratorFunction;

  end;

  TReactorGenerator = class abstract(TInterfacedObject, IIterator<TReactor>)
  private
    FSettings: IEvolutionSettings;
    FCurrentIndex: Integer;
    FCurrent: TReactor;

    function GetCurrent: TReactor;

  protected
    procedure Generate(AReactor: TReactor; AIndex: Integer); virtual; abstract;

  public
    constructor Create(ASettings: IEvolutionSettings); virtual;

    function MoveNext: Boolean;
    property Current: TReactor read GetCurrent;

    property Settings: IEvolutionSettings read FSettings;

  end;

  TReactorMutator = class(TReactorGenerator)
  private
    FParentGeneration: TEvolutionGeneration;

  public
    constructor Create(AParentGeneration: TEvolutionGeneration); reintroduce; virtual;

    property ParentGeneration: TEvolutionGeneration read FParentGeneration;

  end;

  /// <summary>Generates a new generation of reactors from a given generation.</summary>
  TMutationFunction = class abstract(TGeneratorFunction, IMutationFunction)
  private
    FParentGeneration: TEvolutionGeneration;

    function GetParentGeneration: TEvolutionGeneration;
    procedure SetParentGeneration(const Value: TEvolutionGeneration);

  public
    property ParentGeneration: TEvolutionGeneration read GetParentGeneration write SetParentGeneration;

    function Copy(ASettings: IEvolutionSettings): IMutationFunction;

  end;

  TEvolutionSettings = class(TInterfacedObject, IEvolutionSettings)
  private
    FReactorSize: TIntVector3;
    FFuelBasePower: Single;
    FFuelBaseHeat: Single;
    FPopulationSize: Integer;
    FGeneratorFunction: IGeneratorFunction;
    FMutationFunction: IMutationFunction;
    FFitnessFunction: IFitnessFunction;
    FBlockTypes: TReactor.TBlockTypes;
    FBlockTypeList: IList<TReactor.TBlockType>;

    function GetReactorSize: TIntVector3;
    procedure SetReactorSize(const Value: TIntVector3);
    function GetReactorSizeX: Integer;
    procedure SetReactorSizeX(const Value: Integer);
    function GetReactorSizeY: Integer;
    procedure SetReactorSizeY(const Value: Integer);
    function GetReactorSizeZ: Integer;
    procedure SetReactorSizeZ(const Value: Integer);

    function GetFuelBasePower: Single;
    procedure SetFuelBasePower(const Value: Single);
    function GetFuelBaseHeat: Single;
    procedure SetFuelBaseHeat(const Value: Single);

    function GetBlockTypes: TReactor.TBlockTypes;
    procedure SetBlockTypes(const Value: TReactor.TBlockTypes);
    function GetBlockType(ABlockType: TReactor.TBlockType): Boolean;
    procedure SetBlockType(ABlockType: TReactor.TBlockType; const Value: Boolean);
    function GetBlockTypeList: IList<TReactor.TBlockType>;

    function GetPopulationSize: Integer;
    procedure SetPopulationSize(const Value: Integer);
    function GetGeneratorFunction: IGeneratorFunction;
    function GetMutationFunction: IMutationFunction;
    function GetFitnessFunction: IFitnessFunction;

  public
    constructor Create;

    property ReactorSize: TIntVector3 read GetReactorSize write SetReactorSize;
    property ReactorSizeX: Integer read GetReactorSizeX write SetReactorSizeX;
    property ReactorSizeY: Integer read GetReactorSizeY write SetReactorSizeY;
    property ReactorSizeZ: Integer read GetReactorSizeZ write SetReactorSizeZ;

    property FuelBasePower: Single read GetFuelBasePower write SetFuelBasePower;
    property FuelBaseHeat: Single read GetFuelBaseHeat write SetFuelBaseHeat;

    property BlockTypes: TReactor.TBlockTypes read GetBlockTypes write SetBlockTypes;
    property HasBlockType[ABlockType: TReactor.TBlockType]: Boolean read GetBlockType write SetBlockType;
    property BlockTypeList: IList<TReactor.TBlockType> read GetBlockTypeList;

    property PopulationSize: Integer read GetPopulationSize write SetPopulationSize;
    property GeneratorFunction: IGeneratorFunction read GetGeneratorFunction;
    property MutationFunction: IMutationFunction read GetMutationFunction;
    property FitnessFunction: IFitnessFunction read GetFitnessFunction;

    procedure SetGeneratorFunction(AClass: TGeneratorFunctionClass);
    procedure SetMutationFunction(AClass: TMutationFunctionClass);
    procedure SetFitnessFunction(AClass: TFitnessFunctionClass);

    procedure Assign(AFrom: IEvolutionSettings);
    function Copy: IEvolutionSettings;

  end;

  TReactorEvolution = class
  private
  class var
    FGeneratorFunctions: IList<TGeneratorFunctionClass>;
    FMutationFunctions: IList<TMutationFunctionClass>;
    FFitnessFunctions: IList<TFitnessFunctionClass>;

    class function GetGeneratorFunctions: IReadonlyList<TGeneratorFunctionClass>; static;
    class function GetMutationFunctions: IReadonlyList<TMutationFunctionClass>; static;
    class function GetFitnessFunctions: IReadonlyList<TFitnessFunctionClass>; static;

  public
    class constructor Create;

    class procedure RegisterFunction(AGeneratorFunctionClass: TGeneratorFunctionClass); overload;
    class procedure RegisterFunction(AMutationFunctionClass: TMutationFunctionClass); overload;
    class procedure RegisterFunction(AFitnessFunctionClass: TFitnessFunctionClass); overload;

    class property GeneratorFunctions: IReadonlyList<TGeneratorFunctionClass> read GetGeneratorFunctions;
    class property MutationFunctions: IReadonlyList<TMutationFunctionClass> read GetMutationFunctions;
    class property FitnessFunctions: IReadonlyList<TFitnessFunctionClass> read GetFitnessFunctions;

  private
    FSettings: IEvolutionSettings;
    FGeneration: TEvolutionGeneration;
    FReactorBreakthroughs: IList<TReactorRating>;

    function GetSettings: IEvolutionSettings;
    function GetReactorBreakthroughs: IReadonlyList<TReactorRating>;

  public
    constructor Create(ASettings: IEvolutionSettings);
    destructor Destroy; override;

    property Generation: TEvolutionGeneration read FGeneration;
    property ReactorBreakthroughs: IReadonlyList<TReactorRating> read GetReactorBreakthroughs;

    property Settings: IEvolutionSettings read GetSettings;

    procedure Evolve;

  end;

implementation

{ TEvolutionGeneration }

constructor TEvolutionGeneration.Create(ASettings: IEvolutionSettings; AParentGeneration: TEvolutionGeneration);
var
  Generator: IGeneratorFunction;
begin
  FSettings := ASettings;

  if AParentGeneration = nil then
  begin
    Generator := ASettings.GeneratorFunction;
    FIndex := 1;
  end
  else
  begin
    ASettings.MutationFunction.ParentGeneration := AParentGeneration;
    Generator := ASettings.MutationFunction;
    FIndex := AParentGeneration.Index + 1;
  end;

  FReactorRatings := TSortedObjectList<TReactorRating>.Create;
  FReactorRatings.Compare := function(A, B: TReactorRating): Boolean
    begin
      Result := A.Fitness > B.Fitness;
    end;

  FReactorRatings.AddRange(Generator.Generator.Iterate.Generic.Map<TReactorRating>(
    function(Reactor: TReactor): TReactorRating
    begin
      Result := TReactorRating.Create(Reactor, Index, Settings);
    end
  ));
end;

function TEvolutionGeneration.CreateStats(AValueExtractor: TFunc<TReactorRating, Single>;
ALowerBetter: Boolean): TStats;
var
  I: Integer;
  Value: Single;
begin
  if ReactorRatings.Empty then
    Value := NaN
  else
    Value := AValueExtractor(ReactorRatings[0]);
  Result.Worst := Value;
  Result.Average := Value;
  Result.Best := Value;

  if ALowerBetter then
    for I := 1 to ReactorRatings.MaxIndex do
    begin
      Value := AValueExtractor(ReactorRatings[I]);
      Result.Best := Min(Result.Best, Value);
      Result.Worst := Max(Result.Worst, Value);
      Result.Average := Result.Average + Value;
    end
  else
    for I := 1 to ReactorRatings.MaxIndex do
    begin
      Value := AValueExtractor(ReactorRatings[I]);
      Result.Best := Max(Result.Best, Value);
      Result.Worst := Min(Result.Worst, Value);
      Result.Average := Result.Average + Value;
    end;

  Result.Average := Result.Average / ReactorRatings.Count;
end;

function TEvolutionGeneration.GetBest: TReactorRating;
begin
  Result := ReactorRatings.First;
end;

function TEvolutionGeneration.GetEfficiencyStats: TStats;
begin
  Result := CreateStats(
    function(ReactorRating: TReactorRating): Single
    begin
      Result := ReactorRating.Efficiency;
    end
    );
end;

function TEvolutionGeneration.GetFitnessStats: TStats;
begin
  Result := CreateStats(
    function(ReactorRating: TReactorRating): Single
    begin
      Result := ReactorRating.Fitness;
    end
    );
end;

function TEvolutionGeneration.GetNetHeatGenerationStats: TStats;
begin
  Result := CreateStats(
    function(ReactorRating: TReactorRating): Single
    begin
      Result := ReactorRating.NetHeatGeneration;
    end,
    True);
end;

function TEvolutionGeneration.GetPopulationSize: Integer;
begin
  Result := ReactorRatings.Count;
end;

function TEvolutionGeneration.GetPowerGenerationStats: TStats;
begin
  Result := CreateStats(
    function(ReactorRating: TReactorRating): Single
    begin
      Result := ReactorRating.PowerGeneration;
    end
    );
end;

function TEvolutionGeneration.GetReactorRatings: IReadonlyList<TReactorRating>;
begin
  Result := FReactorRatings.ReadonlyList;
end;

function TEvolutionGeneration.GetSettings: IEvolutionSettings;
begin
  Result := FSettings.Copy;
end;

function TEvolutionGeneration.GetWorst: TReactorRating;
begin
  Result := ReactorRatings.Last;
end;

{ TReactorEvolution }

constructor TReactorEvolution.Create(ASettings: IEvolutionSettings);
begin
  FSettings := ASettings.Copy;
  FReactorBreakthroughs := TObjectList<TReactorRating>.Create;
  Evolve;
end;

destructor TReactorEvolution.Destroy;
begin
  FGeneration.Free;
  inherited;
end;

procedure TReactorEvolution.Evolve;
var
  OldGeneration: TEvolutionGeneration;
begin
  OldGeneration := FGeneration;
  FGeneration := TEvolutionGeneration.Create(Settings, OldGeneration);
  OldGeneration.Free;
  if FReactorBreakthroughs.Empty or (Generation.Best.Fitness > FReactorBreakthroughs.Last.Fitness) then
    FReactorBreakthroughs.Add(Generation.Best.Copy);
end;

class constructor TReactorEvolution.Create;
begin
  FGeneratorFunctions := TList<TGeneratorFunctionClass>.Create;
  FMutationFunctions := TList<TMutationFunctionClass>.Create;
  FFitnessFunctions := TList<TFitnessFunctionClass>.Create;
end;

class function TReactorEvolution.GetFitnessFunctions: IReadonlyList<TFitnessFunctionClass>;
begin
  Result := FFitnessFunctions.ReadonlyList;
end;

class function TReactorEvolution.GetGeneratorFunctions: IReadonlyList<TGeneratorFunctionClass>;
begin
  Result := FGeneratorFunctions.ReadonlyList;
end;

class function TReactorEvolution.GetMutationFunctions: IReadonlyList<TMutationFunctionClass>;
begin
  Result := FMutationFunctions.ReadonlyList;
end;

function TReactorEvolution.GetReactorBreakthroughs: IReadonlyList<TReactorRating>;
begin
  Result := FReactorBreakthroughs.ReadonlyList;
end;

function TReactorEvolution.GetSettings: IEvolutionSettings;
begin
  Result := FSettings.Copy;
end;

class procedure TReactorEvolution.RegisterFunction(AGeneratorFunctionClass: TGeneratorFunctionClass);
begin
  FGeneratorFunctions.Add(AGeneratorFunctionClass);
end;

class procedure TReactorEvolution.RegisterFunction(AMutationFunctionClass: TMutationFunctionClass);
begin
  FMutationFunctions.Add(AMutationFunctionClass);
end;

class procedure TReactorEvolution.RegisterFunction(AFitnessFunctionClass: TFitnessFunctionClass);
begin
  FFitnessFunctions.Add(AFitnessFunctionClass);
end;

{ TReactorGenerator }

constructor TReactorGenerator.Create(ASettings: IEvolutionSettings);
begin
  FSettings := ASettings;
  FCurrentIndex := -1;
end;

function TReactorGenerator.GetCurrent: TReactor;
begin
  Result := FCurrent;
end;

function TReactorGenerator.MoveNext: Boolean;
begin
  Inc(FCurrentIndex);
  Result := FCurrentIndex < Settings.PopulationSize;
  if Result then
  begin
    FCurrent := TReactor.Create(Settings.ReactorSize);
    Generate(FCurrent, FCurrentIndex);
  end;
end;

{ TMutationFunction }

function TMutationFunction.Copy(ASettings: IEvolutionSettings): IMutationFunction;
begin
  Supports(inherited Copy(ASettings), IMutationFunction, Result);
end;

function TMutationFunction.GetParentGeneration: TEvolutionGeneration;
begin
  Result := FParentGeneration;
end;

procedure TMutationFunction.SetParentGeneration(const Value: TEvolutionGeneration);
begin
  FParentGeneration := Value;
end;

{ TEvolutionSettings }

function TEvolutionSettings.GetReactorSize: TIntVector3;
begin
  Result := FReactorSize;
end;

procedure TEvolutionSettings.SetReactorSize(const Value: TIntVector3);
begin
  FReactorSize := Value;
end;

function TEvolutionSettings.GetReactorSizeX: Integer;
begin
  Result := FReactorSize.X;
end;

procedure TEvolutionSettings.SetReactorSizeX(const Value: Integer);
begin
  FReactorSize.X := Value;
end;

function TEvolutionSettings.GetReactorSizeY: Integer;
begin
  Result := FReactorSize.Y;
end;

procedure TEvolutionSettings.SetReactorSizeY(const Value: Integer);
begin
  FReactorSize.Y := Value;
end;

function TEvolutionSettings.GetReactorSizeZ: Integer;
begin
  Result := FReactorSize.Z;
end;

procedure TEvolutionSettings.SetReactorSizeZ(const Value: Integer);
begin
  FReactorSize.Z := Value;
end;

function TEvolutionSettings.GetFuelBasePower: Single;
begin
  Result := FFuelBasePower;
end;

procedure TEvolutionSettings.SetFuelBasePower(const Value: Single);
begin
  FFuelBasePower := Value;
end;

function TEvolutionSettings.GetFuelBaseHeat: Single;
begin
  Result := FFuelBaseHeat;
end;

procedure TEvolutionSettings.SetFuelBaseHeat(const Value: Single);
begin
  FFuelBaseHeat := Value;
end;

function TEvolutionSettings.GetBlockTypes: TReactor.TBlockTypes;
begin
  Result := FBlockTypes;
end;

procedure TEvolutionSettings.SetBlockTypes(const Value: TReactor.TBlockTypes);
begin
  FBlockTypeList := nil;
  FBlockTypes := Value;
end;

function TEvolutionSettings.GetBlockType(ABlockType: TReactor.TBlockType): Boolean;
begin
  Result := ABlockType in FBlockTypes;
end;

function TEvolutionSettings.GetBlockTypeList: IList<TReactor.TBlockType>;
var
  BlockType: TReactor.TBlockType;
begin
  if FBlockTypeList = nil then
  begin
    FBlockTypeList := TList<TReactor.TBlockType>.Create;
    for BlockType in BlockTypes do
      FBlockTypeList.Add(BlockType);
  end;
  Result := FBlockTypeList;
end;

procedure TEvolutionSettings.SetBlockType(ABlockType: TReactor.TBlockType; const Value: Boolean);
begin
  FBlockTypeList := nil;
  if Value then
    Include(FBlockTypes, ABlockType)
  else
    Exclude(FBlockTypes, ABlockType);
end;

function TEvolutionSettings.GetPopulationSize: Integer;
begin
  Result := FPopulationSize;
end;

procedure TEvolutionSettings.SetPopulationSize(const Value: Integer);
begin
  FPopulationSize := Value;
end;

function TEvolutionSettings.GetGeneratorFunction: IGeneratorFunction;
begin
  Result := FGeneratorFunction;
end;

function TEvolutionSettings.GetMutationFunction: IMutationFunction;
begin
  Result := FMutationFunction;
end;

function TEvolutionSettings.GetFitnessFunction: IFitnessFunction;
begin
  Result := FFitnessFunction;
end;

constructor TEvolutionSettings.Create;
begin
  ReactorSize := 3;
  FuelBasePower := 100;
  FuelBaseHeat := 10;
  PopulationSize := 20;
  BlockTypes := TReactor.BlockTypes;
end;

procedure TEvolutionSettings.SetGeneratorFunction(AClass: TGeneratorFunctionClass);
begin
  FGeneratorFunction := AClass.Create(Self);
end;

procedure TEvolutionSettings.SetMutationFunction(AClass: TMutationFunctionClass);
begin
  FMutationFunction := AClass.Create(Self);
end;

procedure TEvolutionSettings.SetFitnessFunction(AClass: TFitnessFunctionClass);
begin
  FFitnessFunction := AClass.Create(Self);
end;

procedure TEvolutionSettings.Assign(AFrom: IEvolutionSettings);
begin
  ReactorSize := AFrom.ReactorSize;
  FuelBasePower := AFrom.FuelBasePower;
  FuelBaseHeat := AFrom.FuelBaseHeat;
  PopulationSize := AFrom.PopulationSize;
  FFitnessFunction := nil;
  FGeneratorFunction := nil;
  FMutationFunction := nil;
  if AFrom.FitnessFunction <> nil then
    FFitnessFunction := AFrom.FitnessFunction.Copy(Self);
  if AFrom.GeneratorFunction <> nil then
    FGeneratorFunction := AFrom.GeneratorFunction.Copy(Self);
  if AFrom.MutationFunction <> nil then
    FMutationFunction := AFrom.MutationFunction.Copy(Self);
  BlockTypes := AFrom.BlockTypes;
end;

function TEvolutionSettings.Copy: IEvolutionSettings;
begin
  Result := TEvolutionSettings.Create;
  Result.Assign(Self);
end;

{ TEvolutionFunction }

procedure TEvolutionFunction.Assign(AFrom: IEvolutionFunction);
begin
  // nothing by default
end;

function TEvolutionFunction.Copy(ASettings: IEvolutionSettings): IEvolutionFunction;
begin
  Result := FunctionClass.Create(ASettings);
  Result.Assign(Self);
end;

constructor TEvolutionFunction.Create(ASettings: IEvolutionSettings);
begin
  FSettings := ASettings;
end;

class function TEvolutionFunction.FunctionClass: TEvolutionFunctionClass;
begin
  Result := Self;
end;

function TEvolutionFunction.FunctionClassI: TEvolutionFunctionClass;
begin
  Result := FunctionClass;
end;

function TEvolutionFunction.GetDisplayNameI: string;
begin
  Result := GetDisplayName;
end;

{ TFitnessFunction }

function TFitnessFunction.Copy(ASettings: IEvolutionSettings): IFitnessFunction;
begin
  Assert(Supports(inherited Copy(ASettings), IFitnessFunction, Result));
end;

{ TGeneratorFunction }

function TGeneratorFunction.Copy(ASettings: IEvolutionSettings): IGeneratorFunction;
begin
  Assert(Supports(inherited Copy(ASettings), IGeneratorFunction, Result));
end;

function TGeneratorFunction.Generator: IIterable<TReactor>;
begin
  Result := Self;
end;

function TGeneratorFunction.Iterate: IIterate<TReactor>;
begin
  Result := TIterableIterate<TReactor>.Create(Self);
end;

{ TReactorMutator }

constructor TReactorMutator.Create(AParentGeneration: TEvolutionGeneration);
begin
  inherited Create(AParentGeneration.Settings);
  FParentGeneration := AParentGeneration;
end;

{ TReactorRating }

function TReactorRating.Copy: TReactorRating;
begin
  Result := TReactorRating.Create(Reactor.Copy, Generation, Settings);
end;

constructor TReactorRating.Create(AReactor: TReactor; AGeneration: Integer; ASettings: IEvolutionSettings);
var
  Calculation: TReactor.TCalculation;
begin
  FReactor := AReactor;
  FGeneration := AGeneration;
  FSettings := ASettings;

  Calculation := Reactor.Calculation;
  FPowerGeneration := Calculation.PowerGeneration(ASettings.FuelBasePower);
  FHeatGeneration := Calculation.HeatGeneration(ASettings.FuelBaseHeat);
  FNetHeatGeneration := Calculation.NetHeatGeneration(ASettings.FuelBaseHeat);
  FFitness := ASettings.FitnessFunction.Calculate(Reactor, ASettings);
end;

destructor TReactorRating.Destroy;
begin
  FReactor.Free;
  inherited;
end;

function TReactorRating.GetCellCount: Integer;
begin
  Result := Reactor.Calculation.CellCount;
end;

function TReactorRating.GetCoolingRate: Single;
begin
  Result := Reactor.Calculation.CoolingRate;
end;

function TReactorRating.GetEfficiency: Single;
begin
  Result := Reactor.Calculation.Efficiency;
end;

function TReactorRating.GetHeatFactor: Single;
begin
  Result := Reactor.Calculation.HeatFactor;
end;

end.
