unit ReactorEvolutionDefine;

interface

uses
  System.SysUtils,

  Pengine.CollectionInterfaces,
  Pengine.Collections,
  Pengine.IntMaths,

  ReactorDefine,
  System.Math;

type

  TRatedReactor = class(TReactor)
  private
    FFitness: Single;
    FHeatGeneration: Single;
    FPowerGeneration: Single;
    FNetHeatGeneration: Single;

  public
    property Fitness: Single read FFitness;
    function PowerGeneration: Single; overload;
    function HeatGeneration: Single; overload;
    function NetHeatGeneration: Single; overload;

  end;

  IEvolutionSettings = interface;

  TEvolutionGeneration = class
  public type

    TStats = record
      Best: Single;
      Average: Single;
      Worst: Single;
    end;

    TReactors = TObjectArray<TRatedReactor>;

  private
    FSettings: IEvolutionSettings;
    FReactors: TReactors;

    function GetReactors: TReactors.TReader;
    function GetPopulationSize: Integer;

    function GetBest: TRatedReactor;
    function GetWorst: TRatedReactor;

    function GetFitnessStats: TStats;
    function GetEfficiencyStats: TStats;
    function GetPowerGenerationStats: TStats;
    function GetNetHeatGenerationStats: TStats;

    function CreateStats(AValueExtractor: TFunc<TRatedReactor, Single>; ALowerBetter: Boolean = False): TStats;
    function GetSettings: IEvolutionSettings;

  public
    constructor Create(ASettings: IEvolutionSettings; AParentGeneration: TEvolutionGeneration = nil); overload;
    destructor Destroy; override;

    property Settings: IEvolutionSettings read GetSettings;

    /// <summary>Ordered by fitness from best to worst.</summary>
    property Reactors: TReactors.TReader read GetReactors;
    /// <summary>The reactor count.</summary>
    property PopulationSize: Integer read GetPopulationSize;

    /// <summary>The reactor with the best fitness of this generation.</summary>
    property Best: TRatedReactor read GetBest;
    /// <summary>The reactor with the worst fitness of this generation.</summary>
    property Worst: TRatedReactor read GetWorst;

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
    function Calculate(AReactor: TRatedReactor): Single;

    function Copy(ASettings: IEvolutionSettings): IFitnessFunction;

  end;

  IGeneratorFunction = interface(IEvolutionFunction)
    ['{3CC45E2C-4779-4FE4-AC75-51F3C59BD7A4}']
    function GetEnumerator: IIterator<TRatedReactor>;

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
    function GetBlockTypeArray: TArray<TReactor.TBlockType>.TReader;

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
    property BlockTypeArray: TArray<TReactor.TBlockType>.TReader read GetBlockTypeArray;

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
    function Calculate(AReactor: TRatedReactor): Single; virtual; abstract;

    function Copy(ASettings: IEvolutionSettings): IFitnessFunction;

  end;

  IReactorIterator = IIterator<TRatedReactor>;

  TGeneratorFunction = class abstract(TEvolutionFunction, IGeneratorFunction)
  public
    function GetEnumerator: IReactorIterator; virtual; abstract;

    function Copy(ASettings: IEvolutionSettings): IGeneratorFunction;

  end;

  TReactorGenerator = class abstract(TInterfacedObject, IReactorIterator)
  private
    FSettings: IEvolutionSettings;
    FCurrentIndex: Integer;
    FCurrent: TRatedReactor;

    function GetCurrent: TRatedReactor;

  protected
    procedure Generate(AReactor: TReactor; AIndex: Integer); virtual; abstract;

  public
    constructor Create(ASettings: IEvolutionSettings); virtual;

    function MoveNext: Boolean;
    property Current: TRatedReactor read GetCurrent;

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
  public type

    TBlockTypeArray = TArray<TReactor.TBlockType>;

  private
    FReactorSize: TIntVector3;
    FFuelBasePower: Single;
    FFuelBaseHeat: Single;
    FPopulationSize: Integer;
    FGeneratorFunction: IGeneratorFunction;
    FMutationFunction: IMutationFunction;
    FFitnessFunction: IFitnessFunction;
    FBlockTypes: TReactor.TBlockTypes;
    FBlockTypeArray: TBlockTypeArray;

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
    function GetBlockTypeArray: TBlockTypeArray.TReader;

    function GetPopulationSize: Integer;
    procedure SetPopulationSize(const Value: Integer);
    function GetGeneratorFunction: IGeneratorFunction;
    function GetMutationFunction: IMutationFunction;
    function GetFitnessFunction: IFitnessFunction;

  public
    constructor Create;
    destructor Destroy; override;

    property ReactorSize: TIntVector3 read GetReactorSize write SetReactorSize;
    property ReactorSizeX: Integer read GetReactorSizeX write SetReactorSizeX;
    property ReactorSizeY: Integer read GetReactorSizeY write SetReactorSizeY;
    property ReactorSizeZ: Integer read GetReactorSizeZ write SetReactorSizeZ;

    property FuelBasePower: Single read GetFuelBasePower write SetFuelBasePower;
    property FuelBaseHeat: Single read GetFuelBaseHeat write SetFuelBaseHeat;

    property BlockTypes: TReactor.TBlockTypes read GetBlockTypes write SetBlockTypes;
    property HasBlockType[ABlockType: TReactor.TBlockType]: Boolean read GetBlockType write SetBlockType;
    property BlockTypeArray: TBlockTypeArray.TReader read GetBlockTypeArray;

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
  public type

    TGenerations = TObjectArray<TEvolutionGeneration>;

  private
  class var
    FGeneratorFunctions: TArray<TGeneratorFunctionClass>;
    FMutationFunctions: TArray<TMutationFunctionClass>;
    FFitnessFunctions: TArray<TFitnessFunctionClass>;

    class function GetGeneratorFunctions: TArray<TGeneratorFunctionClass>.TReader; static;
    class function GetMutationFunctions: TArray<TMutationFunctionClass>.TReader; static;
    class function GetFitnessFunctions: TArray<TFitnessFunctionClass>.TReader; static;

  public
    class constructor Create;
    class destructor Destroy;

    class procedure RegisterFunction(AGeneratorFunctionClass: TGeneratorFunctionClass); overload;
    class procedure RegisterFunction(AMutationFunctionClass: TMutationFunctionClass); overload;
    class procedure RegisterFunction(AFitnessFunctionClass: TFitnessFunctionClass); overload;

    class property GeneratorFunctions: TArray<TGeneratorFunctionClass>.TReader read GetGeneratorFunctions;
    class property MutationFunctions: TArray<TMutationFunctionClass>.TReader read GetMutationFunctions;
    class property FitnessFunctions: TArray<TFitnessFunctionClass>.TReader read GetFitnessFunctions;

  private
    FSettings: IEvolutionSettings;
    FGenerations: TGenerations;

    function GetGenerations: TGenerations.TReader;
    function GetSettings: IEvolutionSettings;

  public
    constructor Create(ASettings: IEvolutionSettings);
    destructor Destroy; override;

    property Generations: TGenerations.TReader read GetGenerations;

    property Settings: IEvolutionSettings read GetSettings;

    procedure Evolve;

  end;

implementation

{ TEvolutionGeneration }

constructor TEvolutionGeneration.Create(ASettings: IEvolutionSettings; AParentGeneration: TEvolutionGeneration);
var
  Reactor: TRatedReactor;
  Generator: IGeneratorFunction;
begin
  FSettings := ASettings;
  FReactors := TReactors.Create;
  FReactors.Capacity := ASettings.PopulationSize;

  if AParentGeneration = nil then
  begin
    Generator := ASettings.GeneratorFunction;
  end
  else
  begin
    ASettings.MutationFunction.ParentGeneration := AParentGeneration;
    Generator := ASettings.MutationFunction;
  end;

  for Reactor in Generator do
  begin
    Reactor.FPowerGeneration := Reactor.PowerGeneration(Settings.FuelBasePower);
    Reactor.FHeatGeneration := Reactor.HeatGeneration(Settings.FuelBaseHeat);
    Reactor.FNetHeatGeneration := Reactor.NetHeatGeneration(Settings.FuelBaseHeat);
    Reactor.FFitness := ASettings.FitnessFunction.Calculate(Reactor);
    FReactors.Add(Reactor);
  end;
  FReactors.Sort(
    function(A, B: TRatedReactor): Boolean
    begin
      Result := A.Fitness > B.Fitness;
    end
    );
end;

function TEvolutionGeneration.CreateStats(AValueExtractor: TFunc<TRatedReactor, Single>; ALowerBetter: Boolean): TStats;
var
  I: Integer;
  Value: Single;
begin
  if Reactors.Empty then
    Value := NaN
  else
    Value := AValueExtractor(Reactors[0]);
  Result.Worst := Value;
  Result.Average := Value;
  Result.Best := Value;

  if ALowerBetter then
    for I := 1 to Reactors.MaxIndex do
    begin
      Value := AValueExtractor(Reactors[I]);
      Result.Best := Min(Result.Best, Value);
      Result.Worst := Max(Result.Worst, Value);
      Result.Average := Result.Average + Value;
    end
  else
    for I := 1 to Reactors.MaxIndex do
    begin
      Value := AValueExtractor(Reactors[I]);
      Result.Best := Max(Result.Best, Value);
      Result.Worst := Min(Result.Worst, Value);
      Result.Average := Result.Average + Value;
    end;

  Result.Average := Result.Average / Reactors.Count;
end;

destructor TEvolutionGeneration.Destroy;
begin
  FReactors.Free;
  inherited;
end;

function TEvolutionGeneration.GetBest: TRatedReactor;
begin
  Result := Reactors.First;
end;

function TEvolutionGeneration.GetEfficiencyStats: TStats;
begin
  Result := CreateStats(
    function(AReactor: TRatedReactor): Single
    begin
      Result := AReactor.Efficiency;
    end
    );
end;

function TEvolutionGeneration.GetFitnessStats: TStats;
begin
  Result := CreateStats(
    function(AReactor: TRatedReactor): Single
    begin
      Result := AReactor.Fitness;
    end
    );
end;

function TEvolutionGeneration.GetNetHeatGenerationStats: TStats;
begin
  Result := CreateStats(
    function(AReactor: TRatedReactor): Single
    begin
      Result := AReactor.NetHeatGeneration;
    end,
    True);
end;

function TEvolutionGeneration.GetPopulationSize: Integer;
begin
  Result := Reactors.Count;
end;

function TEvolutionGeneration.GetPowerGenerationStats: TStats;
begin
  Result := CreateStats(
    function(AReactor: TRatedReactor): Single
    begin
      Result := AReactor.PowerGeneration;
    end
    );
end;

function TEvolutionGeneration.GetReactors: TReactors.TReader;
begin
  Result := FReactors.Reader;
end;

function TEvolutionGeneration.GetSettings: IEvolutionSettings;
begin
  Result := FSettings.Copy;
end;

function TEvolutionGeneration.GetWorst: TRatedReactor;
begin
  Result := Reactors.Last;
end;

{ TReactorEvolution }

constructor TReactorEvolution.Create(ASettings: IEvolutionSettings);
begin
  FSettings := ASettings.Copy;
  FGenerations := TGenerations.Create;
  FGenerations.Add(TEvolutionGeneration.Create(FSettings));
end;

destructor TReactorEvolution.Destroy;
begin
  FGenerations.Free;
end;

procedure TReactorEvolution.Evolve;
begin
  FGenerations.Add(TEvolutionGeneration.Create(Settings, FGenerations.Last));
end;

class constructor TReactorEvolution.Create;
begin
  FGeneratorFunctions := TArray<TGeneratorFunctionClass>.Create;
  FMutationFunctions := TArray<TMutationFunctionClass>.Create;
  FFitnessFunctions := TArray<TFitnessFunctionClass>.Create;
end;

class destructor TReactorEvolution.Destroy;
begin
  FGeneratorFunctions.Free;
  FMutationFunctions.Free;
  FFitnessFunctions.Free;
end;

class function TReactorEvolution.GetFitnessFunctions: TArray<TFitnessFunctionClass>.TReader;
begin
  Result := FFitnessFunctions.Reader;
end;

function TReactorEvolution.GetGenerations: TGenerations.TReader;
begin
  Result := FGenerations.Reader;
end;

class function TReactorEvolution.GetGeneratorFunctions: TArray<TGeneratorFunctionClass>.TReader;
begin
  Result := FGeneratorFunctions.Reader;
end;

class function TReactorEvolution.GetMutationFunctions: TArray<TMutationFunctionClass>.TReader;
begin
  Result := FMutationFunctions.Reader;
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

function TReactorGenerator.GetCurrent: TRatedReactor;
begin
  Result := FCurrent;
end;

function TReactorGenerator.MoveNext: Boolean;
begin
  Inc(FCurrentIndex);
  Result := FCurrentIndex < Settings.PopulationSize;
  if Result then
  begin
    FCurrent := TRatedReactor.Create(Settings.ReactorSize);
    Generate(FCurrent, FCurrentIndex);
    FCurrent.Lock;
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
  FreeAndNil(FBlockTypeArray);
  FBlockTypes := Value;
end;

function TEvolutionSettings.GetBlockType(ABlockType: TReactor.TBlockType): Boolean;
begin
  Result := ABlockType in FBlockTypes;
end;

function TEvolutionSettings.GetBlockTypeArray: TBlockTypeArray.TReader;
var
  BlockType: TReactor.TBlockType;
begin
  if FBlockTypeArray = nil then
  begin
    FBlockTypeArray := TBlockTypeArray.Create;
    for BlockType in BlockTypes do
      FBlockTypeArray.Add(BlockType);
  end;
  Result := FBlockTypeArray.Reader;
end;

procedure TEvolutionSettings.SetBlockType(ABlockType: TReactor.TBlockType; const Value: Boolean);
begin
  FreeAndNil(FBlockTypeArray);
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

destructor TEvolutionSettings.Destroy;
begin
  FBlockTypeArray.Free;
  inherited;
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

{ TRatedReactor }

function TRatedReactor.HeatGeneration: Single;
begin
  Result := FHeatGeneration;
end;

function TRatedReactor.NetHeatGeneration: Single;
begin
  Result := FNetHeatGeneration;
end;

function TRatedReactor.PowerGeneration: Single;
begin
  Result := FPowerGeneration;
end;

{ TReactorMutator }

constructor TReactorMutator.Create(AParentGeneration: TEvolutionGeneration);
begin
  inherited Create(AParentGeneration.Settings);
  FParentGeneration := AParentGeneration;
end;

end.
