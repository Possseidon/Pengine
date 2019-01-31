unit ReactorEvolutionDefine;

interface

uses
  System.SysUtils,

  Pengine.CollectionInterfaces,
  Pengine.Collections,

  ReactorDefine,
  Pengine.IntMaths;

type

  TReactorEvolution = class
  public type

    ISettings = interface;

    IHasSettingsDialog = interface
      ['{BE872851-764E-4616-B5DB-2C1D62A53147}']
      procedure ShowSettingsDialog;

    end;

    IFunction = interface
      ['{C2A9C568-DB20-492F-95BD-E27A89BDA8CD}']
      function GetDisplayName: string;

    end;

    TFunctionClass = class of TFunction;

    TFunction = class(TInterfacedObject, IFunction)
    private
      [Weak] FSettings: ISettings;

    public
      constructor Create(ASettings: ISettings); virtual;

      function GetDisplayName: string; virtual; abstract;

      property Settings: ISettings read FSettings;

    end;

    IFitnessFunction = interface(IFunction)
      ['{CBC54E5B-6AB4-47A6-9A1A-B245A4297997}']
      function Calculate(AReactor: TReactor): Single;

    end;

    TFitnessFunctionClass = class of TFitnessFunction;

    /// <summary>Generates a fitness value for a given reactor.</summary>
    TFitnessFunction = class abstract(TFunction, IFitnessFunction)
    public
      function Calculate(AReactor: TReactor): Single; virtual; abstract;

    end;

    TStats = record
      Worst: Single;
      Average: Single;
      Best: Single;
    end;

    TRatedReactor = class(TReactor)
    private
      FFitness: Single;

    public
      property Fitness: Single read FFitness;

    end;

    IGeneratorFunction = interface(IFunction)
      ['{3CC45E2C-4779-4FE4-AC75-51F3C59BD7A4}']
      function GetEnumerator: IIterator<TRatedReactor>;

    end;

    TGeneratorFunctionClass = class of TGeneratorFunction;

    IReactorIterator = IIterator<TRatedReactor>;

    TGeneratorFunction = class(TFunction, IGeneratorFunction)
    public
      function GetEnumerator: IReactorIterator; virtual; abstract;

    end;

    TReactorGenerator = class(TInterfacedObject, IReactorIterator)
    private
      FSettings: ISettings;
      FCurrentIndex: Integer;
      FCurrent: TRatedReactor;

      function GetCurrent: TRatedReactor;

    protected
      procedure Generate(AReactor: TReactor; AIndex: Integer); virtual; abstract;

    public
      constructor Create(ASettings: ISettings); virtual;

      function MoveNext: Boolean;
      property Current: TRatedReactor read GetCurrent;

      property Settings: ISettings read FSettings;

    end;

    TGeneration = class
    public type

      TReactors = TObjectArray<TRatedReactor>;

    private
      FReactors: TReactors;

      function GetReactors: TReactors.TReader;
      function GetPopulationSize: Integer;

    public
      constructor Create(ASettings: ISettings);
      destructor Destroy; override;

      /// <summary>Ordered by fitness from worst to best.</summary>
      property Reactors: TReactors.TReader read GetReactors;
      /// <summary>The reactor count.</summary>
      property PopulationSize: Integer read GetPopulationSize;

      /// <summary>The reactor with the best fitness of this generation.</summary>
      // property Best: TRatedReactor read GetBest;
      /// <summary>The reactor with the worst fitness of this generation.</summary>
      // property Worst: TRatedReactor read GetWorst;

      /// <summary>Worst, average and best fitnesses of this generation.</summary>
      // property FitnessStats: TStats read GetFitnessStats;
      /// <summary>Worst, average and best efficiency of this generation.</summary>
      // property EfficiencyStats: TStats read GetEfficiencyStats;
      /// <summary>Worst, average and best power generation of this generation.</summary>
      // property PowerGenerationStats: TStats read GetPowerGenerationStats;
      /// <summary>Worst, average and best heat generation of this generation.</summary>
      // property HeatGenerationStats: TStats read GetHeatGenerationStats;

    end;

    IMutationFunction = interface(IGeneratorFunction)
      ['{3662408E-35BE-4076-AD04-BDC5B29B0824}']
      function GetParentGeneration: TGeneration;
      procedure SetParentGeneration(const Value: TGeneration);
      property ParentGeneration: TGeneration read GetParentGeneration write SetParentGeneration;

    end;

    TMutationFunctionClass = class of TMutationFunction;

    /// <summary>Generates a new generation of reactors from a given generation.</summary>
    TMutationFunction = class abstract(TGeneratorFunction, IMutationFunction)
    private
      FParentGeneration: TGeneration;

      function GetParentGeneration: TGeneration;
      procedure SetParentGeneration(const Value: TGeneration);

    public
      property ParentGeneration: TGeneration read GetParentGeneration write SetParentGeneration;

    end;

    ISettings = interface
      ['{A5E4086D-B8FB-41D6-858A-21D51E286790}']
      function GetReactorSize: TIntVector3;
      procedure SetReactorSize(const Value: TIntVector3);
      function GetFuelBasePower: Single;
      procedure SetFuelBasePower(const Value: Single);
      function GetFuelBaseHeat: Single;
      procedure SetFuelBaseHeat(const Value: Single);

      function GetBlockTypes: TReactor.TBlockTypes;
      procedure SetBlockTypes(const Value: TReactor.TBlockTypes);
      function GetBlockType(ABlockType: TReactor.TBlockType): Boolean;
      procedure SetBlockType(ABlockType: TReactor.TBlockType; const Value: Boolean);

      function GetPopulationSize: Integer;
      procedure SetPopulationSize(const Value: Integer);
      function GetGeneratorFunction: IGeneratorFunction;
      procedure SetGeneratorFunction(const Value: IGeneratorFunction);
      function GetMutationFunction: IMutationFunction;
      procedure SetMutationFunction(const Value: IMutationFunction);
      function GetFitnessFunction: IFitnessFunction;
      procedure SetFitnessFunction(const Value: IFitnessFunction);

      property ReactorSize: TIntVector3 read GetReactorSize write SetReactorSize;
      property FuelBasePower: Single read GetFuelBasePower write SetFuelBasePower;
      property FuelBaseHeat: Single read GetFuelBaseHeat write SetFuelBaseHeat;

      property BlockTypes: TReactor.TBlockTypes read GetBlockTypes write SetBlockTypes;
      property HasBlockType[ABlockType: TReactor.TBlockType]: Boolean read GetBlockType write SetBlockType;

      property PopulationSize: Integer read GetPopulationSize write SetPopulationSize;
      property GeneratorFunction: IGeneratorFunction read GetGeneratorFunction write SetGeneratorFunction;
      property MutationFunction: IMutationFunction read GetMutationFunction write SetMutationFunction;
      property FitnessFunction: IFitnessFunction read GetFitnessFunction write SetFitnessFunction;

      procedure Assign(AFrom: ISettings);
      function Copy: ISettings;

    end;

    TSettings = class(TInterfacedObject, ISettings)
    private
      FReactorSize: TIntVector3;
      FFuelBasePower: Single;
      FFuelBaseHeat: Single;
      FPopulationSize: Integer;
      FGeneratorFunction: IGeneratorFunction;
      FMutationFunction: IMutationFunction;
      FFitnessFunction: IFitnessFunction;
      FBlockTypes: TReactor.TBlockTypes;

      function GetReactorSize: TIntVector3;
      procedure SetReactorSize(const Value: TIntVector3);
      function GetFuelBasePower: Single;
      procedure SetFuelBasePower(const Value: Single);
      function GetFuelBaseHeat: Single;
      procedure SetFuelBaseHeat(const Value: Single);

      function GetBlockTypes: TReactor.TBlockTypes;
      procedure SetBlockTypes(const Value: TReactor.TBlockTypes);
      function GetBlockType(ABlockType: TReactor.TBlockType): Boolean;
      procedure SetBlockType(ABlockType: TReactor.TBlockType; const Value: Boolean);

      function GetPopulationSize: Integer;
      procedure SetPopulationSize(const Value: Integer);
      function GetGeneratorFunction: IGeneratorFunction;
      procedure SetGeneratorFunction(const Value: IGeneratorFunction);
      function GetMutationFunction: IMutationFunction;
      procedure SetMutationFunction(const Value: IMutationFunction);
      function GetFitnessFunction: IFitnessFunction;
      procedure SetFitnessFunction(const Value: IFitnessFunction);

    public
      constructor Create;

      property ReactorSize: TIntVector3 read GetReactorSize write SetReactorSize;
      property FuelBasePower: Single read GetFuelBasePower write SetFuelBasePower;
      property FuelBaseHeat: Single read GetFuelBaseHeat write SetFuelBaseHeat;

      property BlockTypes: TReactor.TBlockTypes read GetBlockTypes write SetBlockTypes;
      property HasBlockType[ABlockType: TReactor.TBlockType]: Boolean read GetBlockType write SetBlockType;

      property PopulationSize: Integer read GetPopulationSize write SetPopulationSize;
      property GeneratorFunction: IGeneratorFunction read GetGeneratorFunction write SetGeneratorFunction;
      property MutationFunction: IMutationFunction read GetMutationFunction write SetMutationFunction;
      property FitnessFunction: IFitnessFunction read GetFitnessFunction write SetFitnessFunction;

      procedure Assign(AFrom: ISettings);
      function Copy: ISettings;

    end;

    TGenerations = TObjectArray<TGeneration>;

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
    FSettings: ISettings;
    FGenerations: TGenerations;

    function GetGenerations: TGenerations.TReader;
    function GetSettings: ISettings;

  public
    constructor Create(ASettings: ISettings);
    destructor Destroy; override;

    property Generations: TGenerations.TReader read GetGenerations;

    property Settings: ISettings read GetSettings;

  end;

  IReactorIterator = TReactorEvolution.IReactorIterator;

  IReactorFunction = TReactorEvolution.IFunction;

  TReactorGenerator = TReactorEvolution.TReactorGenerator;
  IReactorGeneratorFunction = TReactorEvolution.IGeneratorFunction;
  TReactorGeneratorFunction = TReactorEvolution.TGeneratorFunction;

  IReactorMutationFunction = TReactorEvolution.IMutationFunction;
  TReactorMutationFunction = TReactorEvolution.TMutationFunction;

  IReactorFitnessFunction = TReactorEvolution.IFitnessFunction;
  TReactorFitnessFunction = TReactorEvolution.TFitnessFunction;

implementation

{ TReactorEvolution.TPopulation }

constructor TReactorEvolution.TGeneration.Create(ASettings: ISettings);
var
  Reactor: TRatedReactor;
begin
  FReactors := TReactors.Create;
  FReactors.Capacity := ASettings.PopulationSize;
  for Reactor in ASettings.GeneratorFunction do
  begin
    Reactor.FFitness := ASettings.FitnessFunction.Calculate(Reactor);
    FReactors.Add(Reactor);
  end;
  FReactors.Sort(
    function(A, B: TRatedReactor): Boolean
    begin
      Result := A.Fitness < B.Fitness;
    end
    );
end;

destructor TReactorEvolution.TGeneration.Destroy;
begin
  FReactors.Free;
  inherited;
end;

function TReactorEvolution.TGeneration.GetPopulationSize: Integer;
begin
  Result := Reactors.Count;
end;

function TReactorEvolution.TGeneration.GetReactors: TReactors.TReader;
begin
  Result := FReactors.Reader;
end;

{ TReactorEvolution }

constructor TReactorEvolution.Create(ASettings: ISettings);
begin
  FSettings := ASettings.Copy;
  FGenerations := TGenerations.Create;
  FGenerations.Add(TGeneration.Create(FSettings));
end;

destructor TReactorEvolution.Destroy;
begin
  FGenerations.Free;
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

function TReactorEvolution.GetSettings: ISettings;
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

{ TReactorEvolution.TReactorGenerator }

constructor TReactorEvolution.TReactorGenerator.Create(ASettings: ISettings);
begin
  FSettings := ASettings;
  FCurrentIndex := -1;
end;

function TReactorEvolution.TReactorGenerator.GetCurrent: TRatedReactor;
begin
  Result := FCurrent;
end;

function TReactorEvolution.TReactorGenerator.MoveNext: Boolean;
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

{ TReactorEvolution.TMutationFunction }

function TReactorEvolution.TMutationFunction.GetParentGeneration: TGeneration;
begin
  Result := FParentGeneration;
end;

procedure TReactorEvolution.TMutationFunction.SetParentGeneration(const Value: TGeneration);
begin
  FParentGeneration := Value;
end;

{ TReactorEvolution.TSettings }

function TReactorEvolution.TSettings.GetReactorSize: TIntVector3;
begin
  Result := FReactorSize;
end;

procedure TReactorEvolution.TSettings.SetReactorSize(const Value: TIntVector3);
begin
  FReactorSize := Value;
end;

function TReactorEvolution.TSettings.GetFuelBasePower: Single;
begin
  Result := FFuelBasePower;
end;

procedure TReactorEvolution.TSettings.SetFuelBasePower(const Value: Single);
begin
  FFuelBasePower := Value;
end;

function TReactorEvolution.TSettings.GetFuelBaseHeat: Single;
begin
  Result := FFuelBaseHeat;
end;

procedure TReactorEvolution.TSettings.SetFuelBaseHeat(const Value: Single);
begin
  FFuelBaseHeat := Value;
end;

function TReactorEvolution.TSettings.GetPopulationSize: Integer;
begin
  Result := FPopulationSize;
end;

procedure TReactorEvolution.TSettings.SetPopulationSize(const Value: Integer);
begin
  FPopulationSize := Value;
end;

function TReactorEvolution.TSettings.GetBlockType(ABlockType: TReactor.TBlockType): Boolean;
begin
  Result := ABlockType in FBlockTypes;
end;

function TReactorEvolution.TSettings.GetBlockTypes: TReactor.TBlockTypes;
begin
  Result := FBlockTypes;
end;

function TReactorEvolution.TSettings.GetFitnessFunction: IFitnessFunction;
begin
  Result := FFitnessFunction;
end;

procedure TReactorEvolution.TSettings.SetBlockType(ABlockType: TReactor.TBlockType; const Value: Boolean);
begin
  if Value then
    Include(FBlockTypes, ABlockType)
  else
    Exclude(FBlockTypes, ABlockType);
end;

procedure TReactorEvolution.TSettings.SetBlockTypes(const Value: TReactor.TBlockTypes);
begin
  FBlockTypes := Value;
end;

procedure TReactorEvolution.TSettings.SetFitnessFunction(const Value: IFitnessFunction);
begin
  FFitnessFunction := Value;
end;

function TReactorEvolution.TSettings.GetGeneratorFunction: IGeneratorFunction;
begin
  Result := FGeneratorFunction;
end;

procedure TReactorEvolution.TSettings.SetGeneratorFunction(const Value: IGeneratorFunction);
begin
  FGeneratorFunction := Value;
end;

function TReactorEvolution.TSettings.GetMutationFunction: IMutationFunction;
begin
  Result := FMutationFunction;
end;

procedure TReactorEvolution.TSettings.SetMutationFunction(const Value: IMutationFunction);
begin
  FMutationFunction := Value;
end;

procedure TReactorEvolution.TSettings.Assign(AFrom: ISettings);
begin
  ReactorSize := AFrom.ReactorSize;
  FuelBasePower := AFrom.FuelBasePower;
  FuelBaseHeat := AFrom.FuelBaseHeat;
  PopulationSize := AFrom.PopulationSize;
  // TODO: Must create copy functions!
  FitnessFunction := AFrom.FitnessFunction;
  GeneratorFunction := AFrom.GeneratorFunction;
  MutationFunction := AFrom.MutationFunction;
  BlockTypes := AFrom.BlockTypes;
end;

function TReactorEvolution.TSettings.Copy: ISettings;
begin
  Result := TSettings.Create;
  Result.Assign(Self);
end;

constructor TReactorEvolution.TSettings.Create;
begin
  ReactorSize := 1;
  FuelBasePower := 100;
  FuelBaseHeat := 10;
  PopulationSize := 20;
  BlockTypes := TReactor.BlockTypes;
end;

{ TReactorEvolution.TFunction }

constructor TReactorEvolution.TFunction.Create(ASettings: ISettings);
begin
  FSettings := ASettings;
end;

end.
