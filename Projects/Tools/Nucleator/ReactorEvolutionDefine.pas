unit ReactorEvolutionDefine;

interface

uses
  System.SysUtils,

  Pengine.CollectionInterfaces,
  Pengine.Collections,

  ReactorDefine;

type

  TReactorEvolution = class
  public type

    IFitnessFunction = interface
      function GetName: string; virtual; abstract;

      function Calculate(AReactor: TReactor): Single; virtual; abstract;

    end;

    /// <summary>Generates a fitness value for a given reactor.</summary>
    TFitnessFunction = class abstract(TInterfacedObject)
    public
      function GetName: string; virtual; abstract;
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

    IReactorGenerator = IIterator<TRatedReactor>;

    TReactorGenerator = class(TInterfacedObject, IIterator<TRatedReactor>)
    private
      FCount: Integer;
      FCurrentIndex: Integer;
      FCurrent: TRatedReactor;

      function GetCurrent: TRatedReactor;

    protected
      function Generate(AIndex: Integer): TRatedReactor; virtual; abstract;

    public
      constructor Create(ACount: Integer);

      function MoveNext: Boolean;
      property Current: TRatedReactor read GetCurrent;

    end;

    TGeneration = class
    public type

      TReactors = TObjectArray<TRatedReactor>;

    private
      FFitnessFunction: TFitnessFunction;
      FReactors: TReactors;

      function GetReactors: TReactors.TReader;

    public
      constructor Create(AGenerator: IReactorGenerator);

      /// <summary>Ordered by fitness from worst to best.</summary>
      property Reactors: TReactors.TReader read GetReactors;

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

    /// <summary>Generates a new generation of reactors from a given generation.</summary>
    TMutationFunction = class abstract(TReactorGenerator)
    private
      FBaseGeneration: TGeneration;

    public
      constructor Create(ABaseGeneration: TGeneration);

      property BaseGeneration: TGeneration read FBaseGeneration;

    end;

    TGenerations = TObjectArray<TGeneration>;

  private
    FGenerations: TGenerations;
    FFitnessFunction: TFitnessFunction;
    FIntializationFunction: TInitializationFunction;
    FMutationFunction: TMutationFunction;

    function GetGenerations: TGenerations.TReader;

  public
    property Generations: TGenerations.TReader read GetGenerations;

  end;

implementation

{ TReactorEvolution.TPopulation }

function TReactorEvolution.TGeneration.GetReactors: TReactors.TReader;
begin
  Result := FReactors.Reader;
end;

{ TReactorEvolution }

function TReactorEvolution.GetGenerations: TGenerations.TReader;
begin
  Result := FGenerations.Reader;
end;

{ TReactorEvolution.TReactorGenerator }

constructor TReactorEvolution.TReactorGenerator.Create(ACount: Integer);
begin
  FCount := ACount;
  FCurrentIndex := -1;
end;

function TReactorEvolution.TReactorGenerator.GetCurrent: TRatedReactor;
begin
  Result := FCurrent;
end;

function TReactorEvolution.TReactorGenerator.MoveNext: Boolean;
begin
  Inc(FCurrentIndex);
  Result := FCurrentIndex < FCount;
  if Result then
    FCurrent := Generate(FCurrentIndex);
end;

end.
