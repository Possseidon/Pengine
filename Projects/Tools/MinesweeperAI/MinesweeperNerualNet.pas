unit MinesweeperNerualNet;

interface

uses
  System.Threading,

  Pengine.NeuralNetwork,
  Pengine.Bitfield,
  Pengine.IntMaths,

  MinesweeperDefine,
  Pengine.Collections,
  System.Math,
  System.SysUtils;

type

  TMinesweeperNeuralNet = class
  public type

    TInput = class(TNeuralNet.TInput)
    private
      FOffset: TIntVector2;

    public
      constructor Create(ANet: TNeuralNet; AOffset: TIntVector2); reintroduce;

      property Offset: TIntVector2 read FOffset;

    end;

    TInputs = TRefArray<TInput>;

    THiddenLayers = TObjectArray<TNeuralNet.THiddenNodes>;

  private
    FNet: TNeuralNet;
    FOutput: TNeuralNet.TNeuron;
    FHiddenLayers: THiddenLayers;
    FInputs: TInputs;
    FScanRange: Integer;
    FUpdate: Integer;

    procedure SetScanRange(const Value: Integer);

    procedure BuildNetwork;

    /// <returns>revealed -> 0 .. 8<para/>not revealed -> -4<para/>out of bounds -> -8</returns>
    function GetFieldValue(AMinesweeper: TMinesweeper; APos: TIntVector2): Integer;

    function GetHiddenLayers: Integer;
    function GetHiddenLayerSize(AIndex: Integer): Integer;
    procedure SetHiddenLayerSize(AIndex: Integer; const Value: Integer);
    procedure SetHiddenLayers(const Value: Integer);

  public
    constructor Create;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    property ScanRange: Integer read FScanRange write SetScanRange;

    property HiddenLayers: Integer read GetHiddenLayers write SetHiddenLayers;
    property HiddenLayerSize[AIndex: Integer]: Integer read GetHiddenLayerSize write SetHiddenLayerSize;

    function EvaluateField(AMinesweeper: TMinesweeper; APos: TIntVector2): Single;
    function BestPosition(AMinesweeper: TMinesweeper): TIntVector2;
    function RevealBestPosition(AMinesweeper: TMinesweeper): TRevealResult; inline;

    property Net: TNeuralNet read FNet;

    procedure Randomize;
    procedure Evolve;

  end;

  TMinesweeperNeuralNetEvolver = class
  public type

    TRatedNeuralNet = class
    public const

      PlayCount = 10;
      FieldSize: TIntVector2 = (X: 7; Y: 7);
      MineCount = 8;

    private
      FFitness: Single;
      FNet: TMinesweeperNeuralNet;

      procedure InitializeNet;
      
    public
      constructor Create; overload;
      constructor Create(AParent1, AParent2: TRatedNeuralNet); overload;
      destructor Destroy; override;

      procedure Rate;

      property Net: TMinesweeperNeuralNet read FNet;
      property Fitness: Single read FFitness;

    end;

    TRatedNets = TObjectArray<TRatedNeuralNet>;

  private
    FNets: TRatedNets;

    function GetNets: TRatedNets.TReader;
    function GetBestNet: TRatedNeuralNet;
    function GetWorstNet: TRatedNeuralNet;

  public
    constructor Create(ANetCount: Integer);
    destructor Destroy; override;

    procedure RateAndSort;
    procedure Evolve;

    property Nets: TRatedNets.TReader read GetNets;
    property BestNet: TRatedNeuralNet read GetBestNet;
    property WorstNet: TRatedNeuralNet read GetWorstNet;

  end;

implementation

{ TMinesweeperNeuralNet }

procedure TMinesweeperNeuralNet.BeginUpdate;
begin
  Inc(FUpdate);
end;

function TMinesweeperNeuralNet.BestPosition(AMinesweeper: TMinesweeper): TIntVector2;
var
  Pos: TIntVector2;
  Value, BestValue: Single;
begin
  BestValue := -Infinity;
  Result := 0;
  for Pos in AMinesweeper.Size do
  begin
    if AMinesweeper.IsRevealed(Pos) then
      Continue;

    Value := EvaluateField(AMinesweeper, Pos);
    if Value > BestValue then
    begin
      BestValue := Value;
      Result := Pos;
    end;
  end;
end;

procedure TMinesweeperNeuralNet.BuildNetwork;
var
  HiddenLayer: TNeuralNet.THiddenNodes;
  Pos: TIntVector2;
  I, J: Integer;
  Node: TNeuralNet.TNode;
begin
  if FUpdate <> 0 then
    Exit;

  FInputs.Clear;
  FNet.Clear;

  for Pos in IBounds2I(-ScanRange, ScanRange) do
    if Pos <> 0 then
      FInputs.Add(TInput.Create(FNet, Pos));

  FOutput := FNet.AddOutput;

  for I := 0 to FHiddenLayers.MaxIndex do
  begin
    HiddenLayer := FHiddenLayers[I];
    for J := 0 to HiddenLayer.MaxIndex do
    begin
      HiddenLayer[J] := FNet.AddHiddenNode;

      if I = 0 then
      begin
        for Node in FInputs do
          HiddenLayer[J].AddInput(Node);
      end
      else
      begin
        for Node in FHiddenLayers[I - 1] do
          HiddenLayer[J].AddInput(Node);
      end;
    end;
  end;

  if HiddenLayers = 0 then
  begin
    for Node in FInputs do
      FOutput.AddInput(Node);
  end
  else
  begin
    for Node in FHiddenLayers.Last do
      FOutput.AddInput(Node);
  end;
end;

constructor TMinesweeperNeuralNet.Create;
begin
  FNet := TNeuralNet.Create;
  FInputs := TInputs.Create;
  FHiddenLayers := THiddenLayers.Create;
  ScanRange := 1;
end;

destructor TMinesweeperNeuralNet.Destroy;
begin
  FNet.Free;
  FHiddenLayers.Free;
  FInputs.Free;
  inherited;
end;

procedure TMinesweeperNeuralNet.EndUpdate;
begin
  Dec(FUpdate);
  if FUpdate = 0 then
    BuildNetwork;
end;

function TMinesweeperNeuralNet.EvaluateField(AMinesweeper: TMinesweeper; APos: TIntVector2): Single;
var
  Input: TInput;
begin
  for Input in FInputs do
    Input.OutputInt[IBounds1I(-8, 8)] := GetFieldValue(AMinesweeper, APos + Input.Offset);
  Result := FOutput.Output;
end;

procedure TMinesweeperNeuralNet.Evolve;
var
  Neuron: TNeuralNet.TNeuron;
  WeightedNode: TNeuralNet.TNeuron.TWeightedNode;
begin
  for Neuron in FNet.Neurons do
  begin
    for WeightedNode in Neuron.WeightedNodes do
    begin
      if Random > 0.1 then
        Continue;
      WeightedNode.Offset := WeightedNode.Offset + (Random - 0.5) * 0.05;
      WeightedNode.Weight := WeightedNode.Weight + (Random - 0.5) * 0.05;
    end;
  end;
end;

function TMinesweeperNeuralNet.GetFieldValue(AMinesweeper: TMinesweeper; APos: TIntVector2): Integer;
begin
  // revealed -> 0 .. 8
  if AMinesweeper.IsRevealed(APos) then
    Exit(AMinesweeper.AdjacentMines(APos));

  // out of bounds -> -8
  if not AMinesweeper.WrapAround then
    if not(APos in AMinesweeper.Size) then
      Exit(-8);

  // not revealed -> -4
  Result := -4;
end;

function TMinesweeperNeuralNet.GetHiddenLayers: Integer;
begin
  Result := FHiddenLayers.Count;
end;

function TMinesweeperNeuralNet.GetHiddenLayerSize(AIndex: Integer): Integer;
begin
  Result := FHiddenLayers[AIndex].Count;
  BuildNetwork;
end;

procedure TMinesweeperNeuralNet.Randomize;
var
  Neuron: TNeuralNet.TNeuron;
  WeightedNode: TNeuralNet.TNeuron.TWeightedNode;
begin
  for Neuron in FNet.Neurons do
  begin
    for WeightedNode in Neuron.WeightedNodes do
    begin
      WeightedNode.Offset := Random * 2 - 1;
      WeightedNode.Weight := Random * 2 - 1;
    end;
  end;
end;

function TMinesweeperNeuralNet.RevealBestPosition(AMinesweeper: TMinesweeper): TRevealResult;
begin
  Result := AMinesweeper.Reveal(BestPosition(AMinesweeper));
end;

procedure TMinesweeperNeuralNet.SetHiddenLayers(const Value: Integer);
begin
  if HiddenLayers = Value then
    Exit;

  while FHiddenLayers.Count < Value do
    FHiddenLayers.Add(TNeuralNet.THiddenNodes.Create);
  while FHiddenLayers.Count > Value do
    FHiddenLayers.RemoveLast;
  BuildNetwork;
end;

procedure TMinesweeperNeuralNet.SetHiddenLayerSize(AIndex: Integer; const Value: Integer);
begin
  while FHiddenLayers[AIndex].Count < Value do
    FHiddenLayers[AIndex].Add(FNet.AddHiddenNode);
  while FHiddenLayers[AIndex].Count > Value do
    FHiddenLayers[AIndex].RemoveLast;
  BuildNetwork;
end;

procedure TMinesweeperNeuralNet.SetScanRange(const Value: Integer);
begin
  if ScanRange = Value then
    Exit;
  FScanRange := Value;
  BuildNetwork;
end;

{ TMinesweeperNeuralNet.TInput }

constructor TMinesweeperNeuralNet.TInput.Create(ANet: TNeuralNet; AOffset: TIntVector2);
begin
  inherited Create(ANet);
  FOffset := AOffset;
end;

{ TMinesweeperNeuralNetEvolver }

constructor TMinesweeperNeuralNetEvolver.Create(ANetCount: Integer);
var
  I: Integer;
begin
  FNets := TRatedNets.Create;
  for I := 0 to ANetCount - 1 do
    FNets.Add(TRatedNeuralNet.Create);
end;

destructor TMinesweeperNeuralNetEvolver.Destroy;
begin
  FNets.Free;
  inherited;
end;

procedure TMinesweeperNeuralNetEvolver.Evolve;
var
  OldNets: TRefArray<TRatedNeuralNet>;
  I: Integer;
begin
  OldNets := FNets.Copy;
  FNets.OwnsObjects := False;
  OldNets.OwnsObjects := True;

  for I := 0 to FNets.MaxIndex do
    FNets[I] := TRatedNeuralNet.Create(OldNets[Random(I + 1)], OldNets[Random(I + 1)]);

  OldNets.Free;
  FNets.OwnsObjects := True;
end;

function TMinesweeperNeuralNetEvolver.GetBestNet: TRatedNeuralNet;
begin
  Result := Nets.First;
end;

function TMinesweeperNeuralNetEvolver.GetNets: TRatedNets.TReader;
begin
  Result := FNets.Reader;
end;

function TMinesweeperNeuralNetEvolver.GetWorstNet: TRatedNeuralNet;
begin
  Result := Nets.Last;
end;

procedure TMinesweeperNeuralNetEvolver.RateAndSort;
begin
  TParallel.For(0, FNets.MaxIndex, 
    procedure (I: Integer)
    begin
      FNets[I].Rate;
    end);

  FNets.Sort(
    function(A, B: TRatedNeuralNet): Boolean
    begin
      Result := A.Fitness > B.Fitness;
    end);
end;

{ TMinesweeperNeuralNetEvolver.TRatedNeuralNet }

constructor TMinesweeperNeuralNetEvolver.TRatedNeuralNet.Create;
begin
  InitializeNet;
  Net.Randomize;
end;

constructor TMinesweeperNeuralNetEvolver.TRatedNeuralNet.Create(AParent1, AParent2: TRatedNeuralNet);
var
  I, J: Integer;
begin
  InitializeNet;
  for I := 0 to Net.Net.Neurons.MaxIndex do
  begin
    for J := 0 to Net.Net.Neurons[I].WeightedNodes.MaxIndex do
    begin
      if (1 - Sqr(Random)) * AParent1.Fitness > (1 - Sqr(Random)) * AParent2.Fitness then
      begin
        Net.Net.Neurons[I].WeightedNodes[J].Weight := AParent1.Net.Net.Neurons[I].WeightedNodes[J].Weight;
        Net.Net.Neurons[I].WeightedNodes[J].Offset := AParent1.Net.Net.Neurons[I].WeightedNodes[J].Offset;
      end
      else
      begin
        Net.Net.Neurons[I].WeightedNodes[J].Weight := AParent2.Net.Net.Neurons[I].WeightedNodes[J].Weight;
        Net.Net.Neurons[I].WeightedNodes[J].Offset := AParent2.Net.Net.Neurons[I].WeightedNodes[J].Offset;
      end;
    end;
  end;
  Net.Evolve;
end;

destructor TMinesweeperNeuralNetEvolver.TRatedNeuralNet.Destroy;
begin
  FNet.Free;
  inherited;
end;

procedure TMinesweeperNeuralNetEvolver.TRatedNeuralNet.InitializeNet;
begin
  FNet := TMinesweeperNeuralNet.Create;
  FNet.BeginUpdate;
  FNet.ScanRange := 2;
  FNet.HiddenLayers := 1;
  FNet.HiddenLayerSize[0] := 25;
  FNet.EndUpdate;
end;

procedure TMinesweeperNeuralNetEvolver.TRatedNeuralNet.Rate;
var
  I: Integer;
  FitnessSum: Single;
  Minesweeper: TMinesweeper;
begin
  FitnessSum := 0;
  for I := 0 to PlayCount - 1 do
  begin
    Minesweeper := TMinesweeper.Create(FieldSize);
    Minesweeper.Generate(MineCount, Net.BestPosition(Minesweeper));
    repeat
      if Net.RevealBestPosition(Minesweeper) = rrMine then
        Break;
    until Minesweeper.IsFullyRevealed;
    FitnessSum := FitnessSum + Minesweeper.RevealedPercentage;
    Minesweeper.Free;
  end;
  FFitness := FitnessSum / PlayCount;
end;

end.
