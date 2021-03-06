unit Pengine.NeuralNetwork;

interface

uses
  System.Math,
  System.SysUtils,

  Pengine.Vector,
  Pengine.IntMaths,
  Pengine.Collections;

type

  ENeuralNetError = class(Exception);

  TNeuralNet = class
  public type

    TNeuron = class;

    TNode = class
    public type

      TOutputs = TRefArray<TNeuron>;

    private
      FNet: TNeuralNet;
      FOutputs: TOutputs;

    protected
      function GetOutputBool: Boolean;
      function GetOutputInt(ARange: TIntBounds1): Integer;
      function GetOutputRanged(ARange: TBounds1): Single;
      function GetOutputs: TOutputs.TReader;

      function GetOutput: Single; virtual; abstract;

      procedure Invalidate; virtual;

    public
      constructor Create(ANet: TNeuralNet);
      destructor Destroy; override;

      property Net: TNeuralNet read FNet;

      property Outputs: TOutputs.TReader read GetOutputs;

      property Output: Single read GetOutput;
      property OutputRanged[ARange: TBounds1]: Single read GetOutputRanged;
      property OutputBool: Boolean read GetOutputBool;
      property OutputInt[ARange: TIntBounds1]: Integer read GetOutputInt;

    end;

    TInput = class(TNode)
    private
      FOutput: Single;

      procedure SetOutput(const Value: Single);
      procedure SetOutputBool(const Value: Boolean);
      procedure SetOutputInt(ARange: TIntBounds1; const Value: Integer);
      procedure SetOutputRanged(ARange: TBounds1; const Value: Single);
      function GetOutputBool: Boolean;
      function GetOutputInt(ARange: TIntBounds1): Integer;
      function GetOutputRanged(ARange: TBounds1): Single;

    protected
      function GetOutput: Single; override;

    public
      constructor Create(ANet: TNeuralNet); virtual;
      destructor Destroy; override;

      property Output: Single read GetOutput write SetOutput;
      property OutputRanged[ARange: TBounds1]: Single read GetOutputRanged write SetOutputRanged;
      property OutputBool: Boolean read GetOutputBool write SetOutputBool;
      property OutputInt[ARange: TIntBounds1]: Integer read GetOutputInt write SetOutputInt;

    end;

    TNeuron = class(TNode)
    public type

      TWeightedNode = class
      private
        FParent: TNode;
        FNode: TNode;
        FOffset: Single;
        FWeight: Single;

        procedure SetWeight(const Value: Single);
        procedure SetOffset(const Value: Single);

      public
        constructor Create(AParent, ANode: TNode; AOffset: Single = 0; AWeight: Single = 0);

        property Parent: TNode read FParent;
        property Node: TNode read FNode;
        property Offset: Single read FOffset write SetOffset;
        property Weight: Single read FWeight write SetWeight;
        function Output: Single;

      end;

      TWeightedNodes = TObjectArray<TWeightedNode>;

    private
      FCalculated: Boolean;
      FWeightedNodes: TWeightedNodes;
      FOutput: Single;
      FIsOutput: Boolean;

      function GetWeightedNodes: TWeightedNodes.TReader;

    protected
      procedure Invalidate; override;
      procedure CalculateOutput;

      function GetOutput: Single; override;

    public
      constructor Create(ANet: TNeuralNet; AIsOutput: Boolean);
      destructor Destroy; override;

      property IsOutput: Boolean read FIsOutput;

      property WeightedNodes: TWeightedNodes.TReader read GetWeightedNodes;

      function AddInput(ANode: TNode; AOffset: Single = 0; AWeight: Single = 0): TWeightedNode;
      procedure RemoveInput(ANode: TNode);

      procedure ClearInputs;

    end;

    TNodes = TRefArray<TNode>;
    TInputs = TRefArray<TInput>;
    TNeurons = TRefArray<TNeuron>;
    THiddenNodes = TRefArray<TNeuron>;
    TOutputs = TRefArray<TNeuron>;

  private
    FNodes: TNodes;
    FInputs: TInputs;
    FNeurons: TNeurons;
    FHiddenNodes: THiddenNodes;
    FOutputs: TOutputs;

    function GetHiddenNodes: THiddenNodes.TReader;
    function GetInputs: TInputs.TReader;
    function GetNodes: TNodes.TReader;
    function GetOutputs: TOutputs.TReader;
    function GetNeurons: TNeurons.TReader;

    procedure FreeNodes;

  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>Turns any given value into range <c>(-1, +1)</c>, but keeps relations.</summary>
    class function Sigmoid(AValue: Single): Single; static;

    procedure Clear;

    function AddInput: TInput; overload;
    function AddInput<T: TInput>: T; overload;
    function AddHiddenNode: TNeuron;
    function AddOutput: TNeuron;

    property Nodes: TNodes.TReader read GetNodes;
    property Inputs: TInputs.TReader read GetInputs;
    property Neurons: TNeurons.TReader read GetNeurons;
    property HiddenNodes: THiddenNodes.TReader read GetHiddenNodes;
    property Outputs: TOutputs.TReader read GetOutputs;

    procedure Randomize;

  end;

  TLayeredNeuralNet = class
  private
    FNet: TNeuralNet;
    FInputCount: Integer;
    FHiddenLayers: array of Integer;
    FOutputCount: Integer;
    FUpdate: Integer;

    procedure BuildNetwork;

    function GetHiddenLayerCount: Integer;
    function GetHiddenNodes: TNeuralNet.THiddenNodes.TReader;
    function GetInputCount: Integer;
    function GetInputs: TNeuralNet.TInputs.TReader;
    function GetNeurons: TNeuralNet.TNeurons.TReader;
    function GetNodes: TNeuralNet.TNodes.TReader;
    function GetOutputCount: Integer;
    function GetOutputs: TNeuralNet.TOutputs.TReader;
    procedure SetHiddenLayerCount(const Value: Integer);
    procedure SetInputCount(const Value: Integer);
    procedure SetOutputCount(const Value: Integer);
    function GetHiddenLayer(AIndex: Integer): Integer;
    procedure SetHiddenLayer(AIndex: Integer; const Value: Integer);

  public
    constructor Create;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    property InputCount: Integer read GetInputCount write SetInputCount;
    property OutputCount: Integer read GetOutputCount write SetOutputCount;
    property HiddenLayerCount: Integer read GetHiddenLayerCount write SetHiddenLayerCount;
    property HiddenLayers[AIndex: Integer]: Integer read GetHiddenLayer write SetHiddenLayer;

    property Nodes: TNeuralNet.TNodes.TReader read GetNodes;
    property Inputs: TNeuralNet.TInputs.TReader read GetInputs;
    property Neurons: TNeuralNet.TNeurons.TReader read GetNeurons;
    property HiddenNodes: TNeuralNet.THiddenNodes.TReader read GetHiddenNodes;
    property Outputs: TNeuralNet.TOutputs.TReader read GetOutputs;

    procedure Randomize;

  end;

implementation

{ TNeuralNet.TInput }

constructor TNeuralNet.TInput.Create(ANet: TNeuralNet);
begin
  inherited Create(ANet);
  Net.FInputs.Add(Self);
end;

destructor TNeuralNet.TInput.Destroy;
begin
  if Net <> nil then
    Net.FInputs.Remove(Self);
  inherited;
end;

function TNeuralNet.TInput.GetOutput: Single;
begin
  Result := FOutput;
end;

function TNeuralNet.TInput.GetOutputBool: Boolean;
begin
  Result := inherited;
end;

function TNeuralNet.TInput.GetOutputInt(ARange: TIntBounds1): Integer;
begin
  Result := inherited;
end;

function TNeuralNet.TInput.GetOutputRanged(ARange: TBounds1): Single;
begin
  Result := inherited;
end;

procedure TNeuralNet.TInput.SetOutput(const Value: Single);
begin
  if Output = Value then
    Exit;
  FOutput := Value;
  Invalidate;
end;

procedure TNeuralNet.TInput.SetOutputBool(const Value: Boolean);
begin
  Output := IfThen(Value, 1, -1);
end;

procedure TNeuralNet.TInput.SetOutputInt(ARange: TIntBounds1; const Value: Integer);
begin
  Output := TBounds1(ARange).Convert(Value, Bounds1(-1, +1));
end;

procedure TNeuralNet.TInput.SetOutputRanged(ARange: TBounds1; const Value: Single);
begin
  Output := ARange.Convert(Value, Bounds1(-1, +1));
end;

{ TNeuralNet.TNode }

constructor TNeuralNet.TNode.Create(ANet: TNeuralNet);
begin
  FNet := ANet;
  FOutputs := TOutputs.Create;
  Net.FNodes.Add(Self);
end;

destructor TNeuralNet.TNode.Destroy;
var
  Node: TNeuron;
begin
  Invalidate;
  if Net <> nil then
  begin
    Net.FNodes.Remove(Self);
    for Node in Outputs.InReverse do
      Node.RemoveInput(Self);
  end;
  FOutputs.Free;
  inherited;
end;

function TNeuralNet.TNode.GetOutputBool: Boolean;
begin
  Result := Output >= 0;
end;

function TNeuralNet.TNode.GetOutputInt(ARange: TIntBounds1): Integer;
begin
  Result := Floor(Bounds1(-1, 1).Convert(Output, ARange) + 0.5);
end;

function TNeuralNet.TNode.GetOutputRanged(ARange: TBounds1): Single;
begin
  Result := Bounds1(-1, 1).Convert(Output, ARange);
end;

function TNeuralNet.TNode.GetOutputs: TOutputs.TReader;
begin
  Result := FOutputs.Reader;
end;

procedure TNeuralNet.TNode.Invalidate;
var
  Node: TNode;
begin
  for Node in Outputs do
    Node.Invalidate;
end;

{ TNeuralNet }

function TNeuralNet.AddHiddenNode: TNeuron;
begin
  Result := TNeuron.Create(Self, False);
end;

function TNeuralNet.AddInput: TInput;
begin
  Result := TInput.Create(Self);
end;

function TNeuralNet.AddInput<T>: T;
begin
  Result := T.Create(Self);
end;

function TNeuralNet.AddOutput: TNeuron;
begin
  Result := TNeuron.Create(Self, True);
end;

procedure TNeuralNet.Clear;
begin
  FInputs.Clear;
  FOutputs.Clear;
  FNeurons.Clear;
  FHiddenNodes.Clear;
  FreeNodes;
  FNodes.Clear;
end;

constructor TNeuralNet.Create;
begin
  FNodes := TNodes.Create;
  FInputs := TInputs.Create;
  FNeurons := TNeurons.Create;
  FHiddenNodes := THiddenNodes.Create;
  FOutputs := TOutputs.Create;
end;

destructor TNeuralNet.Destroy;
begin
  FInputs.Free;
  FNeurons.Free;
  FHiddenNodes.Free;
  FOutputs.Free;
  FreeNodes;
  FNodes.Free;
  inherited;
end;

procedure TNeuralNet.FreeNodes;
var
  Node: TNode;
begin
  for Node in Nodes do
  begin
    Node.FNet := nil;
    Node.Free;
  end;
end;

function TNeuralNet.GetHiddenNodes: THiddenNodes.TReader;
begin
  Result := FHiddenNodes.Reader;
end;

function TNeuralNet.GetInputs: TInputs.TReader;
begin
  Result := FInputs.Reader;
end;

function TNeuralNet.GetNeurons: TNeurons.TReader;
begin
  Result := FNeurons.Reader;
end;

function TNeuralNet.GetNodes: TNodes.TReader;
begin
  Result := FNodes.Reader;
end;

function TNeuralNet.GetOutputs: TOutputs.TReader;
begin
  Result := FOutputs.Reader;
end;

procedure TNeuralNet.Randomize;
var
  Neuron: TNeuron;
  Node: TNeuron.TWeightedNode;
begin
  for Neuron in Neurons do
    for Node in Neuron.WeightedNodes do
    begin
      Node.Weight := Random * 2 - 1;
      Node.Offset := Random * 2 - 1;
    end;
end;

class function TNeuralNet.Sigmoid(AValue: Single): Single;
begin
  Result := AValue / (1 + Abs(AValue));
end;

{ TNeuralNet.TNeuron }

function TNeuralNet.TNeuron.AddInput(ANode: TNode; AOffset, AWeight: Single): TWeightedNode;
begin
  ANode.FOutputs.Add(Self);
  Result := FWeightedNodes.Add(TWeightedNode.Create(Self, ANode, AOffset, AWeight));
  Invalidate;
end;

procedure TNeuralNet.TNeuron.CalculateOutput;
var
  Node: TWeightedNode;
begin
  FOutput := 0;
  for Node in WeightedNodes do
    FOutput := FOutput + Node.Output;
  FOutput := Sigmoid(FOutput / WeightedNodes.Count * 2);
  FCalculated := True;
end;

procedure TNeuralNet.TNeuron.ClearInputs;
var
  Node: TWeightedNode;
begin
  for Node in FWeightedNodes do
    RemoveInput(Node.Node);
end;

constructor TNeuralNet.TNeuron.Create(ANet: TNeuralNet; AIsOutput: Boolean);
begin
  inherited Create(ANet);
  FWeightedNodes := TWeightedNodes.Create;
  FIsOutput := AIsOutput;
  Net.FNeurons.Add(Self);
  if IsOutput then
    Net.FOutputs.Add(Self)
  else
    Net.FHiddenNodes.Add(Self);
end;

destructor TNeuralNet.TNeuron.Destroy;
begin
  FWeightedNodes.Free;
  if Net <> nil then
  begin
    Net.FNeurons.Remove(Self);
    if IsOutput then
      Net.FOutputs.Remove(Self)
    else
      Net.FHiddenNodes.Remove(Self);
  end;
  inherited;
end;

function TNeuralNet.TNeuron.GetOutput: Single;
begin
  if not FCalculated then
    CalculateOutput;
  Result := FOutput;
end;

function TNeuralNet.TNeuron.GetWeightedNodes: TWeightedNodes.TReader;
begin
  Result := FWeightedNodes.Reader;
end;

procedure TNeuralNet.TNeuron.Invalidate;
begin
  if not FCalculated then
    Exit;
  FCalculated := False;
  inherited;
end;

procedure TNeuralNet.TNeuron.RemoveInput(ANode: TNode);
var
  Index: Integer;
begin
  Index := FWeightedNodes.FindFirstIndex(
    function(AWeightedNode: TWeightedNode): Boolean
    begin
      Result := AWeightedNode.Node = ANode;
    end);
  if Index = -1 then
    raise ENeuralNetError.Create('Node not found.');
  FWeightedNodes.RemoveAt(Index);
  ANode.FOutputs.Remove(Self);
  Invalidate;
end;

{ TNeuralNet.TNeuron.TWeightedNode }

constructor TNeuralNet.TNeuron.TWeightedNode.Create(AParent, ANode: TNode; AOffset, AWeight: Single);
begin
  FParent := AParent;
  FNode := ANode;
  FOffset := AOffset;
  FWeight := AWeight;
end;

function TNeuralNet.TNeuron.TWeightedNode.Output: Single;
begin
  Result := Offset + Node.Output * Weight;
end;

procedure TNeuralNet.TNeuron.TWeightedNode.SetOffset(const Value: Single);
begin
  if Offset = Value then
    Exit;
  FOffset := Value;
  Parent.Invalidate;
end;

procedure TNeuralNet.TNeuron.TWeightedNode.SetWeight(const Value: Single);
begin
  if Weight = Value then
    Exit;
  FWeight := Value;
  Parent.Invalidate;
end;

{ TLayeredNeuralNet }

procedure TLayeredNeuralNet.BeginUpdate;
begin
  Inc(FUpdate);
end;

procedure TLayeredNeuralNet.BuildNetwork;
var
  I, J: Integer;
  Layer, LastLayer, CurrentLayer: Integer;
  Node: TNeuralNet.TNode;
  Neuron: TNeuralNet.TNeuron;
begin
  if FUpdate <> 0 then
    Exit;

  FNet.Clear;

  for I := 0 to InputCount - 1 do
    FNet.AddInput;

  LastLayer := 0;
  CurrentLayer := 0;
  for Layer := 0 to HiddenLayerCount - 1 do
  begin
    for I := 0 to HiddenLayers[Layer] do
    begin
      Neuron := FNet.AddHiddenNode;
      if Layer = 0 then
      begin
        for Node in FNet.Inputs do
          Neuron.AddInput(Node);
      end
      else
      begin
        for J := LastLayer to CurrentLayer - 1 do
          Neuron.AddInput(FNet.HiddenNodes[J]); 
      end;
    end;  
    LastLayer := CurrentLayer;                      
    Inc(CurrentLayer, HiddenLayers[Layer]);
  end;

  for I := 0 to OutputCount - 1 do
  begin
    Neuron := FNet.AddOutput;
    if HiddenLayerCount = 0 then
    begin
      for Node in FNet.Inputs do
        Neuron.AddInput(Node);
    end
    else
    begin
      for Node in FNet.HiddenNodes.InReverse do
      begin
        Neuron.AddInput(Node);
      end;
    end;
  end;
end;

constructor TLayeredNeuralNet.Create;
begin
  FNet := TNeuralNet.Create;
end;

destructor TLayeredNeuralNet.Destroy;
begin
  FNet.Free;
  inherited;
end;

procedure TLayeredNeuralNet.EndUpdate;
begin
  Dec(FUpdate);
  if FUpdate = 0 then
    BuildNetwork;
end;

function TLayeredNeuralNet.GetHiddenLayer(AIndex: Integer): Integer;
begin
  Result := FHiddenLayers[AIndex];
end;

function TLayeredNeuralNet.GetHiddenLayerCount: Integer;
begin
  Result := Length(FHiddenLayers);
end;

function TLayeredNeuralNet.GetHiddenNodes: TNeuralNet.THiddenNodes.TReader;
begin
  Result := FNet.HiddenNodes;
end;

function TLayeredNeuralNet.GetInputCount: Integer;
begin
  Result := FInputCount;
end;

function TLayeredNeuralNet.GetInputs: TNeuralNet.TInputs.TReader;
begin
  Result := FNet.Inputs;
end;

function TLayeredNeuralNet.GetNeurons: TNeuralNet.TNeurons.TReader;
begin
  Result := FNet.Neurons;
end;

function TLayeredNeuralNet.GetNodes: TNeuralNet.TNodes.TReader;
begin
  Result := FNet.Nodes;
end;

function TLayeredNeuralNet.GetOutputCount: Integer;
begin
  Result := FOutputCount;
end;

function TLayeredNeuralNet.GetOutputs: TNeuralNet.TOutputs.TReader;
begin
  Result := FNet.Outputs;
end;

procedure TLayeredNeuralNet.Randomize;
begin
  FNet.Randomize;
end;

procedure TLayeredNeuralNet.SetHiddenLayer(AIndex: Integer; const Value: Integer);
begin
  if HiddenLayers[AIndex] = Value then
    Exit;
  FHiddenLayers[AIndex] := Value;
  BuildNetwork;
end;

procedure TLayeredNeuralNet.SetHiddenLayerCount(const Value: Integer);
begin
  if HiddenLayerCount = Value then
    Exit;
  SetLength(FHiddenLayers, Value);
  BuildNetwork;
end;

procedure TLayeredNeuralNet.SetInputCount(const Value: Integer);
begin
  if InputCount = Value then
    Exit;
  FInputCount := Value;
  BuildNetwork;
end;

procedure TLayeredNeuralNet.SetOutputCount(const Value: Integer);
begin
  if OutputCount = Value then
    Exit;
  FOutputCount := Value;
  BuildNetwork;
end;

end.
