unit TakeitEasy.NeuralNet;

interface

uses
  System.Math,

  Pengine.NeuralNetwork,
  Pengine.IntMaths,
  Pengine.Collections,

  TakeitEasy.Game;

type

  TTakeItEasyNeuralNet = class
  public type

    TPlayer = class(TTakeItEasy.TPlayer)
    private
      FNet: TTakeItEasyNeuralNet;

      procedure SetNet(const Value: TTakeItEasyNeuralNet);

    protected
      function BeginPlacePiece(APiece: TTakeItEasy.TPiece): TTakeItEasy.TPlayer.TTurn; override;

    public
      destructor Destroy; override;

      property Net: TTakeItEasyNeuralNet read FNet write SetNet;

    end;

  private
    FNet: TLayeredNeuralNet;
    FTotalScore: Integer;
    FPlayCount: Integer;

    function GetPosValue(APos: TTakeItEasy.TBoard.TPiecePos): Single;
    function GetAverageScore: Single;

  public
    constructor Create(ARandomize: Boolean = True); overload;
    destructor Destroy; override;

    procedure PlacePiece(APos: TTakeItEasy.TBoard.TPiecePos; APiece: TTakeItEasy.TPiece);
    procedure SetCurrentPiece(APiece: TTakeItEasy.TPiece);

    property Value[APos: TTakeItEasy.TBoard.TPiecePos]: Single read GetPosValue; default;
    function BestPos: TTakeItEasy.TBoard.TPiecePos;

    procedure ResetInputs;
    procedure Randomize;

    procedure Reproduce(ANetA, ANetB: TTakeItEasyNeuralNet);

    property AverageScore: Single read GetAverageScore;

  end;

  TTakeItEasyEvolver = class
  public type

    TNets = TObjectArray<TTakeItEasyNeuralNet>;
    
  private
    FNets: TNets;
    FSimulated: Boolean;

    function GetBest: TTakeItEasyNeuralNet;
    function GetWorst: TTakeItEasyNeuralNet;

  public
    constructor Create(ANetCount: Integer);
    destructor Destroy; override;

    function Nets: TNets.TReader;

    property Best: TTakeItEasyNeuralNet read GetBest;
    property Worst: TTakeItEasyNeuralNet read GetWorst;

    property Simulated: Boolean read FSimulated;

    procedure Simulate;
    procedure Evolve;

  end;
  
implementation

{ TTakeItEasyNeuralNet.TPlayer }

function TTakeItEasyNeuralNet.TPlayer.BeginPlacePiece(APiece: TTakeItEasy.TPiece): TTakeItEasy.TPlayer.TTurn;
var
  Pos: TTakeItEasy.TBoard.TPiecePos;
begin
  FNet.SetCurrentPiece(APiece);
  Pos := FNet.BestPos;
  FNet.PlacePiece(Pos, APiece);
  Board.PlacePiece(Pos, APiece);
  if Board.IsFilled then
  begin
    Inc(FNet.FTotalScore, Board.CalculateScore);
    Inc(FNet.FPlayCount);
  end;
  Result := nil;
end;

destructor TTakeItEasyNeuralNet.TPlayer.Destroy;
begin
  if Net <> nil then
    Board.OnReset.Remove(FNet.ResetInputs);
  inherited;
end;

procedure TTakeItEasyNeuralNet.TPlayer.SetNet(const Value: TTakeItEasyNeuralNet);
begin
  Assert(Net = nil, 'Can only set neural net once.');
  FNet := Value;
  Board.OnReset.Add(FNet.ResetInputs);
end;

{ TTakeItEasyNeuralNet }

function TTakeItEasyNeuralNet.BestPos: TTakeItEasy.TBoard.TPiecePos;
var
  Pos: TTakeItEasy.TBoard.TPiecePos;
  Value, BestValue: Single;
begin
  BestValue := -Infinity;
  Result := Low(Pos);
  for Pos := Low(Pos) to High(Pos) do
  begin
    if FNet.Inputs[Pos * 3].Output <> -1 then
      Continue;
    Value := FNet.Outputs[Pos].Output;
    if Value > BestValue then
    begin
      BestValue := Value;
      Result := Pos;
    end;
  end;
end;

constructor TTakeItEasyNeuralNet.Create(ARandomize: Boolean);
begin
  FNet := TLayeredNeuralNet.Create;
  // fields + current piece
  FNet.BeginUpdate;
  FNet.InputCount := (TTakeItEasy.TBoard.FieldCount + 1) * 3;
  FNet.HiddenLayerCount := 0;
  // FNet.HiddenLayers[0] := 40;
  FNet.OutputCount := TTakeItEasy.TBoard.FieldCount;
  FNet.EndUpdate;

  ResetInputs;

  if ARandomize then
    Randomize;
end;

procedure TTakeItEasyNeuralNet.Reproduce(ANetA, ANetB: TTakeItEasyNeuralNet);
var
  I, J: Integer;
begin
  FTotalScore := 0;
  FPlayCount := 0;
  for I := 0 to FNet.Neurons.MaxIndex do
  begin
    for J := 0 to FNet.Neurons[I].WeightedNodes.MaxIndex do
    begin
      if ANetA.AverageScore * Random > ANetB.AverageScore * Random then
      begin
        FNet.Neurons[I].WeightedNodes[J].Weight := ANetA.FNet.Neurons[I].WeightedNodes[J].Weight;
        FNet.Neurons[I].WeightedNodes[J].Offset := ANetA.FNet.Neurons[I].WeightedNodes[J].Offset;
      end
      else
      begin
        FNet.Neurons[I].WeightedNodes[J].Weight := ANetB.FNet.Neurons[I].WeightedNodes[J].Weight;
        FNet.Neurons[I].WeightedNodes[J].Offset := ANetB.FNet.Neurons[I].WeightedNodes[J].Offset;
      end;
      FNet.Neurons[I].WeightedNodes[J].Weight := FNet.Neurons[I].WeightedNodes[J].Weight + Random * 0.1 - 0.05;
      FNet.Neurons[I].WeightedNodes[J].Offset := FNet.Neurons[I].WeightedNodes[J].Offset + Random * 0.1 - 0.05;
    end;
  end;
end;

destructor TTakeItEasyNeuralNet.Destroy;
begin
  FNet.Free;
  inherited;
end;

function TTakeItEasyNeuralNet.GetAverageScore: Single;
begin
  Result := FTotalScore / FPlayCount;
end;

function TTakeItEasyNeuralNet.GetPosValue(APos: TTakeItEasy.TBoard.TPiecePos): Single;
begin
  Result := FNet.Outputs[APos].Output;
end;

procedure TTakeItEasyNeuralNet.PlacePiece(APos: TTakeItEasy.TBoard.TPiecePos; APiece: TTakeItEasy.TPiece);
var
  RowDir: TTakeItEasy.TRowDirection;
begin
  for RowDir := Low(RowDir) to High(RowDir) do
    FNet.Inputs[APos * 3 + Ord(RowDir)].OutputInt[IBounds1I(0, 9)] := APiece[RowDir];
end;

procedure TTakeItEasyNeuralNet.Randomize;
begin
  FNet.Randomize;
end;

procedure TTakeItEasyNeuralNet.ResetInputs;
var
  Pos: TTakeItEasy.TBoard.TPiecePos;
begin
  for Pos := Low(Pos) to High(Pos) do
    PlacePiece(Pos, TTakeItEasy.TPiece.Empty);
end;

procedure TTakeItEasyNeuralNet.SetCurrentPiece(APiece: TTakeItEasy.TPiece);
var
  RowDir: TTakeItEasy.TRowDirection;
begin
  for RowDir := Low(RowDir) to High(RowDir) do
    FNet.Inputs[High(TTakeItEasy.TBoard.TPiecePos) * 3 + 1 + Ord(RowDir)].OutputInt[IBounds1I(0, 9)] := APiece[RowDir];
end;

{ TTakeItEasyEvolver }

constructor TTakeItEasyEvolver.Create(ANetCount: Integer);
var
  I: Integer;
begin
  FNets := TNets.Create;
  for I := 0 to ANetCount - 1 do
    FNets.Add(TTakeItEasyNeuralNet.Create);
end;

destructor TTakeItEasyEvolver.Destroy;
begin
  FNets.Free;
  inherited;
end;

procedure TTakeItEasyEvolver.Evolve;
var
  I, Middle: Integer;
begin
  FSimulated := False;

  Middle := FNets.Count div 2;
  for I := Middle to FNets.MaxIndex do
  begin
    FNets[I].Reproduce(FNets[Random(I - Middle)], FNets[Random(I - Middle)]);
  end;
end;

function TTakeItEasyEvolver.GetBest: TTakeItEasyNeuralNet;
begin
  Result := Nets.First;
end;

function TTakeItEasyEvolver.GetWorst: TTakeItEasyNeuralNet;
begin
  Result := Nets.Last;
end;

function TTakeItEasyEvolver.Nets: TNets.TReader;
begin
  Result := FNets.Reader;
end;

procedure TTakeItEasyEvolver.Simulate;
var
  Game: TTakeItEasy;
  Player: TTakeItEasyNeuralNet.TPlayer;
  Net: TTakeItEasyNeuralNet;
  I: Integer;
begin
  Game := TTakeItEasy.Create;

  for Net in Nets do
  begin
    Player := Game.AddPlayer<TTakeItEasyNeuralNet.TPlayer>;
    Player.Net := Net;
  end;

  for I := 1 to 20 do
  begin
    Game.Start;
    Game.PlayAll;
  end;

  FNets.Sort(
    function (A, B: TTakeItEasyNeuralNet): Boolean
    begin
      Result := A.AverageScore > B.AverageScore;
    end);

  Game.Free;

  FSimulated := True;
end;

end.
