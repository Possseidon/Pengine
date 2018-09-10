unit BilliardGame;

interface

uses
  System.SysUtils,
  System.Math,

  Pengine.Vector,
  Pengine.Box2D,
  Pengine.Collections,
  Pengine.EventHandling;

type

  EBilliard = class(Exception);

  TBilliard = class
  public type

    TPocket = class;

    TBall = class
    public type

      TType = (
        btNone,
        btSolids,
        btStripes,
        btSolidsStripes,
        btEight,
        btCue,
        btAll
        );

      TIndex = 1 .. 16;
      TIndices = set of TIndex;

      TEventInfo = TSenderEventInfo<TBall>;

      TEvent = TEvent<TEventInfo>;

    public const

      Count = High(TIndex);

      // Indices
      CueBallIndex = 16;
      Indices: array [TType] of TIndices = (
        [],
        [1 .. 7],
        [9 .. 15],
        [1 .. 7, 9 .. 15],
        [8],
        [16],
        [1 .. 16]
        );

      // Ball Properties
      Diameter = 0.14;
      Radius = Diameter / 2;

      // Physics Properties
      Density = 1.0;
      Friction = 0.05;
      Restitution = 0.97;
      LinearDamping = 1.5;
      AngularDamping = 2.5;

      DefaultBallGridPos: array [TIndex] of TVector2 = (
        // Solids
        (X: 0; Y: 0), (X: 0; Y: 1), (X: 0; Y: 2), (X: 0; Y: 3), (X: 4; Y: 0), (X: 1; Y: 3), (X: 2; Y: 1),

        // Eight
        (X: 1; Y: 1),

        // Stripes
        (X: 1; Y: 0), (X: 2; Y: 0), (X: 3; Y: 0), (X: 0; Y: 4), (X: 3; Y: 1), (X: 1; Y: 2), (X: 2; Y: 2),

        // Cue (unused)
        (X: 0; Y: 0));

    private
      FBilliard: TBilliard;
      FBody: TBody;
      FIndex: TIndex;
      FPocket: TPocket;
      FOnPocketedChanged: TEvent;
      FOnLocationChanged: TEvent;
      FOnCollideTable: TEvent;

      function GetDefaultPos: TVector2;
      function GetPos: TVector2;
      procedure SetPos(const Value: TVector2);
      function GetVelocity: TVector2;
      procedure SetVelocity(const Value: TVector2);
      function GetAngVelocity: Single;
      procedure SetAngVelocity(const Value: Single);
      function GetMoving: Boolean;
      function GetPocketed: Boolean;
      procedure SetPocket(const Value: TPocket);

      procedure BodyLocationChanged(AInfo: TBody.TEventInfo);
      function GetRotation: Single;
      procedure SetRotation(const Value: Single);

    public
      constructor Create(ABilliard: TBilliard; AIndex: Integer);

      property Billiard: TBilliard read FBilliard;
      property Index: TIndex read FIndex;
      property DefaultPos: TVector2 read GetDefaultPos;

      function IsType(AType: TType): Boolean;

      property Pocketed: Boolean read GetPocketed;
      property Pocket: TPocket read FPocket write SetPocket;

      procedure UpdatePocketed;

      function OnPocket(APocket: TPocket): Boolean; overload;
      function OnPocket: TPocket; overload;

      property Body: TBody read FBody;
      property Pos: TVector2 read GetPos write SetPos;
      property Velocity: TVector2 read GetVelocity write SetVelocity;
      property Rotation: Single read GetRotation write SetRotation;
      property AngVelocity: Single read GetAngVelocity write SetAngVelocity;

      property Moving: Boolean read GetMoving;

      procedure Hit(AForce: TVector2);
      procedure Reset;
      procedure Place(APosition: TVector2);

      function OnPocketedChanged: TEvent.TAccess;
      function OnLocationChanged: TEvent.TAccess;

      function OnCollideTable: TEvent.TAccess;

    end;

    TPocket = class
    public type

      TIndex = 1 .. 6;
      TIndices = set of TIndex;

    public const

      EdgeOffset = 0.057;
      EdgeRadius = 0.16;
      MiddleOffset = 0.14;
      MiddleRadius = 0.14;
      PocketedMiddleOffset = 0.07;
      PocketedEdgeOffset = 0.02;

      Edge: TIndices = [1, 2, 5, 6];
      Middle: TIndices = [3, 4];
      Top: TIndices = [1 .. 3];
      Bottom: TIndices = [4 .. 6];
      Left: TIndices = [1, 6];
      Right: TIndices = [3, 4];

      Count = High(TIndex);

    private
      FIndex: TIndex;

      function GetCheckCenter: TVector2;
      function GetCheckRadius: Single;
      function GetPocketedPosition: TVector2;

    public
      constructor Create(AIndex: TIndex);

      property Index: TIndex read FIndex;

      property CheckCenter: TVector2 read GetCheckCenter;
      property CheckRadis: Single read GetCheckRadius;
      property PocketedPosition: TVector2 read GetPocketedPosition;

      function Check(APosition: TVector2): Boolean;

    end;

    TTable = class
    public const

      MiddlePocketRadius = 0.12;
      MiddlePocketBackRadius = 0.16;
      MiddlePocketBackOffset = 0.08;
      EdgePocketRadius = 0.18;
      EdgePocketBackOffset = 0.14;

      PlacementDistance = 0.0;

      PlacementBounds: TBounds2 = (
        C1: (X: - 2 + TBall.Radius + PlacementDistance; Y: - 1 + TBall.Radius + PlacementDistance);
        C2: (X: + 2 - TBall.Radius - PlacementDistance; Y: + 1 - TBall.Radius - PlacementDistance)
        );

      Restitution = 0.5;
      Friction = 10.0;

    private
      FBilliard: TBilliard;
      FPockets: array [TPocket.TIndex] of TPocket;
      FBody: TBody;

      class function CreateShape: TChain;
      function GetPocket(AIndex: TPocket.TIndex): TPocket;

    public
      constructor Create(ABilliard: TBilliard);
      destructor Destroy; override;

      property Billiard: TBilliard read FBilliard;

      property Pockets[AIndex: TPocket.TIndex]: TPocket read GetPocket;

    end;

    TPlayer = class
    public type

      TOrder = (poFirst, poSecond);

      TTurn = class
      public type

        TResult = (
          trContinue,
          trSwitch,
          trFoul,
          trFoulBreak,
          trWin,
          trLose
          );

      private
        FPlayer: TPlayer;
        FCueHitRightBallFirst: Boolean;
        FCueHitAnyBall: Boolean;
        FPocketedRightBall: Boolean;
        FPocketedWrongBall: Boolean;
        FAnyHitWall: Boolean;

        function GetBilliard: TBilliard;

        procedure BallCollideCue(AInfo: TBall.TEventInfo);
        procedure BallCollideWall(AInfo: TBall.TEventInfo);
        procedure BallPocketed(AInfo: TBall.TEventInfo);

      public
        constructor Create(APlayer: TPlayer; AForce: TVector2);
        destructor Destroy; override;

        property Billiard: TBilliard read GetBilliard;
        property Player: TPlayer read FPlayer;

        property CueHitRightBallFirst: Boolean read FCueHitRightBallFirst;
        property CueHitAnyBall: Boolean read FCueHitAnyBall;
        property PocketedRightBall: Boolean read FPocketedRightBall;
        property PocketedWrongBall: Boolean read FPocketedWrongBall;
        property AnyHitWall: Boolean read FAnyHitWall;
        function Fouled: Boolean;

        function TurnResult: TResult;

        procedure EndTurn;

      end;

    private
      FBilliard: TBilliard;
      FOrder: TOrder;
      FBallType: TBall.TType;
      FLastTurn: TTurn;

      function GetOtherPlayer: TPlayer;

    protected
      procedure TurnStarts; virtual;

    public
      constructor Create(ABilliard: TBilliard; AOrder: TOrder);
      destructor Destroy; override;

      property Billiard: TBilliard read FBilliard;

      property OtherPlayer: TPlayer read GetOtherPlayer;
      property Order: TOrder read FOrder;
      function IsCurrent: Boolean;

      property BallType: TBall.TType read FBallType write FBallType;

      property LastTurn: TTurn read FLastTurn;
      procedure MakeTurn(AForce: TVector2);
      procedure PlaceCue(APosition: TVector2);

    end;

    TState = (
      bsWaitingForInput,
      bsRunning,
      bsCuePlacement,
      bsGameOver
      );

    TEventInfo = TSenderEventInfo<TBilliard>;

    TEvent = TEvent<TEventInfo>;

  private
    FWorld: TWorld;
    FTable: TTable;
    FBalls: array [TBall.TIndex] of TBall;
    FPlayers: array [TPlayer.TOrder] of TPlayer;
    FCurrentPlayer: TPlayer.TOrder;
    FWinner: TPlayer;
    FLoser: TPlayer;
    FState: TState;
    FOnBallPocketedChange: TBall.TEvent;
    FOnStateChange: TEvent;
    FOnBallCollideCue: TBall.TEvent;
    FOnBallCollideWall: TBall.TEvent;

    function GetBall(AIndex: TBall.TIndex): TBall;
    function GetCueBall: TBall;
    function GetEightBall: TBall;

    procedure BallPocketedChange(AInfo: TBall.TEventInfo);
    function GetPlayer(AOrder: TPlayer.TOrder): TPlayer;
    function GetCurrentPlayer: TPlayer;
    procedure ChangeState(AState: TState);

    procedure BeginContact(AInfo: TWorld.TContactEventInfo);

  public
    constructor Create;
    destructor Destroy; override;

    property World: TWorld read FWorld;

    property State: TState read FState;

    procedure SwitchPlayer;
    procedure InitiateCuePlacement(ABreakCueFoul: Boolean);
    procedure EndGame(ACurrentPlayerWon: Boolean);

    property Table: TTable read FTable;
    property Balls[AIndex: TBall.TIndex]: TBall read GetBall;
    property EightBall: TBall read GetEightBall;
    property CueBall: TBall read GetCueBall;

    procedure HitCueBall(AForce: TVector2);
    function AllBallsStill: Boolean;
    function BallsPocketed(AType: TBall.TType): Boolean;

    function PositionOnTable(APosition: TVector2): Boolean;
    function CanPlaceCueBall(APosition: TVector2): Boolean;
    procedure EndTurn;
    procedure PlaceCueBall(APosition: TVector2);

    property Players[AOrder: TPlayer.TOrder]: TPlayer read GetPlayer;
    property CurrentPlayer: TPlayer read GetCurrentPlayer;
    property Winner: TPlayer read FWinner;
    property Loser: TPlayer read FLoser;

    procedure Step(ATimeStep: Single);
    procedure SimulateAll(ATimeStep: Single);
    procedure ResetBalls;

    function OnBallPocketedChange: TBall.TEvent.TAccess;
    function OnStateChange: TEvent.TAccess;
    function OnBallCollideCue: TBall.TEvent.TAccess;
    function OnBallCollideWall: TBall.TEvent.TAccess;

  end;

implementation

{ TBilliard.TBall }

procedure TBilliard.TBall.BodyLocationChanged(AInfo: TBody.TEventInfo);
begin
  FOnLocationChanged.Execute(TEventInfo.Create(Self));
end;

constructor TBilliard.TBall.Create(ABilliard: TBilliard; AIndex: Integer);
var
  BodyDef: TBody.TDef;
  FixtureDef: TFixture.TDef;
  BallShape: TCircle;
begin
  FBilliard := ABilliard;
  FIndex := AIndex;

  // Body
  BodyDef := TBody.TDef.Create(btDynamic, DefaultPos);
  BodyDef.Bullet := True;
  BodyDef.LinearDamping := LinearDamping;
  BodyDef.AngularDamping := AngularDamping;
  FBody := Billiard.World.Bodies.Add(BodyDef);

  // Shape
  BallShape := TCircle.Create(Radius);

  // Fixture
  FixtureDef := TFixture.TDef.Create(BallShape);
  FixtureDef.Density := Density;
  FixtureDef.Friction := Friction;
  FixtureDef.Restitution := Restitution;
  FBody.Fixtures.Add(FixtureDef);

  BallShape.Free;

  Body.OnLocationChanged.Add(BodyLocationChanged);
  Body.UserData := Self;

end;

function TBilliard.TBall.GetAngVelocity: Single;
begin
  Result := Body.AngularVelocity;
end;

function TBilliard.TBall.GetDefaultPos: TVector2;
var
  AxisSystem: TAxisSystem2;
begin
  if IsType(btCue) then
    Exit(Vec2(-1, 0));

  AxisSystem.S := Vec2(1, 0);
  AxisSystem.DX := Vec2(Diameter, 0).Rotate(-30);
  AxisSystem.DY := Vec2(Diameter, 0).Rotate(+30);
  Result := AxisSystem[DefaultBallGridPos[Index]];
end;

function TBilliard.TBall.GetMoving: Boolean;
begin
  Result := Body.Active and Body.Awake;
end;

function TBilliard.TBall.GetPocketed: Boolean;
begin
  Result := FPocket <> nil;
end;

function TBilliard.TBall.GetPos: TVector2;
begin
  Result := Body.Position;
end;

function TBilliard.TBall.GetRotation: Single;
begin
  Result := Body.Rotation;
end;

function TBilliard.TBall.GetVelocity: TVector2;
begin
  Result := Body.LinearVelocity;
end;

procedure TBilliard.TBall.Hit(AForce: TVector2);
begin
  Body.ApplyForceToCenter(AForce);
end;

function TBilliard.TBall.IsType(AType: TType): Boolean;
begin
  Result := Index in Indices[AType];
end;

function TBilliard.TBall.OnCollideTable: TBall.TEvent.TAccess;
begin
  Result := FOnCollideTable.Access;
end;

function TBilliard.TBall.OnLocationChanged: TBall.TEvent.TAccess;
begin
  Result := FOnLocationChanged.Access;
end;

function TBilliard.TBall.OnPocket: TPocket;
var
  I: TPocket.TIndex;
begin
  for I := Low(TPocket.TIndex) to High(TPocket.TIndex) do
  begin
    Result := Billiard.Table.Pockets[I];
    if Result.Check(Pos) then
      Exit;
  end;
  Result := nil;
end;

function TBilliard.TBall.OnPocketedChanged: TBall.TEvent.TAccess;
begin
  Result := FOnPocketedChanged.Access;
end;

procedure TBilliard.TBall.Place(APosition: TVector2);
begin
  Pos := APosition;
  Pocket := nil;
end;

procedure TBilliard.TBall.Reset;
begin
  Pos := DefaultPos;
  Velocity := 0;
  AngVelocity := 0;
  Rotation := 0;
  Pocket := nil;
end;

function TBilliard.TBall.OnPocket(APocket: TPocket): Boolean;
begin
  Result := APocket.Check(Pos);
end;

procedure TBilliard.TBall.SetAngVelocity(const Value: Single);
begin
  Body.AngularVelocity := Value;
end;

procedure TBilliard.TBall.SetPocket(const Value: TPocket);
begin
  if Pocket = Value then
    Exit;
  FPocket := Value;
  if Pocketed then
  begin
    Pos := Pocket.PocketedPosition;
    Body.Active := False;
    Velocity := 0;
  end
  else
  begin
    Pos := DefaultPos;
    Body.Active := True;
  end;
  FOnPocketedChanged.Execute(TEventInfo.Create(Self));
end;

procedure TBilliard.TBall.SetPos(const Value: TVector2);
begin
  Body.Position := Value;
  Body.UpdateLocations;
end;

procedure TBilliard.TBall.SetRotation(const Value: Single);
begin
  Body.Rotation := Value;
end;

procedure TBilliard.TBall.SetVelocity(const Value: TVector2);
begin
  Body.LinearVelocity := Value;
end;

procedure TBilliard.TBall.UpdatePocketed;
begin
  Pocket := OnPocket;
end;

{ TBilliard }

function TBilliard.BallsPocketed(AType: TBall.TType): Boolean;
var
  Index: TBall.TIndex;
begin
  for Index in TBall.Indices[AType] do
    if not FBalls[Index].Pocketed then
      Exit(False);
  Result := True;
end;

function TBilliard.AllBallsStill: Boolean;
var
  Ball: TBall;
begin
  for Ball in FBalls do
    if Ball.Moving then
      Exit(False);
  Result := True;
end;

procedure TBilliard.BallPocketedChange(AInfo: TBall.TEventInfo);
begin
  FOnBallPocketedChange.Execute(AInfo, False);
end;

constructor TBilliard.Create;
var
  I: TBall.TIndex;
  Order: TBilliard.TPlayer.TOrder;
begin
  FWorld := TWorld.Create(0);
  World.VelocityIterations := 10;
  World.PositionIterations := 5;
  FTable := TTable.Create(Self);
  for I := Low(TBall.TIndex) to High(TBall.TIndex) do
  begin
    FBalls[I] := TBall.Create(Self, I);
    FBalls[I].OnPocketedChanged.Add(BallPocketedChange);
  end;
  for Order := Low(TBilliard.TPlayer.TOrder) to High(TBilliard.TPlayer.TOrder) do
    FPlayers[Order] := TPlayer.Create(Self, Order);

  World.OnBeginContact.Add(BeginContact);
end;

destructor TBilliard.Destroy;
var
  I: TBall.TIndex;
  Player: TPlayer;
begin
  FTable.Free;
  for I := Low(TBall.TIndex) to High(TBall.TIndex) do
    FBalls[I].Free;
  for Player in FPlayers do
    Player.Free;
  FWorld.Free;
  inherited;
end;

procedure TBilliard.EndGame(ACurrentPlayerWon: Boolean);
begin
  if ACurrentPlayerWon then
    FWinner := CurrentPlayer
  else
    FWinner := CurrentPlayer.OtherPlayer;
  FLoser := Winner.OtherPlayer;
  ChangeState(bsGameOver);
end;

procedure TBilliard.EndTurn;
begin
  ChangeState(bsWaitingForInput);
end;

function TBilliard.GetBall(AIndex: TBall.TIndex): TBall;
begin
  Result := FBalls[AIndex];
end;

function TBilliard.GetCueBall: TBall;
begin
  Result := Balls[TBall.CueBallIndex];
end;

function TBilliard.GetCurrentPlayer: TPlayer;
begin
  Result := Players[FCurrentPlayer];
end;

function TBilliard.GetEightBall: TBall;
begin
  Result := Balls[8];
end;

function TBilliard.GetPlayer(AOrder: TPlayer.TOrder): TPlayer;
begin
  Result := FPlayers[AOrder];
end;

procedure TBilliard.HitCueBall(AForce: TVector2);
begin
  CueBall.Hit(AForce);
  ChangeState(bsRunning);
end;

procedure TBilliard.InitiateCuePlacement(ABreakCueFoul: Boolean);
begin
  ChangeState(bsCuePlacement);
end;

function TBilliard.OnBallCollideCue: TBall.TEvent.TAccess;
begin
  Result := FOnBallCollideCue.Access;
end;

function TBilliard.OnBallCollideWall: TBall.TEvent.TAccess;
begin
  Result := FOnBallCollideWall.Access;
end;

function TBilliard.OnBallPocketedChange: TBall.TEvent.TAccess;
begin
  Result := FOnBallPocketedChange.Access;
end;

function TBilliard.OnStateChange: TEvent.TAccess;
begin
  Result := FOnStateChange.Access;
end;

procedure TBilliard.PlaceCueBall(APosition: TVector2);
begin
  raise ENotImplemented.Create('PlaceCueBall');
end;

function TBilliard.PositionOnTable(APosition: TVector2): Boolean;
begin
  Result := APosition in TTable.PlacementBounds;
end;

procedure TBilliard.ResetBalls;
var
  Ball: TBall;
begin
  ChangeState(bsWaitingForInput);
  for Ball in FBalls do
    Ball.Reset;
end;

function TBilliard.CanPlaceCueBall(APosition: TVector2): Boolean;
var
  Ball: TBall;
begin
  Result := PositionOnTable(APosition);
  if Result then
  begin
    for Ball in FBalls do
    begin
      if Ball.IsType(btCue) then
        Continue;
      if APosition.DistanceTo(Ball.Pos) <= TBall.Radius * 2 + TTable.PlacementDistance then
        Exit(False);
    end;
  end;
end;

procedure TBilliard.ChangeState(AState: TState);
begin
  FState := AState;
  FOnStateChange.Execute(TEventInfo.Create(Self));
end;

procedure TBilliard.SimulateAll(ATimeStep: Single);
begin
  repeat
    Step(ATimeStep);
  until AllBallsStill;
  World.UpdateLocations;
end;

procedure TBilliard.Step(ATimeStep: Single);
var
  Ball: TBall;
begin
  if AllBallsStill then
    Exit;
  World.Step(ATimeStep);
  for Ball in FBalls do
    Ball.UpdatePocketed;
  if (State = bsRunning) and AllBallsStill then
    CurrentPlayer.LastTurn.EndTurn;
end;

procedure TBilliard.SwitchPlayer;
begin
  FCurrentPlayer := TPlayer.TOrder(1 - Ord(FCurrentPlayer));
end;

procedure TBilliard.BeginContact(AInfo: TWorld.TContactEventInfo);
var
  A, B: TObject;
begin
  A := AInfo.Contact.UserDataA;
  B := AInfo.Contact.UserDataB;

  if A is TBall then
  begin
    if B = CueBall then
      FOnBallCollideCue.Execute(TBall.TEventInfo.Create(TBall(A)))
    else
      FOnBallCollideWall.Execute(TBall.TEventInfo.Create(TBall(A)));
  end
  else if B is TBall then
  begin
    if A = CueBall then
      FOnBallCollideCue.Execute(TBall.TEventInfo.Create(TBall(B)))
    else
      FOnBallCollideWall.Execute(TBall.TEventInfo.Create(TBall(B)));
  end;
end;

{ TBilliard.TTable }

constructor TBilliard.TTable.Create(ABilliard: TBilliard);
var
  FixtureDef: TFixture.TDef;
  Shape: TChain;
  I: TPocket.TIndex;
begin
  FBilliard := ABilliard;

  // Body
  FBody := Billiard.World.Bodies.Add(TBody.TDef.Create(btStatic, 0));

  // Shape
  Shape := CreateShape;

  // Fixture
  FixtureDef := TFixture.TDef.Create(Shape);
  FixtureDef.Friction := Friction;
  FixtureDef.Restitution := Restitution;
  FBody.Fixtures.Add(FixtureDef);

  Shape.Free;

  for I := Low(TPocket.TIndex) to High(TPocket.TIndex) do
    FPockets[I] := TPocket.Create(I);

end;

class function TBilliard.TTable.CreateShape: TChain;
var
  I: TPocket.TIndex;
  Points: array [TPocket.TIndex, 0 .. 3] of TVector2;
  J: Integer;
begin
  // Top Middle
  Points[2, 0] := Vec2(-MiddlePocketRadius, 1);
  Points[2, 1] := Vec2(-MiddlePocketBackOffset, 1 + MiddlePocketBackRadius);
  Points[2, 2] := Vec2(+MiddlePocketBackOffset, 1 + MiddlePocketBackRadius);
  Points[2, 3] := Vec2(+MiddlePocketRadius, 1);

  // Top Left
  Points[1, 0] := Vec2(-2, 1 - EdgePocketRadius);
  Points[1, 1] := Vec2(-2 - EdgePocketBackOffset, 1);
  Points[1, 2] := Vec2(-2, 1 + EdgePocketBackOffset);
  Points[1, 3] := Vec2(-2 + EdgePocketRadius, 1);

  // Flip left to right
  for J := 0 to 3 do
    Points[3, J] := Points[1, 3 - J] * Vec2(-1, 1);

  // Flip top to bottom
  for I in TPocket.Bottom do
    for J := 0 to 3 do
      Points[I, J] := Points[7 - I, 3 - J] * Vec2(1, -1);

  Result := TChain.Create;
  Result.SetLoop(@Points[1, 0], SizeOf(Points) div SizeOf(TVector2));
end;

destructor TBilliard.TTable.Destroy;
var
  I: TPocket.TIndex;
begin
  for I := Low(TPocket.TIndex) to High(TPocket.TIndex) do
    FPockets[I].Free;

  FBody.Free;
  inherited;
end;

function TBilliard.TTable.GetPocket(AIndex: TPocket.TIndex): TPocket;
begin
  Result := FPockets[AIndex];
end;

{ TBilliard.TPocket }

function TBilliard.TPocket.Check(APosition: TVector2): Boolean;
begin
  Result := CheckCenter.DistanceTo(APosition) <= CheckRadis;
end;

constructor TBilliard.TPocket.Create(AIndex: TIndex);
begin
  FIndex := AIndex;
end;

function TBilliard.TPocket.GetCheckCenter: TVector2;
begin
  if Index in Middle then
    Result := Vec2(0, 1 + MiddleOffset)
  else
    Result := Vec2(2 + EdgeOffset, 1 + EdgeOffset);

  if Index in Left then
    Result.X := -Result.X;

  if Index in Bottom then
    Result.Y := -Result.Y;
end;

function TBilliard.TPocket.GetCheckRadius: Single;
begin
  if Index in Middle then
    Result := MiddleRadius
  else
    Result := EdgeRadius;
end;

function TBilliard.TPocket.GetPocketedPosition: TVector2;
begin
  if Index in Middle then
    Result := Vec2(0, 1 + PocketedMiddleOffset)
  else
    Result := Vec2(2, 1) + PocketedEdgeOffset;

  if Index in Left then
    Result.X := -Result.X;

  if Index in Bottom then
    Result.Y := -Result.Y;
end;

{ TBilliard.TPlayer }

constructor TBilliard.TPlayer.Create(ABilliard: TBilliard; AOrder: TOrder);
begin
  FBilliard := ABilliard;
  FOrder := AOrder;
  FBallType := btSolidsStripes;
end;

destructor TBilliard.TPlayer.Destroy;
begin
  FLastTurn.Free;
  inherited;
end;

function TBilliard.TPlayer.GetOtherPlayer: TPlayer;
begin
  Result := Billiard.Players[TOrder(1 - Ord(Order))];
end;

function TBilliard.TPlayer.IsCurrent: Boolean;
begin
  Result := Billiard.CurrentPlayer = Self;
end;

procedure TBilliard.TPlayer.MakeTurn(AForce: TVector2);
begin
  if Billiard.CueBall.Pocketed then
    raise EBilliard.Create('Cue not placed.');

  if not IsCurrent then
    raise EBilliard.Create('Not this players turn to hit the ball.');

  FLastTurn.Free;
  FLastTurn := TTurn.Create(Self, AForce);
end;

procedure TBilliard.TPlayer.PlaceCue(APosition: TVector2);
begin
  raise ENotImplemented.Create('PlaceCue');
end;

procedure TBilliard.TPlayer.TurnStarts;
begin
  // nothing by default
end;

{ TBilliard.TPlayer.TTurn }

procedure TBilliard.TPlayer.TTurn.BallCollideWall(AInfo: TBall.TEventInfo);
begin
  FAnyHitWall := True;
  Billiard.OnBallCollideWall.Remove(BallCollideWall);
end;

procedure TBilliard.TPlayer.TTurn.BallPocketed(AInfo: TBall.TEventInfo);
begin
  if AInfo.Sender.IsType(Player.BallType) then
  begin
    if Player.BallType = btSolidsStripes then
    begin
      if AInfo.Sender.IsType(btSolids) then
        Player.BallType := btSolids
      else if AInfo.Sender.IsType(btStripes) then
        Player.BallType := btStripes;
    end;
    FPocketedRightBall := True
  end
  else
    FPocketedWrongBall := True;
end;

constructor TBilliard.TPlayer.TTurn.Create(APlayer: TPlayer; AForce: TVector2);
begin
  FPlayer := APlayer;
  Billiard.HitCueBall(AForce);
  Billiard.OnBallCollideCue.Add(BallCollideCue);
  Billiard.OnBallCollideWall.Add(BallCollideWall);
  Billiard.OnBallPocketedChange.Add(BallPocketed);
  Billiard.ChangeState(bsRunning);
end;

procedure TBilliard.TPlayer.TTurn.BallCollideCue(AInfo: TBall.TEventInfo);
begin
  if AInfo.Sender.IsType(Player.BallType) then
    FCueHitRightBallFirst := True;
  FCueHitAnyBall := True;
  Billiard.OnBallCollideCue.Remove(BallCollideCue);
end;

destructor TBilliard.TPlayer.TTurn.Destroy;
begin
  if not CueHitAnyBall then
    Billiard.OnBallCollideCue.Remove(BallCollideCue);
  if not AnyHitWall then
    Billiard.OnBallCollideWall.Remove(BallCollideWall);
  Billiard.OnBallPocketedChange.Remove(BallPocketed);
  inherited;
end;

procedure TBilliard.TPlayer.TTurn.EndTurn;
var
  Result: TResult;
begin
  Result := TurnResult;

  if Result in [trWin, trLose] then
  begin
    Billiard.EndGame(Result = trWin);
    Exit;
  end;

  if Result in [trSwitch, trFoul, trFoulBreak] then
    Billiard.SwitchPlayer;

  if Result in [trFoul, trFoulBreak] then
      Billiard.InitiateCuePlacement(Result = trFoulBreak);

  if Result in [trSwitch, trContinue] then
    Billiard.EndTurn;
end;

function TBilliard.TPlayer.TTurn.Fouled: Boolean;
begin
  Result := Billiard.CueBall.Pocketed or not CueHitRightBallFirst or not AnyHitWall;
end;

function TBilliard.TPlayer.TTurn.GetBilliard: TBilliard;
begin
  Result := Player.Billiard;
end;

function TBilliard.TPlayer.TTurn.TurnResult: TResult;
begin
  if Billiard.EightBall.Pocketed then
  begin
    if Fouled or not Billiard.BallsPocketed(Player.BallType) then
      Exit(trLose);
    Exit(trWin);
  end;

  if Fouled then
    Exit(trFoul);

  if PocketedRightBall and not PocketedWrongBall then
    Exit(trContinue);

  Result := trSwitch;
end;

end.
