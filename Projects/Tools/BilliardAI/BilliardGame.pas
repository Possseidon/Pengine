unit BilliardGame;

interface

uses
  System.SysUtils,
  System.Math,

  Pengine.Vector,
  Pengine.Box2D,
  Pengine.Collections,
  Pengine.EventHandling, Pengine.Utility;

type

  EBilliard = class(Exception);

  TBilliard = class
  public type

    TPocket = class;

    TBall = class
    public type

      TType = (
        btNone,
        btFull,
        btHalf,
        btFullHalf,
        btEight,
        btCue,
        btAll
      );

      TIndex = 1 .. 16;
      TIndices = set of TIndex;

      TEventInfo = TSenderEventInfo<TBall>;

      TEvent = TEvent<TEventInfo>;

      TCollideBallEventInfo = class(TEventInfo)
      private
        FOther: TBall;

      public
        constructor Create(ASender: TBall; AOtherBall: TBall);

        property Other: TBall read FOther;

      end;

      TCollideBallEvent = TEvent<TCollideBallEventInfo>;

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
      LinearDamping = 1.2;
      AngularDamping = 1.0;

      DefaultBallGridPos: array [TIndex] of TVector2 = (
        // Full
        (X: 0; Y: 0), (X: 0; Y: 1), (X: 0; Y: 2), (X: 0; Y: 3), (X: 4; Y: 0), (X: 1; Y: 3), (X: 2; Y: 1),

        // Eight
        (X: 1; Y: 1),

        // Half
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
      FOnCollideBall: TCollideBallEvent;
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

      procedure BeginContact(AInfo: TWorld.TContactEventInfo);

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

      function OnPocketedChanged: TEvent.TAccess;
      function OnLocationChanged: TEvent.TAccess;

      /// <remarks>Only used on the cue ball.</remarks>
      function OnCollideBall: TCollideBallEvent.TAccess;
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

    // TODO: TTurn für Spielerzug jedesmal erstellen, statt alles im Spieler
    TPlayer = class
    public type

      TOrder = (poFirst, poSecond);

    private
      FBilliard: TBilliard;
      FOrder: TOrder;
      FBallType: TBall.TType;
      FFirstCollision: Boolean;
      FMadeFoolTurn: Boolean;

      function GetOtherPlayer: TPlayer;
      procedure SetBallType(const Value: TBall.TType);

      procedure CueCollidesWithBall(AInfo: TBall.TCollideBallEventInfo);

    public
      constructor Create(ABilliard: TBilliard; AOrder: TOrder);

      property Billiard: TBilliard read FBilliard;

      property OtherPlayer: TPlayer read GetOtherPlayer;
      property Order: TOrder read FOrder;
      function IsCurrent: Boolean;

      function HasBallType: Boolean;
      property BallType: TBall.TType read FBallType write SetBallType;

      property MadeFoolTurn: Boolean read FMadeFoolTurn;

      procedure MakeTurn(AForce: TVector2);
      procedure EndTurn;

    end;

    TState = (bsWaitingForInput, bsRunning, bsCuePlacement);

    TEventInfo = TSenderEventInfo<TBilliard>;

    TEvent = TEvent<TEventInfo>;

  private
    FWorld: TWorld;
    FTable: TTable;
    FBalls: array [TBall.TIndex] of TBall;
    FPlayers: array [TPlayer.TOrder] of TPlayer;
    FCurrentPlayer: TPlayer.TOrder;
    FState: TState;
    FOnBallPocketedChange: TBall.TEvent;
    FOnStateChange: TEvent;

    function GetBall(AIndex: TBall.TIndex): TBall;
    function GetCueBall: TBall;
    function GetEightBall: TBall;

    procedure BallPocketedChange(AInfo: TBall.TEventInfo);
    function GetPlayer(AOrder: TPlayer.TOrder): TPlayer;
    function GetCurrentPlayer: TPlayer;
    procedure ChangeState(AState: TState);
    procedure SetCurrentPlayer(const Value: TPlayer);

  public
    constructor Create;
    destructor Destroy; override;

    property World: TWorld read FWorld;

    property State: TState read FState;

    property Table: TTable read FTable;
    property Balls[AIndex: TBall.TIndex]: TBall read GetBall;
    property EightBall: TBall read GetEightBall;
    property CueBall: TBall read GetCueBall;

    function AllPocketed(ABalls: TBall.TIndices): Boolean;
    function AllFullsPocketed: Boolean;
    
    procedure HitCueBall(AForce: TVector2);
    function AllBallsStill: Boolean;

    property Players[AOrder: TPlayer.TOrder]: TPlayer read GetPlayer;
    property CurrentPlayer: TPlayer read GetCurrentPlayer write SetCurrentPlayer;

    procedure Step(ATimeStep: Single);
    procedure SimulateAll(ATimeStep: Single);
    procedure Reset;

    function OnBallPocketedChange: TBall.TEvent.TAccess;
    function OnStateChange: TEvent.TAccess;

  end;

implementation

{ TBilliard.TBall }

procedure TBilliard.TBall.BeginContact(AInfo: TWorld.TContactEventInfo);
var
  Other: TObject;
begin
  if AInfo.Contact.FixtureA = Body.Fixtures.First then
    Other := AInfo.Contact.FixtureB.Body.UserData
  else if AInfo.Contact.FixtureB = Body.Fixtures.First then
    Other := AInfo.Contact.FixtureA.Body.UserData
  else
    Other := nil;

  if Other is TBall then
    FOnCollideBall.Execute(TCollideBallEventInfo.Create(Self, TBall(Other)));
end;

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

  if IsType(btCue) then
    Billiard.World.OnBeginContact.Add(BeginContact);
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

function TBilliard.TBall.OnCollideBall: TCollideBallEvent.TAccess;
begin
  Result := FOnCollideBall.Access;
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
end;

destructor TBilliard.Destroy;
var
  I: TBall.TIndex;
begin
  FTable.Free;
  for I := Low(TBall.TIndex) to High(TBall.TIndex) do
    FBalls[I].Free;
  FWorld.Free;
  inherited;
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
  FState := bsRunning;
end;

function TBilliard.OnBallPocketedChange: TBall.TEvent.TAccess;
begin
  Result := FOnBallPocketedChange.Access;
end;

function TBilliard.OnStateChange: TEvent.TAccess;
begin
  Result := FOnStateChange.Access;
end;

procedure TBilliard.Reset;
var
  Ball: TBall;
begin
  ChangeState(bsWaitingForInput);
  for Ball in FBalls do
    Ball.Reset;
end;

procedure TBilliard.ChangeState(AState: TState);
begin
  FState := AState;
  FOnStateChange.Execute(TEventInfo.Create(Self));
end;

procedure TBilliard.SetCurrentPlayer(const Value: TPlayer);
var
  Player: TPlayer;
begin
  for Player in FPlayers do
  begin
    if Player = Value then
    begin
      FCurrentPlayer := Value.Order;
      Exit;
    end;
  end;
  raise EBilliard.Create('Player does not belong to this billiard game.');
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
  World.Step(ATimeStep);
  for Ball in FBalls do
    Ball.UpdatePocketed;
  if (State = bsRunning) and AllBallsStill then
    CurrentPlayer.EndTurn;
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
  Billiard.CueBall.OnCollideBall.Add(CueCollidesWithBall);
end;

procedure TBilliard.TPlayer.CueCollidesWithBall(AInfo: TBall.TCollideBallEventInfo);
var
  Ball: TBall;
begin
  Ball := AInfo.Sender;
  if FFirstCollision then
  begin
    FFirstCollision := False;
    case BallType of
      btOpen:
        ;
      btFull, btHalf:
        if Billiard.AllPocketed(TBall.FullBallIndices) then
        
        ;
    end; 
  end;
end;

procedure TBilliard.TPlayer.EndTurn;
begin
  if Billiard.CueBall.Pocketed then
  begin
    FMadeFoolTurn := True;
    Billiard.ChangeState(bsCuePlacement);
    Billiard.CurrentPlayer := OtherPlayer;
  end
  else if MadeFoolTurn then
  begin
    Billiard.ChangeState(bsWaitingForInput);
    Billiard.CurrentPlayer := OtherPlayer;
  end
  else
  begin
    Billiard.ChangeState(bsWaitingForInput);
  end;
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
  FMadeFoolTurn := False;
  Billiard.ChangeState(bsRunning);
  Billiard.HitCueBall(AForce);
end;

procedure TBilliard.TPlayer.SetBallType(const Value: TBallType);
begin
  FBallType := Value;
  case BallType of
    btOpen:
      OtherPlayer.FBallType := btOpen;
    btFull:
      OtherPlayer.FBallType := btHalf;
    btHalf:
      OtherPlayer.FBallType := btFull;
  end;
end;

{ TBilliard.TBall.TCollideBallEventInfo }

constructor TBilliard.TBall.TCollideBallEventInfo.Create(ASender: TBall; AOtherBall: TBall);
begin
  inherited Create(ASender);
  FOther := AOtherBall;
end;

end.
