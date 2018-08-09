unit BilliardGame;

interface

uses
  Pengine.Vector,
  Pengine.Box2D,
  Pengine.Collections,
  Pengine.EventHandling;

type

  TBilliard = class
  public type

    TPocket = class;

    TBall = class
    public type
     
      TIndex = 1 .. 16;
      TIndices = set of TIndex;

      TEventInfo = TSenderEventInfo<TBall>;

      TEvent = TEvent<TEventInfo>;

    public const

      Count = High(TIndex);

      // Indices
      FullBallIndices: TIndices = [1 .. 7];
      HalfBallIndices: TIndices = [9 .. 15];
      EightBallIndex: TIndex = 8;
      CueBallIndex: TIndex = 16;

      // Ball Properties
      Diameter = 0.14;
      Radius = Diameter / 2;

      // Physics Properties
      Density = 1.0;
      Friction = 0.05;
      Restitution = 0.95;
      LinearDamping = 1.0;
      AngularDamping = 1.0;

    private
      FBilliard: TBilliard;
      FBody: TBody;
      FIndex: TIndex;
      FPocket: TPocket;
      FOnPocketedChanged: TEvent;
      FOnLocationChanged: TEvent;

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
      
    public
      constructor Create(ABilliard: TBilliard; AIndex: Integer);

      property Billiard: TBilliard read FBilliard;
      property Index: TIndex read FIndex;
      property DefaultPos: TVector2 read GetDefaultPos;

      function IsCueBall: Boolean;
      function IsEightBall: Boolean;
      function IsFull: Boolean;
      function IsHalf: Boolean;

      property Pocketed: Boolean read GetPocketed;
      property Pocket: TPocket read FPocket write SetPocket;

      procedure UpdatePocketed;

      function OnPocket(APocket: TPocket): Boolean; overload;
      function OnPocket: TPocket; overload;

      property Body: TBody read FBody;
      property Pos: TVector2 read GetPos write SetPos;
      property Velocity: TVector2 read GetVelocity write SetVelocity;
      property AngVelocity: Single read GetAngVelocity write SetAngVelocity;

      property Moving: Boolean read GetMoving;

      procedure Hit(AForce: TVector2);

      function OnPocketedChanged: TEvent.TAccess;
      function OnLocationChanged: TEvent.TAccess;
      
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
      EdgePocketRadius = 0.2;
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

    TPlayer = class
    private

    end;

  private
    FWorld: TWorld;
    FTable: TTable;
    FBalls: array [TBall.TIndex] of TBall;
    FOnBallPocketedChange: TBall.TEvent;

    function GetBall(AIndex: TBall.TIndex): TBall;
    function GetCueBall: TBall;
    function GetEightBall: TBall;

    procedure BallPocketedChange(AInfo: TBall.TEventInfo);

  public
    constructor Create;
    destructor Destroy; override;

    property World: TWorld read FWorld;

    property Balls[AIndex: TBall.TIndex]: TBall read GetBall;
    property EightBall: TBall read GetEightBall;
    property CueBall: TBall read GetCueBall;

    function OnBallPocketedChange: TBall.TEvent.TAccess;

    property Table: TTable read FTable;

    function AllBallsStill: Boolean;

    procedure Step(ATimeStep: Single);

    procedure SimulateAll(ATimeStep: Single);

    procedure HitCueBall(AForce: TVector2);

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
end;

function TBilliard.TBall.GetAngVelocity: Single;
begin
  Result := Body.AngularVelocity;
end;

function TBilliard.TBall.GetDefaultPos: TVector2;
begin

end;

function TBilliard.TBall.GetMoving: Boolean;
begin
  Result := Velocity <> 0;
end;

function TBilliard.TBall.GetPocketed: Boolean;
begin
  Result := FPocket <> nil;
end;

function TBilliard.TBall.GetPos: TVector2;
begin
  Result := Body.Position;
end;

function TBilliard.TBall.GetVelocity: TVector2;
begin
  Result := Body.LinearVelocity;
end;

procedure TBilliard.TBall.Hit(AForce: TVector2);
begin
  Body.ApplyForceToCenter(AForce);
end;

function TBilliard.TBall.IsCueBall: Boolean;
begin
  Result := Index = CueBallIndex;
end;

function TBilliard.TBall.IsEightBall: Boolean;
begin
  Result := Index = EightBallIndex;
end;

function TBilliard.TBall.IsFull: Boolean;
begin
  Result := Index in FullBallIndices;
end;

function TBilliard.TBall.IsHalf: Boolean;
begin
  Result := Index in HalfBallIndices;
end;

function TBilliard.TBall.OnLocationChanged: TEvent.TAccess;
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

function TBilliard.TBall.OnPocketedChanged: TEvent.TAccess;
begin
  Result := FOnPocketedChanged.Access;
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

procedure TBilliard.TBall.SetVelocity(const Value: TVector2);
begin
  FBody.LinearVelocity := Value;
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

function TBilliard.GetEightBall: TBall;
begin
  Result := Balls[TBall.EightBallIndex];
end;

procedure TBilliard.HitCueBall(AForce: TVector2);
begin
  CueBall.Hit(AForce);
end;

function TBilliard.OnBallPocketedChange: TBall.TEvent.TAccess;
begin
  Result := FOnBallPocketedChange.Access;
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

end.
