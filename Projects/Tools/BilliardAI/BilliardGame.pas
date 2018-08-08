unit BilliardGame;

interface

uses
  Pengine.Vector,
  Pengine.Box2D;

type

  TBilliard = class
  public type

    TBall = class
    public type

      TIndex = 1 .. 16;
      TIndices = set of TIndex;

    public const

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
      LinearDamping = 0.01;

    private
      FBody: TBody;
      FIndex: TIndex;
      FPocketed: Boolean;

      function GetDefaultPos: TVector2;
      function GetPos: TVector2;
      procedure SetPos(const Value: TVector2);
      procedure SetPocketed(const Value: Boolean);

    public
      constructor Create(AWorld: TWorld; AIndex: Integer);

      property Index: TIndex read FIndex;
      property DefaultPos: TVector2 read GetDefaultPos;

      function IsCueBall: Boolean;
      function IsEightBall: Boolean;
      function IsFull: Boolean;
      function IsHalf: Boolean;

      property Pocketed: Boolean read FPocketed write SetPocketed;

      property Body: TBody read FBody;
      property Pos: TVector2 read GetPos write SetPos;

      procedure Hit(AForce: TVector2);

    end;

    TPocket = class
    public type

      TIndex = 1 .. 6;

    private

    public

    end;

    TTable = class
    public const

      Restitution = 0.75;

      MiddlePocketRadius = 0.12;

    private
      // some pocket coordinates
      // -0.84
      // -1.84
      // -0.12
      // +0.12
      // +1.84

      FPockets: array [TPocket.TIndex] of TPocket;

    public

    end;

    TPlayer = class
    private

    end;

  private
    FWorld: TWorld;
    FTable: TTable;
    FBalls: array [TBall.TIndex] of TBall;

    function GetBall(AIndex: TBall.TIndex): TBall;
    function GetCueBall: TBall;
    function GetEightBall: TBall;

  public
    constructor Create;
    destructor Destroy; override;

    property World: TWorld read FWorld;

    property Balls[AIndex: TBall.TIndex]: TBall read GetBall;
    property EightBall: TBall read GetEightBall;
    property CueBall: TBall read GetCueBall;

    procedure HitCueBall(AForce: TVector2);

  end;

implementation

{ TBilliard.TBall }

constructor TBilliard.TBall.Create(AWorld: TWorld; AIndex: Integer);
var
  BodyDef: TBody.TDef;
  FixtureDef: TFixture.TDef;
  BallShape: TCircle;
begin
  FIndex := AIndex;

  // Body
  BodyDef := TBody.TDef.Create(btDynamic, DefaultPos);
  BodyDef.FixedRotation := True;
  BodyDef.Bullet := True;
  BodyDef.LinearDamping := LinearDamping;
  FBody := AWorld.Bodies.Add(BodyDef);

  // Shape
  BallShape := TCircle.Create(Radius);

  // Fixture
  FixtureDef := TFixture.TDef.Create(BallShape);
  FixtureDef.Density := Density;
  FixtureDef.Friction := Friction;
  FixtureDef.Restitution := Restitution;
  FBody.Fixtures.Add(FixtureDef);

  BallShape.Free;
end;

function TBilliard.TBall.GetDefaultPos: TVector2;
begin

end;

function TBilliard.TBall.GetPos: TVector2;
begin
  Result := Body.Position;
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

procedure TBilliard.TBall.SetPocketed(const Value: Boolean);
begin
  FPocketed := Value;
end;

procedure TBilliard.TBall.SetPos(const Value: TVector2);
begin
  Body.Position := Value;
  Body.UpdateLocations;
end;

{ TBilliard }

constructor TBilliard.Create;
var
  I: TBall.TIndex;
begin
  FWorld := TWorld.Create(0);
  FTable := TTable.Create;
  for I := Low(TBall.TIndex) to High(TBall.TIndex) do
    FBalls[I] := TBall.Create(World, I);
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

end.
