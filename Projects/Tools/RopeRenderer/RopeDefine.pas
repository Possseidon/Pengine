unit RopeDefine;

interface

uses
  System.SysUtils,
  System.Math,

  Pengine.Vector,
  Pengine.Camera,
  Pengine.Interfaces,
  Pengine.VAO,
  Pengine.GLEnums,
  Pengine.Color,
  Pengine.GLState,
  Pengine.Collections,
  Pengine.IntMaths,

  ModelShader;

type

  ERopePointsSamePosition = class(Exception)
  public
    constructor Create;
  end;

  TRopeBase = class abstract(TRenderable)
  public type
    TData = TModelGLProgram.TData;
    TVAO = TVAOMutable<TData>;

  private
    FVAO: TVAO;

    FChanged: Boolean;

    FPoint1: TVector3;
    FPoint2: TVector3;
    FOsculatingRadius: Single;
    FGLState: TGLState;

    procedure SetPoint1(const Value: TVector3);
    procedure SetPoint2(const Value: TVector3);
    procedure SetOsculatingRadius(const Value: Single);
    function GetLength: Single;
    procedure SetLength(const Value: Single);

    function GetLine: TLine3;
    function GetDerivative(APos: Single): Single;
    function GetTangent(APos: Single): TVector3;
    function GetNormal(APos, AAngle: Single): TVector3;
    function GetHangDistance(APos: Single): Single;

    function GetPoint(APos: Single): TVector3; overload;
    function GetPoint(APos, AAngle, ARadius: Single): TVector3; overload;

    function GetSegment(ARange: TBounds1): TLine3;

  protected
    property VAO: TVAO read FVAO;

    procedure BuildVAO; virtual;
    procedure Changed;

  public
    constructor Create(AGLState: TGLState; APoint1, APoint2: TVector3; AOsculatingRadius: Single);
    destructor Destroy; override;

    /// <summary>The first hanging-point of the rope.</summary>
    property Point1: TVector3 read FPoint1 write SetPoint1;
    /// <summary>The second hanging-point of the rope.</summary>
    property Point2: TVector3 read FPoint2 write SetPoint2;
    /// <summary>The radius of a circle, laying as tangent in the rope.</summary>
    property OsculatingRadius: Single read FOsculatingRadius write SetOsculatingRadius;
    /// <summary>The length of the rope. Setting is not supported currently.</summary>
    property Length: Single read GetLength write SetLength;

    /// <summary>A line from Point1 to Point2.</summary>
    property Line: TLine3 read GetLine;
    /// <summary>The derivative (Ableitung) of the rope at a specific point.</summary>
    property Derivative[APos: Single]: Single read GetDerivative;
    /// <summary>The tangent of the rope at a specific point, pointing from Point1 to Point2.</summary>
    property Tangent[APos: Single]: TVector3 read GetTangent;
    /// <summary>The normal of the rope at a specific point. 0° goes to the right and 90° goes upwards.</summary>
    property Normal[APos, AAngle: Single]: TVector3 read GetNormal;
    /// <summary>How far the rope goes down at a specific point from <see cref="RopeDefine|TRope.Line"/>.</summary>
    property HangDistance[APos: Single]: Single read GetHangDistance;

    /// <summary>Converts from <c>[0, 1]</c> to a point on the center of the rope.</summary>
    property Point[APos: Single]: TVector3 read GetPoint; default;
    /// <summary>Converts from <c>[0, 1]</c> to a point on the outside of the rope with a specific radius.</summary>
    property Point[APos, AAngle, ARadius: Single]: TVector3 read GetPoint; default;

    /// <summary>Get a line segment between the given bounds.</summary>
    property Segment[ARange: TBounds1]: TLine3 read GetSegment;

    procedure Render; override;

  end;

  TRope = class(TRopeBase)
  private
    FRadius: TBounds1;
    FSteps: TIntVector2;
    FSmooth: Boolean;

    procedure SetRadius(const Value: TBounds1);
    procedure SetInnerRadius(const Value: Single);
    procedure SetOuterRadius(const Value: Single);

    procedure SetSteps(const Value: TIntVector2);
    function GetStepsX: Integer;
    procedure SetStepsX(const Value: Integer);
    function GetStepsR: Integer;
    procedure SetStepsR(const Value: Integer);

    function GetStepPoint(AStep: TIntVector2; AOutside: Boolean): TVector3;
    function GetStepNormal(AStep: TIntVector2): TVector3;

    procedure SetSmooth(const Value: Boolean);

    function GetInnerPoint(APos, AAngle: Single): TVector3;
    function GetOuterPoint(APos, AAngle: Single): TVector3;

  protected
    procedure BuildVAO; override;

  public
    constructor Create(AGLState: TGLState; APoint1, APoint2: TVector3; AOsculatingRadius: Single);

    /// <summary>The inner and outer radius of the rope as <see cref="Pengine.Vector|TBounds1"/>.</summary>
    property Radius: TBounds1 read FRadius write SetRadius;
    /// <summary>The inner radius of the rope.</summary>
    property InnerRadius: Single read FRadius.C1 write SetInnerRadius;
    /// <summary>The outer radius of the rope.</summary>
    property OuterRadius: Single read FRadius.C2 write SetOuterRadius;

    /// <summary><c>Steps.X</c> equals <c>StepsX</c> and <c>Steps.Y</c> equals <c>StepsR</c>.</summary>
    property Steps: TIntVector2 read FSteps write SetSteps;
    /// <summary>The amount of steps between Point1 and Point2, used to build the VAO.</summary>
    property StepsX: Integer read GetStepsX write SetStepsX;
    /// <summary>The amount of steps around the rope, used to build the VAO.</summary>
    property StepsR: Integer read GetStepsR write SetStepsR;

    /// <summary>Convert from <c>[0, StepsX]</c> to <c>[0, 1]</c> and from <c>[0, StepsR]</c> to <c>[0, 360]</c>.</summary>
    function StepToPos(AStep: TIntVector2): TVector2;

    /// <summary>An actual point on the rendered rope, using the given step counts, which have to be in range.</summary>
    property StepPoint[AStep: TIntVector2; AOutside: Boolean]: TVector3 read GetStepPoint;
    /// <summary>An actual normal of the rendered rope, using the given step counts, which have to be in range.</summary>
    property StepNormal[AStep: TIntVector2]: TVector3 read GetStepNormal;

    /// <summary>Specifies, wether the VAO normals are per face or per vertex.</summary>
    /// <remarks>Building the VAO with flat shading is slightly faster currently, but smooth shading could get optimized.
    /// to result in pretty much no difference.</remarks>
    property Smooth: Boolean read FSmooth write SetSmooth;

    /// <summary>Converts from <c>[0, 1]</c> to a point on the outside of the rope.</summary>
    property OuterPoint[APos, AAngle: Single]: TVector3 read GetOuterPoint;
    /// <summary>Converts from <c>[0, 1]</c> to a point on the inside of the rope.</summary>
    property InnerPoint[APos, AAngle: Single]: TVector3 read GetInnerPoint;

  end;

  TRopeProxy = class(TRopeBase)
  private
    FBaseRope: TRope;

    function GetStepPoint(AStep: TIntVector2; AOutside: Boolean): TVector3;

  protected
    procedure BuildVAO; override;

  public
    constructor Create(ABaseRope: TRope; APoint1, APoint2: TVector3; AOsculatingRadius: Single);

    /// <summary>Convert from a point on the base rope to a point on the new rope.</summary>
    property StepPoint[AStep: TIntVector2; AOutside: Boolean]: TVector3 read GetStepPoint;

  end;

implementation

{ ERopePointsSamePosition }

constructor ERopePointsSamePosition.Create;
begin
  inherited Create('The two points X or Z coordinates must be different.');
end;

{ TRopeBase }

procedure TRopeBase.SetPoint1(const Value: TVector3);
begin
  if Point1 = Value then
    Exit;
  FPoint1 := Value;
  Changed;
end;

procedure TRopeBase.SetPoint2(const Value: TVector3);
begin
  if Point2 = Value then
    Exit;
  FPoint2 := Value;
  Changed;
end;

procedure TRopeBase.SetOsculatingRadius(const Value: Single);
begin
  if OsculatingRadius = Value then
    Exit;
  FOsculatingRadius := Value;
  Changed;
end;

function TRopeBase.GetLength: Single;
var
  A: Single;
begin
  A := OsculatingRadius;
  Result := 2 * A * Sinh(Line.D.Length / (2 * A));
end;

procedure TRopeBase.SetLength(const Value: Single);
begin
  raise ENotSupportedException.Create('Setting length is not supported.');
end;

function TRopeBase.GetLine: TLine3;
begin
  Result := Point1.LineTo(Point2);
end;

function TRopeBase.GetDerivative(APos: Single): Single;
var
  A: Single;
begin
  if Point1.XZ = Point2.XZ then
    raise ERopePointsSamePosition.Create;

  A := OsculatingRadius;
  Result := 2 * Sinh((2 * (APos * 2 - 1)) / A);
end;

function TRopeBase.GetTangent(APos: Single): TVector3;
begin
  Result := Line.D.Normalize;
  Result.Y := Result.Y + Derivative[APos];
  Result := Result.Normalize;
end;

function TRopeBase.GetNormal(APos, AAngle: Single): TVector3;
var
  Right, Up: TVector3;
begin
  Right := Point1.VectorTo(Point2).Cross(Vec3(0, 1, 0)).Normalize;
  Up := Right.Cross(Tangent[APos]).Normalize;
  Result := Right * Cos(DegToRad(AAngle)) + Up * Sin(DegToRad(AAngle));
end;

function TRopeBase.GetHangDistance(APos: Single): Single;
var
  A: Single;
begin
  A := OsculatingRadius;
  Result := A * Cosh((APos * 2 - 1) / A) - A * Cosh(1 / A);
  Result := -Result * Line.D.Length * 2;
end;

function TRopeBase.GetPoint(APos: Single): TVector3;
begin
  Result := Line[APos];
  Result.Y := Result.Y - HangDistance[APos];
end;

function TRopeBase.GetPoint(APos, AAngle, ARadius: Single): TVector3;
begin
  Result := Point[APos] + Normal[APos, AAngle] * ARadius;
end;

function TRopeBase.GetSegment(ARange: TBounds1): TLine3;
begin
  Result := Point[ARange.C1].LineTo(Point[ARange.C2]);
end;

procedure TRopeBase.BuildVAO;
begin
  FChanged := False;
end;

procedure TRopeBase.Changed;
begin
  FChanged := True;
end;

constructor TRopeBase.Create(AGLState: TGLState; APoint1, APoint2: TVector3; AOsculatingRadius: Single);
begin
  FGLState := AGLState;
  FVAO := TVAO.Create(TModelGLProgram.Make(FGLState.ResParam));

  Point1 := APoint1;
  Point2 := APoint2;
  OsculatingRadius := AOsculatingRadius;
end;

destructor TRopeBase.Destroy;
begin
  TModelGLProgram.Release(FGLState.ResParam);
  FVAO.Free;
  inherited;
end;

procedure TRopeBase.Render;
begin
  if FChanged then
    BuildVAO;
  FVAO.Render;
end;

{ TRope }

procedure TRope.SetRadius(const Value: TBounds1);
begin
  if Radius = Value then
    Exit;
  FRadius := Value;
  Changed;
end;

procedure TRope.SetInnerRadius(const Value: Single);
begin
  FRadius.C1 := Value;
end;

procedure TRope.SetOuterRadius(const Value: Single);
begin
  if OuterRadius = Value then
    Exit;
  FRadius.C1 := Value;
  Changed;
end;

procedure TRope.SetSteps(const Value: TIntVector2);
begin
  if FSteps = Value then
    Exit;
  FSteps := Value;
  Changed;
end;

function TRope.GetStepsX: Integer;
begin
  Result := FSteps.X;
end;

procedure TRope.SetStepsX(const Value: Integer);
begin
  if StepsX = Value then
    Exit;
  FSteps.X := Value;
  Changed;
end;

function TRope.StepToPos(AStep: TIntVector2): TVector2;
begin
  Result.X := AStep.X / StepsX;
  Result.Y := AStep.Y / StepsR * 360;
end;

function TRope.GetStepsR: Integer;
begin
  Result := FSteps.Y;
end;

procedure TRope.SetStepsR(const Value: Integer);
begin
  if StepsR = Value then
    Exit;
  FSteps.Y := Value;
  Changed;
end;

function TRope.GetStepPoint(AStep: TIntVector2; AOutside: Boolean): TVector3;
var
  Pos: TVector2;
begin
  Pos := StepToPos(AStep);
  if AOutside then
    Result := OuterPoint[Pos.X, Pos.Y]
  else
    Result := InnerPoint[Pos.X, Pos.Y];
end;

function TRope.GetStepNormal(AStep: TIntVector2): TVector3;
var
  Pos: TVector2;
begin
  Pos := StepToPos(AStep);
  Result := Normal[Pos.X, Pos.Y];
end;

procedure TRope.SetSmooth(const Value: Boolean);
begin
  if FSmooth = Value then
    Exit;
  FSmooth := Value;
  Changed;
end;

function TRope.GetInnerPoint(APos, AAngle: Single): TVector3;
begin
  Result := Self[APos, AAngle, InnerRadius];
end;

function TRope.GetOuterPoint(APos, AAngle: Single): TVector3;
begin
  Result := Self[APos, AAngle, OuterRadius];
end;

procedure TRope.BuildVAO;
var
  Data: TVAO.TData;
  Step: TIntVector2;
  RInt: Integer;
  R: TBounds1;
  T: TIntVector2;
begin
  inherited;

  VAO.VBO.Generate((StepsX + 1) * StepsR * 6 * 2, buDynamicDraw);
  with VAO.VBO.Map do
  begin

    Data.Color := ColorRGB(0, 0.3, 1);

    // Rope
    for Step in Steps do
    begin
      if not Smooth then
        Data.Normal := StepNormal[Step];

      // Outer
      for T in QuadTexCoords do
      begin
        if Smooth then
          Data.Normal := StepNormal[Step + T];
        Data.Pos := StepPoint[Step + T, True];
        AddToBuffer(Data);
      end;

      if not Smooth then
        Data.Normal := -Data.Normal;

      // Inner
      for T in QuadTexCoords do
      begin
        if Smooth then
          Data.Normal := -StepNormal[Step + T.YX];
        Data.Pos := StepPoint[Step + T.YX, False];
        AddToBuffer(Data);
      end;

    end;

    // Point-Caps
    for RInt := 0 to StepsR - 1 do
    begin
      R := Bounds1(RInt / StepsR, (RInt + 1) / StepsR) * 360;

      // Point1
      Data.Normal := -Tangent[0];
      for T in QuadTexCoords do
      begin
        Data.Pos := Point[0] + Normal[0, R[T.Y]] * Radius[T.X];
        AddToBuffer(Data);
      end;

      // Point2
      Data.Normal := Tangent[1];
      for T in QuadTexCoords do
      begin
        Data.Pos := Point[1] + Normal[1, R[1 - T.Y]] * Radius[T.X];
        AddToBuffer(Data);
      end;
    end;

    Free;
  end;
end;

constructor TRope.Create(AGLState: TGLState; APoint1, APoint2: TVector3; AOsculatingRadius: Single);
begin
  inherited;
  Radius := Bounds1(0.5, 1);
  StepsX := 16;
  StepsR := 8;
end;

{ TRopeProxy }

procedure TRopeProxy.BuildVAO;
var
  Step: TIntVector2;
  T: TIntVector2;
  Data: TVAO.TData;
  RInt: Integer;
  R: TBounds1;
begin
  inherited;

  VAO.VBO.Generate((FBaseRope.StepsX + 1) * FBaseRope.StepsR * 6 * 2, buDynamicDraw);
  with VAO.VBO.Map do
  begin

    Data.Color := ColorRGB(0, 0.3, 1);

    // Rope
    for Step in FBaseRope.Steps do
    begin

      Data.Normal := Normal[Step.X / FBaseRope.StepsX, Step.Y / FBaseRope.StepsR * 360]; // TODO

      // Outer
      for T in QuadTexCoords do
      begin
        Data.Pos := StepPoint[Step + T, True];
        AddToBuffer(Data);
      end;

      Data.Normal := -Data.Normal;

      // Inner
      for T in QuadTexCoords do
      begin
        Data.Pos := StepPoint[Step + T.YX, False];
        AddToBuffer(Data);
      end;

    end;


    // Point-Caps
    for RInt := 0 to FBaseRope.StepsR - 1 do
    begin
      R := Bounds1(RInt / FBaseRope.StepsR, (RInt + 1) / FBaseRope.StepsR) * 360;

      // Point1
      Data.Normal := -Tangent[0];
      for T in QuadTexCoords do
      begin
        Data.Pos := StepPoint[IVec2(0, RInt + T.Y), T.X = 1];
        AddToBuffer(Data);
      end;

      // Point2
      Data.Normal := Tangent[1];
      for T in QuadTexCoords do
      begin
        Data.Pos := StepPoint[IVec2(FBaseRope.StepsX, RInt + T.Y), T.X = 0];
        AddToBuffer(Data);
      end;
    end;

    Free;
  end;
end;

constructor TRopeProxy.Create(ABaseRope: TRope; APoint1, APoint2: TVector3; AOsculatingRadius: Single);
begin
  inherited Create(ABaseRope.VAO.GLState, APoint1, APoint2, AOsculatingRadius);
  FBaseRope := ABaseRope;
end;

function TRopeProxy.GetStepPoint(AStep: TIntVector2; AOutside: Boolean): TVector3;
const
  Segments = 64;
var
  TestPoint, LinePoint, Direction, NormalSide, NormalUp: TVector3;
  Distance, Factor, LowestDistance, LowestFactor, LowestI, Angle: Single;
  I: Integer;
  Segment: TLine3;
begin
  // Get the the point on the base rope
  TestPoint := FBaseRope.StepPoint[AStep, AOutside];

  // Find the segment, where the point is closest
  LowestFactor := NaN;
  LowestDistance := 0; // just to hide the warning... could set it for the values of the for loop at the first iteration
  LowestI := 0;        // but I'm lazy :P
  for I := 0 to Segments - 1 do
  begin
    Segment := FBaseRope.Segment[Bounds1(I / Segments, (I + 1) / Segments)];
    Factor := Segment.OrthoProj(TestPoint);
    if (Abs(Factor - 0.5) < Abs(LowestFactor - 0.5)) or IsNan(LowestFactor) then
    begin
      LowestFactor := Factor;
      LinePoint := Segment[Factor];
      Distance := TestPoint.DistanceTo(LinePoint);
      LowestDistance := Distance;
      LowestI := I;
    end;
  end;

  // We already have the distance, but we still need the angle to convert it
  Segment := FBaseRope.Segment[Bounds1(LowestI / Segments, (LowestI + 1) / Segments)];
  NormalSide := Segment.D.Cross(Vec3(0, 1, 0));
  NormalUp := NormalSide.Cross(Segment.D);
  Direction := Segment[LowestFactor].VectorTo(TestPoint);
  Angle := Direction.AngleTo(NormalSide);
  if Direction.Dot(NormalUp) < 0 then
    Angle := -Angle;

  // We got the angle, now convert it over
  Segment := Self.Segment[Bounds1(LowestI / Segments, (LowestI + 1) / Segments)];
  NormalSide := Self.Line.D.Cross(Vec3(0, 1, 0)).Normalize;
  NormalUp := NormalSide.Cross(Segment.D).Normalize;
  Result := Segment[LowestFactor] + (NormalSide * Cos(DegToRad(Angle)) + NormalUp * Sin(DegToRad(Angle))) * LowestDistance;
end;

end.
