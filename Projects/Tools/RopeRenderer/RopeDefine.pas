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

  ModelShader;

type

  ERopePointsSamePosition = class(Exception)
  public
    constructor Create;
  end;

  TRope = class(TRenderable)
  private
    FVAO: TVAO;

    FChanged: Boolean;

    FPoint1: TVector3;
    FPoint2: TVector3;
    FOsculatingRadius: Single;
    FStepsX: Integer;
    FRadius: TBounds1;
    FStepsR: Integer;

    procedure SetPoint1(const Value: TVector3);
    procedure SetPoint2(const Value: TVector3);
    procedure SetOsculatingRadius(const Value: Single);
    function GetLength: Single;
    procedure SetLength(const Value: Single);

    function GetPoint(APos: Single): TVector3; overload;
    function GetPoint(APos, AAngle: Single): TVector3; overload;
    function GetLine: TLine3;
    function GetDerivative(APos: Single): Single;
    function GetTangent(APos: Single): TVector3;
    function GetNormal(APos, AAngle: Single): TVector3;

    procedure BuildVAO;

    procedure Changed;
    function GetInnerPoint(APos, AAngle: Single): TVector3;
    procedure SetStepsR(const Value: Integer);
    procedure SetInnerRadius(const Value: Single);
    procedure SetOuterRadius(const Value: Single);
    procedure SetStepsX(const Value: Integer);
    procedure SetRadius(const Value: TBounds1);
    function GetHangDistance(APos: Single): Single;

  public
    constructor Create(APoint1, APoint2: TVector3; AOsculatingRadius: Single);
    destructor Destroy; override;

    property Point1: TVector3 read FPoint1 write SetPoint1;
    property Point2: TVector3 read FPoint2 write SetPoint2;
    property OsculatingRadius: Single read FOsculatingRadius write SetOsculatingRadius;
    property Length: Single read GetLength write SetLength;

    property Radius: TBounds1 read FRadius write SetRadius;
    property InnerRadius: Single read FRadius.C1 write SetInnerRadius;
    property OuterRadius: Single read FRadius.C2 write SetOuterRadius;

    property StepsX: Integer read FStepsX write SetStepsX;
    property StepsR: Integer read FStepsR write SetStepsR;

    property Line: TLine3 read GetLine;
    property Derivative[APos: Single]: Single read GetDerivative;
    property Tangent[APos: Single]: TVector3 read GetTangent;
    property Normal[APos, AAngle: Single]: TVector3 read GetNormal;
    property HangDistance[APos: Single]: Single read GetHangDistance;

    /// <summary>Converts from <c>[0, +1]</c> to a point on the center of the rope.</summary>
    property Point[APos: Single]: TVector3 read GetPoint; default;
    property Point[APos, AAngle: Single]: TVector3 read GetPoint; default;
    property InnerPoint[APos, AAngle: Single]: TVector3 read GetInnerPoint;

    procedure Render; override;

  end;

implementation

{ ERopePointsSamePosition }

constructor ERopePointsSamePosition.Create;
begin
  inherited Create('The two points X or Z coordinates must be different.');
end;

{ TRope }

procedure TRope.SetPoint1(const Value: TVector3);
begin
  if Point1 = Value then
    Exit;
  FPoint1 := Value;
  Changed;
end;

procedure TRope.SetPoint2(const Value: TVector3);
begin
  if Point2 = Value then
    Exit;
  FPoint2 := Value;
  Changed;
end;

procedure TRope.SetRadius(const Value: TBounds1);
begin
  if Radius = Value then
    Exit;
  FRadius := Value;
  Changed;
end;

procedure TRope.SetStepsR(const Value: Integer);
begin
  if StepsR = Value then
    Exit;
  FStepsR := Value;
  Changed;
end;

procedure TRope.SetStepsX(const Value: Integer);
begin
  if StepsX = Value then
    Exit;
  FStepsX := Value;
  Changed;
end;

procedure TRope.SetOsculatingRadius(const Value: Single);
begin
  if OsculatingRadius = Value then
    Exit;
  FOsculatingRadius := Value;
  Changed;
end;

procedure TRope.SetOuterRadius(const Value: Single);
begin
  if OuterRadius = Value then
    Exit;
  FRadius.C1 := Value;
  Changed;
end;

function TRope.GetLength: Single;
var
  A: Single;
begin
  A := OsculatingRadius;
  Result := 2 * A * Sinh(Line.D.Length / (2 * A));
end;

procedure TRope.SetInnerRadius(const Value: Single);
begin
  FRadius.C1 := Value;
end;

procedure TRope.SetLength(const Value: Single);
begin
  raise ENotSupportedException.Create('Setting length is not supported.');  
end;

function TRope.GetPoint(APos: Single): TVector3;
begin
  if Point1.XZ = Point2.XZ then
    raise ERopePointsSamePosition.Create;

  Result := Line[APos];
  Result.Y := Result.Y + HangDistance[APos];
end;

function TRope.GetPoint(APos, AAngle: Single): TVector3;
begin
  Result := Point[APos] + Normal[APos, AAngle] * OuterRadius;
end;

function TRope.GetLine: TLine3;
begin
  Result := Point1.LineTo(Point2);
end;

function TRope.GetDerivative(APos: Single): Single;
var
  A: Single;
begin
  if Point1.XZ = Point2.XZ then
    raise ERopePointsSamePosition.Create;

  A := OsculatingRadius;
  Result := 2 * Sinh((2 * (APos * 2 - 1)) / A);
end;

function TRope.GetHangDistance(APos: Single): Single;
var
  A: Single;
begin
  A := OsculatingRadius;
  Result := A * Cosh((APos * 2 - 1) / A) - A * Cosh(1 / A);
  Result := Result * Line.D.Length * 2;
end;

function TRope.GetInnerPoint(APos, AAngle: Single): TVector3;
begin
  Result := Point[APos] + Normal[APos, AAngle] * InnerRadius;
end;

procedure TRope.BuildVAO;
var
  Data: TModelShader.TData;
  XInt, RInt: Integer;
  T: TVector2;
  X, R: TBounds1;
begin
  FVAO.Generate(StepsX * StepsR * 6 * 2 + StepsR * 6 * 2, buStaticDraw);
  FVAO.Map(baWriteOnly);

  // Rope
  for XInt := 0 to StepsX - 1 do
  begin
    for RInt := 0 to StepsR - 1 do
    begin
      X := Bounds1(XInt / StepsX, (XInt + 1) / StepsX);
      R := Bounds1(RInt / StepsR, (RInt + 1) / StepsR) * 360;
      // Outer
      for T in QuadTexCoords do
      begin
        Data.Color := X[T.X] * ColorRGB(0, 0.3, 1) + (1 - X[T.X]) * ColorRGB(1, 0.3, 0);
        Data.Normal := Normal[X[T.X], R[T.Y]];
        Data.Pos := Point[X[T.X], R[T.Y]];
        FVAO.AddVertex(Data);
      end;
      // Inner
      for T in QuadTexCoords do
      begin
        Data.Color := X[T.X] * ColorRGB(0, 0.3, 1) + (1 - X[T.X]) * ColorRGB(1, 0.3, 0);
        Data.Normal := -Normal[X[T.X], R[1 - T.Y]];
        Data.Pos := InnerPoint[X[T.X], R[1 - T.Y]];
        FVAO.AddVertex(Data);
      end;
    end;
  end;

  // Point-Caps
  for RInt := 0 to StepsR - 1 do
  begin
    R := Bounds1(RInt / StepsR, (RInt + 1) / StepsR) * 360;
      
    // Point1
    Data.Color := ColorRGB(1, 0.3, 0);
    Data.Normal := -Tangent[0];
    for T in QuadTexCoords do
    begin
      Data.Pos := Point[0] + Normal[0, R[T.Y]] * Radius[T.X];
      FVAO.AddVertex(Data);
    end;

    // Point2
    Data.Color := ColorRGB(0, 0.3, 1);
    Data.Normal := Tangent[1];
    for T in QuadTexCoords do
    begin
      Data.Pos := Point[1] + Normal[1, R[1 - T.Y]] * Radius[T.X];
      FVAO.AddVertex(Data);
    end;

  end;

  FVAO.Unmap;

  FChanged := False;
end;

procedure TRope.Changed;
begin
  FChanged := True;
end;

function TRope.GetTangent(APos: Single): TVector3;
begin
  Result := Line.D.Normalize;
  Result.Y := Result.Y + Derivative[APos];
  Result := Result.Normalize;
end;

function TRope.GetNormal(APos, AAngle: Single): TVector3;
var
  Right, Up: TVector3;
begin
  Right := Point1.VectorTo(Point2).Cross(Vec3(0, 1, 0)).Normalize;
  Up := Right.Cross(Tangent[APos]).Normalize;
  Result := Right * Cos(DegToRad(AAngle)) + Up * Sin(DegToRad(AAngle));
end;

constructor TRope.Create(APoint1, APoint2: TVector3; AOsculatingRadius: Single);
begin
  FVAO := TVAO.Create(TModelShader.Data);

  Point1 := APoint1;
  Point2 := APoint2;
  OsculatingRadius := AOsculatingRadius;
  Radius := Bounds1(0.5, 1);
  StepsX := 16;
  StepsR := 8;
end;

destructor TRope.Destroy;
begin
  FVAO.Free;
  inherited;
end;

procedure TRope.Render;
begin
  if FChanged then
    BuildVAO;
  FVAO.Render;
end;

end.

