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

  public
    constructor Create(APoint1, APoint2: TVector3; AOsculatingRadius: Single);
    destructor Destroy; override;

    property Point1: TVector3 read FPoint1 write SetPoint1;
    property Point2: TVector3 read FPoint2 write SetPoint2;
    property OsculatingRadius: Single read FOsculatingRadius write SetOsculatingRadius;
    property Length: Single read GetLength write SetLength;

    /// <summary>Converts from <c>[0, +1]</c> to a point on the center of the rope.</summary>
    property Point[APos: Single]: TVector3 read GetPoint; default;
    property Point[APos, AAngle: Single]: TVector3 read GetPoint; default;
    property Line: TLine3 read GetLine;
    property Derivative[APos: Single]: Single read GetDerivative;
    property Tangent[APos: Single]: TVector3 read GetTangent;
    property Normal[APos, AAngle: Single]: TVector3 read GetNormal;

    procedure Render; override;

  end;

implementation

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

procedure TRope.SetOsculatingRadius(const Value: Single);
begin
  if OsculatingRadius = Value then
    Exit;
  FOsculatingRadius := Value;
  Changed;
end;

function TRope.GetLength: Single;
begin
  raise ENotImplemented.Create('Length conversion is not implemented.');
end;

procedure TRope.SetLength(const Value: Single);
begin
  raise ENotImplemented.Create('Length conversion is not implemented.');
end;

function TRope.GetPoint(APos: Single): TVector3;
var
  A, Y: Single;
begin
  if Point1.XZ = Point2.XZ then
    raise ERopePointsSamePosition.Create;

  A := OsculatingRadius;
  Y := A * Cosh((APos * 2 - 1) / A) - A * Cosh(1 / A);
  Result := Line[APos];
  Result.Y := Result.Y + Y * Line.D.Length * 2;
end;

function TRope.GetPoint(APos, AAngle: Single): TVector3;
begin
  Result := Point[APos] + Normal[APos, AAngle] * 0.5;
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

procedure TRope.BuildVAO;
const
  StepsX = 200;
  StepsR = 90;
var
  Data: TModelShader.TData;
  XInt, RInt: Integer;
  T: TVector2;
  X, R: TBounds1;
begin
  FVAO.Generate(StepsX * StepsR * 6 * 2, buStaticDraw);
  FVAO.Map(baWriteOnly);

  // Rope
  for XInt := 0 to StepsX - 1 do
  begin
    for RInt := 0 to StepsR - 1 do
    begin
      X := Bounds1(XInt / StepsX, (XInt + 1) / StepsX);
      R := Bounds1(RInt / StepsR, (RInt + 1) / StepsR) * 360;       
      for T in QuadTexCoords do
      begin
        Data.Color := X[T.X] * ColorRGB(0, 0.3, 1) + (1 - X[T.X]) * ColorRGB(1, 0.3, 0);
        Data.Pos := Point[X[T.X], R[T.Y]];
        Data.Normal := Normal[X[T.X], R[T.Y]];
        FVAO.AddVertex(Data);
      end;
      for T in QuadTexCoords do
      begin
        Data.Color := X[1 - T.X] * ColorRGB(0, 0.3, 1) + (1 - X[1 - T.X]) * ColorRGB(1, 0.3, 0);
        Data.Pos := Point[X[1 - T.X], R[T.Y]];
        Data.Normal := -Normal[X[1 - T.X], R[T.Y]];
        FVAO.AddVertex(Data);
      end;
    end;
  end;

  // Point1-Cap
  // TODO

  // Point2-Cap
  // TODO

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

{ ERopePointsSamePosition }

constructor ERopePointsSamePosition.Create;
begin
  inherited Create('The two points X or Z coordinates must be different.');
end;

end.

