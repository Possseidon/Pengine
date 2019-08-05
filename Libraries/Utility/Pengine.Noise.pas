unit Pengine.Noise;

interface

uses
  System.Math,

  Pengine.IntMaths,
  Pengine.Vector;

type

  IGradientSource<TPos, TBounds, TGrad> = interface
    function GetGradient(APos: TPos): TGrad;
    function GetBounds: TBounds;

    property Gradients[APos: TPos]: TGrad read GetGradient; default;
    property Bounds: TBounds read GetBounds;
    function HasBounds: Boolean;

  end;

  IGradientSource1 = IGradientSource<Integer, TIntBounds1, Single>;

  IGradientSource2 = IGradientSource<TIntVector2, TIntBounds2, TVector2>;

  IGradientSource3 = IGradientSource<TIntVector3, TIntBounds3, TVector3>;

  TPerlinNoise1 = class
  private
    FGradientSource: IGradientSource1;

    function GetBounds: TIntBounds1;
    function GetGradient(APos: Integer): Single;
    function GetValue(APos: Single): Single;

  public
    property GradientSource: IGradientSource1 read FGradientSource write FGradientSource;

    property Gradients[APos: Integer]: Single read GetGradient;
    property Bounds: TIntBounds1 read GetBounds;
    function HasBounds: Boolean;

    property Values[APos: Single]: Single read GetValue; default;

  end;

  TPerlinNoise2 = class
  private
    FGradientSource: IGradientSource2;

    function GetBounds: TIntBounds2;
    function GetGradient(APos: TIntVector2): TVector2;
    function GetValue(APos: TVector2): Single;

  public
    property GradientSource: IGradientSource2 read FGradientSource write FGradientSource;

    property Gradients[APos: TIntVector2]: TVector2 read GetGradient;
    property Bounds: TIntBounds2 read GetBounds;
    function HasBounds: Boolean;

    property Values[APos: TVector2]: Single read GetValue; default;

  end;

  TPerlinNoise3 = class
  private
    FGradientSource: IGradientSource3;

    function GetBounds: TIntBounds3;
    function GetGradient(APos: TIntVector3): TVector3;
    function GetValue(APos: TVector3): Single;

  public
    property GradientSource: IGradientSource3 read FGradientSource write FGradientSource;

    property Gradients[APos: TIntVector3]: TVector3 read GetGradient;
    property Bounds: TIntBounds3 read GetBounds;
    function HasBounds: Boolean;

    property Values[APos: TVector3]: Single read GetValue; default;

  end;

implementation

function CubicInterpolation(const L, H, W: Single): Single; inline;
begin
  Result := L + W * W * (3 - 2 * W) * (H - L);
end;

{ TPerlinNoise1 }

function TPerlinNoise1.GetBounds: TIntBounds1;
begin
  Result := GradientSource.Bounds;
end;

function TPerlinNoise1.GetGradient(APos: Integer): Single;
begin
  Result := GradientSource[APos];
end;

function TPerlinNoise1.GetValue(APos: Single): Single;
var
  L, H: Integer;
  DL, DH: Single;
begin
  L := Floor(APos);
  H := L + 1;
  DL := APos - L;
  DH := APos - H;
  Result := CubicInterpolation(Gradients[L] * DL, Gradients[H] * DH, DL) / Sqrt(1 / 4);
end;

function TPerlinNoise1.HasBounds: Boolean;
begin
  Result := GradientSource.HasBounds;
end;

{ TPerlinNoise2 }

function TPerlinNoise2.GetBounds: TIntBounds2;
begin
  Result := GradientSource.Bounds;
end;

function TPerlinNoise2.GetGradient(APos: TIntVector2): TVector2;
begin
  Result := GradientSource[APos];
end;

function TPerlinNoise2.GetValue(APos: TVector2): Single;
var
  L, H, LH, HL: TIntVector2;
  DL, DH, DHL, DLH: TVector2;
begin
  L := APos.Floor;
  H := L + 1;
  LH := IVec2(L.X, H.Y);
  HL := IVec2(H.X, L.Y);
  DL := APos - L;
  DHL := APos - HL;
  DH := APos - H;
  DLH := APos - LH;
  Result := CubicInterpolation(
    CubicInterpolation(Gradients[L].Dot(DL), Gradients[HL].Dot(DHL), DL.X),
    CubicInterpolation(Gradients[LH].Dot(DLH), Gradients[H].Dot(DH), DL.X),
    DL.Y) / Sqrt(2 / 4);
end;

function TPerlinNoise2.HasBounds: Boolean;
begin
  Result := GradientSource.HasBounds;
end;

{ TPerlinNoise3 }

function TPerlinNoise3.GetBounds: TIntBounds3;
begin
  Result := GradientSource.Bounds;
end;

function TPerlinNoise3.GetGradient(APos: TIntVector3): TVector3;
begin
  Result := GradientSource.Gradients[APos];
end;

function TPerlinNoise3.GetValue(APos: TVector3): Single;
var
  LLL, HLL, LHL, HHL, LLH, HLH, LHH, HHH: TIntVector3;
  DLLL, DHLL, DLHL, DHHL, DLLH, DHLH, DLHH, DHHH: TVector3;
begin
  LLL := APos.Floor;
  HHH := LLL + 1;
  HLL := IVec3(HHH.X, LLL.Y, LLL.Z);
  LHL := IVec3(LLL.X, HHH.Y, LLL.Z);
  HHL := IVec3(HHH.X, HHH.Y, LLL.Z);
  LLH := IVec3(LLL.X, LLL.Y, HHH.Z);
  HLH := IVec3(HHH.X, LLL.Y, HHH.Z);
  LHH := IVec3(LLL.X, HHH.Y, HHH.Z);
  DLLL := APos - LLL;
  DHLL := APos - HLL;
  DLHL := APos - LHL;
  DHHL := APos - HHL;
  DLLH := APos - LLH;
  DHLH := APos - HLH;
  DLHH := APos - LHH;
  DHHH := APos - HHH;
  Result := CubicInterpolation(
    CubicInterpolation(
    CubicInterpolation(Gradients[LLL].Dot(DLLL), Gradients[HLL].Dot(DHLL), DLLL.X),
    CubicInterpolation(Gradients[LHL].Dot(DLHL), Gradients[HHL].Dot(DHHL), DLLL.X),
    DLLL.Y),
    CubicInterpolation(
    CubicInterpolation(Gradients[LLH].Dot(DLLH), Gradients[HLH].Dot(DHLH), DLLL.X),
    CubicInterpolation(Gradients[LHH].Dot(DLHH), Gradients[HHH].Dot(DHHH), DLLL.X),
    DLLL.Y),
    DLLL.Z) / Sqrt(3 / 4);
end;

function TPerlinNoise3.HasBounds: Boolean;
begin
  Result := GradientSource.HasBounds;
end;

end.
