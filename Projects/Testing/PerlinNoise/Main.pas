unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,

  Pengine.Random,
  Pengine.IntMaths,
  Pengine.Vector,
  Pengine.Color,
  Pengine.ICollections,
  Pengine.Noise, Vcl.ExtCtrls, Vcl.AppEvnts;

type

  TGradient2 = class(TInterfacedObject, IGradientSource2)
  private
    FData: array of array of TVector2;

    function GetGradient(APos: TIntVector2): TVector2;
    function GetBounds: TIntBounds2;

  public
    property Gradients[APos: TIntVector2]: TVector2 read GetGradient; default;
    property Bounds: TIntBounds2 read GetBounds;
    function HasBounds: Boolean;

  end;

  TGradient3 = class(TInterfacedObject, IGradientSource3)
  private
    FData: array of array of TVector3;

    function GetGradient(APos: TIntVector3): TVector3;
    function GetBounds: TIntBounds3;

  public
    property Gradients[APos: TIntVector3]: TVector3 read GetGradient; default;
    property Bounds: TIntBounds3 read GetBounds;
    function HasBounds: Boolean;

  end;

  TForm10 = class(TForm)
    ApplicationEvents1: TApplicationEvents;
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    FPerlinNoise: TPerlinNoise3;

    class function LinearInterpolation(A, B, W: Single): Single; static;
    class function QuadraticInterpolation(A, B, W: Single): Single; static;
    class function CubicInterpolation(A, B, W: Single): Single; static;
    class function SineInterpolation(A, B, W: Single): Single; static;

  end;

var
  Form10: TForm10;

implementation

{$R *.dfm}

procedure TForm10.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
var
  Size, Pos: TIntVector2;
  PosF: TVector2;
  I: Integer;
  N: Single;
  C: TColorRGB;
  Factor: Single;
begin
  Factor := GetTickCount / 20000;
  Size := IVec2(ClientWidth, ClientHeight);
  for I := 0 to Size.Area div 1 do
  begin
    Pos := TIntVector2.Random(Size);
    PosF := TVector2(Pos) / 20;
    N := FPerlinNoise[Vec3(PosF.X, PosF.Y, Factor)];
    //if Abs(N) < 0.1 then
    //  C := TColorRGB.Gray(1 - Abs(N) / 0.1)
    //else
    //  C := 0;
    C := TColorRGB.Gray(Abs(N));
    Canvas.Pixels[Pos.X, Pos.Y] := C.ToWinColor;
  end;
  Done := False;
end;

procedure TForm10.FormCreate(Sender: TObject);
begin
  Randomize;
  FPerlinNoise := TPerlinNoise3.Create;
  FPerlinNoise.GradientSource := TGradient3.Create;
end;

class function TForm10.LinearInterpolation(A, B, W: Single): Single;
begin
  Result := A + W * (B - A);
end;

class function TForm10.QuadraticInterpolation(A, B, W: Single): Single;
begin
  if W > 0.5 then
    W := 1 - 2 * Sqr(W - 1)
  else
    W := 2 * Sqr(W);
  Result := A + W * (B - A);
end;

class function TForm10.CubicInterpolation(A, B, W: Single): Single;
begin
  Result := A + (3 * W * W - 2 * W * W * W) * (B - A);
end;

class function TForm10.SineInterpolation(A, B, W: Single): Single;
begin
  Result := A + (Sin((W - 0.5) * Pi) + 1) * 0.5 * (B - A);
end;

{ TGradient }

function TGradient2.GetBounds: TIntBounds2;
begin
  Result := IBounds2(0);
end;

function TGradient2.GetGradient(APos: TIntVector2): TVector2;
var
  Rand: TRandom;
begin
  Rand := TRandom.FromSeed(TDefault<TIntVector2>.Hash(APos));
  Result := TVector2.FromAngleRad(Rand.NextSingle * 2 * Pi);
  // Result := Vec2(0, 1).Rotate((Sin(APos.X) + Cos(APos.Y)) * 30);
end;

function TGradient2.HasBounds: Boolean;
begin
  Result := False;
end;

{ TGradient3 }

function TGradient3.GetBounds: TIntBounds3;
begin
  Result := IBounds3(0);
end;

function TGradient3.GetGradient(APos: TIntVector3): TVector3;
var
  Rand: TRandom;
  O: Single;
  U: Single;
begin
  Rand := TRandom.FromSeed(TDefault<TIntVector3>.Hash(APos));
  O := Rand.NextSingle * 2 * Pi;
  U := Rand.NextSingle * 2 - 1;
  Result.X := Sqrt(1 - Sqr(U)) * Sin(O);
  Result.Y := Sqrt(1 - Sqr(U)) * Cos(O);
  Result.Z := U;
end;

function TGradient3.HasBounds: Boolean;
begin
  Result := False;
end;

end.
