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
  Pengine.ICollections;

type

  TInterpolate = reference to function(A, B, W: Single): Single;

  TPerlinNoise = class
  private
    FGradient: array of array of TVector2;
    FSize: TIntVector2;
    FInterpolate: TInterpolate;

    function GetNoise(APos: TVector2): Single;
    function GetGradient(APos: TIntVector2): TVector2;

    function DotGridGradient(AGridPos: TIntVector2; APos: TVector2): Single;

  public
    constructor Create(ASize: TIntVector2);

    property Size: TIntVector2 read FSize;
    property Interpolate: TInterpolate read FInterpolate write FInterpolate;
    property Gradient[APos: TIntVector2]: TVector2 read GetGradient;
    property Noise[APos: TVector2]: Single read GetNoise; default;

  end;

  TForm10 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
  private
    FPerlinNoises: IList<TPerlinNoise>;
    FFactor: Single;

    procedure SetInterpolationFunction(AFunc: TInterpolate);

    class function LinearInterpolation(A, B, W: Single): Single; static;
    class function QuadraticInterpolation(A, B, W: Single): Single; static;
    class function CubicInterpolation(A, B, W: Single): Single; static;
    class function SineInterpolation(A, B, W: Single): Single; static;

  end;

var
  Form10: TForm10;

implementation

{$R *.dfm}

procedure TForm10.FormCreate(Sender: TObject);
const
  Noises: array [0 .. 9] of Integer = (3, 10, 18, 29, 58, 79, 80, 278, 392, 478);
var
  I: Integer;
begin
  Randomize;
  FPerlinNoises := TList<TPerlinNoise>.Create;
  FFactor := 0.5;
  for I := 1 to 12 do
    FPerlinNoises.Add(TPerlinNoise.Create(IVec2(5)));
  SetInterpolationFunction(CubicInterpolation);
end;

procedure TForm10.FormKeyDown(Sender: TObject; var Key: Word; Shift:
  TShiftState);
begin
  case Key of
    VK_UP:
      FFactor := FFactor + 0.1;
    VK_DOWN:
      FFactor := FFactor - 0.1;
    Ord('A'):
      SetInterpolationFunction(LinearInterpolation);
    Ord('S'):
      SetInterpolationFunction(QuadraticInterpolation);
    Ord('D'):
      SetInterpolationFunction(CubicInterpolation);
    Ord('F'):
      SetInterpolationFunction(SineInterpolation);
    VK_SPACE:
      Invalidate;
  end;
end;

procedure TForm10.FormPaint(Sender: TObject);
var
  P: TIntVector2;
  Size: TIntVector2;
  PerlinNoise: TPerlinNoise;
  V, H: Single;
  Color: TColorRGB;
begin
  Size := IVec2(ClientWidth, ClientHeight);
  for P in Size do
  begin
    Color := 1;
    H := 0;
    for PerlinNoise in FPerlinNoises do
    begin
      if Abs(PerlinNoise[TVector2(P) / Size]) < 0.05 then
      begin
        V := 1 - Abs(PerlinNoise[TVector2(P) / Size]) * 10;
        Color := Color + TColorRGB.HSV(H, 0.8, V);
      end;
      H := H + 0.5;
    end;
    Canvas.Pixels[P.X, P.Y] := Color.EnsureColor.ToWinColor;

  end;
end;

procedure TForm10.SetInterpolationFunction(AFunc: TInterpolate);
var
  PerlinNoise: TPerlinNoise;
begin
  for PerlinNoise in FPerlinNoises do
    PerlinNoise.Interpolate := AFunc;
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

{ TPerlin }

constructor TPerlinNoise.Create(ASize: TIntVector2);
var
  Rand: TRandom;
  P: TIntVector2;
begin
  FSize := ASize;
  SetLength(FGradient, Size.X + 1, Size.Y + 1);
  // Rand := TRandom.FromSeed(42);
  for P in Size do
    FGradient[P.X, P.Y] := TVector2.RandomNormal;
end;

function TPerlinNoise.DotGridGradient(AGridPos: TIntVector2; APos: TVector2): Single;
var
  Delta: TVector2;
begin
  Delta := AGridPos - APos;
  Result := Gradient[AGridPos].Dot(Delta);
end;

function TPerlinNoise.GetGradient(APos: TIntVector2): TVector2;
begin
  APos := IBounds2(Size).RangedMod(APos);
  Result := FGradient[APos.X, APos.Y];
end;

function TPerlinNoise.GetNoise(APos: TVector2): Single;
var
  GridPos: TIntVector2;
  Delta: TVector2;
  Top, Bottom: Single;
begin
  APos := APos * Size;
  GridPos := APos.Floor;
  Delta := APos - GridPos;

  Top := Interpolate(DotGridGradient(GridPos, APos), DotGridGradient(GridPos + IVec2(1, 0), APos), Delta.X);
  Bottom := Interpolate(DotGridGradient(GridPos + IVec2(0, 1), APos), DotGridGradient(GridPos + 1, APos), Delta.X);
  Result := Interpolate(Top, Bottom, Delta.Y);
  Result := Result / Sqrt(0.5);
end;

end.
