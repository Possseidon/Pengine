unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,

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

  TPerlinNoise = class
  private
    FGradient: array of array of TVector3;
    FSize: TIntVector2;

    function Lerp(A, B, C, D: TVector3; V: TVector2): TVector3; inline;
    function GetNoise(APos: TVector2): TVector3;
    function GetGradient(APos: TIntVector2): TVector3;
    
  public
    constructor Create(ASize: TIntVector2);
    
    property Size: TIntVector2 read FSize;
    property Gradient[APos: TIntVector2]: TVector3 read GetGradient; 
    property Noise[APos: TVector2]: TVector3 read GetNoise; default;
    
  end;

  TForm10 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
  private
    FPerlinNoises: IList<TPerlinNoise>;  
    FFactor: Single;
    
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
  FPerlinNoises := TList<TPerlinNoise>.Create;
  FFactor := 10000;
  for I := 1 to 20 do
    FPerlinNoises.Add(TPerlinNoise.Create(IVec2(Random(42) + 2)));
end;

procedure TForm10.FormKeyDown(Sender: TObject; var Key: Word; Shift:
    TShiftState);
begin
  case Key of
    VK_UP:
      FFactor := FFactor + 0.1;
    VK_DOWN:                   
      FFactor := FFactor - 0.1;      
  end;
  // Invalidate;
end;

procedure TForm10.FormPaint(Sender: TObject);
var
  P: TIntVector2;
  Size: TIntVector2;
  PerlinNoise: TPerlinNoise;
  V: TVector3;
begin
  Size := IVec2(ClientWidth, ClientHeight);
  for P in Size do
  begin
    V := 1;
    for PerlinNoise in FPerlinNoises do
        V := V * PerlinNoise[TVector2(P) / Size];
    V := V * FPerlinNoises.Count * FFactor;
    Canvas.Pixels[P.X, P.Y] := TColorRGB(V).ToWinColor;  
  end;
end;

{ TPerlin }

constructor TPerlinNoise.Create(ASize: TIntVector2);
var
  Rand: TRandom;
  P: TIntVector2;
begin
  FSize := ASize;
  SetLength(FGradient, Size.X, Size.Y);
  Rand := TRandom.FromSeed(42);
  for P in Size do
    FGradient[P.X, P.Y] := Vec3(Rand.NextSingle, Rand.NextSingle, Rand.NextSingle);
end;

function TPerlinNoise.GetGradient(APos: TIntVector2): TVector3;
begin
  APos := IBounds2(Size).Clamp(APos);
  Result := FGradient[APos.X, APos.Y];
end;

function TPerlinNoise.GetNoise(APos: TVector2): TVector3;
var
  GridPos: TIntVector2;
  Delta: TVector2;
begin
  APos := APos * (Size - 1);
  GridPos := APos.Floor;
  Delta := APos - GridPos;
  Result := Lerp(Gradient[GridPos], Gradient[GridPos + IVec2(1, 0)],
  Gradient[GridPos + IVec2(0, 1)], Gradient[GridPos + IVec2(1, 1)], Delta);
end;

function TPerlinNoise.Lerp(A, B, C, D: TVector3; V: TVector2): TVector3;
var
  Top, Bottom: TVector3;
begin
  Top := A * (1 - V.X) + B * V.X;
  Bottom := C * (1 - V.X) + D * V.X;
  Result := Top * (1 - V.Y) + Bottom * V.Y;
end;

end.
