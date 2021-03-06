unit Pengine.Color;

interface

uses
  Vcl.Graphics,

  GdiPlus,

  Pengine.Vector;

type

  TColorRGBA = packed record
  public type

    PBytes = ^TBytes;
    TBytes = packed record
      R, G, B, A: Byte;

      function Convert: TColorRGBA;

    end;

  public
    R, G, B, A: Single;

    class operator Add(A, B: TColorRGBA): TColorRGBA;
    class operator Add(A: TColorRGBA; V: Single): TColorRGBA;
    class operator Add(V: Single; A: TColorRGBA): TColorRGBA;

    class operator Subtract(A, B: TColorRGBA): TColorRGBA;
    class operator Subtract(A: TColorRGBA; V: Single): TColorRGBA;
    class operator Subtract(V: Single; A: TColorRGBA): TColorRGBA;

    class operator Multiply(A, B: TColorRGBA): TColorRGBA;
    class operator Multiply(A: TColorRGBA; V: Single): TColorRGBA;
    class operator Multiply(V: Single; A: TColorrGBA): TColorRGBA;

    class operator LogicalNot(A: TColorRGBA): TColorRGBA;
    class operator Equal(A, B: TColorRGBA): Boolean;
    class operator NotEqual(A, B: TColorRGBA): Boolean;

    constructor Create(AColor: TColor; A: Single = 1); overload;
    constructor Create(R, G, B: Single; A: Single = 1); overload;

    class function HSV(H, S, V: Single; A: Single = 1): TColorRGBA; static;
    class function Gray(V: Single; A: Single = 1): TColorRGBA; static;
    class function Rainbow(H: Single; A: Single = 1): TColorRGBA; static;

    class operator Implicit(AValue: TColor): TColorRGBA;
    class operator Implicit(AValue: TColorRGBA): TGPColor;
    class operator Implicit(AValue: TGPColor): TColorRGBA;

    function ToWinColor: TColor;
    function ToBytes: TBytes;
    function ToVector4: TVector4;
    function ToVector: TVector3;

    function EnsureColor: TColorRGBA;

  end;

  TColorRGB = packed record
  public type

    PBytes = ^TBytes;
    TBytes = packed record
      R, G, B: Byte;

      function Convert: TColorRGB;

    end;

  public
    R, G, B: Single;

    class operator Add(A, B: TColorRGB): TColorRGB;
    class operator Add(A: TColorRGB; V: Single): TColorRGB;
    class operator Add(V: Single; A: TColorRGB): TColorRGB;

    class operator Subtract(A, B: TColorRGB): TColorRGB;
    class operator Subtract(A: TColorRGB; V: Single): TColorRGB;
    class operator Subtract(V: Single; A: TColorRGB): TColorRGB;

    class operator Multiply(A, B: TColorRGB): TColorRGB;
    class operator Multiply(A: TColorrGB; V: Single): TColorRGB;
    class operator Multiply(V: Single; A: TColorrGB): TColorRGB;

    class operator LogicalNot(A: TColorRGB): TColorRGB;
    class operator Equal(A, B: TColorRGB): Boolean;
    class operator NotEqual(A, B: TColorRGB): Boolean;

    constructor Create(AColor: TColor); overload;
    constructor Create(R, G, B: Single); overload;

    class function HSV(H, S, V: Single): TColorRGB; static;
    class function Gray(V: Single): TColorRGB; static;
    class function Rainbow(H: Single): TColorRGB; static;

    function ToRGBA(A: Single = 1): TColorRGBA;

    class operator Implicit(AValue: TColorRGBA): TColorRGB;
    class operator Implicit(AValue: TColorRGB): TColorRGBA;
    class operator Implicit(AValue: TColor): TColorRGB;
    class operator Implicit(AValue: Single): TColorRGB;
    class operator Implicit(AValue: TColorRGB): TGPColor;
    class operator Implicit(AValue: TGPColor): TColorRGB;

    function ToWinColor: TColor;
    function ToBytes: TBytes;

    function EnsureColor: TColorRGB;

  end;

function ColorRGB(R, G, B: Single): TColorRGB;
function ColorRGBA(R, G, B: Single; A: Single = 1): TColorRGBA;

const
  ColorTransparent: TColorRGBA = (R: 0.0; G: 0.0; B: 0.0; A: 0.0);

  ColorWhite: TColorRGB = (R: 1.0; G: 1.0; B: 1.0);
  ColorLightGray: TColorRGB = (R: 0.75; G: 0.75; B: 0.75);
  ColorGray: TColorRGB = (R: 0.5; G: 0.5; B: 0.5);
  ColorDarkGray: TColorRGB = (R: 0.25; G: 0.25; B: 0.25);
  ColorBlack: TColorRGB = (R: 0.0; G: 0.0; B: 0.0);

  ColorRed: TColorRGB = (R: 1.0; G: 0.0; B: 0.0);
  ColorOrange: TColorRGB = (R: 1.0; G: 0.5; B: 0.0);
  ColorYellow: TColorRGB = (R: 1.0; G: 1.0; B: 0.0);
  ColorLawnGreen: TColorRGB = (R: 0.5; G: 1.0; B: 0.0);
  ColorLime: TColorRGB = (R: 0.0; G: 1.0; B: 0.0);
  ColorSpringGreen: TColorRGB = (R: 0.0; G: 1.0; B: 0.5);
  ColorCyan: TColorRGB = (R: 0.0; G: 1.0; B: 1.0);
  ColorDodgerBlue: TColorRGB = (R: 0.0; G: 0.5; B: 1.0);
  ColorBlue: TColorRGB = (R: 0.0; G: 0.0; B: 1.0);
  ColorElectricIndigo: TColorRGB = (R: 0.5; G: 0.0; B: 1.0);
  ColorMagenta: TColorRGB = (R: 1.0; G: 0.0; B: 1.0);
  ColorDeepPink: TColorRGB = (R: 1.0; G: 0.0; B: 0.5);

implementation

uses
  Math;

{ TColorRGB }

class operator TColorRGB.Add(A, B: TColorRGB): TColorRGB;
begin
  Result.R := A.R + B.R;
  Result.G := A.G + B.G;
  Result.B := A.B + B.B;
end;

class operator TColorRGB.Subtract(A, B: TColorRGB): TColorRGB;
begin
  Result.R := A.R - B.R;
  Result.G := A.G - B.G;
  Result.B := A.B - B.B;
end;

class operator TColorRGB.Multiply(A, B: TColorRGB): TColorRGB;
begin
  Result.R := A.R * B.R;
  Result.G := A.G * B.G;
  Result.B := A.B * B.B;
end;

class operator TColorRGB.Multiply(A: TColorrGB; V: Single): TColorRGB;
begin
  Result.R := A.R * V;
  Result.G := A.G * V;
  Result.B := A.B * V;
end;

class operator TColorRGB.Multiply(V: Single; A: TColorrGB): TColorRGB;
begin
  Result.R := V * A.R;
  Result.G := V * A.G;
  Result.B := V * A.B;
end;

class operator TColorRGB.LogicalNot(A: TColorRGB): TColorRGB;
begin
  Result := 1 - A;
end;

class operator TColorRGB.Equal(A, B: TColorRGB): Boolean;
begin
  Result := (A.R = B.R) and (A.G = B.G) and (A.B = B.B);
end;

class operator TColorRGB.NotEqual(A, B: TColorRGB): Boolean;
begin
   Result := (A.R <> B.R) or (A.G <> B.G) or (A.B <> B.B);
end;

constructor TColorRGB.Create(AColor: TColor);
begin
  R := AColor and $FF / $FF;
  G := AColor shr 8 and $FF / $FF;
  B := AColor shr 16 and $FF / $FF;
end;

class operator TColorRGB.Add(V: Single; A: TColorRGB): TColorRGB;
begin
  Result.R := V + A.R;
  Result.G := V + A.G;
  Result.B := V + A.B;
end;

class operator TColorRGB.Add(A: TColorRGB; V: Single): TColorRGB;
begin
  Result.R := A.R + V;
  Result.G := A.G + V;
  Result.B := A.B + V;
end;

constructor TColorRGB.Create(R, G, B: Single);
begin
  Self.R := R;
  Self.G := G;
  Self.B := B;
end;

class function TColorRGB.HSV(H, S, V: Single): TColorRGB;

  function HMod(H: Single): Integer; inline;
  begin
    Result := Floor(H * 6);
  end;

  function F(H: Single): Single; inline;
  begin
    Result := H * 6 - HMod(H);
  end;

  function P(V, S: Single): Single; inline;
  begin
    Result := V * (1 - S);
  end;

  function Q(V, S, F: Single): Single; inline;
  begin
    Result := V * (1 - S * F);
  end;

  function T(V, S, F: Single): Single; inline;
  begin
    Result := V * (1 - S * (1 - F));
  end;

begin
  H := Bounds1(0, 6).RangedModL(H) / 6;
  case HMod(H) of
    0:
      Result := TColorRGB.Create(V, T(V, S, F(H)), P(V, S));
    1:
      Result := TColorRGB.Create(Q(V, S, F(H)), V, P(V, S));
    2:
      Result := TColorRGB.Create(P(V, S), V, T(V, S, F(H)));
    3:
      Result := TColorRGB.Create(P(V, S), Q(V, S, F(H)), V);
    4:
      Result := TColorRGB.Create(T(V, S, F(H)), P(V, S), V);
    else // 5
      Result := TColorRGB.Create(V, P(V, S), Q(V, S, F(H)));
  end;
end;

class function TColorRGB.Gray(V: Single): TColorRGB;
begin
  Result.R := V;
  Result.G := V;
  Result.B := V;
end;

class function TColorRGB.Rainbow(H: Single): TColorRGB;
begin
  Result := HSV(H, 1, 1);
end;

class operator TColorRGB.Subtract(V: Single; A: TColorRGB): TColorRGB;
begin

end;

class operator TColorRGB.Subtract(A: TColorRGB; V: Single): TColorRGB;
begin

end;

function TColorRGB.ToBytes: TBytes;
begin
  Result.R := Floor(R * High(Byte) + 0.5);
  Result.G := Floor(G * High(Byte) + 0.5);
  Result.B := Floor(B * High(Byte) + 0.5);
end;

function TColorRGB.ToRGBA(A: Single): TColorRGBA;
begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
  Result.A := A;
end;

class operator TColorRGB.Implicit(AValue: TColorRGBA): TColorRGB;
begin
  Result.R := AValue.R;
  Result.G := AValue.G;
  Result.B := AValue.B;
end;

class operator TColorRGB.Implicit(AValue: TColorRGB): TColorRGBA;
begin
  Result := AValue.ToRGBA(1);
end;

function TColorRGB.ToWinColor: TColor;
begin
  Result := Floor(R * $FF) or Floor(G * $FF) shl 8 or Floor(B * $FF) shl 16;
end;

function TColorRGB.EnsureColor: TColorRGB;
begin
  Result.R := EnsureRange(R, 0, 1);
  Result.G := EnsureRange(G, 0, 1);
  Result.B := EnsureRange(B, 0, 1);
end;

class operator TColorRGB.Implicit(AValue: TColor): TColorRGB;
begin
  Result := TColorRGB.Create(AValue);
end;

class operator TColorRGB.Implicit(AValue: Single): TColorRGB;
begin
  Result := TColorRGB.Gray(AValue);
end;

class operator TColorRGB.Implicit(AValue: TGPColor): TColorRGB;
var
  B: TBytes;
begin
  B.R := AValue.R;
  B.G := AValue.G;
  B.B := AValue.B;
  Result := B.Convert;
end;

class operator TColorRGB.Implicit(AValue: TColorRGB): TGPColor;
var
  B: TBytes;
begin
  B := AValue.ToBytes;
  Result.R := B.R;
  Result.G := B.G;
  Result.B := B.B;
  Result.A := 1;
end;

{ TColorRGBA }

class operator TColorRGBA.Add(A, B: TColorRGBA): TColorRGBA;
begin
  Result.R := A.R + B.R;
  Result.G := A.G + B.G;
  Result.B := A.B + B.B;
  Result.A := A.A;
end;

class operator TColorRGBA.Equal(A, B: TColorRGBA): Boolean;
begin
  Result := (A.R = B.R) and (A.G = B.G) and (A.B = B.B) and (A.A = B.A);
end;

class operator TColorRGBA.LogicalNot(A: TColorRGBA): TColorRGBA;
begin
  Result := 1 - A;
end;

class operator TColorRGBA.Multiply(A, B: TColorRGBA): TColorRGBA;
begin
  Result.R := A.R * B.R;
  Result.G := A.G * B.G;
  Result.B := A.B * B.B;
  Result.A := A.A;
end;

class operator TColorRGBA.Multiply(A: TColorrGBA; V: Single): TColorRGBA;
begin
  Result.R := A.R * V;
  Result.G := A.G * V;
  Result.B := A.B * V;
  Result.A := A.A;
end;

class operator TColorRGBA.Multiply(V: Single; A: TColorrGBA): TColorRGBA;
begin
  Result.R := V  * A.R;
  Result.G := V  * A.G;
  Result.B := V  * A.B;
  Result.A := A.A;
end;

class operator TColorRGBA.NotEqual(A, B: TColorRGBA): Boolean;
begin
  Result := (A.R <> B.R) or (A.G <> B.G) or (A.B <> B.B) or (A.A <> B.A);
end;

constructor TColorRGBA.Create(AColor: TColor; A: Single);
begin
  R := AColor and $FF / $FF;
  G := AColor shr 8 and $FF / $FF;
  B := AColor shr 16 and $FF / $FF;
  Self.A := A;
end;

class operator TColorRGBA.Add(V: Single; A: TColorRGBA): TColorRGBA;
begin
  Result.R := V + A.R;
  Result.G := V + A.G;
  Result.B := V + A.B;
  Result.A := A.A;
end;

class operator TColorRGBA.Add(A: TColorRGBA; V: Single): TColorRGBA;
begin
  Result.R := A.R + V;
  Result.G := A.G + V;
  Result.B := A.B + V;
  Result.A := A.A;
end;

constructor TColorRGBA.Create(R, G, B: Single; A: Single);
begin
  Self.R := R;
  Self.G := G;
  Self.B := B;
  Self.A := A;
end;

class function TColorRGBA.HSV(H, S, V: Single; A: Single): TColorRGBA;
begin
  Result := TColorRGB.HSV(H, S, V);
  Result.A := A;
end;

class operator TColorRGBA.Implicit(AValue: TGPColor): TColorRGBA;
var
  B: TBytes;
begin
  B.R := AValue.R;
  B.G := AValue.G;
  B.B := AValue.B;
  B.A := AValue.A;
  Result := B.Convert;
end;

class operator TColorRGBA.Implicit(AValue: TColorRGBA): TGPColor;
var
  B: TBytes;
begin
  B := AValue.ToBytes;
  Result.R := B.R;
  Result.G := B.G;
  Result.B := B.B;
  Result.A := B.A;
end;

class function TColorRGBA.Gray(V: Single; A: Single): TColorRGBA;
begin
  Result.R := V;
  Result.G := V;
  Result.B := V;
  Result.A := A;
end;

class function TColorRGBA.Rainbow(H: Single; A: Single): TColorRGBA;
begin
  Result := TColorRGB.Rainbow(H);
  Result.A := A;
end;

class operator TColorRGBA.Subtract(V: Single; A: TColorRGBA): TColorRGBA;
begin
  Result.R := V - A.R;
  Result.G := V - A.G;
  Result.B := V - A.B;
  Result.A := A.A;
end;

class operator TColorRGBA.Subtract(A: TColorRGBA; V: Single): TColorRGBA;
begin
  Result.R := A.R - V;
  Result.G := A.G - V;
  Result.B := A.B - V;
  Result.A := A.A;
end;

class operator TColorRGBA.Implicit(AValue: TColor): TColorRGBA;
begin
  Result := TColorRGBA.Create(AValue, 1);
end;

function TColorRGBA.ToBytes: TBytes;
begin
  Result.R := Floor(R * High(Byte) + 0.5);
  Result.G := Floor(G * High(Byte) + 0.5);
  Result.B := Floor(B * High(Byte) + 0.5);
  Result.A := Floor(A * High(Byte) + 0.5);
end;

function TColorRGBA.ToVector: TVector3;
begin
  Result := ToVector4;
end;

function TColorRGBA.ToVector4: TVector4;
begin
  Result := PVector4(@Self)^;
end;

function TColorRGBA.ToWinColor: TColor;
begin
  Result := TColorRGB(Self).ToWinColor;
end;

function TColorRGBA.EnsureColor: TColorRGBA;
begin
  Result.R := EnsureRange(R, 0, 1);
  Result.G := EnsureRange(G, 0, 1);
  Result.B := EnsureRange(B, 0, 1);
  Result.A := EnsureRange(A, 0, 1);
end;

class operator TColorRGBA.Subtract(A, B: TColorRGBA): TColorRGBA;
begin
  Result.R := A.R - B.R;
  Result.G := A.G - B.G;
  Result.B := A.B - B.B;
  Result.A := A.A;
end;

{ Shorthand constructors }

function ColorRGB(R, G, B: Single): TColorRGB;
begin
  Result := TColorRGB.Create(R, G, B);
end;

function ColorRGBA(R, G, B: Single; A: Single = 1): TColorRGBA;
begin
  Result := TColorRGBA.Create(R, G, B, A);
end;

{ TColorRGBA.TBytes }

function TColorRGBA.TBytes.Convert: TColorRGBA;
begin
  Result.R := R / 255;
  Result.G := G / 255;
  Result.B := B / 255;
  Result.A := A / 255;
end;

{ TColorRGB.TBytes }

function TColorRGB.TBytes.Convert: TColorRGB;
begin
  Result.R := R / 255;
  Result.G := G / 255;
  Result.B := B / 255;
end;

end.
