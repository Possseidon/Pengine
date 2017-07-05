unit Matrix;

interface

uses
  SysUtils;

type

  TMatrixResult2 = array [0 .. 1] of Single;
  TMatrixResult3 = array [0 .. 2] of Single;

  { EMatrixMultiply }

  EMatrixMultiply = class (Exception)
  public
    constructor Create(AACols, ABRows: Integer);
  end;

  { TMatrix }

  TMatrix = record
  private
    FElement: array of array of Single;

    function GetElement(I, J: Byte): Single;
    procedure SetElement(I, J: Byte; const Value: Single);

    property E[I, J: Byte]: Single read GetElement write SetElement; default;
  public
    class operator Add(A, B: TMatrix): TMatrix;

    class operator Multiply(V: Single; A: TMatrix): TMatrix;
    class operator Multiply(A, B: TMatrix): TMatrix;

    procedure SetSize(Cols, Rows: Byte);
    procedure Clear(Value: Single = 0);
    procedure LoadIdentity;

    procedure Swap(Col1, Col2: Byte);

    function ColSum(Col: Byte): Single;
    function RowSum(Row: Byte): Single;

    function SolveCol(Col: Byte; out R: Single): Boolean;
    function Solve(out R: array of Single): Boolean;
    function Solveable: Boolean;

    function Transpose: TMatrix;
    function Minor(I, J: Integer): TMatrix;
    function Cofactor(I, J: Integer): Single;
    function CofactorMatrix: TMatrix;
    function Adjugate: TMatrix;
    function Inverse: TMatrix;
    function Determinant: Single;

    function Cols: Byte;
    function Rows: Byte;
  end;

  { TMatrix3 }

  TMatrix3 = record
  private
    FElement: array [0 .. 2, 0 .. 2] of Single;

    function GetElement(I, J: Byte): Single;
    procedure SetElement(I, J: Byte; const Value: Single);

    property E[I, J: Byte]: Single read GetElement write SetElement; default;
  public
    class operator Add(A, B: TMatrix3): TMatrix3;
    class operator Multiply(V: Single; A: TMatrix3): TMatrix3;
    class operator Multiply(A, B: TMatrix3): TMatrix3;

    procedure Clear(Value: Single = 0);
    procedure LoadIdentity;

    function Transpose: TMatrix3;
    function Determinant: Single;
    function Minor(I, J: Integer): TMatrix;
    function Cofactor(I, J: Integer): Single;
    function CofactorMatrix: TMatrix3;
    function Adjugate: TMatrix3;
    function Inverse: TMatrix3;
  end;

  { TMatrix4 }

  TMatrix4 = record
  private
    FElement: array [0 .. 3, 0 .. 3] of Single;

    function GetElement(I, J: Byte): Single;
    function GetMinor(I, J: Integer): TMatrix3;

    procedure SetElement(I, J: Byte; const Value: Single);
    procedure SetMinor(I, J: Integer; AValue: TMatrix3);

    property E[I, J: Byte]: Single read GetElement write SetElement; default;
  public
    class operator Add(A, B: TMatrix4): TMatrix4;
    class operator Multiply(V: Single; A: TMatrix4): TMatrix4;
    class operator Multiply(A, B: TMatrix4): TMatrix4;

    class operator Equal(A, B: TMatrix4): Boolean;
    class operator NotEqual(A, B: TMatrix4): Boolean;

    procedure Clear(Value: Single = 0);
    procedure LoadIdentity;

    function Transpose: TMatrix4;
    function Determinant: Single;
    property Minor[I, J: Integer]: TMatrix3 read GetMinor write SetMinor;
    function Cofactor(I, J: Integer): Single;
    function CofactorMatrix: TMatrix4;
    function Adjugate: TMatrix4;
    function Inverse: TMatrix4;

    function DataPointer: Pointer;
  end;

  PMatrix = ^TMatrix;
  PMatrix4 = ^TMatrix4;
  PMatrix3 = ^TMatrix3;

var
  M3x2: TMatrix;
  M4x3: TMatrix;

const
  Matrix4Identity: TMatrix4 = (FElement:(
    (1, 0, 0, 0),
    (0, 1, 0, 0),
    (0, 0, 1, 0),
    (0, 0, 0, 1)
  ));

  Matrix3Identity: TMatrix3 = (FElement:(
    (1, 0, 0),
    (0, 1, 0),
    (0, 0, 1)
  ));

implementation

uses
  Math;

{ EMatrixMultiply }

constructor EMatrixMultiply.Create(AACols, ABRows: Integer);
begin
  CreateFmt('Columns of Matrix A not equal to Rows of Matrix B! %d <> %d', [AACols, ABRows]);
end;

{ TMatrix3 }

function TMatrix3.GetElement(I, J: Byte): Single;
begin
  Result := FElement[I, J];
end;

procedure TMatrix3.SetElement(I, J: Byte; const Value: Single);
begin
  FElement[I, J] := Value;
end;

class operator TMatrix3.Add(A, B: TMatrix3): TMatrix3;
begin
  Result[0, 0] := A[0, 0] + B[0, 0];
  Result[1, 0] := A[1, 0] + B[1, 0];
  Result[2, 0] := A[2, 0] + B[2, 0];
  Result[0, 1] := A[0, 1] + B[0, 1];
  Result[1, 1] := A[1, 1] + B[1, 1];
  Result[2, 1] := A[2, 1] + B[2, 1];
  Result[0, 2] := A[0, 2] + B[0, 2];
  Result[1, 2] := A[1, 2] + B[1, 2];
  Result[2, 2] := A[2, 2] + B[2, 2];
end;

class operator TMatrix3.Multiply(V: Single; A: TMatrix3): TMatrix3;
begin
  Result[0, 0] := A[0, 0] * V;
  Result[1, 0] := A[1, 0] * V;
  Result[2, 0] := A[2, 0] * V;
  Result[0, 1] := A[0, 1] * V;
  Result[1, 1] := A[1, 1] * V;
  Result[2, 1] := A[2, 1] * V;
  Result[0, 2] := A[0, 2] * V;
  Result[1, 2] := A[1, 2] * V;
  Result[2, 2] := A[2, 2] * V;
end;

class operator TMatrix3.Multiply(A, B: TMatrix3): TMatrix3;
begin
  Result[0, 0] := A[0, 0] * B[0, 0] + A[1, 0] * B[0, 1] + A[2, 0] * B[0, 2];
  Result[1, 0] := A[0, 0] * B[1, 0] + A[1, 0] * B[1, 1] + A[2, 0] * B[1, 2];
  Result[2, 0] := A[0, 0] * B[2, 0] + A[1, 0] * B[2, 1] + A[2, 0] * B[2, 2];
  Result[0, 1] := A[0, 1] * B[0, 0] + A[1, 1] * B[0, 1] + A[2, 1] * B[0, 2];
  Result[1, 1] := A[0, 1] * B[1, 0] + A[1, 1] * B[1, 1] + A[2, 1] * B[1, 2];
  Result[2, 1] := A[0, 1] * B[2, 0] + A[1, 1] * B[2, 1] + A[2, 1] * B[2, 2];
  Result[0, 2] := A[0, 2] * B[0, 0] + A[1, 2] * B[0, 1] + A[2, 2] * B[0, 2];
  Result[1, 2] := A[0, 2] * B[1, 0] + A[1, 2] * B[1, 1] + A[2, 2] * B[1, 2];
  Result[2, 2] := A[0, 2] * B[2, 0] + A[1, 2] * B[2, 1] + A[2, 2] * B[2, 2];
end;

procedure TMatrix3.Clear(Value: Single);
begin
  if Value = 0 then
    FillChar(FElement, SizeOf(Single) * 9, 0)
  else
  begin
    FElement[0, 0] := Value;
    FElement[1, 0] := Value;
    FElement[2, 0] := Value;
    FElement[0, 1] := Value;
    FElement[1, 1] := Value;
    FElement[2, 1] := Value;
    FElement[0, 2] := Value;
    FElement[1, 2] := Value;
    FElement[2, 2] := Value;
  end;
end;

procedure TMatrix3.LoadIdentity;
begin
  Move(Matrix3Identity, FElement, 64);
end;

function TMatrix3.Transpose: TMatrix3;
begin
  Result[0, 0] := FElement[0, 0];
  Result[1, 0] := FElement[0, 1];
  Result[2, 0] := FElement[0, 2];
  Result[0, 1] := FElement[1, 0];
  Result[1, 1] := FElement[1, 1];
  Result[2, 1] := FElement[1, 2];
  Result[0, 2] := FElement[2, 0];
  Result[1, 2] := FElement[2, 1];
  Result[2, 2] := FElement[2, 2];
end;

function TMatrix3.Determinant: Single;
begin
  Result := E[0, 0] * E[1, 1] * E[2, 2] +
            E[0, 1] * E[1, 2] * E[2, 0] +
            E[0, 2] * E[1, 0] * E[2, 1] -
            E[2, 0] * E[1, 1] * E[0, 2] -
            E[2, 1] * E[1, 2] * E[0, 0] -
            E[2, 2] * E[1, 0] * E[0, 1];
end;

function TMatrix3.Minor(I, J: Integer): TMatrix;
var
  X1, Y1, X2, Y2: Integer;
begin
  Result.SetSize(2, 2);
  X2 := 0;
  for X1 := 0 to 2 do
  begin
    if X1 = I then
      Continue;
    Y2 := 0;
    for Y1 := 0 to 2 do
    begin
      if Y1 = J then
        Continue;
      Result[X2, Y2] := E[X1, Y1];
      Y2 := Y2 + 1;
    end;
    X2 := X2 + 1;
  end;
end;

function TMatrix3.Cofactor(I, J: Integer): Single;
begin
  if ((I + J) mod 2) = 0 then
    Result := Minor(I, J).Determinant
  else
    Result := -Minor(I, J).Determinant;
end;

function TMatrix3.CofactorMatrix: TMatrix3;
var
  I, J: Integer;
begin
  for I := 0 to 2 do
    for J := 0 to 2 do
      Result[I, J] := Cofactor(I, J);
end;

function TMatrix3.Adjugate: TMatrix3;
begin
  Result := CofactorMatrix.Transpose;
end;

function TMatrix3.Inverse: TMatrix3;
begin
  Result := 1 / Determinant * Adjugate;
end;

{ TMatrix }

class operator TMatrix.Add(A, B: TMatrix): TMatrix;
var
  I, J: Integer;
begin
  if (A.Cols = B.Cols) and (A.Rows = B.Rows) then
  begin
    Result.SetSize(A.Cols, A.Rows);
    for I := 0 to A.Cols - 1 do
      for J := 0 to A.Rows - 1 do
        Result[I, J] := A[I, J] + B[I, J];
  end
  else
    Result.SetSize(0, 0);
end;

function TMatrix.Adjugate: TMatrix;
begin
  Result := CofactorMatrix.Transpose;
end;

procedure TMatrix.Clear(Value: Single = 0);
var
  I, J: Integer;
begin
  if Value = 0 then
    for I := 0 to Cols - 1 do
      FillChar(FElement[I, 0], Rows * SizeOf(Single), 0)
  else
    for I := 0 to Cols - 1 do
      for J := 0 to Rows - 1 do
        FElement[I, J] := Value;
end;

procedure TMatrix.SetElement(I, J: Byte; const Value: Single);
begin
  FElement[I, J] := Value;
end;

procedure TMatrix.SetSize(Cols, Rows: Byte);
begin
  SetLength(FElement, Cols, Rows);
end;

function TMatrix.Determinant: Single;
var
  S, R: Byte;
begin
  S := Min(Cols, Rows);
  // 3x3/2x2(/1x1) direkt berechnen (schneller)
  if S = 3 then
    Exit(
      E[0, 0] * E[1, 1] * E[2, 2] +
      E[0, 1] * E[1, 2] * E[2, 0] +
      E[0, 2] * E[1, 0] * E[2, 1] -
      E[2, 0] * E[1, 1] * E[0, 2] -
      E[2, 1] * E[1, 2] * E[0, 0] -
      E[2, 2] * E[1, 0] * E[0, 1])
  else if S = 2 then
    Exit(
      E[0, 0] * E[1, 1] -
      E[1, 0] * E[0, 1])
  else if S = 1 then
    Exit(E[0, 0]);

  Result := 0;
  // rekursive Determinanten berechnung bis 3x3
  for R := 0 to S - 1 do
    if R mod 2 = 0 then
      Result := Result + E[R, 0] * Minor(R, 0).Determinant
    else
      Result := Result - E[R, 0] * Minor(R, 0).Determinant
end;

function TMatrix.GetElement(I, J: Byte): Single;
begin
  Result := FElement[I, J];
end;

function TMatrix.Inverse: TMatrix;
begin
  Result := 1 / Determinant * Adjugate;
end;

procedure TMatrix.LoadIdentity;
var
  I: Integer;
begin
  Clear;
  for I := 0 to Rows - 1 do
    E[I, I] := 1;
end;

function TMatrix.Rows: Byte;
begin
  Result := Length(FElement[0]);
end;

function TMatrix.RowSum(Row: Byte): Single;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Cols do
    Result := Result + E[I, Row];
end;

class operator TMatrix.Multiply(V: Single; A: TMatrix): TMatrix;
var
  I, J: Integer;
begin
  Result.SetSize(A.Cols, A.Rows);
  for I := 0 to A.Cols - 1 do
    for J := 0 to A.Rows - 1 do
      Result[I, J] := A[I, J] * V;
end;

function TMatrix.Solve(out R: array of Single): Boolean;
var
  I: Byte;
  OldDet, NewDet: Single;
begin
  Result := True;
  OldDet := Determinant;

  if OldDet = 0 then
    Exit(False);

  for I := 0 to Cols - 2 do
  begin
    Swap(I, Cols - 1);
    NewDet := Determinant;
    Swap(I, Cols - 1);
    R[I] := NewDet / OldDet;
  end;
end;

function TMatrix.Solveable: Boolean;
begin
  Result := Determinant <> 0;
end;

function TMatrix.SolveCol(Col: Byte; out R: Single): Boolean;
var
  OldDet, NewDet: Single;
begin
  OldDet := Determinant;
  Swap(Col, Cols - 1);
  NewDet := Determinant;
  Swap(Col, Cols - 1);
  R := NewDet / OldDet;
  if OldDet = 0 then
    Result := False
  else
    Result := True;
end;

procedure TMatrix.Swap(Col1, Col2: Byte);
var
  P: Pointer;
begin
  if Col1 = Col2 then
    Exit;
  P := Pointer(FElement[Col1]);
  Pointer(FElement[Col1]) := Pointer(FElement[Col2]);
  Pointer(FElement[Col2]) := P;
end;

function TMatrix.Transpose: TMatrix;
var
  I, J: Byte;
begin
  Result.SetSize(Rows, Cols);
  for I := 0 to Cols - 1 do
    for J := 0 to Rows - 1 do
      Result[J, I] := E[I, J];
end;

function TMatrix.Cofactor(I, J: Integer): Single;
begin
  if ((I + J) mod 2) = 0 then
    Result := Minor(I, J).Determinant
  else
    Result := -Minor(I, J).Determinant;
end;

function TMatrix.CofactorMatrix: TMatrix;
var
  I, J: Integer;
begin
  Result.SetSize(Rows, Cols);
  for I := 0 to Rows - 1 do
    for J := 0 to Cols - 1 do
      Result[I, J] := Cofactor(I, J);
end;

function TMatrix.Cols: Byte;
begin
  Result := Length(FElement);
end;

function TMatrix.ColSum(Col: Byte): Single;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Cols do
    Result := Result + E[Col, I];
end;

function TMatrix.Minor(I, J: Integer): TMatrix;
var
  X1, Y1, X2, Y2: Integer;
begin
  Result.SetSize(Cols - 1, Rows - 1);
  X2 := 0;
  for X1 := 0 to Rows - 1 do
  begin
    if X1 = I then
      Continue;
    Y2 := 0;
    for Y1 := 0 to Cols - 1 do
    begin
      if Y1 = J then
        Continue;
      Result[X2, Y2] := E[X1, Y1];
      Y2 := Y2 + 1;
    end;
    X2 := X2 + 1;
  end;
end;

class operator TMatrix.Multiply(A, B: TMatrix): TMatrix;
var
  I, J, K: Integer;
begin
  if A.Cols = B.Rows then
  begin
    Result.SetSize(B.Cols, A.Rows);
    for I := 0 to Result.Rows - 1 do
      for J := 0 to Result.Cols - 1 do
        for K := 0 to A.Cols - 1 do
          Result[J, I] := Result[J, I] + A[K, I] * B[J, K];
  end
  else
    raise EMatrixMultiply.Create(A.Cols, B.Rows);
end;

{ TMatrix4 }
// most without loops for performance boooost

class operator TMatrix4.Add(A, B: TMatrix4): TMatrix4;
begin
  Result[0, 0] := A[0, 0] + B[0, 0];
  Result[1, 0] := A[1, 0] + B[1, 0];
  Result[2, 0] := A[2, 0] + B[2, 0];
  Result[3, 0] := A[3, 0] + B[3, 0];
  Result[0, 1] := A[0, 1] + B[0, 1];
  Result[1, 1] := A[1, 1] + B[1, 1];
  Result[2, 1] := A[2, 1] + B[2, 1];
  Result[3, 1] := A[3, 1] + B[3, 1];
  Result[0, 2] := A[0, 2] + B[0, 2];
  Result[1, 2] := A[1, 2] + B[1, 2];
  Result[2, 2] := A[2, 2] + B[2, 2];
  Result[3, 2] := A[3, 2] + B[3, 2];
  Result[0, 3] := A[0, 3] + B[0, 3];
  Result[1, 3] := A[1, 3] + B[1, 3];
  Result[2, 3] := A[2, 3] + B[2, 3];
  Result[3, 3] := A[3, 3] + B[3, 3];
end;

function TMatrix4.Adjugate: TMatrix4;
begin
  Result := CofactorMatrix.Transpose;
end;

procedure TMatrix4.Clear(Value: Single);
begin
  if Value = 0 then
    FillChar(FElement, 16 * SizeOf(Single), 0)
  else
  begin
    FElement[0, 0] := Value;
    FElement[1, 0] := Value;
    FElement[2, 0] := Value;
    FElement[3, 0] := Value;
    FElement[0, 1] := Value;
    FElement[1, 1] := Value;
    FElement[2, 1] := Value;
    FElement[3, 1] := Value;
    FElement[0, 2] := Value;
    FElement[1, 2] := Value;
    FElement[2, 2] := Value;
    FElement[3, 2] := Value;
    FElement[0, 3] := Value;
    FElement[1, 3] := Value;
    FElement[2, 3] := Value;
    FElement[3, 3] := Value;
  end;
end;

function TMatrix4.Cofactor(I, J: Integer): Single;
begin
  if ((I + J) mod 2) = 0 then
    Result := Minor[I, J].Determinant
  else
    Result := -Minor[I, J].Determinant;
end;

function TMatrix4.CofactorMatrix: TMatrix4;
var
  I, J: Integer;
begin
  for I := 0 to 3 do
    for J := 0 to 3 do
      Result[I, J] := Cofactor(I, J);
end;

function TMatrix4.Determinant: Single;
var
  R, X, Y: Byte;
  Pos: Boolean;
  T: TMatrix;
begin
  Result := 0;
  Pos := True;
  T.SetSize(3, 3);
  for R := 0 to 3 do
  begin
    for X := 0 to 2 do
      for Y := 0 to 2 do
        T[X, Y] := E[X + 1, Y + Ord(Y >= R)]; // Y >= R -> [+1] else -> [+0]
    if E[0, R] <> 0 then
      Result := Result + E[0, R] * T.Determinant * (Ord(Pos) * 2 - 1);
    Pos := not Pos;
  end;
end;

function TMatrix4.GetElement(I, J: Byte): Single;
begin
  Result := FElement[I, J];
end;

function TMatrix4.GetMinor(I, J: Integer): TMatrix3;
var
  X1, Y1, X2, Y2: Integer;
begin
  X2 := 0;
  for X1 := 0 to 3 do
  begin
    if X1 = I then
      Continue;
    Y2 := 0;
    for Y1 := 0 to 3 do
    begin
      if Y1 = J then
        Continue;
      Result[X2, Y2] := E[X1, Y1];
      Y2 := Y2 + 1;
    end;
    X2 := X2 + 1;
  end;
end;

function TMatrix4.Inverse: TMatrix4;
begin
  Result := 1 / Determinant * Adjugate;
end;

function TMatrix4.DataPointer: Pointer;
begin
  Result := @FElement[0, 0];
end;

procedure TMatrix4.LoadIdentity;
begin
  Move(Matrix4Identity, FElement, SizeOf(Single) * 16);
end;

class operator TMatrix4.Multiply(A, B: TMatrix4): TMatrix4;
begin
  Result[0, 0] := A[0, 0] * B[0, 0] + A[1, 0] * B[0, 1] + A[2, 0] * B[0, 2] + A[3, 0] * B[0, 3];
  Result[1, 0] := A[0, 0] * B[1, 0] + A[1, 0] * B[1, 1] + A[2, 0] * B[1, 2] + A[3, 0] * B[1, 3];
  Result[2, 0] := A[0, 0] * B[2, 0] + A[1, 0] * B[2, 1] + A[2, 0] * B[2, 2] + A[3, 0] * B[2, 3];
  Result[3, 0] := A[0, 0] * B[3, 0] + A[1, 0] * B[3, 1] + A[2, 0] * B[3, 2] + A[3, 0] * B[3, 3];
  Result[0, 1] := A[0, 1] * B[0, 0] + A[1, 1] * B[0, 1] + A[2, 1] * B[0, 2] + A[3, 1] * B[0, 3];
  Result[1, 1] := A[0, 1] * B[1, 0] + A[1, 1] * B[1, 1] + A[2, 1] * B[1, 2] + A[3, 1] * B[1, 3];
  Result[2, 1] := A[0, 1] * B[2, 0] + A[1, 1] * B[2, 1] + A[2, 1] * B[2, 2] + A[3, 1] * B[2, 3];
  Result[3, 1] := A[0, 1] * B[3, 0] + A[1, 1] * B[3, 1] + A[2, 1] * B[3, 2] + A[3, 1] * B[3, 3];
  Result[0, 2] := A[0, 2] * B[0, 0] + A[1, 2] * B[0, 1] + A[2, 2] * B[0, 2] + A[3, 2] * B[0, 3];
  Result[1, 2] := A[0, 2] * B[1, 0] + A[1, 2] * B[1, 1] + A[2, 2] * B[1, 2] + A[3, 2] * B[1, 3];
  Result[2, 2] := A[0, 2] * B[2, 0] + A[1, 2] * B[2, 1] + A[2, 2] * B[2, 2] + A[3, 2] * B[2, 3];
  Result[3, 2] := A[0, 2] * B[3, 0] + A[1, 2] * B[3, 1] + A[2, 2] * B[3, 2] + A[3, 2] * B[3, 3];
  Result[0, 3] := A[0, 3] * B[0, 0] + A[1, 3] * B[0, 1] + A[2, 3] * B[0, 2] + A[3, 3] * B[0, 3];
  Result[1, 3] := A[0, 3] * B[1, 0] + A[1, 3] * B[1, 1] + A[2, 3] * B[1, 2] + A[3, 3] * B[1, 3];
  Result[2, 3] := A[0, 3] * B[2, 0] + A[1, 3] * B[2, 1] + A[2, 3] * B[2, 2] + A[3, 3] * B[2, 3];
  Result[3, 3] := A[0, 3] * B[3, 0] + A[1, 3] * B[3, 1] + A[2, 3] * B[3, 2] + A[3, 3] * B[3, 3];
end;

class operator TMatrix4.Equal(A, B: TMatrix4): Boolean;
begin
  Result := CompareMem(@A, @B, SizeOf(Single) * 16);
end;

class operator TMatrix4.NotEqual(A, B: TMatrix4): Boolean;
begin
  Result := not (A = B);
end;

class operator TMatrix4.Multiply(V: Single; A: TMatrix4): TMatrix4;
begin
  Result[0, 0] := V * A[0, 0];
  Result[1, 0] := V * A[1, 0];
  Result[2, 0] := V * A[2, 0];
  Result[3, 0] := V * A[3, 0];
  Result[0, 1] := V * A[0, 1];
  Result[1, 1] := V * A[1, 1];
  Result[2, 1] := V * A[2, 1];
  Result[3, 1] := V * A[3, 1];
  Result[0, 2] := V * A[0, 2];
  Result[1, 2] := V * A[1, 2];
  Result[2, 2] := V * A[2, 2];
  Result[3, 2] := V * A[3, 2];
  Result[0, 3] := V * A[0, 3];
  Result[1, 3] := V * A[1, 3];
  Result[2, 3] := V * A[2, 3];
  Result[3, 3] := V * A[3, 3];
end;

procedure TMatrix4.SetElement(I, J: Byte; const Value: Single);
begin
  FElement[I, J] := Value;
end;

procedure TMatrix4.SetMinor(I, J: Integer; AValue: TMatrix3);
var
  X1, Y1, X2, Y2: Integer;
begin
  X2 := 0;
  for X1 := 0 to 3 do
  begin
    if X1 = I then
      Continue;
    Y2 := 0;
    for Y1 := 0 to 3 do
    begin
      if Y1 = J then
        Continue;
      E[X2, Y2] := AValue[X1, Y1];
      Y2 := Y2 + 1;
    end;
    X2 := X2 + 1;
  end;
end;

function TMatrix4.Transpose: TMatrix4;
begin
  Result[0, 0] := FElement[0, 0];
  Result[1, 0] := FElement[0, 1];
  Result[2, 0] := FElement[0, 2];
  Result[3, 0] := FElement[0, 3];
  Result[0, 1] := FElement[1, 0];
  Result[1, 1] := FElement[1, 1];
  Result[2, 1] := FElement[1, 2];
  Result[3, 1] := FElement[1, 3];
  Result[0, 2] := FElement[2, 0];
  Result[1, 2] := FElement[2, 1];
  Result[2, 2] := FElement[2, 2];
  Result[3, 2] := FElement[2, 3];
  Result[0, 3] := FElement[3, 0];
  Result[1, 3] := FElement[3, 1];
  Result[2, 3] := FElement[3, 2];
  Result[3, 3] := FElement[3, 3];
end;

begin
  M3x2.SetSize(3, 2);
  M3x2.Clear;
  M4x3.SetSize(4, 3);
  M4x3.Clear;

end.
