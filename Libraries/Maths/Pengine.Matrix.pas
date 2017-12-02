unit Pengine.Matrix;

interface

uses
  System.Math,
  System.SysUtils,

  Pengine.IntMaths;

type

  // TODO: XmlDoc
  EMatrixMultiply = class(Exception)
  public
    constructor Create;
  end;

  // TODO: XmlDoc
  EMatrixDimension = class(Exception)
  public
    constructor Create;
  end;

  // TODO: XmlDoc
  EMatrixRangeError = class(Exception)
  public
    constructor Create;
  end;

  /// <remarks>This is not a value type<p/>Use <see cref="Pengine.Matrix|TMatrix.Copy"/> instead.</remarks>
  TMatrix = record
  private
    Data: array of array of Single;
    
    function GetCols: Integer;
    function GetRows: Integer;
    procedure SetCols(const Value: Integer);
    procedure SetRows(const Value: Integer);

    function GetSize: TIntVector2;
    procedure SetSize(const Value: TIntVector2); inline;

    function GetNum(I, J: Byte): Single; overload; inline;
    procedure SetNum(I, J: Byte; const Value: Single); overload; inline;
    function GetNum(V: TIntVector2): Single; overload;
    procedure SetNum(V: TIntVector2; const Value: Single); overload;

    function GetMinor(V: TIntVector2): TMatrix; overload;
    function GetMinor(I, J: Integer): TMatrix; overload; inline;
    procedure SetMinor(V: TIntVector2; const Value: TMatrix); overload;
    procedure SetMinor(I, J: Integer; const Value: TMatrix); overload; inline;

    function GetCofactor(V: TIntVector2): Single;
    
    property Num[I, J: Byte]: Single read GetNum write SetNum; default;
    {$HINTS OFF}
    property Num[V: TIntVector2]: Single read GetNum write SetNum; default;
    {$HINTS ON}
    
    procedure RangeCheck(V: TIntVector2); inline;
    class procedure DimensionCheck(const A, B: TMatrix); static; inline;

  public
    class operator Add(A, B: TMatrix): TMatrix; overload;
    class operator Add(A: TMatrix; B: Single): TMatrix; overload;
    class operator Add(A: Single; B: TMatrix): TMatrix; overload;

    class operator Subtract(A, B: TMatrix): TMatrix; overload;
    class operator Subtract(A: TMatrix; B: Single): TMatrix; overload;
    class operator Subtract(A: Single; B: TMatrix): TMatrix; overload;

    class operator Multiply(A, B: TMatrix): TMatrix; overload;

    class operator Multiply(A: TMatrix; B: Single): TMatrix; overload;
    class operator Multiply(A: Single; B: TMatrix): TMatrix; overload;
                                                                      
    class operator Divide(A, B: TMatrix): TMatrix; overload;
    class operator Divide(A: TMatrix; B: Single): TMatrix; overload;
    class operator Divide(A: Single; B: TMatrix): TMatrix; overload;

    class operator Equal(A, B: TMatrix): Boolean;
    class operator NotEqual(A, B: TMatrix): Boolean; inline;

    property Size: TIntVector2 read GetSize write SetSize;

    procedure Clear(Value: Single = 0);
    procedure LoadIdentity;

    procedure Swap(Col1, Col2: Byte);

    function ColSum(Col: Byte): Single;
    function RowSum(Row: Byte): Single;

    function SolveCol(Col: Byte; out R: Single): Boolean;
    function Solve(out R: array of Single): Boolean;
    function Solveable: Boolean; inline;

    function Transpose: TMatrix;
    property Minor[V: TIntVector2]: TMatrix read GetMinor write SetMinor;
    property MinorIJ[I, J: Integer]: TMatrix read GetMinor write SetMinor;
    property Cofactor[V: TIntVector2]: Single read GetCofactor;
    function CofactorMatrix: TMatrix;
    function Adjugate: TMatrix; inline;
    function Inverse: TMatrix; inline;
    function Determinant: Single;

    property Cols: Integer read GetCols write SetCols;
    property Rows: Integer read GetRows write SetRows;

    /// <returns>An exact copy of the Matrix.</returns>
    /// <remarks><see cref="Pengine.Matrix|TMatrix"/> is not a value type, use this instead.</remarks>
    function Copy: TMatrix;

    function ToString: string;

  end;

  // TODO: XmlDoc
  TMatrix2 = record
  public
    // public to allow constant definitions
    Data: array [0 .. 1, 0 .. 1] of Single;

  private
    function GetNum(I, J: Byte): Single; overload; inline;
    procedure SetNum(I, J: Byte; const Value: Single); overload; inline;
    function GetNum(V: TIntVector2): Single; overload;
    procedure SetNum(V: TIntVector2; const Value: Single); overload;

    function GetMinor(V: TIntVector2): Single; overload;
    function GetMinor(I, J: Integer): Single; overload; inline;
    procedure SetMinor(V: TIntVector2; const Value: Single); overload;
    procedure SetMinor(I, J: Integer; const Value: Single); overload; inline;

    function GetCofactor(V: TIntVector2): Single;

    property Num[I, J: Byte]: Single read GetNum write SetNum; default;
    {$HINTS OFF}
    property Num[V: TIntVector2]: Single read GetNum write SetNum; default;
    {$HINTS ON}
    
    procedure RangeCheck(V: TIntVector2); inline;

  public
    class operator Add(A: TMatrix2; B: Single): TMatrix2;
    class operator Add(A: Single; B: TMatrix2): TMatrix2;
    class operator Add(A, B: TMatrix2): TMatrix2;
    
    class operator Subtract(A: TMatrix2; B: Single): TMatrix2;
    class operator Subtract(A: Single; B: TMatrix2): TMatrix2;
    class operator Subtract(A, B: TMatrix2): TMatrix2;
    
    class operator Divide(A: TMatrix2; B: Single): TMatrix2;
    class operator Divide(A: Single; B: TMatrix2): TMatrix2;
    class operator Divide(A, B: TMatrix2): TMatrix2;  

    class operator Multiply(A: TMatrix2; B: Single): TMatrix2;
    class operator Multiply(A: Single; B: TMatrix2): TMatrix2;
    
    class operator Multiply(A, B: TMatrix2): TMatrix2;
    
    class operator Equal(A, B: TMatrix2): Boolean;
    class operator NotEqual(A, B: TMatrix2): Boolean; inline;

    class operator Implicit(A: TMatrix): TMatrix2; 
    class operator Implicit(A: TMatrix2): TMatrix; 
    
    procedure Clear(Value: Single = 0);
    procedure LoadIdentity; inline;

    function Transpose: TMatrix2;
    property Minor[V: TIntVector2]: Single read GetMinor write SetMinor;
    property MinorIJ[I, J: Integer]: Single read GetMinor write SetMinor;
    property Cofactor[V: TIntVector2]: Single read GetCofactor;
    function CofactorMatrix: TMatrix2;
    function Adjugate: TMatrix2; inline;
    function Inverse: TMatrix2; inline;
    function Determinant: Single;
                
    function ToString: string; inline;

  end;

  // TODO: XmlDoc
  TMatrix3 = record
  public
    // public to allow constant definitions
    Data: array [0 .. 2, 0 .. 2] of Single;

  private
    function GetNum(I, J: Byte): Single; overload; inline;
    procedure SetNum(I, J: Byte; const Value: Single); overload;
    function GetNum(V: TIntVector2): Single; overload;
    procedure SetNum(V: TIntVector2; const Value: Single); overload;

    function GetMinor(V: TIntVector2): TMatrix2; overload;
    function GetMinor(I, J: Integer): TMatrix2; overload;
    procedure SetMinor(V: TIntVector2; const Value: TMatrix2); overload;
    procedure SetMinor(I, J: Integer; const Value: TMatrix2); overload;

    function GetCofactor(V: TIntVector2): Single;

    property Num[I, J: Byte]: Single read GetNum write SetNum; default;
    {$HINTS OFF}
    property Num[V: TIntVector2]: Single read GetNum write SetNum; default;
    {$HINTS ON}
    
    procedure RangeCheck(V: TIntVector2); inline;

  public
    class operator Add(A: TMatrix3; B: Single): TMatrix3;
    class operator Add(A: Single; B: TMatrix3): TMatrix3;
    class operator Add(A, B: TMatrix3): TMatrix3;
    
    class operator Subtract(A: TMatrix3; B: Single): TMatrix3;
    class operator Subtract(A: Single; B: TMatrix3): TMatrix3;
    class operator Subtract(A, B: TMatrix3): TMatrix3;
    
    class operator Divide(A: TMatrix3; B: Single): TMatrix3;
    class operator Divide(A: Single; B: TMatrix3): TMatrix3;
    class operator Divide(A, B: TMatrix3): TMatrix3;  

    class operator Multiply(A: TMatrix3; B: Single): TMatrix3;
    class operator Multiply(A: Single; B: TMatrix3): TMatrix3;
    
    class operator Multiply(A, B: TMatrix3): TMatrix3;

    class operator Equal(A, B: TMatrix3): Boolean;
    class operator NotEqual(A, B: TMatrix3): Boolean;
                          
    class operator Implicit(A: TMatrix): TMatrix3; 
    class operator Implicit(A: TMatrix3): TMatrix; 
    
    procedure Clear(Value: Single = 0);
    procedure LoadIdentity;

    function Transpose: TMatrix3;
    property Minor[V: TIntVector2]: TMatrix2 read GetMinor write SetMinor;
    property MinorIJ[I, J: Integer]: TMatrix2 read GetMinor write SetMinor;
    property Cofactor[V: TIntVector2]: Single read GetCofactor;
    function CofactorMatrix: TMatrix3;
    function Adjugate: TMatrix3; inline;
    function Inverse: TMatrix3; inline;
    function Determinant: Single;
                 
    function ToString: string;

  end;

  // TODO: XmlDoc
  TMatrix4 = record
  public
    // public to allow constant definitions
    Data: array [0 .. 3, 0 .. 3] of Single;

  private
    function GetNum(I, J: Byte): Single; overload; inline;
    procedure SetNum(I, J: Byte; const Value: Single); overload;
    function GetNum(V: TIntVector2): Single; overload;
    procedure SetNum(V: TIntVector2; const Value: Single); overload;

    function GetMinor(V: TIntVector2): TMatrix3; overload;
    function GetMinor(I, J: Integer): TMatrix3; overload;
    procedure SetMinor(V: TIntVector2; const Value: TMatrix3); overload;
    procedure SetMinor(I, J: Integer; const Value: TMatrix3); overload;

    function GetCofactor(V: TIntVector2): Single;

    property Num[I, J: Byte]: Single read GetNum write SetNum; default;
    {$HINTS OFF}
    property Num[V: TIntVector2]: Single read GetNum write SetNum; default;
    {$HINTS ON}
    
    procedure RangeCheck(V: TIntVector2); inline;

  public
    class operator Add(A: TMatrix4; B: Single): TMatrix4;
    class operator Add(A: Single; B: TMatrix4): TMatrix4;
    class operator Add(A, B: TMatrix4): TMatrix4;
    
    class operator Subtract(A: TMatrix4; B: Single): TMatrix4;
    class operator Subtract(A: Single; B: TMatrix4): TMatrix4;
    class operator Subtract(A, B: TMatrix4): TMatrix4;
    
    class operator Divide(A: TMatrix4; B: Single): TMatrix4;
    class operator Divide(A: Single; B: TMatrix4): TMatrix4;
    class operator Divide(A, B: TMatrix4): TMatrix4; 

    class operator Multiply(A: TMatrix4; B: Single): TMatrix4;
    class operator Multiply(A: Single; B: TMatrix4): TMatrix4;
    
    class operator Multiply(A, B: TMatrix4): TMatrix4;
    
    class operator Equal(A, B: TMatrix4): Boolean;
    class operator NotEqual(A, B: TMatrix4): Boolean; inline;
                         
    class operator Implicit(A: TMatrix): TMatrix4; 
    class operator Implicit(A: TMatrix4): TMatrix; 
    
    procedure Clear(Value: Single = 0);
    procedure LoadIdentity;

    function Transpose: TMatrix4;
    property Minor[V: TIntVector2]: TMatrix3 read GetMinor write SetMinor;
    property MinorIJ[I, J: Integer]: TMatrix3 read GetMinor write SetMinor;
    property Cofactor[V: TIntVector2]: Single read GetCofactor;
    function CofactorMatrix: TMatrix4;
    function Adjugate: TMatrix4; inline;
    function Inverse: TMatrix4; inline;
    function Determinant: Single;
             
    function ToString: string;

  end;

  PMatrix = ^TMatrix;
  PMatrix2 = ^TMatrix2;
  PMatrix3 = ^TMatrix3;
  PMatrix4 = ^TMatrix4;

var

  // Only use with atomic mathmatical operations to avoid conflicts
  M3x2: TMatrix;
  M4x3: TMatrix;

const

  Matrix4Identity: TMatrix4 = (Data: (
    (1, 0, 0, 0),
    (0, 1, 0, 0),
    (0, 0, 1, 0),
    (0, 0, 0, 1)
    ));

  Matrix3Identity: TMatrix3 = (Data: (
    (1, 0, 0),
    (0, 1, 0),
    (0, 0, 1)
    ));

  Matrix2Identity: TMatrix2 = (Data: (
    (1, 0),
    (0, 1)
    ));

implementation

procedure FillDWORD(var Destination; Count: Integer; Value: Cardinal);

{$IFDEF X86}

asm
  push edi
  mov  edi, eax
  mov  eax, ecx
  mov  ecx, edx
  rep  stosd
  pop edi
end;

{$ELSE}

type
  Cardinals = array [0 .. 0] of Cardinal;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Cardinals(Destination)[I] := Value;
end;

{$ENDIF}

{ EMatrixMultiply }

constructor EMatrixMultiply.Create;
begin
  inherited Create('Columns of first matrix not equal to rows of second matrix.');
end;

{ EMatrixDimension }

constructor EMatrixDimension.Create;
begin
  inherited Create('Matrix dimensions are not correct.');
end;

{ EMatrixRangeError }

constructor EMatrixRangeError.Create;
begin
  inherited Create('Matrix index out of range.');
end;
                 
{ TMatrix }

function TMatrix.GetCols: Integer;
begin
  Result := Length(Data);
end;

function TMatrix.GetRows: Integer;
begin
  Result := Length(Data[0]);
end;

procedure TMatrix.SetCols(const Value: Integer);
begin
  SetLength(Data, Value, Rows);
end;

procedure TMatrix.SetRows(const Value: Integer);
begin                      
  SetLength(Data, Cols, Value);
end;

function TMatrix.GetSize: TIntVector2;
begin
  Result.Create(Cols, Rows);
end;

procedure TMatrix.SetSize(const Value: TIntVector2);
begin
  SetLength(Data, Value.X, Value.Y);
end;

function TMatrix.GetNum(I, J: Byte): Single;
begin
  Result := Num[IVec2(I, J)];
end;

procedure TMatrix.SetNum(I, J: Byte; const Value: Single);
begin
  Num[IVec2(I, J)] := Value;
end;

function TMatrix.GetNum(V: TIntVector2): Single;
begin
  RangeCheck(V);
  Result := Data[V.X, V.Y];
end;

procedure TMatrix.SetNum(V: TIntVector2; const Value: Single);
begin
  RangeCheck(V);
  Data[V.X, V.Y] := Value;
end;

function TMatrix.GetMinor(V: TIntVector2): TMatrix;
var
  X1, Y1, X2, Y2: Integer;
begin
  RangeCheck(V);
  Result.Size := Size - 1;
  X2 := 0;
  for X1 := 0 to Rows - 1 do
  begin
    if X1 = V.X then
      Continue;
    Y2 := 0;
    for Y1 := 0 to Cols - 1 do
    begin
      if Y1 = V.Y then
        Continue;
      Result[X2, Y2] := Data[X1, Y1];
      Inc(Y2);
    end;
    Inc(X2);
  end;
end;

function TMatrix.GetMinor(I, J: Integer): TMatrix;
begin
  Result := Minor[IVec2(I, J)];
end;

procedure TMatrix.SetMinor(V: TIntVector2; const Value: TMatrix);
var
  X1, Y1, X2, Y2: Integer;
begin
  RangeCheck(V);
  if Size - 1 <> Value.Size then
    raise EMatrixDimension.Create;
  X2 := 0;
  for X1 := 0 to Rows - 1 do
  begin
    if X1 = V.X then
      Continue;
    Y2 := 0;
    for Y1 := 0 to Cols - 1 do
    begin
      if Y1 = V.Y then
        Continue;
      Data[X2, Y2] := Value[X1, Y1];
      Inc(Y2);
    end;
    Inc(X2);
  end;
end;

procedure TMatrix.SetMinor(I, J: Integer; const Value: TMatrix);
begin
  Minor[IVec2(I, J)] := Value;
end;

function TMatrix.GetCofactor(V: TIntVector2): Single;
begin
  RangeCheck(V);
  if (V.X + V.Y) mod 2 = 0 then
    Result := +Minor[V].Determinant
  else
    Result := -Minor[V].Determinant;
end;

procedure TMatrix.RangeCheck(V: TIntVector2);
begin
  if not (V in Size) then
    raise EMatrixRangeError.Create;
end;

class procedure TMatrix.DimensionCheck(const A, B: TMatrix);
begin
  if A.Size <> B.Size then
    raise EMatrixDimension.Create;
end;

class operator TMatrix.Add(A, B: TMatrix): TMatrix;
var
  V: TIntVector2;
begin
  DimensionCheck(A, B);
  Result.Size := A.Size;
  for V in A.Size do
    Result[V] := A[V] + B[V];
end;

class operator TMatrix.Add(A: TMatrix; B: Single): TMatrix;
var
  V: TIntVector2;
begin
  Result.Size := A.Size;
  for V in A.Size do
    Result[V] := A[V] + B;
end;

class operator TMatrix.Add(A: Single; B: TMatrix): TMatrix;
var
  V: TIntVector2;
begin
  Result.Size := A.Size;
  for V in B.Size do
    Result[V] := A + B[V];
end;

class operator TMatrix.Subtract(A, B: TMatrix): TMatrix;
var
  V: TIntVector2;
begin
  DimensionCheck(A, B);
  Result.Size := A.Size;
  for V in A.Size do
    Result[V] := A[V] - B[V];
end;

class operator TMatrix.Subtract(A: TMatrix; B: Single): TMatrix;
var
  V: TIntVector2;
begin
  Result.Size := A.Size;
  for V in A.Size do
    Result[V] := A[V] - B;
end;

class operator TMatrix.Subtract(A: Single; B: TMatrix): TMatrix;
var
  V: TIntVector2;
begin
  Result.Size := A.Size;
  for V in B.Size do
    Result[V] := A - B[V];
end;

class operator TMatrix.Multiply(A, B: TMatrix): TMatrix;
var
  V: TIntVector2;
  I: Integer;
begin
  DimensionCheck(A, B);
  Result.Size := IVec2(B.Cols, A.Rows);
  for V in Result.Size do
    for I := 0 to A.Cols - 1 do
      Result[V.Y, V.X] := Result[V.Y, V.X] + A[I, V.X] * B[V.Y, I];
end;

class operator TMatrix.Multiply(A: TMatrix; B: Single): TMatrix;
var
  V: TIntVector2;
begin
  Result.Size := A.Size;
  for V in A.Size do
    Result[V] := A[V] * B;
end;

class operator TMatrix.Multiply(A: Single; B: TMatrix): TMatrix;
var
  V: TIntVector2;
begin
  Result.Size := A.Size;
  for V in B.Size do
    Result[V] := A * B[V];
end;

class operator TMatrix.Divide(A, B: TMatrix): TMatrix;
begin
  Result := A * B.Inverse;  
end;

class operator TMatrix.Divide(A: TMatrix; B: Single): TMatrix;
var
  V: TIntVector2;
begin
  Result.Size := A.Size;
  for V in A.Size do
    Result[V] := A[V] / B;
end;

class operator TMatrix.Divide(A: Single; B: TMatrix): TMatrix;
var
  V: TIntVector2;
begin
  Result.Size := A.Size;
  for V in B.Size do
    Result[V] := A / B[V];
end;

class operator TMatrix.Equal(A, B: TMatrix): Boolean;
var
  I: Integer;
begin
  DimensionCheck(A, B);
  for I := 0 to A.Cols - 1 do
    if not CompareMem(@A.Data[I, 0], @B.Data[I, 0], SizeOf(Single) * A.Rows) then
      Exit(False);
  Result := True;
end;

class operator TMatrix.NotEqual(A, B: TMatrix): Boolean;
begin
  Result := not (A = B);
end;

procedure TMatrix.Clear(Value: Single = 0);
var
  I: Integer;
begin
  for I := 0 to Cols - 1 do
    FillDWORD(Data[I, 0], Rows, PCardinal(@Value)^)
end;

procedure TMatrix.LoadIdentity;
var
  I: Integer;
begin
  Clear;
  for I := 0 to Rows - 1 do
    Num[I, I] := 1;
end;

procedure TMatrix.Swap(Col1, Col2: Byte);
var
  P: Pointer;
begin
  if Col1 = Col2 then
    Exit;
  P := Pointer(Data[Col1]);
  Pointer(Data[Col1]) := Pointer(Data[Col2]);
  Pointer(Data[Col2]) := P;
end;

function TMatrix.ColSum(Col: Byte): Single;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Cols do
    Result := Result + Num[Col, I];
end;

function TMatrix.RowSum(Row: Byte): Single;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Cols do
    Result := Result + Num[I, Row];
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

function TMatrix.Transpose: TMatrix;
var
  V: TIntVector2;
begin
  Result.Size := Size.YX;
  for V in Size do
    Result[V.YX] := Num[V];
end;

function TMatrix.CofactorMatrix: TMatrix;
var
  V: TIntVector2;
begin
  Result.Size := Size;
  for V in Size do
    Result[V] := Cofactor[V];
end;

function TMatrix.Adjugate: TMatrix;
begin
  Result := CofactorMatrix.Transpose;
end;

function TMatrix.Inverse: TMatrix;
begin
  Result := Adjugate / Determinant;
end;

function TMatrix.Determinant: Single;
var
  S, R: Byte;
begin
  S := Min(Cols, Rows);
  // 3x3/2x2(/1x1) direkt berechnen (schneller)
  if S = 3 then
    Exit(
      Num[0, 0] * Num[1, 1] * Num[2, 2] +
      Num[0, 1] * Num[1, 2] * Num[2, 0] +
      Num[0, 2] * Num[1, 0] * Num[2, 1] -
      Num[2, 0] * Num[1, 1] * Num[0, 2] -
      Num[2, 1] * Num[1, 2] * Num[0, 0] -
      Num[2, 2] * Num[1, 0] * Num[0, 1])
  else if S = 2 then
    Exit(
      Num[0, 0] * Num[1, 1] -
      Num[1, 0] * Num[0, 1])
  else if S = 1 then
    Exit(Num[0, 0]);

  Result := 0;
  // rekursive Determinanten berechnung bis 3x3
  for R := 0 to S - 1 do
    if R mod 2 = 0 then
      Result := Result + Num[R, 0] * MinorIJ[R, 0].Determinant
    else
      Result := Result - Num[R, 0] * MinorIJ[R, 0].Determinant
end;

function TMatrix.Copy: TMatrix;
var
  I: Integer;
begin
  Result.Size := Size;
  for I := 0 to Cols - 1 do
    Move(Data[I, 0], Result.Data[I, 0], SizeOf(Single) * Rows);
end;

function TMatrix.ToString: string;
var
  I, J: Integer;
begin
  if (Cols = 0) or (Rows = 0) then
    Exit('');
  for J := 0 to Rows - 1 do
  begin
    Result := Result + '' + Format('%f', [Self[0, J]]);  
    for I := 1 to Cols - 1 do
    begin
      Result := Result + ' ' + Format('%f', [Self[I, J]]);
    end;
    Result := Result + '';
    if J <> Rows - 1 then
      Result := Result + sLineBreak;       
  end;
end;

{ TMatrix2 }

function TMatrix2.GetNum(I, J: Byte): Single;
begin
  Result := Num[IVec2(I, J)];
end;

procedure TMatrix2.SetNum(I, J: Byte; const Value: Single);
begin
  Num[IVec2(I, J)] := Value;
end;

function TMatrix2.GetNum(V: TIntVector2): Single;
begin
  RangeCheck(V);
  Result := Data[V.X, V.Y];
end;

procedure TMatrix2.SetNum(V: TIntVector2; const Value: Single);
begin
  RangeCheck(V);
  Data[V.X, V.Y] := Value;
end;

function TMatrix2.GetMinor(V: TIntVector2): Single;
begin
  RangeCheck(V);
  Result := Num[1 - V];
end;

function TMatrix2.GetMinor(I, J: Integer): Single;
begin
  Result := Minor[IVec2(I, J)];
end;

procedure TMatrix2.SetMinor(V: TIntVector2; const Value: Single);
begin
  RangeCheck(V);
  Num[1 - V] := Value;
end;

procedure TMatrix2.SetMinor(I, J: Integer; const Value: Single);
begin
  Minor[IVec2(I, J)] := Value;
end;

function TMatrix2.GetCofactor(V: TIntVector2): Single;
begin
  if (V.X + V.Y) mod 2 = 0 then
    Result := +Minor[V]
  else
    Result := -Minor[V];
end;

procedure TMatrix2.RangeCheck(V: TIntVector2);
begin
  if not (V in IBounds2(2)) then
    raise EMatrixRangeError.Create;
end;

class operator TMatrix2.Add(A: TMatrix2; B: Single): TMatrix2;
begin
  Result[0, 0] := A[0, 0] + B;
  Result[1, 0] := A[1, 0] + B;
  Result[0, 1] := A[0, 1] + B;
  Result[1, 1] := A[1, 1] + B;
end;

class operator TMatrix2.Add(A: Single; B: TMatrix2): TMatrix2;
begin
  Result[0, 0] := A + B[0, 0];
  Result[1, 0] := A + B[1, 0];
  Result[0, 1] := A + B[0, 1];
  Result[1, 1] := A + B[1, 1];
end;

class operator TMatrix2.Add(A, B: TMatrix2): TMatrix2;
begin
  Result[0, 0] := A[0, 0] + B[0, 0];
  Result[1, 0] := A[1, 0] + B[1, 0];
  Result[0, 1] := A[0, 1] + B[0, 1];
  Result[1, 1] := A[1, 1] + B[1, 1];
end;

class operator TMatrix2.Subtract(A: TMatrix2; B: Single): TMatrix2;
begin
  Result[0, 0] := A[0, 0] - B;
  Result[1, 0] := A[1, 0] - B;
  Result[0, 1] := A[0, 1] - B;
  Result[1, 1] := A[1, 1] - B;
end;

class operator TMatrix2.Subtract(A: Single; B: TMatrix2): TMatrix2;
begin
  Result[0, 0] := A - B[0, 0];
  Result[1, 0] := A - B[1, 0];
  Result[0, 1] := A - B[0, 1];
  Result[1, 1] := A - B[1, 1];
end;

class operator TMatrix2.Subtract(A, B: TMatrix2): TMatrix2;
begin
  Result[0, 0] := A[0, 0] - B[0, 0];
  Result[1, 0] := A[1, 0] - B[1, 0];
  Result[0, 1] := A[0, 1] - B[0, 1];
  Result[1, 1] := A[1, 1] - B[1, 1];
end;

class operator TMatrix2.Divide(A: TMatrix2; B: Single): TMatrix2;
begin
  Result[0, 0] := A[0, 0] / B;
  Result[1, 0] := A[1, 0] / B;
  Result[0, 1] := A[0, 1] / B;
  Result[1, 1] := A[1, 1] / B;
end;

class operator TMatrix2.Divide(A: Single; B: TMatrix2): TMatrix2;
begin
  Result[0, 0] := A / B[0, 0];
  Result[1, 0] := A / B[1, 0];
  Result[0, 1] := A / B[0, 1];
  Result[1, 1] := A / B[1, 1];
end;

class operator TMatrix2.Divide(A, B: TMatrix2): TMatrix2;
begin
  Result := A * B.Inverse;
end;

class operator TMatrix2.Multiply(A: TMatrix2; B: Single): TMatrix2;
begin
  Result[0, 0] := A[0, 0] * B;
  Result[1, 0] := A[1, 0] * B;
  Result[0, 1] := A[0, 1] * B;
  Result[1, 1] := A[1, 1] * B;
end;

class operator TMatrix2.Multiply(A: Single; B: TMatrix2): TMatrix2;
begin
  Result[0, 0] := A * B[0, 0];
  Result[1, 0] := A * B[1, 0];
  Result[0, 1] := A * B[0, 1];
  Result[1, 1] := A * B[1, 1];
end;

class operator TMatrix2.Multiply(A, B: TMatrix2): TMatrix2;
begin
  Result[0, 0] := A[0, 0] * B[0, 0] + A[1, 0] * B[0, 1];
  Result[1, 0] := A[0, 0] * B[1, 0] + A[1, 0] * B[1, 1];
  Result[0, 1] := A[0, 1] * B[0, 0] + A[1, 1] * B[0, 1];
  Result[1, 1] := A[0, 1] * B[1, 0] + A[1, 1] * B[1, 1];
end;

class operator TMatrix2.Equal(A, B: TMatrix2): Boolean;
begin
  Result := CompareMem(@A, @B, SizeOf(Single) * 2 * 2);
end;

class operator TMatrix2.NotEqual(A, B: TMatrix2): Boolean;
begin
  Result := not (A = B);
end;

class operator TMatrix2.Implicit(A: TMatrix): TMatrix2;
begin
  if A.Size <> 2 then
    raise EMatrixDimension.Create;
  Result[0, 0] := A[0, 0];
  Result[1, 0] := A[1, 0];
  Result[0, 1] := A[0, 1];
  Result[1, 1] := A[1, 1];
end;

class operator TMatrix2.Implicit(A: TMatrix2): TMatrix;
begin
  Result.Size := 2;
  Result[0, 0] := A[0, 0];
  Result[1, 0] := A[1, 0];
  Result[0, 1] := A[0, 1];
  Result[1, 1] := A[1, 1];  
end;

procedure TMatrix2.Clear(Value: Single);
begin
  Data[0, 0] := Value;
  Data[1, 0] := Value;
  Data[0, 1] := Value;
  Data[1, 1] := Value;
end;

procedure TMatrix2.LoadIdentity;
begin
  Self := Matrix2Identity;
end;

function TMatrix2.Transpose: TMatrix2;
begin
  Result[0, 0] := Data[0, 0];
  Result[1, 0] := Data[0, 1];
  Result[0, 1] := Data[1, 0];
  Result[1, 1] := Data[1, 1];
end;

function TMatrix2.CofactorMatrix: TMatrix2;
begin
  Result[0, 0] := Cofactor[IVec2(0, 0)];
  Result[1, 0] := Cofactor[IVec2(1, 0)];
  Result[0, 1] := Cofactor[IVec2(0, 1)];
  Result[1, 1] := Cofactor[IVec2(1, 1)];
end;

function TMatrix2.Adjugate: TMatrix2;
begin
  Result := CofactorMatrix.Transpose;
end;

function TMatrix2.Inverse: TMatrix2;
begin
  Result := Adjugate / Determinant;
end;

function TMatrix2.Determinant: Single;
begin
  Result := Num[0, 0] * Num[1, 1] - Num[1, 0] * Num[0, 1];
end;

function TMatrix2.ToString: string;
begin
  Result := TMatrix(Self).ToString;
end;

{ TMatrix3 }

function TMatrix3.GetNum(I, J: Byte): Single;
begin
  Result := Num[IVec2(I, J)];
end;

procedure TMatrix3.SetNum(I, J: Byte; const Value: Single);
begin
  Num[IVec2(I, J)] := Value;
end;

function TMatrix3.GetNum(V: TIntVector2): Single;
begin
  RangeCheck(V);
  Result := Data[V.X, V.Y];
end;

procedure TMatrix3.SetNum(V: TIntVector2; const Value: Single);
begin
  RangeCheck(V);
  Data[V.X, V.Y] := Value;
end;

function TMatrix3.GetMinor(V: TIntVector2): TMatrix2;
var
  X1, Y1, X2, Y2: Integer;
begin
  X2 := 0;
  for X1 := 0 to 2 do
  begin
    if X1 = V.X then
      Continue;
    Y2 := 0;
    for Y1 := 0 to 2 do
    begin
      if Y1 = V.Y then
        Continue;
      Result[X2, Y2] := Num[X1, Y1];
      Inc(Y2);
    end;
    Inc(X2);
  end;
end;

function TMatrix3.GetMinor(I, J: Integer): TMatrix2;
begin
  Result := Minor[IVec2(I, J)];
end;

procedure TMatrix3.SetMinor(V: TIntVector2; const Value: TMatrix2);
var
  X1, Y1, X2, Y2: Integer;
begin
  X2 := 0;
  for X1 := 0 to 2 do
  begin
    if X1 = V.X then
      Continue;
    Y2 := 0;
    for Y1 := 0 to 2 do
    begin
      if Y1 = V.Y then
        Continue;
      Num[X2, Y2] := Value[X1, Y1];
      Inc(Y2);
    end;
    Inc(X2);
  end;
end;

procedure TMatrix3.SetMinor(I, J: Integer; const Value: TMatrix2);
begin
  Minor[IVec2(I, J)] := Value;
end;

function TMatrix3.GetCofactor(V: TIntVector2): Single;
begin
  if (V.X + V.Y) mod 2 = 0 then
    Result := +Minor[V].Determinant
  else
    Result := -Minor[V].Determinant;
end;

procedure TMatrix3.RangeCheck(V: TIntVector2);
begin
  if not (V in IBounds2(3)) then
    raise EMatrixRangeError.Create;
end;

class operator TMatrix3.Add(A: TMatrix3; B: Single): TMatrix3;
begin
  Result[0, 0] := A[0, 0] + B;
  Result[1, 0] := A[1, 0] + B;
  Result[2, 0] := A[2, 0] + B;
  Result[0, 1] := A[0, 1] + B;
  Result[1, 1] := A[1, 1] + B;
  Result[2, 1] := A[2, 1] + B;
  Result[0, 2] := A[0, 2] + B;
  Result[1, 2] := A[1, 2] + B;
  Result[2, 2] := A[2, 2] + B;
end;

class operator TMatrix3.Add(A: Single; B: TMatrix3): TMatrix3;
begin
  Result[0, 0] := A + B[0, 0];
  Result[1, 0] := A + B[1, 0];
  Result[2, 0] := A + B[2, 0];
  Result[0, 1] := A + B[0, 1];
  Result[1, 1] := A + B[1, 1];
  Result[2, 1] := A + B[2, 1];
  Result[0, 2] := A + B[0, 2];
  Result[1, 2] := A + B[1, 2];
  Result[2, 2] := A + B[2, 2];
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

class operator TMatrix3.Subtract(A: TMatrix3; B: Single): TMatrix3;
begin
  Result[0, 0] := A[0, 0] - B;
  Result[1, 0] := A[1, 0] - B;
  Result[2, 0] := A[2, 0] - B;
  Result[0, 1] := A[0, 1] - B;
  Result[1, 1] := A[1, 1] - B;
  Result[2, 1] := A[2, 1] - B;
  Result[0, 2] := A[0, 2] - B;
  Result[1, 2] := A[1, 2] - B;
  Result[2, 2] := A[2, 2] - B;
end;

class operator TMatrix3.Subtract(A: Single; B: TMatrix3): TMatrix3;
begin
  Result[0, 0] := A - B[0, 0];
  Result[1, 0] := A - B[1, 0];
  Result[2, 0] := A - B[2, 0];
  Result[0, 1] := A - B[0, 1];
  Result[1, 1] := A - B[1, 1];
  Result[2, 1] := A - B[2, 1];
  Result[0, 2] := A - B[0, 2];
  Result[1, 2] := A - B[1, 2];
  Result[2, 2] := A - B[2, 2];
end;

class operator TMatrix3.Subtract(A, B: TMatrix3): TMatrix3;
begin
  Result[0, 0] := A[0, 0] - B[0, 0];
  Result[1, 0] := A[1, 0] - B[1, 0];
  Result[2, 0] := A[2, 0] - B[2, 0];
  Result[0, 1] := A[0, 1] - B[0, 1];
  Result[1, 1] := A[1, 1] - B[1, 1];
  Result[2, 1] := A[2, 1] - B[2, 1];
  Result[0, 2] := A[0, 2] - B[0, 2];
  Result[1, 2] := A[1, 2] - B[1, 2];
  Result[2, 2] := A[2, 2] - B[2, 2];
end;

class operator TMatrix3.Divide(A: TMatrix3; B: Single): TMatrix3;
begin
  Result[0, 0] := A[0, 0] / B;
  Result[1, 0] := A[1, 0] / B;
  Result[2, 0] := A[2, 0] / B;
  Result[0, 1] := A[0, 1] / B;
  Result[1, 1] := A[1, 1] / B;
  Result[2, 1] := A[2, 1] / B;
  Result[0, 2] := A[0, 2] / B;
  Result[1, 2] := A[1, 2] / B;
  Result[2, 2] := A[2, 2] / B;
end;

class operator TMatrix3.Divide(A: Single; B: TMatrix3): TMatrix3;
begin
  Result[0, 0] := A / B[0, 0];
  Result[1, 0] := A / B[1, 0];
  Result[2, 0] := A / B[2, 0];
  Result[0, 1] := A / B[0, 1];
  Result[1, 1] := A / B[1, 1];
  Result[2, 1] := A / B[2, 1];
  Result[0, 2] := A / B[0, 2];
  Result[1, 2] := A / B[1, 2];
  Result[2, 2] := A / B[2, 2];
end;

class operator TMatrix3.Divide(A, B: TMatrix3): TMatrix3;
begin
  Result := A * B.Inverse;
end;

class operator TMatrix3.Multiply(A: TMatrix3; B: Single): TMatrix3;
begin
  Result[0, 0] := A[0, 0] * B;
  Result[1, 0] := A[1, 0] * B;
  Result[2, 0] := A[2, 0] * B;
  Result[0, 1] := A[0, 1] * B;
  Result[1, 1] := A[1, 1] * B;
  Result[2, 1] := A[2, 1] * B;
  Result[0, 2] := A[0, 2] * B;
  Result[1, 2] := A[1, 2] * B;
  Result[2, 2] := A[2, 2] * B;
end;

class operator TMatrix3.Multiply(A: Single; B: TMatrix3): TMatrix3;
begin
  Result[0, 0] := A * B[0, 0];
  Result[1, 0] := A * B[1, 0];
  Result[2, 0] := A * B[2, 0];
  Result[0, 1] := A * B[0, 1];
  Result[1, 1] := A * B[1, 1];
  Result[2, 1] := A * B[2, 1];
  Result[0, 2] := A * B[0, 2];
  Result[1, 2] := A * B[1, 2];
  Result[2, 2] := A * B[2, 2];
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

class operator TMatrix3.Equal(A, B: TMatrix3): Boolean;
begin
  Result := CompareMem(@A, @B, SizeOf(Single) * 3 * 3);
end;

class operator TMatrix3.NotEqual(A, B: TMatrix3): Boolean;
begin
  Result := not (A = B);
end;

class operator TMatrix3.Implicit(A: TMatrix): TMatrix3;
begin
  if A.Size <> 3 then
    raise EMatrixDimension.Create;
  Result[0, 0] := A[0, 0];
  Result[1, 0] := A[1, 0];
  Result[2, 0] := A[2, 0];
  Result[0, 1] := A[0, 1];
  Result[1, 1] := A[1, 1];
  Result[2, 1] := A[2, 1];
  Result[0, 2] := A[0, 2];
  Result[1, 2] := A[1, 2];
  Result[2, 2] := A[2, 2];   
end;

class operator TMatrix3.Implicit(A: TMatrix3): TMatrix;
begin
  Result.Size := 3;
  Result[0, 0] := A[0, 0];
  Result[1, 0] := A[1, 0];
  Result[2, 0] := A[2, 0];
  Result[0, 1] := A[0, 1];
  Result[1, 1] := A[1, 1];
  Result[2, 1] := A[2, 1];
  Result[0, 2] := A[0, 2];
  Result[1, 2] := A[1, 2];
  Result[2, 2] := A[2, 2];
end;

procedure TMatrix3.Clear(Value: Single);
begin
  Self[0, 0] := Value;
  Self[1, 0] := Value;
  Self[2, 0] := Value;
  Self[0, 1] := Value;
  Self[1, 1] := Value;
  Self[2, 1] := Value;
  Self[0, 2] := Value;
  Self[1, 2] := Value;
  Self[2, 2] := Value;
end;

procedure TMatrix3.LoadIdentity;
begin
  Self := Matrix3Identity;
end;

function TMatrix3.Transpose: TMatrix3;
begin
  Result[0, 0] := Data[0, 0];
  Result[1, 0] := Data[0, 1];
  Result[2, 0] := Data[0, 2];
  Result[0, 1] := Data[1, 0];
  Result[1, 1] := Data[1, 1];
  Result[2, 1] := Data[1, 2];
  Result[0, 2] := Data[2, 0];
  Result[1, 2] := Data[2, 1];
  Result[2, 2] := Data[2, 2];
end;

function TMatrix3.CofactorMatrix: TMatrix3;
begin
  Result[0, 0] := Cofactor[IVec2(0, 0)];
  Result[1, 0] := Cofactor[IVec2(1, 0)];
  Result[2, 0] := Cofactor[IVec2(2, 0)];
  Result[0, 1] := Cofactor[IVec2(0, 1)];
  Result[1, 1] := Cofactor[IVec2(1, 1)];
  Result[2, 1] := Cofactor[IVec2(2, 1)];
  Result[0, 2] := Cofactor[IVec2(0, 2)];
  Result[1, 2] := Cofactor[IVec2(1, 2)];
  Result[2, 2] := Cofactor[IVec2(2, 2)];
end;

function TMatrix3.Adjugate: TMatrix3;
begin
  Result := CofactorMatrix.Transpose;
end;

function TMatrix3.Inverse: TMatrix3;
begin
  Result := Adjugate / Determinant;
end;

function TMatrix3.Determinant: Single;
begin
  Result :=
    Num[0, 0] * Num[1, 1] * Num[2, 2] +
    Num[0, 1] * Num[1, 2] * Num[2, 0] +
    Num[0, 2] * Num[1, 0] * Num[2, 1] -
    Num[2, 0] * Num[1, 1] * Num[0, 2] -
    Num[2, 1] * Num[1, 2] * Num[0, 0] -
    Num[2, 2] * Num[1, 0] * Num[0, 1];
end;

function TMatrix3.ToString: string;
begin                              
  Result := TMatrix(Self).ToString;
end;

{ TMatrix4 }

function TMatrix4.GetNum(I, J: Byte): Single;
begin
  Result := Num[IVec2(I, J)];
end;

procedure TMatrix4.SetNum(I, J: Byte; const Value: Single);
begin
  Num[IVec2(I, J)] := Value;
end;

function TMatrix4.GetNum(V: TIntVector2): Single;
begin
  RangeCheck(V);
  Result := Data[V.X, V.Y];
end;

procedure TMatrix4.SetNum(V: TIntVector2; const Value: Single);
begin
  RangeCheck(V);
  Data[V.X, V.Y] := Value;
end;

function TMatrix4.GetMinor(V: TIntVector2): TMatrix3;
var
  X1, Y1, X2, Y2: Integer;
begin
  X2 := 0;
  for X1 := 0 to 3 do
  begin
    if X1 = V.X then
      Continue;
    Y2 := 0;
    for Y1 := 0 to 3 do
    begin
      if Y1 = V.Y then
        Continue;
      Result[X2, Y2] := Num[X1, Y1];
      Inc(Y2);
    end;
    Inc(X2);
  end;
end;

function TMatrix4.GetMinor(I, J: Integer): TMatrix3;
begin
  Result := Minor[IVec2(I, J)];
end;

procedure TMatrix4.SetMinor(V: TIntVector2; const Value: TMatrix3);
var
  X1, Y1, X2, Y2: Integer;
begin
  X2 := 0;
  for X1 := 0 to 3 do
  begin
    if X1 = V.X then
      Continue;
    Y2 := 0;
    for Y1 := 0 to 3 do
    begin
      if Y1 = V.Y then
        Continue;
      Num[X2, Y2] := Value[X1, Y1];
      Inc(Y2);
    end;
    Inc(X2);
  end;
end;

procedure TMatrix4.SetMinor(I, J: Integer; const Value: TMatrix3);
begin
  Minor[IVec2(I, J)] := Value;
end;

function TMatrix4.GetCofactor(V: TIntVector2): Single;
begin
  if (V.X + V.Y) mod 2 = 0 then
    Result := +Minor[V].Determinant
  else
    Result := -Minor[V].Determinant;
end;

procedure TMatrix4.RangeCheck(V: TIntVector2);
begin
  if not (V in IBounds2(4)) then
    raise EMatrixRangeError.Create;
end;

class operator TMatrix4.Add(A: TMatrix4; B: Single): TMatrix4;
begin
  Result[0, 0] := A[0, 0] + B;
  Result[1, 0] := A[1, 0] + B;
  Result[2, 0] := A[2, 0] + B;
  Result[3, 0] := A[3, 0] + B;
  Result[0, 1] := A[0, 1] + B;
  Result[1, 1] := A[1, 1] + B;
  Result[2, 1] := A[2, 1] + B;
  Result[3, 1] := A[3, 1] + B;
  Result[0, 2] := A[0, 2] + B;
  Result[1, 2] := A[1, 2] + B;
  Result[2, 2] := A[2, 2] + B;
  Result[3, 2] := A[3, 2] + B;
  Result[0, 3] := A[0, 3] + B;
  Result[1, 3] := A[1, 3] + B;
  Result[2, 3] := A[2, 3] + B;
  Result[3, 3] := A[3, 3] + B;
end;

class operator TMatrix4.Add(A: Single; B: TMatrix4): TMatrix4;
begin
  Result[0, 0] := A + B[0, 0];
  Result[1, 0] := A + B[1, 0];
  Result[2, 0] := A + B[2, 0];
  Result[3, 0] := A + B[3, 0];
  Result[0, 1] := A + B[0, 1];
  Result[1, 1] := A + B[1, 1];
  Result[2, 1] := A + B[2, 1];
  Result[3, 1] := A + B[3, 1];
  Result[0, 2] := A + B[0, 2];
  Result[1, 2] := A + B[1, 2];
  Result[2, 2] := A + B[2, 2];
  Result[3, 2] := A + B[3, 2];
  Result[0, 3] := A + B[0, 3];
  Result[1, 3] := A + B[1, 3];
  Result[2, 3] := A + B[2, 3];
  Result[3, 3] := A + B[3, 3];
end;

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

class operator TMatrix4.Subtract(A: TMatrix4; B: Single): TMatrix4;
begin
  Result[0, 0] := A[0, 0] - B;
  Result[1, 0] := A[1, 0] - B;
  Result[2, 0] := A[2, 0] - B;
  Result[3, 0] := A[3, 0] - B;
  Result[0, 1] := A[0, 1] - B;
  Result[1, 1] := A[1, 1] - B;
  Result[2, 1] := A[2, 1] - B;
  Result[3, 1] := A[3, 1] - B;
  Result[0, 2] := A[0, 2] - B;
  Result[1, 2] := A[1, 2] - B;
  Result[2, 2] := A[2, 2] - B;
  Result[3, 2] := A[3, 2] - B;
  Result[0, 3] := A[0, 3] - B;
  Result[1, 3] := A[1, 3] - B;
  Result[2, 3] := A[2, 3] - B;
  Result[3, 3] := A[3, 3] - B;
end;

class operator TMatrix4.Subtract(A: Single; B: TMatrix4): TMatrix4;
begin
  Result[0, 0] := A - B[0, 0];
  Result[1, 0] := A - B[1, 0];
  Result[2, 0] := A - B[2, 0];
  Result[3, 0] := A - B[3, 0];
  Result[0, 1] := A - B[0, 1];
  Result[1, 1] := A - B[1, 1];
  Result[2, 1] := A - B[2, 1];
  Result[3, 1] := A - B[3, 1];
  Result[0, 2] := A - B[0, 2];
  Result[1, 2] := A - B[1, 2];
  Result[2, 2] := A - B[2, 2];
  Result[3, 2] := A - B[3, 2];
  Result[0, 3] := A - B[0, 3];
  Result[1, 3] := A - B[1, 3];
  Result[2, 3] := A - B[2, 3];
  Result[3, 3] := A - B[3, 3];
end;

class operator TMatrix4.Subtract(A, B: TMatrix4): TMatrix4;
begin
  Result[0, 0] := A[0, 0] - B[0, 0];
  Result[1, 0] := A[1, 0] - B[1, 0];
  Result[2, 0] := A[2, 0] - B[2, 0];
  Result[3, 0] := A[3, 0] - B[3, 0];
  Result[0, 1] := A[0, 1] - B[0, 1];
  Result[1, 1] := A[1, 1] - B[1, 1];
  Result[2, 1] := A[2, 1] - B[2, 1];
  Result[3, 1] := A[3, 1] - B[3, 1];
  Result[0, 2] := A[0, 2] - B[0, 2];
  Result[1, 2] := A[1, 2] - B[1, 2];
  Result[2, 2] := A[2, 2] - B[2, 2];
  Result[3, 2] := A[3, 2] - B[3, 2];
  Result[0, 3] := A[0, 3] - B[0, 3];
  Result[1, 3] := A[1, 3] - B[1, 3];
  Result[2, 3] := A[2, 3] - B[2, 3];
  Result[3, 3] := A[3, 3] - B[3, 3];
end;

class operator TMatrix4.Divide(A: TMatrix4; B: Single): TMatrix4;
begin
  Result[0, 0] := A[0, 0] / B;
  Result[1, 0] := A[1, 0] / B;
  Result[2, 0] := A[2, 0] / B;
  Result[3, 0] := A[3, 0] / B;
  Result[0, 1] := A[0, 1] / B;
  Result[1, 1] := A[1, 1] / B;
  Result[2, 1] := A[2, 1] / B;
  Result[3, 1] := A[3, 1] / B;
  Result[0, 2] := A[0, 2] / B;
  Result[1, 2] := A[1, 2] / B;
  Result[2, 2] := A[2, 2] / B;
  Result[3, 2] := A[3, 2] / B;
  Result[0, 3] := A[0, 3] / B;
  Result[1, 3] := A[1, 3] / B;
  Result[2, 3] := A[2, 3] / B;
  Result[3, 3] := A[3, 3] / B;
end;

class operator TMatrix4.Divide(A: Single; B: TMatrix4): TMatrix4;
begin
  Result[0, 0] := A / B[0, 0];
  Result[1, 0] := A / B[1, 0];
  Result[2, 0] := A / B[2, 0];
  Result[3, 0] := A / B[3, 0];
  Result[0, 1] := A / B[0, 1];
  Result[1, 1] := A / B[1, 1];
  Result[2, 1] := A / B[2, 1];
  Result[3, 1] := A / B[3, 1];
  Result[0, 2] := A / B[0, 2];
  Result[1, 2] := A / B[1, 2];
  Result[2, 2] := A / B[2, 2];
  Result[3, 2] := A / B[3, 2];
  Result[0, 3] := A / B[0, 3];
  Result[1, 3] := A / B[1, 3];
  Result[2, 3] := A / B[2, 3];
  Result[3, 3] := A / B[3, 3];
end;

class operator TMatrix4.Divide(A, B: TMatrix4): TMatrix4;
begin
  Result := A * B.Inverse;
end;

class operator TMatrix4.Multiply(A: TMatrix4; B: Single): TMatrix4;
begin
  Result[0, 0] := A[0, 0] * B;
  Result[1, 0] := A[1, 0] * B;
  Result[2, 0] := A[2, 0] * B;
  Result[3, 0] := A[3, 0] * B;
  Result[0, 1] := A[0, 1] * B;
  Result[1, 1] := A[1, 1] * B;
  Result[2, 1] := A[2, 1] * B;
  Result[3, 1] := A[3, 1] * B;
  Result[0, 2] := A[0, 2] * B;
  Result[1, 2] := A[1, 2] * B;
  Result[2, 2] := A[2, 2] * B;
  Result[3, 2] := A[3, 2] * B;
  Result[0, 3] := A[0, 3] * B;
  Result[1, 3] := A[1, 3] * B;
  Result[2, 3] := A[2, 3] * B;
  Result[3, 3] := A[3, 3] * B;
end;

class operator TMatrix4.Multiply(A: Single; B: TMatrix4): TMatrix4;
begin
  Result[0, 0] := A * B[0, 0];
  Result[1, 0] := A * B[1, 0];
  Result[2, 0] := A * B[2, 0];
  Result[3, 0] := A * B[3, 0];
  Result[0, 1] := A * B[0, 1];
  Result[1, 1] := A * B[1, 1];
  Result[2, 1] := A * B[2, 1];
  Result[3, 1] := A * B[3, 1];
  Result[0, 2] := A * B[0, 2];
  Result[1, 2] := A * B[1, 2];
  Result[2, 2] := A * B[2, 2];
  Result[3, 2] := A * B[3, 2];
  Result[0, 3] := A * B[0, 3];
  Result[1, 3] := A * B[1, 3];
  Result[2, 3] := A * B[2, 3];
  Result[3, 3] := A * B[3, 3];
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
  Result := CompareMem(@A, @B, SizeOf(Single) * 4 * 4);
end;

class operator TMatrix4.NotEqual(A, B: TMatrix4): Boolean;
begin
  Result := not (A = B);
end;

class operator TMatrix4.Implicit(A: TMatrix): TMatrix4;
begin
  if A.Size <> 4 then
    raise EMatrixDimension.Create;
  Result[0, 0] := A[0, 0];
  Result[1, 0] := A[1, 0];
  Result[2, 0] := A[2, 0];
  Result[3, 0] := A[3, 0];
  Result[0, 1] := A[0, 1];
  Result[1, 1] := A[1, 1];
  Result[2, 1] := A[2, 1];
  Result[3, 1] := A[3, 1];
  Result[0, 2] := A[0, 2];
  Result[1, 2] := A[1, 2];
  Result[2, 2] := A[2, 2];
  Result[3, 2] := A[3, 2];
  Result[0, 3] := A[0, 3];
  Result[1, 3] := A[1, 3];
  Result[2, 3] := A[2, 3];
  Result[3, 3] := A[3, 3];
end;

class operator TMatrix4.Implicit(A: TMatrix4): TMatrix;
begin
  Result.Size := 4;
  Result[0, 0] := A[0, 0];
  Result[1, 0] := A[1, 0];
  Result[2, 0] := A[2, 0];
  Result[3, 0] := A[3, 0];
  Result[0, 1] := A[0, 1];
  Result[1, 1] := A[1, 1];
  Result[2, 1] := A[2, 1];
  Result[3, 1] := A[3, 1];
  Result[0, 2] := A[0, 2];
  Result[1, 2] := A[1, 2];
  Result[2, 2] := A[2, 2];
  Result[3, 2] := A[3, 2];
  Result[0, 3] := A[0, 3];
  Result[1, 3] := A[1, 3];
  Result[2, 3] := A[2, 3];
  Result[3, 3] := A[3, 3];
end;

procedure TMatrix4.Clear(Value: Single);
begin
  Data[0, 0] := Value;
  Data[1, 0] := Value;
  Data[2, 0] := Value;
  Data[3, 0] := Value;
  Data[0, 1] := Value;
  Data[1, 1] := Value;
  Data[2, 1] := Value;
  Data[3, 1] := Value;
  Data[0, 2] := Value;
  Data[1, 2] := Value;
  Data[2, 2] := Value;
  Data[3, 2] := Value;
  Data[0, 3] := Value;
  Data[1, 3] := Value;
  Data[2, 3] := Value;
  Data[3, 3] := Value;
end;

procedure TMatrix4.LoadIdentity;
begin
  Self := Matrix4Identity;
end;

function TMatrix4.Transpose: TMatrix4;
begin
  Result[0, 0] := Data[0, 0];
  Result[1, 0] := Data[0, 1];
  Result[2, 0] := Data[0, 2];
  Result[3, 0] := Data[0, 3];
  Result[0, 1] := Data[1, 0];
  Result[1, 1] := Data[1, 1];
  Result[2, 1] := Data[1, 2];
  Result[3, 1] := Data[1, 3];
  Result[0, 2] := Data[2, 0];
  Result[1, 2] := Data[2, 1];
  Result[2, 2] := Data[2, 2];
  Result[3, 2] := Data[2, 3];
  Result[0, 3] := Data[3, 0];
  Result[1, 3] := Data[3, 1];
  Result[2, 3] := Data[3, 2];
  Result[3, 3] := Data[3, 3];
end;

function TMatrix4.CofactorMatrix: TMatrix4;
begin
  Result[0, 0] := Cofactor[IVec2(0, 0)];
  Result[1, 0] := Cofactor[IVec2(1, 0)];
  Result[2, 0] := Cofactor[IVec2(2, 0)];
  Result[3, 0] := Cofactor[IVec2(3, 0)];
  Result[0, 1] := Cofactor[IVec2(0, 1)];
  Result[1, 1] := Cofactor[IVec2(1, 1)];
  Result[2, 1] := Cofactor[IVec2(2, 1)];
  Result[3, 1] := Cofactor[IVec2(3, 1)];
  Result[0, 2] := Cofactor[IVec2(0, 2)];
  Result[1, 2] := Cofactor[IVec2(1, 2)];
  Result[2, 2] := Cofactor[IVec2(2, 2)];
  Result[3, 2] := Cofactor[IVec2(3, 2)];
  Result[0, 3] := Cofactor[IVec2(0, 3)];
  Result[1, 3] := Cofactor[IVec2(1, 3)];
  Result[2, 3] := Cofactor[IVec2(2, 3)];
  Result[3, 3] := Cofactor[IVec2(3, 3)];
end;

function TMatrix4.Adjugate: TMatrix4;
begin
  Result := CofactorMatrix.Transpose;
end;

function TMatrix4.Inverse: TMatrix4;
begin
  Result := Adjugate / Determinant;
end;

function TMatrix4.Determinant: Single;
var
  R, X, Y: Byte;
  Pos: Boolean;
  T: TMatrix3;
begin
  Result := 0;
  Pos := True;
  for R := 0 to 3 do
  begin
    for X := 0 to 2 do
      for Y := 0 to 2 do
        T[X, Y] := Num[X + 1, Y + Ord(Y >= R)]; // Y >= R -> [+1] else -> [+0]

    if Num[0, R] <> 0 then
      Result := Result + Num[0, R] * T.Determinant * (Ord(Pos) * 2 - 1);
    Pos := not Pos;
  end;
end;

function TMatrix4.ToString: string;
begin                              
  Result := TMatrix(Self).ToString;
end;

initialization

M3x2.Size := IVec2(3, 2);
M3x2.Clear;
M4x3.Size := IVec2(4, 3);
M4x3.Clear;

end.
