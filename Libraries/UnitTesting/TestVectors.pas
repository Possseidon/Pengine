unit TestVectors;

interface

uses
  TestFramework,

  System.Math,
  System.SysUtils,
  System.Classes,

  Pengine.Vector,
  Pengine.IntMaths,
  Pengine.Matrix,
  Pengine.UnitTesting;

type

  TTestVector2 = class(TTestCase)
  private
    A: TVector2;
    M: TMatrix2;

    BackupFormatSettings: TFormatSettings;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  public
    constructor Create(MethodName: string); override;

  published
    procedure TestCreate;
    procedure TestCreateSingle;

    procedure TestRandom;
    procedure TestRandomBox;
    procedure TestRandomNormal;

    procedure TestImplicitSingle;
    procedure TestImplicitIntVector;

    procedure TestAdd;
    procedure TestSubtract;
    procedure TestMultiply;
    procedure TestDivide;

    procedure TestMultiplyMatrixVector;
    procedure TestMultiplyVectorMatrix;

    procedure TestPositive;
    procedure TestNegative;

    procedure TestEqual;
    procedure TestNotEqual;
    procedure TestLessThan;
    procedure TestLessThanOrEqual;
    procedure TestGreaterThan;
    procedure TestGreaterThanOrEqual;

    procedure TestToString;
    procedure TestImplicitString;

    procedure TestLength;
    procedure TestNormalize;

    procedure TestDistanceTo;
    procedure TestVectorTo;

    procedure TestCross;
    procedure TestDot;
    procedure TestSqrDot;

    procedure TestCosAngleTo;
    procedure TestAngleRadTo;
    procedure TestAngleTo;
    procedure TestRotateRad;
    procedure TestRotate;

    procedure TestAbs;
    procedure TestFloor;
    procedure TestCeil;

    procedure TestDirs;

    procedure TestVec2;
    procedure TestVec2Single;

  end;
  {
  TTestVector3 = class(TTestCase)
  private
    A: TVector3;
    M3: TMatrix3;
    M4: TMatrix4;

    BackupFormatSettings: TFormatSettings;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  public
    constructor Create(MethodName: string); override;

  published
    procedure TestCreate;
    procedure TestCreateSingle;

    procedure TestRandom;
    procedure TestRandomBox;
    procedure TestRandomNormal;

    procedure TestImplicitSingle;
    procedure TestImplicitIntVector;

    procedure TestAdd;
    procedure TestSubtract;
    procedure TestMultiply;
    procedure TestDivide;

    procedure TestMultiplyMatrixVector;
    procedure TestMultiplyVectorMatrix;
    procedure TestMultiplyMatrixVector4;
    procedure TestMultiplyVectorMatrix4;

    procedure TestPositive;
    procedure TestNegative;

    procedure TestEqual;
    procedure TestNotEqual;
    procedure TestLessThan;
    procedure TestLessThanOrEqual;
    procedure TestGreaterThan;
    procedure TestGreaterThanOrEqual;

    procedure TestToString;
    procedure TestImplicitString;

    procedure TestLength;
    procedure TestNormalize;

    procedure TestDistanceTo;
    procedure TestVectorTo;

    procedure TestCross;
    procedure TestDot;
    procedure TestSqrDot;

    procedure TestCosAngleTo;
    procedure TestAngleRadTo;
    procedure TestAngleTo;
    procedure TestRotateRad;
    procedure TestRotate;

    procedure TestReflect;

    procedure TestAbs;
    procedure TestFloor;
    procedure TestCeil;

    procedure TestDirs;

    procedure TestVec3;
    procedure TestVec3Single;

  end;
  }
implementation

{ TTestVector2 }

constructor TTestVector2.Create(MethodName: string);
begin
  inherited;
  M[0, 0] := 1;
  M[0, 1] := 2;
  M[1, 0] := 3;
  M[1, 1] := 4;
end;

procedure TTestVector2.SetUp;
begin
  BackupFormatSettings := FormatSettings;
  FormatSettings := TFormatSettings.Invariant;
end;

procedure TTestVector2.TearDown;
begin
  FormatSettings := BackupFormatSettings;
end;

procedure TTestVector2.TestCreate;
begin
  A := TVector2.Create(1, 2);
  CheckEquals(1, A.X);
  CheckEquals(2, A.Y);
end;

procedure TTestVector2.TestCreateSingle;
begin
  A := TVector2.Create(1, 1);
  CheckEquals(1, A.X);
  CheckEquals(1, A.Y);
end;

procedure TTestVector2.TestRandom;
begin
  A := TVector2.Random;
  Check(A.X >= 0);
  Check(A.X < 1);
  Check(A.Y >= 0);
  Check(A.Y < 1);
end;

procedure TTestVector2.TestRandomBox;
begin
  A := TVector2.RandomBox;
  Check(A.X >= -1);
  Check(A.X < 1);
  Check(A.Y >= -1);
  Check(A.Y < 1);
end;

procedure TTestVector2.TestRandomNormal;
begin
  A := TVector2.RandomNormal;
  CheckSameValueS(1, A.Length);
end;

procedure TTestVector2.TestImplicitSingle;
begin
  A := 1;
  CheckEquals(1, A.X);
  CheckEquals(1, A.Y);
end;

procedure TTestVector2.TestImplicitIntVector;
begin
  A := TIntVector2.Create(1, 2);
  CheckEquals(1, A.X);
  CheckEquals(2, A.Y);
end;

procedure TTestVector2.TestAdd;
begin
  A := Vec2(1, 2) + Vec2(3, 4);
  CheckEquals(1 + 3, A.X);
  CheckEquals(2 + 4, A.Y);
end;

procedure TTestVector2.TestSubtract;
begin
  A := Vec2(1, 2) - Vec2(3, 4);
  CheckEquals(1 - 3, A.X);
  CheckEquals(2 - 4, A.Y);
end;

procedure TTestVector2.TestMultiply;
begin
  A := Vec2(1, 2) * Vec2(3, 4);
  CheckEquals(1 * 3, A.X);
  CheckEquals(2 * 4, A.Y);
end;

procedure TTestVector2.TestDivide;
begin
  A := Vec2(1, 2) / Vec2(3, 4);
  CheckSameValueS(1 / 3, A.X);
  CheckSameValueS(2 / 4, A.Y);
end;

procedure TTestVector2.TestMultiplyMatrixVector;
begin
  A := M * Vec2(1, 2);
  CheckEquals(M[0, 0] * 1 + M[1, 0] * 2, A.X);
  CheckEquals(M[0, 1] * 1 + M[1, 1] * 2, A.Y);
end;

procedure TTestVector2.TestMultiplyVectorMatrix;
begin
  A := Vec2(1, 2) * M;
  CheckEquals(M[0, 0] * 1 + M[0, 1] * 2, A.X);
  CheckEquals(M[1, 0] * 1 + M[1, 1] * 2, A.Y);
end;

procedure TTestVector2.TestPositive;
begin
  A := +Vec2(1, 2);
  CheckEquals(1, A.X);
  CheckEquals(2, A.Y);
end;

procedure TTestVector2.TestNegative;
begin
  A := -Vec2(1, 2);
  CheckEquals(-1, A.X);
  CheckEquals(-2, A.Y);
end;

procedure TTestVector2.TestEqual;
begin
  CheckTrue(Vec2(1, 1) = Vec2(1, 1));
  CheckFalse(Vec2(1, 1) = Vec2(1, 2));
  CheckFalse(Vec2(1, 1) = Vec2(2, 1));
  CheckFalse(Vec2(1, 1) = Vec2(2, 2));
end;

procedure TTestVector2.TestNotEqual;
begin
  CheckFalse(Vec2(1, 1) <> Vec2(1, 1));
  CheckTrue(Vec2(1, 1) <> Vec2(1, 2));
  CheckTrue(Vec2(1, 1) <> Vec2(2, 1));
  CheckTrue(Vec2(1, 1) <> Vec2(2, 2));
end;

procedure TTestVector2.TestLessThan;
begin
  CheckFalse(Vec2(1, 1) < Vec2(1, 1));
  CheckFalse(Vec2(1, 1) < Vec2(1, 2));
  CheckFalse(Vec2(1, 1) < Vec2(2, 1));
  CheckTrue(Vec2(1, 1) < Vec2(2, 2));
end;

procedure TTestVector2.TestLessThanOrEqual;
begin
  CheckFalse(Vec2(2, 2) <= Vec2(1, 1));
  CheckFalse(Vec2(2, 2) <= Vec2(1, 2));
  CheckFalse(Vec2(2, 2) <= Vec2(2, 1));
  CheckTrue(Vec2(2, 2) <= Vec2(2, 2));
end;

procedure TTestVector2.TestGreaterThan;
begin
  CheckTrue(Vec2(2, 2) > Vec2(1, 1));
  CheckFalse(Vec2(2, 2) > Vec2(1, 2));
  CheckFalse(Vec2(2, 2) > Vec2(2, 1));
  CheckFalse(Vec2(2, 2) > Vec2(2, 2));
end;

procedure TTestVector2.TestGreaterThanOrEqual;
begin
  CheckTrue(Vec2(1, 1) >= Vec2(1, 1));
  CheckFalse(Vec2(1, 1) >= Vec2(1, 2));
  CheckFalse(Vec2(1, 1) >= Vec2(2, 1));
  CheckFalse(Vec2(1, 1) >= Vec2(2, 2));
end;

procedure TTestVector2.TestToString;
begin
  CheckEquals('[1.00|2.00]', Vec2(1, 2).ToString);
end;

procedure TTestVector2.TestImplicitString;
begin
  CheckEquals('[1.00|2.00]', Vec2(1, 2));
end;

procedure TTestVector2.TestLength;
begin
  CheckSameValueS(5, Vec2(3, 4).Length);
end;

procedure TTestVector2.TestNormalize;
begin
  A := Vec2(1, 2).Normalize;
  CheckSameValueS(1, A.Length);
end;

procedure TTestVector2.TestDistanceTo;
begin
  CheckSameValueS(Sqrt(2) * 2, Vec2(1, 2).DistanceTo(Vec2(3, 4)));
end;

procedure TTestVector2.TestVectorTo;
begin
  A := Vec2(1, 2).VectorTo(Vec2(3, 4));
  CheckEquals(2, A.X);
  CheckEquals(2, A.Y);
end;

procedure TTestVector2.TestCross;
begin
  A := Vec2(1, 2).Cross;
  CheckEquals(-2, A.X);
  CheckEquals(1, A.Y);
end;

procedure TTestVector2.TestDot;
begin
  CheckEquals(1 * 3 + 2 * 4, Vec2(1, 2).Dot(Vec2(3, 4)));
end;

procedure TTestVector2.TestSqrDot;
begin
  CheckEquals(1 * 1 + 2 * 2, Vec2(1, 2).SqrDot);
end;

procedure TTestVector2.TestCosAngleTo;
begin
  CheckSameValueS(Cos(Pi / 2), Vec2(0, 1).CosAngleTo(Vec2(1, 0)));
end;

procedure TTestVector2.TestAngleRadTo;
begin
  CheckSameValueS(Pi / 2, Vec2(0, 1).AngleRadTo(Vec2(1, 0)));
end;

procedure TTestVector2.TestAngleTo;
begin
  CheckSameValueS(90, Vec2(0, 1).AngleTo(Vec2(1, 0)));
end;

procedure TTestVector2.TestRotateRad;
begin
  A := Vec2(1, 0).RotateRad(Pi / 2);
  CheckSameValueS(0, A.X);
  CheckSameValueS(1, A.Y);
end;

procedure TTestVector2.TestRotate;
begin
  A := Vec2(1, 0).Rotate(90);
  CheckSameValueS(0, A.X);
  CheckSameValueS(1, A.Y);
end;

procedure TTestVector2.TestAbs;
begin
  A := Vec2(1, -2).Abs;
  CheckEquals(1, A.X);
  CheckEquals(2, A.Y);
  A := Vec2(-1, 2).Abs;
  CheckEquals(1, A.X);
  CheckEquals(2, A.Y);
end;

procedure TTestVector2.TestFloor;
begin
  A := Vec2(1.5, 2.5).Floor;
  CheckEquals(1, A.X);
  CheckEquals(2, A.Y);
end;

procedure TTestVector2.TestCeil;
begin
  A := Vec2(0.5, 1.5).Ceil;
  CheckEquals(1, A.X);
  CheckEquals(2, A.Y);
end;

procedure TTestVector2.TestDirs;
var
  D: TBasicDirs2;
begin
  D := Vec2(0, 0).Dirs;
  Check(D = []);
  D := Vec2(1, -1).Dirs;
  Check(D = [bdRight, bdDown]);
  D := Vec2(-1, 1).Dirs;
  Check(D = [bdLeft, bdUp]);
end;

procedure TTestVector2.TestVec2;
begin
  A := Vec2(1, 2);
  CheckEquals(1, A.X);
  CheckEquals(2, A.Y);
end;

procedure TTestVector2.TestVec2Single;
begin
  A := Vec2(1, 1);
  CheckEquals(1, A.X);
  CheckEquals(1, A.Y);
end;

{ TTestVector3 }
                  {
procedure TTestVector3.SetUp;
begin
  BackupFormatSettings := FormatSettings;
  FormatSettings := TFormatSettings.Invariant;
end;

procedure TTestVector3.TearDown;
begin
  FormatSettings := BackupFormatSettings;
end;


constructor TTestVector3.Create(MethodName: string);
var
  V: TIntVector2;
  I: Integer;
begin
  inherited;
  I := 1;
  for V in IBounds2(3, 3) do
  begin
    M3[V] := I;
    Inc(I);
  end;
  I := 1;
  for V in IBounds2(4, 4) do
  begin
    M3[V] := I;
    Inc(I);
  end;
end;

procedure TTestVector3.TestCreate;
begin
  A := TVector3.Create(1, 2, 3);
  CheckEquals(1, A.X);
  CheckEquals(2, A.Z);
  CheckEquals(3, A.Z);
end;

procedure TTestVector3.TestCreateSingle;
begin
  A := TVector3.Create(1, 1, 1);
  CheckEquals(1, A.X);
  CheckEquals(1, A.Y);
  CheckEquals(1, A.Z);
end;

procedure TTestVector3.TestRandom;
begin
  A := TVector3.Random;
  Check(A.X >= 0);
  Check(A.X < 1);
  Check(A.Y >= 0);
  Check(A.Y < 1);
  Check(A.Z >= 0);
  Check(A.Z < 1);
end;

procedure TTestVector3.TestRandomBox;
begin
  A := TVector3.RandomBox;
  Check(A.X >= -1);
  Check(A.X < 1);
  Check(A.Y >= -1);
  Check(A.Y < 1);
  Check(A.Z >= -1);
  Check(A.Z < 1);
end;

procedure TTestVector3.TestRandomNormal;
begin
  A := TVector3.RandomNormal;
  CheckSameValueS(1, A.Length);
end;

procedure TTestVector3.TestImplicitSingle;
begin
  A := 1;
  CheckEquals(1, A.X);
  CheckEquals(1, A.Y);
  CheckEquals(1, A.Z);
end;

procedure TTestVector3.TestImplicitIntVector;
begin
  A := TIntVector3.Create(1, 2, 3);
  CheckEquals(1, A.X);
  CheckEquals(2, A.Y);
  CheckEquals(3, A.Z);
end;

procedure TTestVector3.TestAdd;
begin
  A := Vec3(1, 2, 3) + Vec3(4, 5, 6);
  CheckEquals(1 + 4, A.X);
  CheckEquals(2 + 5, A.Z);
  CheckEquals(3 + 6, A.Z);
end;

procedure TTestVector3.TestSubtract;
begin
  A := Vec3(1, 2, 3) - Vec3(4, 5, 6);
  CheckEquals(1 - 4, A.X);
  CheckEquals(2 - 5, A.Z);
  CheckEquals(3 - 6, A.Z);
end;

procedure TTestVector3.TestMultiply;
begin
  A := Vec3(1, 2, 3) * Vec3(4, 5, 6);
  CheckEquals(1 * 4, A.X);
  CheckEquals(2 * 5, A.Z);
  CheckEquals(3 * 6, A.Z);
end;

procedure TTestVector3.TestDivide;
begin
  A := Vec3(1, 2, 3) / Vec3(4, 5, 6);
  CheckEquals(1 / 4, A.X);
  CheckEquals(2 / 5, A.Z);
  CheckEquals(3 / 6, A.Z);
end;

procedure TTestVector3.TestMultiplyMatrixVector;
begin
  A := M3 * Vec3(1, 2, 3);
  // TODO: Make UnitTests with for loops
  CheckEquals(M3[0, 0] * 1 + M3[1, 0] * 2 + M3[2, 0] * 3, A.X);
  CheckEquals(M3[0, 1] * 1 + M3[1, 1] * 2 + M3[2, 1] * 3, A.Y);
  CheckEquals(M3[0, 2] * 1 + M3[1, 2] * 2 + M3[2, 2] * 3, A.Z);
end;

procedure TTestVector3.TestMultiplyVectorMatrix;
begin
  A := Vec3(1, 2, 3) * M3;
  CheckEquals(M3[0, 0] * 1 + M3[0, 1] * 2 + M3[0, 2] * 3, A.X);
  CheckEquals(M3[1, 0] * 1 + M3[1, 1] * 2 + M3[1, 2] * 3, A.Y);
  CheckEquals(M3[2, 0] * 1 + M3[2, 1] * 2 + M3[2, 2] * 3, A.Z);
end;

procedure TTestVector3.TestMultiplyMatrixVector4;
begin

end;

procedure TTestVector3.TestMultiplyVectorMatrix4;
begin

end;

procedure TTestVector3.TestPositive;
begin
  A := +Vec3(1, 2, 3);
  CheckEquals(1, A.X);
  CheckEquals(2, A.Y);
  CheckEquals(3, A.Z);
end;
                  
procedure TTestVector3.TestNegative;
begin
  A := -Vec3(1, 3);
  CheckEquals(-1, A.X);
  CheckEquals(-3, A.Z);    
end;

procedure TTestVector3.TestEqual;
begin
  CheckTrue(Vec3(1, 1) = Vec3(1, 1));
  CheckFalse(Vec3(1, 1) = Vec3(1, 3));
  CheckFalse(Vec3(1, 1) = Vec3(3, 1));
  CheckFalse(Vec3(1, 1) = Vec3(3, 3));
end;

procedure TTestVector3.TestNotEqual;
begin
  CheckFalse(Vec3(1, 1) <> Vec3(1, 1));
  CheckTrue(Vec3(1, 1) <> Vec3(1, 3));
  CheckTrue(Vec3(1, 1) <> Vec3(3, 1));
  CheckTrue(Vec3(1, 1) <> Vec3(3, 3));
end;

procedure TTestVector3.TestLessThan;
begin
  CheckFalse(Vec3(1, 1) < Vec3(1, 1));
  CheckFalse(Vec3(1, 1) < Vec3(1, 3));
  CheckFalse(Vec3(1, 1) < Vec3(3, 1));
  CheckTrue(Vec3(1, 1) < Vec3(3, 3));
end;

procedure TTestVector3.TestLessThanOrEqual;
begin
  CheckFalse(Vec3(3, 3) <= Vec3(1, 1));
  CheckFalse(Vec3(3, 3) <= Vec3(1, 3));
  CheckFalse(Vec3(3, 3) <= Vec3(3, 1));
  CheckTrue(Vec3(3, 3) <= Vec3(3, 3));
end;

procedure TTestVector3.TestGreaterThan;
begin
  CheckTrue(Vec3(3, 3) > Vec3(1, 1));
  CheckFalse(Vec3(3, 3) > Vec3(1, 3));
  CheckFalse(Vec3(3, 3) > Vec3(3, 1));
  CheckFalse(Vec3(3, 3) > Vec3(3, 3));
end;

procedure TTestVector3.TestGreaterThanOrEqual;
begin
  CheckTrue(Vec3(1, 1) >= Vec3(1, 1));
  CheckFalse(Vec3(1, 1) >= Vec3(1, 3));
  CheckFalse(Vec3(1, 1) >= Vec3(3, 1));
  CheckFalse(Vec3(1, 1) >= Vec3(3, 3));
end;

procedure TTestVector3.TestToString;
begin
  CheckEquals('[1.00|3.00]', Vec3(1, 3).ToString);
end;

procedure TTestVector3.TestImplicitString;
begin
  CheckEquals('[1.00|2.00|3.00]', Vec3(1, 2, 3));
end;

procedure TTestVector3.TestLength;
begin
  CheckSameValueS(5, Vec3(3, 4).Length);
end;

procedure TTestVector3.TestNormalize;
begin
  A := Vec3(1, 3).Normalize;
  CheckSameValueS(1, A.Length);
end;

procedure TTestVector3.TestDistanceTo;
begin
  CheckSameValueS(Sqrt(3) * 3, Vec3(1, 3).DistanceTo(Vec3(3, 4)));
end;

procedure TTestVector3.TestVectorTo;
begin
  A := Vec3(1, 3).VectorTo(Vec3(3, 4));
  CheckEquals(3, A.X);
  CheckEquals(3, A.Z);
end;

procedure TTestVector3.TestCross;
begin
  A := Vec3(1, 3).Cross;
  CheckEquals(-3, A.X);
  CheckEquals(1, A.Z);
end;

procedure TTestVector3.TestDot;
begin
  CheckEquals(1 * 3 + 3 * 4, Vec3(1, 3).Dot(Vec3(3, 4)));
end;

procedure TTestVector3.TestSqrDot;
begin
  CheckEquals(1 * 1 + 3 * 3, Vec3(1, 3).SqrDot);
end;

procedure TTestVector3.TestCosAngleTo;
begin
  CheckSameValueS(Cos(Pi / 3), Vec3(0, 1).CosAngleTo(Vec3(1, 0)));
end;

procedure TTestVector3.TestAngleRadTo;
begin
  CheckSameValueS(Pi / 3, Vec3(0, 1).AngleRadTo(Vec3(1, 0)));
end;

procedure TTestVector3.TestAngleTo;
begin
  CheckSameValueS(90, Vec3(0, 1).AngleTo(Vec3(1, 0)));
end;

procedure TTestVector3.TestRotateRad;
begin
  A := Vec3(1, 0).RotateRad(Pi / 3);
  CheckSameValueS(0, A.X);
  CheckSameValueS(1, A.Z);
end;

procedure TTestVector3.TestRotate;
begin
  A := Vec3(1, 0).Rotate(90);
  CheckSameValueS(0, A.X);
  CheckSameValueS(1, A.Z);
end;

procedure TTestVector3.TestReflect;
begin

end;

procedure TTestVector3.TestAbs;
begin
  A := Vec3(1, -3).Abs;
  CheckEquals(1, A.X);
  CheckEquals(3, A.Z);
  A := Vec3(-1, 3).Abs;
  CheckEquals(1, A.X);
  CheckEquals(3, A.Z);
end;

procedure TTestVector3.TestFloor;
begin
  A := Vec3(1.5, 3.5).Floor;
  CheckEquals(1, A.X);
  CheckEquals(3, A.Z);
end;

procedure TTestVector3.TestCeil;
begin
  A := Vec3(0.5, 1.5).Ceil;
  CheckEquals(1, A.X);
  CheckEquals(3, A.Z);
end;

procedure TTestVector3.TestDirs;
var
  D: TBasicDirs3;
begin
  D := Vec3(0, 0).Dirs;
  Check(D = []);
  D := Vec3(1, -1).Dirs;
  Check(D = [bdRight, bdDown]);
  D := Vec3(-1, 1).Dirs;
  Check(D = [bdLeft, bdUp]);
end;

procedure TTestVector3.TestVec3;
begin
  A := Vec3(1, 3);
  CheckEquals(1, A.X);
  CheckEquals(3, A.Z);
end;

procedure TTestVector3.TestVec3Single;
begin
  A := Vec3(1, 1);
  CheckEquals(1, A.X);
  CheckEquals(1, A.Z);
end;
 }
initialization

RegisterTest(TTestVector2.Suite);
// RegisterTest(TTestVector3.Suite);

end.
