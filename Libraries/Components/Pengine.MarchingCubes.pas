unit Pengine.MarchingCubes;

interface

uses
  Pengine.IntMaths,
  Pengine.Vector,
  Pengine.Utility,

  // REMOVE:
  Pengine.TimeManager;

type

  TMarchingCubes = class
  public type

    TPlanePoint = record
      Position: TVector3;
      Direction: TVector3;

      constructor Create(APosition, ADirection: TVector3);

    end;

    TPlaneInfo = record
      Inverted: Boolean;
      Points: array [0 .. 2] of TPlanePoint;

    end;

  private
    class var
      FLookup: array [Byte] of TArray<TPlaneInfo>;

    class function PlanePoint(APosition, ADirection: TVector3): TPlanePoint;
    class function PlaneInfo(AInverted: Boolean; A, B, C: TPlanePoint): TPlaneInfo;

  public
    class constructor Create;

    class function GetTriangles(ACorners: TCorners3; AOffset: Single = 0.5): TArray<TPlane3>;

  end;

implementation

{ TMarchingCubes }

class constructor TMarchingCubes.Create;
var
  I: Byte;
  Inverted: Boolean;

  procedure SwapPos(var A, B: TIntVector3);
  var
    Tmp: TIntVector3;
  begin
    Tmp := A;
    A := B;
    B := Tmp;
  end;

  function SplitConnected(ACorners: TCorners3): TArray<TArray<TCorner3>>;
  var
    Outer, Inner: TCorner3;
    ConnectedIndex, CornerIndex: Integer;
    I: Integer;
  begin
    SetLength(Result, 0);
    for Outer := Low(TCorner3) to High(TCorner3) do
    begin
      if ((Byte(ACorners) shr Byte(Outer)) and 1) = 0 then
        Continue;
      Exclude(ACorners, Outer);
      ConnectedIndex := Length(Result);
      SetLength(Result, ConnectedIndex + 1);
      Result[ConnectedIndex] := [Outer];
      I := 0;
      while I < Length(Result[ConnectedIndex]) do
      begin
        for Inner := Low(TCorner3) to High(TCorner3) do
        begin
          if ((Byte(ACorners) shr Byte(Inner)) and 1) = 0 then
            Continue;
          if (Result[ConnectedIndex][I] = Inner) or not CornersConnected(Result[ConnectedIndex][I], Inner) then
            Continue;
          Exclude(ACorners, Inner);
          CornerIndex := Length(Result[ConnectedIndex]);
          SetLength(Result[ConnectedIndex], CornerIndex + 1);
          Result[ConnectedIndex][CornerIndex] := Inner;
        end;
        Inc(I);
      end;
    end;
  end;

  procedure AppendLookup(AData: array of TPlaneInfo);
  var
    L, J: Integer;
  begin
    L := Length(FLookup[I]);
    SetLength(FLookup[I], L + Length(AData));
    for J := 0 to Length(AData) - 1 do
      FLookup[I][L + J] := AData[J];
  end;

  procedure Case1(A: TCorner3);
  var
    Pos: TIntVector3;
  begin
    Pos := Corner3Pos[A];
    AppendLookup([
      PlaneInfo(Inverted,
      PlanePoint(Pos, -Vec3Dir[CornerDirectionArrays[A, 0]]),
      PlanePoint(Pos, -Vec3Dir[CornerDirectionArrays[A, 1]]),
      PlanePoint(Pos, -Vec3Dir[CornerDirectionArrays[A, 2]]))
      ]);
  end;

  procedure Case2(A, B: TCorner3);
  var
    PosA, PosB, Normal, DirAB, DirA, DirB: TIntVector3;
    Flip: Boolean;
  begin
    PosA := Corner3Pos[A];
    PosB := Corner3Pos[B];
    Normal := PosA + PosB - 1;
    DirAB := PosB - PosA;
    Flip := (PosA = 0) or (PosB = 1);
    DirA := -DirAB.Cross(not Flip) * Normal;
    DirB := -DirAB.Cross(Flip) * Normal;
    AppendLookup([
      PlaneInfo(Inverted,
      PlanePoint(PosA, DirA),
      PlanePoint(PosA, DirB),
      PlanePoint(PosB, DirB)),
      PlaneInfo(Inverted,
      PlanePoint(PosB, DirB),
      PlanePoint(PosB, DirA),
      PlanePoint(PosA, DirA))
      ]);
  end;

  procedure Case3(A, B, C: TCorner3);
  var
    PosA, PosB, PosMid, Normal, DirA, DirB: TIntVector3;
  begin
    // make sure, that PosMid is in the middle between the two other
    // find, where distance is greater (unequal) than 1
    // note: C cannot be mid, because of the iteration order
    if (Corner3Pos[A] - Corner3Pos[C]).SqrDot <> 1 then
    begin
      PosA := Corner3Pos[C];
      PosB := Corner3Pos[A];
      PosMid := Corner3Pos[B];
    end
    else
    begin
      PosA := Corner3Pos[C];
      PosB := Corner3Pos[B];
      PosMid := Corner3Pos[A];
    end;
    if ((PosMid - PosA).Cross(PosMid - PosB) <= 0) = (PosA.Dot(PosB) <= 0) then
      SwapPos(PosA, PosB);

    DirA := (PosB - PosMid);
    DirB := (PosA - PosMid);
    Normal := DirB.Cross(DirA);

    AppendLookup([
      PlaneInfo(Inverted,
      PlanePoint(PosA, Normal),
      PlanePoint(PosB, Normal),
      PlanePoint(PosMid, Normal)),
      PlaneInfo(Inverted,
      PlanePoint(PosA, DirA),
      PlanePoint(PosB, DirB),
      PlanePoint(PosB, Normal)),
      PlaneInfo(Inverted,
      PlanePoint(PosB, Normal),
      PlanePoint(PosA, Normal),
      PlanePoint(PosA, DirA))
      ]);
  end;

  procedure Case4(A, B, C, D: TCorner3);
  var
    Pos: array [0 .. 3] of TIntVector3;
    Axis: TCoordAxis3;
    Slab, Hexagon: Boolean;
    Normal, HexDir, DirA, DirB, DirC: TIntVector3;
    J, K: Integer;
  begin
    Pos[0] := Corner3Pos[A];
    Pos[1] := Corner3Pos[B];
    Pos[2] := Corner3Pos[C];
    Pos[3] := Corner3Pos[D];

    for Axis := Low(TCoordAxis3) to High(TCoordAxis3) do
    begin
      Slab := True;
      for J := 1 to 3 do
      begin
        if Pos[0][Axis] <> Pos[J][Axis] then
        begin
          Slab := False;
          Break;
        end;
      end;
      if Slab then
        Break;
    end;

    if Slab then
    begin
      Normal := Vec3Axis[Axis] * (1 - Pos[0][Axis] * 2);
      if (Normal.X < 0) or (Normal.Z < 0) or (Normal.Y > 0) then
        SwapPos(Pos[3], Pos[0]);
      AppendLookup([
        PlaneInfo(Inverted,
        PlanePoint(Pos[0], Normal),
        PlanePoint(Pos[1], Normal),
        PlanePoint(Pos[2], Normal)),
        PlaneInfo(Inverted,
        PlanePoint(Pos[1], Normal),
        PlanePoint(Pos[3], Normal),
        PlanePoint(Pos[2], Normal))
        ]);
      Exit;
    end;

    for J := 0 to 3 do
    begin
      HexDir := 0;
      for K := 0 to 3 do
      begin
        if J = K then
          Continue;
        HexDir := HexDir + Pos[J] - Pos[K];
      end;
      Hexagon := HexDir.Abs = 1;
      if Hexagon then
        Break;
    end;

    if Hexagon then
    begin
      if J <> 3 then
        SwapPos(Pos[J], Pos[3]);
      if (Pos[3] = IVec3(1, 0, 1)) or (Pos[3] = IVec3(0, 1, 0)) or (Pos[3] = IVec3(1, 1, 1)) then
        SwapPos(Pos[0], Pos[1]);
      DirA := Pos[0] - Pos[3];
      DirB := Pos[1] - Pos[3];
      DirC := Pos[2] - Pos[3];
      AppendLookup([
        PlaneInfo(Inverted,
        PlanePoint(Pos[0], DirB),
        PlanePoint(Pos[1], DirC),
        PlanePoint(Pos[2], DirA)),
        PlaneInfo(Inverted,
        PlanePoint(Pos[0], DirB),
        PlanePoint(Pos[1], DirA),
        PlanePoint(Pos[1], DirC)),
        PlaneInfo(Inverted,
        PlanePoint(Pos[1], DirC),
        PlanePoint(Pos[2], DirB),
        PlanePoint(Pos[2], DirA)),
        PlaneInfo(Inverted,
        PlanePoint(Pos[2], DirA),
        PlanePoint(Pos[0], DirC),
        PlanePoint(Pos[0], DirB))
        ]);
      Exit;
    end;

    // Ensure, 0 and 3 to be opposing points
    if (Pos[1] - Pos[3]).SqrDot = 3 then
      SwapPos(Pos[0], Pos[1]);
    if (Pos[2] - Pos[3]).SqrDot = 3 then
      SwapPos(Pos[0], Pos[2]);

    // Ensure 0 and 1 are next to each other
    if (Pos[0] - Pos[1]).SqrDot <> 1 then
      SwapPos(Pos[1], Pos[2]);

    // Enough checks, as assertions don't hit
    // Assert((Pos[0] - Pos[3]).SqrDot = 3);
    // Assert((Pos[0] - Pos[1]).SqrDot = 1);

    // Now, points are in order

    if ((Pos[1] - Pos[0]).Cross(Pos[2] - Pos[1]) <= 0) = 
      ((Pos[2] - Pos[1]).Cross(Pos[3] - Pos[2]) <= 0) then
    begin
      SwapPos(Pos[0], Pos[3]);
      SwapPos(Pos[1], Pos[2]);
    end;

    // Directions point along the snake
    DirA := Pos[1] - Pos[0];
    DirB := Pos[2] - Pos[1];
    DirC := Pos[3] - Pos[2];

    if DirA.Cross(DirB) = DirC then
    begin
      AppendLookup([ 
        PlaneInfo(Inverted,
        PlanePoint(Pos[0], DirC),
        PlanePoint(Pos[1], DirC),
        PlanePoint(Pos[3], -DirB)),
        PlaneInfo(Inverted,
        PlanePoint(Pos[3], -DirA),
        PlanePoint(Pos[0], DirC),
        PlanePoint(Pos[3], -DirB)),
        PlaneInfo(Inverted,
        PlanePoint(Pos[0], DirB),
        PlanePoint(Pos[0], DirC),
        PlanePoint(Pos[3], -DirA)),
        PlaneInfo(Inverted,
        PlanePoint(Pos[2], -DirA),
        PlanePoint(Pos[0], DirB),
        PlanePoint(Pos[3], -DirA))
        ]);
    end
    else
    begin
      AppendLookup([ 
        PlaneInfo(Inverted,
        PlanePoint(Pos[1], DirC),
        PlanePoint(Pos[0], DirC),
        PlanePoint(Pos[3], -DirB)),
        PlaneInfo(Inverted,
        PlanePoint(Pos[0], DirC),
        PlanePoint(Pos[3], -DirA),
        PlanePoint(Pos[3], -DirB)),
        PlaneInfo(Inverted,
        PlanePoint(Pos[0], DirC),
        PlanePoint(Pos[0], DirB),
        PlanePoint(Pos[3], -DirA)),
        PlaneInfo(Inverted,
        PlanePoint(Pos[0], DirB),
        PlanePoint(Pos[2], -DirA),
        PlanePoint(Pos[3], -DirA))
        ]);
    end;
  end;

  procedure ProcessCorners(ACornerArray: TArray<TCorner3>);
  begin
    case Length(ACornerArray) of
      1:
        Case1(ACornerArray[0]);
      2:
        Case2(ACornerArray[0], ACornerArray[1]);
      3:
        Case3(ACornerArray[0], ACornerArray[1], ACornerArray[2]);
      4:
        Case4(ACornerArray[0], ACornerArray[1], ACornerArray[2], ACornerArray[3]);
    end;
  end;

  procedure DiagonalFix(A, B: TCorner3);
  var
    PosA, PosB, Normal, Left, Down: TIntVector3;
  begin
    PosA := Corner3Pos[A];
    PosB := Corner3Pos[B];
    // Must be two diagonally apart
    if (PosB - PosA).SqrDot <> 2 then
      Exit;
        
    Normal := PosA + PosB - 1;
    Left := -Normal.Cross(Normal >= 0);  
    Down := Left.Cross(Normal);           
    if Normal <= 0 then
    begin
      Left := -Left;
      Down := -Down;   
    end;

    // AHHHHH SEND HELP
    // SwapPos(Left, Down); 
    
    AppendLookup([
      PlaneInfo(Inverted,
      PlanePoint(PosA, -Down),
      PlanePoint(PosA, -Left),
      PlanePoint(PosB, Down)),
      PlaneInfo(Inverted,
      PlanePoint(PosB, Down),
      PlanePoint(PosB, Left),
      PlanePoint(PosA, -Down))
      ]);
  end;

var
  Corner: TCorner3;
  Corners: TCorners3;
  CornerArray, InnerCornerArray: TArray<TCorner3>;
  SplitCorners: TArray<TArray<TCorner3>>;
  J: Integer;
begin
  Writeln('Pre-Generating Marching Cubes:');
  StartTimer;

  for I := 0 to 255 do
  begin
    for CornerArray in SplitConnected(TCorners3(I)) do
    begin
      Inverted := Length(CornerArray) > 4;
      if not Inverted then
      begin
        ProcessCorners(CornerArray);
        Continue;
      end;

      Corners := [Low(TCorner3) .. High(TCorner3)];
      for Corner in CornerArray do
        Exclude(Corners, Corner);

      // Special case of 2 diagonally opposing corners
      // o-----+    o-----+               o-----+
      // |#/   |    |###\ |  this leaves  | /#\ |
      // |/   /| vs |\###\|  a rectangle  |<###>|
      // |   /#|    | \###|  open on inv  | \#/ |
      // +-----o    +-----o               +-----o

      SplitCorners := SplitConnected(Corners);
      if Length(SplitCorners) = 2 then
      begin
        // Check, wether this is the case by checking if the taxicab distance between the corners is 2
        case GetBitCount(Byte(Corners)) of
          2:
            DiagonalFix(SplitCorners[0][0], SplitCorners[1][0]);
          3:
            if Length(SplitCorners[0]) = 2 then
            begin
              DiagonalFix(SplitCorners[0][0], SplitCorners[1][0]);
              DiagonalFix(SplitCorners[0][1], SplitCorners[1][0]);
            end
            else
            begin
              DiagonalFix(SplitCorners[0][0], SplitCorners[1][0]);
              DiagonalFix(SplitCorners[0][0], SplitCorners[1][1]);
            end;
        end;
      end;

      // Inversion makes it neccesary to split again
      for InnerCornerArray in SplitCorners do
        ProcessCorners(InnerCornerArray);
    end;
  end;

  StopTimerAndOutput;
end;

class function TMarchingCubes.GetTriangles(ACorners: TCorners3; AOffset: Single): TArray<TPlane3>;
var
  Info: TArray<TPlaneInfo>;
  I: Integer;
  Pos: TVector3;
  Offset: Single;
begin
  Info := FLookup[Byte(ACorners)];
  SetLength(Result, Length(Info));
  for I := 0 to Length(Info) - 1 do
  begin
    if Info[I].Inverted then
      Offset := 1 - AOffset
    else
      Offset := AOffset;
    Pos := Info[I].Points[0].Position;
    Result[I].Create(
      Pos + Offset * Info[I].Points[0].Direction,
      Pos.VectorTo(Info[I].Points[1].Position) + Offset * (Info[I].Points[1].Direction - Info[I].Points[0].Direction),
      Pos.VectorTo(Info[I].Points[2].Position) + Offset * (Info[I].Points[2].Direction - Info[I].Points[0].Direction));
  end;
end;

class function TMarchingCubes.PlaneInfo(AInverted: Boolean; A, B, C: TPlanePoint): TPlaneInfo;
begin
  Result.Points[0] := A;
  if AInverted then
  begin
    Result.Points[1] := C;
    Result.Points[2] := B;
  end
  else
  begin
    Result.Points[1] := B;
    Result.Points[2] := C;
  end;
  Result.Inverted := AInverted;
end;

class function TMarchingCubes.PlanePoint(APosition, ADirection: TVector3): TPlanePoint;
begin
  Result.Create(APosition, ADirection);
end;

{ TMarchingCubes.TPlanePoint }

constructor TMarchingCubes.TPlanePoint.Create(APosition, ADirection: TVector3);
begin
  Position := APosition;
  Direction := ADirection;
end;

end.
