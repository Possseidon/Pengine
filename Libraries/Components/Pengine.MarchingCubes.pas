unit Pengine.MarchingCubes;

interface

uses
  Pengine.IntMaths,
  Pengine.Vector,
  Pengine.Utility;

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
  Corners: TCorners3;
  CornerArray: TArray<TCorner3>;
  Inverted: Boolean;

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

  procedure Case1(A: TCorner3);
  var
    Pos: TIntVector3;
  begin
    Pos := Corner3Pos[A];
    FLookup[I] := [
      PlaneInfo(Inverted,
      PlanePoint(Pos, -Vec3Dir[CornerDirectionArrays[A, 0]]),
      PlanePoint(Pos, -Vec3Dir[CornerDirectionArrays[A, 1]]),
      PlanePoint(Pos, -Vec3Dir[CornerDirectionArrays[A, 2]]))
      ];
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
    FLookup[I] := [
      PlaneInfo(Inverted,
      PlanePoint(PosA, DirA),
      PlanePoint(PosA, DirB),
      PlanePoint(PosB, DirB)),
      PlaneInfo(Inverted,
      PlanePoint(PosB, DirB),
      PlanePoint(PosB, DirA),
      PlanePoint(PosA, DirA))
      ];
  end;

  procedure Case3(A, B, C: TCorner3);
  var
    PosA, PosB, PosMid, Normal, DirA, DirB, Tmp: TIntVector3;
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
    begin
      Tmp := PosA;
      PosA := PosB;
      PosB := Tmp;
    end;

    DirA := (PosB - PosMid);
    DirB := (PosA - PosMid);
    Normal := DirB.Cross(DirA);

    FLookup[I] := [
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
      ];
  end;

  procedure Case4(A, B, C, D: TCorner3);
  var
    Pos: array [0 .. 3] of TIntVector3;
    Axis: TCoordAxis3;
    Slab: Boolean;
    Normal, Tmp: TIntVector3;
    J: Integer;
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
      begin
        Tmp := Pos[3];
        Pos[3] := Pos[0];
        Pos[0] := Tmp;
      end;
      FLookup[I] := [
        PlaneInfo(Inverted,
        PlanePoint(Pos[0], Normal),
        PlanePoint(Pos[1], Normal),
        PlanePoint(Pos[2], Normal)),
        PlaneInfo(Inverted,
        PlanePoint(Pos[1], Normal),
        PlanePoint(Pos[3], Normal),
        PlanePoint(Pos[2], Normal))
        ];
      Exit;
    end;

    // distinguish slab, all-axes and Z-shape

    // 1) is there any axis same on each -> slab

    // 2) is there one for which each point is adjacent -> all axes from that point

    // 3) else, Z-shape

    FLookup[I] := [];
  end;

begin
  for I := 0 to 255 do
  begin
    Inverted := GetBitCount(I) > 4;
    if Inverted then
      Corners := TCorners3(not I)
    else
      Corners := TCorners3(I);

    for CornerArray in SplitConnected(Corners) do
    begin
      case Length(CornerArray) of
        0:
          FLookup[I] := [];
        1:
          Case1(CornerArray[0]);
        2:
          Case2(CornerArray[0], CornerArray[1]);
        3:
          Case3(CornerArray[0], CornerArray[1], CornerArray[2]);
        4:
          Case4(CornerArray[0], CornerArray[1], CornerArray[2], CornerArray[3]);
      end;
    end;
  end;
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
