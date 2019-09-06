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

      class operator Equal(const A, B: TPlanePoint): Boolean;

    end;

    TPlaneInfo = record
      Points: array [0 .. 2] of TPlanePoint;

    end;

  private
    class var
      FLookup: array [Byte] of TArray<TPlaneInfo>;

    class function NewPlanePoint(APosition, ADirection: TVector3): TPlanePoint; overload;
    class function NewPlanePoint(APosition: TIntVector3; AAxis: TCoordAxis3): TPlanePoint; overload;
    class function NewPlaneInfo(A, B, C: TPlanePoint): TPlaneInfo;

  public
    class constructor Create;

    class function GetTriangles(ACorners: TCorners3; AOffset: Single = 0.5): TArray<TPlane3>; overload;
    // class function GetVoxelTriangles(ACorners: TCorners3): TArray<TPlane3>;

  end;

implementation

{ TMarchingCubes }

class constructor TMarchingCubes.Create;

type

  TLine = record
    Start: TPlanePoint;
    Stop: TPlanePoint;
  end;

  TLoop = record
    Points: array [0 .. 6] of TPlanePoint;
    Count: Integer;
  end;

  function NewLine(AFlip, AInvert: Boolean; AStart, AStop: TPlanePoint): TLine;
  begin
    if AFlip then
    begin
      Result.Start := AStop;
      Result.Stop := AStart;
    end
    else
    begin
      Result.Start := AStart;
      Result.Stop := AStop;
    end;

    if AInvert then
    begin
      Result.Start.Position := Result.Start.Position + Result.Start.Direction;
      Result.Start.Direction := -Result.Start.Direction;
      Result.Stop.Position := Result.Stop.Position + Result.Stop.Direction;
      Result.Stop.Direction := -Result.Stop.Direction;
    end;
  end;

  function CornersToArray(ACorners: TCorners3): TArray<TCorner3>;
  var
    Corner: TCorner3;
    I: Integer;
  begin
    SetLength(Result, GetBitCount(Byte(ACorners)));
    I := 0;
    for Corner in ACorners do
    begin
      Result[I] := Corner;
      Inc(I);
    end;
  end;

  function CountWindings(ACorner: TCorner3): Boolean;
  begin
    Result := (Corner3Pos[ACorner].SqrDot mod 2) = 0;
  end;

var
  Lines: array [0 .. 11] of TLine;
  Loops: array [0 .. 3] of TLoop;
  LineCount, LoopCount: Integer;

  procedure AddLine(const ALine: TLine);
  begin
    Lines[LineCount] := ALine;
    Inc(LineCount);
  end;

  procedure RemoveLine(I: Integer);
  begin
    Move(Lines[I + 1], Lines[I], (LineCount - I - 1) * SizeOf(TLine));
    Dec(LineCount);
  end;

var
  I: Byte;
  Corners: TCorners3;
  Dir: TBasicDir3;
  MaskedCorners: TArray<TCorner3>;
  Normal: TIntVector3;
  Loop: TLoop;
  Line: TLine;
  Done: Boolean;
  Right, Up: TCoordAxis3;
  Flipped: Boolean;
  Corner: TCorner3;
  Offset, J, K: Integer;
  PlanePoint: TPlanePoint;
  Center, CenterLow, CenterHigh: TPlanePoint;
begin
  for I := 0 to 255 do
  begin
    Corners := TCorners3(I);

    LineCount := 0;
    for Dir := Low(TBasicDir3) to High(TBasicDir3) do
    begin
      MaskedCorners := CornersToArray(Corners * CubeSideCornerMask[Dir]);
      if Length(MaskedCorners) in [0, 4] then
        Continue;
      Right := TCoordAxis3((Ord(BasicDirAxis[Dir]) - Ord(caX) + 2) mod 3 + Ord(caX));
      Up := TCoordAxis3((Ord(BasicDirAxis[Dir]) - Ord(caX) + 1) mod 3 + Ord(caX));
      case Length(MaskedCorners) of
        1:
          AddLine(NewLine(CountWindings(MaskedCorners[0]), False,
            NewPlanePoint(Corner3Pos[MaskedCorners[0]], Right),
            NewPlanePoint(Corner3Pos[MaskedCorners[0]], Up)
            ));
        2:
          if CornersConnected(MaskedCorners[0], MaskedCorners[1]) then
          begin
            Normal := Vec3Dir[Dir].Cross(Corner3Pos[MaskedCorners[1]] - Corner3Pos[MaskedCorners[0]]);
            Flipped := Vec3Dir[Dir].Cross(Corner3Pos[MaskedCorners[1]] + Corner3Pos[MaskedCorners[0]] - 1) <= 0;
            if Flipped then
              Normal := -Normal;
            AddLine(NewLine(Flipped, False,
              NewPlanePoint(Corner3Pos[MaskedCorners[0]], Normal),
              NewPlanePoint(Corner3Pos[MaskedCorners[1]], Normal)
              ));
          end
          else
          begin
            AddLine(NewLine(CountWindings(MaskedCorners[0]), False,
              NewPlanePoint(Corner3Pos[MaskedCorners[0]], Right),
              NewPlanePoint(Corner3Pos[MaskedCorners[0]], Up)
              ));
            AddLine(NewLine(CountWindings(MaskedCorners[1]), False,
              NewPlanePoint(Corner3Pos[MaskedCorners[1]], Right),
              NewPlanePoint(Corner3Pos[MaskedCorners[1]], Up)
              ));
          end;
        3:
          begin
            Corner := TCorner3(ILog2(Byte(CubeSideCornerMask[Dir] - Corners)));
            AddLine(NewLine(not CountWindings(Corner), True,
              NewPlanePoint(Corner3Pos[Corner], Right),
              NewPlanePoint(Corner3Pos[Corner], Up)
              ));
          end;
      end;
    end;

    LoopCount := 0;
    while LineCount > 0 do
    begin
      Line := Lines[LineCount - 1];
      Dec(LineCount);
      Loop.Count := 2;
      Loop.Points[0] := Line.Start;
      Loop.Points[1] := Line.Stop;
      repeat
        Done := False;
        for J := 0 to LineCount - 1 do
        begin
          if Lines[J].Start = Loop.Points[Loop.Count - 1] then
          begin
            Done := Lines[J].Stop = Loop.Points[0];
            if not Done then
            begin
              Loop.Points[Loop.Count] := Lines[J].Stop;
              Inc(Loop.Count);
            end;
            RemoveLine(J);
            Break;
          end;
        end;
      until Done;
      Loops[LoopCount] := Loop;
      Inc(LoopCount);
    end;

    Offset := 0;
    for J := 0 to LoopCount - 1 do
    begin
      Loop := Loops[J];
      SetLength(FLookup[I], Offset + Loop.Count);
      case Loop.Count of
        3:
          FLookup[I][Offset] := NewPlaneInfo(Loop.Points[0], Loop.Points[1], Loop.Points[2]);
      else
        Center := NewPlanePoint(0, 0);
        for K := 0 to Loop.Count - 1 do
        begin
          Center.Position := Center.Position + Loop.Points[K].Position;
          Center.Direction := Center.Direction + Loop.Points[K].Direction;
        end;
        Center.Position := Center.Position / Loop.Count;
        Center.Direction := Center.Direction / Loop.Count;

        for K := 0 to Loop.Count - 1 do
          FLookup[I][Offset + K] := NewPlaneInfo(Loop.Points[K], Loop.Points[K + 1], Center);
        FLookup[I][Offset + Loop.Count - 1] := NewPlaneInfo(Loop.Points[Loop.Count - 1], Loop.Points[0], Center);
      end;
      Inc(Offset, Loop.Count);
    end;
  end;
end;

class function TMarchingCubes.GetTriangles(ACorners: TCorners3; AOffset: Single): TArray<TPlane3>;
var
  Info: TArray<TPlaneInfo>;
  I: Integer;
  Pos: TVector3;
begin
  Info := FLookup[Byte(ACorners)];
  SetLength(Result, Length(Info));
  for I := 0 to Length(Info) - 1 do
  begin
    Pos := Info[I].Points[0].Position;
    Result[I].Create(
      Pos + AOffset * TVector3(Info[I].Points[0].Direction),
      Pos.VectorTo(Info[I].Points[1].Position) + AOffset * (Info[I].Points[1].Direction -
      TVector3(Info[I].Points[0].Direction)),
      Pos.VectorTo(Info[I].Points[2].Position) + AOffset * (Info[I].Points[2].Direction -
      TVector3(Info[I].Points[0].Direction)));
  end;
end;

class function TMarchingCubes.NewPlaneInfo(A, B, C: TPlanePoint): TPlaneInfo;
begin
  Result.Points[0] := A;
  Result.Points[1] := B;
  Result.Points[2] := C;
end;

class function TMarchingCubes.NewPlanePoint(APosition: TIntVector3; AAxis: TCoordAxis3): TPlanePoint;
var
  Dir: TIntVector3;
begin
  Dir := 0;
  Dir[AAxis] := 1 - APosition[AAxis] * 2;
  Result.Create(APosition, Dir);
end;

class function TMarchingCubes.NewPlanePoint(APosition, ADirection: TVector3): TPlanePoint;
begin
  Result.Create(APosition, ADirection);
end;

{ TMarchingCubes.TPlanePoint }

constructor TMarchingCubes.TPlanePoint.Create(APosition, ADirection: TVector3);
begin
  Position := APosition;
  Direction := ADirection;
end;

class operator TMarchingCubes.TPlanePoint.Equal(const A, B: TPlanePoint): Boolean;
begin
  Result := (A.Position = B.Position) and (A.Direction = B.Direction);
end;

end.
