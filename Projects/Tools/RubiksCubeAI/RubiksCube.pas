unit RubiksCube;

interface

uses
  Pengine.IntMaths;

type

  TRubiksCube = class
  public type

    TColor = (
      rcBlue,
      rcGreen,
      rcYellow,
      rcWhite,
      rcRed,
      rcOrange
      );

    TSide = array of array of TColor;

  private
    FSides: array [TBasicDir3] of TSide;

    function GetSize: Integer;
    procedure SetSize(const Value: Integer);
    function GetSide(ASide: TBasicDir3; Pos: TIntVector2): TColor;

    procedure TurnSide(ASide: TBasicDir3; ACount: Integer);
    procedure TurnSlice(ASide: TCoordAxis3; ALayer, ACount: Integer);

  public
    constructor Create(ASize: Integer = 3);

    property Size: Integer read GetSize write SetSize;
    property Sides[ASide: TBasicDir3; Pos: TIntVector2]: TColor read GetSide;

    procedure Turn(ASide: TCoordAxis3; ALayer, ACount: Integer);

  end;

implementation

{ TRubiksCube }

constructor TRubiksCube.Create(ASize: Integer);
begin
  Size := ASize;
end;

function TRubiksCube.GetSide(ASide: TBasicDir3; Pos: TIntVector2): TColor;
begin
  Result := FSides[ASide][Pos.X, Pos.Y];
end;

function TRubiksCube.GetSize: Integer;
begin
  Result := Length(FSides[bdLeft]);
end;

procedure TRubiksCube.SetSize(const Value: Integer);
var
  Side: TBasicDir3;
begin
  if Size = Value then
    Exit;
  for Side := Low(TBasicDir3) to High(TBasicDir3) do
    SetLength(FSides[Side], Value, Value);
end;

procedure TRubiksCube.Turn(ASide: TCoordAxis3; ALayer, ACount: Integer);
begin
  if ALayer = 0 then
    TurnSide(AxisBasicDir[ASide], ACount);
  if ALayer = Size - 1 then
    TurnSide(FlipDir(AxisBasicDir[ASide]), ACount);
  TurnSlice(ASide, ALayer, ACount);
end;

procedure TRubiksCube.TurnSide(ASide: TBasicDir3; ACount: Integer);
begin

end;

procedure TRubiksCube.TurnSlice(ASide: TCoordAxis3; ALayer, ACount: Integer);
var
  Buffer: TSide;
  Pos: TIntVector2;
begin
  Move(FSides[ASide][0, 0], Buffer[0, 0], SizeOf(TColor) * Sqr(Size));
  case ACount mod 4 of
    1:
      for Pos in IVec2(Size) do
        FSides[ASide][Pos.X, Pos.Y] := Buffer[Size - Pos.Y - 1, Pos.X];
    2:
      for Pos in IVec2(Size) do
        FSides[ASide][Pos.X, Pos.Y] := Buffer[Size - Pos.X - 1, Size - Pos.Y - 1];
    3:
      for Pos in IVec2(Size) do
        FSides[ASide][Pos.X, Pos.Y] := Buffer[Pos.Y, Size - Pos.X - 1];
  end;
end;

end.
