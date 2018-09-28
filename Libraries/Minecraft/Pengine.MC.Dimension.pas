unit Pengine.MC.Dimension;

interface

type

  TDimension = (
    dimOverworld,
    dimNether,
    dimEnd
    );

const

  DimensionNames: array [TDimension] of string = (
    'overworld',
    'nether',
    'the_end'
    );

  DimensionDisplayNames: array [TDimension] of string = (
    'Overworld',
    'Nether',
    'End'
    );

function DimensionFromName(AName: string; out ADimension: TDimension): Boolean;

implementation

function DimensionFromName(AName: string; out ADimension: TDimension): Boolean;
var
  Dimension: TDimension;
begin
  for Dimension := Low(TDimension) to High(TDimension) do
    if AName = DimensionNames[Dimension] then
    begin
      ADimension := Dimension;
      Exit(True);
    end;
  Result := False;
end;

end.
