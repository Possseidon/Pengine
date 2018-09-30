unit Pengine.MC.Dimension;

interface

uses
  Pengine.MC.Namespace;

type

  TDimension = (
    dimOverworld,
    dimNether,
    dimEnd
    );

const

  DimensionNames: array [TDimension] of string = (
    'overworld',
    'the_nether',
    'the_end'
    );

  DimensionDisplayNames: array [TDimension] of string = (
    'Overworld',
    'Nether',
    'End'
    );

function DimensionFromName(ANSPath: TNSPath; out ADimension: TDimension): Boolean;

implementation

function DimensionFromName(ANSPath: TNSPath; out ADimension: TDimension): Boolean;
var
  Dimension: TDimension;
begin
  for Dimension := Low(TDimension) to High(TDimension) do
    if ANSPath = DimensionNames[Dimension] then
    begin
      ADimension := Dimension;
      Exit(True);
    end;
  Result := False;
end;

end.
