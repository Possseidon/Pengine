unit Pengine.MC.Gamemode;

interface

type

  TGamemode = (
    gmSurvival,
    gmCreative,
    gmAdventure,
    gmSpectator
    );

const

  GamemodeNames: array [TGamemode] of string = (
    'survival',
    'creative',
    'adventure',
    'spectator'
    );

  GamemodeDisplayNames: array [TGamemode] of string = (
    'Survival',
    'Creative',
    'Adventure',
    'Spectator'
    );

function GamemodeFromName(AName: string; out AGamemode: TGamemode): Boolean;

implementation

function GamemodeFromName(AName: string; out AGamemode: TGamemode): Boolean;
var
  Gamemode: TGamemode;
begin
  for Gamemode := Low(TGamemode) to High(TGamemode) do
    if AName = GamemodeNames[Gamemode] then
    begin
      AGamemode := Gamemode;
      Exit(True);
    end;
  Result := False;
end;

end.
