unit Pengine.MC.TextComponent;

interface

type

  TMCColor = (
    mclBlack,
    mclDarkBlue,
    mclDarkGreen,
    mclDarkAqua,
    mclDarkRed,
    mclDarkPurple,
    mclGold,
    mclGray,
    mclDarkGray,
    mclBlue,
    mclGreen,
    mclAqua,
    mclRed,
    mclLightPurple,
    mclYellow,
    mclWhite,
    mclReset
    );

  TMCColorNoReset = mclBlack .. mclWhite;

const

  MCColorNames: array [TMCColor] of string = (
    'black',
    'dark_blue',
    'dark_green',
    'dark_aqua',
    'dark_red',
    'dark_purple',
    'gold',
    'gray',
    'dark_gray',
    'blue',
    'green',
    'aqua',
    'red',
    'light_purple',
    'yellow',
    'white',
    'reset'
    );

  MCColorDisplayNames: array [TMCColor] of string = (
    'Black',
    'Dark Blue',
    'Dark Green',
    'Dark Aqua',
    'Dark Red',
    'Dark Purple',
    'Gold',
    'Gray',
    'Dark Gray',
    'Blue',
    'Green',
    'Aqua',
    'Red',
    'Light Purple',
    'Yellow',
    'White',
    'Reset'
    );

function MCColorFromName(AName: string; out AMCColor: TMCColor): Boolean;
function MCColorFromNameNoReset(AName: string; out AMCColor: TMCColorNoReset): Boolean;

implementation

function MCColorFromName(AName: string; out AMCColor: TMCColor): Boolean;
var
  MCColor: TMCColor;
begin
  for MCColor := Low(TMCColor) to High(TMCColor) do
    if AName = MCColorNames[MCColor] then
    begin
      AMCColor := MCColor;
      Exit(True);
    end;
  Result := False;
end;

function MCColorFromNameNoReset(AName: string; out AMCColor: TMCColorNoReset): Boolean;
var
  MCColor: TMCColorNoReset;
begin
  for MCColor := Low(TMCColorNoReset) to High(TMCColorNoReset) do
    if AName = MCColorNames[MCColor] then
    begin
      AMCColor := MCColor;
      Exit(True);
    end;
  Result := False;
end;

end.
