unit Pengine.MC.Scoreboard;

interface

uses
  System.SysUtils,

  Pengine.Parser,

  Pengine.MC.TextComponent,
  Pengine.MC.General;

type

  TScoreboardSlot = class
  public type

    TParser = class(TObjectParser<TScoreboardSlot>)
    public type

      TSuggestions = class(TParseSuggestionsGenerated<TParser>)
      protected
        procedure Generate; override;

      end;

    protected
      function Parse: Boolean; override;

    end;

  public
    function GetName: string; virtual; abstract;

  end;

  TScoreboardSlotBelowName = class(TScoreboardSlot)
  public const

    Name = 'belowName';

  public
    function GetName: string; override;

  end;

  TScoreboardSlotList = class(TScoreboardSlot)
  public const

    Name = 'list';

  public
    function GetName: string; override;

  end;

  TScoreboardSlotSidebar = class(TScoreboardSlot)
  public const

    Name = 'sidebar';

  public
    function GetName: string; override;

  end;

  TScoreboardSlotSidebarTeam = class(TScoreboardSlot)
  public const

    NamePrefix = 'sidebar.team.';

  private
    FTeamColor: TMCColor;

  public
    constructor Create(ATeamColor: TMCColor); overload;

    property TeamColor: TMCColor read FTeamColor write FTeamColor;

    function GetName: string; override;

  end;

implementation

{ TScoreboardSlot.TParser }

function TScoreboardSlot.TParser.Parse: Boolean;
var
  Name: string;
  Color: TMCColorNoReset;
begin
  BeginSuggestions(TSuggestions.Create);
  Name := ReadWhile(IdentChars);
  if Name.IsEmpty then
    Exit(False);

  if Name = TScoreboardSlotBelowName.Name then
  begin
    SetParseResult(TScoreboardSlotBelowName.Create);
    Exit(True);
  end;

  if Name = TScoreboardSlotList.Name then
  begin
    SetParseResult(TScoreboardSlotList.Create);
    Exit(True);
  end;

  if Name = TScoreboardSlotSidebar.Name then
  begin
    SetParseResult(TScoreboardSlotSidebar.Create);
    Exit(True);
  end;

  if Name.StartsWith(TScoreboardSlotSidebarTeam.NamePrefix) then
  begin
    Name := Name.Substring(Length(TScoreboardSlotSidebarTeam.NamePrefix));
    if not MCColorFromNameNoReset(Name, Color) then
      Exit(False);
    SetParseResult(TScoreboardSlotSidebarTeam.Create(Color));
    Exit(True);
  end;

  Result := False;
end;

{ TScoreboardSlotSidebarTeam }

constructor TScoreboardSlotSidebarTeam.Create(ATeamColor: TMCColor);
begin
  FTeamColor := ATeamColor;
end;

function TScoreboardSlotSidebarTeam.GetName: string;
begin
  Result := NamePrefix + MCColorNames[TeamColor];
end;

{ TScoreboardSlotSidebar }

function TScoreboardSlotSidebar.GetName: string;
begin
  Result := Name;
end;

{ TScoreboardSlotList }

function TScoreboardSlotList.GetName: string;
begin
  Result := Name;
end;

{ TScoreboardSlotBelowName }

function TScoreboardSlotBelowName.GetName: string;
begin
  Result := Name;
end;

{ TScoreboardSlot.TParser.TSuggestions }

procedure TScoreboardSlot.TParser.TSuggestions.Generate;
var
  Color: TMCColorNoReset;
begin
  AddSuggestion(TScoreboardSlotList.Name);
  AddSuggestion(TScoreboardSlotBelowName.Name);
  AddSuggestion(TScoreboardSlotSidebar.Name);
  AddSuggestion(TScoreboardSlotSidebarTeam.NamePrefix);
  for Color := Low(TMCColorNoReset) to High(TMCColorNoReset) do
    AddSuggestion(TScoreboardSlotSidebarTeam.NamePrefix + MCColorNames[Color]);
end;

end.
