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
        {
  TScoreboardCriteria = class

  public
    function Format: string; virtual; abstract;

  end;

  TScoreboardCriteriaSimple = class
  public type

    TType = (
      ctDummy,
      ctTrigger,
      ctDeathCount,
      ctPlayerKillCount,
      ctTotalKillCount,
      ctHealth,
      ctXP,
      ctLevel,
      ctFood,
      ctAir,
      ctArmor
      );

  public const

    Names: array [TType] of string = (
      'dummy',
      'trigger',
      'deathCount',
      'playerKillCount',
      'totalKillCount',
      'health',
      'xp',
      'level',
      'food',
      'air',
      'armor'
      );

  private
    FCriteriaType: TType;

  public
    property CriteriaType: TType read FCriteriaType write FCriteriaType;

    // function Format: string; override;

  end;

  TScoreboardCriteriaComplex = class(TScoreboardCriteria)
  public type

    TType = (
      ctCustom,
      ctCrafted,
      ctUsed,
      ctBroken,
      ctMined,
      ctKilled,
      ctPickedUp,
      ctDropped,
      ctKilledBy,
      ctTeamKill,
      ctKilledByTeam
      );

  public const

    Names: array [TType] of string = (
      'minecraft.custom:minecraft',
      'minecraft.crafted:minecraft',
      'minecraft.used:minecraft',
      'minecraft.broken:minecraft',
      'minecraft.mined:minecraft',
      'minecraft.killed:minecraft',
      'minecraft.picked_up:minecraft',
      'minecraft.dropped:minecraft',
      'minecraft.killed_by:minecraft',
      'teamkill',
      'killedByTeam'
      );

  public
    class function GetType: TType; virtual; abstract;
    class function GetName: string; virtual;
    class function GetSubTypeCount: Integer;
    class function GetSubTypeName();

  end;

  TScoreboardCriteriaStatistic = class(TScoreboardCriteriaComplex)

  end;

  TScoreboardCriteriaCustom = class(TScoreboardCriteriaStatistic)
  public
    class function GetType: TType; override;

  end;

  TScoreboardCriteriaCrafted = class(TScoreboardCriteriaStatistic)
  public
    class function GetType: TType; override;

  end;

  TScoreboardCriteriaUsed = class(TScoreboardCriteriaStatistic)
  public
    class function GetType: TType; override;

  end;

  TScoreboardCriteriaBroken = class(TScoreboardCriteriaStatistic)
  public
    class function GetType: TType; override;

  end;

  TScoreboardCriteriaMined = class(TScoreboardCriteriaStatistic)
  public
    class function GetType: TType; override;

  end;

  TScoreboardCriteriaKilled = class(TScoreboardCriteriaStatistic)
  public
    class function GetType: TType; override;

  end;

  TScoreboardCriteriaPickedUp = class(TScoreboardCriteriaStatistic)
  public
    class function GetType: TType; override;

  end;

  TScoreboardCriteriaDropped = class(TScoreboardCriteriaStatistic)
  public
    class function GetType: TType; override;

  end;

  TScoreboardCriteriaKilledBy = class(TScoreboardCriteriaStatistic)
  public
    class function GetType: TType; override;

  end;

  TScoreboardCriteriaTeamKill = class(TScoreboardCriteriaComplex)
  public
    class function GetType: TType; override;

  end;

  TScoreboardCriteriaKilledByTeam = class(TScoreboardCriteriaComplex)
  public
    class function GetType: TType; override;

  end;
           }
  TScoreboardOperation = (
    soAssign,
    soAdd,
    soSubtract,
    soMultiply,
    soDivide,
    soModulus,
    soEnsureLess,
    soEnsureGreater,
    soSwap
    );

const

  ScoreboardOperationChars = ['=', '+', '-', '*', '/', '%', '<', '>'];

  ScoreboardOperationNames: array [TScoreboardOperation] of string = (
    '=',
    '+=',
    '-=',
    '*=',
    '/=',
    '%=',
    '<',
    '>',
    '><'
    );

  ScoreboardOperationDisplayNames: array [TScoreboardOperation] of string = (
    'Assign',
    'Add',
    'Subtract',
    'Multiply',
    'Divide',
    'Modulus',
    'Ensure Less',
    'Ensure Greater',
    'Swap'
    );

function ScoreboardOperationFromName(AName: string; out AOperation: TScoreboardOperation): Boolean;

implementation

function ScoreboardOperationFromName(AName: string; out AOperation: TScoreboardOperation): Boolean;
var
  Operation: TScoreboardOperation;
begin
  for Operation := Low(TScoreboardOperation) to High(TScoreboardOperation) do
    if AName = ScoreboardOperationNames[Operation] then
    begin
      AOperation := Operation;
      Exit(True);
    end;
  Result := False;
end;

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

{ TScoreboardCriteriaSimple }
                             {
function TScoreboardCriteriaSimple.Format: string;
begin
  Result := Names[CriteriaType];
end;

{ TScoreboardCriteriaComplex }
                    {
class function TScoreboardCriteriaComplex.GetName: string;
begin
  Result := Names[GetType];
end;
                     }
end.
