unit Pengine.MC.Scoreboard;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,

  Pengine.Parsing,
  Pengine.HashCollections,
  Pengine.Hasher,
  Pengine.Collections,
  Pengine.Settings,
  Pengine.JSON,
  Pengine.JSON.Serialization,

  Pengine.MC.TextComponent,
  Pengine.MC.General,
  Pengine.MC.Item,
  Pengine.MC.Namespace,
  Pengine.MC.Registries;

type

  TScoreboardSlot = class
  public type

    IParser = IObjectParser<TScoreboardSlot>;

    TParser = class(TObjectParser<TScoreboardSlot>, IParser)
    public type

      TSuggestions = class(TParseSuggestionsGenerated<TParser>)
      protected
        procedure Generate; override;

      end;

    protected
      function Parse: Boolean; override;

    end;

  public
    class function Parser: TParser;

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
    FTeamColor: TMCColorNoReset;

  public
    constructor Create(ATeamColor: TMCColorNoReset); overload;

    property TeamColor: TMCColorNoReset read FTeamColor write FTeamColor;

    function GetName: string; override;

  end;

  TScoreboardCriteria = class
  public type

    IParser = IObjectParser<TScoreboardCriteria>;

    TParser = class(TObjectParser<TScoreboardCriteria>, IParser)
    public type

      TSuggestions = class(TParseSuggestionsGenerated<TParser>)
      protected
        procedure Generate; override;

      public
        function GetBreakChars: TSysCharSet; override;

      end;

    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  public
    class function Parser: IParser;

    function Format: string; virtual; abstract;

  end;

  TScoreboardCriteriaSimple = class(TScoreboardCriteria)
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
    constructor Create(AType: TType); overload;

    property CriteriaType: TType read FCriteriaType write FCriteriaType;

    function Format: string; override;

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

    TSubTypeSuggestion = class(TParseSuggestionsGenerated)
    private
      FType: TScoreboardCriteriaComplex.TType;

    protected
      procedure Generate; override;

    public
      constructor Create(AType: TScoreboardCriteriaComplex.TType);

      function GetTitle: string; override;
      function GetBreakChars: TSysCharSet; override;

    end;

  public const

    NamespacedTypes = [ctCustom .. ctKilledBy];

    NamespacePrefix = 'minecraft.';

    Names: array [TType] of string = (
      'custom',
      'crafted',
      'used',
      'broken',
      'mined',
      'killed',
      'picked_up',
      'dropped',
      'killed_by',
      'teamkill',
      'killedByTeam'
      );

  private
    FSubType: Integer;

  public
    constructor Create(ASubType: Integer); overload; virtual;

    class function GetType: TType; virtual; abstract;
    class function GetName: string;
    class function GetSubTypeCount: Integer; virtual; abstract;
    class function GetSubTypeName(ASubType: Integer): string; virtual; abstract;
    class function GetSubTypeFromName(AName: string; out ASubType: Integer): Boolean; virtual;
    class function IsNamespaceType: Boolean;

    property SubType: Integer read FSubType write FSubType;

    function Format: string; override;

  end;

  TScoreboardCriteriaComplexClass = class of TScoreboardCriteriaComplex;

  TScoreboardCriteriaFileSettings = class(TSettings)
  public const

    DefaultPath = 'Data\scoreboard_criterias';

  public type

    TCriteriaType = class
    public type

      TSubTypes = TArray<string>;
      TLookup = TMap<string, Integer, TStringHasher>;

    private
      FSubTypes: TSubTypes;
      FLookup: TLookup;

      function GetLookup: TLookup.TReader;
      function GetSubTypes: TSubTypes.TReader;

    public
      constructor Create(AFileName: string);
      destructor Destroy; override;

      property SubTypes: TSubTypes.TReader read GetSubTypes;
      property Lookup: TLookup.TReader read GetLookup;

    end;

    TCriteriaTypes = TToObjectMap<string, TCriteriaType, TStringHasher>;

  private
    FPath: string;
    FCriteriaTypes: TCriteriaTypes;

    function GetCriteriaType(AName: string): TCriteriaType;
    procedure SetPath(const Value: string);

  protected
    constructor Create(ARoot: TRootSettings); override;

    class function GetNameForVersion(AVersion: Integer): string; override;

  public
    destructor Destroy; override;

    procedure SetDefaults; override;

    property Path: string read FPath write SetPath;
    property CriteriaType[AName: string]: TCriteriaType read GetCriteriaType;

    procedure DefineJStorage(ASerializer: TJSerializer); override;

  end;

  TScoreboardCriteriaFromFile = class(TScoreboardCriteriaComplex)
  private
    FSubTypes: TScoreboardCriteriaFileSettings.TCriteriaType;

  public
    constructor Create; overload;
    constructor Create(ASubType: Integer); overload; override;

    class function GetSubTypes: TScoreboardCriteriaFileSettings.TCriteriaType;
    class function GetSubTypeCount: Integer; override;
    class function GetSubTypeName(AIndex: Integer): string; override;
    class function GetSubTypeFromName(AName: string; out AIndex: Integer): Boolean; override;
    class function GetFileName: string; virtual; abstract;

  end;

  TScoreboardCriteriaEntity = class(TScoreboardCriteriaComplex)
  public
    class function GetSubTypeCount: Integer; override;
    class function GetSubTypeFromName(AName: string; out ASubType: Integer): Boolean; override;
    class function GetSubTypeName(ASubType: Integer): string; override;

  end;

  /// <summary>One of the many statistics on the stats page.</summary>
  TScoreboardCriteriaCustom = class(TScoreboardCriteriaFromFile)
  public
    class function GetType: TScoreboardCriteriaComplex.TType; override;
    class function GetFileName: string; override;

  end;

  TScoreboardCriteriaCrafted = class(TScoreboardCriteriaFromFile)
  public
    class function GetType: TScoreboardCriteriaComplex.TType; override;
    class function GetFileName: string; override;

  end;

  TScoreboardCriteriaUsed = class(TScoreboardCriteriaFromFile)
  public
    class function GetType: TScoreboardCriteriaComplex.TType; override;
    class function GetFileName: string; override;

  end;

  TScoreboardCriteriaBroken = class(TScoreboardCriteriaFromFile)
  public
    class function GetType: TScoreboardCriteriaComplex.TType; override;
    class function GetFileName: string; override;

  end;

  TScoreboardCriteriaMined = class(TScoreboardCriteriaFromFile)
  public
    class function GetType: TScoreboardCriteriaComplex.TType; override;
    class function GetFileName: string; override;

  end;

  TScoreboardCriteriaKilled = class(TScoreboardCriteriaEntity)
  public
    class function GetType: TScoreboardCriteriaComplex.TType; override;

  end;

  TScoreboardCriteriaItem = class(TScoreboardCriteriaComplex)
  private
    FItems: TItemTypeCollection;

  public
    constructor Create;

    class function GetSubTypeCount: Integer; override;
    class function GetSubTypeName(ASubType: Integer): string; override;

  end;

  TScoreboardCriteriaPickedUp = class(TScoreboardCriteriaItem)
  public
    class function GetType: TScoreboardCriteriaComplex.TType; override;

  end;

  TScoreboardCriteriaDropped = class(TScoreboardCriteriaItem)
  public
    class function GetType: TScoreboardCriteriaComplex.TType; override;

  end;

  TScoreboardCriteriaKilledBy = class(TScoreboardCriteriaEntity)
  public
    class function GetType: TScoreboardCriteriaComplex.TType; override;

  end;

  TScoreboardCriteriaColored = class(TScoreboardCriteriaComplex)
  private
    FColor: TMCColorNoReset;

  public
    class function GetSubTypeCount: Integer; override;
    class function GetSubTypeName(AIndex: Integer): string; override;

    property Color: TMCColorNoReset read FColor write FColor;

  end;

  TScoreboardCriteriaTeamKill = class(TScoreboardCriteriaColored)
  public
    class function GetType: TScoreboardCriteriaComplex.TType; override;

  end;

  TScoreboardCriteriaKilledByTeam = class(TScoreboardCriteriaColored)
  public
    class function GetType: TScoreboardCriteriaComplex.TType; override;

  end;

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

  ScoreboardCriteriaComplexClasses: array [TScoreboardCriteriaComplex.TType] of TScoreboardCriteriaComplexClass = (
    TScoreboardCriteriaCustom,
    TScoreboardCriteriaCrafted,
    TScoreboardCriteriaUsed,
    TScoreboardCriteriaBroken,
    TScoreboardCriteriaMined,
    TScoreboardCriteriaKilled,
    TScoreboardCriteriaPickedUp,
    TScoreboardCriteriaDropped,
    TScoreboardCriteriaKilledBy,
    TScoreboardCriteriaTeamKill,
    TScoreboardCriteriaKilledByTeam
    );

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
    ParseResult := TScoreboardSlotBelowName.Create;
    Exit(True);
  end;

  if Name = TScoreboardSlotList.Name then
  begin
    ParseResult := TScoreboardSlotList.Create;
    Exit(True);
  end;

  if Name = TScoreboardSlotSidebar.Name then
  begin
    ParseResult := TScoreboardSlotSidebar.Create;
    Exit(True);
  end;

  if Name.StartsWith(TScoreboardSlotSidebarTeam.NamePrefix) then
  begin
    Name := Name.Substring(Length(TScoreboardSlotSidebarTeam.NamePrefix));
    if not MCColorFromNameNoReset(Name, Color) then
      Exit(False);
    ParseResult := TScoreboardSlotSidebarTeam.Create(Color);
    Exit(True);
  end;

  Result := False;
end;

{ TScoreboardSlotSidebarTeam }

constructor TScoreboardSlotSidebarTeam.Create(ATeamColor: TMCColorNoReset);
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

constructor TScoreboardCriteriaSimple.Create(AType: TType);
begin
  inherited Create;
  FCriteriaType := AType;
end;

function TScoreboardCriteriaSimple.Format: string;
begin
  Result := Names[CriteriaType];
end;

{ TScoreboardCriteriaComplex }

constructor TScoreboardCriteriaComplex.Create(ASubType: Integer);
begin
  inherited Create;
  FSubType := ASubType;
end;

function TScoreboardCriteriaComplex.Format: string;
begin
  if IsNamespaceType then
  begin
    // TODO: prefix minecraft. in front of each type
    Result := GetName + ':' + GetSubTypeName(SubType);
  end
  else
    Result := GetName + '.' + GetSubTypeName(SubType);
end;

class function TScoreboardCriteriaComplex.GetName: string;
begin
  Result := Names[GetType];
end;

class function TScoreboardCriteriaComplex.GetSubTypeFromName(AName: string; out ASubType: Integer): Boolean;
var
  I: Integer;
begin
  for I := 0 to GetSubTypeCount - 1 do
    if AName = GetSubTypeName(I) then
    begin
      ASubType := I;
      Exit(True);
    end;
  Result := False;
end;

class function TScoreboardCriteriaComplex.IsNamespaceType: Boolean;
begin
  Result := GetType in NamespacedTypes;
end;

{ TScoreboardCriteriaColored }

class function TScoreboardCriteriaColored.GetSubTypeCount: Integer;
begin
  // no reset
  Result := Length(MCColorNames) - 1;
end;

class function TScoreboardCriteriaColored.GetSubTypeName(AIndex: Integer): string;
begin
  Result := MCColorNames[TMCColor(AIndex)];
end;

{ TScoreboardCriteriaTeamKill }

class function TScoreboardCriteriaTeamKill.GetType: TScoreboardCriteriaComplex.TType;
begin
  Result := ctTeamKill;
end;

{ TScoreboardCriteriaKilledByTeam }

class function TScoreboardCriteriaKilledByTeam.GetType: TScoreboardCriteriaComplex.TType;
begin
  Result := ctKilledByTeam;
end;

{ TScoreboardCriteriaItem }

constructor TScoreboardCriteriaItem.Create;
begin
  inherited;
  FItems := RootSettingsG.Get<TItemSettings>.Items;
end;

class function TScoreboardCriteriaItem.GetSubTypeCount: Integer;
begin
  Result := RootSettingsG.Get<TItemSettings>.Items.Order.Count;
end;

class function TScoreboardCriteriaItem.GetSubTypeName(ASubType: Integer): string;
begin
  Result := RootSettingsG.Get<TItemSettings>.Items.Order[ASubType].NSPath.Path;
end;

{ TScoreboardCriteriaFileSettings.TCriteriaType }

constructor TScoreboardCriteriaFileSettings.TCriteriaType.Create(AFileName: string);
var
  Line: string;
  I: Integer;
begin
  FSubTypes := TSubTypes.Create;
  FLookup := TLookup.Create;
  I := 0;
  for Line in TFile.ReadAllLines(AFileName) do
  begin
    FSubTypes.Add(Line);
    FLookup[Line] := I;
    Inc(I);
  end;
end;

destructor TScoreboardCriteriaFileSettings.TCriteriaType.Destroy;
begin
  FSubTypes.Free;
  FLookup.Free;
  inherited;
end;

function TScoreboardCriteriaFileSettings.TCriteriaType.GetLookup: TLookup.TReader;
begin
  Result := FLookup.Reader;
end;

function TScoreboardCriteriaFileSettings.TCriteriaType.GetSubTypes: TSubTypes.TReader;
begin
  Result := FSubTypes.Reader;
end;

{ TScoreboardCriteriaFileSettings }

constructor TScoreboardCriteriaFileSettings.Create(ARoot: TRootSettings);
begin
  inherited;
  FCriteriaTypes := TCriteriaTypes.Create;
end;

procedure TScoreboardCriteriaFileSettings.DefineJStorage(ASerializer: TJSerializer);
begin
  ASerializer.Define('path', FPath);
end;

destructor TScoreboardCriteriaFileSettings.Destroy;
begin
  FCriteriaTypes.Free;
  inherited;
end;

function TScoreboardCriteriaFileSettings.GetCriteriaType(AName: string): TCriteriaType;
begin
  if not FCriteriaTypes.Get(AName, Result) then
  begin
    Result := TCriteriaType.Create(TPath.Combine(Path, AName + '.scl'));
    FCriteriaTypes[AName] := Result;
  end;
end;

class function TScoreboardCriteriaFileSettings.GetNameForVersion(AVersion: Integer): string;
begin
  Result := 'mc_scoreboard';
end;

procedure TScoreboardCriteriaFileSettings.SetDefaults;
begin
  inherited;
  Path := DefaultPath;
end;

procedure TScoreboardCriteriaFileSettings.SetPath(const Value: string);
begin
  FPath := Value;
  FCriteriaTypes.Clear;
end;

{ TScoreboardCriteriaFromFile }

constructor TScoreboardCriteriaFromFile.Create(ASubType: Integer);
begin
  inherited;
  FSubTypes := GetSubTypes;
end;

constructor TScoreboardCriteriaFromFile.Create;
begin
  inherited;
  FSubTypes := GetSubTypes;
end;

class function TScoreboardCriteriaFromFile.GetSubTypeCount: Integer;
begin
  Result := GetSubTypes.SubTypes.Count;
end;

class function TScoreboardCriteriaFromFile.GetSubTypeFromName(AName: string; out AIndex: Integer): Boolean;
begin
  Result := GetSubTypes.Lookup.Get(AName, AIndex);
end;

class function TScoreboardCriteriaFromFile.GetSubTypeName(AIndex: Integer): string;
begin
  Result := GetSubTypes.SubTypes[AIndex];
end;

class function TScoreboardCriteriaFromFile.GetSubTypes: TScoreboardCriteriaFileSettings.TCriteriaType;
begin
  Result := RootSettingsG.Get<TScoreboardCriteriaFileSettings>.CriteriaType[GetFileName];
end;

{ TScoreboardCriteriaCustom }

class function TScoreboardCriteriaCustom.GetFileName: string;
begin
  Result := 'custom';
end;

class function TScoreboardCriteriaCustom.GetType: TScoreboardCriteriaComplex.TType;
begin
  Result := ctCustom;
end;

{ TScoreboardCriteriaCrafted }

class function TScoreboardCriteriaCrafted.GetFileName: string;
begin
  Result := 'crafted';
end;

class function TScoreboardCriteriaCrafted.GetType: TScoreboardCriteriaComplex.TType;
begin
  Result := ctCrafted;
end;

{ TScoreboardCriteriaUsed }

class function TScoreboardCriteriaUsed.GetFileName: string;
begin
  Result := 'used';
end;

class function TScoreboardCriteriaUsed.GetType: TScoreboardCriteriaComplex.TType;
begin
  Result := ctUsed;
end;

{ TScoreboardCriteriaBroken }

class function TScoreboardCriteriaBroken.GetFileName: string;
begin
  Result := 'broken';
end;

class function TScoreboardCriteriaBroken.GetType: TScoreboardCriteriaComplex.TType;
begin
  Result := ctBroken;
end;

{ TScoreboardCriteriaMined }

class function TScoreboardCriteriaMined.GetFileName: string;
begin
  Result := 'mined';
end;

class function TScoreboardCriteriaMined.GetType: TScoreboardCriteriaComplex.TType;
begin
  Result := ctMined;
end;

{ TScoreboardCriteriaKilled }

class function TScoreboardCriteriaKilled.GetType: TScoreboardCriteriaComplex.TType;
begin
  Result := ctKilled;
end;

{ TScoreboardCriteriaPickedUp }

class function TScoreboardCriteriaPickedUp.GetType: TScoreboardCriteriaComplex.TType;
begin
  Result := ctPickedUp;
end;

{ TScoreboardCriteriaDropped }

class function TScoreboardCriteriaDropped.GetType: TScoreboardCriteriaComplex.TType;
begin
  Result := ctDropped;
end;

{ TScoreboardCriteriaKilledBy }

class function TScoreboardCriteriaKilledBy.GetType: TScoreboardCriteriaComplex.TType;
begin
  Result := ctKilledBy;
end;

{ TScoreboardCriteria.TParser }

class function TScoreboardCriteria.TParser.GetResultName: string;
begin
  Result := 'Objective-Criteria';
end;

function TScoreboardCriteria.TParser.Parse: Boolean;
var
  Name, NameNoDot: string;
  CriteriaType: TScoreboardCriteriaSimple.TType;
  ComplexType: TScoreboardCriteriaComplex.TType;
  ComplexClass: TScoreboardCriteriaComplexClass;
  SubType: Integer;
  Marker: TLogMarker;
  SplitChar: Char;
begin
  Marker := GetMarker;

  BeginSuggestions(TSuggestions.Create);

  Name := ReadWhile(NamespaceChars, False);
  NameNoDot := ReadWhile(NamespaceChars - ['.']);

  EndSuggestions;

  if Name.IsEmpty then
    Exit(False);

  if Name.StartsWith(TScoreboardCriteriaComplex.NamespacePrefix) then
    Name := Name.Substring(TScoreboardCriteriaComplex.NamespacePrefix.Length);

  for ComplexType := Low(ComplexType) to High(ComplexType) do
  begin
    if (NameNoDot = TScoreboardCriteriaComplex.Names[ComplexType]) or
      (Name = TScoreboardCriteriaComplex.Names[ComplexType]) then
    begin
      if ComplexType in TScoreboardCriteriaComplex.NamespacedTypes then
        SplitChar := ':'
      else
        SplitChar := '.';

      if ComplexType in TScoreboardCriteriaComplex.NamespacedTypes then
        ReadWhile(NamespaceChars);

      if not StartsWith(SplitChar) then
      begin
        Log(1, 'Expected "%s" followed by Criteria Sub-Type.', [SplitChar], elFatal);
        Exit(True);
      end;

      ComplexClass := ScoreboardCriteriaComplexClasses[ComplexType];

      BeginSuggestions(TScoreboardCriteriaComplex.TSubTypeSuggestion.Create(ComplexType));
      Name := ReadWhile(NamespaceChars);
      if (ComplexType in TScoreboardCriteriaComplex.NamespacedTypes) and
        Name.StartsWith(TScoreboardCriteriaComplex.NamespacePrefix) then
        Name := Name.Substring(TScoreboardCriteriaComplex.NamespacePrefix.Length);

      EndSuggestions;
      if not ComplexClass.GetSubTypeFromName(Name, SubType) then
      begin
        Log(Marker, 'Invalid Criteria Sub-Type.', elFatal);
        Exit(True);
      end;

      ParseResult := ComplexClass.Create(SubType);
      Exit(True);
    end;
  end;

  for CriteriaType := Low(CriteriaType) to High(CriteriaType) do
  begin
    if Name = TScoreboardCriteriaSimple.Names[CriteriaType] then
    begin
      ParseResult := TScoreboardCriteriaSimple.Create(CriteriaType);
      Exit(True);
    end;
  end;

  Exit(False);

end;

{ TScoreboardCriteria.TParser.TSuggestions }

procedure TScoreboardCriteria.TParser.TSuggestions.Generate;
var
  Name: string;
  ComplexType: TScoreboardCriteriaComplex.TType;
begin
  for Name in TScoreboardCriteriaSimple.Names do
    AddSuggestion(Name);
  for Name in TScoreboardCriteriaComplex.Names do
    AddSuggestion(Name);
  for ComplexType in TScoreboardCriteriaComplex.NamespacedTypes do
    AddSuggestion(TScoreboardCriteriaComplex.NamespacePrefix + TScoreboardCriteriaComplex.Names[ComplexType]);
end;

function TScoreboardCriteria.TParser.TSuggestions.GetBreakChars: TSysCharSet;
begin
  Result := inherited + [':'] - ['.'];
end;

{ TScoreboardCriteriaComplex.TSubTypeSuggestion }

constructor TScoreboardCriteriaComplex.TSubTypeSuggestion.Create(AType: TScoreboardCriteriaComplex.TType);
begin
  FType := AType;
end;

procedure TScoreboardCriteriaComplex.TSubTypeSuggestion.Generate;
var
  ComplexClass: TScoreboardCriteriaComplexClass;
  I: Integer;
begin
  ComplexClass := ScoreboardCriteriaComplexClasses[FType];
  for I := 0 to ComplexClass.GetSubTypeCount - 1 do
    AddSuggestion(ComplexClass.GetSubTypeName(I));
  for I := 0 to ComplexClass.GetSubTypeCount - 1 do
    AddSuggestion(TScoreboardCriteriaComplex.NamespacePrefix + ComplexClass.GetSubTypeName(I));
end;

function TScoreboardCriteriaComplex.TSubTypeSuggestion.GetBreakChars: TSysCharSet;
begin
  if FType in TScoreboardCriteriaComplex.NamespacedTypes then
    Result := inherited + [':'] - ['.']
  else
    Result := inherited;
end;

function TScoreboardCriteriaComplex.TSubTypeSuggestion.GetTitle: string;
begin
  Result := 'Criteria Sub-Type';
end;

{ TScoreboardCriteriaEntity }

class function TScoreboardCriteriaEntity.GetSubTypeCount: Integer;
begin
  Result := MCRegistries.EntityType.Entries.Count;
end;

class function TScoreboardCriteriaEntity.GetSubTypeFromName(AName: string; out ASubType: Integer): Boolean;
begin
  Result := MCRegistries.EntityType.Get(AName, ASubType);
end;

class function TScoreboardCriteriaEntity.GetSubTypeName(ASubType: Integer): string;
begin
  Result := MCRegistries.EntityType.Entries[ASubType].Format(False);
end;

{ TScoreboardSlot }

class function TScoreboardSlot.Parser: TParser;
begin
  Result := TParser.Create;
end;

{ TScoreboardCriteria }

class function TScoreboardCriteria.Parser: IParser;
begin
  Result := TParser.Create;
end;

end.
