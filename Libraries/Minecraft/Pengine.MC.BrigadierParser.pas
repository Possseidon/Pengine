unit Pengine.MC.BrigadierParser;

interface

uses
  System.SysUtils,
  System.IOUtils,
  System.JSON,
  System.Math,

  Pengine.IntMaths,
  Pengine.Vector,
  Pengine.Parser,
  Pengine.Utility,
  Pengine.Collections,
  Pengine.Settings,

  Pengine.MC.Brigadier,
  Pengine.MC.BlockState,
  Pengine.MC.Item,
  Pengine.MC.EntitySelector,
  Pengine.MC.General,
  Pengine.MC.NBT,
  Pengine.MC.Vector,
  Pengine.MC.Namespace,
  Pengine.MC.Entity,
  Pengine.MC.Enchantment;

type

  // TODO: Replace min and max with intbounds, now that I can do that

  // TODO: Probably better to split this up and keep it in correct unit
  TFormatNamespaceSettings = class(TSettings)
  public type

    TNamespaceScope = (
      nsResourceLocation, // advancement, sound, recipe, bossbar-id
      nsEntitySummon,
      nsSelectorEntity,
      nsBlockState,
      nsBlockPredicate,
      nsItemStack,
      nsItemPredicate,
      nsFunc,
      nsParticle,
      nsItemEnchantment
      );

    TNamespaceScopes = set of TNamespaceScope;

  public const

    DefaultFormattedScopes = [];

  private
    FFormattedScopes: TNamespaceScopes;

    procedure SetFormattedScopes(const Value: TNamespaceScopes);
    function GetScope(AScope: TNamespaceScope): Boolean;
    procedure SetScope(AScope: TNamespaceScope; const Value: Boolean);

  public
    class function GetTitle: string; override;
    class function GetDescription: string; override;
    procedure SetDefaults; override;

    property FormattedScopes: TNamespaceScopes read FFormattedScopes write SetFormattedScopes;
    /// <summary>Currently used for: Advancements, Sounds, Recipes, Bossbar-IDs</summary>
    property ResourceLocation: Boolean index nsResourceLocation read GetScope write SetScope;
    property EntitySummon: Boolean index nsEntitySummon read GetScope write SetScope;
    property SelectorEntity: Boolean index nsSelectorEntity read GetScope write SetScope;
    property BlockState: Boolean index nsBlockState read GetScope write SetScope;
    property BlockPredicate: Boolean index nsBlockPredicate read GetScope write SetScope;
    property ItemStack: Boolean index nsItemStack read GetScope write SetScope;
    property ItemPredicate: Boolean index nsItemPredicate read GetScope write SetScope;
    property Func: Boolean index nsFunc read GetScope write SetScope;
    property Particle: Boolean index nsParticle read GetScope write SetScope;
    property ItemEnchantment: Boolean index nsItemEnchantment read GetScope write SetScope;

  end;

  /// <summary>A single boolean value.</summary>
  TBrigadierBool = class(TBrigadierArgumentParameter)
  private
    FValue: Boolean;

  public
    constructor Create(AArgument: TBrigadierArgument; AValue: Boolean); reintroduce; overload;

    property Value: Boolean read FValue write FValue;

    function Format: string; override;

  end;

  TBrigadierBoolParser = class(TBrigadierParser<TBrigadierBool>)
  public type

    TSuggestions = class(TParseSuggestionsSimple<TBrigadierBoolParser>)
    public
      class function GetCount: Integer; override;
      class function GetSuggestion(AIndex: Integer): TParseSuggestion; override;

    end;

  public const
    BoolStrings: array [Boolean] of string = ('false', 'true');

  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>A single integer value with possible limitations.</summary>
  TBrigadierInteger = class(TBrigadierArgumentParameter)
  private
    FValue: Integer;

  public
    constructor Create(AArgument: TBrigadierArgument; AValue: Integer); reintroduce; overload;

    property Value: Integer read FValue write FValue;

    function Format: string; override;

  end;

  TBrigadierIntegerProperties = class(TBrigadierParserProperties)
  private
    FMin: Integer;
    FMax: Integer;

  public
    constructor Create(AProperties: TJSONObject); override;

    property Min: Integer read FMin write FMin;
    property Max: Integer read FMax write FMax;

  end;

  TBrigadierIntegerParser = class(TBrigadierParser<TBrigadierInteger, TBrigadierIntegerProperties>)
  public const

    ValidChars = ['0' .. '9', '-'];

  protected
    function Parse: Boolean; override;

  public
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>A single float value with possible limitations.</summary>
  TBrigadierFloat = class(TBrigadierArgumentParameter)
  private
    FValue: Single;

  public
    constructor Create(AArgument: TBrigadierArgument; AValue: Single); reintroduce; overload;

    property Value: Single read FValue write FValue;

    function Format: string; override;

  end;

  TBrigadierFloatProperties = class(TBrigadierParserProperties)
  private
    FMin: Single;
    FMax: Single;

  public
    constructor Create(AProperties: TJSONObject); override;

    property Min: Single read FMin write FMin;
    property Max: Single read FMax write FMax;

  end;

  TBrigadierFloatParser = class(TBrigadierParser<TBrigadierFloat, TBrigadierFloatProperties>)
  public const

    ValidChars = ['0' .. '9', '-', '.'];

  protected
    function Parse: Boolean; override;

  public
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>A single double value with possible limitations.</summary>
  TBrigadierDouble = class(TBrigadierArgumentParameter)
  private
    FValue: Single;

  public
    constructor Create(AArgument: TBrigadierArgument; AValue: Double); reintroduce; overload;

    property Value: Single read FValue write FValue;

    function Format: string; override;

  end;

  TBrigadierDoubleProperties = class(TBrigadierParserProperties)
  private
    FMin: Double;
    FMax: Double;

  public
    constructor Create(AProperties: TJSONObject); override;

    property Min: Double read FMin write FMin;
    property Max: Double read FMax write FMax;

  end;

  TBrigadierDoubleParser = class(TBrigadierParser<TBrigadierDouble, TBrigadierDoubleProperties>)
  public const

    ValidChars = ['0' .. '9', '-', '.'];

  protected
    function Parse: Boolean; override;

  public
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>A string, which is taken from a single word, that and possibly encased by quotes or the rest of the command.</summary>
  TBrigadierString = class(TBrigadierArgumentParameter)
  public type

    TMode = (
      smWord,
      smPhrase,
      smGreedy
      );

  public const

    ModeNames: array [TMode] of string = (
      'word',
      'phrase',
      'greedy'
      );

  private
    FMode: TMode;
    FText: string;

  public
    constructor Create(AArgument: TBrigadierArgument; AMode: TMode; AText: string); reintroduce; overload;

    property Mode: TMode read FMode write FMode;
    property Text: string read FText write FText;

    function Format: string; override;

  end;

  TBrigadierStringProperties = class(TBrigadierParserProperties)
  private
    FMode: TBrigadierString.TMode;

  public
    constructor Create(AProperties: TJSONObject); override;

    property Mode: TBrigadierString.TMode read FMode write FMode;

  end;

  TBrigadierStringParser = class(TBrigadierParser<TBrigadierString, TBrigadierStringProperties>)
  protected
    function Parse: Boolean; override;

  public
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>Either a player name, an entity selector or a UUID.</summary>
  TBrigadierEntity = class(TBrigadierArgumentParameter);

  /// <summary>A playername, with a limited amount of characters.</summary>
  TBrigadierEntityPlayer = class(TBrigadierEntity)
  private
    FName: string;

  public
    constructor Create(AArgument: TBrigadierArgument; AName: string); reintroduce; overload;

    property Name: string read FName write FName;

    function Format: string; override;

  end;

  /// <summary>An entity selector with possible context specific constraints.</summary>
  TBrigadierEntitySelector = class(TBrigadierEntity)
  private
    FSelector: TEntitySelector;

  public
    constructor Create(AArgument: TBrigadierArgument); overload; override;
    constructor Create(AArgument: TBrigadierArgument; ASelector: TEntitySelector); reintroduce; overload;
    destructor Destroy; override;

    property Selector: TEntitySelector read FSelector;

    function Format: string; override;

  end;

  TBrigadierEntityProperties = class(TBrigadierParserProperties)
  public type

    TAmount = (
      eaSingle,
      eaMultiple
      );

    TType = (
      etPlayers,
      etEntities
      );

  public const

    AmountNames: array [TAmount] of string = (
      'single',
      'multiple'
      );

    TypeNames: array [TType] of string = (
      'players',
      'entities'
      );

  private
    FAmount: TAmount;
    FType: TType;

  public
    constructor Create(AProperties: TJSONObject); override;

    property Amount: TAmount read FAmount write FAmount;
    property EntityType: TType read FType write FType;

  end;

  TBrigadierEntityParser = class(TBrigadierParser<TBrigadierEntity, TBrigadierEntityProperties>)
  protected
    function Parse: Boolean; override;

  public
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>An NBT-Compound value.</summary>
  TBrigadierNBT = class(TBrigadierArgumentParameter)
  private
    FNBT: TNBTCompound;

  public
    constructor Create(AArgument: TBrigadierArgument); overload; override;
    constructor Create(AArgument: TBrigadierArgument; ANBT: TNBTCompound); reintroduce; overload;
    destructor Destroy; override;

    property NBT: TNBTCompound read FNBT;

    function Format: string; override;

  end;

  TBrigadierNBTParser = class(TBrigadierParser<TBrigadierNBT>)
  protected
    function Parse: Boolean; override;

  public
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>An NBT-Path through compounds and arrays.</summary>
  TBrigadierNBTPath = class(TBrigadierArgumentParameter)
  private
    FPath: TNBTPath;

  public
    constructor Create(AArgument: TBrigadierArgument); overload; override;
    constructor Create(AArgument: TBrigadierArgument; APath: TNBTPath); reintroduce; overload;
    destructor Destroy; override;

    property Path: TNBTPath read FPath;

    function Format: string; override;

  end;

  TBrigadierNBTPathParser = class(TBrigadierParser<TBrigadierNBTPath>)
  protected
    function Parse: Boolean; override;

  public
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>TODO</summary>
  TBrigadierVec2 = class(TBrigadierArgumentParameter)
  private
    FVector: TMCVec2;

  public
    constructor Create(AArgument: TBrigadierArgument); overload; override;
    constructor Create(AArgument: TBrigadierArgument; AVector: TMCVec2); reintroduce; overload;
    destructor Destroy; override;

    property Vector: TMCVec2 read FVector;

    function Format: string; override;

  end;

  TBrigadierVec2Parser = class(TBrigadierParser<TBrigadierVec2>)
  protected
    function Parse: Boolean; override;

  public
    class function GetParserString: TNSPath; override;

  end;

  TBrigadierRotationParser = class(TBrigadierVec2Parser)
  public
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>TODO</summary>
  TBrigadierVec3 = class(TBrigadierArgumentParameter)
  private
    FVector: TMCVec3;

  public
    constructor Create(AArgument: TBrigadierArgument); overload; override;
    constructor Create(AArgument: TBrigadierArgument; AVector: TMCVec3); reintroduce; overload;
    destructor Destroy; override;

    property Vector: TMCVec3 read FVector;

    function Format: string; override;

  end;

  TBrigadierVec3Parser = class(TBrigadierParser<TBrigadierVec3>)
  protected
    function Parse: Boolean; override;

  public
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>A block pos must be integer if absolute.</summary>
  TBrigadierBlockPosParser = class(TBrigadierParser<TBrigadierVec3>)
  protected
    function Parse: Boolean; override;

  public
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>An entity-relative anchor.</summary>
  TBrigadierEntityAnchor = class(TBrigadierArgumentParameter)
  public type

    TPosition = (
      apEyes,
      apFeet
      );

  public const

    PositionStrings: array [TBrigadierEntityAnchor.TPosition] of string = (
      'eyes',
      'feet'
      );

  private
    FPosition: TPosition;

  public
    constructor Create(AArgument: TBrigadierArgument; APosition: TPosition); reintroduce; overload;

    property Position: TPosition read FPosition write FPosition;

    function Format: string; override;

  end;

  TBrigadierEntityAnchorParser = class(TBrigadierParser<TBrigadierEntityAnchor>)
  public type

    TSuggestions = class(TParseSuggestionsSimple<TBrigadierEntityAnchorParser>)
    public
      class function GetCount: Integer; override;
      class function GetSuggestion(AIndex: Integer): TParseSuggestion; override;

    end;

  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>A swizzle of the x, y and z axes.</summary>
  TBrigadierSwizzle = class(TBrigadierArgumentParameter)
  private
    FSwizzle: TMCSwizzle;

  public
    constructor Create(AArgument: TBrigadierArgument); overload; override;
    constructor Create(AArgument: TBrigadierArgument; ASwizzle: TMCSwizzle); reintroduce; overload;
    destructor Destroy; override;

    property Swizzle: TMCSwizzle read FSwizzle;

    function Format: string; override;

  end;

  TBrigadierSwizzleParser = class(TBrigadierParser<TBrigadierSwizzle>)
  protected
    function Parse: Boolean; override;

  public
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>A game profile, used in op or ban commands.</summary>
  TBrigadierGameProfile = class(TBrigadierArgumentParameter)
  private
    FName: string;

  public
    constructor Create(AArgument: TBrigadierArgument; AName: string); reintroduce; overload;

    property Name: string read FName write FName;

    function Format: string; override;

  end;

  TBrigadierGameProfileParser = class(TBrigadierParser<TBrigadierGameProfile>)
  protected
    function Parse: Boolean; override;

  public
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>A simple string used for normal messages and also kick/ban reasons.</summary>
  /// <remarks>Can include entity selectors.</remarks>
  TBrigadierMessage = class(TBrigadierArgumentParameter)
  public type

    TSection = class abstract
    public
      function Format: string; virtual; abstract;

    end;

    TTextSection = class(TSection)
    private
      FText: string;

    public
      constructor Create(AText: string);

      property Text: string read FText write FText;

      function Format: string; override;

    end;

    TSelectorSection = class(TSection)
    private
      FSelector: TEntitySelector;

    public
      constructor Create; overload;
      constructor Create(ASelector: TEntitySelector); overload;
      destructor Destroy; override;

      property Selector: TEntitySelector read FSelector;

      function Format: string; override;

    end;

    TSections = TObjectArray<TSection>;

  private
    FSections: TSections;

  public
    constructor Create(AArgument: TBrigadierArgument); override;
    destructor Destroy; override;

    property Sections: TSections read FSections;

    function Format: string; override;

  end;

  TBrigadierMessageParser = class(TBrigadierParser<TBrigadierMessage>)
  protected
    function Parse: Boolean; override;
    class function KeepEndSuggestions: Boolean; override;

  public
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>A single potion effect.</summary>
  TBrigadierMobEffect = class(TBrigadierArgumentParameter)

  end;

  TBrigadierMobEffectParser = class(TBrigadierParser<TBrigadierMobEffect>)
  protected
    function Parse: Boolean; override;

  public
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>A block state with properties and nbt.</summary>
  TBrigadierBlockState = class(TBrigadierArgumentParameter)
  private
    FBlockState: TBlockState;

  public
    constructor Create(AArgument: TBrigadierArgument); overload; override;
    constructor Create(AArgument: TBrigadierArgument; ABlockState: TBlockState); reintroduce; overload;
    destructor Destroy; override;

    property BlockState: TBlockState read FBlockState;

    function Format: string; override;

  end;

  TBrigadierBlockStateParser = class(TBrigadierParser<TBrigadierBlockState>)
  protected
    function Parse: Boolean; override;

  public
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>A block predicate with properties and nbt.</summary>
  /// <remarks>For the most part identical with block states, but can have tags.</remarks>
  TBrigadierBlockPredicate = class(TBrigadierArgumentParameter)
  private
    FPredicate: TBlockState;

  public
    constructor Create(AArgument: TBrigadierArgument); overload; override;
    constructor Create(AArgument: TBrigadierArgument; APredicate: TBlockState); reintroduce; overload;
    destructor Destroy; override;

    /// <summary>Either a block state or a block tag.</summary>
    property Predicate: TBlockState read FPredicate;

    function Format: string; override;

  end;

  TBrigadierBlockPredicateParser = class(TBrigadierParser<TBrigadierBlockPredicate>)
  protected
    function Parse: Boolean; override;

  public
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>An entity used in the summon command.</summary>
  TBrigadierEntitySummon = class(TBrigadierArgumentParameter)
  private
    FEntity: TEntity;

  public
    constructor Create(AArgument: TBrigadierArgument; AEntity: TEntity); reintroduce; overload;

    property Entity: TEntity read FEntity;

    function Format: string; override;

  end;

  TBrigadierEntitySummonParser = class(TBrigadierParser<TBrigadierEntitySummon>)
  public type

    TSuggestions = class(TParseSuggestionsSimple<TBrigadierEntitySummonParser>)
    public
      class function GetCount: Integer; override;
      class function GetSuggestion(AIndex: Integer): TParseSuggestion; override;

    end;

  protected
    function Parse: Boolean; override;

  public
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>An entity used in the summon command.</summary>
  TBrigadierItemEnchantment = class(TBrigadierArgumentParameter)
  private
    FEnchantment: TEnchantment;

  public
    constructor Create(AArgument: TBrigadierArgument; AEnchantment: TEnchantment); reintroduce; overload;

    property Enchantment: TEnchantment read FEnchantment;

    function Format: string; override;

  end;

  TBrigadierItemEnchantmentParser = class(TBrigadierParser<TBrigadierItemEnchantment>)
  public type

    TSuggestions = class(TParseSuggestionsSimple<TBrigadierItemEnchantmentParser>)
    public
      class function GetCount: Integer; override;
      class function GetSuggestion(AIndex: Integer): TParseSuggestion; override;

    end;

  protected
    function Parse: Boolean; override;

  public
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>A name of an existing team.</summary>
  TBrigadierTeam = class(TBrigadierArgumentParameter)
  private
    FName: string;

  public
    constructor Create(AArgument: TBrigadierArgument; AName: string); reintroduce; overload;

    property Name: string read FName write FName;

    function Format: string; override;
    
  end;

  TBrigadierTeamParser = class(TBrigadierParser<TBrigadierTeam>)
  protected
    function Parse: Boolean; override;

  public
    class function GetParserString: TNSPath; override;

  end;

implementation

{ TBrigadierBoolParser }

class function TBrigadierBoolParser.GetParserString: TNSPath;
begin
  Result := NSPath('brigadier', 'bool');
end;

class function TBrigadierBoolParser.GetResultName: string;
begin
  Result := 'Boolean';
end;

function TBrigadierBoolParser.Parse: Boolean;
var
  B: Boolean;
  Ident: string;
begin
  BeginSuggestions(TSuggestions);
  Ident := ReadWhile(IdentChars);
  for B := Low(Boolean) to High(Boolean) do
  begin
    if Ident = BoolStrings[B] then
    begin
      SetParseResult(TBrigadierBool.Create(Argument, B));
      Exit(True);
    end;
  end;
  Result := False;
end;

{ TBrigadierBool }

constructor TBrigadierBool.Create(AArgument: TBrigadierArgument; AValue: Boolean);
begin
  inherited Create(AArgument);
  FValue := AValue;
end;

function TBrigadierBool.Format: string;
begin
  Result := TBrigadierBoolParser.BoolStrings[Value];
end;

{ TBrigadierInteger }

constructor TBrigadierInteger.Create(AArgument: TBrigadierArgument; AValue: Integer);
begin
  inherited Create(AArgument);
  FValue := AValue;
end;

function TBrigadierInteger.Format: string;
begin
  Result := IntToStr(Value);
end;

{ TBrigadierIntegerProperties }

constructor TBrigadierIntegerProperties.Create(AProperties: TJSONObject);
var
  Node: TJSONNumber;
begin
  inherited;
  if (AProperties <> nil) and AProperties.TryGetValue<TJSONNumber>('min', Node) then
    FMin := Node.AsInt
  else
    FMin := Integer.MinValue;
  if (AProperties <> nil) and AProperties.TryGetValue<TJSONNumber>('max', Node) then
    FMax := Node.AsInt
  else
    FMax := Integer.MaxValue;
end;

{ TBrigadierIntegerParser }

class function TBrigadierIntegerParser.GetParserString: TNSPath;
begin
  Result := NSPath('brigadier', 'integer');
end;

function TBrigadierIntegerParser.Parse: Boolean;
var
  Marker: TLogMarker;
  Value: Integer;
begin
  Marker := GetMarker;

  if not TryStrToInt(ReadWhile(ValidChars), Value) then
    Exit(False);

  if Value < Properties.Min then
    Log(Marker, 'Value must be at least %s.', [PrettyFloat(Properties.Min)]);
  if Value > Properties.Max then
    Log(Marker, 'Value must be at most %s.', [PrettyFloat(Properties.Max)]);

  SetParseResult(TBrigadierInteger.Create(Argument, Value));

  Result := True;
end;

{ TBrigadierEntityParser }

class function TBrigadierEntityParser.GetParserString: TNSPath;
begin
  Result := 'entity';
end;

function TBrigadierEntityParser.Parse: Boolean;
var
  SelectorParser: TEntitySelector.TParser;
  Name: string;
  Marker: TLogMarker;
  Selector: TEntitySelector;
begin
  BeginSuggestions(TEntitySelector.TSuggestions);
  if StartsWith(TEntitySelector.Prefix, False) then
  begin
    Marker := GetMarker;

    SelectorParser := TEntitySelector.TParser.Create(Info, True);
    Selector := SelectorParser.OwnParseResult;
    SetParseResult(TBrigadierEntitySelector.Create(Argument, Selector));
    SelectorParser.Free;

    if (Properties.Amount = eaSingle) and Selector.AllowsMultiple then
      Log(Marker, 'The selector must not be able to match multiple entities.');

    if (Properties.EntityType = etPlayers) and not Selector.AllowsPlayersOnly then
      Log(Marker, 'The selector must only be able to match players.');
  end
  else
  begin
    Name := ReadWhile(IdentChars);
    if Name.IsEmpty then
      Exit(False);
    if Name.Length > UsernameMaxLength then
      Log(-Name.Length, 'Usernames can only be %d characters long.', [UsernameMaxLength]);

    SetParseResult(TBrigadierEntityPlayer.Create(Argument, Name));
  end;
  Result := True;
end;

{ TBrigadierEntityProperties }

constructor TBrigadierEntityProperties.Create(AProperties: TJSONObject);

  function AmountFromString(AName: string): TAmount;
  begin
    for Result := Low(TAmount) to High(TAmount) do
      if AName = AmountNames[Result] then
        Exit;
    raise EBrigadierProperties.CreateFmt('%s properties got an unknown amount "%s".',
      [TBrigadierEntityParser.GetParserString.Format, AName]);
  end;

  function TypeFromString(AName: string): TType;
  begin
    for Result := Low(TType) to High(TType) do
      if AName = TypeNames[Result] then
        Exit;
    raise EBrigadierProperties.CreateFmt('%s properties got an unknown type "%s".',
      [TBrigadierEntityParser.GetParserString.Format, AName]);
  end;

var
  Node: TJSONString;
begin
  inherited;
  if not AProperties.TryGetValue<TJSONString>('amount', Node) then
    raise EBrigadierProperties.CreateFmt('%s properties requires amount node.',
      [TBrigadierEntityParser.GetParserString.Format]);
  FAmount := AmountFromString(Node.Value);

  if not AProperties.TryGetValue<TJSONString>('type', Node) then
    raise EBrigadierProperties.CreateFmt('%s properties requires type node.',
      [TBrigadierEntityParser.GetParserString.Format]);
  FType := TypeFromString(Node.Value);
end;

{ TBrigadierEntitySelector }

constructor TBrigadierEntitySelector.Create(AArgument: TBrigadierArgument);
begin
  inherited;
  FSelector := TEntitySelector.Create;
end;

constructor TBrigadierEntitySelector.Create(AArgument: TBrigadierArgument; ASelector: TEntitySelector);
begin
  inherited Create(AArgument);
  FSelector := ASelector;
end;

destructor TBrigadierEntitySelector.Destroy;
begin
  FSelector.Free;
  inherited;
end;

function TBrigadierEntitySelector.Format: string;
begin
  Result := Selector.Format;
end;

{ TBrigadierEntityPlayer }

constructor TBrigadierEntityPlayer.Create(AArgument: TBrigadierArgument; AName: string);
begin
  inherited Create(AArgument);
  FName := AName;
end;

function TBrigadierEntityPlayer.Format: string;
begin
  Result := Name;
end;

{ TBrigadierBoolParser.TSuggestions }

class function TBrigadierBoolParser.TSuggestions.GetCount: Integer;
begin
  Result := Length(BoolStrings);
end;

class function TBrigadierBoolParser.TSuggestions.GetSuggestion(AIndex: Integer): TParseSuggestion;
begin
  Result := BoolStrings[Boolean(AIndex)];
end;

{ TBrigadierNBT }

constructor TBrigadierNBT.Create(AArgument: TBrigadierArgument);
begin
  inherited;
  FNBT := TNBTCompound.Create;
end;

constructor TBrigadierNBT.Create(AArgument: TBrigadierArgument; ANBT: TNBTCompound);
begin
  inherited Create(AArgument);
  FNBT := ANBT;
end;

destructor TBrigadierNBT.Destroy;
begin
  FNBT.Free;
  inherited;
end;

function TBrigadierNBT.Format: string;
begin
  Result := NBT.Format;
end;

{ TBrigadierNBTParser }

class function TBrigadierNBTParser.GetParserString: TNSPath;
begin
  Result := 'nbt';
end;

function TBrigadierNBTParser.Parse: Boolean;
var
  Parser: TNBTParserCompound;
begin
  Parser := TNBTParserCompound.Create(Info, False);
  Result := Parser.Success;
  if Result then
    SetParseResult(TBrigadierNBT.Create(Argument, Parser.OwnParseResult));
  Parser.Free;
end;

{ TBrigadierFloat }

constructor TBrigadierFloat.Create(AArgument: TBrigadierArgument; AValue: Single);
begin
  inherited Create(AArgument);
  FValue := AValue;
end;

function TBrigadierFloat.Format: string;
begin
  Result := PrettyFloat(Value);
end;

{ TBrigadierFloatProperties }

constructor TBrigadierFloatProperties.Create(AProperties: TJSONObject);
var
  Node: TJSONNumber;
begin
  inherited;
  if (AProperties <> nil) and AProperties.TryGetValue<TJSONNumber>('min', Node) then
    FMin := Node.AsDouble
  else
    FMin := -Infinity;
  if (AProperties <> nil) and AProperties.TryGetValue<TJSONNumber>('max', Node) then
    FMax := Node.AsDouble
  else
    FMax := Infinity;
end;

{ TBrigadierFloatParser }

class function TBrigadierFloatParser.GetParserString: TNSPath;
begin
  Result := NSPath('brigadier', 'float');
end;

function TBrigadierFloatParser.Parse: Boolean;
var
  Marker: TLogMarker;
  Value: Single;
begin
  Marker := GetMarker;

  if not TryStrToFloat(ReadWhile(ValidChars), Value, FormatSettings.Invariant) then
    Exit(False);

  if Value < Properties.Min then
    Log(Marker, 'Value must be at least %s.', [PrettyFloat(Properties.Min)]);
  if Value > Properties.Max then
    Log(Marker, 'Value must be at most %s.', [PrettyFloat(Properties.Max)]);

  SetParseResult(TBrigadierFloat.Create(Argument, Value));

  Result := True;
end;

{ TBrigadierDouble }

constructor TBrigadierDouble.Create(AArgument: TBrigadierArgument; AValue: Double);
begin
  inherited Create(AArgument);
  FValue := AValue;
end;

function TBrigadierDouble.Format: string;
begin
  Result := PrettyFloat(Value);
end;

{ TBrigadierDoubleProperties }

constructor TBrigadierDoubleProperties.Create(AProperties: TJSONObject);
var
  Node: TJSONNumber;
begin
  inherited;
  if (AProperties <> nil) and AProperties.TryGetValue<TJSONNumber>('min', Node) then
    FMin := Node.AsDouble
  else
    FMin := -Infinity;
  if (AProperties <> nil) and AProperties.TryGetValue<TJSONNumber>('max', Node) then
    FMax := Node.AsDouble
  else
    FMax := Infinity;
end;

{ TBrigadierDoubleParser }

class function TBrigadierDoubleParser.GetParserString: TNSPath;
begin
  Result := NSPath('brigadier', 'double');
end;

function TBrigadierDoubleParser.Parse: Boolean;
var
  Marker: TLogMarker;
  Value: Double;
begin
  Marker := GetMarker;

  if not TryStrToFloat(ReadWhile(ValidChars), Value, FormatSettings.Invariant) then
    Exit(False);

  if Value < Properties.Min then
    Log(Marker, 'Value must be at least %s.', [PrettyFloat(Properties.Min)]);
  if Value > Properties.Max then
    Log(Marker, 'Value must be at most %s.', [PrettyFloat(Properties.Max)]);

  SetParseResult(TBrigadierDouble.Create(Argument, Value));

  Result := True;
end;

{ TBrigadierString }

constructor TBrigadierString.Create(AArgument: TBrigadierArgument; AMode: TMode; AText: string);
begin
  inherited Create(AArgument);
  FMode := AMode;
  FText := AText;
end;

function TBrigadierString.Format: string;
begin
  case Mode of
    smWord, smGreedy:
      Result := Text;
    smPhrase:
      if ContainsOnly(Text, IdentChars) then
        Result := Text
      else
        Result := DblQuoted(Text);
  end;
end;

{ TBrigadierStringProperties }

constructor TBrigadierStringProperties.Create(AProperties: TJSONObject);
var
  Node: TJSONString;
  Mode: TBrigadierString.TMode;
begin
  inherited;
  if AProperties = nil then
    raise EBrigadierProperties.CreateFmt('Missing properties for %s.', [TBrigadierStringParser.GetParserString.Format]);
  if not AProperties.TryGetValue<TJSONString>('type', Node) then
    raise EBrigadierProperties.CreateFmt('Missing type node for %s.', [TBrigadierStringParser.GetParserString.Format]);

  for Mode := Low(TBrigadierString.TMode) to High(TBrigadierString.TMode) do
    if Node.Value = TBrigadierString.ModeNames[Mode] then
    begin
      FMode := Mode;
      Exit;
    end;

  raise EBrigadierProperties.CreateFmt('Unknown value as type for %s.',
    [TBrigadierStringParser.GetParserString.Format]);
end;

{ TBrigadierStringParser }

class function TBrigadierStringParser.GetParserString: TNSPath;
begin
  Result := NSPath('brigadier', 'string');
end;

function TBrigadierStringParser.Parse: Boolean;
var
  Parser: TStringOrIdentParser;
  Text: string;
begin
  case Properties.Mode of
    smWord:
      begin
        Text := ReadWhile(IdentChars);
        if Text.IsEmpty then
          Exit(False);
      end;
    smPhrase:
      begin
        Parser := TStringOrIdentParser.Create(Info, False);
        if not Parser.Success or Parser.IsIdent and Parser.ParseResult.IsEmpty then
        begin
          Parser.Free;
          Exit(False);
        end;
        Text := Parser.ParseResult;
        Parser.Free;
      end;
    smGreedy:
      begin
        Text := AllText;
        if Text.IsEmpty then
          Exit(False);
        AdvanceToEnd;
      end;
  end;
  SetParseResult(TBrigadierString.Create(Argument, Properties.Mode, Text));
  Result := True;
end;

{ TBrigadierNBTPath }

constructor TBrigadierNBTPath.Create(AArgument: TBrigadierArgument; APath: TNBTPath);
begin
  inherited Create(AArgument);
  FPath := APath;
end;

destructor TBrigadierNBTPath.Destroy;
begin
  FPath.Free;
  inherited;
end;

function TBrigadierNBTPath.Format: string;
begin
  Result := Path.Format;
end;

constructor TBrigadierNBTPath.Create(AArgument: TBrigadierArgument);
begin
  inherited;
  FPath := TNBTPath.Create;
end;

{ TBrigadierNBTPathParser }

class function TBrigadierNBTPathParser.GetParserString: TNSPath;
begin
  Result := 'minecraft:nbt_path';
end;

function TBrigadierNBTPathParser.Parse: Boolean;
var
  Parser: TNBTPathParser;
begin
  Parser := TNBTPathParser.Create(Info, False);
  Result := Parser.Success;
  if Result then
    SetParseResult(TBrigadierNBTPath.Create(Argument, Parser.OwnParseResult));
  Parser.Free;
end;

{ TBrigadierVec2 }

constructor TBrigadierVec2.Create(AArgument: TBrigadierArgument);
begin
  inherited;
  FVector := TMCVec2.Create;
end;

constructor TBrigadierVec2.Create(AArgument: TBrigadierArgument; AVector: TMCVec2);
begin
  inherited Create(AArgument);
  FVector := AVector;
end;

destructor TBrigadierVec2.Destroy;
begin
  FVector.Free;
  inherited;
end;

function TBrigadierVec2.Format: string;
begin
  Result := Vector.Format;
end;

{ TBrigadierVec2Parser }

class function TBrigadierVec2Parser.GetParserString: TNSPath;
begin
  Result := 'minecraft:vec2';
end;

function TBrigadierVec2Parser.Parse: Boolean;
var
  Parser: TMCVec2.TParser;
  Axis: TCoordAxis2;
  Marker: TLogMarker;
begin
  Marker := GetMarker;
  Parser := TMCVec2.TParser.Create(Info, False);
  Result := Parser.Success;
  if Result then
  begin
    SetParseResult(TBrigadierVec2.Create(Argument, Parser.OwnParseResult));
    for Axis := Low(TCoordAxis2) to High(TCoordAxis2) do
      if ParseResult.Vector.Values[Axis].Mode = vmLocal then
      begin
        Log(Marker, 'Local coordinates cannot be used for a rotation.');
        Break;
      end;
  end;
  Parser.Free;
end;

{ TBrigadierVec3 }

constructor TBrigadierVec3.Create(AArgument: TBrigadierArgument);
begin
  inherited;
  FVector := TMCVec3.Create;
end;

constructor TBrigadierVec3.Create(AArgument: TBrigadierArgument; AVector: TMCVec3);
begin
  inherited Create(AArgument);
  FVector := AVector;
end;

destructor TBrigadierVec3.Destroy;
begin
  FVector.Free;
  inherited;
end;

function TBrigadierVec3.Format: string;
begin
  Result := FVector.Format;
end;

{ TBrigadierVec3Parser }

class function TBrigadierVec3Parser.GetParserString: TNSPath;
begin
  Result := 'minecraft:vec3';
end;

function TBrigadierVec3Parser.Parse: Boolean;
var
  Parser: TMCVec3.TParser;
begin
  Parser := TMCVec3.TParser.Create(Info, False, False);
  Result := Parser.Success;
  if Result then
    SetParseResult(TBrigadierVec3.Create(Argument, Parser.OwnParseResult));
  Parser.Free;
end;

{ TBrigadierEntityAnchor }

constructor TBrigadierEntityAnchor.Create(AArgument: TBrigadierArgument; APosition: TPosition);
begin
  inherited Create(AArgument);
  FPosition := APosition;
end;

function TBrigadierEntityAnchor.Format: string;
begin
  Result := PositionStrings[Position];
end;

{ TBrigadierEntityAnchorParser.TSuggestions }

class function TBrigadierEntityAnchorParser.TSuggestions.GetCount: Integer;
begin
  Result := Length(TBrigadierEntityAnchor.PositionStrings);
end;

class function TBrigadierEntityAnchorParser.TSuggestions.GetSuggestion(AIndex: Integer): TParseSuggestion;
begin
  Result := TBrigadierEntityAnchor.PositionStrings[TBrigadierEntityAnchor.TPosition(AIndex)];
end;

{ TBrigadierEntityAnchorParser }

class function TBrigadierEntityAnchorParser.GetParserString: TNSPath;
begin
  Result := 'minecraft:entity_anchor';
end;

class function TBrigadierEntityAnchorParser.GetResultName: string;
begin
  Result := 'Entity-Anchor';
end;

function TBrigadierEntityAnchorParser.Parse: Boolean;
var
  Position: TBrigadierEntityAnchor.TPosition;
  Ident: string;
begin
  BeginSuggestions(TSuggestions);
  Ident := ReadWhile(IdentChars);
  for Position := Low(TBrigadierEntityAnchor.TPosition) to High(TBrigadierEntityAnchor.TPosition) do
  begin
    if Ident = TBrigadierEntityAnchor.PositionStrings[Position] then
    begin
      SetParseResult(TBrigadierEntityAnchor.Create(Argument, Position));
      Exit(True);
    end;
  end;
  Result := False;
end;

{ TBrigadierRotationParser }

class function TBrigadierRotationParser.GetParserString: TNSPath;
begin
  Result := 'rotation';
end;

{ TBrigadierSwizzle }

constructor TBrigadierSwizzle.Create(AArgument: TBrigadierArgument);
begin
  inherited;
  FSwizzle := TMCSwizzle.Create;
end;

constructor TBrigadierSwizzle.Create(AArgument: TBrigadierArgument; ASwizzle: TMCSwizzle);
begin
  inherited Create(AArgument);
  FSwizzle := ASwizzle;
end;

destructor TBrigadierSwizzle.Destroy;
begin
  FSwizzle.Free;
  inherited;
end;

function TBrigadierSwizzle.Format: string;
begin
  Result := Swizzle.Format;
end;

{ TBrigadierSwizzleParser }

class function TBrigadierSwizzleParser.GetParserString: TNSPath;
begin
  Result := 'swizzle';
end;

function TBrigadierSwizzleParser.Parse: Boolean;
var
  Parser: TMCSwizzle.TParser;
begin
  Parser := TMCSwizzle.TParser.Create(Info, False);
  Result := Parser.Success;
  if Result then
    SetParseResult(TBrigadierSwizzle.Create(Argument, Parser.OwnParseResult));
  Parser.Free;
end;

{ TBrigadierBlockPosParser }

class function TBrigadierBlockPosParser.GetParserString: TNSPath;
begin
  Result := 'minecraft:block_pos';
end;

function TBrigadierBlockPosParser.Parse: Boolean;
var
  Parser: TMCVec3.TParser;
begin
  Parser := TMCVec3.TParser.Create(Info, True, False);
  Result := Parser.Success;
  if Result then
    SetParseResult(TBrigadierVec3.Create(Argument, Parser.OwnParseResult));
  Parser.Free;
end;

{ TBrigadierGameProfile }

constructor TBrigadierGameProfile.Create(AArgument: TBrigadierArgument; AName: string);
begin
  inherited Create(AArgument);
  FName := AName;
end;

function TBrigadierGameProfile.Format: string;
begin
  Result := Name;
end;

{ TBrigadierGameProfileParser }

class function TBrigadierGameProfileParser.GetParserString: TNSPath;
begin
  Result := 'minecraft:game_profile';
end;

function TBrigadierGameProfileParser.Parse: Boolean;
var
  Name: string;
begin
  Name := ReadWhile(IdentChars);
  if Name.Length = 0 then
    Exit(False);
  if Name.Length > UsernameMaxLength then
    Log(-Name.Length, 'Usernames can only be %d characters long.', [UsernameMaxLength]);
  SetParseResult(TBrigadierGameProfile.Create(Argument, Name));
  Result := True;
end;

{ TBrigadierMessage }

constructor TBrigadierMessage.Create(AArgument: TBrigadierArgument);
begin
  inherited;
  FSections := TSections.Create;
end;

destructor TBrigadierMessage.Destroy;
begin
  FSections.Free;
  inherited;
end;

function TBrigadierMessage.Format: string;
var
  Section: TSection;
begin
  Result := '';
  for Section in Sections do
    Result := Result + Section.Format;
end;

{ TBrigadierMessage.TMessageSection }

constructor TBrigadierMessage.TTextSection.Create(AText: string);
begin
  FText := AText;
end;

function TBrigadierMessage.TTextSection.Format: string;
begin
  Result := Text;
end;

{ TBrigadierMessage.TSelectorSection }

constructor TBrigadierMessage.TSelectorSection.Create(ASelector: TEntitySelector);
begin
  FSelector := ASelector;
end;

destructor TBrigadierMessage.TSelectorSection.Destroy;
begin
  FSelector.Free;
  inherited;
end;

constructor TBrigadierMessage.TSelectorSection.Create;
begin
  FSelector := TEntitySelector.Create;
end;

function TBrigadierMessage.TSelectorSection.Format: string;
begin
  Result := Selector.Format;
end;

{ TBrigadierMessageParser }

class function TBrigadierMessageParser.GetParserString: TNSPath;
begin
  Result := 'message';
end;

class function TBrigadierMessageParser.KeepEndSuggestions: Boolean;
begin
  Result := True;
end;

function TBrigadierMessageParser.Parse: Boolean;
var
  Text: string;
  Parser: TEntitySelector.TParser;
begin
  BeginSuggestions(TEntitySelector.TSuggestions);

  if ReachedEnd then
    Exit(False);

  SetParseResult(TBrigadierMessage.Create(Argument));
  repeat
    Text := ReadUntil(['@']);
    if Text.Length > 0 then
      ParseResult.Sections.Add(TBrigadierMessage.TTextSection.Create(Text));

    if ReachedEnd then
      Break;

    Parser := TEntitySelector.TParser.Create(Info, True);
    ParseResult.Sections.Add(TBrigadierMessage.TSelectorSection.Create(Parser.OwnParseResult));
    Parser.Free;

    BeginSuggestions(TEntitySelector.TSuggestions);

  until ReachedEnd;

  Result := True;
end;

{ TBrigadierMobEffectParser }

class function TBrigadierMobEffectParser.GetParserString: TNSPath;
begin
  Result := 'minecraft:mob_effect';
end;

function TBrigadierMobEffectParser.Parse: Boolean;
begin
  Result := False;
end;

{ TFormatNamespaceSettings }

class function TFormatNamespaceSettings.GetDescription: string;
begin
  Result := Format(
    'Allows individual settings, as to wether the formatter should show or hide the default namespace "%s:".',
    [TNSPath.DefaultNamespace]);
end;

function TFormatNamespaceSettings.GetScope(AScope: TNamespaceScope): Boolean;
begin
  Result := AScope in FormattedScopes;
end;

class function TFormatNamespaceSettings.GetTitle: string;
begin
  Result := 'Default-Namespace Formatting';
end;

procedure TFormatNamespaceSettings.SetDefaults;
begin
  FormattedScopes := DefaultFormattedScopes;
end;

procedure TFormatNamespaceSettings.SetFormattedScopes(const Value: TNamespaceScopes);
begin
  if FormattedScopes = Value then
    Exit;
  FFormattedScopes := Value;
end;

procedure TFormatNamespaceSettings.SetScope(AScope: TNamespaceScope; const Value: Boolean);
begin
  if Value then
    FormattedScopes := FormattedScopes + [AScope]
  else
    FormattedScopes := FormattedScopes - [AScope];
end;

{ TBrigadierBlockState }

constructor TBrigadierBlockState.Create(AArgument: TBrigadierArgument);
begin
  inherited;
  FBlockState := TBlockState.Create;
end;

constructor TBrigadierBlockState.Create(AArgument: TBrigadierArgument; ABlockState: TBlockState);
begin
  inherited Create(AArgument);
  FBlockState := ABlockState;
end;

destructor TBrigadierBlockState.Destroy;
begin
  FBlockState.Free;
  inherited;
end;

function TBrigadierBlockState.Format: string;
begin
  Result := BlockState.Format;
end;

{ TBrigadierBlockStateParser }

class function TBrigadierBlockStateParser.GetParserString: TNSPath;
begin
  Result := 'block_state';
end;

function TBrigadierBlockStateParser.Parse: Boolean;
var
  Parser: TBlockState.TParser;
begin
  Parser := TBlockState.TParser.Create(Info, Settings, False);
  Result := Parser.Success;
  if Result then
    SetParseResult(TBrigadierBlockState.Create(Argument, Parser.OwnParseResult));
  Parser.Free;
end;

{ TBrigadierBlockPredicate }

constructor TBrigadierBlockPredicate.Create(AArgument: TBrigadierArgument);
begin
  inherited;
end;

constructor TBrigadierBlockPredicate.Create(AArgument: TBrigadierArgument; APredicate: TBlockState);
begin
  inherited Create(AArgument);
  FPredicate := APredicate;
end;

destructor TBrigadierBlockPredicate.Destroy;
begin
  FPredicate.Free;
  inherited;
end;

function TBrigadierBlockPredicate.Format: string;
begin
  Result := Predicate.Format;
end;

{ TBrigadierBlockPredicateParser }

class function TBrigadierBlockPredicateParser.GetParserString: TNSPath;
begin
  Result := 'block_predicate';
end;

function TBrigadierBlockPredicateParser.Parse: Boolean;
var
  BlockTagParser: TBlockStateTag.TParser;
  BlockStateParser: TBlockState.TParser;
begin
  if StartsWith('#', False) then
  begin
    BlockTagParser := TBlockStateTag.TParser.Create(Info, Settings, False);
    Result := BlockTagParser.Success;
    if Result then
      SetParseResult(TBrigadierBlockPredicate.Create(Argument, BlockTagParser.OwnParseResult));
    BlockTagParser.Free;
  end
  else
  begin
    BlockStateParser := TBlockState.TParser.Create(Info, Settings, False);
    Result := BlockStateParser.Success;
    if Result then
      SetParseResult(TBrigadierBlockPredicate.Create(Argument, BlockStateParser.OwnParseResult));
    BlockStateParser.Free;
  end;
end;

{ TBrigadierEntitySummon }

constructor TBrigadierEntitySummon.Create(AArgument: TBrigadierArgument; AEntity: TEntity);
begin
  inherited Create(AArgument);
  FEntity := AEntity;
end;

function TBrigadierEntitySummon.Format: string;
begin
  Result := EntityNames[Entity];
end;

{ TBrigadierEntitySummonParser }

class function TBrigadierEntitySummonParser.GetParserString: TNSPath;
begin
  Result := 'minecraft:entity_summon';
end;

function TBrigadierEntitySummonParser.Parse: Boolean;
var
  Name: string;
  Entity: TEntity;
begin
  BeginSuggestions(TSuggestions);
  Name := ReadWhile(IdentChars);
  EndSuggestions;

  if Name.IsEmpty then
    Exit(False);

  if not EntityFromName(Name, Entity) then
    Exit(False);

  SetParseResult(TBrigadierEntitySummon.Create(Argument, Entity));

  Result := True;
end;

{ TBrigadierEntitySummonParser.TSuggestions }

class function TBrigadierEntitySummonParser.TSuggestions.GetCount: Integer;
begin
  // Ignore first entry (player)
  Result := Length(EntityNames) - 1;
end;

class function TBrigadierEntitySummonParser.TSuggestions.GetSuggestion(AIndex: Integer): TParseSuggestion;
begin
  // Ignore first entry (player)
  Inc(AIndex);
  Result := ParseSuggestion(EntityDisplayNames[TEntity(AIndex)], EntityNames[TEntity(AIndex)]);
end;

{ TBrigadierItemEnchantment }

constructor TBrigadierItemEnchantment.Create(AArgument: TBrigadierArgument; AEnchantment: TEnchantment);
begin
  inherited Create(AArgument);
  FEnchantment := AEnchantment;
end;

function TBrigadierItemEnchantment.Format: string;
begin
  Result := EnchantmentNames[Enchantment];
end;

{ TBrigadierItemEnchantmentParser }

class function TBrigadierItemEnchantmentParser.GetParserString: TNSPath;
begin
  Result := 'minecraft:item_enchantment';
end;

function TBrigadierItemEnchantmentParser.Parse: Boolean;
var
  Name: string;
  Enchantment: TEnchantment;
begin
  BeginSuggestions(TSuggestions);
  Name := ReadWhile(IdentChars);
  EndSuggestions;

  if Name.IsEmpty then
    Exit(False);

  if not EnchantmentFromName(Name, Enchantment) then
    Exit(False);

  SetParseResult(TBrigadierItemEnchantment.Create(Argument, Enchantment));

  Result := True;
end;

{ TBrigadierItemEnchantmentParser.TSuggestions }

class function TBrigadierItemEnchantmentParser.TSuggestions.GetCount: Integer;
begin
  Result := Length(EnchantmentNames);
end;

class function TBrigadierItemEnchantmentParser.TSuggestions.GetSuggestion(AIndex: Integer): TParseSuggestion;
begin
  Result := ParseSuggestion(EnchantmentDisplayNames[TEnchantment(AIndex)], EnchantmentNames[TEnchantment(AIndex)]);
end;

{ TBrigadierTeamParser }

class function TBrigadierTeamParser.GetParserString: TNSPath;
begin
  Result := 'minecraft:team';
end;

function TBrigadierTeamParser.Parse: Boolean;
var
  Name: string;
begin
  Name := ReadWhile(IdentChars);
  if Name.IsEmpty then
    Exit(False);

  SetParseResult(TBrigadierTeam.Create(Argument, Name));
    
  Result := True;
end;

{ TBrigadierTeam }

constructor TBrigadierTeam.Create(AArgument: TBrigadierArgument; AName: string);
begin
  inherited Create(AArgument);
  FName := AName;
end;

function TBrigadierTeam.Format: string;
begin
  Result := Name;
end;

initialization

// Progress: 20 / 35
// /-----------------------------------\
// [|||||||||||||||||||||              ]
// \-----------------------------------/

// - Basic Types -

TBrigadierBoolParser.RegisterClass;
TBrigadierIntegerParser.RegisterClass;
TBrigadierFloatParser.RegisterClass;
TBrigadierDoubleParser.RegisterClass;
TBrigadierStringParser.RegisterClass;
// TBrigadierRangeParser.RegisterClass; // TODO: brigadier:range -> minecraft:int_range

// - Position related -

TBrigadierRotationParser.RegisterClass;
TBrigadierVec2Parser.RegisterClass;
TBrigadierVec3Parser.RegisterClass;
TBrigadierSwizzleParser.RegisterClass;
TBrigadierBlockPosParser.RegisterClass;
// TBrigadierDimensionParser.RegisterClass;
// TBrigadierColumnPos.RegisterClass;

// - Entity related -

TBrigadierEntityParser.RegisterClass;
TBrigadierEntityAnchorParser.RegisterClass;
TBrigadierGameProfileParser.RegisterClass;
TBrigadierMobEffectParser.RegisterClass;
// TBrigadierParticleParser.RegisterClass;
TBrigadierEntitySummonParser.RegisterClass;

// - NBT related -

TBrigadierNBTParser.RegisterClass;
TBrigadierNBTPathParser.RegisterClass;

// - Text related -

// TBrigadierComponentParser.RegisterClass; // big guy (json)
TBrigadierMessageParser.RegisterClass;

// - Scoreboard related -

// TBrigadierObjectiveParser.RegisterClass;
// TBrigadierObjectiveCriteriaParser.RegisterClass; // kindof big, split it up, or huuuge suggestion list
// TBrigadierScoreHolderParser.RegisterClass;
// TBrigadierScoreboardSlotParser.RegisterClass;
// TBrigadierOperationParser.RegisterClass;

// - Team related -

TBrigadierTeamParser.RegisterClass;
// TBrigadierColorParser.RegisterClass;

// - Item related -

// TBrigadierItemStackParser.RegisterClass; // big one
// TBrigadierItemPredicateParser.RegisterClass; // and this
TBrigadierItemEnchantmentParser.RegisterClass;
// TBrigadierItemSlotParser.RegisterClass;

// - Block related -

TBrigadierBlockPredicateParser.RegisterClass;
TBrigadierBlockStateParser.RegisterClass;

// - Resource related -

// TBrigadierResourceLocationParser.RegisterClass;
// TBrigadierFunctionParser.RegisterClass;

end.
