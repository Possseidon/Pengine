unit Pengine.MC.BrigadierParser;

interface

uses
  System.SysUtils,
  System.IOUtils,
  System.Math,
  System.Types,

  Pengine.IntMaths,
  Pengine.Vector,
  Pengine.Parsing,
  Pengine.Utility,
  Pengine.Collections,
  Pengine.Settings,
  Pengine.JSON,

  Pengine.MC.Brigadier,
  Pengine.MC.BlockState,
  Pengine.MC.Item,
  Pengine.MC.EntitySelector,
  Pengine.MC.General,
  Pengine.MC.NBT,
  Pengine.MC.Vector,
  Pengine.MC.Namespace,
  Pengine.MC.TextComponent,
  Pengine.MC.Scoreboard,
  Pengine.MC.Registries;

type

  // TODO: Replace most of the EParseError raisings with Log elFatal

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
    FBounds: TIntBounds1;

  public
    constructor Create(AProperties: TJObject); override;

    property Bounds: TIntBounds1 read FBounds write FBounds;
    property Min: Integer read FBounds.C1 write FBounds.C1;
    property Max: Integer read FBounds.C2 write FBounds.C2;

  end;

  TBrigadierIntegerParser = class(TBrigadierParser<TBrigadierInteger, TBrigadierIntegerProperties>)
  public const

    ValidChars = ['0' .. '9', '-'];

  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
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
    FBounds: TBounds1;

  public
    constructor Create(AProperties: TJObject); override;

    property Bounds: TBounds1 read FBounds write FBounds;
    property Min: Single read FBounds.C1 write FBounds.C1;
    property Max: Single read FBounds.C2 write FBounds.C2;

  end;

  TBrigadierFloatParser = class(TBrigadierParser<TBrigadierFloat, TBrigadierFloatProperties>)
  public const

    ValidChars = ['0' .. '9', '-', '.'];

  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
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
    constructor Create(AProperties: TJObject); override;

    property Min: Double read FMin write FMin;
    property Max: Double read FMax write FMax;

  end;

  TBrigadierDoubleParser = class(TBrigadierParser<TBrigadierDouble, TBrigadierDoubleProperties>)
  public const

    ValidChars = ['0' .. '9', '-', '.'];

  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
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
    constructor Create(AProperties: TJObject); override;

    property Mode: TBrigadierString.TMode read FMode write FMode;

  end;

  TBrigadierStringParser = class(TBrigadierParser<TBrigadierString, TBrigadierStringProperties>)
  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
    class function GetParserString: TNSPath; override;

  end;

  TBrigadierIntRange = class(TBrigadierArgumentParameter)
  private
    FBounds: TIntBounds1;

  public
    constructor Create(AArgument: TBrigadierArgument; ABounds: TIntBounds1); reintroduce; overload;

    property Bounds: TIntBounds1 read FBounds write FBounds;

    function Format: string; override;

  end;

  TBrigadierIntRangeParser = class(TBrigadierParser<TBrigadierIntRange>)
  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
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

  TBrigadierEntityUUID = class(TBrigadierEntity)
  private
    FUUID: TGUID;

  public
    constructor Create(AArgument: TBrigadierArgument; AUUID: TGUID); reintroduce; overload;

    property UUID: TGUID read FUUID write FUUID;

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
    constructor Create(AProperties: TJObject); override;

    property Amount: TAmount read FAmount write FAmount;
    property EntityType: TType read FType write FType;

  end;

  TBrigadierEntityParser = class(TBrigadierParser<TBrigadierEntity, TBrigadierEntityProperties>)
  public const

    TokenUsername = 1;
    TokenUUID = 2;

    TokenNames: array [TokenUsername .. TokenUUID] of string = (
      'Username',
      'UUID'
      );

  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
    class function GetParserString: TNSPath; override;
    class function GetTokenCount: Integer; override;
    class function GetTokenName(AIndex: Integer): string; override;

  end;

  TBrigadierScoreHolder = class(TBrigadierArgumentParameter);

  /// <summary>The placeholder "*", that stands for all entries of the context specific objective.</summary>
  TBrigadierScoreHolderAll = class(TBrigadierScoreHolder)
  public
    function Format: string; override;

  end;

  TBrigadierScoreHolderSelector = class(TBrigadierScoreHolder)
  private
    FSelector: TEntitySelector;

  public
    constructor Create(AArgument: TBrigadierArgument); overload; override;
    constructor Create(AArgument: TBrigadierArgument; ASelector: TEntitySelector); reintroduce; overload;
    destructor Destroy; override;

    property Selector: TEntitySelector read FSelector;

    function Format: string; override;

  end;

  TBrigadierScoreHolderText = class(TBrigadierScoreHolder)
  private
    FText: string;

  public
    constructor Create(AArgument: TBrigadierArgument; AText: string); reintroduce; overload;

    property Text: string read FText;

    function Format: string; override;

  end;

  TBrigadierScoreHolderParser = class(TBrigadierParser<TBrigadierScoreHolder>)
  public type

    TSuggestions = class(TParseSuggestionsSimple<TBrigadierScoreHolderParser>)
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

  /// <summary>The name on an objective.</summary>
  TBrigadierObjective = class(TBrigadierArgumentParameter)
  private
    FName: string;

  public
    constructor Create(AArgument: TBrigadierArgument; AName: string); reintroduce; overload;

    property Name: string read FName write FName;

    function Format: string; override;

  end;

  TBrigadierObjectiveParser = class(TBrigadierParser<TBrigadierObjective>)
  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
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

  TBrigadierNBTCompoundTagParser = class(TBrigadierParser<TBrigadierNBT>)
  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
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
    class function GetResultName: string; override;
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
    class function GetResultName: string; override;
    class function GetParserString: TNSPath; override;

  end;

  TBrigadierRotationParser = class(TBrigadierVec2Parser)
  public
    class function GetResultName: string; override;
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
    class function GetResultName: string; override;
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>A chunk pos must be integer if absolute.</summary>
  TBrigadierColumnPosParser = class(TBrigadierParser<TBrigadierVec2>)
  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>A block pos must be integer if absolute.</summary>
  TBrigadierBlockPosParser = class(TBrigadierParser<TBrigadierVec3>)
  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
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
    class function GetResultName: string; override;
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
    class function GetResultName: string; override;
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
    class function GetResultName: string; override;
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>A single potion effect.</summary>
  TBrigadierMobEffect = class(TBrigadierArgumentParameter)
  private
    FEffect: TNSPath;

  public
    constructor Create(AArgument: TBrigadierArgument; AEffect: TNSPath); reintroduce; overload;

    property Effect: TNSPath read FEffect write FEffect;

    function Format: string; override;

  end;

  TBrigadierMobEffectParser = class(TBrigadierParser<TBrigadierMobEffect>)
  public type

    TSuggestions = class(TParseSuggestionsGenerated<TBrigadierMobEffectParser>)
    protected
      procedure Generate; override;

    end;

  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
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
    class function GetResultName: string; override;
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>An item stack with nbt.</summary>
  TBrigadierItemStack = class(TBrigadierArgumentParameter)
  private
    FItemStack: TItemStack;

  public
    constructor Create(AArgument: TBrigadierArgument); overload; override;
    constructor Create(AArgument: TBrigadierArgument; AItemStack: TItemStack); reintroduce; overload;
    destructor Destroy; override;

    property ItemStack: TItemStack read FItemStack;

    function Format: string; override;

  end;

  TBrigadierItemStackParser = class(TBrigadierParser<TBrigadierItemStack>)
  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>An item predicate with nbt.</summary>
  /// <remarks>For the most part identical with item stacks, but can have tags.</remarks>
  TBrigadierItemPredicate = class(TBrigadierArgumentParameter)
  private
    FPredicate: TOwned<TItemStack>;

  public
    constructor Create(AArgument: TBrigadierArgument); overload; override;
    constructor Create(AArgument: TBrigadierArgument; APredicate: TItemStack); reintroduce; overload;

    /// <summary>Either an item stack or an item tag.</summary>
    property Predicate: TOwned<TItemStack> read FPredicate;

    function Format: string; override;

  end;

  TBrigadierItemPredicateParser = class(TBrigadierParser<TBrigadierItemPredicate>)
  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>A block predicate with properties and nbt.</summary>
  /// <remarks>For the most part identical with block states, but can have tags.</remarks>
  TBrigadierBlockPredicate = class(TBrigadierArgumentParameter)
  private
    FPredicate: TOwned<TBlockState>;

  public
    constructor Create(AArgument: TBrigadierArgument; APredicate: TBlockState); reintroduce; overload;

    /// <summary>Either a block state or a block tag.</summary>
    property Predicate: TOwned<TBlockState> read FPredicate;

    function Format: string; override;

  end;

  TBrigadierBlockPredicateParser = class(TBrigadierParser<TBrigadierBlockPredicate>)
  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>An entity used in the summon command.</summary>
  TBrigadierEntitySummon = class(TBrigadierArgumentParameter)
  private
    FEntity: TNSPath;

  public
    constructor Create(AArgument: TBrigadierArgument; AEntity: TNSPath); reintroduce; overload;

    property Entity: TNSPath read FEntity;

    function Format: string; override;

  end;

  TBrigadierEntitySummonParser = class(TBrigadierParser<TBrigadierEntitySummon>)
  public type

    TSuggestions = class(TParseSuggestionsGenerated<TBrigadierEntitySummonParser>)
    protected
      procedure Generate; override;

    end;

  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>An entity used in the summon command.</summary>
  TBrigadierItemEnchantment = class(TBrigadierArgumentParameter)
  private
    FEnchantment: TNSPath;

  public
    constructor Create(AArgument: TBrigadierArgument; AEnchantment: TNSPath); reintroduce; overload;

    property Enchantment: TNSPath read FEnchantment;

    function Format: string; override;

  end;

  TBrigadierItemEnchantmentParser = class(TBrigadierParser<TBrigadierItemEnchantment>)
  public type

    TSuggestions = class(TParseSuggestionsGenerated<TBrigadierItemEnchantmentParser>)
    protected
      procedure Generate; override;

    end;

  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
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
    class function GetResultName: string; override;
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>A dimension.</summary>
  TBrigadierDimension = class(TBrigadierArgumentParameter)
  private
    FDimension: TNSPath;

  public
    constructor Create(AArgument: TBrigadierArgument; ADimension: TNSPath); reintroduce; overload;

    property Dimension: TNSPath read FDimension;

    function Format: string; override;

  end;

  TBrigadierDimensionParser = class(TBrigadierParser<TBrigadierDimension>)
  public type

    TSuggestions = class(TParseSuggestionsGenerated<TBrigadierDimensionParser>)
    protected
      procedure Generate; override;

    end;

  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>A json text component used in commands like tellraw and such.</summary>
  TBrigadierComponent = class(TBrigadierArgumentParameter)
  private
    FJSON: TOwned<TJBase>;

  public
    constructor Create(AArgument: TBrigadierArgument); overload; override;
    constructor Create(AArgument: TBrigadierArgument; AJSON: TJBase); reintroduce; overload;

    property JSON: TOwned<TJBase> read FJSON;

    function Format: string; override;

  end;

  TBrigadierComponentParser = class(TBrigadierParser<TBrigadierComponent>)
  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>Basically a namespace path, used in various commands.</summary>
  TBrigadierResourceLocation = class(TBrigadierArgumentParameter)
  private
    FNSPath: TNSPath;

  public
    constructor Create(AArgument: TBrigadierArgument; ANSPath: TNSPath); reintroduce; overload;

    property NSPath: TNSPath read FNSPath write FNSPath;

    function Format: string; override;

  end;

  TBrigadierResourceLocationParser = class(TBrigadierParser<TBrigadierResourceLocation>)
  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>A namespace path to an mcfunction.</summary>
  TBrigadierFunction = class(TBrigadierResourceLocation);

  TBrigadierFunctionParser = class(TBrigadierParser<TBrigadierFunction>)
  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>A particle type used in the particle command.</summary>
  TBrigadierParticle = class(TBrigadierArgumentParameter)
  private
    FParticle: TNSPath;

  public
    constructor Create(AArgument: TBrigadierArgument; AParticle: TNSPath); reintroduce; overload;

    property Particle: TNSPath read FParticle write FParticle;

    function Format: string; override;

  end;

  TBrigadierParticleParser = class(TBrigadierParser<TBrigadierParticle>)
  public type

    TSuggestions = class(TParseSuggestionsGenerated<TBrigadierParticleParser>)
    protected
      procedure Generate; override;

    end;

  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>A color used for teams, bossbars, in text-components and such.</summary>
  TBrigadierColor = class(TBrigadierArgumentParameter)
  private
    FColor: TMCColor;

  public
    constructor Create(AArgument: TBrigadierArgument; AColor: TMCColor); reintroduce; overload;

    property Color: TMCColor read FColor write FColor;

    function Format: string; override;

  end;

  TBrigadierColorParser = class(TBrigadierParser<TBrigadierColor>)
  public type

    TSuggestions = class(TParseSuggestionsSimple<TBrigadierColorParser>)
    public
      class function GetCount: Integer; override;
      class function GetSuggestion(AIndex: Integer): TParseSuggestion; override;

    end;

  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
    class function GetParserString: TNSPath; override;
    class function GetTokenCount: Integer; override;
    class function GetTokenName(AIndex: Integer): string; override;

  end;

  /// <summary>A slot, to display a scoreboard objective.</summary>
  TBrigadierScoreboardSlot = class(TBrigadierArgumentParameter)
  private
    FScoreboardSlot: TScoreboardSlot;

  public
    constructor Create(AArgument: TBrigadierArgument; AScoreboardSlot: TScoreboardSlot); reintroduce; overload;
    destructor Destroy; override;

    property ScoreboardSlot: TScoreboardSlot read FScoreboardSlot;

    function Format: string; override;

  end;

  TBrigadierScoreboardSlotParser = class(TBrigadierParser<TBrigadierScoreboardSlot>)
  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
    class function GetParserString: TNSPath; override;

  end;

  TBrigadierOperation = class(TBrigadierArgumentParameter)
  private
    FOperation: TScoreboardOperation;

  public
    constructor Create(AArgument: TBrigadierArgument; AOperation: TScoreboardOperation); reintroduce; overload;

    property Operation: TScoreboardOperation read FOperation write FOperation;

    function Format: string; override;

  end;

  TBrigadierOperationParser = class(TBrigadierParser<TBrigadierOperation>)
  public type

    TSuggestions = class(TParseSuggestionsSimple<TBrigadierOperationParser>)
    public
      class function GetCount: Integer; override;
      class function GetSuggestion(AIndex: Integer): TParseSuggestion; override;

      class function GetBreakChars: TSysCharSet; override;

    end;

  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>A slot in the form "<c>container.42</c>" with optional number or string index.</summary>
  TBrigadierItemSlot = class(TBrigadierArgumentParameter)
  private
    FSlot: TOwned<TItemSlot>;

  public
    constructor Create(AArgument: TBrigadierArgument); overload; override;
    constructor Create(AArgument: TBrigadierArgument; ASlot: TItemSlot); reintroduce; overload;

    property Slot: TOwned<TItemSlot> read FSlot;

    function Format: string; override;

  end;

  TBrigadierItemSlotParser = class(TBrigadierParser<TBrigadierItemSlot>)
  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
    class function GetParserString: TNSPath; override;

  end;

  /// <summary>A criteria for a scoreboard objective.</summary>
  TBrigadierObjectiveCriteria = class(TBrigadierArgumentParameter)
  private
    FCriteria: TOwned<TScoreboardCriteria>;

  public
    constructor Create(AArgument: TBrigadierArgument; ACriteria: TScoreboardCriteria); reintroduce; overload;

    property Criteria: TOwned<TScoreboardCriteria> read FCriteria;

    function Format: string; override;

  end;

  TBrigadierObjectiveCriteriaParser = class(TBrigadierParser<TBrigadierObjectiveCriteria>)
  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
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
      ParseResult := TBrigadierBool.Create(Argument, B);
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

constructor TBrigadierIntegerProperties.Create(AProperties: TJObject);
begin
  inherited;
  Min := AProperties['min'] or Integer.MinValue;
  Max := AProperties['max'] or Integer.MaxValue;
end;

{ TBrigadierIntegerParser }

class function TBrigadierIntegerParser.GetParserString: TNSPath;
begin
  Result := NSPath('brigadier', 'integer');
end;

class function TBrigadierIntegerParser.GetResultName: string;
begin
  Result := 'Integer';
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

  ParseResult := TBrigadierInteger.Create(Argument, Value);

  Result := True;
end;

{ TBrigadierEntityParser }

class function TBrigadierEntityParser.GetParserString: TNSPath;
begin
  Result := 'entity';
end;

class function TBrigadierEntityParser.GetResultName: string;
begin
  Result := 'Entity';
end;

class function TBrigadierEntityParser.GetTokenCount: Integer;
begin
  Result := Length(TokenNames);
end;

class function TBrigadierEntityParser.GetTokenName(AIndex: Integer): string;
begin
  Result := TokenNames[AIndex];
end;

function TBrigadierEntityParser.Parse: Boolean;
var
  Name: string;
  Marker: TLogMarker;
  Selector: TEntitySelector;
  UUIDBytes: array [0 .. 15] of Byte;
  I: Integer;
begin
  BeginSuggestions(TEntitySelector.TSuggestions);
  if StartsWith(TEntitySelector.Prefix, False) then
  begin
    Marker := GetMarker;

    Selector := TEntitySelector.Parser.Require(Info);
    ParseResult := TBrigadierEntitySelector.Create(Argument, Selector);

    if (Properties.Amount = eaSingle) and Selector.AllowsMultiple then
      Log(Marker, 'The selector must not be able to match multiple entities.');

    if (Properties.EntityType = etPlayers) and not Selector.AllowsPlayersOnly and (Selector.Variable <> svSender) then
      Log(Marker, 'The selector must only be able to match players.');
  end
  else
  begin
    Marker := GetMarker;
    Name := ReadWhile(IdentChars, False);
    if Name.Length > UsernameMaxLength then
    begin
      Token := TokenUUID;
      Advance(Name.Length);
      Name := Name.Replace('-', '');
      FillChar(UUIDBytes, SizeOf(UUIDBytes), 0);
      if Name.Length <> 32 then
      begin
        Log(Marker, 'Invalid UUID length.', elFatal);
      end
      else
      begin
        for I := 0 to 15 do
        begin
          if not Byte.TryParse('$' + Name.Substring(I * 2, 2), UUIDBytes[I]) then
          begin
            Log(Marker, 'Invalid UUID.', elFatal);
            Break;
          end;
        end;
      end;

      ParseResult := TBrigadierEntityUUID.Create(Argument, TGUID.Create(UUIDBytes, TEndian.Big));
      Exit(True);
    end;

    Token := TokenUsername;
    Advance(Name.Length);
    if Name.IsEmpty then
      Exit(False);

    ParseResult := TBrigadierEntityPlayer.Create(Argument, Name);
  end;
  Result := True;
end;

{ TBrigadierEntityProperties }

constructor TBrigadierEntityProperties.Create(AProperties: TJObject);

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

begin
  inherited;
  FAmount := AmountFromString(AProperties['amount'].AsString);
  FType := TypeFromString(AProperties['type'].AsString);
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

class function TBrigadierNBTCompoundTagParser.GetParserString: TNSPath;
begin
  Result := 'nbt_compound_tag';
end;

class function TBrigadierNBTCompoundTagParser.GetResultName: string;
begin
  Result := 'NBT-Compound';
end;

function TBrigadierNBTCompoundTagParser.Parse: Boolean;
var
  Parser: TNBTCompound.IParser;
begin
  Parser := TNBTCompound.Parser;
  Parser.Parse(Info, False);
  Result := Parser.Success;
  if Result then
    ParseResult := TBrigadierNBT.Create(Argument, Parser.OwnParseResult);
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

constructor TBrigadierFloatProperties.Create(AProperties: TJObject);
begin
  inherited;
  Min := AProperties['min'] or -Infinity;
  Max := AProperties['max'] or +Infinity;
end;

{ TBrigadierFloatParser }

class function TBrigadierFloatParser.GetParserString: TNSPath;
begin
  Result := NSPath('brigadier', 'float');
end;

class function TBrigadierFloatParser.GetResultName: string;
begin
  Result := 'Float';
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

  ParseResult := TBrigadierFloat.Create(Argument, Value);

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

constructor TBrigadierDoubleProperties.Create(AProperties: TJObject);
begin
  inherited;
  Min := AProperties['min'] or -Infinity;
  Max := AProperties['max'] or +Infinity;
end;

{ TBrigadierDoubleParser }

class function TBrigadierDoubleParser.GetParserString: TNSPath;
begin
  Result := NSPath('brigadier', 'double');
end;

class function TBrigadierDoubleParser.GetResultName: string;
begin
  Result := 'Double';
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

  ParseResult := TBrigadierDouble.Create(Argument, Value);

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

constructor TBrigadierStringProperties.Create(AProperties: TJObject);

  function ModeFromString(AMode: string): TBrigadierString.TMode;
  begin
    for Result := Low(TBrigadierString.TMode) to High(TBrigadierString.TMode) do
      if AMode = TBrigadierString.ModeNames[Result] then
        Exit;
    raise EBrigadierProperties.CreateFmt('%s properties got an unknown mode "%s".',
      [TBrigadierEntityParser.GetParserString.Format, AMode]);
  end;

begin
  inherited;
  FMode := ModeFromString(AProperties['type'].AsString);
end;

{ TBrigadierStringParser }

class function TBrigadierStringParser.GetParserString: TNSPath;
begin
  Result := NSPath('brigadier', 'string');
end;

class function TBrigadierStringParser.GetResultName: string;
begin
  Result := 'String';
end;

function TBrigadierStringParser.Parse: Boolean;
var
  Parser: TNBTString.IStringOrIdentParser;
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
        Parser := TNBTString.StringOrIdentParser;
        Parser.Parse(Info, False);
        if not Parser.Success or Parser.IsIdent and Parser.ParseResult.IsEmpty then
          Exit(False);
        Text := Parser.ParseResult;
      end;
    smGreedy:
      begin
        Text := AllText;
        if Text.IsEmpty then
          Exit(False);
        AdvanceToEnd;
      end;
  end;
  ParseResult := TBrigadierString.Create(Argument, Properties.Mode, Text);
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
  Result := 'nbt_path';
end;

class function TBrigadierNBTPathParser.GetResultName: string;
begin
  Result := 'NBT-Path';
end;

function TBrigadierNBTPathParser.Parse: Boolean;
var
  Parser: TNBTPath.IParser;
begin
  Parser := TNBTPath.Parser;
  Parser.Parse(Info, False);
  Result := Parser.Success;
  if Result then
    ParseResult := TBrigadierNBTPath.Create(Argument, Parser.OwnParseResult);
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
  Result := 'vec2';
end;

class function TBrigadierVec2Parser.GetResultName: string;
begin
  Result := 'Vec2';
end;

function TBrigadierVec2Parser.Parse: Boolean;
var
  Parser: TMCVec2.IParser;
  Axis: TCoordAxis2;
  Marker: TLogMarker;
begin
  Marker := GetMarker;
  Parser := TMCVec2.Parser;
  Parser.Parse(Info, False);
  Result := Parser.Success;
  if Result then
  begin
    ParseResult := TBrigadierVec2.Create(Argument, Parser.OwnParseResult);
    for Axis := Low(TCoordAxis2) to High(TCoordAxis2) do
      if ParseResult.Vector.Values[Axis].Mode = vmLocal then
      begin
        Log(Marker, 'Local coordinates cannot be used for a rotation.');
        Break;
      end;
  end;
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
  Result := 'vec3';
end;

class function TBrigadierVec3Parser.GetResultName: string;
begin
  Result := 'Vec3';
end;

function TBrigadierVec3Parser.Parse: Boolean;
var
  Parser: TMCVec3.IParser;
begin
  Parser := TMCVec3.Parser;
  Parser.Parse(Info, False);
  Result := Parser.Success;
  if Result then
    ParseResult := TBrigadierVec3.Create(Argument, Parser.OwnParseResult);
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
  Result := 'entity_anchor';
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
      ParseResult := TBrigadierEntityAnchor.Create(Argument, Position);
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

class function TBrigadierRotationParser.GetResultName: string;
begin
  Result := 'Rotation';
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

class function TBrigadierSwizzleParser.GetResultName: string;
begin
  Result := 'Coordinate-Swizzle';
end;

function TBrigadierSwizzleParser.Parse: Boolean;
var
  Parser: TMCSwizzle.IParser;
begin
  Parser := TMCSwizzle.Parser;
  Parser.Parse(Info, False);
  Result := Parser.Success;
  if Result then
    ParseResult := TBrigadierSwizzle.Create(Argument, Parser.OwnParseResult);
end;

{ TBrigadierBlockPosParser }

class function TBrigadierBlockPosParser.GetParserString: TNSPath;
begin
  Result := 'block_pos';
end;

class function TBrigadierBlockPosParser.GetResultName: string;
begin
  Result := 'Block-Pos';
end;

function TBrigadierBlockPosParser.Parse: Boolean;
var
  Parser: TMCVec3.IParser;
begin
  Parser := TMCVec3.Parser;
  Parser.BlockPos := True;
  Parser.Parse(Info, False);
  Result := Parser.Success;
  if Result then
    ParseResult := TBrigadierVec3.Create(Argument, Parser.OwnParseResult);
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
  Result := 'game_profile';
end;

class function TBrigadierGameProfileParser.GetResultName: string;
begin
  Result := 'Game-Profile';
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
  ParseResult := TBrigadierGameProfile.Create(Argument, Name);
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

class function TBrigadierMessageParser.GetResultName: string;
begin
  Result := 'Message';
end;

class function TBrigadierMessageParser.KeepEndSuggestions: Boolean;
begin
  Result := True;
end;

function TBrigadierMessageParser.Parse: Boolean;
var
  Text: string;
  Selector: TEntitySelector;
begin
  BeginSuggestions(TEntitySelector.TSuggestions);

  if ReachedEnd then
    Exit(False);

  ParseResult := TBrigadierMessage.Create(Argument);
  repeat
    Text := ReadUntil(['@']);
    if Text.Length > 0 then
      ParseResult.Sections.Add(TBrigadierMessage.TTextSection.Create(Text));

    if ReachedEnd then
      Break;

    Selector := TEntitySelector.Parser.Require(Info);
    ParseResult.Sections.Add(TBrigadierMessage.TSelectorSection.Create(Selector));

    BeginSuggestions(TEntitySelector.TSuggestions);

  until ReachedEnd;

  Result := True;
end;

{ TBrigadierMobEffectParser }

class function TBrigadierMobEffectParser.GetParserString: TNSPath;
begin
  Result := 'mob_effect';
end;

class function TBrigadierMobEffectParser.GetResultName: string;
begin
  Result := 'Mob-Effect';
end;

function TBrigadierMobEffectParser.Parse: Boolean;
var
  Effect: TNSPath;
  Marker: TLogMarker;
begin
  Marker := GetMarker;

  BeginSuggestions(TSuggestions.Create);
  Effect := ReadWhile(NamespacePathChars);
  EndSuggestions;

  if not MCRegistries.MobEffect.Has(Effect) then
    Log(Marker, 'Invalid mob effect "%s".', [Effect.Format]);

  ParseResult := TBrigadierMobEffect.Create(Argument, Effect);

  Result := True;
end;

{ TFormatNamespaceSettings }

class function TFormatNamespaceSettings.GetDescription: string;
begin
  Result := Format(
    'Allows individual settings, as to whether the formatter should show or hide the default namespace "%s:".',
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

class function TBrigadierBlockStateParser.GetResultName: string;
begin
  Result := 'Block-State';
end;

function TBrigadierBlockStateParser.Parse: Boolean;
var
  Parser: TBlockState.IParser;
begin
  Parser := TBlockState.Parser;
  Parser.Parse(Info, False);
  Result := Parser.Success;
  if Result then
    ParseResult := TBrigadierBlockState.Create(Argument, Parser.OwnParseResult);
end;

{ TBrigadierBlockPredicate }

constructor TBrigadierBlockPredicate.Create(AArgument: TBrigadierArgument; APredicate: TBlockState);
begin
  inherited Create(AArgument);
  FPredicate := TOwned<TBlockState>.Create(APredicate);
end;

function TBrigadierBlockPredicate.Format: string;
begin
  Result := Predicate.Value.Format;
end;

{ TBrigadierBlockPredicateParser }

class function TBrigadierBlockPredicateParser.GetParserString: TNSPath;
begin
  Result := 'block_predicate';
end;

class function TBrigadierBlockPredicateParser.GetResultName: string;
begin
  Result := 'Block-Predicate';
end;

function TBrigadierBlockPredicateParser.Parse: Boolean;
var
  BlockTagParser: TBlockStateTag.IParser;
  BlockStateParser: TBlockState.IParser;
begin
  if StartsWith('#', False) then
  begin
    BlockTagParser := TBlockStateTag.Parser;
    BlockTagParser.Parse(Info, False);
    Result := BlockTagParser.Success;
    if Result then
      ParseResult := TBrigadierBlockPredicate.Create(Argument, BlockTagParser.OwnParseResult);
  end
  else
  begin
    BlockStateParser := TBlockState.Parser;
    BlockStateParser.Parse(Info, False);
    Result := BlockStateParser.Success;
    if Result then
      ParseResult := TBrigadierBlockPredicate.Create(Argument, BlockStateParser.OwnParseResult);
  end;
end;

{ TBrigadierEntitySummon }

constructor TBrigadierEntitySummon.Create(AArgument: TBrigadierArgument; AEntity: TNSPath);
begin
  inherited Create(AArgument);
  FEntity := AEntity;
end;

function TBrigadierEntitySummon.Format: string;
begin
  Result := Entity.Format;
end;

{ TBrigadierEntitySummonParser }

class function TBrigadierEntitySummonParser.GetParserString: TNSPath;
begin
  Result := 'entity_summon';
end;

class function TBrigadierEntitySummonParser.GetResultName: string;
begin
  Result := 'Entity-Type';
end;

function TBrigadierEntitySummonParser.Parse: Boolean;
var
  Entity: TNSPath;
  Marker: TLogMarker;
begin
  Marker := GetMarker;

  BeginSuggestions(TSuggestions.Create);
  Entity := ReadWhile(NamespacePathChars);
  EndSuggestions;

  if not MCRegistries.EntityType.Has(Entity) then
    Log(Marker, 'Invalid entity type "%s".', [Entity.Format]);

  ParseResult := TBrigadierEntitySummon.Create(Argument, Entity);

  Result := True;
end;

{ TBrigadierItemEnchantment }

constructor TBrigadierItemEnchantment.Create(AArgument: TBrigadierArgument; AEnchantment: TNSPath);
begin
  inherited Create(AArgument);
  FEnchantment := AEnchantment;
end;

function TBrigadierItemEnchantment.Format: string;
begin
  Result := Enchantment;
end;

{ TBrigadierItemEnchantmentParser }

class function TBrigadierItemEnchantmentParser.GetParserString: TNSPath;
begin
  Result := 'item_enchantment';
end;

class function TBrigadierItemEnchantmentParser.GetResultName: string;
begin
  Result := 'Item-Enchantment';
end;

function TBrigadierItemEnchantmentParser.Parse: Boolean;
var
  Enchantment: TNSPath;
  Marker: TLogMarker;
begin
  Marker := GetMarker;

  BeginSuggestions(TSuggestions.Create);
  Enchantment := ReadWhile(NamespacePathChars);
  EndSuggestions;

  if not MCRegistries.Enchantment.Has(Enchantment) then
    Log(Marker, 'Invalid enchantment "%s".', [Enchantment.Format]);

  ParseResult := TBrigadierItemEnchantment.Create(Argument, Enchantment);

  Result := True;
end;

{ TBrigadierItemEnchantmentParser.TSuggestions }

procedure TBrigadierItemEnchantmentParser.TSuggestions.Generate;
var
  Enchantment: TNSPath;
begin
  for Enchantment in MCRegistries.Enchantment do
    AddSuggestion(Enchantment.Format(False));
  AddSuggestion(TNSPath.Empty.Format);
  for Enchantment in MCRegistries.Enchantment do
    AddSuggestion(Enchantment.Format);
end;

{ TBrigadierTeamParser }

class function TBrigadierTeamParser.GetParserString: TNSPath;
begin
  Result := 'team';
end;

class function TBrigadierTeamParser.GetResultName: string;
begin
  Result := 'Team';
end;

function TBrigadierTeamParser.Parse: Boolean;
var
  Name: string;
begin
  Name := ReadWhile(IdentChars);
  if Name.IsEmpty then
    Exit(False);

  ParseResult := TBrigadierTeam.Create(Argument, Name);

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

{ TBrigadierDimension }

constructor TBrigadierDimension.Create(AArgument: TBrigadierArgument; ADimension: TNSPath);
begin
  inherited Create(AArgument);
  FDimension := ADimension;
end;

function TBrigadierDimension.Format: string;
begin
  Result := Dimension;
end;

{ TBrigadierDimensionParser }

class function TBrigadierDimensionParser.GetParserString: TNSPath;
begin
  Result := 'dimension';
end;

class function TBrigadierDimensionParser.GetResultName: string;
begin
  Result := 'Dimension';
end;

function TBrigadierDimensionParser.Parse: Boolean;
var
  Marker: TLogMarker;
  Dimension: TNSPath;
begin
  Marker := GetMarker;

  BeginSuggestions(TSuggestions.Create);
  Dimension := ReadWhile(NamespacePathChars);
  EndSuggestions;

  if not MCRegistries.DimensionType.Has(Dimension) then
    Log(Marker, 'Invalid dimension "%s".', [Dimension.Format]);

  ParseResult := TBrigadierDimension.Create(Argument, Dimension);

  Result := True;
end;

{ TBrigadierColumnPosParser }

class function TBrigadierColumnPosParser.GetParserString: TNSPath;
begin
  Result := 'column_pos';
end;

class function TBrigadierColumnPosParser.GetResultName: string;
begin
  Result := 'Column-Pos';
end;

function TBrigadierColumnPosParser.Parse: Boolean;
var
  Marker: TLogMarker;
  Parser: TMCVec2.IParser;
  Axis: TCoordAxis2;
begin
  Marker := GetMarker;
  Parser := TMCVec2.Parser;
  Parser.ChunkPos := True;
  Parser.Parse(Info, False);
  Result := Parser.Success;
  if Result then
  begin
    ParseResult := TBrigadierVec2.Create(Argument, Parser.OwnParseResult);
    for Axis := Low(TCoordAxis2) to High(TCoordAxis2) do
      if ParseResult.Vector.Values[Axis].Mode <> vmAbsolute then
      begin
        Log(Marker, 'Chunk coordinates can only be absolute.');
        Break;
      end;
  end;
end;

{ TBrigadierItemStack }

constructor TBrigadierItemStack.Create(AArgument: TBrigadierArgument);
begin
  inherited;
  FItemStack := TItemStack.Create;
end;

constructor TBrigadierItemStack.Create(AArgument: TBrigadierArgument; AItemStack: TItemStack);
begin
  inherited Create(AArgument);
  FItemStack := AItemStack;
end;

destructor TBrigadierItemStack.Destroy;
begin
  FItemStack.Free;
  inherited;
end;

function TBrigadierItemStack.Format: string;
begin
  Result := ItemStack.Format;
end;

{ TBrigadierItemStackParser }

class function TBrigadierItemStackParser.GetParserString: TNSPath;
begin
  Result := 'item_stack';
end;

class function TBrigadierItemStackParser.GetResultName: string;
begin
  Result := 'Item-Stack';
end;

function TBrigadierItemStackParser.Parse: Boolean;
var
  Parser: TItemStack.IParser;
begin
  Parser := TItemStack.Parser;
  Parser.Parse(Info, False);
  Result := Parser.Success;
  if Result then
    ParseResult := TBrigadierItemStack.Create(Argument, Parser.OwnParseResult);
end;

{ TBrigadierScoreHolderParser }

class function TBrigadierScoreHolderParser.GetParserString: TNSPath;
begin
  Result := 'score_holder';
end;

class function TBrigadierScoreHolderParser.GetResultName: string;
begin
  Result := 'Score-Holder';
end;

function TBrigadierScoreHolderParser.Parse: Boolean;
var
  Text: string;
  Marker: TLogMarker;
  Selector: TEntitySelector;
begin
  BeginSuggestions(TSuggestions);
  Marker := GetMarker;

  if StartsWith(TEntitySelector.Prefix, False) then
  begin
    Selector := TEntitySelector.Parser.Require(Info);
    ParseResult := TBrigadierScoreHolderSelector.Create(Argument, Selector);
    Exit(True);
  end;

  Text := ReadUntil([' ']);

  if Text.IsEmpty then
    Exit(False);

  if Text = '*' then
  begin
    ParseResult := TBrigadierScoreHolderAll.Create(Argument);
    Exit(True);
  end;

  if Text.Length > 40 then
    Log(Marker, 'The score holder can be at most 40 characters long.');

  ParseResult := TBrigadierScoreHolderText.Create(Argument, Text);
  Result := True;
end;

{ TBrigadierScoreHolderParser.TSuggestions }

class function TBrigadierScoreHolderParser.TSuggestions.GetCount: Integer;
begin
  Result := Length(TEntitySelector.VariableChars) + 1;
end;

class function TBrigadierScoreHolderParser.TSuggestions.GetSuggestion(AIndex: Integer): TParseSuggestion;
begin
  if AIndex = 0 then
    Result.Create('* (All Score-Holders)', '*')
  else
    Result := TEntitySelector.Prefix + TEntitySelector.VariableChars[TEntitySelector.TVariable(AIndex - 1)];
end;

{ TBrigadierObjective }

constructor TBrigadierObjective.Create(AArgument: TBrigadierArgument; AName: string);
begin
  inherited Create(AArgument);
  FName := AName;
end;

function TBrigadierObjective.Format: string;
begin
  Result := Name;
end;

{ TBrigadierObjectiveParser }

class function TBrigadierObjectiveParser.GetParserString: TNSPath;
begin
  Result := 'objective';
end;

class function TBrigadierObjectiveParser.GetResultName: string;
begin
  Result := 'Objective';
end;

function TBrigadierObjectiveParser.Parse: Boolean;
var
  Name: string;
begin
  Name := ReadWhile(IdentChars);
  if Name.IsEmpty then
    Exit(False);
  if Name.Length > 16 then
    Log(-Name.Length, 'Objective names can be at most 16 characters long.');
  ParseResult := TBrigadierObjective.Create(Argument, Name);
  Result := True;
end;

{ TBrigadierComponent }

constructor TBrigadierComponent.Create(AArgument: TBrigadierArgument);
begin
  inherited;
  FJSON := TJObject.Create;
end;

constructor TBrigadierComponent.Create(AArgument: TBrigadierArgument; AJSON: TJBase);
begin
  inherited Create(AArgument);
  FJSON := AJSON;
end;

function TBrigadierComponent.Format: string;
begin
  Result := JSON.Value.Format(jfInline);
end;

{ TBrigadierComponentParser }

class function TBrigadierComponentParser.GetParserString: TNSPath;
begin
  Result := 'component';
end;

class function TBrigadierComponentParser.GetResultName: string;
begin
  Result := 'Text-Component';
end;

function TBrigadierComponentParser.Parse: Boolean;
var
  Parser: TJBase.IParser;
begin
  Parser := TJBase.Parser;
  Parser.Parse(Info, False);
  Result := Parser.Success;
  if Result then
    ParseResult := TBrigadierComponent.Create(Argument, Parser.OwnParseResult);
end;

{ TBrigadierDimensionParser.TSuggestions }

procedure TBrigadierDimensionParser.TSuggestions.Generate;
var
  Dimension: TNSPath;
begin
  for Dimension in MCRegistries.DimensionType do
    AddSuggestion(Dimension.Format(False));
  AddSuggestion(TNSPath.Empty.Format);
  for Dimension in MCRegistries.DimensionType do
    AddSuggestion(Dimension.Format);
end;

{ TBrigadierResourceLocationParser }

class function TBrigadierResourceLocationParser.GetParserString: TNSPath;
begin
  Result := 'resource_location';
end;

class function TBrigadierResourceLocationParser.GetResultName: string;
begin
  Result := 'Resource-Location';
end;

function TBrigadierResourceLocationParser.Parse: Boolean;
var
  Name: string;
begin
  Name := ReadWhile(NamespacePathChars);
  if Name.IsEmpty then
    Exit(False);
  ParseResult := TBrigadierResourceLocation.Create(Argument, Name);
  Result := True;
end;

{ TBrigadierFunctionParser }

class function TBrigadierFunctionParser.GetParserString: TNSPath;
begin
  Result := 'function';
end;

class function TBrigadierFunctionParser.GetResultName: string;
begin
  Result := 'Function';
end;

function TBrigadierFunctionParser.Parse: Boolean;
var
  Name: string;
begin
  Name := ReadWhile(NamespacePathChars);
  if Name.IsEmpty then
    Exit(False);
  ParseResult := TBrigadierFunction.Create(Argument, Name);
  Result := True;
end;

{ TBrigadierResourceLocation }

constructor TBrigadierResourceLocation.Create(AArgument: TBrigadierArgument; ANSPath: TNSPath);
begin
  inherited Create(AArgument);
  FNSPath := ANSPath;
end;

function TBrigadierResourceLocation.Format: string;
begin
  Result := NSPath.Format;
end;

{ TBrigadierParticleParser }

class function TBrigadierParticleParser.GetParserString: TNSPath;
begin
  Result := 'particle';
end;

class function TBrigadierParticleParser.GetResultName: string;
begin
  Result := 'Particle';
end;

function TBrigadierParticleParser.Parse: Boolean;
var
  Marker: TLogMarker;
  Particle: TNSPath;
begin
  Marker := GetMarker;

  BeginSuggestions(TSuggestions.Create);
  Particle := ReadWhile(NamespacePathChars);
  EndSuggestions;

  if not MCRegistries.ParticleType.Has(Particle) then
    Log(Marker, 'Invalid particle "%s".', [Particle.Format]);

  ParseResult := TBrigadierParticle.Create(Argument, Particle);

  Result := True;
end;

{ TBrigadierParticle }

constructor TBrigadierParticle.Create(AArgument: TBrigadierArgument; AParticle: TNSPath);
begin
  inherited Create(AArgument);
  FParticle := AParticle;
end;

function TBrigadierParticle.Format: string;
begin
  Result := Particle;
end;

{ TBrigadierParticleParser.TSuggestions }

procedure TBrigadierParticleParser.TSuggestions.Generate;
var
  Particle: TNSPath;
begin
  for Particle in MCRegistries.ParticleType do
    AddSuggestion(Particle.Format(False));
  AddSuggestion(TNSPath.Empty.Format);
  for Particle in MCRegistries.ParticleType do
    AddSuggestion(Particle.Format);
end;

{ TBrigadierColor }

constructor TBrigadierColor.Create(AArgument: TBrigadierArgument; AColor: TMCColor);
begin
  inherited Create(AArgument);
  FColor := AColor;
end;

function TBrigadierColor.Format: string;
begin
  Result := MCColorNames[Color];
end;

{ TBrigadierColorParser }

class function TBrigadierColorParser.GetParserString: TNSPath;
begin
  Result := 'color';
end;

class function TBrigadierColorParser.GetResultName: string;
begin
  Result := 'Color';
end;

class function TBrigadierColorParser.GetTokenCount: Integer;
begin
  Result := Length(MCColorNames);
end;

class function TBrigadierColorParser.GetTokenName(AIndex: Integer): string;
begin
  Result := MCColorDisplayNames[TMCColor(AIndex - 1)];
end;

function TBrigadierColorParser.Parse: Boolean;
var
  Name: string;
  Color: TMCColor;
begin
  BeginSuggestions(TSuggestions);
  Name := ReadWhile(IdentChars, False);
  EndSuggestions;

  if Name.IsEmpty or not MCColorFromName(Name, Color) then
    Exit(False);

  Token := Ord(Color) + 1;
  Advance(Name.Length);
  ParseResult := TBrigadierColor.Create(Argument, Color);

  Result := True;
end;

{ TBrigadierColorParser.TSuggestions }

class function TBrigadierColorParser.TSuggestions.GetCount: Integer;
begin
  Result := Length(MCColorNames);
end;

class function TBrigadierColorParser.TSuggestions.GetSuggestion(AIndex: Integer): TParseSuggestion;
begin
  Result.Create(MCColorDisplayNames[TMCColor(AIndex)], MCColorNames[TMCColor(AIndex)]);
end;

{ TBrigadierMobEffect }

constructor TBrigadierMobEffect.Create(AArgument: TBrigadierArgument; AEffect: TNSPath);
begin
  inherited Create(AArgument);
  FEffect := AEffect;
end;

function TBrigadierMobEffect.Format: string;
begin
  Result := Effect;
end;

{ TBrigadierMobEffectParser.TSuggestions }

procedure TBrigadierMobEffectParser.TSuggestions.Generate;
var
  Effect: TNSPath;
begin
  for Effect in MCRegistries.MobEffect do
    AddSuggestion(Effect.Format(False));
  AddSuggestion(TNSPath.Empty.Format);
  for Effect in MCRegistries.MobEffect do
    AddSuggestion(Effect.Format);
end;

{ TBrigadierEntitySummonParser.TSuggestions }

procedure TBrigadierEntitySummonParser.TSuggestions.Generate;
var
  Entity: TNSPath;
begin
  for Entity in MCRegistries.EntityType do
    if Entity <> 'player' then
      AddSuggestion(Entity.Format(False));
  AddSuggestion(TNSPath.Empty.Format);
  for Entity in MCRegistries.EntityType do
    if Entity <> 'player' then
      AddSuggestion(Entity.Format);
end;

{ TBrigadierScoreboardSlot }

constructor TBrigadierScoreboardSlot.Create(AArgument: TBrigadierArgument; AScoreboardSlot: TScoreboardSlot);
begin
  inherited Create(AArgument);
  FScoreboardSlot := AScoreboardSlot;
end;

destructor TBrigadierScoreboardSlot.Destroy;
begin
  FScoreboardSlot.Free;
  inherited;
end;

function TBrigadierScoreboardSlot.Format: string;
begin
  Result := ScoreboardSlot.GetName;
end;

{ TBrigadierScoreboardSlotParser }

class function TBrigadierScoreboardSlotParser.GetParserString: TNSPath;
begin
  Result := 'scoreboard_slot';
end;

class function TBrigadierScoreboardSlotParser.GetResultName: string;
begin
  Result := 'Scoreboard-Slot';
end;

function TBrigadierScoreboardSlotParser.Parse: Boolean;
var
  Parser: TScoreboardSlot.IParser;
begin
  Parser := TScoreboardSlot.Parser;
  Parser.Parse(Info, False);
  Result := Parser.Success;
  if Result then
    ParseResult := TBrigadierScoreboardSlot.Create(Argument, Parser.OwnParseResult);
end;

{ TBrigadierRange }

constructor TBrigadierIntRange.Create(AArgument: TBrigadierArgument; ABounds: TIntBounds1);
begin
  inherited Create(AArgument);
  FBounds := ABounds;
end;

function TBrigadierIntRange.Format: string;
begin
  Result := TEntitySelector.TIntRangeParser.Format(Bounds);
end;

{ TBrigadierRangeParser }

class function TBrigadierIntRangeParser.GetParserString: TNSPath;
begin
  Result := 'int_range';
end;

class function TBrigadierIntRangeParser.GetResultName: string;
begin
  Result := 'Integer-Range';
end;

function TBrigadierIntRangeParser.Parse: Boolean;
var
  Parser: TEntitySelector.IIntRangeParser;
begin
  Parser := TEntitySelector.IntRangeParser;
  Parser.Parse(Info, False);
  Result := Parser.Success;
  if Result then
    ParseResult := TBrigadierIntRange.Create(Argument, Parser.ParseResult);
end;

{ TBrigadierOperation }

constructor TBrigadierOperation.Create(AArgument: TBrigadierArgument; AOperation: TScoreboardOperation);
begin
  inherited Create(AArgument);
  FOperation := AOperation;
end;

function TBrigadierOperation.Format: string;
begin
  Result := ScoreboardOperationNames[Operation];
end;

{ TBrigadierOperationParser }

class function TBrigadierOperationParser.GetParserString: TNSPath;
begin
  Result := 'operation';
end;

class function TBrigadierOperationParser.GetResultName: string;
begin
  Result := 'Scoreboard-Operation';
end;

function TBrigadierOperationParser.Parse: Boolean;
var
  Name: string;
  Operation: TScoreboardOperation;
begin
  BeginSuggestions(TSuggestions);
  Name := ReadWhile(ScoreboardOperationChars);
  EndSuggestions;

  if Name.IsEmpty or not ScoreboardOperationFromName(Name, Operation) then
    Exit(False);

  ParseResult := TBrigadierOperation.Create(Argument, Operation);

  Result := True;
end;

{ TBrigadierOperationParser.TSuggestions }

class function TBrigadierOperationParser.TSuggestions.GetBreakChars: TSysCharSet;
begin
  Result := [' '];
end;

class function TBrigadierOperationParser.TSuggestions.GetCount: Integer;
begin
  Result := Length(ScoreboardOperationNames);
end;

class function TBrigadierOperationParser.TSuggestions.GetSuggestion(AIndex: Integer): TParseSuggestion;
begin
  Result.Insert := ScoreboardOperationNames[TScoreboardOperation(AIndex)];
  Result.Display := ScoreboardOperationDisplayNames[TScoreboardOperation(AIndex)];
  Result.Display := Result.Insert + ' (' + Result.Display + ')';
end;

{ TBrigadierItemPredicate }

constructor TBrigadierItemPredicate.Create(AArgument: TBrigadierArgument);
begin
  inherited;
  FPredicate := TItemStack.Create;
end;

constructor TBrigadierItemPredicate.Create(AArgument: TBrigadierArgument; APredicate: TItemStack);
begin
  inherited Create(AArgument);
  FPredicate := APredicate;
end;

function TBrigadierItemPredicate.Format: string;
begin
  Result := Predicate.Value.Format;
end;

{ TBrigadierItemPredicateParser }

class function TBrigadierItemPredicateParser.GetParserString: TNSPath;
begin
  Result := 'item_predicate';
end;

class function TBrigadierItemPredicateParser.GetResultName: string;
begin
  Result := 'Item-Predicate';
end;

function TBrigadierItemPredicateParser.Parse: Boolean;
var
  ItemTagParser: TItemStackTag.IParser;
  ItemStackParser: TItemStack.IParser;
begin
  if StartsWith('#', False) then
  begin
    ItemTagParser := TItemStackTag.Parser;
    ItemTagParser.Parse(Info, False);
    Result := ItemTagParser.Success;
    if Result then
      ParseResult := TBrigadierItemPredicate.Create(Argument, ItemTagParser.OwnParseResult);
  end
  else
  begin
    ItemStackParser := TItemStack.Parser;
    ItemStackParser.Parse(Info, False);
    Result := ItemStackParser.Success;
    if Result then
      ParseResult := TBrigadierItemPredicate.Create(Argument, ItemStackParser.OwnParseResult);
  end;
end;

{ TBrigadierItemSlot }

constructor TBrigadierItemSlot.Create(AArgument: TBrigadierArgument);
begin
  inherited;
  FSlot := TItemSlotContainer.Create;
  FSlot.Value.SetIndex(0);
end;

constructor TBrigadierItemSlot.Create(AArgument: TBrigadierArgument; ASlot: TItemSlot);
begin
  inherited Create(AArgument);
  FSlot := ASlot;
end;

function TBrigadierItemSlot.Format: string;
begin
  Result := FSlot.Value.Format;
end;

{ TBrigadierItemSlotParser }

class function TBrigadierItemSlotParser.GetParserString: TNSPath;
begin
  Result := 'item_slot';
end;

class function TBrigadierItemSlotParser.GetResultName: string;
begin
  Result := 'Item-Slot';
end;

function TBrigadierItemSlotParser.Parse: Boolean;
var
  Parser: TItemSlot.IParser;
begin
  Parser := TItemSlot.Parser;
  Parser.Parse(Info, False);
  Result := Parser.Success;
  if Result then
    ParseResult := TBrigadierItemSlot.Create(Argument, Parser.OwnParseResult);
end;

{ TBrigadierObjectiveCriteria }

constructor TBrigadierObjectiveCriteria.Create(AArgument: TBrigadierArgument; ACriteria: TScoreboardCriteria);
begin
  inherited Create(AArgument);
  FCriteria := ACriteria;
end;

function TBrigadierObjectiveCriteria.Format: string;
begin
  Result := Criteria.Value.Format;
end;

{ TBrigadierObjectiveCriteriaParser }

class function TBrigadierObjectiveCriteriaParser.GetParserString: TNSPath;
begin
  Result := 'objective_criteria';
end;

class function TBrigadierObjectiveCriteriaParser.GetResultName: string;
begin
  Result := 'Objective-Criteria';
end;

function TBrigadierObjectiveCriteriaParser.Parse: Boolean;
var
  Parser: TScoreboardCriteria.IParser;
begin
  Parser := TScoreboardCriteria.Parser;
  Parser.Parse(Info, False);
  Result := Parser.Success;
  if Result then
    ParseResult := TBrigadierObjectiveCriteria.Create(Argument, Parser.OwnParseResult);
end;

{ TBrigadierEntityUUID }

constructor TBrigadierEntityUUID.Create(AArgument: TBrigadierArgument; AUUID: TGUID);
begin
  inherited Create(AArgument);
  FUUID := AUUID;
end;

function TBrigadierEntityUUID.Format: string;
begin
  Result := UUID.ToString;
  // remove the {}
  Result := Result.Substring(1, Result.Length - 2);
end;

{ TBrigadierScoreHolderAll }

function TBrigadierScoreHolderAll.Format: string;
begin
  Result := '*';
end;

{ TBrigadierScoreHolderSelector }

constructor TBrigadierScoreHolderSelector.Create(AArgument: TBrigadierArgument);
begin
  inherited;
  FSelector := TEntitySelector.Create;
end;

constructor TBrigadierScoreHolderSelector.Create(AArgument: TBrigadierArgument; ASelector: TEntitySelector);
begin
  inherited Create(AArgument);
  FSelector := ASelector;
end;

destructor TBrigadierScoreHolderSelector.Destroy;
begin
  FSelector.Free;
  inherited;
end;

function TBrigadierScoreHolderSelector.Format: string;
begin
  Result := Selector.Format;
end;

{ TBrigadierScoreHolderText }

constructor TBrigadierScoreHolderText.Create(AArgument: TBrigadierArgument; AText: string);
begin
  inherited Create(AArgument);
  FText := AText;
end;

function TBrigadierScoreHolderText.Format: string;
begin
  Result := Text;
end;

initialization

// Progress: 38 / 38
// /--------------------------------------\
// [||||||||||||||||||||||||||||||||||||||]
// \--------------------------------------/

// - Basic Types -

TBrigadierBoolParser.RegisterClass;
TBrigadierIntegerParser.RegisterClass;
TBrigadierFloatParser.RegisterClass;
TBrigadierDoubleParser.RegisterClass;
TBrigadierStringParser.RegisterClass;
TBrigadierIntRangeParser.RegisterClass;

// - Position related -

TBrigadierRotationParser.RegisterClass;
TBrigadierVec2Parser.RegisterClass;
TBrigadierVec3Parser.RegisterClass;
TBrigadierSwizzleParser.RegisterClass;
TBrigadierBlockPosParser.RegisterClass;
TBrigadierDimensionParser.RegisterClass;
TBrigadierColumnPosParser.RegisterClass;

// - Entity related -

TBrigadierEntityParser.RegisterClass;
TBrigadierEntityAnchorParser.RegisterClass;
TBrigadierGameProfileParser.RegisterClass;
TBrigadierMobEffectParser.RegisterClass;
TBrigadierParticleParser.RegisterClass;
TBrigadierEntitySummonParser.RegisterClass;

// - NBT related -

TBrigadierNBTCompoundTagParser.RegisterClass;
TBrigadierNBTPathParser.RegisterClass;

// - Text related -

TBrigadierComponentParser.RegisterClass;
TBrigadierMessageParser.RegisterClass;
TBrigadierColorParser.RegisterClass;

// - Scoreboard related -

TBrigadierObjectiveParser.RegisterClass;
TBrigadierObjectiveCriteriaParser.RegisterClass;
TBrigadierScoreHolderParser.RegisterClass;
TBrigadierScoreboardSlotParser.RegisterClass;
TBrigadierOperationParser.RegisterClass;
TBrigadierTeamParser.RegisterClass;

// - Item related -

TBrigadierItemStackParser.RegisterClass;
TBrigadierItemPredicateParser.RegisterClass;
TBrigadierItemEnchantmentParser.RegisterClass;
TBrigadierItemSlotParser.RegisterClass;

// - Block related -

TBrigadierBlockPredicateParser.RegisterClass;
TBrigadierBlockStateParser.RegisterClass;

// - Resource related -

TBrigadierResourceLocationParser.RegisterClass;
TBrigadierFunctionParser.RegisterClass;

end.
