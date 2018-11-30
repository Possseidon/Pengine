unit Pengine.MC.EntitySelector;

interface

uses
  System.SysUtils,
  System.Math,

  Pengine.Vector,
  Pengine.IntMaths,
  Pengine.Parsing,
  Pengine.Collections,
  Pengine.Utility,
  Pengine.JSON,

  Pengine.MC.NBT,
  Pengine.MC.Gamemode,
  Pengine.MC.Entity,
  Pengine.MC.General,
  Pengine.MC.Namespace;

type

  {
    TEntitySelectorSettings = class(TSettings)
    private
    FEntityNamespacePrefix: Boolean;
    FSpaceAfterComma: Boolean;

    protected
    class function GetNameForVersion(AVersion: Integer): string; override;

    public
    property EntityNamespacePrefix: Boolean read FEntityNamespacePrefix write FEntityNamespacePrefix;
    property SpaceAfterComma: Boolean read FSpaceAfterComma write FSpaceAfterComma;

    procedure SetDefaults; override;

    procedure DefineJStorage(ASerializer: TJSerializer); override;

    end;
  }

  TEntitySelector = class
  public type

    TVariable = (
      svSender,
      svAllPlayers,
      svAllEntites,
      svNearestPlayer,
      svRandomPlayer
      );

  public const

    VariableChars: array [TVariable] of Char = (
      's',
      'a',
      'e',
      'p',
      'r'
      );

    VariableDisplayNames: array [TVariable] of string = (
      'Sender',
      'All Players',
      'All Entities',
      'Nearest Player',
      'Random Player'
      );

    VariableDefaultMulti = [svAllPlayers, svAllEntites];

    IntChars = ['0' .. '9', '-'];
    FloatChars = IntChars + ['.'];

    Prefix = '@';

  public type

    IIntRangeParser = IParser<TIntBounds1>;

    TIntRangeParser = class(TParser<TIntBounds1>, IIntRangeParser)
    public const

      TokenValue = 1;
      TokenSplitter = 2;

      TokenNames: array [TokenValue .. TokenSplitter] of string = (
        'Value',
        'Splitter'
        );

    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;
      class function GetTokenCount: Integer; override;
      class function GetTokenName(AIndex: Integer): string; override;

      class function Format(ARange: TIntBounds1): string;

    end;

    IRangeParser = IParser<TBounds1>;

    TRangeParser = class(TParser<TBounds1>, IRangeParser)
    public const

      TokenValue = 1;
      TokenSplitter = 2;

      TokenNames: array [TokenValue .. TokenSplitter] of string = (
        'Value',
        'Splitter'
        );

    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;
      class function GetTokenCount: Integer; override;
      class function GetTokenName(AIndex: Integer): string; override;

      class function Format(ARange: TBounds1): string;

    end;

    TOptionClass = class of TOption;

    TOption = class
    public type

      TQuantityType = (
        qtSingle,
        qtMultipleInverted,
        qtMultiple
        );

      TType = (
        soLimit,
        soSort,
        soDistance,
        soX,
        soY,
        soZ,
        soDX,
        soDY,
        soDZ,
        soXRotation,
        soYRotation,
        soName,
        soNBT,
        soScores,
        soTag,
        soTeam,
        soType,
        soAdvancements,
        soGamemode,
        soLevel
        );

      IParser = interface(IObjectParser<TOption>)
        function GetSelector: TEntitySelector;
        procedure SetSelector(const Value: TEntitySelector);

        property Selector: TEntitySelector read GetSelector write SetSelector;

      end;

      TParser = class(TObjectParser<TOption>, IParser)
      public const

        TokenOption = 1;
        TokenEquals = 2;
        TokenInvert = 3;

        TokenNames: array [TokenOption .. TokenInvert] of string = (
          'Option',
          'Equals',
          'Invert'
          );

      private
        FSelector: TEntitySelector;

        function GetSelector: TEntitySelector;
        procedure SetSelector(const Value: TEntitySelector);

      protected
        function Parse: Boolean; override;

      public
        class function GetResultName: string; override;
        class function GetTokenCount: Integer; override;
        class function GetTokenName(AIndex: Integer): string; override;

        property Selector: TEntitySelector read GetSelector write SetSelector;

      end;

      IDataParser = IDecorateParser<TOption>;

      TDataParser<T: TOption> = class(TDecorateParser<T>);

      TSuggestions = class(TParseSuggestionsGenerated<TParser>)
      private
        FSelector: TEntitySelector;

      protected
        procedure Generate; override;

      public
        constructor Create(ASelector: TEntitySelector);

      end;

    private
      FSelector: TEntitySelector;

      function GetIndex: Integer;
      procedure SetIndex(const Value: Integer);

    public
      constructor Create(ASelector: TEntitySelector); virtual;

      class function GetClass(AName: string): TOptionClass; static;
      class function CreateTyped(ASelector: TEntitySelector; AName: string): TOption; static;

      class function Parser: IParser;
      class function DataParser: IDataParser;

      class function GetDataParserClass: TParserClass; virtual; abstract;
      class function GetType: TType; virtual; abstract;
      class function GetName: string;
      class function IsApplicable(AVariable: TVariable): Boolean; virtual;
      class function GetQuantityType: TQuantityType; virtual;

      property Selector: TEntitySelector read FSelector;
      property Index: Integer read GetIndex write SetIndex;

      function FormatData: string; virtual; abstract;
      function Format: string;

    end;

    TOptions = TObjectArray<TOption>;

    TOptionValueOrRange = class(TOption)
    public type

      TParser = class(TDecorateParser<TOptionValueOrRange>)
      protected
        function Parse: Boolean; override;

      public
        class function GetResultName: string; override;

      end;

    private
      FRange: TBounds1;

    public
      class function GetDataParserClass: TParserClass; override;

      class function MustBeNormalized: Boolean; virtual;
      class function GetMinimum(out AValue: Single): Boolean; virtual;
      class function GetMaximum(out AValue: Single): Boolean; virtual;

      property Range: TBounds1 read FRange write FRange;
      property RangeMin: Single read FRange.C1 write FRange.C1;
      property RangeMax: Single read FRange.C2 write FRange.C2;

      function FormatData: string; override;

    end;

    TOptionIntValueOrRange = class(TOption)
    public type

      TParser = class(TDataParser<TOptionIntValueOrRange>)
      protected
        function Parse: Boolean; override;

      public
        class function GetResultName: string; override;

      end;

    private
      FRange: TIntBounds1;

    public
      class function GetDataParserClass: TParserClass; override;

      class function MustBeNormalized: Boolean; virtual;
      class function GetMinimum(out AValue: Integer): Boolean; virtual;
      class function GetMaximum(out AValue: Integer): Boolean; virtual;

      property Range: TIntBounds1 read FRange write FRange;
      property RangeMin: Integer read FRange.C1 write FRange.C1;
      property RangeMax: Integer read FRange.C2 write FRange.C2;

      function FormatData: string; override;

    end;

    TOptionInvertible = class(TOption)
    private
      FInverted: Boolean;

    public
      property Inverted: Boolean read FInverted write FInverted;

      function FormatData: string; override;

    end;

    TOptionInteger = class(TOption)
    public type

      TParser = class(TDataParser<TOptionInteger>)
      protected
        function Parse: Boolean; override;

      public
        class function GetResultName: string; override;

      end;

    private
      FValue: Integer;

    public
      class function GetDataParserClass: TParserClass; override;
      class function GetMinimum(out AValue: Integer): Boolean; virtual;
      class function GetMaximum(out AValue: Integer): Boolean; virtual;

      property Value: Integer read FValue write FValue;

      function FormatData: string; override;

    end;

    TOptionFloat = class(TOption)
    public type

      TParser = class(TDataParser<TOptionFloat>)
      protected
        function Parse: Boolean; override;

      public
        class function GetResultName: string; override;

      end;

    private
      FValue: Single;

    public
      class function GetDataParserClass: TParserClass; override;

      property Value: Single read FValue write FValue;

      function FormatData: string; override;

    end;

    TOptionString = class(TOptionInvertible)
    public type

      TParser = class(TDataParser<TOptionString>)
      protected
        function Parse: Boolean; override;

      public
        class function GetResultName: string; override;
        class function IgnoreContext: Boolean; override;

      end;

    private
      FText: string;

    public
      class function GetDataParserClass: TParserClass; override;

      property Text: string read FText write FText;

      function FormatData: string; override;

    end;

    TOptionIdentifier = class(TOptionInvertible)
    public type

      TParser = class(TDataParser<TOptionIdentifier>)
      protected
        function Parse: Boolean; override;

      public
        class function GetResultName: string; override;

      end;

    private
      FText: string;

    public
      class function GetDataParserClass: TParserClass; override;

      property Text: string read FText write FText;

      function FormatData: string; override;

    end;

    TOptionLimit = class(TOptionInteger)
    public
      class function GetType: TOption.TType; override;
      class function IsApplicable(AVariable: TVariable): Boolean; override;
      class function GetMinimum(out AValue: Integer): Boolean; override;

    end;

    TOptionSort = class(TOption)
    public type

      TSortType = (
        stArbitrary,
        stNearest,
        stFurthest,
        stRandom
        );

      TParser = class(TDataParser<TOptionSort>)
      protected
        function Parse: Boolean; override;

      public
        class function GetResultName: string; override;

      end;

      TSuggestions = class(TParseSuggestionsSimple<TParser>)
      public
        class function GetCount: Integer; override;
        class function GetSuggestion(AIndex: Integer): TParseSuggestion; override;

      end;

    public const

      SortTypeNames: array [TSortType] of string = (
        'arbitrary',
        'nearest',
        'furthest',
        'random'
        );

    private
      FSortType: TSortType;

      function GetSortTypeName: string;

    public
      class function GetType: TOption.TType; override;
      class function IsApplicable(AVariable: TVariable): Boolean; override;
      class function GetDataParserClass: TParserClass; override;

      property SortType: TSortType read FSortType write FSortType;
      property SortTypeName: string read GetSortTypeName;

      function FormatData: string; override;

    end;

    TOptionDistance = class(TOptionValueOrRange)
    public
      class function GetType: TOption.TType; override;
      class function GetMinimum(out AValue: Single): Boolean; override;

    end;

    TOptionX = class(TOptionFloat)
    public
      class function GetType: TOption.TType; override;

    end;

    TOptionY = class(TOptionFloat)
    public
      class function GetType: TOption.TType; override;

    end;

    TOptionZ = class(TOptionFloat)
    public
      class function GetType: TOption.TType; override;

    end;

    TOptionDX = class(TOptionFloat)
    public
      class function GetType: TOption.TType; override;

    end;

    TOptionDY = class(TOptionFloat)
    public
      class function GetType: TOption.TType; override;

    end;

    TOptionDZ = class(TOptionFloat)
    public
      class function GetType: TOption.TType; override;

    end;

    TOptionXRotation = class(TOptionValueOrRange)
    public
      class function GetType: TOption.TType; override;
      class function MustBeNormalized: Boolean; override;

    end;

    TOptionYRotation = class(TOptionValueOrRange)
    public
      class function GetType: TOption.TType; override;
      class function MustBeNormalized: Boolean; override;

    end;

    TOptionName = class(TOptionString)
    public
      class function GetType: TOption.TType; override;
      class function GetQuantityType: TOption.TQuantityType; override;

    end;

    TOptionNBT = class(TOptionInvertible)
    public type

      TParser = class(TDataParser<TOptionNBT>)
      protected
        function Parse: Boolean; override;

      public
        class function GetResultName: string; override;
        class function IgnoreContext: Boolean; override;

      end;

    private
      FNBT: TNBTCompound;

      function GetNBT: TNBTCompound;

    public
      destructor Destroy; override;

      class function GetType: TOption.TType; override;
      class function GetQuantityType: TOption.TQuantityType; override;
      class function GetDataParserClass: TParserClass; override;

      property NBT: TNBTCompound read GetNBT;

      function FormatData: string; override;

    end;

    TOptionScores = class(TOption)
    public type

      TScore = TPair<string, TIntBounds1>;
      TScores = TArray<TScore>;

      TParser = class(TDataParser<TOptionScores>)
      public const

        TokenBrackets = 1;
        TokenName = 2;
        TokenEquals = 3;
        TokenComma = 4;

        TokenNames: array [TokenBrackets .. TokenComma] of string = (
          'Brackets',
          'Name',
          'Equals',
          'Comma'
          );

      protected
        function Parse: Boolean; override;

      public
        class function GetResultName: string; override;
        class function GetTokenCount: Integer; override;
        class function GetTokenName(AIndex: Integer): string; override;

      end;

    private
      FScores: TScores;

    public
      constructor Create(ASelector: TEntitySelector); override;
      destructor Destroy; override;

      class function GetType: TOption.TType; override;
      class function GetDataParserClass: TParserClass; override;

      property Scores: TScores read FScores;

      function FormatData: string; override;

    end;

    TOptionTag = class(TOptionIdentifier)
    public
      class function GetType: TOption.TType; override;
      class function GetQuantityType: TOption.TQuantityType; override;

    end;

    TOptionTeam = class(TOptionIdentifier)
    public
      class function GetType: TOption.TType; override;
      class function GetQuantityType: TOption.TQuantityType; override;

    end;

    TOptionType = class(TOptionInvertible)
    public type

      TParser = class(TDataParser<TOptionType>)
      protected
        function Parse: Boolean; override;

      public
        class function GetResultName: string; override;

      end;

      TSuggestions = class(TParseSuggestionsGenerated<TParser>)
      protected
        procedure Generate; override;

      end;

    private
      FEntity: TEntity;

    public
      class function GetType: TOption.TType; override;
      class function GetQuantityType: TOption.TQuantityType; override;
      class function IsApplicable(AVariable: TVariable): Boolean; override;

      property Entity: TEntity read FEntity write FEntity;

      class function GetDataParserClass: TParserClass; override;

      function FormatData: string; override;

    end;

    TOptionAdvancements = class(TOption)
    public type

      TAdvancement = TPair<string, Boolean>;
      TAdvancements = TArray<TAdvancement>;

      TParser = class(TDataParser<TOptionAdvancements>)
      public const

        TokenBrackets = 1;
        TokenName = 2;
        TokenEquals = 3;
        TokenComma = 4;
        TokenBoolean = 5;

        TokenNames: array [TokenBrackets .. TokenBoolean] of string = (
          'Brackets',
          'Name',
          'Equals',
          'Comma',
          'Boolean'
          );

      protected
        function Parse: Boolean; override;

      public
        class function GetResultName: string; override;
        class function GetTokenCount: Integer; override;
        class function GetTokenName(AIndex: Integer): string; override;

      end;

    private
      FAdvancements: TAdvancements;

    public
      constructor Create(ASelector: TEntitySelector); override;
      destructor Destroy; override;

      class function GetType: TOption.TType; override;
      class function GetDataParserClass: TParserClass; override;

      property Advancements: TAdvancements read FAdvancements;

      function FormatData: string; override;

    end;

    TOptionGamemode = class(TOptionInvertible)
    public type

      TParser = class(TDataParser<TOptionGamemode>)
      protected
        function Parse: Boolean; override;

      public
        class function GetResultName: string; override;

      end;

      TSuggestions = class(TParseSuggestionsSimple<TParser>)
      public
        class function GetCount: Integer; override;
        class function GetSuggestion(AIndex: Integer): TParseSuggestion; override;

      end;

    private
      FGamemode: TGamemode;

      function GetGamemodeName: string;

    public
      class function GetDataParserClass: TParserClass; override;
      class function GetType: TOption.TType; override;

      property Gamemode: TGamemode read FGamemode write FGamemode;
      property GamemodeName: string read GetGamemodeName;

      function FormatData: string; override;

    end;

    TOptionLevel = class(TOptionIntValueOrRange)
    public
      class function GetType: TOption.TType; override;
      class function GetMinimum(out AValue: Integer): Boolean; override;

    end;

    IParser = IObjectParser<TEntitySelector>;

    TParser = class(TObjectParser<TEntitySelector>, IParser)
    public const

      TokenPrefix = 1;
      TokenVariable = 2;
      TokenBrackets = 3;
      TokenComma = 4;

      TokenNames: array [TokenPrefix .. TokenComma] of string = (
        'Prefix',
        'Variable',
        'Brackets',
        'Comma'
        );

    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;
      class function GetTokenCount: Integer; override;
      class function GetTokenName(AIndex: Integer): string; override;

    end;

    TSuggestions = class(TParseSuggestionsSimple<TParser>)
    public
      class function GetCount: Integer; override;
      class function GetSuggestion(AIndex: Integer): TParseSuggestion; override;

    end;

  public const

    OptionNames: array [TOption.TType] of string = (
      'limit',
      'sort',
      'distance',
      'x',
      'y',
      'z',
      'dx',
      'dy',
      'dz',
      'x_rotation',
      'y_rotation',
      'name',
      'nbt',
      'scores',
      'tag',
      'team',
      'type',
      'advancements',
      'gamemode',
      'level'
      );

    OptionClasses: array [TOption.TType] of TOptionClass = (
      TOptionLimit,
      TOptionSort,
      TOptionDistance,
      TOptionX,
      TOptionY,
      TOptionZ,
      TOptionDX,
      TOptionDY,
      TOptionDZ,
      TOptionXRotation,
      TOptionYRotation,
      TOptionName,
      TOptionNBT,
      TOptionScores,
      TOptionTag,
      TOptionTeam,
      TOptionType,
      TOptionAdvancements,
      TOptionGamemode,
      TOptionLevel
      );

  private
    FVariable: TVariable;
    FOptions: TOptions;

    function GetVariableChar: Char;

  public
    constructor Create(AVariable: TVariable = svSender);
    destructor Destroy; override;

    class function Parser: IParser;
    class function IntRangeParser: IIntRangeParser;
    class function RangeParser: IRangeParser;

    function AllowsMultiple: Boolean;
    function AllowsPlayersOnly: Boolean;

    property Variable: TVariable read FVariable write FVariable;
    property VariableChar: Char read GetVariableChar;

    property Options: TOptions read FOptions;

    function Format: string;

  end;

implementation

{ TEntitySelector.TOption }

constructor TEntitySelector.TOption.Create(ASelector: TEntitySelector);
begin
  FSelector := ASelector;
end;

class function TEntitySelector.TOption.CreateTyped(ASelector: TEntitySelector; AName: string): TOption;
var
  OptionClass: TOptionClass;
begin
  OptionClass := GetClass(AName);
  if OptionClass = nil then
    Exit(nil);
  Result := OptionClass.Create(ASelector);
end;

class function TEntitySelector.TOption.DataParser: IDataParser;
begin
  Result := TDataParser<TOption>(GetDataParserClass.Create);
end;

function TEntitySelector.TOption.Format: string;
begin
  Result := GetName + '=' + FormatData;
end;

class function TEntitySelector.TOption.GetClass(AName: string): TOptionClass;
var
  T: TType;
begin
  for T := Low(TType) to High(TType) do
    if AName = OptionNames[T] then
      Exit(OptionClasses[T]);
  Result := nil;
end;

function TEntitySelector.TOption.GetIndex: Integer;
begin
  Result := Selector.Options.Find(Self);
end;

class function TEntitySelector.TOption.GetName: string;
begin
  Result := OptionNames[GetType];
end;

class function TEntitySelector.TOption.GetQuantityType: TQuantityType;
begin
  Result := qtSingle;
end;

class function TEntitySelector.TOption.IsApplicable(AVariable: TVariable): Boolean;
begin
  Result := True;
end;

class function TEntitySelector.TOption.Parser: TEntitySelector.TOption.IParser;
begin
  Result := TParser.Create;
end;

procedure TEntitySelector.TOption.SetIndex(const Value: Integer);
begin
  Selector.Options.SetIndex(Index, Value);
end;

{ TEntitySelector.TParser }

class function TEntitySelector.TParser.GetResultName: string;
begin
  Result := 'Entity Selector';
end;

class function TEntitySelector.TParser.GetTokenCount: Integer;
begin
  Result := High(TokenNames);
end;

class function TEntitySelector.TParser.GetTokenName(AIndex: Integer): string;
begin
  Result := TokenNames[AIndex];
end;

function TEntitySelector.TParser.Parse: Boolean;
var
  Variable: TVariable;
  OptionParser: TOption.IParser;
  Required: Boolean;
begin
  BeginSuggestions(TSuggestions);

  Token := TokenPrefix;

  if not StartsWith(Prefix) then
    Exit(False);

  Result := True;

  if ReachedEnd then
    raise EParseError.Create('Expected entity selector variable.');

  Token := TokenVariable;

  for Variable := Low(TVariable) to High(TVariable) do
  begin
    if StartsWith(VariableChars[Variable]) then
    begin
      ParseResult := TEntitySelector.Create(Variable);
      Break;
    end;
  end;

  EndSuggestions;

  if ParseResult = nil then
    raise EParseError.CreateFmt('Invalid entity selector variable "@%s".', [First]);

  Token := TokenBrackets;

  if not StartsWith('[') then
    Exit;

  OptionParser := TOption.Parser;
  OptionParser.Selector := ParseResult;

  Required := False;
  while True do
  begin
    ResetToken;

    OptionParser.Parse(Info, Required);
    if OptionParser.Success then
    begin
      ParseResult.Options.Add(OptionParser.OwnParseResult);
      Required := True;
    end;

    SkipWhitespace;

    if Required then
    begin
      case First of
        ']':
          begin
            Token := TokenBrackets;
            Advance;
            Break;
          end;
        ',':
          begin
            Token := TokenComma;
            Advance;
          end
      else
        if Required then
          raise EParseError.Create('Expected "," or "]".')
      end;
    end
    else
    begin
      Token := TokenBrackets;
      if StartsWith(']') then
        Break;
      raise EParseError.Create('Expected option or "]".');
    end;
  end;
end;

{ TEntitySelector.TOptionLimit }

class function TEntitySelector.TOptionLimit.GetMinimum(out AValue: Integer): Boolean;
begin
  AValue := 1;
  Result := True;
end;

class function TEntitySelector.TOptionLimit.GetType: TOption.TType;
begin
  Result := soLimit;
end;

class function TEntitySelector.TOptionLimit.IsApplicable(AVariable: TVariable): Boolean;
begin
  Result := AVariable <> svSender;
end;

{ TEntitySelector.TOptionSort }

function TEntitySelector.TOptionSort.FormatData: string;
begin
  Result := SortTypeName;
end;

class function TEntitySelector.TOptionSort.GetDataParserClass: TParserClass;
begin
  Result := TParser;
end;

function TEntitySelector.TOptionSort.GetSortTypeName: string;
begin
  Result := SortTypeNames[SortType];
end;

class function TEntitySelector.TOptionSort.GetType: TOption.TType;
begin
  Result := soSort;
end;

class function TEntitySelector.TOptionSort.IsApplicable(AVariable: TVariable): Boolean;
begin
  Result := AVariable <> svSender;
end;

{ TEntitySelector.TOptionDistance }

class function TEntitySelector.TOptionDistance.GetMinimum(out AValue: Single): Boolean;
begin
  AValue := 0;
  Result := True;
end;

class function TEntitySelector.TOptionDistance.GetType: TOption.TType;
begin
  Result := soDistance;
end;

{ TEntitySelector.TOptionX }

class function TEntitySelector.TOptionX.GetType: TOption.TType;
begin
  Result := soX;
end;

{ TEntitySelector.TOptionY }

class function TEntitySelector.TOptionY.GetType: TOption.TType;
begin
  Result := soY;
end;

{ TEntitySelector.TOptionZ }

class function TEntitySelector.TOptionZ.GetType: TOption.TType;
begin
  Result := soZ;
end;

{ TEntitySelector.TOptionDX }

class function TEntitySelector.TOptionDX.GetType: TOption.TType;
begin
  Result := soDX;
end;

{ TEntitySelector.TOptionDY }

class function TEntitySelector.TOptionDY.GetType: TOption.TType;
begin
  Result := soDY;
end;

{ TEntitySelector.TOptionDZ }

class function TEntitySelector.TOptionDZ.GetType: TOption.TType;
begin
  Result := soDZ;
end;

{ TEntitySelector.TOptionXRotation }

class function TEntitySelector.TOptionXRotation.GetType: TOption.TType;
begin
  Result := soXRotation;
end;

class function TEntitySelector.TOptionXRotation.MustBeNormalized: Boolean;
begin
  Result := False;
end;

{ TEntitySelector.TOptionYRotation }

class function TEntitySelector.TOptionYRotation.GetType: TOption.TType;
begin
  Result := soYRotation;
end;

class function TEntitySelector.TOptionYRotation.MustBeNormalized: Boolean;
begin
  Result := False;
end;

{ TEntitySelector.TOptionName }

class function TEntitySelector.TOptionName.GetQuantityType: TOption.TQuantityType;
begin
  Result := qtMultipleInverted;
end;

class function TEntitySelector.TOptionName.GetType: TOption.TType;
begin
  Result := soName;
end;

{ TEntitySelector.TOptionNBT }

destructor TEntitySelector.TOptionNBT.Destroy;
begin
  FNBT.Free;
  inherited;
end;

function TEntitySelector.TOptionNBT.FormatData: string;
begin
  Result := inherited + NBT.Format;
end;

class function TEntitySelector.TOptionNBT.GetDataParserClass: TParserClass;
begin
  Result := TParser;
end;

function TEntitySelector.TOptionNBT.GetNBT: TNBTCompound;
begin
  if FNBT = nil then
    FNBT := TNBTCompound.Create;
  Result := FNBT;
end;

class function TEntitySelector.TOptionNBT.GetQuantityType: TOption.TQuantityType;
begin
  Result := qtMultiple;
end;

class function TEntitySelector.TOptionNBT.GetType: TOption.TType;
begin
  Result := soNBT;
end;

{ TEntitySelector.TOptionScores }

constructor TEntitySelector.TOptionScores.Create(ASelector: TEntitySelector);
begin
  inherited;
  FScores := TScores.Create;
end;

destructor TEntitySelector.TOptionScores.Destroy;
begin
  FScores.Free;
  inherited;
end;

function TEntitySelector.TOptionScores.FormatData: string;
var
  I: Integer;
begin
  Result := '{';
  for I := 0 to Scores.MaxIndex do
  begin
    if I <> 0 then
      Result := Result + ', ';
    Result := Result + Scores[I].Key + '=' + TIntRangeParser.Format(Scores[I].Value);
  end;
  Result := Result + '}';
end;

class function TEntitySelector.TOptionScores.GetDataParserClass: TParserClass;
begin
  Result := TParser;
end;

class function TEntitySelector.TOptionScores.GetType: TOption.TType;
begin
  Result := soScores;
end;

{ TEntitySelector.TOptionTag }

class function TEntitySelector.TOptionTag.GetQuantityType: TOption.TQuantityType;
begin
  Result := qtMultiple;
end;

class function TEntitySelector.TOptionTag.GetType: TOption.TType;
begin
  Result := soTag;
end;

{ TEntitySelector.TOptionTeam }

class function TEntitySelector.TOptionTeam.GetQuantityType: TOption.TQuantityType;
begin
  Result := qtMultipleInverted;
end;

class function TEntitySelector.TOptionTeam.GetType: TOption.TType;
begin
  Result := soTeam;
end;

{ TEntitySelector.TOptionType }

function TEntitySelector.TOptionType.FormatData: string;
begin
  Result := inherited + NSPath(EntityNames[Entity]).Format;
end;

class function TEntitySelector.TOptionType.GetDataParserClass: TParserClass;
begin
  Result := TParser;
end;

class function TEntitySelector.TOptionType.GetQuantityType: TOption.TQuantityType;
begin
  Result := qtMultipleInverted;
end;

class function TEntitySelector.TOptionType.GetType: TOption.TType;
begin
  Result := soType;
end;

class function TEntitySelector.TOptionType.IsApplicable(AVariable: TVariable): Boolean;
begin
  Result := AVariable in [svSender, svAllEntites];
end;

{ TEntitySelector.TOptionAdvancements }

constructor TEntitySelector.TOptionAdvancements.Create(ASelector: TEntitySelector);
begin
  inherited;
  FAdvancements := TAdvancements.Create;
end;

destructor TEntitySelector.TOptionAdvancements.Destroy;
begin
  FAdvancements.Free;
  inherited;
end;

function TEntitySelector.TOptionAdvancements.FormatData: string;
var
  I: Integer;
begin
  Result := '{';
  for I := 0 to Advancements.MaxIndex do
  begin
    if I <> 0 then
      Result := Result + ', ';
    Result := Result + Advancements[I].Key + '=';
    if Advancements[I].Value then
      Result := Result + 'true'
    else
      Result := Result + 'false';
  end;
  Result := Result + '}';
end;

class function TEntitySelector.TOptionAdvancements.GetDataParserClass: TParserClass;
begin
  Result := TParser;
end;

class function TEntitySelector.TOptionAdvancements.GetType: TOption.TType;
begin
  Result := soAdvancements;
end;

{ TEntitySelector.TOptionGamemode }

function TEntitySelector.TOptionGamemode.FormatData: string;
begin
  Result := inherited + GamemodeName;
end;

class function TEntitySelector.TOptionGamemode.GetDataParserClass: TParserClass;
begin
  Result := TParser;
end;

function TEntitySelector.TOptionGamemode.GetGamemodeName: string;
begin
  Result := GamemodeNames[Gamemode];
end;

class function TEntitySelector.TOptionGamemode.GetType: TOption.TType;
begin
  Result := soGamemode;
end;

{ TEntitySelector.TOptionLevel }

class function TEntitySelector.TOptionLevel.GetMinimum(out AValue: Integer): Boolean;
begin
  AValue := 0;
  Result := True;
end;

class function TEntitySelector.TOptionLevel.GetType: TOption.TType;
begin
  Result := soLevel;
end;

{ TEntitySelector }

function TEntitySelector.AllowsMultiple: Boolean;
var
  Option: TOption;
begin
  if not TOptionLimit.IsApplicable(Variable) then
    Exit(False);

  for Option in Options do
    if Option is TOptionLimit then
      Exit(TOptionLimit(Option).Value > 1);

  Result := Variable in VariableDefaultMulti;
end;

function TEntitySelector.AllowsPlayersOnly: Boolean;
var
  Option: TOption;
begin
  if not TOptionType.IsApplicable(Variable) then
    Exit(True);

  for Option in Options do
  begin
    if Option is TOptionType then
    begin
      if TOptionType(Option).Entity = etPlayer then
        Exit(not TOptionType(Option).Inverted)
      else if not TOptionType(Option).Inverted then
        Exit(False);
    end;
  end;

  Result := False;
end;

constructor TEntitySelector.Create(AVariable: TVariable);
begin
  FVariable := AVariable;
  FOptions := TOptions.Create;
end;

destructor TEntitySelector.Destroy;
begin
  FOptions.Free;
  inherited;
end;

function TEntitySelector.Format: string;
var
  Option: TOption;
begin
  Result := '@' + VariableChar;
  if Options.Empty then
    Exit;
  Result := Result + '[';
  for Option in Options do
  begin
    Result := Result + Option.Format;
    if Option.Index <> Options.MaxIndex then
    begin
      Result := Result + ',';
      if True then
        Result := Result + ' ';
    end;
  end;
  Result := Result + ']';
end;

function TEntitySelector.GetVariableChar: Char;
begin
  Result := VariableChars[Variable];
end;

class function TEntitySelector.IntRangeParser: IIntRangeParser;
begin
  Result := TIntRangeParser.Create;
end;

class function TEntitySelector.Parser: IParser;
begin
  Result := TParser.Create;
end;

class function TEntitySelector.RangeParser: IRangeParser;
begin
  Result := TRangeParser.Create;
end;

{ TEntitySelector.TOptionValueOrRange }

function TEntitySelector.TOptionValueOrRange.FormatData: string;
begin
  Result := TRangeParser.Format(Range);
end;

class function TEntitySelector.TOptionValueOrRange.GetDataParserClass: TParserClass;
begin
  Result := TParser;
end;

class function TEntitySelector.TOptionValueOrRange.GetMaximum(out AValue: Single): Boolean;
begin
  Result := False;
end;

class function TEntitySelector.TOptionValueOrRange.GetMinimum(out AValue: Single): Boolean;
begin
  Result := False;
end;

class function TEntitySelector.TOptionValueOrRange.MustBeNormalized: Boolean;
begin
  Result := True;
end;

{ TEntitySelector.TOptionInvertible }

function TEntitySelector.TOptionInvertible.FormatData: string;
begin
  if Inverted then
    Exit('!');
  Result := '';
end;

{ TEntitySelector.TOptionInteger }

function TEntitySelector.TOptionInteger.FormatData: string;
begin
  Result := Value.ToString;
end;

class function TEntitySelector.TOptionInteger.GetDataParserClass: TParserClass;
begin
  Result := TParser;
end;

class function TEntitySelector.TOptionInteger.GetMaximum(out AValue: Integer): Boolean;
begin
  Result := False;
end;

class function TEntitySelector.TOptionInteger.GetMinimum(out AValue: Integer): Boolean;
begin
  Result := False;
end;

{ TEntitySelector.TOptionFloat }

function TEntitySelector.TOptionFloat.FormatData: string;
begin
  Result := PrettyFloat(Value);
end;

class function TEntitySelector.TOptionFloat.GetDataParserClass: TParserClass;
begin
  Result := TParser;
end;

{ TEntitySelector.TOptionString }

function TEntitySelector.TOptionString.FormatData: string;
begin
  Result := inherited + '"' + Text.Replace('\', '\\').Replace('"', '\"') + '"';
end;

class function TEntitySelector.TOptionString.GetDataParserClass: TParserClass;
begin
  Result := TParser;
end;

{ TEntitySelector.TOptionIdentifier }

function TEntitySelector.TOptionIdentifier.FormatData: string;
begin
  Result := inherited + Text;
end;

class function TEntitySelector.TOptionIdentifier.GetDataParserClass: TParserClass;
begin
  Result := TParser;
end;

{ TEntitySelector.TOption.TParser }

class function TEntitySelector.TOption.TParser.GetResultName: string;
begin
  Result := 'Entity Selector Option';
end;

function TEntitySelector.TOption.TParser.GetSelector: TEntitySelector;
begin
  Result := FSelector;
end;

class function TEntitySelector.TOption.TParser.GetTokenCount: Integer;
begin
  Result := High(TokenNames);
end;

class function TEntitySelector.TOption.TParser.GetTokenName(AIndex: Integer): string;
begin
  Result := TokenNames[AIndex];
end;

function TEntitySelector.TOption.TParser.Parse: Boolean;
var
  OptionName: string;
  OptionClass: TOptionClass;
  Option: TOption;
  FoundInverted: Boolean;
  Marker: TLogMarker;
begin
  BeginSuggestions(TSuggestions.Create(Selector));
  SkipWhitespace;

  Marker := GetMarker;

  Token := TokenOption;

  OptionName := ReadWhile(IdentChars);

  if OptionName.IsEmpty then
    Exit(False);

  OptionClass := TOption.GetClass(OptionName);
  if OptionClass = nil then
    raise EParseError.CreateFmt('Unknown entity selector option "%s".', [OptionName], OptionName.Length);

  FoundInverted := False;
  if Selector <> nil then
  begin
    if not OptionClass.IsApplicable(Selector.Variable) then
      Log(-OptionName.Length, '"%s" is not applicable for "@%s".',
        [OptionClass.GetName, Selector.VariableChar]);

    case OptionClass.GetQuantityType of
      qtSingle:
        for Option in Selector.Options do
          if Option is OptionClass then
            Log(-OptionName.Length, 'Multiple occurences of "%s" are not valid.',
              [OptionClass.GetName]);
      qtMultipleInverted:
        begin
          for Option in Selector.Options do
          begin
            if Option is OptionClass then
            begin
              if not TOptionInvertible(Option).Inverted then
                Log(-OptionName.Length, 'Entity selector option "%s" got set already.',
                  [OptionClass.GetName])
              else
                FoundInverted := True;
            end;
          end;
        end;
    end;
  end;

  ResetToken;

  EndSuggestions;

  SkipWhitespace;

  Token := TokenEquals;

  if not StartsWith('=') then
    raise EParseError.Create('Expected "=".');

  ResetToken;

  SkipWhitespace;

  ParseResult := OptionClass.Create(Selector);

  Token := TokenInvert;
  if ParseResult is TOptionInvertible then
  begin
    if StartsWith('!') then
    begin
      TOptionInvertible(ParseResult).Inverted := True;
      ResetToken;
      SkipWhitespace;
    end
    else if FoundInverted then
    begin
      Log(Marker, 'Previous inverted option "%s" would be useless.', [OptionClass.GetName], elWarning);
    end;
  end;

  ResetToken;

  ParseResult.DataParser.Parse(ParseResult, Info, True);

  Result := True;
end;

procedure TEntitySelector.TOption.TParser.SetSelector(const Value: TEntitySelector);
begin
  FSelector := Value;
end;

{ TEntitySelector.TOptionValueOrRange.TParser }

class function TEntitySelector.TOptionValueOrRange.TParser.GetResultName: string;
begin
  Result := 'Value or Range of Values';
end;

function TEntitySelector.TOptionValueOrRange.TParser.Parse: Boolean;
var
  Value: Single;
  Marker: TLogMarker;
  Range: TBounds1;
begin
  Marker := GetMarker;
  Range := RangeParser.Require(Info);
  ParseObject.Range := Range;

  if ParseObject.MustBeNormalized and not Range.Normalized then
    Log(Marker, 'The minumum can''t be bigger than the maximum.');

  if IsInfinite(Range.C1) then
    ParseObject.GetMinimum(Range.C1);
  if IsInfinite(Range.C2) then
    ParseObject.GetMaximum(Range.C2);

  Range := Range.Normalize;

  if ParseObject.GetMinimum(Value) and not(Range >= Value) then
    Log(Marker, 'The value must be at least %f.', [Value]);
  if ParseObject.GetMaximum(Value) and not(Range <= Value) then
    Log(Marker, 'The value must be at most %f.', [Value]);

  Result := True;
end;

{ TEntitySelector.TOptionNBT.TParser }

class function TEntitySelector.TOptionNBT.TParser.GetResultName: string;
begin
  Result := 'NBT Option';
end;

class function TEntitySelector.TOptionNBT.TParser.IgnoreContext: Boolean;
begin
  Result := True;
end;

function TEntitySelector.TOptionNBT.TParser.Parse: Boolean;
begin
  ParseObject.FNBT := TNBTCompound.Parser.Require(Info);
  Result := True;
end;

{ TEntitySelector.TOptionString.TParser }

class function TEntitySelector.TOptionString.TParser.GetResultName: string;
begin
  Result := 'String';
end;

class function TEntitySelector.TOptionString.TParser.IgnoreContext: Boolean;
begin
  Result := True;
end;

function TEntitySelector.TOptionString.TParser.Parse: Boolean;
begin
  ParseObject.Text := TNBTString.StringOrIdentParser.Require(Info);
  Result := True;
end;

{ TEntitySelector.TOptionIdentifier.TParser }

class function TEntitySelector.TOptionIdentifier.TParser.GetResultName: string;
begin
  Result := 'Identifier';
end;

function TEntitySelector.TOptionIdentifier.TParser.Parse: Boolean;
begin
  ParseObject.Text := ReadWhile(IdentChars);
  Result := True;
end;

{ TEntitySelector.TOptionInteger.TParser }

class function TEntitySelector.TOptionInteger.TParser.GetResultName: string;
begin
  Result := 'Integer';
end;

function TEntitySelector.TOptionInteger.TParser.Parse: Boolean;
var
  Value, Bound: Integer;
  Marker: TLogMarker;
begin
  Marker := GetMarker;
  Result := TryStrToInt(ReadWhile(IntChars), Value);
  if not Result then
    Exit;
  ParseObject.Value := Value;
  if ParseObject.GetMinimum(Bound) and (Value < Bound) then
    Log(Marker, 'The value must be at least %d.', [Bound]);
  if ParseObject.GetMaximum(Bound) and (Value > Bound) then
    Log(Marker, 'The value must be at most %d.', [Bound]);
end;

{ TEntitySelector.TOptionFloat.TParser }

class function TEntitySelector.TOptionFloat.TParser.GetResultName: string;
begin
  Result := 'Float';
end;

function TEntitySelector.TOptionFloat.TParser.Parse: Boolean;
var
  Value: Single;
begin
  Result := TryStrToFloat(ReadWhile(FloatChars), Value, TFormatSettings.Invariant);
  if Result then
    ParseObject.Value := Value;
end;

{ TEntitySelector.TOptionSort.TParser }

class function TEntitySelector.TOptionSort.TParser.GetResultName: string;
begin
  Result := 'Sort Type';
end;

function TEntitySelector.TOptionSort.TParser.Parse: Boolean;
var
  Text: string;
  SortType: TSortType;
begin
  BeginSuggestions(TSuggestions);
  Text := ReadWhile(IdentChars);
  Result := not Text.IsEmpty;
  if not Result then
    Exit;

  for SortType := Low(TSortType) to High(TSortType) do
    if SortTypeNames[SortType] = Text then
    begin
      ParseObject.SortType := SortType;
      Exit;
    end;

  Log(-Text.Length, '"%s" is not a valid sort type.', [Text], elFatal);
end;

{ TEntitySelector.TOptionGamemode.TParser }

class function TEntitySelector.TOptionGamemode.TParser.GetResultName: string;
begin
  Result := 'Gamemode';
end;

function TEntitySelector.TOptionGamemode.TParser.Parse: Boolean;
var
  Name: string;
  Gamemode: TGamemode;
begin
  BeginSuggestions(TSuggestions);
  Name := ReadWhile(IdentChars);
  Result := not Name.IsEmpty;
  if not Result then
    Exit;
  if GamemodeFromName(Name, Gamemode) then
    ParseObject.Gamemode := Gamemode
  else
    Log(-Name.Length, '"%s" is not a valid gamemode.', [Name], elFatal);
end;

{ TEntitySelector.TOptionScores.TParser }

class function TEntitySelector.TOptionScores.TParser.GetResultName: string;
begin
  Result := 'Scores';
end;

class function TEntitySelector.TOptionScores.TParser.GetTokenCount: Integer;
begin
  Result := High(TokenNames);
end;

class function TEntitySelector.TOptionScores.TParser.GetTokenName(AIndex: Integer): string;
begin
  Result := TokenNames[AIndex];
end;

function TEntitySelector.TOptionScores.TParser.Parse: Boolean;
var
  Name: string;
begin
  ParseObject.Scores.Clear;

  Token := TokenBrackets;

  if not StartsWith('{') then
    Exit(False);

  ResetToken;

  SkipWhitespace;

  Token := TokenBrackets;

  if StartsWith('}') then
    Exit(True);

  while True do
  begin
    Token := TokenName;

    Name := ReadWhile(IdentChars);
    if Name.IsEmpty then
      raise EParseError.Create('Expected score name.');

    ResetToken;

    SkipWhitespace;

    Token := TokenEquals;

    if not StartsWith('=') then
      raise EParseError.Create('Expected "=".');

    ResetToken;

    SkipWhitespace;

    ParseObject.Scores.Add(TScore.Create(Name, IntRangeParser.Require(Info)));

    SkipWhitespace;

    Token := TokenBrackets;

    if StartsWith('}') then
      Break;

    Token := TokenComma;

    if not StartsWith(',') then
      raise EParseError.Create('Expected "}" or ",".');

    ResetToken;

    SkipWhitespace;
  end;

  Result := True;
end;

{ TEntitySelector.TIntRangeParser }

class function TEntitySelector.TIntRangeParser.Format(ARange: TIntBounds1): string;

  function Format(AValue: Single): string;
  begin
    Result := PrettyFloat(AValue);
  end;

begin
  if ARange.C1 = ARange.C2 then
    Exit(Format(ARange.C1));
  Result := '..';
  if ARange.C1 <> Integer.MinValue then
    Result := Format(ARange.C1) + Result;
  if ARange.C2 <> Integer.MaxValue then
    Result := Result + Format(ARange.C2);
end;

class function TEntitySelector.TIntRangeParser.GetResultName: string;
begin
  Result := 'Integer or Range';
end;

class function TEntitySelector.TIntRangeParser.GetTokenCount: Integer;
begin
  Result := High(TokenNames);
end;

class function TEntitySelector.TIntRangeParser.GetTokenName(AIndex: Integer): string;
begin
  Result := TokenNames[AIndex];
end;

function TEntitySelector.TIntRangeParser.Parse: Boolean;
var
  OldPos: Integer;

  function ParseInteger(AText: string): Integer;
  begin
    if not TryStrToInt(AText, Result) then
      raise EParseError.Create('Invalid integer or range.', Info.Pos - OldPos);
  end;

  function ParseOptionaInteger(AText: string; APositive: Boolean): Integer;
  begin
    if AText.IsEmpty then
      Exit(IfThen(APositive, Integer.MaxValue, Integer.MinValue));
    Result := ParseInteger(AText);
  end;

const
  Splitter = '..';
var
  RangeMin, RangeMax: string;
  Range: TIntBounds1;
  Split: Boolean;
begin
  OldPos := Info.Pos;
  Split := False;
  RangeMin := '';
  while True do
  begin
    Token := TokenSplitter;
    if StartsWith(Splitter) then
    begin
      Split := True;
      Break;
    end;
    if not CharInSet(First, IntChars) then
      Break;
    RangeMin := RangeMin + First;
    Token := TokenValue;
    Advance;
  end;
  if Split then
  begin
    Token := TokenValue;
    RangeMax := ReadWhile(IntChars);
    Range.C1 := ParseOptionaInteger(RangeMin, False);
    Range.C2 := ParseOptionaInteger(RangeMax, True);
    if (Range.C1 = Integer.MinValue) and (Range.C2 = Integer.MaxValue) then
      Exit(False);
  end
  else
  begin
    Range.C1 := ParseInteger(RangeMin);
    Range.C2 := Range.C1;
  end;
  ParseResult := Range;
  Result := True;
end;

{ TEntitySelector.TRangeParser }

class function TEntitySelector.TRangeParser.Format(ARange: TBounds1): string;

  function Format(AValue: Single): string;
  begin
    Result := PrettyFloat(AValue);
  end;

begin
  if ARange.C1 = ARange.C2 then
    Exit(Format(ARange.C1));
  Result := '..';
  if not ARange.C1.IsNegativeInfinity then
    Result := Format(ARange.C1) + Result;
  if not ARange.C2.IsPositiveInfinity then
    Result := Result + Format(ARange.C2);
end;

class function TEntitySelector.TRangeParser.GetResultName: string;
begin
  Result := 'Float or Range';
end;

class function TEntitySelector.TRangeParser.GetTokenCount: Integer;
begin
  Result := High(TokenNames);
end;

class function TEntitySelector.TRangeParser.GetTokenName(AIndex: Integer): string;
begin
  Result := TokenNames[AIndex];
end;

function TEntitySelector.TRangeParser.Parse: Boolean;
var
  OldPos: Integer;

  function ParseFloat(AText: string): Double;
  begin
    if not TryStrToFloat(AText, Result, TFormatSettings.Invariant) then
      raise EParseError.Create('Invalid float value.', Info.Pos - OldPos);
  end;

  function ParseOptionalFloat(AText: string; APositive: Boolean): Double;
  begin
    if AText.IsEmpty then
      Exit(IfThen(APositive, Infinity, -Infinity));
    Result := ParseFloat(AText);
  end;

const
  Splitter = '..';
var
  RangeMin, RangeMax: string;
  Range: TBounds1;
  Split: Boolean;
begin
  OldPos := Info.Pos;
  Split := False;
  RangeMin := '';
  while True do
  begin
    Token := TokenSplitter;
    if StartsWith(Splitter) then
    begin
      Split := True;
      Break;
    end;
    if not CharInSet(First, FloatChars) then
      Break;
    RangeMin := RangeMin + First;
    Token := TokenValue;
    Advance;
  end;
  if Split then
  begin
    Token := TokenValue;
    RangeMax := ReadWhile(FloatChars);
    Range.C1 := ParseOptionalFloat(RangeMin, False);
    Range.C2 := ParseOptionalFloat(RangeMax, True);
    if Range.C1.IsNegativeInfinity and Range.C2.IsPositiveInfinity then
      Exit(False);
  end
  else
  begin
    Range.C1 := ParseFloat(RangeMin);
    Range.C2 := Range.C1;
  end;
  ParseResult := Range;
  Result := True;
end;

{ TEntitySelector.TOptionAdvancements.TParser }

class function TEntitySelector.TOptionAdvancements.TParser.GetResultName: string;
begin
  Result := 'Advancements';
end;

class function TEntitySelector.TOptionAdvancements.TParser.GetTokenCount: Integer;
begin
  Result := High(TokenNames);
end;

class function TEntitySelector.TOptionAdvancements.TParser.GetTokenName(AIndex: Integer): string;
begin
  Result := TokenNames[AIndex];
end;

function TEntitySelector.TOptionAdvancements.TParser.Parse: Boolean;
var
  Name: string;
begin
  ParseObject.Advancements.Clear;

  Token := TokenBrackets;

  if not StartsWith('{') then
    Exit(False);

  ResetToken;

  SkipWhitespace;

  Token := TokenBrackets;

  if StartsWith('}') then
    Exit(True);

  while True do
  begin
    Token := TokenName;

    Name := ReadWhile(NamespaceChars);
    if Name.IsEmpty then
      raise EParseError.Create('Expected advancement name.');

    ResetToken;

    SkipWhitespace;

    Token := TokenEquals;

    if not StartsWith('=') then
      raise EParseError.Create('Expected "=".');

    ResetToken;

    SkipWhitespace;

    Token := TokenBoolean;

    if StartsWith('true') then
      ParseObject.Advancements.Add(TAdvancement.Create(Name, True))
    else if StartsWith('false') then
      ParseObject.Advancements.Add(TAdvancement.Create(Name, False))
    else
      raise EParseError.Create('Expected true or false.');

    ResetToken;

    SkipWhitespace;

    Token := TokenBrackets;

    if StartsWith('}') then
      Break;

    Token := TokenComma;

    if not StartsWith(',') then
      raise EParseError.Create('Expected "}" or ",".');

    ResetToken;

    SkipWhitespace;
  end;

  Result := True;
end;

{ TEntitySelector.TOptionType.TParser }

class function TEntitySelector.TOptionType.TParser.GetResultName: string;
begin
  Result := 'Entity Type';
end;

function TEntitySelector.TOptionType.TParser.Parse: Boolean;
var
  NSPath: TNSPath;
  Entity: TEntity;
  Marker: TLogMarker;
begin
  Marker := GetMarker;
  BeginSuggestions(TSuggestions.Create);
  NSPath := ReadWhile(NamespacePathChars);
  Result := not NSPath.IsEmpty;
  if not Result then
    Exit;
  if EntityFromName(NSPath, Entity) then
    ParseObject.Entity := Entity
  else
    Log(Marker, '"%s" is not a valid entity type.', [NSPath.Format], elFatal);
end;

{ TEntitySelector.TSuggesions }

class function TEntitySelector.TSuggestions.GetCount: Integer;
begin
  Result := Ord(High(TVariable)) + 1;
end;

class function TEntitySelector.TSuggestions.GetSuggestion(AIndex: Integer): TParseSuggestion;
begin
  Result.Display := System.SysUtils.Format('@%s (%s)',
    [VariableChars[TVariable(AIndex)], VariableDisplayNames[TVariable(AIndex)]]);
  Result.Insert := System.SysUtils.Format('@%s', [VariableChars[TVariable(AIndex)]]);
end;

{ TEntitySelector.TOption.TSuggestions }

constructor TEntitySelector.TOption.TSuggestions.Create(ASelector: TEntitySelector);
begin
  inherited Create;
  FSelector := ASelector;
end;

procedure TEntitySelector.TOption.TSuggestions.Generate;
var
  Option: TOptionClass;
begin
  // TODO: More advanced checking, if the type would actually still make sense, like multiple limits
  for Option in OptionClasses do
    if (FSelector = nil) or Option.IsApplicable(FSelector.Variable) then
      AddSuggestion(Option.GetName);
end;

{ TEntitySelector.TOptionIntValueOrRange }

function TEntitySelector.TOptionIntValueOrRange.FormatData: string;
begin
  Result := TIntRangeParser.Format(Range);
end;

class function TEntitySelector.TOptionIntValueOrRange.GetDataParserClass: TParserClass;
begin
  Result := TParser;
end;

class function TEntitySelector.TOptionIntValueOrRange.GetMaximum(out AValue: Integer): Boolean;
begin
  Result := False;
end;

class function TEntitySelector.TOptionIntValueOrRange.GetMinimum(out AValue: Integer): Boolean;
begin
  Result := False;
end;

class function TEntitySelector.TOptionIntValueOrRange.MustBeNormalized: Boolean;
begin
  Result := True;
end;

{ TEntitySelector.TOptionIntValueOrRange.TParser }

class function TEntitySelector.TOptionIntValueOrRange.TParser.GetResultName: string;
begin
  Result := 'Integer or Range of Integers';
end;

function TEntitySelector.TOptionIntValueOrRange.TParser.Parse: Boolean;
var
  Value: Integer;
  Marker: TLogMarker;
  Range: TIntBounds1;
begin
  Marker := GetMarker;
  Range := IntRangeParser.Require(Info);
  ParseObject.Range := Range;

  if ParseObject.MustBeNormalized and not Range.Normalized then
    Log(Marker, 'The minumum can''t be bigger than the maximum.');

  if Range.C1 = Integer.MinValue then
    ParseObject.GetMinimum(Range.C1);
  if Range.C2 = Integer.MaxValue then
    ParseObject.GetMaximum(Range.C2);

  Range := Range.Normalize;

  if ParseObject.GetMinimum(Value) and not(Range >= Value) then
    Log(Marker, 'The value must be at least %d.', [Value]);
  if ParseObject.GetMaximum(Value) and not(Range <= Value) then
    Log(Marker, 'The value must be at most %d.', [Value]);

  Result := True;
end;

{ TEntitySelector.TOptionType.TSuggestions }

procedure TEntitySelector.TOptionType.TSuggestions.Generate;
var
  Entity: TEntity;
begin
  for Entity := Low(TEntity) to High(TEntity) do
    AddSuggestion(EntityNames[Entity]);
  AddSuggestion(TNSPath.Empty.Format);
  for Entity := Low(TEntity) to High(TEntity) do
    AddSuggestion(NSPath(EntityNames[Entity]).Format);
end;

{ TEntitySelector.TOptionSort.TSuggestions }

class function TEntitySelector.TOptionSort.TSuggestions.GetCount: Integer;
begin
  Result := Length(SortTypeNames);
end;

class function TEntitySelector.TOptionSort.TSuggestions.GetSuggestion(AIndex: Integer): TParseSuggestion;
begin
  Result := SortTypeNames[TSortType(AIndex)];
end;

{ TEntitySelector.TOptionGamemode.TSuggestions }

class function TEntitySelector.TOptionGamemode.TSuggestions.GetCount: Integer;
begin
  Result := Length(GamemodeNames);
end;

class function TEntitySelector.TOptionGamemode.TSuggestions.GetSuggestion(AIndex: Integer): TParseSuggestion;
begin
  Result.Display := GamemodeDisplayNames[TGamemode(AIndex)];
  Result.Insert := GamemodeNames[TGamemode(AIndex)];
end;

{ TEntitySelectorSettings }
{
  procedure TEntitySelectorSettings.DefineJStorage(ASerializer: TJSerializer);
  begin
  inherited;
  with ASerializer do
  begin
  Define('entity_namespace_prefix', FEntityNamespacePrefix);
  Define('space_after_comma', FSpaceAfterComma);
  end;
  end;

  class function TEntitySelectorSettings.GetNameForVersion(AVersion: Integer): string;
  begin
  Result := 'mc_entityselector';
  end;

  procedure TEntitySelectorSettings.SetDefaults;
  begin
  FEntityNamespacePrefix := True;
  FSpaceAfterComma := True;
  end;
}
end.
