unit Pengine.MC.EntitySelector;

interface

uses
  System.SysUtils,
  System.Math,

  Pengine.Vector,
  Pengine.IntMaths,
  Pengine.Parser,
  Pengine.ObservableCollections,
  Pengine.Collections,

  Pengine.MC.NBT,
  Pengine.MC.Gamemode,
  Pengine.MC.Entity,
  Pengine.MC.General;

type

  TEntitySelector = class
  public type

    TVariable = (
      svExecutor,
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

    IntChars = ['0' .. '9', '-'];
    FloatChars = IntChars + ['.'];

  public type

    TIntRangeParser = class(TParser<TIntBounds1>)
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

    TRangeParser = class(TParser<TBounds1>)
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

      TParser = class(TObjectParser<TOption>)
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

      protected
        function Parse: Boolean; override;

      public
        constructor Create(AInfo: TParseInfo; ASelector: TEntitySelector; ARequired: Boolean = True);

        class function GetResultName: string; override;
        class function GetTokenCount: Integer; override;
        class function GetTokenName(AIndex: Integer): string; override;

        property Selector: TEntitySelector read FSelector;

      end;

      TDataParser = class(TRefParser<TOption>);
      TDataParserClass = class of TDataParser;

      TDataParser<T: TOption> = class(TDataParser)
      private
        function GetParseObject: T;

      public
        property ParseObject: T read GetParseObject;

      end;

    private
      FSelector: TEntitySelector;

      function GetIndex: Integer;
      procedure SetIndex(const Value: Integer);

    public
      constructor Create(ASelector: TEntitySelector); virtual;

      class function GetClass(AName: string): TOptionClass; static;
      class function CreateTyped(ASelector: TEntitySelector; AName: string): TOption; static;

      class function GetDataParserClass: TDataParserClass; virtual; abstract;
      class function GetType: TType; virtual; abstract;
      class function GetName: string;
      class function IsApplicable(AVariable: TVariable): Boolean; virtual;
      class function GetQuantityType: TQuantityType; virtual;

      property Selector: TEntitySelector read FSelector;
      property Index: Integer read GetIndex write SetIndex;

      function FormatData: string; virtual; abstract;
      function Format: string;

    end;

    TOptions = TObservableObjectArray<TOption>;

    TOptionValueOrRange = class(TOption)
    public type

      TParser = class(TDataParser<TOptionValueOrRange>)
      protected
        function Parse: Boolean; override;

      public
        class function GetResultName: string; override;

      end;

    private
      FRange: TBounds1;

    public
      class function GetDataParserClass: TOption.TDataParserClass; override;

      property Range: TBounds1 read FRange write FRange;
      property RangeMin: Single read FRange.C1 write FRange.C1;
      property RangeMax: Single read FRange.C2 write FRange.C2;

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
      class function GetDataParserClass: TOption.TDataParserClass; override;

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
      class function GetDataParserClass: TOption.TDataParserClass; override;

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
        class function IgnoreCharInfo: Boolean; override;

      end;

    private
      FText: string;

    public
      class function GetDataParserClass: TOption.TDataParserClass; override;

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
      class function GetDataParserClass: TOption.TDataParserClass; override;

      property Text: string read FText write FText;

      function FormatData: string; override;

    end;

    TOptionLimit = class(TOptionInteger)
    public
      class function GetType: TOption.TType; override;
      class function IsApplicable(AVariable: TVariable): Boolean; override;

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
      class function GetDataParserClass: TOption.TDataParserClass; override;

      property SortType: TSortType read FSortType write FSortType;
      property SortTypeName: string read GetSortTypeName;

      function FormatData: string; override;

    end;

    TOptionDistance = class(TOptionValueOrRange)
    public
      class function GetType: TOption.TType; override;

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

    end;

    TOptionYRotation = class(TOptionValueOrRange)
    public
      class function GetType: TOption.TType; override;

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
        class function IgnoreCharInfo: Boolean; override;

      end;

    private
      FNBT: TNBTCompound;

      function GetNBT: TNBTCompound;

    public
      destructor Destroy; override;

      class function GetType: TOption.TType; override;
      class function GetQuantityType: TOption.TQuantityType; override;
      class function GetDataParserClass: TOption.TDataParserClass; override;

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
      class function GetDataParserClass: TOption.TDataParserClass; override;

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

    private
      FEntity: TEntity;

    public
      class function GetType: TOption.TType; override;
      class function GetQuantityType: TOption.TQuantityType; override;
      class function IsApplicable(AVariable: TVariable): Boolean; override;

      property Entity: TEntity read FEntity write FEntity;

      class function GetDataParserClass: TOption.TDataParserClass; override;

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
      class function GetDataParserClass: TOption.TDataParserClass; override;

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

    private
      FGamemode: TGamemode;

      function GetGamemodeName: string;

    public
      class function GetDataParserClass: TOption.TDataParserClass; override;
      class function GetType: TOption.TType; override;

      property Gamemode: TGamemode read FGamemode write FGamemode;
      property GamemodeName: string read GetGamemodeName;

      function FormatData: string; override;

    end;

    TOptionLevel = class(TOptionValueOrRange)
    public
      class function GetType: TOption.TType; override;

    end;

    TParser = class(TObjectParser<TEntitySelector>)
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
    constructor Create(AVariable: TVariable = svExecutor);
    destructor Destroy; override;

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
  OptionParser: TOption.TParser;
begin
  ExtraData := TokenPrefix;

  if not Info.StartsWith('@') then
    Exit(False);

  if Info.ReachedEnd then
    raise EParseError.Create(Info, 'Expected entity selector variable.');

  ExtraData := TokenVariable;

  for Variable := Low(TVariable) to High(TVariable) do
  begin
    if Info.StartsWith(VariableChars[Variable]) then
    begin
      SetParseResult(TEntitySelector.Create(Variable));
      Break;
    end;
  end;

  if ParseResult = nil then
    raise EParseError.CreateFmt(Info, 'Invalid entity selector variable "@%s".', [Info.First]);

  ExtraData := TokenBrackets;

  if not Info.StartsWith('[') then
    Exit(True);

  ResetExtraData;

  Info.SkipWhitespace;

  ExtraData := TokenBrackets;

  if Info.StartsWith(']') then
    Exit(True);

  while True do
  begin
    ResetExtraData;

    OptionParser := TOption.TParser.Create(Info, ParseResult);
    ParseResult.Options.Add(OptionParser.OwnParseResult);
    OptionParser.Free;

    Info.SkipWhitespace;

    ExtraData := TokenBrackets;

    if Info.StartsWith(']') then
      Break;

    ExtraData := TokenComma;

    if not Info.StartsWith(',') then
      raise EParseError.Create(Info, 'Expected "," or "]".');

    ResetExtraData;

    Info.SkipWhitespace;
  end;

  Result := True;
end;

{ TEntitySelector.TOptionLimit }

class function TEntitySelector.TOptionLimit.GetType: TOption.TType;
begin
  Result := soLimit;
end;

class function TEntitySelector.TOptionLimit.IsApplicable(AVariable: TVariable): Boolean;
begin
  Result := AVariable <> svExecutor;
end;

{ TEntitySelector.TOptionSort }

function TEntitySelector.TOptionSort.FormatData: string;
begin
  Result := SortTypeName;
end;

class function TEntitySelector.TOptionSort.GetDataParserClass: TOption.TDataParserClass;
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
  Result := AVariable <> svExecutor;
end;

{ TEntitySelector.TOptionDistance }

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

{ TEntitySelector.TOptionYRotation }

class function TEntitySelector.TOptionYRotation.GetType: TOption.TType;
begin
  Result := soYRotation;
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

class function TEntitySelector.TOptionNBT.GetDataParserClass: TOption.TDataParserClass;
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

class function TEntitySelector.TOptionScores.GetDataParserClass: TOption.TDataParserClass;
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
  Result := inherited + EntityNames[Entity];
end;

class function TEntitySelector.TOptionType.GetDataParserClass: TOption.TDataParserClass;
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
  Result := AVariable in [svExecutor, svAllEntites];
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

class function TEntitySelector.TOptionAdvancements.GetDataParserClass: TOption.TDataParserClass;
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

class function TEntitySelector.TOptionGamemode.GetDataParserClass: TOption.TDataParserClass;
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

class function TEntitySelector.TOptionLevel.GetType: TOption.TType;
begin
  Result := soLevel;
end;

{ TEntitySelector }

constructor TEntitySelector.Create(AVariable: TVariable = svExecutor);
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
      Result := Result + ', ';
  end;
  Result := Result + ']';
end;

function TEntitySelector.GetVariableChar: Char;
begin
  Result := VariableChars[Variable];
end;

{ TEntitySelector.TOptionValueOrRange }

function TEntitySelector.TOptionValueOrRange.FormatData: string;
begin
  Result := TRangeParser.Format(Range);
end;

class function TEntitySelector.TOptionValueOrRange.GetDataParserClass: TOption.TDataParserClass;
begin
  Result := TParser;
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

class function TEntitySelector.TOptionInteger.GetDataParserClass: TOption.TDataParserClass;
begin
  Result := TParser;
end;

{ TEntitySelector.TOptionFloat }

function TEntitySelector.TOptionFloat.FormatData: string;
begin
  Result := Value.ToString(ffGeneral, 7, 0, TFormatSettings.Invariant);
end;

class function TEntitySelector.TOptionFloat.GetDataParserClass: TOption.TDataParserClass;
begin
  Result := TParser;
end;

{ TEntitySelector.TOptionString }

function TEntitySelector.TOptionString.FormatData: string;
begin
  Result := inherited + '"' + Text.Replace('\', '\\').Replace('"', '\"') + '"';
end;

class function TEntitySelector.TOptionString.GetDataParserClass: TOption.TDataParserClass;
begin
  Result := TParser;
end;

{ TEntitySelector.TOptionIdentifier }

function TEntitySelector.TOptionIdentifier.FormatData: string;
begin
  Result := inherited + Text;
end;

class function TEntitySelector.TOptionIdentifier.GetDataParserClass: TOption.TDataParserClass;
begin
  Result := TParser;
end;

{ TEntitySelector.TOption.TParser }

constructor TEntitySelector.TOption.TParser.Create(AInfo: TParseInfo; ASelector: TEntitySelector; ARequired: Boolean);
begin
  FSelector := ASelector;
  inherited Create(AInfo, ARequired);
end;

class function TEntitySelector.TOption.TParser.GetResultName: string;
begin
  Result := 'Entity Selector Option';
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
begin
  ExtraData := TokenOption;

  OptionName := Info.ReadWhile(IdentChars);

  if OptionName.IsEmpty then
    Exit(False);

  OptionClass := TOption.GetClass(OptionName);
  if OptionClass = nil then
    raise EParseError.CreateFmt(Info, 'Unknown entity selector option "%s".', [OptionName]);

  if not OptionClass.IsApplicable(Selector.Variable) then
    raise EParseError.CreateFmt(Info, '"%s" is not applicable to an entity selector "@%s".',
      [OptionClass.GetName, Selector.VariableChar]);

  case OptionClass.GetQuantityType of
    qtSingle:
      for Option in Selector.Options do
        if Option is OptionClass then
          raise EParseError.CreateFmt(Info, 'Multiple occurences of "%s" are not valid in an entity selector.',
            [OptionClass.GetName]);
    qtMultipleInverted:
      begin
        FoundInverted := False;
        for Option in Selector.Options do
        begin
          if Option is OptionClass then
          begin
            if not TOptionInvertible(Option).Inverted then
              raise EParseError.CreateFmt(Info, 'Entity selector option "%s" got set already.',
                [OptionClass.GetName]);
            FoundInverted := True;
          end;
        end;
      end;
  end;

  ResetExtraData;

  Info.SkipWhitespace;

  ExtraData := TokenEquals;

  if not Info.StartsWith('=') then
    raise EParseError.Create(Info, 'Expected "=".');

  ResetExtraData;

  Info.SkipWhitespace;

  SetParseResult(OptionClass.Create(Selector));

  ExtraData := TokenInvert;
  if ParseResult is TOptionInvertible then
  begin
    if Info.StartsWith('!') then
    begin
      TOptionInvertible(ParseResult).Inverted := True;
      ResetExtraData;
      Info.SkipWhitespace;
    end
    else if FoundInverted then
    begin
      raise EParseError.CreateFmt(Info, 'Previous inverted entity selector options "%s" would be useless.',
        [OptionClass.GetName]);
    end;
  end;

  ResetExtraData;

  ParseResult.GetDataParserClass.Create(ParseResult, Info).Free;

  Result := True;
end;

{ TEntitySelector.TOption.TDataParser<T> }

function TEntitySelector.TOption.TDataParser<T>.GetParseObject: T;
begin
  Result := T(inherited ParseObject);
end;

{ TEntitySelector.TOptionValueOrRange.TParser }

class function TEntitySelector.TOptionValueOrRange.TParser.GetResultName: string;
begin
  Result := 'Value or Range of Values';
end;

function TEntitySelector.TOptionValueOrRange.TParser.Parse: Boolean;
var
  Parser: TRangeParser;
begin
  Parser := TRangeParser.Create(Info);
  ParseObject.Range := Parser.ParseResult;
  Parser.Free;
  Result := True;
end;

{ TEntitySelector.TOptionNBT.TParser }

class function TEntitySelector.TOptionNBT.TParser.GetResultName: string;
begin
  Result := 'NBT Option';
end;

class function TEntitySelector.TOptionNBT.TParser.IgnoreCharInfo: Boolean;
begin
  Result := True;
end;

function TEntitySelector.TOptionNBT.TParser.Parse: Boolean;
var
  Parser: TNBTParserCompound;
begin
  Parser := TNBTParserCompound.Create(Info, True);
  ParseObject.FNBT := Parser.OwnParseResult;
  Parser.Free;
  Result := True;
end;

{ TEntitySelector.TOptionString.TParser }

class function TEntitySelector.TOptionString.TParser.GetResultName: string;
begin
  Result := 'String';
end;

class function TEntitySelector.TOptionString.TParser.IgnoreCharInfo: Boolean;
begin
  Result := True;
end;

function TEntitySelector.TOptionString.TParser.Parse: Boolean;
var
  Parser: TStringOrIdentParser;
begin
  Parser := TStringOrIdentParser.Create(Info, True);
  ParseObject.Text := Parser.ParseResult;
  Parser.Free;
  Result := True;
end;

{ TEntitySelector.TOptionIdentifier.TParser }

class function TEntitySelector.TOptionIdentifier.TParser.GetResultName: string;
begin
  Result := 'Identifier';
end;

function TEntitySelector.TOptionIdentifier.TParser.Parse: Boolean;
begin
  ParseObject.Text := Info.ReadWhile(IdentChars);
  Result := True;
end;

{ TEntitySelector.TOptionInteger.TParser }

class function TEntitySelector.TOptionInteger.TParser.GetResultName: string;
begin
  Result := 'Integer';
end;

function TEntitySelector.TOptionInteger.TParser.Parse: Boolean;
var
  Value: Integer;
begin
  Result := TryStrToInt(Info.ReadWhile(IntChars), Value);
  if Result then
    ParseObject.Value := Value;
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
  Result := TryStrToFloat(Info.ReadWhile(IntChars), Value, TFormatSettings.Invariant);
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
  Text := Info.ReadWhile(IdentChars);
  for SortType := Low(TSortType) to High(TSortType) do
    if SortTypeNames[SortType] = Text then
    begin
      ParseObject.SortType := SortType;
      Exit(True);
    end;
  Result := False;
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
  Name := Info.ReadWhile(IdentChars);
  Result := GamemodeFromName(Name, Gamemode);
  if Result then
    ParseObject.Gamemode := Gamemode;
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
  Parser: TIntRangeParser;
begin
  ParseObject.Scores.Clear;

  ExtraData := TokenBrackets;

  if not Info.StartsWith('{') then
    Exit(False);

  ResetExtraData;

  Info.SkipWhitespace;

  ExtraData := TokenBrackets;

  if Info.StartsWith('}') then
    Exit(True);

  while True do
  begin
    ExtraData := TokenName;

    Name := Info.ReadWhile(IdentChars);
    if Name.IsEmpty then
      raise EParseError.Create(Info, 'Expected score name.');

    ResetExtraData;

    Info.SkipWhitespace;

    ExtraData := TokenEquals;

    if not Info.StartsWith('=') then
      raise EParseError.Create(Info, 'Expected "=".');

    ResetExtraData;

    Info.SkipWhitespace;

    Parser := TIntRangeParser.Create(Info, True);
    ParseObject.Scores.Add(TScore.Create(Name, Parser.ParseResult));
    Parser.Free;

    Info.SkipWhitespace;

    ExtraData := TokenBrackets;

    if Info.StartsWith('}') then
      Break;

    ExtraData := TokenComma;

    if not Info.StartsWith(',') then
      raise EParseError.Create(Info, 'Expected "}" or ",".');

    ResetExtraData;

    Info.SkipWhitespace;
  end;

  Result := True;
end;

{ TEntitySelector.TIntRangeParser }

class function TEntitySelector.TIntRangeParser.Format(ARange: TIntBounds1): string;

  function Format(AValue: Single): string;
  begin
    Result := AValue.ToString(ffGeneral, 7, 0, TFormatSettings.Invariant);
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

  function ParseInteger(AText: string): Integer;
  begin
    if not TryStrToInt(AText, Result) then
      raise EParseError.Create(Info, 'Invalid integer value.');
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
  Split := False;
  RangeMin := '';
  while True do
  begin
    ExtraData := TokenSplitter;
    if Info.StartsWith(Splitter) then
    begin
      Split := True;
      Break;
    end;
    if not CharInSet(Info.First, IntChars) then
      Break;
    RangeMin := RangeMin + Info.First;
    ExtraData := TokenValue;
    Info.Advance;
  end;
  if Split then
  begin
    ExtraData := TokenValue;
    RangeMax := Info.ReadWhile(IntChars);
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
  SetParseResult(Range);
  Result := True;
end;

{ TEntitySelector.TRangeParser }

class function TEntitySelector.TRangeParser.Format(ARange: TBounds1): string;

  function Format(AValue: Single): string;
  begin
    Result := AValue.ToString(ffGeneral, 7, 0, TFormatSettings.Invariant);
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

  function ParseFloat(AText: string): Double;
  begin
    if not TryStrToFloat(AText, Result, TFormatSettings.Invariant) then
      raise EParseError.Create(Info, 'Invalid float value.');
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
  Split := False;
  RangeMin := '';
  while True do
  begin
    ExtraData := TokenSplitter;
    if Info.StartsWith(Splitter) then
    begin
      Split := True;
      Break;
    end;
    if not CharInSet(Info.First, FloatChars) then
      Break;
    RangeMin := RangeMin + Info.First;
    ExtraData := TokenValue;
    Info.Advance;
  end;
  if Split then
  begin
    ExtraData := TokenValue;
    RangeMax := Info.ReadWhile(FloatChars);
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
  SetParseResult(Range);
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

  ExtraData := TokenBrackets;

  if not Info.StartsWith('{') then
    Exit(False);

  ResetExtraData;

  Info.SkipWhitespace;

  ExtraData := TokenBrackets;

  if Info.StartsWith('}') then
    Exit(True);

  while True do
  begin
    ExtraData := TokenName;

    Name := Info.ReadWhile(NamespaceChars);
    if Name.IsEmpty then
      raise EParseError.Create(Info, 'Expected advancement name.');

    ResetExtraData;

    Info.SkipWhitespace;

    ExtraData := TokenEquals;

    if not Info.StartsWith('=') then
      raise EParseError.Create(Info, 'Expected "=".');

    ResetExtraData;

    Info.SkipWhitespace;

    ExtraData := TokenBoolean;

    if Info.StartsWith('true') then
      ParseObject.Advancements.Add(TAdvancement.Create(Name, True))
    else if Info.StartsWith('false') then
      ParseObject.Advancements.Add(TAdvancement.Create(Name, False))
    else
      raise EParseError.Create(Info, 'Expected true or false.');

    ResetExtraData;

    Info.SkipWhitespace;

    ExtraData := TokenBrackets;

    if Info.StartsWith('}') then
      Break;

    ExtraData := TokenComma;

    if not Info.StartsWith(',') then
      raise EParseError.Create(Info, 'Expected "}" or ",".');

    ResetExtraData;

    Info.SkipWhitespace;
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
  Name: string;
  Entity: TEntity;
begin
  Name := Info.ReadWhile(NamespaceChars);
  Result := EntityFromName(Name, Entity);
  if Result then
    ParseObject.Entity := Entity;
end;

end.
