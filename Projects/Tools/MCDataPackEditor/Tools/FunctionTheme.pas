unit FunctionTheme;

interface

uses
  System.Classes,
  System.SysUtils,

  Vcl.Graphics,

  Pengine.Hasher,
  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.Parser,
  Pengine.IntMaths,
  Pengine.Color, // TODO: Remove this
  Pengine.JSON,

  Pengine.MC.General,
  Pengine.MC.Brigadier,
  Pengine.MC.NBT,
  Pengine.MC.EntitySelector,
  Pengine.MC.BrigadierParser,
  Pengine.MC.Vector,

  SynEditHighlighter;

type

  {
    Features for Function Theme:
    - Attributes
    |-> Optional Text Color
    |-> Optional Text Style
    '-> Optional Background Color
    - Global (non optional) Default Attributes
    - Assigning Attributes per Parser
    |-> Token Attributes
    |-> Default Attributes
    '-> Used if Token Attributes are not set
    - Category
    |-> A list of multile parsers, grouped together
    |-> Gives better overwiew
    |-> Always show a full category at once in the form
    '-> Has a preview text, that shows everything
    - Presets
  }

  {
    Categories:

    - Basic Types
    |-> TBrigadierBoolParser
    |-> TBrigadierIntegerParser
    |-> TBrigadierFloatParser
    |-> TBrigadierDoubleParser
    |-> TBrigadierStringParser
    '-> TBrigadierIntRangeParser

    - Positional
    |-> TBrigadierVec3Parser
    |-> TBrigadierBlockPosParser
    |-> TBrigadierSwizzleParser
    |-> TBrigadierVec2Parser
    |-> TBrigadierRotationParser
    |-> TBrigadierColumnPosParser
    '-> TBrigadierDimensionParser

    - Entity and Player
    |-> TBrigadierEntityParser
    |-> TBrigadierEntityAnchorParser
    |-> TBrigadierGameProfileParser
    |-> TBrigadierMobEffectParser
    |-> TBrigadierParticleParser
    '-> TBrigadierEntitySummonParser

    - NBT
    |-> TBrigadierNBTParser
    '-> TBrigadierNBTPathParser

    - Text
    |-> TBrigadierComponentParser
    |-> TBrigadierMessageParser
    '-> TBrigadierColorParser

    - Scoreboard
    |-> TBrigadierObjectiveParser
    |-> TBrigadierObjectiveCriteriaParser
    |-> TBrigadierScoreHolderParser
    |-> TBrigadierScoreboardSlotParser
    |-> TBrigadierOperationParser
    '-> TBrigadierTeamParser

    - Blocks and Items
    |-> TBrigadierItemStackParser
    |-> TBrigadierItemPredicateParser
    |-> TBrigadierItemEnchantmentParser
    |-> TBrigadierItemSlotParser
    |-> TBrigadierBlockPredicateParser
    '-> TBrigadierBlockStateParser

    - Resources
    |-> TBrigadierResourceLocationParser
    '-> TBrigadierFunctionParser
  }

  TFunctionTheme = class
  public type

    TTextAttributesBase = class
    private
      FForeground: TColor;
      FBackground: TColor;
      FStyle: TFontStyles;

    public
      constructor Create; overload;
      constructor Create(AForeground, ABackground: TColor; AStyles: TFontStyles = []); overload;

      property Foreground: TColor read FForeground write FForeground;
      property Background: TColor read FBackground write FBackground;
      property Style: TFontStyles read FStyle write FStyle;

      function ToSynAttributes: TSynHighlighterAttributes;
      procedure Apply(AAttributes: TSynHighlighterAttributes);

    end;

    TTextAttributes = class(TTextAttributesBase)
    private
      FOverrideBackground: Boolean;
      FOverrideForeground: Boolean;

    public
      property OverrideForeground: Boolean read FOverrideForeground write FOverrideForeground;
      property OverrideBackground: Boolean read FOverrideBackground write FOverrideBackground;

      procedure SetFg(AForeground: TColor; AStyle: TFontStyles = []); overload;

      procedure Merge(AAttributes: TSynHighlighterAttributes);

    end;

    TCategoryClass = class of TCategory;

    TCategory = class
    public
      class function GetName: string; virtual; abstract;
      class function GetParserCount: Integer; virtual; abstract;
      class function GetParser(AIndex: Integer): TParserClass; virtual; abstract;
      class function GetExample: string; virtual;

    end;

    TCategoryCommand = class(TCategory)
    public const

      Parsers: array [0 .. 0] of TParserClass = (
        TBrigadierCommandParser
        );

    public
      class function GetName: string; override;
      class function GetParserCount: Integer; override;
      class function GetParser(AIndex: Integer): TParserClass; override;

    end;

    TCategoryBasicTypes = class(TCategory)
    public const

      Parsers: array [0 .. 4] of TParserClass = (
        TBrigadierBoolParser,
        TBrigadierIntegerParser,
        TBrigadierFloatParser,
        TBrigadierDoubleParser,
        TBrigadierStringParser
        );

    public
      class function GetName: string; override;
      class function GetParserCount: Integer; override;
      class function GetParser(AIndex: Integer): TParserClass; override;

    end;

    TCategoryPositional = class(TCategory)
    public const

      Parsers: array [0 .. 9] of TParserClass = (
        // TODO: Tokenize
        TBrigadierVec3Parser,
        TBrigadierBlockPosParser,
        TBrigadierVec2Parser,
        TBrigadierRotationParser,
        TBrigadierColumnPosParser,

        TMCVec3.TParser,
        TMCVec2.TParser,
        TMCVecValue.TParser,

        TBrigadierSwizzleParser,
        TBrigadierDimensionParser
        );

    public
      class function GetName: string; override;
      class function GetParserCount: Integer; override;
      class function GetParser(AIndex: Integer): TParserClass; override;

    end;

    TCategoryEntityAndPlayer = class(TCategory)
    public const

      Parsers: array [0 .. 5] of TParserClass = (
        TBrigadierEntityParser,
        TBrigadierEntityAnchorParser,
        TBrigadierGameProfileParser,
        TBrigadierMobEffectParser,
        TBrigadierParticleParser,
        TBrigadierEntitySummonParser
        );

    public
      class function GetName: string; override;
      class function GetParserCount: Integer; override;
      class function GetParser(AIndex: Integer): TParserClass; override;

    end;

    TCategoryEntitySelector = class(TCategory)
    public const

      Parsers: array [0 .. 13] of TParserClass = (
        // TODO:Tokenize
        TEntitySelector.TParser,
        TEntitySelector.TOption.TParser,
        TEntitySelector.TOptionValueOrRange.TParser,
        TEntitySelector.TOptionIntValueOrRange.TParser,
        TEntitySelector.TOptionInteger.TParser,
        TEntitySelector.TOptionFloat.TParser,
        TEntitySelector.TOptionString.TParser,
        TEntitySelector.TOptionIdentifier.TParser,
        TEntitySelector.TOptionSort.TParser,
        TEntitySelector.TOptionNBT.TParser,
        TEntitySelector.TOptionScores.TParser,
        TEntitySelector.TOptionType.TParser,
        TEntitySelector.TOptionAdvancements.TParser,
        TEntitySelector.TOptionGamemode.TParser
        );

    public
      class function GetName: string; override;
      class function GetParserCount: Integer; override;
      class function GetParser(AIndex: Integer): TParserClass; override;

    end;

    TCategoryNBT = class(TCategory)
    public const

      Parsers: array [0 .. 7] of TParserClass = (
        TBrigadierNBTParser,
        TNBTCompound.TParser,
        TNBTListOrArrayParser,
        TNBTString.TStringOrIdentParser,
        TNBTString.TStringParser,
        TNBTNumberParser,

        TBrigadierNBTPathParser,
        TNBTPath.TParser
        );

    public
      class function GetName: string; override;
      class function GetParserCount: Integer; override;
      class function GetParser(AIndex: Integer): TParserClass; override;

    end;

    TCategoryText = class(TCategory)
    public const

      Parsers: array [0 .. 9] of TParserClass = (
        TBrigadierComponentParser,
        TJBase.TParser,
        TJObject.TParser,
        TJArray.TParser,
        TJString.TParser,
        TJNumber.TParser,
        TJBool.TParser,
        TJNull.TParser,
        TBrigadierMessageParser,
        TBrigadierColorParser
        );

    public
      class function GetName: string; override;
      class function GetParserCount: Integer; override;
      class function GetParser(AIndex: Integer): TParserClass; override;

    end;

    TCategoryScoreboard = class(TCategory)
    public const

      Parsers: array [0 .. 5] of TParserClass = (
        // TODO: Split up and tokenize
        TBrigadierObjectiveParser,
        TBrigadierObjectiveCriteriaParser,
        TBrigadierScoreHolderParser,
        TBrigadierScoreboardSlotParser,
        TBrigadierOperationParser,
        TBrigadierTeamParser
        );

    public
      class function GetName: string; override;
      class function GetParserCount: Integer; override;
      class function GetParser(AIndex: Integer): TParserClass; override;

    end;

    TCategoryBlocksAndItems = class(TCategory)
    public const

      Parsers: array [0 .. 5] of TParserClass = (
        // TODO: Split up and tokenize
        TBrigadierItemStackParser,
        TBrigadierItemPredicateParser,
        TBrigadierItemEnchantmentParser,
        TBrigadierItemSlotParser,
        TBrigadierBlockPredicateParser,
        TBrigadierBlockStateParser
        );

    public
      class function GetName: string; override;
      class function GetParserCount: Integer; override;
      class function GetParser(AIndex: Integer): TParserClass; override;

    end;

    TCategoryResources = class(TCategory)
    public const

      Parsers: array [0 .. 1] of TParserClass = (
        // TODO: Split up and tokenize
        TBrigadierResourceLocationParser,
        TBrigadierFunctionParser
        );

    public
      class function GetName: string; override;
      class function GetParserCount: Integer; override;
      class function GetParser(AIndex: Integer): TParserClass; override;

    end;

    TPreset = class
    public
      class function GetName: string; virtual; abstract;
      class procedure Apply(ATheme: TFunctionTheme); virtual; abstract;

    end;

    TParserTokenData = class
    public type

      TTokens = TObjectArray<TTextAttributes>;

    private
      FParserClass: TParserClass;
      FTokens: TTokens;

      function GetTokens: TTokens.TReader;
      function GetTokenName(AIndex: Integer): string;

    public
      constructor Create(AParserClass: TParserClass);
      destructor Destroy; override;

      property ParserClass: TParserClass read FParserClass;
      property TokenName[AIndex: Integer]: string read GetTokenName;
      property Tokens: TTokens.TReader read GetTokens;

    end;

    TPresetClass = class of TPreset;

    TPresets = TArray<TPresetClass>;

    TCategories = TArray<TCategoryClass>;

    TParsers = TClassObjectMap<TParserClass, TParserTokenData>;

  public const

    CategoryClasses: array [0 .. 9] of TCategoryClass = (
      TCategoryCommand,
      TCategoryBasicTypes,
      TCategoryPositional,
      TCategoryEntityAndPlayer,
      TCategoryEntitySelector,
      TCategoryNBT,
      TCategoryText,
      TCategoryScoreboard,
      TCategoryBlocksAndItems,
      TCategoryResources
      );

  private
    class var
      FPresets: TPresets;

  private
    FName: string;
    FFontName: TFontName;
    FDefault: TTextAttributesBase;
    FCurrentLineColor: TColor;
    FError: TTextAttributesBase;
    FParsers: TParsers;

    procedure InitParsers;
    function GetParsers: TParsers.TReader;
    function GetText(AParser: TParserClass; AToken: Integer): TTextAttributes;

  public
    class constructor Create;
    class destructor Destroy;

    constructor Create;
    destructor Destroy; override;

    procedure LoadPreset(APresetClass: TPresetClass);

    property Name: string read FName write FName;

    property FontName: TFontName read FFontName write FFontName;
    property DefaultAttributes: TTextAttributesBase read FDefault;
    property CurrentLineColor: TColor read FCurrentLineColor write FCurrentLineColor;
    property ErrorAttributes: TTextAttributesBase read FError;
    property Parsers: TParsers.TReader read GetParsers;

    procedure SetDefault(ATextColor, ABackgroundColor: TColor; AStyle: TFontStyles = []);
    procedure SetError(ATextColor, ABackgroundColor: TColor; AStyle: TFontStyles = []);
    property Text[AParser: TParserClass; AToken: Integer]: TTextAttributes read GetText;

    procedure Assign(ATheme: TFunctionTheme);
    function Copy: TFunctionTheme;

    class function Presets: TPresets.TReader;

  end;

  TLine = class
  private
    FText: string;
    FCommand: TBrigadierCommand;
    FContext: TParseInfo.TContext;
    FSettings: TBrigadierCommandParser.TSettings;
    FReferences: Integer;

    procedure Compile;
    procedure SetText(Value: string);

  public
    constructor Create(ASettings: TBrigadierCommandParser.TSettings; AText: string);
    destructor Destroy; override;

    property Text: string read FText write SetText;
    property Context: TParseInfo.TContext read FContext;
    property Command: TBrigadierCommand read FCommand;

    procedure AddRef;
    procedure RemoveRef;
    function HasReference: Boolean;

  end;

  TLines = TRefArray<TLine>;

  TFunctionHighlighter = class(TSynCustomHighlighter)
  public type

    TAttributeHasher = class(THasher<TSynHighlighterAttributes>)
    public
      class function Equal(const A, B: TSynHighlighterAttributes): Boolean; override;
      class function GetHash(const AValue: TSynHighlighterAttributes): Cardinal; override;

    end;

    TAttributes = TObjectSet<TSynHighlighterAttributes, TAttributeHasher>;

  private
    FLines: TLines.TReader;
    FTheme: TFunctionTheme;
    FDummyAttributes: TSynHighlighterAttributes;
    FCurrentAttributes: TSynHighlighterAttributes;

{$MESSAGE WARN 'Attributes are never cleared, and will accumulate. Reset on Theme change to get rid of not more possible colors.'}
    FAttributes: TAttributes;

    function GetAttributes: TSynHighlighterAttributes;
    procedure SetTheme(const Value: TFunctionTheme);

  protected
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Lines: TLines.TReader read FLines write FLines;
    property Theme: TFunctionTheme read FTheme write SetTheme;

    function GetEol: Boolean; override;
    procedure Next; override;
    function GetTokenKind: Integer; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;

  end;

implementation

{ TLine }

procedure TLine.AddRef;
begin
  Inc(FReferences);
end;

procedure TLine.Compile;
begin
  FreeAndNil(FCommand);
  FreeAndNil(FContext);

  with TBrigadierCommandParser.Create(FSettings, Text, True) do
  begin
    if Success then
      FCommand := OwnParseResult;
    FContext := OwnContext;
    Free;
  end;
end;

constructor TLine.Create(ASettings: TBrigadierCommandParser.TSettings; AText: string);
begin
  FSettings := ASettings;
  FText := AText;
  Compile;
end;

destructor TLine.Destroy;
begin
  FCommand.Free;
  FContext.Free;
  inherited;
end;

function TLine.HasReference: Boolean;
begin
  Result := FReferences > 0;
end;

procedure TLine.RemoveRef;
begin
  Dec(FReferences);
end;

procedure TLine.SetText(Value: string);
begin
  Value := Value.TrimRight;
  if Text = Value then
    Exit;
  FText := Value;
  Compile;
end;

{ TFunctionHighlighter }

constructor TFunctionHighlighter.Create(AOwner: TComponent);
begin
  inherited;

  fCaseSensitive := True;

  FDummyAttributes := TSynHighlighterAttributes.Create('Dummy');
  AddAttribute(FDummyAttributes);

  FAttributes := TAttributes.Create;
  FCurrentAttributes := TSynHighlighterAttributes.Create('Current');
end;

destructor TFunctionHighlighter.Destroy;
begin
  FCurrentAttributes.Free;
  FAttributes.Free;
  inherited;
end;

function TFunctionHighlighter.GetAttributes: TSynHighlighterAttributes;
begin
  if not FAttributes.GetActualValue(FCurrentAttributes, Result) then
  begin
    Result := FAttributes.Add(FCurrentAttributes);
    FCurrentAttributes := TSynHighlighterAttributes.Create('');
    FCurrentAttributes.AssignColorAndStyle(Result);
  end;
end;

function TFunctionHighlighter.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  FTheme.DefaultAttributes.Apply(FCurrentAttributes);
  Result := GetAttributes;
end;

function TFunctionHighlighter.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TFunctionHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
var
  Pos: Integer;
  Line: TLine;
  Token: Integer;
  Parser: TParserClass;
  I: Integer;
  TokenData: TFunctionTheme.TParserTokenData;
begin
  if Theme = nil then
    Exit(FDummyAttributes);

  if not FLines.RangeCheck(fLineNumber) then
  begin
    Theme.DefaultAttributes.Apply(FCurrentAttributes);
    Exit(GetAttributes);
  end;

  Line := FLines[fLineNumber];

  if Line = nil then
  begin
    Theme.DefaultAttributes.Apply(FCurrentAttributes);
    Exit(GetAttributes);
  end;

  Pos := fExpandedTokenPos;

  if Pos >= Line.Context.Length then
  begin
    Theme.ErrorAttributes.Apply(FCurrentAttributes);
    Exit(GetAttributes);
  end;

  Theme.DefaultAttributes.Apply(FCurrentAttributes);
  for I := 0 to Line.Context.Parsers[Pos].MaxIndex do
  begin
    Parser := Line.Context.Parsers[Pos][I].ParserClass;
    if FTheme.Parsers.Get(Parser, TokenData) then
      TokenData.Tokens[0].Merge(FCurrentAttributes);
  end;

  Token := Line.Context.Tokens[Pos];
  if Token <> 0 then
  begin
    Parser := Line.Context.Parsers[Pos].Last.ParserClass;
    if FTheme.Parsers.Get(Parser, TokenData) then
      TokenData.Tokens[Token].Merge(FCurrentAttributes);
  end;

  Result := GetAttributes;
end;

function TFunctionHighlighter.GetTokenKind: Integer;
begin
  Result := 0;
end;

procedure TFunctionHighlighter.Next;
begin
  Inc(Run);
  inherited;
end;

procedure TFunctionHighlighter.SetTheme(const Value: TFunctionTheme);
begin
  FTheme := Value;
end;

{ TTheme }

procedure TFunctionTheme.Assign(ATheme: TFunctionTheme);
begin
  raise ENotImplemented.Create('TFunctionTheme.Assign');
  {
    FName := ATheme.Name;
    FFontName := ATheme.FontName;
    Default.Assign(ATheme.Default);
    Comment.Assign(ATheme.Comment);
    Error.Assign(ATheme.Error);
    for Pair in ATheme.FParsers do
    for I := 0 to Pair.Value.MaxIndex do
    FParsers[Pair.Key][I].Assign(Pair.Value[I]);
  }
end;

function TFunctionTheme.Copy: TFunctionTheme;
begin
  Result := TFunctionTheme.Create;
  Result.Assign(Self);
end;

constructor TFunctionTheme.Create;
begin
  FDefault := TTextAttributesBase.Create;
  FError := TTextAttributesBase.Create;
  InitParsers;
end;

class constructor TFunctionTheme.Create;
begin
  FPresets := TPresets.Create;
end;

class destructor TFunctionTheme.Destroy;
begin
  FPresets.Free;
end;

destructor TFunctionTheme.Destroy;
begin
  FParsers.Free;
  FError.Free;
  FDefault.Free;
  inherited;
end;

function TFunctionTheme.GetParsers: TParsers.TReader;
begin
  Result := FParsers.Reader;
end;

function TFunctionTheme.GetText(AParser: TParserClass; AToken: Integer): TTextAttributes;
begin
  Result := FParsers[AParser].Tokens[AToken];
end;

procedure TFunctionTheme.InitParsers;
var
  Category: TCategoryClass;
  I: Integer;
  ParserClass: TParserClass;
begin
  FParsers := TParsers.Create;
  for Category in CategoryClasses do
  begin
    for I := 0 to Category.GetParserCount - 1 do
    begin
      ParserClass := Category.GetParser(I);
      if not FParsers.KeyExists(ParserClass) then
        FParsers[ParserClass] := TParserTokenData.Create(ParserClass);
    end;
  end;
end;

procedure TFunctionTheme.LoadPreset(APresetClass: TPresetClass);
begin
  APresetClass.Apply(Self);
end;

class function TFunctionTheme.Presets: TPresets.TReader;
begin
  Result := FPresets.Reader;
end;

procedure TFunctionTheme.SetDefault(ATextColor, ABackgroundColor: TColor; AStyle: TFontStyles);
begin
  DefaultAttributes.Foreground := ATextColor;
  DefaultAttributes.Background := ABackgroundColor;
  DefaultAttributes.Style := AStyle;
end;

procedure TFunctionTheme.SetError(ATextColor, ABackgroundColor: TColor; AStyle: TFontStyles);
begin
  ErrorAttributes.Foreground := ATextColor;
  ErrorAttributes.Background := ABackgroundColor;
  ErrorAttributes.Style := AStyle;
end;

{ TFunctionTheme.TCategoryNBT }

class function TFunctionTheme.TCategoryNBT.GetName: string;
begin
  Result := 'NBT';
end;

class function TFunctionTheme.TCategoryNBT.GetParser(AIndex: Integer): TParserClass;
begin
  Result := Parsers[AIndex];
end;

class function TFunctionTheme.TCategoryNBT.GetParserCount: Integer;
begin
  Result := Length(Parsers);
end;

{ TFunctionTheme.TCategory }

class function TFunctionTheme.TCategory.GetExample: string;
begin
  Result := '# There is no Example for this Category yet.';
end;

{ TFunctionTheme.TParserTokenData }

constructor TFunctionTheme.TParserTokenData.Create(AParserClass: TParserClass);
var
  I: Integer;
begin
  FParserClass := AParserClass;
  FTokens := TTokens.Create;
  for I := 0 to FParserClass.GetTokenCount do
  begin
    FTokens.Add(TTextAttributes.Create);
    FTokens.Last.SetFg(TColorRGB.HSV(Random * 6, 1 - Random * 0.25, 1 - Random * 0.25).ToWinColor);
  end;
end;

destructor TFunctionTheme.TParserTokenData.Destroy;
begin
  FTokens.Free;
  inherited;
end;

function TFunctionTheme.TParserTokenData.GetTokenName(AIndex: Integer): string;
begin
  Result := ParserClass.GetTokenName(AIndex);
end;

function TFunctionTheme.TParserTokenData.GetTokens: TTokens.TReader;
begin
  Result := FTokens.Reader;
end;

{ TFunctionTheme.TTextAttributesBase }

constructor TFunctionTheme.TTextAttributesBase.Create;
begin
  FBackground := clWhite;
end;

procedure TFunctionTheme.TTextAttributesBase.Apply(AAttributes: TSynHighlighterAttributes);
begin
  AAttributes.SetColors(Foreground, Background);
  AAttributes.Style := Style;
end;

constructor TFunctionTheme.TTextAttributesBase.Create(AForeground, ABackground: TColor; AStyles: TFontStyles);
begin
  FForeground := AForeground;
  FBackground := ABackground;
  FStyle := AStyles;
end;

function TFunctionTheme.TTextAttributesBase.ToSynAttributes: TSynHighlighterAttributes;
begin
  Result := TSynHighlighterAttributes.Create('');
  Apply(Result);
end;

{ TFunctionTheme.TTextAttributes }

procedure TFunctionTheme.TTextAttributes.Merge(AAttributes: TSynHighlighterAttributes);
begin
  if OverrideForeground then
    AAttributes.Foreground := Foreground;

  if OverrideBackground then
    AAttributes.Background := Background;

  AAttributes.Style := Style;
end;

procedure TFunctionTheme.TTextAttributes.SetFg(AForeground: TColor; AStyle: TFontStyles);
begin
  Foreground := AForeground;
  OverrideForeground := True;
  Style := AStyle;
end;

{ TFunctionHighlighter.TAttributeHasher }

class function TFunctionHighlighter.TAttributeHasher.Equal(const A, B: TSynHighlighterAttributes): Boolean;
begin
  Result :=
    (A.Foreground = B.Foreground) and
    (A.Background = B.Background) and
    (A.Style = B.Style);
end;

class function TFunctionHighlighter.TAttributeHasher.GetHash(const AValue: TSynHighlighterAttributes): Cardinal;
var
  Style: TFontStyle;
begin
  Result := HashOf(AValue.Foreground) xor HashOf(AValue.Background);
  for Style in AValue.Style do
    Result := Result xor ($03000000 shl (Ord(Style) * 2));
end;

{ TFunctionTheme.TCategoryBasicTypes }

class function TFunctionTheme.TCategoryBasicTypes.GetName: string;
begin
  Result := 'Basic Types';
end;

class function TFunctionTheme.TCategoryBasicTypes.GetParser(AIndex: Integer): TParserClass;
begin
  Result := Parsers[AIndex];
end;

class function TFunctionTheme.TCategoryBasicTypes.GetParserCount: Integer;
begin
  Result := Length(Parsers);
end;

{ TFunctionTheme.TCategoryPositional }

class function TFunctionTheme.TCategoryPositional.GetName: string;
begin
  Result := 'Positional';
end;

class function TFunctionTheme.TCategoryPositional.GetParser(AIndex: Integer): TParserClass;
begin
  Result := Parsers[AIndex];
end;

class function TFunctionTheme.TCategoryPositional.GetParserCount: Integer;
begin
  Result := Length(Parsers);
end;

{ TFunctionTheme.TCategoryEntityAndPlayer }

class function TFunctionTheme.TCategoryEntityAndPlayer.GetName: string;
begin
  Result := 'Entity and Player';
end;

class function TFunctionTheme.TCategoryEntityAndPlayer.GetParser(AIndex: Integer): TParserClass;
begin
  Result := Parsers[AIndex];
end;

class function TFunctionTheme.TCategoryEntityAndPlayer.GetParserCount: Integer;
begin
  Result := Length(Parsers);
end;

{ TFunctionTheme.TCategoryText }

class function TFunctionTheme.TCategoryText.GetName: string;
begin
  Result := 'Text';
end;

class function TFunctionTheme.TCategoryText.GetParser(AIndex: Integer): TParserClass;
begin
  Result := Parsers[AIndex];
end;

class function TFunctionTheme.TCategoryText.GetParserCount: Integer;
begin
  Result := Length(Parsers);
end;

{ TFunctionTheme.TCategoryScoreboard }

class function TFunctionTheme.TCategoryScoreboard.GetName: string;
begin
  Result := 'Scoreboard';
end;

class function TFunctionTheme.TCategoryScoreboard.GetParser(AIndex: Integer): TParserClass;
begin
  Result := Parsers[AIndex];
end;

class function TFunctionTheme.TCategoryScoreboard.GetParserCount: Integer;
begin
  Result := Length(Parsers);
end;

{ TFunctionTheme.TCategoryBlocksAndItems }

class function TFunctionTheme.TCategoryBlocksAndItems.GetName: string;
begin
  Result := 'Blocks and Items';
end;

class function TFunctionTheme.TCategoryBlocksAndItems.GetParser(AIndex: Integer): TParserClass;
begin
  Result := Parsers[AIndex];
end;

class function TFunctionTheme.TCategoryBlocksAndItems.GetParserCount: Integer;
begin
  Result := Length(Parsers);
end;

{ TFunctionTheme.TCategoryResources }

class function TFunctionTheme.TCategoryResources.GetName: string;
begin
  Result := 'Resources';
end;

class function TFunctionTheme.TCategoryResources.GetParser(AIndex: Integer): TParserClass;
begin
  Result := Parsers[AIndex];
end;

class function TFunctionTheme.TCategoryResources.GetParserCount: Integer;
begin
  Result := Length(Parsers);
end;

{ TFunctionTheme.TCategoryCommand }

class function TFunctionTheme.TCategoryCommand.GetName: string;
begin
  Result := 'Command';
end;

class function TFunctionTheme.TCategoryCommand.GetParser(AIndex: Integer): TParserClass;
begin
  Result := Parsers[AIndex];
end;

class function TFunctionTheme.TCategoryCommand.GetParserCount: Integer;
begin
  Result := Length(Parsers);
end;

{ TFunctionTheme.TCategoryEntitySelector }

class function TFunctionTheme.TCategoryEntitySelector.GetName: string;
begin
  Result := 'Entity-Selector';
end;

class function TFunctionTheme.TCategoryEntitySelector.GetParser(AIndex: Integer): TParserClass;
begin
  Result := Parsers[AIndex];
end;

class function TFunctionTheme.TCategoryEntitySelector.GetParserCount: Integer;
begin
  Result := Length(Parsers);
end;

end.
