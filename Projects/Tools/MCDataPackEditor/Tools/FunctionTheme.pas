unit FunctionTheme;

interface

uses
  System.Classes,
  System.SysUtils,

  Vcl.Graphics,

  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.Parser,
  Pengine.IntMaths,
  Pengine.Hasher,

  Pengine.MC.Brigadier,
  Pengine.MC.NBT,
  Pengine.MC.EntitySelector,
  Pengine.MC.General,

  SynEditHighlighter;

type

  TFunctionTheme = class
  public type

    TCategory = class
    public
      class function GetName: string; virtual; abstract;
      class function GetDisplayName: string; virtual; abstract;

    end;

    TCategoryGeneral = class(TCategory)
    public

    end;

    TCategoryNBT = class(TCategory)
    public

    end;

    TCategoryEntitySelector = class(TCategory)
    public

    end;

    TParserTokens = TObjectArray<TSynHighlighterAttributes>;

    TParsers = TClassObjectMap<TParserTokens>;

    TParsersReader = TClassObjectMap<TParserTokens.TReader>.TReader;

    TPreset = class
    public
      class procedure Register;

      class function GetName: string; virtual; abstract;
      class procedure Apply(ATheme: TFunctionTheme); virtual; abstract;

    end;

    TPresetClass = class of TPreset;

    TPresets = TArray<TPresetClass>;

  public const

    ParserClasses: array [0 .. 17] of TParserClass = (
      TBrigadierCommandParser,

      TStringParser,
      TStringOrIdentParser,

      TEntitySelector.TParser,
      TEntitySelector.TIntRangeParser,
      TEntitySelector.TRangeParser,
      TEntitySelector.TOption.TParser,
      TEntitySelector.TOptionInteger.TParser,
      TEntitySelector.TOptionFloat.TParser,
      TEntitySelector.TOptionIdentifier.TParser,
      TEntitySelector.TOptionSort.TParser,
      TEntitySelector.TOptionScores.TParser,
      TEntitySelector.TOptionType.TParser,
      TEntitySelector.TOptionAdvancements.TParser,
      TEntitySelector.TOptionGamemode.TParser,

      TNBTParserCompound,
      TNBTParserListOrArray,
      TNBTParserNumber
      );

  private
    class var
      FPresets: TPresets;

  private
    FName: string;
    FFontName: TFontName;
    FDefault: TSynHighlighterAttributes;
    FComment: TSynHighlighterAttributes;
    FError: TSynHighlighterAttributes;
    FParsers: TParsers;

    function GetParsers: TParsersReader;

    procedure InitParsers;

  public
    class constructor Create;
    class destructor Destroy;

    constructor Create;
    destructor Destroy; override;

    procedure LoadPreset(APresetClass: TPresetClass);

    property Name: string read FName write FName;

    property FontName: TFontName read FFontName write FFontName;
    property Default: TSynHighlighterAttributes read FDefault;
    property Comment: TSynHighlighterAttributes read FComment;
    property Error: TSynHighlighterAttributes read FError;
    property Parsers: TParsersReader read GetParsers;

    procedure SetDefault(ATextColor, ABackgroundColor: TColor; AStyle: TFontStyles = []);
    procedure SetComment(AColor: TColor; AStyle: TFontStyles = []);
    procedure SetError(AColor: TColor; AStyle: TFontStyles = []);
    procedure SetParser(AParserClass: TParserClass; AToken: Integer; AColor: TColor; AStyle: TFontStyles = []);

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
  private
    FLines: TLines.TReader;
    FTheme: TFunctionTheme;
    FDummyAttributes: TSynHighlighterAttributes;

  protected
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;

  public
    constructor Create(AOwner: TComponent); override;

    property Lines: TLines.TReader read FLines write FLines;
    property Theme: TFunctionTheme read FTheme write FTheme;

    function GetEol: Boolean; override;
    procedure Next; override;
    function GetTokenKind: Integer; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;

  end;

implementation

uses
  Main;

{ TLine }

procedure TLine.AddRef;
begin
  Inc(FReferences);
end;

procedure TLine.Compile;
begin
  FreeAndNil(FCommand);
  FreeAndNil(FContext);

  with TBrigadierCommandParser.Create(FSettings, Text) do
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

  FDummyAttributes := TSynHighlighterAttributes.Create('', '');
  AddAttribute(FDummyAttributes);

end;

function TFunctionHighlighter.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  Result := FDummyAttributes;
end;

function TFunctionHighlighter.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TFunctionHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
var
  Pos: Integer;
  Line: TLine;
  Parser: TParserClass;
  Token: Integer;
  ParserTokens: TFunctionTheme.TParserTokens.TReader;
begin
  if Theme = nil then
    Exit(FDummyAttributes);

  if not FLines.RangeCheck(fLineNumber) then
    Exit(Theme.Default);

  Line := FLines[fLineNumber];

  if Line = nil then
    Exit(Theme.Default);

  Pos := fExpandedTokenPos;

  if Pos >= Line.Context.Length then
    Exit(Theme.Error);

  Token := Line.Context.Tokens[Pos];
  Parser := Line.Context.Parsers[Pos].Last;

  if not Theme.Parsers.Get(Parser, ParserTokens) then
    Exit(Theme.Default);

  if not ParserTokens.RangeCheck(Token) then
    Exit(Theme.Default);

  Result := ParserTokens[Token];

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

{ TTheme }

procedure TFunctionTheme.Assign(ATheme: TFunctionTheme);
var
  Pair: TParsers.TPair;
  I: Integer;
begin
  FName := ATheme.Name;
  FFontName := ATheme.FontName;
  Default.Assign(ATheme.Default);
  Comment.Assign(ATheme.Comment);
  Error.Assign(ATheme.Error);
  for Pair in ATheme.FParsers do
    for I := 0 to Pair.Value.MaxIndex do
      FParsers[Pair.Key][I].Assign(Pair.Value[I]);
end;

function TFunctionTheme.Copy: TFunctionTheme;
begin
  Result := TFunctionTheme.Create;
  Result.Assign(Self);
end;

constructor TFunctionTheme.Create;
begin
  FDefault := TSynHighlighterAttributes.Create('', '');
  FComment := TSynHighlighterAttributes.Create('', '');
  FError := TSynHighlighterAttributes.Create('', '');
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
  FComment.Free;
  FDefault.Free;
  inherited;
end;

function TFunctionTheme.GetParsers: TParsersReader;
begin
  Result := TParsersReader(FParsers);
end;

procedure TFunctionTheme.InitParsers;
var
  C: TClass;
  ParserClass: TParserClass;
  ParserTokens: TParserTokens;
  I: Integer;
begin
  FParsers := TParsers.Create;
  for C in ParserClasses do
  begin
    ParserClass := TParserClass(C);
    ParserTokens := TParserTokens.Create;
    for I := 0 to ParserClass.GetTokenCount do
      ParserTokens.Add(TSynHighlighterAttributes.Create('', ''));
    FParsers[ParserClass] := ParserTokens;
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

procedure TFunctionTheme.SetComment(AColor: TColor; AStyle: TFontStyles);
begin
  Comment.Foreground := AColor;
  Comment.Style := AStyle;
end;

procedure TFunctionTheme.SetDefault(ATextColor, ABackgroundColor: TColor; AStyle: TFontStyles);
begin
  Default.Foreground := ATextColor;
  Default.Background := ABackgroundColor;
  Default.Style := AStyle;
end;

procedure TFunctionTheme.SetError(AColor: TColor; AStyle: TFontStyles);
begin
  Error.Foreground := AColor;
  Error.Style := AStyle;
end;

procedure TFunctionTheme.SetParser(AParserClass: TParserClass; AToken: Integer; AColor: TColor; AStyle: TFontStyles);
var
  Attributes: TSynHighlighterAttributes;
begin
  Attributes := FParsers[AParserClass][AToken];
  Attributes.Foreground := AColor;
  Attributes.Style := AStyle;
end;

{ TTheme.TPreset }

class procedure TFunctionTheme.TPreset.Register;
begin
  TFunctionTheme.FPresets.Add(Self);
end;

end.
