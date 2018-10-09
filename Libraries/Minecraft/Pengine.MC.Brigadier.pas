unit Pengine.MC.Brigadier;

interface

uses
  System.SysUtils,
  System.Math,
  System.Generics.Collections,
  System.Character,
  System.IOUtils,

  Pengine.CollectionInterfaces,
  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.Hasher,
  Pengine.Parser,
  Pengine.Settings,
  Pengine.JSON,
  
  Pengine.MC.General,
  Pengine.MC.Namespace;

type

  EBrigadierUnknownChildType = class(Exception)
  public
    constructor Create;
  end;

  EBrigadierMultipleRoots = class(Exception)
  public
    constructor Create;
  end;

  EBrigadierChildNotFound = class(Exception)
  public
    constructor Create;
  end;

  EBrigadierProperties = class(Exception);

  TBrigadierRoot = class;
  TBrigadierLiteral = class;
  TBrigadierArgument = class;
  TBrigadierArgumentParameter = class;

  TBrigadierSettings = class(TSettings)
  public const

    DefaultPath = 'Data\reports\commands.json';

  private
    FPath: string;
    FBrigadier: TBrigadierRoot;

    procedure SetPath(const Value: string);

  public
    destructor Destroy; override;

    class function GetTitle: string; override;
    class function GetDescription: string; override;

    procedure SetDefaults; override;

    property Path: string read FPath write SetPath;
    property Brigadier: TBrigadierRoot read FBrigadier;

    procedure Reload;

  end;

  TBrigadierParser = class;
  TBrigadierParserClass = class of TBrigadierParser;
  TBrigadierParameterClass = class of TBrigadierArgumentParameter;

  TBrigadierParserPropertiesClass = class of TBrigadierParserProperties;

  TBrigadierParserProperties = class abstract
  public
    constructor Create(AProperties: TJObject); virtual;
  end;

  TBrigadierParser = class abstract(TObjectParser<TBrigadierArgumentParameter>)
  public type

    TLookup = TMap<string, TBrigadierParserClass, TStringHasher>;

  private
    FProperties: TBrigadierParserProperties;
    FArgument: TBrigadierArgument;

    class var
      FLookup: TLookup;

  protected
    property Argument: TBrigadierArgument read FArgument;

  public
    class constructor Create;
    class destructor Destroy;

    class procedure RegisterClass;

    class function GetParserClass(AName: string): TBrigadierParserClass; static;
    class function GetParserString: TNSPath; virtual; abstract;
    class function GetParameterClass: TBrigadierParameterClass; virtual; abstract;
    class function GetPropertiesClass: TBrigadierParserPropertiesClass; virtual;

    constructor Create(AInfo: TParseInfo; AArgument: TBrigadierArgument; AProperties: TBrigadierParserProperties); virtual;

    function ToString: string; override;

  end;

  TBrigadierParser<T: TBrigadierArgumentParameter> = class(TBrigadierParser)
  protected
    procedure SetParseResult(AResult: T);

  public
    function ParseResult: T;

  end;

  TBrigadierParser<T: TBrigadierArgumentParameter; P: TBrigadierParserProperties> = class(TBrigadierParser<T>)
  protected
    function Properties: P;

  public
    class function GetPropertiesClass: TBrigadierParserPropertiesClass; override;

  end;

  TBrigadierChild = class
  public type

    TLiterals = TObjectArray<TBrigadierLiteral>;
    TArguments = TObjectArray<TBrigadierArgument>;
    TRedirection = TArray<string>;

  private
    FRoot: TBrigadierRoot;

    FExecutable: Boolean;

    FLiterals: TLiterals;
    FArguments: TArguments;

    FRedirection: TRedirection;
    FRedirectedChild: TBrigadierChild;

    procedure LoadExecutable(AJObject: TJObject);
    procedure LoadChildren(AJObject: TJObject);
    procedure LoadRedirect(AJObject: TJObject);

    function GetArguments: TArguments.TReader;
    function GetLiterals: TLiterals.TReader;
    function GetChild(AName: string): TBrigadierChild;

    function GetRedirectedChild: TBrigadierChild;
    function GetRedirection: TRedirection.TReader;
    function GetSettings: TRootSettings;

  public
    constructor Create(ARoot: TBrigadierRoot; AJObject: TJObject);
    destructor Destroy; override;

    class function CreateTyped(ARoot: TBrigadierRoot; AJPair: TJPair): TBrigadierChild;

    class function GetTypeString: string; virtual; abstract;

    property Root: TBrigadierRoot read FRoot;
    property Settings: TRootSettings read GetSettings;

    property Executable: Boolean read FExecutable;

    property Literals: TLiterals.TReader read GetLiterals;
    property Arguments: TArguments.TReader read GetArguments;
    property Children[AName: string]: TBrigadierChild read GetChild; default;

    property Redirection: TRedirection.TReader read GetRedirection;
    property RedirectedChild: TBrigadierChild read GetRedirectedChild;

    function IsOrRedirects(AChild: TBrigadierChild): Boolean;

  end;

  TBrigadierLiteral = class(TBrigadierChild)
  private
    FLiteral: string;

  public
    constructor Create(ARoot: TBrigadierRoot; AJPair: TJPair);

    class function GetTypeString: string; override;

    property Literal: string read FLiteral;

    function ToString: string; override;

  end;

  TBrigadierArgument = class(TBrigadierChild)
  private
    FName: string;
    FParserClass: TBrigadierParserClass;
    FParserProperties: TBrigadierParserProperties;

  public
    constructor Create(ARoot: TBrigadierRoot; AJPair: TJPair);
    destructor Destroy; override;

    class function GetTypeString: string; override;

    property Name: string read FName;
    property ParserClass: TBrigadierParserClass read FParserClass;
    property ParserProperties: TBrigadierParserProperties read FParserProperties;

    function CreateParser(AInfo: TParseInfo): TBrigadierParser;

    function ToString: string; override;

  end;

  /// <summary>An abstract base class for a single parsed parameter of a brigadier command.</summary>
  TBrigadierParameter = class abstract
  protected
    function GetChild: TBrigadierChild; virtual; abstract;
    function GetExecutable: Boolean;

  public
    /// <summary>Converts the parsed content back into a string, which could have been given as parameter.</summary>
    function Format: string; virtual; abstract;

    /// <summary>Propagates from backing literal or argument.</summary>
    property Executable: Boolean read GetExecutable;

    property Child: TBrigadierChild read GetChild;

  end;

  TBrigadierParameter<T: TBrigadierChild> = class abstract(TBrigadierParameter)
  private
    FChild: T;

  protected
    function GetChild: TBrigadierChild; override;

  public
    constructor Create(AChild: T); virtual;

    property Child: T read FChild;

  end;

  /// <summary>A simple literal parameter.</summary>
  TBrigadierLiteralParameter = class(TBrigadierParameter<TBrigadierLiteral>)
  public
    function Format: string; override;

  end;

  /// <summary>An abstract base class for arguments using a parser.</summary>
  TBrigadierArgumentParameter = class(TBrigadierParameter<TBrigadierArgument>);

  /// <summary>Stores all kinds of information of a parsed command.</summary>
  TBrigadierCommand = class
  public type

    TParameters = TObjectArray<TBrigadierParameter>;

  private
    FParameters: TParameters;
    FComment: string;

    function GetExecutable: Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>A list of all parsed parameters, used in the command.</summary>
    property Parameters: TParameters read FParameters;
    /// <returns>Wether the command is just a comment.</returns>
    function IsComment: Boolean;
    /// <summary>The comment text.</summary>
    property Comment: string read FComment;
    /// <summary>If the command as-is could get executed.</summary>
    property Executable: Boolean read GetExecutable;

    /// <summary>Create a command string, which would parse into the same result.</summary>
    function Format: string;

  end;

  /// <summary>The root of the brigadier command system.</summary>
  TBrigadierRoot = class(TBrigadierChild)
  private
    FTeleport: TBrigadierChild;

    procedure TeleportFix;

  public
    constructor Create(APath: string);

    class function GetTypeString: string; override;

    property Teleport: TBrigadierChild read FTeleport;

  end;

  TBrigadierCommandParser = class(TObjectParser<TBrigadierCommand>)
  public type

    TSettings = class
    public type

      TSlashMode = (
        smNone,
        smOptional,
        smRequired
        );

    private
      FSlashMode: TSlashMode;
      FAllowComment: Boolean;
      FAllowPreceedingSpace: Boolean;
      FAllowEmpty: Boolean;

    public
      property SlashMode: TSlashMode read FSlashMode write FSlashMode;
      property AllowComment: Boolean read FAllowComment write FAllowComment;
      property AllowPreceedingSpace: Boolean read FAllowPreceedingSpace write FAllowPreceedingSpace;
      property AllowEmpty: Boolean read FAllowEmpty write FAllowEmpty;

    end;

    TSuggestions = class(TParseSuggestionsGenerated)
    private
      FChild: TBrigadierChild;

    protected
      procedure Generate; override;

    public
      constructor Create(AChild: TBrigadierChild);

      function GetTitle: string; override;

    end;

  public const

    TokenMainCommand = 1;
    TokenSubCommand = 2;
    TokenInvalidLiteral = 3;
    TokenComment = 4;
    TokenSlash = 5;

    TokenNames: array [TokenMainCommand.. TokenSlash] of string = (
      'Main-Command',
      'Sub-Command',
      'Invalid Literal',
      'Comment',
      'Slash'
      );

  private
    FSettings: TBrigadierCommandParser.TSettings;

    procedure RaiseExpectedError(AChild: TBrigadierChild);

  protected
    function Parse: Boolean; override;

    property Settings: TBrigadierCommandParser.TSettings read FSettings;

  public
    constructor Create(ASettings: TBrigadierCommandParser.TSettings; AText: string; AGenerateEditInfo: Boolean);

    class function GetTokenCount: Integer; override;
    class function GetTokenName(AIndex: Integer): string; override;
    class function GetResultName: string; override;

  end;

implementation

uses
  Pengine.MC.BrigadierParser;

{ TBrigadierRoot }

constructor TBrigadierRoot.Create(APath: string);
var
  BrigadierText: string;
  BrigadierJ: TJObject;
begin
  if TFile.Exists(APath) then
  begin
    BrigadierText := TFile.ReadAllText(APath);
    BrigadierJ := TJObject.Parse(BrigadierText);
  end
  else
  begin
    BrigadierJ := TJObject.Create;
    BrigadierJ.Add('type', 'root');
    BrigadierJ.AddObject('children');
  end;

  try
    inherited Create(Self, BrigadierJ);
  finally
    BrigadierJ.Free;
  end;

  TeleportFix;
end;

class function TBrigadierRoot.GetTypeString: string;
begin
  Result := 'root';
end;

procedure TBrigadierRoot.TeleportFix;
begin
  FTeleport := Self['teleport'];
  if FTeleport = nil then
    Exit;
  Teleport.FArguments.Remove(Teleport['destination'] as TBrigadierArgument);
  with Teleport['targets'] as TBrigadierArgument do
  begin
    // Swap entity and vec3, to check vec3 first, as entity matches any vec3 and vec3 is never tried to parse
    FArguments.Swap(0, 1);
    FExecutable := True;
  end;
end;

{ TBrigadierSystem.TLiteral }

constructor TBrigadierLiteral.Create(ARoot: TBrigadierRoot; AJPair: TJPair);
begin
  inherited Create(ARoot, AJPair.AsObject);
  FLiteral := AJPair.Key;
end;

class function TBrigadierLiteral.GetTypeString: string;
begin
  Result := 'literal';
end;

function TBrigadierLiteral.ToString: string;
begin
  Result := Literal;
end;

{ TBrigadierArgument }

constructor TBrigadierArgument.Create(ARoot: TBrigadierRoot; AJPair: TJPair);
var
  ParserName: string;
begin
  inherited Create(ARoot, AJPair.AsObject);
  FName := AJPair.Key;
  ParserName := AJPair.AsObject['parser'].AsString;
  FParserClass := TBrigadierParser.GetParserClass(ParserName);
  if ParserClass <> nil then
  begin
    if ParserClass.GetPropertiesClass <> nil then
      FParserProperties := ParserClass.GetPropertiesClass.Create(AJPair.AsObject['properties'].ObjectOrNil);
  end
  else
  begin
    // TFile.AppendAllText('output.txt', ParserName + sLineBreak);
    // raise ENotImplemented.CreateFmt('Unknown Parser: %s', [ParserName]);
  end;
end;

function TBrigadierArgument.CreateParser(AInfo: TParseInfo): TBrigadierParser;
begin
  if ParserClass <> nil then
    Exit(ParserClass.Create(AInfo, Self, ParserProperties));
  Result := nil;
end;

destructor TBrigadierArgument.Destroy;
begin
  FParserProperties.Free;
  inherited;
end;

class function TBrigadierArgument.GetTypeString: string;
begin
  Result := 'argument';
end;

function TBrigadierArgument.ToString: string;
begin
  if ParserClass = nil then
    Result := '<' + Name + '|parser_missing>'
  else
    Result := '<' + Name + '|' + ParserClass.GetParserString + '>';
end;

{ TBrigadierChild }

constructor TBrigadierChild.Create(ARoot: TBrigadierRoot; AJObject: TJObject);
begin
  FRoot := ARoot;
  LoadExecutable(AJObject);
  LoadChildren(AJObject);
  LoadRedirect(AJObject);
end;

class function TBrigadierChild.CreateTyped(ARoot: TBrigadierRoot; AJPair: TJPair): TBrigadierChild;
var
  ChildType: string;
begin
  ChildType := AJPair.AsObject['type'].AsString;
  if ChildType = TBrigadierLiteral.GetTypeString then
    Result := TBrigadierLiteral.Create(ARoot, AJPair)
  else if ChildType = TBrigadierArgument.GetTypeString then
    Result := TBrigadierArgument.Create(ARoot, AJPair)
  else if ChildType = TBrigadierRoot.GetTypeString then
    raise EBrigadierMultipleRoots.Create
  else
    raise EBrigadierUnknownChildType.Create;
end;

destructor TBrigadierChild.Destroy;
begin
  FLiterals.Free;
  FArguments.Free;
  FRedirection.Free;
  inherited;
end;

function TBrigadierChild.GetArguments: TArguments.TReader;
var
  Re: TBrigadierChild;
begin
  Re := RedirectedChild;
  if Re = nil then
    Result := FArguments.Reader
  else
    Result := Re.FArguments.Reader;
end;

function TBrigadierChild.GetChild(AName: string): TBrigadierChild;
var
  Literal: TBrigadierLiteral;
  Argument: TBrigadierArgument;
begin
  // search through literals
  for Literal in Literals do
    if Literal.Literal = AName then
      Exit(Literal);

  // search through arguments
  for Argument in Arguments do
    if Argument.Name = AName then
      Exit(Argument);

  Result := nil;
end;

function TBrigadierChild.GetLiterals: TLiterals.TReader;
var
  Re: TBrigadierChild;
begin
  Re := RedirectedChild;
  if Re = nil then
    Result := FLiterals.Reader
  else
    Result := Re.FLiterals.Reader;
end;

function TBrigadierChild.GetRedirectedChild: TBrigadierChild;
var
  Redirect: string;
begin
  if FRedirectedChild <> nil then
    Exit(FRedirectedChild);

  if FRedirection.Empty then
  begin
    if not Executable and FLiterals.Empty and FArguments.Empty then
      Exit(FRoot) // workaround for missing "execute run ..." redirection
    else
      Exit(nil);
  end;

  // get the redirected child and save it for further access
  FRedirectedChild := FRoot;
  for Redirect in Redirection do
    FRedirectedChild := FRedirectedChild[Redirect];

  Result := FRedirectedChild;
end;

function TBrigadierChild.GetRedirection: TRedirection.TReader;
begin
  Result := FRedirection.Reader;
end;

function TBrigadierChild.GetSettings: TRootSettings;
begin
  Result := Root.Settings;
end;

function TBrigadierChild.IsOrRedirects(AChild: TBrigadierChild): Boolean;
begin
  Result := (AChild = Self) or (AChild = RedirectedChild);
end;

procedure TBrigadierChild.LoadChildren(AJObject: TJObject);
var
  JChildren: TJObject;
  JChild: TJPair;
  Child: TBrigadierChild;
begin
  FLiterals := TLiterals.Create;
  FArguments := TArguments.Create;
  JChildren := AJObject['children'].ObjectOrNil;
  if JChildren.Exists then
  begin
    for JChild in JChildren do
    begin
      Child := TBrigadierChild.CreateTyped(Root, JChild);
      if Child is TBrigadierLiteral then
        FLiterals.Add(TBrigadierLiteral(Child))
      else if Child is TBrigadierArgument then
        FArguments.Add(TBrigadierArgument(Child))
      else
        raise EBrigadierUnknownChildType.Create;
    end;
  end;
end;

procedure TBrigadierChild.LoadExecutable(AJObject: TJObject);
begin
  FExecutable := AJObject['executable'].BoolOrDefault;
end;

procedure TBrigadierChild.LoadRedirect(AJObject: TJObject);
var
  JRedirects: TJArray;
  JRedirect: TJValue;
begin
  FRedirection := TRedirection.Create;
  JRedirects := AJObject['redirect'].ArrayOrNil;
  for JRedirect in JRedirects do
    FRedirection.Add(JRedirect.AsString);
end;

{ EBrigadierUnknownChildType }

constructor EBrigadierUnknownChildType.Create;
begin
  inherited Create('Unknown brigadier child type.');
end;

{ EBrigadierMultipleRoots }

constructor EBrigadierMultipleRoots.Create;
begin
  inherited Create('Only one child of type root can exist in the brigadier command tree.');
end;

{ TBrigadierCommand }

constructor TBrigadierCommand.Create;
begin
  FParameters := TParameters.Create;
end;

destructor TBrigadierCommand.Destroy;
begin
  FParameters.Free;
  inherited;
end;

function TBrigadierCommand.Format: string;
var
  I: Integer;
begin
  if IsComment then
    Exit(Comment);

  Result := '';
  for I := 0 to Parameters.MaxIndex do
  begin
    Result := Result + Parameters[I].Format;
    if I <> Parameters.MaxIndex then
      Result := Result + ' ';
  end;
end;

function TBrigadierCommand.GetExecutable: Boolean;
begin
  Result := not Parameters.Empty and Parameters.Last.Executable;
end;

function TBrigadierCommand.IsComment: Boolean;
begin
  Result := not FComment.IsEmpty;
end;

{ EBrigadierChildNotFound }

constructor EBrigadierChildNotFound.Create;
begin
  inherited Create('The brigadier child node could not be found.');
end;

{ TBrigadierParser }

class constructor TBrigadierParser.Create;
begin
  FLookup := TLookup.Create;
end;

constructor TBrigadierParser.Create(AInfo: TParseInfo; AArgument: TBrigadierArgument; AProperties: TBrigadierParserProperties);
begin
  if (GetPropertiesClass <> nil) and not(AProperties is GetPropertiesClass) then
    raise EBrigadierProperties.Create('Invalid brigadier property class.');
  FProperties := AProperties;
  FArgument := AArgument;
  inherited Create(AInfo, False);
end;

class destructor TBrigadierParser.Destroy;
begin
  FLookup.Free;
end;

class function TBrigadierParser.GetParserClass(AName: string): TBrigadierParserClass;
var
  ParserClass: TBrigadierParserClass;
begin
  if FLookup.Get(AName, ParserClass) then
    Exit(ParserClass);
  Result := nil;
  // raise EBriadierParserNotRegistered.Create(AName);
end;

class function TBrigadierParser.GetPropertiesClass: TBrigadierParserPropertiesClass;
begin
  Result := nil;
end;

class procedure TBrigadierParser.RegisterClass;
begin
  FLookup[GetParserString] := Self;
end;

function TBrigadierParser.ToString: string;
begin
  Result := GetParserString;
end;

{ TBrigadierCommandParser }

constructor TBrigadierCommandParser.Create(ASettings: TSettings; AText: string; AGenerateEditInfo: Boolean);
begin
  FSettings := ASettings;
  inherited Create(AText, AGenerateEditInfo);
end;

procedure TBrigadierCommandParser.RaiseExpectedError(AChild: TBrigadierChild);
var
  Msg: string;
  EntryCount, I: Integer;
  Entries: array of string;
begin
  SkipWhitespace;

  EntryCount := AChild.Literals.Count + AChild.Arguments.Count;
  if EntryCount = 0 then
    raise EParseError.Create('Expected end of command.', -AllText.Length);

  SetLength(Entries, EntryCount);
  for I := 0 to AChild.Literals.MaxIndex do
    Entries[I] := '"' + AChild.Literals[I].Literal + '"';
  for I := 0 to AChild.Arguments.MaxIndex do
    Entries[I + AChild.Literals.Count] := AChild.Arguments[I].Name;

  Msg := 'Expected ';
  for I := 0 to EntryCount - 1 do
  begin
    Msg := Msg + Entries[I];
    if I = EntryCount - 2 then
      Msg := Msg + ' or '
    else if I <> EntryCount - 1 then
      Msg := Msg + ', ';
  end;
  Msg := Msg + '.';
  if ReachedEnd then
    Log(1, Msg, elError)
  else
    Log(1, Msg, elFatal);
end;

class function TBrigadierCommandParser.GetResultName: string;
begin
  Result := 'Minecraft Command';
end;

class function TBrigadierCommandParser.GetTokenCount: Integer;
begin
  Result := High(TokenNames);
end;

class function TBrigadierCommandParser.GetTokenName(AIndex: Integer): string;
begin
  Result := TokenNames[AIndex];
end;

function TBrigadierCommandParser.Parse: Boolean;
var
  CurrentChild: TBrigadierChild;
  Literal: TBrigadierLiteral;
  Argument: TBrigadierArgument;
  Found: Boolean;
  ArgumentParser: TBrigadierParser;
  ScannedLiteral: string;
  LastIteration: Boolean;
  PreceedingWhitespace: Boolean;
  StartMarker: TLogMarker;
  Brigadier: TBrigadierRoot;
begin
  Result := True;
  SetParseResult(TBrigadierCommand.Create);
  Brigadier := RootSettings.Get<TBrigadierSettings>.Brigadier;
  CurrentChild := Brigadier;

  StartMarker := GetMarker;

  // Preceeding Whitespace
  PreceedingWhitespace := First.IsWhiteSpace;
  if PreceedingWhitespace then
    SkipWhitespace;

  // Empty/Whitespace strings
  if ReachedEnd then
  begin
    if not Settings.AllowEmpty then
      Exit(False);
    BeginSuggestions(TSuggestions.Create(CurrentChild));
    Exit;
  end
  else if PreceedingWhitespace and not Settings.AllowPreceedingSpace then
    Log(StartMarker, 'Preceeding whitespaces are not allowed.');

  // Comments
  if StartsWith('#', False) then
  begin
    Token := TokenComment;
    ParseResult.FComment := AllText;
    if not Settings.AllowComment then
      Log(ParseResult.FComment.Length, 'Comments are not allowed.');
    AdvanceToEnd;
    Exit;
  end;

  Token := TokenSlash;
  if StartsWith('/') and (Settings.SlashMode = smNone) then
    Log(-1, 'A preceeding slash is not allowed.');

  // Command
  LastIteration := False;
  while True do
  begin
    Found := False;

    // Check for Literals
    if not CurrentChild.Literals.Empty then
    begin
      BeginSuggestions(TSuggestions.Create(CurrentChild));
      ScannedLiteral := ReadWhile(
        function(C: Char): Boolean
        begin
          Result := not C.IsWhiteSpace
        end, False);
      for Literal in CurrentChild.Literals do
      begin
        if ScannedLiteral.TrimLeft <> Literal.Literal then
          Continue;
        ParseResult.FParameters.Add(TBrigadierLiteralParameter.Create(Literal));
        Found := True;
        Token := IfThen(CurrentChild.IsOrRedirects(Brigadier), TokenMainCommand, TokenSubCommand);
        Advance(ScannedLiteral.Length);
        ResetToken;
        CurrentChild := Literal;
        Break;
      end;
      EndSuggestions;
    end;

    // Check for Arguments, if no Literal was found
    if not Found then
    begin
      for Argument in CurrentChild.Arguments do
      begin
        ArgumentParser := Argument.CreateParser(Info);
        if ArgumentParser <> nil then
        begin
          try
            if ArgumentParser.Success then
            begin
              ParseResult.FParameters.Add(ArgumentParser.OwnParseResult);
              Found := True;
              CurrentChild := Argument;
              Break;
            end;
          finally
            ArgumentParser.Free;
          end;
        end
        else
          Log(1, 'The parser for %s is not yet implemented.', [Argument.Name], elHint);
      end;
    end;

    if LastIteration then
      Break;

    if not Found then
    begin
      RaiseExpectedError(CurrentChild);
      Exit(True);
    end;

    if ReachedEnd then
    begin
      LastIteration := True;

      // Teleport Fix
      if (ParseResult.Parameters.Count = 2) and
        ParseResult.Parameters[0].Child.IsOrRedirects(Brigadier.Teleport) and
        (ParseResult.Parameters[1] is TBrigadierEntitySelector) and
        TBrigadierEntitySelector(ParseResult.Parameters[1]).Selector.AllowsMultiple then
      begin
        Log(StartMarker, 'If the entity is used as destination, the selector must not be able to match multiple entities.');
      end;

    end
    else if CurrentChild.Literals.Count + CurrentChild.Arguments.Count = 0 then
      RaiseExpectedError(CurrentChild)
    else if not First.IsWhiteSpace then
      raise EParseError.Create('Expected whitespace.');

    Advance;

    IncrementParserIndex;

    SkipWhitespace;

  end;

  if not ParseResult.Executable then
    RaiseExpectedError(CurrentChild);
end;

{ TBrigadierParserSettings }

constructor TBrigadierParserProperties.Create(AProperties: TJObject);
begin
  // nothing by default
end;

{ TBrigadierParser<T> }

function TBrigadierParser<T>.ParseResult: T;
begin
  Result := T(inherited ParseResult);
end;

procedure TBrigadierParser<T>.SetParseResult(AResult: T);
begin
  inherited SetParseResult(AResult);
end;

{ TBrigadierParser<T, P> }

class function TBrigadierParser<T, P>.GetPropertiesClass: TBrigadierParserPropertiesClass;
begin
  Result := P;
end;

function TBrigadierParser<T, P>.Properties: P;
begin
  Result := P(FProperties);
end;

{ TBrigadierCommandParser.TSuggestions }

constructor TBrigadierCommandParser.TSuggestions.Create(AChild: TBrigadierChild);
begin
  FChild := AChild;
end;

procedure TBrigadierCommandParser.TSuggestions.Generate;
var
  Literal: TBrigadierLiteral;
begin
  for Literal in FChild.Literals do
    AddSuggestion(Literal.Literal);
end;

function TBrigadierCommandParser.TSuggestions.GetTitle: string;
begin
  if FChild is TBrigadierRoot then
    Result := TokenNames[TokenMainCommand]
  else
    Result := TokenNames[TokenSubCommand];
end;

{ TBrigadierParameter }

function TBrigadierParameter.GetExecutable: Boolean;
begin
  Result := Child.Executable;
end;

{ TBrigadierParameter<T> }

constructor TBrigadierParameter<T>.Create(AChild: T);
begin
  FChild := AChild;
end;

function TBrigadierParameter<T>.GetChild: TBrigadierChild;
begin
  Result := FChild;
end;

{ TBrigadierLiteralParameter }

function TBrigadierLiteralParameter.Format: string;
begin
  Result := Child.Literal;
end;

{ TBrigadierSettings }

destructor TBrigadierSettings.Destroy;
begin
  FBrigadier.Free;
  inherited;
end;

class function TBrigadierSettings.GetDescription: string;
begin
  Result := 'Path configuration for commands, items and blocks json files.';
end;

class function TBrigadierSettings.GetTitle: string;
begin
  Result := 'Brigadier';
end;

procedure TBrigadierSettings.Reload;
begin
  FBrigadier.Free;
  FBrigadier := TBrigadierRoot.Create(Path);
end;

procedure TBrigadierSettings.SetDefaults;
begin
  Path := DefaultPath;
end;

procedure TBrigadierSettings.SetPath(const Value: string);
begin
  FPath := Value;
  Reload;
end;

end.
