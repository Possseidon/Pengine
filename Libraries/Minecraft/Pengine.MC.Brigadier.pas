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
  Pengine.Parsing,
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

  protected
    class function GetNameForVersion(AVersion: Integer): string; override;

    procedure DoReload; override;

  public
    destructor Destroy; override;

    class function GetTitle: string; override;
    class function GetDescription: string; override;

    procedure SetDefaults; override;

    property Path: string read FPath write SetPath;
    property Brigadier: TBrigadierRoot read FBrigadier;

    procedure DefineJStorage(ASerializer: TJSerializer); override;

  end;

  TBrigadierParser = class;
  TBrigadierParserClass = class of TBrigadierParser;
  TBrigadierParameterClass = class of TBrigadierArgumentParameter;

  TBrigadierParserPropertiesClass = class of TBrigadierParserProperties;

  TBrigadierParserProperties = class abstract
  public
    constructor Create(AProperties: TJObject); virtual;

  end;

  IBrigadierParser = interface(IObjectParser<TBrigadierArgumentParameter>)
    function GetArgument: TBrigadierArgument;
    function GetProperties: TBrigadierParserProperties;
    procedure SetArgument(const Value: TBrigadierArgument);
    procedure SetProperties(const Value: TBrigadierParserProperties);

    property Argument: TBrigadierArgument read GetArgument write SetArgument;
    property Properties: TBrigadierParserProperties read GetProperties write SetProperties;

  end;

  TBrigadierParser = class abstract(TObjectParser<TBrigadierArgumentParameter>, IBrigadierParser)
  public type

    TLookup = TMap<string, TBrigadierParserClass, TStringHasher>;

  private
    FProperties: TBrigadierParserProperties;
    FArgument: TBrigadierArgument;

    class var
      FLookup: TLookup;

    function GetArgument: TBrigadierArgument;
    function GetProperties: TBrigadierParserProperties;
    procedure SetArgument(const Value: TBrigadierArgument);
    procedure SetProperties(const Value: TBrigadierParserProperties);

  public
    class constructor Create;
    class destructor Destroy;

    class procedure RegisterClass;

    class function GetParserClass(AName: string): TBrigadierParserClass; static;
    class function GetParserString: TNSPath; virtual; abstract;
    class function GetParameterClass: TBrigadierParameterClass; virtual; abstract;
    class function GetPropertiesClass: TBrigadierParserPropertiesClass; virtual;

    property Argument: TBrigadierArgument read GetArgument write SetArgument;
    property Properties: TBrigadierParserProperties read GetProperties write SetProperties;

    function ToString: string; override;

  end;

  TBrigadierParser<T: TBrigadierArgumentParameter> = class(TBrigadierParser)
  private
    function GetParseResult: T;
    procedure SetParseResult(AResult: T);

  public
    property ParseResult: T read GetParseResult write SetParseResult;

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

    function Parser: IBrigadierParser;

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

    TSlashMode = (
      smNone,
      smOptional,
      smRequired
      );

    IParser = interface(IObjectParser<TBrigadierCommand>)
      function GetSlashMode: TSlashMode;
      procedure SetSlashMode(const Value: TSlashMode);
      function GetAllowComment: Boolean;
      procedure SetAllowComment(const Value: Boolean);
      function GetAllowPreceedingSpace: Boolean;
      procedure SetAllowPreceedingSpace(const Value: Boolean);
      function GetAllowEmpty: Boolean;
      procedure SetAllowEmpty(const Value: Boolean);

      property SlashMode: TSlashMode read GetSlashMode write SetSlashMode;
      property AllowComment: Boolean read GetAllowComment write SetAllowComment;
      property AllowPreceedingSpace: Boolean read GetAllowPreceedingSpace write SetAllowPreceedingSpace;
      property AllowEmpty: Boolean read GetAllowEmpty write SetAllowEmpty;

    end;

    TParser = class(TObjectParser<TBrigadierCommand>, IParser)
    public type

      TSuggestions = class(TParseSuggestionsGenerated)
      private
        FChild: TBrigadierChild;

      protected
        procedure Generate; override;

      public
        constructor Create(AChild: TBrigadierChild);

        function GetTitle: string; override;
        function GetBreakChars: TSysCharSet; override;

      end;

    public const

      TokenMainCommand = 1;
      TokenSubCommand = 2;
      TokenComment = 3;
      TokenSlash = 4;

      TokenNames: array [TokenMainCommand .. TokenSlash] of string = (
        'Main-Command',
        'Sub-Command',
        'Comment',
        'Slash'
        );

    private
      FSlashMode: TSlashMode;
      FAllowComment: Boolean;
      FAllowPreceedingSpace: Boolean;
      FAllowEmpty: Boolean;

      procedure RaiseExpectedError(AChild: TBrigadierChild);

      function GetSlashMode: TSlashMode;
      procedure SetSlashMode(const Value: TSlashMode);
      function GetAllowComment: Boolean;
      procedure SetAllowComment(const Value: Boolean);
      function GetAllowPreceedingSpace: Boolean;
      procedure SetAllowPreceedingSpace(const Value: Boolean);
      function GetAllowEmpty: Boolean;
      procedure SetAllowEmpty(const Value: Boolean);

    protected
      function Parse: Boolean; override;

    public
      class function GetTokenCount: Integer; override;
      class function GetTokenName(AIndex: Integer): string; override;
      class function GetResultName: string; override;

      property SlashMode: TSlashMode read GetSlashMode write SetSlashMode;
      property AllowComment: Boolean read GetAllowComment write SetAllowComment;
      property AllowPreceedingSpace: Boolean read GetAllowPreceedingSpace write SetAllowPreceedingSpace;
      property AllowEmpty: Boolean read GetAllowEmpty write SetAllowEmpty;

    end;

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
    BrigadierJ['type'] := 'root';
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
  inherited Create(ARoot, AJPair.Value);
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
  inherited Create(ARoot, AJPair.Value);
  FName := AJPair.Key;
  ParserName := AJPair.Value['parser'].AsString;
  FParserClass := TBrigadierParser.GetParserClass(ParserName);
  if ParserClass <> nil then
  begin
    if ParserClass.GetPropertiesClass <> nil then
      FParserProperties := ParserClass.GetPropertiesClass.Create(AJPair.Value['properties']);
  end
  else
  begin
    // TFile.AppendAllText('output.txt', ParserName + sLineBreak);
    // raise ENotImplemented.CreateFmt('Unknown Parser: %s', [ParserName]);
  end;
end;

function TBrigadierArgument.Parser: IBrigadierParser;
begin
  if ParserClass = nil then
    Exit(nil);
  Result := ParserClass.Create;
  Result.Argument := Self;
  Result.Properties := ParserProperties;
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
  ChildType := AJPair.Value['type'];
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
  JChild: TJPair;
  Child: TBrigadierChild;
begin
  FLiterals := TLiterals.Create;
  FArguments := TArguments.Create;
  for JChild in AJObject['children'].AsObject do
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

procedure TBrigadierChild.LoadExecutable(AJObject: TJObject);
begin
  FExecutable := AJObject['executable'] or False;
end;

procedure TBrigadierChild.LoadRedirect(AJObject: TJObject);
var
  JRedirects: TJArray;
  JRedirect: TJValue;
begin
  FRedirection := TRedirection.Create;
  JRedirects := AJObject['redirect'].AsArray;
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

class destructor TBrigadierParser.Destroy;
begin
  FLookup.Free;
end;

function TBrigadierParser.GetArgument: TBrigadierArgument;
begin
  Result := FArgument;
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

function TBrigadierParser.GetProperties: TBrigadierParserProperties;
begin
  Result := FProperties;
end;

class function TBrigadierParser.GetPropertiesClass: TBrigadierParserPropertiesClass;
begin
  Result := nil;
end;

class procedure TBrigadierParser.RegisterClass;
begin
  FLookup[GetParserString] := Self;
end;

procedure TBrigadierParser.SetArgument(const Value: TBrigadierArgument);
begin
  FArgument := Value;
end;

procedure TBrigadierParser.SetProperties(const Value: TBrigadierParserProperties);
begin
  if (GetPropertiesClass <> nil) and not(Value is GetPropertiesClass) then
    raise EBrigadierProperties.Create('Invalid brigadier property class.');
  FProperties := Value;
end;

function TBrigadierParser.ToString: string;
begin
  Result := GetParserString;
end;

{ TBrigadierCommand.TParser }

procedure TBrigadierCommand.TParser.RaiseExpectedError(AChild: TBrigadierChild);
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

function TBrigadierCommand.TParser.GetAllowComment: Boolean;
begin
  Result := FAllowComment;
end;

function TBrigadierCommand.TParser.GetAllowEmpty: Boolean;
begin
  Result := FAllowEmpty;
end;

function TBrigadierCommand.TParser.GetAllowPreceedingSpace: Boolean;
begin
  Result := FAllowPreceedingSpace;
end;

procedure TBrigadierCommand.TParser.SetAllowComment(const Value: Boolean);
begin
  FAllowComment := Value;
end;

procedure TBrigadierCommand.TParser.SetAllowEmpty(const Value: Boolean);
begin
  FAllowEmpty := Value;
end;

procedure TBrigadierCommand.TParser.SetAllowPreceedingSpace(const Value: Boolean);
begin
  FAllowPreceedingSpace := Value;
end;

procedure TBrigadierCommand.TParser.SetSlashMode(const Value: TSlashMode);
begin
  FSlashMode := Value;
end;

class function TBrigadierCommand.TParser.GetResultName: string;
begin
  Result := 'Minecraft Command';
end;

function TBrigadierCommand.TParser.GetSlashMode: TSlashMode;
begin
  Result := FSlashMode;
end;

class function TBrigadierCommand.TParser.GetTokenCount: Integer;
begin
  Result := High(TokenNames);
end;

class function TBrigadierCommand.TParser.GetTokenName(AIndex: Integer): string;
begin
  Result := TokenNames[AIndex];
end;

function TBrigadierCommand.TParser.Parse: Boolean;
var
  CurrentChild: TBrigadierChild;
  Literal: TBrigadierLiteral;
  Argument: TBrigadierArgument;
  Found: Boolean;
  ArgumentParser: IBrigadierParser;
  ScannedLiteral: string;
  LastIteration: Boolean;
  PreceedingWhitespace: Boolean;
  StartMarker: TLogMarker;
  Brigadier: TBrigadierRoot;
begin
  Result := True;
  ParseResult := TBrigadierCommand.Create;
  Brigadier := RootSettingsG.Get<TBrigadierSettings>.Brigadier;
  CurrentChild := Brigadier;

  StartMarker := GetMarker;

  // Preceeding Whitespace
  PreceedingWhitespace := First.IsWhiteSpace;
  if PreceedingWhitespace then
    SkipWhitespace;

  // Empty/Whitespace strings
  if ReachedEnd then
  begin
    if not AllowEmpty then
      Exit(False);
    BeginSuggestions(TSuggestions.Create(CurrentChild));
    Exit;
  end
  else if PreceedingWhitespace and not AllowPreceedingSpace then
    Log(StartMarker, 'Preceeding whitespaces are not allowed.');

  // Comments
  if StartsWith('#', False) then
  begin
    Token := TokenComment;
    ParseResult.FComment := AllText;
    if not AllowComment then
      Log(ParseResult.FComment.Length, 'Comments are not allowed.');
    AdvanceToEnd;
    Exit;
  end;

  Token := TokenSlash;
  if StartsWith('/') and (SlashMode = smNone) then
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
        ArgumentParser := Argument.Parser;
        if ArgumentParser <> nil then
        begin
          ArgumentParser.Parse(Info, False);
          if ArgumentParser.Success then
          begin
            ParseResult.FParameters.Add(ArgumentParser.OwnParseResult);
            Found := True;
            CurrentChild := Argument;
            Break;
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
        Log(StartMarker,
          'If the entity is used as destination, the selector must not be able to match multiple entities.');
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

function TBrigadierParser<T>.GetParseResult: T;
begin
  Result := T(inherited ParseResult);
end;

procedure TBrigadierParser<T>.SetParseResult(AResult: T);
begin
  inherited ParseResult := AResult;
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

{ TBrigadierCommand.TParser.TSuggestions }

constructor TBrigadierCommand.TParser.TSuggestions.Create(AChild: TBrigadierChild);
begin
  FChild := AChild;
end;

procedure TBrigadierCommand.TParser.TSuggestions.Generate;
var
  Literal: TBrigadierLiteral;
begin
  for Literal in FChild.Literals do
    AddSuggestion(Literal.Literal);
end;

function TBrigadierCommand.TParser.TSuggestions.GetBreakChars: TSysCharSet;
begin
  // Those exist in the execute command...
  Result := inherited - ['=', '<', '>'];
end;

function TBrigadierCommand.TParser.TSuggestions.GetTitle: string;
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

procedure TBrigadierSettings.DefineJStorage(ASerializer: TJSerializer);
begin
  inherited;
  with ASerializer do
  begin
    Define('path', FPath);
  end;
end;

destructor TBrigadierSettings.Destroy;
begin
  FBrigadier.Free;
  inherited;
end;

class function TBrigadierSettings.GetDescription: string;
begin
  Result := 'Path configuration for commands, items and blocks json files.';
end;

class function TBrigadierSettings.GetNameForVersion(AVersion: Integer): string;
begin
  Result := 'mc_brigadier';
end;

class function TBrigadierSettings.GetTitle: string;
begin
  Result := 'Brigadier';
end;

procedure TBrigadierSettings.DoReload;
begin
  FBrigadier.Free;
  FBrigadier := TBrigadierRoot.Create(Path);
end;

procedure TBrigadierSettings.SetDefaults;
begin
  FPath := DefaultPath;
end;

procedure TBrigadierSettings.SetPath(const Value: string);
begin
  FPath := Value;
  Reload;
end;

end.
