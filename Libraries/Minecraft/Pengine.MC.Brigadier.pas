unit Pengine.MC.Brigadier;

interface

uses
  System.SysUtils,
  System.JSON,
  System.Generics.Collections,
  System.RegularExpressions,
  System.Character,

  Pengine.CollectionInterfaces,
  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.Hasher,
  Pengine.Parser;

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

  EBriadierParserNotRegistered = class(Exception)
  public
    constructor Create(AParserString: string);
  end;

  EBrigadierInvalidProperties = class(Exception)
  public
    constructor Create;
  end;

  TBrigadierRoot = class;
  TBrigadierLiteral = class;
  TBrigadierArgument = class;
  TBrigadierArgumentParameter = class;

  TBrigadierParser = class;
  TBrigadierParserClass = class of TBrigadierParser;
  TBrigadierParameterClass = class of TBrigadierArgumentParameter;

  TBrigadierParserPropertiesClass = class of TBrigadierParserProperties;

  TBrigadierParserProperties = class abstract
  public
    constructor Create(AProperties: TJSONObject); virtual;
  end;

  TBrigadierParser = class abstract(TParser<TBrigadierArgumentParameter>)
  public type

    TLookup = TMap<string, TBrigadierParserClass, TStringHasher>;
    
  private
    FProperties: TBrigadierParserProperties;

  class var
    FLookup: TLookup;

  public
    class constructor Create;
    class destructor Destroy;

    class procedure RegisterClass;

    class function GetParserClass(AName: string): TBrigadierParserClass; static;
    class function GetParserString: string; virtual; abstract;
    class function GetParameterClass: TBrigadierParameterClass; virtual; abstract;
    class function GetPropertiesClass: TBrigadierParserPropertiesClass; virtual;

    constructor Create(AInfo: TParseInfo; AArgument: TBrigadierArgument; AProperties: TBrigadierParserProperties); virtual;

    function ToString: string; override;

  end;

  TBrigadierParser<T: TBrigadierArgumentParameter> = class(TBrigadierParser)
  protected
    procedure Cleanup; override;

  public
    constructor Create(AInfo: TParseInfo; AArgument: TBrigadierArgument; AProperties: TBrigadierParserProperties); override;

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

    procedure LoadExecutable(AJSONObject: TJSONObject);
    procedure LoadChildren(AJSONObject: TJSONObject);
    procedure LoadRedirect(AJSONObject: TJSONObject);

    function GetArguments: TArguments.TReader;
    function GetLiterals: TLiterals.TReader;
    function GetChild(AName: string): TBrigadierChild;

    function GetRedirectedChild: TBrigadierChild;
    function GetRedirection: TRedirection.TReader;

  public
    constructor Create(ARoot: TBrigadierRoot; AJSONObject: TJSONObject);
    destructor Destroy; override;

    class function CreateTyped(ARoot: TBrigadierRoot; AJSONPair: TJSONPair): TBrigadierChild;

    class function GetTypeString: string; virtual; abstract;

    property Root: TBrigadierRoot read FRoot;

    property Executable: Boolean read FExecutable;

    property Literals: TLiterals.TReader read GetLiterals;
    property Arguments: TArguments.TReader read GetArguments;
    property Children[AName: string]: TBrigadierChild read GetChild; default;

    property Redirection: TRedirection.TReader read GetRedirection;
    property RedirectedChild: TBrigadierChild read GetRedirectedChild;

  end;

  TBrigadierLiteral = class(TBrigadierChild)
  private
    FLiteral: string;

  public
    constructor Create(ARoot: TBrigadierRoot; AJSONPair: TJSONPair);

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
    constructor Create(ARoot: TBrigadierRoot; AJSONPair: TJSONPair);
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
    function GetExecutable: Boolean; virtual; abstract;

  public
    /// <summary>Converts the parsed content back into a string, which could have been given as parameter.</summary>
    function Format: string; virtual; abstract;

    /// <summary>Propagates from backing literal or argument.</summary>
    property Executable: Boolean read GetExecutable;

  end;

  /// <summary>A simple literal parameter.</summary>
  TBrigadierLiteralParameter = class(TBrigadierParameter)
  private
    FLiteral: TBrigadierLiteral;

  protected
    function GetExecutable: Boolean; override;

  public
    constructor Create(ALiteral: TBrigadierLiteral);

    /// <summary>The literal, which was used for this parameter.</summary>
    property Literal: TBrigadierLiteral read FLiteral;

    function Format: string; override;

  end;

  /// <summary>An abstract base class for arguments using a parser.</summary>
  TBrigadierArgumentParameter = class abstract(TBrigadierParameter)
  private
    FArgument: TBrigadierArgument;
    FSuccess: Boolean;

  protected
    function GetExecutable: Boolean; override;

  public
    constructor Create(AArgument: TBrigadierArgument); virtual;

    /// <returns>The parser class of the current argument.</returns>
    function ParserClass: TBrigadierParserClass;
    /// <summary>The argument, which this parameter was meant for.</summary>
    property Argument: TBrigadierArgument read FArgument;
    /// <summary>Wether the parsing of the parameter was successful.</summary>
    property Success: Boolean read FSuccess;

  end;

  /// <summary>Stores all kinds of information of a parsed command.</summary>
  TBrigadierParseResult = class
  public type

    TParameters = TObjectArray<TBrigadierParameter>;
    TSuggestions = TRefArray<TBrigadierChild>;

  private
    FParameters: TParameters;
    FSuggestions: TSuggestions;
    FSuccess: Boolean;

    function GetExecutable: Boolean;
    function GetParameters: TParameters.TReader;
    function GetSuggestions: TSuggestions.TReader;

  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>True, if there was no error while parsing the command.</summary>
    property Success: Boolean read FSuccess;
    /// <summary>A readonly list of all parsed parameters, used in the command.</summary>
    property Parameters: TParameters.TReader read GetParameters;
    /// <summary>All children, which could continue the current command.</summary>
    property Suggestions: TSuggestions.TReader read GetSuggestions;
    /// <summary>If the command as-is could get executed.</summary>
    property Executable: Boolean read GetExecutable;

    /// <summary>Create a command string, which would parse into the same result.</summary>
    function Format: string;

  end;

  /// <summary>The root of the brigadier command system.</summary>
  TBrigadierRoot = class(TBrigadierChild)
  public
    constructor Create(AJSONObject: TJSONObject);

    class function GetTypeString: string; override;

  end;

  TBrigadierCommandParser = class(TParser<TBrigadierParseResult>)
  private
    FRoot: TBrigadierRoot;

  protected
    function Parse: Boolean; override;

    procedure Cleanup; override;

  public
    constructor Create(ARoot: TBrigadierRoot; AText: string);

  end;

implementation

uses
  Minecraft.BrigadierParser;

{ TBrigadierRoot }

constructor TBrigadierRoot.Create(AJSONObject: TJSONObject);
begin
  inherited Create(Self, AJSONObject);
end;

class function TBrigadierRoot.GetTypeString: string;
begin
  Result := 'root';
end;

{ TBrigadierSystem.TLiteral }

constructor TBrigadierLiteral.Create(ARoot: TBrigadierRoot; AJSONPair: TJSONPair);
begin
  inherited Create(ARoot, AJSONPair.JsonValue as TJSONObject);
  FLiteral := AJSONPair.JsonString.Value;
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

constructor TBrigadierArgument.Create(ARoot: TBrigadierRoot; AJSONPair: TJSONPair);
var
  ParserName: string;
  PropertiesNode: TJSONObject;
begin
  inherited Create(ARoot, AJSONPair.JsonValue as TJSONObject);
  FName := AJSONPair.JsonString.Value;
  ParserName := AJSONPair.JsonValue.GetValue<TJSONString>('parser').Value;
  FParserClass := TBrigadierParser.GetParserClass(ParserName);
  if ParserClass <> nil then
  begin
    if not AJSONPair.JsonValue.TryGetValue<TJSONObject>('properties', PropertiesNode) then
      PropertiesNode := nil;
    if ParserClass.GetPropertiesClass <> nil then
      FParserProperties := ParserClass.GetPropertiesClass.Create(PropertiesNode);
  end
  else
  begin
    raise ENotImplemented.CreateFmt('Unknown Parser: %s', [ParserName]);
  end;
end;

function TBrigadierArgument.CreateParser(AInfo: TParseInfo): TBrigadierParser;
begin
  Result := ParserClass.Create(AInfo, Self, ParserProperties);
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

constructor TBrigadierChild.Create(ARoot: TBrigadierRoot; AJSONObject: TJSONObject);
begin
  FRoot := ARoot;
  LoadExecutable(AJSONObject);
  LoadChildren(AJSONObject);
  LoadRedirect(AJSONObject);
end;

class function TBrigadierChild.CreateTyped(ARoot: TBrigadierRoot; AJSONPair: TJSONPair): TBrigadierChild;
var
  ChildType: string;
begin
  ChildType := AJSONPair.JsonValue.GetValue<TJSONString>('type').Value;
  if ChildType = TBrigadierLiteral.GetTypeString then
    Result := TBrigadierLiteral.Create(ARoot, AJSONPair)
  else if ChildType = TBrigadierArgument.GetTypeString then
    Result := TBrigadierArgument.Create(ARoot, AJSONPair)
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
begin
  Result := FArguments.Reader;
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

  raise EBrigadierChildNotFound.Create;
end;

function TBrigadierChild.GetLiterals: TLiterals.TReader;
begin
  Result := FLiterals.Reader;
end;

function TBrigadierChild.GetRedirectedChild: TBrigadierChild;
var
  Redirect: string;
begin
  if FRedirectedChild <> nil then
    Exit(FRedirectedChild);

  if FRedirection.Empty then
  begin
    if not Executable and Literals.Empty and Arguments.Empty then
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

procedure TBrigadierChild.LoadChildren;
var
  ChildrenNode: TJSONObject;
  ChildNode: TJSONPair;
  Child: TBrigadierChild;
begin
  FLiterals := TLiterals.Create;
  FArguments := TArguments.Create;
  if AJSONObject.TryGetValue<TJSONObject>('children', ChildrenNode) then
  begin
    for ChildNode in ChildrenNode do
    begin
      Child := TBrigadierChild.CreateTyped(Root, ChildNode);
      if Child is TBrigadierLiteral then
        FLiterals.Add(TBrigadierLiteral(Child))
      else if Child is TBrigadierArgument then
        FArguments.Add(TBrigadierArgument(Child))
      else
        raise EBrigadierUnknownChildType.Create;
    end;
  end;
end;

procedure TBrigadierChild.LoadExecutable(AJSONObject: TJSONObject);
var
  ExecutableNode: TJSONBool;
begin
  if AJSONObject.TryGetValue<TJSONBool>('executable', ExecutableNode) then
    FExecutable := ExecutableNode.AsBoolean;
end;

procedure TBrigadierChild.LoadRedirect(AJSONObject: TJSONObject);
var
  RedirectNode: TJSONArray;
  Redirection: TJSONValue;
begin
  FRedirection := TRedirection.Create;
  if AJSONObject.TryGetValue<TJSONArray>('redirect', RedirectNode) then
    for Redirection in RedirectNode do
      FRedirection.Add(Redirection.GetValue<TJSONString>.Value);
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

{ TBrigadierParseResult }

constructor TBrigadierParseResult.Create;
begin
  FParameters := TParameters.Create;
  FSuggestions := TSuggestions.Create;
end;

destructor TBrigadierParseResult.Destroy;
begin
  FParameters.Free;
  FSuggestions.Free;
  inherited;
end;

function TBrigadierParseResult.Format: string;
var
  I: Integer;
begin
  for I := 0 to Parameters.MaxIndex do
  begin
    Result := Result + Parameters[I].Format;
    if I <> Parameters.MaxIndex then
      Result := Result + ' ';
  end;
end;

function TBrigadierParseResult.GetExecutable: Boolean;
begin
  Result := Success and not Parameters.Empty and Parameters.Last.Executable;
end;

function TBrigadierParseResult.GetParameters: TParameters.TReader;
begin
  Result := FParameters.Reader;
end;

function TBrigadierParseResult.GetSuggestions: TSuggestions.TReader;
begin
  Result := FSuggestions.Reader;
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
  if (GetPropertiesClass <> nil) and not (AProperties is GetPropertiesClass) then
    raise EBrigadierInvalidProperties.Create;
  FProperties := AProperties;
  inherited Create(AInfo);
end;
{
class function TBrigadierParser.CreateTyped(AName: string; AProperties: TJSONObject): TBrigadierParser;
var
  ParserClass: TBrigadierParserClass;
  Properties: TSet<string, TStringHasher>;
  Prop: TJSONPair;
begin
  if not FParserProperties.Get(AName, Properties) then
  begin
    Properties := TSet<string, TStringHasher>.Create;
    FParserProperties[AName] := Properties;
  end;

  if AProperties <> nil then
    for Prop in AProperties do
      Properties.TryAdd(Prop.JsonString.Value);

  if FLookup.Get(AName, ParserClass) then
    Exit(ParserClass.Create(AProperties));

  raise EBriadierParserNotRegistered.Create(AName);
end;
}
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

{ TBrigadierLiteralParameter }

constructor TBrigadierLiteralParameter.Create(ALiteral: TBrigadierLiteral);
begin
  FLiteral := ALiteral;
end;

function TBrigadierLiteralParameter.Format: string;
begin
  Result := Literal.Literal;
end;

function TBrigadierLiteralParameter.GetExecutable: Boolean;
begin
  Result := Literal.Executable;
end;

{ TBrigadierArgumentParameter }

constructor TBrigadierArgumentParameter.Create(AArgument: TBrigadierArgument);
begin
  FArgument := AArgument;
end;

function TBrigadierArgumentParameter.GetExecutable: Boolean;
begin
  Result := Argument.Executable;
end;

function TBrigadierArgumentParameter.ParserClass: TBrigadierParserClass;
begin
  Result := Argument.ParserClass;
end;

{ EBgriadierParserNotRegistered }

constructor EBriadierParserNotRegistered.Create(AParserString: string);
begin
  inherited CreateFmt('The parser "%s" was not registered.', [AParserString]);
end;

{ TBrigadierCommandParser }

procedure TBrigadierCommandParser.Cleanup;
begin
  ParseResult.Free;
end;

constructor TBrigadierCommandParser.Create(ARoot: TBrigadierRoot; AText: string);
begin
  FRoot := ARoot;
  inherited Create(AText);
end;

function TBrigadierCommandParser.Parse: Boolean;
var
  CurrentChild: TBrigadierChild;
  Literal: TBrigadierLiteral;
  Argument: TBrigadierArgument;
  Found: Boolean;
  OldPos: Integer;
  ArgumentParser: TBrigadierParser;
  ScannedLiteral: string;
begin
  Result := True;

  Info.SkipWhitespace;
  Info.StartsWith('/');

  SetParseResult(TBrigadierParseResult.Create);

  // start the parsing
  CurrentChild := FRoot;

  // after a part got parsed it gets removed, therefore it should be empty at the end
  while not Info.ReachedEnd do
  begin
    OldPos := Info.Pos;
    Found := False;

    if not CurrentChild.Literals.Empty then
    begin
      ScannedLiteral := Info.ReadWhile(
        function(C: Char): Boolean
        begin
          Result := C <> ' ';
        end);

      // first scan through the simple literals
      for Literal in CurrentChild.Literals do
      begin
        if ScannedLiteral <> Literal.Literal then
          Continue;
        ParseResult.FParameters.Add(TBrigadierLiteralParameter.Create(Literal));
        Found := True;
        Info.SkipWhitespace;
        CurrentChild := Literal;
        Break;
      end;
    end;

    if not Found then
    begin
      Info.Pos := OldPos;
      // no literal found, try to parse each argument
      for Argument in CurrentChild.Arguments do
      begin
        ArgumentParser := Argument.CreateParser(Info);
        try
          if ArgumentParser.Success then
          begin
            ParseResult.FParameters.Add(ArgumentParser.ParseResult);
            Found := True;
            Info.SkipWhitespace;
            CurrentChild := Argument;
            Break;
          end;
        finally
          ArgumentParser.Free;
        end;
      end;
    end;

    if CurrentChild.RedirectedChild <> nil then
      CurrentChild := CurrentChild.RedirectedChild;

    if not Found then
      Exit(True);
  end;

  ParseResult.FSuggestions.Add<TBrigadierLiteral>(CurrentChild.Literals);
  ParseResult.FSuggestions.Add<TBrigadierArgument>(CurrentChild.Arguments);

  ParseResult.FSuccess := True;
end;

{ TBrigadierParserSettings }

constructor TBrigadierParserProperties.Create(AProperties: TJSONObject);
begin
  // nothing by default
end;

{ EBrigadierInvalidProperties }

constructor EBrigadierInvalidProperties.Create;
begin
  inherited Create('Invalid brigadier argument property class.');
end;

{ TBrigadierParser<T> }

procedure TBrigadierParser<T>.Cleanup;
begin
  ParseResult.Free;
end;

constructor TBrigadierParser<T>.Create(AInfo: TParseInfo; AArgument: TBrigadierArgument; AProperties: TBrigadierParserProperties);
begin
  SetParseResult(T.Create(AArgument));
  inherited;
end;

function TBrigadierParser<T>.ParseResult: T;
begin
  Result := T(inherited ParseResult);
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

end.
