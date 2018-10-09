unit Pengine.Parser;

interface

uses
  System.SysUtils,
  System.Character,
  System.Math,

  Pengine.IntMaths,
  Pengine.Collections,
  Pengine.CollectionInterfaces,
  Pengine.Settings;

type

  TLogMarker = record
  private
    Pos: Integer;

  public
    class operator Implicit(APos: Integer): TLogMarker;
    class operator Implicit(AMarker: TLogMarker): Integer;

  end;

  /// <summary>Raised while parsing to cancel.</summary>
  EParseError = class(Exception)
  private
    FLength: Integer;

  public
    constructor Create(AMessage: string; ALength: Integer = -1);
    constructor CreateFmt(AMessage: string; const AFmt: array of const; ALength: Integer = -1);

    property Length: Integer read FLength;

  end;

  EParseSuggestionException = class(Exception)
  public
    constructor Create;
  end;

  TParser = class;
  TParserClass = class of TParser;

  /// <summary>A record for one possible suggestion with displayed text and actually inserted text.</summary>
  /// <remarks>Implicit casting from string will set display and insert at once.</remarks>
  TParseSuggestion = record
  public
    Display: string;
    Insert: string;

    constructor Create(ADisplay, AInsert: string);

    class operator Implicit(AText: string): TParseSuggestion;

  end;

  /// <summary>Combines position in the string, current line and position in the current line.</summary>
  /// <remarks>All are one-based.</remarks>
  TParsePosition = record
  public
    Pos: Integer;
    Line: Integer;
    LinePos: Integer;

    function FormatLinePos: string;

  end;

  /// <summary>A class representing a single log-message while parsing.</summary>
  TParseError = class
  public type

    TLevel = (
      elNone,
      elHint,
      elWarning, // probably wrong
      elError, // can't execute
      elFatal // can't ececute and can't format (like exception)
      );

    TLevels = set of TLevel;

  public const

    LevelNames: array [TLevel] of string = (
      'None',
      'Hint',
      'Warning',
      'Error',
      'Fatal'
      );

  private
    FPosition: TParsePosition;
    FLength: Integer;
    FLevel: TLevel;
    FMessage: string;

    function GetLevelName: string;

  public
    constructor Create(APosition: TParsePosition; ALength: Integer; AMessage: string; ALevel: TLevel = elError);

    property Position: TParsePosition read FPosition;
    property Length: Integer read FLength;
    property Level: TLevel read FLevel;
    property LevelName: string read GetLevelName;
    property Message: string read FMessage;

  end;

  /// <summary>A simple version for a baseclass allowing you to store suggestion info in the parser context.</summary>
  /// <remarks>No instance is created for this, as it uses class methods instead.</remarks>
  TParseSuggestionsSimple = class
  public
    class function GetTitle: string; virtual; abstract;
    class function GetCount: Integer; virtual; abstract;
    class function GetSuggestion(AIndex: Integer): TParseSuggestion; virtual; abstract;
    class function GetBreakChars: TSysCharSet; virtual;

  end;

  /// <summary>This class overrides the GetTitle method with the name of the given parsers result name.</summary>
  TParseSuggestionsSimple<T: TParser> = class(TParseSuggestionsSimple)
  public
    class function GetTitle: string; override;

  end;

  /// <summary>A baseclass allowing you to store suggestion info in the parser context.</summary>
  TParseSuggestions = class
  public const

    DefaultBreakChars = [' ', '(', ')', '[', ']', '=', ',', '{', '}', '!', '|', '#', '.'];

  public
    function GetTitle: string; virtual; abstract;
    function GetCount: Integer; virtual; abstract;
    function GetSuggestion(AIndex: Integer): TParseSuggestion; virtual; abstract;
    function GetBreakChars: TSysCharSet; virtual;

  end;

  /// <summary>This class overrides the GetTitle method with the name of the given parsers result name.</summary>
  TParseSuggestions<T: TParser> = class(TParseSuggestions)
  public
    function GetTitle: string; override;

  end;

  /// <summary>A baseclass allowing you to store suggestion info in the parser context.</summary>
  TParseSuggestionsGenerated = class(TParseSuggestions)
  public type

    TSuggestions = TArray<TParseSuggestion>;

  private
    FSuggestions: TSuggestions;

    function GetSuggestions: TSuggestions;

    property Suggestions: TSuggestions read GetSuggestions;

  protected
    procedure Generate; virtual; abstract;
    /// <summary>Usually called in the <c>Generate</c> procedure to add suggestions.</summary>
    /// <remarks>Use <c>AddUniqueSuggestion</c> to automatically prevent duplicates.</remarks>
    procedure AddSuggestion(ASuggestion: TParseSuggestion);
    /// <summary>Same as <c>AddSuggestion</c>, but prevents duplicate display strings.</summary>
    procedure AddUniqueSuggestion(ASuggestion: TParseSuggestion);

  public
    destructor Destroy; override;

    function GetCount: Integer; override;
    function GetSuggestion(AIndex: Integer): TParseSuggestion; override;

  end;

  /// <summary>This class overrides the GetTitle method with the name of the given parsers result name.</summary>
  TParseSuggestionsGenerated<T: TParser> = class(TParseSuggestionsGenerated)
  public
    function GetTitle: string; override;

  end;

  TParseSuggestionsClass = class of TParseSuggestionsSimple;

  /// <summary>A class used only while parsing and freed afterwards.</summary>
  TParseInfo = class
  public type

    TParserInfo = record
      ParserClass: TParserClass;
      Index: Integer;

      constructor Create(AParserClass: TParserClass; AIndex: Integer);
    end;

    TParserStack = TArray<TParserInfo>;

    TParserIndices = TArray<Integer>;

    TTokenStack = TStack<Integer>;

    /// <summary>The context for the parsed text, which gets owned by the parser after the parsing.</summary>
    TContext = class
    public type

      /// <summary>Parse info for a single char.</summary>
      TCharInfoRec = record
        ParserStack: TParserStack;
        Token: Integer;
        case UseSuggestionsClass: Boolean of
          False:
            (Suggestions: TParseSuggestions);
          True:
            (SuggestionsClass: TParseSuggestionsClass);
      end;

      /// <summary>An array, that holds charinfo for multiple chars and automatically frees its content.</summary>
      TCharInfo = class(TArray<TCharInfoRec>)
      private
        FSkipFree: Boolean;

      protected
        function ShouldFreeItems: Boolean; override;
        procedure ItemRemoved(AIndex: Integer); override;

      end;

      TSuggestions = TObjectArray<TParseSuggestions>;

      TLog = TObjectArray<TParseError>;

    private type

      TSuggestionPos = (
        spGiven,
        spEnd,
        spNone
        );

    private
      FSuggestions: TSuggestions;
      FCurrentSuggestionsSimple: TParseSuggestionsClass;
      FCurrentSuggestions: TParseSuggestions;
      FCharInfo: TCharInfo;
      FMaxParserDepth: Integer;
      FLog: TLog;
      FErrorLevels: TParseError.TLevels;

      function GetLength: Integer;

      function GetParsers(APos: Integer): TParserStack.TReader;
      function GetToken(APos: Integer): Integer;

      function GetSuggestionTitle(APos: Integer): string;
      function GetSuggestionCount(APos: Integer): Integer;
      function GetSuggestion(APos, AIndex: Integer): TParseSuggestion;

      function GetLog: TLog.TReader;
      function GetHighestErrorLevel: TParseError.TLevel;
      function GetSuccess: Boolean;

      /// <returns>Wether to use the pos, current suggestion or nothing.</returns>
      function GetSuggestionPos(APos: Integer): TSuggestionPos;
      function GetSuggestionBreakChars(APos: Integer): TSysCharSet;
      function GetSuggestionBreakCharString(APos: Integer): string;

    public
      constructor Create;
      destructor Destroy; override;

      /// <summary>The length of the actual parsed string.</summary>
      /// <remarks>Can be shorter, that the string, initially supplied to the parser.</remarks>
      property Length: Integer read GetLength;

      /// <summary>The maximum count of parsers at a single char position.</summary>
      property MaxParserDepth: Integer read FMaxParserDepth;
      /// <summary>A read-only stack of all parsers for a single char.</summary>
      property Parsers[APos: Integer]: TParserStack.TReader read GetParsers;
      /// <summary>The parser token of the top-level parser of the char at the given position.</summary>
      property Tokens[APos: Integer]: Integer read GetToken;

      /// <summary>A list of errors, that did not have to cancel the parsing process via raise.</summary>
      property Log: TLog.TReader read GetLog;
      property ErrorLevels: TParseError.TLevels read FErrorLevels;
      property HighestErrorLevel: TParseError.TLevel read GetHighestErrorLevel;
      property Success: Boolean read GetSuccess;

      /// <returns>Wether the given position has any suggestions.</returns>
      function HasSuggestions(APos: Integer): Boolean;
      /// <returns>A set of all chars, that could break up a token of a suggestion.</returns>
      property SuggestionBreakChars[APos: Integer]: TSysCharSet read GetSuggestionBreakChars;
      property SuggestionBreakCharString[APos: Integer]: string read GetSuggestionBreakCharString;
      /// <summary>The title of the suggestions-box at a given position.</summary>
      property SuggestionTitle[APos: Integer]: string read GetSuggestionTitle;
      /// <summary>The count of suggestions at a given position.</summary>
      property SuggestionCount[APos: Integer]: Integer read GetSuggestionCount;
      /// <summary>Query a single suggestion for an index lower than SuggestionCount at that position.</summary>
      property Suggestion[APos, AIndex: Integer]: TParseSuggestion read GetSuggestion;

    end;

    TIterator = class(TIterator<Char>)
    private
      FInfo: TParseInfo;
      FFirst: Boolean;

    public
      constructor Create(AInfo: TParseInfo);

      function MoveNext: Boolean; override;
      function GetCurrent: Char; override;

    end;

  private
    FText: string;
    FPosition: TParsePosition;
    FParserStack: TParserStack;
    FParserIndices: TParserIndices;
    FTokenStack: TTokenStack;
    FToken: Integer;
    FSuggestionsEnd: Boolean;
    FContext: TContext;

    function GetFirst: Char;

    procedure UpdateSuggestionsEnd;
    procedure AddSuggestionToWhitespace;
    procedure AddSuggestionsToInfo(var AInfo: TContext.TCharInfoRec);

    function EditInfoEnabled: Boolean; inline;

  public
    constructor Create(AText: string; AGenerateEditInfo: Boolean);
    destructor Destroy; override;

    property Position: TParsePosition read FPosition;
    property Pos: Integer read FPosition.Pos;
    property Line: Integer read FPosition.Line;
    property LinePos: Integer read FPosition.LinePos;

    /// <summary>The char at the current position or #0 if the end is reached.</summary>
    property First: Char read GetFirst;

    /// <summary>Advances the position and stores information for all advanced chars.</summary>
    procedure Advance(AAmount: Integer = 1);
    /// <summary>Advances the position to the end of the string.</summary>
    procedure AdvanceToEnd; inline;
    /// <returns>Wether the current position starts with the given char.</returns>
    function StartsWith(AChar: Char; AAdvanceOnMatch: Boolean = True): Boolean; overload;
    /// <returns>Wether the current position starts with the given text.</returns>
    function StartsWith(AText: string; AAdvanceOnMatch: Boolean = True): Boolean; overload;
    /// <returns>A string, which gets read from the current position, as long as the condition is met.</returns>
    function ReadWhile(APredicate: TPredicate<Char>; AAdvance: Boolean = True): string; overload;
    /// <summary>A string, which gets read from the current position, as long as the char is in the given set.</summary>
    function ReadWhile(AChars: TSysCharSet; AAdvance: Boolean = True): string; overload;
    function ReadWhile(AChars: TSysCharSet; AMax: Integer; AAdvance: Boolean = True): string; overload;
    function ReadUntil(AChars: TSysCharSet; AAdvance: Boolean = True): string; overload;
    /// <summary>Advances over any whitespace and raises EParseError if spaces are required.</summary>
    procedure SkipWhitespace;

    /// <remarks>Wether the end of the given string is reached.</remarks>
    function ReachedEnd: Boolean;

    /// <summary>All text from the current position.</summary>
    /// <remarks>Only use this if absolutly necessary, as it has to create a new possibly very big string.</remarks>
    function AllText: string;

    /// <summary>Used internally to add a new parser class onto the stack.</summary>
    procedure PushParser(AParserClass: TParserClass);
    /// <summary>Used internally to remove the topmost parser calss from the stack.</summary>
    procedure PopParser;

    /// <summary>Allows manual increase of the current parser index.</summary>
    procedure IncrementParserIndex;

    /// <summary>Starts a new suggestion section with the given simple suggestion class.</summary>
    procedure BeginSuggestions(ASuggestions: TParseSuggestionsClass); overload;
    /// <summary>Starts a new suggestion section with the given suggestion object.</summary>
    procedure BeginSuggestions(ASuggestions: TParseSuggestions); overload;
    /// <summary>Ends a suggestion section.</summary>
    procedure EndSuggestions;

    procedure Log(ALength: Integer; AMessage: string; ALevel: TParseError.TLevel = elError); overload;
    procedure Log(AMarker: TLogMarker; AMessage: string; ALevel: TParseError.TLevel = elError); overload; inline;
    procedure Log(ALength: Integer; AMessage: string; const AFmt: array of const;
      ALevel: TParseError.TLevel = elError); overload;
    procedure Log(AMarker: TLogMarker; AMessage: string; const AFmt: array of const;
      ALevel: TParseError.TLevel = elError); overload;

    function GetMarker: TLogMarker; inline;

    /// <summary>Used to own the context after parsing.</summary>
    function OwnContext: TContext;

    function GetEnumerator: TIterator;

  end;

  /// <summary>A baseclass, which parses text using the protected parse function in its constructor.</summary>
  TParser = class
  public const

    TokenNone = 0;

  private
    FInfo: TParseInfo;
    FSuccess: Boolean;
    FContext: TParseInfo.TContext;

    function GetContext: TParseInfo.TContext; inline;
    function GetToken: Integer; inline;
    procedure SetToken(AIndex: Integer); inline;
    function GetFirst: Char; inline;

  protected
    function Parse: Boolean; virtual; abstract;

    property Info: TParseInfo read FInfo;
    property Token: Integer read GetToken write SetToken;
    procedure ResetToken;

    property First: Char read GetFirst;

    procedure Advance(AAmount: Integer = 1); inline;
    procedure AdvanceToEnd; inline;
    function StartsWith(AChar: Char; AAdvanceOnMatch: Boolean = True): Boolean; overload; inline;
    function StartsWith(AText: string; AAdvanceOnMatch: Boolean = True): Boolean; overload; inline;
    function ReadWhile(APredicate: TPredicate<Char>; AAdvance: Boolean = True): string; overload; inline;
    function ReadWhile(AChars: TSysCharSet; AAdvance: Boolean = True): string; overload; inline;
    function ReadWhile(AChars: TSysCharSet; AMax: Integer; AAdvance: Boolean = True): string; overload; inline;
    function ReadUntil(AChars: TSysCharSet; AAdvance: Boolean = True): string; overload; inline;
    procedure SkipWhitespace;

    function ReachedEnd: Boolean; inline;
    function AllText: string; inline;

    procedure PushParser(AParserClass: TParserClass); inline;
    procedure PopParser; inline;

    procedure IncrementParserIndex; inline;

    procedure BeginSuggestions(ASuggestions: TParseSuggestionsClass); overload; inline;
    procedure BeginSuggestions(ASuggestions: TParseSuggestions); overload; inline;
    procedure EndSuggestions;

    procedure Log(ALength: Integer; AMessage: string; ALevel: TParseError.TLevel = elError); overload; inline;
    procedure Log(AMarker: TLogMarker; AMessage: string; ALevel: TParseError.TLevel = elError); overload; inline;
    procedure Log(ALength: Integer; AMessage: string; const AFmt: array of const;
      ALevel: TParseError.TLevel = elError); overload;
    procedure Log(AMarker: TLogMarker; AMessage: string; const AFmt: array of const;
      ALevel: TParseError.TLevel = elError); overload;

    function GetMarker: TLogMarker; inline;

    class function KeepEndSuggestions: Boolean; virtual;

  public
    /// <summary>Parses the given string using the Parse function.</summary>
    /// <param name="AText">The text to parse.</param>
    /// <param name="ANoContext">Disable context if not required to speed up parsing.</param>
    constructor Create(AText: string; AGenerateEditInfo: Boolean); overload;
    /// <summary>Parses using the given TParseInfo object.</summary>
    /// <exception><see cref="Pengine.Parser|EParseError"/> if no success and ARequired is True.</exception>
    constructor Create(AInfo: TParseInfo; ARequired: Boolean); overload; virtual;
    destructor Destroy; override;

    class function GetResultName: string; virtual;
    class function IgnoreContext: Boolean; virtual;

    /// <summary>Wether the parsing was successful.</summary>
    /// <remarks>This will always be true, if ARequired was set to True in the constructor.</remarks>
    property Success: Boolean read FSuccess;

    property Context: TParseInfo.TContext read GetContext;
    function OwnContext: TParseInfo.TContext;

    /// <summary>Returns the count of tokens for the parser.</summary>
    class function GetTokenCount: Integer; virtual;
    /// <summary>Get the name of a token starting from 1, as 0 is TokenNone, which does not have a name.</summary>
    class function GetTokenName(AIndex: Integer): string; virtual;

  end;

  TParser<T> = class(TParser)
  private
    FParseResult: T;

  protected
    procedure SetParseResult(AResult: T);

  public
    property ParseResult: T read FParseResult;

    class function Require(AInfo: TParseInfo): T;

  end;

  /// <summary>Creates a new object, that can be taken ownage of by using OwnParseResult.</summary>
  TObjectParser<T: class> = class(TParser<T>)
  protected
    function GetOptional: T;

  public
    destructor Destroy; override;

    function OwnParseResult: T;

    class function Require(AInfo: TParseInfo): T; reintroduce;
    class function Optional(AInfo: TParseInfo): T; reintroduce;

  end;

  /// <summary>Adds information to an existing object, given in constructor.</summary>
  TRefParser<T: class> = class(TParser)
  private
    FParseObject: T;

  public
    constructor Create(AParseObject: T; AText: string; AGenerateEditInfo: Boolean); overload;
    constructor Create(AParseObject: T; AInfo: TParseInfo; ARequired: Boolean = True); overload;

    property ParseObject: T read FParseObject;

  end;

function ParseSuggestion(ADisplay, AInsert: string): TParseSuggestion;

implementation

function ParseSuggestion(ADisplay, AInsert: string): TParseSuggestion;
begin
  Result := TParseSuggestion.Create(ADisplay, AInsert);
end;

{ TParser }

procedure TParser.Advance(AAmount: Integer);
begin
  Info.Advance(AAmount);
end;

procedure TParser.AdvanceToEnd;
begin
  Info.AdvanceToEnd;
end;

function TParser.AllText: string;
begin
  Result := Info.AllText;
end;

procedure TParser.BeginSuggestions(ASuggestions: TParseSuggestions);
begin
  Info.BeginSuggestions(ASuggestions);
end;

procedure TParser.BeginSuggestions(ASuggestions: TParseSuggestionsClass);
begin
  Info.BeginSuggestions(ASuggestions);
end;

constructor TParser.Create(AInfo: TParseInfo; ARequired: Boolean);
var
  OldPosition: TParsePosition;
  OldErrorCount: Integer;
begin
  FInfo := AInfo;
  OldPosition := Info.Position;
  OldErrorCount := Info.FContext.Log.Count;
  if not IgnoreContext then
    Info.PushParser(TParserClass(ClassType));
  FSuccess := Parse;
  if not KeepEndSuggestions then
    EndSuggestions;
  if not Success then
  begin
    if ARequired then
      raise EParseError.Create('Expected ' + GetResultName + '.');
    Info.FPosition := OldPosition;
    while Info.FContext.Log.Count > OldErrorCount do
      Info.FContext.FLog.RemoveLast;
    while Info.FContext.Length >= Info.Position.Pos do
      Info.FContext.FCharInfo.RemoveLast;
  end;
  if not IgnoreContext then
    Info.PopParser;
end;

destructor TParser.Destroy;
begin
  FContext.Free;
  inherited;
end;

procedure TParser.EndSuggestions;
begin
  Info.EndSuggestions;
end;

function TParser.GetContext: TParseInfo.TContext;
begin
  Result := FContext;
end;

function TParser.GetFirst: Char;
begin
  Result := Info.First;
end;

function TParser.GetMarker: TLogMarker;
begin
  Result := Info.GetMarker;
end;

function TParser.GetToken: Integer;
begin
  Result := Info.FToken;
end;

class function TParser.GetResultName: string;
begin
  Result := ClassName;
end;

class function TParser.GetTokenCount: Integer;
begin
  Result := 0;
end;

class function TParser.GetTokenName(AIndex: Integer): string;
begin
  raise ENotImplemented.Create('GetTokenName is not implemented.');
end;

class function TParser.IgnoreContext: Boolean;
begin
  Result := False;
end;

procedure TParser.IncrementParserIndex;
begin
  Info.IncrementParserIndex;
end;

class function TParser.KeepEndSuggestions: Boolean;
begin
  Result := False;
end;

procedure TParser.Log(ALength: Integer; AMessage: string; ALevel: TParseError.TLevel);
begin
  Info.Log(ALength, AMessage, ALevel);
end;

procedure TParser.Log(ALength: Integer; AMessage: string; const AFmt: array of const; ALevel: TParseError.TLevel);
begin
  Info.Log(ALength, AMessage, AFmt, ALevel);
end;

function TParser.OwnContext: TParseInfo.TContext;
begin
  Result := Context;
  FContext := nil;
end;

procedure TParser.PopParser;
begin
  Info.PopParser;
end;

procedure TParser.PushParser(AParserClass: TParserClass);
begin
  Info.PushParser(AParserClass);
end;

function TParser.ReachedEnd: Boolean;
begin
  Result := Info.ReachedEnd;
end;

function TParser.ReadWhile(APredicate: TPredicate<Char>; AAdvance: Boolean): string;
begin
  Result := Info.ReadWhile(APredicate, AAdvance);
end;

function TParser.ReadUntil(AChars: TSysCharSet; AAdvance: Boolean): string;
begin
  Result := Info.ReadUntil(AChars, AAdvance);
end;

function TParser.ReadWhile(AChars: TSysCharSet; AMax: Integer; AAdvance: Boolean): string;
begin
  Result := Info.ReadWhile(AChars, AMax, AAdvance);
end;

function TParser.ReadWhile(AChars: TSysCharSet; AAdvance: Boolean): string;
begin
  Result := Info.ReadWhile(AChars, AAdvance);
end;

procedure TParser.ResetToken;
begin
  Info.FToken := TokenNone;
end;

procedure TParser.SetToken(AIndex: Integer);
begin
  Info.FToken := AIndex;
end;

procedure TParser.SkipWhitespace;
begin
  Info.SkipWhitespace;
end;

function TParser.StartsWith(AChar: Char; AAdvanceOnMatch: Boolean): Boolean;
begin
  Result := Info.StartsWith(AChar, AAdvanceOnMatch);
end;

function TParser.StartsWith(AText: string; AAdvanceOnMatch: Boolean): Boolean;
begin
  Result := Info.StartsWith(AText, AAdvanceOnMatch);
end;

constructor TParser.Create(AText: string; AGenerateEditInfo: Boolean);
begin
  try
    Create(TParseInfo.Create(AText, AGenerateEditInfo), True);
  except
    on E: EParseError do
      Log(-E.Length, E.Message);
  end;
  if not Success and Info.FContext.Log.Empty then
    Log(1, 'Expected ' + GetResultName + '.', elFatal);
  FContext := Info.OwnContext;

  FInfo.Free;
end;

procedure TParser.Log(AMarker: TLogMarker; AMessage: string; const AFmt: array of const; ALevel: TParseError.TLevel);
begin
  Info.Log(AMarker, AMessage, AFmt, ALevel);
end;

procedure TParser.Log(AMarker: TLogMarker; AMessage: string; ALevel: TParseError.TLevel);
begin
  Info.Log(AMarker, AMessage, ALevel);
end;

{ TParseInfo }

procedure TParseInfo.Advance(AAmount: Integer);
var
  I: Integer;
  Info: TContext.TCharInfoRec;
begin
  for I := 0 to AAmount - 1 do
  begin
    if EditInfoEnabled then
    begin
      Info.ParserStack := FParserStack.Copy;
      Info.ParserStack.ForceCount(Info.ParserStack.Count);

      FContext.FMaxParserDepth := Max(FContext.FMaxParserDepth, Info.ParserStack.Count);

      Info.Token := FToken;

      AddSuggestionsToInfo(Info);

      FContext.FCharInfo.Add(Info);

      UpdateSuggestionsEnd;
    end;

    Inc(FPosition.LinePos);
    if First = #10 then
    begin
      Inc(FPosition.Line);
      FPosition.LinePos := 1;
    end;
    Inc(FPosition.Pos);
  end;
end;

procedure TParseInfo.AdvanceToEnd;
begin
  Advance(FText.Length - Pos + 1);
end;

function TParseInfo.AllText: string;
begin
  Result := FText.Substring(FPosition.Pos - 1);
end;

procedure TParseInfo.BeginSuggestions(ASuggestions: TParseSuggestions);
begin
  FSuggestionsEnd := False;
  FContext.FCurrentSuggestions := ASuggestions;
  FContext.FCurrentSuggestionsSimple := nil;
  FContext.FSuggestions.Add(ASuggestions);
  AddSuggestionToWhitespace;
end;

procedure TParseInfo.BeginSuggestions(ASuggestions: TParseSuggestionsClass);
begin
  FSuggestionsEnd := False;
  FContext.FCurrentSuggestionsSimple := ASuggestions;
  FContext.FCurrentSuggestions := nil;
  AddSuggestionToWhitespace;
end;

function TParseInfo.EditInfoEnabled: Boolean;
begin
  Result := FParserStack <> nil;
end;

constructor TParseInfo.Create(AText: string; AGenerateEditInfo: Boolean);
begin
  FText := AText;
  FPosition.Pos := 1;
  FPosition.Line := 1;
  FPosition.LinePos := 1;
  FContext := TContext.Create;
  if AGenerateEditInfo then
  begin
    FParserStack := TParserStack.Create;
    FParserIndices := TParserIndices.Create;
    FParserIndices.Add(0);
    FTokenStack := TTokenStack.Create;
  end;
end;

destructor TParseInfo.Destroy;
begin
  FParserStack.Free;
  FParserIndices.Free;
  FContext.Free;
  FTokenStack.Free;
  inherited;
end;

procedure TParseInfo.AddSuggestionToWhitespace;
var
  I: Integer;
  Info: TContext.TCharInfoRec;
begin
  I := Pos - 1;
  while (I >= 1) and FText[I].IsWhiteSpace do
  begin
    Info := FContext.FCharInfo[I - 1];
    if Info.Suggestions = nil then
      AddSuggestionsToInfo(Info);
    FContext.FCharInfo.FSkipFree := True;
    FContext.FCharInfo[I - 1] := Info;
    FContext.FCharInfo.FSkipFree := False;
    Dec(I);
  end;
end;

procedure TParseInfo.EndSuggestions;
begin
  FSuggestionsEnd := True;
end;

procedure TParseInfo.Log(ALength: Integer; AMessage: string; const AFmt: array of const; ALevel: TParseError.TLevel);
begin
  Log(ALength, Format(AMessage, AFmt, FormatSettings.Invariant), ALevel);
end;

procedure TParseInfo.Log(ALength: Integer; AMessage: string; ALevel: TParseError.TLevel);
begin
  Include(FContext.FErrorLevels, ALevel);
  FContext.FLog.Add(TParseError.Create(Self.Position, ALength, AMessage, ALevel));
end;

function TParseInfo.GetEnumerator: TIterator;
begin
  Result := TIterator.Create(Self);
end;

procedure TParseInfo.AddSuggestionsToInfo(var AInfo: TContext.TCharInfoRec);
begin
  if FContext.FCurrentSuggestions <> nil then
  begin
    AInfo.UseSuggestionsClass := False;
    AInfo.Suggestions := FContext.FCurrentSuggestions;
  end
  else if FContext.FCurrentSuggestionsSimple <> nil then
  begin
    AInfo.UseSuggestionsClass := True;
    AInfo.SuggestionsClass := FContext.FCurrentSuggestionsSimple;
  end
  else
  begin
    AInfo.UseSuggestionsClass := False;
    AInfo.Suggestions := nil;
  end;
end;

function TParseInfo.GetFirst: Char;
begin
  if ReachedEnd then
    Exit(#0);
  Result := FText[Pos];
end;

function TParseInfo.GetMarker: TLogMarker;
begin
  Result := Pos;
end;

procedure TParseInfo.IncrementParserIndex;
var
  NewIndex: Integer;
begin
  NewIndex := FParserStack.Last.Index + 1;
  FParserIndices[FParserIndices.MaxIndex - 1] := NewIndex;
  FParserStack.Last := TParserInfo.Create(FParserStack.Last.ParserClass, NewIndex);
end;

procedure TParseInfo.Log(AMarker: TLogMarker; AMessage: string; const AFmt: array of const; ALevel: TParseError.TLevel);
begin
  Log(AMarker, Format(AMessage, AFmt, FormatSettings.Invariant), ALevel);
end;

procedure TParseInfo.Log(AMarker: TLogMarker; AMessage: string; ALevel: TParseError.TLevel);
begin
  Log(AMarker.Pos - Pos, AMessage, ALevel);
end;

function TParseInfo.OwnContext: TContext;
begin
  Result := FContext;
  FContext := nil;
end;

procedure TParseInfo.PopParser;
begin
  if EditInfoEnabled then
  begin
    FParserStack.RemoveLast;
    FParserIndices.RemoveLast;
    FToken := FTokenStack.Pop;
  end;
end;

procedure TParseInfo.PushParser(AParserClass: TParserClass);
begin
  if EditInfoEnabled then
  begin
    FParserStack.Add(TParserInfo.Create(AParserClass, FParserIndices.Last));
    FParserIndices.Last := FParserIndices.Last + 1;
    FParserIndices.Add(0);
    FTokenStack.Push(FToken);
    FToken := 0;
  end;
end;

function TParseInfo.ReachedEnd: Boolean;
begin
  Result := Pos > FText.Length;
end;

function TParseInfo.ReadUntil(AChars: TSysCharSet; AAdvance: Boolean): string;
begin
  Result := ReadWhile(
    function(C: Char): Boolean
    begin
      Result := not CharInSet(C, AChars);
    end,
    AAdvance);
end;

function TParseInfo.ReadWhile(AChars: TSysCharSet; AMax: Integer; AAdvance: Boolean): string;
begin
  Result := ReadWhile(
    function(C: Char): Boolean
    begin
      Result := CharInSet(C, AChars) and (AMax <> 0);
      Dec(AMax);
    end,
    AAdvance);
end;

function TParseInfo.ReadWhile(AChars: TSysCharSet; AAdvance: Boolean): string;
begin
  Result := ReadWhile(
    function(C: Char): Boolean
    begin
      Result := CharInSet(C, AChars);
    end,
    AAdvance);
end;

function TParseInfo.ReadWhile(APredicate: TPredicate<Char>; AAdvance: Boolean): string;
var
  Len: Integer;
begin
  Len := 0;
  while (Pos + Len <= FText.Length) and APredicate(FText[Pos + Len]) do
    Inc(Len);
  Result := FText.Substring(Pos - 1, Len);
  if AAdvance then
    Advance(Len);
end;

function TParseInfo.StartsWith(AChar: Char; AAdvanceOnMatch: Boolean): Boolean;
begin
  Result := First = AChar;
  if Result and AAdvanceOnMatch then
    Advance;
end;

procedure TParseInfo.SkipWhitespace;
begin
  while First.IsWhiteSpace do
    Advance;
end;

function TParseInfo.StartsWith(AText: string; AAdvanceOnMatch: Boolean): Boolean;
var
  Len: Integer;
begin
  Len := AText.Length;
  Result := FText.Substring(Pos - 1, Len) = AText;
  if Result and AAdvanceOnMatch then
    Advance(Len);
end;

procedure TParseInfo.UpdateSuggestionsEnd;
begin
  if FSuggestionsEnd then
  begin
    FContext.FCurrentSuggestionsSimple := nil;
    FContext.FCurrentSuggestions := nil;
    FSuggestionsEnd := False;
  end;
end;

{ TParseInfo.TIterator }

constructor TParseInfo.TIterator.Create;
begin
  FInfo := AInfo;
  FFirst := True;
end;

function TParseInfo.TIterator.GetCurrent: Char;
begin
  Result := FInfo.FText[FInfo.Pos];
end;

function TParseInfo.TIterator.MoveNext: Boolean;
begin
  if not FFirst then
    FInfo.Advance
  else
    FFirst := False;
  Result := not FInfo.ReachedEnd;
end;

{ TParser<T> }

class function TParser<T>.Require(AInfo: TParseInfo): T;
begin
  with Self.Create(AInfo, True) do
  begin
    Result := ParseResult;
    Free;
  end;
end;

procedure TParser<T>.SetParseResult(AResult: T);
begin
  FParseResult := AResult;
end;

{ TObjectParser<T> }

destructor TObjectParser<T>.Destroy;
begin
  ParseResult.Free;
  inherited;
end;

function TObjectParser<T>.GetOptional: T;
begin
  if Success then
    Result := OwnParseResult
  else
    Result := nil;
  Free;
end;

class function TObjectParser<T>.Optional(AInfo: TParseInfo): T;
begin
  Result := Self.Create(AInfo, False).GetOptional;
end;

function TObjectParser<T>.OwnParseResult: T;
begin
  Result := ParseResult;
  SetParseResult(nil);
end;

class function TObjectParser<T>.Require(AInfo: TParseInfo): T;
begin
  with Self.Create(AInfo, True) do
  begin
    Result := OwnParseResult;
    Free;
  end;
end;

{ TParseInfo.TContext }

constructor TParseInfo.TContext.Create;
begin
  FCharInfo := TCharInfo.Create;
  FSuggestions := TSuggestions.Create;
  FLog := TLog.Create;
end;

destructor TParseInfo.TContext.Destroy;
begin
  FLog.Free;
  FSuggestions.Free;
  FCharInfo.Free;
  inherited;
end;

function TParseInfo.TContext.GetLog: TLog.TReader;
begin
  Result := FLog.Reader;
end;

function TParseInfo.TContext.GetHighestErrorLevel: TParseError.TLevel;
begin
  for Result := High(TParseError.TLevel) downto Low(TParseError.TLevel) do
    if Result in ErrorLevels then
      Exit;
  Result := elNone;
end;

function TParseInfo.TContext.GetLength: Integer;
begin
  Result := FCharInfo.Count;
end;

function TParseInfo.TContext.GetToken(APos: Integer): Integer;
begin
  if not FCharInfo.RangeCheck(APos) then
    Exit(0);
  Result := FCharInfo[APos].Token;
end;

function TParseInfo.TContext.HasSuggestions(APos: Integer): Boolean;
begin
  Result := GetSuggestionPos(APos) <> spNone;
end;

function TParseInfo.TContext.GetSuggestionPos(APos: Integer): TSuggestionPos;
begin
  if APos >= Length then
  begin
    if (FCurrentSuggestionsSimple <> nil) or (FCurrentSuggestions <> nil) then
      Exit(spEnd);
    Exit(spNone);
  end;
  if FCharInfo[APos].Suggestions <> nil then
    Exit(spGiven);
  Result := spNone;
end;

function TParseInfo.TContext.GetParsers(APos: Integer): TParserStack.TReader;
begin
  if not FCharInfo.RangeCheck(APos) then
    Exit(nil);
  Result := FCharInfo[APos].ParserStack.Reader;
end;

function TParseInfo.TContext.GetSuccess: Boolean;
begin
  Result := HighestErrorLevel <= elWarning;
end;

function TParseInfo.TContext.GetSuggestion(APos, AIndex: Integer): TParseSuggestion;
begin
  case GetSuggestionPos(APos) of
    spGiven:
      begin
        if FCharInfo[APos].UseSuggestionsClass then
          Result := FCharInfo[APos].SuggestionsClass.GetSuggestion(AIndex)
        else
          Result := FCharInfo[APos].Suggestions.GetSuggestion(AIndex);
      end;
    spEnd:
      begin
        if FCurrentSuggestions <> nil then
          Result := FCurrentSuggestions.GetSuggestion(AIndex)
        else
          Result := FCurrentSuggestionsSimple.GetSuggestion(AIndex);
      end;
    spNone:
      Assert(False);
  end;
end;

function TParseInfo.TContext.GetSuggestionBreakChars(APos: Integer): TSysCharSet;
begin
  case GetSuggestionPos(APos) of
    spGiven:
      begin
        if FCharInfo[APos].UseSuggestionsClass then
          Result := FCharInfo[APos].SuggestionsClass.GetBreakChars
        else
          Result := FCharInfo[APos].Suggestions.GetBreakChars;
      end;
    spEnd:
      begin
        if FCurrentSuggestions <> nil then
          Result := FCurrentSuggestions.GetBreakChars
        else
          Result := FCurrentSuggestionsSimple.GetBreakChars;
      end;
  else
    raise EParseSuggestionException.Create;
  end;
end;

function TParseInfo.TContext.GetSuggestionBreakCharString(APos: Integer): string;
var
  C: Char;
  Len: Integer;
begin
  Result := '';
  Len := 0;
  for C in SuggestionBreakChars[APos] do
    Inc(Len);

  SetLength(Result, Len);
  Len := 1;
  for C in SuggestionBreakChars[APos] do
  begin
    Result[Len] := C;
    Inc(Len);
  end;
end;

function TParseInfo.TContext.GetSuggestionCount(APos: Integer): Integer;
begin
  case GetSuggestionPos(APos) of
    spGiven:
      begin
        if FCharInfo[APos].UseSuggestionsClass then
          Result := FCharInfo[APos].SuggestionsClass.GetCount
        else
          Result := FCharInfo[APos].Suggestions.GetCount;
      end;
    spEnd:
      begin
        if FCurrentSuggestions <> nil then
          Result := FCurrentSuggestions.GetCount
        else
          Result := FCurrentSuggestionsSimple.GetCount;
      end;
  else
    raise EParseSuggestionException.Create;
  end;
end;

function TParseInfo.TContext.GetSuggestionTitle(APos: Integer): string;
begin
  case GetSuggestionPos(APos) of
    spGiven:
      begin
        if FCharInfo[APos].UseSuggestionsClass then
          Result := FCharInfo[APos].SuggestionsClass.GetTitle
        else
          Result := FCharInfo[APos].Suggestions.GetTitle;
      end;
    spEnd:
      begin
        if FCurrentSuggestions <> nil then
          Result := FCurrentSuggestions.GetTitle
        else
          Result := FCurrentSuggestionsSimple.GetTitle;
      end;
    spNone:
      Assert(False);
  end;
end;

{ TRefParser<T> }

constructor TRefParser<T>.Create(AParseObject: T; AText: string; AGenerateEditInfo: Boolean);
begin
  FParseObject := AParseObject;
  inherited Create(AText, AGenerateEditInfo);
end;

constructor TRefParser<T>.Create(AParseObject: T; AInfo: TParseInfo; ARequired: Boolean);
begin
  FParseObject := AParseObject;
  inherited Create(AInfo, ARequired);
end;

{ TParseInfo.TContext.TCharInfo }

procedure TParseInfo.TContext.TCharInfo.ItemRemoved(AIndex: Integer);
begin
  inherited;
  Self[AIndex].ParserStack.Free;
end;

function TParseInfo.TContext.TCharInfo.ShouldFreeItems: Boolean;
begin
  Result := not FSkipFree;
end;

{ TParseSuggestionsSimple<T> }

class function TParseSuggestionsSimple<T>.GetTitle: string;
begin
  Result := T.GetResultName;
end;

{ TParseSuggestion }

constructor TParseSuggestion.Create(ADisplay, AInsert: string);
begin
  Display := ADisplay;
  Insert := AInsert;
end;

class operator TParseSuggestion.Implicit(AText: string): TParseSuggestion;
begin
  Result.Display := AText;
  Result.Insert := AText;
end;

{ TParseError }

constructor TParseError.Create(APosition: TParsePosition; ALength: Integer; AMessage: string; ALevel: TLevel);
begin
  FPosition := APosition;
  if ALength < 0 then
  begin
    FPosition.LinePos := Position.LinePos + ALength;
    Assert(FPosition.LinePos >= 1);
    FLength := -ALength;
  end
  else
  begin
    FLength := ALength;
  end;
  FMessage := AMessage;
  FLevel := ALevel;
end;

function TParseError.GetLevelName: string;
begin
  Result := LevelNames[Level];
end;

{ TParsePosition }

function TParsePosition.FormatLinePos: string;
begin
  Result := Format('%d:%d', [Line, LinePos]);
end;

{ EParseSuggestionException }

constructor EParseSuggestionException.Create;
begin
  inherited Create('THe given position does not have any suggestions.');
end;

{ TParseSuggestionsGenerated }

procedure TParseSuggestionsGenerated.AddSuggestion(ASuggestion: TParseSuggestion);
begin
  FSuggestions.Add(ASuggestion);
end;

procedure TParseSuggestionsGenerated.AddUniqueSuggestion(ASuggestion: TParseSuggestion);
var
  Suggestion: TParseSuggestion;
begin
  for Suggestion in FSuggestions do
    if Suggestion.Display = ASuggestion.Display then
      Exit;
  AddSuggestion(ASuggestion);
end;

destructor TParseSuggestionsGenerated.Destroy;
begin
  FSuggestions.Free;
  inherited;
end;

function TParseSuggestionsGenerated.GetCount: Integer;
begin
  Result := Suggestions.Count;
end;

function TParseSuggestionsGenerated.GetSuggestion(AIndex: Integer): TParseSuggestion;
begin
  Result := Suggestions[AIndex];
end;

function TParseSuggestionsGenerated.GetSuggestions: TSuggestions;
begin
  if FSuggestions = nil then
  begin
    FSuggestions := TSuggestions.Create;
    Generate;
  end;
  Result := FSuggestions;
end;

{ TParseSuggestions<T> }

function TParseSuggestions<T>.GetTitle: string;
begin
  Result := T.GetResultName;
end;

{ TParseSuggestionsGenerated<T> }

function TParseSuggestionsGenerated<T>.GetTitle: string;
begin
  Result := T.GetResultName;
end;

{ EParseError }

constructor EParseError.Create(AMessage: string; ALength: Integer);
begin
  inherited Create(AMessage);
  FLength := ALength;
end;

constructor EParseError.CreateFmt(AMessage: string; const AFmt: array of const; ALength: Integer);
begin
  inherited CreateFmt(AMessage, AFmt);
  FLength := ALength;
end;

{ TLogMarker }

class operator TLogMarker.Implicit(AMarker: TLogMarker): Integer;
begin
  Result := AMarker.Pos;
end;

class operator TLogMarker.Implicit(APos: Integer): TLogMarker;
begin
  Result.Pos := APos;
end;

{ TParseSuggestions }

function TParseSuggestions.GetBreakChars: TSysCharSet;
begin
  Result := DefaultBreakChars;
end;

{ TParseSuggestionsSimple }

class function TParseSuggestionsSimple.GetBreakChars: TSysCharSet;
begin
  Result := TParseSuggestions.DefaultBreakChars;
end;

{ TParseInfo.TParserInfo }

constructor TParseInfo.TParserInfo.Create(AParserClass: TParserClass; AIndex: Integer);
begin
  ParserClass := AParserClass;
  Index := AIndex;
end;

end.
