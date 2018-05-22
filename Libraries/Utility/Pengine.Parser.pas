unit Pengine.Parser;

interface

uses
  System.SysUtils,
  System.Character,
  System.Math,

  Pengine.Collections,
  Pengine.CollectionInterfaces;

type

  TParser = class;
  TParserClass = class of TParser;

  TParseInfo = class
  public type

    TParserStack = TArray<TParserClass>;

    TExtraStack = TStack<Integer>;

    TCharInfo = class
    public type

      TParserStacks = TObjectArray<TParserStack>;

      TExtraData = TArray<Integer>;

    private
      FParsers: TParserStacks;
      FMaxDepth: Integer;
      FExtraData: TExtraData;

      function GetCount: Integer;
      function GetParsers(AIndex: Integer): TParserStack.TReader;
      function GetExtraData(AIndex: Integer): Integer;

    public
      constructor Create;
      destructor Destroy; override;

      property Parsers[AIndex: Integer]: TParserStack.TReader read GetParsers;
      property ExtraData[AIndex: Integer]: Integer read GetExtraData;
      property Count: Integer read GetCount;
      property MaxDepth: Integer read FMaxDepth;

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
    FPos: Integer;
    FLine: Integer;
    FLinePos: Integer;
    FParserStack: TParserStack;
    FExtraStack: TExtraStack;
    FExtraData: Integer;
    FCharInfo: TCharInfo;

    function GetFirst: Char;

  public
    constructor Create(AText: string);
    destructor Destroy; override;

    property Pos: Integer read FPos;
    property Line: Integer read FLine;
    property LinePos: Integer read FLinePos;

    property First: Char read GetFirst;

    procedure Advance(AAmount: Integer = 1);
    function StartsWith(AChar: Char; AAdvanceOnMatch: Boolean = True): Boolean; overload;
    function StartsWith(AText: string; AAdvanceOnMatch: Boolean = True): Boolean; overload;
    function ReadWhile(APredicate: TPredicate<Char>; AAdvance: Boolean = True): string; overload;
    function ReadWhile(AChars: TSysCharSet; AAdvance: Boolean = True): string; overload;
    procedure SkipWhitespace;
    function ReadUntilWhitespace: string;

    function ReachedEnd: Boolean;

    function AllText: string;

    procedure PushParser(AParserClass: TParserClass);
    procedure PopParser;

    function OwnCharInfo: TCharInfo;

    function GetEnumerator: TIterator;

  end;

  TParser = class
  public const

    TokenNone = 0;

  private
    FInfo: TParseInfo;
    FLeftover: string;
    FErrorMessage: string;
    FErrorPos: Integer;
    FErrorLine: Integer;
    FErrorLinePos: Integer;
    FSuccess: Boolean;
    FParsers: TParseInfo.TCharInfo;

    function GetCharInfo: TParseInfo.TCharInfo;
    function GetExtraData: Integer;
    procedure SetExtraData(AIndex: Integer);

  protected
    function Parse: Boolean; virtual; abstract;

    property Info: TParseInfo read FInfo;
    property ExtraData: Integer read GetExtraData write SetExtraData;
    procedure ResetExtraData;

  public
    /// <summary>Parses the given string using the Parse function.</summary>
    constructor Create(AText: string); overload;
    /// <summary>Parses using the given TParseInfo object.</summary>
    /// <exception><see cref="Pengine.Parser|EParseError"/> if no success and ARequired is True.</exception>
    constructor Create(AInfo: TParseInfo; ARequired: Boolean = False); overload;
    destructor Destroy; override;

    class function GetResultName: string; virtual;
    class function IgnoreCharInfo: Boolean; virtual;

    /// <returns>Wether there leftover, which got not used in parsing.</returns>
    /// <remarks>If the parsing was no successful, there cannot be any leftover.</remarks>
    function HasLeftover: Boolean;
    /// <summary>Any leftover, that got not used in parsing.</summary>
    /// <remarks>If the parsing was no successful, there cannot be any leftover.</remarks>
    property Leftover: string read FLeftover;
    /// <summary>Wether the parsing was successful.</summary>
    /// <remarks>This will always be true, if ARequired was set to True in the constructor.</remarks>
    property Success: Boolean read FSuccess;
    /// <summary>The error message if success was False.</summary>
    property ErrorMessage: string read FErrorMessage;
    /// <summary>The error position in the string if success was False.</summary>
    property ErrorPos: Integer read FErrorPos;
    /// <summary>The error line if success was False.</summary>
    property ErrorLine: Integer read FErrorLine;
    /// <summary>The error position in the error line if success was False.</summary>
    property ErrorLinePos: Integer read FErrorLinePos;

    property CharInfo: TParseInfo.TCharInfo read GetCharInfo;

    class function GetTokenCount: Integer; virtual;
    class function GetTokenName(AIndex: Integer): string; virtual;

  end;

  TParser<T> = class(TParser)
  private
    FParseResult: T;

  protected
    procedure SetParseResult(AResult: T);

  public
    property ParseResult: T read FParseResult;

  end;

  /// <summary>Creates a new object, that can be taken ownage of by using OwnParseResult.</summary>
  TObjectParser<T: class> = class(TParser<T>)
  public
    destructor Destroy; override;

    function OwnParseResult: T;

  end;

  /// <summary>Adds information to an existing object, given in constructor.</summary>
  TRefParser<T: class> = class(TParser)
  private
    FParseObject: T;

  public
    constructor Create(AParseObject: T; AText: string); overload;
    constructor Create(AParseObject: T; AInfo: TParseInfo; ARequired: Boolean = True); overload;

    property ParseObject: T read FParseObject;

  end;

  TIdentifierParser = class(TParser<string>)
  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;

  end;

  TStringOrIdentParser = class(TParser<string>)
  public const

    TokenQuote = 1;
    TokenContent = 2;
    TokenBackslash = 3;
    TokenEscaped = 4;
    TokenUnquoted = 5;

    TokenNames: array [TokenQuote .. TokenUnquoted] of string = (
      'Quotes',
      'Content',
      'Backslash',
      'Escaped',
      'Unquoted'
    );

  private
    FIsIdent: Boolean;

  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
    class function GetTokenCount: Integer; override;
    class function GetTokenName(AIndex: Integer): string; override;

    property IsIdent: Boolean read FIsIdent;

  end;

  EParseError = class(Exception)
  private
    FPos: Integer;
    FLine: Integer;
    FLinePos: Integer;

    procedure CheckInfo(AInfo: TParseInfo);

  public
    constructor Create(AInfo: TParseInfo; AMessage: string);
    constructor CreateFmt(AInfo: TParseInfo; AMessage: string; const AArgs: array of const);

    property Pos: Integer read FPos;
    property Line: Integer read FLine;
    property LinePos: Integer read FLinePos;

  end;

implementation

{ TParser }

constructor TParser.Create(AInfo: TParseInfo; ARequired: Boolean);
var
  OldPos, OldLine, OldLinePos: Integer;
begin
  FInfo := AInfo;
  OldPos := Info.Pos;
  OldLine := Info.Line;
  OldLinePos := Info.LinePos;
  if not IgnoreCharInfo then
    Info.PushParser(TParserClass(ClassType));
  if Parse then
  begin
    FSuccess := True;
    FLeftover := Info.AllText;
  end
  else
  begin
    if ARequired then
      raise EParseError.Create(Info, 'Expected ' + GetResultName + '.');
    Info.FPos := OldPos;
    Info.FLine := OldLine;
    Info.FLinePos := OldLinePos;
    while Info.FCharInfo.FParsers.Count >= Info.FPos do
    begin
      Info.FCharInfo.FParsers.RemoveLast;
      Info.FCharInfo.FExtraData.RemoveLast;
    end;
  end;
  if not IgnoreCharInfo then
    Info.PopParser;
end;

destructor TParser.Destroy;
begin
  FParsers.Free;
  inherited;
end;

function TParser.GetCharInfo: TParseInfo.TCharInfo;
begin
  Result := FParsers;
end;

function TParser.GetExtraData: Integer;
begin
  Result := Info.FExtraData;
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

function TParser.HasLeftover: Boolean;
begin
  Result := not FLeftover.IsEmpty;
end;

class function TParser.IgnoreCharInfo: Boolean;
begin
  Result := False;
end;

procedure TParser.ResetExtraData;
begin
  Info.FExtraData := TokenNone;
end;

procedure TParser.SetExtraData(AIndex: Integer);
begin
  Info.FExtraData := AIndex;
end;

constructor TParser.Create(AText: string);
begin
  try
    Create(TParseInfo.Create(AText));
  except
    on E: EParseError do
    begin
      FErrorMessage := E.Message;
      FErrorPos := E.Pos;
      FErrorLine := E.Line;
      FErrorLinePos := E.LinePos;
    end;
  end;
  if not Success and ErrorMessage.IsEmpty then
  begin
    FErrorMessage := 'Expected ' + GetResultName + '.';
    FErrorPos := Info.Pos;
    FErrorLine := Info.Line;
    FErrorLinePos := Info.LinePos;
  end;
  FParsers := Info.OwnCharInfo;
  Info.Free;
end;

{ TParseInfo }

procedure TParseInfo.Advance(AAmount: Integer);
var
  I: Integer;
begin
  for I := 0 to AAmount - 1 do
  begin
    FCharInfo.FParsers.Add(FParserStack.Copy);
    FCharInfo.FParsers.Last.ForceCount(FCharInfo.FParsers.Last.Count);
    FCharInfo.FMaxDepth := Max(FCharInfo.FMaxDepth, FCharInfo.FParsers.Last.Count);
    FCharInfo.FExtraData.Add(FExtraData);
    Inc(FLinePos);
    if First = #13 then
    begin
      Inc(FLine);
      FLinePos := 1;
    end;
    Inc(FPos);
  end;
end;

function TParseInfo.AllText: string;
begin
  Result := FText.Substring(FPos - 1);
end;

constructor TParseInfo.Create(AText: string);
begin
  FText := AText;
  FPos := 1;
  FLine := 1;
  FLinePos := 1;
  FParserStack := TParserStack.Create;
  FCharInfo := TCharInfo.Create;
  FExtraStack := TExtraStack.Create;
end;

destructor TParseInfo.Destroy;
begin
  FParserStack.Free;
  FCharInfo.Free;
  FExtraStack.Free;
  inherited;
end;

function TParseInfo.GetEnumerator: TIterator;
begin
  Result := TIterator.Create(Self);
end;

function TParseInfo.GetFirst: Char;
begin
  if ReachedEnd then
    Exit(#0);
  Result := FText[Pos];
end;

function TParseInfo.OwnCharInfo: TCharInfo;
begin
  Result := FCharInfo;
  FCharInfo := nil;
end;

procedure TParseInfo.PopParser;
begin
  FParserStack.RemoveLast;
  FExtraData := FExtraStack.Pop;
end;

procedure TParseInfo.PushParser(AParserClass: TParserClass);
begin
  FParserStack.Add(AParserClass);
  FExtraStack.Push(FExtraData);
  FExtraData := 0;
end;

function TParseInfo.ReachedEnd: Boolean;
begin
  Result := Pos > FText.Length;
end;

function TParseInfo.ReadUntilWhitespace: string;
begin
  Result := ReadWhile(
    function(C: Char): Boolean
    begin
      Result := not C.IsWhiteSpace;
    end
    );
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
  while not ReachedEnd and APredicate(FText[Pos + Len]) do
    Inc(Len);
  Result := FText.Substring(FPos - 1, Len);
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
  Result := FText.Substring(FPos - 1, Len) = AText;
  if Result and AAdvanceOnMatch then
    Advance(Len);
end;

{ TParseInfo.TIterator }

constructor TParseInfo.TIterator.Create;
begin
  FInfo := AInfo;
  FFirst := True;
end;

function TParseInfo.TIterator.GetCurrent: Char;
begin
  Result := FInfo.FText[FInfo.FPos];
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

procedure TParser<T>.SetParseResult(AResult: T);
begin
  FParseResult := AResult;
end;

{ TIdentifierParser }

class function TIdentifierParser.GetResultName: string;
begin
  Result := 'Identifier';
end;

function TIdentifierParser.Parse: Boolean;
const
  Alpha = ['a' .. 'z', 'A' .. 'Z', '_'];
  AlphaNum = Alpha + ['0' .. '9'];

begin
  if not CharInSet(Info.First, Alpha) then
    Exit(False);
  FParseResult := Info.First;
  Info.Advance;
  FParseResult := FParseResult + Info.ReadWhile(AlphaNum);
  Result := True;
end;

{ TObjectParser<T> }

destructor TObjectParser<T>.Destroy;
begin
  ParseResult.Free;
  inherited;
end;

function TObjectParser<T>.OwnParseResult: T;
begin
  Result := ParseResult;
  SetParseResult(nil);
end;

{ EParseError }

procedure EParseError.CheckInfo(AInfo: TParseInfo);
begin
  FPos := AInfo.Pos;
  FLine := AInfo.Line;
  FLinePos := AInfo.LinePos;
end;

constructor EParseError.Create(AInfo: TParseInfo; AMessage: string);
begin
  inherited Create(AMessage);
  CheckInfo(AInfo);
end;

constructor EParseError.CreateFmt(AInfo: TParseInfo; AMessage: string; const AArgs: array of const);
begin
  inherited CreateFmt(AMessage, AArgs);
  CheckInfo(AInfo);
end;

{ TParseInfo.TCharInfo }

constructor TParseInfo.TCharInfo.Create;
begin
  FParsers := TParserStacks.Create;
  FExtraData := TExtraData.Create;
end;

destructor TParseInfo.TCharInfo.Destroy;
begin
  FExtraData.Free;
  FParsers.Free;
  inherited;
end;

function TParseInfo.TCharInfo.GetCount: Integer;
begin
  Result := FParsers.Count;
end;

function TParseInfo.TCharInfo.GetExtraData(AIndex: Integer): Integer;
begin
  Result := FExtraData[AIndex];
end;

function TParseInfo.TCharInfo.GetParsers(AIndex: Integer): TParserStack.TReader;
begin
  Result := FParsers[AIndex].Reader;
end;

{ TRefParser<T> }

constructor TRefParser<T>.Create(AParseObject: T; AText: string);
begin
  FParseObject := AParseObject;
  inherited Create(AText);
end;

constructor TRefParser<T>.Create(AParseObject: T; AInfo: TParseInfo; ARequired: Boolean);
begin
  FParseObject := AParseObject;
  inherited Create(AInfo, ARequired);
end;

{ TStringOrIdentParser }

class function TStringOrIdentParser.GetResultName: string;
begin
  Result := 'String or Identifier';
end;

class function TStringOrIdentParser.GetTokenCount: Integer;
begin
  Result := High(TokenNames);
end;

class function TStringOrIdentParser.GetTokenName(AIndex: Integer): string;
begin
  Result := TokenNames[AIndex];
end;

function TStringOrIdentParser.Parse: Boolean;
const
  AlphaNum = ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '_'];
var
  C: Char;
begin
  SetParseResult('');

  ExtraData := TokenQuote;

  FIsIdent := not Info.StartsWith('"');
  if IsIdent then
  begin
    ExtraData := TokenUnquoted;
    for C in Info do
    begin
      if not CharInSet(C, AlphaNum) then
        Break;
      SetParseResult(ParseResult + C);
    end;
    Result := True;
  end
  else
  begin
    ExtraData := TokenContent;
    for C in Info do
    begin
      if ExtraData = TokenEscaped then
        ExtraData := TokenContent;
      case C of
        '"':
          begin
            ExtraData := TokenQuote;
            Info.Advance;
            Exit(True);
          end;
        '\':
          begin
            ExtraData := TokenBackslash;
            Info.Advance;
            if not CharInSet(Info.First, ['"', '\']) then
              raise EParseError.Create(Info, 'Only \" and \\ are valid escape sequences.');
            ExtraData := TokenEscaped;
          end;
      end;
      SetParseResult(ParseResult + Info.First);
    end;
    raise EParseError.Create(Info, 'Found unterminated string.');
  end;
end;

end.
