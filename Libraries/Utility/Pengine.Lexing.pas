unit Pengine.Lexing;

interface

uses
  System.SysUtils,
  System.Character,

  Pengine.Parsing,
  Pengine.ICollections;

type

  ELexingRuleError = class(Exception);

  ISyntaxTree = interface;

  TEBNF = class
  public type

    TExpression = class;

    IEBNFParser<T: TExpression> = interface(IObjectParser<T>)
      function GetEBNF: TEBNF;

      property EBNF: TEBNF read GetEBNF;

    end;

    TEBNFParser<T: TExpression> = class(TObjectParser<T>, IEBNFParser<T>)
    private
      FEBNF: TEBNF;

      function GetEBNF: TEBNF;

    public
      constructor Create(AEBNF: TEBNF);

      property EBNF: TEBNF read GetEBNF;

    end;

    TLexer = class;

    TExpression = class
    public type

      IParser = IEBNFParser<TExpression>;

      TParser = class(TEBNFParser<TExpression>, IParser)
      protected
        function Parse: Boolean; override;

      public
        class function GetResultName: string; override;

      end;

      TGroupParser = class(TEBNFParser<TExpression>, IParser)
      protected
        function Parse: Boolean; override;

      public
        class function GetResultName: string; override;

      end;

    protected
      function Analyze(ALexer: TLexer): Boolean; virtual; abstract;

    public
      class function Parser(AEBNF: TEBNF): TExpression.IParser; static;
      class function GroupParser(AEBNF: TEBNF): TExpression.IParser; static;

    end;

    TTerminal = class(TExpression)
    public type

      IParser = IEBNFParser<TTerminal>;

      TParser = class(TEBNFParser<TTerminal>, IParser)
      protected
        function Parse: Boolean; override;

      public
        class function GetResultName: string; override;

      end;

    private
      FTerminal: string;

    protected
      function Analyze(ALexer: TLexer): Boolean; override;

    public
      constructor Create(ATerminal: string);

      class function Parser(AEBNF: TEBNF): TTerminal.IParser; static;

      property Terminal: string read FTerminal;

      function ToString: string; override;

    end;

    TUnaryExpression = class(TExpression)
    private
      FExpression: TExpression;

    public
      constructor Create(AExpression: TExpression);

      class function Parser(AEBNF: TEBNF): IParser; static;

      property Expression: TExpression read FExpression;

    end;

    TVariadicExpressionClass = class of TVariadicExpression;

    TVariadicExpression = class(TExpression)
    private
      FExpressions: IList<TExpression>;

      function GetExpressions: IReadonlyList<TExpression>;

    public
      constructor Create(AExpressions: IIterable<TExpression>); overload; virtual;
      constructor Create(AExpressions: TArray<TExpression>); overload; virtual;

      property Expressions: IReadonlyList<TExpression> read GetExpressions;

    end;

    TConcatenation = class(TVariadicExpression)
    protected
      function Analyze(ALexer: TLexer): Boolean; override;

    public
      function ToString: string; override;

    end;

    TAlternation = class(TVariadicExpression)
    protected
      function Analyze(ALexer: TLexer): Boolean; override;

    public
      function ToString: string; override;

    end;

    TOptional = class(TUnaryExpression)
    public type

      IParser = IEBNFParser<TOptional>;

      TParser = class(TEBNFParser<TOptional>, IParser)
      protected
        function Parse: Boolean; override;

      public
        class function GetResultName: string; override;

      end;

    protected
      function Analyze(ALexer: TLexer): Boolean; override;

    public
      class function Parser(AEBNF: TEBNF): TOptional.IParser; static;

      function ToString: string; override;

    end;

    TRepetition = class(TUnaryExpression)
    public type

      IParser = IEBNFParser<TRepetition>;

      TParser = class(TEBNFParser<TRepetition>, IParser)
      protected
        function Parse: Boolean; override;

      public
        class function GetResultName: string; override;

      end;

    protected
      function Analyze(ALexer: TLexer): Boolean; override;

    public
      class function Parser(AEBNF: TEBNF): TRepetition.IParser; static;

      function ToString: string; override;

    end;

    TRule = class(TExpression)
    public type

      IParser = IEBNFParser<TRule>;

      TParser = class(TEBNFParser<TRule>, IParser)
      protected
        function Parse: Boolean; override;

      public
        class function GetResultName: string; override;

      end;

    private
      FEBNF: TEBNF;
      FName: string;
      FExpressionString: string;
      FExpression: TExpression;

      function GetExpression: TExpression;
      function GetExpressionString: string;

    protected
      function Analyze(ALexer: TLexer): Boolean; override;

    public
      constructor Create(AEBNF: TEBNF; AName: string; AExpression: TExpression); overload;
      constructor Create(AEBNF: TEBNF; AName, AExpressionString: string); overload;

      class function Parser(AEBNF: TEBNF): TRule.IParser; static;

      property EBNF: TEBNF read FEBNF;
      property Expression: TExpression read GetExpression;
      property ExpressionString: string read GetExpressionString;
      property Name: string read FName;

      function ToString: string; override;

      function Lex(AText: string): ISyntaxTree; overload;
      function Lex<T: TLexer>(AText: string): ISyntaxTree; overload;

    end;

    /// <summary>Builds an abstract syntax tree from a given EBNF expression.</summary>
    TLexer = class
    private
      FSyntaxTree: ISyntaxTree;
      FCurrentSyntaxTree: ISyntaxTree;
      FText: string;
      FPosition: Integer;
      FSavedPositions: IStack<Integer>;
      FRulePositions: IList<Integer>; // used to detect recursion

    public
      constructor Create(ARule: TRule; AText: string); virtual;

      property SyntaxTree: ISyntaxTree read FSyntaxTree;

      property Text: string read FText;
      property Position: Integer read FPosition;
      function ReachedEnd: Boolean;

      function LastRulePosition(ARule: TRule): Integer;
      function AdvancedSinceRule(ARule: TRule): Boolean;
      procedure BeginRule(ARule: TRule);
      procedure EndRule(AKeep: Boolean);

      procedure Save;
      procedure DiscardSave;
      procedure Revert;

      procedure Advance(AAmount: Integer);
      function StartsWith(AText: string; AAdvanceOnMatch: Boolean = True): Boolean;
      procedure SkipWhitespace;

    end;

  private
    FRules: IList<TRule>;

    function GetRuleString(AName: string): string;
    procedure SetRuleString(AName: string; const Value: string);
    function GetRules: IReadonlyList<TRule>;

  public
    constructor Create;

    function ExpressionParser: TExpression.IParser;
    function ParseExpression(AText: string): TExpression;

    function AddRule(AName: string; AExpression: TExpression): TRule;
    property RuleStrings[AName: string]: string read GetRuleString write SetRuleString; default;
    property Rules: IReadonlyList<TRule> read GetRules;

    function FindRule(AName: string): TRule;

  end;

  ISyntaxTree = interface
    function GetParent: ISyntaxTree;
    function GetRule: TEBNF.TRule;
    function GetNodes: IList<ISyntaxTree>;

    property Parent: ISyntaxTree read GetParent;
    property Rule: TEBNF.TRule read GetRule;
    property Nodes: IList<ISyntaxTree> read GetNodes;

  end;

  TSyntaxTree = class(TInterfacedObject, ISyntaxTree)
  private
    [Weak]
    FParent: ISyntaxTree;
    FRule: TEBNF.TRule;
    FNodes: IList<ISyntaxTree>;

    // ISyntaxTree
    function GetParent: ISyntaxTree;
    function GetRule: TEBNF.TRule;
    function GetNodes: IList<ISyntaxTree>;

  public
    constructor Create(ARule: TEBNF.TRule; AParent: ISyntaxTree = nil);

    // ISyntaxTree
    property Parent: ISyntaxTree read GetParent;
    property Rule: TEBNF.TRule read GetRule;
    property Nodes: IList<ISyntaxTree> read GetNodes;

  end;

implementation

{ TEBNF }

function TEBNF.AddRule(AName: string; AExpression: TExpression): TRule;
begin
  Result := TRule.Create(Self, AName, AExpression);
  FRules.Add(Result);
end;

constructor TEBNF.Create;
begin
  FRules := TObjectList<TRule>.Create;
end;

function TEBNF.ExpressionParser: TExpression.IParser;
begin
  Result := TExpression.TParser.Create(Self);
end;

function TEBNF.FindRule(AName: string): TRule;
begin
  for Result in FRules do
    if Result.Name = AName then
      Exit;
  raise ELexingRuleError.CreateFmt('Unknown rule "%s".', [AName]);
end;

function TEBNF.GetRules: IReadonlyList<TRule>;
begin
  Result := FRules.ReadonlyList;
end;

function TEBNF.GetRuleString(AName: string): string;
begin
  Result := FindRule(AName).ExpressionString;
end;

function TEBNF.ParseExpression(AText: string): TExpression;
begin
  Result := ExpressionParser.Optional(AText);
  if Result = nil then
    raise ELexingRuleError.Create('Invalid EBNF-Rule.');
end;

procedure TEBNF.SetRuleString(AName: string; const Value: string);
begin
  FRules.Add(TRule.Create(Self, AName, Value));
end;

{ TEBNF.TRule }

function TEBNF.TRule.Analyze(ALexer: TLexer): Boolean;
begin
  if not ALexer.AdvancedSinceRule(Self) then
    Exit(False);
  // Writeln(Name);
  ALexer.BeginRule(Self);
  Result := Expression.Analyze(ALexer);
  ALexer.EndRule(Result);
end;

constructor TEBNF.TRule.Create(AEBNF: TEBNF; AName: string; AExpression: TExpression);
begin
  FEBNF := AEBNF;
  FName := AName;
  FExpression := AExpression;
end;

function TEBNF.TRule.ToString: string;
begin
  Result := Name;
end;

{ TEBNF.TRule }

constructor TEBNF.TRule.Create(AEBNF: TEBNF; AName, AExpressionString: string);
begin
  FEBNF := AEBNF;
  FName := AName;
  FExpressionString := AExpressionString;
end;

function TEBNF.TRule.Lex(AText: string): ISyntaxTree;
begin
  Result := Lex<TLexer>(AText);
end;

function TEBNF.TRule.Lex<T>(AText: string): ISyntaxTree;
var
  Lexer: T;
begin
  Lexer := T.Create(Self, AText);
  try
    Result := Lexer.SyntaxTree;
  finally
    Lexer.Free;
  end;
end;

class function TEBNF.TRule.Parser(AEBNF: TEBNF): TRule.IParser;
begin
  Result := TParser.Create(AEBNF);
end;

function TEBNF.TRule.GetExpression: TExpression;
begin
  if FExpression = nil then
    FExpression := EBNF.ParseExpression(FExpressionString);
  Result := FExpression;
end;

function TEBNF.TRule.GetExpressionString: string;
begin
  Result := Expression.ToString;
end;

{ TEBNF.TLexer }

constructor TEBNF.TLexer.Create(ARule: TRule; AText: string);
begin
  FSyntaxTree := TSyntaxTree.Create(ARule);
  FCurrentSyntaxTree := SyntaxTree;
  FText := AText;
  FPosition := 1;
  FSavedPositions := TStack<Integer>.Create;
  FRulePositions := TList<Integer>.Create;
  if not ARule.Expression.Analyze(Self) then
    raise ENotImplemented.Create('No match!');
  if Position <> Length(Text) + 1 then
    raise ENotImplemented.Create('Trailing data! ' + Text.Substring(Position - 1));
end;

function TEBNF.TLexer.AdvancedSinceRule(ARule: TRule): Boolean;
begin
  Result := Position > LastRulePosition(ARule);
end;

procedure TEBNF.TLexer.BeginRule(ARule: TRule);
var
  OldSyntaxTree: ISyntaxTree;
begin
  OldSyntaxTree := FCurrentSyntaxTree;
  FCurrentSyntaxTree := TSyntaxTree.Create(ARule, FCurrentSyntaxTree);
  OldSyntaxTree.Nodes.Add(FCurrentSyntaxTree);
  FRulePositions.Add(Position);
end;

procedure TEBNF.TLexer.EndRule(AKeep: Boolean);
var
  NewSyntaxTree: ISyntaxTree;
begin
  FRulePositions.RemoveAt(FRulePositions.MaxIndex);
  NewSyntaxTree := FCurrentSyntaxTree;
  FCurrentSyntaxTree := FCurrentSyntaxTree.Parent;
  if not AKeep then
    FCurrentSyntaxTree.Nodes.Remove(NewSyntaxTree);
end;

function TEBNF.TLexer.LastRulePosition(ARule: TRule): Integer;
var
  I: Integer;
  CheckedSyntaxTree: ISyntaxTree;
begin
  CheckedSyntaxTree := FCurrentSyntaxTree;
  for I := FRulePositions.MaxIndex downto 0 do
  begin
    if CheckedSyntaxTree.Rule = ARule then
      Exit(FRulePositions[I]);
    CheckedSyntaxTree := CheckedSyntaxTree.Parent;
  end;
  Exit(0);
end;

procedure TEBNF.TLexer.Save;
begin
  FSavedPositions.Push(FPosition);
end;

procedure TEBNF.TLexer.SkipWhitespace;
begin
  while not ReachedEnd and Text[Position].IsWhiteSpace do
    Inc(FPosition);
end;

procedure TEBNF.TLexer.DiscardSave;
begin
  FSavedPositions.Pop;
end;

function TEBNF.TLexer.ReachedEnd: Boolean;
begin
  Result := Position = Length(Text) + 1;
end;

procedure TEBNF.TLexer.Revert;
begin
  FPosition := FSavedPositions.Pop;
end;

procedure TEBNF.TLexer.Advance(AAmount: Integer);
begin
  Inc(FPosition, AAmount);
end;

function TEBNF.TLexer.StartsWith(AText: string; AAdvanceOnMatch: Boolean): Boolean;
var
  Len, I: Integer;
begin
  Len := Length(AText);
  if Position + Len > Length(Text) + 1 then
    Exit(False);
  for I := 1 to Len do
    if Text[Position + I - 1] <> AText[I] then
      Exit(False);
  Result := True;
  if AAdvanceOnMatch then
    Advance(Len);
end;

{ TEBNF.TVariadicExpression }

function TEBNF.TVariadicExpression.GetExpressions: IReadonlyList<TExpression>;
begin
  Result := FExpressions.ReadonlyList;
end;

constructor TEBNF.TVariadicExpression.Create(AExpressions: IIterable<TExpression>);
begin
  FExpressions := TObjectList<TExpression>.Create(AExpressions);
end;

constructor TEBNF.TVariadicExpression.Create(AExpressions: TArray<TExpression>);
begin
  FExpressions := TObjectList<TExpression>.Create(AExpressions);
end;

{ TEBNF.TUnaryExpression }

constructor TEBNF.TUnaryExpression.Create(AExpression: TExpression);
begin
  FExpression := AExpression;
end;

class function TEBNF.TUnaryExpression.Parser(AEBNF: TEBNF): IParser;
begin
  Result := TParser.Create(AEBNF);
end;

{ TEBNF.TTerminal }

function TEBNF.TTerminal.Analyze(ALexer: TLexer): Boolean;
begin
  ALexer.SkipWhitespace;
  if ALexer.ReachedEnd then
    Exit(False);
  Result := ALexer.StartsWith(Terminal);
end;

constructor TEBNF.TTerminal.Create(ATerminal: string);
begin
  FTerminal := ATerminal;
end;

class function TEBNF.TTerminal.Parser(AEBNF: TEBNF): TTerminal.IParser;
begin
  Result := TParser.Create(AEBNF);
end;

function TEBNF.TTerminal.ToString: string;
begin
  Result := '"' + Terminal + '"';
end;

{ TEBNF.TConcatenation }

function TEBNF.TConcatenation.Analyze(ALexer: TLexer): Boolean;
var
  Expression: TExpression;
begin
  for Expression in Expressions do
    if not Expression.Analyze(ALexer) then
      Exit(False);
  Result := True;
end;

function TEBNF.TConcatenation.ToString: string;
begin
  Result := Expressions.Iterate.Generic.Map<string>(
    function(E: TExpression): string
    begin
      Result := E.ToString;
      if E is TAlternation then
        Result := '(' + Result + ')';
    end
    ).Reduce(
    function(A, B: string): string
    begin
      // Result := A + ', ' + B;
      Result := A + ' ' + B;
    end
    );
end;

{ TEBNF.TAlternation }

function TEBNF.TAlternation.Analyze(ALexer: TLexer): Boolean;
var
  Expression: TExpression;
begin
  for Expression in Expressions do
  begin
    ALexer.Save;
    if Expression.Analyze(ALexer) then
    begin
      ALexer.DiscardSave;
      Exit(True);
    end;
    ALexer.Revert;
  end;
  Result := False;
end;

function TEBNF.TAlternation.ToString: string;
begin
  Result := Expressions.Iterate.Generic.Map<string>(
    function(E: TExpression): string
    begin
      Result := E.ToString;
    end
    ).Reduce(
    function(A, B: string): string
    begin
      Result := A + ' | ' + B;
    end
    );
end;

{ TEBNF.TOptional }

function TEBNF.TOptional.Analyze(ALexer: TLexer): Boolean;
begin
  if ALexer.ReachedEnd then
    Exit(True);
  ALexer.Save;
  Result := Expression.Analyze(ALexer);
  if Result then
    ALexer.DiscardSave
  else
    ALexer.Revert;
end;

class function TEBNF.TOptional.Parser(AEBNF: TEBNF): TOptional.IParser;
begin
  Result := TParser.Create(AEBNF);
end;

function TEBNF.TOptional.ToString: string;
begin
  Result := '[' + Expression.ToString + ']';
end;

{ TEBNF.TRepetition }

function TEBNF.TRepetition.Analyze(ALexer: TLexer): Boolean;
begin
  if ALexer.ReachedEnd then
    Exit(True);
  repeat
    ALexer.Save;
    Result := Expression.Analyze(ALexer);
    if Result then
      ALexer.DiscardSave
    else
      ALexer.Revert;
  until not Result;
  Result := True;
end;

class function TEBNF.TRepetition.Parser(AEBNF: TEBNF): TRepetition.IParser;
begin
  Result := TParser.Create(AEBNF);
end;

function TEBNF.TRepetition.ToString: string;
begin
  Result := '{' + Expression.ToString + '}';
end;

{ TSyntaxTree }

function TSyntaxTree.GetRule: TEBNF.TRule;
begin
  Result := FRule;
end;

function TSyntaxTree.GetNodes: IList<ISyntaxTree>;
begin
  Result := FNodes;
end;

function TSyntaxTree.GetParent: ISyntaxTree;
begin
  Result := FParent;
end;

constructor TSyntaxTree.Create(ARule: TEBNF.TRule; AParent: ISyntaxTree);
begin
  FRule := ARule;
  FParent := AParent;
  FNodes := TList<ISyntaxTree>.Create;
end;

{ TEBNF.TEBNFParser<T> }

function TEBNF.TEBNFParser<T>.GetEBNF: TEBNF;
begin
  Result := FEBNF;
end;

constructor TEBNF.TEBNFParser<T>.Create(AEBNF: TEBNF);
begin
  inherited Create;
  FEBNF := AEBNF;
end;

{ TEBNF.TExpression.TParser }

function TEBNF.TExpression.TParser.Parse: Boolean;

  function ParseExpression(var AExpression: TExpression): Boolean;
  begin
    SkipWhitespace;
    case First of
      '{':
        AExpression := TRepetition.Parser(EBNF).Require(Info);
      '[':
        AExpression := TOptional.Parser(EBNF).Require(Info);
      '(':
        AExpression := GroupParser(EBNF).Require(Info);
      '"':
        AExpression := TTerminal.Parser(EBNF).Require(Info);
      'a' .. 'z', 'A' .. 'Z', '_':
        AExpression := TRule.Parser(EBNF).Require(Info);
    else
      Exit(False);
    end;
    Result := True;
  end;

  function ParseConcatenation(var AExpression: TExpression): Boolean;
  var
    Expressions: IList<TExpression>;
  begin
    Result := ParseExpression(AExpression);
    if not Result then
      Exit;             
    Expressions := TList<TExpression>.Create;
    repeat
      Expressions.Add(AExpression);
      SkipWhitespace;
      StartsWith(','); // skip
    until not ParseExpression(AExpression);
    if Expressions.Count = 1 then
      AExpression := Expressions.First
    else
      AExpression := TConcatenation.Create(Expressions);
  end;

  function ParseAlternation(var AExpression: TExpression): Boolean;
  var
    Expressions: IList<TExpression>;
  begin
    Result := ParseConcatenation(AExpression);
    if not Result then
      Exit;
    SkipWhitespace;
    if not StartsWith('|') then
      Exit;
    Expressions := TList<TExpression>.Create;
    Expressions.Add(AExpression);
    repeat
      if not ParseConcatenation(AExpression) then
        raise EParseError.Create('Expected next alternation option.');
      Expressions.Add(AExpression);
      SkipWhitespace;
    until not StartsWith('|');
    AExpression := TAlternation.Create(Expressions)
  end;

var
  Expression: TExpression;
begin
  Result := ParseAlternation(Expression);
  if Result then
    ParseResult := Expression;
end;

class function TEBNF.TExpression.TParser.GetResultName: string;
begin
  Result := 'EBNF-Expression';
end;

{ TEBNF.TTerminal.TParser }

function TEBNF.TTerminal.TParser.Parse: Boolean;
var
  Text: string;
begin
  Result := StartsWith('"');
  if not Result then
    Exit;
  Text := '';
  while not StartsWith('"') do
  begin
    if First = '\' then
      Advance;
    Text := Text + First;
    Advance;
  end;
  ParseResult := TTerminal.Create(Text);
end;

class function TEBNF.TTerminal.TParser.GetResultName: string;
begin
  Result := 'EBNF-Terminal';
end;

{ TEBNF.TOptional.TParser }

function TEBNF.TOptional.TParser.Parse: Boolean;
begin
  Result := StartsWith('[');
  SkipWhitespace;
  ParseResult := TOptional.Create(TExpression.Parser(EBNF).Require(Info));
  SkipWhitespace;
  if not StartsWith(']') then
    raise EParseError.Create('Expected closing bracket "]".');
end;

class function TEBNF.TOptional.TParser.GetResultName: string;
begin
  Result := 'EBNF-Optional';
end;

{ TEBNF.TRepetition.TParser }

function TEBNF.TRepetition.TParser.Parse: Boolean;
begin
  Result := StartsWith('{');
  SkipWhitespace;
  ParseResult := TRepetition.Create(TExpression.Parser(EBNF).Require(Info));
  SkipWhitespace;
  if not StartsWith('}') then
    raise EParseError.Create('Expected closing bracket "]".');
end;

class function TEBNF.TRepetition.TParser.GetResultName: string;
begin
  Result := 'EBNF-Repetition';
end;

{ TEBNF.TRule.TParser }

function TEBNF.TRule.TParser.Parse: Boolean;
var
  Identifier: string;
begin
  Identifier := ReadWhile(['a' .. 'z', 'A' .. 'Z', '_', '0' .. '9']);
  Result := Identifier.Length <> 0;
  if Result then
    ParseResult := EBNF.FindRule(Identifier);
end;

class function TEBNF.TRule.TParser.GetResultName: string;
begin
  Result := 'EBNF-Rule';
end;

{ TEBNF.TExpression.TGroupParser }

function TEBNF.TExpression.TGroupParser.Parse: Boolean;
begin
  Result := StartsWith('(');
  SkipWhitespace;
  ParseResult := TExpression.Parser(EBNF).Require(Info);
  SkipWhitespace;
  if not StartsWith(')') then
    raise EParseError.Create('Expected closing bracket "]".');
end;

class function TEBNF.TExpression.TGroupParser.GetResultName: string;
begin
  Result := 'EBNF-Group';
end;

{ TEBNF.TExpression }

class function TEBNF.TExpression.GroupParser(AEBNF: TEBNF): TExpression.IParser;
begin
  Result := TGroupParser.Create(AEBNF);
end;

class function TEBNF.TExpression.Parser(AEBNF: TEBNF): TExpression.IParser;
begin
  Result := TParser.Create(AEBNF);
end;

end.
