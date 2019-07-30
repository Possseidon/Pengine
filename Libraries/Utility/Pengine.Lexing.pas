unit Pengine.Lexing;

interface

uses
  System.SysUtils,
  System.Character,

  Pengine.ICollections;

type

  ELexingRuleError = class(Exception);

  ISyntaxTree = interface;

  TEBNF = class
  public type

    TLexer = class;

    TExpression = class
    protected
      function Analyze(ALexer: TLexer): Boolean; virtual; abstract;

    end;

    TTerminal = class(TExpression)
    private
      FTerminal: string;

    protected
      function Analyze(ALexer: TLexer): Boolean; override;

    public
      constructor Create(ATerminal: string);

      property Terminal: string read FTerminal;

      function ToString: string; override;

    end;

    TUnaryExpression = class(TExpression)
    private
      FExpression: TExpression;

    public
      constructor Create(AExpression: TExpression);

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
    protected
      function Analyze(ALexer: TLexer): Boolean; override;

    public
      function ToString: string; override;

    end;

    TRepetition = class(TUnaryExpression)
    protected
      function Analyze(ALexer: TLexer): Boolean; override;

    public
      function ToString: string; override;

    end;

    TRule = class(TExpression)
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

      property EBNF: TEBNF read FEBNF;
      property Expression: TExpression read GetExpression;
      property ExpressionString: string read GetExpressionString;
      property Name: string read FName;

      function ToString: string; override;

      function Lex(AText: string): ISyntaxTree; overload;
      function Lex<T: TLexer>(AText: string): ISyntaxTree; overload;

    end;

    /// <summary>Builds an abstract syntax tree from a given EBNF expression.</summary>
    TLexer = class(TInterfacedObject)
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

    function AddRule(AName: string; AExpression: TExpression): TRule;
    property RuleStrings[AName: string]: string read GetRuleString write SetRuleString; default;
    property Rules: IReadonlyList<TRule> read GetRules;

    function FindRule(AName: string): TRule;

    function ParseExpression(AExpressionString: string): TExpression;

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

function TEBNF.ParseExpression(AExpressionString: string): TExpression;
var
  I, TextStart: Integer;
  Expressions: IStack<TExpression>;
  ControlChars: IStack<Char>;
  IsRule, NeedsConcatenation: Boolean;

  function ReachedEnd: Boolean;
  begin
    Result := I = Length(AExpressionString) + 1;
  end;

  function First: Char;
  begin
    Result := AExpressionString[I];
  end;

  procedure SkipWhitespace;
  begin
    while not ReachedEnd and First.IsWhiteSpace do
      Inc(I);
  end;

  procedure CombineExpressionsFor(AChar: Char; AClass: TVariadicExpressionClass);
  var
    Count: Integer;
  begin
    Count := 1;
    while not ControlChars.Empty and (ControlChars.Top = AChar) do
    begin
      ControlChars.Pop;
      Inc(Count);
    end;
    if Count > Expressions.Count then
      raise ELexingRuleError.Create('Expected expression after alternation or concatenation.');
    Expressions.Push(AClass.Create(Expressions.PopMany.Iterate.Take(Count).ToList.Reverse));
  end;

  procedure CombineExpressions;
  begin
    if ControlChars.Top = '|' then
      CombineExpressionsFor('|', TAlternation)
    else if ControlChars.Top = ',' then
      CombineExpressionsFor(',', TConcatenation);
  end;

begin
  I := 1;
  Expressions := TStack<TExpression>.Create;
  ControlChars := TStack<Char>.Create;
  NeedsConcatenation := False;
  while not ReachedEnd do
  begin
    SkipWhitespace;
    if ReachedEnd then
      Break;
    IsRule := False;
    case First of
      '{', '[', '(':
        begin
          if NeedsConcatenation then
            ControlChars.Push(',');
          ControlChars.Push(First);
          NeedsConcatenation := False;
        end;
      '|', ',':
        begin
          ControlChars.Push(First);
          NeedsConcatenation := False;
        end;
      '}':
        begin
          CombineExpressions;
          if ControlChars.Top <> '{' then
            raise ELexingRuleError.CreateFmt('Attempt to close "%s" with "}".', [ControlChars.Top]);
          ControlChars.Pop;
          Expressions.Push(TRepetition.Create(Expressions.Pop));
          NeedsConcatenation := True;
        end;
      ']':
        begin
          CombineExpressions;
          if ControlChars.Top <> '[' then
            raise ELexingRuleError.CreateFmt('Attempt to close "%s" with "]".', [ControlChars.Top]);
          ControlChars.Pop;
          Expressions.Push(TOptional.Create(Expressions.Pop));
          NeedsConcatenation := True;
        end;
      ')':
        begin
          CombineExpressions;
          if ControlChars.Top <> '(' then
            raise ELexingRuleError.CreateFmt('Attempt to close "%s" with ")".', [ControlChars.Top]);
          ControlChars.Pop;
          NeedsConcatenation := True;
        end;
      '"':
        begin
          if NeedsConcatenation then
            ControlChars.Push(',');
          TextStart := I;
          repeat
            Inc(I);
            if ReachedEnd then
              raise ELexingRuleError.Create('Found unterminated terminal.');
          until First = '"';
          Expressions.Push(TTerminal.Create(AExpressionString.Substring(TextStart, I - TextStart - 1)));
          NeedsConcatenation := True;
        end;
    else
      if NeedsConcatenation then
        ControlChars.Push(',');
      IsRule := True;
      TextStart := I - 1;
      while True do
      begin
        Inc(I);
        if ReachedEnd or First.IsWhiteSpace or CharInSet(First, ['{', '}', '[', ']', '(', ')', '"', '|', ',']) then
          Break;
      end;
      Expressions.Push(FindRule(AExpressionString.Substring(TextStart, I - TextStart - 1)));
      NeedsConcatenation := True;
    end;
    if not IsRule then
      Inc(I);
  end;
  while not ControlChars.Empty do
    CombineExpressions;
  //if not ControlChars.Empty then
  //  raise ELexingRuleError.CreateFmt('Unclosed bracket "%s".', [ControlChars.Top]);
  if not Expressions.Count = 1 then
    raise ELexingRuleError.Create('Is this even reachable? If yes, investiage.');
  Result := Expressions.Top;
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
  Writeln(Name);
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
begin
  Result := T.Create(Self, AText).SyntaxTree;
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
  Result := '( ' + Expressions.Iterate.Generic.Map<string>(
    function(E: TExpression): string
    begin
      Result := E.ToString;
    end
    ).Reduce(
    function(A, B: string): string
    begin
      Result := A + ', ' + B;
    end
    ) + ' )';
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
  Result := '( ' + Expressions.Iterate.Generic.Map<string>(
    function(E: TExpression): string
    begin
      Result := E.ToString;
    end
    ).Reduce(
    function(A, B: string): string
    begin
      Result := A + ' | ' + B;
    end
    ) + ' )';
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

function TEBNF.TOptional.ToString: string;
begin
  Result := '[ ' + Expression.ToString + ' ]';
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

function TEBNF.TRepetition.ToString: string;
begin
  Result := '{ ' + Expression.ToString + ' }';
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

end.
