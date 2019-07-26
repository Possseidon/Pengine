unit Pengine.Lexing;

interface

uses
  System.SysUtils,

  Pengine.ICollections;

type

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

    TVariadicExpression = class(TExpression)
    private
      FExpressions: IList<TExpression>;

      function GetExpressions: IReadonlyList<TExpression>;

    public
      constructor Create(AExpressions: IIterable<TExpression>); overload;
      constructor Create(AExpressions: TArray<TExpression>); overload;

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

    TRule = class(TUnaryExpression)
    private
      FName: string;

    protected
      function Analyze(ALexer: TLexer): Boolean; override;

    public
      constructor Create(AName: string; AExpression: TExpression);

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

    public
      constructor Create(ARule: TRule; AText: string); virtual;

      property SyntaxTree: ISyntaxTree read FSyntaxTree;

      property Text: string read FText;
      property Position: Integer read FPosition;

      function Analyze(AExpression: TExpression): Boolean;

      procedure Save;
      procedure DiscardSave;
      procedure Revert;

      procedure Advance(AAmount: Integer);
      function StartsWith(AText: string; AAdvanceOnMatch: Boolean = True): Boolean;

    end;

    TBasicLexer = class(TLexer)

    end;

  private
    FRules: IList<TRule>;

    procedure SetRule(AName: string; const Value: string); overload;

  public
    constructor Create;

    function AddRule(AName: string; AExpression: TExpression): TRule;

    property Rules[AName: string]: string write SetRule; default;

  end;

  ISyntaxTree = interface
    function GetRule: TEBNF.TRule;
    function GetNodes: IList<ISyntaxTree>;

    property Rule: TEBNF.TRule read GetRule;
    property Nodes: IList<ISyntaxTree> read GetNodes;

  end;

  TSyntaxTree = class(TInterfacedObject, ISyntaxTree)
  private
    FParent: ISyntaxTree;
    FRule: TEBNF.TRule;
    FNodes: IList<ISyntaxTree>;

    // ISyntaxTree
    function GetRule: TEBNF.TRule;
    function GetNodes: IList<ISyntaxTree>;

  public
    constructor Create(ARule: TEBNF.TRule; AParent: ISyntaxTree = nil);

    // ISyntaxTree
    property Rule: TEBNF.TRule read GetRule;
    property Nodes: IList<ISyntaxTree> read GetNodes;

  end;

implementation

{ TEBNF }

function TEBNF.AddRule(AName: string; AExpression: TExpression): TRule;
begin
  Result := TRule.Create(AName, AExpression);
  FRules.Add(Result);
end;

constructor TEBNF.Create;
begin
  FRules := TObjectList<TRule>.Create;
end;

procedure TEBNF.SetRule(AName: string; const Value: string);
begin
  raise ENotImplemented.Create('EBNF Assignment-Syntax');
  // AddRule(AName, TExpression.Parse(Value));
end;

{ TEBNF.TRule }

function TEBNF.TRule.Analyze(ALexer: TLexer): Boolean;
begin
  Result := ALexer.Analyze(Expression);
end;

constructor TEBNF.TRule.Create(AName: string; AExpression: TExpression);
begin
  FName := AName;
  FExpression := AExpression;
end;

function TEBNF.TRule.ToString: string;
begin
  Result := Name;
end;

{ TEBNF.TRule }

function TEBNF.TRule.Lex(AText: string): ISyntaxTree;
begin
  Result := Lex<TBasicLexer>(AText);
end;

function TEBNF.TRule.Lex<T>(AText: string): ISyntaxTree;
begin
  Result := T.Create(Self, AText).SyntaxTree;
end;

{ TEBNF.TLexer }

procedure TEBNF.TLexer.Advance(AAmount: Integer);
begin
  Inc(FPosition, AAmount);
end;

function TEBNF.TLexer.Analyze(AExpression: TExpression): Boolean;
var
  OldSyntaxTree: ISyntaxTree;
  IsRule: Boolean;
begin
  IsRule := AExpression is TRule;
  if IsRule then
  begin
    OldSyntaxTree := FCurrentSyntaxTree;
    FCurrentSyntaxTree := TSyntaxTree.Create(TRule(AExpression), OldSyntaxTree);
  end;
  Result := AExpression.Analyze(Self);
  if IsRule then
  begin
    if Result then
      OldSyntaxTree.Nodes.Add(FCurrentSyntaxTree);
    FCurrentSyntaxTree := OldSyntaxTree;
  end;
end;

constructor TEBNF.TLexer.Create(ARule: TRule; AText: string);
begin
  FSyntaxTree := TSyntaxTree.Create(ARule);
  FCurrentSyntaxTree := SyntaxTree;
  FText := AText;
  FPosition := 1;
  FSavedPositions := TStack<Integer>.Create;
  if not ARule.Analyze(Self) then
    raise ENotImplemented.Create('No match!');
  if Position <> Length(Text) then
    raise ENotImplemented.Create('Trailing data!');
end;

procedure TEBNF.TLexer.Save;
begin
  FSavedPositions.Push(FPosition);
end;

procedure TEBNF.TLexer.DiscardSave;
begin
  FSavedPositions.Pop;
end;

procedure TEBNF.TLexer.Revert;
begin
  FPosition := FSavedPositions.Pop;
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
    if not ALexer.Analyze(Expression) then
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
    if ALexer.Analyze(Expression) then
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
  ALexer.Save;
  Result := ALexer.Analyze(Expression);
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
  while ALexer.Analyze(Expression) do
      ; // nothing
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

constructor TSyntaxTree.Create(ARule: TEBNF.TRule; AParent: ISyntaxTree);
begin
  FRule := ARule;
  FParent := AParent;
  FNodes := TList<ISyntaxTree>.Create;
end;

end.
