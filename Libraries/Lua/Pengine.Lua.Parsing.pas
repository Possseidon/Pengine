unit Pengine.Lua.Parsing;

interface

uses
  Pengine.ICollections,
  Pengine.Parsing;

type

  // All class definitions strongly mirror the official EBNF on the Lua Reference Manual page
  // It can be found here: https://www.lua.org/manual/5.3/manual.html#9

  (* --- The Complete Syntax of Lua ---

    [X] TLuaChunk
    chunk ::= block

    [X] TLuaBlock
    block ::= {stat} [retstat]

    [X] TLuaStatement
    stat ::=
    [X] TLuaEmptyStatement
    >        ';' |
    [X] TLuaAssignmentStatement
    >        varlist '=' explist |
    [X] TLuaCallStatement
    >        functioncall |
    [X] TLuaLabelStatement
    >        label |
    [X] TLuaBreakStatement
    >        break |
    [X] TLuaGotoStatement
    >        goto Name |
    [X] TLuaDoStatement
    >        do block end |
    [X] TLuaWhileStatement
    >        while exp do block end |
    [X] TLuaRepeatStatement
    >        repeat block until exp |
    [X] TLuaIfStatement
    >        if exp then block {elseif exp then block} [else block] end |
    [X] TLuaForStatement
    [X] TLuaNumericForStatement
    >        for Name '=' exp ',' exp [',' exp] do block end |
    [X] TLuaGenericForStatement
    >        for namelist in explist do block end |
    [X] TLuaFunctionStatement
    >        function funcname funcbody |
    [X] TLuaLocalFunctionStatement
    >        local function Name funcbody |
    [X] TLuaLocalAssignmentStatement
    >        local namelist ['=' explist]

    [X] TLuaReturnStatement
    retstat ::= return [explist] [';']

    [X] TLuaLabelStatement
    label ::= '::' Name '::'

    [X] TLuaFunctionName
    funcname ::= Name {'.' Name} [':' Name]

    [X] TLuaVariableList
    varlist ::= var {',' var}

    [X] TLuaVariableExpression
    var ::=
    [X] TLuaVariableNameExpression
    >       Name |
    [X] TLuaVariableIndexExpression
    >       prefixexp '[' exp ']' |
    [X] TLuaVariableNameIndexExpression
    >       prefixexp '.' Name

    [X] TLuaNameList
    namelist ::= Name {',' Name}

    [X] TLuaExpressionList
    explist ::= exp {',' exp}

    [X] TLuaExpression
    exp ::=
    [X] TLuaNilExpression
    >       nil |
    [X] TLuaBooleanExpression
    >       false | true |
    [X] TLuaNumeralExpression
    >       Numeral |
    [X] TLuaLiteralStringExpression
    >       LiteralString |
    [X] TLuaEllipsisExpression
    >       '...' |
    [X] TLuaFunctionDefinitionExpression
    >       functiondef |
    [X] TLuaPrefixExpression
    >       prefixexp |
    [X] TLuaTableConstructorExpression
    >       tableconstructor |
    [X] TLuaBinaryOperationExpression
    >       exp binop exp |
    [X] TLuaUnaryOperationExpression
    >       unop exp

    [X] TLuaPrefixExpression
    prefixexp ::=
    [X] TLuaVariableExpression
    >             var |
    [X] TLuaCallExpression
    >             functioncall |
    [X] TLuaParanthesesExpression
    >             '(' exp ')'

    [X] TLuaCallExpression
    functioncall ::= prefixexp args |
    [X] TLuaSelfCallExpression
    >                prefixexp ':' Name args

    [X] TLuaArguments
    args ::=
    [X] TLuaParameterArguments
    >        '(' [explist] ')' |
    [X] TLuaTableArgument
    >        tableconstructor |
    [X] TLuaStringArgument
    >        LiteralString

    [X] TLuaFunctionDefinitionExpression
    functiondef ::= function funcbody

    [X] TLuaFunctionBody
    funcbody ::= '(' [parlist] ')' block end

    [X] TLuaParameterList
    parlist ::= namelist [',' '...'] | '...'

    [X] TLuaTableConstructorExpression
    tableconstructor ::= '{' [fieldlist] '}'

    [X] TLuaFieldList
    fieldlist ::= field {fieldsep field} [fieldsep]

    [X] TLuaField
    field ::=
    [X] TLuaKeyValueField
    >         '[' exp ']' '=' exp |
    [X] TLuaNamedField
    >         Name '=' exp |
    [X] (TLuaField)
    >         exp

    [X] TLuaFieldSeparator -> enum
    fieldsep ::= ',' | ';'

    [X] TLuaBinaryOp -> enum
    binop ::= '+' | '-' | '*' | '/' | '//' | '^' | '%' |
    >         '&' | '~' | '|' | '>>' | '<<' | '..' |
    >         '<' | '<=' | '>' | '>=' | '==' | '~=' |
    >         and | or

    [X] TLuaUnaryOp -> enum
    unop ::= '-' | not | '#' | '~'

    --- The Complete Syntax of Lua --- *)

  // --- Forward declarations ---

  TLuaBlock = class;
  TLuaExpression = class;
  TLuaTableConstructorExpression = class;
  TLuaLiteralStringExpression = class;

  // --- Helpers ---

  TLuaBinaryOp = (
    lbAdd,
    lbSubtract,
    lbMultiply,
    lbDivide,
    lbIntDivide,
    lbPower,
    lbModulus,
    lbBinaryAnd,
    lbBinaryXor,
    lbBinaryOr,
    lbShiftRight,
    lbShiftLeft,
    lbConcat,
    lbLessThan,
    lbLessOrEqual,
    lbGreater,
    lbGreaterOrEqual,
    lbEqual,
    lbNotEqual,
    lbAnd,
    lbOr
    );

  TLuaUnaryOp = (
    luMinus,
    luNot,
    luLength,
    luBinaryNot
    );

  TLuaFieldSeparator = (
    fsColon,
    fsSemicolon
    );

  /// <summary>A simple identifier, used mainly for variable names.</summary>
  /// <remarks>
  /// Quote on the Lua Reference Manual:<p/>
  /// Can be any string of letters, digits, and underscores, not beginning with a digit and not being a reserved word.
  /// </remarks>
  TLuaName = record
  private
    FName: string;

    // TODO: Implicit conversion to string

  end;

  TLuaFunctionName = class
  private
    FNames: IList<TLuaName>; // at least one name required
    FSelfCall: Boolean; // only valid for at least two names

  end;

  /// <summary>A list of names.</summary>
  TLuaNameList = class
  private
    // TODO: For parsing, in some cases this cannot be an empty list
    FNames: IList<TLuaName>;

  end;

  /// <summary>A list of named parameters and an optional ellipsis at the end.</summary>
  TLuaParameterList = class
  private
    FNameList: TLuaNameList;
    FEllipsis: Boolean;

  end;

  /// <summary>Consists of a parameterlist and the actual function body.</summary>
  TLuaFunctionBody = class
  private
    FParameters: TLuaParameterList;
    FBlock: TLuaBlock;

  end;

  TLuaField = class
  private
    FValue: TLuaExpression;
    FFieldSeparator: TLuaFieldSeparator;

  end;

  TLuaKeyValueField = class(TLuaField)
  private
    FKey: TLuaExpression;

  end;

  TLuaNamedField = class(TLuaField)
  private
    FName: TLuaName;

  end;

  TLuaFieldList = class
  private
    FFields: IObjectList<TLuaField>;
    FTrailingFieldSeparator: Boolean;

  end;

  TLuaArguments = class abstract
  end;

  TLuaParameterArguments = class(TLuaArguments)
  private
    FParameterList: TLuaParameterList;

  end;

  TLuaTableArgument = class(TLuaArguments)
  private
    FTable: TLuaTableConstructorExpression;

  end;

  TLuaStringArgument = class(TLuaArguments)
  private
    FArgument: TLuaLiteralStringExpression;

  end;

  TLuaConditionalBlock = class
  private
    FCondition: TLuaExpression;
    FBlock: TLuaBlock;

  end;

  // --- Expressions ---

  /// <summary>An expression, which evaluates to a value.</summary>
  TLuaExpression = class

  end;

  /// <summary>
  /// The nil expression.
  /// <code> nil</code>
  /// </summary>
  TLuaNilExpression = class(TLuaExpression)

  end;

  /// <summary>
  /// Boolean expression true and false.
  /// <code> true<p/> false</code>
  /// </summary>
  TLuaBooleanExpression = class(TLuaExpression)
  private
    FValue: Boolean;

  end;

  /// <summary>
  /// A numeral expression supporting various number formats.
  /// <code> 1, 1.0, 1.5e3, 0xFF, 0x2.8</code>
  /// </summary>
  TLuaNumeralExpression = class(TLuaExpression)
  private
    FNumeral: string;

  end;

  /// <summary>
  /// A literal string, supporting single and double quotes and a special multiline syntax.
  /// <code> 'single quoted'<p/> "double quoted"<p/> [[ multiline string ]]</code>
  /// </summary>
  TLuaLiteralStringExpression = class(TLuaExpression)
  private
    FLiteral: string;

  end;

  /// <summary>
  /// The ellipsis expression, used for functions with variadic arguments.</summary>
  /// <code> TODO</code>
  /// </summary>
  TLuaEllipsisExpression = class(TLuaExpression)

  end;

  /// <summary>
  /// An anonymous function definition.
  /// <code> function(param) end</code>
  /// </summary>
  TLuaFunctionDefinitionExpression = class(TLuaExpression)
  private
    FFunctionBody: TLuaFunctionBody;

  end;

  /// <summary>
  /// Either of the following:<p/>
  /// <code>
  /// - variable with table access using dot and bracket notation.
  /// - function call<p/>
  /// - parantheses enclosed expression<p/>
  /// </code>
  /// </summary>
  TLuaPrefixExpression = class abstract(TLuaExpression)
  end;

  /// <summary>
  /// A variable with possible table access using dot and bracket notation.
  /// </summary>
  TLuaVariableExpression = class abstract(TLuaPrefixExpression)
  end;

  TLuaVariableNameExpression = class(TLuaVariableExpression)
  private
    FName: TLuaName;

  end;

  TLuaVariableIndexExpression = class(TLuaVariableExpression)
  private
    FExpression: TLuaPrefixExpression;
    FIndex: TLuaExpression;

  end;

  TLuaVariableNameIndexExpression = class(TLuaVariableExpression)
  private
    FExpression: TLuaPrefixExpression;
    FName: TLuaName;

  end;

  TLuaCallExpression = class(TLuaPrefixExpression)
  private
    FExpression: TLuaPrefixExpression;
    FArguments: TLuaArguments;

  end;

  TLuaSelfCallExpression = class(TLuaCallExpression)
  private
    FName: TLuaName;

  end;

  TLuaParanthesesExpression = class(TLuaPrefixExpression)
  private
    FExpression: TLuaExpression;

  end;

  /// <summary>
  /// A table constructor, defining ordered array values or key-value pairs.
  /// <code> { 1, 2, 3, a = 42, ["key"] = "value" }</code>
  /// </summary>
  TLuaTableConstructorExpression = class(TLuaExpression)
  private
    FFieldList: TLuaFieldList;

  end;

  /// <summary>
  /// A binary operation.
  /// <code> value + 42</code>
  /// </summary>
  TLuaBinaryOperationExpression = class(TLuaExpression)
  private
    FOperator: TLuaBinaryOp;
    FLeft: TLuaExpression;
    FRight: TLuaExpression;

  end;

  /// <summary>
  /// A unary operation.
  /// <code> #list</code>
  /// </summary>
  TLuaUnaryOperationExpression = class(TLuaExpression)
  private
    FOperator: TLuaUnaryOp;
    FExpression: TLuaExpression;

  end;

  TLuaExpressionList = class
  private
    FExpressions: IObjectList<TLuaExpression>;

  end;

  TLuaVariableList = class
  private
    FVariables: IObjectList<TLuaVariableExpression>;

  end;

  // --- Statements ---

  /// <summary>A statement can be executed and discards results if any.</summary>
  TLuaStatement = class

  end;

  /// <summary>
  /// An empty statement, consisting of a single semicolon.
  /// <code> ;</code>
  /// </summary>
  TLuaEmptyStatement = class(TLuaStatement)

  end;

  /// <summary>
  /// An assignment statement, assigning one or multiple values.
  /// <code> a, b, c = x, y, z</code>
  /// </summary>
  TLuaAssignmentStatement = class(TLuaStatement)
  private
    FExpressionList: TLuaExpressionList;
    FVariableList: TLuaVariableList;

  end;

  /// <summary>
  /// A function call used as a statement, meaning its result gets discarded.
  /// <code> table.insert(list, 42)</code>
  /// </summary>
  TLuaCallStatement = class(TLuaStatement)
  private
    FCall: TLuaCallExpression;

  end;

  /// <summary>
  /// A label, which can be used by a goto.
  /// <code> ::mylabel::</code>
  /// </summary>
  TLuaLabelStatement = class(TLuaStatement)
  private
    FName: TLuaName;

  end;

  /// <summary>
  /// A break statement, allowing you to immediately jump out of a loop.
  /// <code> break</code>
  /// </summary>
  TLuaBreakStatement = class(TLuaStatement)

  end;

  /// <summary>
  /// A goto statement, allowing you to jump to a label.
  /// <code> goto mylabel</code>
  /// </summary>
  TLuaGotoStatement = class(TLuaStatement)
  private
    FLabelName: TLuaName;

  end;

  /// <summary>
  /// A do-end block, simply limiting the scope without altering the control-flow.
  /// <code> do<p/>   ...<p/> end</code>
  /// </summary>
  TLuaDoStatement = class(TLuaStatement)
  private
    FBlock: TLuaBlock;

  end;

  /// <summary>
  /// A while-loop, repeating its content as long, as a certain condition is met.
  /// <code> while &lt;condition&gt; do<p/>   ...<p/> end</code>
  /// </summary>
  TLuaWhileStatement = class(TLuaStatement)
  private
    FCondition: TLuaExpression;
    FBlock: TLuaBlock;

  end;

  /// <summary>
  /// A repeat-until-loop, executing its content once and then repeating it until a certain condition is met.
  /// <code> repeat<p/>   ...<p/> until &lt;condition&gt;</code>
  /// </summary>
  TLuaRepeatStatement = class(TLuaStatement)
  private
    FBlock: TLuaBlock;
    FCondition: TLuaExpression;

  end;

  /// <summary>
  /// An if-statement, only executing its content if the condition is met. Supports optional elseif and else parts.
  /// <code>
  /// if &lt;condition&gt; then<p/>
  /// ...<p/>
  /// elseif &lt;condition&gt; then<p/>
  /// ...<p/>
  /// else<p/>
  /// ...<p/>
  /// end<p/>
  /// </code>
  /// </summary>
  TLuaIfStatement = class(TLuaStatement)
  private
    FBlocks: IObjectList<TLuaConditionalBlock>;
    FElseBlock: TLuaBlock;

  end;

  TLuaForStatement = class(TLuaStatement)
  private
    FBlock: TLuaBlock;

  end;

  /// <summary>
  /// A for statement using a numeric start, stop and optional step value.
  /// <code> for i = start, stop[, step] do<p/>   ...<p/> end</code>
  /// </summary>
  TLuaNumericForStatement = class(TLuaForStatement)
  private
    FVariableName: TLuaName;
    FStart: TLuaExpression;
    FEnd: TLuaExpression;
    FStep: TLuaExpression;

  end;

  /// <summary>
  /// A generic for-in statement, used for example with pairs or ipairs.
  /// <code> for k, v in pairs(t) do<p/>   ...<p/> end</code>
  /// </summary>
  TLuaGenericForStatement = class(TLuaForStatement)
  private
    FNameList: TLuaNameList;
    FExpressionList: TLuaExpressionList;

  end;

  /// <summary>
  /// A function-definition, which is syntactic sugar for assigning a function to a variable.
  /// <code> function myfunction(a, b, c)<p/>   ...<p/> end</code>
  /// </summary>
  TLuaFunctionStatement = class(TLuaStatement)
  private
    FName: TLuaFunctionName;
    FBody: TLuaFunctionBody;

  end;

  /// <summary>
  /// A local function-definition, which is syntactic sugar for assigning a function to a local variable.
  /// <code> function myfunction(a, b, c)<p/>   ...<p/> end</code>
  /// </summary>
  TLuaLocalFunctionStatement = class(TLuaStatement)
  private
    FName: TLuaName;
    FBody: TLuaFunctionBody;

  end;

  /// <summary>
  /// Defines (or redefines) local variables for the current scope.
  /// <code> local a<p/> local x, y, z = 1, 2, 3</code>
  /// </summary>
  TLuaLocalAssignmentStatement = class(TLuaStatement)
  private
    FNameList: TLuaNameList;
    FExpressionList: TLuaExpressionList;

  end;

  TLuaReturnStatement = class
  private
    FResults: TLuaExpressionList;
    FTrailingSemicolon: Boolean; // TODO: Don't forget me! <3

  end;

  /// <summary>A list of statements with an optional return statement at the end.</summary>
  TLuaBlock = class
  public type

    IParser = IObjectParser<TLuaBlock>;

    TParser = class(TObjectParser<TLuaBlock>, IParser)
    protected
      // TODO: function Parse: Boolean; override;

    end;

  private
    FStatements: IObjectList<TLuaStatement>;
    FReturnStatement: TLuaReturnStatement;

    function GetStatements: IReadonlyList<TLuaStatement>;

  public
    constructor Create;

    property Statements: IReadonlyList<TLuaStatement> read GetStatements;

    function HasReturnStatement: Boolean;
    property ReturnStatement: TLuaReturnStatement read FReturnStatement;

  end;

  TLuaChunk = TLuaBlock;

implementation

{ TLuaBlock }

constructor TLuaBlock.Create;
begin
  FStatements := TObjectList<TLuaStatement>.Create;
end;

function TLuaBlock.GetStatements: IReadonlyList<TLuaStatement>;
begin
  Result := FStatements.ReadonlyList;
end;

function TLuaBlock.HasReturnStatement: Boolean;
begin
  Result := FReturnStatement <> nil;
end;

end.
