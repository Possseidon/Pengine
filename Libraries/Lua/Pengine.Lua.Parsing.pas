unit Pengine.Lua.Parsing;

interface

uses
  System.SysUtils,

  Pengine.ICollections,
  Pengine.Parsing;

type

  // All class definitions strongly mirror the official EBNF on the Lua Reference Manual page
  // It can be found here: https://www.lua.org/manual/5.3/manual.html#9

  (* --- The Complete Syntax of Lua ---

    --- Stage ---
    [X] Implement Structure
    [-] Parsers
    [ ] Formatters
    [ ] Documentation
    [ ] Sorting and Cleanup

    Progress: 2 / 63
    /---------------------------------------------------------------\
    [||                                                             ]
    \---------------------------------------------------------------/

    EBNF:

    [X] TLuaName
    Name ::= letters, digits, underscores, not started by digit

    [ ] TLuaChunk
    chunk ::= block

    [ ] TLuaBlock
    block ::= {stat} [retstat]

    [ ] TLuaStatement
    stat ::=
    [ ] TLuaEmptyStatement
    >        ';' |
    [ ] TLuaAssignmentStatement
    >        varlist '=' explist |
    [ ] TLuaCallStatement
    >        functioncall |
    [ ] TLuaLabelStatement
    >        label |
    [ ] TLuaBreakStatement
    >        break |
    [ ] TLuaGotoStatement
    >        goto Name |
    [ ] TLuaDoStatement
    >        do block end |
    [ ] TLuaWhileStatement
    >        while exp do block end |
    [ ] TLuaRepeatStatement
    >        repeat block until exp |
    [ ] TLuaIfStatement
    >        if exp then block {elseif exp then block} [else block] end |
    [ ] TLuaForStatement
    [ ] TLuaNumericForStatement
    >        for Name '=' exp ',' exp [',' exp] do block end |
    [ ] TLuaGenericForStatement
    >        for namelist in explist do block end |
    [ ] TLuaFunctionStatement
    >        function funcname funcbody |
    [ ] TLuaLocalFunctionStatement
    >        local function Name funcbody |
    [ ] TLuaLocalAssignmentStatement
    >        local namelist ['=' explist]

    [ ] TLuaReturnStatement
    retstat ::= return [explist] [';']

    [ ] TLuaLabelStatement
    label ::= '::' Name '::'

    [X] TLuaFunctionName
    funcname ::= Name {'.' Name} [':' Name]

    [ ] TLuaVariableList
    varlist ::= var {',' var}

    [ ] TLuaVariableExpression
    var ::=
    [ ] TLuaVariableNameExpression
    >       Name |
    [ ] TLuaVariableIndexExpression
    >       prefixexp '[' exp ']' |
    [ ] TLuaVariableNameIndexExpression
    >       prefixexp '.' Name

    [ ] TLuaNameList
    namelist ::= Name {',' Name}

    [ ] TLuaExpressionList
    explist ::= exp {',' exp}

    [ ] TLuaExpression
    exp ::=
    [ ] TLuaNilExpression
    >       nil |
    [ ] TLuaBooleanExpression
    >       false | true |
    [ ] TLuaNumeralExpression
    >       Numeral |
    [ ] TLuaLiteralStringExpression
    >       LiteralString |
    [ ] TLuaEllipsisExpression
    >       '...' |
    [ ] TLuaFunctionDefinitionExpression
    >       functiondef |
    [ ] TLuaPrefixExpression
    >       prefixexp |
    [ ] TLuaTableConstructorExpression
    >       tableconstructor |
    [ ] TLuaBinaryOperationExpression
    >       exp binop exp |
    [ ] TLuaUnaryOperationExpression
    >       unop exp

    [ ] TLuaPrefixExpression
    prefixexp ::=
    [ ] TLuaVariableExpression
    >             var |
    [ ] TLuaCallExpression
    >             functioncall |
    [ ] TLuaParanthesesExpression
    >             '(' exp ')'

    [ ] TLuaCallExpression
    functioncall ::= prefixexp args |
    [ ] TLuaSelfCallExpression
    >                prefixexp ':' Name args

    [ ] TLuaArguments
    args ::=
    [ ] TLuaParameterArguments
    >        '(' [explist] ')' |
    [ ] TLuaTableArgument
    >        tableconstructor |
    [ ] TLuaStringArgument
    >        LiteralString

    [ ] TLuaFunctionDefinitionExpression
    functiondef ::= function funcbody

    [ ] TLuaFunctionBody
    funcbody ::= '(' [parlist] ')' block end

    [ ] TLuaParameterList
    parlist ::= namelist [',' '...'] | '...'

    [ ] TLuaTableConstructorExpression
    tableconstructor ::= '{' [fieldlist] '}'

    [ ] TLuaFieldList
    fieldlist ::= field {fieldsep field} [fieldsep]

    [ ] TLuaField
    field ::=
    [ ] TLuaKeyValueField
    >         '[' exp ']' '=' exp |
    [ ] TLuaNamedField
    >         Name '=' exp |
    [ ] (TLuaField)
    >         exp

    [ ] TLuaFieldSeparator -> enum
    fieldsep ::= ',' | ';'

    [ ] TLuaBinaryOp -> enum
    binop ::= '+' | '-' | '*' | '/' | '//' | '^' | '%' |
    >         '&' | '~' | '|' | '>>' | '<<' | '..' |
    >         '<' | '<=' | '>' | '>=' | '==' | '~=' |
    >         and | or

    [ ] TLuaUnaryOp -> enum
    unop ::= '-' | not | '#' | '~'

    --- The Complete Syntax of Lua --- *)

  // --- Forward declarations ---

  TLuaBlock = class;
  TLuaExpression = class;
  TLuaTableConstructorExpression = class;
  TLuaLiteralStringExpression = class;
  TLuaVariableExpression = class;

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

  public
    constructor Create(AName: string);

    class function Parser: IParser<TLuaName>; static;

    property Name: string read FName;

    class operator Implicit(AName: string): TLuaName;
    class operator Implicit(AName: TLuaName): string;

  end;

  ILuaNameParser = IParser<TLuaName>;

  TLuaNameParser = class(TParser<TLuaName>, ILuaNameParser)
  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;

  end;

  TLuaFunctionName = class
  public type

    IParser = IObjectParser<TLuaFunctionName>;

    TParser = class(TObjectParser<TLuaFunctionName>, IParser)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  private
    FNames: IList<TLuaName>;
    FSelfCall: Boolean;

  public
    constructor Create;

    class function Parser: IParser;

    property Names: IList<TLuaName> read FNames;
    property SelfCall: Boolean read FSelfCall write FSelfCall;

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

  TLuaExpressionList = class
  public type

    IParser = IObjectParser<TLuaExpressionList>;

    TParser = class(TObjectParser<TLuaExpressionList>, IParser)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  private
    FExpressions: IObjectList<TLuaExpression>;

  public
    constructor Create;

    class function Parser: IParser;

    property Expressions: IObjectList<TLuaExpression> read FExpressions;

  end;

  TLuaVariableList = class
  public type

    IParser = IObjectParser<TLuaVariableList>;

    TParser = class(TObjectParser<TLuaVariableList>, IParser)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  private
    FVariables: IObjectList<TLuaVariableExpression>;

  public
    class function Parser: IParser;

  end;

  // --- Expressions ---

  /// <summary>An expression, which evaluates to a value.</summary>
  TLuaExpression = class
  public type

    IParser = IObjectParser<TLuaExpression>;

    TParser = class(TObjectParser<TLuaExpression>, IParser)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  public
    class function Parser: IParser;

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

  // --- Statements ---

  TLuaStatementClass = class of TLuaStatement;

  /// <summary>A statement can be executed and discards results if any.</summary>
  TLuaStatement = class
  public type

    IParser = IObjectParser<TLuaStatement>;

    TParser = class(TObjectParser<TLuaStatement>, IParser)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  public
    class function Parser: IParser;
    class function TypedParser: IParser; virtual; abstract;

  end;

  /// <summary>
  /// An empty statement, consisting of a single semicolon.
  /// <code> ;</code>
  /// </summary>
  TLuaEmptyStatement = class(TLuaStatement)
  public type

    IParser = IObjectParser<TLuaEmptyStatement>;

    TParser = class(TObjectParser<TLuaEmptyStatement>, IParser)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  public
    class function Parser: IParser;
    class function TypedParser: TLuaStatement.IParser; override;

  end;

  /// <summary>
  /// An assignment statement, assigning one or multiple values.
  /// <code> a, b, c = x, y, z</code>
  /// </summary>
  TLuaAssignmentStatement = class(TLuaStatement)
  public type

    IParser = IObjectParser<TLuaAssignmentStatement>;

    TParser = class(TObjectParser<TLuaAssignmentStatement>, IParser)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  private
    FVariableList: TLuaVariableList;
    FExpressionList: TLuaExpressionList;

    procedure SetExpressionList(const Value: TLuaExpressionList);
    procedure SetVariableList(const Value: TLuaVariableList);

  public
    constructor Create(AVariableList: TLuaVariableList; AExpressionList: TLuaExpressionList);
    destructor Destroy; override;

    class function Parser: IParser;
    class function TypedParser: TLuaStatement.IParser; override;

    property VariableList: TLuaVariableList read FVariableList write SetVariableList;
    property ExpressionList: TLuaExpressionList read FExpressionList write SetExpressionList;

  end;

  /// <summary>
  /// A function call used as a statement, meaning its result gets discarded.
  /// <code> table.insert(list, 42)</code>
  /// </summary>
  TLuaCallStatement = class(TLuaStatement)
  public type

    IParser = IObjectParser<TLuaCallStatement>;

    TParser = class(TObjectParser<TLuaCallStatement>, IParser)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  private
    FCall: TLuaCallExpression;

  public
    class function Parser: IParser;
    class function TypedParser: TLuaStatement.IParser; override;

  end;

  /// <summary>
  /// A label, which can be used by a goto.
  /// <code> ::mylabel::</code>
  /// </summary>
  TLuaLabelStatement = class(TLuaStatement)
  public type

    IParser = IObjectParser<TLuaLabelStatement>;

    TParser = class(TObjectParser<TLuaLabelStatement>, IParser)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  private
    FName: TLuaName;

  public
    class function Parser: IParser;
    class function TypedParser: TLuaStatement.IParser; override;

  end;

  /// <summary>
  /// A break statement, allowing you to immediately jump out of a loop.
  /// <code> break</code>
  /// </summary>
  TLuaBreakStatement = class(TLuaStatement)
  public type

    IParser = IObjectParser<TLuaBreakStatement>;

    TParser = class(TObjectParser<TLuaBreakStatement>, IParser)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  public
    class function Parser: IParser;
    class function TypedParser: TLuaStatement.IParser; override;

  end;

  /// <summary>
  /// A goto statement, allowing you to jump to a label.
  /// <code> goto mylabel</code>
  /// </summary>
  TLuaGotoStatement = class(TLuaStatement)
  public type

    IParser = IObjectParser<TLuaGotoStatement>;

    TParser = class(TObjectParser<TLuaGotoStatement>, IParser)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  private
    FLabelName: TLuaName;

  public
    class function Parser: IParser;
    class function TypedParser: TLuaStatement.IParser; override;

  end;

  /// <summary>
  /// A do-end block, simply limiting the scope without altering the control-flow.
  /// <code> do<p/>   ...<p/> end</code>
  /// </summary>
  TLuaDoStatement = class(TLuaStatement)
  public type

    IParser = IObjectParser<TLuaDoStatement>;

    TParser = class(TObjectParser<TLuaDoStatement>, IParser)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  private
    FBlock: TLuaBlock;

  public
    class function Parser: IParser;
    class function TypedParser: TLuaStatement.IParser; override;

  end;

  /// <summary>
  /// A while-loop, repeating its content as long, as a certain condition is met.
  /// <code> while &lt;condition&gt; do<p/>   ...<p/> end</code>
  /// </summary>
  TLuaWhileStatement = class(TLuaStatement)
  public type

    IParser = IObjectParser<TLuaWhileStatement>;

    TParser = class(TObjectParser<TLuaWhileStatement>, IParser)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  private
    FCondition: TLuaExpression;
    FBlock: TLuaBlock;

  public
    class function Parser: IParser;
    class function TypedParser: TLuaStatement.IParser; override;

  end;

  /// <summary>
  /// A repeat-until-loop, executing its content once and then repeating it until a certain condition is met.
  /// <code> repeat<p/>   ...<p/> until &lt;condition&gt;</code>
  /// </summary>
  TLuaRepeatStatement = class(TLuaStatement)
  public type

    IParser = IObjectParser<TLuaRepeatStatement>;

    TParser = class(TObjectParser<TLuaRepeatStatement>, IParser)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  private
    FBlock: TLuaBlock;
    FCondition: TLuaExpression;

  public
    class function Parser: IParser;
    class function TypedParser: TLuaStatement.IParser; override;

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
  public type

    IParser = IObjectParser<TLuaIfStatement>;

    TParser = class(TObjectParser<TLuaIfStatement>, IParser)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  private
    FBlocks: IObjectList<TLuaConditionalBlock>;
    FElseBlock: TLuaBlock;

  public
    class function Parser: IParser;
    class function TypedParser: TLuaStatement.IParser; override;

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
  public type

    IParser = IObjectParser<TLuaNumericForStatement>;

    TParser = class(TObjectParser<TLuaNumericForStatement>, IParser)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  private
    FVariableName: TLuaName;
    FStart: TLuaExpression;
    FEnd: TLuaExpression;
    FStep: TLuaExpression;

  public
    class function Parser: IParser;
    class function TypedParser: TLuaStatement.IParser; override;

  end;

  /// <summary>
  /// A generic for-in statement, used for example with pairs or ipairs.
  /// <code> for k, v in pairs(t) do<p/>   ...<p/> end</code>
  /// </summary>
  TLuaGenericForStatement = class(TLuaForStatement)
  public type

    IParser = IObjectParser<TLuaGenericForStatement>;

    TParser = class(TObjectParser<TLuaGenericForStatement>, IParser)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  private
    FNameList: TLuaNameList;
    FExpressionList: TLuaExpressionList;

  public
    class function Parser: IParser;
    class function TypedParser: TLuaStatement.IParser; override;

  end;

  /// <summary>
  /// A function-definition, which is syntactic sugar for assigning a function to a variable.
  /// <code> function myfunction(a, b, c)<p/>   ...<p/> end</code>
  /// </summary>
  TLuaFunctionStatement = class(TLuaStatement)
  public type

    IParser = IObjectParser<TLuaFunctionStatement>;

    TParser = class(TObjectParser<TLuaFunctionStatement>, IParser)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  private
    FName: TLuaFunctionName;
    FBody: TLuaFunctionBody;

  public
    class function Parser: IParser;
    class function TypedParser: TLuaStatement.IParser; override;

  end;

  /// <summary>
  /// A local function-definition, which is syntactic sugar for assigning a function to a local variable.
  /// <code> function myfunction(a, b, c)<p/>   ...<p/> end</code>
  /// </summary>
  TLuaLocalFunctionStatement = class(TLuaStatement)
  public type

    IParser = IObjectParser<TLuaLocalFunctionStatement>;

    TParser = class(TObjectParser<TLuaLocalFunctionStatement>, IParser)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  private
    FName: TLuaName;
    FBody: TLuaFunctionBody;

  public
    class function Parser: IParser;
    class function TypedParser: TLuaStatement.IParser; override;

  end;

  /// <summary>
  /// Defines (or redefines) local variables for the current scope.
  /// <code> local a<p/> local x, y, z = 1, 2, 3</code>
  /// </summary>
  TLuaLocalAssignmentStatement = class(TLuaStatement)
  public type

    IParser = IObjectParser<TLuaLocalAssignmentStatement>;

    TParser = class(TObjectParser<TLuaLocalAssignmentStatement>, IParser)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  private
    FNameList: TLuaNameList;
    FExpressionList: TLuaExpressionList;

  public
    class function Parser: IParser;
    class function TypedParser: TLuaStatement.IParser; override;

  end;

  TLuaReturnStatement = class
  public type

    IParser = IObjectParser<TLuaReturnStatement>;

    TParser = class(TObjectParser<TLuaReturnStatement>, IParser)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  private
    FResults: TLuaExpressionList;
    FTrailingSemicolon: Boolean;

    procedure SetResults(const Value: TLuaExpressionList);

  public
    destructor Destroy; override;

    class function Parser: IParser;

    property Results: TLuaExpressionList read FResults write SetResults;

    property TrailingSemicolon: Boolean read FTrailingSemicolon write FTrailingSemicolon;

  end;

  /// <summary>A list of statements with an optional return statement at the end.</summary>
  TLuaBlock = class
  public type

    IParser = IObjectParser<TLuaBlock>;

    TParser = class(TObjectParser<TLuaBlock>, IParser)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  private
    FStatements: IObjectList<TLuaStatement>;
    FReturnStatement: TLuaReturnStatement;

    procedure SetReturnStatement(const Value: TLuaReturnStatement);

  public
    constructor Create;
    destructor Destroy; override;

    class function Parser: IParser;

    property Statements: IObjectList<TLuaStatement> read FStatements;

    function HasReturnStatement: Boolean;
    property ReturnStatement: TLuaReturnStatement read FReturnStatement write SetReturnStatement;

  end;

  TLuaChunk = TLuaBlock;

const

  LuaStatementClasses: array [0 .. 14] of TLuaStatementClass = (
    TLuaEmptyStatement,
    TLuaAssignmentStatement,
    TLuaCallStatement,
    TLuaLabelStatement,
    TLuaBreakStatement,
    TLuaGotoStatement,
    TLuaDoStatement,
    TLuaWhileStatement,
    TLuaRepeatStatement,
    TLuaIfStatement,
    TLuaNumericForStatement,
    TLuaGenericForStatement,
    TLuaFunctionStatement,
    TLuaLocalFunctionStatement,
    TLuaLocalAssignmentStatement
    );

implementation

{ TLuaBlock }

constructor TLuaBlock.Create;
begin
  FStatements := TObjectList<TLuaStatement>.Create;
end;

destructor TLuaBlock.Destroy;
begin
  FReturnStatement.Free;
  inherited;
end;

function TLuaBlock.HasReturnStatement: Boolean;
begin
  Result := FReturnStatement <> nil;
end;

class function TLuaBlock.Parser: IParser;
begin
  Result := TParser.Create;
end;

procedure TLuaBlock.SetReturnStatement(const Value: TLuaReturnStatement);
begin
  FReturnStatement.Free;
  FReturnStatement := Value;
end;

{ TLuaBlock.TParser }

class function TLuaBlock.TParser.GetResultName: string;
begin
  Result := 'code-block';
end;

function TLuaBlock.TParser.Parse: Boolean;
begin
  ParseResult := TLuaBlock.Create;
  while not ReachedEnd and not StartsWith('end') do
    ParseResult.Statements.Add(TLuaStatement.Parser.Require(Info));
  ParseResult.ReturnStatement := TLuaReturnStatement.Parser.Optional(Info);
  Result := True;
end;

{ TLuaStatement.TParser }

class function TLuaStatement.TParser.GetResultName: string;
begin
  Result := 'statement';
end;

function TLuaStatement.TParser.Parse: Boolean;
var
  StatementClass: TLuaStatementClass;
begin
  for StatementClass in LuaStatementClasses do
  begin
    ParseResult := StatementClass.TypedParser.Optional(Info);
    if ParseResult <> nil then
      Exit(True);
  end;
  Result := False;
end;

{ TLuaStatement }

class function TLuaStatement.Parser: IParser;
begin
  Result := TParser.Create;
end;

{ TLuaReturnStatement }

destructor TLuaReturnStatement.Destroy;
begin
  FResults.Free;
  inherited;
end;

class function TLuaReturnStatement.Parser: IParser;
begin
  Result := TParser.Create;
end;

procedure TLuaReturnStatement.SetResults(const Value: TLuaExpressionList);
begin
  FResults.Free;
  FResults := Value;
end;

{ TLuaReturnStatement.TParser }

class function TLuaReturnStatement.TParser.GetResultName: string;
begin
  Result := 'return-statement';
end;

function TLuaReturnStatement.TParser.Parse: Boolean;
begin
  if not StartsWith('return') then
    Exit(False);
  ParseResult := TLuaReturnStatement.Create;
  ParseResult.FResults := TLuaExpressionList.Parser.Optional(Info);
  ParseResult.FTrailingSemicolon := StartsWith(';');
  Result := True;
end;

{ TLuaExpressionList }

constructor TLuaExpressionList.Create;
begin
  FExpressions := TObjectList<TLuaExpression>.Create;
end;

class function TLuaExpressionList.Parser: IParser;
begin
  Result := TParser.Create;
end;

{ TLuaExpressionList.TParser }

class function TLuaExpressionList.TParser.GetResultName: string;
begin
  Result := 'expression-list';
end;

function TLuaExpressionList.TParser.Parse: Boolean;
var
  Expression: TLuaExpression;
begin
  ParseResult := TLuaExpressionList.Create;
  ParseResult.Expressions.Add(TLuaExpression.Parser.Require(Info));
  while not ReachedEnd and TLuaExpression.Parser.Optional(Info, Expression) do
    ParseResult.Expressions.Add(Expression);

  Result := False;
end;

{ TLuaExpression.TParser }

class function TLuaExpression.TParser.GetResultName: string;
begin
  Result := 'expression';
end;

function TLuaExpression.TParser.Parse: Boolean;
begin
  raise ENotImplemented.Create(ClassName + '.Parse');
  Result := True;
end;

{ TLuaExpression }

class function TLuaExpression.Parser: IParser;
begin
  Result := TParser.Create;
end;

{ TLuaEmptyStatement.TParser }

class function TLuaEmptyStatement.TParser.GetResultName: string;
begin
  Result := 'semicolon';
end;

function TLuaEmptyStatement.TParser.Parse: Boolean;
begin
  Result := StartsWith(';');
  if Result then
    ParseResult := TLuaEmptyStatement.Create;
end;

{ TLuaEmptyStatement }

class function TLuaEmptyStatement.Parser: IParser;
begin
  Result := TParser.Create;
end;

class function TLuaEmptyStatement.TypedParser: TLuaStatement.IParser;
begin
  Result := TLuaStatement.IParser(Parser);
end;

{ TLuaAssignmentStatement.TParser }

class function TLuaAssignmentStatement.TParser.GetResultName: string;
begin
  Result := 'assignment';
end;

function TLuaAssignmentStatement.TParser.Parse: Boolean;
var
  VariableList: TLuaVariableList;
  ExpressionList: TLuaExpressionList;
begin
  VariableList := TLuaVariableList.Parser.Require(Info);
  SkipWhitespace;
  if not StartsWith('=') then
    Exit(False);
  SkipWhitespace;
  ExpressionList := TLuaExpressionList.Parser.Require(Info);
  ParseResult := TLuaAssignmentStatement.Create(VariableList, ExpressionList);
  Result := True;
end;

{ TLuaCallStatement.TParser }

class function TLuaCallStatement.TParser.GetResultName: string;
begin
  Result := 'function-call';
end;

function TLuaCallStatement.TParser.Parse: Boolean;
begin
  raise ENotImplemented.Create(ClassName + '.Parse');
end;

{ TLuaLabelStatement.TParser }

class function TLuaLabelStatement.TParser.GetResultName: string;
begin
  Result := 'label';
end;

function TLuaLabelStatement.TParser.Parse: Boolean;
begin
  raise ENotImplemented.Create(ClassName + '.Parse');
end;

{ TLuaBreakStatement.TParser }

class function TLuaBreakStatement.TParser.GetResultName: string;
begin
  Result := 'break';
end;

function TLuaBreakStatement.TParser.Parse: Boolean;
begin
  raise ENotImplemented.Create(ClassName + '.Parse');
end;

{ TLuaGotoStatement.TParser }

class function TLuaGotoStatement.TParser.GetResultName: string;
begin
  Result := 'goto';
end;

function TLuaGotoStatement.TParser.Parse: Boolean;
begin
  raise ENotImplemented.Create(ClassName + '.Parse');
end;

{ TLuaDoStatement.TParser }

class function TLuaDoStatement.TParser.GetResultName: string;
begin
  Result := 'do-block';
end;

function TLuaDoStatement.TParser.Parse: Boolean;
begin
  raise ENotImplemented.Create(ClassName + '.Parse');
end;

{ TLuaWhileStatement.TParser }

class function TLuaWhileStatement.TParser.GetResultName: string;
begin
  Result := 'while-statement';
end;

function TLuaWhileStatement.TParser.Parse: Boolean;
begin
  raise ENotImplemented.Create(ClassName + '.Parse');
end;

{ TLuaRepeatStatement.TParser }

class function TLuaRepeatStatement.TParser.GetResultName: string;
begin
  Result := 'repeat-until';
end;

function TLuaRepeatStatement.TParser.Parse: Boolean;
begin
  raise ENotImplemented.Create(ClassName + '.Parse');
end;

{ TLuaIfStatement.TParser }

class function TLuaIfStatement.TParser.GetResultName: string;
begin
  Result := 'if-statement';
end;

function TLuaIfStatement.TParser.Parse: Boolean;
begin
  raise ENotImplemented.Create(ClassName + '.Parse');
end;

{ TLuaNumericForStatement.TParser }

class function TLuaNumericForStatement.TParser.GetResultName: string;
begin
  Result := 'numeric-for-statement';
end;

function TLuaNumericForStatement.TParser.Parse: Boolean;
begin
  raise ENotImplemented.Create(ClassName + '.Parse');
end;

{ TLuaGenericForStatement.TParser }

class function TLuaGenericForStatement.TParser.GetResultName: string;
begin
  Result := 'generic-for-statement';
end;

function TLuaGenericForStatement.TParser.Parse: Boolean;
begin
  raise ENotImplemented.Create(ClassName + '.Parse');
end;

{ TLuaFunctionStatement.TParser }

class function TLuaFunctionStatement.TParser.GetResultName: string;
begin
  Result := 'function-definition';
end;

function TLuaFunctionStatement.TParser.Parse: Boolean;
begin
  raise ENotImplemented.Create(ClassName + '.Parse');
end;

{ TLuaLocalFunctionStatement.TParser }

class function TLuaLocalFunctionStatement.TParser.GetResultName: string;
begin
  Result := 'local-function-definition';
end;

function TLuaLocalFunctionStatement.TParser.Parse: Boolean;
begin
  raise ENotImplemented.Create(ClassName + '.Parse');
end;

{ TLuaLocalAssignmentStatement.TParser }

class function TLuaLocalAssignmentStatement.TParser.GetResultName: string;
begin
  Result := 'local-variable-declaration';
end;

function TLuaLocalAssignmentStatement.TParser.Parse: Boolean;
begin
  raise ENotImplemented.Create(ClassName + '.Parse');
end;

{ TLuaAssignmentStatement }

constructor TLuaAssignmentStatement.Create(AVariableList: TLuaVariableList; AExpressionList: TLuaExpressionList);
begin
  FVariableList := AVariableList;
  FExpressionList := AExpressionList;
end;

destructor TLuaAssignmentStatement.Destroy;
begin
  FExpressionList.Free;
  FVariableList.Free;
  inherited;
end;

class function TLuaAssignmentStatement.Parser: IParser;
begin
  Result := TParser.Create;
end;

procedure TLuaAssignmentStatement.SetExpressionList(const Value: TLuaExpressionList);
begin
  FExpressionList.Free;
  FExpressionList := Value;
end;

procedure TLuaAssignmentStatement.SetVariableList(const Value: TLuaVariableList);
begin
  FVariableList.Free;
  FVariableList := Value;
end;

class function TLuaAssignmentStatement.TypedParser: TLuaStatement.IParser;
begin
  Result := TLuaStatement.IParser(Parser);
end;

{ TLuaCallStatement }

class function TLuaCallStatement.Parser: IParser;
begin
  Result := TParser.Create;
end;

class function TLuaCallStatement.TypedParser: TLuaStatement.IParser;
begin
  Result := TLuaStatement.IParser(Parser);
end;

{ TLuaLabelStatement }

class function TLuaLabelStatement.Parser: IParser;
begin
  Result := TParser.Create;
end;

class function TLuaLabelStatement.TypedParser: TLuaStatement.IParser;
begin
  Result := TLuaStatement.IParser(Parser);
end;

{ TLuaBreakStatement }

class function TLuaBreakStatement.Parser: IParser;
begin
  Result := TParser.Create;
end;

class function TLuaBreakStatement.TypedParser: TLuaStatement.IParser;
begin
  Result := TLuaStatement.IParser(Parser);
end;

{ TLuaGotoStatement }

class function TLuaGotoStatement.Parser: IParser;
begin
  Result := TParser.Create;
end;

class function TLuaGotoStatement.TypedParser: TLuaStatement.IParser;
begin
  Result := TLuaStatement.IParser(Parser);
end;

{ TLuaDoStatement }

class function TLuaDoStatement.Parser: IParser;
begin
  Result := TParser.Create;
end;

class function TLuaDoStatement.TypedParser: TLuaStatement.IParser;
begin
  Result := TLuaStatement.IParser(Parser);
end;

{ TLuaWhileStatement }

class function TLuaWhileStatement.Parser: IParser;
begin
  Result := TParser.Create;
end;

class function TLuaWhileStatement.TypedParser: TLuaStatement.IParser;
begin
  Result := TLuaStatement.IParser(Parser);
end;

{ TLuaRepeatStatement }

class function TLuaRepeatStatement.Parser: IParser;
begin
  Result := TParser.Create;
end;

class function TLuaRepeatStatement.TypedParser: TLuaStatement.IParser;
begin
  Result := TLuaStatement.IParser(Parser);
end;

{ TLuaIfStatement }

class function TLuaIfStatement.Parser: IParser;
begin
  Result := TParser.Create;
end;

class function TLuaIfStatement.TypedParser: TLuaStatement.IParser;
begin
  Result := TLuaStatement.IParser(Parser);
end;

{ TLuaNumericForStatement }

class function TLuaNumericForStatement.Parser: IParser;
begin
  Result := TParser.Create;
end;

class function TLuaNumericForStatement.TypedParser: TLuaStatement.IParser;
begin
  Result := TLuaStatement.IParser(Parser);
end;

{ TLuaGenericForStatement }

class function TLuaGenericForStatement.Parser: IParser;
begin
  Result := TParser.Create;
end;

class function TLuaGenericForStatement.TypedParser: TLuaStatement.IParser;
begin
  Result := TLuaStatement.IParser(Parser);
end;

{ TLuaFunctionStatement }

class function TLuaFunctionStatement.Parser: IParser;
begin
  Result := TParser.Create;
end;

class function TLuaFunctionStatement.TypedParser: TLuaStatement.IParser;
begin
  Result := TLuaStatement.IParser(Parser);
end;

{ TLuaLocalFunctionStatement }

class function TLuaLocalFunctionStatement.Parser: IParser;
begin
  Result := TParser.Create;
end;

class function TLuaLocalFunctionStatement.TypedParser: TLuaStatement.IParser;
begin
  Result := TLuaStatement.IParser(Parser);
end;

{ TLuaLocalAssignmentStatement }

class function TLuaLocalAssignmentStatement.Parser: IParser;
begin
  Result := TParser.Create;
end;

class function TLuaLocalAssignmentStatement.TypedParser: TLuaStatement.IParser;
begin
  Result := TLuaStatement.IParser(Parser);
end;

{ TLuaVariableList.TParser }

class function TLuaVariableList.TParser.GetResultName: string;
begin
  Result := 'variable-list';
end;

function TLuaVariableList.TParser.Parse: Boolean;
begin
  raise ENotImplemented.Create(ClassName + '.Parse');
end;

{ TLuaName }

constructor TLuaName.Create(AName: string);
begin
  FName := AName;
end;

class operator TLuaName.Implicit(AName: string): TLuaName;
begin
  Result.Create(AName);
end;

class operator TLuaName.Implicit(AName: TLuaName): string;
begin
  Result := AName.FName;
end;

class function TLuaName.Parser: IParser<TLuaName>;
begin
  Result := TLuaNameParser.Create;
end;

{ TLuaNameParser }

class function TLuaNameParser.GetResultName: string;
begin
  Result := 'name';
end;

function TLuaNameParser.Parse: Boolean;
const
  Alpha = ['a' .. 'z', 'A' .. 'Z', '_'];
  Num = ['0' .. '9'];
  AlphaNum = Alpha + Num;
begin
  if not CharInSet(First, Alpha) then
    Exit(False);
  ParseResult := ReadWhile(AlphaNum);
  Result := True;
end;

{ TLuaFunctionName.TParser }

class function TLuaFunctionName.TParser.GetResultName: string;
begin
  Result := 'function-name';
end;

function TLuaFunctionName.TParser.Parse: Boolean;
var
  Name: TLuaName;
begin
  Result := TLuaName.Parser.Optional(Info, Name);
  if not Result then
    Exit;
  ParseResult := TLuaFunctionName.Create;
  ParseResult.Names.Add(Name);
  SkipWhitespace;
  while StartsWith('.') do
  begin
    SkipWhitespace;
    ParseResult.Names.Add(TLuaName.Parser.Require(Info));
    SkipWhitespace;
  end;
  if StartsWith(':') then
  begin
    SkipWhitespace;
    ParseResult.SelfCall := True;
    ParseResult.Names.Add(TLuaName.Parser.Require(Info));
  end;
end;

{ TLuaVariableList }

class function TLuaVariableList.Parser: IParser;
begin
  Result := TParser.Create;
end;

{ TLuaFunctionName }

constructor TLuaFunctionName.Create;
begin
  FNames := TList<TLuaName>.Create;
end;

class function TLuaFunctionName.Parser: IParser;
begin
  Result := TParser.Create;
end;

end.
