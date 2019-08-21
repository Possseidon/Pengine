program LexerTesting;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,

  Pengine.Lexing;

procedure DrawSyntaxTree(ASyntaxTree: ISyntaxTree; AIndent: Integer = 0);
var
  Node: ISyntaxTree;
begin
  Write(StringOfChar(' ', 2 * AIndent));
  // Writeln(ASyntaxTree.Rule.Name + ' ::= ' + ASyntaxTree.Rule.Expression.ToString);
  Writeln(ASyntaxTree.Rule.Name);
  for Node in ASyntaxTree.Nodes do
    DrawSyntaxTree(Node, AIndent + 1);
end;

procedure Test(ARule: TEBNF.TRule; AText: string);
begin
  Writeln('"', AText, '"');
  try
    DrawSyntaxTree(ARule.Lex(AText));
  except
    on E: Exception do
      Writeln(E.Message);
  end;
  Writeln;
end;

var
  EBNF: TEBNF;

begin
  try

    EBNF := TEBNF.Create;

    EBNF['Name'] := '"abc"';
    EBNF['Numeral'] := '"123"';
    EBNF['LiteralString'] := '"''abc''"';

    EBNF['chunk'] := 'block';
    EBNF['block'] := '{stat} [retstat]';
    EBNF['stat'] :=
      '";" | varlist "=" explist | functioncall | label | "break" | "goto" Name | "do" block "end" |' +
      '"while" exp "do" block "end" | "repeat" block "until" exp |' +
      '"if" exp "then" block {"elseif" exp "then" block} ["else" block] "end" |' +
      '"for" Name "=" exp "," exp ["," exp] "do" block "end" | "for" namelist "in" explist "do" block "end" |' +
      '"function" funcname funcbody | "local" "function" Name funcbody | "local" namelist ["=" explist]';
    EBNF['retstat'] := '"return" [explist] [";"]';
    EBNF['label'] := '"::" Name "::"';
    EBNF['funcname'] := 'Name {"." Name} [":" Name]';
    EBNF['varlist'] := 'var {"," var}';
    EBNF['var'] :=  'Name | prefixexp "[" exp "]" | prefixexp "." Name';
    EBNF['namelist'] := 'Name {"," Name}';
    EBNF['explist'] := 'exp {"," exp}';
    EBNF['exp'] :=
      '"nil" | "false" | "true" | Numeral | LiteralString | "..." | functiondef | prefixexp | tableconstructor |' +
      'exp binop exp | unop exp';
    EBNF['prefixexp'] := 'var | functioncall | "(" exp ")"';
    EBNF['functioncall'] := 'prefixexp args | prefixexp ":" Name args';
    EBNF['args'] :=  '"(" [explist] ")" | tableconstructor | LiteralString';
    EBNF['functiondef'] := '"function" funcbody';
    EBNF['funcbody'] := '"(" [parlist] ")" block "end"';
    EBNF['parlist'] := 'namelist ["," "..."] | "..."';
    EBNF['tableconstructor'] := '"{" [fieldlist] "}"';
    EBNF['fieldlist'] := 'field {fieldsep field} [fieldsep]';
    EBNF['field'] := '"[" exp "]" "=" exp | Name "=" exp | exp';
    EBNF['fieldsep'] := '"," | ";"';
    EBNF['binop'] :=
      '"+" | "-" | "*" | "/" | "//" | "^" | "%" | "&" | "~" | "|" | ">>" | "<<" | ".." | "<" | "<=" | ">" | ">=" |' +
      '"==" | "~=" | "and" | "or"';
    EBNF['unop'] := '"-" | "not" | "#" | "~"';

    //for var Rule in EBNF.Rules do
    //  Writeln(Rule.Name, ' ::= ', Rule.ExpressionString);

    Test(EBNF.FindRule('chunk'), 'abc = abc.abc');

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;

end.
