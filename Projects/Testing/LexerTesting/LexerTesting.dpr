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
  Writeln(ASyntaxTree.Rule.Name + ' ::= ' + ASyntaxTree.Rule.Expression.ToString);
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

    var A := EBNF.AddRule('a', TEBNF.TTerminal.Create('hello'));
    var B := EBNF.AddRule('b', TEBNF.TTerminal.Create('world'));
    var C := EBNF.AddRule('c', TEBNF.TTerminal.Create('awesome'));

    var Concatenation := EBNF.AddRule('concatenation', TEBNF.TConcatenation.Create([A, B, C]));
    var Alternation := EBNF.AddRule('alternation', TEBNF.TAlternation.Create([A, B, C]));
    var Optional := EBNF.AddRule('optional', TEBNF.TOptional.Create(Concatenation));
    var Repetition := EBNF.AddRule('repetition', TEBNF.TRepetition.Create(Concatenation));

    Test(A, 'hello');
    Test(Concatenation, 'helloworldawesomeh');
    Test(Alternation, 'world');
    Test(Optional, 'helloworldawesome');
    Test(Repetition, 'helloworldawesomehelloworldawesome');

    Readln;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

