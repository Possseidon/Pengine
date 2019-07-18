program LuaParsing;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,

  Pengine.Parsing,
  Pengine.JSON,
  Pengine.Lua.Parsing;

var
  S: string;
  Parser: TLuaFunctionName.IParser;
begin
  while True do
  begin
    try

      Readln(S);
      Parser := TLuaFunctionName.Parser;
      Parser.Parse(S, True);
      if Parser.Success then
      begin
        for var Name in Parser.ParseResult.Names do
          Write(Name.Name, ' ');
        Writeln;
        Writeln('Self-Call: ', Parser.ParseResult.SelfCall);
      end
      else
        for var Entry in Parser.Context.Value.Log do
          Writeln(Entry.Message);

    except
      on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
    end;
  end;

end.
