program LuaParsing;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,

  Pengine.Lua.Parsing;

begin
  try



  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

