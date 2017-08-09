program LuaTest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, LuaHeader;

var
  L: TLuaState;
  Params: array of Variant;
begin
  ReportMemoryLeaksOnShutdown := True;

  try

    L := NewLuaState;
    L.pushstring('blub');
    L.pushfstring('%s: %s', [L.typenameat(L.top), L.tostring(L.top)]);
    Writeln(L.tostring(L.top));
    L.concat(2);
    Writeln(L.tostring(L.top));
    L.close;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

  Readln;
end.
