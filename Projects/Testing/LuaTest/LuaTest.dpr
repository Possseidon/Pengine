program LuaTest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, Lua;

function LuaAlloc(ud, ptr: Pointer; osize, nsize: NativeUInt): Pointer;
begin
  if nsize = 0 then
  begin
    FreeMem(ptr);
    Exit(nil);
  end;
  Result := ReallocMemory(ptr, nsize);
end;

var
  L: Plua_State;
begin
  ReportMemoryLeaksOnShutdown := True;

  try
    L := lua_newstate(LuaAlloc, nil);

    lua_close(L);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

  Readln;
end.
