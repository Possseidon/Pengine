program LuaTest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, LuaHeader;

var
  L: Plua_State;
  Err, I: Integer;
  isnum: LongBool;

function LuaAlloc(ud, ptr: Pointer; osize, nsize: NativeUInt): Pointer;
begin
  if nsize = 0 then
  begin
    FreeMem(ptr);
    Exit(nil);
  end;
  Result := ReallocMemory(ptr, nsize);
end;

function DataReader(L: Plua_State; ud: Pointer; sz: PNativeUInt): PAnsiChar;
begin

end;

begin
  ReportMemoryLeaksOnShutdown := True;

  try
    L := lua_newstate(LuaAlloc, nil);
    luaL_openlibs(L);
    lua_close(L);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

  Readln;
end.
