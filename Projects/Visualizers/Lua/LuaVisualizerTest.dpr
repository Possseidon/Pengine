program LuaVisualizerTest;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,

  Vcl.Graphics,

  Pengine.Lua,
  Pengine.LuaHeader;

function DebugAlloc(ud, ptr: Pointer; osize, nsize: NativeUInt): Pointer; cdecl;
begin
  Result := LuaDefaultAlloc(ud, ptr, osize, nsize);
  Writeln(NativeUInt(ptr).ToHexString, ' -> ', NativeUInt(Result).ToHexString, ' osize: ', osize, ' nsize: ', nsize);
end;

procedure Main;
var
  L: TLuaState;
begin
  L := NewLuaState(LuaDefaultAlloc);
  L.Close;
end;

begin
  try
    Main;
    Readln;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
