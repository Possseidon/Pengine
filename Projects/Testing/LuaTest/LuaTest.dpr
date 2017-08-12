program LuaTest;

{$APPTYPE CONSOLE}
{$R *.res}


uses
  SysUtils, LuaHeader, Classes, Math, StrUtils, AnsiStrings;

function L_LoadString(L: TLuaState): Integer; cdecl;
begin
  L.CheckType(1, ltString);
  L.CheckEnd(2);
  L.LoadString(L.ToString(1));
  Result := 1;
end;

function L_ToString(L: TLuaState): Integer; cdecl;
begin
  L.CheckAny(1);
  L.CheckEnd(2);
  L.ToString_X;
  Result := 1;
end;

function L_Print(L: TLuaState): Integer; cdecl;
var
  Params, I: Integer;
begin
  Params := L.Top;
  for I := 1 to Params do
  begin
    Write(L.ToString(I) + #9);
  end;
  Writeln;
  Result := 0;
end;

function L_Error(L: TLuaState): Integer; cdecl;
var
  Level: Integer;
begin
  if L.IsNone(1) then
  begin
    Level := 1;
    L.PushNil
  end
  else
  begin
    Level := L.CheckOrDefault(2, 1);
    L.CheckEnd(3);
  end;
  Result := L.Error(L.ToString(1), Level);
end;

function L_Next(L: TLuaState): Integer; cdecl;
begin
  L.CheckType(1, ltTable);
  L.Top := 2;
  L.CheckEnd(3);
  if L.Next(1) then
    Result := 2
  else
    Result := 0;
end;

function L_Type(L: TLuaState): Integer; cdecl;
var
  T: TLuaType;
begin
  T := L.CheckAny(1);
  L.CheckEnd(2);
  L.PushString(L.TypeName(T));
  Result := 1;
end;

function L_PCall(L: TLuaState): Integer; cdecl;
begin
  L.CheckType(1, ltFunction);
  L.PushBoolean(L.PCall(L.Top - 1, LUA_MULTRET, 0) = lceOK);
  L.Insert(1);
  Result := L.Top;
end;

function L_GetMetatable(L: TLuaState): Integer; cdecl;
begin
  L.CheckType(1, ltTable);
  L.CheckEnd(2);
  if L.GetMetatable(1) then
  begin
    if L.GetField('__metatable', 2) = ltNil then
      L.Top := 2;
  end
  else
    L.PushNil;
  Result := 1;
end;

function L_PrintArray(L: TLuaState): Integer; cdecl;
var
  I: Integer;
  Digits: Integer;
  S: AnsiString;
begin
  L.CheckType(1, ltTable);
  L.CheckEnd(2);
  I := 1;
  L.Len(1);
  Digits := Length(IntToStr(L.ToInteger(2)));
  L.Top := 1;
  L.GetGlobal('print');
  while True do
  begin
    if L.GetI(I, 1) = ltNil then
      Break;
    L.PushValue(2);
    S := Format(AnsiStrings.Format('[%%%dd] = %%s', [Digits]), [I, L.ToString(3)]);
    L.PushString(PPAnsiChar(@S)^);
    L.Call(1, 0);
    L.Top := 2;
    Inc(I);
  end;
  Result := 0;
end;

function L_SetMetatable(L: TLuaState): Integer; cdecl;
begin
  L.CheckType(1, ltTable);
  L.CheckType(2, [ltTable, ltNil]);
  L.CheckEnd(3);
  if L.GetMetatable(1) then
  begin
    if L.GetField('__metatable', 3) <> ltNil then
      L.Error('cannot change a protected metatable');
    L.Top := 2;
  end;
  L.SetMetatable(1);
  Result := 1;
end;

var
  L: TLuaState;
  LoadErr: TLuaLoadError;
  PCallErr: TLuaPCallError;
  I: Integer;
  Valid: Boolean;
  Err: AnsiString;
  Input, S: AnsiString;
  Line: Integer;

begin

  try

    L := NewLuaState;
    try

      L.Register('print', L_Print);
      L.Register('loadstring', L_LoadString);
      L.Register('error', L_Error);
      L.Register('next', L_Next);
      L.Register('tostring', L_ToString);
      L.Register('type', L_Type);
      L.Register('pcall', L_PCall);
      L.Register('setmetatable', L_SetMetatable);
      L.Register('getmetatable', L_GetMetatable);
      L.Register('printarray', L_PrintArray);

      while True do
      begin
        Line := 1;
        Input := '';
        Write('> ');
        repeat
          Valid := True;
          Readln(S);
          Input := Input + S + #10;
          LoadErr := L.LoadString('return ' + Input, 'stdin');
          if LoadErr <> lleOK then
          begin
            LoadErr := L.LoadString(Input, 'stdin');
            if LoadErr <> lleOK then
            begin
              Err := L.ToString;
              if AnsiStrings.ContainsText(Err, '<eof>') then
                Valid := False
              else
              begin
                Writeln('Compile Error:');
                Writeln(L.ToString);
              end;
              L.Top := 0;
            end
            else
              L.Remove(1);
          end;
          if not Valid then
          begin
            Inc(Line);
            Write(Format('%.2d> ', [Line]));
          end;
        until Valid;

        if LoadErr = lleOK then
        begin
          PCallErr := L.PCall(0, LUA_MULTRET, 0);
          if PCallErr <> lceOK then
          begin
            Writeln(L.ToString);
            L.Top := 0;
          end
          else if L.Top > 0 then
          begin
            L_Print(L);
            L.Top := 0;
          end;
        end;
      end;

      if L.Top > 0 then
      begin
        Writeln(Format('%d leftover elements on stack:', [L.Top]));
        for I := L.Top downto 1 do
        begin
          Writeln(Format('[%d] ''%s''', [I, L.ToString(I)]));
        end;
      end;

    finally
      L.Close;
    end;

  except
    on E: Exception do
    begin
      Writeln(E.ClassName + ': ' + E.Message);
    end;
  end;

  Readln;

end.
