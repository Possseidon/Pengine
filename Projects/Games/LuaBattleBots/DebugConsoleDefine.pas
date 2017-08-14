unit DebugConsoleDefine;

interface

procedure DebugWriteLine; overload; inline;
procedure DebugWriteLine(AMessage: string); overload; inline;
procedure DebugWriteLine(AMessage: AnsiString); overload; inline;
procedure DebugWrite(AMessage: string); overload; inline;
procedure DebugWrite(AMessage: AnsiString); overload; inline;

implementation

procedure DebugWriteLine;
begin
{$IFDEF DEBUG}
  Writeln;
{$ENDIF}
end;

procedure DebugWriteLine(AMessage: string);
begin
{$IFDEF DEBUG}
  Writeln(AMessage);
{$ENDIF}
end;

procedure DebugWriteLine(AMessage: AnsiString);
begin
{$IFDEF DEBUG}
  Writeln(AMessage);
{$ENDIF}
end;

procedure DebugWrite(AMessage: string);
begin
{$IFDEF DEBUG}
  Write(AMessage);
{$ENDIF}
end;

procedure DebugWrite(AMessage: AnsiString);
begin
{$IFDEF DEBUG}
  Write(AMessage);
{$ENDIF}
end;

end.
