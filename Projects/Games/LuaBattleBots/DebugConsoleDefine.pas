unit DebugConsoleDefine;

interface

uses
  Classes;

procedure DebugWriteLine(AMessage: string); inline; overload;
procedure DebugWriteLine(AMessage: AnsiString); inline; overload;

procedure DebugWrite(AMessage: string); inline; overload;
procedure DebugWrite(AMessage: AnsiString); inline; overload;

procedure DebugWriteBuf(AMessage: string); overload;
procedure DebugWriteBuf(AMessage: AnsiString); overload;

procedure DebugFlushBuf(ANewLine: Boolean);

implementation

{$IFDEF DEBUG}
var
  DebugBuffer: string;
{$ENDIF}

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

procedure DebugWriteBuf(AMessage: string);
begin
{$IFDEF DEBUG}
  DebugBuffer := DebugBuffer + AMessage;
{$ENDIF}
end;

procedure DebugWriteBuf(AMessage: AnsiString);
begin
{$IFDEF DEBUG}
  DebugBuffer := DebugBuffer + string(AMessage);
{$ENDIF}
end;

procedure DebugFlushBuf(ANewLine: Boolean);
begin
{$IFDEF DEBUG}
  if ANewLine then
    Writeln(DebugBuffer)
  else
    Write(DebugBuffer);
  DebugBuffer := '';
{$ENDIF}
end;

end.
