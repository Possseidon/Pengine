unit DebugConsoleDefine;

(*
  To use this Unit correctly, follow these instructions:
  - Add DebugConsole configuration to project
  - Go to Linking-Settings in the new configuration and enable Console
*)

interface

uses
  Classes, Windows;

procedure DebugWriteLine(AMessage: string); inline; overload;
procedure DebugWriteLine(AMessage: AnsiString); inline; overload;

procedure DebugWrite(AMessage: string); inline; overload;
procedure DebugWrite(AMessage: AnsiString); inline; overload;

procedure DebugWriteBuf(AMessage: string); overload;
procedure DebugWriteBuf(AMessage: AnsiString); overload;

procedure DebugFlushBuf(ANewLine: Boolean);

implementation

{$IFDEF CONSOLE}
var
  DebugBuffer: string;
{$ENDIF}

procedure DebugWriteLine(AMessage: string);
begin
{$IFDEF CONSOLE}
  Writeln(AMessage);
{$ENDIF}
end;

procedure DebugWriteLine(AMessage: AnsiString);
begin
{$IFDEF CONSOLE}
  Writeln(AMessage);
{$ENDIF}
end;

procedure DebugWrite(AMessage: string);
begin
{$IFDEF CONSOLE}
  Write(AMessage);
{$ENDIF}
end;

procedure DebugWrite(AMessage: AnsiString);
begin
{$IFDEF CONSOLE}
  Write(AMessage);
{$ENDIF}
end;

procedure DebugWriteBuf(AMessage: string);
begin
{$IFDEF CONSOLE}
  DebugBuffer := DebugBuffer + AMessage;
{$ENDIF}
end;

procedure DebugWriteBuf(AMessage: AnsiString);
begin
{$IFDEF CONSOLE}
  DebugBuffer := DebugBuffer + string(AMessage);
{$ENDIF}
end;

procedure DebugFlushBuf(ANewLine: Boolean);
begin
{$IFDEF CONSOLE}
  if ANewLine then
    Writeln(DebugBuffer)
  else
    Write(DebugBuffer);
  DebugBuffer := '';
{$ENDIF}
end;

{$IFDEF CONSOLE}
procedure OnExit;
begin
  while (GetAsyncKeyState(VK_ESCAPE) and (1 shl 15)) = 0 do
    Sleep(10);
end;
{$ENDIF}

{$IFDEF CONSOLE}
initialization
  ExitProcessProc := OnExit;
{$ENDIF}

end.
