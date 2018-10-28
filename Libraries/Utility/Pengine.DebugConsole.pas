unit Pengine.DebugConsole;

(*
  To use this Unit correctly, follow these instructions:
  - Add DebugConsole configuration to project
  - Go to Linking-Settings in the new configuration and enable Console
*)

interface

uses
  System.Classes,
  System.SyncObjs,

  Winapi.Windows;

procedure DebugWriteLine(AMessage: string); overload;
procedure DebugWriteLine(AMessage: AnsiString); overload;

procedure DebugWrite(AMessage: string); overload;
procedure DebugWrite(AMessage: AnsiString); overload;

procedure DebugWriteBuf(AMessage: string); overload;
procedure DebugWriteBuf(AMessage: AnsiString); overload;

procedure DebugFlushBuf(ANewLine: Boolean);

implementation

{$IFDEF CONSOLE}

  threadvar
  DebugBuffer: string;

var
  Lock: TCriticalSection;

  {$ENDIF}

procedure DebugWriteLine(AMessage: string);
begin

  {$IFDEF CONSOLE}

  Lock.Enter;
  Writeln(AMessage);
  Lock.Leave;

  {$ENDIF}

end;

procedure DebugWriteLine(AMessage: AnsiString);
begin

  {$IFDEF CONSOLE}

  Lock.Enter;
  Writeln(AMessage);
  Lock.Leave;

  {$ENDIF}

end;

procedure DebugWrite(AMessage: string);
begin

  {$IFDEF CONSOLE}

  Lock.Enter;
  Write(AMessage);
  Lock.Leave;

  {$ENDIF}

end;

procedure DebugWrite(AMessage: AnsiString);
begin

  {$IFDEF CONSOLE}

  Lock.Enter;
  Write(AMessage);
  Lock.Leave;

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

  Lock.Enter;
  if ANewLine then
    Writeln(DebugBuffer)
  else
    Write(DebugBuffer);
  Lock.Leave;
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

Lock := TCriticalSection.Create;

{$IFDEF LEAVE_CONSOLE_OPEN}
ExitProcessProc := OnExit;
{$ENDIF}

finalization

Lock.Free;

{$ENDIF}

end.
