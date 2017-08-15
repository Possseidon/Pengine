unit DebugConsoleDefine;

interface

uses
  Classes;

procedure DebugWriteLine(AMessage: string); overload;
procedure DebugWriteLine(AMessage: AnsiString); overload;
procedure DebugWriteLine; overload;
procedure DebugWrite(AMessage: string); overload;
procedure DebugWrite(AMessage: AnsiString); overload;

implementation

var
  FBuffer: string;

procedure DebugWriteLine(AMessage: string);
begin
{$IFDEF DEBUG}
  DebugWrite(AMessage);
  DebugWriteLine;
{$ENDIF}
end;

procedure DebugWriteLine(AMessage: AnsiString);
begin
{$IFDEF DEBUG}
  DebugWrite(AMessage);
  DebugWriteLine;
{$ENDIF}
end;

procedure DebugWriteLine;
begin
{$IFDEF DEBUG}
  Writeln(FBuffer);
  FBuffer := '';
{$ENDIF}
end;

procedure DebugWrite(AMessage: string);
begin
{$IFDEF DEBUG}
  FBuffer := FBuffer + AMessage;
{$ENDIF}
end;

procedure DebugWrite(AMessage: AnsiString);
begin
{$IFDEF DEBUG}
  FBuffer := FBuffer + string(AMessage);
{$ENDIF}
end;

end.
