program LuaBattleBots;

{$IFDEF DEBUG}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Vcl.Forms, Windows,
  Main in 'Main.pas' {frmMain},
  Game in 'Game.pas',
  CustomModules in 'CustomModules.pas',
  EntityDefine in 'EntityDefine.pas',
  DebugConsoleDefine in 'DebugConsoleDefine.pas';

{$R *.res}

{$IFDEF DEBUG}
procedure OnExit;
begin
  while (GetAsyncKeyState(VK_ESCAPE) and (1 shl 15)) = 0 do
    Sleep(10);
end;
{$ENDIF}

begin
{$IFDEF DEBUG}
  ExitProcessProc := OnExit;
{$ENDIF}
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
