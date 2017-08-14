program LuaBattleBots;

{$IFDEF DEBUG}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Vcl.Forms,
  Main in 'Main.pas' {frmMain},
  Game in 'Game.pas',
  CustomModules in 'CustomModules.pas',
  EntityDefine in 'EntityDefine.pas',
  DebugConsoleDefine in 'DebugConsoleDefine.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
