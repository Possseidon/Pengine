program LuaBattleBots;

uses
  Vcl.Forms,
  Windows,
  DebugConsoleDefine,
  Main in 'Main.pas' {frmMain},
  Game in 'Game.pas',
  CustomModules in 'CustomModules.pas',
  EntityDefine in 'EntityDefine.pas',
  Resources in 'Resources.pas';

{$R *.res}

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

  DebugWriteLine('--- Initializing everything...');

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  DebugWriteLine('--- Everything initialized!');

  Application.Run;

  DebugWriteLine('--- Application stopped');
end.
