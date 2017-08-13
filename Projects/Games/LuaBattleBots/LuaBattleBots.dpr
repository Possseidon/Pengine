program LuaBattleBots;

uses
  Vcl.Forms,
  Main in 'Main.pas' {frmMain},
  Game in 'Game.pas',
  DebugConsoleDefine in 'DebugConsoleDefine.pas' {DebugConsole},
  CustomModules in 'CustomModules.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
