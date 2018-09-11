program MinesweeperAI;

uses
  Vcl.Forms,
  Main in 'Main.pas' {frmMain},
  MinesweeperDefine in 'MinesweeperDefine.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

