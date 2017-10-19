program Dandelifeon;

uses
  Vcl.Forms,
  Main in 'Main.pas' {frmMain},
  GameOfLifeDefine in 'GameOfLifeDefine.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

