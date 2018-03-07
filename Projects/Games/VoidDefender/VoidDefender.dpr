program VoidDefender;

uses
  Vcl.Forms,
  Main in 'Main.pas' {frmMain},
  VD.Entity in 'VD.Entity.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

