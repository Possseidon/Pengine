program FontLearn;

{$R *.dres}

uses
  Vcl.Forms,
  Main in 'Main.pas' {frmMain},
  StatisticsForm in 'StatisticsForm.pas' {frmStatistics};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmStatistics, frmStatistics);
  Application.Run;
end.

