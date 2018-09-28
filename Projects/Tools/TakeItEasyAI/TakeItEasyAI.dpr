program TakeItEasyAI;



{$R *.dres}

uses
  Vcl.Forms,
  TakeItEasy.Main in 'TakeItEasy.Main.pas' {frmMain},
  TakeItEasy.Game in 'TakeItEasy.Game.pas',
  TakeItEasy.Control in 'TakeItEasy.Control.pas',
  TakeitEasy.NeuralNet in 'TakeitEasy.NeuralNet.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

