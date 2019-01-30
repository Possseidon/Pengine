program Nucleator;

uses
  Vcl.Forms,
  Main in 'Main.pas' {frmMain},
  PreviewFrame in 'PreviewFrame.pas' {frmPreview: TFrame},
  ReactorDefine in 'ReactorDefine.pas',
  InitializationDialog in 'InitializationDialog.pas' {frmInitialization},
  ReactorEvolutionDefine in 'ReactorEvolutionDefine.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmInitialization, frmInitialization);
  Application.Run;
end.
