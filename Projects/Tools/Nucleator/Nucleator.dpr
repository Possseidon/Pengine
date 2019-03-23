program Nucleator;

{$R *.dres}

uses
  Vcl.Forms,
  Main in 'Main.pas' {frmMain},
  PreviewFrame in 'PreviewFrame.pas' {frmPreview: TFrame},
  ReactorDefine in 'ReactorDefine.pas',
  SettingsDialog in 'SettingsDialog.pas' {frmSettings},
  ReactorEvolutionDefine in 'ReactorEvolutionDefine.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmSettings, frmSettings);
  Application.Run;
end.
