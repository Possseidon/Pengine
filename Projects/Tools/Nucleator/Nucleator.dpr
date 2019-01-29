program Nucleator;

uses
  Vcl.Forms,
  Main in 'Main.pas' {frmMain},
  PreviewFrame in 'PreviewFrame.pas' {frmPreview: TFrame},
  ReactorDefine in 'ReactorDefine.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
