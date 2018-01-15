program RopeRenderer;

{$R *.dres}

uses
  Vcl.Forms,
  Main in 'Main.pas' {frmMain},
  RopeDefine in 'RopeDefine.pas',
  ModelShader in 'ModelShader.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

