program Skybox;

uses
  Vcl.Forms,
  Main in 'Main.pas' {frmMain},
  Pengine.HashCollections in '..\..\..\Libraries\Utility\Pengine.HashCollections.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

