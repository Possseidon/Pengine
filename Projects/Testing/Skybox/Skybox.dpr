program Skybox;

uses
  Vcl.Forms,
  Main in 'Main.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Run;
end.

