program FactorioCalculator;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form9},
  FactoryDefine in 'FactoryDefine.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm9, Form9);
  Application.Run;
end.

