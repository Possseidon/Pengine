program FactorioCalculator;

uses
  Vcl.Forms,
  Main in 'Main.pas' {frmMain},
  FactoryDefine in 'FactoryDefine.pas',
  RecipeForm in 'RecipeForm.pas' {frmRecipes},
  FactoryRendering in 'FactoryRendering.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmRecipes, frmRecipes);
  Application.Run;
end.

