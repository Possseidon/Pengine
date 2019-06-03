program FactorioCalculator;

uses
  Vcl.Forms,
  Main in 'Main.pas' {frmMain},
  FactoryDefine in 'FactoryDefine.pas',
  RecipeForm in 'RecipeForm.pas' {frmRecipes},
  FactoryFrame in 'FactoryFrame.pas' {frmFactory: TFrame};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmRecipes, frmRecipes);
  Application.Run;
end.

