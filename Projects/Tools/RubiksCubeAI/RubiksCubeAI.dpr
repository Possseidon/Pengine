program RubiksCubeAI;

uses
  Vcl.Forms,
  Main in 'Main.pas' {frmRubiksCubeAI},
  RubiksCube in 'RubiksCube.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmRubiksCubeAI, frmRubiksCubeAI);
  Application.Run;
end.

