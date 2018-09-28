program BrigadierFunctionParser;

uses
  Vcl.Forms,
  Main in 'Main.pas' {frmMain},
  CodeSuggestionBox in 'CodeSuggestionBox.pas' {frmSuggestionBox};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmSuggestionBox, frmSuggestionBox);
  Application.Run;
end.

