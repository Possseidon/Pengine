program MCDataPackEditor;

uses
  Vcl.Forms,
  Main in 'Main.pas' {frmMain},
  FrameSynEdit in 'FrameSynEdit.pas' {frmSynEdit: TFrame};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

