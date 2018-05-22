program MCDataPackEditor;

uses
  Vcl.Forms,
  Main in 'Main.pas' {frmMain},
  EditorFrameFunction in 'EditorFrames\EditorFrameFunction.pas' {frmEditorFunctions: TFrame},
  DatapackView in 'DatapackView.pas',
  EditorFrameLootTable in 'EditorFrames\EditorFrameLootTable.pas' {frmEditorLootTables: TFrame},
  LootTableFramePool in 'EditorFrames\LootTableFrames\LootTableFramePool.pas' {frmPool: TFrame},
  HelperFrameIntBounds in 'EditorFrames\HelperFrames\HelperFrameIntBounds.pas' {frmHelperIntBounds: TFrame},
  HelperFrameBounds in 'EditorFrames\HelperFrames\HelperFrameBounds.pas' {frmHelperBounds: TFrame},
  LootTableFrameCondition in 'EditorFrames\LootTableFrames\LootTableFrameCondition.pas' {frmCondition: TFrame},
  ConditionFrameEntityProperties in 'EditorFrames\LootTableFrames\ConditionFrames\ConditionFrameEntityProperties.pas' {frmConditionEntityProperties: TFrame};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

