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
  ConditionFrameEntityProperties in 'EditorFrames\LootTableFrames\ConditionFrames\ConditionFrameEntityProperties.pas' {frmConditionEntityProperties: TFrame},
  ToolFunctionPreferences in 'Tools\ToolFunctionPreferences.pas' {frmFunctionPreferences},
  TextAttributeFrame in 'Tools\HelperFrames\TextAttributeFrame.pas' {frmHighlighterAttributes: TFrame},
  ParserHighlightingFrame in 'Tools\HelperFrames\ParserHighlightingFrame.pas' {frmParserHighlighting: TFrame},
  FunctionTheme in 'Tools\FunctionTheme.pas',
  SynEditorFrame in 'Tools\HelperFrames\SynEditorFrame.pas' {frmSynEditor: TFrame},
  LightThemePreset in 'ThemePresets\LightThemePreset.pas',
  Vcl.Themes,
  Vcl.Styles,
  SettingsForm in 'Tools\SettingsForm.pas' {frmSettings},
  SynEdit,
  System.SysUtils,
  Vcl.StdCtrls {frmSettings},
  EditorFrameTag in 'EditorFrames\EditorFrameTag.pas' {frmEditorTags: TFrame},
  EditorFrameAdvancement in 'EditorFrames\EditorFrameAdvancement.pas' {frmEditorAdvancements: TFrame},
  EditorFrameRecipe in 'EditorFrames\EditorFrameRecipe.pas' {frmEditorRecipes: TFrame},
  EditorFrameStructure in 'EditorFrames\EditorFrameStructure.pas' {frmEditorStructures: TFrame};

{$R *.res}

begin
  ChDir(ExtractFilePath(Application.ExeName));
  TStyleManager.Engine.RegisterStyleHook(TCustomSynEdit, TMemoStyleHook);
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmFunctionPreferences, frmFunctionPreferences);
  Application.CreateForm(TfrmSettings, frmSettings);
  Application.Run;
end.

