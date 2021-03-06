unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShellApi,
  Winapi.ShlObj,

  System.SysUtils,
  System.IOUtils,
  System.Actions,
  System.ImageList,
  System.Classes,

  Vcl.Forms,
  Vcl.Menus,
  Vcl.ImgList,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.ActnList,
  Vcl.ComCtrls,
  Vcl.ToolWin,
  Vcl.ExtCtrls,
  Vcl.ClipBrd,
  Vcl.Graphics,

  GdiPlus,

  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.Hasher,
  Pengine.TimeManager,
  Pengine.Parsing,
  Pengine.Settings,

  Pengine.MC.BrigadierParser,
  Pengine.MC.Brigadier,
  Pengine.MC.NBT,
  Pengine.MC.Datapack,
  Pengine.MC.Item,
  Pengine.MC.BlockState,
  Pengine.MC.Assets,
  Pengine.MC.ItemIcons,
  Pengine.MC.BlockRenderer,

  DatapackView,
  ToolFunctionPreferences,
  EditorFrameFunction,
  FunctionTheme,
  LightThemePreset,
  SettingsForm,
  StartupView,
  Vcl.StdCtrls;

type

  TfrmMain = class(TForm)
    alActions: TActionList;
    mmMain: TMainMenu;
    miFile: TMenuItem;
    miAdd: TMenuItem;
    miOpen: TMenuItem;
    Datapack1: TMenuItem;
    Namespace1: TMenuItem;
    miFileDiv1: TMenuItem;
    miExit: TMenuItem;
    Panel1: TPanel;
    tbView: TToolBar;
    actOpenDatapack: TAction;
    actExit: TAction;
    dlgOpenDatapack: TOpenDialog;
    tvDatapacks: TTreeView;
    tbNewNamespace: TToolButton;
    ilIcons: TImageList;
    ToolButton1: TToolButton;
    actNewNamespace: TAction;
    actNewDatapack: TAction;
    sbMain: TStatusBar;
    splView: TSplitter;
    pmView: TPopupMenu;
    actCopyPath: TAction;
    Copyfullpath1: TMenuItem;
    actCollapseNamespaces: TAction;
    actCollapseTypes: TAction;
    actCollapseAll: TAction;
    actExpandAll: TAction;
    actExpandNamespaces: TAction;
    actExpandTypes: TAction;
    Expand1: TMenuItem;
    actExpandAll1: TMenuItem;
    actExpandNamespaces1: TMenuItem;
    actExpandTypes1: TMenuItem;
    actCollapseAll1: TMenuItem;
    actCollapseAll2: TMenuItem;
    actCollapseTypes1: TMenuItem;
    N1: TMenuItem;
    actEdit: TAction;
    actEdit1: TMenuItem;
    N3: TMenuItem;
    actCopyName: TAction;
    Copynametoclipboard1: TMenuItem;
    N4: TMenuItem;
    pcTabs: TPageControl;
    tbNewDirectory: TToolButton;
    actNewDirectory: TAction;
    Directory1: TMenuItem;
    actOpenInExplorer: TAction;
    OpeninExplorer1: TMenuItem;
    ToolButton2: TToolButton;
    actRefresh: TAction;
    actSave: TAction;
    actSaveAll: TAction;
    Save1: TMenuItem;
    Saveall1: TMenuItem;
    actRename: TAction;
    Rename1: TMenuItem;
    actDelete: TAction;
    Delete1: TMenuItem;
    N2: TMenuItem;
    Datapack2: TMenuItem;
    N5: TMenuItem;
    Refresh1: TMenuItem;
    N7: TMenuItem;
    ToolButton3: TToolButton;
    ools1: TMenuItem;
    Formatmcfunction1: TMenuItem;
    Preferences1: TMenuItem;
    FormatcurrentFile1: TMenuItem;
    FormatAllFiles1: TMenuItem;
    actFormatCurrent: TAction;
    actFormatAll: TAction;
    actFunctionPreferences: TAction;
    pmTabs: TPopupMenu;
    actCloseTab: TAction;
    actCloseAllOtherTabs: TAction;
    Closetab1: TMenuItem;
    Closeallothertabs1: TMenuItem;
    actOpenVanilla: TAction;
    OpenVanilla1: TMenuItem;
    RecentlyUsed1: TMenuItem;
    Close1: TMenuItem;
    N8: TMenuItem;
    actCloseDatapack: TAction;
    miAdd2: TMenuItem;
    Directory2: TMenuItem;
    Namespace2: TMenuItem;
    N9: TMenuItem;
    N10: TMenuItem;
    procedure actCloseAllOtherTabsExecute(Sender: TObject);
    procedure actCloseDatapackExecute(Sender: TObject);
    procedure actCloseTabExecute(Sender: TObject);
    procedure actCollapseAllExecute(Sender: TObject);
    procedure actCopyNameExecute(Sender: TObject);
    procedure actCopyNameUpdate(Sender: TObject);
    procedure actCopyPathExecute(Sender: TObject);
    procedure actCopyPathUpdate(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actDeleteUpdate(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actOpenDatapackExecute(Sender: TObject);
    procedure actEditExecute(Sender: TObject);
    procedure actExpandAllExecute(Sender: TObject);
    procedure actFormatCurrentExecute(Sender: TObject);
    procedure actFormatCurrentUpdate(Sender: TObject);
    procedure actFunctionPreferencesExecute(Sender: TObject);
    procedure actNewDatapackExecute(Sender: TObject);
    procedure actNewDirectoryExecute(Sender: TObject);
    procedure actNewDirectoryUpdate(Sender: TObject);
    procedure actNewNamespaceExecute(Sender: TObject);
    procedure actNewNamespaceUpdate(Sender: TObject);
    procedure actOpenInExplorerExecute(Sender: TObject);
    procedure actOpenInExplorerUpdate(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actRefreshUpdate(Sender: TObject);
    procedure actRenameExecute(Sender: TObject);
    procedure actSaveAllExecute(Sender: TObject);
    procedure actSaveAllUpdate(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSaveUpdate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pcTabsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure pcTabsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure pcTabsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pcTabsMouseEnter(Sender: TObject);
    procedure pcTabsMouseLeave(Sender: TObject);
    procedure pcTabsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pcTabsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pcTabsStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure tvDatapacksContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure tvDatapacksMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure tvDatapacksMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FDatapackTreeView: TDatapackTreeView;
    FDraggedPage: TTabSheet;
    FRemoveTab: Integer;
    FAddActions: array [TDatapack.TDataType] of TAction;
    FFunctionTheme: TFunctionTheme;

    procedure InitDataTypes;
    procedure InitTheme;
    procedure CheckAppParams;
    procedure ExceptionLagFix; inline;
    procedure LoadSettings;

    procedure AppActivate(Sender: TObject);

    procedure AddData(Sender: TObject);
    procedure LoadDatapack(AFilename: TFilename);

    function DatapackOpen: Boolean;

  protected
    procedure UpdateActions; override;

  public
    property FunctionTheme: TFunctionTheme read FFunctionTheme;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}


procedure TfrmMain.actCloseAllOtherTabsExecute(Sender: TObject);
var
  I, ActivePage: Integer;
begin
  ActivePage := pcTabs.ActivePageIndex;
  for I := pcTabs.PageCount - 1 downto 0 do
    if I <> ActivePage then
      pcTabs.Pages[I].Editor.Close;
end;

procedure TfrmMain.actCloseDatapackExecute(Sender: TObject);
begin
  if tvDatapacks.Selected = nil then
    Exit;
  FDatapackTreeView.DatapackCollection.Remove(tvDatapacks.Selected.NodeData.Datapack);
end;

procedure TfrmMain.actCloseTabExecute(Sender: TObject);
begin
  if pcTabs.ActivePage <> nil then
    pcTabs.ActivePage.Editor.Close;
end;

procedure TfrmMain.actCollapseAllExecute(Sender: TObject);
begin
  FDatapackTreeView.CollapseAll;
end;

procedure TfrmMain.actCopyNameExecute(Sender: TObject);
begin
  if tvDatapacks.Selected = nil then
    Exit;
  Clipboard.AsText := tvDatapacks.Selected.NodeData.NamespacePath;
end;

procedure TfrmMain.actCopyNameUpdate(Sender: TObject);
begin
  actCopyName.Enabled := tvDatapacks.Focused and
    (tvDatapacks.Selected <> nil) and
    (tvDatapacks.Selected.NodeData <> nil) and
    tvDatapacks.Selected.NodeData.HasNamespacePath;
end;

procedure TfrmMain.actCopyPathExecute(Sender: TObject);
begin
  Clipboard.AsText := tvDatapacks.Selected.NodeData.FullPath;
end;

procedure TfrmMain.actCopyPathUpdate(Sender: TObject);
begin
  actCopyPath.Enabled := tvDatapacks.Focused and (tvDatapacks.Selected <> nil);
end;

procedure TfrmMain.actDeleteExecute(Sender: TObject);

  function HasSelectedParent(ANode: TTreeNode): Boolean;
  begin
    Result := (ANode.Parent <> nil) and (ANode.Parent.Selected or HasSelectedParent(ANode.Parent));
  end;

var
  I: Cardinal;
  Selected: TRefArray<TTreeNode>;
  Node: TTreeNode;
begin
  Selected := TRefArray<TTreeNode>.Create;

  try
    for I := 0 to tvDatapacks.SelectionCount - 1 do
      if not HasSelectedParent(tvDatapacks.Selections[I]) then
        Selected.Add(tvDatapacks.Selections[I]);

    for Node in Selected do
    begin
      Node.MakeVisible;
      Node.NodeData.Delete;
    end;

  finally
    Selected.Free;
  end;

end;

procedure TfrmMain.actDeleteUpdate(Sender: TObject);
begin
  actDelete.Enabled := tvDatapacks.Focused and (tvDatapacks.Selected <> nil);
end;

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.actOpenDatapackExecute(Sender: TObject);
begin
  if dlgOpenDatapack.Execute then
    LoadDatapack(dlgOpenDatapack.FileName);
end;

procedure TfrmMain.AddData(Sender: TObject);
var
  DataType: TDatapack.TDataType;
begin
  DataType := TDatapack.TDataType(TComponent(Sender).Tag);
  FDatapackTreeView.AddData(DataType);
end;

procedure TfrmMain.AppActivate(Sender: TObject);
begin
  FDatapackTreeView.PageControl.UpdateOpenPages;
end;

procedure TfrmMain.LoadDatapack(AFilename: TFilename);
begin
  FDatapackTreeView.DatapackCollection.Add(AFilename);
end;

procedure TfrmMain.LoadSettings;
const

  SettingsClasses: array [0 .. 6] of TSettingsClass = (
    TAssetsSettings,
    TItemSettings,
    TItemTagSettings,
    TBlockSettings,
    TBlockTagSettings,
    TBrigadierSettings,
    TItemIconSettings
    );

var
  SettingsClass: TSettingsClass;
begin
  for SettingsClass in SettingsClasses do
    RootSettingsG.Preload(SettingsClass);
end;

procedure TfrmMain.CheckAppParams;
begin
  if ParamCount >= 1 then
  begin
    LoadDatapack(ParamStr(1));
  end;
end;

function TfrmMain.DatapackOpen: Boolean;
begin
  Result := not FDatapackTreeView.DatapackCollection.Datapacks.Empty;
end;

procedure TfrmMain.ExceptionLagFix;
begin
  // The first time, this exception is raised, the raising takes a little longer with the debugger...
  // So let's do it at the start so it's not anoying while typing.

  {$IFDEF DEBUG}
  try
    raise EParseError.Create('');
  except
  end;

  {$ENDIF}

end;

procedure TfrmMain.actEditExecute(Sender: TObject);

  procedure AddRecursive(ANode: TTreeNode);
  begin
    ANode := ANode.GetFirstChild;
    while ANode <> nil do
    begin
      if ANode.NodeData is TDatapack.TFile then
        FDatapackTreeView.PageControl.Open(ANode)
      else
        AddRecursive(ANode);
      ANode := ANode.GetNextSibling;
    end;
  end;

var
  I: Integer;
begin
  pcTabs.DisableAlign;
  for I := 0 to tvDatapacks.SelectionCount - 1 do
  begin
    if tvDatapacks.Selections[I].NodeData is TDatapack.TFile then
      FDatapackTreeView.PageControl.Open(tvDatapacks.Selections[I])
    else
      AddRecursive(tvDatapacks.Selections[I]);
  end;
  pcTabs.EnableAlign;
end;

procedure TfrmMain.actExpandAllExecute(Sender: TObject);
begin
  FDatapackTreeView.ExpandAll;
end;

procedure TfrmMain.actFormatCurrentExecute(Sender: TObject);
begin
  TEditorFunctions(FDatapackTreeView.PageControl.CurrentPage.Editor).Format;
end;

procedure TfrmMain.actFormatCurrentUpdate(Sender: TObject);
begin
  actFormatCurrent.Enabled := FDatapackTreeView.PageControl.IsEditing(dtFunction);
end;

procedure TfrmMain.actFunctionPreferencesExecute(Sender: TObject);
begin
  frmFunctionPreferences.Execute(FFunctionTheme);
end;

procedure TfrmMain.actNewDatapackExecute(Sender: TObject);
begin
  raise ENotImplemented.Create('New Datapack is not yet implemented.');
end;

procedure TfrmMain.actNewDirectoryExecute(Sender: TObject);
begin
  FDatapackTreeView.AddDirectory;
end;

procedure TfrmMain.actNewDirectoryUpdate(Sender: TObject);
begin
  actNewDirectory.Enabled := FDatapackTreeView.CanAddDirectory;
end;

procedure TfrmMain.actNewNamespaceExecute(Sender: TObject);
begin
  FDatapackTreeView.AddNamespace();
end;

procedure TfrmMain.actNewNamespaceUpdate(Sender: TObject);
begin
  actNewNamespace.Enabled := DatapackOpen;
end;

procedure TfrmMain.actOpenInExplorerExecute(Sender: TObject);
var
  ItemIDList: PItemIDList;
  NodeData: TDatapackBase;
  FileData: TDatapack.TFile;
begin
  NodeData := tvDatapacks.Selected.NodeData;
  if NodeData is TDatapack.TFile then
  begin
    FileData := TDatapack.TFile(NodeData);
    if FileData.FileExists then
    begin
      ItemIDList := ILCreateFromPath(PChar(FileData.FullPath));
      SHOpenFolderAndSelectItems(ItemIDList, 0, nil, 0);
      ILFree(ItemIDList);
      if GetLastError <> 0 then
        RaiseLastOSError;
      Exit;
    end
    else
      NodeData := FileData.Parent;
  end;

  ShellExecute(Application.Handle, nil, PChar(NodeData.FullPath), nil, nil, SW_NORMAL);
  if GetLastError <> 0 then
    RaiseLastOSError;

end;

procedure TfrmMain.actOpenInExplorerUpdate(Sender: TObject);
begin
  actCopyPath.Update;
  actOpenInExplorer.Enabled := actCopyPath.Enabled;
end;

procedure TfrmMain.actRefreshExecute(Sender: TObject);
begin
  // StartTimer;
  FDatapackTreeView.Update;
  // ShowMessage(Format('Refresh took %s', [StopTimerGetString]));
end;

procedure TfrmMain.actRefreshUpdate(Sender: TObject);
begin
  actRefresh.Enabled := DatapackOpen;
end;

procedure TfrmMain.actRenameExecute(Sender: TObject);
begin
  tvDatapacks.Selected.EditText;
end;

procedure TfrmMain.actSaveAllExecute(Sender: TObject);
begin
  FDatapackTreeView.PageControl.SaveAll;
end;

procedure TfrmMain.actSaveAllUpdate(Sender: TObject);
begin
  actSaveAll.Enabled := DatapackOpen;
end;

procedure TfrmMain.actSaveExecute(Sender: TObject);
begin
  FDatapackTreeView.PageControl.SaveCurrent;
end;

procedure TfrmMain.actSaveUpdate(Sender: TObject);
begin
  actSave.Enabled := DatapackOpen;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  while pcTabs.ActivePage <> nil do
    if not pcTabs.ActivePage.Editor.Close then
    begin
      CanClose := False;
      Break;
    end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // SHAutoComplete(edtTestInput.Handle, SHACF_AUTOAPPEND_FORCE_OFF or SHACF_AUTOSUGGEST_FORCE_OFF);
  InitDataTypes;
  InitTheme;
  ExceptionLagFix;
  FDatapackTreeView := TDatapackTreeView.Create(tvDatapacks, pcTabs);
  CheckAppParams;
  Application.OnActivate := AppActivate;
  LoadSettings;

  // for var S in TDirectory.GetFiles('C:\Users\Dominik\Documents\Test Datapack\data\minecraft\recipes') do
  {
    for var S in TDirectory.GetFiles('C:\Users\Dominik\Desktop\Server\Test\generated\data\minecraft\recipes') do
    begin
    var JRecipe := TJObject.CreateFromFile(S);

    if JRecipe['type'] = RecipeNames[TRecipeSmelting.GetType] then
    if JRecipe['group'].Exists then
    raise Exception.Create('Yeah boi!');

    try
    var Recipe := TRecipe.CreateTyped(JRecipe);
    finally
    JRecipe.Free;
    end;
    end;
  }
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FFunctionTheme.Free;
  FDatapackTreeView.Free;
end;

procedure TfrmMain.InitDataTypes;
var
  DataType: TDatapack.TDataType;
  MenuItem: TMenuItem;
  Action: TAction;
  ToolButton: TToolButton;
begin
  for DataType := Low(TDatapack.TDataType) to High(TDatapack.TDataType) do
  begin
    Action := TAction.Create(alActions);
    Action.Caption := DPClasses[DataType].GetDisplayNameNormal;
    Action.ImageIndex := Ord(DataType);
    Action.Tag := Ord(DataType);
    Action.OnExecute := AddData;
    // Action.Hint := 'Add a new ' + DPClasses[DataType].GetDisplayNameNormal + '.';

    FAddActions[DataType] := Action;

    MenuItem := TMenuItem.Create(miAdd);
    MenuItem.Action := Action;
    miAdd.Add(MenuItem);

    MenuItem := TMenuItem.Create(miAdd2);
    MenuItem.Action := Action;
    miAdd2.Add(MenuItem);

    ToolButton := TToolButton.Create(tbView);
    ToolButton.Left := tbView.Buttons[tbView.ButtonCount - 1].Left + 1;
    ToolButton.Parent := tbView;
    ToolButton.Action := Action;

  end;
end;

procedure TfrmMain.InitTheme;
begin
  FFunctionTheme := TFunctionTheme.Create;
  FFunctionTheme.LoadPreset(TLightTheme);
end;

procedure TfrmMain.pcTabsDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Index: Integer;
begin
  Index := pcTabs.IndexOfTabAt(X, Y);
  if Index <> -1 then
    FDraggedPage.PageIndex := Index
  else
    FDraggedPage.PageIndex := pcTabs.PageCount - 1;
end;

procedure TfrmMain.pcTabsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  I: Integer;
begin
  Accept := FDraggedPage is TTabSheet;
  if Accept then
  begin
    // Do it twice, to prevent flickering when moving shorter name which switches it back
    for I := 0 to 1 do
      pcTabsDragDrop(Self, Source, X, Y);
  end;
end;

procedure TfrmMain.pcTabsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TabIndex: Integer;
begin
  case Button of
    mbLeft:
      pcTabs.BeginDrag(False);
    mbMiddle:
      begin
        TabIndex := pcTabs.IndexOfTabAt(X, Y);
        if TabIndex = -1 then
          Exit;
        SetCaptureControl(pcTabs);
        FRemoveTab := TabIndex;
      end;
    mbRight:
      pcTabs.ActivePageIndex := pcTabs.IndexOfTabAt(X, Y);
  end;
end;

procedure TfrmMain.pcTabsMouseEnter(Sender: TObject);
begin
  pcTabs.ShowHint := True;
end;

procedure TfrmMain.pcTabsMouseLeave(Sender: TObject);
begin
  pcTabs.ShowHint := False;
end;

procedure TfrmMain.pcTabsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  TabIndex: Integer;
  NewHint: string;
begin
  TabIndex := pcTabs.IndexOfTabAt(X, Y);
  if TabIndex = -1 then
    Exit;
  NewHint := pcTabs.Pages[TabIndex].NodeData.NamespacePath;
  if (TabIndex = -1) or (pcTabs.Hint = NewHint) then
    Exit;
  Application.CancelHint;
  pcTabs.Hint := NewHint;
end;

procedure TfrmMain.pcTabsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TabIndex: Integer;
begin
  case Button of
    mbMiddle:
      begin
        TabIndex := pcTabs.IndexOfTabAt(X, Y);
        if TabIndex = FRemoveTab then
          pcTabs.Pages[TabIndex].Editor.Close;
        SetCaptureControl(nil);
      end;
  end;
end;

procedure TfrmMain.pcTabsStartDrag(Sender: TObject; var DragObject: TDragObject);
var
  Pos: TPoint;
  TabIndex: Integer;
begin
  Pos := pcTabs.ScreenToClient(Mouse.CursorPos);
  TabIndex := pcTabs.IndexOfTabAt(Pos.X, Pos.Y);
  if TabIndex <> -1 then
    FDraggedPage := pcTabs.Pages[TabIndex];
end;

procedure TfrmMain.tvDatapacksContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  HitTest: THitTests;
  Node: TTreeNode;
begin
  HitTest := tvDatapacks.GetHitTestInfoAt(MousePos.X, MousePos.Y);
  if not(htOnItem in HitTest) then
  begin
    Handled := True;
    Exit;
  end;

  Node := tvDatapacks.GetNodeAt(MousePos.X, MousePos.Y);
  if Node = nil then
  begin
    Handled := True;
    Exit;
  end;

  Node.Selected := True;

end;

procedure TfrmMain.tvDatapacksMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetCaptureControl(tvDatapacks);
end;

procedure TfrmMain.tvDatapacksMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetCaptureControl(nil);
  if not tvDatapacks.MouseInClient then
    Exit;
  if Shift - [ssShift, ssCtrl] <> Shift then
    Exit;
  if tvDatapacks.GetHitTestInfoAt(X, Y) <= [htNowhere, htOnRight, htOnIndent] then
    tvDatapacks.ClearSelection;
end;

procedure TfrmMain.UpdateActions;
var
  DataType: TDatapack.TDataType;
  CanAdd: Boolean;
begin
  inherited;
  for DataType := Low(TDatapack.TDataType) to High(TDatapack.TDataType) do
  begin
    CanAdd := FDatapackTreeView.CanAddData(DataType);
    FAddActions[DataType].Visible := CanAdd;
    FAddActions[DataType].Enabled := CanAdd;
  end;
end;

end.
