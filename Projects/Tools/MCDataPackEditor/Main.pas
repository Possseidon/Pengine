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

  // TODO: remove
  Pengine.MC.ItemIcons,

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

  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.Hasher,
  Pengine.TimeManager,
  Pengine.Parser,
  Pengine.Settings,

  Pengine.MC.BrigadierParser,
  Pengine.MC.Brigadier,
  Pengine.MC.NBT,
  Pengine.MC.Datapack,

  DatapackView,
  ToolFunctionPreferences,
  EditorFrameFunction,
  FunctionTheme,
  LightThemePreset,
  SettingsForm,
  Pengine.MC.Item,
  Vcl.Imaging.pngimage,
  Pengine.MC.BlockState;

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
    tvNamespaces: TTreeView;
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
    N6: TMenuItem;
    FormatcurrentFile1: TMenuItem;
    FormatAllFiles1: TMenuItem;
    actFormatCurrent: TAction;
    actFormatAll: TAction;
    actFunctionPreferences: TAction;
    PaintBox1: TPaintBox;
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
    procedure actRenameUpdate(Sender: TObject);
    procedure actSaveAllExecute(Sender: TObject);
    procedure actSaveAllUpdate(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSaveUpdate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure pcTabsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure pcTabsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure pcTabsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pcTabsMouseEnter(Sender: TObject);
    procedure pcTabsMouseLeave(Sender: TObject);
    procedure pcTabsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pcTabsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pcTabsStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure tvNamespacesContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure tvNamespacesMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure tvNamespacesMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FDatapack: TDatapack;
    FDatapackTreeView: TDatapackTreeView;
    FDraggedPage: TTabSheet;
    FRemoveTab: Integer;
    FAddActions: array [TDatapack.TDataType] of TAction;
    FFunctionTheme: TFunctionTheme;

    procedure InitDataTypes;
    procedure InitTheme;
    procedure CheckAppParams;
    procedure ExceptionLagFix; inline;

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


procedure TfrmMain.actCollapseAllExecute(Sender: TObject);
begin
  FDatapackTreeView.CollapseAll;
end;

procedure TfrmMain.actCopyNameExecute(Sender: TObject);
begin
  Clipboard.AsText := tvNamespaces.Selected.NodeData.NamespacePath;
end;

procedure TfrmMain.actCopyNameUpdate(Sender: TObject);
begin
  actCopyName.Enabled := tvNamespaces.Focused and
    (tvNamespaces.Selected <> nil) and
    (tvNamespaces.Selected.NodeData <> nil) and
    tvNamespaces.Selected.NodeData.HasNamespacePath;
end;

procedure TfrmMain.actCopyPathExecute(Sender: TObject);
begin
  Clipboard.AsText := tvNamespaces.Selected.NodeData.FullPath;
end;

procedure TfrmMain.actCopyPathUpdate(Sender: TObject);
begin
  actCopyPath.Enabled := tvNamespaces.Focused and (tvNamespaces.Selected <> nil);
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
    for I := 0 to tvNamespaces.SelectionCount - 1 do
      if not HasSelectedParent(tvNamespaces.Selections[I]) then
        Selected.Add(tvNamespaces.Selections[I]);

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
  actDelete.Enabled := tvNamespaces.Focused and (tvNamespaces.Selected <> nil);
end;

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.actOpenDatapackExecute(Sender: TObject);
begin
  if dlgOpenDatapack.Execute then
  begin
    LoadDatapack(dlgOpenDatapack.FileName);
  end;
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
  FDatapackTreeView.Datapack := nil;
  FDatapack.Free;
  FDatapack := TDatapack.Create(AFilename);
  FDatapackTreeView.Datapack := FDatapack;
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
  Result := FDatapack <> nil;
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
  for I := 0 to tvNamespaces.SelectionCount - 1 do
  begin
    if tvNamespaces.Selections[I].NodeData is TDatapack.TFile then
      FDatapackTreeView.PageControl.Open(tvNamespaces.Selections[I])
    else
      AddRecursive(tvNamespaces.Selections[I]);
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
  FDatapackTreeView.AddNamespace;
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
  NodeData := tvNamespaces.Selected.NodeData;
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
  FDatapackTreeView.UpdateDatapack;
  // ShowMessage(Format('Refresh took %s', [StopTimerGetString]));
end;

procedure TfrmMain.actRefreshUpdate(Sender: TObject);
begin
  actRefresh.Enabled := DatapackOpen;
end;

procedure TfrmMain.actRenameExecute(Sender: TObject);
begin
  tvNamespaces.Selected.EditText;
end;

procedure TfrmMain.actRenameUpdate(Sender: TObject);
var
  AllowEdit: Boolean;
begin
  if tvNamespaces.Selected <> nil then
    tvNamespaces.OnEditing(nil, tvNamespaces.Selected, AllowEdit)
  else
    AllowEdit := False;
  actRename.Enabled := AllowEdit;
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
  FDatapackTreeView := TDatapackTreeView.Create(tvNamespaces, pcTabs);
  CheckAppParams;
  Application.OnActivate := AppActivate;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FFunctionTheme.Free;
  FDatapackTreeView.Free;
  FDatapack.Free;
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

procedure TfrmMain.PaintBox1Paint(Sender: TObject);
var
  Item: TItemType;
  Block: TBlockType;
  X, Y: Integer;
  Image: TPngImage;
begin
  X := 0;
  Y := 0;
  for Item in RootSettings.Get<TItemSettings>.Items.Order do
  begin
    if not RootSettings.Get<TItemIconSettings>.ItemIcons.Get(Item, Image) then
      PaintBox1.Canvas.Rectangle(X, Y, X + 15, Y + 15)
    else
      PaintBox1.Canvas.Draw(X, Y, Image);
    Inc(X, 16);
    if X > 800 then
    begin
      X := 0;
      Inc(Y, 16);
    end;
  end;

  Y := Y + 20;
  X := 0;

  for Block in RootSettings.Get<TBlockSettings>.Blocks.Order do
  begin
    if not RootSettings.Get<TItemIconSettings>.BlockIcons.Get(Block, Image) then
      PaintBox1.Canvas.Rectangle(X, Y, X + 15, Y + 15)
    else
      PaintBox1.Canvas.Draw(X, Y, Image);
    Inc(X, Image.Width);
    if X > 800 then
    begin
      X := 0;
      Inc(Y, Image.Height);
    end;
  end;
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

procedure TfrmMain.tvNamespacesContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  Node: TTreeNode;
  NodeClicked: Boolean;
  I, FileCount, DirCount: Integer;
  NodeData: TDatapackBase;
begin
  if not(htOnLabel in tvNamespaces.GetHitTestInfoAt(MousePos.X, MousePos.Y)) then
    Exit;

  Node := tvNamespaces.GetNodeAt(MousePos.X, MousePos.Y);
  NodeClicked := Node <> nil;
  if NodeClicked then
  begin
    tvNamespaces.Selected := Node;
  end;

  if NodeClicked then
  begin
    FileCount := 0;
    DirCount := 0;
    for I := 0 to tvNamespaces.SelectionCount - 1 do
    begin
      NodeData := tvNamespaces.Selections[I].NodeData;
      if NodeData is TDatapack.TFile then
        Inc(FileCount)
      else if NodeData is TDatapack.TDirectory then
        Inc(DirCount);
    end;
    actCopyName.Enabled := (tvNamespaces.SelectionCount = 1) and (FileCount + DirCount = 1);
  end
  else
  begin
    actCopyName.Enabled := False;
  end;

  actCopyPath.Enabled := tvNamespaces.SelectionCount = 1;
end;

procedure TfrmMain.tvNamespacesMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetCaptureControl(tvNamespaces);
end;

procedure TfrmMain.tvNamespacesMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetCaptureControl(nil);
  if not tvNamespaces.MouseInClient then
    Exit;
  if Shift - [ssShift, ssCtrl] <> Shift then
    Exit;
  if tvNamespaces.GetHitTestInfoAt(X, Y) <= [htNowhere, htOnRight, htOnIndent] then
    tvNamespaces.ClearSelection;
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
