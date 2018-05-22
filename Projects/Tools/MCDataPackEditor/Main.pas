unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShLwApi,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.JSON,
  System.IOUtils,
  System.Zip,
  System.Actions,
  System.ImageList,

  Vcl.Clipbrd,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.ActnList,
  Vcl.ImgList,
  Vcl.ToolWin,

  SynEdit,

  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.Hasher,
  Pengine.TimeManager,
  Pengine.Parser,

  Minecraft.BrigadierParser,
  Minecraft.Brigadier,
  Minecraft.NBT,
  Minecraft.Datapack,

  EditorFrameFunction,
  DatapackView;

type

  TfrmMain = class(TForm)
    alActions: TActionList;
    mmMain: TMainMenu;
    miFile: TMenuItem;
    miAdd: TMenuItem;
    miOpen: TMenuItem;
    Datapack1: TMenuItem;
    N2: TMenuItem;
    Namespace1: TMenuItem;
    miFileDiv1: TMenuItem;
    miExit: TMenuItem;
    Panel1: TPanel;
    tbView: TToolBar;
    actOpen: TAction;
    actExit: TAction;
    dlgOpenDatapack: TOpenDialog;
    tvNamespaces: TTreeView;
    tbNewNamespace: TToolButton;
    actDataTypeFirst: TAction;
    miView: TMenuItem;
    miDataTypeFirst: TMenuItem;
    ilIcons: TImageList;
    ToolButton1: TToolButton;
    actNewNamespace: TAction;
    Datapack2: TMenuItem;
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
    ListView1: TListView;
    Splitter1: TSplitter;
    N5: TMenuItem;
    procedure actCollapseAllExecute(Sender: TObject);
    procedure actCopyNameExecute(Sender: TObject);
    procedure actCopyPathExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actDataTypeFirstExecute(Sender: TObject);
    procedure actEditExecute(Sender: TObject);
    procedure actExpandAllExecute(Sender: TObject);
    procedure actNewDirectoryExecute(Sender: TObject);
    procedure actNewNamespaceExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pcTabsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure pcTabsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure pcTabsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pcTabsMouseLeave(Sender: TObject);
    procedure pcTabsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pcTabsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pcTabsStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure tvNamespacesContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure tvNamespacesMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure tvNamespacesMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FBrigadierRoot: TBrigadierRoot;
    FDatapack: TDatapack;
    FDatapackTreeView: TDatapackTreeView;
    FDraggedPage: TTabSheet;
    FRemoveTab: Integer;

    procedure InitBrigadierSystem;
    procedure InitDataTypes;
    procedure CheckAppParams;

    procedure AddData(Sender: TObject);
    procedure LoadDatapack(AFilename: TFilename);

  public
    property Brigadier: TBrigadierRoot read FBrigadierRoot;

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
  Clipboard.AsText := tvNamespaces.Selected.NodeData.FullName;
end;

procedure TfrmMain.actCopyPathExecute(Sender: TObject);
var
  NodeData: TDatapackTreeView.TNodeData;
begin
  NodeData := tvNamespaces.Selected.NodeData;
  case NodeData.NodeType of
    ntFile, ntDirectory:
      Clipboard.AsText := NodeData.FullPath;
    ntType:
      Clipboard.AsText := NodeData.Namespace.Data[NodeData.DataType].FullPath;
    ntNamespace:
      Clipboard.AsText := NodeData.Namespace.FullPath;
  end;
end;

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.actOpenExecute(Sender: TObject);
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
end;

procedure TfrmMain.LoadDatapack(AFilename: TFilename);
begin
  FDatapack.Free;
  FDatapack := TDatapack.Create(AFilename);
  // TODO: Close all open pages
  FDatapackTreeView.Datapack := FDatapack;
end;

procedure TfrmMain.CheckAppParams;
begin
  if ParamCount >= 1 then
  begin
    LoadDatapack(ParamStr(1));
  end;
end;

procedure TfrmMain.actDataTypeFirstExecute(Sender: TObject);
begin
  FDatapackTreeView.DataTypeFirst := actDataTypeFirst.Checked;
end;

procedure TfrmMain.actEditExecute(Sender: TObject);

  procedure AddRecursive(ANode: TTreeNode);
  begin
    ANode := ANode.GetFirstChild;
    while ANode <> nil do
    begin
      if ANode.NodeData.NodeType = ntFile then
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
    if tvNamespaces.Selections[I].NodeData.NodeType = ntFile then
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

procedure TfrmMain.actNewDirectoryExecute(Sender: TObject);
begin
  // TODO: Add Directory
end;

procedure TfrmMain.actNewNamespaceExecute(Sender: TObject);
begin
  // TODO: Add Namespace
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // SHAutoComplete(edtTestInput.Handle, SHACF_AUTOAPPEND_FORCE_OFF or SHACF_AUTOSUGGEST_FORCE_OFF);
  InitBrigadierSystem;
  InitDataTypes;
  FDatapackTreeView := TDatapackTreeView.Create(tvNamespaces, pcTabs);
  CheckAppParams;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FDatapackTreeView.Free;
  FDatapack.Free;
  FBrigadierRoot.Free;
end;

procedure TfrmMain.InitBrigadierSystem;
var
  CommandTreeText: string;
  CommandTree: TJSONObject;
begin
  CommandTreeText := TFile.ReadAllText('Data/reports/commands.json');
  CommandTree := TJSONObject.ParseJSONValue(CommandTreeText) as TJSONObject;

  try
    FBrigadierRoot := TBrigadierRoot.Create(CommandTree);
  finally
    CommandTree.Free;
  end;

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
    Action.Caption := DPClasses[DataType].GetDisplayName;
    Action.ImageIndex := Ord(DataType);
    Action.Tag := Ord(DataType);
    Action.OnExecute := AddData;
    Action.Hint := 'Add a new ' + DPClasses[DataType].GetDisplayName + '.';

    MenuItem := TMenuItem.Create(miAdd);
    MenuItem.Action := Action;

    miAdd.Add(MenuItem);

    ToolButton := TToolButton.Create(tbView);
    ToolButton.Parent := tbView;
    ToolButton.Action := Action;
    ToolButton.Left := tbView.Buttons[tbView.ButtonCount - 1].Left + 1;

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
    FDraggedPage.PageIndex := pcTabs.PageCount;
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

procedure TfrmMain.pcTabsMouseLeave(Sender: TObject);
begin
  pcTabs.ShowHint := False;
end;

procedure TfrmMain.pcTabsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  TabIndex: Integer;
begin
  TabIndex := pcTabs.IndexOfTabAt(X, Y);
  if (TabIndex = -1) or (pcTabs.Hint = pcTabs.Pages[TabIndex].Hint) then
    Exit;
  Application.CancelHint;
  pcTabs.Hint := pcTabs.Pages[TabIndex].Hint;
  pcTabs.ShowHint := True;
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
          pcTabs.Pages[TabIndex].Free;
        SetCaptureControl(nil);
      end;
  end;
end;

procedure TfrmMain.pcTabsStartDrag(Sender: TObject; var DragObject: TDragObject);
var
  Pos: TPoint;
begin
  Pos := pcTabs.ScreenToClient(Mouse.CursorPos);
  FDraggedPage := pcTabs.Pages[pcTabs.IndexOfTabAt(Pos.X, Pos.Y)];
end;

procedure TfrmMain.tvNamespacesContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  Node: TTreeNode;
  NodeClicked: Boolean;
  I, FileCount, DirCount: Integer;
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
      case tvNamespaces.Selections[I].NodeData.NodeType of
        ntFile:
          Inc(FileCount);
        ntDirectory:
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

end.
