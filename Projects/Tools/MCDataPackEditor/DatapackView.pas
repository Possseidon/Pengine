unit DatapackView;

interface

uses
  Winapi.Windows,

  System.SysUtils,
  System.Types,
  System.Classes,
  System.UITypes,

  Vcl.ComCtrls,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,

  Pengine.Collections,

  Pengine.MC.Datapack;

type

  // TODO: Make sure editors get closed, when a datapack gets closed and other cases maybe

  TDatapackPageControl = class
  private
    FPageControl: TPageControl;
    FDatapackCollection: TDatapackCollection;

    procedure AddPage(ATreeNode: TTreeNode);
    function GetCurrentData: TDatapack.TFile;
    function GetCurrentPage: TTabSheet;

    procedure OnChangePage(Sender: TObject);

  public
    constructor Create(APageControl: TPageControl);

    property PageControl: TPageControl read FPageControl;
    property Datapacks: TDatapackCollection read FDatapackCollection;

    procedure Open(ATreeNode: TTreeNode);

    procedure UpdateOpenPages;

    procedure SaveAll;
    procedure SaveCurrent;

    property CurrentPage: TTabSheet read GetCurrentPage;
    property CurrentData: TDatapack.TFile read GetCurrentData;

    function IsEditing(AType: TDatapack.TDataType): Boolean;

  end;

  TDatapackTreeView = class
  private
    FTreeView: TTreeView;
    FPageControl: TDatapackPageControl;
    FDatapack: TDatapack;

    procedure SetDatapack(const Value: TDatapack);

    procedure FillDatapack;
    procedure FillNamespace(ANamespace: TDatapack.TNamespace);
    function FillData(AData: TDatapack.TData; ANamespaceNode: TTreeNode = nil): TTreeNode;
    function FillFileSystemEntry(AEntry: TDatapack.TFileSystemEntry; ANode: TTreeNode = nil): TTreeNode;
    function FillDirectory(ADirectory: TDatapack.TDirectory; ATreeNode: TTreeNode = nil): TTreeNode;
    function FillFile(AFile: TDatapack.TFile; ANode: TTreeNode = nil): TTreeNode;

    procedure UpdateAll;

    function CompareData(ALeft, ARight: TDatapackBase): Integer;
    function CompareNodes(ALeft, ARight: TTreeNode): Integer;

    procedure Compare(Sender: TObject; AItem1, AItem2: TTreeNode; AData: Integer; var ACompare: Integer);
    procedure DblClick(Sender: TObject);
    procedure Editing(Sender: TObject; ANode: TTreeNode; var AllowEdit: Boolean);
    procedure Edited(Sender: TObject; ANode: TTreeNode; var NewText: string);
    procedure KeyPress(Sender: TObject; var Key: Char);

    function GenerateNamespaceName: string;
    function GenerateDirectoryName(ADirectory: TDatapack.TDirectory): string;
    function GenerateFileName(ADirectory: TDatapack.TDirectory): string;

    procedure NamespaceAdd(AInfo: TDatapack.TNamespace.TEventInfo);
    procedure NamespaceRemove(AInfo: TDatapack.TNamespace.TEventInfo);
    procedure DataEnable(AInfo: TDatapack.TData.TEventInfo);
    procedure DataDisable(AInfo: TDatapack.TData.TEventInfo);
    procedure FileSystemEntryAdd(AInfo: TDatapack.TFileSystemEntry.TEventInfo);
    procedure FileSystemEntryRemove(AInfo: TDatapack.TFileSystemEntry.TEventInfo);

    procedure AddEvents;
    procedure RemoveEvents;

  public
    constructor Create(ATreeView: TTreeView; APageControl: TPageControl);
    destructor Destroy; override;

    property TreeView: TTreeView read FTreeView;
    property Datapack: TDatapack read FDatapack write SetDatapack;
    property PageControl: TDatapackPageControl read FPageControl;

    function GetNode(ANodeData: TDatapackBase; out ANode: TTreeNode): Boolean;

    procedure ExpandAll;
    procedure CollapseAll;

    procedure UpdateDatapack;

    procedure AddNamespace;

    function CanAddData(ADataType: TDatapack.TDataType): Boolean;
    procedure AddData(ADataType: TDatapack.TDataType);

    function CanAddDirectory: Boolean;
    procedure AddDirectory;

  end;

  TTreeNodeHelper = class helper for TTreeNode
    function NodeData: TDatapackBase;
  end;

  TFrameClass = class of TFrame;

  TEditor = class(TComponent)
  private
    FTabSheet: TTabSheet;
    FNodeData: TDatapack.TFile;
    FFrame: TFrame;
    FModified: Boolean;
    FSaved: Boolean;

    function GetDisplayName: string;

    procedure SetModified(const Value: Boolean);

    procedure UpdateCaption;
    procedure FileChange;
    procedure FileRemoved;

  protected
    procedure SaveProc; virtual; abstract;
    procedure LoadProc; virtual; abstract;
    procedure ModifiedChanged; virtual;

  public
    constructor Create(ATabSheet: TTabSheet; ANodeData: TDatapack.TFile); reintroduce; virtual;
    destructor Destroy; override;

    procedure AfterConstruction; override;

    class function GetFrameClass: TFrameClass; virtual; abstract;

    property TabSheet: TTabSheet read FTabSheet;
    property NodeData: TDatapack.TFile read FNodeData;
    property DisplayName: string read GetDisplayName;
    property Frame: TFrame read FFrame;

    property Modified: Boolean read FModified write SetModified;

    procedure Save;
    procedure Load;
    function Close: Boolean;

  end;

  TEditorClass = class of TEditor;

  TTabSheetHelper = class helper for TTabSheet
    function Editor: TEditor;
    function NodeData: TDatapack.TFile;
  end;

  TEditorFrameHelper = class helper for TFrame
    function Editor: TEditor;
  end;

implementation

uses
  EditorFrameAdvancement,
  EditorFrameFunction,
  EditorFrameLootTable,
  EditorFrameRecipe,
  EditorFrameStructure,
  EditorFrameTag;

const

  EditorClasses: array [TDatapack.TDataType] of TEditorClass = (
    TEditorAdvancements,
    TEditorFunctions,
    TEditorLootTables,
    TEditorRecipes,
    TEditorStructures,
    TEditorTags
    );

  { TDatapackTreeview }

procedure TDatapackTreeView.AddData(ADataType: TDatapack.TDataType);
var
  NodeData: TDatapackBase;
  FileNode: TDatapack.TFileSystemEntry;
  NewNode: TTreeNode;
  NewFile: TDatapack.TFile;
  DirNode: TDatapack.TDirectory;
begin
  NodeData := TreeView.Selected.NodeData;

  if NodeData is TDatapack.TDataSimple then
    NodeData := TDatapack.TDataSimple(NodeData).Directory;

  if NodeData is TDatapack.TFileSystemEntry then
  begin
    FileNode := TDatapack.TFileSystemEntry(NodeData);
    if FileNode is TDatapack.TDirectory then
    begin
      DirNode := TDatapack.TDirectory(FileNode);
      NewFile := DirNode.AddFile(GenerateFileName(DirNode))
    end
    else
    begin
      NewFile := FileNode.Parent.AddFile(GenerateFileName(FileNode.Parent));
    end;

    if GetNode(NewFile, NewNode) then
    begin
      NewNode.MakeVisible;
      PageControl.Open(NewNode);
      NewNode.EditText;
    end;
  end
  else if NodeData is TDatapack.TNamespace then
    TDatapack.TNamespace(NodeData).EnableData(ADataType);
end;

procedure TDatapackTreeView.AddDirectory;
var
  NewDir: TDatapack.TDirectory;
  NodeData: TDatapackBase;
  FileNode: TDatapack.TFileSystemEntry;
  DirNode: TDatapack.TDirectory;
  NewNode: TTreeNode;
begin
  NodeData := TreeView.Selected.NodeData;

  if NodeData is TDatapack.TDataSimple then
    NodeData := TDatapack.TDataSimple(NodeData).Directory;

  FileNode := TDatapack.TFileSystemEntry(NodeData);
  if NodeData is TDatapack.TDirectory then
  begin
    DirNode := TDatapack.TDirectory(NodeData);
    NewDir := DirNode.AddDirectory(GenerateDirectoryName(DirNode));
  end
  else
  begin
    NewDir := FileNode.Parent.AddDirectory(GenerateDirectoryName(FileNode.Parent));
  end;

  if GetNode(NewDir, NewNode) then
  begin
    NewNode.MakeVisible;
    NewNode.EditText;
  end;
end;

procedure TDatapackTreeView.AddEvents;
begin
  if Datapack = nil then
    Exit;
  Datapack.OnAddNamespace.Add(NamespaceAdd);
  Datapack.OnRemoveNamespace.Add(NamespaceRemove);
  Datapack.OnEnableData.Add(DataEnable);
  Datapack.OnDisableData.Add(DataDisable);
  Datapack.OnAddFileSystemEntry.Add(FileSystemEntryAdd);
  Datapack.OnRemoveFileSystemEntry.Add(FileSystemEntryRemove);
end;

procedure TDatapackTreeView.AddNamespace;
var
  Name: string;
  Namespace: TDatapack.TNamespace;
  NamespaceNode: TTreeNode;
begin
  Name := GenerateNamespaceName;
  Namespace := Datapack.AddNamespace(Name);
  if GetNode(Namespace, NamespaceNode) then
    NamespaceNode.EditText;
end;

function TDatapackTreeView.CanAddData(ADataType: TDatapack.TDataType): Boolean;
var
  NodeData: TDatapackBase;
begin
  if TreeView.Selected = nil then
    Exit(False);
  NodeData := TreeView.Selected.NodeData;
  Result := (NodeData is TDatapack.TDataSimple) and (TDatapack.TDataSimple(NodeData).GetType = ADataType) or
    (NodeData is TDatapack.TFileSystemEntry) and (TDatapack.TFileSystemEntry(NodeData).Data.GetType = ADataType) or
    (NodeData is TDatapack.TNamespace) and not TDatapack.TNamespace(NodeData).DataEnabled(ADataType);
end;

function TDatapackTreeView.CanAddDirectory: Boolean;
var
  NodeData: TDatapackBase;
begin
  if TreeView.Selected = nil then
    Exit(False);
  NodeData := TreeView.Selected.NodeData;
  Result := (NodeData is TDatapack.TDataSimple) or (NodeData is TDatapack.TFileSystemEntry);
end;

procedure TDatapackTreeView.CollapseAll;
var
  Node: TTreeNode;
begin
  TreeView.Items.BeginUpdate;
  for Node in TreeView.Items do
    Node.Collapse(False);
  TreeView.Items.EndUpdate;
end;

procedure TDatapackTreeView.Compare(Sender: TObject; AItem1, AItem2: TTreeNode; AData: Integer; var ACompare: Integer);
begin
  ACompare := CompareNodes(AItem1, AItem2);
end;

function TDatapackTreeView.CompareData(ALeft, ARight: TDatapackBase): Integer;
begin
  if (ALeft is TDatapack.TDirectory) and (ARight is TDatapack.TFile) then
    Exit(-1);
  if (ALeft is TDatapack.TFile) and (ARight is TDatapack.TDirectory) then
    Exit(1);
  Result := 0;
end;

function TDatapackTreeView.CompareNodes(ALeft, ARight: TTreeNode): Integer;
begin
  Result := CompareData(ALeft.NodeData, ARight.NodeData);
  if Result = 0 then
    Result := CompareText(ALeft.Text, ARight.Text);
end;

constructor TDatapackTreeView.Create(ATreeView: TTreeView; APageControl: TPageControl);
begin
  FTreeView := ATreeView;
  FPageControl := TDatapackPageControl.Create(APageControl);
  TreeView.OnCompare := Compare;
  TreeView.OnDblClick := DblClick;
  TreeView.OnEditing := Editing;
  TreeView.OnEdited := Edited;
  TreeView.OnKeyPress := KeyPress;
end;

procedure TDatapackTreeView.DataDisable(AInfo: TDatapack.TData.TEventInfo);
var
  Node: TTreeNode;
begin
  Assert(GetNode(AInfo.Sender, Node));
  Node.Delete;
end;

procedure TDatapackTreeView.DataEnable(AInfo: TDatapack.TData.TEventInfo);
begin
  FillData(AInfo.Sender).AlphaSort(True);
end;

procedure TDatapackTreeView.DblClick(Sender: TObject);
var
  MousePos: TPoint;
  Node: TTreeNode;
begin
  MousePos := TreeView.ScreenToClient(Mouse.CursorPos);
  Node := TreeView.GetNodeAt(MousePos.X, MousePos.Y);
  if Node <> nil then
    PageControl.Open(Node);
end;

destructor TDatapackTreeView.Destroy;
begin
  FPageControl.Free;
  inherited;
end;

procedure TDatapackTreeView.Edited(Sender: TObject; ANode: TTreeNode; var NewText: string);
begin
  ANode.NodeData.DisplayName := NewText;
  ANode.Text := NewText;
  if ANode.Parent <> nil then
    ANode.Parent.AlphaSort
  else
    TreeView.Items.AlphaSort;
end;

procedure TDatapackTreeView.Editing(Sender: TObject; ANode: TTreeNode; var AllowEdit: Boolean);
begin
  AllowEdit := ANode.NodeData.NameEditable;
end;

procedure TDatapackTreeView.ExpandAll;
var
  Node: TTreeNode;
begin
  TreeView.Items.BeginUpdate;
  for Node in TreeView.Items do
    Node.Expand(False);
  TreeView.Items.EndUpdate;
end;

function TDatapackTreeView.FillDirectory(ADirectory: TDatapack.TDirectory; ATreeNode: TTreeNode = nil): TTreeNode;
var
  Entry: TDatapack.TFileSystemEntry;
begin
  Assert((ATreeNode <> nil) or GetNode(ADirectory.Parent, ATreeNode));
  Result := ATreeNode;

  TreeView.Items.BeginUpdate;

  try
    if not ADirectory.Name.IsEmpty then
    begin
      ATreeNode := TreeView.Items.AddChildObject(ATreeNode, ADirectory.DisplayName, ADirectory);
      ATreeNode.ImageIndex := 8;
      ATreeNode.SelectedIndex := 8;
    end;

    for Entry in ADirectory.Entries.Values do
      FillFileSystemEntry(Entry, ATreeNode);

  finally
    TreeView.Items.EndUpdate;

  end;
end;

function TDatapackTreeView.FillFile(AFile: TDatapack.TFile; ANode: TTreeNode): TTreeNode;
var
  DataNode: TTreeNode;
begin
  Assert((ANode <> nil) or GetNode(AFile.Parent, ANode));
  Result := ANode;

  TreeView.Items.BeginUpdate;

  try
    DataNode := TreeView.Items.AddChildObject(ANode, AFile.DisplayName, AFile);
    DataNode.ImageIndex := Ord(AFile.Data.GetType);
    DataNode.SelectedIndex := Ord(AFile.Data.GetType);

  finally
    TreeView.Items.EndUpdate;

  end;
end;

function TDatapackTreeView.FillFileSystemEntry(AEntry: TDatapack.TFileSystemEntry; ANode: TTreeNode): TTreeNode;
var
  Data: TDatapackBase;
begin
  if ANode = nil then
  begin
    if (AEntry.Parent <> nil) and not AEntry.Parent.Name.IsEmpty then
      Data := AEntry.Parent
    else
      Data := AEntry.Data;
    Assert(GetNode(Data, ANode));
  end;

  if AEntry is TDatapack.TFile then
    Result := FillFile(TDatapack.TFile(AEntry), ANode)
  else
    Result := FillDirectory(TDatapack.TDirectory(AEntry), ANode)
end;

procedure TDatapackTreeView.FillNamespace(ANamespace: TDatapack.TNamespace);
var
  NamespaceNode: TTreeNode;
  DataType: TDatapack.TDataType;
  Data: TDatapack.TData;
begin
  TreeView.Items.BeginUpdate;

  try
    NamespaceNode := TreeView.Items.AddChildObject(nil, ANamespace.DisplayName, ANamespace);
    NamespaceNode.ImageIndex := 6;
    NamespaceNode.SelectedIndex := 6;

    for DataType := Low(TDatapack.TDataType) to High(TDatapack.TDataType) do
    begin
      Data := ANamespace.Data[DataType];
      if Data <> nil then
        FillData(Data, NamespaceNode);
    end;

    NamespaceNode.Expanded := True;

  finally
    TreeView.Items.EndUpdate;

  end;
end;

function TDatapackTreeView.GenerateDirectoryName(ADirectory: TDatapack.TDirectory): string;
const
  DefaultName = 'new';
var
  I: Integer;
begin
  Result := DefaultName;
  I := 0;
  while ADirectory.Entries.KeyExists(Result) do
  begin
    Inc(I);
    Result := DefaultName + I.ToString;
  end;
end;

function TDatapackTreeView.GenerateFileName(ADirectory: TDatapack.TDirectory): string;
const
  DefaultName = 'new';
var
  I: Integer;
begin
  Result := DefaultName + ADirectory.Data.GetFileExtension;
  I := 0;
  while ADirectory.Entries.KeyExists(Result) do
  begin
    Inc(I);
    Result := DefaultName + I.ToString + ADirectory.Data.GetFileExtension;
  end;
end;

function TDatapackTreeView.GenerateNamespaceName: string;
const
  DefaultName = 'new';
var
  I: Integer;
begin
  Result := DefaultName;
  I := 0;
  while Datapack.Namespaces.KeyExists(Result) do
  begin
    Inc(I);
    Result := DefaultName + I.ToString;
  end;
end;

function TDatapackTreeView.GetNode(ANodeData: TDatapackBase; out ANode: TTreeNode): Boolean;
begin
  ANode := TreeView.Items.GetFirstNode;
  while ANode <> nil do
  begin
    if ANode.NodeData = ANodeData then
      Exit(True);
    ANode := ANode.GetNext;
  end;
  Exit(False);
end;

procedure TDatapackTreeView.KeyPress(Sender: TObject; var Key: Char);
var
  Node: TTreeNode;
begin
  case Key of
    Chr(VK_RETURN):
      begin
        Key := #0;
        Node := TreeView.Selected;
        if Node <> nil then
        begin
          if Node.HasChildren then
            Node.Expand(False)
          else
            PageControl.Open(Node);
        end;
      end;
  end;
end;

procedure TDatapackTreeView.NamespaceAdd(AInfo: TDatapack.TNamespace.TEventInfo);
var
  Node: TTreeNode;
begin
  FillNamespace(AInfo.Sender);
  Assert(GetNode(AInfo.Sender, Node));
  TreeView.Items.AlphaSort;
  Node.AlphaSort(True);
end;

procedure TDatapackTreeView.NamespaceRemove(AInfo: TDatapack.TNamespace.TEventInfo);
var
  Node: TTreeNode;
begin
  Assert(GetNode(AInfo.Sender, Node));
  Node.Delete;
end;

procedure TDatapackTreeView.RemoveEvents;
begin
  if Datapack = nil then
    Exit;
  Datapack.OnAddNamespace.Remove(NamespaceAdd);
  Datapack.OnRemoveNamespace.Remove(NamespaceRemove);
  Datapack.OnEnableData.Remove(DataEnable);
  Datapack.OnDisableData.Remove(DataDisable);
  Datapack.OnAddFileSystemEntry.Remove(FileSystemEntryAdd);
  Datapack.OnRemoveFileSystemEntry.Remove(FileSystemEntryRemove);
end;

procedure TDatapackTreeView.FileSystemEntryAdd(AInfo: TDatapack.TFileSystemEntry.TEventInfo);
begin
  FillFileSystemEntry(AInfo.Sender).AlphaSort(True);
end;

procedure TDatapackTreeView.FileSystemEntryRemove(AInfo: TDatapack.TFileSystemEntry.TEventInfo);
var
  Node: TTreeNode;
begin
  Assert(GetNode(AInfo.Sender, Node));
  Node.Delete;
end;

function TDatapackTreeView.FillData(AData: TDatapack.TData; ANamespaceNode: TTreeNode = nil): TTreeNode;
var
  DataNode: TTreeNode;
begin
  Assert((ANamespaceNode <> nil) or GetNode(AData.Namespace, ANamespaceNode));
  Result := ANamespaceNode;

  TreeView.Items.BeginUpdate;

  try
    DataNode := TreeView.Items.AddChildObject(ANamespaceNode, AData.DisplayName, AData);
    DataNode.ImageIndex := Ord(AData.GetType);
    DataNode.SelectedIndex := Ord(AData.GetType);

    if AData is TDatapack.TDataSimple then
    begin
      FillDirectory(TDatapack.TDataSimple(AData).Directory, DataNode);
    end
    else if AData is TDatapack.TDataMulti then
    begin
      // TODO: Iterate over stuff
    end
    else
      Assert(False);

  finally
    TreeView.Items.EndUpdate;

  end;
end;

procedure TDatapackTreeView.FillDatapack;
var
  Namespace: TDatapack.TNamespace;
begin
  TreeView.Items.BeginUpdate;

  try
    for Namespace in Datapack.Namespaces.Values do
      FillNamespace(Namespace);

  finally
    TreeView.Items.EndUpdate;

  end;
end;

procedure TDatapackTreeView.SetDatapack(const Value: TDatapack);
begin
  RemoveEvents;
  FDatapack := Value;
  if Datapack = nil then
    Exit;
  UpdateAll;
  AddEvents;
end;

procedure TDatapackTreeView.UpdateAll;
begin
  TreeView.Items.Clear;
  if Datapack = nil then
    Exit;
  FillDatapack;
  TreeView.AlphaSort;
end;

procedure TDatapackTreeView.UpdateDatapack;
begin
  TreeView.Items.BeginUpdate;
  Datapack.Update;
  TreeView.Items.EndUpdate;
end;

{ TDatapackPageControl }

procedure TDatapackPageControl.AddPage(ATreeNode: TTreeNode);
var
  NodeData: TDatapack.TFile;
  TabSheet: TTabSheet;
  EditorClass: TEditorClass;
begin
  if not(ATreeNode.NodeData is TDatapack.TFile) then
    raise ENotSupportedException.Create('Only files can be edited.');

  NodeData := TDatapack.TFile(ATreeNode.NodeData);

  EditorClass := EditorClasses[NodeData.Data.GetType];
  if EditorClass = nil then
    raise ENotImplemented.CreateFmt('Editing %s is not yet implemented.',
      [DPDisplayNamesPlural[NodeData.Data.GetType]]);

  if PageControl.ActivePage <> nil then
    PageControl.ActivePage.Align := alNone;

  TabSheet := TTabSheet.Create(PageControl);
  TabSheet.PageControl := PageControl;
  TabSheet.ImageIndex := Ord(NodeData.Data.GetType);
  PageControl.ActivePage := TabSheet;

  EditorClass.Create(TabSheet, NodeData);
end;

constructor TDatapackPageControl.Create(APageControl: TPageControl);
begin
  FPageControl := APageControl;
  FPageControl.OnChange := OnChangePage;
  FDatapackCollection := TDatapackCollection.Create;
end;

function TDatapackPageControl.GetCurrentData: TDatapack.TFile;
begin
  if CurrentPage = nil then
    Exit(nil);
  Result := CurrentPage.NodeData;
end;

function TDatapackPageControl.GetCurrentPage: TTabSheet;
begin
  Result := PageControl.ActivePage;
end;

function TDatapackPageControl.IsEditing(AType: TDatapack.TDataType): Boolean;
begin
  Result := (CurrentPage <> nil) and (CurrentData.Data.GetType = AType);
end;

procedure TDatapackPageControl.OnChangePage(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to FPageControl.PageCount - 1 do
    FPageControl.Pages[I].Align := alNone;
  FPageControl.ActivePage.Align := alClient;
end;

procedure TDatapackPageControl.Open(ATreeNode: TTreeNode);
var
  I: Integer;
begin
  if not(ATreeNode.NodeData is TDatapack.TFile) then
    Exit;
  for I := 0 to PageControl.PageCount - 1 do
  begin
    if PageControl.Pages[I].NodeData = ATreeNode.NodeData then
    begin
      FPageControl.ActivePageIndex := I;
      OnChangePage(nil);
      Exit;
    end;
  end;
  AddPage(ATreeNode);
end;

procedure TDatapackPageControl.SaveAll;
var
  I: Integer;
begin
  for I := 0 to PageControl.PageCount - 1 do
    PageControl.Pages[I].Editor.Save;
end;

procedure TDatapackPageControl.SaveCurrent;
begin
  PageControl.ActivePage.Editor.Save;
end;

procedure TDatapackPageControl.UpdateOpenPages;
var
  I: Integer;
begin
  for I := 0 to PageControl.PageCount - 1 do
    PageControl.Pages[I].Editor.NodeData.Update;
end;

{ TEditorInfo }

procedure TEditor.AfterConstruction;
begin
  Load;
end;

function TEditor.Close: Boolean;
var
  ModalResult: Integer;
  PageControl: TPageControl;
begin
  if Modified then
    ModalResult := MessageDlg(Format('Save changes in %s before closing?', [NodeData.NamespacePath.Format]),
      mtConfirmation, mbYesNoCancel, 0, mbYes)
  else
    ModalResult := mrNo;

  if ModalResult = mrYes then
    Save;

  Result := ModalResult <> mrCancel;
  if Result then
  begin
    PageControl := TabSheet.PageControl;
    TabSheet.Free;
    if PageControl.ActivePage <> nil then
      PageControl.ActivePage.Align := alClient;
  end;
end;

constructor TEditor.Create(ATabSheet: TTabSheet; ANodeData: TDatapack.TFile);
begin
  inherited Create(ATabSheet);

  FTabSheet := ATabSheet;
  FTabSheet.Tag := NativeInt(Self);
  FNodeData := ANodeData;

  UpdateCaption;

  FFrame := GetFrameClass.Create(Self);
  Frame.Parent := TabSheet;
  Frame.Align := alClient;

  NodeData.OnChange.Add(FileChange);
  NodeData.OnFileRemoved.Add(FileRemoved);
  NodeData.OnRename.Add(UpdateCaption);
  NodeData.OnRemove.Add(TabSheet.Free);
end;

destructor TEditor.Destroy;
begin
  NodeData.OnChange.Remove(FileChange);
  NodeData.OnFileRemoved.Remove(FileRemoved);
  NodeData.OnRename.Remove(UpdateCaption);
  NodeData.OnRemove.Remove(TabSheet.Free);
  inherited;
end;

procedure TEditor.FileChange;
begin
  if FSaved then
  begin
    FSaved := False;
    Exit;
  end;
  Modified := True;
  TabSheet.PageControl.ActivePage := TabSheet;
  if MessageDlg(Format('Content of %s changed, do you want to reload it?', [NodeData.NamespacePath.Format]),
    mtConfirmation, mbYesNo, 0, mbYes) = mrYes then
  begin
    Load;
  end;
end;

procedure TEditor.FileRemoved;
begin
  Modified := True;
  if MessageDlg(Format('File for %s does not exist anymore, do you want to recreate it?', [NodeData.NamespacePath.Format]),
    mtConfirmation, mbYesNo, 0, mbYes) = mrYes then
  begin
    Save;
  end;
end;

function TEditor.GetDisplayName: string;
begin
  Result := NodeData.DisplayName;
end;

procedure TEditor.Load;
begin
  if NodeData.FileExists then
    LoadProc;
  Modified := not NodeData.FileExists;
end;

procedure TEditor.ModifiedChanged;
begin
  // nothing by default
end;

procedure TEditor.UpdateCaption;
begin
  TabSheet.Caption := DisplayName;
  if Modified then
    TabSheet.Caption := '*' + DisplayName;
end;

procedure TEditor.Save;
begin
  if Modified then
  begin
    SaveProc;
    FSaved := True;
    NodeData.Update;
    Modified := False;
  end;
end;

procedure TEditor.SetModified(const Value: Boolean);
begin
  if Modified = Value then
    Exit;
  FModified := Value;
  UpdateCaption;
  ModifiedChanged;
end;

{ TEditorFrameHelper }

function TEditorFrameHelper.Editor: TEditor;
begin
  Result := TEditor(Owner);
end;

{ TTreeNodeHelper }

function TTreeNodeHelper.NodeData: TDatapackBase;
begin
  Result := TDatapackBase(Data);
end;

{ TTabSheetHelper }

function TTabSheetHelper.Editor: TEditor;
begin
  Result := TEditor(Tag);
end;

function TTabSheetHelper.NodeData: TDatapack.TFile;
begin
  Result := Editor.NodeData;
end;

end.
