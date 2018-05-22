unit DatapackView;

interface

uses
  System.SysUtils,
  System.Types,
  System.Classes,

  Vcl.ComCtrls,
  Vcl.Controls,
  Vcl.Forms,

  Pengine.Collections,

  Minecraft.Datapack;

type

  TDatapackPageControl = class
  private
    FPageControl: TPageControl;
    FDatapack: TDatapack;
  
    procedure SetDatapack(const Value: TDatapack);

    procedure AddPage(ATreeNode: TTreeNode);

  public
    constructor Create(APageControl: TPageControl);
   
    property PageControl: TPageControl read FPageControl;
    property Datapack: TDatapack read FDatapack write SetDatapack;

    procedure Open(ATreeNode: TTreeNode);

  end;

  TDatapackTreeView = class
  public type

    TNodeData = class
    public type

      TType = (
        ntNamespace, 
        ntType, 
        ntDirectory, 
        ntFile
        );
        
    private
      FNamespace: TDatapack.TNamespace;
      FDataType: TDatapack.TDataType;
      FData: TDatapack.TData;
      FType: TType;
      FName: string;

    public                                                                
      constructor Create(AData: TDatapack.TData); overload;
      constructor Create(AData: TDatapack.TData; AName: string); overload;
      constructor Create(ANamespace: TDatapack.TNamespace); overload;
      constructor Create(ANamespace: TDatapack.TNamespace; ADataType: TDatapack.TDataType); overload;

      property Namespace: TDatapack.TNamespace read FNamespace;
      property DataType: TDatapack.TDataType read FDataType;
      property Data: TDatapack.TData read FData;
      property NodeType: TType read FType;
      property Name: string read FName;

      function FullPath: string;
      function FullName: string;
      
    end;
    
  private
    FTreeView: TTreeView;
    FPageControl: TDatapackPageControl;
    FDatapack: TDatapack;
    FDataTypeFirst: Boolean;

    procedure SetDatapack(const Value: TDatapack);

    procedure FillDefault;
    procedure FillDataTypeFirst;
    procedure FillData(AData: TDatapack.TData; ATreeNode: TTreeNode);

    procedure UpdateAll;

    procedure Compare(Sender: TObject; AItem1, AItem2: TTreeNode; AData: Integer; var ACompare: Integer);
    procedure Deletion(Sender: TObject; ANode: TTreeNode);
    procedure DblClick(Sender: TObject);
    
    procedure SetDataTypeFirst(const Value: Boolean);

  public
    constructor Create(ATreeView: TTreeView; APageControl: TPageControl);
    destructor Destroy; override;

    property TreeView: TTreeView read FTreeView;
    property Datapack: TDatapack read FDatapack write SetDatapack;
    property PageControl: TDatapackPageControl read FPageControl;

    /// <summary>Wether namespace or data type is used in the top level of the TTreeView.</summary>
    property DataTypeFirst: Boolean read FDataTypeFirst write SetDataTypeFirst;

    procedure ExpandAll;
    procedure CollapseAll;

  end;

  TTreeNodeHelper = class helper for TTreeNode
    function NodeData: TDatapackTreeView.TNodeData;
  end;

  TFrameClass = class of TFrame;

  TEditor = class(TComponent)
  private
    FFrame: TFrame;
    FModified: Boolean;

    function GetNodeData: TDatapackTreeView.TNodeData;
    function GetDisplayName: string;

    procedure SetModified(const Value: Boolean);
    function GetTabSheet: TTabSheet;

  protected
    procedure SaveProc; virtual; abstract;

  public
    constructor Create(ATabSheet: TTabSheet); reintroduce; virtual;
    
    function GetFrameClass: TFrameClass; virtual; abstract;

    property TabSheet: TTabSheet read GetTabSheet;
    property NodeData: TDatapackTreeView.TNodeData read GetNodeData;
    property DisplayName: string read GetDisplayName;
    property Frame: TFrame read FFrame;

    property Modified: Boolean read FModified write SetModified;
    procedure Save;

  end;

  TEditorClass = class of TEditor;

  TEditorFrameHelper = class helper for TFrame
    function Editor: TEditor;
  end;

implementation

uses
  EditorFrameFunction,
  EditorFrameLootTable;

const

  EditorClasses: array [TDatapack.TDataType] of TEditorClass = (
    nil, // Advancement
    TEditorFunctions,
    TEditorLootTables,
    nil, // Recipe
    nil, // Structure
    nil  // Tag
  );

{ TDatapackTreeview }

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
  if (AItem1.NodeData.NodeType = ntDirectory) and (AItem2.NodeData.NodeType = ntFile) then
    ACompare := -1
  else
    ACompare := CompareText(AItem1.Text, AItem2.Text);
end;

constructor TDatapackTreeView.Create(ATreeView: TTreeView; APageControl: TPageControl);
begin
  FTreeView := ATreeView;
  FPageControl := TDatapackPageControl.Create(APageControl);
  FTreeView.OnCompare := Compare;
  FTreeView.OnDeletion := Deletion;
  FTreeView.OnDblClick := DblClick;
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

procedure TDatapackTreeView.Deletion(Sender: TObject; ANode: TTreeNode);
begin
  ANode.NodeData.Free;
end;

destructor TDatapackTreeView.Destroy;
begin
  FPageControl.Free;
  inherited;
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

procedure TDatapackTreeView.FillData(AData: TDatapack.TData; ATreeNode: TTreeNode);
var
  Dir: TDatapack.TData.TDirectories.TPair;
  DataNode: TTreeNode;
  FileName: string;
begin
  for Dir in AData.Directories do
  begin
    DataNode := TreeView.Items.AddChild(ATreeNode, Dir.Key);
    DataNode.ImageIndex := 8;
    DataNode.SelectedIndex := 8;
    DataNode.Data := TNodeData.Create(Dir.Value);
    FillData(Dir.Value, DataNode);
  end;

  for FileName in AData.Files do
  begin
    DataNode := TreeView.Items.AddChild(ATreeNode, ChangeFileExt(FileName, ''));
    DataNode.ImageIndex := Ord(AData.GetType);
    DataNode.SelectedIndex := Ord(AData.GetType);
    DataNode.Data := TNodeData.Create(AData, FileName);
  end;
end;

procedure TDatapackTreeView.FillDataTypeFirst;
var
  Pair: TDatapack.TNamespaces.TPair;
  NamespaceName: string;
  Namespace: TDatapack.TNamespace;
  DataNodes: array [TDatapack.TDataType] of TTreeNode;
  DataType: TDatapack.TDataType;
  Data: TDatapack.TData;
  NamespaceNode: TTreeNode;
begin
  FillChar(DataNodes, SizeOf(DataNodes), 0);
  for Pair in Datapack.Namespaces do
  begin
    NamespaceName := Pair.Key;
    Namespace := Pair.Value;

    for DataType := Low(TDatapack.TDataType) to High(TDatapack.TDataType) do
    begin
      Data := Namespace.Data[DataType];
      if Data <> nil then
      begin
        if DataNodes[DataType] = nil then
        begin
          DataNodes[DataType] := TreeView.Items.Add(nil, Data.GetDisplayNamePlural);
          DataNodes[DataType].ImageIndex := Ord(Data.GetType);
          DataNodes[DataType].SelectedIndex := Ord(Data.GetType);
          DataNodes[DataType].Data := TNodeData.Create(Namespace, Data.GetType);
        end;
        NamespaceNode := TreeView.Items.AddChild(DataNodes[DataType], NamespaceName);
        NamespaceNode.ImageIndex := 6;
        NamespaceNode.SelectedIndex := 6;
        NamespaceNode.Data := TNodeData.Create(Namespace);
        FillData(Data, NamespaceNode);
        DataNodes[DataType].Expanded := True;
      end;
    end;
  end;

end;

procedure TDatapackTreeView.FillDefault;
var
  Pair: TDatapack.TNamespaces.TPair;
  NamespaceName: string;
  Namespace: TDatapack.TNamespace;
  NamespaceNode, DataNode: TTreeNode;
  DataType: TDatapack.TDataType;
  Data: TDatapack.TData;
begin
  for Pair in Datapack.Namespaces do
  begin
    NamespaceName := Pair.Key;
    Namespace := Pair.Value;
    NamespaceNode := TreeView.Items.Add(nil, NamespaceName);
    NamespaceNode.ImageIndex := 6;
    NamespaceNode.SelectedIndex := 6;   
    NamespaceNode.Data := TNodeData.Create(Namespace);
    for DataType := Low(DataType) to High(DataType) do
    begin
      Data := Namespace.Data[DataType];
      if Data <> nil then
      begin
        DataNode := TreeView.Items.AddChild(NamespaceNode, Data.GetDisplayNamePlural);
        DataNode.ImageIndex := Ord(Data.GetType);
        DataNode.SelectedIndex := Ord(Data.GetType);
        DataNode.Data := TNodeData.Create(Namespace, Data.GetType);
        FillData(Data, DataNode);
      end;
    end;
    NamespaceNode.Expanded := True;
  end;
end;

procedure TDatapackTreeView.SetDatapack(const Value: TDatapack);
begin
  if Datapack = Value then
    Exit;
  FDatapack := Value;
  UpdateAll;
end;

procedure TDatapackTreeView.SetDataTypeFirst(const Value: Boolean);
begin
  if DataTypeFirst = Value then
    Exit;
  FDataTypeFirst := Value;
  UpdateAll;
end;

procedure TDatapackTreeView.UpdateAll;
begin
  TreeView.Items.BeginUpdate;
  TreeView.Items.Clear;
  if Datapack = nil then
    Exit;
  if DataTypeFirst then
    FillDataTypeFirst
  else
    FillDefault;
  TreeView.AlphaSort;
  TreeView.Items.EndUpdate;
end;

{ TDatapackTreeView.TNodeData }

constructor TDatapackTreeView.TNodeData.Create(AData: TDatapack.TData; AName: string);
begin
  FType := ntFile;
  FData := AData;
  FNamespace := Data.Namespace;
  FDataType := Data.GetType;
  FName := AName;
end;

constructor TDatapackTreeView.TNodeData.Create(AData: TDatapack.TData);
begin
  FType := ntDirectory;
  FData := AData;
  FNamespace := Data.Namespace;
  FDataType := Data.GetType;
end;

constructor TDatapackTreeView.TNodeData.Create(ANamespace: TDatapack.TNamespace; ADataType: TDatapack.TDataType);
begin
  FType := ntType;
  FDataType := ADataType;
  FNamespace := ANamespace;
end;

function TDatapackTreeView.TNodeData.FullName: string;
begin
  Result := Data.FullNameTo(Name);
end;

function TDatapackTreeView.TNodeData.FullPath: string;
begin
  Result := Data.FullPathTo(Name);
end;

constructor TDatapackTreeView.TNodeData.Create(ANamespace: TDatapack.TNamespace);
begin
  FType := ntNamespace;
  FNamespace := ANamespace;
end;
      
{ TDatapackPageControl }

procedure TDatapackPageControl.AddPage(ATreeNode: TTreeNode);
var
  TabSheet: TTabSheet;
  NodeData: TDatapackTreeView.TNodeData;
  EditorClass: TEditorClass;
begin
  NodeData := ATreeNode.NodeData;
  EditorClass := EditorClasses[NodeData.DataType];
  if EditorClass = nil then
    raise ENotImplemented.CreateFmt('Editing %s is not yet implemented.', [DPDisplayNamesPlural[NodeData.DataType]]);

  TabSheet := TTabSheet.Create(PageControl);
  TabSheet.PageControl := PageControl;
  TabSheet.Caption := ChangeFileExt(NodeData.Name, '');
  TabSheet.Hint := NodeData.FullName;
  TabSheet.ImageIndex := Ord(NodeData.DataType);
  TabSheet.Tag := NativeInt(NodeData);
  PageControl.ActivePage := TabSheet;

  EditorClass.Create(TabSheet);
end;

constructor TDatapackPageControl.Create(APageControl: TPageControl);
begin
  FPageControl := APageControl;
end;

procedure TDatapackPageControl.Open(ATreeNode: TTreeNode);
var
  I: Integer;
begin
  if ATreeNode.NodeData.NodeType <> ntFile then
    Exit;
  for I := 0 to PageControl.PageCount - 1 do
  begin
    if TObject(PageControl.Pages[I].Tag) = ATreeNode.NodeData then
    begin
      FPageControl.ActivePageIndex := I;
      Exit;
    end;
  end;
  AddPage(ATreeNode);
end;

procedure TDatapackPageControl.SetDatapack(const Value: TDatapack);
var
  I: Integer;
begin
  if Datapack = Value then
    Exit;
  FDatapack := Value;
  for I := PageControl.PageCount - 1 to 0 do
    PageControl.Pages[I].Free;
end;

{ TEditorInfo }

constructor TEditor.Create(ATabSheet: TTabSheet);
begin
  inherited Create(ATabSheet);

  FFrame := GetFrameClass.Create(Self);
  Frame.Parent := TabSheet;
  Frame.Align := alClient;
end;

function TEditor.GetDisplayName: string;
begin
  Result := ChangeFileExt(NodeData.Name, '');
end;

function TEditor.GetNodeData: TDatapackTreeView.TNodeData;
begin
  Result := TDatapackTreeView.TNodeData(TabSheet.Tag);
end;

function TEditor.GetTabSheet: TTabSheet;
begin
  Result := TTabSheet(Owner);
end;

procedure TEditor.Save;
begin
  if Modified then
  begin
    SaveProc;
    Modified := False;
    TabSheet.Caption := DisplayName;
  end;
end;

procedure TEditor.SetModified(const Value: Boolean);
begin
  if Modified = Value then
    Exit;
  FModified := Value;
  if Modified then
    TabSheet.Caption := '*' + DisplayName
  else
    TabSheet.Caption := DisplayName;
end;

{ TEditorFrameHelper }

function TEditorFrameHelper.Editor: TEditor;
begin
  Result := TEditor(Owner);
end;

{ TTreeNodeHelper }

function TTreeNodeHelper.NodeData: TDatapackTreeView.TNodeData;
begin
  Result := TDatapackTreeView.TNodeData(Data);
end;

end.
