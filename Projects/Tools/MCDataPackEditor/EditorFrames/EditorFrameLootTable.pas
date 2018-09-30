unit EditorFrameLootTable;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.IOUtils,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,

  System.JSON,

  Pengine.MC.LootTable,

  DatapackView,
  LootTableFramePool;

type

  TEditorLootTables = class(TEditor)
  public
    class function GetFrameClass: TFrameClass; override;

  end;

  TfrmEditorLootTables = class(TFrame)
    sbPools: TScrollBox;
  private
    FLootTable: TLootTable;

    procedure AddPool(AInfo: TLootTable.TPoolEventInfo);
    procedure RemovePool(AInfo: TLootTable.TPoolEventInfo);
    procedure ClearPools(AInfo: TLootTable.TEventInfo);
    procedure MovePool(AInfo: TLootTable.TPoolMoveEventInfo);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  end;

implementation

{$R *.dfm}

{ TEditorLootTables }

class function TEditorLootTables.GetFrameClass: TFrameClass;
begin
  Result := TfrmEditorLootTables;
end;

{ TfrmEditorLootTables }

procedure TfrmEditorLootTables.AddPool(AInfo: TLootTable.TPoolEventInfo);
var
  Frame: TfrmPool;
begin
  Frame := TfrmPool.Create(sbPools, AInfo.Pool);
  Frame.Parent := sbPools;
  Frame.Align := alLeft;
  Frame.AlignWithMargins := True;
  Frame.Name := '';
end;

procedure TfrmEditorLootTables.ClearPools(AInfo: TLootTable.TEventInfo);
begin

end;

constructor TfrmEditorLootTables.Create(AOwner: TComponent);
var
  FileText: string;
  JSONValue: TJSONValue;
begin
  inherited;
  FLootTable := TLootTable.Create;
  FLootTable.OnAddPool.Add(AddPool);
  FLootTable.OnRemovePool.Add(RemovePool);
  FLootTable.OnClearPools.Add(ClearPools);
  FLootTable.OnMovePool.Add(MovePool);
  FileText := TFile.ReadAllText(Editor.NodeData.FullPath);
  JSONValue := TJSONObject.ParseJSONValue(FileText);
  try
    FLootTable.Load(JSONValue);
  finally
    JSONValue.Free;
  end;
end;

destructor TfrmEditorLootTables.Destroy;
begin
  FLootTable.Free;
  inherited;
end;

procedure TfrmEditorLootTables.MovePool(AInfo: TLootTable.TPoolMoveEventInfo);
begin

end;

procedure TfrmEditorLootTables.RemovePool(AInfo: TLootTable.TPoolEventInfo);
begin

end;

end.
