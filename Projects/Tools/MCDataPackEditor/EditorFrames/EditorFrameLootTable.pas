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
  LootTableFramePool, Pengine.JSON;

type

  TfrmEditorLootTables = class(TFrame)
    sbPools: TScrollBox;
  private
    FLootTable: TLootTable;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  end;

  TEditorLootTables = class(TEditor)
  protected
    procedure SaveProc; override;
    procedure LoadProc; override;
    procedure ModifiedChanged; override;

  public
    class function GetFrameClass: TFrameClass; override;

    function Frame: TfrmEditorLootTables;

  end;

implementation

{$R *.dfm}

{ TEditorLootTables }

function TEditorLootTables.Frame: TfrmEditorLootTables;
begin
  Result := TfrmEditorLootTables(inherited Frame);
end;

class function TEditorLootTables.GetFrameClass: TFrameClass;
begin
  Result := TfrmEditorLootTables;
end;

procedure TEditorLootTables.LoadProc;
var
  FileText: string;
  JObject: TJObject;
begin
  FileText := TFile.ReadAllText(NodeData.FullPath);
  JObject := TJObject.Parse(FileText);
  try
    Frame.FLootTable.Load(JObject);
  finally
    JObject.Free;
  end;
end;

procedure TEditorLootTables.ModifiedChanged;
begin
  // TODO: What?
end;

procedure TEditorLootTables.SaveProc;
begin
  // TODO: Save
end;

{ TfrmEditorLootTables }

constructor TfrmEditorLootTables.Create(AOwner: TComponent);
begin
  inherited;
  FLootTable := TLootTable.Create;
end;

destructor TfrmEditorLootTables.Destroy;
begin
  FLootTable.Free;
  inherited;
end;

end.
