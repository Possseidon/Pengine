unit Pengine.MC.Item;

interface

uses
  System.SysUtils,
  System.JSON,
  System.IOUtils,
  System.Generics.Collections,

  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.Settings,

  Pengine.MC.Namespace;

type

  TItems = class
  public type

    TSet = TSet<TNSPath, TNSPathHasher>;
    TOrder = TArray<TNSPath>;

  private
    FSet: TSet;
    FOrder: TOrder;
    FSorted: TOrder;

    function GetOrder: TOrder.TReader;
    function GetSorted: TOrder.TReader;

  public
    constructor Create(AJSONObject: TJSONObject);
    destructor Destroy; override;

    function Exists(ANSPath: TNSPath): Boolean;

    /// <summary>A read-only array of the order, as found in the file.</summary>
    property Order: TOrder.TReader read GetOrder;
    /// <summary>An alpha-sorted version of the same array.</summary>
    property Sorted: TOrder.TReader read GetSorted;

  end;

  TItemSettings = class(TSettings)
  public const

    DefaultPath = 'Data\reports\items.json';

  private
    FItems: TItems;
    FPath: string;

    procedure SetPath(const Value: string);

  public
    destructor Destroy; override;

    class function GetTitle: string; override;
    class function GetDescription: string; override;

    procedure SetDefaults; override;

    property Path: string read FPath write SetPath;
    property Items: TItems read FItems;

    procedure Reload;

  end;

  // TODO: Tags

implementation

{ TItems }

constructor TItems.Create(AJSONObject: TJSONObject);
var
  JSONPair: TJSONPair;
begin
  FSet := TSet.Create;
  FOrder := TOrder.Create;

  for JSONPair in AJSONObject do
  begin
    FSet[JSONPair.JsonString.Value] := True;
    FOrder.Add(JSONPair.JsonString.Value);
  end;

  FSorted := FOrder.Copy;
  FSorted.Sort(
    function(A, B: TNSPath): Boolean
    begin
      Result := A < B;
    end);
end;

destructor TItems.Destroy;
begin
  FSet.Free;
  FOrder.Free;
  FSorted.Free;
  inherited;
end;

function TItems.Exists(ANSPath: TNSPath): Boolean;
begin
  Result := FSet[ANSPath];
end;

function TItems.GetOrder: TOrder.TReader;
begin
  Result := FOrder.Reader;
end;

function TItems.GetSorted: TOrder.TReader;
begin
  Result := FSorted.Reader;
end;

{ TItemSettings }

destructor TItemSettings.Destroy;
begin
  FItems.Free;
  inherited;
end;

class function TItemSettings.GetDescription: string;
begin
  Result := 'Path configuration for items in items.json file.';
end;

class function TItemSettings.GetTitle: string;
begin
  Result := 'Items';
end;

procedure TItemSettings.Reload;
var
  ItemsText: string;
  ItemsJSON: TJSONObject;
begin
  FItems.Free;
  if TFile.Exists(Path) then
  begin
    ItemsText := TFile.ReadAllText(Path);
    ItemsJSON := TJSONObject.ParseJSONValue(ItemsText) as TJSONObject;
  end
  else
    ItemsJSON := TJSONObject.Create;
  try
    FItems := TItems.Create(ItemsJSON);
  finally
    ItemsJSON.Free;
  end;
end;

procedure TItemSettings.SetDefaults;
begin
  Path := DefaultPath;
end;

procedure TItemSettings.SetPath(const Value: string);
begin
  if Path = Value then
    Exit;
  FPath := Value;
  Reload;
end;

end.
