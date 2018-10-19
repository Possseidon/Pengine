unit Pengine.MC.ItemIcons;

interface

uses
  System.ZIP,
  System.IOUtils,
  System.Classes,
  System.SysUtils,

  Vcl.Imaging.pngimage,

  Pengine.Settings,
  Pengine.HashCollections,

  Pengine.MC.Item,
  Pengine.MC.BlockState,
  Pengine.MC.Namespace,
  Pengine.JSON;

type

  {
    Item Icon Generation:
    - models\items\<name_without_namespace>.json
    - item/generated -> use layer0
    -
  }


  TItemIconSettings = class(TSettings)
  public type

    TItemIcons = TRefObjectMap<TItemType, TPngImage>;

    TBlockIcons = TRefObjectMap<TBlockType, TPngImage>;

  private
    FPath: string;
    FItemIcons: TItemIcons;
    FBlockIcons: TBlockIcons;

    procedure SetPath(const Value: string);
    function GetItemIcons: TItemIcons.TReader;
    function GetBlockIcons: TBlockIcons.TReader;

  protected
    constructor Create(ARoot: TRootSettings); override;

    procedure DoReload; override;

    class function GetNameForVersion(AVersion: Integer): string; override;

  public
    destructor Destroy; override;

    class function GetTitle: string; override;
    class function DefaultPath: string; static;

    procedure SetDefaults; override;

    property Path: string read FPath write SetPath;

    property ItemIcons: TItemIcons.TReader read GetItemIcons;
    property BlockIcons: TBlockIcons.TReader read GetBlockIcons;

    procedure DefineJStorage(ASerializer: TJSerializer); override;

  end;

implementation

{ TItemIconSettings }

constructor TItemIconSettings.Create(ARoot: TRootSettings);
begin
  inherited;
  FItemIcons := TItemIcons.Create;
  FBlockIcons := TBlockIcons.Create;
  AddDependent(RootSettingsG.Get<TItemSettings>);
  AddDependent(RootSettingsG.Get<TBlockSettings>);
end;

class function TItemIconSettings.DefaultPath: string;
begin
  Result := TPath.Combine(TPath.GetHomePath, '.minecraft/versions/1.13.1/1.13.1.jar');
end;

procedure TItemIconSettings.DefineJStorage(ASerializer: TJSerializer);
begin
  inherited;
  with ASerializer do
  begin
    Define('path', FPath);
  end;
end;

destructor TItemIconSettings.Destroy;
begin
  FItemIcons.Free;
  FBlockIcons.Free;
  inherited;
end;

procedure TItemIconSettings.DoReload;
var
  Item: TItemType;
  Block: TBlockType;
  Jar: TZipFile;
  I: Integer;
  Image: TPngImage;
  PngStream: TStream;
  Header: TZipHeader;
  NSPath: TNSPath;
  Items: TItemTypeCollection;
  Blocks: TBlockTypeCollection;
begin
  FItemIcons.Clear;
  FBlockIcons.Clear;

  Jar := TZipFile.Create;
  try
    Jar.Open(Path, zmRead);

    Items := RootSettingsG.Get<TItemSettings>.Items;
    Blocks := RootSettingsG.Get<TBlockSettings>.Blocks;

    for I := 0 to Jar.FileCount - 1 do
    begin
      if not Jar.FileName[I].EndsWith('.png', True) or
        not(Jar.FileName[I].StartsWith('assets/minecraft/textures/item') or
        Jar.FileName[I].StartsWith('assets/minecraft/textures/block')) then
        Continue;
      NSPath := ChangeFileExt(Jar.FileName[I].Substring(Jar.FileName[I].LastIndexOf('/') + 1), '');
      if Items.Get(NSPath, Item) then
      begin
        Image := TPngImage.Create;
        Jar.Read(I, PngStream, Header);
        Image.LoadFromStream(PngStream);
        Image.Resize(16, 16);
        PngStream.Free;
        FItemIcons[Item] := Image;
      end;
      if Blocks.Get(NSPath, Block) then
      begin
        Image := TPngImage.Create;
        Jar.Read(I, PngStream, Header);
        Image.LoadFromStream(PngStream);
        Image.Resize(16, 16);
        PngStream.Free;
        FBlockIcons[Block] := Image;
      end;

    end;

  finally
    Jar.Free;

  end;
end;

function TItemIconSettings.GetBlockIcons: TBlockIcons.TReader;
begin
  Result := FBlockIcons.Reader;
end;

function TItemIconSettings.GetItemIcons: TItemIcons.TReader;
begin
  Result := FItemIcons.Reader;
end;

class function TItemIconSettings.GetNameForVersion(AVersion: Integer): string;
begin
  Result := 'mc_itemicons';
end;

class function TItemIconSettings.GetTitle: string;
begin
  Result := 'Item-Icons';
end;

procedure TItemIconSettings.SetDefaults;
begin
  Path := DefaultPath;
end;

procedure TItemIconSettings.SetPath(const Value: string);
begin
  FPath := Value;
  Reload;
end;

end.
