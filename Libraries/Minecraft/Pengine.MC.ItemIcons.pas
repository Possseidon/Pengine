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
  Pengine.Collections, // remove me
  Pengine.JSON,
  Pengine.Utility,

  Pengine.MC.Item,
  Pengine.MC.BlockState,
  Pengine.MC.Namespace;

type

  TItemIconSettings = class(TSettings)
  public type

    TIcons = TRefObjectMap<TItemType, TPngImage>;

  private
    FPath: string;
    FIcons: TIcons;

    procedure SetPath(const Value: string);
    function GetIcons: TIcons.TReader;

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

    property Icons: TIcons.TReader read GetIcons;

    procedure DefineJStorage(ASerializer: TJSerializer); override;

  end;

implementation

{ TItemIconSettings }

constructor TItemIconSettings.Create(ARoot: TRootSettings);
begin
  inherited;
  FIcons := TIcons.Create;
  AddDependent(RootSettingsG.Get<TItemSettings>);
end;

class function TItemIconSettings.DefaultPath: string;
begin
  Result := '%appdata%\.minecraft\versions\1.13.1\1.13.1.jar';
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
  FIcons.Free;
  inherited;
end;

procedure TItemIconSettings.DoReload;
var
  Jar: TZipFile;
  Items: TItemTypeCollection;
  I: Integer;
  FileName, ItemName: string;
  Found, Missing: Integer;
  Blocks: TBlockTypeCollection;
  MissingBlocks: TMap<TNSPath, TBlockType, TNSPathHasher>;
  M: TArray<TNSPath>;
begin
  FIcons.Clear;

  Jar := TZipFile.Create;
  try
    Jar.Open(ExpandEnvVars(Path), zmRead);

    Items := RootSettingsG.Get<TItemSettings>.Items;
    Blocks := RootSettingsG.Get<TBlockSettings>.Blocks;

    MissingBlocks := Blocks.Map.Copy;

    Found := 0;
    for I := 0 to Jar.FileCount - 1 do
    begin
      FileName := Jar.FileName[I];
      if not FileName.StartsWith('assets/minecraft/blockstates/') or not FileName.EndsWith('.json') then
        Continue;
      ItemName := ChangeFileExt(FileName.Substring(FileName.LastIndexOf('/') + 1), '');
      if Blocks.Exists(ItemName) then
      begin
        Inc(Found);
        MissingBlocks.TryRemove(ItemName);
      end;
    end;

    M := MissingBlocks.Keys.ToArray;
    Missing := Blocks.Count - Found;

  finally
    Jar.Free;

  end;
end;

function TItemIconSettings.GetIcons: TIcons.TReader;
begin
  Result := FIcons.Reader;
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
