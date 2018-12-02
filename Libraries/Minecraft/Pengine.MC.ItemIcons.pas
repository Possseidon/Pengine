unit Pengine.MC.ItemIcons;

interface

uses
  System.ZIP,
  System.IOUtils,
  System.Classes,
  System.SysUtils,

  GdiPlus,

  Pengine.Settings,
  Pengine.HashCollections,
  Pengine.JSON,
  Pengine.Utility,
  Pengine.IntMaths,
  Pengine.GLFormless,
  Pengine.Light,
  Pengine.Vector,

  Pengine.MC.BlockRenderer,
  Pengine.MC.Assets,
  Pengine.MC.Item,
  Pengine.MC.BlockState,
  Pengine.MC.Namespace;

type

  TItemIconSettings = class(TSettings)
  public const

    DefaultPath = 'Data\assets\itemicons';

  public type

    TIcons = TRefMap<TItemType, IGPBitmap>;

    TGenerator = class
    private
      FIcons: TIcons;

      function GetIcons: TIcons.TReader;

    public
      constructor Create;
      destructor Destroy; override;

      property Icons: TIcons.TReader read GetIcons;

      procedure Generate;

    end;

  private
    FIcons: TIcons;
    FPath: string;

    function GetIcons: TIcons.TReader;
    procedure SetPath(const Value: string);

  protected
    constructor Create(ARoot: TRootSettings); override;

    class function GetNameForVersion(AVersion: Integer): string; override;

    procedure DoReload; override;

  public
    destructor Destroy; override;

    class function SkipSave: Boolean; override;
    class function GetTitle: string; override;

    property Path: string read FPath write SetPath;

    procedure SetDefaults; override;

    property Icons: TIcons.TReader read GetIcons;

  end;

implementation

{ TItemIconSettings }

constructor TItemIconSettings.Create(ARoot: TRootSettings);
begin
  inherited;
  FIcons := TIcons.Create;
  Root.Get<TItemSettings>.AddDependent(Self);
end;

destructor TItemIconSettings.Destroy;
begin
  FIcons.Free;
  inherited;
end;

procedure TItemIconSettings.DoReload;
var
  Item: TItemType;
  FilePath: string;
  // Generator: TGenerator;
  // Pair: TIcons.TPair;
begin
  {
    Generator := TGenerator.Create;
    Generator.Generate;
    for Pair in Generator.Icons do
    Pair.Value.Save(TPath.Combine(Path, Pair.Key.NSPath.Path + '.png'), TGPImageFormat.Png);
    Generator.Free;
  }
  FIcons.Clear;
  for Item in Root.Get<TItemSettings>.Items.Order do
  begin
    FilePath := TPath.Combine(Path, Item.NSPath.Path + '.png');
    FIcons[Item] := TGPBitmap.FromFile(FilePath);
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
  FPath := DefaultPath;
end;

procedure TItemIconSettings.SetPath(const Value: string);
begin
  FPath := Value;
  Reload;
end;

class function TItemIconSettings.SkipSave: Boolean;
begin
  Result := True;
end;

{ TItemIconSettings.TGenerator }

constructor TItemIconSettings.TGenerator.Create;
begin
  FIcons := TIcons.Create;
end;

destructor TItemIconSettings.TGenerator.Destroy;
begin
  FIcons.Free;
  inherited;
end;

procedure TItemIconSettings.TGenerator.Generate;
var
  Item: TItemType;
  Assets: TAssetsSettings;
  ItemModel: TItemModel;
  Icon, Missing: IGPBitmap;
  I: Integer;
  P: TIntVector2;
  BlockRenderer: TModelRenderer;
  ModelRenderable: TModelRenderable;
  Light: TDirectionalLight;
  ColorMatrix: TGPColorMatrix;
  Attributes: IGPImageAttributes;
  Texture: TTexture;

  procedure Blt(AImage: IGPBitmap);
  var
    Graphics: IGPGraphics;
  begin
    if Icon = nil then
      Icon := TGPBitmap.Create(Integer(AImage.Width), Integer(AImage.Height));

    Graphics := TGPGraphics.Create(Icon);
    Graphics.DrawImage(
      AImage,
      TGPRect.Create(0, 0, Icon.Width, Icon.Height),
      0, 0, Icon.Width, Icon.Height,
      TGPUnit.UnitPixel, Attributes);
    Graphics.Flush;
  end;

begin
  FIcons.Clear;

  Attributes := TGPImageAttributes.Create;
  ColorMatrix.SetToIdentity;

  Missing := TGPBitmap.Create(4, 4);
  for P in IVec2(Missing.Width, Missing.Height) do
    Missing.Pixels[P.X, P.Y] := $FF000000 or not(Cardinal(P.X xor P.Y)) and 1 * $FF00FF;

  BlockRenderer := TModelRenderer.Create;
  BlockRenderer.Camera.FOV := 15;
  BlockRenderer.Camera.Location.OffsetZ := 4;
  BlockRenderer.Camera.Location.TurnAngle := 0;

  BlockRenderer.LightSystem.Ambient := 0.25; // $444444;
  Light := TDirectionalLight.Create(BlockRenderer.LightSystem);
  Light.Direction := Vec3(0, -2, -1.2);

  ModelRenderable := TModelRenderable.Create(BlockRenderer.GL.Context);
  ModelRenderable.Location.Offset := -0.5;
  BlockRenderer.Renderable := ModelRenderable;

  Assets := RootSettingsG.Get<TAssetsSettings>;
  for Item in RootSettingsG.Get<TItemSettings>.Items.Order do
  begin
    Icon := nil;
    ColorMatrix.SetToIdentity;
    if Assets.ModelCollection.ItemModels.Get(Item.NSPath.Path, ItemModel) then
    begin
      if ItemModel.IsBuiltinType(btGenerated) then
      begin
        if ItemModel.LayerCount = 0 then
          Blt(Missing)
        else
        begin
          for I := 0 to ItemModel.LayerCount - 1 do
            if ItemModel.Layers[I] is TTextureVariableDirect then
            begin
              Texture := TTextureVariableDirect(ItemModel.Layers[I]).Texture;
              ColorMatrix.M[0, 0] := Texture.TintColor.R;
              ColorMatrix.M[1, 1] := Texture.TintColor.G;
              ColorMatrix.M[2, 2] := Texture.TintColor.B;
              Attributes.SetColorMatrix(ColorMatrix);
              Blt(Texture.Image);
              ColorMatrix.SetToIdentity;
              Attributes.SetColorMatrix(ColorMatrix);
            end;
        end;
      end
      else
      begin
        if ItemModel.GUIDisplay <> nil then
        begin
          ModelRenderable.Location.TurnAngle := -ItemModel.GUIDisplay.Rotation.Y + 15;
          BlockRenderer.Camera.Location.PitchAngle := -ItemModel.GUIDisplay.Rotation.X;
          BlockRenderer.Camera.Location.RollAngle := -ItemModel.GUIDisplay.Rotation.Z;
          ModelRenderable.Location.Pos := ItemModel.GUIDisplay.Translation / 16;
          ModelRenderable.Location.Scale := ItemModel.GUIDisplay.Scale;
        end;
        ModelRenderable.Model := ItemModel;
        Blt(BlockRenderer.RenderImage(128));
      end;
    end
    else
      Blt(Missing);
    FIcons[Item] := Icon;
  end;

  Light.Free;
  ModelRenderable.Free;
  BlockRenderer.Free;
end;

function TItemIconSettings.TGenerator.GetIcons: TIcons.TReader;
begin
  Result := FIcons.Reader;
end;

end.
