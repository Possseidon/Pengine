unit Pengine.MC.BlockRenderer;

interface

uses
  System.SysUtils,

  GdiPlus,

  Pengine.Texture,
  Pengine.ResourceManager,
  Pengine.Vector,
  Pengine.Interfaces,
  Pengine.Camera,
  Pengine.GLProgram,
  Pengine.GLContext,
  Pengine.VAO,
  Pengine.TextureAtlas,
  Pengine.Settings,
  Pengine.GLFormless,
  Pengine.Collections,
  Pengine.GLEnums,
  Pengine.IntMaths,
  Pengine.GLState,
  Pengine.Color,
  Pengine.Light,

  Pengine.MC.BlockState,
  Pengine.MC.Assets,
  Pengine.MC.BlockStructure;

type

  TBlockModelGLProgram = class(TGLProgramResource)
  public type

    TData = packed record
      Pos: TVector3;
      Color: TColorRGB;
      TexCoord: TVector2;
      Border: TBounds2;
      Normal: TVector3;
    end;

  protected
    class function GetAttributeOrder: TGLProgram.TAttributeOrder; override;
    class procedure GetData(out AName: string; out AResource: Boolean); override;

  end;

  TModelRenderableBase = class(TRenderable)
  public type

    TVAO = TVAOMutable<TBlockModelGLProgram.TData>;

    TDataArray = TArray<TBlockModelGLProgram.TData>;

  private
    FContext: TGLContext;
    FAssets: TAssetsSettings;
    FLocation: TLocation3;
    FTextureAtlas: TTextureAtlas;
    FTextureUniform: TGLProgram.TUniformSampler;
    FVAO: TVAO;
    FChanged: Boolean;

    procedure BlocksChanged;
    procedure BuildVAO;

    procedure FillTextureAtlas;

  protected
    function GetLocation: TLocation3; override;

    procedure AddModelData(ADataArray: TDataArray; AModel: TModel);
    procedure AddData(ADataArray: TDataArray); virtual; abstract;

  public
    constructor Create(AContext: TGLContext);
    destructor Destroy; override;

    procedure Render; override;

  end;

  TModelRenderable = class(TModelRenderableBase)
  private
    FModel: TModel;

    procedure SetModel(const Value: TModel);

  protected
    procedure AddData(ADataArray: TModelRenderableBase.TDataArray); override;

  public
    property Model: TModel read FModel write SetModel;

  end;

  TBlockStructureRenderable = class(TModelRenderableBase)
  private
    FStructure: TBlockStructure;

  protected
    procedure AddData(ADataArray: TModelRenderableBase.TDataArray); override;

  public
    constructor Create(AContext: TGLContext);
    destructor Destroy; override;

    property Structure: TBlockStructure read FStructure;

  end;

  TModelRenderer = class
  private
    FGL: TGLFormless;
    FCamera: TCamera;
    FLightSystem: TLightSystem;
    FRenderable: TModelRenderableBase;

    procedure SetRenderable(const Value: TModelRenderableBase);

  public
    constructor Create;
    destructor Destroy; override;

    property GL: TGLFormless read FGL;

    property Camera: TCamera read FCamera;
    property LightSystem: TLightSystem read FLightSystem;
    property Renderable: TModelRenderableBase read FRenderable write SetRenderable;

    function RenderImage(ASize: TIntVector2): IGPBitmap;

  end;

implementation

{ TBlockModelGLProgram }

class function TBlockModelGLProgram.GetAttributeOrder: TGLProgram.TAttributeOrder;
begin
  Result := [
    'vpos',
    'vcolor',
    'vtexcoord',
    'vborderlow',
    'vborderhigh',
    'vnormal'
    ];
end;

class procedure TBlockModelGLProgram.GetData(out AName: string; out AResource: Boolean);
begin
  AName := 'MODEL';
  AResource := True;
end;

{ TBlockRenderer }

procedure TModelRenderableBase.AddModelData(ADataArray: TDataArray; AModel: TModel);
var
  Elements: TModel.TElements.TReader;
  Element: TModel.TElement;
  Dir: TBasicDir3;
  Plane: TPlane3;
  TexTile: TTexTile;
  Data: TVAO.TData;
  TexCoord: TTexCoord2;
  TextureName: string;
  Bounds: TBounds2;
begin
  Elements := AModel.Elements;
  if Elements = nil then
    Exit;
  for Element in Elements do
  begin
    for Dir := Low(TBasicDir3) to High(TBasicDir3) do
    begin
      if not Element.FaceExists(Dir) then
        Continue;
      if not Element.Faces[Dir].ResolveTexture(AModel, TextureName) then
        Continue;
      Plane := Element.Bounds.Plane[Dir];
      TexTile := FTextureAtlas[TextureName];
      Bounds.C1 := Bounds2(0, 1).Convert(Element.Faces[Dir].UV.C1, TexTile.Bounds);
      Bounds.C2 := Bounds2(0, 1).Convert(Element.Faces[Dir].UV.C2, TexTile.Bounds);
      Data.Border := FTextureAtlas.HalfPixelInset(Bounds.Normalize);
      Data.Normal := Plane.Normal;
      if Element.Faces[Dir].Tintindex then
        Data.Color := FAssets.Textures.Textures[TextureName].TintColor
      else
        Data.Color := ColorWhite;
      for TexCoord in QuadTexCoords do
      begin
        Data.Pos := Plane[TexCoord];
        Data.TexCoord := Bounds2(0, 1).Convert(Element.Faces[Dir].UVRotated[TexCoord], TexTile.Bounds);
        ADataArray.Add(Data);
      end;
    end;
  end;
end;

procedure TModelRenderableBase.BlocksChanged;
begin
  FChanged := True;
end;

procedure TModelRenderableBase.BuildVAO;
var
  DataArray: TArray<TVAO.TData>;
begin
  DataArray := TArray<TVAO.TData>.Create;
  DataArray.GrowAmount := 4 * 6; // Data per element
  AddData(DataArray);
  FVAO.VBO.Generate(DataArray, buStaticDraw);
  DataArray.Free;
  FChanged := False;
end;

constructor TModelRenderableBase.Create(AContext: TGLContext);
begin
  FContext := AContext;
  FAssets := RootSettingsG.Get<TAssetsSettings>;
  FLocation := TLocation3.Create;
  FTextureAtlas := TTextureAtlas.Create(FContext.GLState);
  FTextureAtlas.Texture.MagFilter := magNearest;
  FillTextureAtlas;
  FVAO := TVAO.Create(TBlockModelGLProgram.Make(FContext.GLState.ResParam));
  FTextureUniform := FVAO.GLProgram.UniformSampler('diffusemap');
end;

destructor TModelRenderableBase.Destroy;
begin
  TBlockModelGLProgram.Release(FContext.GLState.ResParam);
  FVAO.Free;
  FTextureAtlas.Free;
  FLocation.Free;
  inherited;
end;

procedure TModelRenderableBase.FillTextureAtlas;
var
  Texture: TTexture;
begin
  for Texture in FAssets.Textures.Textures.Values do
    FTextureAtlas.Add(Texture.Name, TTextureData.Create(Texture.Image));
end;

function TModelRenderableBase.GetLocation: TLocation3;
begin
  Result := FLocation;
end;

procedure TModelRenderableBase.Render;
begin
  if FChanged then
    BuildVAO;
  FTextureAtlas.Texture.Bind;
  FTextureAtlas.Uniform(FTextureUniform);
  FVAO.Render;
end;

{ TBlockStructureRenderer }

constructor TModelRenderer.Create;
begin
  FGL := RootSettingsG.Get<TBackgroundGLSettings>.GL;
  FCamera := TCamera.Create(60, 1, 0.01, 100);
  FLightSystem := TLightSystem.Create(GL.Context);
end;

destructor TModelRenderer.Destroy;
begin
  FLightSystem.Free;
  FCamera.Free;
  inherited;
end;

function TModelRenderer.RenderImage(ASize: TIntVector2): IGPBitmap;
begin
  GL.Bind;
  GL.Size := ASize;
  FCamera.Aspect := GL.Aspect;
  GL.Context.GLState.Push;
  GL.Context.GLState[stClearColor] := ColorTransparent;
  GL.Clear;
  FCamera.Render;
  Result := GL.ToImage;
  GL.Context.GLState.Pop;
end;

procedure TModelRenderer.SetRenderable(const Value: TModelRenderableBase);
begin
  if Renderable <> nil then
  begin
    FCamera.RemoveRenderable(Renderable);
    FCamera.RemoveUniforms(Renderable.FVAO.GLProgram);
  end;
  FRenderable := Value;
  if Renderable <> nil then
  begin
    FCamera.AddRenderable(Renderable);
    FCamera.AddUniforms(Renderable.FVAO.GLProgram);
    FLightSystem.BindToGLProgram(Renderable.FVAO.GLProgram);
  end;
end;

{ TBlockStructureRenderable }

procedure TBlockStructureRenderable.AddData(ADataArray: TModelRenderableBase.TDataArray);
begin
  raise ENotImplemented.Create('BlockStructureRendering');
  {
    for P in Structure.Size do
    begin
    if not Structure.Exists(P) then
    Continue;
    Block := Structure[P];
    BlockStateModel := FAssets.BlockStates.Models[Block.NSPath.Path];
    Variants := BlockStateModel.GetVariants(Block.Properties.Value);

    for Variant in Variants do
    begin
    for Element in Variant.Model.Elements do
    begin
    for Dir := Low(TBasicDir3) to High(TBasicDir3) do
    begin
    Plane := CubePlanes[Dir];
    if not Element.FaceExists(Dir) then
    Continue;
    TexTile := FTextureAtlas[Element.Faces[Dir].ResolveTexture];
    Data.Border := TexTile.BoundsHalfPixelInset;
    Data.Normal := Plane.Normal;
    Data.Tangent := Plane.DX;
    Data.Bitangent := Plane.DY;
    for TexCoord in QuadTexCoords do
    begin
    Data.Pos := Plane[TexCoord];
    Data.TexCoord := TexTile.Bounds[TexCoord];
    DataArray.Add(Data);
    end;
    end;
    end;
    end;

    Variants.Free;
    end;
  }
end;

constructor TBlockStructureRenderable.Create(AContext: TGLContext);
begin
  inherited;
  FStructure := TBlockStructure.Create;
  Structure.OnChanged.Add(BlocksChanged);
end;

destructor TBlockStructureRenderable.Destroy;
begin
  FStructure.Free;
  inherited;
end;

{ TModelRenderable }

procedure TModelRenderable.AddData(ADataArray: TModelRenderableBase.TDataArray);
begin
  AddModelData(ADataArray, Model);
end;

procedure TModelRenderable.SetModel(const Value: TModel);
begin
  FModel := Value;
  FChanged := True;
end;

end.
