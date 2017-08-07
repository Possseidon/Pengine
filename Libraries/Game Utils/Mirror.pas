unit Mirror;

interface

uses
  FBOManager, VAOManager, VectorGeometry, TextureManager, Camera, GLEnums, SysUtils, Shaders;

type

  { TMirror }

  TMirror = class (TAutoUpdateVAO)
  private
    type
      TData = record
        Pos: TGVector3;
        TexCoord: TTexCoord2;
      end;

  private
    FLocation: TLocation;
    FSize: TGVector2;

    FFBO: TFBO;
    FCamera: TCamera;
    FReflectedCamera: TCamera;

    procedure SetHeight(AValue: Single);
    procedure SetSize(AValue: TGVector2);
    procedure SetWidth(AValue: Single);

  protected
    procedure BuildVAO; override;

    procedure BeforeRender; override;

  public
    constructor Create(AShader: TShader; ACamera: TCamera);
    destructor Destroy; override;

    property Location: TLocation read FLocation;
    property Size: TGVector2 read FSize write SetSize;
    property Width: Single read FSize.X write SetWidth;
    property Height: Single read FSize.Y write SetHeight;

    // not a good solution, rewrite camera so it has a TObjectArray<TVAO> to render or get a better idea!
    procedure BeginRenderReflection;

  end;

implementation

{ TMirror }

procedure TMirror.SetHeight(AValue: Single);
begin
  if FSize.X = AValue then
    Exit;
  FSize.X := AValue;
end;

procedure TMirror.SetSize(AValue: TGVector2);
begin
  if FSize = AValue then
    Exit;
  FSize := AValue;
end;

procedure TMirror.SetWidth(AValue: Single);
begin
  if FSize.Y = AValue then
    Exit;
  FSize.Y := AValue;
end;

procedure TMirror.BeforeRender;
begin
  inherited BeforeRender;
  FCamera.SetModelLocation(Location);
end;

procedure TMirror.BuildVAO;
var
  Data: TData;
  I: TQuadSide;
begin
  Map(baWriteOnly);

  for I := Low(TQuadSide) to High(TQuadSide) do
  begin
    Data.Pos := QuadTexCoords[I];
    Data.TexCoord := QuadTexCoords[I];
    AddVertex(Data);
  end;

  Unmap;
end;

constructor TMirror.Create(AShader: TShader; ACamera: TCamera);
begin
  inherited Create(AShader);
  FCamera := ACamera;
  FFBO := TFBO.Create(512, 512);
  FFBO.Enable2DTexture(fbaColor, pfRGB);
  if not FFBO.Finish then
    raise Exception.Create('Could not initialize Mirror Framebuffer!');
  FLocation := TLocation.Create;
  FFBO.Textures[fbaColor].Uniform(AShader, 'reflection');

  FReflectedCamera := TCamera.Create(FCamera.FOV, FCamera.Aspect, FCamera.NearClip, FCamera.FarClip);
end;

destructor TMirror.Destroy;
begin
  FReflectedCamera.Free;
  FFBO.Free;
  FLocation.Free;
  inherited Destroy;
end;

procedure TMirror.BeginRenderReflection;
begin
  FFBO.Bind;
  FReflectedCamera.Location.Assign(FCamera.Location);
  FReflectedCamera.Location.FreeMirror(TGLine.Create(FLocation.RealPosition, FLocation.Look));
end;

end.
