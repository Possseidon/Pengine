unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, OpenGLContext, Camera, VAOManager, Shaders, Lists, Color, Matrix,
  VectorGeometry, GLEnums, ControlledCamera, TextureManager, dglOpenGL, InputHandler, Lights, SkyDome,
  IntegerMaths, Math, Particles;

type

  TData = record
    Pos: TVector3;
    TexCoord: TTexCoord2;
    Normal: TVector3;
    Tangent: TVector3;
    Bitangent: TVector3;
    Border: TBounds2;
  end;

  TMyParticle = class (TBasicParticle)
  private
    FVelocity: TVector3;
    FAngularSpeed: Single;
    FTime: Single;
    FParent: TBasicParticleGen;

  public
    constructor Create(APos: TVector3; AParent: TBasicParticleGen);
    procedure Update(ADeltaTime: Single); override;

    class function GetMode: TParticleMode; override;
  end;

  TMyParticleGen = class (TBasicParticleGen)
  private
    FTime: Single;
  public
    procedure Update(ADeltaTime: Single); override;
  end;

  { TfrmMain }

  TfrmMain = class(TGLForm)
  private
    FCamera: TCamera;
    FVAO: TVAO;
    FTexturePage: TTexturePage;
    FShader: TShader;
    FParticleShader: TShader;

    FLightSystem: TLightSystem;
    FLights: TObjectArray<TPointLight>;

    FSkyDome: TSkyDome;

    FParticleGen: TMyParticleGen;
    FParticleSystem: TParticleSystem;

    procedure InitVAO;
    procedure InitTexture;
    procedure InitShader;
    procedure InitCamera;
    procedure InitLightSystem;
    procedure InitSkyDome;
    procedure InitParticles;

  public
    procedure Init; override;
    procedure Finalize; override;

    procedure ResizeFunc; override;
    procedure UpdateFunc; override;
    procedure RenderFunc; override;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}


{ TMyParticle }

constructor TMyParticle.Create(APos: TVector3; AParent: TBasicParticleGen);
begin
  inherited Create(APos, 0, 0.3, ColorYellow, 'log_side');
  FParent := AParent;
  FVelocity := Vec3(0, 1, 0).Cross(FPos);
  FAngularSpeed := Random * 360 - 180;
end;

procedure TMyParticle.Update(ADeltaTime: Single);
var
  C: TVector3;
begin
  inherited;
  FRotation := FRotation + ADeltaTime * FAngularSpeed;
  C := FPos.VectorTo(0).Normalize;
  FVelocity := FVelocity + C / Sqr(C.Length) * ADeltaTime;
  FPos := FPos + FVelocity * ADeltaTime;
  FTime := FTime - ADeltaTime;
  if FTime <= 0 then
  begin
    // FParent.AddParticle(TParticleTrace.Create(FPos, -FVelocity + TGVector3.Random * 0.08, ColorOrange * 0.5));
    FTime := FTime + 0.01;
  end;
end;

class function TMyParticle.GetMode: TParticleMode;
begin
  Result := pmRealAlpha;
end;

{ TMyParticleGen }

procedure TMyParticleGen.Update(ADeltaTime: Single);
var
  I: Integer;
begin
  FTime := FTime - ADeltaTime;
  while FTime <= 0 do
  begin
    for I := 0 to 2 do
      AddParticle(TMyParticle.Create(TVector3.RandomNormal, Self));
    FTime := FTime + Infinity;
  end;
end;

{ TfrmMain }

procedure TfrmMain.Init;
begin
  VSync := False;
  // State.DebugOutput := True;
  // State.DebugOutputSynced := True;
  InitShader;
  InitTexture;
  InitVAO;
  InitCamera;
  InitLightSystem;
  InitSkyDome;
  InitParticles;
end;

procedure TfrmMain.Finalize;
begin
  FParticleSystem.Free;
  FLights.Free;
  FLightSystem.Free;
  FTexturePage.Free;
  FShader.Free;
  FParticleShader.Free;
  FCamera.Free;
  FVAO.Free;
end;

procedure TfrmMain.RenderFunc;
begin
  FLightSystem.RenderShadows;
  FCamera.Render;
end;

procedure TfrmMain.InitCamera;
begin
  FCamera := TControlledCamera.Create(60, ClientWidth / ClientHeight, 0.1, 1000, Input);
  FCamera.Location.OffsetZ := 5;
  FCamera.AddUniforms(FShader);
  FCamera.AddRenderObject(FVAO);
end;

procedure TfrmMain.InitLightSystem;
var
  I: Integer;
  Light: TPointLightShaded;
begin
  FLightSystem := TLightSystem.Create(Self);
  FLightSystem.Ambient := TColorRGB.Gray(0.05);
  FLightSystem.BindToShader(FShader);

  FLights := TObjectArray<TPointLight>.Create;
  for I := 0 to 0 do
  begin
    Light := TPointLightShaded.Create(FLightSystem);
    Light.Attenuation := 0.05;
    //Light.Color := TColorRGB.Rainbow(I / 3 * 6) * 0.5;
    //Light.Position := TVector3.RandomNormal * 2;
    Light.AddOccluder(FVAO);
    FLights.Add(Light);
  end;

end;

procedure TfrmMain.InitParticles;
begin
  FParticleShader := TShader.Create;
  FParticleShader.LoadFromFile('Data/particle');
  FParticleShader.SetAttributeOrder(['vpos', 'vrotation', 'voffset', 'vcolor', 'vtexcoord']);
  FCamera.AddUniforms(FParticleShader);

  FParticleGen := TMyParticleGen.Create;
  
  FParticleSystem := TParticleSystem.Create(FParticleShader, FCamera, Self);
  FParticleSystem.AddTextureFromFile('Data/log_side.png', 'log_side');          
  FParticleSystem.AddGenerator(FParticleGen);

  FCamera.AddRenderObject(FParticleSystem);
end;

procedure TfrmMain.InitShader;
const
  Attributes: array [0 .. 6] of AnsiString = (
    'vpos',
    'vtexcoord',
    'vnormal',
    'vtangent',
    'vbitangent',
    'vborderlow',
    'vborderhigh'
  );
begin
  FShader := TShader.Create;
  FShader.LoadFromFile('Data/model');
  FShader.SetAttributeOrder(Attributes);
end;

procedure TfrmMain.InitSkyDome;
begin
  // FSkyDome := TSkyDome.Create(Self, FCamera, FSkyDomeShader);
  // FSkyDome.AddStripe
end;

procedure TfrmMain.InitTexture;
begin
  FTexturePage := TTexturePage.Create;
  FTexturePage.UniformDefaults(FShader);
                                                                       
  FTexturePage.AddTextureFromFile('Data/stone_bricks.png');
  FTexturePage.AddTextureFromFile('Data/log_side.png');
  FTexturePage.BuildPage(32);              

end;

procedure TfrmMain.InitVAO;
var
  Data: TData;
  C: TTexCoord2;
  P: TPlane3;
  Pos: TIntVector3;
begin
  FVAO := TVAO.Create(FShader);
  FVAO.GLLabel := 'Texture-Test Plane';
  FVAO.Generate(6 * 6 * 21 * 21 * 21, buStaticDraw);
  FVAO.Map(baWriteOnly);
                     
  for Pos in Range3(-10, +11) do
  begin       
    if not InRange(TVector3(Pos - Vec3(0, 3, 0)).Length, 13, 14) then
      Continue;    
  
    for P in CubePlanes do
    begin
      Data.Normal := P.NormalX;
      Data.Tangent := P.DVS;
      Data.Bitangent := P.DVT;     
      Data.Border := FRange2(1 / 128, 0.5 - 1 / 128);
      for C in QuadTexCoords do
      begin
        Data.Pos := Pos * 2 + P[C] * 2 - 1;
        Data.TexCoord := C * 0.5;
        FVAO.AddVertex(Data);
      end;
    end;     
  end;                       

  {
  for P in CubePlanes do
  begin
    Data.Normal := P.NormalX;
    Data.Tangent := -P.DVS;
    Data.Bitangent := -P.DVT;
    Data.Border := FRange2(1 / 128, 0.5 - 1 / 128) + Vec2(0.5, 0);
    for C in QuadTexCoords do
    begin
      Data.Pos := -P[C] * 30 + 15;
      Data.TexCoord := (C * 0.5) + Vec2(0.5, 0);
      FVAO.AddVertex(Data);
    end;
  end;
  }

  FVAO.Unmap;
end;

procedure TfrmMain.ResizeFunc;
begin
  FCamera.Aspect := ClientWidth / ClientHeight;
end;

procedure TfrmMain.UpdateFunc;
var
  Light: TPointLight;
begin
  if MustUpdateFPS then
    Caption := Format('Physics 3D Test - FPS: %d', [FPSInt]);

  if FCamera is TControlledCamera then
    TControlledCamera(FCamera).Update;

  FParticleSystem.Update(DeltaTime);

  for Light in FLights do
  begin
    Light.Position := Light.Position.Rotate(Vec3(0, 1, 0), DeltaTime * 20);
    if Input.ButtonDown(mbMiddle) then
      Light.Position := FCamera.Location.RealPosition;
  end;

  if Input.KeyPressed(VK_SPACE) then
    Samples := 32;
end;

end.
