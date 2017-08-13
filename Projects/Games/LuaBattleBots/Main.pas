unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, OpenGLContext, Camera, Shaders, VAOManager, VectorGeometry, IntfBase,
  Matrix, Lists, TextureManager, Lights, ControlledCamera, GLEnums, IntegerMaths, Color, SkyDome, LuaHeader;

type

  TData = record
    Pos: TVector3;
    TexCoord: TTexCoord2;
    Normal: TVector3;
    Tangent: TVector3;
    Bitangent: TVector3;
    Border: TBounds2;
  end;  

  { TForm1 }

  TForm1 = class(TGLForm)
  private
    FShader: TShader;
    FCamera: TControlledCamera;
    FTexturePage: TTexturePage;
    FFloorVAO: TVAO;
    FCubeVAO: TVAO;
    FCubes: TObjectArray<TVAOProxy>;

    FLightSystem: TLightSystem;
    FSun: TDirectionalLightShaded;

    FSkyDomeShader: TShader;
    FSkyDome: TSkyDome;

    procedure InitCamera;
    procedure InitShader;
    procedure InitSkyDomeShader;
    procedure InitTexturePage;
    procedure InitFloorVAO;
    procedure InitCubeVAO;
    procedure InitCubes;
    procedure InitLightSystem;
    procedure InitSkyDome;

  public
    procedure Init; override;
    procedure Finalize; override;

    procedure ResizeFunc; override;
    procedure UpdateFunc; override;
    procedure RenderFunc; override;

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.Finalize;
begin
  FSkyDome.Free;
  FSkyDomeShader.Free;
  FSun.Free;
  FLightSystem.Free;
  FCubes.Free;
  FCubeVAO.Free;
  FFloorVAO.Free;
  FTexturePage.Free;
  FCamera.Free;
  FShader.Free;
end;

procedure TForm1.Init;
begin
  State.DebugOutput := False;
  VSync := True;
  // FPSLimit := 300;

  InitCamera;

  InitShader;
  InitSkyDomeShader;
  
  InitSkyDome;
  
  InitTexturePage;

  InitFloorVAO;
  InitCubeVAO;
  InitCubes;

  InitLightSystem;
  
end;

procedure TForm1.InitCamera;
begin
  FCamera := TControlledCamera.Create(60, Aspect, 0.05, 420, Input);
  FCamera.Location.OffsetZ := 3;
  FCamera.Location.PitchAngle := -20;
  FCamera.Location.TurnAngle := -30;
  FCamera.PitchUpperLimit := -4.2;
  FCamera.PosLowerLimitY := 0.1;
end;

procedure TForm1.InitCubes;
var
  Cube: TVAOProxy;
begin
  FCubes := TObjectArray<TVAOProxy>.Create;

  FCamera.AddRenderObject(FFloorVAO);
  for Cube in FCubes do
    FCamera.AddRenderObject(Cube);
end;

procedure TForm1.InitCubeVAO;
begin
  FCubeVAO := TVAO.Create(FShader);
end;

procedure TForm1.InitFloorVAO;
const
  Plane: TPlane3 = (
    SV: (X: 0; Y: 0; Z: 0);
    DVS: (X: 0; Y: 0; Z: 1);
    DVT: (X: 1; Y: 0; Z: 0);
  );
var
  Data: TData;
  T: TVector2;
  GridPos: TIntVector2;
  Grid: TIntBounds2;
begin
  Grid.Create(-20, 20);
  
  FFloorVAO := TVAO.Create(FShader);
  FFloorVAO.Generate(6 * Grid.Area, buStaticDraw);

  FFloorVAO.Map(baWriteOnly);

  Data.Normal := Vec3(0, 1, 0);
  Data.Tangent := Vec3(1, 0, 0);
  Data.Bitangent := Vec3(0, 0, 1);
  Data.Border := FTexturePage.GetTexBounds('Data/grass_top.png', FRange2(0, 1));
  
  for GridPos in Grid do
  begin
    for T in QuadTexCoords do
    begin
      Data.Pos := Plane[T];
      Data.Pos.XZ := Data.Pos.XZ + GridPos;
      Data.TexCoord := Data.Border[T];
      FFloorVAO.AddVertex(Data);  
    end; 
  end;   

  Data.Border := FTexturePage.HalfPixelInset(Data.Border);

  FFloorVAO.Unmap;
  
  FCamera.AddRenderObject(FFloorVAO);
end;

procedure TForm1.InitLightSystem;
begin
  FLightSystem := TLightSystem.Create(Self);
  FLightSystem.Ambient := TColorRGB.Gray(0.2);
  FLightSystem.BindToShader(FShader);

  FSun := TDirectionalLightShaded.Create(FLightSystem);
  FSun.Direction := Vec3(-1, -2, -1).Normalize;
  FSun.Color := TColorRGB.Gray(0.8);
  FSun.Size := 60;
  FSun.AddOccluder(FFloorVAO);
end;

procedure TForm1.InitShader;
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
  FShader.Uniform<Boolean>('depthonly').Value := False;
  
  FCamera.AddUniforms(FShader);
end;

procedure TForm1.InitSkyDome;
begin
 FSkyDome := TSkyDome.Create(Self, FCamera, FSkyDomeShader);
 FSkyDome.AddStripe(TColorRGB.Create(0.7, 1.0, 0.9), -90);
 FSkyDome.AddStripe(TColorRGB.Create(0.4, 0.6, 0.9), 0);
 FSkyDome.AddStripe(TColorRGB.Create(0.1, 0.2, 0.9), +90);

 FCamera.AddRenderObject(FSkyDome); 
end;

procedure TForm1.InitSkyDomeShader;
begin
  FSkyDomeShader := TShader.Create;
  FSkyDomeShader.LoadFromFile('Data/skydome');
  FSkyDomeShader.SetAttributeOrder(['vpos', 'vpitch']);

  FCamera.AddUniforms(FSkyDomeShader);
end;

procedure TForm1.InitTexturePage;
begin
  FTexturePage := TTexturePage.Create;
  FTexturePage.UniformDefaults(FShader);
  FTexturePage.AddTextureFromFile('Data/stone_bricks.png');
  FTexturePage.AddTextureFromFile('Data/grass_top.png');
  FTexturePage.AddTextureFromFile('Data/log_side.png');
  FTexturePage.BuildPage(32);
end;

procedure TForm1.RenderFunc;
begin
  FLightSystem.RenderShadows;
  FCamera.Render;
end;

procedure TForm1.ResizeFunc;
begin
  FCamera.Aspect := Aspect;
end;

procedure TForm1.UpdateFunc;
begin
  if MustUpdateFPS then
    Caption := Format('LuaBattleBots - FPS: %d', [FPSInt]);

  // if Input.ButtonPressed(mbMiddle) then
  //   FSun.Position := FCamera.Location.RealPosition;
  // FSun.Direction := FSun.Direction.Rotate(Vec3(1, 1, 0).Normalize, 30 * DeltaTime);
    
  FCamera.Update;
end;

end.
