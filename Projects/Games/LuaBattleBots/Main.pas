unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, OpenGLContext, Camera, Shaders, VAOManager, VectorGeometry, IntfBase,
  Matrix, Lists, TextureManager, Lights;

type

  { TForm1 }

  TForm1 = class(TGLForm)
  private
    FCamera: TCamera;
    FShader: TShader;
    FFloorVAO: TVAO;
    FCubeVAO: TVAO;

    FSun: TDirectionalLight;
    
    FTexturePage: TTexturePage;

    FCubes: TObjectArray<TVAOProxy>;

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

end;

procedure TForm1.Init;
var
  Cube: TVAOProxy;
begin

  FTexturePage := TTexturePage.Create;
  FTexturePage.AddTextureFromFile('data/stone_bricks');
  FTexturePage.BuildPage(16);

  FShader := TShader.Create;
  FShader.LoadFromFile('data/shader');

  FCamera := TCamera.Create(60, Aspect, 0.1, 100);
  FCamera.AddUniforms(FShader);

  FFloorVAO := TVAO.Create(FShader);


  FCubeVAO := TVAO.Create(FShader);

  FCubes := TObjectArray<TVAOProxy>.Create;
  
  FCamera.AddRenderObject(FFloorVAO);
  for Cube in FCubes do
    FCamera.AddRenderObject(Cube);     
end;

procedure TForm1.RenderFunc;
begin
  FCamera.Render;
end;

procedure TForm1.ResizeFunc;
begin
  FCamera.Aspect := Aspect;
end;

procedure TForm1.UpdateFunc;
begin

end;

end.
