unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.UITypes,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,

  Pengine.IntMaths,
  Pengine.InputHandler,
  Pengine.TimeManager,
  Pengine.EventHandling,
  Pengine.GLEnums,
  Pengine.Utility,
  Pengine.Vector,
  Pengine.GLForm,
  Pengine.GLProgram,
  Pengine.UBO,
  Pengine.VAO;

const

  GradientSizeX = 16;
  GradientSizeY = 16;
  GradientSizeZ = 16;
  GradientSize: TIntVector3 = (X: GradientSizeX; Y: GradientSizeY; Z: GradientSizeZ);

type

  TPerlinNoiseGLProgram = class(TGLProgramResource)
  protected
    class function GetAttributeOrder: TGLProgram.TAttributeOrder; override;
    class procedure GetData(out AName: string; out AResource: Boolean); override;

  end;

  TGLCanvas = TVAOMutable<TVector2>;

  TGradientData = record
    Data: array [0 .. GradientSizeX - 1, 0 .. GradientSizeY - 1, 0 .. GradientSizeZ - 1] of record
      Vec: TVector3;
      Filler: Single;
    end;
  end;

  TfrmMain = class(TGLForm)
  private
    FGLProgram: TGLProgram;
    FCanvas: TGLCanvas;
    FScale: Single;
    FOffset: TVector2;
    FDrag: TOpt<TVector2>;
    FTimeUniform: TGLProgram.TUniform<Single>;
    FGradientUBO: TUBO<TGradientData>;

    procedure DoRender;
    procedure DoUpdate;

    procedure UpdateFPS(const ADeltaTimer: TDeltaTimer);
    procedure DoResize;

    procedure SetOffset(const Value: TVector2);
    procedure SetScale(const Value: Single);

  public
    procedure Init; override;
    procedure Finalize; override;

    property Offset: TVector2 read FOffset write SetOffset;
    property Scale: Single read FScale write SetScale;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ TPerlinNoiseGLProgram }

class function TPerlinNoiseGLProgram.GetAttributeOrder: TGLProgram.TAttributeOrder;
begin
  Result := ['vpos'];
end;

class procedure TPerlinNoiseGLProgram.GetData(out AName: string; out AResource: Boolean);
begin
  AResource := True;
  AName := 'PERLIN';
end;

{ TForm10 }

procedure TfrmMain.DoRender;
begin
  FCanvas.Render;
end;

procedure TfrmMain.DoUpdate;
var
  OldOffset: TVector2;
begin
  if Input.ButtonPressed(mbLeft) then
    FDrag := Input.MousePos;
  if Input.ButtonReleased(mbLeft) then
    FDrag.Clear;
  if FDrag.HasValue and Input.MouseMoved then
  begin
    Offset := Offset + FDrag - Input.MousePos;
    FDrag := Input.MousePos;
  end;

  if Input.Scrolled then
    OldOffset := (Offset + Input.MousePos) / Scale;
  if Input.ScrolledUp then
    Scale := Scale / 0.8;
  if Input.ScrolledDown then
    Scale := Scale * 0.8;
  if Input.Scrolled then
    Offset := OldOffset * Scale - Input.MousePos;

  FTimeUniform.Value := Game.Time;

end;

procedure TfrmMain.UpdateFPS(const ADeltaTimer: TDeltaTimer);
begin
  Caption := Format('Perlin Noise - FPS: %d', [Round(Game.FPS)]);
end;

procedure TfrmMain.DoResize;
begin
  FGLProgram.Uniform<Single>('aspect').Value := Game.Aspect;
end;

procedure TfrmMain.SetOffset(const Value: TVector2);
begin
  FOffset := Value;
  FGLProgram.Uniform<TVector2>('offset').Value := Value;
end;

procedure TfrmMain.SetScale(const Value: Single);
begin
  FScale := Value;
  FGLProgram.Uniform<Single>('scale').Value := Value;
end;

procedure TfrmMain.Init;
var
  P: TIntVector3;
  GradientData: TGradientData;
begin
  Context.VSync := True;
  Context.MultiSampled := False;
  Fullscreen := False;

  FGLProgram := TPerlinNoiseGLProgram.Make(GLState.ResParam);

  FCanvas := TGLCanvas.Create(FGLProgram, rmTriangles);
  FCanvas.VBO.Generate(6, buStaticDraw, @QuadMiddleCoords[0]);

  Scale := 4;
  Offset := Vec2(0, 0);

  Game.OnUpdate.Add(DoUpdate);
  Game.OnResize.Add(DoResize);
  Game.OnRender.Add(DoRender);

  for P in GradientSize do
    GradientData.Data[P.X, P.Y, P.Z].Vec := TVector3.RandomNormal;

  FGradientUBO := TUBO<TGradientData>.Create(GLState, buStreamDraw);
  FGradientUBO.BindToGLProgram(FGLProgram, 'gradients');
  FGradientUBO.SubData(0, SizeOf(TGradientData), GradientData);

  Game.Timer.OnFPSUpdate.Add(UpdateFPS);

  FTimeUniform := FGLProgram.Uniform<Single>('time');
end;

procedure TfrmMain.Finalize;
begin
  FGradientUBO.Free;
  FCanvas.Free;
  TPerlinNoiseGLProgram.Release(GLState.ResParam);
end;

end.
