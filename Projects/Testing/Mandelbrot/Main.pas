unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.UITypes,
  System.Math,
  System.Types,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Clipbrd,

  Pengine.Utility,
  Pengine.InputHandler,
  Pengine.GLEnums,
  Pengine.Collections,
  Pengine.IntMaths,
  Pengine.Vector,
  Pengine.GLProgram,
  Pengine.GLForm,
  Pengine.VAO;

type

  TMandelbrotGLProgram = class(TGLProgramResource)
  protected
    class procedure GetData(out AName: string; out AResource: Boolean); override;
    class function GetAttributeOrder: TGLProgram.TAttributeOrder; override;

  end;

  TGLCanvas = TVAOMutable<TVector2>;

  TfrmMain = class(TGLForm)
  private
    FGLProgram: TGLProgram;
    FCanvas: TGLCanvas;
    FSteps: Integer;
    FScale: Single;
    FOffset: TVector2;
    FDrag: TOpt<TVector2>;
    FTimeUniform: TGLProgram.TUniform<Single>;

    procedure DoRender;
    procedure DoUpdate;

    procedure UpdateFPS;
    procedure DoResize;

    procedure SetSteps(const Value: Integer);
    procedure SetOffset(const Value: TVector2);
    procedure SetScale(const Value: Single);

  public
    procedure Init; override;
    procedure Finalize; override;

    property Steps: Integer read FSteps write SetSteps;
    property Offset: TVector2 read FOffset write SetOffset;
    property Scale: Single read FScale write SetScale;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ TMandelbrotGLProgram }

class function TMandelbrotGLProgram.GetAttributeOrder: TGLProgram.TAttributeOrder;
begin
  Result := ['vpos'];
end;

class procedure TMandelbrotGLProgram.GetData(out AName: string; out AResource: Boolean);
begin
  AResource := True;
  AName := 'MANDELBROT';
end;

{ TfrmMain }

procedure TfrmMain.Init;
begin
  Context.VSync := False;

  FGLProgram := TMandelbrotGLProgram.Make(GLState.ResParam);

  FCanvas := TGLCanvas.Create(FGLProgram, rmTriangles);
  FCanvas.VBO.Generate(6, buStaticDraw, @QuadMiddleCoords[0]);

  Steps := 1000;
  Scale := 0.75;
  //Offset := Vec2(-0.5, 0);

  Game.OnUpdate.Add(DoUpdate);
  Game.OnResize.Add(DoResize);
  Game.OnRender.Add(DoRender);

  Game.Timer.OnFPSUpdate.Add(UpdateFPS);

  FTimeUniform := FGLProgram.Uniform<Single>('time');

end;

procedure TfrmMain.DoRender;
begin
  FCanvas.Render;
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

procedure TfrmMain.SetSteps(const Value: Integer);
begin
  FSteps := Value;
  FGLProgram.Uniform<Integer>('steps').Value := Value;
end;

procedure TfrmMain.UpdateFPS;
begin
  Caption := Format('Mandelbrot - FPS: %d', [Round(Game.FPS)]);
end;

procedure TfrmMain.DoResize;
begin
  FGLProgram.Uniform<Single>('aspect').Value := ClientWidth / ClientHeight;
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

  if Input.KeyTyped(VK_UP) then
    Steps := Steps + 50;
  if Input.KeyTyped(VK_DOWN) then
    Steps := Steps - 50;

  if Input.KeyPressed(VK_RETURN) then
    Clipboard.AsText := Format('Offset := Vec2(%f, %f); Scale := %f;', [Offset.X, Offset.Y, Scale],
      FormatSettings.Invariant);

  FTimeUniform.Value := Game.Time;

end;

procedure TfrmMain.Finalize;
begin
  FCanvas.Free;
  TMandelbrotGLProgram.Release(GLState.ResParam);
end;

end.
