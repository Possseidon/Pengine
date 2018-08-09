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

  Pengine.GLForm,
  Pengine.GUI,
  Pengine.GUIControls,
  Pengine.SpriteSystem,
  Pengine.GLProgram,
  Pengine.Vector,
  Pengine.TimeManager,

  BilliardGame,
  BilliardControl,
  Pengine.InputHandler;

type

  TSpriteGLProgram = class(TSpriteGLProgramBase)
  protected
    class procedure GetData(out AName: string; out AResource: Boolean); override;

  end;

  TfrmMain = class(TGLForm)
  private
    FSpriteGLProgram: TGLProgram;
    FGUI: TGUI;
    FBilliard: TBilliard;
    FBilliardControl: TBilliardControl;

    procedure GameUpdate;
    procedure ButtonDown(AInfo: TButtonEventInfo);
    procedure FPSUpdate;

    procedure CueBallCollideBall(AInfo: TBilliard.TBall.TCollideBallEventInfo);

  public
    procedure Init; override;
    procedure Finalize; override;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ TfrmMain }

procedure TfrmMain.GameUpdate;
var
  V: TVector2;
begin
  if Input.ButtonDown(mbRight) then
  begin
    V := FBilliard.CueBall.Pos.VectorTo(FBilliardControl.InnerBounds.InvPoint[Input.MousePos]);
    V := V.Normalize * 0.5;
    FBilliard.HitCueBall(V);
  end;
  FBilliard.Step(DeltaTime);
end;

procedure TfrmMain.Init;
var
  Button: TButton;
begin
  Context.VSync := True;
  Context.Samples := Context.MaxSamples;

  FBilliard := TBilliard.Create;

  FSpriteGLProgram := TSpriteGLProgram.Make(GLState.ResParam);

  FGUI := TGUI.Create(Game, FSpriteGLProgram);
  // FGUI.TextureAtlas.AddFromFile(TBilliardControl.DefaultBallTextureName, 'Data\Textures\BilliardBalls.png');
  // FGUI.TextureAtlas.AddFromFile(TBilliardControl.DefaultTableTextureName, 'Data\Textures\BilliardTable.png');
  // FGUI.TextureAtlas.AddFromFile('font', 'Data\Textures\font.png');
  FGUI.TextureAtlas.AddFromResource(TBilliardControl.DefaultBallTextureName, 'BILLIARD_BALLS');
  FGUI.TextureAtlas.AddFromResource(TBilliardControl.DefaultBallReflectionTextureName, 'BILLIARD_BALL_REFLECTION');
  FGUI.TextureAtlas.AddFromResource(TBilliardControl.DefaultTableTextureName, 'BILLIARD_TABLE');
  FGUI.TextureAtlas.AddFromResource(TButton.DefaultTextureName, 'STONE_BUTTON');
  FGUI.TextureAtlas.AddFromResource('font', 'FONT');

  FBilliardControl := FGUI.Add<TBilliardControl>;
  FBilliardControl.Billiard := FBilliard;
  FBilliardControl.Origin := Origin(oxCenter, oyBottom);
  FBilliardControl.Location.Scale := 1.7;

  Button := FGUI.Add<TButton>;
  Button.Origin := Origin(oxCenter, oyTop);
  Button.Location.Scale := 0.2;
  Button.Location.Offset := Vec2(0, -0.25);
  Button.Width := 5;
  Button.Caption := 'Reset';
  Button.OnPressed.Add(FBilliard.Reset);

  Game.OnUpdate.Add(GameUpdate);
  Game.Timer.OnFPSUpdate.Add(FPSUpdate);
  Input.OnButtonDown.Add(ButtonDown);

  FBilliard.CueBall.OnCollideBall.Add(CueBallCollideBall);
end;

procedure TfrmMain.ButtonDown(AInfo: TButtonEventInfo);
var
  V: TVector2;
begin
  case AInfo.Button of
    mbLeft:
      begin
        if not(Input.MousePos in FBilliardControl.Bounds) then
          Exit;

        V := FBilliard.CueBall.Pos.VectorTo(FBilliardControl.InnerBounds.InvPoint[Input.MousePos]);
        FBilliard.CueBall.AngVelocity := 0;
        FBilliard.CueBall.Velocity := V * 5;

        // StartTimer;
        // FBilliard.SimulateAll(1 / 60);
        // ShowMessage(Format('Simulation took %s.', [StopTimerGetString]));
      end;
  end;
end;

procedure TfrmMain.CueBallCollideBall(AInfo: TBilliard.TBall.TCollideBallEventInfo);
begin
  // AInfo.Sender.Velocity := 0;
end;

procedure TfrmMain.Finalize;
begin
  TSpriteGLProgram.Release(GLState.ResParam);
  FGUI.Free;
  FBilliard.Free;
end;

procedure TfrmMain.FPSUpdate;
begin
  Caption := Format('Billiard AI - FPS: %.0f', [Game.FPS]);
end;

{ TSpriteGLProgram }

class procedure TSpriteGLProgram.GetData(out AName: string; out AResource: Boolean);
begin
  AResource := True;
  if AResource then
    AName := 'SPRITE'
  else
    AName := 'Data\Shaders\sprite';
end;

end.
