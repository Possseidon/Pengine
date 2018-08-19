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
    FInfoLabel: TLabel;

    procedure GameUpdate;
    procedure ButtonDown(AInfo: TButtonEventInfo);
    procedure FPSUpdate;
    procedure StateChanged;

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
begin
  FBilliard.Step(DeltaTime);
end;

procedure TfrmMain.Init;
var
  Button: TButton;
begin
  Context.VSync := True;
  Context.Samples := Context.MaxSamples;

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

  FInfoLabel := FGUI.Add<TLabel>;
  FInfoLabel.Origin := Origin(oxLeft, oyTop);
  FInfoLabel.Location.Scale := 0.2;
  FInfoLabel.Location.Offset := Vec2(0.25, -0.25);

  FBilliard := TBilliard.Create;

  FBilliardControl := FGUI.Add<TBilliardControl>;
  FBilliardControl.Billiard := FBilliard;
  FBilliardControl.Origin := Origin(oxCenter, oyBottom);
  FBilliardControl.Location.Scale := 1.7;

  Button := FGUI.Add<TButton>;
  Button.Origin := Origin(oxRight, oyTop);
  Button.Location.Scale := 0.2;
  Button.Location.Offset := -0.25;
  Button.Width := 5;
  Button.Caption := 'Reset';
  Button.OnPressed.Add(FBilliard.Reset);

  Game.OnUpdate.Add(GameUpdate);
  Game.Timer.OnFPSUpdate.Add(FPSUpdate);
  Input.OnButtonDown.Add(ButtonDown);
  FBilliard.OnStateChange.Add(StateChanged);
  StateChanged;

end;

procedure TfrmMain.StateChanged;
begin
  case FBilliard.State of
    bsWaitingForInput:
      case FBilliard.CurrentPlayer.Order of
        poFirst:
          FInfoLabel.Caption := 'Player 1 Turn';
        poSecond:
          FInfoLabel.Caption := 'Player 2 Turn';
      end;
    bsRunning:
      FInfoLabel.Caption := 'Wating for balls...';
    bsCuePlacement:
      case FBilliard.CurrentPlayer.Order of
        poFirst:
          FInfoLabel.Caption := 'Player 1 place Cue';
        poSecond:
          FInfoLabel.Caption := 'Player 2 place Cue';
      end;
  end;
end;

procedure TfrmMain.ButtonDown(AInfo: TButtonEventInfo);
var
  V: TVector2;
begin
  case FBilliard.State of
    bsWaitingForInput:
      if AInfo.Button = mbLeft then
      begin
        if not FBilliardControl.MouseHovered then
          Exit;

        V := FBilliard.CueBall.Pos.VectorTo(FBilliardControl.InnerBounds.InvPoint[Input.MousePos]);
        FBilliard.CurrentPlayer.MakeTurn(V * 5);

      end;
    bsRunning:
      ;
    bsCuePlacement:
      ;
  end;
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
