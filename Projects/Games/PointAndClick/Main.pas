unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,

  Pengine.GLEnums,
  Pengine.GLContext,
  Pengine.SpriteSystem,
  Pengine.GLProgram,
  Pengine.GLState,
  Pengine.Color,
  Pengine.InputHandler,
  Pengine.Vector,
  Pengine.EventHandling,
  Pengine.IntMaths;

type

  TSpriteGLProgram = class(TSpriteGLProgamBase)
  protected
    class procedure GetData(out AName: string; out AResource: Boolean); override;
  end;

  TRainbowBehavior = class(TSprite.TBehavior)
  private
    FCurrent: Single;
    FSpeed: Single;

    procedure ButtonDown(AInfo: TButtonEventInfo);
    procedure Scroll(AInfo: TScrollEventInfo);

  protected
    procedure AddEvents; override;
    procedure DelEvents; override;

  public
    constructor Create(ASprite: TSprite); overload; override;
    constructor Create(ASprite: TSprite; ASpeed: Single); reintroduce; overload;

    procedure Update; override;

    property Speed: Single read FSpeed write FSpeed;

  end;

  TAnimationBehavior = class(TSprite.TBehavior)
  private
    FFrame: Integer;
    FTimeLeft: Single;
  public
    procedure Update; override;
  end;

  TFollowMouseBehavior = class(TSprite.TBehavior)
  private
    procedure MouseMove;
  protected
    procedure AddEvents; override;
    procedure DelEvents; override;
  end;

  THoverBehavior = class(TSprite.TBehavior)
  private
    FHover: Boolean;
    FFade: TSprite.TFadeBehavior;

    procedure MouseMove;
    procedure HoverDone(AInfo: TSprite.TBehavior.TSenderEventInfo);
    
  protected
    procedure AddEvents; override;
    procedure DelEvents; override;
    
  end;

  TfrmMain = class(TGLForm)
  private
    FSpriteGLProgram: TGLProgram;
    FSpriteSystem: TSpriteSystem;

    procedure KeyDown(AInfo: TKeyEventInfo); reintroduce;

  public
    procedure Init; override;
    procedure Finalize; override;

    procedure GameUpdate;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ TfrmMain }

procedure TfrmMain.Init;
var
  Sprite: TSprite;
begin
  Context.VSync := False;

  Context.Samples := Context.MaxSamples;

  FSpriteGLProgram := TSpriteGLProgram.Make(GLState.ResParam);

  FSpriteSystem := TSpriteSystem.Create(Game, FSpriteGLProgram);
  FSpriteSystem.SpriteAtlas.Texture.MagFilter := magNearest;
  FSpriteSystem.SpriteAtlas.AddFromFile('log', 'Data/log.png');
  FSpriteSystem.SpriteAtlas.AddFromFile('bricks', 'Data/bricks.png');
  FSpriteSystem.SpriteAtlas.AddFromFile('animation', 'Data/animation.png');

  Sprite := FSpriteSystem.AddSprite('animation');

  TAnimationBehavior.Create(Sprite);
  
  {
  for P in IVec2(32) do
  begin
    Sprite := FSpriteSystem.AddSprite('log');
    Sprite.Scale := 1 / 16;
    Sprite.Pos := TVector2(P) / 16 - 1 + 1/32;
    THoverBehavior.Create(Sprite);
    TRainbowBehavior.Create(Sprite);
  end;
  }
  
  Game.OnUpdate.Add(GameUpdate);
  Input.OnKeyDown.Add(KeyDown);
end;

procedure TfrmMain.KeyDown(AInfo: TKeyEventInfo);
begin
  
end;

procedure TfrmMain.Finalize;
begin
  FSpriteSystem.Free;
  if FSpriteGLProgram <> nil then
    TSpriteGLProgram.Release(GLState.ResParam);
end;

procedure TfrmMain.GameUpdate;
begin
  if Context.MustUpdateFPS then
    Caption := Format('Point and Click - FPS: %d', [Context.FPSInt]);
end;

{ TSpriteGLProgram }

class procedure TSpriteGLProgram.GetData(out AName: string; out AResource: Boolean);
begin
  AName := 'Data/sprite';
  AResource := False;
end;

{ TRainbowBehaivor }

constructor TRainbowBehavior.Create(ASprite: TSprite);
begin
  inherited;
  FSpeed := 1;
end;

procedure TRainbowBehavior.AddEvents;
begin
  Game.Input.OnButtonDown.Add(ButtonDown);
  Game.Input.OnScroll.Add(Scroll);
end;

procedure TRainbowBehavior.ButtonDown(AInfo: TButtonEventInfo);
begin
  if AInfo.Button = mbLeft then
  begin
    if Game.Input.MousePos in Sprite.Bounds then
      Remove;
  end;
end;

constructor TRainbowBehavior.Create(ASprite: TSprite; ASpeed: Single);
begin
  inherited Create(ASprite);
  FSpeed := ASpeed;
end;

procedure TRainbowBehavior.DelEvents;
begin
  Game.Input.OnScroll.Del(Scroll);
  Game.Input.OnButtonDown.Del(ButtonDown);
end;

procedure TRainbowBehavior.Scroll(AInfo: TScrollEventInfo);
begin
  if AInfo.ScrolledUp then
    Speed := Speed + 1
  else
    Speed := Speed - 1;
end;

procedure TRainbowBehavior.Update;
begin
  FCurrent := Bounds1(0, 6).RangedModL(FCurrent + Game.DeltaTime * Speed);
  Sprite.Color := TColorRGB.HSV(FCurrent, 0.1, 1);
end;

{ TFollowMouseBehavior }

procedure TFollowMouseBehavior.AddEvents;
begin
  Game.Input.OnMouseMove.Add(MouseMove);
end;

procedure TFollowMouseBehavior.DelEvents;
begin
  Game.Input.OnMouseMove.Del(MouseMove);
end;

procedure TFollowMouseBehavior.MouseMove;
begin
  Sprite.Pos := Game.Input.MousePos;
end;

{ THoverBehavior }

procedure THoverBehavior.AddEvents;
begin
  Game.Input.OnMouseMove.Add(MouseMove);
end;

procedure THoverBehavior.DelEvents;
begin
  Game.Input.OnMouseMove.Del(MouseMove);
end;

procedure THoverBehavior.HoverDone(AInfo: TSprite.TBehavior.TSenderEventInfo);
begin
  FFade := nil;
end;

procedure THoverBehavior.MouseMove;
begin
  if FHover <> (Game.Input.MousePos in Sprite.Bounds) then
  begin
    FHover := not FHover;
    if FFade <> nil then
    begin
      FFade.Remove;
      Sprite.Fade := 1 - Sprite.Fade;
      Sprite.Texture := Sprite.FadeTexture;
    end;
    if FHover then
    begin
      FFade := TSprite.TFadeBehavior.Create(Sprite, 'bricks', 0);
      Sprite.Scale := Sprite.Scale * Sqrt(20);
      Sprite.ZOrder := -0.1;
      Sprite.Rotation := Random * 40 - 20;
    end
    else
    begin
      FFade := TSprite.TFadeBehavior.Create(Sprite, 'log', 0.5);
      Sprite.Scale := Sprite.Scale / Sqrt(20);
      Sprite.ZOrder := 0;
      Sprite.Rotation := 0;
    end;
    FFade.OnDone.Add(HoverDone)
  end;  
end;

{ TAnimationBehavior }

procedure TAnimationBehavior.Update;
begin
  FTimeLeft := FTimeLeft - Game.DeltaTime;
  while FTimeLeft < 0 do
  begin
    FTimeLeft := FTimeLeft + 0.1;
    Sprite.FadeTextureTile := Sprite.TextureTile.SubTiles[FFrame];
    FFrame := (FFrame + 1) mod Sprite.TextureTile.SubTiles.Count;
    Sprite.TextureTile := Sprite.TextureTile.SubTiles[FFrame];
  end;
  Sprite.Fade := FTimeLeft / 0.1;
end;

end.
