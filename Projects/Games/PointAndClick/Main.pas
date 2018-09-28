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

  Pengine.Texture,
  Pengine.Collections,
  Pengine.GLEnums,
  Pengine.GLContext,
  Pengine.GLProgram,
  Pengine.GLState,
  Pengine.Color,
  Pengine.InputHandler,
  Pengine.Vector,
  Pengine.EventHandling,
  Pengine.IntMaths,
  Pengine.TimeManager,
  Pengine.GLForm,
  Pengine.SpriteSystem,
  Pengine.GUI,
  Pengine.GUIControls,
  Pengine.UBO;

type

  TSpriteGLProgram = class(TSpriteGLProgamBase)
  protected
    class procedure GetData(out AName: string; out AResource: Boolean); override;
  end;

  TRainbowBehavior = class(TSprite.TUpdateBehavior)
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
    constructor Add(ASprite: TSprite; ASpeed: Single); reintroduce; overload;

    procedure Update; override;

    property Speed: Single read FSpeed write FSpeed;

  end;

  THoverBehavior = class(TSprite.TBehavior)
  private
    FHover: Boolean;

    procedure MouseMove;

  protected
    procedure AddEvents; override;
    procedure DelEvents; override;

  end;

  TfrmMain = class(TGLForm)
  private
    FSpriteGLProgram: TGLProgram;
    FSpriteSystem: TSpriteSystem;
    FGUI: TGUI;
    FFPSLabel: TLabel;
    FTypeLabel: TLabel;
    FButton: TButton;

    procedure KeyDown(AInfo: TKeyEventInfo); reintroduce;
    procedure TypeText(AInfo: TTypeEventInfo);
    procedure UpdateCaption;
    procedure GameUpdate;

    procedure ButtonPressed(AInfo: TButton.TEventInfo);

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
  if Input.KeyDown(VK_LEFT) then
    FButton.Location.PosX := FButton.Location.PosX - DeltaTime;
  if Input.KeyDown(VK_RIGHT) then
    FButton.Location.PosX := FButton.Location.PosX + DeltaTime;
  if Input.KeyDown(VK_DOWN) then
    FButton.Location.PosY := FButton.Location.PosY - DeltaTime;
  if Input.KeyDown(VK_UP) then
    FButton.Location.PosY := FButton.Location.PosY + DeltaTime;
end;

procedure TfrmMain.Init;
var
  I: Integer;
begin
  Context.VSync := False;

  // Context.Samples := Context.MaxSamples;

  FSpriteGLProgram := TSpriteGLProgram.Make(GLState.ResParam);

  FGUI := TGUI.Create(Game, FSpriteGLProgram);

  FGUI.TextureAtlas.Texture.MagFilter := magNearest;
  FGUI.TextureAtlas.AddFromResource('font', 'FONT');
  FGUI.TextureAtlas.AddFromResource('mcfont', 'MCFONT');
  FGUI.TextureAtlas.AddFromResource('dokufont', 'DOKUFONT');
  FGUI.TextureAtlas.AddFromResource('button', 'STONE_BUTTON');

  FTypeLabel := FGUI.Add<TLabel>;
  FTypeLabel.FontColor := ColorRed;
  FTypeLabel.Location.Scale := 0.1;
  FTypeLabel.Location.OffsetY := 0.5;
  FTypeLabel.OriginY := oyBottom;
  FTypeLabel.Caption := 'Press 1, 2 or 3 to change font.';

  FFPSLabel := FGUI.Add<TLabel>;
  FFPSLabel.Location.Scale := 0.1;
  FFPSLabel.Origin := TOrigin.Create(oxLeft, oyTop);
  // FFPSLabel.Location.Scale := 1;
  FFPSLabel.FontColor := clYellow;

  for I := 1 to 8 do
  begin
    FButton := FGUI.Add<TButton>;
    FButton.Location.Scale := 0.15;
    FButton.Location.PosY := 1 - (I + 0.5) / 5;
    FButton.OriginX := oxCenter;
    FButton.OriginY := oyCenter;
    FButton.Caption := 'Press me you sweet bastard! Yeah! Gimme!';
    FButton.OnPressed.Add(ButtonPressed);
    FButton.Width := 10;
  end;

  {
  FSpriteSystem := TSpriteSystem.Create(Game, FSpriteGLProgram);
  FSpriteSystem.SpriteAtlas.Texture.MagFilter := magNearest;

  FSpriteSystem.SpriteAtlas.AddFromFile('char', 'Data/font.png');

  Sprite := FSpriteSystem.Add<TCharSprite>('char');
  Sprite.Char := 'A';
  Sprite.Location.Scale := 0.1;
  TSprite.TFollowMouseBehavior.Add(Sprite);

  Sprite2 := FSpriteSystem.Add<TCharSprite>('char');
  Sprite2.Char := 'B';
  Sprite2.Location.OffsetX := Sprite.WidthSpaced / 2 + Sprite2.WidthSpaced / 2;
  Sprite2.Location.Parent := Sprite.Location;
  }

  Game.Timer.OnFPSUpdate.Add(UpdateCaption);
  Game.OnUpdate.Add(GameUpdate);
  Input.OnKeyTyped.Add(KeyDown);
  // Input.OnType.Add(TypeText);
end;

procedure TfrmMain.KeyDown(AInfo: TKeyEventInfo);
begin
  case AInfo.Key of
    VK_F11:
      Fullscreen := not Fullscreen;
    Ord('1'):
      FGUI.Font := 'font';
    Ord('2'):
      FGUI.Font := 'mcfont';
    Ord('3'):
      FGUI.Font := 'dokufont';
  end;
end;

procedure TfrmMain.TypeText(AInfo: TTypeEventInfo);
begin
  FTypeLabel.Caption := FTypeLabel.Caption + AInfo.Text;
end;

procedure TfrmMain.ButtonPressed(AInfo: TButton.TEventInfo);
begin
  if AInfo.Sender.Width < 20 then
  begin
    AInfo.Sender.Width := AInfo.Sender.Width + 1;
    if AInfo.Sender.Width = 20 then
      AInfo.Sender.Caption := 'thx';
  end;
end;

procedure TfrmMain.Finalize;
begin
  FGUI.Free;
  FSpriteSystem.Free;
  if FSpriteGLProgram <> nil then
    TSpriteGLProgram.Release(GLState.ResParam);
end;

procedure TfrmMain.UpdateCaption;
begin
  Caption := Format('Point and Click - FPS: %d', [Context.FPSInt]);
  FFPSLabel.Caption := Format('Fraps per Second: %d', [Context.FPSInt]);
end;

{ TSpriteGLProgram }

class procedure TSpriteGLProgram.GetData(out AName: string; out AResource: Boolean);
begin
  AResource := False;
  if AResource then
    AName := 'SPRITE'
  else
    AName := 'Data/sprite';
end;

{ TRainbowBehaivor }

constructor TRainbowBehavior.Create(ASprite: TSprite);
begin
  inherited;
  FSpeed := 1;
end;

procedure TRainbowBehavior.AddEvents;
begin
  inherited;
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

constructor TRainbowBehavior.Add(ASprite: TSprite; ASpeed: Single);
begin
  inherited Create(ASprite);
  FSpeed := ASpeed;
end;

procedure TRainbowBehavior.DelEvents;
begin
  inherited;
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
  Sprite.Color := TColorRGB.HSV(FCurrent, 1, 1);
  // Sprite.Rotation := Sprite.Rotation + Game.DeltaTime * 60;
  Sprite.Location.Scale := Sin(Game.Time) * 0.125 + 0.25;
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

procedure THoverBehavior.MouseMove;
var
  Behavior: TSprite.TBehavior;
begin
  // if FHover <> (Game.Input.MousePos in Sprite.Bounds) then
  if FHover <> (Game.Input.MousePos.DistanceTo(Sprite.Location.Pos) < 0.3) then
  begin
    FHover := not FHover;
    {
    if FFade <> nil then
    begin
      FFade.Remove;
      Sprite.Fade := 1 - Sprite.Fade;
      Sprite.Texture := Sprite.FadeTexture;
    end;
 }
    if FHover then
    begin
      Sprite.Location.Scale := Sprite.Location.Scale * 1.2;
      for Behavior in Sprite.Behaviors do
        if Behavior is TSprite.TAnimationBeavior then
          TSprite.TAnimationBeavior(Behavior).Reversed := True;
      {
      FFade := TSprite.TFadeBehavior.Create(Sprite, 'bricks', 0);
      Sprite.Scale := Sprite.Scale * Sqrt(20);
      Sprite.ZOrder := -0.1;
      Sprite.Rotation := Random * 40 - 20;
 }
    end
    else
    begin
      Sprite.Location.Scale := Sprite.Location.Scale / 1.2;
      for Behavior in Sprite.Behaviors do
        if Behavior is TSprite.TAnimationBeavior then
          TSprite.TAnimationBeavior(Behavior).Reversed := False;
      {
      FFade := TSprite.TFadeBehavior.Create(Sprite, 'log', 0.5);
      Sprite.Scale := Sprite.Scale / Sqrt(20);
      Sprite.ZOrder := 0;
      Sprite.Rotation := 0;
 }
    end;
    // FFade.OnDone.Add(HoverDone)
  end;
end;

end.
