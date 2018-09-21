unit Main;

interface

uses
  System.SysUtils,
  System.Math,
  System.UITypes,

  Box2D.Common,
  Box2D.Dynamics,
  Box2D.Collision,

  Pengine.GLForm,
  Pengine.GLProgram,
  Pengine.SpriteSystem,
  Pengine.Vector,
  Pengine.Collections,
  Pengine.InputHandler,
  Pengine.CollectionInterfaces,
  Pengine.GLEnums,
  Pengine.GLState,
  Pengine.Box2D,
  Pengine.TimeManager,
  Pengine.Color,
  Pengine.NeuralNetwork,

  Vcl.Graphics,
  Vcl.Controls,
  Pengine.TextureAtlas;

type

  TPhysicBehaivor = class(TSprite.TUpdateBehavior)
  private
    FWorld: TWorld;
    FBody: TBody;

  protected
    procedure Update; override;

  public
    constructor Create(ASprite: TSprite; AWorld: TWorld; ABodyDef: TBody.TDef); reintroduce;

    property World: TWorld read FWorld;
    property Body: TBody read FBody;

  end;

  TSpriteGLProgram = class(TSpriteGLProgramBase)
  protected
    class procedure GetData(out AName: string; out AResource: Boolean); override;

  end;

  TfrmMain = class(TGLForm)
  private
    FWorld: TWorld;
    FFloor: TBody;
    FSpriteGLProgram: TGLProgram;
    FSpriteSystem: TSpriteSystem;

    procedure UpdateFPS;
    procedure GameUpdate;

    procedure InitPhysics;
    procedure InitSprites;

    procedure AddBox(APos: TVector2);
    procedure AddCircle(APos: TVector2);

    procedure ButtonDown(AInfo: TButtonEventInfo);

  public
    procedure Init; override;
    procedure Finalize; override;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ TfrmMain }

procedure TfrmMain.AddBox(APos: TVector2);
var
  Shape: TCircle;
  FixtureDef: TFixture.TDef;
  Sprite: TSprite;
  Behavior: TPhysicBehaivor;
  TextureTile: TTexTile;
begin

  TextureTile := FSpriteSystem.SpriteAtlas.Tiles['billiard_ball'];
  TextureTile := TextureTile.SubTiles[Random(TextureTile.SubTiles.Count)];
  Sprite := FSpriteSystem.Add<TSprite>(TextureTile);
  Sprite.Location.Scale := 0.2;

  Behavior := TPhysicBehaivor.Create(Sprite, FWorld, TBody.TDef.Create(btDynamic, APos));

  Shape := TCircle.Create;
  Shape.Radius := Sprite.Location.Scale.X * 0.49;

  FixtureDef := TFixture.TDef.Create(Shape);
  FixtureDef.Density := 20;
  FixtureDef.Friction := 0.1;
  FixtureDef.Restitution := 0.3;

  Behavior.Body.Fixtures.Add(FixtureDef);
  Behavior.Body.FixedRotation := True;

  Shape.Free;

end;

procedure TfrmMain.AddCircle(APos: TVector2);
var
  Shape: TCircle;
  FixtureDef: TFixture.TDef;
  Sprite: TSprite;
  Behavior: TPhysicBehaivor;
  BodyDef: TBody.TDef;
begin
  Sprite := FSpriteSystem.Add<TSprite>('circle');
  Sprite.Location.Scale := 0.1;

  BodyDef := TBody.TDef.Create(btDynamic, APos);
  BodyDef.GravityScale := -2;

  Behavior := TPhysicBehaivor.Create(Sprite, FWorld, BodyDef);

  Shape := TCircle.Create;
  Shape.Radius := Sprite.Location.Scale.X * 0.49;

  FixtureDef := TFixture.TDef.Create(Shape);
  FixtureDef.Density := 10;
  FixtureDef.Friction := 0.3;
  FixtureDef.Restitution := 0.1;

  Behavior.Body.Fixtures.Add(FixtureDef);

  Shape.Free;
end;

procedure TfrmMain.ButtonDown(AInfo: TButtonEventInfo);
begin
  case AInfo.Button of
    mbLeft:
      AddBox(Input.MousePos);
    mbRight:
      AddCircle(Input.MousePos);
    mbMiddle:;
  end;
end;

procedure TfrmMain.Finalize;
begin
  FWorld.Free;
  FSpriteSystem.Free;
  TSpriteGLProgram.Release(GLState.ResParam);
end;

procedure TfrmMain.GameUpdate;
var
  Joint: TJoint;
begin
  if Input.ButtonDown(mbMiddle) then
    AddCircle(Input.MousePos);

  for Joint in FWorld.Joints do
  begin
    if Joint is TDistanceJoint then
      with TDistanceJoint(Joint) do
      begin
        Length := Length + DeltaTime * 0.1;
      end;

    if Joint is TRevoluteJoint then
      with TRevoluteJoint(Joint) do
      begin
        MotorSpeed := Sin(Game.Time) * 10;
      end;

    if Joint is TPrismaticJoint then
      with TPrismaticJoint(Joint) do
      begin
        MotorSpeed := Sin(Game.Time * 50) * 1000;
      end;
  end;

  FWorld.Step(Min(1 / 60, DeltaTime));
  {
  for Body in FWorld.Bodies do
  begin
    if not (Body.Position in Bounds2(-5, 5)) then
    begin
      Body.Free;
      Continue;
    end;
  end;
 }
end;

procedure TfrmMain.Init;
begin
  GLState.State[stClearColor] := clSkyBlue;
  Context.VSync := False;
  Context.Samples := Context.MaxSamples;

  Game.Timer.OnFPSUpdate.Add(UpdateFPS);
  Game.OnUpdate.Add(GameUpdate);
  Input.OnButtonDown.Add(ButtonDown);

  InitPhysics;
  InitSprites;

end;

procedure TfrmMain.InitPhysics;
const
  Points: array [0 .. 7] of TVector2 = (
    (X: -1.7; Y: 2.0),
    (X: -1.5; Y: 0.7),
    (X: -1.0; Y: 0.2),
    (X: -0.5; Y: 0.0),
    (X: +0.5; Y: 0.0),
    (X: +1.0; Y: 0.2),
    (X: +1.5; Y: 0.7),
    (X: +1.7; Y: 2.0)
    );

var
  BodyDef: TBody.TDef;
  FixtureDef: TFixture.TDef;
  Chain: TChain;
begin
  // Create the world
  FWorld := TWorld.Create(Vec2(0, -10));

  BodyDef := TBody.TDef.Create(btStatic, Vec2(0, -1));
  // BodyDef.LinearVelocity := Vec2(0, 0.02);
  FFloor := FWorld.Bodies.Add(BodyDef);

  Chain := TChain.Create;
  Chain.SetChain(@Points[0], Length(Points));

  FixtureDef := TFixture.TDef.Create(Chain);
  FixtureDef.Density := 1.0;
  FixtureDef.Friction := 0.3;
  FixtureDef.Restitution := 0.1;

  FFloor.Fixtures.Add(FixtureDef);

  Chain.Free;
end;

procedure TfrmMain.InitSprites;
begin
  FSpriteGLProgram := TSpriteGLProgram.Make(GLState.ResParam);
  FSpriteSystem := TSpriteSystem.Create(Game, FSpriteGLProgram);

  FSpriteSystem.SpriteAtlas.AddFromResource('box', 'BOX');
  FSpriteSystem.SpriteAtlas.AddFromResource('circle', 'CIRCLE');
  FSpriteSystem.SpriteAtlas.AddFromResource('circle_big', 'CIRCLE_BIG');
  FSpriteSystem.SpriteAtlas.AddFromFile('billiard_ball', 'Data\Textures\BilliardBalls.png');

  // FSpriteSystem.SpriteAtlas.Texture.MagFilter := magNearest;

end;

procedure TfrmMain.UpdateFPS;
begin
  Caption := Format('Box2D Testing - FPS: %.0f', [Game.FPS]);
end;

{ TSpriteGLProgram }

class
  procedure TSpriteGLProgram.GetData(out AName: string; out AResource: Boolean);
begin
  AResource := True;
  AName := 'SPRITE';
end;

{ TPhysicBehaivor }

constructor TPhysicBehaivor.Create(ASprite: TSprite; AWorld: TWorld; ABodyDef: TBody.TDef);
begin
  inherited Create(ASprite);
  FWorld := AWorld;
  FBody := World.Bodies.Add(ABodyDef);
  Body.AddLocation(Sprite.Location);
end;

procedure TPhysicBehaivor.Update;
begin
  if Body.Awake then
    Sprite.Color := ColorRGB(1.0, 1.0, 1.0)
  else
    Sprite.Color := ColorRGB(0.2, 0.5, 0.8);

  if not(Body.Position in Bounds2(-10, 10)) then
  begin
    Body.Free;
    Sprite.Remove;
  end;
end;

end.
