unit BilliardControl;

interface

uses
  System.SysUtils,

  Pengine.SpriteSystem,
  Pengine.GUI,
  Pengine.GUIControls,
  Pengine.TextureAtlas,
  Pengine.Vector,
  Pengine.Color,

  BilliardGame;

type

  EBilliardControl = class(Exception);

  TBilliardControl = class(TControl)
  public const

    DefaultBallTextureName = 'billiard_ball';
    DefaultBallReflectionTextureName = 'billiard_ball_reflection';
    DefaultTableTextureName = 'billiard_table';

    ScaleCorrectionFactor = 2.48;

  private
    FBallSprites: array [TBilliard.TBall.TIndex] of TSprite;
    FBallReflectionSprites: array [TBilliard.TBall.TIndex] of TSprite;
    FTableSprite: TSprite;
    FBilliard: TBilliard;
    FBallTexture: TTexTile;
    FBallReflectionTexture: TTexTile;
    FTableTexture: TTexTile;
    FInnerLocation: TLocation2;

    procedure SetBilliard(const Value: TBilliard);
    function GetBallTexture: TTexTile;
    function GetTableTexture: TTexTile;
    procedure SetBallTexture(const Value: TTexTile);
    procedure SetTableTexture(const Value: TTexTile);
    function GetInnerBounds: TAxisSystem2;

    function GetBallReflectionTexture: TTexTile;
    procedure SetBallReflectionTexture(const Value: TTexTile);

    procedure BallPocketedChange(AInfo: TBilliard.TBall.TEventInfo);
    procedure BallLocationChanged(AInfo: TBilliard.TBall.TEventInfo);

  protected
    function GetAspect: Single; override;

  public
    constructor Create(AParent: TControl); override;

    property BallTexture: TTexTile read GetBallTexture write SetBallTexture;
    property BallReflectionTexture: TTexTile read GetBallReflectionTexture write SetBallReflectionTexture;
    property TableTexture: TTexTile read GetTableTexture write SetTableTexture;

    property Billiard: TBilliard read FBilliard write SetBilliard;
    property InnerLocation: TLocation2 read FInnerLocation;
    property InnerBounds: TAxisSystem2 read GetInnerBounds;

  end;

implementation

{ TBilliardRenderer }

procedure TBilliardControl.BallLocationChanged(AInfo: TBilliard.TBall.TEventInfo);
begin
  with AInfo.Sender do
  begin
    FBallReflectionSprites[Index].Location.Pos := Body.Position;
    FBallSprites[Index].Location.Rotation := Body.Rotation;
  end;
end;

procedure TBilliardControl.BallPocketedChange(AInfo: TBilliard.TBall.TEventInfo);
begin
  with AInfo.Sender do
  begin
    if Pocketed then
    begin
      FBallSprites[Index].Color := ColorRGB(0.4, 0.4, 0.6);
      FBallReflectionSprites[Index].Color := ColorRGB(0.4, 0.4, 0.6);
      FBallReflectionSprites[Index].Location.Scale := TBilliard.TBall.Diameter * 0.85;
    end
    else
    begin
      FBallSprites[Index].Color := ColorWhite;
      FBallReflectionSprites[Index].Color := ColorWhite;
      FBallReflectionSprites[Index].Location.Scale := TBilliard.TBall.Diameter;
    end;
  end;
end;

constructor TBilliardControl.Create(AParent: TControl);
begin
  inherited;
  FInnerLocation := TLocation2.Create;
  FInnerLocation.Parent := Location;
  FInnerLocation.Scale := 1 / ScaleCorrectionFactor;
end;

function TBilliardControl.GetAspect: Single;
begin
  Result := FTableSprite.Aspect;
end;

function TBilliardControl.GetBallReflectionTexture: TTexTile;
begin
  if FBallReflectionTexture = nil then
    BallReflectionTexture := TextureAtlas[DefaultBallReflectionTextureName];
  Result := FBallReflectionTexture;
end;

function TBilliardControl.GetBallTexture: TTexTile;
begin
  if FBallTexture = nil then
    BallTexture := TextureAtlas[DefaultBallTextureName];
  Result := FBallTexture;
end;

function TBilliardControl.GetInnerBounds: TAxisSystem2;
begin
  Result := FInnerLocation.AxisSystem;
end;

function TBilliardControl.GetTableTexture: TTexTile;
begin
  if FTableTexture = nil then
    TableTexture := TextureAtlas[DefaultTableTextureName];
  Result := FTableTexture;
end;

procedure TBilliardControl.SetBallReflectionTexture(const Value: TTexTile);
var
  Sprite: TSprite;
begin
  if FBallReflectionTexture = Value then
    Exit;
  FBallReflectionTexture := Value;
  if FBallReflectionSprites[1] <> nil then
    for Sprite in FBallReflectionSprites do
      Sprite.TextureTile := BallReflectionTexture;
end;

procedure TBilliardControl.SetBallTexture(const Value: TTexTile);
var
  Sprite: TSprite;
begin
  if FBallTexture = Value then
    Exit;
  FBallTexture := Value;
  if FBallSprites[1] <> nil then
    for Sprite in FBallSprites do
      Sprite.TextureTile := BallTexture;
end;

procedure TBilliardControl.SetBilliard(const Value: TBilliard);
var
  I: TBilliard.TBall.TIndex;
begin
  if Billiard <> nil then
    raise EBilliardControl.Create('The context of the billiard renderer can only be set once.');
  FBilliard := Value;

  Billiard.OnBallPocketedChange.Add(BallPocketedChange);

  FTableSprite := Add<TSprite>(TableTexture);
  for I := Low(TBilliard.TBall.TIndex) to High(TBilliard.TBall.TIndex) do
  begin
    FBallSprites[I] := Add<TSprite>(BallTexture.SubTiles[I - 1]);

    FBallReflectionSprites[I] := Add<TSprite>(BallReflectionTexture);
    FBallReflectionSprites[I].Location.Scale := TBilliard.TBall.Diameter;
    FBallReflectionSprites[I].Location.Parent := FInnerLocation;

    FBallSprites[I].Location.Parent := FBallReflectionSprites[I].Location;

    FBilliard.Balls[I].OnLocationChanged.Add(BallLocationChanged);
  end;
end;

procedure TBilliardControl.SetTableTexture(const Value: TTexTile);
begin
  if FTableTexture = Value then
    Exit;
  FTableTexture := Value;
  if FTableSprite <> nil then
    FTableSprite.TextureTile := TableTexture;
end;

end.
