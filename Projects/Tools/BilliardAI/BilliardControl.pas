unit BilliardControl;

interface

uses
  System.SysUtils,

  Vcl.Controls,

  Pengine.SpriteSystem,
  Pengine.GUI,
  Pengine.GUIControls,
  Pengine.TextureAtlas,
  Pengine.Vector,
  Pengine.Color,
  Pengine.InputHandler,

  BilliardGame;

type

  EBilliardControl = class(Exception);

  TBallPocketBehavior = class(TSprite.TUpdateBehavior)
  private
    FBall: TBilliard.TBall;
    FReflection: Boolean;
    FTime: Single;

  protected
    constructor Create(ASprite: TSprite; ABall: TBilliard.TBall; AReflection: Boolean); reintroduce;

    property Ball: TBilliard.TBall read FBall;
    property Reflection: Boolean read FReflection;

    procedure Update; override;

  end;

  TBilliardControl = class(TControl)
  public const

    DefaultBallTextureName = 'billiard_ball';
    DefaultBallReflectionTextureName = 'billiard_ball_reflection';
    DefaultTableTextureName = 'billiard_table';

    ScaleCorrectionFactor = 2.48;

  public type

    TPlayer = class(TBilliard.TPlayer)

    end;

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
    procedure BilliardStateChange;

  protected
    function GetAspect: Single; override;

  public
    constructor Create(AParent: TControl); override;
    destructor Destroy; override;

    property BallTexture: TTexTile read GetBallTexture write SetBallTexture;
    property BallReflectionTexture: TTexTile read GetBallReflectionTexture write SetBallReflectionTexture;
    property TableTexture: TTexTile read GetTableTexture write SetTableTexture;

    property Billiard: TBilliard read FBilliard write SetBilliard;
    property InnerLocation: TLocation2 read FInnerLocation;
    property InnerBounds: TAxisSystem2 read GetInnerBounds;

  end;

  TCuePlacementBehavior = class(TSprite.TBehavior)
  private
    FControl: TBilliardControl;

    procedure MouseMove;
    procedure ButtonDown(AInfo: TButtonEventInfo);
    function GetCueBall: TBilliard.TBall;
    function GetReflectionSprite: TSprite;
    function GetBilliard: TBilliard;
    function GetBallSprite: TSprite;

    // TODO: Cache as FPos and update in MouseMove
    function GetPos: TVector2;

  protected
    procedure AddEvents; override;
    procedure RemoveEvents; override;

  public
    constructor Create(AControl: TBilliardControl); reintroduce;

    property Control: TBilliardControl read FControl;
    property Billiard: TBilliard read GetBilliard;
    property CueBall: TBilliard.TBall read GetCueBall;
    property ReflectionSprite: TSprite read GetReflectionSprite;
    property BallSprite: TSprite read GetBallSprite;

  end;

implementation

{ TBilliardRenderer }

procedure TBilliardControl.BallLocationChanged(AInfo: TBilliard.TBall.TEventInfo);
begin
  with AInfo.Sender do
  begin
    if Pocketed then
      Exit;
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
      FBallSprites[Index].ZOrder := 0;
      FBallReflectionSprites[Index].ZOrder := 0;
      TBallPocketBehavior.Create(FBallSprites[Index], AInfo.Sender, False);
      TBallPocketBehavior.Create(FBallReflectionSprites[Index], AInfo.Sender, True);
      {
        FBallSprites[Index].Color := ColorRGB(0.4, 0.4, 0.6);
        FBallReflectionSprites[Index].Color := ColorRGB(0.4, 0.4, 0.6);
        FBallReflectionSprites[Index].Location.Scale := TBilliard.TBall.Diameter * 0.85;
      }
    end
    else
    begin
      FBallSprites[Index].ZOrder := 0.1;
      FBallSprites[Index].Color := ColorWhite;
      FBallReflectionSprites[Index].ZOrder := 0.1;
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

destructor TBilliardControl.Destroy;
begin
  Billiard.OnBallPocketedChange.Remove(BallPocketedChange);
  Billiard.OnStateChange.Remove(BilliardStateChange);
  FInnerLocation.Free;
  inherited;
end;

procedure TBilliardControl.BilliardStateChange;
begin
  case Billiard.State of
    bsCuePlacement:
      TCuePlacementBehavior.Create(Self);
  end;
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
  Billiard.OnStateChange.Add(BilliardStateChange);

  FTableSprite := Add<TSprite>(TableTexture);
  FTableSprite.ZOrder := -0.1;
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

{ TBallPocketBehavior }

constructor TBallPocketBehavior.Create(ASprite: TSprite; ABall: TBilliard.TBall; AReflection: Boolean);
begin
  inherited Create(ASprite);
  FBall := ABall;
  FReflection := AReflection;
end;

procedure TBallPocketBehavior.Update;
const
  TargetColor: TColorRGB = (R: 0.4; G: 0.4; B: 0.6);
  TargetScale = TBilliard.TBall.Diameter * 0.85;
begin
  if not Ball.Pocketed then
  begin
    Remove;
    Exit;
  end;

  FTime := FTime + Game.DeltaTime;
  if FTime > 1.0 then
  begin
    Sprite.Color := TargetColor;
    if Reflection then
    begin
      Sprite.Location.Pos := Ball.Pocket.PocketedPosition;
      Sprite.Location.Scale := TargetScale;
    end;
    Remove;
    Exit;
  end;

  Sprite.Color := Sprite.Color - (Sprite.Color - TargetColor) * Game.DeltaTime * 10;
  if Reflection then
  begin
    Sprite.Location.Pos := Sprite.Location.Pos - (Sprite.Location.Pos - Ball.Pocket.PocketedPosition) *
      Game.DeltaTime * 20;
    Sprite.Location.Scale := Sprite.Location.Scale - (Sprite.Location.Scale - TargetScale) * Game.DeltaTime * 10;
  end;
end;

{ TBallPlacementBehavior }

procedure TCuePlacementBehavior.AddEvents;
begin
  Game.Input.OnMouseMove.Add(MouseMove);
  Game.Input.OnButtonDown.Add(ButtonDown);
end;

procedure TCuePlacementBehavior.ButtonDown(AInfo: TButtonEventInfo);
var
  NewPos: TVector2;
begin
  if AInfo.Button <> mbLeft then
    Exit;

  NewPos := GetPos;
  if not Billiard.CanPlaceCueBall(GetPos) then
    Exit;

  Billiard.PlaceCueBall(NewPos);
  Remove;
end;

constructor TCuePlacementBehavior.Create(AControl: TBilliardControl);
begin
  FControl := AControl;
  inherited Create(ReflectionSprite);
  MouseMove;
end;

function TCuePlacementBehavior.GetBallSprite: TSprite;
begin
  Result := Control.FBallSprites[TBilliard.TBall.CueBallIndex];
end;

function TCuePlacementBehavior.GetBilliard: TBilliard;
begin
  Result := Control.Billiard;
end;

function TCuePlacementBehavior.GetCueBall: TBilliard.TBall;
begin
  Result := Billiard.CueBall;
end;

function TCuePlacementBehavior.GetPos: TVector2;
begin
  Result := Control.InnerBounds.InvPoint[Game.Input.MousePos];
end;

function TCuePlacementBehavior.GetReflectionSprite: TSprite;
begin
  Result := Control.FBallReflectionSprites[TBilliard.TBall.CueBallIndex];
end;

procedure TCuePlacementBehavior.MouseMove;
var
  NewPos: TVector2;
begin
  NewPos := GetPos;
  Sprite.Location.Pos := NewPos;
  if Billiard.CanPlaceCueBall(NewPos) then
  begin
    ReflectionSprite.Color := ColorWhite;
    BallSprite.Color := ColorWhite;
  end
  else
  begin
    ReflectionSprite.Color := $7F7FFF;
    BallSprite.Color := $7F7FFF;
  end;
end;

procedure TCuePlacementBehavior.RemoveEvents;
begin
  Game.Input.OnMouseMove.Remove(MouseMove);
  Game.Input.OnButtonDown.Remove(ButtonDown);
end;

end.
