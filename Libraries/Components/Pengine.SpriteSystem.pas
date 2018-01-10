unit Pengine.SpriteSystem;

interface

uses
  System.SysUtils,
  System.Math,

  Pengine.GLState,
  Pengine.Collections,
  Pengine.VAO,
  Pengine.Vector,
  Pengine.GLProgram,
  Pengine.Color,
  Pengine.Texture,
  Pengine.TextureAtlas,
  Pengine.HashCollections,
  Pengine.Hasher,
  Pengine.EventHandling,
  Pengine.GLGame,
  Pengine.GLEnums;

type

  ESpriteBehaviorNoDefault = class(Exception)
  public
    constructor Create;
  end;

  TSpriteGLProgamBase = class(TGLProgramResource)
  public type

    TData = record
      Pos: TVector2;
      ZOrder: Single;
      Color: TColorRGBA;
      Fade: Single;
      Borders: array [0 .. 1] of TBounds2;
      TexCoords: array [0 .. 1] of TVector2;
    end;

  protected
    class function GetAttributeOrder: TGLProgram.TAttributeOrder; override;

  end;

  TAnimation = class
  public type

    TFrame = class
    private
      FTile: TTextureAtlas.TTile;
      FDuration: Single;
      FFadePercentage: Single;

    public

    end;

    TFrames = TObjectArray<TFrame>;

  private
    FFrames: TFrames;

  public
    constructor Create;
    destructor Destroy; override;

  end;

  TSpriteSystem = class;

  TSprite = class
  public type

    TBehavior = class
    public type

      TSenderEventInfo = class(TEventInfo, IEventSender<TBehavior>)
      private
        FSender: TBehavior;

      public
        constructor Create(ASender: TBehavior);

        function Sender: TBehavior;

      end;

      TSenderEvent = TEvent<TSenderEventInfo>;

    private
      FSprite: TSprite;
      FEnabled: Boolean;
      FRemoved: Boolean;
      FOnDone: TSenderEvent;

      function GetGame: TGLGame;

      procedure SetEnabled(const Value: Boolean);

      function GetOnDone: TSenderEvent.TAccess;

    protected
      procedure AddEvents; virtual;
      procedure DelEvents; virtual;

    public
      constructor Create(ASprite: TSprite); virtual;
      destructor Destroy; override;

      property Game: TGLGame read GetGame;
      property Sprite: TSprite read FSprite;

      property Enabled: Boolean read FEnabled write SetEnabled;
      procedure Enable;
      procedure Disable;

      procedure Remove;
      property Removed: Boolean read FRemoved;

      property OnDone: TSenderEvent.TAccess read GetOnDone;

      procedure Update; virtual;

    end;

    TBehaviorClass = class of TBehavior;

    TBehaviors = TRefSet<TBehavior, TRefHasher<TBehavior>>;

    TChange = (
      scPosX,
      scPosY,
      scZOrder,
      scColor,
      scFade,
      scTexCoords
      );

    TChanges = set of TChange;

    TChangeEventInfo = class(TEventInfo, IEventSender<TSprite>)
    private
      FSender: TSprite;
      FChanges: TChanges;

    public
      constructor Create(ASender: TSprite; AChanges: TChanges);

      function Sender: TSprite;
      property Changes: TChanges read FChanges;

    end;

    TChangeEvent = TEvent<TChangeEventInfo>;

    TFadeBehavior = class(TBehavior)
    private
      FDuration: Single;
      FTimeLeft: Single;

      function GetProgress: Single;

    public
      constructor Create(ASprite: TSprite); overload; override;
      constructor Create(ASprite: TSprite; ATexture: string; ADuration: Single); reintroduce; overload;
      constructor Create(ASprite: TSprite; ATexture: TTextureAtlas.TTile; ADuration: Single); reintroduce; overload;

      procedure Update; override;

      property Duration: Single read FDuration;
      property TimeLeft: Single read FTimeLeft;
      property Progress: Single read GetProgress;

    end;

  private
    FSpriteSystem: TSpriteSystem;
    FRemoved: Boolean;
    FChanges: TChanges;
    FBehaviors: TBehaviors;
    FOnChanged: TChangeEvent;

    FPos: TVector2;
    FOffset: TVector2;
    FRotation: Single;
    FZOrder: Single;
    FScale: TVector2;

    FColor: TColorRGBA;
    FTexture: string;
    FTextureTile: TTextureAtlas.TTile;
    FFadeTexture: string;
    FFadeTextureTile: TTextureAtlas.TTile;
    FFade: Single;

    FBounds: TOpt<TPlane2>;

    function GetGame: TGLGame;

    procedure SetPos(const Value: TVector2);
    procedure SetPosX(const Value: Single);
    procedure SetPosY(const Value: Single);

    procedure SetOffset(const Value: TVector2);
    procedure SetOffsetX(const Value: Single);
    procedure SetOffsetY(const Value: Single);

    procedure SetRotation(Value: Single);

    procedure SetZOrder(const Value: Single);

    procedure SetScale(const Value: TVector2);

    procedure SetColor(const Value: TColorRGBA);
    procedure SetTexture(const Value: string);
    procedure SetFadeTexture(const Value: string);
    procedure SetFade(const Value: Single);

    function GetBounds: TPlane2;

    function GetOnChanged: TChangeEvent.TAccess;
    function GetBehaviors: TBehaviors.TReader;
    procedure SetFadeTextureTile(const Value: TTextureAtlas.TTile);
    procedure SetTextureTile(const Value: TTextureAtlas.TTile);

  public
    constructor Create(ASpriteSystem: TSpriteSystem; ATextureTile: TTextureAtlas.TTile);
    destructor Destroy; override;

    property SpriteSystem: TSpriteSystem read FSpriteSystem;
    property Game: TGLGame read GetGame;

    procedure Remove;
    property Removed: Boolean read FRemoved;

    property Pos: TVector2 read FPos write SetPos;
    property PosX: Single read FPos.X write SetPosX;
    property PosY: Single read FPos.Y write SetPosY;

    property Offset: TVector2 read FOffset write SetOffset;
    property OffsetX: Single read FOffset.X write SetOffsetX;
    property OffsetY: Single read FOffset.Y write SetOffsetY;

    property Rotation: Single read FRotation write SetRotation;

    property ZOrder: Single read FZOrder write SetZOrder;

    property Scale: TVector2 read FScale write SetScale;

    property Color: TColorRGBA read FColor write SetColor;

    property Texture: string read FTexture write SetTexture;
    property TextureTile: TTextureAtlas.TTile read FTextureTile write SetTextureTile;
    property FadeTexture: string read FFadeTexture write SetFadeTexture;
    property FadeTextureTile: TTextureAtlas.TTile read FFadeTextureTile write SetFadeTextureTile;
    property Fade: Single read FFade write SetFade;

    property Bounds: TPlane2 read GetBounds;

    property Behaviors: TBehaviors.TReader read GetBehaviors;

    procedure Update;
    procedure UpdateVAO(AWriter: TVBO<TSpriteGLProgamBase.TData>.TWriter);

    property OnChanged: TChangeEvent.TAccess read GetOnChanged;

  end;

  TSpriteSystem = class
  public type

    TSprites = TRefArray<TSprite>;
    TVAO = TVAOMutable<TSpriteGLProgamBase.TData>;

  private
    FGame: TGLGame;
    FAspectUniform: TGLProgram.TUniform<Single>;
    FSpriteAtlas: TTextureAtlas;
    FSprites: TSprites;
    FSpritesSorted: Boolean;
    FVAOChanged: Boolean;
    FVAO: TVAO;

    procedure SortSprites;
    procedure UpdateVAO;

    function GetSprites: TSprites.TReader;

    procedure SpriteChanged(AInfo: TSprite.TChangeEventInfo);

    procedure Update;
    procedure Render;
    procedure Resize;

  public
    constructor Create(AGame: TGLGame; AGLProgram: TGLProgram);
    destructor Destroy; override;

    property Game: TGLGame read FGame;

    function AddSprite(ATexture: string): TSprite; overload;
    function AddSprite(ATexture: TTextureAtlas.TTile): TSprite; overload;
    function AddSprite(AAnimation: TAnimation): TSprite; overload;

    property Sprites: TSprites.TReader read GetSprites;
    property SpriteAtlas: TTextureAtlas read FSpriteAtlas;

  end;

implementation

{ TSpriteGLProgamBase }

class function TSpriteGLProgamBase.GetAttributeOrder: TGLProgram.TAttributeOrder;
begin
  Result := [
    'vpos',
    'vcolor',
    'vfade',
    'vborderlow0',
    'vborderhigh0',
    'vborderlow1',
    'vborderhigh1',
    'vtexcoord0',
    'vtexcoord1'
    ];
end;

{ TSprite }

procedure TSprite.SetPos(const Value: TVector2);
begin
  if Pos = Value then
    Exit;
  FPos := Value;
  FChanges := FChanges + [scPosX, scPosY];
  FBounds.Clear;
end;

procedure TSprite.SetPosX(const Value: Single);
begin
  if PosX = Value then
    Exit;
  FPos.X := Value;
  Include(FChanges, scPosX);
  FBounds.Clear;
end;

procedure TSprite.SetPosY(const Value: Single);
begin
  if PosY = Value then
    Exit;
  FPos.Y := Value;
  Include(FChanges, scPosY);
  FBounds.Clear;
end;

procedure TSprite.SetOffset(const Value: TVector2);
begin
  if Offset = Value then
    Exit;
  FOffset := Value;
  FChanges := FChanges + [scPosX, scPosY];
  FBounds.Clear;
end;

procedure TSprite.SetOffsetX(const Value: Single);
begin
  if OffsetX = Value then
    Exit;
  FOffset.X := Value;
  Include(FChanges, scPosX);
  FBounds.Clear;
end;

procedure TSprite.SetOffsetY(const Value: Single);
begin
  if OffsetY = Value then
    Exit;
  FOffset.Y := Value;
  Include(FChanges, scPosY);
  FBounds.Clear;
end;

procedure TSprite.SetRotation(Value: Single);
begin
  Value := Bounds1(0, 360).RangedModL(Value);
  if Rotation = Value then
    Exit;
  FRotation := Value;
  FChanges := FChanges + [scPosX, scPosY];
  FBounds.Clear;
end;

procedure TSprite.SetZOrder(const Value: Single);
begin
  if ZOrder = Value then
    Exit;
  FZOrder := Value;
  Include(FChanges, scZOrder);
  FBounds.Clear;
end;

procedure TSprite.SetScale(const Value: TVector2);
begin
  if Scale = Value then
    Exit;
  FScale := Value;
  FChanges := FChanges + [scPosX, scPosY];
  FBounds.Clear;
end;

procedure TSprite.SetColor(const Value: TColorRGBA);
begin
  if Color = Value then
    Exit;
  FColor := Value;
  Include(FChanges, scColor);
end;

procedure TSprite.SetTexture(const Value: string);
begin
  if Texture = Value then
    Exit;
  FTexture := Value;
  TextureTile := SpriteSystem.SpriteAtlas[Texture];
end;

procedure TSprite.SetTextureTile(const Value: TTextureAtlas.TTile);
var
  OldTile: TTextureAtlas.TTile;
begin
  if TextureTile = Value then
    Exit;
  OldTile := FTextureTile;
  FTextureTile := Value;
  if (OldTile <> nil) and (OldTile.Aspect <> TextureTile.Aspect) then
  begin
    FBounds.Clear;
    FChanges := FChanges + [scPosX, scPosY];
  end;
  Include(FChanges, scTexCoords);
  Texture := TextureTile.Name;
end;

procedure TSprite.SetFadeTexture(const Value: string);
begin
  if FadeTexture = Value then
    Exit;
  FFadeTexture := Value;
  FadeTextureTile := SpriteSystem.SpriteAtlas[FFadeTexture];
end;

procedure TSprite.SetFadeTextureTile(const Value: TTextureAtlas.TTile);
var
  OldTile: TTextureAtlas.TTile;
begin
  if FadeTextureTile = Value then
    Exit;
  OldTile := FFadeTextureTile;
  FFadeTextureTile := Value;
  if (OldTile <> nil) and (OldTile.Aspect <> FadeTextureTile.Aspect) then
  begin
    FBounds.Clear;
    FChanges := FChanges + [scPosX, scPosY];
  end;
  Include(FChanges, scTexCoords);
  FadeTexture := FadeTextureTile.Name;
end;

function TSprite.GetBehaviors: TBehaviors.TReader;
begin
  Result := FBehaviors.Reader;
end;

function TSprite.GetBounds: TPlane2;
begin
  if not FBounds.HasValue then
  begin
    Result.Create(
      Pos,
      TVector2.FromAngle(Rotation) * Scale.X * TextureTile.Size.X / TextureTile.Size.Y,
      TVector2.FromAngle(Rotation + 90) * Scale.Y
      );
    Result.S := Result.S + Result.D1 * (Offset.X - 0.5) + Result.D2 * (Offset.Y - 0.5);
    FBounds.Value := Result;
  end
  else
    Result := FBounds.Value;
end;

function TSprite.GetGame: TGLGame;
begin
  Result := SpriteSystem.Game;
end;

procedure TSprite.SetFade(const Value: Single);
begin
  if Fade = Value then
    Exit;
  FFade := Value;
  Include(FChanges, scFade);
end;

function TSprite.GetOnChanged: TChangeEvent.TAccess;
begin
  Result := FOnChanged.Access;
end;

constructor TSprite.Create(ASpriteSystem: TSpriteSystem; ATextureTile: TTextureAtlas.TTile);
begin
  FSpriteSystem := ASpriteSystem;
  FBehaviors := TBehaviors.Create;
  FBounds := TOpt<TPlane2>.Create;
  FScale := Vec2(1, 1);
  FColor := ColorWhite;
  TextureTile := ATextureTile;
end;

destructor TSprite.Destroy;
var
  Behavior: TBehavior;
begin
  FBounds.Free;
  for Behavior in FBehaviors do
    Behavior.Free;
  FBehaviors.Free;
  if FRemoved then
    SpriteSystem.FSprites.Del(Self);
  inherited;
end;

procedure TSprite.Remove;
begin
  FRemoved := True;
end;

procedure TSprite.Update;
var
  Behavior: TBehavior;
  Removed: TObjectArray<TBehavior>;
begin
  Removed := TObjectArray<TBehavior>.Create;
  for Behavior in FBehaviors do
  begin
    if Behavior.Enabled then
      Behavior.Update;
    if Behavior.Removed then
      Removed.Add(Behavior);
  end;
  Removed.Free;
  if FChanges <> [] then
    FOnChanged.Execute(TChangeEventInfo.Create(Self, FChanges));
end;

procedure TSprite.UpdateVAO(AWriter: TVBO<TSpriteGLProgamBase.TData>.TWriter);
var
  I: TQuadIndex;
begin
  with AWriter do
  begin
    if FChanges = [] then
    begin
      BufferPos := BufferPos + 6;
      Exit;
    end;

    if scPosX in FChanges then
      for I := Low(TQuadIndex) to High(TQuadIndex) do
        BufferData[BufferPos + I].Pos.X := Bounds[QuadTexCoords[I]].X;

    if scPosY in FChanges then
      for I := Low(TQuadIndex) to High(TQuadIndex) do
        BufferData[BufferPos + I].Pos.Y := Bounds[QuadTexCoords[I]].Y;

    if scZOrder in FChanges then
      for I := Low(TQuadIndex) to High(TQuadIndex) do
        BufferData[BufferPos + I].ZOrder := ZOrder;

    if scColor in FChanges then
    begin
      BufferData[BufferPos + 2].Color := Color;
      BufferData[BufferPos + 5].Color := Color;
    end;

    if scFade in FChanges then
    begin
      BufferData[BufferPos + 2].Fade := Fade;
      BufferData[BufferPos + 5].Fade := Fade;
    end;

    if scTexCoords in FChanges then
    begin
      if TextureTile <> nil then
      begin
        BufferData[BufferPos + 2].Borders[0] := TextureTile.BoundsHalfPixelInset;
        BufferData[BufferPos + 5].Borders[0] := BufferData[BufferPos + 2].Borders[0];
      end;
      if FadeTextureTile <> nil then
      begin
        BufferData[BufferPos + 2].Borders[1] := FadeTextureTile.BoundsHalfPixelInset;
        BufferData[BufferPos + 5].Borders[1] := BufferData[BufferPos + 2].Borders[1];
      end;
      for I := Low(TQuadIndex) to High(TQuadIndex) do
      begin
        with BufferData[BufferPos + I] do
        begin
          if TextureTile <> nil then
            TexCoords[0] := TextureTile.Bounds[QuadTexCoords[I]];
          if FadeTextureTile <> nil then
            TexCoords[1] := FadeTextureTile.Bounds[QuadTexCoords[I]];
        end;
      end;
    end;

    FChanges := [];

    BufferPos := BufferPos + 6;
  end;
end;

{ TSpriteSystem }

function TSpriteSystem.AddSprite(ATexture: string): TSprite;
begin
  Result := AddSprite(SpriteAtlas[ATexture]);
end;

function TSpriteSystem.AddSprite(ATexture: TTextureAtlas.TTile): TSprite;
begin
  Result := TSprite.Create(Self, ATexture);
  Result.OnChanged.Add(SpriteChanged);
  FSprites.Add(Result);
  FSpritesSorted := False;
  FVAO.VBO.Generate(FSprites.Count * 6, buDynamicDraw);
  FVAOChanged := True;
end;

function TSpriteSystem.AddSprite(AAnimation: TAnimation): TSprite;
begin
  raise ENotImplemented.Create('AddSprite(Animation)');
end;

constructor TSpriteSystem.Create(AGame: TGLGame; AGLProgram: TGLProgram);
begin
  FGame := AGame;
  Game.OnUpdate.Add(Update);
  Game.OnRender.Add(Render);
  Game.OnResize.Add(Resize);
  FAspectUniform := AGLProgram.Uniform<Single>('aspect');
  FVAO := TVAO.Create(AGLProgram);
  FSpriteAtlas := TTextureAtlas.Create(AGame.GLState);
  FSprites := TSprites.Create;
end;

destructor TSpriteSystem.Destroy;
var
  Sprite: TSprite;
begin
  for Sprite in FSprites do
    Sprite.Free;
  FSprites.Free;
  FSpriteAtlas.Free;
  FVAO.Free;
  inherited;
end;

function TSpriteSystem.GetSprites: TSprites.TReader;
begin
  Result := FSprites.Reader;
end;

procedure TSpriteSystem.Update;
var
  Sprite: TSprite;
  Removed: TObjectArray<TSprite>;
begin
  Removed := TObjectArray<TSprite>.Create;
  for Sprite in FSprites do
  begin
    Sprite.Update;
    if Sprite.Removed then
      Removed.Add(Sprite);
  end;
  Removed.Free;
  if not FSpritesSorted then
    SortSprites;
end;

procedure TSpriteSystem.UpdateVAO;
var
  Writer: TVBO<TSpriteGLProgamBase.TData>.TWriter;
  Sprite: TSprite;
begin
  Writer := FVAO.VBO.Map;
  try
    for Sprite in FSprites do
      Sprite.UpdateVAO(Writer);
  finally
    Writer.Free;
  end;
  FVAOChanged := False;
end;

procedure TSpriteSystem.Render;
begin
  if FVAOChanged then
    UpdateVAO;
  FVAO.Render;
end;

procedure TSpriteSystem.Resize;
begin
  FAspectUniform.Value := Game.Aspect;
end;

procedure TSpriteSystem.SortSprites;
var
  Sprite: TSprite;
begin
  FSprites.Sort(
    function(ALeft, ARight: TSprite): Boolean
    begin
      Result := ALeft.ZOrder > ARight.ZOrder;
    end
    );
  for Sprite in FSprites do
    Sprite.FChanges := [Low(TSprite.TChange) .. High(TSprite.TChange)];
  FSpritesSorted := True;
end;

procedure TSpriteSystem.SpriteChanged(AInfo: TSprite.TChangeEventInfo);
begin
  if scZOrder in AInfo.Changes then
    FSpritesSorted := False;
  FVAOChanged := True;
end;

{ TAnimation }

constructor TAnimation.Create;
begin
  FFrames := TFrames.Create;
end;

destructor TAnimation.Destroy;
begin
  FFrames.Free;
  inherited;
end;

{ TSprite.TChangeEventInfo }

constructor TSprite.TChangeEventInfo.Create(ASender: TSprite; AChanges: TChanges);
begin
  FSender := ASender;
  FChanges := AChanges;
end;

function TSprite.TChangeEventInfo.Sender: TSprite;
begin
  Result := Sender;
end;

{ TSprite.TBehavior }

procedure TSprite.TBehavior.AddEvents;
begin
  // nothing
end;

constructor TSprite.TBehavior.Create(ASprite: TSprite);
begin
  FSprite := ASprite;
  Sprite.FBehaviors.Add(Self);
  Enabled := True;
end;

procedure TSprite.TBehavior.DelEvents;
begin
  // nothing
end;

destructor TSprite.TBehavior.Destroy;
begin
  Enabled := False;
  if FRemoved then
    FSprite.FBehaviors.Del(Self);
  inherited;
end;

procedure TSprite.TBehavior.Disable;
begin
  Enabled := False;
end;

procedure TSprite.TBehavior.Enable;
begin
  Enabled := True;
end;

function TSprite.TBehavior.GetGame: TGLGame;
begin
  Result := Sprite.SpriteSystem.Game;
end;

function TSprite.TBehavior.GetOnDone: TSenderEvent.TAccess;
begin
  Result := FOnDone.Access;
end;

procedure TSprite.TBehavior.Remove;
begin
  FRemoved := True;
  FOnDone.Execute(TSenderEventInfo.Create(Self));
end;

procedure TSprite.TBehavior.SetEnabled(const Value: Boolean);
begin
  if Enabled = Value then
    Exit;
  FEnabled := Value;
  if Enabled then
    AddEvents
  else
    DelEvents;
end;

procedure TSprite.TBehavior.Update;
begin
  // nothing by default
end;

{ ESpriteBehaviorNoDefault }

constructor ESpriteBehaviorNoDefault.Create;
begin
  inherited Create('The sprite behavior class does not have a default constructor.');
end;

{ TSprite.TFadeBehavior }

constructor TSprite.TFadeBehavior.Create(ASprite: TSprite);
begin
  raise ESpriteBehaviorNoDefault.Create;
end;

constructor TSprite.TFadeBehavior.Create(ASprite: TSprite; ATexture: string; ADuration: Single);
begin
  Create(ASprite, ASprite.SpriteSystem.SpriteAtlas[ATexture], ADuration);
end;

constructor TSprite.TFadeBehavior.Create(ASprite: TSprite; ATexture: TTextureAtlas.TTile; ADuration: Single);
begin
  inherited Create(ASprite);
  Sprite.FadeTextureTile := ATexture;
  FDuration := ADuration;
  FTimeLeft := (1 - Sprite.Fade) * FDuration;
end;

function TSprite.TFadeBehavior.GetProgress: Single;
begin
  Result := 1 - TimeLeft / Duration;
end;

procedure TSprite.TFadeBehavior.Update;
begin
  FTimeLeft := Max(TimeLeft - Game.DeltaTime, 0);
  if FTimeLeft = 0 then
  begin
    Sprite.TextureTile := Sprite.FadeTextureTile;
    Sprite.Fade := 0;
    Remove;
  end
  else
    Sprite.Fade := Progress;
end;

{ TSprite.TBehavior.TSenderEventInfo }

constructor TSprite.TBehavior.TSenderEventInfo.Create(ASender: TBehavior);
begin
  FSender := ASender;
end;

function TSprite.TBehavior.TSenderEventInfo.Sender: TBehavior;
begin
  Result := FSender;
end;

end.
