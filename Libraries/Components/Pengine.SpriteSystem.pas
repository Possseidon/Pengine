unit Pengine.SpriteSystem;

interface

uses
  dglOpenGL,

  System.SysUtils,
  System.Math,

  Pengine.IntMaths,
  Pengine.TimeManager,
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
  Pengine.EventMap,
  Pengine.GLGame,
  Pengine.GLEnums,
  Pengine.FBO,
  Pengine.Utility,
  Pengine.InputHandler;

type

  ESpriteBehaviorNoDefault = class(Exception)
  public
    constructor Create;
  end;

  ECharSpriteInvalidFont = class(Exception)
  public
    constructor Create;
  end;

  ESpriteRemovedAlready = class(Exception)
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

  TSpriteSystem = class;

  TSprite = class
  public type

    TBehavior = class
    public type

      TEventInfo = TSenderEventInfo<TBehavior>;
      TEvent = TEvent<TEventInfo>;

    private
      FSprite: TSprite;
      FEnabled: Boolean;
      FRemoved: Boolean;
      FOnDone: TEvent;

      function GetGame: TGLGame;

      procedure SetEnabled(const Value: Boolean);

      function GetOnDone: TEvent.TAccess;

    protected
      procedure AddEvents; virtual;
      procedure DelEvents; virtual;

    public
      constructor Create(ASprite: TSprite); virtual;
      class procedure Add(ASprite: TSprite);
      destructor Destroy; override;

      property Game: TGLGame read GetGame;
      property Sprite: TSprite read FSprite;

      property Enabled: Boolean read FEnabled write SetEnabled;
      procedure Enable;
      procedure Disable;

      procedure Remove;
      property Removed: Boolean read FRemoved;

      property OnDone: TEvent.TAccess read GetOnDone;

    end;

    TBehaviorClass = class of TBehavior;

    TBehaviors = TRefSet<TBehavior, TRefHasher<TBehavior>>;

    TEventInfo = TSenderEventInfo<TSprite>;

    TEvent = TEvent<TEventInfo>;

    TChange = (
      scPos,
      scZOrder,
      scColor,
      scFade,
      scTexture
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

    TUpdateBehavior = class(TBehavior)
    protected
      procedure AddEvents; override;
      procedure DelEvents; override;

    protected
      procedure Update; virtual; abstract;

    end;

    TFadeBehavior = class(TUpdateBehavior)
    private
      FDuration: TSeconds;
      FTimeLeft: TSeconds;

      function GetProgress: Single;

    public
      constructor Create(ASprite: TSprite); overload; override;
      constructor Create(ASprite: TSprite; ATexture: string; ADuration: TSeconds); reintroduce; overload;
      constructor Create(ASprite: TSprite; ATexture: TTextureAtlas.TTile; ADuration: TSeconds); reintroduce; overload;

      procedure Update; override;

      property Duration: TSeconds read FDuration;
      property TimeLeft: TSeconds read FTimeLeft;
      property Progress: Single read GetProgress;

    end;

    TAnimationBeavior = class(TUpdateBehavior)
    private
      FBaseTextureTile: TTextureAtlas.TTile;
      FFrameTime: TSeconds;
      FTimeLeft: TSeconds;
      FFadePercentage: Single;
      FRange: TIntBounds1;
      FReversed: Boolean;

      procedure SpriteChanged(AInfo: TChangeEventInfo);

      function GetDuration: TSeconds;
      procedure SetDuration(const Value: TSeconds);
      procedure SetFrameTime(const Value: TSeconds);

      function GetFrame: Integer;
      procedure SetRange(const Value: TIntBounds1);
      procedure SetFrame(const Value: Integer);
      function GetFrameFadePercentage: Single;

      procedure SetFrameWithoutFrameTime(AFrame: Integer);

      procedure SetReversed(const Value: Boolean);

    protected
      procedure AddEvents; override;
      procedure DelEvents; override;

    public
      constructor Create(ASrite: TSprite); overload; override;
      constructor Create(ASprite: TSprite; ADuration: TSeconds; AFadePercentage: Single = 1); reintroduce; overload;

      property FadePercentage: Single read FFadePercentage write FFadePercentage;
      property Duration: TSeconds read GetDuration write SetDuration;
      property FrameTime: TSeconds read FFrameTime write SetFrameTime;

      property Frame: Integer read GetFrame write SetFrame;
      property Range: TIntBounds1 read FRange write SetRange;
      property FrameTimeLeft: TSeconds read FTimeLeft;
      property FrameFadePercentage: Single read GetFrameFadePercentage;

      property Reversed: Boolean read FReversed write SetReversed;

      procedure Restart;

      procedure Update; override;

    end;

    TFollowMouseBehavior = class(TUpdateBehavior)
    protected
      procedure Update; override;
    end;

  private
    FSpriteSystem: TSpriteSystem;
    FRemoved: Boolean;
    FIndex: Integer;
    FChanges: TChanges;
    FInChangedSprites: Boolean;
    FUpdateCounter: Integer;
    FBehaviors: TBehaviors;
    FOnChanged: TChangeEvent;
    FOnRemove: TEvent;

    FLocation: TLocation2;
    FZOrder: Single;

    FColor: TColorRGBA;
    FTexture: string;
    FTextureTile: TTextureAtlas.TTile;
    FFadeTexture: string;
    FFadeTextureTile: TTextureAtlas.TTile;
    FFade: Single;

    FBounds: TOpt<TAxisSystem2>;

    function GetGame: TGLGame;

    procedure SetZOrder(const Value: Single);

    procedure SetColor(const Value: TColorRGBA);

    procedure SetTexture(const Value: string);
    procedure SetTextureTile(const Value: TTextureAtlas.TTile);
    function GetTextureIndex: Integer;
    procedure SetTextureIndex(const Value: Integer);

    procedure SetFadeTexture(const Value: string);
    procedure SetFadeTextureTile(const Value: TTextureAtlas.TTile);
    function GetFadeSubTextureIndex: Integer;
    procedure SetFadeSubTextureIndex(const Value: Integer);

    procedure SetFade(const Value: Single);

    function GetAspect: Single;
    function GetAspectPlus(APixels: Single): Single;
    function GetAspectScaled: Single;
    function GetAspectScaledPlus(APixels: Single): Single;

    function GetBounds: TAxisSystem2;

    function GetBehaviors: TBehaviors.TReader;

    function GetOnChanged: TChangeEvent.TAccess;
    function GetOnUpdate: Pengine.EventHandling.TEvent.TAccess;

    procedure Changed(AChange: TChange); overload;
    procedure Changed(AChanges: TChanges); overload;

    procedure LocationChanged(AInfo: TLocation2.TChangeEventInfo);
    function GetOnRemove: TEvent.TAccess;

  public
    constructor Create(ASpriteSystem: TSpriteSystem; ATextureTile: TTextureAtlas.TTile); virtual;
    destructor Destroy; override;

    property SpriteSystem: TSpriteSystem read FSpriteSystem;
    property Game: TGLGame read GetGame;

    procedure Remove;
    property Removed: Boolean read FRemoved;

    property Location: TLocation2 read FLocation;

    property ZOrder: Single read FZOrder write SetZOrder;

    property Color: TColorRGBA read FColor write SetColor;

    property Texture: string read FTexture write SetTexture;
    property TextureTile: TTextureAtlas.TTile read FTextureTile write SetTextureTile;
    property SubTextureIndex: Integer read GetTextureIndex write SetTextureIndex;

    property FadeTexture: string read FFadeTexture write SetFadeTexture;
    property FadeTextureTile: TTextureAtlas.TTile read FFadeTextureTile write SetFadeTextureTile;
    property FadeSubTextureIndex: Integer read GetFadeSubTextureIndex write SetFadeSubTextureIndex;
    property Fade: Single read FFade write SetFade;

    property Aspect: Single read GetAspect;
    property AspectPlus[APixels: Single]: Single read GetAspectPlus;
    property AspectScaled: Single read GetAspectScaled;
    property AspectScaledPlus[APixels: Single]: Single read GetAspectScaledPlus;

    property Bounds: TAxisSystem2 read GetBounds;

    property Behaviors: TBehaviors.TReader read GetBehaviors;

    procedure UpdateVAO(AWriter: TVBO<TSpriteGLProgamBase.TData>.TWriter);

    procedure BeginUpdate;
    procedure EndUpdate;

    property OnChanged: TChangeEvent.TAccess read GetOnChanged;
    property OnRemove: TEvent.TAccess read GetOnRemove;

    property OnUpdate: Pengine.EventHandling.TEvent.TAccess read GetOnUpdate;

  end;

  TSpriteSystem = class
  public type

    TSprites = TRefArray<TSprite>;
    TSpriteUpdater = TEventMap<TSprite, TRefHasher<TSprite>>;
    TVAO = TVAOMutable<TSpriteGLProgamBase.TData>;

  private
    FGame: TGLGame;
    FFBOBinding: TGLObjectBinding<TFBO>;
    FAspectUniform: TGLProgram.TUniform<Single>;
    FSpriteAtlas: TTextureAtlas;
    FSprites: TSprites;
    FChangedSprites: TSprites;
    FRemovedSprites: TSprites;
    FSpriteUpdater: TSpriteUpdater;
    FSpritesSorted: Boolean;
    FVAOChanged: Boolean;
    FVAO: TVAO;

    procedure SortSprites;
    procedure UpdateVAO(AUpdateAll: Boolean);

    function GetSprites: TSprites.TReader;

    procedure SpriteChanged(AInfo: TSprite.TChangeEventInfo);
    procedure SpriteRemoved(AInfo: TSprite.TEventInfo);

    procedure Update;
    procedure Render;
    procedure Resize;

  public
    constructor Create(AGame: TGLGame; AGLProgram: TGLProgram);
    destructor Destroy; override;

    property Game: TGLGame read FGame;

    function Add<T: TSprite>(ATexture: string): T; overload;
    function Add<T: TSprite>(ATexture: TTextureAtlas.TTile): T; overload;

    property Sprites: TSprites.TReader read GetSprites;
    property SpriteAtlas: TTextureAtlas read FSpriteAtlas;

  end;

  TCharSprite = class(TSprite)
  private
    FOldFontTile: TTextureAtlas.TTile;
    FSpacePixels: Single;

    function GetChar: Char;
    procedure SetChar(const Value: Char);
    function GetFontTile: TTextureAtlas.TTile;
    procedure SetFontTile(const Value: TTextureAtlas.TTile);
    function GetFont: string;
    procedure SetFont(const Value: string);
    function GetWidth: Single;
    function GetWidthSpaced: Single;

    procedure SpriteChanged(AInfo: TSprite.TChangeEventInfo);

  public
    constructor Create(ASpriteSystem: TSpriteSystem; ATextureTile: TTextureAtlas.TTile); override;

    property Char: Char read GetChar write SetChar;
    property FontTile: TTextureAtlas.TTile read GetFontTile write SetFontTile;
    property Font: string read GetFont write SetFont;
    property Width: Single read GetWidth;
    property WidthSpaced: Single read GetWidthSpaced;

  end;

  TCursorSprite = class(TSprite)

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

procedure TSprite.SetZOrder(const Value: Single);
begin
  if ZOrder = Value then
    Exit;
  FZOrder := Value;
  Changed(scZOrder);
end;

procedure TSprite.SetColor(const Value: TColorRGBA);
begin
  if Color = Value then
    Exit;
  FColor := Value;
  Changed(scColor);
end;

procedure TSprite.SetTexture(const Value: string);
begin
  if Texture = Value then
    Exit;
  FTexture := Value;
  FTextureTile := SpriteSystem.SpriteAtlas[Texture];
end;

procedure TSprite.SetTextureIndex(const Value: Integer);
begin
  TextureTile := TextureTile.SubTiles[Value];
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
    Changed(scPos);
  end;
  Changed(scTexture);
  FTexture := TextureTile.Name;
end;

procedure TSprite.SetFadeTexture(const Value: string);
begin
  if FadeTexture = Value then
    Exit;
  FFadeTexture := Value;
  FadeTextureTile := SpriteSystem.SpriteAtlas[FFadeTexture];
end;

procedure TSprite.SetFadeTextureTile(const Value: TTextureAtlas.TTile);
begin
  if FadeTextureTile = Value then
    Exit;
  FFadeTextureTile := Value;
  Changed(scTexture);
  FFadeTexture := FadeTextureTile.Name;
end;

function TSprite.GetAspect: Single;
begin
  if FadeTextureTile <> nil then
    Result := (1 - Fade) * TextureTile.Aspect + Fade * FadeTextureTile.Aspect
  else
    Result := TextureTile.Aspect;
end;

function TSprite.GetAspectPlus(APixels: Single): Single;
begin
  if FadeTextureTile <> nil then
    Result :=
      (1 - Fade) * (TextureTile.Size.X + APixels) / TextureTile.Size.Y +
      Fade * (FadeTextureTile.Size.X + APixels) / FadeTextureTile.Size.Y
  else
    Result := (TextureTile.Size.X + APixels) / TextureTile.Size.Y;
end;

function TSprite.GetAspectScaled: Single;
begin
  Result := Aspect * Location.Scale.X / Location.Scale.Y;
end;

function TSprite.GetAspectScaledPlus(APixels: Single): Single;
begin
  Result := AspectPlus[APixels] * Location.Scale.X / Location.Scale.Y;
end;

function TSprite.GetBehaviors: TBehaviors.TReader;
begin
  Result := FBehaviors.Reader;
end;

function TSprite.GetBounds: TAxisSystem2;
begin
  if not FBounds.HasValue then
  begin
    Result := Location.AxisSystem;
    Result.S := Result.S - Aspect * 0.5 * Result.DX - 0.5 * Result.DY;
    Result.DX := Result.DX * Aspect;
    FBounds.Value := Result;
  end
  else
    Result := FBounds.Value;
end;

function TSprite.GetFadeSubTextureIndex: Integer;
begin
  Result := FadeTextureTile.SubTileIndex;
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
  if (FadeTextureTile <> nil) and (TextureTile.Aspect <> FadeTextureTile.Aspect) then
    Changed([scFade, scPos])
  else
    Changed(scFade);
end;

procedure TSprite.SetFadeSubTextureIndex(const Value: Integer);
begin
  FadeTextureTile := FadeTextureTile.SubTiles[Value];
end;

function TSprite.GetOnChanged: TChangeEvent.TAccess;
begin
  Result := FOnChanged.Access;
end;

function TSprite.GetOnRemove: TEvent.TAccess;
begin
  Result := FOnRemove.Access;
end;

function TSprite.GetOnUpdate: Pengine.EventHandling.TEvent.TAccess;
begin
  Result := FSpriteSystem.FSpriteUpdater.Access[Self];
end;

function TSprite.GetTextureIndex: Integer;
begin
  Result := TextureTile.SubTileIndex;
end;

procedure TSprite.LocationChanged(AInfo: TLocation2.TChangeEventInfo);
begin
  Changed(scPos);
end;

procedure TSprite.Changed(AChange: TChange);
begin
  if not FInChangedSprites then
  begin
    SpriteSystem.FChangedSprites.Add(Self);
    FInChangedSprites := True;
  end;
  if FUpdateCounter = 0 then
    FOnChanged.Execute(TChangeEventInfo.Create(Self, [AChange]));
  Include(FChanges, AChange);
  if AChange = scPos then
    FBounds.Clear;
end;

procedure TSprite.BeginUpdate;
begin
  Inc(FUpdateCounter);
end;

procedure TSprite.Changed(AChanges: TChanges);
begin
  if not FInChangedSprites then
  begin
    SpriteSystem.FChangedSprites.Add(Self);
    FInChangedSprites := True;
  end;
  if FUpdateCounter = 0 then
    FOnChanged.Execute(TChangeEventInfo.Create(Self, AChanges));
  FChanges := FChanges + AChanges;
  if scPos in AChanges then
    FBounds.Clear;
end;

constructor TSprite.Create(ASpriteSystem: TSpriteSystem; ATextureTile: TTextureAtlas.TTile);
begin
  FSpriteSystem := ASpriteSystem;
  FBehaviors := TBehaviors.Create;
  FBounds := TOpt<TAxisSystem2>.Create;
  FLocation := TLocation2.Create;
  FColor := ColorWhite;
  TextureTile := ATextureTile;
  Location.OnChanged.Add(LocationChanged);
end;

destructor TSprite.Destroy;
var
  Behavior: TBehavior;
  I: Integer;
begin
  FLocation.Free;
  FBounds.Free;
  for Behavior in FBehaviors do
    Behavior.Free;
  FBehaviors.Free;
  if FRemoved then
  begin
    if FInChangedSprites then
      SpriteSystem.FChangedSprites.Del(Self);
    SpriteSystem.FSprites.DelAt(Self.FIndex);
    for I := FIndex to SpriteSystem.Sprites.MaxIndex do
    begin
      Dec(SpriteSystem.FSprites[I].FIndex);
      SpriteSystem.FSprites[I].Changed([Low(TChange) .. High(TChange)]);
    end;
  end;
  inherited;
end;

procedure TSprite.EndUpdate;
begin
  Dec(FUpdateCounter);
  if FUpdateCounter = 0 then
    FOnChanged.Execute(TChangeEventInfo.Create(Self, FChanges));
end;

procedure TSprite.Remove;
begin
  if FRemoved then
    raise ESpriteRemovedAlready.Create;
  FRemoved := True;
  FOnRemove.Execute(TEventInfo.Create(Self));
end;

procedure TSprite.UpdateVAO(AWriter: TVBO<TSpriteGLProgamBase.TData>.TWriter);
var
  I: TQuadIndex;
begin
  with AWriter do
  begin
    FInChangedSprites := False;

    BufferPos := FIndex * 6;

    if scPos in FChanges then
      for I := Low(TQuadIndex) to High(TQuadIndex) do
        BufferData[BufferPos + I].Pos := Bounds[QuadTexCoords[I]];

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

    if scTexture in FChanges then
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
  end;
end;

{ TSpriteSystem }

function TSpriteSystem.Add<T>(ATexture: string): T;
begin
  Result := Add<T>(SpriteAtlas[ATexture]);
end;

function TSpriteSystem.Add<T>(ATexture: TTextureAtlas.TTile): T;
begin
  Result := T.Create(Self, ATexture);
  Result.OnChanged.Add(SpriteChanged);
  Result.OnRemove.Add(SpriteRemoved);
  Result.FIndex := FSprites.Count;
  FSprites.Add(Result);
  FSpritesSorted := False;
  FVAOChanged := True;
end;

constructor TSpriteSystem.Create(AGame: TGLGame; AGLProgram: TGLProgram);
begin
  FGame := AGame;
  Game.OnUpdate.Add(Update);
  Game.OnRender.Add(Render);
  Game.OnResize.Add(Resize);
  FAspectUniform := AGLProgram.Uniform<Single>('aspect');
  FFBOBinding := Game.GLState.GLObjectBindings.Get<TFBO>;
  FVAO := TVAO.Create(AGLProgram);
  FSpriteAtlas := TTextureAtlas.Create(AGame.GLState);
  FSprites := TSprites.Create;
  FChangedSprites := TSprites.Create;
  FRemovedSprites := TSprites.Create;
  FSpriteUpdater := TSpriteUpdater.Create;
  FSpritesSorted := True;
end;

destructor TSpriteSystem.Destroy;
var
  Sprite: TSprite;
begin
  StartTimer;
  // in reverse, so that (behavior) event handlers are found faster
  for Sprite in FSprites.InReverse do
    Sprite.Free;
  FSpriteUpdater.Free;
  FRemovedSprites.Free;
  FChangedSprites.Free;
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
begin
  FSpriteUpdater.Execute;

  if not FRemovedSprites.Empty then
  begin
    for Sprite in FRemovedSprites do
    begin
      Sprite.Free;
      FSpritesSorted := False;
    end;
    FRemovedSprites.Clear;
  end;

  if not FSpritesSorted then
    SortSprites;
end;

procedure TSpriteSystem.UpdateVAO(AUpdateAll: Boolean);
var
  Writer: TVBO<TSpriteGLProgamBase.TData>.TWriter;
  Sprite: TSprite;
begin
  Writer := FVAO.VBO.Map;
  try
    if AUpdateAll then
    begin
      for Sprite in FSprites do
      begin
        Sprite.FChanges := [Low(TSprite.TChange) .. High(TSprite.TChange)];
        Sprite.UpdateVAO(Writer);
      end;
    end
    else
    begin
      for Sprite in FChangedSprites do
        Sprite.UpdateVAO(Writer);
    end;

  finally
    Writer.Free;
  end;
  FVAOChanged := False;
end;

procedure TSpriteSystem.Render;
var
  Sprite: TSprite;
  VBOCountChanged: Boolean;
  VBOCount: Integer;
begin
  if not FChangedSprites.Empty then
  begin
    VBOCountChanged := False;
    VBOCount := FVAO.VBO.Count div 6;
    while FSprites.Count > VBOCount do
    begin
      VBOCount := Max(VBOCount, 1) shl 1;
      VBOCountChanged := True;
    end;
    while FSprites.Count * 4 < VBOCount do
    begin
      VBOCount := VBOCount shr 1;
      VBOCountChanged := True;
    end;
    if VBOCountChanged then
      FVAO.VBO.Generate(VBOCount * 6, buDynamicDraw);
    if not FChangedSprites.Empty then
    begin
      UpdateVAO(VBOCountChanged);
      FChangedSprites.Clear;
    end;
  end;

  Game.GLContext.Clear([amDepth]);

  Game.GLState.Push;

  Game.GLState[stBlend] := True;
  Game.GLState[stBlendFunc] := TGLBlendFunc.Make(bfsSrcAlpha, bfdOneMinusSrcAlpha);
  Game.GLState[stCullFace] := False;

  FVAO.Render(IBounds1(FSprites.Count * 6));

  Game.GLState.Pop;
end;

procedure TSpriteSystem.Resize;
begin
  FAspectUniform.Value := Game.Aspect;
end;

procedure TSpriteSystem.SortSprites;
var
  I: Integer;
begin
  FSprites.Sort(
    function(ALeft, ARight: TSprite): Boolean
    begin
      Result := (ALeft.ZOrder < ARight.ZOrder) or
        (ALeft.ZOrder = ARight.ZOrder) and (ALeft.FIndex < ARight.FIndex);
    end
    );
  for I := 0 to FSprites.MaxIndex do
  begin
    if FSprites[I].FIndex <> I then
    begin
      FSprites[I].FIndex := I;
      FSprites[I].FChanges := [Low(TSprite.TChange) .. High(TSprite.TChange)];
    end;
  end;
  FSpritesSorted := True;
end;

procedure TSpriteSystem.SpriteChanged(AInfo: TSprite.TChangeEventInfo);
begin
  if scZOrder in AInfo.Changes then
    FSpritesSorted := False;
  FVAOChanged := True;
end;

procedure TSpriteSystem.SpriteRemoved(AInfo: TSprite.TEventInfo);
begin
  FRemovedSprites.Add(AInfo.Sender);
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

class procedure TSprite.TBehavior.Add(ASprite: TSprite);
begin
  Self.Create(ASprite);
end;

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

function TSprite.TBehavior.GetOnDone: TBehavior.TEvent.TAccess;
begin
  Result := FOnDone.Access;
end;

procedure TSprite.TBehavior.Remove;
begin
  FRemoved := True;
  FOnDone.Execute(TSenderEventInfo<TBehavior>.Create(Self));
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

constructor TSprite.TFadeBehavior.Create(ASprite: TSprite; ATexture: string; ADuration: TSeconds);
begin
  Create(ASprite, ASprite.SpriteSystem.SpriteAtlas[ATexture], ADuration);
end;

constructor TSprite.TFadeBehavior.Create(ASprite: TSprite; ATexture: TTextureAtlas.TTile; ADuration: TSeconds);
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

{ TSprite.TSimpleAnimationBeavior }

procedure TSprite.TAnimationBeavior.SpriteChanged(AInfo: TChangeEventInfo);
begin
  if (scTexture in AInfo.Changes) and (FBaseTextureTile <> Sprite.TextureTile.SubTiles[0]) then
  begin
    FBaseTextureTile := Sprite.TextureTile;
    Restart;
  end;
end;

function TSprite.TAnimationBeavior.GetDuration: TSeconds;
begin
  Result := FFrameTime * Range.Length;
end;

procedure TSprite.TAnimationBeavior.SetDuration(const Value: TSeconds);
begin
  if Duration = Value then
    Exit;
  FFrameTime := Value / Range.Length;
end;

procedure TSprite.TAnimationBeavior.SetFrameTime(const Value: TSeconds);
begin
  if FrameTime = Value then
    Exit;
  FTimeLeft := FrameTimeLeft * Value / FFrameTime;
  FFrameTime := Value;
end;

procedure TSprite.TAnimationBeavior.SetFrameWithoutFrameTime(AFrame: Integer);
begin
  Sprite.SubTextureIndex := AFrame;
  Sprite.FadeTextureTile := FBaseTextureTile.SubTiles[Range.RangedMod(AFrame + 1)];
end;

procedure TSprite.TAnimationBeavior.SetRange(const Value: TIntBounds1);
begin
  if FRange = Value then
    Exit;
  FRange := Value;
  Frame := FRange.C1;
end;

procedure TSprite.TAnimationBeavior.SetReversed(const Value: Boolean);
begin
  if Reversed = Value then
    Exit;
  FReversed := Value;
  FTimeLeft := FFrameTime - FTimeLeft;
end;

function TSprite.TAnimationBeavior.GetFrame: Integer;
begin
  Result := Sprite.SubTextureIndex;
end;

procedure TSprite.TAnimationBeavior.SetFrame(const Value: Integer);
begin
  FTimeLeft := FrameTime;
  SetFrameWithoutFrameTime(Value);
end;

function TSprite.TAnimationBeavior.GetFrameFadePercentage: Single;
begin
  if FadePercentage = 0 then
    Exit(0);
  Result := Max(0, 1 - FrameTimeLeft / FrameTime / FadePercentage);
  if Reversed then
    Result := 1 - Result;
end;

procedure TSprite.TAnimationBeavior.Restart;
begin
  Frame := 0;
end;

procedure TSprite.TAnimationBeavior.AddEvents;
begin
  inherited;
  Sprite.OnChanged.Add(SpriteChanged);
end;

procedure TSprite.TAnimationBeavior.DelEvents;
begin
  inherited;
  Sprite.OnChanged.Del(SpriteChanged);
end;

constructor TSprite.TAnimationBeavior.Create(ASrite: TSprite);
begin
  inherited;
  FBaseTextureTile := Sprite.TextureTile.SubTiles[0];
  Range := IBounds1(FBaseTextureTile.SubTiles.Count);
  FFrameTime := 0.1;
  FTimeLeft := FrameTime;
  FFadePercentage := 1;
end;

constructor TSprite.TAnimationBeavior.Create(ASprite: TSprite; ADuration: TSeconds; AFadePercentage: Single);
begin
  inherited Create(ASprite);
  FBaseTextureTile := Sprite.TextureTile.SubTiles[0];
  Range := IBounds1(FBaseTextureTile.SubTiles.Count);
  Duration := ADuration;
  FTimeLeft := FrameTime;
  FFadePercentage := AFadePercentage;
end;

procedure TSprite.TAnimationBeavior.Update;
begin
  FTimeLeft := FrameTimeLeft - Game.DeltaTime;
  while FTimeLeft < 0 do
  begin
    FTimeLeft := FrameTimeLeft + FrameTime;
    if Reversed then
      Frame := Range.RangedMod(Frame - 1)
    else
      Frame := Range.RangedMod(Frame + 1);
  end;
  Sprite.Fade := FrameFadePercentage;
end;

{ TSprite.TUpdateBehavior }

procedure TSprite.TUpdateBehavior.AddEvents;
begin
  Sprite.OnUpdate.Add(Update);
end;

procedure TSprite.TUpdateBehavior.DelEvents;
begin
  Sprite.OnUpdate.Del(Update);
end;

{ TSprite.TFollowMouseBehavior }

procedure TSprite.TFollowMouseBehavior.Update;
begin
  Sprite.Location.Pos := Game.Input.MousePosRaw;
end;

{ TCharSprite }

function TCharSprite.GetChar: Char;
begin
  Result := System.Char(SubTextureIndex);
end;

function TCharSprite.GetFont: string;
begin
  Result := Texture;
end;

function TCharSprite.GetFontTile: TTextureAtlas.TTile;
begin
  Result := TextureTile;
end;

function TCharSprite.GetWidth: Single;
begin
  Result := Aspect;
end;

function TCharSprite.GetWidthSpaced: Single;
begin
  Result := AspectPlus[FSpacePixels];
end;

procedure TCharSprite.SetChar(const Value: Char);
begin
  SubTextureIndex := Ord(Value);
end;

procedure TCharSprite.SetFont(const Value: string);
begin
  FontTile := SpriteSystem.SpriteAtlas[Value];
end;

procedure TCharSprite.SetFontTile(const Value: TTextureAtlas.TTile);
begin
  TextureTile := Value.SubTiles[SubTextureIndex];
end;

procedure TCharSprite.SpriteChanged(AInfo: TSprite.TChangeEventInfo);
begin
  if scTexture in AInfo.Changes then
  begin
    if TextureTile.SubTiles.Count <> 256 then
      raise ECharSpriteInvalidFont.Create;
    if (FadeTextureTile <> nil) and (FadeTextureTile.SubTiles.Count <> 256) then
      raise ECharSpriteInvalidFont.Create;
    if (FOldFontTile = nil) or not FOldFontTile.HasSubTiles or not TextureTile.HasSubTiles or
      (FOldFontTile.SubTiles[0] <> TextureTile.SubTiles[0]) then
    begin
      FOldFontTile := TextureTile;
      FSpacePixels := 0;
      if TextureTile.InfoValues <> nil then
        TextureTile.InfoValues.Get(TTextureAtlas.TTile.CharSpacingPixelsIdentifier, FSpacePixels);
    end;
  end;
end;

constructor TCharSprite.Create(ASpriteSystem: TSpriteSystem; ATextureTile: TTextureAtlas.TTile);
begin
  OnChanged.Add(SpriteChanged);
  inherited;
end;

{ ECharSpriteInvalidFont }

constructor ECharSpriteInvalidFont.Create;
begin
  inherited Create('The texture for the char sprite is not a valid font.');
end;

{ ESpriteRemovedAlready }

constructor ESpriteRemovedAlready.Create;
begin
  inherited Create('The sprite got removed already.');
end;

end.
