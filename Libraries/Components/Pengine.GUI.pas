unit Pengine.GUI;

interface

uses
  System.SysUtils,

  Pengine.EventHandling,
  Pengine.SpriteSystem,
  Pengine.GLGame,
  Pengine.GLProgram,
  Pengine.TextureAtlas,
  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.Hasher,
  Pengine.Vector,
  Pengine.Color,
  Pengine.Utility;

type
 
  EGUIFontMissing = class(Exception)
  public
    constructor Create;
  end;

  EGUIFontColorMissing = class(Exception)
  public
    constructor Create;
  end;

  TGUI = class;
  TContainerControl = class;

  TOriginX = (oxLeft, oxCenter, oxRight);
  TOriginY = (oyBottom, oyCenter, oyTop);

  TOrigin = record
    X: TOriginX;
    Y: TOriginY;

    constructor Create(X: TOriginX; Y: TOriginY);

    class operator Equal(const A, B: TOrigin): Boolean; static;
    class operator NotEqual(const A, B: TOrigin): Boolean; static;
  end;

  TControl = class abstract
  public type

    TEventInfo = TSenderEventInfo<TControl>;
    TEvent = TEvent<TEventInfo>;

    TFontChangeEventInfo = class(TEventInfo)
    private
      FOldFontTile: TTextureAtlas.TTile;

      function GetOldFont: string;

    public
      constructor Create(ASender: TControl; AOldFontTile: TTextureAtlas.TTile);

      property OldFontTile: TTextureAtlas.TTile read FOldFontTile;
      property OldFont: string read GetOldFont;

    end;

    TFontChangeEvent = TEvent<TFontChangeEventInfo>;

    TFontColorChangeEventInfo = class(TEventInfo)
    private
      FOldColor: TOpt<TColorRGBA>;

      function GetOldColor: TOpt<TColorRGBA>.TReader;

    public
      constructor Create(ASender: TControl); overload;
      constructor Create(ASender: TControl; AOldColor: TColorRGBA); overload;
      constructor Create(ASender: TControl; AOldColor: TOpt<TColorRGBA>); overload;
      destructor Destroy; override;
      
      property OldColor: TOpt<TColorRGBA>.TReader read GetOldColor;

    end;

    TFontColorChangeEvent = TEvent<TFontColorChangeEventInfo>;

    TOriginChangeEventInfo = class(TEventInfo)
    private
      FOldOrigin: TOrigin;

    public
      constructor Create(ASender: TControl; AOldOrigin: TOrigin);

      property OldOrigin: TOrigin read FOldOrigin;

    end;

    TOriginChangeEvent = TEvent<TOriginChangeEventInfo>;

    TSprites = TRefArray<TSprite>;
    TCharSprites = TRefArray<TCharSprite>;

  private
    FGUI: TGUI;
    FParent: TContainerControl;
    FSprites: TSprites;
    FCharSprites: TCharSprites;
    FFontTile: TTextureAtlas.TTile;
    FFontColor: TOpt<TColorRGBA>;
    FLocation: TLocation2;
    FOrigin: TOrigin;
    FOnFontChanged: TFontChangeEvent;
    FOnFontColorChanged: TFontColorChangeEvent;
    FOnOriginChanged: TOriginChangeEvent;
    FOnAspectChanged: TEvent;
    FOnBoundsChanged: TEvent;

    function GetSpriteSystem: TSpriteSystem;

    function GetFontTile: TTextureAtlas.TTile;
    procedure SetFontTile(const Value: TTextureAtlas.TTile);
    function GetFont: string;
    procedure SetFont(const Value: string);
    function GetFontColor: TColorRGBA;
    procedure SetFontColor(const Value: TColorRGBA);

    procedure SetOrigin(const Value: TOrigin);
    procedure SetOriginX(const Value: TOriginX);
    procedure SetOriginY(const Value: TOriginY);
    function GetBoundsCentered: TAxisSystem2;
    function GetBounds: TAxisSystem2;

    procedure SpriteRemoved(AInfo: TSprite.TEventInfo);
    procedure CharRemoved(AInfo: TSprite.TEventInfo);

    procedure ParentFontChanged(AInfo: TFontChangeEventInfo);
    procedure ParentFontColorChanged(AInfo: TFontColorChangeEventInfo);
    procedure ParentBoundsChanged(AInfo: TEventInfo);

    procedure SelfOriginChanged(AInfo: TOriginChangeEventInfo);
    procedure SelfAspectChanged(AInfo: TEventInfo);
    procedure SelfLocationChanged(AInfo: TLocation2.TChangeEventInfo);

    function GetOnFontChanged: TFontChangeEvent.TAccess;
    function GetOnFontColorChanged: TFontColorChangeEvent.TAccess;
    function GetOnOriginChanged: TOriginChangeEvent.TAccess;
    function GetOnLocationChanged: TLocation2.TChangeEvent.TAccess;
    function GetOnAspectChanged: TEvent.TAccess;
    function GetOnBoundsChanged: TEvent.TAccess;

  protected
    property SpriteSystem: TSpriteSystem read GetSpriteSystem;

    function Add<T: TSprite>(ATexture: string): T; overload;
    function Add<T: TSprite>(ATexture: TTextureAtlas.TTile): T; overload;
    function AddChar: TCharSprite;

    procedure AspectChanged;
    function GetAspect: Single; virtual; abstract;

    class procedure AddRequiredTextures(ARequiredTextures: TArray<string>); virtual;
    
  public
    constructor Create(AParent: TContainerControl); virtual;
    destructor Destroy; override;

    class function RequiredTextures: TArray<string>;

    property GUI: TGUI read FGUI;
    property Parent: TContainerControl read FParent;

    property FontTile: TTextureAtlas.TTile read GetFontTile write SetFontTile;
    property Font: string read GetFont write SetFont;
    procedure UseParentFont;

    property FontColor: TColorRGBA read GetFontColor write SetFontColor;
    procedure UseParentFontColor;

    property Location: TLocation2 read FLocation;

    property Origin: TOrigin read FOrigin write SetOrigin;
    property OriginX: TOriginX read FOrigin.X write SetOriginX;
    property OriginY: TOriginY read FOrigin.Y write SetOriginY;
    property Aspect: Single read GetAspect;
    property BoundsCentered: TAxisSystem2 read GetBoundsCentered;
    property Bounds: TAxisSystem2 read GetBounds;

    property OnFontChanged: TFontChangeEvent.TAccess read GetOnFontChanged;
    property OnFontColorChanged: TFontColorChangeEvent.TAccess read GetOnFontColorChanged;
    property OnLocationChanged: TLocation2.TChangeEvent.TAccess read GetOnLocationChanged;
    property OnOriginChanged: TOriginChangeEvent.TAccess read GetOnOriginChanged;
    property OnAspectChanged: TEvent.TAccess read GetOnAspectChanged;
    property OnBoundsChanged: TEvent.TAccess read GetOnBoundsChanged;

  end;

  TControlClass = class of TControl;

  TControls = TRefArray<TControl>;

  TContainerControl = class abstract(TControl)
  private
    FControls: TControls;

    function GetControls: TControls.TReader;

  public
    constructor Create(AParent: TContainerControl); override; 
    destructor Destroy; override;

    function Add<T: TControl>: T; overload;
    function Add(AControlClass: TControlClass): TControl; overload;

    property Controls: TControls.TReader read GetControls;

  end;

  TGUI = class(TContainerControl)
  private
    FSpriteSystem: TSpriteSystem;
    FControlClasses: TClassSet;

    function GetTextureAtlas: TTextureAtlas;
    function GetGame: TGLGame;

  protected
    function GetAspect: Single; override;

  public
    constructor Create(AGLGame: TGLGame; AGLProgram: TGLProgram); reintroduce;
    destructor Destroy; override;

    property Game: TGLGame read GetGame;

    property Textures: TTextureAtlas read GetTextureAtlas;

    procedure LoadControlClass(AControlClass: TControlClass);
    
  end;

implementation

{ TControl }

class procedure TControl.AddRequiredTextures(ARequiredTextures: TArray<string>);
begin
  // nothing
end;

procedure TControl.AspectChanged;
begin
  FOnAspectChanged.Execute(TEventInfo.Create(Self));
end;

function TControl.Add<T>(ATexture: string): T;
begin
  Result := Add<T>(SpriteSystem.SpriteAtlas[ATexture]);
end;

function TControl.Add<T>(ATexture: TTextureAtlas.TTile): T;
begin
  Result := SpriteSystem.Add<T>(ATexture);
  FSprites.Add(Result);
  Result.OnRemove.Add(SpriteRemoved);
end;

function TControl.AddChar: TCharSprite;
begin
  Result := FCharSprites.Add(SpriteSystem.Add<TCharSprite>(Font));
  Result.Color := FontColor;
  Result.OnRemove.Add(CharRemoved);
end;

procedure TControl.CharRemoved(AInfo: TSprite.TEventInfo);
begin
  FCharSprites.Del(TCharSprite(AInfo.Sender));
end;

constructor TControl.Create(AParent: TContainerControl);
begin
  FLocation := TLocation2.Create;
  if AParent <> nil then
  begin
    FGUI := AParent.GUI;
    FParent := AParent;
    FParent.OnFontChanged.Add(ParentFontChanged);
    FParent.OnFontColorChanged.Add(ParentFontColorChanged);
    FParent.OnBoundsChanged.Add(ParentBoundsChanged);
    FLocation.Parent := Parent.Location;
  end;
  FSprites := TSprites.Create;
  FCharSprites := TCharSprites.Create;
  FFontColor := TOpt<TColorRGBA>.Create;
  FOrigin := TOrigin.Create(oxCenter, oyCenter);
  OnOriginChanged.Add(SelfOriginChanged);
  OnAspectChanged.Add(SelfAspectChanged);
  OnLocationChanged.Add(SelfLocationChanged);
end;

destructor TControl.Destroy;
begin
  FFontColor.Free;
  FCharSprites.Free;
  FSprites.Free;
  FLocation.Free;
  if FParent <> nil then
    FParent.FControls.Del(Self);
  inherited;
end;

function TControl.GetBounds: TAxisSystem2;
begin
  Result := BoundsCentered;
  Result.S := Result.S - Result.DX - Result.DY;
  Result.DX := Result.DX * 2;
  Result.DY := Result.DY * 2;
end;

function TControl.GetBoundsCentered: TAxisSystem2;
begin
  Result := Location.AxisSystem;
  Result.DX := Result.DX * Aspect / 2;
  Result.DY := Result.DY / 2;

  if Parent <> nil then
  begin
    case OriginX of
      oxLeft:
        Result.S := Result.S + Parent.BoundsCentered.DX * (Aspect / Parent.Aspect - 2 / Location.ScaleX);
      oxRight:
        Result.S := Result.S + Parent.BoundsCentered.DX * (2 / Location.ScaleX - Aspect / Parent.Aspect);
    end;

    case OriginY of
      oyBottom:
        Result.S := Result.S + Parent.BoundsCentered.DY * (1 - 2 / Location.ScaleY);
      oyTop:
        Result.S := Result.S + Parent.BoundsCentered.DY * (2 / Location.ScaleY - 1);
    end;
  end;
end;

function TControl.GetFont: string;
begin
  Result := FontTile.Name;
end;

function TControl.GetFontColor: TColorRGBA;
begin
  if FFontColor.HasValue then
    Exit(FFontColor.Value);
  if Parent <> nil then
    Exit(Parent.FontColor);
  raise EGUIFontColorMissing.Create;
end;

function TControl.GetFontTile: TTextureAtlas.TTile;
begin
  if FFontTile <> nil then
    Exit(FFontTile);
  if Parent <> nil then
    Exit(Parent.FontTile);
  raise EGUIFontMissing.Create;
end;

function TControl.GetOnAspectChanged: TEvent.TAccess;
begin
  Result := FOnAspectChanged.Access;
end;

function TControl.GetOnBoundsChanged: TEvent.TAccess;
begin
  Result := FOnBoundsChanged.Access;
end;

function TControl.GetOnFontChanged: TFontChangeEvent.TAccess;
begin
  Result := FOnFontChanged.Access;
end;

function TControl.GetOnFontColorChanged: TFontColorChangeEvent.TAccess;
begin
  Result := FOnFontColorChanged.Access;
end;

function TControl.GetOnLocationChanged: TLocation2.TChangeEvent.TAccess;
begin
  Result := Location.OnChanged;
end;

function TControl.GetOnOriginChanged: TOriginChangeEvent.TAccess;
begin
  Result := FOnOriginChanged.Access;
end;

function TControl.GetSpriteSystem: TSpriteSystem;
begin
  Result := FGUI.FSpriteSystem;
end;

procedure TControl.ParentBoundsChanged(AInfo: TEventInfo);
begin
  FOnBoundsChanged.Execute(AInfo, False);
end;

procedure TControl.ParentFontChanged(AInfo: TFontChangeEventInfo);
var
  CharSprite: TCharSprite;
begin
  for CharSprite in FCharSprites do
    CharSprite.FontTile := FontTile;
  FOnFontChanged.Execute(AInfo, False);
end;

procedure TControl.ParentFontColorChanged(AInfo: TFontColorChangeEventInfo);
var
  CharSprite: TCharSprite;
begin
  for CharSprite in FCharSprites do
    CharSprite.Color := FontColor;
  FOnFontColorChanged.Execute(AInfo, False);  
end;

class function TControl.RequiredTextures: TArray<string>;
begin
  Result := TArray<string>.Create;
  AddRequiredTextures(Result);
end;

procedure TControl.SelfAspectChanged(AInfo: TEventInfo);
begin
  FOnBoundsChanged.Execute(AInfo, False);
end;

procedure TControl.SelfLocationChanged(AInfo: TLocation2.TChangeEventInfo);
begin
  FOnBoundsChanged.Execute(TEventInfo.Create(Self));
end;

procedure TControl.SelfOriginChanged(AInfo: TOriginChangeEventInfo);
begin
  FOnBoundsChanged.Execute(TEventInfo.Create(Self));
end;

procedure TControl.SetFont(const Value: string);
begin
  FontTile := GUI.Textures[Value];
end;

procedure TControl.SetFontColor(const Value: TColorRGBA);
var
  CharSprite: TCharSprite;
  OldColor: TOpt<TColorRGBA>;
begin
  if FFontColor.HasValue and (FFontColor.Value = Value) then
    Exit;
  if (Parent <> nil) and not FFontColor.HasValue then
    Parent.OnFontColorChanged.Del(ParentFontColorChanged);
  OldColor := FFontColor.Copy;
  FFontColor.Value := Value;
  for CharSprite in FCharSprites do
    CharSprite.Color := FontColor;
  FOnFontColorChanged.Execute(TFontColorChangeEventInfo.Create(Self, OldColor));
  OldColor.Free;
end;

procedure TControl.SetFontTile(const Value: TTextureAtlas.TTile);
var
  CharSprite: TCharSprite;
  OldFont: TTextureAtlas.TTile;
begin
  if FFontTile = Value then
    Exit;
  if Parent <> nil then
  begin
    if (FontTile = nil) and (Value <> nil) then
      Parent.OnFontChanged.Del(ParentFontChanged)
    else if (FontTile <> nil) and (Value = nil) then
      Parent.OnFontChanged.Add(ParentFontChanged);
  end;
  OldFont := FFontTile;
  FFontTile := Value;
  for CharSprite in FCharSprites do
    CharSprite.FontTile := FontTile;
  FOnFontChanged.Execute(TFontChangeEventInfo.Create(Self, OldFont));
end;

procedure TControl.SetOrigin(const Value: TOrigin);
var
  OldOrigin: TOrigin;
begin
  if Origin = Value then
    Exit;
  OldOrigin := Origin;
  FOrigin := Value;
  FOnOriginChanged.Execute(TOriginChangeEventInfo.Create(Self, OldOrigin));
end;

procedure TControl.SetOriginX(const Value: TOriginX);
var
  OldOrigin: TOrigin;
begin
  if OriginX = Value then
    Exit;
  OldOrigin := Origin;
  FOrigin.X := Value;
  FOnOriginChanged.Execute(TOriginChangeEventInfo.Create(Self, OldOrigin));
end;

procedure TControl.SetOriginY(const Value: TOriginY);
var
  OldOrigin: TOrigin;
begin
  if OriginY = Value then
    Exit;
  OldOrigin := Origin;
  FOrigin.Y := Value;
  FOnOriginChanged.Execute(TOriginChangeEventInfo.Create(Self, OldOrigin));
end;

procedure TControl.SpriteRemoved(AInfo: TSprite.TEventInfo);
begin
  FSprites.Del(AInfo.Sender);
end;

procedure TControl.UseParentFont;
begin
  FontTile := nil;
end;

procedure TControl.UseParentFontColor;
var
  OldFontColor: TColorRGBA;
  EventInfo: TFontColorChangeEventInfo;
  CharSprite: TCharSprite;
begin
  if not FFontColor.HasValue then
    Exit;
  if Parent <> nil then
    Parent.OnFontColorChanged.Add(ParentFontColorChanged);
  OldFontColor := FontColor;
  FFontColor.Clear;
  for CharSprite in FCharSprites do
    CharSprite.Color := FontColor;
  if OldFontColor <> FontColor then
    FOnFontColorChanged.Execute(TFontColorChangeEventInfo.Create(Self, OldFontColor));
end;

{ TContainerControl }

function TContainerControl.GetControls: TControls.TReader;
begin
  Result := FControls.Reader;
end;

destructor TContainerControl.Destroy;
var
  Control: TControl;
begin
  for Control in FControls.InReverse do
    Control.Free;
  FControls.Free;
  inherited;
end;

function TContainerControl.Add(AControlClass: TControlClass): TControl;
begin
  Result := FControls.Add(AControlClass.Create(Self));
end;

function TContainerControl.Add<T>: T;
begin
  Result := T(FControls.Add(T.Create(Self)));
end;

constructor TContainerControl.Create(AParent: TContainerControl);
begin
  inherited;
  FControls := TControls.Create;
end;

{ TGUI }

function TGUI.GetAspect: Single;
begin
  Result := Game.Aspect;
end;

function TGUI.GetGame: TGLGame;
begin
  Result := SpriteSystem.Game;
end;

function TGUI.GetTextureAtlas: TTextureAtlas;
begin
  Result := SpriteSystem.SpriteAtlas;
end;

constructor TGUI.Create;
begin
  inherited Create(nil);
  FControlClasses := TClassSet.Create;
  FSpriteSystem := TSpriteSystem.Create(AGLGame, AGLProgram);
  FGUI := Self;
  FontColor := ColorWhite;
  Game.OnResize.Add(AspectChanged);
end;

destructor TGUI.Destroy;
begin
  FSpriteSystem.Free;
  FControlClasses.Free;
  inherited;
end;

procedure TGUI.LoadControlClass(AControlClass: TControlClass);
begin
  if not FControlClasses[AControlClass] then
  begin
    ;
  end;
end;

{ TControl.TFontChangeEventInfo }

constructor TControl.TFontChangeEventInfo.Create(ASender: TControl; AOldFontTile: TTextureAtlas.TTile);
begin
  inherited Create(ASender);
  FOldFontTile := AOldFontTile;
end;

function TControl.TFontChangeEventInfo.GetOldFont: string;
begin
  Result := FOldFontTile.Name;
end;

{ TControl.TFontColorChangeEventInfo }

constructor TControl.TFontColorChangeEventInfo.Create(ASender: TControl; AOldColor: TOpt<TColorRGBA>);
begin
  inherited Create(ASender);  
  FOldColor := AOldColor.Copy;
end;

constructor TControl.TFontColorChangeEventInfo.Create(ASender: TControl);
begin                           
  inherited Create(ASender);  
  FOldColor := TOpt<TColorRGBA>.Create;
end;

constructor TControl.TFontColorChangeEventInfo.Create(ASender: TControl; AOldColor: TColorRGBA);
begin
  inherited Create(ASender);  
  FOldColor := TOpt<TColorRGBA>.Create(AOldColor);
end;

destructor TControl.TFontColorChangeEventInfo.Destroy;
begin
  FOldColor.Free;
  inherited;
end;

function TControl.TFontColorChangeEventInfo.GetOldColor: TOpt<TColorRGBA>.TReader;
begin
  Result := FOldColor.Reader;
end;

{ EGUIFontMissing }

constructor EGUIFontMissing.Create;
begin
  inherited Create('GUI does not have a font.');
end;

{ EGUIFontColorMissing }

constructor EGUIFontColorMissing.Create;
begin
  inherited Create('GUI does not have a font color.');
end;

{ TControl.TOriginChangeEventInfo }

constructor TControl.TOriginChangeEventInfo.Create(ASender: TControl; AOldOrigin: TOrigin);
begin
  inherited Create(ASender);
  FOldOrigin := AOldOrigin;
end;

{ TOrigin }

constructor TOrigin.Create(X: TOriginX; Y: TOriginY);
begin
  Self.X := X;
  Self.Y := Y;
end;

class operator TOrigin.Equal(const A, B: TOrigin): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

class operator TOrigin.NotEqual(const A, B: TOrigin): Boolean;
begin
  Result := (A.X <> B.X) or (A.Y <> B.Y);
end;

end.

