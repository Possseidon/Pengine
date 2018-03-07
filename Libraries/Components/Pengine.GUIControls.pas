unit Pengine.GUIControls;

interface

uses
  System.SysUtils,
  System.Math,

  Vcl.Controls,

  Pengine.EventHandling,
  Pengine.GUI,
  Pengine.Texture,
  Pengine.Collections,
  Pengine.SpriteSystem,
  Pengine.Utility,
  Pengine.IntMaths,
  Pengine.Vector,
  Pengine.TextureAtlas,
  Pengine.InputHandler;

type

  TLabel = class(TControl)
  public type

    TEventInfo = TSenderEventInfo<TLabel>;

    TEvent = TEvent<TEventInfo>;

    TCaptionChangeEventInfo = class(TEventInfo)
    private
      FOldCaption: string;

    public
      constructor Create(ASender: TLabel; AOldCaption: string);

      property OldCaption: string read FOldCaption;

    end;

    TCaptionChangeEvent = TEvent<TCaptionChangeEventInfo>;

    TCharSprites = TRefArray<TCharSprite>;

  private
    FCaption: string;
    FWidth: TOpt<Single>;
    FChars: TCharSprites;

    FOnCaptionChanged: TCaptionChangeEvent;

    procedure SetCaption(const Value: string);
    procedure GenCaption;
    function GetCaptionLength: Integer;
    function GetWidth: Single;

    procedure FontChanged(AInfo: TControl.TFontChangeEventInfo);
    procedure BoundsChanged(AInfo: TControl.TEventInfo);

    function GetOnCaptionChanged: TCaptionChangeEvent.TAccess;

  protected
    function GetAspect: Single; override;

  public
    constructor Create(AParent: TControl); override;
    destructor Destroy; override;

    property Caption: string read FCaption write SetCaption;
    property CaptionLength: Integer read GetCaptionLength;
    property Width: Single read GetWidth;

    property OnCaptionChanged: TCaptionChangeEvent.TAccess read GetOnCaptionChanged;

  end;

  EButtonTooSmall = class(Exception)
  public
    constructor Create;
  end;

  TButton = class(TControl)
  public type

    TEventInfo = TSenderEventInfo<TButton>;

    TTextureChangeEventInfo = class(TEventInfo)
    private
      FOldTextureTile: TTextureAtlas.TTile;

      function GetOldTexture: string;

    public
      constructor Create(AOldTextureTile: TTextureAtlas.TTile);

      property OldTexture: string read GetOldTexture;
      property OldTextureTile: TTextureAtlas.TTile read FOldTextureTile;

    end;

    TWidthChangeEventInfo = class(TEventInfo)
    private
      FOldWidth: Integer;

    public
      constructor Create(AOldWidth: Integer);

      property OldWidth: Integer read FOldWidth;

    end;

    TEvent = TEvent<TEventInfo>;

    TTextureChangeEvent = TEvent<TTextureChangeEventInfo>;

    TWidthChangeEvent = TEvent<TWidthChangeEventInfo>;

    TTextureIndex = (tiLeft, tiCenter, tiRight);
    TState = (bsDefault, bsHover, bsPressed, bsDisabled);

    TSegments = TRefArray<TSprite>;

  public const

    DefaultTextureName = 'button';
    TextureCount = Ord(High(TTextureIndex)) + 1;

  private
    FLabel: TLabel;
    FTextureTile: TTextureAtlas.TTile;
    FBorderSegments: array [TBasicDir1] of TSprite;
    FSegments: TSegments;
    FState: TState;
    FOnTextureChanged: TTextureChangeEvent;
    FOnWidthChanged: TWidthChangeEvent;
    FOnPressed: TEvent;
    FCaptionScale: Single;

    function GetButtonTexture(ATextureIndex: TTextureIndex): TTextureAtlas.TTile;
    
    procedure SetTextureTile(const Value: TTextureAtlas.TTile);
    function GetTexture: string;
    procedure SetTexture(const Value: string);
    procedure SetWidth(const Value: Integer);
    function GetWidth: Integer;
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    procedure SetCaptionScale(const Value: Single);
    procedure SetState(const Value: TState);

    procedure GenButton;

    function GetOnTextureChanged: TTextureChangeEvent.TAccess;
    function GetOnWidthChanged: TWidthChangeEvent.TAccess;
    function GetOnPressed: TEvent.TAccess;
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);

    function UpdateState: Boolean;
    procedure ScaleCaption;

    procedure LocationChanged;
    procedure AspectChanged;
    procedure MouseMoved;
    procedure MouseDown(AInfo: TButtonEventInfo);
    procedure MouseUp(AInfo: TButtonEventInfo);
    procedure MouseEnteredScreen;
    procedure MouseLeftScreen;

  protected
    function GetAspect: Single; override;

    property State: TState read FState write SetState;
    
  public
    constructor Create(AParent: TControl); override;
    destructor Destroy; override;

    property TextureTile: TTextureAtlas.TTile read FTextureTile write SetTextureTile;
    property Texture: string read GetTexture write SetTexture;

    property Width: Integer read GetWidth write SetWidth;
    property Caption: string read GetCaption write SetCaption;
    property CaptionScale: Single read FCaptionScale write SetCaptionScale;

    property Enabled: Boolean read GetEnabled write SetEnabled;
    
    property OnTextureChanged: TTextureChangeEvent.TAccess read GetOnTextureChanged;
    property OnWidthChanged: TWidthChangeEvent.TAccess read GetOnWidthChanged;
    property OnPressed: TEvent.TAccess read GetOnPressed;
    
  end;

  TPanel = class(TContainerControl)
  private

  public

  end;

implementation

{ TLabel }

procedure TLabel.BoundsChanged(AInfo: TControl.TEventInfo);
begin
  GenCaption;
end;

constructor TLabel.Create(AParent: TControl);
begin
  inherited;
  FChars := TCharSprites.Create;
  FWidth := TOpt<Single>.Create;
  OnFontChanged.Add(FontChanged);
  OnBoundsChanged.Add(BoundsChanged);
end;

destructor TLabel.Destroy;
begin
  FChars.Free;
  FWidth.Free;
  inherited;
end;

procedure TLabel.FontChanged(AInfo: TControl.TFontChangeEventInfo);
begin
  FWidth.Clear;
  NotifyAspectChanged;
end;

procedure TLabel.GenCaption;
var
  I: Integer;
  Offset: Single;
begin
  Offset := -Width / 2;
  for I := 0 to FChars.Count - 1 do
  begin
    FChars[I].BeginUpdate;
    FChars[I].Char := FCaption[I + 1];
    if I = 0 then
      Offset := Offset + FChars[I].Width / 2
    else
      Offset := Offset + FChars[I].WidthSpaced / 2;
    FChars[I].Location.PosX := Offset;
    Offset := Offset + FChars[I].WidthSpaced / 2;
    FChars[I].EndUpdate;
  end;
end;

function TLabel.GetAspect: Single;
begin
  Result := Width;
end;

function TLabel.GetCaptionLength: Integer;
begin
  Result := FChars.Count;
end;

function TLabel.GetOnCaptionChanged: TCaptionChangeEvent.TAccess;
begin
  Result := FOnCaptionChanged.Access;
end;

function TLabel.GetWidth: Single;
begin
  if not FWidth.HasValue then
    FWidth.Value := FontTile.TextWidth(Caption);
  Result := FWidth.Value;
end;

procedure TLabel.SetCaption(const Value: string);
var
  OldCaption: string;
begin
  if Caption = Value then
    Exit;
  OldCaption := Caption;
  FCaption := Value;
  FWidth.Clear;
  while CaptionLength > Length(Caption) do
  begin
    FChars.Last.Remove;
    FChars.DelLast;
  end;
  while CaptionLength < Length(Caption) do
    FChars.Add(AddChar);
  NotifyAspectChanged;
  FOnCaptionChanged.Execute(TCaptionChangeEventInfo.Create(Self, OldCaption));
end;

{ TLabel.TCaptionChangeEventInfo }

constructor TLabel.TCaptionChangeEventInfo.Create(ASender: TLabel; AOldCaption: string);
begin
  inherited Create(ASender);
  FOldCaption := AOldCaption;
end;

{ TButton }

procedure TButton.AspectChanged;
begin
  ScaleCaption;
end;

constructor TButton.Create(AParent: TControl);
var
  Side: TBasicDir1;
begin
  inherited;
  TextureTile := TextureAtlas[DefaultTextureName];
  FSegments := TSegments.Create;
  for Side := Low(TBasicDir1) to High(TBasicDir1) do
  begin
    FBorderSegments[Side] := Add<TSprite>(TextureTile);
    FBorderSegments[Side].Location.Parent := Location;
  end;
  Width := 2;
  OnAspectChanged.Add(AspectChanged);
  Location.OnChanged.Add(LocationChanged);
  Input.OnMouseMove.Add(MouseMoved);
  Input.OnButtonDown.Add(MouseDown);
  Input.OnButtonUp.Add(MouseUp);
  Input.OnEnterScreen.Add(MouseEnteredScreen);
  Input.OnLeaveScreen.Add(MouseLeftScreen);
  FLabel := TLabel.Create(Self);
  FLabel.OnAspectChanged.Add(ScaleCaption);
  FCaptionScale := 0.75;
end;

destructor TButton.Destroy;
begin
  FLabel.Free;
  FSegments.Free;
  inherited;
end;

procedure TButton.GenButton;
var
  I: Integer;
begin
  FBorderSegments[bdLeft].Location.PosX := (GetButtonTexture(tiLeft).Aspect - Aspect) / 2;
  FBorderSegments[bdLeft].TextureTile := GetButtonTexture(tiLeft);
  FBorderSegments[bdRight].Location.PosX := (Aspect - GetButtonTexture(tiRight).Aspect) / 2;
  FBorderSegments[bdRight].TextureTile := GetButtonTexture(tiRight);
  for I := 0 to FSegments.MaxIndex do
  begin
    FSegments[I].TextureTile := GetButtonTexture(tiCenter);
    FSegments[I].Location.PosX :=
      GetButtonTexture(tiLeft).Aspect - Aspect / 2 +
      (I + 0.5) * GetButtonTexture(tiCenter).Aspect;
  end;
end;

function TButton.GetAspect: Single;
var
  Tiles: TArray<TTextureAtlas.TTile>.TReader;
begin
  Tiles := TextureTile.SubTiles;
  Result := Tiles[Ord(tiLeft)].Aspect + Tiles[Ord(tiCenter)].Aspect * (Width - 1) + Tiles[Ord(tiRight)].Aspect;
end;

function TButton.GetOnPressed: TEvent.TAccess;
begin
  Result := FOnPressed.Access;
end;

function TButton.GetOnTextureChanged: TTextureChangeEvent.TAccess;
begin
  Result := FOnTextureChanged.Access;
end;

function TButton.GetOnWidthChanged: TWidthChangeEvent.TAccess;
begin
  Result := FOnWidthChanged.Access;
end;

function TButton.GetTexture: string;
begin
  Result := FTextureTile.Name;
end;

function TButton.GetButtonTexture(ATextureIndex: TTextureIndex): TTextureAtlas.TTile;
begin
  Result := TextureTile.SubTiles[Ord(ATextureIndex) + TextureCount * Ord(State)];
end;

function TButton.GetCaption: string;
begin
  Result := FLabel.Caption;
end;

function TButton.GetEnabled: Boolean;
begin
  Result := State <> bsDisabled;
end;

function TButton.GetWidth: Integer;
begin
  Result := FSegments.Count + 1;
end;

procedure TButton.LocationChanged;
begin
  UpdateState;
end;

procedure TButton.MouseDown(AInfo: TButtonEventInfo);
begin
  if AInfo.Button <> mbLeft then
    Exit;
  if State = bsHover then
    State := bsPressed;
end;

procedure TButton.MouseEnteredScreen;
begin
  UpdateState;
end;

procedure TButton.MouseLeftScreen;
begin
  UpdateState;
end;

procedure TButton.MouseMoved;
begin
  UpdateState;
end;

procedure TButton.MouseUp(AInfo: TButtonEventInfo);
begin
  if AInfo.Button <> mbLeft then
    Exit;
  if State = bsPressed then
  begin
    if MouseHovered then
    begin
      State := bsHover;
      FOnPressed.Execute(TEventInfo.Create(Self));
    end
    else
      State := bsDefault;
  end;
end;

procedure TButton.ScaleCaption;
begin
  if FLabel.Aspect = 0 then
    FLabel.Location.Scale := CaptionScale
  else
    FLabel.Location.Scale := Min(CaptionScale, (Aspect - 1 + CaptionScale) / FLabel.Aspect);
end;

procedure TButton.SetCaption(const Value: string);
begin
  if Caption = Value then
    Exit;
  FLabel.Caption := Value;
  ScaleCaption;
end;

procedure TButton.SetCaptionScale(const Value: Single);
begin
  if CaptionScale = Value then
    Exit;
  FCaptionScale := Value;
  ScaleCaption;
end;

procedure TButton.SetEnabled(const Value: Boolean);
begin
  if Enabled = Value then
    Exit;
  if Value then
  begin
    if MouseHovered then
      State := bsHover
    else
      State := bsDefault;
  end
  else
    State := bsDisabled;
end;

procedure TButton.SetState(const Value: TState);
begin
  if State = Value then
    Exit;
  FState := Value;
  GenButton;
end;

procedure TButton.SetTexture(const Value: string);
begin
  TextureTile := TextureAtlas[Value];
end;

procedure TButton.SetTextureTile(const Value: TTextureAtlas.TTile);
begin
  FTextureTile := Value;
end;

procedure TButton.SetWidth(const Value: Integer);
var
  OldWidth: Integer;
begin
  if Value <= 0 then
    raise EButtonTooSmall.Create;
  if Width = Value then
    Exit;
  OldWidth := Width;
  while Width > Value do
  begin
    FSegments.Last.Remove;
    FSegments.DelLast;
  end;
  while Width < Value do
    FSegments.Add(Add<TSprite>(TextureTile));
  if not UpdateState then
    GenButton;
  NotifyAspectChanged;
  FOnWidthChanged.Execute(TWidthChangeEventInfo.Create(OldWidth));
end;

function TButton.UpdateState: Boolean;
begin
  if State in [bsPressed, bsDisabled] then
    Exit(False);
  if MouseHovered then
  begin
    Result := State <> bsHover;
    State := bsHover;
  end
  else
  begin
    Result := State <> bsDefault;
    State := bsDefault;
  end;
end;

{ TButton.TTextureChangeEventInfo }

constructor TButton.TTextureChangeEventInfo.Create(AOldTextureTile: TTextureAtlas.TTile);
begin
  FOldTextureTile := AOldTextureTile;
end;

function TButton.TTextureChangeEventInfo.GetOldTexture: string;
begin
  Result := OldTextureTile.Name;
end;

{ TButton.TWidthChangeEventInfo }

constructor TButton.TWidthChangeEventInfo.Create(AOldWidth: Integer);
begin
  FOldWidth := AOldWidth;
end;

{ EButtonTooSmall }

constructor EButtonTooSmall.Create;
begin
  inherited Create('The button width must be at least 1.');
end;

end.
