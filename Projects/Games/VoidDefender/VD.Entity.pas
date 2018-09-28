unit VD.Entity;

interface

uses
  System.SysUtils,
  System.Math,

  Pengine.Collections,
  Pengine.ObservableCollections,
  Pengine.Vector,
  Pengine.EventHandling,
  Pengine.TextureAtlas,
  Pengine.SpriteSystem,
  Pengine.GLGame,
  Pengine.GLProgram,
  Pengine.HashCollections;

type

  THealth = Single;
  TPercentage = Single;

  EHealthBarError = class(Exception);

  EEntityError = class(Exception);

  /// <summary>Contains a current health value, a maximum health and an event, to listen for changes.</summary>
  THealthBar = class
  public type

    TEventInfo = TSenderEventInfo<THealthBar>;

    TEvent = TEvent<TEventInfo>;

    THealthChangeEventInfo = class(TEventInfo)
    private
      FOldHealth: THealth;

      function GetHealthDifference: THealth;

    public
      constructor Create(ASender: THealthBar; AOldHealth: THealth);

      property OldHealth: THealth read FOldHealth;
      property HealthDifference: THealth read GetHealthDifference;

      function Damaged: Boolean;
      function Repaired: Boolean;

    end;

    THealthChangeEvent = TEvent<THealthChangeEventInfo>;

  private
    FMax: THealth;
    FCurrent: THealth;
    FOnHealthChange: THealthChangeEvent;
    FOnMaxChange: TEvent;

    procedure SetMax(const Value: THealth);
    procedure SetCurrent(Value: THealth);
    function GetPercentage: TPercentage;
    procedure SetPercentage(const Value: TPercentage);

  public
    constructor Create(AMax: THealth = 1);

    /// <summary>The maximum possible health.</summary>
    /// <remarks>Setting Max, will keep the current health percentage.</remarks>
    property Max: THealth read FMax write SetMax;
    /// <summary>The current health.</summary>
    property Current: THealth read FCurrent write SetCurrent;
    /// <summary>The health percentage relative to max.</summary>
    property Percentage: TPercentage read GetPercentage write SetPercentage;

    /// <returns>Wether the current health is zero.</returns>
    function Empty: Boolean;
    /// <summary>Wether the current health is at max.</summary>
    function Full: Boolean;

    /// <summary>Decreses the health by the given amount.</summary>
    procedure Damage(AAmount: THealth);
    /// <summary>Increases the health by the given amount.</summary>
    procedure Repair(AAmount: THealth);

    /// <summary>Called, whenever the health changes.</summary>
    function OnHealthChange: THealthChangeEvent.TAccess;
    /// <summary>Called, when the maximum health changes.</summary>
    function OnMaxChange: TEvent.TAccess;
    // TODO: OnPercentageChange

    function Copy: THealthBar;

  end;

  EStructureError = class(Exception);

  TEntitySystem = class;

  TEntityBase = class
  public type

    TPart = class
    public type

      TConnections = TRefSet<TPart>;

      TDamageTexture = class
      public type

        // TODO: OnHealthChange
        // TODO: OnTextureChange

      private
        FHealth: TPercentage;
        FTexture: TTexTile;

        procedure SetHealth(const Value: THealth);
        procedure SetTexture(const Value: TTexTile);

      public
        constructor Create(AHealth: THealth; ATexture: TTexTile);

        /// <summary>The texture is applied when the health is less or equal to this percentage.</summary>
        property Health: TPercentage read FHealth { write SetHealth TODO: OnHealthChange to sort in entity };
        /// <summary>The texture to apply, once a low enough percentage is reached.</summary>
        property Texture: TTexTile read FTexture write SetTexture;

        /// <summary>Creates a copy of this object.</summary>
        function Copy: TDamageTexture;

      end;

      TDamageTextures = TObservableObjectArray<TDamageTexture>;

    private
      FEntity: TEntityBase;
      FParent: TPart;
      FSprite: TSprite;
      FName: string;
      FConnections: TConnections;
      FLocation: TLocation2;
      FHealth: THealthBar;
      FDefaultTexture: TTexTile;
      FDamageTextures: TDamageTextures;

      function GetIndex: Integer;
      procedure SetIndex(const Value: Integer);
      procedure SetName(const Value: string);
      procedure SetParent(const Value: TPart);

      function GetConnections: TConnections.TReader;

      function GetTexture: TTexTile;
      procedure SetDefaultTexture(const Value: TTexTile);
      function GetDamageTexture(AHealth: THealth): TTexTile;
      procedure SetDamageTexture(AHealth: THealth; const Value: TTexTile);
      function GetDamageTextures: TDamageTextures.TReader;

    public
      constructor Create(AEntity: TEntityBase);
      destructor Destroy; override;

      /// <summary>The parent entity.</summary>
      property Entity: TEntityBase read FEntity;
      /// <summary>The index of this part in the parent template.</summary>
      property Index: Integer read GetIndex write SetIndex;

      /// <summary>If activated, contains the part sprite, otherwise nil.</summary>
      property Sprite: TSprite read FSprite;

      /// <summary>The name of the part, used in scripting.</summary>
      property Name: string read FName write SetName;

      /// <summary>Optional Parent used as parent for location.</summary>
      /// <remarks>Creates a connection between the parts, which cannot be disconnected.</remarks>
      property Parent: TPart read FParent write SetParent;
      /// <summary>Resets the parent to nil.</summary>
      procedure ResetParent;

      /// <summary>A read-only list of direct connections to other parts.</summary>
      property Connections: TConnections.TReader read GetConnections;
      /// <summary>Connects the part to another part.</summary>
      procedure Connect(APart: TPart);
      /// <summary>Disconnects the part from another part.</summary>
      procedure Disconnect(APart: TPart);
      /// <summary>Removes all connections of this part.</summary>
      procedure DisconnectAll;

      /// <returns>Wether there exists a path to a given part over the connections of all parts.</returns>
      function IsConnectedTo(APart: TPart): Boolean;
      /// <returns>Wether there exists a path over the connections of all parts to the main part.</returns>
      function IsConnectedToMainPart: Boolean;

      /// <summary>The location offset of this part.</summary>
      property Location: TLocation2 read FLocation;
      /// <summary>The health of this part.</summary>
      property Health: THealthBar read FHealth;
      /// <summary>The current texture of this part.</summary>
      property Texture: TTexTile read GetTexture;

      /// <summary>The texture, used until enough damage for the first damage texture (if any) is reached.</summary>
      property DefaultTexture: TTexTile read FDefaultTexture write SetDefaultTexture;
      /// <summary>Get or change textures for specific health percentages.</summary>
      /// <remarks>The getter returns the texture to display at any health, while the setter can add new textures.</remarks>
      property Textures[AHealth: THealth]: TTexTile read GetDamageTexture write SetDamageTexture;
      property DamageTextures: TDamageTextures.TReader read GetDamageTextures;

      /// <summary>Copy all values from other part.</summary>
      procedure Assign(AFrom: TPart); virtual;

      /// <summary>Adds the part into a sprite system.</summary>
      procedure ActivateSprite(ASpriteSystem: TSpriteSystem);

    end;

    TParts = TObservableObjectArray<TPart>;

  private
    FParts: TParts;
    FMainPart: TPart;
    FBaseTransformation: TLocation2;

    function GetPartIndex(APart: TPart): Integer;
    procedure SetMainPart(const Value: TPart);
    procedure SetPartIndex(APart: TPart; const Value: Integer);

  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>A list of all parts.</summary>
    property Parts: TParts read FParts;
    /// <summary>The main part of this structure.</summary>
    property MainPart: TPart read FMainPart write SetMainPart;

    /// <summary>Mainly used to scale down the object, but can also be used for other transformations.</summary>
    property BaseTransformation: TLocation2 read FBaseTransformation;

    /// <summary>Adds a new part to the structure.</summary>
    function AddPart: TPart; virtual;
    /// <summary>Find the index of a certain part or change it.</summary>
    property PartIndex[APart: TPart]: Integer read GetPartIndex write SetPartIndex;

    /// <summary>Assigns all values from the given entity.</summary>
    procedure Assign(AFrom: TEntityBase);

    /// <summary>Adds all parts into a sprite system.</summary>
    procedure ActivateSprites(ASpriteSystem: TSpriteSystem);

  end;

  TStructure = class(TEntityBase)
  public
    procedure MakeEntity(AEntity: TEntityBase);

  end;

  TEntity = class(TEntityBase)
  private
    FEntitySystem: TEntitySystem;
    FLocation: TLocation2;
    FVelocity: TVector2;

    function GetSpriteSystem: TSpriteSystem;

  public
    constructor Create(AEntitySystem: TEntitySystem; AStructure: TStructure);
    destructor Destroy; override;

    procedure Remove;

    property EntitySystem: TEntitySystem read FEntitySystem;
    property SpriteSystem: TSpriteSystem read GetSpriteSystem;

    property Location: TLocation2 read FLocation;
    property Velocity: TVector2 read FVelocity write FVelocity;

  end;

  TEntitySystem = class
  public type

    TEntities = TObjectArray<TEntity>;

  private
    FGame: TGLGame;
    FSpriteSystem: TSpriteSystem;
    FEntities: TEntities;

    function GetEntities: TEntities.TReader;

  public
    constructor Create(AGame: TGLGame; ASpriteGLProgram: TGLProgram);
    destructor Destroy; override;

    property Game: TGLGame read FGame;
    property SpriteSystem: TSpriteSystem read FSpriteSystem;

    function Add(AStructure: TStructure): TEntity;
    property Entities: TEntities.TReader read GetEntities;

  end;

implementation

{ THealthBar }

function THealthBar.Copy: THealthBar;
begin
  Result := THealthBar.Create(Max);
  Result.Current := Current;
end;

constructor THealthBar.Create(AMax: THealth);
begin
  Max := AMax;
  Current := Max;
end;

procedure THealthBar.Damage(AAmount: THealth);
begin
  Current := Current - AAmount;
end;

function THealthBar.Empty: Boolean;
begin
  Result := Current = 0;
end;

function THealthBar.Full: Boolean;
begin
  Result := Current = Max;
end;

function THealthBar.OnHealthChange: THealthChangeEvent.TAccess;
begin
  Result := FOnHealthChange.Access;
end;

function THealthBar.OnMaxChange: TEvent.TAccess;
begin
  Result := FOnMaxChange.Access;
end;

function THealthBar.GetPercentage: TPercentage;
begin
  Result := Current / Max;
end;

procedure THealthBar.Repair(AAmount: THealth);
begin
  Current := Current + AAmount;
end;

procedure THealthBar.SetCurrent(Value: THealth);
begin
  Value := Bounds1(0, Max).Clamp(Value);
  if Value = Current then
    Exit;
  FCurrent := Value;
end;

procedure THealthBar.SetMax(const Value: THealth);
var
  OldPercentage: TPercentage;
begin
  if Max < 0 then
    raise EHealthBarError.Create('The max health must be at least zero.');
  if Max = Value then
    Exit;
  OldPercentage := Percentage;
  FMax := Value;
  Percentage := OldPercentage;
end;

procedure THealthBar.SetPercentage(const Value: TPercentage);
begin
  Current := Value * Max;
end;

{ TEntitySystem }

function TEntitySystem.Add(AStructure: TStructure): TEntity;
begin
  Result := FEntities.Add(TEntity.Create(Self, AStructure));
  Result.ActivateSprites(SpriteSystem);
end;

constructor TEntitySystem.Create(AGame: TGLGame; ASpriteGLProgram: TGLProgram);
begin
  FGame := AGame;
  FSpriteSystem := TSpriteSystem.Create(AGame, ASpriteGLProgram);
  FEntities := TEntities.Create;
  Game.OnUpdate;
end;

destructor TEntitySystem.Destroy;
begin
  FEntities.Free;
  FSpriteSystem.Free;
  inherited;
end;

function TEntitySystem.GetEntities: TEntities.TReader;
begin
  Result := FEntities.Reader;
end;

{ THealthBar.THealthChangeEventInfo }

constructor THealthBar.THealthChangeEventInfo.Create(ASender: THealthBar; AOldHealth: THealth);
begin
  inherited Create(ASender);
  FOldHealth := AOldHealth;
end;

function THealthBar.THealthChangeEventInfo.Damaged: Boolean;
begin
  Result := HealthDifference < 0;
end;

function THealthBar.THealthChangeEventInfo.GetHealthDifference: THealth;
begin
  Result := Sender.Current - OldHealth;
end;

function THealthBar.THealthChangeEventInfo.Repaired: Boolean;
begin
  Result := HealthDifference > 0;
end;

{ TEntityBase.TPart.TDamageTexture }

function TEntityBase.TPart.TDamageTexture.Copy: TDamageTexture;
begin
  Result := TDamageTexture.Create(Health, Texture);
end;

constructor TEntityBase.TPart.TDamageTexture.Create(AHealth: THealth; ATexture: TTexTile);
begin
  FHealth := AHealth;
  FTexture := ATexture;
end;

procedure TEntityBase.TPart.TDamageTexture.SetHealth(const Value: THealth);
begin
  if Health = Value then
    Exit;
  FHealth := Value;
end;

procedure TEntityBase.TPart.TDamageTexture.SetTexture(const Value: TTexTile);
begin
  if Texture = Value then
    Exit;
  FTexture := Value;
end;

{ TEntityBase.TPart }

procedure TEntityBase.TPart.ActivateSprite(ASpriteSystem: TSpriteSystem);
begin
  if DefaultTexture = nil then
    raise EEntityError.Create('No default texture set for entity part.');
  if FSprite <> nil then
    raise EEntityError.Create('Entity sprite already activated into a SpriteSystem.');
  FSprite := ASpriteSystem.Add<TSprite>(FDefaultTexture);
  Sprite.Location.Parent := Location;
end;

procedure TEntityBase.TPart.Assign(AFrom: TPart);
var
  DmgTex: TDamageTexture;
begin
  Name := AFrom.Name;

  Location.Assign(AFrom.Location);

  FHealth.Free;
  FHealth := AFrom.Health.Copy;

  DefaultTexture := AFrom.DefaultTexture;

  FDamageTextures.Clear;
  for DmgTex in AFrom.DamageTextures do
    FDamageTextures.Add(DmgTex.Copy);
end;

procedure TEntityBase.TPart.Connect(APart: TPart);
begin
  if not FConnections.TryAdd(APart) or
    not APart.FConnections.TryAdd(Self) then
    raise EEntityError.Create('Parts are connected already.');
end;

constructor TEntityBase.TPart.Create(AEntity: TEntityBase);
begin
  FEntity := AEntity;
  FConnections := TConnections.Create;
  FHealth := THealthBar.Create;
  FLocation := TLocation2.Create;
  FDamageTextures := TDamageTextures.Create;
end;

destructor TEntityBase.TPart.Destroy;
begin
  if FSprite <> nil then
  begin
    FSprite.Location.Parent := nil;
    FSprite.Remove;
  end;
  FDamageTextures.Free;
  FLocation.Free;
  FHealth.Free;
  DisconnectAll;
  FConnections.Free;
  inherited;
end;

procedure TEntityBase.TPart.Disconnect(APart: TPart);
begin
  if not FConnections.TryRemove(APart) or
    not APart.FConnections.TryRemove(Self) then
    raise EEntityError.Create('Parts are disconnected already.');
end;

procedure TEntityBase.TPart.DisconnectAll;
var
  ToRemove: TConnections;
  Connection: TPart;
begin
  ToRemove := FConnections.Copy;
  for Connection in ToRemove do
    FConnections.TryRemove(Connection);
  ToRemove.Free;
end;

function TEntityBase.TPart.GetConnections: TConnections.TReader;
begin
  Result := FConnections.Reader;
end;

function TEntityBase.TPart.GetDamageTexture(AHealth: THealth): TTexTile;
var
  DmgTex: TDamageTexture;
begin
  Result := FDefaultTexture;
  for DmgTex in FDamageTextures do
    if AHealth <= DmgTex.Health then
      Exit(DmgTex.Texture);
end;

function TEntityBase.TPart.GetDamageTextures: TDamageTextures.TReader;
begin
  Result := FDamageTextures.Reader;
end;

function TEntityBase.TPart.GetIndex: Integer;
begin
  Result := Entity.PartIndex[Self];
end;

function TEntityBase.TPart.GetTexture: TTexTile;
begin
  Result := GetDamageTexture(Health.Current);
end;

function TEntityBase.TPart.IsConnectedTo(APart: TPart): Boolean;
begin
  raise ENotImplemented.Create('IsConnectedTo');
end;

function TEntityBase.TPart.IsConnectedToMainPart: Boolean;
begin
  Result := IsConnectedTo(Entity.MainPart);
end;

procedure TEntityBase.TPart.ResetParent;
begin
  Parent := nil;
end;

procedure TEntityBase.TPart.SetDamageTexture(AHealth: THealth; const Value: TTexTile);
var
  I: Integer;
begin
  for I := 0 to DamageTextures.MaxIndex do
  begin
    if AHealth = DamageTextures[I].Health then
    begin
      DamageTextures[I].Texture := Value;
      Exit;
    end;
    if AHealth < DamageTextures[I].Health then
    begin
      FDamageTextures.Insert(TDamageTexture.Create(AHealth, Value), I);
      Exit;
    end;
  end;
  FDamageTextures.Add(TDamageTexture.Create(AHealth, Value));
end;

procedure TEntityBase.TPart.SetDefaultTexture(const Value: TTexTile);
begin
  if DefaultTexture = Value then
    Exit;
  FDefaultTexture := Value;
end;

procedure TEntityBase.TPart.SetIndex(const Value: Integer);
begin
  Entity.PartIndex[Self] := Value;
end;

procedure TEntityBase.TPart.SetName(const Value: string);
begin
  if Name = Value then
    Exit;
  FName := Value;
end;

procedure TEntityBase.TPart.SetParent(const Value: TPart);
begin
  if Parent = Value then
    Exit;
  FParent := Value;
  raise ENotImplemented.Create('Location Parenting');
end;

{ TEntityBase }

procedure TEntityBase.ActivateSprites(ASpriteSystem: TSpriteSystem);
var
  Part: TPart;
begin
  for Part in Parts do
    Part.ActivateSprite(ASpriteSystem);
end;

function TEntityBase.AddPart: TPart;
begin
  Result := FParts.Add(TPart.Create(Self));
  Result.Location.Parent := BaseTransformation;
end;

procedure TEntityBase.Assign(AFrom: TEntityBase);
var
  Part: TPart;
begin
  FParts.Clear;
  for Part in AFrom.Parts do
    AddPart.Assign(Part);
  MainPart := Parts[AFrom.MainPart.Index];
  BaseTransformation.Assign(AFrom.BaseTransformation);
end;

constructor TEntityBase.Create;
begin
  FParts := TParts.Create;
  FBaseTransformation := TLocation2.Create;
end;

destructor TEntityBase.Destroy;
begin
  FBaseTransformation.Free;
  FParts.Free;
  inherited;
end;

function TEntityBase.GetPartIndex(APart: TPart): Integer;
begin
  Result := FParts.Find(APart);
end;

procedure TEntityBase.SetMainPart(const Value: TPart);
begin
  Assert(Value.Entity = Self);
  if MainPart = Value then
    Exit;
  FMainPart := Value;
end;

procedure TEntityBase.SetPartIndex(APart: TPart; const Value: Integer);
begin
  FParts.SetIndex(APart.Index, Value);
end;

{ TEntity }

constructor TEntity.Create(AEntitySystem: TEntitySystem; AStructure: TStructure);
begin
  inherited Create;
  FEntitySystem := AEntitySystem;
  FLocation := TLocation2.Create;
  AStructure.MakeEntity(Self);
  BaseTransformation.Parent := Location;
end;

destructor TEntity.Destroy;
begin
  BaseTransformation.Parent := nil;
  FLocation.Free;
  inherited;
end;

function TEntity.GetSpriteSystem: TSpriteSystem;
begin
  Result := EntitySystem.SpriteSystem;
end;

procedure TEntity.Remove;
begin
  FEntitySystem.FEntities.Remove(Self);
end;

{ TStructure }

procedure TStructure.MakeEntity(AEntity: TEntityBase);
begin
  AEntity.Assign(Self);
end;

end.
