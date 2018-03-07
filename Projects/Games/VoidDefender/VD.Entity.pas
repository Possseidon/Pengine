unit VD.Entity;

interface

uses
  System.SysUtils,

  Pengine.Collections,
  Pengine.Vector,
  Pengine.EventHandling,
  Pengine.TextureAtlas,
  Pengine.SpriteSystem;

type

  EHealthbarInvalidMax = class(Exception)
  public
    constructor Create;
  end;

  THealth = type Single;

  TPercentage = type Single;

  THealthBar = class
  public type

    TEventInfo = TSenderEventInfo<THealthBar>;

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

    TEvent = TEvent<TEventInfo>;
  private
    FMax: THealth;
    FCurrent: THealth;
    FOnHealthChanged: TEvent;

    function GetPercentage: TPercentage;
    procedure SetCurrent(Value: THealth);

    function GetOnHealthChanged: TEvent.TAccess;

  public
    constructor Create(AMax: THealth);

    property Max: THealth read FMax;
    property Current: THealth read FCurrent write SetCurrent;
    property Percentage: TPercentage read GetPercentage;

    function Empty: Boolean;
    function Full: Boolean;

    procedure Damage(AAmount: THealth);
    procedure Repair(AAmount: THealth);

    property OnHealthChanged: TEvent.TAccess read GetOnHealthChanged;

  end;

  TEntity = class
  public type

    TTemplate = class
    public type

      TPart = class
      public type

        TSubParts = TRefArray<TPart>;

      private
        FEntity: TTemplate;
        FParent: TPart;
        FSubParts: TSubParts;
        FLocation: TLocation2;
        FHealth: THealth;
        FTexture: TTextureAtlas.TTile;

        procedure SetHealth(const Value: THealth);

      public
        constructor Create(AParent: TPart);
        destructor Destroy; override;

        property Entity: TTemplate read FEntity;
        property Parent: TPart read FParent;
        property SubParts: TSubParts read FSubParts;

        function AddPart: TPart;

        property Location: TLocation2 read FLocation;
        property Health: THealth read FHealth write SetHealth;
        property Texture: TTextureAtlas.TTile read FTexture write FTexture;

      end;

    private
      FRootPart: TPart;

    public
      constructor Create;
      destructor Destroy; override;

      property RootPart: TPart read FRootPart;

    end;

    TPart = class
    public type

      TSubParts = TRefArray<TPart>;

    private
      FEntity: TEntity;
      FParent: TPart;
      FSubParts: TSubParts;
      FLocation: TLocation2;
      FHealth: THealthBar;
      FSprite: TSprite;

      function GetSubParts: TSubParts.TReader;
      function GetSpriteSystem: TSpriteSystem;

    public
      constructor Create(ATemplate: TTemplate.TPart; AEntity: TEntity); overload;
      constructor Create(ATemplate: TTemplate.TPart; AParent: TPart); overload;
      destructor Destroy; override;

      property Entity: TEntity read FEntity;
      property Parent: TPart read FParent;
      property SubParts: TSubParts.TReader read GetSubParts;

      property SpriteSystem: TSpriteSystem read GetSpriteSystem;

      property Location: TLocation2 read FLocation;
      property Health: THealthBar read FHealth;

    end;

  private
    FSpriteSystem: TSpriteSystem;
    FRootPart: TPart;

    function GetLocation: TLocation2;

  public
    constructor Create(ASpriteSystem: TSpriteSystem; ATemplate: TTemplate);
    destructor Destroy; override;

    property SpriteSystem: TSpriteSystem read FSpriteSystem;

    property RootPart: TPart read FRootPart;
    property Location: TLocation2 read GetLocation;

  end;

implementation

{ THealthBar }

constructor THealthBar.Create(AMax: THealth);
begin
  FMax := AMax;
  FCurrent := Max;
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

function THealthBar.GetOnHealthChanged: TEvent.TAccess;
begin
  Result := FOnHealthChanged.Access;
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

{ TEntity }

constructor TEntity.Create(ASpriteSystem: TSpriteSystem; ATemplate: TTemplate);
begin
  FSpriteSystem := ASpriteSystem;
  FRootPart := TPart.Create(ATemplate.RootPart, Self);
  FRootPart.FEntity := Self;
end;

destructor TEntity.Destroy;
begin
  FRootPart.Free;
  inherited;
end;

function TEntity.GetLocation: TLocation2;
begin
  Result := RootPart.Location;
end;

{ TEntity.TPart }

constructor TEntity.TPart.Create(ATemplate: TTemplate.TPart; AEntity: TEntity);
var
  Part: TEntity.TTemplate.TPart;
begin
  FEntity := AEntity;
  FLocation := ATemplate.Location.Copy;
  FSubParts := TSubParts.Create;
  FHealth := THealthBar.Create(ATemplate.Health);
  FSprite := SpriteSystem.Add<TSprite>(ATemplate.Texture);
  FSprite.Location.Parent := Location;
  for Part in ATemplate.SubParts do
    FSubParts.Add(TPart.Create(Part, Self));
end;

constructor TEntity.TPart.Create(ATemplate: TTemplate.TPart; AParent: TPart);
begin
  Create(ATemplate, AParent.Entity);
  FParent := AParent;
  Location.Parent := Parent.Location;
end;

destructor TEntity.TPart.Destroy;
var
  Part: TPart;
begin
  for Part in SubParts do
    Part.Free;
  FSubParts.Free;
  FSprite.Location.Parent := nil;
  FSprite.Remove;
  FLocation.Free;
  FHealth.Free;
  inherited;
end;

function TEntity.TPart.GetSpriteSystem: TSpriteSystem;
begin
  Result := Entity.SpriteSystem;
end;

function TEntity.TPart.GetSubParts: TSubParts.TReader;
begin
  Result := FSubParts.Reader;
end;

{ THealthBar.THealthChangeEventInfo }

constructor THealthBar.THealthChangeEventInfo.Create(ASender: THealthBar; AOldHealth: THealth);
begin
  inherited Create(ASender);
  FOldHealth := AOldHealth;
end;

function THealthBar.THealthChangeEventInfo.Damaged: Boolean;
begin
  Result := Sender.Current < OldHealth;
end;

function THealthBar.THealthChangeEventInfo.GetHealthDifference: THealth;
begin
  Result := Sender.Current - OldHealth;
end;

function THealthBar.THealthChangeEventInfo.Repaired: Boolean;
begin
  Result := Sender.Current > OldHealth;
end;

{ TEntity.TTemplate.TPart }

function TEntity.TTemplate.TPart.AddPart: TPart;
begin
  Result := SubParts.Add(TPart.Create(Self));
end;

constructor TEntity.TTemplate.TPart.Create(AParent: TPart);
begin
  FLocation := TLocation2.Create;
  if AParent <> nil then
  begin
    FParent := AParent;
    FEntity := Parent.Entity;
    Location.Parent := Parent.Location;
  end;
  FSubParts := TSubParts.Create;
  FHealth := 1;
end;

destructor TEntity.TTemplate.TPart.Destroy;
var
  Part: TPart;
begin
  for Part in SubParts do
    Part.Free;
  FSubParts.Free;
  FLocation.Free;
  inherited;
end;

procedure TEntity.TTemplate.TPart.SetHealth(const Value: THealth);
begin
  if Value <= 0 then
    raise EHealthbarInvalidMax.Create;
  if Health = Value then
    Exit;
  FHealth := Value;
end;

{ EHealthbarInvalidMax }

constructor EHealthbarInvalidMax.Create;
begin
  inherited Create('The maximum health must be greater than zero!');
end;

{ TEntity.TTemplate }

constructor TEntity.TTemplate.Create;
begin
  FRootPart := TPart.Create(nil);
end;

function TEntity.TTemplate.CreateEntity: TEntity;
begin
  Result := TEntity.Create();
end;

destructor TEntity.TTemplate.Destroy;
begin
  FRootPart.Free;
  inherited;
end;

end.
