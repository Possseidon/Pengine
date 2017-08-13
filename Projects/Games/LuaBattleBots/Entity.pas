unit Entity;

interface

uses
  VAOManager, LuaHeader, Lists;

type

  TEntity = class;

  { TEntityList }

  TEntityList = class
  private
    FEntities: TRefArray<TEntity>;
    
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddEntity(AEntity: TEntity);
    procedure DelEntity(AEntity: TEntity);

  end;

  { TEntity }

  TEntity = class(TVAOProxy)
  private
    FEntityList: TEntityList;

    FHealth, FMaxHealth: Single;
  protected

  public
    constructor Create(AEntityList: TEntityList; ASourceVAO: TVAO; AHealth: Single);
    destructor Destroy; override;

    procedure Update(ADeltaTime: Single); virtual;
  end;

  { TLuaEntity }

  TLuaEntity = class(TEntity)
  private
    FLua: TLuaState;
  protected

  public
    constructor Create(ASourceVAO: TVAO; AHealth: Single);
    destructor Destroy; override;

    procedure Update(ADeltaTime: Single); override;
  end;

implementation

{ TEntity }

constructor TEntity.Create(AEntityList: TEntityList; ASourceVAO: TVAO; AHealth: Single);
begin
  inherited Create(ASourceVAO);
  FHealth := AHealth;
  FMaxHealth := AHealth;
end;

destructor TEntity.Destroy;
begin
  FEntityList.DelEntity(Self);
  inherited;
end;

procedure TEntity.Update(ADeltaTime: Single);
begin

end;

{ TLuaEntity }

constructor TLuaEntity.Create(ASourceVAO: TVAO; AHealth: Single);
begin
  inherited Create(ASourceVAO, AHealth);

  // Lua Init
  FLua := NewLuaState;
end;

destructor TLuaEntity.Destroy;
begin
  FLua.Close;

  inherited;
end;

procedure TLuaEntity.Update(ADeltaTime: Single);
begin
  inherited;
end;

{ TEntityList }

constructor TEntityList.Create;
begin
  FEntities := TObjectArray<TEntity>.Create;
end;

procedure TEntityList.DelEntity(AEntity: TEntity);
begin
  FEntities.Del(AEntity);
end;

destructor TEntityList.Destroy;
begin
  for Entity in FEntities do
    Entity.Free;
  FEntities.Free;
  inherited;
end;

procedure TEntityList.AddEntity(AEntity: TEntity);
begin
  FEntities.Add(AEntity);
end;

end.
