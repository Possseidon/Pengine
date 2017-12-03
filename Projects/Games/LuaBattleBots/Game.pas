unit Game;

interface

uses
  Pengine.Collections,
  Pengine.Camera,
  Pengine.Vector,
  Pengine.Matrix,
  Pengine.Lua,

  EntityDefine,
  GameLogicDefine;

type

  TGame = class
  private
    FGameLogic: TGameLogic;

    FCamera: TCamera;
    FEntities: TRefArray<TEntity>;
    FLuaUpdateTime: Single;

    FLua: TLua;

  public
    constructor Create(ACamera: TCamera);
    destructor Destroy; override;

    procedure Update(ADeltaTime: Single);

    procedure AddEntity(AEntity: TEntity);

  end;

implementation

{ TGame }

procedure TGame.AddEntity(AEntity: TEntity);
begin
  FEntities.Add(AEntity);
  FCamera.AddRenderable(AEntity);
end;

constructor TGame.Create(ACamera: TCamera);
begin
  FCamera := ACamera;
  FEntities := TRefArray<TEntity>.Create(True);

  FLua := TLua.Create;
end;

destructor TGame.Destroy;
var
  Entity: TEntity;
begin
  for Entity in FEntities do
    FCamera.DelRenderable(Entity);
  FEntities.Free;

  FLua.Free;
  inherited;
end;

procedure TGame.Update(ADeltaTime: Single);
var
  Entity: TEntity;
  UpdateLua: Boolean;
begin
  FLuaUpdateTime := FLuaUpdateTime - ADeltaTime;

  UpdateLua := FLuaUpdateTime <= 0;
  if UpdateLua then
    FLuaUpdateTime := TLuaEntity.LuaUpdateInterval;
  
  for Entity in FEntities do
  begin
    Entity.Update(ADeltaTime);
    if UpdateLua and (Entity is TLuaEntity) then
      TLuaEntity(Entity).UpdateLua;
  end;      
end;

end.
