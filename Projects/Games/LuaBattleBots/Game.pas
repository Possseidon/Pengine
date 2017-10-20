unit Game;

interface

uses
  EntityDefine, Lists, Camera, VectorGeometry, Matrix, LuaDefine;

type

  TGame = class
  private
    FCamera: TCamera;
    FEntities: TObjectArray<TEntity>;
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
  FCamera.AddRenderObject(AEntity);
end;

constructor TGame.Create(ACamera: TCamera);
begin
  FCamera := ACamera;
  FEntities := TObjectArray<TEntity>.Create;

  FLua := TLua.Create;
end;

destructor TGame.Destroy;
var
  Entity: TEntity;
begin
  for Entity in FEntities do
    FCamera.DelRenderObject(Entity);
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
