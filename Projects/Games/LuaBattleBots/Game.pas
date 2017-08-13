unit Game;

interface

uses
  EntityDefine, Lists, Camera, VectorGeometry, Matrix;

type

  TGame = class
  public const
    LuaUpdateInterval = 0.1;  
                      
  private
    FCamera: TCamera;
    FEntities: TObjectArray<TEntity>;
    FLuaUpdateTime: Single;

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
end;

destructor TGame.Destroy;
var
  Entity: TEntity;
begin
  for Entity in FEntities do
    FCamera.DelRenderObject(Entity);
  FEntities.Free;
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
    FLuaUpdateTime := LuaUpdateInterval;
  
  for Entity in FEntities do
  begin
    Entity.Update(ADeltaTime);
    if Entity is TLuaEntity then
      // TODO: TLuaEntity(Entity).UpdateLua;
  end;      
end;

end.
