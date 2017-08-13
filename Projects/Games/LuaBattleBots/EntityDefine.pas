unit EntityDefine;

interface

uses
  VAOManager, LuaHeader, Lists;

type

  { TEntity }

  TEntity = class(TVAOProxy)
  private
    FDead: Boolean;
    FHealth, FMaxHealth: Single;
  protected

  public
    constructor Create(ASourceVAO: TVAO; AHealth: Single);
    destructor Destroy; override;

    procedure Update(ADeltaTime: Single); virtual;

    property Dead: Boolean read FDead;
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

constructor TEntity.Create(ASourceVAO: TVAO; AHealth: Single);
begin
  inherited Create(ASourceVAO);
  FHealth := AHealth;
  FMaxHealth := AHealth;
end;

destructor TEntity.Destroy;
begin
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

end.
