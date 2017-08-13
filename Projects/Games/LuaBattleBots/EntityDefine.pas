unit EntityDefine;

interface

uses
  VAOManager, LuaHeader, Math, LuaConf, VectorGeometry, IntegerMaths;

type

  { TEntity }

  TEntity = class(TVAOProxy)
  private
    FDead: Boolean;
    FHealth, FMaxHealth: Single;

    FName: string;
    
    procedure SetHealth(Value: Single);
    procedure SetMaxHealth(Value: Single);
  protected

  public
    constructor Create(ASourceVAO: TVAO; AHealth: Single);
    destructor Destroy; override;

    procedure Update(ADeltaTime: Single); virtual;

    property Health: Single read FHealth write SetHealth;
    property MaxHealth: Single read FMaxHealth write SetMaxHealth;
    
    property Dead: Boolean read FDead;

    property Name: string read FName;
  end;

  { TLuaEntity }

  TLuaEntity = class(TEntity)
  private
    FLua: TLuaState;
    FLuaValid: Boolean;

    class function GetSelf(L: TLuaState): TLuaEntity; static;

    class function LuaGetHealth(L: TLuaState): Integer; static; cdecl;
    class function LuaGetMaxHealth(L: TLuaState): Integer; static; cdecl;
    class function LuaPrint(L: TLuaState): Integer; static; cdecl;
  protected

  public
    constructor Create(ASourceVAO: TVAO; AHealth: Single);
    destructor Destroy; override;

    procedure Update(ADeltaTime: Single); override;
    procedure UpdateLua(); virtual;

    procedure SetUpdateFunction(const AFunction: AnsiString);
  end;

  TBotCore = class;

  { TBotModule }

  TBotModule = class(TEntity)
  private
    FParent: TBotCore;
    FSide: TBasicDir3;
  protected

  public
    constructor Create(ASourceVAO: TVAO; AHealth: Single; AParent: TBotCore; ASide: TBasicDir3);
    destructor Destroy; override;

    procedure Update(ADeltaTime: Single); override;
  end;

  { TBotCore }

  TBotCore = class(TLuaEntity)
  private
    FModules: array [TBasicDir3] of TBotModule;
  protected

  public
    constructor Create(ASourceVAO: TVAO);
    destructor Destroy; override;

    procedure Update(ADeltaTime: Single); override;
    procedure UpdateLua(); override;
  end;

implementation

{ TEntity }

procedure TEntity.SetHealth(Value: Single);
begin
  Value := EnsureRange(Value, 0, FMaxHealth);
  if Value = FHealth then
    Exit;
  FHealth := Value;
  if Health <= 0 then
    FDead := True;
end;

procedure TEntity.SetMaxHealth(Value: Single);
begin
  Value := Max(Value, 0);
  if Value = FMaxHealth then
    Exit;
  FMaxHealth := Value;
  Health := Min(Health, FMaxHealth);
end;

constructor TEntity.Create(ASourceVAO: TVAO; AHealth: Single);
begin
  inherited Create(ASourceVAO);
  FHealth := AHealth;
  FMaxHealth := AHealth;
  FName := 'unnamed';
end;

destructor TEntity.Destroy;
begin
  inherited;
end;

procedure TEntity.Update(ADeltaTime: Single);
begin

end;

{ TLuaEntity }

class function TLuaEntity.GetSelf(L: TLuaState): TLuaEntity;
begin
  Result := TLuaEntity(PPointer(L.GetExtraSpace)^);
end;

class function TLuaEntity.LuaGetHealth(L: TLuaState): Integer;
var
  Self: TLuaEntity;
begin
  Self := GetSelf(L);

  L.CheckEnd(1);
  L.PushNumber(Self.Health);

  Result := 1;
end;

class function TLuaEntity.LuaGetMaxHealth(L: TLuaState): Integer;
var
  Self: TLuaEntity;
begin
  Self := GetSelf(L);

  L.CheckEnd(1);
  L.PushNumber(Self.MaxHealth);

  Result := 1;
end;

class function TLuaEntity.LuaPrint(L: TLuaState): Integer;
var
  I: Integer;
  Self: TLuaEntity;
begin
  Self := GetSelf(L);
  Write(Self.Name + ': ');
  for I := 1 to L.Top do
  begin
    Write(L.ToString(I));
    if I < L.Top then
      Write(' ');
  end;
  Writeln;
  Result := 0;
end;

constructor TLuaEntity.Create(ASourceVAO: TVAO; AHealth: Single);
begin
  inherited Create(ASourceVAO, AHealth);

  // Lua Init
  FLua := NewLuaState;
  PPointer(FLua.GetExtraSpace)^ := Self;

  // Base Lua Functions
  FLua.Register('getHealth', LuaGetHealth);
  FLua.Register('getMaxHealth', LuaGetMaxHealth);
  FLua.Register('print', LuaPrint);
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

procedure TLuaEntity.UpdateLua;
begin
  if FLuaValid then
  begin
    FLua.GetGlobal('update');
    FLua.Call(0, 0);
  end;
end;

procedure TLuaEntity.SetUpdateFunction(const AFunction: AnsiString);
begin
  FLua.LoadString(AFunction);
  FLua.SetGlobal('update');
end;

{ TBotModule }

constructor TBotModule.Create(ASourceVAO: TVAO; AHealth: Single; AParent: TBotCore; ASide: TBasicDir3);
begin
  inherited Create(ASourceVAO, AHealth);
  FParent := AParent;
  FSide := ASide;
end;

destructor TBotModule.Destroy;
begin
  inherited;
end;

procedure TBotModule.Update(ADeltaTime: Single);
begin
  if FParent <> nil then
  begin
    Location.Assign(FParent.Location);
    Location.Offset := Location.Offset + VecDir[FSide];
  end;
end;

{ TBotCore }

// Needs to be without a VAO in Parameters
constructor TBotCore.Create(ASourceVAO: TVAO);
begin
  inherited Create(ASourceVAO, 100);
end;

destructor TBotCore.Destroy;
begin
  inherited;
end;

procedure TBotCore.Update(ADeltaTime: Single);
var
  Module: TBotModule;
begin
  // Update Core



  // Update Modules

  for Module in FModules do
  begin
    if Module <> nil then
      Module.Update(ADeltaTime);
  end;
end;

procedure TBotCore.UpdateLua;
begin

end;

end.