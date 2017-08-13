unit EntityDefine;

interface

uses
  VAOManager, LuaHeader, Math, LuaConf, VectorGeometry, DebugConsoleDefine, IntegerMaths, SysUtils;

type

  { TEntity }

  TEntity = class abstract(TVAOProxy)
  private
    FDead: Boolean;
    FHealth, FMaxHealth: Single;

    FName: string;
    
    procedure SetHealth(Value: Single);
    procedure SetMaxHealth(Value: Single);
  protected
    class function GetSourceVAO: TVAO; virtual; abstract;
    class function GetInitialHealth: Single; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Update(ADeltaTime: Single); virtual;

    property Health: Single read FHealth write SetHealth;
    property MaxHealth: Single read FMaxHealth;

    property Dead: Boolean read FDead;

    property Name: string read FName;


  end;

  { TLuaEntity }

  TLuaEntity = class abstract(TEntity)
  public const
    LuaUpdateInterval = 0.1;

  private
    FLua: TLuaState;
    FLuaValid: Boolean;

    class function GetSelf(L: TLuaState): TLuaEntity; static;

    class function LuaGetHealth(L: TLuaState): Integer; static; cdecl;
    class function LuaGetMaxHealth(L: TLuaState): Integer; static; cdecl;
    class function LuaPrint(L: TLuaState): Integer; static; cdecl;
  protected

  public
    constructor Create;
    destructor Destroy; override;

    procedure Update(ADeltaTime: Single); override;
    procedure UpdateLua; virtual;

    procedure SetUpdateFunction(const AFunction: AnsiString);
  end;

  { TBotModule }

  TBotCore = class;

  TBotModule = class abstract(TEntity)
  private
    FParent: TBotCore;
    FSide: TBasicDir3;
  protected
    property Parent: TBotCore read FParent;
    property Side: TBasicDir3 read FSide;

  public
    constructor Create(AParent: TBotCore; ASide: TBasicDir3); virtual;
    destructor Destroy; override;

    procedure Update(ADeltaTime: Single); override;
  end;

  TBotModuleClass = class of TBotModule;

  { TBotCore }

  TBotCore = class(TLuaEntity)
  private
    FModules: array [TBasicDir3] of TBotModule;

    function GetModule(ASide: TBasicDir3): TBotModule;
  protected
    class function GetSourceVAO: TVAO; override;
    class function GetInitialHealth: Single; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Update(ADeltaTime: Single); override;
    procedure UpdateLua; override;

    procedure AddModule(ASide: TBasicDir3; AModuleClass: TBotModuleClass);
    property Modules[ASide: TBasicDir3]: TBotModule read GetModule;
  end;

implementation

{ TEntity }

procedure TEntity.SetHealth(Value: Single);
begin
  Value := EnsureRange(Value, 0, GetInitialHealth);
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

constructor TEntity.Create();
begin
  inherited Create(GetSourceVAO);
  FHealth := GetInitialHealth;
  FMaxHealth := GetInitialHealth;
  FName := 'Unnamed';
  Location.Offset := Vec3(-0.5, 0, -0.5);
end;

destructor TEntity.Destroy;
begin
  inherited;
end;

procedure TEntity.Update(ADeltaTime: Single);
begin
  // nothing by default
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
  DebugConsole.Write(Self.Name + ': ');
  for I := 1 to L.Top do
  begin
    DebugConsole.Write(string(L.ToString(I)));
    if I < L.Top then
      DebugConsole.Write(' ');
  end;
  DebugConsole.WriteLine;
  Result := 0;
end;

constructor TLuaEntity.Create;
begin
  inherited Create;

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
var
  Err: TLuaPCallError;
begin
  inherited;
  if FLuaValid then
  begin
    FLua.GetGlobal('update');
    Err := FLua.PCall(0, 0, 0);
    case Err of
      lceErrorRun:
        DebugConsole.WriteLine(string(FLua.ToString));
      lceErrorMemory:
        DebugConsole.WriteLine('Lua Memory Error');
      lceErrorGCMM:
        DebugConsole.WriteLine('Lua GarbageCollector Error');
      lceErrorError:
        DebugConsole.WriteLine('Lua Error-Function Error');
    end;
  end;
end;

procedure TLuaEntity.SetUpdateFunction(const AFunction: AnsiString);
var
  Err: TLuaLoadError;
begin
  Err := FLua.LoadString(AFunction);
  case Err of
    lleErrorSyntax:
      DebugConsole.WriteLine(string(FLua.ToString));
    lleErrorMemory:
      DebugConsole.WriteLine('Lua Memory Error');
    lleErrorGCMM:
      DebugConsole.WriteLine('Lua Garbage Collector Error');
  end;
  FLua.SetGlobal('update');
  FLuaValid := Err = lleOK;
end;

{ TBotModule }

constructor TBotModule.Create(AParent: TBotCore; ASide: TBasicDir3);
begin
  inherited Create;
  FParent := AParent;
  FSide := ASide;
end;

destructor TBotModule.Destroy;
begin
  inherited;
end;

procedure TBotModule.Update(ADeltaTime: Single);
begin
  inherited;
  if FParent <> nil then
  begin
    Location.Assign(FParent.Location);
    Location.Offset := Location.Offset + VecDir[FSide];
  end;
end;

{ TBotCore }

function TBotCore.GetModule(ASide: TBasicDir3): TBotModule;
begin
  Result := FModules[ASide];
end;

class function TBotCore.GetSourceVAO: TVAO;
begin
  Result := nil;
end;

class function TBotCore.GetInitialHealth: Single;
begin
  Result := 100;
end;

constructor TBotCore.Create;
begin
  inherited Create;
end;

destructor TBotCore.Destroy;
begin
  inherited;
end;

procedure TBotCore.Update(ADeltaTime: Single);
var
  Module: TBotModule;
begin
  inherited;
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
  inherited;

end;

procedure TBotCore.AddModule(ASide: TBasicDir3; AModuleClass: TBotModuleClass);
begin
  if FModules[ASide] = nil then
    raise Exception.Create('Only one Module can be attached on a single side!');
  FModules[ASide] := AModuleClass.Create(Self, ASide);
end;

end.
