unit EntityDefine;

interface

uses
  System.SysUtils,
  System.Math,

  Pengine.VAO,
  Pengine.LuaHeader,
  Pengine.Lua,
  Pengine.Vector,
  Pengine.DebugConsole,
  Pengine.IntMaths,
  Pengine.Collections,
  Pengine.CollectionInterfaces,
  Pengine.Camera,

  Resources;

type

  { TEntity }

  TEntity = class abstract(TVAOProxy)
  public const
    MaxNameLength = 42;

  private
    FHealth, FMaxHealth: Single;
    FDead: Boolean;

    FName: AnsiString;

    procedure SetHealth(Value: Single);

  protected
    class function GetSourceVAO: TVAO; virtual; abstract;
    class procedure FreeSourceVAO; virtual; abstract;
    class function GetInitialHealth: Single; virtual; abstract;
    class function GetInitialName: AnsiString; virtual; abstract;

    procedure SetMaxHealth(Value: Single);
    procedure SetName(Value: AnsiString);

    class function LuaGetHealth(L: TLuaState): Integer; static; cdecl;
    class function LuaGetMaxHealth(L: TLuaState): Integer; static; cdecl;
    class function LuaGetHealthPercentage(L: TLuaState): Integer; static; cdecl;
    class function LuaGetName(L: TLuaState): Integer; static; cdecl;
    class function LuaSetName(L: TLuaState): Integer; static; cdecl;

    class function GetSelf(L: TLuaState): TEntity; static;

    procedure RegisterLuaMethod(L: TLuaState; AName: AnsiString; AFunc: TLuaCFunction);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Update(ADeltaTime: Single); virtual;

    property Health: Single read FHealth write SetHealth;
    property MaxHealth: Single read FMaxHealth;
    property Dead: Boolean read FDead;

    property Name: AnsiString read FName write SetName;

  end;

  { TLuaEntity }

  TLuaEntity = class abstract(TEntity)
  public const
    LuaUpdateInterval = 0.1;

  private
    FLua: TLua;
    FLuaValid: Boolean;

    class function LuaPrint(L: TLuaState): Integer; static; cdecl;

    class function GetSelf(L: TLuaState): TLuaEntity; static;
  protected

  public
    constructor Create(ALua: TLua);
    
    procedure Update(ADeltaTime: Single); override;
    procedure UpdateLua; virtual;

    procedure SetUpdateFunction(const AFunction: AnsiString);

  end;

  TBotCore = class;

  { TBotModule }

  TBotModule = class abstract(TEntity)
  private
    FParent: TBotCore;
    FSide: TBasicDir3;
    FLua: TLua;

  protected
    property Parent: TBotCore read FParent;
    property Side: TBasicDir3 read FSide;

    property Lua: TLua read FLua;

    procedure AttachLua; virtual;
    procedure DetachLua; virtual;
  public
    constructor Create(AParent: TBotCore; ASide: TBasicDir3); virtual;
    destructor Destroy; override;

    procedure Update(ADeltaTime: Single); override;

  end;

  TBotModuleClass = class of TBotModule;

  { TBotCore }

  TBotCore = class(TLuaEntity)
  public type

    TRenderableIterator = class(TIterator<IRenderable>)
    private
      FBotCore: TBotCore;
      FCurrent: TBasicDir;

    public
      constructor Create(ABotCore: TBotCore);

      function MoveNext: Boolean; override;
      function GetCurrent: IRenderable; override;
    end;

    TRenderableIterable = class(TIterable<IRenderable>)
    private
      FBotCore: TBotCore;
    public
      constructor Create(ABotCore: TBotCore);
      function GetEnumerator: IIterator<IRenderable>; override;
    end;

  private
    FModules: array [TBasicDir3] of TBotModule;

    function GetModule(ASide: TBasicDir3): TBotModule;

    procedure RegisterLuaModulesTable;

    class function GetModelParams: TResCubeVAOParams;
    procedure UpdateModules(ADeltaTime: Single);

  protected
    class function GetSourceVAO: TVAO; override;
    class procedure FreeSourceVAO; override;
    class function GetInitialHealth: Single; override;
    class function GetInitialName: AnsiString; override;

  public

    constructor Create(FLua: TLua);
    destructor Destroy; override;

    procedure Update(ADeltaTime: Single); override;
    procedure UpdateLua; override;

    procedure AttachModule(ASide: TBasicDir3; AModuleClass: TBotModuleClass);
    procedure DetachModule(ASide: TBasicDir3);
    property Modules[ASide: TBasicDir3]: TBotModule read GetModule;

    function RenderableChildren: IIterable<IRenderable>; override;
  end;

implementation

{ TEntity }

class function TEntity.LuaGetHealth(L: TLuaState): Integer;
var
  Self: TEntity;
begin
  Self := GetSelf(L);

  L.CheckEnd(1);

  L.PushNumber(Self.Health);

  Result := 1;
end;

class function TEntity.LuaGetMaxHealth(L: TLuaState): Integer;
var
  Self: TEntity;
begin
  Self := GetSelf(L);

  L.CheckEnd(1);

  L.PushNumber(Self.MaxHealth);

  Result := 1;
end;

class function TEntity.LuaGetName(L: TLuaState): Integer;
var
  Self: TEntity;
begin
  Self := GetSelf(L);

  L.CheckEnd(1);

  L.PushString(PPAnsiChar(@Self.Name)^);

  Result := 1;
end;

class function TEntity.LuaSetName(L: TLuaState): Integer;
var
  Self: TEntity;
begin
  Self := GetSelf(L);

  L.CheckType(1, ltString);
  L.CheckEnd(2);

  Self.Name := AnsiString(L.ToString);

  Result := 0;
end;

class function TEntity.GetSelf(L: TLuaState): TEntity;
begin
  Result := TEntity(L.ToUserdata(L.UpvalueIndex(1)));
end;

class function TEntity.LuaGetHealthPercentage(L: TLuaState): Integer;
var
  Self: TEntity;
begin
  Self := GetSelf(L);

  L.CheckEnd(1);

  L.PushNumber(Self.Health / Self.MaxHealth * 100);

  Result := 1;
end;

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

procedure TEntity.SetName(Value: AnsiString);
begin
  if Length(Value) > MaxNameLength then
    SetLength(Value, MaxNameLength);
  FName := Value;
end;

procedure TEntity.RegisterLuaMethod(L: TLuaState; AName: AnsiString; AFunc: TLuaCFunction);
begin
  L.PushLightuserdata(Self);
  L.PushCClosure(AFunc, 1);
  L.SetField(PPAnsiChar(@AName)^, -2);
end;

constructor TEntity.Create;
begin
  inherited Create(GetSourceVAO);
  FHealth := GetInitialHealth;
  FMaxHealth := GetInitialHealth;
  FName := GetInitialName;
  Location.Offset := Vec3(-0.5, 0, -0.5);
end;

destructor TEntity.Destroy;
begin
  FreeSourceVAO;
  inherited;
end;

procedure TEntity.Update(ADeltaTime: Single);
begin
  // nothing by default
end;

{ TLuaEntity }

class function TLuaEntity.LuaPrint(L: TLuaState): Integer;
var
  I: Integer;
  Self: TLuaEntity;
begin
  Self := GetSelf(L);

  DebugWriteBuf(Self.Name + ': ');
  for I := 1 to L.Top do
  begin
    DebugWriteBuf(string(L.ToString(I)));
    if I < L.Top then
      DebugWriteBuf(' ');
  end;
  DebugFlushBuf(True);
  Result := 0;
end;

class function TLuaEntity.GetSelf(L: TLuaState): TLuaEntity;
begin
  Result := TLuaEntity(TEntity.GetSelf(L));
end;

constructor TLuaEntity.Create(ALua: TLua);
begin
  inherited Create;

  // Lua Init
  FLua := ALua;
  
  // Base Lua Functions
  {
  FLua.L.PushGlobalTable;

  RegisterLuaMethod(FLua.L, 'print', LuaPrint);
  RegisterLuaMethod(FLua.L, 'getHealth', LuaGetHealth);
  RegisterLuaMethod(FLua.L, 'getMaxHealth', LuaGetMaxHealth);
  RegisterLuaMethod(FLua.L, 'getName', LuaGetName);
  RegisterLuaMethod(FLua.L, 'setName', LuaSetName);
  RegisterLuaMethod(FLua.L, 'getHealthPercentage', LuaGetHealthPercentage);

  FLua.L.Top := 0;
  }
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
    FLua.L.GetGlobal('update');
    if FLua.CallTimeout(0, 0, 1, Err) then
    begin
      case Err of
        lceErrorRun:
          DebugWriteLine(Name + ' Runtime Error: ' + FLua.L.ToString);
        lceErrorMemory:
          DebugWriteLine('Lua Memory Error');
        lceErrorGCMM:
          DebugWriteLine('Lua GarbageCollector Error');
        lceErrorError:
          DebugWriteLine('Lua Error-Function Error');
      end;
    end
    else
    begin
      DebugWriteLine(Name + ' Update Timeout');
      FLuaValid := False;
    end;
  end;
end;

procedure TLuaEntity.SetUpdateFunction(const AFunction: AnsiString);
var
  Err: TLuaLoadError;
begin
  Err := FLua.L.LoadString(AFunction);
  case Err of
    lleErrorSyntax:
      DebugWriteLine(Name + ' Syntax Error: ' + FLua.L.ToString);
    lleErrorMemory:
      DebugWriteLine('Lua Memory Error');
    lleErrorGCMM:
      DebugWriteLine('Lua Garbage Collector Error');
  end;
  FLua.L.SetGlobal('update');
  FLuaValid := Err = lleOK;
end;

{ TBotModule }

procedure TBotModule.AttachLua;
begin
  RegisterLuaMethod(FLua.L, 'getHealth', LuaGetHealth);
  RegisterLuaMethod(FLua.L, 'getMaxHealth', LuaGetMaxHealth);
  RegisterLuaMethod(FLua.L, 'getName', LuaGetName);
  RegisterLuaMethod(FLua.L, 'setName', LuaSetName);
  RegisterLuaMethod(FLua.L, 'getHealthPercentage', LuaGetHealthPercentage);
end;

procedure TBotModule.DetachLua;
begin
  FLua.L.PushNil;
  FLua.L.SetField('getHealth', -2);
  FLua.L.PushNil;
  FLua.L.SetField('getMaxHealth', -2);
  FLua.L.PushNil;
  FLua.L.SetField('getName', -2);
  FLua.L.PushNil;
  FLua.L.SetField('setName', -2);
  FLua.L.PushNil;
  FLua.L.SetField('getHealthPercentage', -2);
end;

constructor TBotModule.Create(AParent: TBotCore; ASide: TBasicDir3);
begin
  inherited Create;
  FLua := AParent.FLua;
  FParent := AParent;
  FSide := ASide;
  Location.Parent := FParent.Location;
  Location.Pos := Vec3Dir[ASide];
  {
  FLua.L.GetGlobal('modules');
  FLua.L.GetField(PPAnsiChar(@BasicPosNames[FSide])^, 1);

  AttachLua;

  FLua.L.Top := 0;
  }
end;

destructor TBotModule.Destroy;
begin
  {
  FLua.L.GetGlobal('modules');
  FLua.L.GetField(PPAnsiChar(@BasicPosNames[FSide])^, 1);

  DetachLua;

  FLua.L.Top := 0;
  }
  inherited;
end;

procedure TBotModule.Update(ADeltaTime: Single);
begin
  inherited;
end;

{ TBotCore.TRenderableIterator }

constructor TBotCore.TRenderableIterator.Create(ABotCore: TBotCore);
begin
  FBotCore := ABotCore;
  FCurrent := Low(TBasicDir);
end;

function TBotCore.TRenderableIterator.MoveNext: Boolean;
begin
  repeat
    if FCurrent = High(TBasicDir3) then
      Exit(False);
    Inc(FCurrent);
  until Current <> nil;
  Result := True;
end;

function TBotCore.TRenderableIterator.GetCurrent: IRenderable;
begin
  Result := FBotCore.FModules[FCurrent];
end;

{ TBotCore.TRenderableIterable }

constructor TBotCore.TRenderableIterable.Create(ABotCore: TBotCore);
begin
  FBotCore := ABotCore;
end;

function TBotCore.TRenderableIterable.GetEnumerator: IIterator<IRenderable>;
begin
  Result := TRenderableIterator.Create(FBotCore);
end;

{ TBotCore }

class function TBotCore.GetModelParams: TResCubeVAOParams;
begin
  Result := TResCubeVAOParams.Create;
  Result.Texture := 'holed_ironplating';
end;

function TBotCore.GetModule(ASide: TBasicDir3): TBotModule;
begin
  Result := FModules[ASide];
end;

procedure TBotCore.RegisterLuaModulesTable;
var
  Name: AnsiString;
begin
  FLua.L.CreateTable(0, 6);

  for Name in BasicPrepositionNames do
  begin
    FLua.L.NewTable;
    FLua.L.SetField(PPAnsiChar(@Name)^, 1);
  end;

  FLua.L.SetGlobal('modules');
end;

class function TBotCore.GetSourceVAO: TVAO;
begin
  Result := TResCubeVAO.Make(GetModelParams);
end;

class procedure TBotCore.FreeSourceVAO;
begin
  TResCubeVAO.Release(GetModelParams);
end;

class function TBotCore.GetInitialHealth: Single;
begin
  Result := 100;
end;

class function TBotCore.GetInitialName: AnsiString;
begin
  Result := 'Basic Bot';
end;

constructor TBotCore.Create(FLua: TLua);
begin
  inherited Create(FLua);

  RegisterLuaModulesTable;
end;

destructor TBotCore.Destroy;
var
  Side: TBasicDir3;
begin
  for Side := Low(TBasicDir3) to High(TBasicDir3) do
    if Modules[Side] <> nil then
      DetachModule(Side);

  inherited;
end;

procedure TBotCore.Update(ADeltaTime: Single);
begin
  inherited;
  UpdateModules(ADeltaTime);
end;

procedure TBotCore.UpdateLua;
begin
  inherited;
end;

procedure TBotCore.AttachModule(ASide: TBasicDir3; AModuleClass: TBotModuleClass);
begin
  if FModules[ASide] <> nil then
    raise Exception.Create('Only one Module can be attached on a single side!');
  FModules[ASide] := AModuleClass.Create(Self, ASide);
end;

procedure TBotCore.DetachModule(ASide: TBasicDir3);
begin
  if FModules[ASide] = nil then
    raise Exception.Create('No module to detach on Bot!');
  FreeAndNil(FModules[ASide]);
end;

function TBotCore.RenderableChildren: IIterable<IRenderable>;
begin
  Result := TRenderableIterable.Create(Self);
end;

procedure TBotCore.UpdateModules(ADeltaTime: Single);
var
  Module: TBotModule;
begin
  for Module in FModules do
  begin
    if Module <> nil then
      Module.Update(ADeltaTime);
  end;
end;

end.
