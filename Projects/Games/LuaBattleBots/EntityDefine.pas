unit EntityDefine;

interface

uses
  VAOManager, LuaHeader, Math, LuaConf, VectorGeometry, DebugConsoleDefine, IntegerMaths, SysUtils, ResourceManager,
  Lists, Camera;

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
    class function GetInitialHealth: Single; virtual; abstract;
    class function GetInitialName: AnsiString; virtual; abstract;

    procedure SetMaxHealth(Value: Single);
    procedure SetName(Value: AnsiString);

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
    FLua: TLuaState;
    FLuaValid: Boolean;

    class function GetSelf(L: TLuaState): TLuaEntity; static;

    class function LuaGetHealth(L: TLuaState): Integer; static; cdecl;
    class function LuaGetMaxHealth(L: TLuaState): Integer; static; cdecl;
    class function LuaGetName(L: TLuaState): Integer; static; cdecl;
    class function LuaSetName(L: TLuaState): Integer; static; cdecl;
    class function LuaGetHealthPercentage(L: TLuaState): Integer; static; cdecl;

    class function LuaPrint(L: TLuaState): Integer; static; cdecl;

  protected

  public
    constructor Create;
    destructor Destroy; override;

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

    TRenderableIterable = class(TRefCountedIterable<IRenderable>)
    private
      FBotCore: TBotCore;
    public
      constructor Create(ABotCore: TBotCore);
      function GetEnumerator: IIterator<IRenderable>; override;
    end;

  private
    FModules: array [TBasicDir3] of TBotModule;

    function GetModule(ASide: TBasicDir3): TBotModule;

  protected
    class function GetSourceVAO: TVAO; override;
    class function GetInitialHealth: Single; override;
    class function GetInitialName: AnsiString; override;

  public
    constructor Create;
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

constructor TEntity.Create();
begin
  inherited Create(GetSourceVAO);
  FHealth := GetInitialHealth;
  FMaxHealth := GetInitialHealth;
  FName := GetInitialName;
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

class function TLuaEntity.LuaGetName(L: TLuaState): Integer;
var
  Self: TLuaEntity;
begin
  Self := GetSelf(L);

  L.CheckEnd(1);

  L.PushString(PPAnsiChar(@Self.Name)^);

  Result := 1;
end;

class function TLuaEntity.LuaSetName(L: TLuaState): Integer;
var
  Self: TLuaEntity;
begin
  Self := GetSelf(L);

  L.CheckType(1, ltString);
  L.CheckEnd(2);

  Self.Name := AnsiString(L.ToString);

  Result := 0;
end;

class function TLuaEntity.LuaGetHealthPercentage(L: TLuaState): Integer;
var
  Self: TLuaEntity;
begin
  Self := GetSelf(L);

  L.CheckEnd(1);

  L.PushNumber(Self.Health / Self.MaxHealth * 100);

  Result := 1;
end;

class function TLuaEntity.LuaPrint(L: TLuaState): Integer;
var
  I: Integer;
  Self: TLuaEntity;
begin
  Self := GetSelf(L);
  DebugWrite(Self.Name + ': ');
  for I := 1 to L.Top do
  begin
    DebugWrite(string(L.ToString(I)));
    if I < L.Top then
      DebugWrite(' ');
  end;
  DebugWriteLine;
  Result := 0;
end;

constructor TLuaEntity.Create;
begin
  inherited Create;

  // Lua Init
  FLua := NewLuaState;
  PPointer(FLua.GetExtraSpace)^ := Self;

  // Base Lua Functions
  FLua.Register('print', LuaPrint);
  FLua.Register('getHealth', LuaGetHealth);
  FLua.Register('getMaxHealth', LuaGetMaxHealth);
  FLua.Register('getName', LuaGetName);
  FLua.Register('setName', LuaSetName);
  FLua.Register('getHealthPercentage', LuaGetHealthPercentage);
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
        DebugWriteLine(Name + ' Runtime Error: ' + FLua.ToString);
      lceErrorMemory:
        DebugWriteLine('Lua Memory Error');
      lceErrorGCMM:
        DebugWriteLine('Lua GarbageCollector Error');
      lceErrorError:
        DebugWriteLine('Lua Error-Function Error');
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
      DebugWriteLine(Name + ' Syntax Error: ' + FLua.ToString);
    lleErrorMemory:
      DebugWriteLine('Lua Memory Error');
    lleErrorGCMM:
      DebugWriteLine('Lua Garbage Collector Error');
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
  Location.Parent := FParent.Location;
  Location.Pos := VecDir[ASide];
end;

destructor TBotModule.Destroy;
begin
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

function TBotCore.GetModule(ASide: TBasicDir3): TBotModule;
begin
  Result := FModules[ASide];
end;

class function TBotCore.GetSourceVAO: TVAO;
begin
  Result := TResCubeVAO.Make;
end;

class function TBotCore.GetInitialHealth: Single;
begin
  Result := 100;
end;

class function TBotCore.GetInitialName: AnsiString;
begin
  Result := 'Basic Bot';
end;

constructor TBotCore.Create;
begin
  inherited;
end;

destructor TBotCore.Destroy;
var
  Side: TBasicDir3;
begin
  for Side := Low(TBasicDir3) to High(TBasicDir3) do
    if Modules[Side] <> nil then
      DetachModule(Side);
  SourceVAO.Free;
  inherited;
end;

procedure TBotCore.Update(ADeltaTime: Single);
var
  Module: TBotModule;
begin
  inherited;
  // Update Core

  // TODO: Update Modules extract to procedure, not-starter-edition has refactoring... xD
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

end.
