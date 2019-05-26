unit Pengine.Factorio.General;

interface

uses
  System.SysUtils,

  GdiPlus,

  Pengine.ICollections,
  Pengine.LuaHeader,
  Pengine.Lua;

type

  TFactorio = class;

  TFactorio = class
  public type

    TPrototypeClass = class of TPrototype;

    TPrototype = class
    public type

      TType = (
        ptItem,
        ptRecipe,
        ptAssemblingMachine,
        ptTransportBelt,
        ptInserter
        );

    private
      FFactorio: TFactorio;
      FName: AnsiString;
      FIconPath: string;
      FIcon: IGPBitmap;

      function GetIcon: IGPBitmap;

    public
      constructor Create(AFactorio: TFactorio; L: TLuaState); virtual;
      class function CreateTyped(AFactorio: TFactorio; L: TLuaState): TPrototype;

      class function GetName: AnsiString;
      class function GetType: TType; virtual; abstract;

      property Factorio: TFactorio read FFactorio;
      property Name: AnsiString read FName;

      property IconPath: string read FIconPath;
      property Icon: IGPBitmap read GetIcon;

    end;

    TItem = class(TPrototype)
    private
      FStackSize: Integer;

    public
      constructor Create(AFactorio: TFactorio; L: TLuaState); override;

      class function GetType: TPrototype.TType; override;

      property StackSize: Integer read FStackSize;

    end;

    TRecipe = class(TPrototype)
    public type

      TItem = class
      private
        FName: AnsiString;
        FAmount: Integer;
        FIsFluid: Boolean;

      public
        constructor Create(L: TLuaState);

        property Name: AnsiString read FName;
        property Amount: Integer read FAmount;
        property IsFluid: Boolean read FIsFluid;

      end;

      TIngredient = class(TItem);

      TResult = class(TItem)
      private
        FProbability: Single;

      public
        constructor Create(L: TLuaState);

        property Probabiliy: Single read FProbability;

      end;

    private
      FEnergyRequired: Single;
      FIngredients: IObjectList<TIngredient>;
      FResults: IObjectList<TResult>;

      function GetIngredients: IReadonlyList<TIngredient>;
      function GetResults: IReadonlyList<TResult>;

    public
      constructor Create(AFactorio: TFactorio; L: TLuaState); override;

      class function GetType: TPrototype.TType; override;

      property EnergyRequired: Single read FEnergyRequired;
      property Ingredients: IReadonlyList<TIngredient> read GetIngredients;
      property Results: IReadonlyList<TResult> read GetResults;

    end;

    TAssemblingMachine = class(TPrototype)
    private
      FCraftingSpeed: Single;

    public
      constructor Create(AFactorio: TFactorio; L: TLuaState); override;

      class function GetType: TPrototype.TType; override;

      property CraftingSpeed: Single read FCraftingSpeed;

    end;

    TTransportBelt = class(TPrototype)
    private
      FSpeed: Single;

    public
      constructor Create(AFactorio: TFactorio; L: TLuaState); override;

      class function GetType: TPrototype.TType; override;

      property Speed: Single read FSpeed;

    end;

    TInserter = class(TPrototype)
    private
      FRotationSpeed: Single;
      FExtensionSpeed: Single;

    public
      constructor Create(AFactorio: TFactorio; L: TLuaState); override;

      class function GetType: TPrototype.TType; override;

      property RotationSpeed: Single read FRotationSpeed;
      property ExtensionSpeed: Single read FExtensionSpeed;

    end;

  public const

    PrototypeClasses: array [TPrototype.TType] of TPrototypeClass = (
      TItem,
      TRecipe,
      TAssemblingMachine,
      TTransportBelt,
      TInserter
      );

    PrototypeNames: array [TPrototype.TType] of AnsiString = (
      'item',
      'recipe',
      'assembling-machine',
      'transport-belt',
      'inserter'
      );

  private
    FLua: TLua;
    FExpensive: Boolean;
    FPrototypes: array [TPrototype.TType] of IObjectList<TPrototype>;

    function GetAssemblingMachine: IReadonlyList<TAssemblingMachine>;
    function GetItem: IReadonlyList<TItem>;
    function GetRecipe: IReadonlyList<TRecipe>;
    function GetInserter: IReadonlyList<TInserter>;
    function GetTransportBelt: IReadonlyList<TTransportBelt>;

  public
    constructor Create(AExpensive: Boolean = False);
    destructor Destroy; override;

    property Expensive: Boolean read FExpensive;

    property Item: IReadonlyList<TItem> read GetItem;
    property Recipe: IReadonlyList<TRecipe> read GetRecipe;
    property AssemblingMachine: IReadonlyList<TAssemblingMachine> read GetAssemblingMachine;
    property TransportBelt: IReadonlyList<TTransportBelt> read GetTransportBelt;
    property Inserter: IReadonlyList<TInserter> read GetInserter;

    function List<T: TPrototype>: IReadonlyList<T>;
    function Find<T: TPrototype>(AName: AnsiString): T;

  end;

implementation

{ TFactorio }

constructor TFactorio.Create(AExpensive: Boolean);
const
  InitCode: PAnsiChar =
    'defines = setmetatable({}, {__index = function(t) return t end})'#10 +
    'local paths = {'#10 +
    '  package.path,'#10 +
    '  ";data/core/lualib/?.lua",'#10 +
    '  ";data/core/?.lua",'#10 +
    '  ";data/base/lualib/?.lua",'#10 +
    '  ";data/base/?.lua"'#10 +
    '}'#10 +
    'package.path = table.concat(paths, ";") '#10 +
    'require "dataloader"'#10 +
    'data.is_demo = false'#10 +
    'require "data.core.data"'#10 +
    'require "data.base.data"';

var
  ErrorMessage: PAnsiChar;
  Prototype: TPrototype;
  T: TPrototype.TType;
  PrototypeName: AnsiString;
begin
  FExpensive := AExpensive;

  FLua := TLua.Create;
  FLua.L.LOpenLibs;
  if FLua.L.LDoString(InitCode) then
  begin
    ErrorMessage := FLua.L.ToString;
    FLua.L.Pop;
    raise Exception.Create(string(AnsiString(ErrorMessage)));
  end;

  for T := Low(TPrototype.TType) to High(TPrototype.TType) do
    FPrototypes[T] := TObjectList<TPrototype>.Create;

  FLua.L.GetGlobal('data');
  FLua.L.GetField('raw');
  FLua.L.PushNil;
  while FLua.L.Next(-2) do
  begin
    PrototypeName := FLua.L.ToString(-2);
    for T := Low(TPrototype.TType) to High(TPrototype.TType) do
    begin
      if PrototypeName <> PrototypeNames[T] then
        Continue;
      FLua.L.PushNil;
      while FLua.L.Next(-2) do
      begin
        Prototype := PrototypeClasses[T].Create(Self, FLua.L);
        FPrototypes[T].Add(Prototype);
        FLua.L.Pop;
      end;
      Break;
    end;
    FLua.L.Pop;
  end;

  FLua.L.Pop(2);
end;

destructor TFactorio.Destroy;
begin
  FLua.Free;
  inherited;
end;

function TFactorio.Find<T>(AName: AnsiString): T;
var
  Prototype: TPrototype;
begin
  for Prototype in FPrototypes[T.GetType] do
    if Prototype.Name = AName then
      Exit(T(Prototype));
  raise Exception.Create('prototype not found');
end;

function TFactorio.GetAssemblingMachine: IReadonlyList<TAssemblingMachine>;
begin
  Result := IReadonlyList<TAssemblingMachine>(FPrototypes[ptAssemblingMachine].ReadonlyList);
end;

function TFactorio.GetInserter: IReadonlyList<TInserter>;
begin
  Result := IReadonlyList<TInserter>(FPrototypes[ptInserter].ReadonlyList);
end;

function TFactorio.GetItem: IReadonlyList<TItem>;
begin
  Result := IReadonlyList<TItem>(FPrototypes[ptItem].ReadonlyList);
end;

function TFactorio.GetRecipe: IReadonlyList<TRecipe>;
begin
  Result := IReadonlyList<TRecipe>(FPrototypes[ptRecipe].ReadonlyList);
end;

function TFactorio.GetTransportBelt: IReadonlyList<TTransportBelt>;
begin
  Result := IReadonlyList<TTransportBelt>(FPrototypes[ptTransportBelt].ReadonlyList);
end;

function TFactorio.List<T>: IReadonlyList<T>;
begin
  Result := IReadonlyList<T>(FPrototypes[T.GetType].ReadonlyList);
end;

{ TFactorio.TPrototype }

constructor TFactorio.TPrototype.Create(AFactorio: TFactorio; L: TLuaState);
begin
  FFactorio := AFactorio;
  L.GetField('name');
  FName := L.ToString;
  L.Pop;

  // if L.GetField('icon') = ltString then
  //  FIcon := L.ToString.Replace('__base__', blabla);
  // else
  FIconPath := string('data\base\graphics\icons\' + FName + '.png');
end;

class function TFactorio.TPrototype.CreateTyped(AFactorio: TFactorio; L: TLuaState): TPrototype;
var
  Prototype: AnsiString;
  T: TType;
begin
  L.GetField('type');
  Prototype := L.ToString;
  for T := Low(TType) to High(TType) do
    if Prototype = PrototypeNames[T] then
    begin
      L.Pop;
      Exit(PrototypeClasses[T].Create(AFactorio, L));
    end;
  L.Pop;
  Result := nil;
end;

function TFactorio.TPrototype.GetIcon: IGPBitmap;
begin
  if FIcon = nil then
  begin
    if FileExists(FIconPath) then
      FIcon := TGPBitmap.Create(FIconPath)
    else
      FIcon := TGPBitmap.Create(1, 1);
  end;
  Result := FIcon;
end;

class function TFactorio.TPrototype.GetName: AnsiString;
begin
  Result := PrototypeNames[GetType];
end;

{ TFactorio.TItem }

class function TFactorio.TItem.GetType: TPrototype.TType;
begin
  Result := ptItem;
end;

constructor TFactorio.TItem.Create(AFactorio: TFactorio; L: TLuaState);
begin
  inherited;
  if L.GetField('stack_size') = ltNumber then
    FStackSize := L.ToInteger;
  L.Pop;
end;

{ TFactorio.TRecipe }

constructor TFactorio.TRecipe.Create(AFactorio: TFactorio; L: TLuaState);
var
  Result: TResult;
  ResultCount: Integer;
  ExtraTable: Boolean;
begin
  inherited;

  if Factorio.Expensive then
    ExtraTable := L.GetField('expensive') = ltTable
  else
    ExtraTable := L.GetField('normal') = ltTable;

  if not ExtraTable then
    L.Pop;

  if L.GetField('energy_required') = ltNumber then
    FEnergyRequired := L.ToNumber
  else
    FEnergyRequired := 0.5;
  L.Pop;

  FIngredients := TObjectList<TIngredient>.Create;
  if L.GetField('ingredients') = ltTable then
  begin
    L.PushNil;
    while L.Next(-2) do
    begin
      FIngredients.Add(TIngredient.Create(L));
      L.Pop;
    end;
  end;
  L.Pop;

  FResults := TObjectList<TResult>.Create;
  if L.GetField('result') = ltString then
    FResults.Add(TResult.Create(L));
  L.Pop;

  if L.GetField('results') = ltTable then
  begin
    L.PushNil;
    while L.Next(-2) do
    begin
      FResults.Add(TResult.Create(L));
      L.Pop;
    end;
  end;
  L.Pop;

  if L.GetField('result_count') = ltNumber then
  begin
    ResultCount := L.ToInteger;
    for Result in FResults do
      Result.FAmount := Result.FAmount * ResultCount;
  end;
  L.Pop;

  if ExtraTable then
    L.Pop;
end;

function TFactorio.TRecipe.GetIngredients: IReadonlyList<TIngredient>;
begin
  Result := FIngredients.ReadonlyList;
end;

function TFactorio.TRecipe.GetResults: IReadonlyList<TResult>;
begin
  Result := FResults.ReadonlyList;
end;

class function TFactorio.TRecipe.GetType: TPrototype.TType;
begin
  Result := ptRecipe;
end;

{ TFactorio.TAssemblingMachine }

constructor TFactorio.TAssemblingMachine.Create(AFactorio: TFactorio; L: TLuaState);
begin
  inherited;

  if L.GetField('crafting_speed') = ltNumber then
    FCraftingSpeed := L.ToNumber;
  L.Pop;
end;

class function TFactorio.TAssemblingMachine.GetType: TPrototype.TType;
begin
  Result := ptAssemblingMachine;
end;

{ TFactorio.TTransportBelt }

constructor TFactorio.TTransportBelt.Create(AFactorio: TFactorio; L: TLuaState);
begin
  inherited;

  if L.GetField('speed') = ltNumber then
    FSpeed := L.ToNumber;
  L.Pop;
end;

class function TFactorio.TTransportBelt.GetType: TPrototype.TType;
begin
  Result := ptTransportBelt;
end;

{ TFactorio.TInserter }

constructor TFactorio.TInserter.Create(AFactorio: TFactorio; L: TLuaState);
begin
  inherited;

  if L.GetField('rotation_speed') = ltNumber then
    FRotationSpeed := L.ToNumber;
  L.Pop;

  if L.GetField('extension_speed') = ltNumber then
    FExtensionSpeed := L.ToNumber;
  L.Pop;
end;

class function TFactorio.TInserter.GetType: TPrototype.TType;
begin
  Result := ptInserter;
end;

{ TFactorio.TRecipe.TItem }

constructor TFactorio.TRecipe.TItem.Create(L: TLuaState);
begin
  if L.GetI(1) = ltString then
  begin
    FName := L.ToString;
    L.Pop;
    L.GetI(2);
    FAmount := L.ToInteger;
    L.Pop;
  end
  else
  begin
    L.Pop;
    L.GetField('type');
    FIsFluid := L.ToString = 'fluid';
    L.Pop;
    L.GetField('name');
    FName := L.ToString;
    L.Pop;
    L.GetField('amount');
    FAmount := L.ToInteger;
    L.Pop;
  end;
end;

{ TFactorio.TRecipe.TResult }

constructor TFactorio.TRecipe.TResult.Create(L: TLuaState);
begin
  if L.&Type = ltString then
  begin
    FName := L.ToString;
    FAmount := 1;
    FProbability := 1;
  end
  else
  begin
    inherited;
    if L.GetField('probability') = ltNumber then
      FProbability := L.ToNumber
    else
      FProbability := 1;
    L.Pop;
  end;
end;

end.
