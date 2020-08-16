unit Pengine.Factorio.General;

interface

uses
  System.SysUtils,
  System.IOUtils,
  System.Math,
  System.Win.Registry,
  System.RegularExpressions,
  
  Winapi.Windows,

  GdiPlus,

  Pengine.JSON,
  Pengine.JSON.Serialization,
  Pengine.Interfaces,
  Pengine.ICollections,
  Pengine.Lua.Header,
  Pengine.IntMaths;

type

  EFactorio = class(Exception);

  TFactorio = class
  public type

    TPrototypeClass = class of TPrototype;

    TPrototype = class
    public type

      TType = (
        // Grouping
        ptItemGroup,
        ptItemSubgroup,

        // Recipe
        ptRecipe,
        ptRecipeCategory,

        // Item
        ptAmmo,
        ptArmor,
        ptBlueprint,
        ptBlueprintBook,
        ptCapsule,
        ptCopyPasteTool,
        ptDeconstructionItem,
        ptEquipment,
        ptGun,
        ptItem,
        ptItemWithEntityData,
        ptMiningTool,
        ptModule,
        ptRailPlanner,
        ptRepairTool,
        ptSelectionTool,
        ptSpidertronRemote,
        ptTool,
        ptUpgradeItem,

        // Fluid
        ptFluid,

        // CraftingMachines
        ptAssemblingMachine,
        ptFurnace,
        ptRocketSilo,

        // Other
        ptInserter,
        ptTransportBelt
        );

    public const

      ItemTypes = [ptAmmo .. ptUpgradeItem];

      ItemOrFluidTypes = [ptAmmo .. ptFluid];

      GroupedTypes = [ptAmmo .. ptFluid];

      CraftingMachines = [ptAssemblingMachine .. ptRocketSilo];

    private
      FFactorio: TFactorio;
      FName: AnsiString;
      FOrder: AnsiString;
      FIconSize: Integer;
      FIconPath: string;
      FIconMipmaps: IList<IGPBitmap>;
      FIconMipmapCount: Integer;
      FFlags: ISet<AnsiString>;
      FHidden: Boolean;

      function GenerateMissingno(AMipmapLayer: Integer): IGPBitmap;
      function GetDisplayName: AnsiString;

      function GetIcon: IGPBitmap;
      function GetFlags: IReadonlySet<AnsiString>;

    protected
      function GetIconMipmaps: IReadonlyList<IGPBitmap>; virtual;
      function GetOrder: AnsiString; virtual;

    public
      constructor Create(AFactorio: TFactorio; L: TLuaState); virtual;
      class function CreateTyped(AFactorio: TFactorio; L: TLuaState): TPrototype;

      class function GetTypeName: AnsiString;
      class function GetType: TType; virtual; abstract;

      property Factorio: TFactorio read FFactorio;

      property Name: AnsiString read FName;
      property DisplayName: AnsiString read GetDisplayName;
      property Order: AnsiString read GetOrder;

      property IconSize: Integer read FIconSize;
      property IconPath: string read FIconPath;
      property Icon: IGPBitmap read GetIcon;
      property IconMipmaps: IReadonlyList<IGPBitmap> read GetIconMipmaps;

      function IconMipmapFor(ASize: Cardinal): IGPBitmap;

      property Flags: IReadonlySet<AnsiString> read GetFlags;
      property Hidden: Boolean read FHidden;

    end;

    TItemSubgroup = class;

    TItemGroup = class(TPrototype)
    private
      FOrderInRecipe: AnsiString;
      FSubgroups: ISortedList<TItemSubgroup>;

      function GetSubgroups: IReadonlyList<TItemSubgroup>;

    public
      constructor Create(AFactorio: TFactorio; L: TLuaState); override;

      class function GetType: TPrototype.TType; override;

      property OrderInRecipe: AnsiString read FOrderInRecipe;
      property Subgroups: IReadonlyList<TItemSubgroup> read GetSubgroups;

    end;

    TGrouped = class;
    TRecipe = class;

    TItemSubgroup = class(TPrototype)
    private
      FEntries: ISortedList<TGrouped>;
      FRecipes: ISortedList<TRecipe>;
      FGroupName: AnsiString;
      FGroup: TItemGroup;

      function GetEntries: IReadonlyList<TGrouped>;
      function GetGroup: TItemGroup;
      function GetRecipes: IReadonlyList<TRecipe>;

    public
      constructor Create(AFactorio: TFactorio; L: TLuaState); override;

      class function GetType: TPrototype.TType; override;

      property Group: TItemGroup read GetGroup;
      property Entries: IReadonlyList<TGrouped> read GetEntries;
      property Recipes: IReadonlyList<TRecipe> read GetRecipes;

    end;

    TGrouped = class(TPrototype)
    private
      FSubgroup: TItemSubgroup;
      FSubgroupName: AnsiString;

      function GetGroup: TItemGroup;
      function GetSubgroup: TItemSubgroup;

    public
      constructor Create(AFactorio: TFactorio; L: TLuaState); override;

      property Group: TItemGroup read GetGroup;
      property Subgroup: TItemSubgroup read GetSubgroup;
      property SubgroupName: AnsiString read FSubgroupName;

    end;

    TRecipeCategory = class(TPrototype)
    public
      class function GetType: TPrototype.TType; override;

    end;

    TItemOrFluid = class;

    TCraftingMachine = class;

    TRecipe = class(TPrototype)
    public type

      TItemStack = class
      private
        FFactorio: TFactorio;
        FItem: TItemOrFluid;
        FName: AnsiString;
        FAmount: Integer;
        FIsFluid: Boolean;

        function GetItem: TItemOrFluid;

      public
        constructor Create(AFactorio: TFactorio; L: TLuaState);

        property Factorio: TFactorio read FFactorio;

        property Name: AnsiString read FName;
        property Item: TItemOrFluid read GetItem;

        property Amount: Integer read FAmount;
        property IsFluid: Boolean read FIsFluid;

      end;

      TIngredient = class(TItemStack);

      TResult = class(TItemStack)
      private
        FProbability: Single;

      public
        constructor Create(AFactorio: TFactorio; L: TLuaState);

        property Probability: Single read FProbability;

      end;

    private
      FEnergyRequired: Single;
      FIngredients: IObjectList<TIngredient>;
      FResults: IObjectList<TResult>;
      FCategoryName: AnsiString;
      FCategory: TRecipeCategory;

      function GetIngredients: IReadonlyList<TIngredient>;
      function GetResults: IReadonlyList<TResult>;

      function GetCategory: TRecipeCategory;
      function GetGroup: TItemGroup;
      function GetSubgroup: TItemSubgroup;

    protected
      function GetIconMipmaps: IReadonlyList<IGPBitmap>; override;
      function GetOrder: AnsiString; override;

    public
      constructor Create(AFactorio: TFactorio; L: TLuaState); override;

      class function GetType: TPrototype.TType; override;

      property EnergyRequired: Single read FEnergyRequired;
      property Ingredients: IReadonlyList<TIngredient> read GetIngredients;
      property Results: IReadonlyList<TResult> read GetResults;
      function ResultHidden: Boolean;

      property CategoryName: AnsiString read FCategoryName;
      property Category: TRecipeCategory read GetCategory;
      property Group: TItemGroup read GetGroup;
      property Subgroup: TItemSubgroup read GetSubgroup;

      function FindCraftingMachine: TCraftingMachine;

    end;

    TItemOrFluid = class(TGrouped)
    public
      function FindRecipe: TRecipe;

    end;

    TItem = class abstract(TItemOrFluid)
    private
      FStackSize: Integer;

    public
      constructor Create(AFactorio: TFactorio; L: TLuaState); override;

      class function GetType: TPrototype.TType; override;

      property StackSize: Integer read FStackSize;

    end;

    TAmmo = class(TItem)
    public
      class function GetType: TPrototype.TType; override;

    end;

    TArmor = class(TItem)
    public
      class function GetType: TPrototype.TType; override;

    end;

    TCapsule = class(TItem)
    public
      class function GetType: TPrototype.TType; override;

    end;

    TEquipment = class(TItem)
    public
      class function GetType: TPrototype.TType; override;

    end;

    TGun = class(TItem)
    public
      class function GetType: TPrototype.TType; override;

    end;

    TItemWithEntityData = class(TItem)
    public
      class function GetType: TPrototype.TType; override;

    end;

    TMiningTool = class(TItem)
    public
      class function GetType: TPrototype.TType; override;

    end;

    TModule = class(TItem)
    public
      class function GetType: TPrototype.TType; override;

    end;

    TRailPlanner = class(TItem)
    public
      class function GetType: TPrototype.TType; override;

    end;

    TRepairTool = class(TItem)
    public
      class function GetType: TPrototype.TType; override;

    end;

    TTool = class(TItem)
    public
      class function GetType: TPrototype.TType; override;

    end;

    TFluid = class(TItemOrFluid)
    public
      constructor Create(AFactorio: TFactorio; L: TLuaState); override;

      class function GetType: TPrototype.TType; override;

    end;

    TCraftingMachine = class(TPrototype)
    private
      FCraftingSpeed: Single;
      FCraftingCategoryNames: IList<AnsiString>;
      FCraftingCategories: ISortedList<TRecipeCategory>;

      function GetCraftingCategories: IReadonlyList<TRecipeCategory>;

    protected
      function GetOrder: AnsiString; override;

    public
      constructor Create(AFactorio: TFactorio; L: TLuaState); override;

      property CraftingSpeed: Single read FCraftingSpeed;
      property CraftingCategories: IReadonlyList<TRecipeCategory> read GetCraftingCategories;

      function CanCraft(ARecipe: TRecipe): Boolean;

    end;

    TAssemblingMachine = class(TCraftingMachine)
    public
      class function GetType: TPrototype.TType; override;

    end;

    TRocketSilo = class(TAssemblingMachine)
    public
      class function GetType: TPrototype.TType; override;

    end;

    TFurnace = class(TCraftingMachine)
    public
      class function GetType: TPrototype.TType; override;

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

    TTransportBelt = class(TPrototype)
    private
      FSpeed: Single;

      function GetItemsPerSecond: Single;

    public
      constructor Create(AFactorio: TFactorio; L: TLuaState); override;

      class function GetType: TPrototype.TType; override;

      property Speed: Single read FSpeed;
      property ItemsPerSecond: Single read GetItemsPerSecond;

    end;

    TBlueprint = class(TItem)
    public
      class function GetType: TPrototype.TType; override;

    end;

    TBlueprintBook = class(TItem)
    public
      class function GetType: TPrototype.TType; override;

    end;

    TCopyPasteTool = class(TItem)
    public
      class function GetType: TPrototype.TType; override;

    end;

    TDeconstructionItem = class(TItem)
    public
      class function GetType: TPrototype.TType; override;

    end;

    TSelectionTool = class(TItem)
    public
      class function GetType: TPrototype.TType; override;

    end;

    TSpidertronRemote = class(TItem)
    public
      class function GetType: TPrototype.TType; override;

    end;

    TUpgradeItem = class(TItem)
    public
      class function GetType: TPrototype.TType; override;

    end;

    {
      "name": "angelsaddons-warehouses",
      "version": "0.4.1",
      "factorio_version": "0.17",
      "title": "Angel's Addons - Warehouses",
      "author": "Arch666Angel",
      "contact": "",
      "homepage": "https://forums.factorio.com/viewtopic.php?f=185&t=30962",
      "description": "Adds warehouses to the game.",
      "dependencies": [
      "base >= 0.17.0",
      "? angelsrefining >= 0.3.0"
      ]
    }

    TMod = class(TInterfaceBase, IJSerializable)
    private
      FPath: string;
      FName: string;
      FVersion: string;
      FFactorioVersion: string;
      FTitle: string;
      FAuthor: string;
      FContact: string;
      FHomepage: string;
      FDescription: string;
      FDependencies: IList<string>;

      function GetDependencies: IReadonlyList<string>;

      procedure ReadInfoFile;

    public
      constructor Create(APath: string);

      property Path: string read FPath;

      property Name: string read FName;
      property Version: string read FVersion;
      property FactorioVersion: string read FFactorioVersion;
      property Title: string read FTitle;
      property Author: string read FAuthor;
      property Contact: string read FAuthor;
      property Homepage: string read FHomepage;
      property Description: string read FDescription;
      property Dependencies: IReadonlyList<string> read GetDependencies;

      function GetJVersion: Integer;
      procedure DefineJStorage(ASerializer: TJSerializer);

    end;

  public const

    PrototypeClasses: array [TPrototype.TType] of TPrototypeClass = (
      // Grouping
      TItemGroup,
      TItemSubgroup,

      // Recipe
      TRecipe,
      TRecipeCategory,

      // Item
      TAmmo,
      TArmor,
      TBlueprint,
      TBlueprintBook,
      TCapsule,
      TCopyPasteTool,
      TDeconstructionItem,
      TEquipment,
      TGun,
      TItem,
      TItemWithEntityData,
      TMiningTool,
      TModule,
      TRailPlanner,
      TRepairTool,
      TSelectionTool,
      TSpidertronRemote,
      TTool,
      TUpgradeItem,

      // Fluid
      TFluid,

      // CraftingMachine
      TAssemblingMachine,
      TFurnace,
      TRocketSilo,

      // Other
      TInserter,
      TTransportBelt
      );

    PrototypeNames: array [TPrototype.TType] of AnsiString = (
      // Grouping
      'item-group',
      'item-subgroup',

      // Recipe
      'recipe',
      'recipe-category',

      // Item
      'ammo',
      'armor',
      'blueprint',
      'blueprint-book',
      'capsule',
      'copy-paste-tool',
      'deconstruction-item',
      'equipment',
      'gun',
      'item',
      'item-with-entity-data',
      'mining-tool',
      'module',
      'rail-planner',
      'repair-tool',
      'selection-tool',
      'spidertron-remote',
      'tool',
      'upgrade-item',

      // Fluid
      'fluid',

      // CraftingMachine
      'assembling-machine',
      'furnace',
      'rocket-silo',

      // Other
      'inserter',
      'transport-belt'
      );

  private
    FPath: string;
    FAvailableMods: IObjectList<TMod>;
    FActiveMods: IList<TMod>;
    FExpensive: Boolean;
    FPrototypeLists: array [TPrototype.TType] of ISortedObjectList<TPrototype>;
    FPrototypeMaps: array [TPrototype.TType] of IMap<AnsiString, TPrototype>;
    FGroupedMap: IMap<AnsiString, TGrouped>;
    FItemMap: IMap<AnsiString, TItemOrFluid>;
    FCraftingMachineList: ISortedList<TCraftingMachine>;
    FCraftingMachineMap: IMap<AnsiString, TCraftingMachine>;
    
    function GetGrouped: IReadonlyMap<AnsiString, TGrouped>;
    function GetItem: IReadonlyMap<AnsiString, TItemOrFluid>;
    function GetFluidOrder: IReadonlyList<TFluid>;
    function GetFluid: IReadonlyMap<AnsiString, TFluid>;

    function GetCraftingMachineOrder: IReadonlyList<TCraftingMachine>;
    function GetCraftingMachine: IReadonlyMap<AnsiString, TCraftingMachine>;

    function GetRecipeCategoryOrder: IReadonlyList<TRecipeCategory>;
    function GetRecipeCategory: IReadonlyMap<AnsiString, TRecipeCategory>;
    function GetRecipeOrder: IReadonlyList<TRecipe>;
    function GetRecipe: IReadonlyMap<AnsiString, TRecipe>;

    function GetAssemblingMachine: IReadonlyMap<AnsiString, TAssemblingMachine>;
    function GetInserter: IReadonlyMap<AnsiString, TInserter>;
    function GetTransportBelt: IReadonlyMap<AnsiString, TTransportBelt>;

    function GetItemGroupOrder: IReadonlyList<TItemGroup>;
    function GetItemGroup: IReadonlyMap<AnsiString, TItemGroup>;
    function GetItemSubgroup: IReadonlyMap<AnsiString, TItemSubgroup>;
    function GetAssemblingMachineOrder: IReadonlyList<TAssemblingMachine>;
    function GetInserterOrder: IReadonlyList<TInserter>;
    function GetTransportBeltOrder: IReadonlyList<TTransportBelt>;
    function GetActiveMods: IReadonlyList<TMod>;
    function GetAvailableMods: IReadonlyList<TMod>;

  public
    constructor Create;
    procedure Load(AExpensive: Boolean = False);

    class function FindGameDirectory: string; static;

    property AvailableMods: IReadonlyList<TMod> read GetAvailableMods;
    property ActiveMods: IReadonlyList<TMod> read GetActiveMods;

    procedure ActivateMod(AMod: TMod);
    procedure DeactivateMod(AMod: TMod);

    property Expensive: Boolean read FExpensive;

    property ItemGroupOrder: IReadonlyList<TItemGroup> read GetItemGroupOrder;
    property ItemGroup: IReadonlyMap<AnsiString, TItemGroup> read GetItemGroup;
    property ItemSubgroup: IReadonlyMap<AnsiString, TItemSubgroup> read GetItemSubgroup;

    property Grouped: IReadonlyMap<AnsiString, TGrouped> read GetGrouped;
    property Item: IReadonlyMap<AnsiString, TItemOrFluid> read GetItem;
    property FluidOrder: IReadonlyList<TFluid> read GetFluidOrder;
    property Fluid: IReadonlyMap<AnsiString, TFluid> read GetFluid;

    property CraftingMachineOrder: IReadonlyList<TCraftingMachine> read GetCraftingMachineOrder;
    property CraftingMachine: IReadonlyMap<AnsiString, TCraftingMachine> read GetCraftingMachine;

    property RecipeCategoryOrder: IReadonlyList<TRecipeCategory> read GetRecipeCategoryOrder;
    property RecipeCategory: IReadonlyMap<AnsiString, TRecipeCategory> read GetRecipeCategory;
    property RecipeOrder: IReadonlyList<TRecipe> read GetRecipeOrder;
    property Recipe: IReadonlyMap<AnsiString, TRecipe> read GetRecipe;

    property AssemblingMachineOrder: IReadonlyList<TAssemblingMachine> read GetAssemblingMachineOrder;
    property AssemblingMachine: IReadonlyMap<AnsiString, TAssemblingMachine> read GetAssemblingMachine;
    property InserterOrder: IReadonlyList<TInserter> read GetInserterOrder;
    property Inserter: IReadonlyMap<AnsiString, TInserter> read GetInserter;
    property TransportBeltOrder: IReadonlyList<TTransportBelt> read GetTransportBeltOrder;
    property TransportBelt: IReadonlyMap<AnsiString, TTransportBelt> read GetTransportBelt;

    function Order<T: TPrototype>: IReadonlyList<T>; overload;
    function Order(AType: TPrototype.TType): IReadonlyList<TPrototype>; overload;
    function Get<T: TPrototype>: IReadonlyMap<AnsiString, T>; overload;
    function Get(AType: TPrototype.TType): IReadonlyMap<AnsiString, TPrototype>; overload;

    // Replaces __base__ and __core__
    function ExpandPath(APath: string): string;
    class function ComparePrototypes(A, B: TPrototype): Boolean; overload; static;
    class function ComparePrototypes<T: TPrototype>(A, B: T): Boolean; overload; static;

  end;

implementation

{ TFactorio }

procedure TFactorio.ActivateMod(AMod: TMod);
begin
  FActiveMods.Add(AMod);
end;

class function TFactorio.ComparePrototypes(A, B: TPrototype): Boolean;
begin
  Result := A.Order < B.Order;
end;

class function TFactorio.ComparePrototypes<T>(A, B: T): Boolean;
begin
  Result := A.Order < B.Order;
end;

constructor TFactorio.Create;
begin
  FPath := FindGameDirectory;
end;

procedure TFactorio.DeactivateMod(AMod: TMod);
begin
  FActiveMods.Remove(AMod);
end;

function TFactorio.ExpandPath(APath: string): string;
var
  CorePath, BasePath: string;
begin
  CorePath := TPath.Combine(FPath, 'data/core');
  BasePath := TPath.Combine(FPath, 'data/base');
  Result := APath.Replace('__core__', CorePath).Replace('__base__', BasePath);
end;

class function TFactorio.FindGameDirectory: string;
var
  Registry: TRegistry;
  Path, Line: string;
  LibraryRegex: TRegEx;
  Match: TMatch;
begin
  // Find Steam directory in:
  // HKEY_LOCAL_MACHINE\SOFTWARE\Valve\Steam
  // HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Valve\Steam
  Registry := TRegistry.Create(KEY_READ);
  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    Registry.OpenKeyReadOnly('SOFTWARE\Valve\Steam');
    Path := Registry.ReadString('InstallPath');
    Registry.CloseKey;

    if Path.IsEmpty then
    begin
      Registry.OpenKeyReadOnly('SOFTWARE\WOW6432Node\Valve\Steam');
      Path := Registry.ReadString('InstallPath');
      Registry.CloseKey;
    end;

    if Path.IsEmpty then
      raise EFactorio.Create('Could not find Steam directory.');

    LibraryRegex := TRegex.Create('^\s*"\d+"\s*(.+)\s*$');
    for Line in TFile.ReadAllLines(TPath.Combine(Path, 'steamapps\libraryfolders.vdf')) do
    begin
      Match := LibraryRegex.Match(Line);
      if Match.Success then
      begin
        if not TJString.StringParser.Optional(Match.Groups[1].Value, Path) then
          raise EFactorio.Create('Could not parse Steam library path.');
        Path := TPath.Combine(Path, 'steamapps\common\Factorio');
        if TDirectory.Exists(Path) then
          Exit(Path);
      end;
    end;

    raise EFactorio.Create('Factorio does not seem to be installed on Steam.');

  finally
    Registry.Free;
  end;
end;

function TFactorio.GetActiveMods: IReadonlyList<TMod>;
begin

end;

function TFactorio.GetAssemblingMachine: IReadonlyMap<AnsiString, TAssemblingMachine>;
begin
  Result := Get<TAssemblingMachine>;
end;

function TFactorio.GetAssemblingMachineOrder: IReadonlyList<TAssemblingMachine>;
begin
  Result := Order<TAssemblingMachine>;
end;

function TFactorio.GetAvailableMods: IReadonlyList<TMod>;
begin

end;

function TFactorio.GetCraftingMachine: IReadonlyMap<AnsiString, TCraftingMachine>;
begin
  Result := FCraftingMachineMap.ReadonlyMap;
end;

function TFactorio.GetCraftingMachineOrder: IReadonlyList<TCraftingMachine>;
begin
  Result := FCraftingMachineList.ReadonlyList;
end;

function TFactorio.GetFluid: IReadonlyMap<AnsiString, TFluid>;
begin
  Result := Get<TFluid>;
end;

function TFactorio.GetFluidOrder: IReadonlyList<TFluid>;
begin
  Result := Order<TFluid>;
end;

function TFactorio.GetGrouped: IReadonlyMap<AnsiString, TGrouped>;
begin
  Result := FGroupedMap.ReadonlyMap;
end;

function TFactorio.GetInserter: IReadonlyMap<AnsiString, TInserter>;
begin
  Result := Get<TInserter>;
end;

function TFactorio.GetInserterOrder: IReadonlyList<TInserter>;
begin
  Result := Order<TInserter>;
end;

function TFactorio.GetItem: IReadonlyMap<AnsiString, TItemOrFluid>;
begin
  Result := FItemMap.ReadonlyMap;
end;

function TFactorio.GetItemGroup: IReadonlyMap<AnsiString, TItemGroup>;
begin
  Result := Get<TItemGroup>;
end;

function TFactorio.GetItemGroupOrder: IReadonlyList<TItemGroup>;
begin
  Result := Order<TItemGroup>;
end;

function TFactorio.GetItemSubgroup: IReadonlyMap<AnsiString, TItemSubgroup>;
begin
  Result := Get<TItemSubgroup>;
end;

function TFactorio.GetRecipe: IReadonlyMap<AnsiString, TRecipe>;
begin
  Result := Get<TRecipe>;
end;

function TFactorio.GetRecipeCategory: IReadonlyMap<AnsiString, TRecipeCategory>;
begin
  Result := Get<TRecipeCategory>;
end;

function TFactorio.GetRecipeCategoryOrder: IReadonlyList<TRecipeCategory>;
begin
  Result := Order<TRecipeCategory>;
end;

function TFactorio.GetRecipeOrder: IReadonlyList<TRecipe>;
begin
  Result := Order<TRecipe>;
end;

function TFactorio.GetTransportBelt: IReadonlyMap<AnsiString, TTransportBelt>;
begin
  Result := Get<TTransportBelt>;
end;

function TFactorio.GetTransportBeltOrder: IReadonlyList<TTransportBelt>;
begin
  Result := Order<TTransportBelt>;
end;

function TFactorio.Order(AType: TPrototype.TType): IReadonlyList<TPrototype>;
begin
  Result := FPrototypeLists[AType].ReadonlyList;
end;

function TFactorio.Order<T>: IReadonlyList<T>;
begin
  Result := IReadonlyList<T>(FPrototypeLists[T.GetType].ReadonlyList);
end;

procedure TFactorio.Load(AExpensive: Boolean);
const
  InitCode: PAnsiChar =
    'defines = setmetatable({}, {__index = function(t) return t end})'#10 +
    'local paths = {'#10 +
    '  package.path,'#10 +
    '  ";data\\core\\lualib\\?.lua",'#10 +
    '  ";data\\core\\?.lua",'#10 +
    '  ";data\\base\\lualib\\?.lua",'#10 +
    '  ";data\\base\\?.lua"'#10 +
    '}'#10 +
    'package.path = table.concat(paths, ";") '#10 +
    'require "dataloader"'#10 +
    'data.is_demo = false'#10 +
    'require "data.core.data"'#10 +
    'require "data.base.data"';

var
  L: TLuaState;
  ErrorMessage: PAnsiChar;
  Prototype: TPrototype;
  PrototypeType: TPrototype.TType;
  PrototypeName: AnsiString;
  UnsortedLists: array [TPrototype.TType] of IList<TPrototype>;
  OldDir: string;
begin
  FExpensive := AExpensive;

  L := NewLuaState(LuaDefaultAlloc);
  L.LOpenLibs;    
     
  OldDir := GetCurrentDir;
  SetCurrentDir(FPath);    
  if L.LDoString(InitCode) then
  begin
    ErrorMessage := L.ToString;
    L.Pop;
    raise Exception.Create(string(AnsiString(ErrorMessage)));
  end;  
  SetCurrentDir(OldDir);

  for PrototypeType := Low(TPrototype.TType) to High(TPrototype.TType) do
  begin
    UnsortedLists[PrototypeType] := TList<TPrototype>.Create;
    FPrototypeMaps[PrototypeType] := TMap<AnsiString, TPrototype>.Create;
  end;

  FGroupedMap := TMap<AnsiString, TGrouped>.Create;
  FItemMap := TMap<AnsiString, TItemOrFluid>.Create;
  FCraftingMachineMap := TMap<AnsiString, TCraftingMachine>.Create;

  L.GetGlobal('data');
  L.GetField('raw');
  L.PushNil;
  while L.Next(-2) do
  begin
    PrototypeName := L.ToString(-2);
    for PrototypeType := Low(TPrototype.TType) to High(TPrototype.TType) do
    begin
      if PrototypeName <> PrototypeNames[PrototypeType] then
        Continue;
      L.PushNil;
      while L.Next(-2) do
      begin
        Prototype := PrototypeClasses[PrototypeType].Create(Self, L);
        UnsortedLists[PrototypeType].Add(Prototype);

        if not FPrototypeMaps[PrototypeType].Add(Prototype.Name, Prototype) then
          raise EFactorio.CreateFmt('Duplicate prototype "%s" found!', [Prototype.Name]);

        if PrototypeType in TPrototype.GroupedTypes then
          if not FGroupedMap.Add(Prototype.Name, TGrouped(Prototype)) then
            raise EFactorio.CreateFmt('Duplicate grouped "%s" found!', [Prototype.Name]);

        if PrototypeType in TPrototype.ItemOrFluidTypes then
          if not FItemMap.Add(Prototype.Name, TItemOrFluid(Prototype)) then
            raise EFactorio.CreateFmt('Duplicate item "%s" found!', [Prototype.Name]);

        if PrototypeType in TPrototype.CraftingMachines then
          if not FCraftingMachineMap.Add(Prototype.Name, TCraftingMachine(Prototype)) then
            raise EFactorio.CreateFmt('Duplicate item "%s" found!', [Prototype.Name]);

        L.Pop;
      end;
      Break;
    end;
    L.Pop;
  end;

  L.Pop(2);
  L.Close;

  for PrototypeType := Low(TPrototype.TType) to High(TPrototype.TType) do
  begin
    FPrototypeLists[PrototypeType] := TSortedObjectList<TPrototype>.Create;
    FPrototypeLists[PrototypeType].Compare := ComparePrototypes;
    FPrototypeLists[PrototypeType].AddRange(UnsortedLists[PrototypeType]);
  end;

  FCraftingMachineList := TSortedList<TCraftingMachine>.Create;
  FCraftingMachineList.Compare := ComparePrototypes<TCraftingMachine>;
  FCraftingMachineList.AddRange(FCraftingMachineMap.Values);
end;

function TFactorio.Get(AType: TPrototype.TType): IReadonlyMap<AnsiString, TPrototype>;
begin
  Result := FPrototypeMaps[AType].ReadonlyMap;
end;

function TFactorio.Get<T>: IReadonlyMap<AnsiString, T>;
begin
  Result := IReadonlyMap<AnsiString, T>(FPrototypeMaps[T.GetType].ReadonlyMap);
end;

{ TFactorio.TPrototype }

constructor TFactorio.TPrototype.Create(AFactorio: TFactorio; L: TLuaState);
var
  I: TLuaInteger;
  Flag: AnsiString;
begin
  FFactorio := AFactorio;
  FFlags := TSet<AnsiString>.Create;

  L.GetField('name');
  FName := L.ToString;
  L.Pop;

  if L.GetField('order') = ltString then
    FOrder := L.ToString;
  L.Pop;

  if L.GetField('icon_size') = ltNumber then
    FIconSize := L.ToInteger;
  L.Pop;

  if L.GetField('icon') = ltString then
    FIconPath := Factorio.ExpandPath(string(AnsiString(L.ToString)));
  L.Pop;

  if L.GetField('icon_mipmaps') = ltNumber then
    FIconMipmapCount := L.ToInteger
  else
    FIconMipmapCount := 1;
  L.Pop;

  if L.GetField('flags') = ltTable then
  begin
    for I := 1 to L.RawLen do
    begin
      L.GetI(I);
      Flag := L.ToString;
      FFlags.Add(Flag);
      if Flag = 'hidden' then
        FHidden := True;
      L.Pop;
    end;
  end;
  L.Pop;
end;

class function TFactorio.TPrototype.CreateTyped(AFactorio: TFactorio; L: TLuaState): TPrototype;
var
  Prototype: AnsiString;
  T: TType;
begin
  L.GetField('type');
  Prototype := L.ToString;
  for T := Low(TType) to High(TType) do
  begin
    if Prototype = PrototypeNames[T] then
    begin
      L.Pop;
      Exit(PrototypeClasses[T].Create(AFactorio, L));
    end;
  end;
  L.Pop;
  Result := nil;
end;

function TFactorio.TPrototype.GenerateMissingno(AMipmapLayer: Integer): IGPBitmap;
var
  Size: Integer;
  G: IGPGraphics;
  Font: IGPFont;
  Brush: IGPBrush;
  Pen: IGPPen;
  BgBrush: IGPBrush;
begin
  Size := IconSize shr AMipmapLayer;
  Result := TGPBitmap.Create(Size, Size);

  G := TGPGraphics.Create(Result);
  G.TextRenderingHint := TextRenderingHintAntiAlias;
  Font := TGPFont.Create('Tahoma', 14);
  Brush := TGPSolidBrush.Create(TGPColor.Black);
  Pen := TGPPen.Create(TGPColor.Black);
  BgBrush := TGPLinearGradientBrush.Create(TGPRectF.Create(0, 0, Size, Size), $3FFF0000, $3F7F0000, 90);

  G.FillRectangle(BgBrush, 0, 0, Size, Size);
  G.DrawString(
    string(DisplayName).Replace(' ', #10#13),
    Font,
    TGPRectF.Create(0, 0, Size, Size),
    TGPStringFormat.Create([StringFormatFlagsNoWrap]),
    Brush);
end;

function TFactorio.TPrototype.GetDisplayName: AnsiString;
var
  I: Integer;
  UpperNext: Boolean;
begin
  Result := Name;
  UpperNext := True;
  for I := 1 to Length(Result) do
  begin
    if UpperNext then
      Result[I] := UpCase(Result[I]);
    UpperNext := Result[I] = '-';
    if UpperNext then
      Result[I] := ' ';
  end;
end;

function TFactorio.TPrototype.GetFlags: IReadonlySet<AnsiString>;
begin
  Result := FFlags.ReadonlySet;
end;

function TFactorio.TPrototype.GetIcon: IGPBitmap;
begin
  Result := IconMipmaps[0];
end;

function TFactorio.TPrototype.GetIconMipmaps: IReadonlyList<IGPBitmap>;
var
  I, Size, X: Integer;
  FullImage, MipmapImage: IGPBitmap;
  G: IGPGraphics;
begin
  if FIconMipmaps = nil then
  begin
    FIconMipmaps := TList<IGPBitmap>.Create;
    if FIconPath.IsEmpty or not FileExists(FIconPath) then
    begin
      for I := 0 to FIconMipmapCount - 1 do
        FIconMipmaps.Add(GenerateMissingno(I));
    end
    else
    begin
      FullImage := TGPBitmap.FromFile(IconPath);
      X := 0;
      for I := 0 to FIconMipmapCount - 1 do
      begin
        Size := IconSize shr I;
        MipmapImage := TGPBitmap.Create(Size, Size);
        G := TGPGraphics.FromImage(MipmapImage);
        G.DrawImage(FullImage, 0, 0, X, 0, Size, Size, UnitPixel);
        FIconMipmaps.Add(MipmapImage);
        Inc(X, Size);
      end;
    end;
  end;
  Result := FIconMipmaps.ReadonlyList;
end;

function TFactorio.TPrototype.GetOrder: AnsiString;
begin
  Result := FOrder;
end;

class function TFactorio.TPrototype.GetTypeName: AnsiString;
begin
  Result := PrototypeNames[GetType];
end;

function TFactorio.TPrototype.IconMipmapFor(ASize: Cardinal): IGPBitmap;
var
  MipmapIndex: Integer;
begin
  for MipmapIndex := 0 to FIconMipmapCount - 1 do
  begin
    Result := IconMipmaps[MipmapIndex];
    if ASize >= Result.Width then
      Break;
  end;
end;

{ TFactorio.TItem }

constructor TFactorio.TItem.Create(AFactorio: TFactorio; L: TLuaState);
begin
  inherited;

  if L.GetField('stack_size') = ltNumber then
    FStackSize := L.ToInteger;
  L.Pop;
end;

class function TFactorio.TItem.GetType: TPrototype.TType;
begin
  Result := ptItem;
end;

{ TFactorio.TRecipe }

constructor TFactorio.TRecipe.Create(AFactorio: TFactorio; L: TLuaState);
var
  Result: TResult;
  ResultCount: Integer;
  ExtraTable: Boolean;
begin
  inherited;

  if L.GetField('category') = ltString then
    FCategoryName := L.ToString
  else
    FCategoryName := 'crafting';
  L.Pop;

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
      FIngredients.Add(TIngredient.Create(Factorio, L));
      L.Pop;
    end;
  end;
  L.Pop;

  FResults := TObjectList<TResult>.Create;
  if L.GetField('result') = ltString then
    FResults.Add(TResult.Create(Factorio, L));
  L.Pop;

  if L.GetField('results') = ltTable then
  begin
    L.PushNil;
    while L.Next(-2) do
    begin
      FResults.Add(TResult.Create(Factorio, L));
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

function TFactorio.TRecipe.FindCraftingMachine: TCraftingMachine;
begin
  for Result in Factorio.CraftingMachineOrder do
    if (Result.Name <> 'escape-pod-assembler') and Result.CanCraft(Self) then
      Exit;
  Result := nil;
end;

function TFactorio.TRecipe.GetCategory: TRecipeCategory;
begin
  if FCategory = nil then
    FCategory := Factorio.RecipeCategory[FCategoryName];
  Result := FCategory;
end;

function TFactorio.TRecipe.GetGroup: TItemGroup;
begin
  Result := Results.First.Item.Group;
end;

function TFactorio.TRecipe.GetIconMipmaps: IReadonlyList<IGPBitmap>;
begin
  if not FIconPath.IsEmpty then
    Exit(inherited);

  if Results.First.IsFluid then
    Result := Factorio.Fluid[Results.First.Name].IconMipmaps
  else
    Result := Factorio.Item[Results.First.Name].IconMipmaps;
end;

function TFactorio.TRecipe.GetIngredients: IReadonlyList<TIngredient>;
begin
  Result := FIngredients.ReadonlyList;
end;

function TFactorio.TRecipe.GetOrder: AnsiString;
begin
  if FOrder = '' then
    FOrder := Results.First.Item.Order;
  Result := FOrder;
end;

function TFactorio.TRecipe.GetResults: IReadonlyList<TResult>;
begin
  Result := FResults.ReadonlyList;
end;

function TFactorio.TRecipe.GetSubgroup: TItemSubgroup;
begin
  Result := Results.First.Item.Subgroup;
end;

class function TFactorio.TRecipe.GetType: TPrototype.TType;
begin
  Result := ptRecipe;
end;

function TFactorio.TRecipe.ResultHidden: Boolean;
begin
  Result := Results.Iterate.Any(
    function(RecipeResult: TResult): Boolean
    begin
      Result := RecipeResult.Item.Hidden;
    end);
end;

{ TFactorio.TCraftingMachine }

function TFactorio.TCraftingMachine.CanCraft(ARecipe: TRecipe): Boolean;
begin
  Result := CraftingCategories.Contains(ARecipe.Category);
end;

constructor TFactorio.TCraftingMachine.Create(AFactorio: TFactorio; L: TLuaState);
begin
  inherited;

  if L.GetField('crafting_speed') = ltNumber then
    FCraftingSpeed := L.ToNumber;
  L.Pop;

  FCraftingCategoryNames := TList<AnsiString>.Create;
  if L.GetField('crafting_categories') = ltTable then
  begin
    L.PushNil;
    while L.Next(-2) do
    begin
      FCraftingCategoryNames.Add(L.ToString);
      L.Pop;
    end;
  end;
  L.Pop;
end;

function TFactorio.TCraftingMachine.GetCraftingCategories: IReadonlyList<TRecipeCategory>;
begin
  if FCraftingCategories = nil then
  begin
    FCraftingCategories := TSortedList<TRecipeCategory>.Create;
    FCraftingCategories.Compare := TFactorio.ComparePrototypes<TRecipeCategory>;
    FCraftingCategories.AddRange(
      FCraftingCategoryNames.Iterate.Generic.Map<TRecipeCategory>(
      function(Name: AnsiString): TRecipeCategory
      begin
        Result := Factorio.RecipeCategory[Name];
      end));
  end;
  Result := FCraftingCategories.ReadonlyList;
end;

function TFactorio.TCraftingMachine.GetOrder: AnsiString;
begin
  if FOrder = '' then
    FOrder := Factorio.Item[Name].Subgroup.Order + '-' + Factorio.Item[Name].Order;
  Result := FOrder;
end;

{ TFactorio.TTransportBelt }

constructor TFactorio.TTransportBelt.Create(AFactorio: TFactorio; L: TLuaState);
begin
  inherited;

  if L.GetField('speed') = ltNumber then
    FSpeed := L.ToNumber;
  L.Pop;
end;

function TFactorio.TTransportBelt.GetItemsPerSecond: Single;
begin
  Result := Speed * 60 * 8;
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

{ TFactorio.TRecipe.TItemStack }

constructor TFactorio.TRecipe.TItemStack.Create(AFactorio: TFactorio; L: TLuaState);
begin
  FFactorio := AFactorio;

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

function TFactorio.TRecipe.TItemStack.GetItem: TItemOrFluid;
begin
  if FItem = nil then
    FItem := Factorio.Item[Name];
  Result := FItem;
end;

{ TFactorio.TRecipe.TResult }

constructor TFactorio.TRecipe.TResult.Create(AFactorio: TFactorio; L: TLuaState);
begin
  if L.&Type = ltString then
  begin
    FFactorio := AFactorio;
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

{ TFactorio.TAmmo }

class function TFactorio.TAmmo.GetType: TPrototype.TType;
begin
  Result := ptAmmo;
end;

{ TFactorio.TArmor }

class function TFactorio.TArmor.GetType: TPrototype.TType;
begin
  Result := ptArmor;
end;

{ TFactorio.TCapsule }

class function TFactorio.TCapsule.GetType: TPrototype.TType;
begin
  Result := ptCapsule;
end;

{ TFactorio.TEquipment }

class function TFactorio.TEquipment.GetType: TPrototype.TType;
begin
  Result := ptEquipment;
end;

{ TFactorio.TGun }

class function TFactorio.TGun.GetType: TPrototype.TType;
begin
  Result := ptGun;
end;

{ TFactorio.TMiningTool }

class function TFactorio.TMiningTool.GetType: TPrototype.TType;
begin
  Result := ptMiningTool;
end;

{ TFactorio.TModule }

class function TFactorio.TModule.GetType: TPrototype.TType;
begin
  Result := ptModule;
end;

{ TFactorio.TRepairTool }

class function TFactorio.TRepairTool.GetType: TPrototype.TType;
begin
  Result := ptRepairTool;
end;

{ TFactorio.TItemWithEntityData }

class function TFactorio.TItemWithEntityData.GetType: TPrototype.TType;
begin
  Result := ptItemWithEntityData;
end;

{ TFactorio.TTool }

class function TFactorio.TTool.GetType: TPrototype.TType;
begin
  Result := ptTool;
end;

{ TFactorio.TFluid }

constructor TFactorio.TFluid.Create(AFactorio: TFactorio; L: TLuaState);
begin
  inherited;

  if L.GetField('subgroup') <> ltString then
    FSubgroupName := 'fluid';
  L.Pop;
end;

class function TFactorio.TFluid.GetType: TPrototype.TType;
begin
  Result := ptFluid;
end;

{ TFactorio.TRailPlanner }

class function TFactorio.TRailPlanner.GetType: TPrototype.TType;
begin
  Result := ptRailPlanner;
end;

{ TFactorio.TGrouped }

constructor TFactorio.TGrouped.Create(AFactorio: TFactorio; L: TLuaState);
begin
  inherited;

  if L.GetField('subgroup') = ltString then
    FSubgroupName := L.ToString
  else
    FSubgroupName := 'other';
  L.Pop;
end;

function TFactorio.TGrouped.GetGroup: TItemGroup;
begin
  Result := Subgroup.Group;
end;

function TFactorio.TGrouped.GetSubgroup: TItemSubgroup;
begin
  if FSubgroup = nil then
    FSubgroup := Factorio.ItemSubgroup[FSubgroupName];
  Result := FSubgroup;
end;

{ TFactorio.TItemGroup }

constructor TFactorio.TItemGroup.Create(AFactorio: TFactorio; L: TLuaState);
begin
  inherited;

  if L.GetField('order_in_recipe') = ltString then
    FOrderInRecipe := L.ToString
  else
    FOrderInRecipe := Order;
  L.Pop;
end;

function TFactorio.TItemGroup.GetSubgroups: IReadonlyList<TItemSubgroup>;
begin
  if FSubgroups = nil then
  begin
    FSubgroups := TSortedList<TItemSubgroup>.Create;
    FSubgroups.Compare := TFactorio.ComparePrototypes<TItemSubgroup>;
    FSubgroups.AddRange(
      Factorio.ItemSubgroup.Values.Iterate.Where(
      function(Subgroup: TItemSubgroup): Boolean
      begin
        Result := Subgroup.Group = Self;
      end));
  end;
  Result := FSubgroups.ReadonlyList;
end;

class function TFactorio.TItemGroup.GetType: TPrototype.TType;
begin
  Result := ptItemGroup;
end;

{ TFactorio.TItemSubgroup }

function TFactorio.TItemSubgroup.GetEntries: IReadonlyList<TGrouped>;
begin
  if FEntries = nil then
  begin
    FEntries := TSortedList<TGrouped>.Create;
    FEntries.Compare := TFactorio.ComparePrototypes<TGrouped>;
    FEntries.AddRange(
      Factorio.Grouped.Values.Iterate.Where(
      function(Grouped: TGrouped): Boolean
      begin
        Result := Grouped.Subgroup = Self;
      end));
  end;
  Result := FEntries.ReadonlyList;
end;

constructor TFactorio.TItemSubgroup.Create(AFactorio: TFactorio; L: TLuaState);
begin
  inherited;

  if L.GetField('group') = ltString then
    FGroupName := L.ToString;
  L.Pop;
end;

function TFactorio.TItemSubgroup.GetGroup: TItemGroup;
begin
  if FGroup = nil then
    FGroup := Factorio.ItemGroup[FGroupName];
  Result := FGroup;
end;

function TFactorio.TItemSubgroup.GetRecipes: IReadonlyList<TRecipe>;
begin
  if FRecipes = nil then
  begin
    FRecipes := TSortedList<TRecipe>.Create;
    FRecipes.Compare := TFactorio.ComparePrototypes<TRecipe>;
    FRecipes.AddRange(
      Factorio.RecipeOrder.Iterate.Where(
      function(Recipe: TRecipe): Boolean
      begin
        Result := Recipe.Subgroup = Self;
      end));
  end;
  Result := FRecipes.ReadonlyList;
end;

class function TFactorio.TItemSubgroup.GetType: TPrototype.TType;
begin
  Result := ptItemSubgroup;
end;

{ TFactorio.TRecipeCategory }

class function TFactorio.TRecipeCategory.GetType: TPrototype.TType;
begin
  Result := ptRecipeCategory;
end;

{ TFactorio.TAssemblingMachine }

class function TFactorio.TAssemblingMachine.GetType: TPrototype.TType;
begin
  Result := ptAssemblingMachine;
end;

{ TFactorio.TRocketSilo }

class function TFactorio.TRocketSilo.GetType: TPrototype.TType;
begin
  Result := ptRocketSilo;
end;

{ TFactorio.TFurnace }

class function TFactorio.TFurnace.GetType: TPrototype.TType;
begin
  Result := ptFurnace;
end;

{ TFactorio.TItemOrFluid }

function TFactorio.TItemOrFluid.FindRecipe: TRecipe;
var
  Group: TItemGroup;
  Subgroup: TItemSubgroup;
begin
  for Group in Factorio.ItemGroupOrder do
    for Subgroup in Group.Subgroups do
      for Result in Subgroup.Recipes do
        if Result.Results.Iterate.Any(
          function(ItemStack: TRecipe.TResult): Boolean
          begin
            Result := ItemStack.Item = Self;
          end) then
          Exit;
  Result := nil;
end;

{ TFactorio.TModInfo }

constructor TFactorio.TMod.Create(APath: string);
begin
  FPath := APath;
  FDependencies := TList<string>.Create;
  ReadInfoFile;
end;

procedure TFactorio.TMod.DefineJStorage(ASerializer: TJSerializer);
begin
  ASerializer.Define('name', FName);
  ASerializer.Define('version', FVersion);
  ASerializer.Define('factorio_version', FFactorioVersion);
  ASerializer.Define('title', FTitle);
  ASerializer.Define('author', FAuthor);
  ASerializer.Define('contact', FContact);
  ASerializer.Define('homepage', FHomepage);
  ASerializer.Define('description', FDescription);
  ASerializer.DefineCollection('dependencies', FDependencies);
end;

function TFactorio.TMod.GetDependencies: IReadonlyList<string>;
begin
  Result := FDependencies.ReadonlyList;
end;

function TFactorio.TMod.GetJVersion: Integer;
begin
  Result := 0;
end;

procedure TFactorio.TMod.ReadInfoFile;
var
  JInfo: TJObject;
begin
  JInfo := TJObject.CreateFromFile(TPath.Combine(Path, 'info.json'));
  try
    TJSerializer.Unserialize(Self, JInfo);
  finally
    JInfo.Free;
  end;
end;

{ TFactorio.TBlueprint }

class function TFactorio.TBlueprint.GetType: TPrototype.TType;
begin
  Result := ptBlueprint;
end;

{ TFactorio.TBlueprintBook }

class function TFactorio.TBlueprintBook.GetType: TPrototype.TType;
begin
  Result := ptBlueprintBook;
end;

{ TFactorio.TCopyPasteTool }

class function TFactorio.TCopyPasteTool.GetType: TPrototype.TType;
begin
  Result := ptCopyPasteTool;
end;

{ TFactorio.TDeconstructionItem }

class function TFactorio.TDeconstructionItem.GetType: TPrototype.TType;
begin
  Result := ptDeconstructionItem;
end;

{ TFactorio.TSelectionTool }

class function TFactorio.TSelectionTool.GetType: TPrototype.TType;
begin
  Result := ptSelectionTool;
end;

{ TFactorio.TSpidertronRemote }

class function TFactorio.TSpidertronRemote.GetType: TPrototype.TType;
begin
  Result := ptSpidertronRemote;
end;

{ TFactorio.TUpgradeItem }

class function TFactorio.TUpgradeItem.GetType: TPrototype.TType;
begin
  Result := ptUpgradeItem;
end;

end.
