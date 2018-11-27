unit Pengine.MC.LootTable;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,

  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.Hasher,
  Pengine.IntMaths,
  Pengine.Vector,
  Pengine.Utility,
  Pengine.JSON,

  Pengine.MC.NBT,
  Pengine.MC.Enchantment,
  Pengine.MC.Attribute,
  Pengine.MC.Namespace;

type

  ELootTable = class(Exception);

  /// <summary>Generates a set of items from multiple pools.</summary>
  TLootTable = class
  public type

    TPool = class;

    TConditioned = class;

    TEntryItem = class;

    /// <summary>Pools, Entries and Functions can have multiple conditions.</summary>
    TCondition = class abstract
    public type

      TType = (
        ctEntityProperties,
        ctEntityScores,
        ctKilledByPlayer,
        ctRandomChance,
        ctRandomChanceWithLooting
        );

    private
      FConditioned: TConditioned;

      function GetIndex: Integer;
      procedure SetIndex(const Value: Integer);

    public
      constructor Create(AConditioned: TConditioned); overload; virtual;
      constructor Create(AConditioned: TConditioned; AValue: TJValue); overload;

      class function GetType: TType; virtual; abstract;
      class function GetName: string;
      class function GetDisplayName: string;

      procedure Load(AValue: TJValue); virtual;
      procedure Save(AValue: TJObject); virtual;

      class function CreateTyped(AConditioned: TConditioned; AValue: TJValue): TCondition; static;

      property Conditioned: TConditioned read FConditioned;
      property Index: Integer read GetIndex write SetIndex;

    end;

    TConditionClass = class of TCondition;

    TConditions = TObjectArray<TCondition>;

    /// <summary>Base class for conditions, that perform a test on an involved entity.</summary>
    TConditionEntity = class abstract(TCondition)
    public type

      /// <summary>Entity Conditions can check on one of the below.</summary>
      TTarget = (
        tgThis,
        tgKiller,
        tgKillerPlayer
        );

    public const

      TargetNames: array [TTarget] of string = (
        'this',
        'killer',
        'killer_player'
        );

      TargetDisplayNames: array [TTarget] of string = (
        'Self',
        'Killer',
        'Killer if Player'
        );

    private
      FTarget: TTarget;

      procedure SetTarget(const Value: TTarget);

    public
      procedure Load(AValue: TJValue); override;
      procedure Save(AValue: TJObject); override;

      property Target: TTarget read FTarget write SetTarget;

    end;

    TConditionEntityProperties = class(TConditionEntity)
    private
      FOnFire: TOpt<Boolean>;

    public
      class function GetType: TCondition.TType; override;

      procedure Load(AValue: TJValue); override;
      procedure Save(AValue: TJObject); override;

      property OnFire: TOpt<Boolean> read FOnFire;

    end;

    TConditionEntityScores = class(TConditionEntity)
    public type

      TScores = TMap<string, TIntBounds1, TStringHasher>;

    private
      FScores: TScores;

      function GetScores: TScores.TReader;

    public
      constructor Create(AConditioned: TConditioned); override;
      destructor Destroy; override;

      class function GetType: TCondition.TType; override;

      procedure Load(AValue: TJValue); override;
      procedure Save(AValue: TJObject); override;

      property Scores: TScores.TReader read GetScores;
      procedure AddScore(AObjective: string; AValue: TIntBounds1);
      procedure RemoveScore(AObjective: string);

    end;

    /// <summary>Checks, wether the killer was a player. Can check for opposite if inverse is set to true.</summary>
    TConditionKilledByPlayer = class(TCondition)
    private
      FInverse: Boolean;

      procedure SetInverse(const Value: Boolean);

    public
      class function GetType: TCondition.TType; override;

      procedure Load(AValue: TJValue); override;
      procedure Save(AValue: TJObject); override;

      property Inverse: Boolean read FInverse write SetInverse;

    end;

    /// <summary>A condition that passes based on random chance.</summary>
    TConditionRandomChance = class(TCondition)
    private
      FChance: Single;

      procedure SetChance(const Value: Single);

    public
      class function GetType: TCondition.TType; override;

      procedure Load(AValue: TJValue); override;
      procedure Save(AValue: TJObject); override;

      property Chance: Single read FChance write SetChance;

    end;

    /// <summary>A condition that passes based on random chance, but can include a looting multiplier.</summary>
    TConditionRandomChanceWithLooting = class(TConditionRandomChance)
    private
      FLootingMultiplier: Single;

    public
      class function GetType: TCondition.TType; override;

      procedure Load(AValue: TJValue); override;
      procedure Save(AValue: TJObject); override;

      property LootingMultiplier: Single read FLootingMultiplier write FLootingMultiplier;

    end;

    TConditioned = class
    private
      FConditions: TConditions;

      function GetConditions: TConditions.TReader;

      function GetConditionIndex(ACondition: TCondition): Integer;
      procedure SetConditionIndex(ACondition: TCondition; const Value: Integer);

    public
      constructor Create;
      destructor Destroy; override;

      procedure Load(AValue: TJValue); virtual;
      procedure Save(AValue: TJObject); virtual;

      property Conditions: TConditions.TReader read GetConditions;

      property ConditionIndex[ACondition: TCondition]: Integer read GetConditionIndex write SetConditionIndex;

      function AddCondition<T: TCondition>: T; overload;
      function AddCondition(T: TConditionClass): TCondition; overload;
      procedure RemoveCondition(ACondition: TCondition);
      procedure ClearConditions;

    end;

    /// <summary>Multiple functions can be applied to item entries.</summary>
    TFunction = class abstract(TConditioned)
    public type

      TType = (
        ftEnchantRandomly,
        ftEnchantWithLevels,
        ftExplorationMap,
        ftFurnaceSmelt,
        ftLootingEnchant,
        ftSetAttributes,
        ftSetCount,
        ftSetDamage,
        ftSetData,
        ftSetNBT
        );

    private
      FEntry: TEntryItem;

      function GetIndex: Integer;
      procedure SetIndex(const Value: Integer);

    public
      constructor Create(AEntry: TEntryItem); overload; virtual;
      constructor Create(AEntry: TEntryItem; AValue: TJValue); overload;

      class function CreateTyped(AEntry: TEntryItem; AValue: TJValue): TFunction;
      procedure Save(AValue: TJObject); override;

      class function GetType: TType; virtual; abstract;
      class function GetName: string;
      class function GetDisplayName: string;

      property Entry: TEntryItem read FEntry;
      property Index: Integer read GetIndex write SetIndex;

    end;

    TFunctionClass = class of TFunction;

    TFunctions = TObjectArray<TFunction>;

    /// <summary>Enchants the item randomly with any applicable enchantment or out of a given set.</summary>
    TFunctionEnchantRandomly = class(TFunction)
    private
      FEnchantments: TEnchantments;

      procedure SetEnchantments(const Value: TEnchantments);

    public
      class function GetType: TFunction.TType; override;

      procedure Load(AValue: TJValue); override;
      procedure Save(AValue: TJObject); override;

      property Enchantments: TEnchantments read FEnchantments write SetEnchantments;

    end;

    /// <summary>Enchants the item with a given amount of levels.</summary>
    TFunctionEnchantWithLevels = class(TFunction)
    private
      FTreasure: Boolean;
      FLevels: TIntBounds1;

      procedure SetLevels(const Value: TIntBounds1);
      procedure SetLevelsMax(const Value: Integer);
      procedure SetLevelsMin(const Value: Integer);
      procedure SetTreasure(const Value: Boolean);

    public
      class function GetType: TFunction.TType; override;

      procedure Load(AValue: TJValue); override;
      procedure Save(AValue: TJObject); override;

      property Treasure: Boolean read FTreasure write SetTreasure;
      property Levels: TIntBounds1 read FLevels write SetLevels;
      property LevelsMin: Integer read FLevels.C1 write SetLevelsMin;
      property LevelsMax: Integer read FLevels.C2 write SetLevelsMax;

    end;

    /// <summary>Converts an empty map item into an explorer map.</summary>
    TFunctionExplorationMap = class(TFunction)
    public const

      DefaultZoom = 2;
      DefaultSearchRadius = 50;
      DefaultSkipExistingChunks = True;

    private
      FDestination: string; // TODO: Structure enum
      FDecoration: string; // TODO: Map decoration enum
      FZoom: Integer;
      FSearchRadius: Integer;
      FSkipExistingChunks: Boolean;

      procedure SetDecoration(const Value: string);
      procedure SetDestination(const Value: string);
      procedure SetSearchRadius(const Value: Integer);
      procedure SetSkipExistingChunks(const Value: Boolean);
      procedure SetZoom(const Value: Integer);

    public
      constructor Create(AEntry: TEntryItem); overload; override;

      class function GetType: TFunction.TType; override;

      procedure Load(AValue: TJValue); override;
      procedure Save(AValue: TJObject); override;

      property Destination: string read FDestination write SetDestination;
      property Decoration: string read FDecoration write SetDecoration;
      property Zoom: Integer read FZoom write SetZoom;
      property SearchRadius: Integer read FSearchRadius write SetSearchRadius;
      property SkipExistingChunks: Boolean read FSkipExistingChunks write SetSkipExistingChunks;

    end;

    /// <summary>Smelts the item similar to what a furnace would do.</summary>
    TFunctionFurnaceSmelt = class(TFunction)
    public
      class function GetType: TFunction.TType; override;

    end;

    /// <summary>Adjust the item count based on the killers looting level.</summary>
    TFunctionLootingEnchant = class(TFunction)
    private
      FCount: TIntBounds1;
      FLimit: Integer;

      procedure SetCount(const Value: TIntBounds1);
      procedure SetCountMax(const Value: Integer);
      procedure SetCountMin(const Value: Integer);
      procedure SetLimit(const Value: Integer);

    public
      class function GetType: TFunction.TType; override;

      procedure Load(AValue: TJValue); override;
      procedure Save(AValue: TJObject); override;

      property Count: TIntBounds1 read FCount write SetCount;
      property CountMin: Integer read FCount.C1 write SetCountMin;
      property CountMax: Integer read FCount.C2 write SetCountMax;
      property Limit: Integer read FLimit write SetLimit;

    end;

    TFunctionSetAttributes = class;

    TModifier = class
    private
      FFunction: TFunctionSetAttributes;
      FName: string;
      FAttribute: TAttribute;
      FOperation: TAttributeOperation;
      FAmount: TBounds1;
      FSlots: TAttributeSlots;
      FUUID: TOpt<TGUID>;

      function GetIndex: Integer;
      procedure SetIndex(const Value: Integer);
      procedure SetName(const Value: string);
      procedure SetAttribute(const Value: TAttribute);
      procedure SetOperation(const Value: TAttributeOperation);
      procedure SetAmount(const Value: TBounds1);
      procedure SetAmountMin(const Value: Single);
      procedure SetAmountMax(const Value: Single);
      procedure SetSlots(const Value: TAttributeSlots);

    public
      constructor Create(AFunction: TFunctionSetAttributes); overload;
      constructor Create(AFunction: TFunctionSetAttributes; AValue: TJValue); overload;

      procedure Load(AValue: TJValue);
      procedure Save(AValue: TJObject);

      property Func: TFunctionSetAttributes read FFunction;
      property Index: Integer read GetIndex write SetIndex;

      property Name: string read FName write SetName;
      property Attribute: TAttribute read FAttribute write SetAttribute;
      property Operation: TAttributeOperation read FOperation write SetOperation;
      property Amount: TBounds1 read FAmount write SetAmount;
      property AmountMin: Single read FAmount.C1 write SetAmountMin;
      property AmountMax: Single read FAmount.C1 write SetAmountMax;
      property Slots: TAttributeSlots read FSlots write SetSlots;
      property UUID: TOpt<TGUID> read FUUID;

    end;

    TModifiers = TObjectArray<TModifier>;

    /// <summary>Adds an attribute modifier to the item.</summary>
    TFunctionSetAttributes = class(TFunction)
    private
      FModifiers: TModifiers;

      function GetModifieres: TModifiers.TReader;
      function GetModifierIndex(AModifier: TModifier): Integer;
      procedure SetModifierIndex(AModifier: TModifier; const Value: Integer);

    public
      constructor Create(AEntry: TEntryItem); overload; override;
      destructor Destroy; override;

      class function GetType: TFunction.TType; override;

      procedure Load(AValue: TJValue); override;
      procedure Save(AValue: TJObject); override;

      property Modifiers: TModifiers.TReader read GetModifieres;
      function AddModifier: TModifier;
      procedure RemoveModifier(AModifier: TModifier);
      procedure ClearModifiers;
      property ModifierIndex[AModifier: TModifier]: Integer read GetModifierIndex write SetModifierIndex;

    end;

    /// <summary>Sets the item stack size.</summary>
    TFunctionSetCount = class(TFunction)
    private
      FCount: TIntBounds1;

      procedure SetCount(const Value: TIntBounds1);
      procedure SetCountMin(const Value: Integer);
      procedure SetCountMax(const Value: Integer);

    public
      constructor Create(AEntry: TEntryItem); overload; override;

      class function GetType: TFunction.TType; override;

      procedure Load(AValue: TJValue); override;
      procedure Save(AValue: TJObject); override;

      property Count: TIntBounds1 read FCount write SetCount;
      property CountMin: Integer read FCount.C1 write SetCountMin;
      property CountMax: Integer read FCount.C2 write SetCountMax;

    end;

    /// <summary>Sets the items durability as a percentage.</summary>
    TFunctionSetDamage = class(TFunction)
    private
      FDamage: TBounds1;

      procedure SetDamage(const Value: TBounds1);
      procedure SetDamageMin(const Value: Single);
      procedure SetDamageMax(const Value: Single);

    public
      constructor Create(AEntry: TEntryItem); overload; override;

      class function GetType: TFunction.TType; override;

      procedure Load(AValue: TJValue); override;
      procedure Save(AValue: TJObject); override;

      property Damage: TBounds1 read FDamage write SetDamage;
      property DamageMin: Single read FDamage.C1 write SetDamageMin;
      property DamageMax: Single read FDamage.C2 write SetDamageMax;

    end;

    /// <summary>Sets the items metadata.</summary>
    TFunctionSetData = class(TFunction)
    private
      FData: TIntBounds1;

      procedure SetData(const Value: TIntBounds1);
      procedure SetDataMin(const Value: Integer);
      procedure SetDataMax(const Value: Integer);

    public
      class function GetType: TFunction.TType; override;

      procedure Load(AValue: TJValue); override;
      procedure Save(AValue: TJObject); override;

      property Data: TIntBounds1 read FData write SetData;
      property DataMin: Integer read FData.C1 write SetDataMin;
      property DataMax: Integer read FData.C2 write SetDataMax;

    end;

    /// <summary>Adds nbt data to an item.</summary>
    TFunctionSetNBT = class(TFunction)
    private
      FTag: TNBTCompound;

      function GetTagString: string;
      procedure SetTagString(const Value: string);

    public
      constructor Create(AEntry: TEntryItem); override;
      destructor Destroy; override;

      class function GetType: TFunction.TType; override;

      procedure Load(AValue: TJValue); override;
      procedure Save(AValue: TJObject); override;

      property Tag: TNBTCompound read FTag;
      property TagString: string read GetTagString write SetTagString;

    end;

    /// <summary>An entry can either be an item, an other loot table or empty to generate nothing.</summary>
    TEntry = class(TConditioned)
    public type

      TType = (
        etItem,
        etLootTable,
        etEmpty
        );

    private
      FPool: TPool;
      FWeight: Integer;
      FQuality: Integer;

      function GetIndex: Integer;
      procedure SetIndex(const Value: Integer);

      procedure SetWeight(const Value: Integer);
      procedure SetQuality(const Value: Integer);

    public
      constructor Create(APool: TPool); overload; virtual;
      constructor Create(APool: TPool; AValue: TJValue); overload;

      procedure Load(AValue: TJValue); override;
      procedure Save(AValue: TJObject); override;

      class function CreateTyped(APool: TPool; AValue: TJValue): TEntry;

      class function GetType: TType; virtual; abstract;
      class function GetName: string;
      class function GetDisplayName: string;

      property Pool: TPool read FPool;
      property Index: Integer read GetIndex write SetIndex;

      property Weight: Integer read FWeight write SetWeight;
      property Quality: Integer read FQuality write SetQuality;

    end;

    TEntryClass = class of TEntry;

    /// <summary>An item entry can have multiple functions, that get applied, if their conditions are met.</summary>
    TEntryItem = class(TEntry)
    private
      FItem: string;
      FFunctions: TFunctions;

      procedure SetItem(const Value: string);

      function GetFunctions: TFunctions.TReader;
      function GetFunctionIndex(AFunction: TFunction): Integer;
      procedure SetFunctionIndex(AFunction: TFunction; const Value: Integer);

    public
      constructor Create(APool: TPool); override;
      destructor Destroy; override;

      procedure Load(AValue: TJValue); override;
      procedure Save(AValue: TJObject); override;

      class function GetType: TEntry.TType; override;

      property Item: string read FItem write SetItem;

      property Functions: TFunctions.TReader read GetFunctions;
      function AddFunction<T: TFunction>: T; overload;
      function AddFunction(AFunctionClass: TFunctionClass): TFunction; overload;
      procedure RemoveFunction(AFunction: TFunction);
      procedure ClearFunctions;
      property FunctionIndex[AFunction: TFunction]: Integer read GetFunctionIndex write SetFunctionIndex;

    end;

    /// <summary>An entry, that generates its items from another loot table in the datapack.</summary>
    TEntryLootTable = class(TEntry)
    private
      FLootTable: string;

      procedure SetLootTable(const Value: string);

    public
      class function GetType: TEntry.TType; override;

      procedure Load(AValue: TJValue); override;
      procedure Save(AValue: TJObject); override;

      property LootTable: string read FLootTable write SetLootTable;

    end;

    /// <summary>An entry, that generates nothing. Is this actually useful? Tbh I don't think so...</summary>
    TEntryEmpty = class(TEntry)
    public
      class function GetType: TEntry.TType; override;

    end;

    TEntries = TObjectArray<TEntry>;

    TPool = class(TConditioned)
    private
      FLootTable: TLootTable;
      FRolls: TIntBounds1;
      FBonusRolls: TBounds1;
      FEntries: TEntries;

      function GetIndex: Integer;
      procedure SetIndex(const Value: Integer);

      procedure SetRolls(const Value: TIntBounds1);
      procedure SetRollsMin(const Value: Integer);
      procedure SetRollsMax(const Value: Integer);

      procedure SetBonusRolls(const Value: TBounds1);
      procedure SetBonusRollsMin(const Value: Single);
      procedure SetBonusRollsMax(const Value: Single);

      function GetEntries: TEntries.TReader;
      function GetEntryIndex(AEntry: TEntry): Integer;
      procedure SetEntryIndex(AEntry: TEntry; const Value: Integer);

    public
      constructor Create(ALootTable: TLootTable); overload;
      constructor Create(ALootTable: TLootTable; AValue: TJValue); overload;
      destructor Destroy; override;

      procedure Load(AValue: TJValue); override;
      procedure Save(AValue: TJObject); override;

      property LootTable: TLootTable read FLootTable;
      property Index: Integer read GetIndex write SetIndex;

      property Rolls: TIntBounds1 read FRolls write SetRolls;
      property RollsMin: Integer read FRolls.C1 write SetRollsMin;
      property RollsMax: Integer read FRolls.C2 write SetRollsMax;

      property BonusRolls: TBounds1 read FBonusRolls write SetBonusRolls;
      property BonusRollsMin: Single read FBonusRolls.C1 write SetBonusRollsMin;
      property BonusRollsMax: Single read FBonusRolls.C2 write SetBonusRollsMax;

      property Entries: TEntries.TReader read GetEntries;
      function AddEntry<T: TEntry>: T; overload;
      function AddEntry(AEntryClass: TEntryClass): TEntry; overload;
      procedure RemoveEntry(AEntry: TEntry);
      procedure ClearEntries;
      property EntryIndex[AEntry: TEntry]: Integer read GetEntryIndex write SetEntryIndex;

    end;

    TPools = TObjectArray<TPool>;

  public const

    ConditionClasses: array [TCondition.TType] of TConditionClass = (
      TConditionEntityProperties,
      TConditionEntityScores,
      TConditionKilledByPlayer,
      TConditionRandomChance,
      TConditionRandomChanceWithLooting
      );

    ConditionNames: array [TCondition.TType] of string = (
      'entity_properties',
      'entity_scores',
      'killed_by_player',
      'random_chance',
      'random_chance_with_looting'
      );

    ConditionDisplayNames: array [TCondition.TType] of string = (
      'Entity Properties',
      'Entity Scores',
      'Killed by Player',
      'Random Chance',
      'Random Chance with Looting'
      );

    FunctionClasses: array [TFunction.TType] of TFunctionClass = (
      TFunctionEnchantRandomly,
      TFunctionEnchantWithLevels,
      TFunctionExplorationMap,
      TFunctionFurnaceSmelt,
      TFunctionLootingEnchant,
      TFunctionSetAttributes,
      TFunctionSetCount,
      TFunctionSetDamage,
      TFunctionSetData,
      TFunctionSetNBT
      );

    FunctionNames: array [TFunction.TType] of string = (
      'enchant_randomly',
      'enchant_with_levels',
      'exploration_map',
      'furnace_smelt',
      'looting_enchant',
      'set_attributes',
      'set_count',
      'set_damage',
      'set_data',
      'set_nbt'
      );

    FunctionDisplayNames: array [TFunction.TType] of string = (
      'Enchant randomly',
      'Enchant using Levels',
      'Convert to Exploration Map',
      'Smelt in Furnace',
      'Looting Bonus',
      'Attribute',
      'Item Count',
      'Tool/Weapon Damage',
      'Metadata',
      'Add NBT'
      );

    EntryClasses: array [TEntry.TType] of TEntryClass = (
      TEntryItem,
      TEntryLootTable,
      TEntryEmpty
      );

    EntryNames: array [TEntry.TType] of string = (
      'item',
      'loot_table',
      'empty'
      );

    EntryDisplayNames: array [TEntry.TType] of string = (
      'Item',
      'Loot Table',
      'Empty'
      );

  private
    FPools: TPools;

    function GetPools: TPools.TReader;
    function GetPoolIndex(APool: TPool): Integer;
    procedure SetPoolIndex(APool: TPool; const Value: Integer);

  public
    constructor Create; overload;
    constructor Create(AValue: TJValue); overload;
    destructor Destroy; override;

    procedure Load(AValue: TJValue);
    function Save: TJObject;

    property Pools: TPools.TReader read GetPools;

    function AddPool: TPool;
    procedure RemovePool(APool: TPool);
    property PoolIndex[APool: TPool]: Integer read GetPoolIndex write SetPoolIndex;

  end;

implementation

function LoadIntBounds(AValue: TJValue; ADefault: Integer = 0): TIntBounds1;
begin
  if AValue.IsNull then
    Exit(ADefault);
  if AValue.IsNumber then
    Exit(AValue.AsInt);
  Result.C1 := AValue['min'];
  Result.C2 := AValue['max'];
end;

function LoadBounds(AValue: TJValue; ADefault: Single = 0): TBounds1;
begin
  if AValue.IsNull then
    Exit(ADefault);
  if AValue.IsNumber then
    Exit(AValue.AsFloat);
  Result.C1 := AValue['min'].AsFloat;
  Result.C2 := AValue['max'].AsFloat;
end;

procedure SaveIntBounds(AObject: TJObject; AName: string; AValue: TBounds1); overload;
var
  JBounds: TJObject;
begin
  if AValue.C1 = AValue.C2 then
    AObject[AName] := AValue.C1
  else
  begin
    JBounds := AObject.AddObject(AName);
    JBounds['min'] := AValue.C1;
    JBounds['max'] := AValue.C2;
  end;
end;

procedure SaveIntBounds(AObject: TJObject; AName: string; AValue: TBounds1; ADefault: Integer); overload;
begin
  if AValue <> ADefault then
    SaveIntBounds(AObject, AName, AValue);
end;

procedure SaveBounds(AObject: TJObject; AName: string; AValue: TBounds1); overload;
var
  JBounds: TJObject;
begin
  if AValue.C1 = AValue.C2 then
    AObject.Add(AName, AValue.C1)
  else
  begin
    JBounds := AObject.AddObject(AName);
    JBounds.Add('min', AValue.C1);
    JBounds.Add('max', AValue.C2);
  end;
end;

procedure SaveBounds(AObject: TJObject; AName: string; AValue: TBounds1; ADefault: Single); overload;
begin
  if AValue <> ADefault then
    SaveBounds(AObject, AName, AValue);
end;

{ TLootTable }

constructor TLootTable.Create;
begin
  FPools := TPools.Create;
end;

function TLootTable.AddPool: TPool;
begin
  Result := FPools.Add(TPool.Create(Self));
end;

constructor TLootTable.Create(AValue: TJValue);
begin
  Create;
  Load(AValue);
end;

destructor TLootTable.Destroy;
begin
  FPools.Free;
  inherited;
end;

function TLootTable.Save: TJObject;
var
  JPools: TJArray;
  Pool: TPool;
begin
  Result := TJObject.Create;
  JPools := Result.AddArray('pools');
  for Pool in Pools do
    Pool.Save(JPools.AddObject);
end;

function TLootTable.GetPoolIndex(APool: TPool): Integer;
begin
  Result := Pools.Find(APool);
end;

function TLootTable.GetPools: TPools.TReader;
begin
  Result := FPools.Reader;
end;

procedure TLootTable.Load(AValue: TJValue);
var
  JPool: TJValue;
begin
  for JPool in AValue['pools'].AsArray do
    FPools.Add(TPool.Create(Self, JPool));
end;

procedure TLootTable.RemovePool(APool: TPool);
begin
  FPools.Remove(APool);
end;

procedure TLootTable.SetPoolIndex(APool: TPool; const Value: Integer);
begin
  FPools.SetIndex(APool.Index, Value);
end;

{ TLootTable.TConditionEntityProperties }

class function TLootTable.TConditionEntityProperties.GetType: TCondition.TType;
begin
  Result := ctEntityProperties;
end;

procedure TLootTable.TConditionEntityProperties.Load(AValue: TJValue);
var
  JOnFire: TJValue;
begin
  inherited;
  JOnFire := AValue['properties']['on_fire'];
  if JOnFire.Exists then
    OnFire.Value := JOnFire.AsBool
  else
    OnFire.Clear;
end;

procedure TLootTable.TConditionEntityProperties.Save(AValue: TJObject);
begin
  inherited;
  if OnFire.HasValue then
    AValue.AddObject('properties').Add('on_fire', OnFire.Value);
end;

{ TLootTable.TConditionEntityScores }

procedure TLootTable.TConditionEntityScores.AddScore(AObjective: string; AValue: TIntBounds1);
begin
  FScores[AObjective] := AValue;
end;

constructor TLootTable.TConditionEntityScores.Create(AConditioned: TConditioned);
begin
  inherited;
  FScores := TScores.Create;
end;

destructor TLootTable.TConditionEntityScores.Destroy;
begin
  FScores.Free;
  inherited;
end;

function TLootTable.TConditionEntityScores.GetScores: TScores.TReader;
begin
  Result := FScores.Reader;
end;

class function TLootTable.TConditionEntityScores.GetType: TCondition.TType;
begin
  Result := ctEntityScores;
end;

procedure TLootTable.TConditionEntityScores.Load(AValue: TJValue);
var
  JScore: TJPair;
begin
  inherited;
  for JScore in AValue['scores'].AsObject do
    AddScore(JScore.Key, LoadIntBounds(JScore.Value));
end;

procedure TLootTable.TConditionEntityScores.RemoveScore(AObjective: string);
begin
  FScores.Remove(AObjective);
end;

procedure TLootTable.TConditionEntityScores.Save(AValue: TJObject);
var
  Score: TScores.TPair;
  JScores: TJObject;
begin
  inherited;
  JScores := AValue.AddObject('scores');
  for Score in Scores do
    SaveIntBounds(JScores, Score.Key, Score.Value);
end;

{ TLootTable.TConditionKilledByPlayer }

class function TLootTable.TConditionKilledByPlayer.GetType: TCondition.TType;
begin
  Result := ctKilledByPlayer;
end;

procedure TLootTable.TConditionKilledByPlayer.Load(AValue: TJValue);
begin
  inherited;
  Inverse := AValue['inverse'] or False;
end;

procedure TLootTable.TConditionKilledByPlayer.Save(AValue: TJObject);
begin
  inherited;
  AValue.Add('inverse', Inverse);
end;

procedure TLootTable.TConditionKilledByPlayer.SetInverse(const Value: Boolean);
begin
  if Inverse = Value then
    Exit;
  FInverse := Value;
end;

{ TLootTable.TConditionRandomChance }

class function TLootTable.TConditionRandomChance.GetType: TCondition.TType;
begin
  Result := ctRandomChance;
end;

procedure TLootTable.TConditionRandomChance.Load(AValue: TJValue);
begin
  inherited;
  Chance := AValue['chance'] or 0.0;
end;

procedure TLootTable.TConditionRandomChance.Save(AValue: TJObject);
begin
  inherited;
  AValue.Add('chance', Chance);
end;

procedure TLootTable.TConditionRandomChance.SetChance(const Value: Single);
begin
  if Chance = Value then
    Exit;
  FChance := Value;
end;

{ TLootTable.TConditionRandomChanceWithLooting }

class function TLootTable.TConditionRandomChanceWithLooting.GetType: TCondition.TType;
begin
  Result := ctRandomChanceWithLooting;
end;

procedure TLootTable.TConditionRandomChanceWithLooting.Load(AValue: TJValue);
begin
  inherited;
  LootingMultiplier := AValue['looting_multiplier'] or 0.0;
end;

procedure TLootTable.TConditionRandomChanceWithLooting.Save(AValue: TJObject);
begin
  inherited;
  AValue.Add('looting_multiplier', LootingMultiplier);
end;

{ TLootTable.TFunctionEnchantRandomly }

procedure TLootTable.TFunctionEnchantRandomly.Save(AValue: TJObject);
var
  JEnchantments: TJArray;
  Enchantment: TEnchantment;
begin
  inherited;
  JEnchantments := AValue.AddArray('enchantments');
  for Enchantment in Enchantments do
    JEnchantments.Add(EnchantmentNames[Enchantment]);
end;

procedure TLootTable.TFunctionEnchantRandomly.SetEnchantments(const Value: TEnchantments);
var
  Changed: TEnchantments;
begin
  if Enchantments = Value then
    Exit;
  Changed := Value - (Value * Enchantments);
  FEnchantments := Value;
end;

class function TLootTable.TFunctionEnchantRandomly.GetType: TFunction.TType;
begin
  Result := ftEnchantRandomly;
end;

procedure TLootTable.TFunctionEnchantRandomly.Load(AValue: TJValue);
var
  JEnchantment: TJValue;
  Enchantment: TEnchantment;
  JString: TJString;
begin
  inherited;
  Enchantments := [];
  for JEnchantment in AValue['enchantments'].AsArray do
  begin
    if JEnchantment.Cast(JString) then
    begin
      for Enchantment := Low(TEnchantment) to High(TEnchantment) do
      begin
        if JString.Text = EnchantmentNames[Enchantment] then
        begin
          Include(FEnchantments, Enchantment);
          Break;
        end;
      end;
    end;
  end;
end;

{ TLootTable.TFunctionEnchantWithLevels }

class function TLootTable.TFunctionEnchantWithLevels.GetType: TFunction.TType;
begin
  Result := ftEnchantWithLevels;
end;

procedure TLootTable.TFunctionEnchantWithLevels.Load(AValue: TJValue);
begin
  inherited;
  Levels := LoadIntBounds(AValue['levels']);
  Treasure := AValue['treasure'] or False;
end;

procedure TLootTable.TFunctionEnchantWithLevels.Save(AValue: TJObject);
begin
  inherited;
  SaveIntBounds(AValue, 'levels', Levels);
  AValue.Add('treasure', Treasure);
end;

procedure TLootTable.TFunctionEnchantWithLevels.SetLevels(const Value: TIntBounds1);
begin
  if Levels = Value then
    Exit;
  FLevels := Value;
end;

procedure TLootTable.TFunctionEnchantWithLevels.SetLevelsMax(const Value: Integer);
begin
  if LevelsMax = Value then
    Exit;
  FLevels.C2 := Value;
end;

procedure TLootTable.TFunctionEnchantWithLevels.SetLevelsMin(const Value: Integer);
begin
  if LevelsMin = Value then
    Exit;
  FLevels.C1 := Value;
end;

procedure TLootTable.TFunctionEnchantWithLevels.SetTreasure(const Value: Boolean);
begin
  if Treasure = Value then
    Exit;
  FTreasure := Value;
end;

{ TLootTable.TFunctionExplorationMap }

constructor TLootTable.TFunctionExplorationMap.Create(AEntry: TEntryItem);
begin
  inherited;
  FZoom := DefaultZoom;
  FSearchRadius := DefaultSearchRadius;
  FSkipExistingChunks := DefaultSkipExistingChunks;
end;

class function TLootTable.TFunctionExplorationMap.GetType: TFunction.TType;
begin
  Result := ftExplorationMap;
end;

procedure TLootTable.TFunctionExplorationMap.Load(AValue: TJValue);
begin
  inherited;
  Destination := AValue['destination'] or '';
  Decoration := AValue['decoration'] or '';
  Zoom := AValue['zoom'] or DefaultZoom;
  SearchRadius := AValue['search_radius'] or DefaultSearchRadius;
  SkipExistingChunks := AValue['skip_existing_chunks'] or DefaultSkipExistingChunks;
end;

procedure TLootTable.TFunctionExplorationMap.Save(AValue: TJObject);
begin
  inherited;
  AValue.Add('destination', Destination);
  AValue.Add('decoration', Decoration);
  AValue.Add('zoom', Zoom);
  AValue.Add('search_radius', SearchRadius);
  AValue.Add('skip_existing_chunks', SkipExistingChunks);
end;

procedure TLootTable.TFunctionExplorationMap.SetDecoration(const Value: string);
begin
  if Decoration = Value then
    Exit;
  FDecoration := Value;
end;

procedure TLootTable.TFunctionExplorationMap.SetDestination(const Value: string);
begin
  if Destination = Value then
    Exit;
  FDestination := Value;
end;

procedure TLootTable.TFunctionExplorationMap.SetSearchRadius(const Value: Integer);
begin
  if SearchRadius = Value then
    Exit;
  FSearchRadius := Value;
end;

procedure TLootTable.TFunctionExplorationMap.SetSkipExistingChunks(const Value: Boolean);
begin
  if SkipExistingChunks = Value then
    Exit;
  FSkipExistingChunks := Value;
end;

procedure TLootTable.TFunctionExplorationMap.SetZoom(const Value: Integer);
begin
  if Zoom = Value then
    Exit;
  FZoom := Value;
end;

{ TLootTable.TFunctionFurnaceSmelt }

class function TLootTable.TFunctionFurnaceSmelt.GetType: TFunction.TType;
begin
  Result := ftFurnaceSmelt;
end;

{ TLootTable.TFunctionLootingEnchant }

class function TLootTable.TFunctionLootingEnchant.GetType: TFunction.TType;
begin
  Result := ftLootingEnchant;
end;

procedure TLootTable.TFunctionLootingEnchant.Load(AValue: TJValue);
begin
  inherited;
  Count := LoadIntBounds(AValue['count'], 1);
  Limit := AValue['limit'] or 0;
end;

procedure TLootTable.TFunctionLootingEnchant.Save(AValue: TJObject);
begin
  inherited;
  SaveIntBounds(AValue, 'count', Count);
  AValue.Add('limit', Limit);
end;

procedure TLootTable.TFunctionLootingEnchant.SetCount(const Value: TIntBounds1);
begin
  if Count = Value then
    Exit;
  FCount := Value;
end;

procedure TLootTable.TFunctionLootingEnchant.SetCountMax(const Value: Integer);
begin
  if CountMax = Value then
    Exit;
  FCount.C2 := Value;
end;

procedure TLootTable.TFunctionLootingEnchant.SetCountMin(const Value: Integer);
begin
  if CountMin = Value then
    Exit;
  FCount.C1 := Value;
end;

procedure TLootTable.TFunctionLootingEnchant.SetLimit(const Value: Integer);
begin
  if Limit = Value then
    Exit;
  FLimit := Value;
end;

{ TLootTable.TFunctionSetAttributes }

function TLootTable.TFunctionSetAttributes.AddModifier: TModifier;
begin
  Result := FModifiers.Add(TModifier.Create(Self));
end;

procedure TLootTable.TFunctionSetAttributes.ClearModifiers;
begin
  if Modifiers.Empty then
    Exit;
  FModifiers.Clear;
end;

constructor TLootTable.TFunctionSetAttributes.Create(AEntry: TEntryItem);
begin
  inherited;
  FModifiers := TModifiers.Create;
end;

destructor TLootTable.TFunctionSetAttributes.Destroy;
begin
  FModifiers.Free;
  inherited;
end;

function TLootTable.TFunctionSetAttributes.GetModifieres: TModifiers.TReader;
begin
  Result := FModifiers.Reader;
end;

function TLootTable.TFunctionSetAttributes.GetModifierIndex(AModifier: TModifier): Integer;
begin
  Result := Modifiers.Find(AModifier);
end;

class function TLootTable.TFunctionSetAttributes.GetType: TFunction.TType;
begin
  Result := ftSetAttributes;
end;

procedure TLootTable.TFunctionSetAttributes.Load(AValue: TJValue);
var
  JModifier: TJValue;
begin
  inherited;
  FModifiers.Clear;
  for JModifier in AValue['modifiers'].AsArray do
    FModifiers.Add(TModifier.Create(Self, JModifier));
end;

procedure TLootTable.TFunctionSetAttributes.RemoveModifier(AModifier: TModifier);
begin
  FModifiers.Remove(AModifier);
end;

procedure TLootTable.TFunctionSetAttributes.Save(AValue: TJObject);
var
  JModifiers: TJArray;
  Modifier: TModifier;
begin
  inherited;
  JModifiers := AValue.AddArray('modifiers');
  for Modifier in Modifiers do
    Modifier.Save(JModifiers.AddObject);
end;

procedure TLootTable.TFunctionSetAttributes.SetModifierIndex(AModifier: TModifier; const Value: Integer);
begin
  FModifiers.SetIndex(AModifier.Index, Value);
end;

{ TLootTable.TFunctionSetCount }

constructor TLootTable.TFunctionSetCount.Create(AEntry: TEntryItem);
begin
  inherited;
  FCount := 1;
end;

class function TLootTable.TFunctionSetCount.GetType: TFunction.TType;
begin
  Result := ftSetCount;
end;

procedure TLootTable.TFunctionSetCount.Load(AValue: TJValue);
begin
  inherited;
  Count := LoadIntBounds(AValue['count'], 1);
end;

procedure TLootTable.TFunctionSetCount.Save(AValue: TJObject);
begin
  inherited;
  SaveIntBounds(AValue, 'count', Count);
end;

procedure TLootTable.TFunctionSetCount.SetCount(const Value: TIntBounds1);
begin
  if Count = Value then
    Exit;
  FCount := Value;
end;

procedure TLootTable.TFunctionSetCount.SetCountMax(const Value: Integer);
begin
  if CountMax = Value then
    Exit;
  FCount.C2 := Value;
end;

procedure TLootTable.TFunctionSetCount.SetCountMin(const Value: Integer);
begin
  if CountMin = Value then
    Exit;
  FCount.C1 := Value;
end;

{ TLootTable.TFunctionSetDamage }

constructor TLootTable.TFunctionSetDamage.Create(AEntry: TEntryItem);
begin
  inherited;
  FDamage := 1;
end;

class function TLootTable.TFunctionSetDamage.GetType: TFunction.TType;
begin
  Result := ftSetDamage;
end;

procedure TLootTable.TFunctionSetDamage.Load(AValue: TJValue);
begin
  inherited;
  Damage := LoadBounds(AValue['damage'], 1);
end;

procedure TLootTable.TFunctionSetDamage.Save(AValue: TJObject);
begin
  inherited;
  SaveBounds(AValue, 'damage', Damage);
end;

procedure TLootTable.TFunctionSetDamage.SetDamage(const Value: TBounds1);
begin
  if Damage = Value then
    Exit;
  FDamage := Value;
end;

procedure TLootTable.TFunctionSetDamage.SetDamageMax(const Value: Single);
begin
  if DamageMax = Value then
    Exit;
  FDamage.C2 := Value;
end;

procedure TLootTable.TFunctionSetDamage.SetDamageMin(const Value: Single);
begin
  if DamageMin = Value then
    Exit;
  FDamage.C1 := Value;
end;

{ TLootTable.TFunctionSetData }

class function TLootTable.TFunctionSetData.GetType: TFunction.TType;
begin
  Result := ftSetData;
end;

procedure TLootTable.TFunctionSetData.Load(AValue: TJValue);
begin
  inherited;
  Data := LoadIntBounds(AValue['data']);
end;

procedure TLootTable.TFunctionSetData.Save(AValue: TJObject);
begin
  inherited;
  SaveIntBounds(AValue, 'data', Data);
end;

procedure TLootTable.TFunctionSetData.SetData(const Value: TIntBounds1);
begin
  if Data = Value then
    Exit;
  FData := Value;
end;

procedure TLootTable.TFunctionSetData.SetDataMax(const Value: Integer);
begin
  if DataMax = Value then
    Exit;
  FData.C2 := Value;
end;

procedure TLootTable.TFunctionSetData.SetDataMin(const Value: Integer);
begin
  if DataMin = Value then
    Exit;
  FData.C1 := Value;
end;

{ TLootTable.TFunctionSetNBT }

constructor TLootTable.TFunctionSetNBT.Create(AEntry: TEntryItem);
begin
  inherited;
  FTag := TNBTCompound.Create;
end;

destructor TLootTable.TFunctionSetNBT.Destroy;
begin
  FTag.Free;
  inherited;
end;

function TLootTable.TFunctionSetNBT.GetTagString: string;
begin
  Result := FTag.Format;
end;

class function TLootTable.TFunctionSetNBT.GetType: TFunction.TType;
begin
  Result := ftSetNBT;
end;

procedure TLootTable.TFunctionSetNBT.Load(AValue: TJValue);
begin
  inherited;
  TagString := AValue['tag'] or '{}';
end;

procedure TLootTable.TFunctionSetNBT.Save(AValue: TJObject);
begin
  inherited;
  AValue.Add('tag', TagString);
end;

procedure TLootTable.TFunctionSetNBT.SetTagString(const Value: string);
var
  Parser: TNBTCompound.TParser;
begin
  Parser := TNBTCompound.TParser.Create(Value, False);
  if Parser.Success then
  begin
    FTag.Free;
    FTag := Parser.OwnParseResult;
  end;
  Parser.Free;
end;

{ TLootTable.TEntryItem }

function TLootTable.TEntryItem.AddFunction(AFunctionClass: TFunctionClass): TFunction;
begin
  Result := FFunctions.Add(AFunctionClass.Create(Self));
end;

function TLootTable.TEntryItem.AddFunction<T>: T;
begin
  Result := T.Create(Self);
  FFunctions.Add(Result);
end;

procedure TLootTable.TEntryItem.ClearFunctions;
begin
  if Functions.Empty then
    Exit;
  FFunctions.Clear;
end;

constructor TLootTable.TEntryItem.Create(APool: TPool);
begin
  inherited Create(APool);
  FFunctions := TFunctions.Create;
end;

destructor TLootTable.TEntryItem.Destroy;
begin
  FFunctions.Free;
  inherited;
end;

function TLootTable.TEntryItem.GetFunctionIndex(AFunction: TFunction): Integer;
begin
  Result := Functions.Find(AFunction);
end;

function TLootTable.TEntryItem.GetFunctions: TFunctions.TReader;
begin
  Result := FFunctions.Reader;
end;

class function TLootTable.TEntryItem.GetType: TEntry.TType;
begin
  Result := etItem;
end;

procedure TLootTable.TEntryItem.Load(AValue: TJValue);
var
  JFunction: TJValue;
begin
  inherited;
  Item := AValue['name'] or '';
  ClearFunctions;
  for JFunction in AValue['functions'].AsArray do
    FFunctions.Add(TFunction.CreateTyped(Self, JFunction));
end;

procedure TLootTable.TEntryItem.RemoveFunction(AFunction: TFunction);
begin
  FFunctions.Remove(AFunction);
end;

procedure TLootTable.TEntryItem.Save(AValue: TJObject);
var
  JFunctions: TJArray;
  Func: TFunction;
begin
  inherited;
  AValue.Add('name', Item);
  if not Functions.Empty then
  begin
    JFunctions := AValue.AddArray('functions');
    for Func in Functions do
      Func.Save(JFunctions.AddObject);
  end;
end;

procedure TLootTable.TEntryItem.SetFunctionIndex(AFunction: TFunction; const Value: Integer);
begin
  FFunctions.SetIndex(AFunction.Index, Value);
end;

procedure TLootTable.TEntryItem.SetItem(const Value: string);
begin
  if Item = Value then
    Exit;
  FItem := Value;
end;

{ TLootTable.TEntryLootTable }

class function TLootTable.TEntryLootTable.GetType: TEntry.TType;
begin
  Result := etLootTable;
end;

procedure TLootTable.TEntryLootTable.Load(AValue: TJValue);
begin
  inherited;
  LootTable := AValue['name'] or '';
end;

procedure TLootTable.TEntryLootTable.Save(AValue: TJObject);
begin
  inherited;
  AValue.Add('name', LootTable);
end;

procedure TLootTable.TEntryLootTable.SetLootTable(const Value: string);
begin
  FLootTable := Value;
end;

{ TLootTable.TEntryEmpty }

class function TLootTable.TEntryEmpty.GetType: TEntry.TType;
begin
  Result := etEmpty;
end;

{ TLootTable.TPool }

constructor TLootTable.TPool.Create(ALootTable: TLootTable);
begin
  inherited Create;
  FLootTable := ALootTable;
  FEntries := TEntries.Create;
end;

function TLootTable.TPool.AddEntry(AEntryClass: TEntryClass): TEntry;
begin
  Result := FEntries.Add(AEntryClass.Create(Self));
end;

function TLootTable.TPool.AddEntry<T>: T;
begin
  Result := T.Create(Self);
  FEntries.Add(Result);
end;

procedure TLootTable.TPool.ClearEntries;
begin
  if Entries.Empty then
    Exit;
  FEntries.Clear;
end;

constructor TLootTable.TPool.Create(ALootTable: TLootTable; AValue: TJValue);
begin
  Create(ALootTable);
  Load(AValue);
end;

destructor TLootTable.TPool.Destroy;
begin
  FEntries.Free;
  inherited;
end;

function TLootTable.TPool.GetEntries: TEntries.TReader;
begin
  Result := FEntries.Reader;
end;

function TLootTable.TPool.GetEntryIndex(AEntry: TEntry): Integer;
begin
  Result := Entries.Find(AEntry);
end;

function TLootTable.TPool.GetIndex: Integer;
begin
  Result := LootTable.PoolIndex[Self];
end;

procedure TLootTable.TPool.Load(AValue: TJValue);
var
  JEntry: TJValue;
begin
  Rolls := LoadIntBounds(AValue['rolls'], 1);
  BonusRolls := LoadBounds(AValue['bonus_rolls'], 0);
  for JEntry in AValue['entries'].AsArray do
    FEntries.Add(TEntry.CreateTyped(Self, JEntry));
end;

procedure TLootTable.TPool.RemoveEntry(AEntry: TEntry);
begin
  FEntries.Remove(AEntry);
end;

procedure TLootTable.TPool.Save(AValue: TJObject);
var
  JEntries: TJArray;
  Entry: TEntry;
begin
  SaveIntBounds(AValue, 'rolls', Rolls);
  SaveBounds(AValue, 'bonus_rolls', BonusRolls, 0);
  JEntries := AValue.AddArray('entries');
  for Entry in Entries do
    Entry.Save(JEntries.AddObject);
end;

procedure TLootTable.TPool.SetBonusRolls(const Value: TBounds1);
begin
  if BonusRolls = Value then
    Exit;
  FBonusRolls := Value;
end;

procedure TLootTable.TPool.SetBonusRollsMax(const Value: Single);
begin
  if BonusRollsMax = Value then
    Exit;
  FBonusRolls.C2 := Value;
end;

procedure TLootTable.TPool.SetBonusRollsMin(const Value: Single);
begin
  if BonusRollsMin = Value then
    Exit;
  FBonusRolls.C1 := Value;
end;

procedure TLootTable.TPool.SetEntryIndex(AEntry: TEntry; const Value: Integer);
begin
  FEntries.SetIndex(AEntry.Index, Value);
end;

procedure TLootTable.TPool.SetIndex(const Value: Integer);
begin
  LootTable.PoolIndex[Self] := Value;
end;

procedure TLootTable.TPool.SetRolls(const Value: TIntBounds1);
begin
  if Rolls = Value then
    Exit;
  FRolls := Value;
end;

procedure TLootTable.TPool.SetRollsMax(const Value: Integer);
begin
  if RollsMax = Value then
    Exit;
  FRolls.C2 := Value;
end;

procedure TLootTable.TPool.SetRollsMin(const Value: Integer);
begin
  if RollsMin = Value then
    Exit;
  FRolls.C1 := Value;
end;

{ TLootTable.TCondition }

constructor TLootTable.TCondition.Create(AConditioned: TConditioned);
begin
  FConditioned := AConditioned;
end;

constructor TLootTable.TCondition.Create(AConditioned: TConditioned; AValue: TJValue);
begin
  Create(AConditioned);
  Load(AValue);
end;

class function TLootTable.TCondition.CreateTyped(AConditioned: TConditioned; AValue: TJValue): TCondition;
var
  T: TType;
  Condition: TNSPath;
begin
  Condition := AValue['condition'].AsString;
  for T := Low(TType) to High(TType) do
    if Condition = ConditionClasses[T].GetName then
      Exit(ConditionClasses[T].Create(AConditioned, AValue));
  raise ELootTable.Create('Unknown condition.');
end;

class function TLootTable.TCondition.GetDisplayName: string;
begin
  Result := ConditionDisplayNames[GetType];
end;

function TLootTable.TCondition.GetIndex: Integer;
begin
  Result := Conditioned.ConditionIndex[Self];
end;

class function TLootTable.TCondition.GetName: string;
begin
  Result := ConditionNames[GetType];
end;

procedure TLootTable.TCondition.Load(AValue: TJValue);
begin
  // nothing by default
end;

procedure TLootTable.TCondition.Save(AValue: TJObject);
begin
  inherited;
  AValue.Add('condition', GetName);
end;

procedure TLootTable.TCondition.SetIndex(const Value: Integer);
begin
  Conditioned.ConditionIndex[Self] := Value;
end;

{ TLootTable.TConditionEntity }

procedure TLootTable.TConditionEntity.Load(AValue: TJValue);
var
  JEntity: TJString;
  T: TTarget;
begin
  inherited;
  JEntity := AValue['entity'].Cast<TJString>;

  for T := Low(TTarget) to High(TTarget) do
  begin
    if JEntity.Text = TargetNames[T] then
    begin
      Target := T;
      Exit;
    end;
  end;

  raise ELootTable.Create('Invalid target entity specified.');
end;

procedure TLootTable.TConditionEntity.Save(AValue: TJObject);
begin
  inherited;
  AValue.Add('entity', TargetNames[Target]);
end;

procedure TLootTable.TConditionEntity.SetTarget(const Value: TTarget);
begin
  if Target = Value then
    Exit;
  FTarget := Value;
end;

{ TLootTable.TEntry }

constructor TLootTable.TEntry.Create(APool: TPool);
begin
  inherited Create;
  FPool := APool;
end;

constructor TLootTable.TEntry.Create(APool: TPool; AValue: TJValue);
begin
  Create(APool);
  Load(AValue);
end;

class function TLootTable.TEntry.CreateTyped(APool: TPool; AValue: TJValue): TEntry;
var
  EntryType: TNSPath;
  T: TType;
begin
  EntryType := AValue['type'].AsString;
  for T := Low(TType) to High(TType) do
    if EntryType = EntryClasses[T].GetName then
      Exit(EntryClasses[T].Create(APool, AValue));
  raise ELootTable.Create('Unknown entry type.');
end;

class function TLootTable.TEntry.GetDisplayName: string;
begin
  Result := EntryDisplayNames[GetType];
end;

function TLootTable.TEntry.GetIndex: Integer;
begin
  Result := Pool.EntryIndex[Self];
end;

class function TLootTable.TEntry.GetName: string;
begin
  Result := EntryNames[GetType];
end;

procedure TLootTable.TEntry.Load(AValue: TJValue);
begin
  inherited;
  Weight := AValue['weight'] or 1;
  Quality := AValue['quality'] or 0;
end;

procedure TLootTable.TEntry.Save(AValue: TJObject);
begin
  inherited;
  AValue.Add('type', GetName);
  AValue.Add('weight', Weight);
  if Quality <> 0 then
    AValue.Add('quality', Quality);
end;

procedure TLootTable.TEntry.SetIndex(const Value: Integer);
begin
  Pool.EntryIndex[Self] := Value;
end;

procedure TLootTable.TEntry.SetQuality(const Value: Integer);
begin
  FQuality := Value;
end;

procedure TLootTable.TEntry.SetWeight(const Value: Integer);
begin
  FWeight := Value;
end;

{ TLootTable.TConditioned }

function TLootTable.TConditioned.AddCondition(T: TConditionClass): TCondition;
begin
  Result := FConditions.Add(T.Create(Self));
end;

function TLootTable.TConditioned.AddCondition<T>: T;
begin
  Result := T.Create;
  FConditions.Add(Result);
end;

procedure TLootTable.TConditioned.ClearConditions;
begin
  if FConditions.Empty then
    Exit;
  FConditions.Clear;
end;

constructor TLootTable.TConditioned.Create;
begin
  FConditions := TConditions.Create;
end;

destructor TLootTable.TConditioned.Destroy;
begin
  FConditions.Free;
  inherited;
end;

function TLootTable.TConditioned.GetConditionIndex(ACondition: TCondition): Integer;
begin
  Result := Conditions.Find(ACondition);
end;

function TLootTable.TConditioned.GetConditions: TConditions.TReader;
begin
  Result := FConditions.Reader;
end;

procedure TLootTable.TConditioned.Load(AValue: TJValue);
var
  JCondition: TJValue;
begin
  ClearConditions;
  for JCondition in AValue['conditions'].AsArray do
    FConditions.Add(TCondition.CreateTyped(Self, JCondition));
end;

procedure TLootTable.TConditioned.RemoveCondition(ACondition: TCondition);
begin
  FConditions.Remove(ACondition);
end;

procedure TLootTable.TConditioned.Save(AValue: TJObject);
var
  JConditions: TJArray;
  Condition: TCondition;
begin
  if not FConditions.Empty then
  begin
    JConditions := AValue.AddArray('conditions');
    for Condition in Conditions do
      Condition.Save(JConditions.AddObject);
  end;
end;

procedure TLootTable.TConditioned.SetConditionIndex(ACondition: TCondition; const Value: Integer);
begin
  FConditions.SetIndex(ACondition.Index, Value);
end;

{ TLootTable.TFunction }

constructor TLootTable.TFunction.Create(AEntry: TEntryItem);
begin
  inherited Create;
  FEntry := AEntry;
end;

constructor TLootTable.TFunction.Create(AEntry: TEntryItem; AValue: TJValue);
begin
  Create(AEntry);
  Load(AValue);
end;

class function TLootTable.TFunction.CreateTyped(AEntry: TEntryItem; AValue: TJValue): TFunction;
var
  Func: TNSPath;
  T: TType;
begin
  Func := AValue['function'].AsString;
  for T := Low(TType) to High(TType) do
    if Func = FunctionClasses[T].GetName then
      Exit(FunctionClasses[T].Create(AEntry, AValue));
  raise ELootTable.Create('Unknown function type.');
end;

class function TLootTable.TFunction.GetDisplayName: string;
begin
  Result := FunctionDisplayNames[GetType];
end;

function TLootTable.TFunction.GetIndex: Integer;
begin
  Result := Entry.FunctionIndex[Self];
end;

class function TLootTable.TFunction.GetName: string;
begin
  Result := FunctionNames[GetType];
end;

procedure TLootTable.TFunction.Save(AValue: TJObject);
begin
  inherited;
  AValue.Add('function', GetName);
end;

procedure TLootTable.TFunction.SetIndex(const Value: Integer);
begin
  Entry.FunctionIndex[Self] := Value;
end;

{ TLootTable.TModifier }

constructor TLootTable.TModifier.Create(AFunction: TFunctionSetAttributes);
begin
  FFunction := AFunction;
end;

constructor TLootTable.TModifier.Create(AFunction: TFunctionSetAttributes; AValue: TJValue);
begin
  Create(AFunction);
  Load(AValue);
end;

function TLootTable.TModifier.GetIndex: Integer;
begin
  Result := Func.ModifierIndex[Self];
end;

procedure TLootTable.TModifier.Load(AValue: TJValue);
var
  JUUID: TJValue;
  JSlot: TJValue;
  Slot: TAttributeSlot;
begin
  Name := AValue['name'].AsString;

  if not AttributeFromName(AValue['attribute'].AsString, FAttribute) then
    raise ELootTable.Create('Unknown attribute type.');

  if not AttributeOperationFromName(AValue['operation'].AsString, FOperation) then
    raise ELootTable.Create('Unknown attribute operation.');

  Amount := LoadBounds(AValue['amount']);

  JUUID := AValue['id'];
  if JUUID.Exists then
    UUID.Value := TGUID.Create('{' + JUUID.AsString + '}');

  JSlot := AValue['slot'];
  if JSlot.IsString then
  begin
    if not AttributeSlotFromName(JSlot, Slot) then
      raise ELootTable.Create('Unknown attribute slot.');
    FSlots := [Slot];
  end
  else
  begin
    FSlots := [];
    for JSlot in JSlot.AsArray do
    begin
      if not AttributeSlotFromName(JSlot, Slot) then
        raise ELootTable.Create('Unknown attribute slot.');
      Include(FSlots, Slot);
    end;
  end;
end;

procedure TLootTable.TModifier.Save(AValue: TJObject);
var
  UUIDString: string;
  Slot: TAttributeSlot;
  SlotCount: Integer;
  JSlot: TJArray;
begin
  AValue.Add('name', Name);
  AValue.Add('attribute', AttributeNames[Attribute]);
  AValue.Add('operation', AttributeOperationNames[Operation]);
  SaveBounds(AValue, 'amount', Amount);
  if UUID.HasValue then
  begin
    UUIDString := UUID.Value.ToString;
    AValue.Add('id', UUIDString.Substring(1, UUIDString.Length - 2));
  end;
  SlotCount := 0;
  for Slot in Slots do
    Inc(SlotCount);
  if SlotCount = 1 then
  begin
    // actually only ran once to find the element
    for Slot in Slots do
      AValue.Add('slot', AttributeSlotNames[Slot]);
  end
  else
  begin
    JSlot := AValue.AddArray('slot');
    for Slot in Slots do
      JSlot.Add(AttributeSlotNames[Slot]);
  end;
end;

procedure TLootTable.TModifier.SetAmount(const Value: TBounds1);
begin
  if Amount = Value then
    Exit;
  FAmount := Value;
end;

procedure TLootTable.TModifier.SetAmountMax(const Value: Single);
begin
  if AmountMax = Value then
    Exit;
  FAmount.C2 := Value;
end;

procedure TLootTable.TModifier.SetAmountMin(const Value: Single);
begin
  if AmountMin = Value then
    Exit;
  FAmount.C1 := Value;
end;

procedure TLootTable.TModifier.SetAttribute(const Value: TAttribute);
begin
  if Attribute = Value then
    Exit;
  FAttribute := Value;
end;

procedure TLootTable.TModifier.SetIndex(const Value: Integer);
begin
  Func.ModifierIndex[Self] := Value;
end;

procedure TLootTable.TModifier.SetName(const Value: string);
begin
  if Name = Value then
    Exit;
  FName := Value;
end;

procedure TLootTable.TModifier.SetOperation(const Value: TAttributeOperation);
begin
  if Operation = Value then
    Exit;
  FOperation := Value;
end;

procedure TLootTable.TModifier.SetSlots(const Value: TAttributeSlots);
begin
  if Slots = Value then
    Exit;
  FSlots := Value;
end;

end.
