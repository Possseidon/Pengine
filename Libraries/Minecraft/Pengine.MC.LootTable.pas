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
  Pengine.EventHandling,
  System.JSON,
  
  Pengine.MC.NBT,
  Pengine.MC.Enchantment,
  Pengine.MC.Attribute;

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
      constructor Create(AConditioned: TConditioned; AValue: TJSONValue); overload;

      class function GetType: TType; virtual; abstract;
      class function GetName: string;
      class function GetDisplayName: string;

      procedure Load(AValue: TJSONValue); virtual;

      class function CreateTyped(AConditioned: TConditioned; AValue: TJSONValue): TCondition; static;

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

      TEventInfo = TSenderEventInfo<TConditionEntity>;

      TEvent = TEvent<TEventInfo>;

      TTargetChangeEventInfo = class(TEventInfo)
      private
        FOldTarget: TTarget;

      public
        constructor Create(ASender: TConditionEntity; AOldTarget: TTarget);

        property OldTarget: TTarget read FOldTarget;

      end;

      TTargetChangeEvent = TEvent<TTargetChangeEventInfo>;

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
      FOnTargetChange: TTargetChangeEvent;

      procedure SetTarget(const Value: TTarget);

    public
      procedure Load(AValue: TJSONValue); override;

      property Target: TTarget read FTarget write SetTarget;

      function OnTargetChange: TTargetChangeEvent.TAccess;

    end;

    TConditionEntityProperties = class(TConditionEntity)
    public type

      TEventInfo = TSenderEventInfo<TConditionEntity>;

      TEvent = TEvent<TEventInfo>;

      TChangedEventInfo = class(TEventInfo)
      private
        FOnFire: TOpt<Boolean>;

      public
        constructor Create(ASender: TConditionEntity; AOnFire: TOpt<Boolean>);
        destructor Destroy; override;

        property OnFire: TOpt<Boolean> read FOnFire;

      end;

    private
      FOnFire: TOpt<Boolean>;

    public
      constructor Create(AConditioned: TConditioned); override;
      destructor Destroy; override;

      class function GetType: TCondition.TType; override;

      procedure Load(AValue: TJSONValue); override;

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

      procedure Load(AValue: TJSONValue); override;

      property Scores: TScores.TReader read GetScores;
      procedure AddScore(AObjective: string; AValue: TIntBounds1);
      procedure RemoveScore(AObjective: string);

    end;

    /// <summary>Checks, wether the killer was a player. Can check for opposite if inverse is set to true.</summary>
    TConditionKilledByPlayer = class(TCondition)
    public type

      TEventInfo = TSenderEventInfo<TConditionKilledByPlayer>;

      TEvent = TEvent<TEventInfo>;

    private
      FInverse: Boolean;
      FOnInverseChange: TEvent;

      procedure SetInverse(const Value: Boolean);

    public
      class function GetType: TCondition.TType; override;

      procedure Load(AValue: TJSONValue); override;

      property Inverse: Boolean read FInverse write SetInverse;

      function OnInverseChange: TConditionKilledByPlayer.TEvent.TAccess;

    end;

    /// <summary>A condition that passes based on random chance.</summary>
    TConditionRandomChance = class(TCondition)
    public type

      TEventInfo = TSenderEventInfo<TConditionRandomChance>;

      TEvent = TEvent<TEventInfo>;

      TChanceChangeEventInfo = class(TEventInfo)
      private
        FChance: Single;

      public
        constructor Create(ASender: TConditionRandomChance; AChance: Single);

        property Chance: Single read FChance;

      end;

      TChanceChangeEvent = TEvent<TChanceChangeEventInfo>;

    private
      FChance: Single;
      FOnChanceChange: TChanceChangeEvent;

      procedure SetChance(const Value: Single);

    public
      class function GetType: TCondition.TType; override;

      procedure Load(AValue: TJSONValue); override;

      property Chance: Single read FChance write SetChance;

      function OnChanceChange: TChanceChangeEvent.TAccess;

    end;

    /// <summary>A condition that passes based on random chance, but can include a looting multiplier.</summary>
    TConditionRandomChanceWithLooting = class(TConditionRandomChance)
    public type

      TEventInfo = TSenderEventInfo<TConditionRandomChanceWithLooting>;

      TEvent = TEvent<TEventInfo>;

      TLootingMultiplierChangeEventInfo = class(TEventInfo)
      private
        FLootingMultiplier: Single;

      public
        constructor Create(ASender: TConditionRandomChanceWithLooting; ALootingMultiplier: Single);

        property LootingMultiplier: Single read FLootingMultiplier;

      end;

      TLootingMultiplierChangeEvent = TEvent<TLootingMultiplierChangeEventInfo>;

    private
      FLootingMultiplier: Single;
      FOnLootingMultiplierChange: TLootingMultiplierChangeEvent;

    public
      class function GetType: TCondition.TType; override;

      procedure Load(AValue: TJSONValue); override;

      property LootingMultiplier: Single read FLootingMultiplier write FLootingMultiplier;

      function OnLootingMultiplierChange: TLootingMultiplierChangeEvent.TAccess;

    end;

    TConditioned = class
    public type

      TEventInfo = TSenderEventInfo<TConditioned>;

      TEvent = TEvent<TEventInfo>;

      TConditionEventInfo = class(TEventInfo)
      private
        FCondition: TCondition;

      public
        constructor Create(ASender: TConditioned; ACondition: TCondition);

        property Condition: TCondition read FCondition;

      end;

      TConditionEvent = TEvent<TConditionEventInfo>;

      TConditionMoveEventInfo = class(TConditionEventInfo)
      private
        FOldIndex: Integer;

      public
        constructor Create(ASender: TConditioned; ACondition: TCondition; AOldIndex: Integer);

        property OldIndex: Integer read FOldIndex;

      end;

      TConditionMoveEvent = TEvent<TConditionMoveEventInfo>;

    private
      FConditions: TConditions;
      FOnAddCondition: TConditionEvent;
      FOnRemoveCondition: TConditionEvent;
      FOnClearConditions: TEvent;
      FOnMoveCondition: TConditionMoveEvent;

      function GetConditions: TConditions.TReader;

      function GetConditionIndex(ACondition: TCondition): Integer;
      procedure SetConditionIndex(ACondition: TCondition; const Value: Integer);

    public
      constructor Create;
      destructor Destroy; override;

      procedure Load(AValue: TJSONValue); virtual;

      property Conditions: TConditions.TReader read GetConditions;

      property ConditionIndex[ACondition: TCondition]: Integer read GetConditionIndex write SetConditionIndex;

      function AddCondition<T: TCondition>: T; overload;
      function AddCondition(T: TConditionClass): TCondition; overload;
      procedure RemoveCondition(ACondition: TCondition);
      procedure ClearConditions;

      function OnAddCondition: TConditionEvent.TAccess;
      function OnRemoveCondition: TConditionEvent.TAccess;
      function OnClearConditions: TConditioned.TEvent.TAccess;
      function OnMoveCondition: TConditionMoveEvent.TAccess;

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
        ftSetAttribute,
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
      constructor Create(AEntry: TEntryItem; AValue: TJSONValue); overload;

      class function CreateTyped(AEntry: TEntryItem; AValue: TJSONValue): TFunction;

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
    public type

      TEventInfo = TSenderEventInfo<TFunctionEnchantRandomly>;

      TEvent = TEvent<TEventInfo>;

      TEnchantmentsChangeEventInfo = class(TEventInfo)
      private
        FChangedEnchantments: TEnchantments;

      public
        constructor Create(ASender: TFunctionEnchantRandomly; AChangedEnchantments: TEnchantments);

        property ChangedEnchantments: TEnchantments read FChangedEnchantments;

      end;

      TEnchantmentsChangeEvent = TEvent<TEnchantmentsChangeEventInfo>;

    private
      FEnchantments: TEnchantments;
      FOnEnchantmentsChange: TEnchantmentsChangeEvent;

      procedure SetEnchantments(const Value: TEnchantments);

    public
      class function GetType: TFunction.TType; override;

      procedure Load(AValue: TJSONValue); override;

      property Enchantments: TEnchantments read FEnchantments write SetEnchantments;

      function OnEchantmentsChange: TEnchantmentsChangeEvent.TAccess;

    end;

    /// <summary>Enchants the item with a given amount of levels.</summary>
    TFunctionEnchantWithLevels = class(TFunction)
    public type

      TEventInfo = TSenderEventInfo<TFunctionEnchantWithLevels>;

      TEvent = TEvent<TEventInfo>;

      TLevelsChangeEventInfo = class(TEventInfo)
      private
        FOldLevels: TIntBounds1;

      public
        constructor Create(ASender: TFunctionEnchantWithLevels; AOldLevels: TIntBounds1);
                       
        property OldLevels: TIntBounds1 read FOldLevels;

      end;

      TLevelsChangeEvent = TEvent<TLevelsChangeEventInfo>;

    private
      FTreasure: Boolean;
      FLevels: TIntBounds1;
      FOnTreasureChange: TEvent;
      FOnLevelsChange: TLevelsChangeEvent;

      procedure SetLevels(const Value: TIntBounds1);
      procedure SetLevelsMax(const Value: Integer);
      procedure SetLevelsMin(const Value: Integer);
      procedure SetTreasure(const Value: Boolean);

    public
      class function GetType: TFunction.TType; override;
                  
      procedure Load(AValue: TJSONValue); override;

      property Treasure: Boolean read FTreasure write SetTreasure;
      property Levels: TIntBounds1 read FLevels write SetLevels;
      property LevelsMin: Integer read FLevels.C1 write SetLevelsMin;
      property LevelsMax: Integer read FLevels.C2 write SetLevelsMax;

      function OnTreasureChange: TFunctionEnchantWithLevels.TEvent.TAccess;
      function OnLevelsChange: TLevelsChangeEvent.TAccess;

    end;

    /// <summary>Converts an empty map item into an explorer map.</summary>
    TFunctionExplorationMap = class(TFunction)
    public type

      TEventInfo = TSenderEventInfo<TFunctionExplorationMap>;

      TEvent = TEvent<TEventInfo>;

      TDestinationChangeEventInfo = class(TEventInfo)
      private
        FOldDestination: string;

      public
        constructor Create(ASender: TFunctionExplorationMap; AOldDestination: string);

        property OldDestination: string read FOldDestination;

      end;

      TDestinationChangeEvent = TEvent<TDestinationChangeEventInfo>;

      TDecorationChangeEventInfo = class(TEventInfo)
      private
        FOldDecoration: string;

      public
        constructor Create(ASender: TFunctionExplorationMap; AOldDecoration: string);

        property OldDecoration: string read FOldDecoration;

      end;

      TDecorationChangeEvent = TEvent<TDecorationChangeEventInfo>;

      TZoomChangeEventInfo = class(TEventInfo)
      private
        FOldZoom: Integer;

      public
        constructor Create(ASender: TFunctionExplorationMap; AOldZoom: Integer);

        property OldZoom: Integer read FOldZoom;

      end;

      TZoomChangeEvent = TEvent<TZoomChangeEventInfo>;

      TSearchRadiusChangeEventInfo = class(TEventInfo)
      private
        FOldSearchRadius: Integer;

      public
        constructor Create(ASender: TFunctionExplorationMap; AOldSearchRadius: Integer);

        property OldSearchRadius: Integer read FOldSearchRadius;

      end;

      TSearchRadiusChangeEvent = TEvent<TSearchRadiusChangeEventInfo>;

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
      FOnDestinationChange: TDestinationChangeEvent;
      FOnDecorationChange: TDecorationChangeEvent;
      FOnZoomChange: TZoomChangeEvent;
      FOnSearchRadiusChange: TSearchRadiusChangeEvent;
      FOnSkipExistingChunksChange: TEvent;

      procedure SetDecoration(const Value: string);
      procedure SetDestination(const Value: string);
      procedure SetSearchRadius(const Value: Integer);
      procedure SetSkipExistingChunks(const Value: Boolean);
      procedure SetZoom(const Value: Integer);

    public
      constructor Create(AEntry: TEntryItem); overload; override;

      class function GetType: TFunction.TType; override;
                       
      procedure Load(AValue: TJSONValue); override;

      property Destination: string read FDestination write SetDestination;
      property Decoration: string read FDecoration write SetDecoration;
      property Zoom: Integer read FZoom write SetZoom;
      property SearchRadius: Integer read FSearchRadius write SetSearchRadius;
      property SkipExistingChunks: Boolean read FSkipExistingChunks write SetSkipExistingChunks;

      function OnDestinationChange: TDestinationChangeEvent.TAccess;
      function OnDecorationChange: TDecorationChangeEvent.TAccess;
      function OnZoomChange: TZoomChangeEvent.TAccess;
      function OnSearchRadiusChange: TSearchRadiusChangeEvent.TAccess;
      function OnSkipExistingChunksChange: TFunctionExplorationMap.TEvent.TAccess;

    end;

    /// <summary>Smelts the item similar to what a furnace would do.</summary>
    TFunctionFurnaceSmelt = class(TFunction)
    public
      class function GetType: TFunction.TType; override;

    end;

    /// <summary>Adjust the item count based on the killers looting level.</summary>
    TFunctionLootingEnchant = class(TFunction)
    public type

      TEventInfo = TSenderEventInfo<TFunctionLootingEnchant>;

      TEvent = TEvent<TEventInfo>;

      TCountChangeEventInfo = class(TEventInfo)
      private
        FOldCount: TIntBounds1;

      public
        constructor Create(ASender: TFunctionLootingEnchant; AOldCount: TIntBounds1);

        property OldCount: TIntBounds1 read FOldCount;

      end;

      TCountChangeEvent = TEvent<TCountChangeEventInfo>;

      TLimitChangeEventInfo = class(TEventInfo)
      private
        FOldLimit: Integer;

      public
        constructor Create(ASender: TFunctionLootingEnchant; AOldLimit: Integer);

        property OldLimit: Integer read FOldLimit;

      end;

      TLimitChangeEvent = TEvent<TLimitChangeEventInfo>;

    private
      FCount: TIntBounds1;
      FLimit: Integer;
      FOnCountChange: TCountChangeEvent;
      FOnLimitChange: TLimitChangeEvent;

      procedure SetCount(const Value: TIntBounds1);
      procedure SetCountMax(const Value: Integer);
      procedure SetCountMin(const Value: Integer);
      procedure SetLimit(const Value: Integer);

    public
      class function GetType: TFunction.TType; override;

      procedure Load(AValue: TJSONValue); override;
      
      property Count: TIntBounds1 read FCount write SetCount;
      property CountMin: Integer read FCount.C1 write SetCountMin;
      property CountMax: Integer read FCount.C2 write SetCountMax;
      property Limit: Integer read FLimit write SetLimit;

      function OnCountChange: TCountChangeEvent.TAccess;
      function OnLimitChange: TLimitChangeEvent.TAccess;

    end;

    TFunctionSetAttribute = class;

    TModifier = class
    public type

      TEventInfo = TSenderEventInfo<TModifier>;

      TEvent = TEvent<TEventInfo>;

      TNameChangeEventInfo = class(TEventInfo)
      private
        FOldName: string;

      public
        constructor Create(ASender: TModifier; AOldName: string);

        property OldName: string read FOldName;

      end;

      TNameChangeEvent = TEvent<TNameChangeEventInfo>;

      TAttributeChangeEventInfo = class(TEventInfo)
      private
        FOldAttribute: TAttribute;

      public
        constructor Create(ASender: TModifier; AOldAttribute: TAttribute);

        property OldAttribute: TAttribute read FOldAttribute;

      end;

      TAttributeChangeEvent = TEvent<TAttributeChangeEventInfo>;

      TOperationChangeEventInfo = class(TEventInfo)
      private
        FOldOperation: TAttributeOperation;

      public
        constructor Create(ASender: TModifier; AOldOperation: TAttributeOperation);

        property OldOperation: TAttributeOperation read FOldOperation;

      end;

      TOperationChangeEvent = TEvent<TOperationChangeEventInfo>;

      TAmountChangeEventInfo = class(TEventInfo)
      private
        FOldAmount: TBounds1;

      public
        constructor Create(ASender: TModifier; AOldAmount: TBounds1);

        property OldAmount: TBounds1 read FOldAmount;

      end;

      TAmountChangeEvent = TEvent<TAmountChangeEventInfo>;

      TSlotsChangeEventInfo = class(TEventInfo)
      private
        FChangedSlots: TAttributeSlots;

      public
        constructor Create(ASender: TModifier; AChangedSlots: TAttributeSlots);

        property ChangedSlots: TAttributeSlots read FChangedSlots;

      end;

      TSlotsChangeEvent = TEvent<TSlotsChangeEventInfo>;

    private
      FFunction: TFunctionSetAttribute;
      FName: string;
      FAttribute: TAttribute;
      FOperation: TAttributeOperation;
      FAmount: TBounds1;
      FSlots: TAttributeSlots;
      FOnNameChange: TNameChangeEvent;
      FOnAttributeChange: TAttributeChangeEvent;
      FOnOperationChange: TOperationChangeEvent;
      FOnAmountChange: TAmountChangeEvent;
      FOnSlotsChange: TSlotsChangeEvent;

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
      constructor Create(AFunction: TFunctionSetAttribute);

      property Func: TFunctionSetAttribute read FFunction;
      property Index: Integer read GetIndex write SetIndex;

      property Name: string read FName write SetName;
      property Attribute: TAttribute read FAttribute write SetAttribute;
      property Operation: TAttributeOperation read FOperation write SetOperation;
      property Amount: TBounds1 read FAmount write SetAmount;
      property AmountMin: Single read FAmount.C1 write SetAmountMin;
      property AmountMax: Single read FAmount.C1 write SetAmountMax;
      property Slots: TAttributeSlots read FSlots write SetSlots;

      function OnNameChange: TNameChangeEvent.TAccess;
      function OnAttributeChange: TAttributeChangeEvent.TAccess;
      function OnOperationChange: TOperationChangeEvent.TAccess;
      function OnAmountChange: TAmountChangeEvent.TAccess;
      function OnSlotsChange: TSlotsChangeEvent.TAccess;

    end;

    TModifiers = TObjectArray<TModifier>;

    /// <summary>Adds an attribute modifier to the item.</summary>
    TFunctionSetAttribute = class(TFunction)
    public type

      TEventInfo = TSenderEventInfo<TFunctionSetAttribute>;

      TEvent = TEvent<TEventInfo>;

      TModifierEventInfo = class(TEventInfo)
      private
        FModifier: TModifier;

      public
        constructor Create(ASender: TFunctionSetAttribute; AModifier: TModifier);

        property Modifier: TModifier read FModifier;

      end;

      TModifierEvent = TEvent<TModifierEventInfo>;

      TModifierMoveEventInfo = class(TModifierEventInfo)
      private
        FOldIndex: Integer;

      public
        constructor Create(ASender: TFunctionSetAttribute; AModifier: TModifier; AOldIndex: Integer);

        property OldIndex: Integer read FOldIndex;

      end;

      TModifierMoveEvent = TEvent<TModifierMoveEventInfo>;

    private
      FModifiers: TModifiers;
      FOnAddModifier: TModifierEvent;
      FOnRemoveModifier: TModifierEvent;
      FOnClearModifiers: TEvent;
      FOnMoveModifier: TModifierMoveEvent;

      function GetModifieres: TModifiers.TReader;
      function GetModifierIndex(AModifier: TModifier): Integer;
      procedure SetModifierIndex(AModifier: TModifier; const Value: Integer);

    public
      constructor Create(AEntry: TEntryItem); overload; override;
      destructor Destroy; override;

      class function GetType: TFunction.TType; override;
                   
      procedure Load(AValue: TJSONValue); override;
      
      property Modifiers: TModifiers.TReader read GetModifieres;
      function AddModifier: TModifier;
      procedure RemoveModifier(AModifier: TModifier);
      procedure ClearModifiers;
      property ModifierIndex[AModifier: TModifier]: Integer read GetModifierIndex write SetModifierIndex;

      function OnAddModifier: TModifierEvent.TAccess;
      function OnRemoveModifier: TModifierEvent.TAccess;
      function OnClearModifiers: TFunctionSetAttribute.TEvent.TAccess;
      function OnMoveModifier: TModifierMoveEvent.TAccess;

    end;

    /// <summary>Sets the item stack size.</summary>
    TFunctionSetCount = class(TFunction)
    public type

      TEventInfo = TSenderEventInfo<TFunctionSetCount>;

      TEvent = TEvent<TEventInfo>;

      TCountChangeEventInfo = class(TEventInfo)
      private
        FOldCount: TIntBounds1;

      public
        constructor Create(ASender: TFunctionSetCount; AOldCount: TIntBounds1);

        property OldCount: TIntBounds1 read FOldCount;

      end;

      TCountChangeEvent = TEvent<TCountChangeEventInfo>;

    private
      FCount: TIntBounds1;
      FOnCountChange: TCountChangeEvent;

      procedure SetCount(const Value: TIntBounds1);
      procedure SetCountMin(const Value: Integer);
      procedure SetCountMax(const Value: Integer);

    public
      constructor Create(AEntry: TEntryItem); overload; override;

      class function GetType: TFunction.TType; override;
                     
      procedure Load(AValue: TJSONValue); override;
      
      property Count: TIntBounds1 read FCount write SetCount;
      property CountMin: Integer read FCount.C1 write SetCountMin;
      property CountMax: Integer read FCount.C2 write SetCountMax;

      function OnCountChange: TCountChangeEvent.TAccess;

    end;

    /// <summary>Sets the items durability as a percentage.</summary>
    TFunctionSetDamage = class(TFunction)
    public type

      TEventInfo = TSenderEventInfo<TFunctionSetDamage>;

      TEvent = TEvent<TEventInfo>;

      TDamageChangeEventInfo = class(TEventInfo)
      private
        FOldDamage: TBounds1;

      public
        constructor Create(ASender: TFunctionSetDamage; AOldDamage: TBounds1);

        property OldDamage: TBounds1 read FOldDamage;

      end;

      TDamageChangeEvent = TEvent<TDamageChangeEventInfo>;

    private
      FDamage: TBounds1;
      FOnDamageChange: TDamageChangeEvent;

      procedure SetDamage(const Value: TBounds1);
      procedure SetDamageMin(const Value: Single);
      procedure SetDamageMax(const Value: Single);

    public
      constructor Create(AEntry: TEntryItem); overload; override;

      class function GetType: TFunction.TType; override;
                     
      procedure Load(AValue: TJSONValue); override;
      
      property Damage: TBounds1 read FDamage write SetDamage;
      property DamageMin: Single read FDamage.C1 write SetDamageMin;
      property DamageMax: Single read FDamage.C2 write SetDamageMax;

      function OnDamageChange: TDamageChangeEvent.TAccess;

    end;

    /// <summary>Sets the items metadata.</summary>
    TFunctionSetData = class(TFunction)
    public type

      TEventInfo = TSenderEventInfo<TFunctionSetData>;

      TEvent = TEvent<TEventInfo>;

      TDataChangeEventInfo = class(TEventInfo)
      private
        FOldData: TIntBounds1;

      public
        constructor Create(ASender: TFunctionSetData; AOldData: TIntBounds1);

        property OldData: TIntBounds1 read FOldData;

      end;

      TDataChangeEvent = TEvent<TDataChangeEventInfo>;

    private
      FData: TIntBounds1;
      FOnDataChange: TDataChangeEvent;

      procedure SetData(const Value: TIntBounds1);
      procedure SetDataMin(const Value: Integer);
      procedure SetDataMax(const Value: Integer);

    public
      class function GetType: TFunction.TType; override;
                     
      procedure Load(AValue: TJSONValue); override;
      
      property Data: TIntBounds1 read FData write SetData;
      property DataMin: Integer read FData.C1 write SetDataMin;
      property DataMax: Integer read FData.C2 write SetDataMax;

      function OnDataChange: TDataChangeEvent.TAccess;

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
                        
      procedure Load(AValue: TJSONValue); override;
      
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

      TEventInfo = TSenderEventInfo<TEntry>;

      TEvent = TEvent<TEventInfo>;

      TWeightChangeEventInfo = class(TEventInfo)
      private
        FOldWeight: Integer;

      public
        constructor Create(ASender: TEntry; AOldWeight: Integer);

        property OldWeight: Integer read FOldWeight;

      end;

      TWeightChangeEvent = TEvent<TWeightChangeEventInfo>;

      TQualityChangeEventInfo = class(TEventInfo)
      private
        FOldQuality: Integer;

      public
        constructor Create(ASender: TEntry; AOldQuality: Integer);

        property OldQuality: Integer read FOldQuality;

      end;

      TQualityChangeEvent = TEvent<TQualityChangeEventInfo>;

    private
      FPool: TPool;
      FWeight: Integer;
      FQuality: Integer;
      FOnWeightChange: TWeightChangeEvent;
      FOnQualityChange: TQualityChangeEvent;

      function GetIndex: Integer;
      procedure SetIndex(const Value: Integer);

      procedure SetWeight(const Value: Integer);
      procedure SetQuality(const Value: Integer);

    public
      constructor Create(APool: TPool); overload; virtual;
      constructor Create(APool: TPool; AValue: TJSONValue); overload;

      procedure Load(AValue: TJSONValue); override;

      class function CreateTyped(APool: TPool; AValue: TJSONValue): TEntry;

      class function GetType: TType; virtual; abstract;
      class function GetName: string;
      class function GetDisplayName: string;

      property Pool: TPool read FPool;
      property Index: Integer read GetIndex write SetIndex;

      property Weight: Integer read FWeight write SetWeight;
      property Quality: Integer read FQuality write SetQuality;

      function OnWeightChange: TWeightChangeEvent.TAccess;
      function OnQualityChange: TQualityChangeEvent.TAccess;

    end;

    TEntryClass = class of TEntry;

    /// <summary>An item entry can have multiple functions, that get applied, if their conditions are met.</summary>
    TEntryItem = class(TEntry)
    public type

      TEventInfo = TSenderEventInfo<TEntryItem>;

      TEvent = TEvent<TEventInfo>;

      TItemChangeEventInfo = class(TEventInfo)
      private
        FOldItem: string;

      public
        constructor Create(ASender: TEntryItem; AOldItem: string);

        property OldItem: string read FOldItem;

      end;

      TItemChangeEvent = TEvent<TItemChangeEventInfo>;

      TFunctionEventInfo = class(TEventInfo)
      private
        FFunc: TFunction;

      public
        constructor Create(ASender: TEntryItem; AFunc: TFunction);

        property Func: TFunction read FFunc;

      end;

      TFunctionEvent = TEvent<TFunctionEventInfo>;

      TFunctionMoveEventInfo = class(TFunctionEventInfo)
      private
        FOldIndex: Integer;

      public
        constructor Create(ASender: TEntryItem; AFunc: TFunction; AOldIndex: Integer);

        property OldIndex: Integer read FOldIndex;

      end;

      TFunctionMoveEvent = TEvent<TFunctionMoveEventInfo>;

    private
      FItem: string;
      FFunctions: TFunctions;
      FOnItemChange: TItemChangeEvent;
      FOnAddFunction: TFunctionEvent;
      FOnRemoveFunction: TFunctionEvent;
      FOnClearFunctions: TEvent;
      FOnMoveFunction: TFunctionMoveEvent;

      procedure SetItem(const Value: string);

      function GetFunctions: TFunctions.TReader;
      function GetFunctionIndex(AFunction: TFunction): Integer;
      procedure SetFunctionIndex(AFunction: TFunction; const Value: Integer);

    public
      constructor Create(APool: TPool); override;
      destructor Destroy; override;
      
      procedure Load(AValue: TJSONValue); override;

      class function GetType: TEntry.TType; override;

      property Item: string read FItem write SetItem;

      property Functions: TFunctions.TReader read GetFunctions;
      function AddFunction<T: TFunction>: T; overload;
      function AddFunction(AFunctionClass: TFunctionClass): TFunction; overload;
      procedure RemoveFunction(AFunction: TFunction);
      procedure ClearFunctions;
      property FunctionIndex[AFunction: TFunction]: Integer read GetFunctionIndex write SetFunctionIndex;

      function OnItemChange: TItemChangeEvent.TAccess;

    end;

    /// <summary>An entry, that generates its items from another loot table in the datapack.</summary>
    TEntryLootTable = class(TEntry)
    public type

      TEventInfo = TSenderEventInfo<TEntryLootTable>;

      TEvent = TEvent<TEventInfo>;

      TLootTableChangeEventInfo = class(TEventInfo)
      private
        FOldLootTable: string;

      public
        constructor Create(ASender: TEntryLootTable; AOldLootTable: string);

        property OldLootTable: string read FOldLootTable;

      end;

      TLootTableChangeEvent = TEvent<TLootTableChangeEventInfo>;
    
    private
      FLootTable: string;
      FOnLootTableChange: TLootTableChangeEvent;

      procedure SetLootTable(const Value: string);

    public
      class function GetType: TEntry.TType; override;

      procedure Load(AValue: TJSONValue); override;
      
      property LootTable: string read FLootTable write SetLootTable;
      
      function OnLootTableChange: TLootTableChangeEvent.TAccess;
      
    end;

    /// <summary>An entry, that generates nothing. Is this actually useful? Tbh I don't think so...</summary>
    TEntryEmpty = class(TEntry)
    public
      class function GetType: TEntry.TType; override;

    end;

    TEntries = TObjectArray<TEntry>;

    TPool = class(TConditioned)
    public type

      TEventInfo = TSenderEventInfo<TPool>;

      TEvent = TEvent<TEventInfo>;

      TRollsChangeEventInfo = class(TEventInfo)
      private
        FOldRolls: TIntBounds1;

      public
        constructor Create(ASender: TPool; AOldRolls: TIntBounds1);

        property OldRolls: TIntBounds1 read FOldRolls;

      end;

      TRollsChangeEvent = TEvent<TRollsChangeEventInfo>;

      TBonusRollsChangeEventInfo = class(TEventInfo)
      private
        FOldBonusRolls: TBounds1;

      public
        constructor Create(ASender: TPool; AOldBonusRolls: TBounds1);

        property OldRolls: TBounds1 read FOldBonusRolls;

      end;

      TBonusRollsChangeEvent = TEvent<TBonusRollsChangeEventInfo>;

      TEntryEventInfo = class(TEventInfo)
      private
        FEntry: TEntry;

      public
        constructor Create(ASender: TPool; AEntry: TEntry);

        property Entry: TEntry read FEntry;

      end;

      TEntryEvent = TEvent<TEntryEventInfo>;

      TEntryMoveEventInfo = class(TEntryEventInfo)
      private
        FOldIndex: Integer;

      public
        constructor Create(ASender: TPool; AEntry: TEntry; AOldIndex: Integer);

        property OldIndex: Integer read FOldIndex;

      end;

      TEntryMoveEvent = TEvent<TEntryMoveEventInfo>;

    private
      FLootTable: TLootTable;
      FRolls: TIntBounds1;
      FBonusRolls: TBounds1;
      FEntries: TEntries;
      FOnRollsChange: TRollsChangeEvent;
      FOnBonusRollsChange: TBonusRollsChangeEvent;
      FOnAddEntry: TEntryEvent;
      FOnRemoveEntry: TEntryEvent;
      FOnClearEntries: TEvent;
      FOnMoveEntry: TEntryMoveEvent;

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

      function GetOnRollsChange: TRollsChangeEvent.TAccess;
      function GetOnBonusRollsChange: TBonusRollsChangeEvent.TAccess;

    public
      constructor Create(ALootTable: TLootTable); overload;
      constructor Create(ALootTable: TLootTable; AValue: TJSONValue); overload;
      destructor Destroy; override;

      procedure Load(AValue: TJSONValue); override;

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

      property OnRollsChange: TRollsChangeEvent.TAccess read GetOnRollsChange;
      property OnBonusRollsChange: TBonusRollsChangeEvent.TAccess read GetOnBonusRollsChange;

      function OnAddEntry: TEntryEvent.TAccess;
      function OnRemoveEntry: TEntryEvent.TAccess;
      function OnClearEntries: TPool.TEvent.TAccess;
      function OnMoveEntry: TEntryMoveEvent.TAccess;
      
    end;

    TPools = TObjectArray<TPool>;

    TEventInfo = TSenderEventInfo<TLootTable>;

    TEvent = TEvent<TEventInfo>;

    TPoolEventInfo = class(TEventInfo)
    private
      FPool: TPool;

    public
      constructor Create(ASender: TLootTable; APool: TPool);

      property Pool: TPool read FPool;

    end;

    TPoolEvent = TEvent<TPoolEventInfo>;

    TPoolMoveEventInfo = class(TPoolEventInfo)
    private
      FOldIndex: Integer;

    public
      constructor Create(ASender: TLootTable; APool: TPool; AOldIndex: Integer);

      property OldIndex: Integer read FOldIndex;

    end;

    TPoolMoveEvent = TEvent<TPoolMoveEventInfo>;

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
      TFunctionSetAttribute,
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
      'set_attribute',
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
    FOnAddPool: TPoolEvent;
    FOnRemovePool: TPoolEvent;
    FOnClearPools: TEvent;
    FOnMovePool: TPoolMoveEvent;

    function GetPools: TPools.TReader;
    function GetPoolIndex(APool: TPool): Integer;
    procedure SetPoolIndex(APool: TPool; const Value: Integer);

  public
    constructor Create; overload;
    constructor Create(AValue: TJSONValue); overload;
    destructor Destroy; override;

    procedure Load(AValue: TJSONValue);

    property Pools: TPools.TReader read GetPools;

    function AddPool: TPool;
    procedure RemovePool(APool: TPool);
    property PoolIndex[APool: TPool]: Integer read GetPoolIndex write SetPoolIndex;

    function OnAddPool: TPoolEvent.TAccess;
    function OnRemovePool: TPoolEvent.TAccess;
    function OnClearPools: TLootTable.TEvent.TAccess;
    function OnMovePool: TPoolMoveEvent.TAccess;

  end;

implementation

function ParseIntBounds(AValue: TJSONValue): TIntBounds1;
var
  NumberNode: TJSONNumber;
begin
  if AValue is TJSONNumber then
    Exit(TJSONNumber(AValue).AsInt);

  if AValue.TryGetValue<TJSONNumber>('min', NumberNode) then
    Result.C1 := NumberNode.AsInt
  else
    Result.C1 := 0;

  if AValue.TryGetValue<TJSONNumber>('max', NumberNode) then
    Result.C2 := NumberNode.AsInt
  else
    Result.C2 := 0;
end;

function ParseBounds(AValue: TJSONValue): TBounds1;
var
  NumberNode: TJSONNumber;
begin
  if AValue is TJSONNumber then
    Exit(TJSONNumber(AValue).AsDouble);

  if AValue.TryGetValue<TJSONNumber>('min', NumberNode) then
    Result.C1 := NumberNode.AsDouble
  else
    Result.C1 := 0;

  if AValue.TryGetValue<TJSONNumber>('max', NumberNode) then
    Result.C2 := NumberNode.AsDouble
  else
    Result.C2 := 0;
end;

{ TLootTable }

constructor TLootTable.Create;
begin
  FPools := TPools.Create;
end;

function TLootTable.AddPool: TPool;
begin
  Result := FPools.Add(TPool.Create);
end;

constructor TLootTable.Create(AValue: TJSONValue);
begin
  Create;
  Load(AValue);
end;

destructor TLootTable.Destroy;
begin
  FPools.Free;
  inherited;
end;

function TLootTable.OnAddPool: TPoolEvent.TAccess;
begin
  Result := FOnAddPool.Access;
end;

function TLootTable.OnClearPools: TLootTable.TEvent.TAccess;
begin
  Result := FOnClearPools.Access;
end;

function TLootTable.OnMovePool: TPoolMoveEvent.TAccess;
begin
  Result := FOnMovePool.Access;
end;

function TLootTable.OnRemovePool: TPoolEvent.TAccess;
begin
  Result := FOnRemovePool.Access;
end;

function TLootTable.GetPoolIndex(APool: TPool): Integer;
begin
  Result := Pools.Find(APool);
end;

function TLootTable.GetPools: TPools.TReader;
begin
  Result := FPools.Reader;
end;

procedure TLootTable.Load(AValue: TJSONValue);
var
  PoolsNode: TJSONArray;
  PoolNode: TJSONValue;
  Pool: TPool;
begin
  if AValue.TryGetValue<TJSONArray>('pools', PoolsNode) then
  begin
    for PoolNode in PoolsNode do
    begin
      Pool := TPool.Create(Self, PoolNode);
      FPools.Add(Pool);
      FOnAddPool.Execute(TPoolEventInfo.Create(Self, Pool));
    end;
  end;
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

constructor TLootTable.TConditionEntityProperties.Create(AConditioned: TConditioned);
begin
  inherited;
  FOnFire := TOpt<Boolean>.Create;
end;

destructor TLootTable.TConditionEntityProperties.Destroy;
begin
  FOnFire.Free;
  inherited;
end;

class function TLootTable.TConditionEntityProperties.GetType: TCondition.TType;
begin
  Result := ctEntityProperties;
end;

procedure TLootTable.TConditionEntityProperties.Load(AValue: TJSONValue);
var
  PropertiesNode: TJSONValue;
  BoolNode: TJSONBool;
begin
  inherited;
  if not AValue.TryGetValue<TJSONValue>('properties', PropertiesNode) then
    raise ELootTable.Create('Properties node expected.');

  if PropertiesNode.TryGetValue<TJSONBool>('on_fire', BoolNode) then
    OnFire.Value := BoolNode.AsBoolean
  else
    OnFire.Clear;
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

procedure TLootTable.TConditionEntityScores.Load(AValue: TJSONValue);
var
  ScoresNode: TJSONObject;
  Score: TJSONPair;
begin
  inherited;
  if AValue.TryGetValue<TJSONObject>('scores', ScoresNode) then
    for Score in ScoresNode do
      AddScore(Score.JsonString.Value, ParseIntBounds(Score.JsonValue));
end;

procedure TLootTable.TConditionEntityScores.RemoveScore(AObjective: string);
begin
  FScores.Remove(AObjective);
end;

{ TLootTable.TConditionKilledByPlayer }

class function TLootTable.TConditionKilledByPlayer.GetType: TCondition.TType;
begin
  Result := ctKilledByPlayer;
end;

procedure TLootTable.TConditionKilledByPlayer.Load(AValue: TJSONValue);
var
  BoolNode: TJSONBool;
begin
  inherited;
  if AValue.TryGetValue<TJSONBool>('inverse', BoolNode) then
    Inverse := BoolNode.AsBoolean;
end;

function TLootTable.TConditionKilledByPlayer.OnInverseChange: TConditionKilledByPlayer.TEvent.TAccess;
begin
  Result := FOnInverseChange.Access;
end;

procedure TLootTable.TConditionKilledByPlayer.SetInverse(const Value: Boolean);
begin
  if Inverse = Value then
    Exit;
  FInverse := Value;
  FOnInverseChange.Execute(TEventInfo.Create(Self));
end;

{ TLootTable.TConditionRandomChance }

class function TLootTable.TConditionRandomChance.GetType: TCondition.TType;
begin
  Result := ctRandomChance;
end;

procedure TLootTable.TConditionRandomChance.Load(AValue: TJSONValue);
var
  NumberNode: TJSONNumber;
begin
  inherited;
  if AValue.TryGetValue<TJSONNumber>('chance', NumberNode) then
    Chance := NumberNode.AsDouble;
end;

function TLootTable.TConditionRandomChance.OnChanceChange: TChanceChangeEvent.TAccess;
begin
  Result := FOnChanceChange.Access;
end;

procedure TLootTable.TConditionRandomChance.SetChance(const Value: Single);
var
  OldValue: Single;
begin
  if Chance = Value then
    Exit;
  OldValue := Chance;
  FChance := Value;
  FOnChanceChange.Execute(TChanceChangeEventInfo.Create(Self, OldValue));
end;

{ TLootTable.TConditionRandomChanceWithLooting }

class function TLootTable.TConditionRandomChanceWithLooting.GetType: TCondition.TType;
begin
  Result := ctRandomChanceWithLooting;
end;

procedure TLootTable.TConditionRandomChanceWithLooting.Load(AValue: TJSONValue);
var
  NumberNode: TJSONNumber;
begin
  inherited;
  if AValue.TryGetValue<TJSONNumber>('looting_multiplier', NumberNode) then
    LootingMultiplier := NumberNode.AsDouble;
end;

function TLootTable.TConditionRandomChanceWithLooting.OnLootingMultiplierChange: TLootingMultiplierChangeEvent.TAccess;
begin
  Result := FOnLootingMultiplierChange.Access;
end;

{ TLootTable.TFunctionEnchantRandomly }

procedure TLootTable.TFunctionEnchantRandomly.SetEnchantments(const Value: TEnchantments);
var
  Changed: TEnchantments;
begin
  if Enchantments = Value then
    Exit;
  Changed := Value - (Value * Enchantments);
  FEnchantments := Value;
  FOnEnchantmentsChange.Execute(TEnchantmentsChangeEventInfo.Create(Self, Changed));
end;

class function TLootTable.TFunctionEnchantRandomly.GetType: TFunction.TType;
begin
  Result := ftEnchantRandomly;
end;

procedure TLootTable.TFunctionEnchantRandomly.Load(AValue: TJSONValue);
var
  EnchantmentsNode: TJSONArray;
  EnchantmentNode: TJSONValue;
  Enchantment: TEnchantment;
  Found: TEnchantments;
begin
  inherited;
  Found := [];
  if AValue.TryGetValue<TJSONArray>('enchantments', EnchantmentsNode) then
  begin
    for EnchantmentNode in EnchantmentsNode do
    begin
      if EnchantmentNode is TJSONString then
      begin
        for Enchantment := Low(TEnchantment) to High(TEnchantment) do
        begin
          if EnchantmentNode.Value = EnchantmentNames[Enchantment] then
          begin
            Include(Found, Enchantment);
            Break;
          end;
        end;
      end;      
    end;
  end;
  Enchantments := Found;
end;

function TLootTable.TFunctionEnchantRandomly.OnEchantmentsChange: TEnchantmentsChangeEvent.TAccess;
begin
  Result := FOnEnchantmentsChange.Access;
end;

{ TLootTable.TFunctionEnchantWithLevels }

class function TLootTable.TFunctionEnchantWithLevels.GetType: TFunction.TType;
begin
  Result := ftEnchantWithLevels;
end;

procedure TLootTable.TFunctionEnchantWithLevels.Load(AValue: TJSONValue);
var
  LevelsNode: TJSONValue;
  TreasureNode: TJSONBool;
begin
  inherited;
  
  if AValue.TryGetValue<TJSONValue>('levels', LevelsNode) then
    Levels := ParseIntBounds(LevelsNode)
  else
    Levels := 0;

  if AValue.TryGetValue<TJSONBool>('treasure', TreasureNode) then
    Treasure := TreasureNode.AsBoolean
  else
    Treasure := False;
end;

function TLootTable.TFunctionEnchantWithLevels.OnLevelsChange: TLevelsChangeEvent.TAccess;
begin
  Result := FOnLevelsChange.Access;
end;

function TLootTable.TFunctionEnchantWithLevels.OnTreasureChange: TFunctionEnchantWithLevels.TEvent.TAccess;
begin
  Result := FOnTreasureChange.Access;
end;

procedure TLootTable.TFunctionEnchantWithLevels.SetLevels(const Value: TIntBounds1);
var
  OldValue: TIntBounds1;
begin
  if Levels = Value then
    Exit;
  OldValue := Levels;
  FLevels := Value;
  FOnLevelsChange.Execute(TLevelsChangeEventInfo.Create(Self, OldValue));
end;

procedure TLootTable.TFunctionEnchantWithLevels.SetLevelsMax(const Value: Integer);
var
  OldValue: TIntBounds1;
begin
  if LevelsMax = Value then
    Exit;
  OldValue := Levels;
  FLevels.C2 := Value;
  FOnLevelsChange.Execute(TLevelsChangeEventInfo.Create(Self, OldValue));
end;

procedure TLootTable.TFunctionEnchantWithLevels.SetLevelsMin(const Value: Integer);
var
  OldValue: TIntBounds1;
begin
  if LevelsMin = Value then
    Exit;
  OldValue := Levels;
  FLevels.C1 := Value;
  FOnLevelsChange.Execute(TLevelsChangeEventInfo.Create(Self, OldValue));
end;

procedure TLootTable.TFunctionEnchantWithLevels.SetTreasure(const Value: Boolean);
begin
  if Treasure = Value then
    Exit;
  FTreasure := Value;
  FOnTreasureChange.Execute(TEventInfo.Create(Self));
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

procedure TLootTable.TFunctionExplorationMap.Load(AValue: TJSONValue);
var
  StringNode: TJSONString;
  NumberNode: TJSONNumber;
  BoolNode: TJSONBool;
begin
  inherited;

  if AValue.TryGetValue<TJSONString>('destination', StringNode) then
    Destination := StringNode.Value
  else
    Destination := '';  
  
  if AValue.TryGetValue<TJSONString>('decoration', StringNode) then
    Decoration := StringNode.Value
  else
    Decoration := '';  

  if AValue.TryGetValue<TJSONNumber>('zoom', NumberNode) then
    Zoom := NumberNode.AsInt
  else
    Zoom := DefaultZoom;

  if AValue.TryGetValue<TJSONNumber>('search_radius', NumberNode) then
    SearchRadius := NumberNode.AsInt
  else
    SearchRadius := DefaultSearchRadius;
  
  if AValue.TryGetValue<TJSONBool>('skip_existing_chunks', BoolNode) then
    SkipExistingChunks := BoolNode.AsBoolean
  else
    SkipExistingChunks := DefaultSkipExistingChunks;
    
end;

function TLootTable.TFunctionExplorationMap.OnDecorationChange: TDecorationChangeEvent.TAccess;
begin
  Result := FOnDecorationChange.Access;
end;

function TLootTable.TFunctionExplorationMap.OnDestinationChange: TDestinationChangeEvent.TAccess;
begin
  Result := FOnDestinationChange.Access;
end;

function TLootTable.TFunctionExplorationMap.OnSearchRadiusChange: TSearchRadiusChangeEvent.TAccess;
begin
  Result := FOnSearchRadiusChange.Access;
end;

function TLootTable.TFunctionExplorationMap.OnSkipExistingChunksChange: TFunctionExplorationMap.TEvent.TAccess;
begin
  Result := FOnSkipExistingChunksChange.Access;
end;

function TLootTable.TFunctionExplorationMap.OnZoomChange: TZoomChangeEvent.TAccess;
begin
  Result := FOnZoomChange.Access;
end;

procedure TLootTable.TFunctionExplorationMap.SetDecoration(const Value: string);
var
  OldValue: string;
begin
  if Decoration = Value then
    Exit;
  OldValue := Decoration;
  FDecoration := Value;
  FOnDecorationChange.Execute(TDecorationChangeEventInfo.Create(Self, OldValue));
end;

procedure TLootTable.TFunctionExplorationMap.SetDestination(const Value: string);
var
  OldValue: string;
begin
  if Destination = Value then
    Exit;
  OldValue := Destination;
  FDestination := Value;
  FOnDestinationChange.Execute(TDestinationChangeEventInfo.Create(Self, OldValue));
end;

procedure TLootTable.TFunctionExplorationMap.SetSearchRadius(const Value: Integer);
var
  OldValue: Integer;
begin
  if SearchRadius = Value then
    Exit;
  OldValue := SearchRadius;
  FSearchRadius := Value;
  FOnSearchRadiusChange.Execute(TSearchRadiusChangeEventInfo.Create(Self, OldValue));
end;

procedure TLootTable.TFunctionExplorationMap.SetSkipExistingChunks(const Value: Boolean);
begin
  if SkipExistingChunks = Value then
    Exit;
  FSkipExistingChunks := Value;
  FOnSkipExistingChunksChange.Execute(TEventInfo.Create(Self));
end;

procedure TLootTable.TFunctionExplorationMap.SetZoom(const Value: Integer);
var
  OldValue: Integer;
begin
  if Zoom = Value then
    Exit;
  OldValue := Zoom;
  FZoom := Value;
  FOnZoomChange.Execute(TZoomChangeEventInfo.Create(Self, OldValue));
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

procedure TLootTable.TFunctionLootingEnchant.Load(AValue: TJSONValue);
var
  CountNode: TJSONValue;
  LimitNode: TJSONNumber;
begin
  inherited;

  if AValue.TryGetValue<TJSONValue>('count', CountNode) then
    Count := ParseIntBounds(CountNode)
  else
    Count := 0;

  if AValue.TryGetValue<TJSONNumber>('limit', LimitNode) then
    Limit := LimitNode.AsInt
  else
    Limit := 0;
end;

function TLootTable.TFunctionLootingEnchant.OnCountChange: TCountChangeEvent.TAccess;
begin
  Result := FOnCountChange.Access;
end;

function TLootTable.TFunctionLootingEnchant.OnLimitChange: TLimitChangeEvent.TAccess;
begin
  Result := FOnLimitChange.Access;
end;

procedure TLootTable.TFunctionLootingEnchant.SetCount(const Value: TIntBounds1);
var
  OldValue: TIntBounds1;
begin
  if Count = Value then
    Exit;
  OldValue := Count;
  FCount := Value;
  FOnCountChange.Execute(TCountChangeEventInfo.Create(Self, OldValue));
end;

procedure TLootTable.TFunctionLootingEnchant.SetCountMax(const Value: Integer);
var
  OldValue: TIntBounds1;
begin
  if CountMax = Value then
    Exit;
  OldValue := Count;
  FCount.C2 := Value;
  FOnCountChange.Execute(TCountChangeEventInfo.Create(Self, OldValue));
end;

procedure TLootTable.TFunctionLootingEnchant.SetCountMin(const Value: Integer);
var
  OldValue: TIntBounds1;
begin
  if CountMin = Value then
    Exit;
  OldValue := Count;
  FCount.C1 := Value;
  FOnCountChange.Execute(TCountChangeEventInfo.Create(Self, OldValue));
end;

procedure TLootTable.TFunctionLootingEnchant.SetLimit(const Value: Integer);
var
  OldValue: Integer;
begin
  if Limit = Value then
    Exit;
  OldValue := Limit;
  FLimit := Value;
  FOnLimitChange.Execute(TLimitChangeEventInfo.Create(Self, OldValue));
end;

{ TLootTable.TFunctionSetAttribute }

function TLootTable.TFunctionSetAttribute.AddModifier: TModifier;
begin
  Result := FModifiers.Add(TModifier.Create(Self));
  FOnAddModifier.Execute(TModifierEventInfo.Create(Self, Result));
end;

procedure TLootTable.TFunctionSetAttribute.ClearModifiers;
begin
  if Modifiers.Empty then
    Exit;
  FOnClearModifiers.Execute(TEventInfo.Create(Self));
  FModifiers.Clear;
end;

constructor TLootTable.TFunctionSetAttribute.Create(AEntry: TEntryItem);
begin
  inherited;
  FModifiers := TModifiers.Create;
end;

destructor TLootTable.TFunctionSetAttribute.Destroy;
begin
  FModifiers.Free;
  inherited;
end;

function TLootTable.TFunctionSetAttribute.GetModifieres: TModifiers.TReader;
begin
  Result := FModifiers.Reader;
end;

function TLootTable.TFunctionSetAttribute.GetModifierIndex(AModifier: TModifier): Integer;
begin
  Result := Modifiers.Find(AModifier);
end;

class function TLootTable.TFunctionSetAttribute.GetType: TFunction.TType;
begin
  Result := ftSetAttribute;
end;

procedure TLootTable.TFunctionSetAttribute.Load(AValue: TJSONValue);
begin
  inherited;
  raise ENotImplemented.Create('TFunctionSetAttribute.Load');
end;

function TLootTable.TFunctionSetAttribute.OnAddModifier: TModifierEvent.TAccess;
begin
  Result := FOnAddModifier.Access;
end;

function TLootTable.TFunctionSetAttribute.OnClearModifiers: TFunctionSetAttribute.TEvent.TAccess;
begin
  Result := FOnClearModifiers.Access;
end;

function TLootTable.TFunctionSetAttribute.OnMoveModifier: TModifierMoveEvent.TAccess;
begin
  Result := FOnMoveModifier.Access;
end;

function TLootTable.TFunctionSetAttribute.OnRemoveModifier: TModifierEvent.TAccess;
begin
  Result := FOnRemoveModifier.Access;
end;

procedure TLootTable.TFunctionSetAttribute.RemoveModifier(AModifier: TModifier);
begin
  FOnRemoveModifier.Execute(TModifierEventInfo.Create(Self, AModifier));
  FModifiers.Remove(AModifier);
end;

procedure TLootTable.TFunctionSetAttribute.SetModifierIndex(AModifier: TModifier; const Value: Integer);
var
  OldIndex: Integer;
begin
  OldIndex := AModifier.Index;
  FModifiers.SetIndex(OldIndex, Value);
  FOnMoveModifier.Execute(TModifierMoveEventInfo.Create(Self, AModifier, OldIndex));
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

procedure TLootTable.TFunctionSetCount.Load(AValue: TJSONValue);
var
  CountNode: TJSONValue;
begin
  inherited;
  if AValue.TryGetValue<TJSONValue>('count', CountNode) then
    Count := ParseIntBounds(CountNode)
  else
    Count := 1;
end;

function TLootTable.TFunctionSetCount.OnCountChange: TCountChangeEvent.TAccess;
begin
  Result := FOnCountChange.Access;
end;

procedure TLootTable.TFunctionSetCount.SetCount(const Value: TIntBounds1);
var
  OldValue: TIntBounds1;
begin
  if Count = Value then
    Exit;
  OldValue := Count;
  FCount := Value;
  FOnCountChange.Execute(TCountChangeEventInfo.Create(Self, OldValue));
end;

procedure TLootTable.TFunctionSetCount.SetCountMax(const Value: Integer);
var
  OldValue: TIntBounds1;
begin
  if CountMax = Value then
    Exit;
  OldValue := Count;
  FCount.C2 := Value;
  FOnCountChange.Execute(TCountChangeEventInfo.Create(Self, OldValue));
end;

procedure TLootTable.TFunctionSetCount.SetCountMin(const Value: Integer);
var
  OldValue: TIntBounds1;
begin
  if CountMin = Value then
    Exit;
  OldValue := Count;
  FCount.C1 := Value;
  FOnCountChange.Execute(TCountChangeEventInfo.Create(Self, OldValue));
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

procedure TLootTable.TFunctionSetDamage.Load(AValue: TJSONValue);
var
  DamageNode: TJSONValue;
begin
  inherited;
  if AValue.TryGetValue<TJSONValue>('damage', DamageNode) then
    Damage := ParseBounds(DamageNode)
  else
    Damage := 1;
end;

function TLootTable.TFunctionSetDamage.OnDamageChange: TDamageChangeEvent.TAccess;
begin
  Result := FOnDamageChange.Access;
end;

procedure TLootTable.TFunctionSetDamage.SetDamage(const Value: TBounds1);
var
  OldValue: TBounds1;
begin
  if Damage = Value then
    Exit;
  OldValue := Damage;
  FDamage := Value;
  FOnDamageChange.Execute(TDamageChangeEventInfo.Create(Self, OldValue));
end;

procedure TLootTable.TFunctionSetDamage.SetDamageMax(const Value: Single);
var
  OldValue: TBounds1;
begin
  if DamageMax = Value then
    Exit;
  OldValue := Damage;
  FDamage.C2 := Value;
  FOnDamageChange.Execute(TDamageChangeEventInfo.Create(Self, OldValue));
end;

procedure TLootTable.TFunctionSetDamage.SetDamageMin(const Value: Single);
var
  OldValue: TBounds1;
begin
  if DamageMin = Value then
    Exit;
  OldValue := Damage;
  FDamage.C1 := Value;
  FOnDamageChange.Execute(TDamageChangeEventInfo.Create(Self, OldValue));
end;

{ TLootTable.TFunctionSetData }

class function TLootTable.TFunctionSetData.GetType: TFunction.TType;
begin
  Result := ftSetData;
end;

procedure TLootTable.TFunctionSetData.Load(AValue: TJSONValue);
var
  DataNode: TJSONValue;
begin
  inherited;
  if AValue.TryGetValue<TJSONValue>('data', DataNode) then
    Data := ParseIntBounds(DataNode)
  else
    Data := 0;
end;

function TLootTable.TFunctionSetData.OnDataChange: TDataChangeEvent.TAccess;
begin
  Result := FOnDataChange.Access;
end;

procedure TLootTable.TFunctionSetData.SetData(const Value: TIntBounds1);
var
  OldValue: TIntBounds1;
begin
  if Data = Value then
    Exit;
  OldValue := Data;
  FData := Value;
  FOnDataChange.Execute(TDataChangeEventInfo.Create(Self, OldValue));
end;

procedure TLootTable.TFunctionSetData.SetDataMax(const Value: Integer);
var
  OldValue: TIntBounds1;
begin
  if DataMax = Value then
    Exit;
  OldValue := Data;
  FData.C2 := Value;
  FOnDataChange.Execute(TDataChangeEventInfo.Create(Self, OldValue));
end;

procedure TLootTable.TFunctionSetData.SetDataMin(const Value: Integer);
var
  OldValue: TIntBounds1;
begin
  if DataMin = Value then
    Exit;
  OldValue := Data;
  FData.C1 := Value;
  FOnDataChange.Execute(TDataChangeEventInfo.Create(Self, OldValue));
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

procedure TLootTable.TFunctionSetNBT.Load(AValue: TJSONValue);
var
  TagNode: TJSONString;
begin
  inherited;
  if AValue.TryGetValue<TJSONString>('tag', TagNode) then
    TagString := TagNode.Value
  else
    TagString := '{}';
end;

procedure TLootTable.TFunctionSetNBT.SetTagString(const Value: string);
var
  Parser: TNBTParserCompound;
begin
  Parser := TNBTParserCompound.Create(Value, False);
  if Parser.Success then
  begin
    FTag.Free;
    FTag := Parser.ParseResult;
  end;
  Parser.Free;
end;

{ TLootTable.TEntryItem }

function TLootTable.TEntryItem.AddFunction(AFunctionClass: TFunctionClass): TFunction;
begin
  Result := FFunctions.Add(AFunctionClass.Create(Self));
  FOnAddFunction.Execute(TFunctionEventInfo.Create(Self, Result));
end;

function TLootTable.TEntryItem.AddFunction<T>: T;
begin
  Result := T.Create(Self);
  FFunctions.Add(Result);                                         
  FOnAddFunction.Execute(TFunctionEventInfo.Create(Self, Result));
end;

procedure TLootTable.TEntryItem.ClearFunctions;
begin
  if Functions.Empty then
    Exit;
  FOnClearFunctions.Execute(TEventInfo.Create(Self));
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

procedure TLootTable.TEntryItem.Load(AValue: TJSONValue);
var
  ItemNode: TJSONString;
  FunctionsNode: TJSONArray;
  FunctionNode: TJSONValue;
  Func: TFunction;
begin
  inherited;

  if AValue.TryGetValue<TJSONString>('name', ItemNode) then
    Item := ItemNode.Value;

  ClearFunctions;

  if AValue.TryGetValue<TJSONArray>('functions', FunctionsNode) then
  begin
    for FunctionNode in FunctionsNode do
    begin
      Func := TFunction.CreateTyped(Self, FunctionNode);
      FFunctions.Add(Func);
      FOnAddFunction.Execute(TFunctionEventInfo.Create(Self, Func));
    end;
  end;
end;

function TLootTable.TEntryItem.OnItemChange: TItemChangeEvent.TAccess;
begin
  Result := FOnItemChange.Access;
end;

procedure TLootTable.TEntryItem.RemoveFunction(AFunction: TFunction);
begin
  FOnRemoveFunction.Execute(TFunctionEventInfo.Create(Self, AFunction));
  FFunctions.Remove(AFunction);
end;

procedure TLootTable.TEntryItem.SetFunctionIndex(AFunction: TFunction; const Value: Integer);
var
  OldIndex: Integer;
begin
  OldIndex := AFunction.Index;
  FFunctions.SetIndex(OldIndex, Value);
  FOnMoveFunction.Execute(TFunctionMoveEventInfo.Create(Self, AFunction, OldIndex));
end;

procedure TLootTable.TEntryItem.SetItem(const Value: string);
var
  OldValue: string;
begin
  if Item = Value then
    Exit;
  OldValue := Item;
  FItem := Value;
  FOnItemChange.Execute(TItemChangeEventInfo.Create(Self, OldValue));
end;

{ TLootTable.TEntryLootTable }

class function TLootTable.TEntryLootTable.GetType: TEntry.TType;
begin
  Result := etLootTable;
end;

procedure TLootTable.TEntryLootTable.Load(AValue: TJSONValue);
var
  NameNode: TJSONString;
begin
  inherited;
  if AValue.TryGetValue<TJSONString>('name', NameNode) then
    LootTable := NameNode.Value
  else
    LootTable := '';
end;

function TLootTable.TEntryLootTable.OnLootTableChange: TLootTableChangeEvent.TAccess;
begin
  Result := FOnLootTableChange.Access;
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
  FOnAddEntry.Execute(TEntryEventInfo.Create(Self, Result));
end;

function TLootTable.TPool.AddEntry<T>: T;
begin
  Result := T.Create(Self);
  FEntries.Add(Result);
  FOnAddEntry.Execute(TEntryEventInfo.Create(Self, Result));
end;

procedure TLootTable.TPool.ClearEntries;
begin
  if Entries.Empty then
    Exit;
  FOnClearEntries.Execute(TEventInfo.Create(Self));
  FEntries.Clear;
end;

constructor TLootTable.TPool.Create(ALootTable: TLootTable; AValue: TJSONValue);
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

function TLootTable.TPool.GetOnBonusRollsChange: TBonusRollsChangeEvent.TAccess;
begin
  Result := FOnBonusRollsChange.Access;
end;

function TLootTable.TPool.GetOnRollsChange: TRollsChangeEvent.TAccess;
begin
  Result := FOnRollsChange.Access;
end;

procedure TLootTable.TPool.Load(AValue: TJSONValue);
var
  ArrayNode: TJSONArray;
  Node, RollsNode: TJSONValue;
  Entry: TEntry;
begin
  if AValue.TryGetValue('rolls', RollsNode) then
    Rolls := ParseIntBounds(RollsNode);

  if AValue.TryGetValue('bonus_rolls', RollsNode) then
    BonusRolls := ParseBounds(RollsNode);

  if AValue.TryGetValue<TJSONArray>('entries', ArrayNode) then
  begin
    for Node in ArrayNode do
    begin
      Entry := TEntry.CreateTyped(Self, Node);
      FEntries.Add(Entry);
      FOnAddEntry.Execute(TEntryEventInfo.Create(Self, Entry));
    end;
  end;
end;

function TLootTable.TPool.OnAddEntry: TEntryEvent.TAccess;
begin
  Result := FOnAddEntry.Access;
end;

function TLootTable.TPool.OnClearEntries: TPool.TEvent.TAccess;
begin
  Result := FOnClearEntries.Access;
end;

function TLootTable.TPool.OnMoveEntry: TEntryMoveEvent.TAccess;
begin
  Result := FOnMoveEntry.Access;
end;

function TLootTable.TPool.OnRemoveEntry: TEntryEvent.TAccess;
begin
  Result := FOnRemoveEntry.Access;
end;

procedure TLootTable.TPool.RemoveEntry(AEntry: TEntry);
begin
  FOnRemoveEntry.Execute(TEntryEventInfo.Create(Self, AEntry));
  FEntries.Remove(AEntry);
end;

procedure TLootTable.TPool.SetBonusRolls(const Value: TBounds1);
var
  OldValue: TBounds1;
begin
  if BonusRolls = Value then
    Exit;
  OldValue := BonusRolls;
  FBonusRolls := Value;
  FOnBonusRollsChange.Execute(TBonusRollsChangeEventInfo.Create(Self, OldValue));
end;

procedure TLootTable.TPool.SetBonusRollsMax(const Value: Single);
var
  OldValue: TBounds1;
begin
  if BonusRollsMax = Value then
    Exit;
  OldValue := BonusRolls;
  FBonusRolls.C2 := Value;
  FOnBonusRollsChange.Execute(TBonusRollsChangeEventInfo.Create(Self, OldValue));
end;

procedure TLootTable.TPool.SetBonusRollsMin(const Value: Single);
var
  OldValue: TBounds1;
begin
  if BonusRollsMin = Value then
    Exit;
  OldValue := BonusRolls;
  FBonusRolls.C1 := Value;
  FOnBonusRollsChange.Execute(TBonusRollsChangeEventInfo.Create(Self, OldValue));
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
var
  OldValue: TIntBounds1;
begin
  if Rolls = Value then
    Exit;
  OldValue := Rolls;
  FRolls := Value;
  FOnRollsChange.Execute(TRollsChangeEventInfo.Create(Self, OldValue));
end;

procedure TLootTable.TPool.SetRollsMax(const Value: Integer);
var
  OldValue: TIntBounds1;
begin
  if RollsMax = Value then
    Exit;
  OldValue := Rolls;
  FRolls.C2 := Value;
  FOnRollsChange.Execute(TRollsChangeEventInfo.Create(Self, OldValue));
end;

procedure TLootTable.TPool.SetRollsMin(const Value: Integer);
var
  OldValue: TIntBounds1;
begin
  if RollsMin = Value then
    Exit;
  OldValue := Rolls;
  FRolls.C1 := Value;
  FOnRollsChange.Execute(TRollsChangeEventInfo.Create(Self, OldValue));
end;

{ TLootTable.TCondition }

constructor TLootTable.TCondition.Create(AConditioned: TConditioned);
begin
  FConditioned := AConditioned;
end;

constructor TLootTable.TCondition.Create(AConditioned: TConditioned; AValue: TJSONValue);
begin
  Create(AConditioned);
  Load(AValue);
end;

class function TLootTable.TCondition.CreateTyped(AConditioned: TConditioned; AValue: TJSONValue): TCondition;
var
  T: TType;
  NameNode: TJSONString;
begin
  if not AValue.TryGetValue<TJSONString>('condition', NameNode) then
    raise ELootTable.Create('condition type missing.');

  for T := Low(TType) to High(TType) do
    if NameNode.Value = ConditionClasses[T].GetName then
      Exit(ConditionClasses[T].Create(AConditioned, AValue));

  Result := nil;
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

procedure TLootTable.TCondition.Load(AValue: TJSONValue);
begin
  // nothing by default
end;

procedure TLootTable.TCondition.SetIndex(const Value: Integer);
begin
  Conditioned.ConditionIndex[Self] := Value;
end;

{ TLootTable.TConditionEntity }

function TLootTable.TConditionEntity.OnTargetChange: TTargetChangeEvent.TAccess;
begin
  Result := FOnTargetChange.Access;
end;

procedure TLootTable.TConditionEntity.Load(AValue: TJSONValue);
var
  EntityNode: TJSONString;
  T: TTarget;
begin
  inherited;

  if not AValue.TryGetValue<TJSONString>('entity', EntityNode) then
    raise ELootTable.Create('Entity target type expected');

  for T := Low(TTarget) to High(TTarget) do
  begin
    if EntityNode.Value = TargetNames[T] then
    begin
      Target := T;
      Exit;
    end;
  end;

  raise ELootTable.Create('Invalid target entity specified.');
end;

procedure TLootTable.TConditionEntity.SetTarget(const Value: TTarget);
var
  OldTarget: TTarget;
begin
  if Target = Value then
    Exit;
  OldTarget := Target;
  FTarget := Value;
  FOnTargetChange.Execute(TTargetChangeEventInfo.Create(Self, OldTarget));
end;

{ TLootTable.TPoolEventInfo }

constructor TLootTable.TPoolEventInfo.Create(ASender: TLootTable; APool: TPool);
begin
  inherited Create(ASender);
  FPool := APool;
end;

{ TLootTable.TEntry }

constructor TLootTable.TEntry.Create(APool: TPool);
begin
  inherited Create;
  FPool := APool;
end;

constructor TLootTable.TEntry.Create(APool: TPool; AValue: TJSONValue);
begin
  Create(APool);
  Load(AValue);
end;

class function TLootTable.TEntry.CreateTyped(APool: TPool; AValue: TJSONValue): TEntry;
var
  NameNode: TJSONString;
  T: TType;
begin
  if not AValue.TryGetValue<TJSONString>('type', NameNode) then
    raise ELootTable.Create('type missing.');

  for T := Low(TType) to High(TType) do
    if NameNode.Value = EntryClasses[T].GetName then
      Exit(EntryClasses[T].Create(APool, AValue));

  Result := nil;
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

function TLootTable.TEntry.OnQualityChange: TQualityChangeEvent.TAccess;
begin
  Result := FOnQualityChange.Access;
end;

function TLootTable.TEntry.OnWeightChange: TWeightChangeEvent.TAccess;
begin
  Result := FOnWeightChange.Access;
end;

procedure TLootTable.TEntry.Load(AValue: TJSONValue);
var
  NumberNode: TJSONNumber;
begin
  inherited;
  if AValue.TryGetValue<TJSONNumber>('weight', NumberNode) then
    Weight := NumberNode.AsInt;
  if AValue.TryGetValue<TJSONNumber>('quality', NumberNode) then
    Quality := NumberNode.AsInt;
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
  FOnClearConditions.Execute(TEventInfo.Create(Self));
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

function TLootTable.TConditioned.OnAddCondition: TConditionEvent.TAccess;
begin
  Result := FOnAddCondition.Access;
end;

function TLootTable.TConditioned.OnClearConditions: TConditioned.TEvent.TAccess;
begin
  Result := FOnClearConditions.Access;
end;

function TLootTable.TConditioned.OnMoveCondition: TConditionMoveEvent.TAccess;
begin
  Result := FOnMoveCondition.Access;
end;

function TLootTable.TConditioned.OnRemoveCondition: TConditionEvent.TAccess;
begin
  Result := FOnRemoveCondition.Access;
end;

procedure TLootTable.TConditioned.Load(AValue: TJSONValue);
var
  ConditionsNode: TJSONArray;
  ConditionNode: TJSONValue;
  Condition: TCondition;
begin
  ClearConditions;

  if AValue.TryGetValue<TJSONArray>('conditions', ConditionsNode) then
  begin
    for ConditionNode in ConditionsNode do
    begin
      Condition := TCondition.CreateTyped(Self, ConditionNode);
      FConditions.Add(Condition);
      FOnAddCondition.Execute(TConditionEventInfo.Create(Self, Condition));
    end;
  end;
end;

procedure TLootTable.TConditioned.RemoveCondition(ACondition: TCondition);
begin
  FOnRemoveCondition.Execute(TConditionEventInfo.Create(Self, ACondition));
  FConditions.Remove(ACondition);
end;

procedure TLootTable.TConditioned.SetConditionIndex(ACondition: TCondition; const Value: Integer);
var
  OldIndex: Integer;
begin
  OldIndex := ACondition.Index;
  FConditions.SetIndex(OldIndex, Value);
  FOnMoveCondition.Execute(TConditionMoveEventInfo.Create(Self, ACondition, OldIndex));
end;

{ TLootTable.TConditionEntity.TTargetChangeEventInfo }

constructor TLootTable.TConditionEntity.TTargetChangeEventInfo.Create(ASender: TConditionEntity; AOldTarget: TTarget);
begin
  inherited Create(ASender);
  FOldTarget := AOldTarget;
end;

{ TLootTable.TConditionEntityProperties.TPropertiesChangedEventInfo }

constructor TLootTable.TConditionEntityProperties.TChangedEventInfo.Create(ASender: TConditionEntity;
  AOnFire: TOpt<Boolean>);
begin
  inherited Create(ASender);
  FOnFire := AOnFire.Copy;
end;

destructor TLootTable.TConditionEntityProperties.TChangedEventInfo.Destroy;
begin
  FOnFire.Free;
  inherited;
end;

{ TLootTable.TConditioned.TConditionEventInfo }

constructor TLootTable.TConditioned.TConditionEventInfo.Create(ASender: TConditioned; ACondition: TCondition);
begin
  inherited Create(ASender);
  FCondition := ACondition;
end;

{ TLootTable.TEntry.TWeightChangeEventInfo }

constructor TLootTable.TEntry.TWeightChangeEventInfo.Create(ASender: TEntry; AOldWeight: Integer);
begin
  inherited Create(ASender);
  FOldWeight := AOldWeight;
end;

{ TLootTable.TEntry.TQualityChangeEventInfo }

constructor TLootTable.TEntry.TQualityChangeEventInfo.Create(ASender: TEntry; AOldQuality: Integer);
begin
  inherited Create(ASender);
  FOldQuality := AOldQuality;
end;

{ TLootTable.TPool.TRollsChangeEventInfo }

constructor TLootTable.TPool.TRollsChangeEventInfo.Create(ASender: TPool; AOldRolls: TIntBounds1);
begin
  inherited Create(ASender);
  FOldRolls := AOldRolls;
end;

{ TLootTable.TPool.TBonusRollsChangeEventInfo }

constructor TLootTable.TPool.TBonusRollsChangeEventInfo.Create(ASender: TPool; AOldBonusRolls: TBounds1);
begin
  inherited Create(ASender);
  FOldBonusRolls := AOldBonusRolls;
end;

{ TLootTable.TPool.TEntryEventInfo }

constructor TLootTable.TPool.TEntryEventInfo.Create(ASender: TPool; AEntry: TEntry);
begin
  inherited Create(ASender);
  FEntry := AEntry;
end;

{ TLootTable.TConditionRandomChance.TChanceChangeEventInfo }

constructor TLootTable.TConditionRandomChance.TChanceChangeEventInfo.Create(ASender: TConditionRandomChance;
  AChance: Single);
begin
  inherited Create(ASender);
  FChance := AChance;
end;

{ TLootTable.TConditionRandomChanceWithLooting.TLootingMultiplierChangeEventInfo }

constructor TLootTable.TConditionRandomChanceWithLooting.TLootingMultiplierChangeEventInfo.Create(
  ASender: TConditionRandomChanceWithLooting; ALootingMultiplier: Single);
begin
  inherited Create(ASender);
  FLootingMultiplier := ALootingMultiplier;
end;

{ TLootTable.TConditioned.TConditionMoveEventInfo }

constructor TLootTable.TConditioned.TConditionMoveEventInfo.Create(ASender: TConditioned; ACondition: TCondition;
  AOldIndex: Integer);
begin
  inherited Create(ASender, ACondition);
  FOldIndex := AOldIndex;
end;

{ TLootTable.TEntryItem.TItemChangeEventInfo }

constructor TLootTable.TEntryItem.TItemChangeEventInfo.Create(ASender: TEntryItem; AOldItem: string);
begin
  inherited Create(ASender);
  FOldItem := AOldItem;
end;

{ TLootTable.TFunction }

constructor TLootTable.TFunction.Create(AEntry: TEntryItem);
begin
  inherited Create;
  FEntry := AEntry;
end;

constructor TLootTable.TFunction.Create(AEntry: TEntryItem; AValue: TJSONValue);
begin
  Create(AEntry);
  Load(AValue);
end;

class function TLootTable.TFunction.CreateTyped(AEntry: TEntryItem; AValue: TJSONValue): TFunction;
var
  TypeNode: TJSONString;
  T: TType;
begin
  if not AValue.TryGetValue<TJSONString>('function', TypeNode) then
    raise ELootTable.Create('function type missing.');

  for T := Low(TType) to High(TType) do
    if TypeNode.Value = FunctionClasses[T].GetName then
      Exit(FunctionClasses[T].Create(AEntry, AValue));

  Result := nil;
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

procedure TLootTable.TFunction.SetIndex(const Value: Integer);
begin
  Entry.FunctionIndex[Self] := Value;
end;

{ TLootTable.TEntryItem.TFunctionEventInfo }

constructor TLootTable.TEntryItem.TFunctionEventInfo.Create(ASender: TEntryItem; AFunc: TFunction);
begin
  inherited Create(ASender);
  FFunc := AFunc;
end;

{ TLootTable.TEntryItem.TFunctionMoveEventInfo }

constructor TLootTable.TEntryItem.TFunctionMoveEventInfo.Create(ASender: TEntryItem; AFunc: TFunction;
  AOldIndex: Integer);
begin
  inherited Create(ASender, AFunc);
  FOldIndex := AOldIndex;
end;

{ TLootTable.TPool.TEntryMoveEventInfo }

constructor TLootTable.TPool.TEntryMoveEventInfo.Create(ASender: TPool; AEntry: TEntry; AOldIndex: Integer);
begin
  inherited Create(ASender, AEntry);
  FOldIndex := AOldIndex;
end;

{ TLootTable.TPoolMoveEventInfo }

constructor TLootTable.TPoolMoveEventInfo.Create(ASender: TLootTable; APool: TPool; AOldIndex: Integer);
begin
  inherited Create(ASender, APool);
  FOldIndex := AOldIndex;
end;

{ TLootTable.TFunctionEnchantRandomly.TEnchantmentsChangeEventInfo }

constructor TLootTable.TFunctionEnchantRandomly.TEnchantmentsChangeEventInfo.Create(ASender: TFunctionEnchantRandomly;
  AChangedEnchantments: TEnchantments);
begin
  inherited Create(ASender);
  FChangedEnchantments := AChangedEnchantments;
end;

{ TLootTable.TFunctionEnchantWithLevels.TLevelsChangeEventInfo }

constructor TLootTable.TFunctionEnchantWithLevels.TLevelsChangeEventInfo.Create(ASender: TFunctionEnchantWithLevels;
  AOldLevels: TIntBounds1);
begin
  inherited Create(ASender);
  FOldLevels := AOldLevels;
end;

{ TLootTable.TFunctionExplorationMap.TDestinationChangeEventInfo }

constructor TLootTable.TFunctionExplorationMap.TDestinationChangeEventInfo.Create(ASender: TFunctionExplorationMap;
  AOldDestination: string);
begin
  inherited Create(ASender);
  FOldDestination := AOldDestination;
end;

{ TLootTable.TFunctionExplorationMap.TDecorationChangeEventInfo }

constructor TLootTable.TFunctionExplorationMap.TDecorationChangeEventInfo.Create(ASender: TFunctionExplorationMap;
  AOldDecoration: string);
begin
  inherited Create(ASender);
  FOldDecoration := AOldDecoration;
end;

{ TLootTable.TFunctionExplorationMap.TZoomChangeEventInfo }

constructor TLootTable.TFunctionExplorationMap.TZoomChangeEventInfo.Create(ASender: TFunctionExplorationMap;
  AOldZoom: Integer);
begin
  inherited Create(ASender);
  FOldZoom := AOldZoom;
end;

{ TLootTable.TFunctionExplorationMap.TSearchRadiusChangeEventInfo }

constructor TLootTable.TFunctionExplorationMap.TSearchRadiusChangeEventInfo.Create(ASender: TFunctionExplorationMap;
  AOldSearchRadius: Integer);
begin
  inherited Create(ASender);
  FOldSearchRadius := AOldSearchRadius;
end;

{ TLootTable.TFunctionLootingEnchant.TCountChangeEventInfo }

constructor TLootTable.TFunctionLootingEnchant.TCountChangeEventInfo.Create(ASender: TFunctionLootingEnchant;
  AOldCount: TIntBounds1);
begin
  inherited Create(ASender);
  FOldCount := AOldCount;
end;

{ TLootTable.TFunctionLootingEnchant.TLimitChangeEventInfo }

constructor TLootTable.TFunctionLootingEnchant.TLimitChangeEventInfo.Create(ASender: TFunctionLootingEnchant;
  AOldLimit: Integer);
begin
  inherited Create(ASender);
  FOldLimit := AOldLimit;
end;

{ TLootTable.TModifier.TAttributeChangeEventInfo }

constructor TLootTable.TModifier.TAttributeChangeEventInfo.Create(ASender: TModifier; AOldAttribute: TAttribute);
begin
  inherited Create(ASender);
  FOldAttribute := AOldAttribute;
end;

{ TLootTable.TModifier.TNameChangeEventInfo }

constructor TLootTable.TModifier.TNameChangeEventInfo.Create(ASender: TModifier; AOldName: string);
begin
  inherited Create(ASender);
  FOldName := AOldName;
end;

{ TLootTable.TModifier.TOperationChangeEventInfo }

constructor TLootTable.TModifier.TOperationChangeEventInfo.Create(ASender: TModifier;
  AOldOperation: TAttributeOperation);
begin
  inherited Create(ASender);
  FOldOperation := AOldOperation;
end;

{ TLootTable.TModifier.TAmountChangeEventInfo }

constructor TLootTable.TModifier.TAmountChangeEventInfo.Create(ASender: TModifier; AOldAmount: TBounds1);
begin
  inherited Create(ASender);
  FOldAmount := AOldAmount;
end;

{ TLootTable.TModifier.TSlotChangeEventInfo }

constructor TLootTable.TModifier.TSlotsChangeEventInfo.Create(ASender: TModifier; AChangedSlots: TAttributeSlots);
begin
  inherited Create(ASender);
  FChangedSlots := AChangedSlots;
end;

{ TLootTable.TModifier }

constructor TLootTable.TModifier.Create(AFunction: TFunctionSetAttribute);
begin
  FFunction := AFunction;
end;

function TLootTable.TModifier.GetIndex: Integer;
begin
  Result := Func.ModifierIndex[Self];
end;

function TLootTable.TModifier.OnAmountChange: TAmountChangeEvent.TAccess;
begin
  Result := FOnAmountChange.Access;
end;

function TLootTable.TModifier.OnAttributeChange: TAttributeChangeEvent.TAccess;
begin
  Result := FOnAttributeChange.Access;
end;

function TLootTable.TModifier.OnNameChange: TNameChangeEvent.TAccess;
begin
  Result := FOnNameChange.Access;
end;

function TLootTable.TModifier.OnOperationChange: TOperationChangeEvent.TAccess;
begin
  Result := FOnOperationChange.Access;
end;

function TLootTable.TModifier.OnSlotsChange: TSlotsChangeEvent.TAccess;
begin
  Result := FOnSlotsChange.Access;
end;

procedure TLootTable.TModifier.SetAmount(const Value: TBounds1);
var
  OldValue: TBounds1;
begin
  if Amount = Value then
    Exit;
  OldValue := Amount;
  FAmount := Value;
  FOnAmountChange.Execute(TAmountChangeEventInfo.Create(Self, OldValue));
end;

procedure TLootTable.TModifier.SetAmountMax(const Value: Single);
var
  OldValue: TBounds1;
begin
  if AmountMax = Value then
    Exit;
  OldValue := Amount;
  FAmount.C2 := Value;
  FOnAmountChange.Execute(TAmountChangeEventInfo.Create(Self, OldValue));
end;

procedure TLootTable.TModifier.SetAmountMin(const Value: Single);
var
  OldValue: TBounds1;
begin
  if AmountMin = Value then
    Exit;
  OldValue := Amount;
  FAmount.C1 := Value;
  FOnAmountChange.Execute(TAmountChangeEventInfo.Create(Self, OldValue));
end;

procedure TLootTable.TModifier.SetAttribute(const Value: TAttribute);
var
  OldValue: TAttribute;
begin
  if Attribute = Value then
    Exit;
  OldValue := Attribute;
  FAttribute := Value;
  FOnAttributeChange.Execute(TAttributeChangeEventInfo.Create(Self, OldValue));
end;

procedure TLootTable.TModifier.SetIndex(const Value: Integer);
begin
  Func.ModifierIndex[Self] := Value;
end;

procedure TLootTable.TModifier.SetName(const Value: string);
var
  OldValue: string;
begin
  if Name = Value then
    Exit;
  OldValue := Name;
  FName := Value;
  FOnNameChange.Execute(TNameChangeEventInfo.Create(Self, OldValue));
end;

procedure TLootTable.TModifier.SetOperation(const Value: TAttributeOperation);
var
  OldValue: TAttributeOperation;
begin
  if Operation = Value then
    Exit;
  OldValue := Operation;
  FOperation := Value;
  FOnOperationChange.Execute(TOperationChangeEventInfo.Create(Self, OldValue));
end;

procedure TLootTable.TModifier.SetSlots(const Value: TAttributeSlots);
var
  OldValue: TAttributeSlots;
begin
  if Slots = Value then
    Exit;
  OldValue := Value - (Value * Slots);
  FSlots := Value;
  FOnSlotsChange.Execute(TSlotsChangeEventInfo.Create(Self, OldValue));
end;

{ TLootTable.TFunctionSetAttribute.TModifierEventInfo }

constructor TLootTable.TFunctionSetAttribute.TModifierEventInfo.Create(ASender: TFunctionSetAttribute;
  AModifier: TModifier);
begin
  inherited Create(ASender);
  FModifier := AModifier;
end;

{ TLootTable.TFunctionSetAttribute.TModifierMoveEventInfo }

constructor TLootTable.TFunctionSetAttribute.TModifierMoveEventInfo.Create(ASender: TFunctionSetAttribute;
  AModifier: TModifier; AOldIndex: Integer);
begin
  inherited Create(ASender, AModifier);
  FOldIndex := AOldIndex;
end;

{ TLootTable.TFunctionSetCount.TCountChangeEventInfo }

constructor TLootTable.TFunctionSetCount.TCountChangeEventInfo.Create(ASender: TFunctionSetCount;
  AOldCount: TIntBounds1);
begin
  inherited Create(ASender);
  FOldCount := AOldCount;
end;

{ TLootTable.TFunctionSetDamage.TDamageChangeEventInfo }

constructor TLootTable.TFunctionSetDamage.TDamageChangeEventInfo.Create(ASender: TFunctionSetDamage; AOldDamage: TBounds1);
begin
  inherited Create(ASender);
  FOldDamage := AOldDamage;
end;

{ TLootTable.TFunctionSetData.TDataChangeEventInfo }

constructor TLootTable.TFunctionSetData.TDataChangeEventInfo.Create(ASender: TFunctionSetData; AOldData: TIntBounds1);
begin
  inherited Create(ASender);
  FOldData := AOldData;
end;

{ TLootTable.TEntryLootTable.TLootTableChangeEventInfo }

constructor TLootTable.TEntryLootTable.TLootTableChangeEventInfo.Create(ASender: TEntryLootTable;
  AOldLootTable: string);
begin
  inherited Create(ASender);
  FOldLootTable := AOldLootTable;
end;

end.
