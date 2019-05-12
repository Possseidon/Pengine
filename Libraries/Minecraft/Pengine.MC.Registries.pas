unit Pengine.MC.Registries;

interface

uses
  System.SysUtils,

  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.CollectionInterfaces,
  Pengine.JSON,

  Pengine.MC.Namespace;

type

  EMCRegistry = class(Exception);

  TMCRegistry = class
  public type

    TEntries = TArray<TNSPath>;
    TEntryMap = TMap<TNSPath, Integer, TNSPathHasher>;

  private
    FName: TNSPath;
    FEntries: TEntries;
    FEntryMap: TEntryMap;

    function GetEntries: TEntries.TReader;

  public
    constructor Create(AJObject: TJObject);
    destructor Destroy; override;

    property Name: TNSPath read FName;

    property Entries: TEntries.TReader read GetEntries;

    function Has(ANSPath: TNSPath): Boolean;
    function Get(ANSPath: TNSPath; out AIndex: Integer): Boolean;

    function GetEnumerator: IIterator<TNSPath>;

  end;

  TMCRegistries = class
  public type

    TType = (
      mctSoundEvent,
      mctFluid,
      mctMobEffect,
      mctBlock,
      mctEnchantment,
      mctEntityType,
      mctItem,
      mctPotion,
      mctBiome,
      mctParticleType,
      mctBlockEntityType,
      mctDimensionType,
      mctCustomStat,
      mctStructureFeature,
      mctRuleTest,
      mctMenu,
      mctRecipeType,
      mctRecipeSerializer,
      mctVillagerType,
      mctVillagerProfession,
      mctPointOfInterestType
      );

  public const

    TypeNames: array [TType] of string = (
      'sound_event',
      'fluid',
      'mob_effect',
      'block',
      'enchantment',
      'entity_type',
      'item',
      'potion',
      'biome',
      'particle_type',
      'block_entity_type',
      'dimension_type',
      'custom_stat',
      'structure_feature',
      'rule_test',
      'menu',
      'recipe_type',
      'recipe_serializer',
      'villager_type',
      'villager_profession',
      'point_of_interest_type'
      );

  private
    FLoaded: Boolean;
    FRegistries: array [TType] of TMCRegistry;

    function GetRegistry(AType: TType): TMCRegistry;

  public
    destructor Destroy; override;

    procedure Load(AJObject: TJObject); overload;
    procedure Load(APath: string); overload;

    property Registries[AType: TType]: TMCRegistry read GetRegistry;

    property SoundEvent: TMCRegistry index mctSoundEvent read GetRegistry;
    property Fluid: TMCRegistry index mctFluid read GetRegistry;
    property MobEffect: TMCRegistry index mctMobEffect read GetRegistry;
    property Block: TMCRegistry index mctBlock read GetRegistry;
    property Enchantment: TMCRegistry index mctEnchantment read GetRegistry;
    property EntityType: TMCRegistry index mctEntityType read GetRegistry;
    property Item: TMCRegistry index mctItem read GetRegistry;
    property Potion: TMCRegistry index mctPotion read GetRegistry;
    property Biome: TMCRegistry index mctBiome read GetRegistry;
    property ParticleType: TMCRegistry index mctParticleType read GetRegistry;
    property BlockEntityType: TMCRegistry index mctBlockEntityType read GetRegistry;
    property DimensionType: TMCRegistry index mctDimensionType read GetRegistry;
    property CustomStat: TMCRegistry index mctCustomStat read GetRegistry;
    property StructureFeature: TMCRegistry index mctStructureFeature read GetRegistry;
    property RuleTest: TMCRegistry index mctRuleTest read GetRegistry;
    property Menu: TMCRegistry index mctMenu read GetRegistry;
    property RecipeType: TMCRegistry index mctRecipeType read GetRegistry;
    property RecipeSerializer: TMCRegistry index mctRecipeSerializer read GetRegistry;
    property VillagerType: TMCRegistry index mctVillagerType read GetRegistry;
    property VillagerProfession: TMCRegistry index mctVillagerProfession read GetRegistry;
    property PointOfInterestType: TMCRegistry index mctPointOfInterestType read GetRegistry;

  end;

var
  MCRegistries: TMCRegistries;

implementation

{ TMCRegistry }

constructor TMCRegistry.Create(AJObject: TJObject);
var
  JEntryPair: TJPair;
begin
  FEntries := TEntries.Create;
  FEntryMap := TEntryMap.Create;
  for JEntryPair in AJObject['entries'].AsObject do
  begin
    FEntries.Add(JEntryPair.Key);
    FEntryMap[JEntryPair.Key] := JEntryPair.Value.Index;
  end;
end;

destructor TMCRegistry.Destroy;
begin
  FEntries.Free;
  FEntryMap.Free;
  inherited;
end;

function TMCRegistry.Get(ANSPath: TNSPath; out AIndex: Integer): Boolean;
begin
  Result := FEntryMap.Get(ANSPath, AIndex);
end;

function TMCRegistry.GetEntries: TEntries.TReader;
begin
  Result := FEntries.Reader;
end;

function TMCRegistry.GetEnumerator: IIterator<TNSPath>;
begin
  Result := FEntries.GetEnumerator;
end;

function TMCRegistry.Has(ANSPath: TNSPath): Boolean;
begin
  Result := FEntryMap.KeyExists(ANSPath);
end;

{ TMCRegistries }

destructor TMCRegistries.Destroy;
var
  RegistryType: TType;
begin
  for RegistryType := Low(TType) to High(TType) do
    FRegistries[RegistryType].Free;
  inherited;
end;

function TMCRegistries.GetRegistry(AType: TType): TMCRegistry;
begin
  if not FLoaded then
    raise EMCRegistry.Create('MCRegistry not loaded.');
  Result := FRegistries[AType];
end;

procedure TMCRegistries.Load(AJObject: TJObject);
var
  RegistryType: TType;
begin
  for RegistryType := Low(TType) to High(TType) do
    FRegistries[RegistryType] := TMCRegistry.Create(AJObject[NSPath(TypeNames[RegistryType])]);
  FLoaded := True;
end;

procedure TMCRegistries.Load(APath: string);
var
  JObject: TJObject;
begin
  JObject := TJObject.CreateFromFile(APath);
  try
    Load(JObject);

  finally
    JObject.Free;

  end;
end;

initialization

MCRegistries := TMCRegistries.Create;

finalization

MCRegistries.Free;

end.
