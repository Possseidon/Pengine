unit Pengine.MC.Entity;

interface

uses
  System.SysUtils,
  
  Pengine.HashCollections,
  Pengine.Hasher;

type

  TEntity = (
    // Player
    etPlayer,

    // Drop
    etItem,
    etExperienceOrb,

    // Immobile
    etAreaEffectCloud,
    etLeashKnot,
    etPainting,
    etItemFrame,
    etArmorStand,
    etEvokerFangs,
    etEndCrystal,

    // Projectiles
    etEgg,
    etArrow,
    etSnowball,
    etFireball,
    etSmallFireball,
    etEnderPearl,
    etEyeOfEnder,
    etPotion,
    etExperienceBottle,
    etWitherSkull,
    etFireworksRocket,
    etSpectralArrow,
    etShulkerBullet,
    etDragonFireball,
    etLlamaSpit,

    // Blocks
    etTNT,
    etFallingBlock,

    // Vehicles
    etBoat,
    etMinecart,
    etChestMinecart,
    etFurnaceMinecart,
    etTNTMinecart,
    etHopperMinecart,
    etSpawnerMinecart,
    etCommandblockMinecart,

    // Hostile
    etElderGuardian,
    etWhiterSkeleton,
    etStray,
    etHusk,
    etZombieVillager,
    etEvoker,
    etVex,
    etVindicator,
    etIllusioner,
    etCreeper,
    etSkeleton,
    etSpider,
    etGiant,
    etZombie,
    etSlime,
    etGhast,
    etZombiePigman,
    etEnderman,
    etCaveSpider,
    etSilverfish,
    etBlaze,
    etMagmaCube,
    etEnderDragon,
    etWither,
    etWitch,
    etEndermite,
    etGuardian,
    etShulker,

    // Passive
    etSkeletonHorse,
    etZombieHorse,
    etDonkey,
    etMule,
    etBat,
    etPig,
    etSheep,
    etCow,
    etChicken,
    etSquid,
    etWolf,
    etMooshroom,
    etSnowGolem,
    etOcelot,
    etIronGolem,
    etHorse,
    etRabbit,
    etPolarBear,
    etLlama,
    etParrot,
    etVillager,

    // Other
    etLightningBolt

    );

  TEntityPlayer = etPlayer .. etPlayer;
  TEntityDrop = etItem .. etExperienceOrb;
  TEntityImmobile = etAreaEffectCloud .. etEndCrystal;
  TEntityProjectile = etEgg .. etLlamaSpit;
  TEntityBlock = etTNT .. etFallingBlock;
  TEntityVechicle = etBoat .. etCommandblockMinecart;
  TEntityHostile = etElderGuardian .. etShulker;
  TEntityPassive = etSkeletonHorse .. etVillager;
  TEntityOther = etLightningBolt .. etLightningBolt;

const

  EntityNames: array [TEntity] of string = (
    // Player
    'player',

    // Drop
    'item',
    'experience_orb',

    // Immobile
    'area_effect_cloud',
    'leash_knot',
    'painting',
    'item_frame',
    'armor_stand',
    'evoker_fangs',
    'ender_crystal',

    // Projectiles
    'egg',
    'arrow',
    'snowball',
    'fireball',
    'small_fireball',
    'ender_pearl',
    'eye_of_ender',
    'potion',
    'experience_bottle',
    'wither_skull',
    'fireworks_rocket',
    'spectral_arrow',
    'shulker_bullet',
    'dragon_fireball',
    'llama_spit',

    // Blocks
    'tnt',
    'falling_block',

    // Vehicles
    'boat',
    'minecart',
    'chest_minecart',
    'furnace_minecart',
    'tnt_minecart',
    'hopper_minecart',
    'spawner_minecart',
    'commandblock_minecart',

    // Hostile
    'elder_guardian',
    'whiter_skeleton',
    'stray',
    'husk',
    'zombie_villager',
    'evoker',
    'vex',
    'vindicator',
    'illusioner',
    'creeper',
    'skeleton',
    'spider',
    'giant',
    'zombie',
    'slime',
    'ghast',
    'zombie_pigman',
    'enderman',
    'cave_spider',
    'silverfish',
    'blaze',
    'magma_cube',
    'ender_dragon',
    'wither',
    'witch',
    'endermite',
    'guardian',
    'shulker',

    // Passive
    'skeleton_horse',
    'zombie_horse',
    'donkey',
    'mule',
    'bat',
    'pig',
    'sheep',
    'cow',
    'chicken',
    'squid',
    'wolf',
    'mooshroom',
    'snow_golem',
    'ocelot',
    'iron_golem',
    'horse',
    'rabbit',
    'polar_bear',
    'llama',
    'parrot',
    'villager',

    // Other
    'lightning_bolt'
    );

  EntityDisplayNames: array [TEntity] of string = (
    // Player
    'Player',

    // Drop
    'Item',
    'Experience Orb',

    // Immobile
    'Area Effect Cloud',
    'Leash Knot',
    'Painting',
    'Item Frame',
    'Armor Stand',
    'Evoker Fangs',
    'End Crystal',

    // Projectiles
    'Egg',
    'Arrow',
    'Snowball',
    'Ghast Fireball',
    'Blaze Fireball',
    'Ender Pearl',
    'Eye of Ender',
    'Potion',
    'Bottle o'' Enchanting',
    'Wither Skull',
    'Firework Rocket',
    'Spectral Arrow',
    'Shulker Bullet',
    'Dragon Fireball',
    'Llama Spit',

    // Blocks
    'Primed TNT',
    'Falling Block',

    // Vehicles
    'Boat',
    'Minecart',
    'Minecart with Chest',
    'Minecart with Furnace',
    'Minecart with TNT',
    'Minecart with Hopper',
    'Minecart with Spawner',
    'Minecart with Commandblock',

    // Hostile
    'Elder Guardian',
    'Whiter Skeleton',
    'Stray',
    'Husk',
    'Zombie Villager',
    'Evoker',
    'Vex',
    'Vindicator',
    'Illusioner',
    'Creeper',
    'Skeleton',
    'Spider',
    'Giant',
    'Zombie',
    'Slime',
    'Ghast',
    'Zombie Pigman',
    'Enderman',
    'Cave Spider',
    'Silverfish',
    'Blaze',
    'Magma Cube',
    'Ender Dragon',
    'Wither',
    'Witch',
    'Endermite',
    'Guardian',
    'Shulker',

    // Passive
    'Skeleton Horse',
    'Zombie Horse',
    'Donkey',
    'Mule',
    'Bat',
    'Pig',
    'Sheep',
    'Cow',
    'Chicken',
    'Squid',
    'Wolf',
    'Mooshroom',
    'Snow Golem',
    'Ocelot',
    'Iron Golem',
    'Horse',
    'Rabbit',
    'Polar Bear',
    'Llama',
    'Parrot',
    'Villager',

    // Other
    'Lightning Bolt'
    );

function EntityFromName(AName: string; out AEntity: TEntity): Boolean;

implementation

type

  TEntityMap = TMap<string, TEntity, TStringHasher>;

var
  EntityMap: TEntityMap;

function EntityFromName(AName: string; out AEntity: TEntity): Boolean;
begin
  Result := EntityMap.Get(AName, AEntity);
end;

procedure InitEntityMap;
var
  Entity: TEntity;
begin       
  EntityMap := TEntityMap.Create;
  for Entity := Low(TEntity) to High(TEntity) do
    EntityMap[EntityNames[Entity]] := Entity;
end;

initialization

InitEntityMap;

finalization

EntityMap.Free;

end.
