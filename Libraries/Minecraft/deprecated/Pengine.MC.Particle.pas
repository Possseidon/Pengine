unit Pengine.MC.Particle;

interface

uses
  Pengine.HashCollections,

  Pengine.MC.Namespace;

type

  TParticle = (
    ptAmbientEntityEffect,
    ptAngryVillager,
    ptBarrier,
    ptBlock,
    ptBubble,
    ptBubbleColumnUp,
    ptBubblePop,
    ptCloud,
    ptCrit,
    ptCurrentDown,
    ptDamageIndicator,
    ptDolphin,
    ptDragonBreath,
    ptDrippingLava,
    ptDrippingWater,
    ptDust,
    ptEffect,
    ptElderGuardian,
    ptEnchant,
    ptEnchantedHit,
    ptEndRod,
    ptEntityEffect,
    ptExplosion,
    ptFallingDust,
    ptFirework,
    ptFishing,
    ptFlame,
    ptHappyVillager,
    ptHeart,
    ptExplosionEmitter,
    ptInstantEffect,
    ptItem,
    ptItemSlime,
    ptItemSnowball,
    ptLargeSmoke,
    ptLava,
    ptMycelium,
    ptNautilus,
    ptNote,
    ptPoof,
    ptPortal,
    ptRain,
    ptSmoke,
    ptSpit,
    ptSplash,
    ptSquidInk,
    ptSweepAttack,
    ptTotemOfUndying,
    ptUnderwater,
    ptWitch
    );

const

  ParticleNames: array [TParticle] of string = (
    'ambient_entity_effect',
    'angry_villager',
    'barrier',
    'block',
    'bubble',
    'bubble_column_up',
    'bubble_pop',
    'cloud',
    'crit',
    'current_down',
    'damage_indicator',
    'dolphin',
    'dragon_breath',
    'dripping_lava',
    'dripping_water',
    'dust',
    'effect',
    'elder_guardian',
    'enchant',
    'enchanted_hit',
    'end_rod',
    'entity_effect',
    'explosion',
    'falling_dust',
    'firework',
    'fishing',
    'flame',
    'happy_villager',
    'heart',
    'explosion_emitter',
    'instant_effect',
    'item',
    'item_slime',
    'item_snowball',
    'large_smoke',
    'lava',
    'mycelium',
    'nautilus',
    'note',
    'poof',
    'portal',
    'rain',
    'smoke',
    'spit',
    'splash',
    'squid_ink',
    'sweep_attack',
    'totem_of_undying',
    'underwater',
    'witch'
    );

  ParticleDisplayNames: array [TParticle] of string = (
    'Ambient Entity Effect',
    'Angry Villager',
    'Barrier',
    'Block',
    'Bubble',
    'Bubble Column Up',
    'Bubble Pop',
    'Cloud',
    'Crit',
    'Current Down',
    'Damage Indicator',
    'Dolphin',
    'Dragon Breath',
    'Dripping Lava',
    'Dripping Water',
    'Dust',
    'Effect',
    'Elder Guardian',
    'Enchant',
    'Enchanted Hit',
    'End Rod',
    'Entity Effect',
    'Explosion',
    'Falling Dust',
    'Firework',
    'Fishing',
    'Flame',
    'Happy Villager',
    'Heart',
    'Explosion Emitter',
    'Instant Effect',
    'Item',
    'Item Slime',
    'Item Snowball',
    'Large Smoke',
    'Lava',
    'Mycelium',
    'Nautilus',
    'Note',
    'Poof',
    'Portal',
    'Rain',
    'Smoke',
    'Spit',
    'Splash',
    'Squid Ink',
    'Sweep Attack',
    'Totem of Undying',
    'Underwater',
    'Witch'
    );

function ParticleFromName(ANSPath: TNSPath; out AParticle: TParticle): Boolean;

implementation

type

  TParticleMap = TMap<TNSPath, TParticle, TNSPathHasher>;

var
  ParticleMap: TParticleMap;

function ParticleFromName(ANSPath: TNSPath; out AParticle: TParticle): Boolean;
begin
  Result := ParticleMap.Get(ANSPath, AParticle);
end;

procedure InitParticleMap;
var
  Particle: TParticle;
begin
  ParticleMap := TParticleMap.Create;
  for Particle := Low(TParticle) to High(TParticle) do
    ParticleMap[ParticleNames[Particle]] := Particle;
end;

initialization

InitParticleMap;

finalization

ParticleMap.Free;

end.
