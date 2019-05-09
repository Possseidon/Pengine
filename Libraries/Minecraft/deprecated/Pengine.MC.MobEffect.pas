unit Pengine.MC.MobEffect;

interface

uses
  Pengine.HashCollections,

  Pengine.MC.Namespace;

type

  TMobEffect = (
    meSpeed,
    meSlowness,
    meHaste,
    meMiningFatigue,
    meStrength,
    meInstantHealth,
    meInstantDamage,
    meJumpBoost,
    meNausea,
    meRegeneration,
    meResistance,
    meFireResistance,
    meWaterBreathing,
    meInvisibility,
    meBlindness,
    meNightVision,
    meHunger,
    meWeakness,
    mePoison,
    meWither,
    meHealthBoost,
    meAbsorption,
    meSaturation,
    meGlowing,
    meLevitation,
    meLuck,
    meBadLuck,
    meSlowFalling,
    meConduitPower,
    meDolphinsGrace
    );

  TMobEffects = set of TMobEffect;

const

  MobEffectNames: array [TMobEffect] of string = (
    'speed',
    'slowness',
    'haste',
    'mining_fatigue',
    'strength',
    'instant_health',
    'instant_damage',
    'jump_boost',
    'nausea',
    'regeneration',
    'resistance',
    'fire_resistance',
    'water_breathing',
    'invisibility',
    'blindness',
    'night_vision',
    'hunger',
    'weakness',
    'poison',
    'wither',
    'health_boost',
    'absorption',
    'saturation',
    'glowing',
    'levitation',
    'luck',
    'bad_luck',
    'slow_falling',
    'conduit_power',
    'dolphins_grace'
    );

  MobEffectDisplayNames: array [TMobEffect] of string = (
    'Speed',
    'Slowness',
    'Haste',
    'Mining Fatigue',
    'Strength',
    'Instant Health',
    'Instant Damage',
    'Jump Boost',
    'Nausea',
    'Regeneration',
    'Resistance',
    'Fire Resistance',
    'Water Breathing',
    'Invisibility',
    'Blindness',
    'Night Vision',
    'Hunger',
    'Weakness',
    'Poison',
    'Wither',
    'Health Boost',
    'Absorption',
    'Saturation',
    'Glowing',
    'Levitation',
    'Luck',
    'Bad Luck',
    'Slow Falling',
    'Conduit Power',
    'Dolphin''s Grace'
    );

function MobEffectFromName(AName: TNSPath; out AMobEffect: TMobEffect): Boolean;

implementation

type

  TMobEffectMap = TMap<TNSPath, TMobEffect, TNSPathHasher>;

var
  MobEffectMap: TMobEffectMap;

function MobEffectFromName(AName: TNSPath; out AMobEffect: TMobEffect): Boolean;
begin
  Result := MobEffectMap.Get(AName, AMobEffect);
end;

procedure InitMobEffectMap;
var
  MobEffect: TMobEffect;
begin
  MobEffectMap := TMobEffectMap.Create;
  for MobEffect := Low(TMobEffect) to High(TMobEffect) do
    MobEffectMap[MobEffectNames[MobEffect]] := MobEffect;
end;

initialization

InitMobEffectMap;

finalization

MobEffectMap.Free;

end.
