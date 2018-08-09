unit Pengine.MC.Enchantment;

interface

type

  TEnchantment = (
    enchProtection,
    enchFireProtection,
    enchFeatherFalling,
    enchBlastProtection,
    enchProjectileProtection,
    enchRespiration,
    enchAquaAffinity,
    enchThorns,
    enchDepthStrider,
    enchFrostWalker,
    enchCurseOfBinding,
    enchSharpness,
    enchSmite,
    enchBaneOfArthropods,
    enchKnockback,
    enchFireAspect,
    enchLooting,
    enchSweepingEdge,
    enchEfficiency,
    enchSilkTouch,
    enchUnbreaking,
    enchFortune,
    enchPower,
    enchPunch,
    enchFlame,
    enchInfinity,
    enchLuckOftheSea,
    enchLure,
    enchLoyalty,
    enchImpaling,
    enchRiptide,
    enchChanneling,
    enchMending,
    enchCurseOfVanishing
    );

  TEnchantments = set of TEnchantment;

const

  EnchantmentDisplayNames: array [TEnchantment] of string = (
    'Protection',
    'Fire Protection',
    'Feather Falling',
    'Blast Protection',
    'Projectile Protection',
    'Respiration',
    'Aqua Affinity',
    'Thorns',
    'Depth Strider',
    'Frost Walker',
    'Curse of Binding',
    'Sharpness',
    'Smite',
    'Bane of Arthropods',
    'Knockback',
    'Fire Aspect',
    'Looting',
    'Sweeping Edge',
    'Efficiency',
    'Silk Touch',
    'Unbreaking ',
    'Fortune',
    'Power',
    'Punch',
    'Flame',
    'Infinity',
    'Luck of the Sea',
    'Lure',
    'Loyalty',
    'Impaling',
    'Riptide',
    'Channeling	',
    'Mending',
    'Curse of Vanishing'
    );

  EnchantmentNames: array [TEnchantment] of string = (
    'protection',
    'fire_protection',
    'feather_falling',
    'blast_protection',
    'projectile_protection',
    'respiration',
    'aqua_affinity',
    'thorns',
    'depth_strider',
    'frost_walker',
    'binding_curse',
    'sharpness',
    'smite',
    'bane_of_arthropods',
    'knockback',
    'fire_aspect',
    'looting',
    'sweeping',
    'efficiency',
    'silk_touch',
    'unbreaking',
    'fortune',
    'power',
    'punch',
    'flame',
    'infinity',
    'luck_of_the_sea',
    'lure',
    'loyalty',
    'impaling',
    'riptide',
    'channeling',
    'mending',
    'vanishing_curse'
    );

implementation

end.