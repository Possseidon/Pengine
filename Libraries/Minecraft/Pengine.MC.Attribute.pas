unit Pengine.MC.Attribute;

interface

type

  TAttribute = (
    atGenericMaxHealth,
    atGenericFollowRange,
    atGenericKnockbackResistance,
    atGenericMovementSpeed,
    atGenericAttackDamage,
    atGenericArmor,
    atGenericArmorToughness,
    atGenericAttackSpeed,
    atGenericLuck,
    atHorseJumpStength,
    atGenericFlyingSpeed,
    atZombieSpawnReinforcements
    );

  TAttributeOperation = (
    aoAddition,
    aoMultiplyBase,
    aoMultiplyTotal
    );

  TAttributeSlot = (
    asMainhand,
    asOffhand,
    asFeet,
    asLegs,
    asChest,
    asHead
    );

  TAttributeSlots = set of TAttributeSlot;

const

  AttributeDisplayNames: array [TAttribute] of string = (
    'Max Health',
    'Follow Range',
    'Knockback Resistance',
    'Movement Speed',
    'Attack Damage',
    'Armor',
    'Armor Toughness',
    'AttackSpeed',
    'Luck',
    'Horse Jump Stength',
    'Parrot Flying Speed',
    'Zombie Reinforcement Spawning'
    );

  AttributeNames: array [TAttribute] of string = (
    'generic.maxHealth',
    'generic.followRange',
    'generic.knockbackResistance',
    'generic.movementSpeed',
    'generic.attackDamage',
    'generic.armor',
    'generic.armorToughness',
    'generic.attackSpeed',
    'generic.luck',
    'horse.jumpStength',
    'generic.flyingSpeed',
    'zombie.spawnReinforcements'
    );

  AttributeOperationDisplayNames: array [TAttributeOperation] of string = (
    'Addition',
    'Multiply Base',
    'Multiply Total'
    );

  AttributeOperationNames: array [TAttributeOperation] of string = (
    'addition',
    'multiply_base',
    'multiply_total'
    );

  AttributeSlotDisplayNames: array [TAttributeSlot] of string = (
    'Mainhand',
    'Offhand',
    'Feet',
    'Legs',
    'Chest',
    'Head'
    );

  AttributeSlotNames: array [TAttributeSlot] of string = (
    'mainhand',
    'offhand',
    'feet',
    'legs',
    'chest',
    'head'
    );

function AttributeFromName(AName: string; out AAttribute: TAttribute): Boolean;
function AttributeOperationFromName(AName: string; out AAttributeOperation: TAttributeOperation): Boolean;
function AttributeSlotFromName(AName: string; out AAttributeSlot: TAttributeSlot): Boolean;

implementation

function AttributeFromName(AName: string; out AAttribute: TAttribute): Boolean;
var
  Attribute: TAttribute;
begin
  for Attribute := Low(TAttribute) to High(TAttribute) do
    if AName = AttributeNames[Attribute] then
    begin
      AAttribute := Attribute;
      Exit(True);
    end;
  Result := False;
end;

function AttributeOperationFromName(AName: string; out AAttributeOperation: TAttributeOperation): Boolean;
var
  AttributeOperation: TAttributeOperation;
begin
  for AttributeOperation := Low(TAttributeOperation) to High(TAttributeOperation) do
    if AName = AttributeOperationNames[AttributeOperation] then
    begin
      AAttributeOperation := AttributeOperation;
      Exit(True);
    end;
  Result := False;
end;

function AttributeSlotFromName(AName: string; out AAttributeSlot: TAttributeSlot): Boolean;
var
  AttributeSlot: TAttributeSlot;
begin
  for AttributeSlot := Low(TAttributeSlot) to High(TAttributeSlot) do
    if AName = AttributeSlotNames[AttributeSlot] then
    begin
      AAttributeSlot := AttributeSlot;
      Exit(True);
    end;
  Result := False;
end;

end.
