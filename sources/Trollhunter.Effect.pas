unit Trollhunter.Effect;

interface

type
  TEffect = (efNone, efLife, efMana, efFood, efTeleportation, efIdentification,
  efAllIdentification, efEnchantItem, efRechargeWand, efCraftStr,
    efCraftDex, efCraftWil, efCraftPer, efCraftAtr, efTownPortal,efRegeneration,
    efCurePoison, efVision, efCureWeak,
    efPrmAthletics, efPrmDodge, efPrmConcentration, efPrmToughness, efPrmBlade,
    efPrmAxe, efPrmSpear,
    efPrmMace, efPrmStaff, efPrmWand, efPrmDagger, efPrmBow, efBloodlust,
    efPrmLife, efPrmMana, efPrmDV,
    efPrmPV, efPrmStr, efPrmDex, efPrmWil, efPrmPer, efRepair,
    efPrmBodybuilding, efPrmMeditation,
    efPrmEnchant_Item, efLight, efBerserk, efPrmStealth, efPrmAwareness,
    efPrmTreasureHunter, efPrmGoldFinder, efPrmSurvival, efCharges, efDisenchant,
    efPoisonWeapon, efPrmPoisoning, efManaShield, efWeaken, efBurn, efDrain);

const
  CraftEffLow = efCraftStr;
  CraftEffHigh = efCraftPer;

const
  EfNameStr: array [CraftEffLow .. CraftEffHigh] of string =
    ('Strength', 'Dexterity', 'Willpower', 'Perception');

type
  TEffects = set of TEffect;

implementation

end.