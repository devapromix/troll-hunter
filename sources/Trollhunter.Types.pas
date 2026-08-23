unit Trollhunter.Types;

interface

type
  Int = System.NativeInt;
  UInt = System.Word;

const
  UIntMax = High(Byte);

type
  TShopEnum = (shPotions, shScrolls, shHealer, shMana, shSmith, shArmors,
    shGloves, shFoods, shWeapons, shBoots, shTavern, shShields, shHelms,
    shJewelry, shGem, shQuivers, shStaves, shWands, shBooks, shBows,
    shDaggers, shVenoms);

  TNPCActionEnum = (naHeal, naRepair, naSell, naBuyArrows, naIdentify);

implementation

end.
