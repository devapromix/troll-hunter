unit Trollhunter.Item.Types;

interface

type
  TItemType = (itNone, itUnavailable, itCorpse, itKey, itCoin, itGem, itPotion, itFlask, itOrb, itStone, itScroll, itBook, itRune, itFood, itPlant,
    itBlade, itAxe, itSpear, itMace, itStaff, itWand, itDagger, itBow, itShield, itHeadgear, itBodyArmor, itHands, itFeet, itRing, itAmulet,
    itTalisman, itArrow, itTorch);

const
  ItemGlyph: array [TItemType] of Char = (' ', ' ', '%', '`', '$', '.', '!', '!', 'o', '8', '?', '?', '*', ',', '&', '\', '/', '|', '_', '~', '-',
    '-', ')', '+', '^', '&', '%', '%', '=', '"', '"', '{', 'i');

  // From Angband:
  // !   A potion (or flask)    /   A pole-arm
  // ?   A scroll (or book)     |   An edged weapon
  // ,   Food                   \   A hafted weapon
  // -   A wand or rod          }   A sling, bow, or x-bow
  // _   A staff                {   A shot, arrow, or bolt
  // =   A ring                 (   Soft armour (cloak, robes, leather armor)
  // "   An amulet              [   Hard armour (metal armor)
  // $   Money or gems          ]   Misc. armour (gloves, helm, boots)
  // ~   Pelts and body parts   )   A shield
  // &   Chests, Containers

  //

const
  CoinTypeItems = [itCoin];
  PotionTypeItems = [itPotion, itFlask];
  ScrollTypeItems = [itScroll];
  RuneTypeItems = [itRune];
  BookTypeItems = [itBook];
  CorpseTypeItems = [itCorpse];
  GemTypeItems = [itGem];
  PlantTypeItems = [itPlant];
  FoodTypeItems = [itFood];
  KeyTypeItems = [itKey];
  GlovesTypeItems = [itHands];
  BootsTypeItems = [itFeet];
  ShieldTypeItems = [itShield];
  HelmTypeItems = [itHeadgear];
  FlaskTypeItems = [itFlask];
  MagicTypeItems = [itOrb, itStone];
  JewelryTypeItems = [itRing, itAmulet, itTalisman];
  WeaponTypeItems = [itBlade, itAxe, itSpear, itMace, itStaff, itWand, itBow, itDagger];
  ArmorTypeItems = [itHeadgear, itBodyArmor, itShield, itHands, itFeet];
  MagicWeaponTypeItems = [itStaff, itWand];
  RangedWeaponItems = [itBow];
  TorchTypeItems = [itTorch];

  IdentTypeItems = WeaponTypeItems + ArmorTypeItems + JewelryTypeItems + FlaskTypeItems;
  AllwaysIdentTypeItems = JewelryTypeItems + FlaskTypeItems;
  DefenseTypeItems = ArmorTypeItems + JewelryTypeItems;
  DamageTypeItems = WeaponTypeItems + JewelryTypeItems;
  RepairTypeItems = FlaskTypeItems;
  SmithTypeItems = DefenseTypeItems + DamageTypeItems;
  UseTypeItems = PotionTypeItems + ScrollTypeItems + FoodTypeItems + PlantTypeItems + RuneTypeItems + BookTypeItems + GemTypeItems + RepairTypeItems +
    MagicTypeItems;
  NotDropTypeItems = [itNone] + KeyTypeItems + CorpseTypeItems + RuneTypeItems;
  NotEquipTypeItems = UseTypeItems + NotDropTypeItems + CoinTypeItems;
  NotInfoTypeItems = [itNone] + KeyTypeItems + CorpseTypeItems + CoinTypeItems;
  AutoPickupItems = CoinTypeItems + PotionTypeItems + ScrollTypeItems + FoodTypeItems + RuneTypeItems + BookTypeItems + GemTypeItems + KeyTypeItems +
    PlantTypeItems + FlaskTypeItems + MagicTypeItems;

  // Silver Sword , Forsworn Sword , Hero Sword
  // Skyforge War Axe , Dragonbone War Axe

  // Hide Boots

  // Hide Shield , Iron Shield , Steel Shield , Dragonscale Shield , Dragonplate Shield
  // Hide Helmet , Iron Helmet , Steel Helmet , Dragonscale Helmet , Dragonplate Helmet
  // Bonemold Helmet , Carved Helmet , Shellbug Helmet , Ebony Helmet
  // Hunter's Bow, Long Bow , Imperial Bow , Ancient Bow , Hero Bow , Dragonbone Bow

  // Honed ... , Skyforge ... , Bloodcursed ... , Sunhallowed ...

  { rough / необроблений
    imperfect / недосконалий
    common / типовий
    precious / дорогоцінний
    flawless / без недоліків
    perfect / досконалий
    outworldly / неземний }

  { Potion, Flask, Extract, Essence, Elixir }

type
  TItemEnum = (
    // All maps
    None, ivCorpse, ivGold,

    // Flasks
    ivRuby_Flask, ivAmethyst_Flask, ivBismuth_Flask, ivSilver_Flask, ivAquamarine_Flask, ivSapphire_Flask, ivQuicksilver_Flask, ivTopaz_Flask,
    ivSulphur_Flask, ivGranite_Flask, ivQuartz_Flask, ivSacred_Flask, ivJade_Flask, ivHallowed_Flask, ivCoruscating_Flask, ivSanctified_Flask,
    ivDivine_Flask, ivGold_Flask, ivDiamond_Flask, ivEternal_Flask,

    // Potions of Healing
    ivPotion_of_Minor_Healing, ivPotion_of_Lesser_Healing, ivPotion_of_Greater_Healing, ivPotion_of_Full_Healing,
    // Potions of Mana
    ivPotion_of_Minor_Mana, ivPotion_of_Lesser_Mana, ivPotion_of_Greater_Mana, ivPotion_of_Full_Mana,

    // Stones
    ivStone_of_Stamina, ivStone_of_Mana, ivStone_of_Recovery,

    // Scrolls
    ivScroll_of_Minor_Healing, ivScroll_of_Lesser_Healing, ivScroll_of_Greater_Healing, ivScroll_of_Full_Healing, ivScroll_of_Hunger,
    ivScroll_of_Sidestepping, ivScroll_of_Phasing, ivScroll_of_Teleportation, ivScroll_of_Disappearing, ivScroll_of_Town_Portal,
    ivScroll_of_Bloodlust, ivScroll_of_Identify, ivScroll_of_Full_Identify, ivScroll_of_Enchant_Item,

    // Runes
    ivRune_of_Minor_Healing, ivRune_of_Lesser_Healing, ivRune_of_Greater_Healing, ivRune_of_Full_Healing, ivRune_of_Teleportation,
    ivRune_of_Town_Portal,

    // Foods and Plants
    ivBread_Ration, ivValley_Root, ivRat_Pod, ivKobold_Bulb, ivHunk_of_Meat,

    // Keys
    ivKey,
    // Torch
    ivTorch, ivOil_Lamp,
    // Orbs
    ivLight_Orb, ivLife_Orb, ivMana_Orb,

    // Foods and Plants
    ivHealing_Herb,

    // Ruby (Gems)
    ivChipped_Ruby, ivFlawed_Ruby, ivRuby, ivFlawless_Ruby, ivPerfect_Ruby, ivImperial_Ruby, ivRoyal_Ruby,
    // Topaz (Gems)
    ivChipped_Topaz, ivFlawed_Topaz, ivTopaz, ivFlawless_Topaz, ivPerfect_Topaz, ivImperial_Topaz, ivRoyal_Topaz,
    // Sapphire (Gems)
    ivChipped_Sapphire, ivFlawed_Sapphire, ivSapphire, ivFlawless_Sapphire, ivPerfect_Sapphire, ivImperial_Sapphire, ivRoyal_Sapphire,
    // Emerald (Gems)
    ivChipped_Emerald, ivFlawed_Emerald, ivEmerald, ivFlawless_Emerald, ivPerfect_Emerald, ivImperial_Emerald, ivRoyal_Emerald,
    // Diamond (Gems)
    ivChipped_Diamond, ivFlawed_Diamond, ivDiamond, ivFlawless_Diamond, ivPerfect_Diamond, ivImperial_Diamond, ivRoyal_Diamond,

    // Rings
    ivMoonstone_Ring, ivValuable_Ring, ivPrecious_Ring, ivEthreal_Ring, ivExquisite_Ring, ivScarab_Ring, ivCrystal_Ring, ivPrismatic_Ring,
    ivCitrine_Ring, ivGold_Ring,

    // Amulets
    ivTurquoise_Amulet, ivOnyx_Amulet, ivViridian_Amulet, iv_Lunar_Amulet, ivJade_Amulet, ivClaw_Amulet, ivAgate_Amulet, ivGothic_Amulet,
    ivAncient_Amulet, ivAlmighty_Amulet,

    // Talismans
    ivLongtooth_Talisman, ivDark_Eye, ivBlack_Maw_Talisman, ivVoid_Eye, ivBlood_Boil, ivDream_Fragment, ivWinter_Heart, ivTear_of_Purity,
    ivCrimson_Talisman, ivDead_Reckoning,

    // Wands
    ivWand1, ivWand2, ivWand3, ivWand4, ivWand5, ivWand6, ivWand7, ivWand8, ivWand9, ivWand10,

    // Staves
    ivQuarterstaff, ivShort_Staff, ivStaff3, ivStaff4, ivStaff5, ivStaff6, ivStaff7, ivStaff8, ivStaff9, ivStaff10,

    // Daggers
    ivDagger, ivKris, ivSacrificial_Kris, ivDagger4, ivDagger5, ivDagger6, ivDagger7, ivDagger8, ivDagger9, ivDagger10,

    // Bows
    ivShort_Bow, ivHunter1s_Bow, ivBow3, ivBow4, ivBow5, ivBow6, ivBow7, ivBow8, ivBow9, ivBow10,

    // Dark Wood
    ivCap, ivWar_Cap, ivHood, ivRed_Hat, // Headgear
    ivQuilted_Armor, ivLeather_Armor, ivLight_Clothes, ivLeather_Apron,
    // Body Armor
    ivLeather_Gloves, ivHide_Gloves, // Gloves
    ivShoes, ivLeather_Boots, // Boots
    ivBuckler, ivTarge_Shield, // Shield
    ivRusty_Sword, ivShort_Sword, // Blade
    ivHatchet, ivBattle_Axe, // Axe
    ivShort_Spear, ivSpear, // Spear
    ivSlag_Hammer, ivSpiked_Cudgel, // Mace
    // Gray Cave
    ivHelm, ivGrand_Helm, ivLeather_Cap, ivMask, // Headgear
    ivHard_Leather_Armor, ivBattle_Armor, ivFancy_Clothes, ivRobe, // Body Armor
    ivKobold_Gloves, ivChain_Gloves, // Gloves
    ivMesh_Boots, ivHeavy_Boots, // Boots
    ivSmall_Shield, ivKite_Shield, // Shield
    ivBroad_Sword, ivLong_Sword, // Blade
    ivMeat_Axe, ivFlesh_Tearer, // Axe
    ivJavelin, ivFuscina, // Spear
    ivWarhammer, ivWar_Mace, // Mace
    // Deep Cave
    ivGreat_Helm, ivFull_Helm, ivBone_Helmet, ivWizard_Hat, // Headgear
    ivBrigantine_Armor, ivRing_Mail, ivLight_Furs, ivClean_Robe, // Body Armor
    ivEtched_Gloves, ivHeavy_Gloves, // Gloves
    ivGreaves, ivBoneweave_Boots, // Boots
    ivBone_Shield, ivHeater_Shield, // Shield
    ivMoon_Blade, ivScimitar, // Blade
    ivWar_Axe, ivDark_Axe, // Axe
    ivWar_Spear, ivHarpoon, // Spear
    ivFlanged_Mace, ivWar_Gavel, // Mace
    // Blood Cave
    ivHorned_Helmet, ivSpired_Helm, ivDiadem, ivTiara, // Headgear
    ivChain_Mail, ivScale_Mail, ivThick_Furs, ivHard_Robe, // Body Armor
    ivBattle_Gauntlets, ivWar_Gauntlets, // Gloves
    ivChain_Boots, ivWar_Boots, // Boots
    ivHeavy_Shield, ivLarge_Shield, // Shield
    ivBastard_Sword, ivGreat_Sword, // Blade
    ivBerserker_Axe, ivMarauder_Axe, // Axe
    ivSilvan_Whisper, ivImpaler, // Spear
    ivBarbarous_Mace, ivAdept_Hammer, // Mace
    // Drom
    ivCasque, ivWinged_Helm, ivMagic_Helmet, ivCrown, // Headgear
    ivSplint_Mail, ivPlate_Mail, ivMoloch_Robe, ivBoneweave_Hauberk,
    // Body Armor
    ivTroll_Gauntlets, ivPlated_Gauntlets, // Gloves
    ivBattle_Boots, ivPlate_Boots, // Boots
    ivTower_Shield, ivGothic_Shield, // Shield
    ivRune_Sword, ivTroll_Slayer, // Blade
    ivChopper, ivDemon_Axe, // Axe
    ivSoul_Reaver, ivHoned_Spear, // Spear
    ivWar_Maul, ivDoom_Hammer // Mace
    );

const
  TavernItems = [ivKey, ivScroll_of_Hunger, ivTorch, ivOil_Lamp, ivLight_Orb];

const
  tfBlessed = 1;
  tfCursed = -1;

type
  TPriceType = (ptNone, ptSell, ptBuy, ptRepair);

type
  TBonusType = (btLife, btMana, btVis, btExtraGold, btStr, btDex, btWil, btPer, btReLife, btReMana, btLifeAfEachKill, btManaAfEachKill);

type
  TSetOfItem = set of TItemType;

implementation

end.
