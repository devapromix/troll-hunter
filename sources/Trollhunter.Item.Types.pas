unit Trollhunter.Item.Types;

interface

type
  TItemType = (itNone, itUnavailable, itCorpse, itKey, itCoin, itGem, itPotion,
    itFlask, itOrb, itStone, itScroll, itBook, itRune, itFood, itPlant, itBlade,
    itAxe, itSpear, itMace, itStaff, itWand, itDagger, itBow, itShield,
    itHeadgear, itBodyArmor, itHands, itFeet, itRing, itAmulet, itTalisman,
    itArrow, itQuiver, itTorch);

const
  ItemGlyph: array [TItemType] of char = (' ', ' ', '%', '`', '$', '.', '!',
    '!', 'o', '8', '?', '?', '*', ',', '&', '\', '/', '|', '_', '~', '-', '-',
    ')', '+', '^', '&', '%', '%', '=', '"', '"', '{', '/', 'i');

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
  // &   Chests, Containers     /   Quiver



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
  WeaponTypeItems = [itBlade, itAxe, itSpear, itMace, itStaff, itWand,
    itBow, itDagger];
  ArmorTypeItems = [itHeadgear, itBodyArmor, itShield, itHands, itFeet];
  QuiverTypeItems = [itQuiver];
  MagicWeaponTypeItems = [itStaff, itWand];
  RangedWeaponItems = [itBow, itWand];
  TorchTypeItems = [itTorch];
  ArrowTypeItems = [itArrow];

  IdentTypeItems = WeaponTypeItems + ArmorTypeItems + JewelryTypeItems +
    FlaskTypeItems + QuiverTypeItems;
  AllwaysIdentTypeItems = JewelryTypeItems + FlaskTypeItems;
  DefenseTypeItems = ArmorTypeItems + JewelryTypeItems;
  DamageTypeItems = WeaponTypeItems + JewelryTypeItems;
  RepairTypeItems = FlaskTypeItems;
  SmithTypeItems = DefenseTypeItems + DamageTypeItems;
  UseTypeItems = PotionTypeItems + ScrollTypeItems + FoodTypeItems +
    PlantTypeItems + RuneTypeItems + BookTypeItems + GemTypeItems +
    RepairTypeItems + MagicTypeItems;
  NotDropTypeItems = [itNone] + KeyTypeItems + CorpseTypeItems + RuneTypeItems +
    ArrowTypeItems;
  NotEquipTypeItems = UseTypeItems + NotDropTypeItems + CoinTypeItems;
  NotInfoTypeItems = [itNone] + KeyTypeItems + CorpseTypeItems + CoinTypeItems +
    ArrowTypeItems;
  AutoPickupItems = CoinTypeItems + PotionTypeItems + ScrollTypeItems +
    FoodTypeItems + RuneTypeItems + BookTypeItems + GemTypeItems +
    KeyTypeItems + PlantTypeItems + FlaskTypeItems + MagicTypeItems +
    ArrowTypeItems;

type
  TItemEnum = (
    // All maps
    itmNone, itmCorpse, itmGold,
    // Flasks
    itmRuby_Flask, itmAmethyst_Flask, itmBismuth_Flask, itmSilver_Flask,
    itmAquamarine_Flask, itmSapphire_Flask, itmQuicksilver_Flask, itmTopaz_Flask,
    itmSulphur_Flask, itmGranite_Flask, itmQuartz_Flask, itmSacred_Flask,
    itmJade_Flask, itmHallowed_Flask, itmCoruscating_Flask, itmSanctified_Flask,
    itmDitmine_Flask, itmGold_Flask, itmDiamond_Flask, itmEternal_Flask,
    // Potions of Healing
    itmPotion_of_Minor_Healing, itmPotion_of_Lesser_Healing,
    itmPotion_of_Greater_Healing, itmPotion_of_Full_Healing,
    // Potions of Mana
    itmPotion_of_Minor_Mana, itmPotion_of_Lesser_Mana, itmPotion_of_Greater_Mana,
    itmPotion_of_Full_Mana,
    // Stones
    itmStone_of_Stamina, itmStone_of_Mana, itmStone_of_Recovery,
    // Scrolls
    itmScroll_of_Minor_Healing, itmScroll_of_Lesser_Healing,
    itmScroll_of_Greater_Healing, itmScroll_of_Full_Healing, itmScroll_of_Hunger,
    itmScroll_of_Sidestepping, itmScroll_of_Phasing, itmScroll_of_Teleportation,
    itmScroll_of_Disappearing, itmScroll_of_Town_Portal, itmScroll_of_Bloodlust,
    itmScroll_of_Identify, itmScroll_of_Full_Identify, itmScroll_of_Enchant_Item,
    itmScroll_of_Recharge,
    // Runes
    itmRune_of_Minor_Healing, itmRune_of_Lesser_Healing,
    itmRune_of_Greater_Healing, itmRune_of_Full_Healing, itmRune_of_Teleportation,
    itmRune_of_Town_Portal,
    // Foods and Plants
    itmBread_Ration, itmValley_Root, itmRat_Pod, itmKobold_Bulb, itmHunk_of_Meat,
    // Keys
    itmKey,
    // Torch
    itmTorch, itmOil_Lamp,
    // Orbs
    itmLight_Orb, itmLife_Orb, itmMana_Orb,
    // Foods and Plants
    itmHealing_Herb,
    // Ruby (Gems)
    itmChipped_Ruby, itmFlawed_Ruby, itmRuby, itmFlawless_Ruby, itmPerfect_Ruby,
    itmImperial_Ruby, itmRoyal_Ruby,
    // Topaz (Gems)
    itmChipped_Topaz, itmFlawed_Topaz, itmTopaz, itmFlawless_Topaz, itmPerfect_Topaz,
    itmImperial_Topaz, itmRoyal_Topaz,
    // Sapphire (Gems)
    itmChipped_Sapphire, itmFlawed_Sapphire, itmSapphire, itmFlawless_Sapphire,
    itmPerfect_Sapphire, itmImperial_Sapphire, itmRoyal_Sapphire,
    // Emerald (Gems)
    itmChipped_Emerald, itmFlawed_Emerald, itmEmerald, itmFlawless_Emerald,
    itmPerfect_Emerald, itmImperial_Emerald, itmRoyal_Emerald,
    // Diamond (Gems)
    itmChipped_Diamond, itmFlawed_Diamond, itmDiamond, itmFlawless_Diamond,
    itmPerfect_Diamond, itmImperial_Diamond, itmRoyal_Diamond,
    // Rings
    itmMoonstone_Ring, itmValuable_Ring, itmPrecious_Ring, itmEthreal_Ring,
    itmExquisite_Ring, itmScarab_Ring, itmCrystal_Ring, itmPrismatic_Ring,
    itmCitrine_Ring, itmGold_Ring,
    // Amulets
    itmTurquoise_Amulet, itmOnyx_Amulet, itmViridian_Amulet, itm_Lunar_Amulet,
    itmJade_Amulet, itmClaw_Amulet, itmAgate_Amulet, itmGothic_Amulet,
    itmAncient_Amulet, itmAlmighty_Amulet,
    // Talismans
    itmLongtooth_Talisman, itmDark_Eye, itmBlack_Maw_Talisman, itmVoid_Eye,
    itmBlood_Boil, itmDream_Fragment, itmWinter_Heart, itmTear_of_Purity,
    itmCrimson_Talisman, itmDead_Reckoning,
    // Wands
    itmYew_Wand, itmWater_Wand, itmBone_Wand, itmIvory_Wand, itmGrim_Wand,
    itmObsidian_Wand, itmGrave_Wand, itmMystic_Wand, itmLich_Wand, itmCelestial_Wand,
    // Daggers
    itmDagger1, itmDagger2, itmDagger3, itmDagger4, itmDagger5, itmDagger6, itmDagger7,
    itmDagger8, itmDagger9, itmDagger10,
    // Bows
    itmShort_Bow, itmHunting_Bow, itmLong_Bow, itmNomad_Bow, itmHeavy_Bow,
    itmComposite_Bow, itmMaster_Bow, itmWar_Bow, itmDragon_Bow, itmAncient_Bow,
    // Quivers
    itmLight_Quiver, itmLeather_Quiver, itmHarpy_Hide_Quiver, itmSilverleaf_Quiver,
    itmTroll_Hide_Quiver, itmKnothide_Quiver, itmMedium_Quiver, itmHunting_Quiver,
    itmWar_Quiver, itmRuned_Quiver, itmQuickdraw_Quiver, itmRaptor_Hide_Quiver,
    itmHeavy_Quiver,
    // Dark Wood
    itmCap, itmWar_Cap, itmHood, itmRed_Hat, // Headgear
    itmQuilted_Armor, itmLeather_Armor, itmLight_Clothes, itmLeather_Apron,
    // Body Armor
    itmLeather_Gloves, itmHide_Gloves, // Gloves
    itmShoes, itmLeather_Boots, // Boots
    itmBuckler, itmTarge_Shield, // Shield
    itmRusty_Sword, itmShort_Sword, // Blade
    itmHatchet, itmBattle_Axe, // Axe
    itmShort_Spear, itmSpear, // Spear
    itmSlag_Hammer, itmSpiked_Cudgel, // Mace
    itmQuarterstaff, itmStaff2, // Staff
    // Gray Cave
    itmHelm, itmGrand_Helm, itmLeather_Cap, itmMask, // Headgear
    itmHard_Leather_Armor, itmBattle_Armor, itmFancy_Clothes, itmRobe, // Body Armor
    itmKobold_Gloves, itmChain_Gloves, // Gloves
    itmMesh_Boots, itmHeavy_Boots, // Boots
    itmSmall_Shield, itmKite_Shield, // Shield
    itmBroad_Sword, itmLong_Sword, // Blade
    itmMeat_Axe, itmFlesh_Tearer, // Axe
    itmJavelin, itmFuscina, // Spear
    itmWarhammer, itmWar_Mace, // Mace
    itmStaff3, itmStaff4, // Staff
    // Deep Cave
    itmGreat_Helm, itmFull_Helm, itmBone_Helmet, itmWizard_Hat, // Headgear
    itmBrigantine_Armor, itmRing_Mail, itmLight_Furs, itmClean_Robe, // Body Armor
    itmEtched_Gloves, itmHeavy_Gloves, // Gloves
    itmGreaves, itmBoneweave_Boots, // Boots
    itmBone_Shield, itmHeater_Shield, // Shield
    itmMoon_Blade, itmScimitar, // Blade
    itmWar_Axe, itmDark_Axe, // Axe
    itmWar_Spear, itmHarpoon, // Spear
    itmFlanged_Mace, itmWar_Gavel, // Mace
    itmStaff5, itmStaff6, // Staff
    // Blood Cave
    itmHorned_Helmet, itmSpired_Helm, itmDiadem, itmTiara, // Headgear
    itmChain_Mail, itmScale_Mail, itmThick_Furs, itmHard_Robe, // Body Armor
    itmBattle_Gauntlets, itmWar_Gauntlets, // Gloves
    itmChain_Boots, itmWar_Boots, // Boots
    itmHeavy_Shield, itmLarge_Shield, // Shield
    itmBastard_Sword, itmGreat_Sword, // Blade
    itmBerserker_Axe, itmMarauder_Axe, // Axe
    itmSilvan_Whisper, itmImpaler, // Spear
    itmBarbarous_Mace, itmAdept_Hammer, // Mace
    itmStaff7, itmStaff8, // Staff
    // Drom
    itmCasque, itmWinged_Helm, itmMagic_Helmet, itmCrown, // Headgear
    itmSplint_Mail, itmPlate_Mail, itmMoloch_Robe, itmBoneweave_Hauberk,
    // Body Armor
    itmTroll_Gauntlets, itmPlated_Gauntlets, // Gloves
    itmBattle_Boots, itmPlate_Boots, // Boots
    itmTower_Shield, itmGothic_Shield, // Shield
    itmRune_Sword, itmTroll_Slayer, // Blade
    itmChopper, itmDemon_Axe, // Axe
    itmSoul_Reaver, itmHoned_Spear, // Spear
    itmWar_Maul, itmDoom_Hammer, // Mace
    itmStaff9, itmStaff10, // Staff
    // Arrows
    itmArrows
    );

const
  TavernItems = [itmKey, itmScroll_of_Hunger, itmTorch, itmOil_Lamp, itmLight_Orb];

const
  tfBlessed = 1;
  tfCursed = -1;

type
  TPriceType = (ptNone, ptSell, ptBuy, ptRepair);

type
  TBonusType = (btLife, btMana, btVis, btExtraGold, btStr, btDex, btWil, btPer,
    btReLife, btReMana, btLifeAfEachKill, btManaAfEachKill, btQuiverCap,
    btWandCap);

type
  TSetOfItem = set of TItemType;

implementation

end.
