unit Trollhunter.Item.Types;

interface

type
  TItemType = (itNone, itUnavailable, itCorpse, itKey, itCoin, itGem, itPotion, itFlask, itOrb, itStone, itScroll, itBook, itRune, itFood, itPlant,
    itBlade, itAxe, itSpear, itMace, itBattleStaff, itStaff, itWand, itDagger, itBow, itCrossbow, itShield, itHeadgear, itBodyArmor, itHands, itFeet,
    itRing, itAmulet, itTalisman, itArrow, itTorch);

const
  ItemGlyph: array [TItemType] of Char = (' ', ' ', '%', '`', '$', '.', '!', '!', 'o', '8', '?', '?', '*', ',', '&', '\', '/', '|', '_', '~', '~',
    '-', '-', ')', '}', '+', '^', '&', '%', '%', '=', '"', '"', '{', 'i');

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
  WeaponTypeItems = [itBlade, itAxe, itSpear, itMace, itStaff, itBattleStaff, itWand, itBow, itDagger];
  ArmorTypeItems = [itHeadgear, itBodyArmor, itShield, itHands, itFeet];
  MagicWeaponTypeItems = [itStaff, itWand];
  RangedWeaponItems = [itBow, itCrossbow];
  TorchTypeItems = [itTorch];

  IdentTypeItems = WeaponTypeItems + ArmorTypeItems + JewelryTypeItems + FlaskTypeItems;
  AllwaysIdentTypeItems = JewelryTypeItems + FlaskTypeItems;
  DefenseTypeItems = ArmorTypeItems + JewelryTypeItems;
  DamageTypeItems = WeaponTypeItems + JewelryTypeItems;
  RepairTypeItems = FlaskTypeItems;
  SmithTypeItems = DefenseTypeItems + DamageTypeItems;
  UseTypeItems = PotionTypeItems + ScrollTypeItems + FoodTypeItems + PlantTypeItems + RuneTypeItems + BookTypeItems + GemTypeItems + RepairTypeItems +
    MagicTypeItems;
  ShootOrCastSpellWeaponTypeItems = RangedWeaponItems + MagicWeaponTypeItems;
  NotDropTypeItems = [itNone] + KeyTypeItems + CorpseTypeItems + RuneTypeItems;
  NotEquipTypeItems = UseTypeItems + NotDropTypeItems + CoinTypeItems;
  NotInfoTypeItems = [itNone] + KeyTypeItems + CorpseTypeItems + CoinTypeItems;
  AutoPickupItems = CoinTypeItems + PotionTypeItems + ScrollTypeItems + FoodTypeItems + RuneTypeItems + BookTypeItems + GemTypeItems + KeyTypeItems +
    PlantTypeItems + FlaskTypeItems + MagicTypeItems;

  // Silver Sword , Forsworn Sword , Hero Sword
  // Skyforge War Axe , Dragonbone War Axe

  // Hide Boots

  // Long Staff , War Staff , Mentor Staff , Obsidian Staff , Elder Staff ,
  // Shamanic Staff , High Priest Staff , Arcane Staff ,

  // Hide Shield , Iron Shield , Steel Shield , Dragonscale Shield , Dragonplate Shield
  // Hide Helmet , Iron Helmet , Steel Helmet , Dragonscale Helmet , Dragonplate Helmet
  // Bonemold Helmet , Carved Helmet , Shellbug Helmet , Ebony Helmet

  // Light Crossbow , Crossbow , Heavy Crossbow , Siege Crossbow , Demon Crossbow
  // Heavy Siege Crossbow , War Crossbow , Phantom Crossbow , Runic Crossbow , Ancient Crossbow

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
    ivWooden_Wand, ivMystery_Wand, ivBonetooth_Wand, ivSanctified_Wand, ivBlack_Maw_Wand, ivDeadhand_Wand, ivLone_Antler_Wand, ivHexclaw_Wand,
    ivMonkey_Paw_Wand, ivFangjaw_Wand,

    // Battle Staves
    ivQuarterstaff, ivBlade_Staff, ivGrinning_Staff, ivHorned_Staff, ivBattle_Staff, ivSerpent_Staff, ivLunaris_Staff, ivTiger_Staff, ivMoon_Staff,
    ivDragon_Staff,

    // Magic Staves
    ivShort_Staff, ivSacrament_Staff, ivGhost_Staff, ivYew_Staff, ivBone_Staff, ivApprentice1s_Staff, ivDisciple1s_Staff, ivAcolyte1s_Staff,
    iv_Archmage1s_Staff, ivAncient_Staff,

    // Daggers
    ivDagger, ivKris, ivSacrificial_Kris, ivLeafblade, ivLong_Dagger, ivJagged_Dagger, ivTrident_Dagger, ivDivine_Dagger, ivBlood_Dagger,
    ivShadow_Dagger,

    // Bows
    ivShort_Bow, ivHunter1s_Bow, ivLong_Bow, ivHero_Bow, ivBattle_Bow, ivComposite_Bow, ivImperial_Bow, ivRevenant_Bow, ivAncient_Bow,
    ivDragonbone_Bow,

    // Body Armors
    ivQuilted_Armor, ivLeather_Armor, ivHard_Leather_Armor, ivBattle_Armor, ivBrigantine_Armor, ivRing_Mail, ivChain_Mail, ivScale_Mail,
    ivSplint_Mail, ivPlate_Mail,

    // Magic Armors
    ivLight_Clothes, ivLeather_Apron, ivFancy_Clothes, ivRobe, ivLight_Furs, ivClean_Robe, ivThick_Furs, ivHard_Robe, ivMoloch_Robe,
    ivBoneweave_Hauberk,

    // Headgear
    ivCap, ivWar_Cap, ivHelm, ivGrand_Helm, ivGreat_Helm, ivFull_Helm, ivHorned_Helmet, ivSpired_Helm, ivCasque, ivWinged_Helm,

    //
    ivHood, ivRed_Hat, ivLeather_Cap, ivMask, ivBone_Helmet, ivWizard_Hat, ivDiadem, ivTiara, ivMagic_Helmet, ivCrown,

    // Gloves
    ivLeather_Gloves, ivHide_Gloves, ivKobold_Gloves, ivChain_Gloves, ivEtched_Gloves, ivHeavy_Gloves, ivBattle_Gauntlets, ivWar_Gauntlets,
    ivTroll_Gauntlets, ivPlated_Gauntlets,

    // Boots
    ivShoes, ivLeather_Boots, ivMesh_Boots, ivHeavy_Boots, ivGreaves, ivBoneweave_Boots, ivChain_Boots, ivWar_Boots, ivBattle_Boots, ivPlate_Boots,

    // Shield
    ivBuckler, ivTarge_Shield, ivSmall_Shield, ivKite_Shield, ivBone_Shield, ivHeater_Shield, ivHeavy_Shield, ivLarge_Shield, ivTower_Shield,
    ivGothic_Shield,

    // Swords
    ivRusty_Sword, ivShort_Sword, ivBroad_Sword, ivLong_Sword, ivMoon_Blade, ivScimitar, ivBastard_Sword, ivGreat_Sword, ivRune_Sword, ivTroll_Slayer,

    // Axes
    ivHatchet, ivBattle_Axe, ivMeat_Axe, ivFlesh_Tearer, ivWar_Axe, ivDark_Axe, ivBerserker_Axe, ivMarauder_Axe, ivChopper, ivDemon_Axe,

    // Spears
    ivShort_Spear, ivSpear, ivJavelin, ivFuscina, ivWar_Spear, ivHarpoon, ivSilvan_Whisper, ivImpaler, ivSoul_Reaver, ivHoned_Spear,

    // Maces
    ivSlag_Hammer, ivSpiked_Cudgel, ivWarhammer, ivWar_Mace, ivFlanged_Mace, ivWar_Gavel, ivBarbarous_Mace, ivAdept_Hammer, ivWar_Maul, ivDoom_Hammer

    //
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
