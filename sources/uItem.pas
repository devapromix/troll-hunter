unit uItem;

interface

{ TODO -cПредметы : Свет от лампы днем должен быть на 1 пункт больше макс. для героя, а не макс. как сейчас. }

uses Trollhunter.Types, Trollhunter.Item.Types, Trollhunter.Player.Types,
  uBearLibItemsCommon, uGame, uMap, uPlayer,
  uEntity, uCreature;

type
  TItemBase = record
    Symbol: Char;
    ItemType: TItemType;
    SlotType: TSlotType;
    MaxStack: UInt;
    MaxDurability: UInt;
    Level: UInt;
    Defense: TMinMax;
    Damage: TBaseDamage;
    Price: UInt;
    Color: Cardinal;
    Deep: set of TMapEnum;
    Effects: TEffects;
    Value: UInt;
    ManaCost: UInt;
    Rare: Boolean;
  end;

const
  ItemBase: array [TItemEnum] of TItemBase = (
    /// / == All maps == ////

    // None
    (Symbol: ' '; ItemType: itNone; SlotType: stNone; MaxStack: 1;
    MaxDurability: 0; Level: 0; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 0; Color: clGray; Deep: []),
    // Corpse
    (Symbol: '%'; ItemType: itCorpse; SlotType: stNone; MaxStack: 1;
    MaxDurability: 0; Level: 0; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 0; Color: clGray; Deep: [deDark_Wood .. deDrom]),
    // Gold
    (Symbol: '$'; ItemType: itCoin; SlotType: stNone; MaxStack: 10000;
    MaxDurability: 0; Level: 0; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 0; Color: clYellow; Deep: [deDark_Wood .. deDrom]),

    // Ruby Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 25; Color: clWhite; Deep: [deDark_Wood]; Value: 10;),
    // Amethyst Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 50; Color: clWhite; Deep: [deDark_Wood]; Value: 20;),
    // Bismuth Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1;
    MaxDurability: 0; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 75; Color: clWhite; Deep: [deDark_Wood]; Value: 30;),
    // Silver Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1;
    MaxDurability: 0; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 100; Color: clWhite; Deep: [deDark_Wood]; Value: 40;),
    // Aquamarine Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1;
    MaxDurability: 0; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 125; Color: clWhite; Deep: [deGray_Cave]; Value: 50;),
    // Sapphire Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1;
    MaxDurability: 0; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 150; Color: clWhite; Deep: [deGray_Cave]; Value: 60;),
    // Quicksilver Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1;
    MaxDurability: 0; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 175; Color: clWhite; Deep: [deGray_Cave]; Value: 70;),
    // Topaz Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1;
    MaxDurability: 0; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 200; Color: clWhite; Deep: [deGray_Cave]; Value: 80;),
    // Sulphur Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1;
    MaxDurability: 0; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 225; Color: clWhite; Deep: [deDeep_Cave]; Value: 90;),
    // Granite Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1;
    MaxDurability: 0; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 250; Color: clWhite; Deep: [deDeep_Cave]; Value: 100;),
    // Quartz Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1;
    MaxDurability: 0; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 275; Color: clWhite; Deep: [deDeep_Cave]; Value: 110;),
    // Sacred Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1;
    MaxDurability: 0; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 300; Color: clWhite; Deep: [deDeep_Cave]; Value: 120;),
    // Jade Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1;
    MaxDurability: 0; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 325; Color: clWhite; Deep: [deBlood_Cave]; Value: 130;),
    // Hallowed Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1;
    MaxDurability: 0; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 350; Color: clWhite; Deep: [deBlood_Cave]; Value: 140;),
    // Coruscating Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1;
    MaxDurability: 0; Level: 8; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 375; Color: clWhite; Deep: [deBlood_Cave]; Value: 150;),
    // Sanctified Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1;
    MaxDurability: 0; Level: 8; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 400; Color: clWhite; Deep: [deBlood_Cave]; Value: 160;),
    // Divine Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1;
    MaxDurability: 0; Level: 9; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 425; Color: clWhite; Deep: [deDrom]; Value: 170;),
    // Gold Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1;
    MaxDurability: 0; Level: 9; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 450; Color: clWhite; Deep: [deDrom]; Value: 180;),
    // Diamond Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1;
    MaxDurability: 0; Level: 10; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 475; Color: clWhite; Deep: [deDrom]; Value: 190;),
    // Eternal Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1;
    MaxDurability: 0; Level: 10; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 500; Color: clWhite; Deep: [deDrom]; Value: 200;),

    // Potion of Minor Healing
    (Symbol: '!'; ItemType: itPotion; SlotType: stNone; MaxStack: 8;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 50; Color: clLightestRed; Deep: [deDark_Wood .. deGray_Cave];
    Effects: [efLife]; Value: 50;),
    // Potion of Lesser Healing
    (Symbol: '!'; ItemType: itPotion; SlotType: stNone; MaxStack: 8;
    MaxDurability: 0; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 100; Color: clLightRed; Deep: [deGray_Cave .. deDeep_Cave];
    Effects: [efLife]; Value: 100;),
    // Potion of Greater Healing
    (Symbol: '!'; ItemType: itPotion; SlotType: stNone; MaxStack: 8;
    MaxDurability: 0; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 150; Color: clRed; Deep: [deDeep_Cave .. deBlood_Cave];
    Effects: [efLife]; Value: 150;),
    // Potion of Full Healing
    (Symbol: '!'; ItemType: itPotion; SlotType: stNone; MaxStack: 8;
    MaxDurability: 0; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 200; Color: clDarkRed; Deep: [deBlood_Cave .. deDrom];
    Effects: [efLife]; Value: 200;),

    // Potion of Minor Mana
    (Symbol: '!'; ItemType: itPotion; SlotType: stNone; MaxStack: 8;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 40; Color: clLightestBlue; Deep: [deDark_Wood .. deGray_Cave];
    Effects: [efMana]; Value: 60;),
    // Potion of Lesser Mana
    (Symbol: '!'; ItemType: itPotion; SlotType: stNone; MaxStack: 8;
    MaxDurability: 0; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 80; Color: clLightBlue; Deep: [deGray_Cave .. deDeep_Cave];
    Effects: [efMana]; Value: 120;),
    // Potion of Greater Mana
    (Symbol: '!'; ItemType: itPotion; SlotType: stNone; MaxStack: 8;
    MaxDurability: 0; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 120; Color: clBlue; Deep: [deDeep_Cave .. deBlood_Cave];
    Effects: [efMana]; Value: 180;),
    // Potion of Full Mana
    (Symbol: '!'; ItemType: itPotion; SlotType: stNone; MaxStack: 8;
    MaxDurability: 0; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 160; Color: clDarkBlue; Deep: [deBlood_Cave .. deDrom];
    Effects: [efMana]; Value: 240;),

    // Stone of
    (Symbol: '8'; ItemType: itStone; SlotType: stNone; MaxStack: 32;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 25; Color: clRed; Deep: [deDark_Wood .. deDrom]; Effects: [efLife];
    Value: 25;),
    // Stone of
    (Symbol: '8'; ItemType: itStone; SlotType: stNone; MaxStack: 32;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 20; Color: clBlue; Deep: [deDark_Wood .. deDrom]; Effects: [efMana];
    Value: 25;),
    // Stone of
    (Symbol: '8'; ItemType: itStone; SlotType: stNone; MaxStack: 32;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 25; Color: clYellow; Deep: [deDark_Wood .. deDrom];
    Effects: [efLife, efMana]; Value: 25;),

    // Scroll of Minor healing
    (Symbol: '?'; ItemType: itScroll; SlotType: stNone; MaxStack: 16;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 40; Color: clLightestBlue; Deep: [deDark_Wood .. deDeep_Cave];
    Effects: [efLife]; Value: 50; ManaCost: 20;),
    // Scroll of Lesser Healing
    (Symbol: '?'; ItemType: itScroll; SlotType: stNone; MaxStack: 16;
    MaxDurability: 0; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 80; Color: clLightBlue; Deep: [deGray_Cave .. deBlood_Cave];
    Effects: [efLife]; Value: 100; ManaCost: 30;),
    // Scroll of Greater Healing
    (Symbol: '?'; ItemType: itScroll; SlotType: stNone; MaxStack: 16;
    MaxDurability: 0; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 160; Color: clBlue; Deep: [deDeep_Cave .. deDrom]; Effects: [efLife];
    Value: 200; ManaCost: 40;),
    // Scroll of Full Healing
    (Symbol: '?'; ItemType: itScroll; SlotType: stNone; MaxStack: 16;
    MaxDurability: 0; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 300; Color: clDarkBlue; Deep: [deBlood_Cave .. deDrom];
    Effects: [efLife]; Value: 250; ManaCost: 50;),

    // Scroll of Hunger
    (Symbol: '?'; ItemType: itScroll; SlotType: stNone; MaxStack: 16;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 100; Color: clDarkYellow; Deep: [deDark_Wood .. deDrom];
    Effects: [efFood]; Value: 350; ManaCost: 25;),

    // Scroll of Sidestepping
    (Symbol: '?'; ItemType: itScroll; SlotType: stNone; MaxStack: 16;
    MaxDurability: 0; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 750; Color: clLightRed; Deep: [deDark_Wood .. deDeep_Cave];
    Effects: [efTeleportation]; Value: 3; ManaCost: 50;),
    // Scroll of Phasing
    (Symbol: '?'; ItemType: itScroll; SlotType: stNone; MaxStack: 16;
    MaxDurability: 0; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1000; Color: clRed; Deep: [deGray_Cave .. deBlood_Cave];
    Effects: [efTeleportation]; Value: 6; ManaCost: 100;),
    // Scroll of Teleportation
    (Symbol: '?'; ItemType: itScroll; SlotType: stNone; MaxStack: 16;
    MaxDurability: 0; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1250; Color: clDarkRed; Deep: [deDeep_Cave .. deDrom];
    Effects: [efTeleportation]; Value: 10; ManaCost: 150;),
    // Scroll of Disappearing
    (Symbol: '?'; ItemType: itScroll; SlotType: stNone; MaxStack: 16;
    MaxDurability: 0; Level: 8; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1500; Color: clDarkRed; Deep: [deBlood_Cave .. deDrom];
    Effects: [efTeleportation]; Value: 15; ManaCost: 200;),

    // Scroll of Town Portal
    (Symbol: '?'; ItemType: itScroll; SlotType: stNone; MaxStack: 16;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 75; Color: clLightGreen; Deep: [deDark_Wood .. deDrom];
    Effects: [efTownPortal]; Value: 0; ManaCost: 20;),
    // Scroll of Bloodlust
    (Symbol: '?'; ItemType: itScroll; SlotType: stNone; MaxStack: 16;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 100; Color: clLightRed; Deep: [deDark_Wood .. deDrom];
    Effects: [efBloodlust]; Value: 10; ManaCost: 25;),
    // Scroll of Identify
    (Symbol: '?'; ItemType: itScroll; SlotType: stNone; MaxStack: 16;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 35; Color: clLightYellow; Deep: [deDark_Wood .. deDrom];
    Effects: [efIdentification]; Value: 1; ManaCost: 5;),
    // Scroll of Full Identify
    (Symbol: '?'; ItemType: itScroll; SlotType: stNone; MaxStack: 16;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 75; Color: clLightYellow; Deep: [deDark_Wood .. deDrom];
    Effects: [efAllIdentification]; Value: 1; ManaCost: 15;),
    // Scroll of Enchant Item
    (Symbol: '?'; ItemType: itScroll; SlotType: stNone; MaxStack: 16;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 750; Color: clLightBlue; Deep: [deDark_Wood .. deDrom];
    Effects: [efEnchantItem]; Value: 1; ManaCost: 75; Rare: True;),

    // Rune of minor healing
    (Symbol: '*'; ItemType: itRune; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1000; Color: clLightestRed; Deep: [deDark_Wood .. deGray_Cave];
    Effects: [efLife]; Value: 75; ManaCost: 20; Rare: True;),
    // Rune of lesser healing
    (Symbol: '*'; ItemType: itRune; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1500; Color: clLightRed; Deep: [deGray_Cave .. deDeep_Cave];
    Effects: [efLife]; Value: 150; ManaCost: 30; Rare: True;),
    // Rune of greater healing
    (Symbol: '*'; ItemType: itRune; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 2000; Color: clRed; Deep: [deDeep_Cave .. deBlood_Cave];
    Effects: [efLife]; Value: 250; ManaCost: 40; Rare: True;),
    // Rune of full healing
    (Symbol: '*'; ItemType: itRune; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 9; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 2500; Color: clDarkRed; Deep: [deBlood_Cave .. deDrom];
    Effects: [efLife]; Value: 250; ManaCost: 50; Rare: True;),

    // Rune of teleportation
    (Symbol: '*'; ItemType: itRune; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 7000; Color: clDarkRed; Deep: [deDeep_Cave .. deBlood_Cave];
    Effects: [efTeleportation]; Value: 10; ManaCost: 150; Rare: True;),
    // Rune of town portal
    (Symbol: '*'; ItemType: itRune; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 4800; Color: clLightGreen; Deep: [deDark_Wood .. deDrom];
    Effects: [efTownPortal]; Value: 0; ManaCost: 50; Rare: True;),

    // Bread ration
    (Symbol: ':'; ItemType: itFood; SlotType: stNone; MaxStack: 16;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 200; Color: clYellow; Deep: [deDark_Wood .. deDrom];
    Effects: [efFood]; Value: 600;),
    // Valley root
    (Symbol: ':'; ItemType: itPlant; SlotType: stNone; MaxStack: 16;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 100; Color: clLightestYellow; Deep: [deDark_Wood .. deDrom];
    Effects: [efFood]; Value: 300;),
    // Rat pod
    (Symbol: ':'; ItemType: itPlant; SlotType: stNone; MaxStack: 16;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 150; Color: clLightestGreen; Deep: [deDark_Wood .. deDrom];
    Effects: [efFood]; Value: 450;),
    // Kobold bulb
    (Symbol: ':'; ItemType: itPlant; SlotType: stNone; MaxStack: 16;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 50; Color: clLightestGreen; Deep: [deDark_Wood .. deDrom];
    Effects: [efFood]; Value: 150;),
    // Hunk of Meat
    (Symbol: ':'; ItemType: itFood; SlotType: stNone; MaxStack: 16;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 300; Color: clLightestYellow; Deep: [deDark_Wood .. deDrom];
    Effects: [efFood]; Value: 900;),

    // Key
    (Symbol: '`'; ItemType: itKey; SlotType: stNone; MaxStack: 16;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 50; Color: clYellow; Deep: [deDark_Wood .. deDrom];),

    // Torch
    (Symbol: 'i'; ItemType: itTorch; SlotType: stTorch; MaxStack: 1;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 60; Color: clLighterRed; Deep: [deDark_Wood .. deDrom]; Value: 100;),
    // Oil Lamp
    (Symbol: 'O'; ItemType: itTorch; SlotType: stTorch; MaxStack: 1;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 90; Color: clLighterYellow; Deep: [deDark_Wood .. deDrom];
    Value: 150;),

    // Light Orb
    (Symbol: 'o'; ItemType: itOrb; SlotType: stNone; MaxStack: 10;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 75; Color: clLightYellow; Deep: [deDark_Wood .. deDrom];
    Effects: [efLight]; Value: 150;),
    // Life Orb
    (Symbol: 'o'; ItemType: itOrb; SlotType: stNone; MaxStack: 10;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 55; Color: clLightRed; Deep: [deDark_Wood .. deDrom];
    Effects: [efLife, efCurePoison]; Value: 45;),
    // Mana Orb
    (Symbol: 'o'; ItemType: itOrb; SlotType: stNone; MaxStack: 10;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 25; Color: clLightBlue; Deep: [deDark_Wood .. deDrom];
    Effects: [efMana]; Value: 35;),

    // Healing Herb
    (Symbol: ':'; ItemType: itPlant; SlotType: stNone; MaxStack: 16;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 15; Color: clYellow; Deep: [deDark_Wood .. deDrom];
    Effects: [efLife, efFood]; Value: 25;),

    // Ruby #1
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 750; Color: clRed; Deep: [deDark_Wood]; Effects: [efCraftStr];
    Value: 0; Rare: True;),
    // Ruby #2
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1000; Color: clRed; Deep: [deDark_Wood .. deGray_Cave];
    Effects: [efCraftStr]; Value: 1; Rare: True;),
    // Ruby #3
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1250; Color: clRed; Deep: [deGray_Cave .. deDeep_Cave];
    Effects: [efCraftStr]; Value: 2; Rare: True;),
    // Ruby #4
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1500; Color: clRed; Deep: [deDeep_Cave .. deBlood_Cave];
    Effects: [efCraftStr]; Value: 3; Rare: True;),
    // Ruby #5
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1750; Color: clRed; Deep: [deBlood_Cave .. deDrom];
    Effects: [efCraftStr]; Value: 4; Rare: True;),
    // Ruby #6
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 2000; Color: clRed; Deep: [deBlood_Cave .. deDrom];
    Effects: [efCraftStr]; Value: 5; Rare: True;),
    // Ruby #7
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 2500; Color: clRed; Deep: [deDrom]; Effects: [efCraftStr]; Value: 6;
    Rare: True;),

    // Topaz #1
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 750; Color: clYellow; Deep: [deDark_Wood]; Effects: [efCraftDex];
    Value: 0; Rare: True;),
    // Topaz #2
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1000; Color: clYellow; Deep: [deDark_Wood .. deGray_Cave];
    Effects: [efCraftDex]; Value: 1; Rare: True;),
    // Topaz #3
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1250; Color: clYellow; Deep: [deGray_Cave .. deDeep_Cave];
    Effects: [efCraftDex]; Value: 2; Rare: True;),
    // Topaz #4
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1500; Color: clYellow; Deep: [deDeep_Cave .. deBlood_Cave];
    Effects: [efCraftDex]; Value: 3; Rare: True;),
    // Topaz #5
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1750; Color: clYellow; Deep: [deBlood_Cave .. deDrom];
    Effects: [efCraftDex]; Value: 4; Rare: True;),
    // Topaz #6
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 2000; Color: clYellow; Deep: [deBlood_Cave .. deDrom];
    Effects: [efCraftDex]; Value: 5; Rare: True;),
    // Topaz #7
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 2500; Color: clYellow; Deep: [deDrom]; Effects: [efCraftDex];
    Value: 6; Rare: True;),

    // Sapphire #1
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 750; Color: clBlue; Deep: [deDark_Wood]; Effects: [efCraftWil];
    Value: 0; Rare: True;),
    // Sapphire #2
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1000; Color: clBlue; Deep: [deDark_Wood .. deGray_Cave];
    Effects: [efCraftWil]; Value: 1; Rare: True;),
    // Sapphire #3
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1250; Color: clBlue; Deep: [deGray_Cave .. deDeep_Cave];
    Effects: [efCraftWil]; Value: 2; Rare: True;),
    // Sapphire #4
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1500; Color: clBlue; Deep: [deDeep_Cave .. deBlood_Cave];
    Effects: [efCraftWil]; Value: 3; Rare: True;),
    // Sapphire #5
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1750; Color: clBlue; Deep: [deBlood_Cave .. deDrom];
    Effects: [efCraftWil]; Value: 4; Rare: True;),
    // Sapphire #6
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 2000; Color: clBlue; Deep: [deBlood_Cave .. deDrom];
    Effects: [efCraftWil]; Value: 5; Rare: True;),
    // Sapphire #7
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 2500; Color: clBlue; Deep: [deDrom]; Effects: [efCraftWil]; Value: 6;
    Rare: True;),

    // Emerald #1
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 750; Color: clGreen; Deep: [deDark_Wood]; Effects: [efCraftPer];
    Value: 0; Rare: True;),
    // Emerald #2
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1000; Color: clGreen; Deep: [deDark_Wood .. deGray_Cave];
    Effects: [efCraftPer]; Value: 1; Rare: True;),
    // Emerald #3
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1250; Color: clGreen; Deep: [deGray_Cave .. deDeep_Cave];
    Effects: [efCraftPer]; Value: 2; Rare: True;),
    // Emerald #4
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1500; Color: clGreen; Deep: [deDeep_Cave .. deBlood_Cave];
    Effects: [efCraftPer]; Value: 3; Rare: True;),
    // Emerald #5
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1750; Color: clGreen; Deep: [deBlood_Cave .. deDrom];
    Effects: [efCraftPer]; Value: 4; Rare: True;),
    // Emerald #6
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 2000; Color: clGreen; Deep: [deBlood_Cave .. deDrom];
    Effects: [efCraftPer]; Value: 5; Rare: True;),
    // Emerald #7
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 2500; Color: clGreen; Deep: [deDrom]; Effects: [efCraftPer];
    Value: 6; Rare: True;),

    // Diamond #1
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1000; Color: clWhite; Deep: [deDark_Wood]; Effects: [efCraftAtr];
    Value: 0; Rare: True;),
    // Diamond #2
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1500; Color: clWhite; Deep: [deDark_Wood .. deGray_Cave];
    Effects: [efCraftAtr]; Value: 1; Rare: True;),
    // Diamond #3
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 2000; Color: clWhite; Deep: [deGray_Cave .. deDeep_Cave];
    Effects: [efCraftAtr]; Value: 2; Rare: True;),
    // Diamond #4
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 2500; Color: clWhite; Deep: [deDeep_Cave .. deBlood_Cave];
    Effects: [efCraftAtr]; Value: 3; Rare: True;),
    // Diamond #5
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 3000; Color: clWhite; Deep: [deBlood_Cave .. deDrom];
    Effects: [efCraftAtr]; Value: 4; Rare: True;),
    // Diamond #6
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 3500; Color: clWhite; Deep: [deBlood_Cave .. deDrom];
    Effects: [efCraftAtr]; Value: 5; Rare: True;),
    // Diamond #7
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3;
    MaxDurability: 0; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 4000; Color: clWhite; Deep: [deDrom]; Effects: [efCraftAtr];
    Rare: True;),

    // Ring #1
    (Symbol: '='; ItemType: itRing; SlotType: stFinger; MaxStack: 1;
    MaxDurability: 15; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1000; Color: clYellow; Deep: [deDark_Wood];),
    // Ring #2
    (Symbol: '='; ItemType: itRing; SlotType: stFinger; MaxStack: 1;
    MaxDurability: 20; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1250; Color: clYellow; Deep: [deDark_Wood];),
    // Ring #3
    (Symbol: '='; ItemType: itRing; SlotType: stFinger; MaxStack: 1;
    MaxDurability: 25; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1500; Color: clYellow; Deep: [deGray_Cave];),
    // Ring #4
    (Symbol: '='; ItemType: itRing; SlotType: stFinger; MaxStack: 1;
    MaxDurability: 30; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1750; Color: clYellow; Deep: [deGray_Cave];),
    // Ring #5
    (Symbol: '='; ItemType: itRing; SlotType: stFinger; MaxStack: 1;
    MaxDurability: 35; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 2000; Color: clYellow; Deep: [deDeep_Cave];),
    // Ring #6
    (Symbol: '='; ItemType: itRing; SlotType: stFinger; MaxStack: 1;
    MaxDurability: 40; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1250; Color: clYellow; Deep: [deDeep_Cave];),
    // Ring #7
    (Symbol: '='; ItemType: itRing; SlotType: stFinger; MaxStack: 1;
    MaxDurability: 45; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1500; Color: clYellow; Deep: [deBlood_Cave];),
    // Ring #8
    (Symbol: '='; ItemType: itRing; SlotType: stFinger; MaxStack: 1;
    MaxDurability: 50; Level: 8; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1750; Color: clYellow; Deep: [deBlood_Cave];),
    // Ring #9
    (Symbol: '='; ItemType: itRing; SlotType: stFinger; MaxStack: 1;
    MaxDurability: 55; Level: 9; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 2000; Color: clYellow; Deep: [deDrom];),
    // Ring #10
    (Symbol: '='; ItemType: itRing; SlotType: stFinger; MaxStack: 1;
    MaxDurability: 60; Level: 10; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 2500; Color: clYellow; Deep: [deDrom];),

    // Amulet #1
    (Symbol: ''''; ItemType: itAmulet; SlotType: stNeck; MaxStack: 1;
    MaxDurability: 35; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1200; Color: clYellow; Deep: [deDark_Wood];),
    // Amulet #2
    (Symbol: ''''; ItemType: itAmulet; SlotType: stNeck; MaxStack: 1;
    MaxDurability: 45; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1400; Color: clYellow; Deep: [deDark_Wood];),
    // Amulet #3
    (Symbol: ''''; ItemType: itAmulet; SlotType: stNeck; MaxStack: 1;
    MaxDurability: 55; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1600; Color: clYellow; Deep: [deGray_Cave];),
    // Amulet #4
    (Symbol: ''''; ItemType: itAmulet; SlotType: stNeck; MaxStack: 1;
    MaxDurability: 65; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1800; Color: clYellow; Deep: [deGray_Cave];),
    // Amulet #5
    (Symbol: ''''; ItemType: itAmulet; SlotType: stNeck; MaxStack: 1;
    MaxDurability: 75; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 2000; Color: clYellow; Deep: [deDeep_Cave];),
    // Amulet #6
    (Symbol: ''''; ItemType: itAmulet; SlotType: stNeck; MaxStack: 1;
    MaxDurability: 85; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 2200; Color: clYellow; Deep: [deDeep_Cave];),
    // Amulet #7
    (Symbol: ''''; ItemType: itAmulet; SlotType: stNeck; MaxStack: 1;
    MaxDurability: 95; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 2400; Color: clYellow; Deep: [deBlood_Cave];),
    // Amulet #8
    (Symbol: ''''; ItemType: itAmulet; SlotType: stNeck; MaxStack: 1;
    MaxDurability: 105; Level: 8; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 2600; Color: clYellow; Deep: [deBlood_Cave];),
    // Amulet #9
    (Symbol: ''''; ItemType: itAmulet; SlotType: stNeck; MaxStack: 1;
    MaxDurability: 115; Level: 9; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 2800; Color: clYellow; Deep: [deDrom];),
    // Amulet #10
    (Symbol: ''''; ItemType: itAmulet; SlotType: stNeck; MaxStack: 1;
    MaxDurability: 125; Level: 10; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 3000; Color: clYellow; Deep: [deDrom];),

    // Talisman #1
    (Symbol: ''''; ItemType: itTalisman; SlotType: stNeck; MaxStack: 1;
    MaxDurability: 40; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1000; Color: clYellow; Deep: [deDark_Wood];),
    // Talisman #2
    (Symbol: ''''; ItemType: itTalisman; SlotType: stNeck; MaxStack: 1;
    MaxDurability: 50; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1300; Color: clYellow; Deep: [deDark_Wood];),
    // Talisman #3
    (Symbol: ''''; ItemType: itTalisman; SlotType: stNeck; MaxStack: 1;
    MaxDurability: 60; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1600; Color: clYellow; Deep: [deGray_Cave];),
    // Talisman #4
    (Symbol: ''''; ItemType: itTalisman; SlotType: stNeck; MaxStack: 1;
    MaxDurability: 70; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1900; Color: clYellow; Deep: [deGray_Cave];),
    // Talisman #5
    (Symbol: ''''; ItemType: itTalisman; SlotType: stNeck; MaxStack: 1;
    MaxDurability: 80; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 2200; Color: clYellow; Deep: [deDeep_Cave];),
    // Talisman #6
    (Symbol: ''''; ItemType: itTalisman; SlotType: stNeck; MaxStack: 1;
    MaxDurability: 90; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 2500; Color: clYellow; Deep: [deDeep_Cave];),
    // Talisman #7
    (Symbol: ''''; ItemType: itTalisman; SlotType: stNeck; MaxStack: 1;
    MaxDurability: 100; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 2800; Color: clYellow; Deep: [deBlood_Cave];),
    // Talisman #8
    (Symbol: ''''; ItemType: itTalisman; SlotType: stNeck; MaxStack: 1;
    MaxDurability: 110; Level: 8; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 3100; Color: clYellow; Deep: [deBlood_Cave];),
    // Talisman #9
    (Symbol: ''''; ItemType: itTalisman; SlotType: stNeck; MaxStack: 1;
    MaxDurability: 120; Level: 9; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 3500; Color: clYellow; Deep: [deDrom];),
    // Talisman #10
    (Symbol: ''''; ItemType: itTalisman; SlotType: stNeck; MaxStack: 1;
    MaxDurability: 130; Level: 10; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 4000; Color: clYellow; Deep: [deDrom];),

    // Wand #1
    (Symbol: '-'; ItemType: itWand; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 10; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 1; Max: 2;); MaxDamage: (Min: 3; Max: 4;));
    Price: 550; Color: clDarkBlue; Deep: [deDark_Wood];),
    // Wand #2
    (Symbol: '-'; ItemType: itWand; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 12; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 5; Max: 6;); MaxDamage: (Min: 7; Max: 8;));
    Price: 600; Color: clDarkBlue; Deep: [deDark_Wood];),
    // Wand #3
    (Symbol: '-'; ItemType: itWand; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 14; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 11; Max: 12;); MaxDamage: (Min: 13; Max: 14;));
    Price: 650; Color: clDarkBlue; Deep: [deGray_Cave];),
    // Wand #4
    (Symbol: '-'; ItemType: itWand; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 16; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 15; Max: 16;); MaxDamage: (Min: 17; Max: 18;));
    Price: 700; Color: clDarkBlue; Deep: [deGray_Cave];),
    // Wand #5
    (Symbol: '-'; ItemType: itWand; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 18; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 21; Max: 22;); MaxDamage: (Min: 23; Max: 24;));
    Price: 750; Color: clDarkBlue; Deep: [deDeep_Cave];),
    // Wand #6
    (Symbol: '-'; ItemType: itWand; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 20; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 25; Max: 26;); MaxDamage: (Min: 27; Max: 28;));
    Price: 800; Color: clDarkBlue; Deep: [deDeep_Cave];),
    // Wand #7
    (Symbol: '-'; ItemType: itWand; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 22; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 31; Max: 32;); MaxDamage: (Min: 33; Max: 34;));
    Price: 850; Color: clDarkBlue; Deep: [deBlood_Cave];),
    // Wand #8
    (Symbol: '-'; ItemType: itWand; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 24; Level: 8; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 35; Max: 36;); MaxDamage: (Min: 37; Max: 38;));
    Price: 900; Color: clDarkBlue; Deep: [deBlood_Cave];),
    // Wand #9
    (Symbol: '-'; ItemType: itWand; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 27; Level: 9; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 41; Max: 42;); MaxDamage: (Min: 43; Max: 44;));
    Price: 950; Color: clDarkBlue; Deep: [deDrom];),
    // Wand #10
    (Symbol: '-'; ItemType: itWand; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 30; Level: 10; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 45; Max: 46;); MaxDamage: (Min: 47; Max: 48;));
    Price: 1000; Color: clDarkBlue; Deep: [deDrom];),

    // Dagger #1
    (Symbol: '-'; ItemType: itDagger; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 13; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 1; Max: 2;); MaxDamage: (Min: 3; Max: 4;));
    Price: 125; Color: clDarkBlue; Deep: [deDark_Wood];),
    // Dagger #2
    (Symbol: '-'; ItemType: itDagger; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 16; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 5; Max: 6;); MaxDamage: (Min: 7; Max: 8;));
    Price: 150; Color: clDarkBlue; Deep: [deDark_Wood];),
    // Dagger #3
    (Symbol: '-'; ItemType: itDagger; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 19; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 11; Max: 12;); MaxDamage: (Min: 13; Max: 14;));
    Price: 175; Color: clDarkBlue; Deep: [deGray_Cave];),
    // Dagger #4
    (Symbol: '-'; ItemType: itDagger; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 22; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 15; Max: 16;); MaxDamage: (Min: 17; Max: 18;));
    Price: 200; Color: clDarkBlue; Deep: [deGray_Cave];),
    // Dagger #5
    (Symbol: '-'; ItemType: itDagger; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 25; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 21; Max: 22;); MaxDamage: (Min: 23; Max: 24;));
    Price: 225; Color: clDarkBlue; Deep: [deDeep_Cave];),
    // Dagger #6
    (Symbol: '-'; ItemType: itDagger; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 28; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 25; Max: 26;); MaxDamage: (Min: 27; Max: 28;));
    Price: 250; Color: clDarkBlue; Deep: [deDeep_Cave];),
    // Dagger #7
    (Symbol: '-'; ItemType: itDagger; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 31; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 31; Max: 32;); MaxDamage: (Min: 33; Max: 34;));
    Price: 275; Color: clDarkBlue; Deep: [deBlood_Cave];),
    // Dagger #8
    (Symbol: '-'; ItemType: itDagger; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 34; Level: 8; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 35; Max: 36;); MaxDamage: (Min: 37; Max: 38;));
    Price: 300; Color: clDarkBlue; Deep: [deBlood_Cave];),
    // Dagger #9
    (Symbol: '-'; ItemType: itDagger; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 37; Level: 9; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 41; Max: 42;); MaxDamage: (Min: 43; Max: 44;));
    Price: 325; Color: clDarkBlue; Deep: [deDrom];),
    // Dagger #10
    (Symbol: '-'; ItemType: itDagger; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 40; Level: 10; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 45; Max: 46;); MaxDamage: (Min: 47; Max: 48;));
    Price: 350; Color: clDarkBlue; Deep: [deDrom];),

    // Bow #1
    (Symbol: ')'; ItemType: itBow; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 35; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 1; Max: 2;); MaxDamage: (Min: 3; Max: 4;));
    Price: 200; Color: clDarkBlue; Deep: [deDark_Wood];),
    // Bow #2
    (Symbol: ')'; ItemType: itBow; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 40; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 5; Max: 6;); MaxDamage: (Min: 7; Max: 8;));
    Price: 250; Color: clDarkBlue; Deep: [deDark_Wood];),
    // Bow #3
    (Symbol: ')'; ItemType: itBow; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 45; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 11; Max: 12;); MaxDamage: (Min: 13; Max: 14;));
    Price: 300; Color: clDarkBlue; Deep: [deGray_Cave];),
    // Bow #4
    (Symbol: ')'; ItemType: itBow; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 50; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 15; Max: 16;); MaxDamage: (Min: 17; Max: 18;));
    Price: 350; Color: clDarkBlue; Deep: [deGray_Cave];),
    // Bow #5
    (Symbol: ')'; ItemType: itBow; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 55; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 21; Max: 22;); MaxDamage: (Min: 23; Max: 24;));
    Price: 400; Color: clDarkBlue; Deep: [deDeep_Cave];),
    // Bow #6
    (Symbol: ')'; ItemType: itBow; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 60; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 25; Max: 26;); MaxDamage: (Min: 27; Max: 28;));
    Price: 450; Color: clDarkBlue; Deep: [deDeep_Cave];),
    // Bow #7
    (Symbol: ')'; ItemType: itBow; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 65; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 31; Max: 32;); MaxDamage: (Min: 33; Max: 34;));
    Price: 500; Color: clDarkBlue; Deep: [deBlood_Cave];),
    // Bow #8
    (Symbol: ')'; ItemType: itBow; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 70; Level: 8; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 35; Max: 36;); MaxDamage: (Min: 37; Max: 38;));
    Price: 550; Color: clDarkBlue; Deep: [deBlood_Cave];),
    // Bow #9
    (Symbol: ')'; ItemType: itBow; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 75; Level: 9; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 41; Max: 42;); MaxDamage: (Min: 43; Max: 44;));
    Price: 600; Color: clDarkBlue; Deep: [deDrom];),
    // Bow #10
    (Symbol: ')'; ItemType: itBow; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 80; Level: 10; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 45; Max: 46;); MaxDamage: (Min: 47; Max: 48;));
    Price: 700; Color: clDarkBlue; Deep: [deDrom];),

    /// / == Dark _Wood == ////

    // Cap
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1;
    MaxDurability: 15; Level: 1; Defense: (Min: 1; Max: 2);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 100; Color: clWhite; Deep: [deDark_Wood];),
    // War Cap
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1;
    MaxDurability: 20; Level: 2; Defense: (Min: 3; Max: 4);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 150; Color: clWhite; Deep: [deDark_Wood];),
    // Hood
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1;
    MaxDurability: 10; Level: 1; Defense: (Min: 1; Max: 2);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 200; Color: clLightestBlue; Deep: [deDark_Wood];),
    // Red Hat
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1;
    MaxDurability: 12; Level: 2; Defense: (Min: 2; Max: 3);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 300; Color: clLightRed; Deep: [deDark_Wood];),
    // Quilted Armor
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1;
    MaxDurability: 25; Level: 1; Defense: (Min: 3; Max: 6);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 300; Color: clWhite; Deep: [deDark_Wood];),
    // Leather Armor
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1;
    MaxDurability: 50; Level: 2; Defense: (Min: 8; Max: 11);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 500; Color: clWhite; Deep: [deDark_Wood];),
    // Light Clothes
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1;
    MaxDurability: 20; Level: 1; Defense: (Min: 1; Max: 2);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 600; Color: clLightestBlue; Deep: [deDark_Wood];),
    // Leather Apron
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1;
    MaxDurability: 40; Level: 2; Defense: (Min: 3; Max: 4);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 800; Color: clLightestBlue; Deep: [deDark_Wood];),

    // Leather Gloves
    (Symbol: '%'; ItemType: itHands; SlotType: stHands; MaxStack: 1;
    MaxDurability: 10; Level: 1; Defense: (Min: 1; Max: 2);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 100; Color: clWhite; Deep: [deDark_Wood];),
    // Hide Gloves
    (Symbol: '%'; ItemType: itHands; SlotType: stHands; MaxStack: 1;
    MaxDurability: 15; Level: 2; Defense: (Min: 3; Max: 4);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 150; Color: clWhite; Deep: [deDark_Wood];),

    // Shoes
    (Symbol: ';'; ItemType: itFeet; SlotType: stFeet; MaxStack: 1;
    MaxDurability: 15; Level: 1; Defense: (Min: 1; Max: 3);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 200; Color: clWhite; Deep: [deDark_Wood];),
    // Leather Boots
    (Symbol: ';'; ItemType: itFeet; SlotType: stFeet; MaxStack: 1;
    MaxDurability: 20; Level: 2; Defense: (Min: 4; Max: 6);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 250; Color: clWhite; Deep: [deDark_Wood];),

    // Buckler
    (Symbol: '+'; ItemType: itShield; SlotType: stOffHand; MaxStack: 1;
    MaxDurability: 25; Level: 1; Defense: (Min: 3; Max: 6);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 120; Color: clLightBlue; Deep: [deDark_Wood];),
    // Targe Shield
    (Symbol: '+'; ItemType: itShield; SlotType: stOffHand; MaxStack: 1;
    MaxDurability: 30; Level: 2; Defense: (Min: 7; Max: 10);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 200; Color: clLightBlue; Deep: [deDark_Wood];),

    // Rusty Sword
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 30; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 3; Max: 5;); MaxDamage: (Min: 6; Max: 9;));
    Price: 185; Color: clDarkRed; Deep: [deDark_Wood];),
    // Short Sword
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 35; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 5; Max: 7;); MaxDamage: (Min: 10; Max: 12;));
    Price: 210; Color: clWhite; Deep: [deDark_Wood];),
    // Hatchet
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 30; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 3; Max: 5;); MaxDamage: (Min: 6; Max: 8;));
    Price: 165; Color: clDarkRed; Deep: [deDark_Wood];),
    // Battle Axe
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 35; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 7; Max: 9;); MaxDamage: (Min: 11; Max: 12;));
    Price: 195; Color: clDarkRed; Deep: [deDark_Wood];),
    // Short Spear
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 30; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 1; Max: 3;); MaxDamage: (Min: 4; Max: 5;));
    Price: 150; Color: clDarkRed; Deep: [deDark_Wood];),
    // Spear
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 35; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 2; Max: 4;); MaxDamage: (Min: 5; Max: 7;));
    Price: 180; Color: clDarkRed; Deep: [deDark_Wood];),
    // Slag Hammer
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 30; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 2; Max: 4;); MaxDamage: (Min: 5; Max: 6;));
    Price: 175; Color: clDarkRed; Deep: [deDark_Wood];),
    // Spiked Cudgel
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 35; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 3; Max: 5;); MaxDamage: (Min: 7; Max: 9;));
    Price: 220; Color: clDarkRed; Deep: [deDark_Wood];),
    // Quarterstaff
    (Symbol: '|'; ItemType: itStaff; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 12; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 1; Max: 2;); MaxDamage: (Min: 2; Max: 3;));
    Price: 300; Color: clDarkGreen; Deep: [deDark_Wood];),
    // Staff2
    (Symbol: '|'; ItemType: itStaff; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 15; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 2; Max: 3;); MaxDamage: (Min: 4; Max: 5;));
    Price: 400; Color: clDarkGreen; Deep: [deDark_Wood];),

    /// / == Gray Cave == ////

    // Helm
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1;
    MaxDurability: 25; Level: 3; Defense: (Min: 4; Max: 6);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 300; Color: clWhite; Deep: [deGray_Cave];),
    // Grand Helm
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1;
    MaxDurability: 30; Level: 4; Defense: (Min: 6; Max: 8);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 400; Color: clWhite; Deep: [deGray_Cave];),
    // Leather Cap
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1;
    MaxDurability: 15; Level: 3; Defense: (Min: 2; Max: 3);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 600; Color: clLightestBlue; Deep: [deGray_Cave];),
    // Mask
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1;
    MaxDurability: 18; Level: 4; Defense: (Min: 3; Max: 4);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 700; Color: clLightestBlue; Deep: [deGray_Cave];),
    // HardLeather Armor
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1;
    MaxDurability: 75; Level: 3; Defense: (Min: 12; Max: 15);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 700; Color: clWhite; Deep: [deGray_Cave];),
    // Battle Armor
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1;
    MaxDurability: 100; Level: 4; Defense: (Min: 17; Max: 20);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 900; Color: clWhite; Deep: [deGray_Cave];),
    // Fancy Clothes
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1;
    MaxDurability: 60; Level: 3; Defense: (Min: 5; Max: 6);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1000; Color: clLightestBlue; Deep: [deGray_Cave];),
    // Robe
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1;
    MaxDurability: 75; Level: 4; Defense: (Min: 7; Max: 8);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1300; Color: clLightestBlue; Deep: [deGray_Cave];),

    // Kobold Gloves
    (Symbol: '%'; ItemType: itHands; SlotType: stHands; MaxStack: 1;
    MaxDurability: 22; Level: 3; Defense: (Min: 5; Max: 6);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 200; Color: clWhite; Deep: [deGray_Cave];),
    // Chain Gloves
    (Symbol: '%'; ItemType: itHands; SlotType: stHands; MaxStack: 1;
    MaxDurability: 29; Level: 4; Defense: (Min: 7; Max: 8);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 250; Color: clWhite; Deep: [deGray_Cave];),

    // Mesh Boots
    (Symbol: ';'; ItemType: itFeet; SlotType: stFeet; MaxStack: 1;
    MaxDurability: 30; Level: 3; Defense: (Min: 7; Max: 9);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 300; Color: clWhite; Deep: [deGray_Cave];),
    // Heavy Boots
    (Symbol: ';'; ItemType: itFeet; SlotType: stFeet; MaxStack: 1;
    MaxDurability: 40; Level: 4; Defense: (Min: 10; Max: 12);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 400; Color: clWhite; Deep: [deGray_Cave];),

    // Small Shield
    (Symbol: '+'; ItemType: itShield; SlotType: stOffHand; MaxStack: 1;
    MaxDurability: 35; Level: 3; Defense: (Min: 10; Max: 12);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 275; Color: clLightBlue; Deep: [deGray_Cave];),
    // Kite Shield
    (Symbol: '+'; ItemType: itShield; SlotType: stOffHand; MaxStack: 1;
    MaxDurability: 40; Level: 4; Defense: (Min: 13; Max: 15);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 380; Color: clLightBlue; Deep: [deGray_Cave];),

    // Broad Sword
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 40; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 11; Max: 14;); MaxDamage: (Min: 19; Max: 21;));
    Price: 345; Color: clDarkRed; Deep: [deGray_Cave];),
    // Long Sword
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 45; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 14; Max: 18;); MaxDamage: (Min: 23; Max: 26;));
    Price: 385; Color: clDarkRed; Deep: [deGray_Cave];),
    // Meat Axe
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 40; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 12; Max: 14;); MaxDamage: (Min: 16; Max: 19;));
    Price: 330; Color: clDarkRed; Deep: [deGray_Cave];),
    // Flesh Tearer
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 45; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 16; Max: 18;); MaxDamage: (Min: 21; Max: 24;));
    Price: 355; Color: clDarkRed; Deep: [deGray_Cave];),
    // Javelin
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 40; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 4; Max: 6;); MaxDamage: (Min: 10; Max: 12;));
    Price: 320; Color: clDarkRed; Deep: [deGray_Cave];),
    // Fuscina
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 45; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 5; Max: 7;); MaxDamage: (Min: 16; Max: 19;));
    Price: 360; Color: clDarkRed; Deep: [deGray_Cave];),
    // Warhammer
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 40; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 6; Max: 9;); MaxDamage: (Min: 11; Max: 13;));
    Price: 345; Color: clDarkRed; Deep: [deGray_Cave];),
    // War Mace
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 45; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 8; Max: 10;); MaxDamage: (Min: 17; Max: 19;));
    Price: 410; Color: clDarkRed; Deep: [deGray_Cave];),
    // Staff3
    (Symbol: '|'; ItemType: itStaff; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 18; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 3; Max: 4;); MaxDamage: (Min: 4; Max: 5;));
    Price: 500; Color: clDarkGreen; Deep: [deGray_Cave];),
    // Staff4
    (Symbol: '|'; ItemType: itStaff; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 21; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 4; Max: 5;); MaxDamage: (Min: 6; Max: 7;));
    Price: 600; Color: clDarkGreen; Deep: [deGray_Cave];),

    /// / == Deep Cave == ////

    // Great Helm
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1;
    MaxDurability: 35; Level: 5; Defense: (Min: 8; Max: 10);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 500; Color: clWhite; Deep: [deDeep_Cave];),
    // Full Helm
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1;
    MaxDurability: 40; Level: 6; Defense: (Min: 10; Max: 12);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 750; Color: clWhite; Deep: [deDeep_Cave];),
    // Bone Helmet
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1;
    MaxDurability: 20; Level: 5; Defense: (Min: 4; Max: 5);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1000; Color: clLightestBlue; Deep: [deDeep_Cave];),
    // Wizard Hat
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1;
    MaxDurability: 25; Level: 6; Defense: (Min: 5; Max: 6);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1200; Color: clLightestBlue; Deep: [deDeep_Cave];),
    // Brigantine Armor
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1;
    MaxDurability: 125; Level: 5; Defense: (Min: 21; Max: 25);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1000; Color: clWhite; Deep: [deDeep_Cave];),
    // Ring Mail
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1;
    MaxDurability: 150; Level: 6; Defense: (Min: 26; Max: 30);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1200; Color: clWhite; Deep: [deDeep_Cave];),
    // Light Furs
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1;
    MaxDurability: 85; Level: 5; Defense: (Min: 9; Max: 10);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1500; Color: clLightestBlue; Deep: [deDeep_Cave];),
    // Clean Robe
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1;
    MaxDurability: 100; Level: 6; Defense: (Min: 11; Max: 12);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1700; Color: clLightestBlue; Deep: [deDeep_Cave];),

    // Etched Gloves
    (Symbol: '%'; ItemType: itHands; SlotType: stHands; MaxStack: 1;
    MaxDurability: 35; Level: 5; Defense: (Min: 9; Max: 10);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 300; Color: clWhite; Deep: [deDeep_Cave];),
    // Heavy Gloves
    (Symbol: '%'; ItemType: itHands; SlotType: stHands; MaxStack: 1;
    MaxDurability: 40; Level: 6; Defense: (Min: 11; Max: 12);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 350; Color: clWhite; Deep: [deDeep_Cave];),

    // Greaves
    (Symbol: ';'; ItemType: itFeet; SlotType: stFeet; MaxStack: 1;
    MaxDurability: 50; Level: 5; Defense: (Min: 13; Max: 15);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 500; Color: clWhite; Deep: [deDeep_Cave];),
    // Boneweave Boots
    (Symbol: ';'; ItemType: itFeet; SlotType: stFeet; MaxStack: 1;
    MaxDurability: 60; Level: 6; Defense: (Min: 16; Max: 18);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 650; Color: clWhite; Deep: [deDeep_Cave];),

    // Bone Shield
    (Symbol: '+'; ItemType: itShield; SlotType: stOffHand; MaxStack: 1;
    MaxDurability: 45; Level: 5; Defense: (Min: 16; Max: 18);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 450; Color: clLightBlue; Deep: [deDeep_Cave];),
    // Heater Shield
    (Symbol: '+'; ItemType: itShield; SlotType: stOffHand; MaxStack: 1;
    MaxDurability: 50; Level: 6; Defense: (Min: 19; Max: 21);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 590; Color: clLightBlue; Deep: [deDeep_Cave];),

    // Moon Blade
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 50; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 17; Max: 20;); MaxDamage: (Min: 27; Max: 31;));
    Price: 570; Color: clDarkRed; Deep: [deDeep_Cave];),
    // Scimitar
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 55; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 21; Max: 25;); MaxDamage: (Min: 35; Max: 38;));
    Price: 600; Color: clDarkRed; Deep: [deDeep_Cave];),
    // War Axe
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 50; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 21; Max: 24;); MaxDamage: (Min: 26; Max: 30;));
    Price: 560; Color: clDarkRed; Deep: [deDeep_Cave];),
    // Dark Axe
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 55; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 24; Max: 27;); MaxDamage: (Min: 30; Max: 33;));
    Price: 585; Color: clDarkRed; Deep: [deDeep_Cave];),
    // War Spear
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 50; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 6; Max: 10;); MaxDamage: (Min: 25; Max: 28;));
    Price: 540; Color: clDarkRed; Deep: [deDeep_Cave];),
    // Harpoon
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 55; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 7; Max: 11;); MaxDamage: (Min: 35; Max: 39;));
    Price: 575; Color: clDarkRed; Deep: [deDeep_Cave];),
    // Flanged Mace
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 50; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 11; Max: 14;); MaxDamage: (Min: 22; Max: 25;));
    Price: 590; Color: clDarkRed; Deep: [deDeep_Cave];),
    // War Gavel
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 55; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 15; Max: 18;); MaxDamage: (Min: 30; Max: 33;));
    Price: 650; Color: clDarkRed; Deep: [deDeep_Cave];),
    // Staff5
    (Symbol: '|'; ItemType: itStaff; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 24; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 5; Max: 6;); MaxDamage: (Min: 6; Max: 7;));
    Price: 700; Color: clDarkGreen; Deep: [deDeep_Cave];),
    // Staff6
    (Symbol: '|'; ItemType: itStaff; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 27; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 6; Max: 7;); MaxDamage: (Min: 8; Max: 9;));
    Price: 800; Color: clDarkGreen; Deep: [deDeep_Cave];),

    /// / == Blood Cave == ////

    // Horned Helmet
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1;
    MaxDurability: 45; Level: 7; Defense: (Min: 12; Max: 14);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 850; Color: clWhite; Deep: [deBlood_Cave];),
    // Spired Helm
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1;
    MaxDurability: 50; Level: 8; Defense: (Min: 14; Max: 16);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1000; Color: clWhite; Deep: [deBlood_Cave];),
    // Diadem
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1;
    MaxDurability: 30; Level: 7; Defense: (Min: 6; Max: 7);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1300; Color: clLightestBlue; Deep: [deBlood_Cave];),
    // Tiara
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1;
    MaxDurability: 35; Level: 8; Defense: (Min: 7; Max: 8);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1700; Color: clLightestBlue; Deep: [deBlood_Cave];),
    // Chain Mail
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1;
    MaxDurability: 175; Level: 7; Defense: (Min: 31; Max: 35);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1300; Color: clWhite; Deep: [deBlood_Cave];),
    // Scale Mail
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1;
    MaxDurability: 200; Level: 8; Defense: (Min: 36; Max: 40);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1600; Color: clWhite; Deep: [deBlood_Cave];),
    // Thick Furs
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1;
    MaxDurability: 120; Level: 7; Defense: (Min: 13; Max: 14);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1900; Color: clLightestBlue; Deep: [deBlood_Cave];),
    // Hard Robe
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1;
    MaxDurability: 150; Level: 8; Defense: (Min: 15; Max: 16);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 2100; Color: clLightestBlue; Deep: [deBlood_Cave];),

    // Battle Gauntlets
    (Symbol: '%'; ItemType: itHands; SlotType: stHands; MaxStack: 1;
    MaxDurability: 45; Level: 7; Defense: (Min: 13; Max: 14);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 400; Color: clWhite; Deep: [deBlood_Cave];),
    // War Gauntlets
    (Symbol: '%'; ItemType: itHands; SlotType: stHands; MaxStack: 1;
    MaxDurability: 50; Level: 8; Defense: (Min: 15; Max: 16);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 450; Color: clWhite; Deep: [deBlood_Cave];),

    // Chain Boots
    (Symbol: ';'; ItemType: itFeet; SlotType: stFeet; MaxStack: 1;
    MaxDurability: 70; Level: 7; Defense: (Min: 19; Max: 21);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 700; Color: clWhite; Deep: [deBlood_Cave];),
    // War Boots
    (Symbol: ';'; ItemType: itFeet; SlotType: stFeet; MaxStack: 1;
    MaxDurability: 80; Level: 8; Defense: (Min: 22; Max: 24);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 850; Color: clWhite; Deep: [deBlood_Cave];),

    // Heavy Shield
    (Symbol: '+'; ItemType: itShield; SlotType: stOffHand; MaxStack: 1;
    MaxDurability: 60; Level: 7; Defense: (Min: 22; Max: 24);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 740; Color: clLightBlue; Deep: [deBlood_Cave];),
    // Large Shield
    (Symbol: '+'; ItemType: itShield; SlotType: stOffHand; MaxStack: 1;
    MaxDurability: 75; Level: 8; Defense: (Min: 25; Max: 27);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 860; Color: clLightBlue; Deep: [deBlood_Cave];),

    // Bastard Sword
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 60; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 24; Max: 27;); MaxDamage: (Min: 39; Max: 43;));
    Price: 770; Color: clDarkRed; Deep: [deBlood_Cave];),
    // Great Sword
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 65; Level: 8; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 27; Max: 30;); MaxDamage: (Min: 44; Max: 48;));
    Price: 820; Color: clDarkRed; Deep: [deBlood_Cave];),
    // Berserker Axe
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 60; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 28; Max: 32;); MaxDamage: (Min: 36; Max: 38;));
    Price: 750; Color: clDarkRed; Deep: [deDeep_Cave];),
    // Marauder Axe
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 65; Level: 8; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 32; Max: 35;); MaxDamage: (Min: 41; Max: 45;));
    Price: 885; Color: clDarkRed; Deep: [deBlood_Cave];),
    // Silvan Whisper
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 60; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 8; Max: 11;); MaxDamage: (Min: 50; Max: 52;));
    Price: 720; Color: clDarkRed; Deep: [deBlood_Cave];),
    // Impaler
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 65; Level: 8; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 9; Max: 11;); MaxDamage: (Min: 65; Max: 67;));
    Price: 790; Color: clDarkRed; Deep: [deBlood_Cave];),
    // Barbarous Mace
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 60; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 19; Max: 22;); MaxDamage: (Min: 38; Max: 41;));
    Price: 780; Color: clDarkRed; Deep: [deBlood_Cave];),
    // Adept Hammer
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 65; Level: 8; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 24; Max: 27;); MaxDamage: (Min: 48; Max: 51;));
    Price: 850; Color: clDarkRed; Deep: [deBlood_Cave];),
    // Staff7
    (Symbol: '|'; ItemType: itStaff; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 30; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 7; Max: 8;); MaxDamage: (Min: 8; Max: 9;));
    Price: 900; Color: clDarkGreen; Deep: [deBlood_Cave];),
    // Staff8
    (Symbol: '|'; ItemType: itStaff; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 33; Level: 8; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 8; Max: 9;); MaxDamage: (Min: 10; Max: 11;));
    Price: 1000; Color: clDarkGreen; Deep: [deBlood_Cave];),

    /// / == Drom == ////

    // Casque
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1;
    MaxDurability: 60; Level: 9; Defense: (Min: 16; Max: 18);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1300; Color: clWhite; Deep: [deDrom];),
    // Winged Helm
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1;
    MaxDurability: 75; Level: 10; Defense: (Min: 18; Max: 20);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1500; Color: clWhite; Deep: [deDrom];),
    // Magic Helmet
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1;
    MaxDurability: 40; Level: 9; Defense: (Min: 8; Max: 10);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 2300; Color: clLightestBlue; Deep: [deDrom];),
    // Crown
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1;
    MaxDurability: 50; Level: 10; Defense: (Min: 10; Max: 12);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 2800; Color: clLightestBlue; Deep: [deDrom];),
    // Splint Mail
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1;
    MaxDurability: 225; Level: 9; Defense: (Min: 41; Max: 45);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1900; Color: clWhite; Deep: [deDrom];),
    // Plate Mail
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1;
    MaxDurability: 250; Level: 10; Defense: (Min: 46; Max: 50);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 2500; Color: clWhite; Deep: [deDrom];),
    // Moloch Robe
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1;
    MaxDurability: 180; Level: 9; Defense: (Min: 17; Max: 18);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 2600; Color: clLightestBlue; Deep: [deDrom];),
    // Boneweave Hauberk
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1;
    MaxDurability: 200; Level: 10; Defense: (Min: 19; Max: 20);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 3000; Color: clLightestBlue; Deep: [deDrom];),

    // Troll Gauntlets
    (Symbol: '%'; ItemType: itHands; SlotType: stHands; MaxStack: 1;
    MaxDurability: 55; Level: 9; Defense: (Min: 17; Max: 18);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 500; Color: clWhite; Deep: [deDrom];),
    // Plated Gauntlets
    (Symbol: '%'; ItemType: itHands; SlotType: stHands; MaxStack: 1;
    MaxDurability: 60; Level: 10; Defense: (Min: 19; Max: 20);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 600; Color: clWhite; Deep: [deDrom];),

    // Battle Boots
    (Symbol: ';'; ItemType: itFeet; SlotType: stFeet; MaxStack: 1;
    MaxDurability: 90; Level: 9; Defense: (Min: 25; Max: 27);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 900; Color: clWhite; Deep: [deDrom];),
    // Plate Boots
    (Symbol: ';'; ItemType: itFeet; SlotType: stFeet; MaxStack: 1;
    MaxDurability: 100; Level: 10; Defense: (Min: 28; Max: 30);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1000; Color: clWhite; Deep: [deDrom];),

    // Tower Shield
    (Symbol: '+'; ItemType: itShield; SlotType: stOffHand; MaxStack: 1;
    MaxDurability: 100; Level: 9; Defense: (Min: 28; Max: 30);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 920; Color: clLightBlue; Deep: [deDrom];),
    // Gothic Shield
    (Symbol: '+'; ItemType: itShield; SlotType: stOffHand; MaxStack: 1;
    MaxDurability: 150; Level: 10; Defense: (Min: 31; Max: 35);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Price: 1000; Color: clLightBlue; Deep: [deDrom];),

    // Rune Sword
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 70; Level: 9; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 30; Max: 33;); MaxDamage: (Min: 50; Max: 54;));
    Price: 930; Color: clDarkRed; Deep: [deDrom];),
    // Troll Slayer,
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 75; Level: 10; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 33; Max: 37;); MaxDamage: (Min: 55; Max: 60;));
    Price: 990; Color: clDarkRed; Deep: [deDrom];),
    // Chopper
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 70; Level: 9; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 36; Max: 39;); MaxDamage: (Min: 45; Max: 49;));
    Price: 940; Color: clDarkRed; Deep: [deDrom];),
    // Demon Axe,
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 75; Level: 10; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 40; Max: 45;); MaxDamage: (Min: 50; Max: 55;));
    Price: 980; Color: clDarkRed; Deep: [deDrom];),
    // Soul Reaver
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 70; Level: 9; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 10; Max: 12;); MaxDamage: (Min: 80; Max: 89;));
    Price: 940; Color: clDarkRed; Deep: [deDrom];),
    // Honed Spear,
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 75; Level: 10; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 10; Max: 15;); MaxDamage: (Min: 90; Max: 100;));
    Price: 970; Color: clDarkRed; Deep: [deDrom];),
    // War Maul
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 70; Level: 9; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 30; Max: 35;); MaxDamage: (Min: 65; Max: 69;));
    Price: 950; Color: clDarkRed; Deep: [deDrom];),
    // Doom Hammer
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 75; Level: 10; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 36; Max: 40;); MaxDamage: (Min: 70; Max: 80;));
    Price: 1000; Color: clDarkRed; Deep: [deDrom];),
    // Staff9
    (Symbol: '|'; ItemType: itStaff; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 36; Level: 9; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 9; Max: 10;); MaxDamage: (Min: 11; Max: 12;));
    Price: 1100; Color: clDarkGreen; Deep: [deDrom];),
    // Staff10
    (Symbol: '|'; ItemType: itStaff; SlotType: stMainHand; MaxStack: 1;
    MaxDurability: 40; Level: 10; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 11; Max: 12;); MaxDamage: (Min: 13; Max: 14;));
    Price: 1200; Color: clDarkGreen; Deep: [deDrom];)

    );

type
  TItems = class(TEntity)
  private
    FItemName: array [TItemEnum] of string;
    function GetName(I: TItemEnum): string; overload;
  public
    Index: UInt;
    class procedure Make(ID: UInt; var AItem: Item);
    class procedure CalcItem(var AItem: Item);
    constructor Create;
    destructor Destroy; override;
    procedure Render(AX, AY: UInt);
    procedure Add(AZ: TMapEnum; AX: Int = -1; AY: Int = -1; AID: Int = -1;
      IsRare: Boolean = False);
    function GetItemEnum(AItemID: Int): TItemEnum;
    function GetItemInfo(AItem: Item; IsManyItems: Boolean = False;
      ACount: UInt = 0; IsShort: Boolean = False): string;
    function RenderInvItem(const AX, AY, I: Int; AItem: Item;
      IsAdvInfo: Boolean = False; IsRender: Boolean = True;
      PriceType: TPriceType = ptNone): string;
    function GetSlotName(const SlotType: TSlotType): string;
    procedure AddItemToInv(Index: Int = 0; AFlag: Boolean = False); overload;
    procedure AddItemToInv(AItemEnum: TItemEnum; AAmount: UInt = 1;
      EqFlag: Boolean = False; IdFlag: Boolean = False;
      SufID: UInt = 0); overload;
    function GetInventory: string;
    function GetPrice(Price: UInt; F: Boolean = False): string;
    function GetLevel(L: UInt): string;
    function GetInfo(Sign: string; Value: UInt; Color: string;
      RareColor: string = ''): string;
    procedure RenderInventory(PriceType: TPriceType = ptNone);
    procedure LootGold(const AX, AY: UInt);
    procedure Loot(const AX, AY: UInt; AItemEnum: TItemEnum); overload;
    procedure Loot(const AX, AY: UInt; AIsBoss: Boolean); overload;
    property Name[I: TItemEnum]: string read GetName;
    function ChItem(AItem: Item): Boolean;
    function Identify(var AItem: Item; IsNew: Boolean = False;
      IsRare: Boolean = False; Index: UInt = 0): Boolean;
    function GetName(AItem: Item; IsShort: Boolean = False): string; overload;
    function GetNameThe(AItem: Item): string;
    procedure AddItemToDungeon(AItem: Item);
    function AddItemInfo(V: array of string): string;
    function StrToItemEnum(const S: string): TItemEnum;
    procedure SetBonus(var AItem: Item; const BonusType: TBonusType;
      const Value: UInt8);
    function GetBonus(const AItem: Item; const BonusType: TBonusType): UInt8;
    procedure DelCorpses();
    procedure AddPlants;
  end;

var
  Items: TItems = nil;

implementation

uses Math, Classes, TypInfo, SysUtils, Trollhunter.Terminal, uLanguage,
  Trollhunter.UI.Log,
  Trollhunter.Item.Shop, uTalent, Trollhunter.Item.Affixes, uAttribute,
  Trollhunter.UI,
  uBearLibItemsDungeon,
  uBearLibItemsInventory, uHelpers;

{ TItems }

class procedure TItems.CalcItem(var AItem: Item);
var
  SB: TSuffixBase;
begin
  // Suffix
  SB := SuffixBase[TSuffixEnum(AItem.Identify)];
  // Damage
  if (AItem.MinDamage > 0) and (AItem.MinDamage >= AItem.MaxDamage) then
    AItem.MinDamage := AItem.MaxDamage - 1;
  // Flask
  if (ItemBase[TItemEnum(AItem.ItemID)].ItemType in FlaskTypeItems) then
  begin
    AItem.Price := ItemBase[TItemEnum(AItem.ItemID)].Price + SB.Price;
    if not SB.Rare then
      AItem.Price := AItem.Price + AItem.Value;
    Exit;
  end;
  { // Oil
    if ItemBase[TItemEnum(AItem.ItemID)].ItemType in OilTypeItems then
    begin
    AItem.Price := ItemBase[TItemEnum(AItem.ItemID)].Price + APrice +
    Round(AItem.Durability * 4.7);
    case AItem.Effect of
    tfCursed:
    AItem.Price := AItem.Price div 2;
    tfBlessed:
    AItem.Price := AItem.Price * 2;
    end;
    Exit;
    end; }
  // Price
  if (ItemBase[TItemEnum(AItem.ItemID)].ItemType in IdentTypeItems) then
  begin
    AItem.Price := ItemBase[TItemEnum(AItem.ItemID)].Price + SB.Price +
      Round(AItem.MaxDurability * 3.7) + Round(AItem.Defense * 4.8) +
      Round(AItem.MaxDamage * 5.6);
  end
  else
    AItem.Price := ItemBase[TItemEnum(AItem.ItemID)].Price;
end;

function TItems.ChItem(AItem: Item): Boolean;
begin
  Result := (ItemBase[TItemEnum(AItem.ItemID)].ItemType in CorpseTypeItems) or
    (AItem.Stack > 1) or (AItem.Amount > 1) or (AItem.Identify = 0);
end;

function TItems.GetItemInfo(AItem: Item; IsManyItems: Boolean = False;
  ACount: UInt = 0; IsShort: Boolean = False): string;
var
  ID: Int;
  S, T, Level, D: string;
  IT: TItemType;
  V: UInt;

  function GetAmount(): string;
  begin
    Result := Format('(%dx)', [AItem.Amount])
  end;

  procedure AddEffect(const AEffect: TEffect; const Sign, Color: string;
    const RareColor: string = '');
  var
    Ef: TEffect;

    procedure Add(Color: string);
    var
      V: UInt;
    begin
      V := AItem.Value;
      if ((V > 0) or (Sign = '&')) then
        S := S + Items.GetInfo(Sign, V, Color, RareColor) + ' ';
    end;

  begin
    if (AEffect in AItem.Effects) then
    begin
      if (AEffect = efCraftAtr) then
      begin
        for Ef := CraftEffLow to Pred(CraftEffHigh) do
          Add(EfNameStr[Ef]);
        Exit;
      end;
      Add(Color);
    end;
  end;

begin
  S := '';
  T := '';
  Level := '';
  Result := '';
  ID := AItem.ItemID;
  IT := ItemBase[TItemEnum(ID)].ItemType;
  // Level
  if (AItem.Level > Player.Attributes.Attrib[atLev].Value) or
    not Game.GetOption(apHdLevOfItem) then
    Level := GetLevel(AItem.Level);
  // Info
  if not IsManyItems then
  begin
    if (IT in RuneTypeItems + ScrollTypeItems) then
    begin
      V := ItemBase[TItemEnum(ID)].ManaCost;
      if (V > 0) then
        S := S + Items.GetInfo('-', V, 'Mana') + ' ';
    end;

    AddEffect(efMana, '+', 'Mana');
    AddEffect(efLife, '+', 'Life');
    AddEffect(efFood, '+', 'Food');
    AddEffect(efCurePoison, '+', 'Poison');
    AddEffect(efBloodlust, '+', 'Poison', 'Blood');
    AddEffect(efIdentification, '&', 'Ident', 'Dark Yellow');
    AddEffect(efAllIdentification, '&', 'Ident', 'Dark Yellow');
    AddEffect(efTownPortal, '&', 'Portal', 'Dark Green');
    AddEffect(efTeleportation, '&', 'Portal', 'Dark Green');
    AddEffect(efRepair, '+', 'Repair', 'Food');
    AddEffect(efCraftStr, '&', 'Strength', 'Strength');
    AddEffect(efCraftDex, '&', 'Dexterity', 'Dexterity');
    AddEffect(efCraftWil, '&', 'Willpower', 'Willpower');
    AddEffect(efCraftPer, '&', 'Perception', 'Perception');
    AddEffect(efCraftAtr, '&', '', '');
    AddEffect(efPrmMana, '*', 'Mana');
    AddEffect(efPrmLife, '*', 'Life');
  end;
  // Amount
  if (AItem.Stack > 1) then
  begin
    if not(IT in NotInfoTypeItems) then
      S := AddItemInfo([Level, S]);
    if (AItem.Amount > 1) then
      S := S + GetAmount();
  end
  // Corpse
  else if (TItemEnum(ID) = ivCorpse) then
    S := ''
    // Light (Torch)
  else if (IT = itTorch) then
    S := S + Format('(%s%d/%d)', [UI.Icon(icFlag), AItem.Value,
      ItemBase[TItemEnum(ID)].Value])
  else
  begin
    if (AItem.SlotID = 0) then
    begin
      Level := Level + ' ' + S;
      S := '';
    end;
    // Defense
    if (IT in ArmorTypeItems + JewelryTypeItems) then
    begin
      if (AItem.Defense > 0) then
        T := Format('%s%d', [UI.Icon(icShield), AItem.Defense]);
      if (AItem.Identify > 0) and
        (TSuffixEnum(AItem.Identify) in DefenseSuffixes) then
        T := Terminal.Colorize(T, 'Rare');
    end;
    // Damage
    if (IT in WeaponTypeItems + JewelryTypeItems) then
    begin
      if (AItem.MinDamage > 0) then
        T := Format('%s%d-%d', [UI.Icon(icSword), AItem.MinDamage,
          AItem.MaxDamage]);
      if (AItem.Identify > 0) and (TSuffixEnum(AItem.Identify) in DamageSuffixes)
      then
        T := Terminal.Colorize(T, 'Rare');
    end;

    if (AItem.Bonus[0] > 0) then
    begin
      if (Items.GetBonus(AItem, btVis) > 0) then
        Level := Level + ' ' + Items.GetInfo('x', Items.GetBonus(AItem, btVis),
          'Vision', 'Rare');
      if (Items.GetBonus(AItem, btLife) > 0) then
        Level := Level + ' ' + Items.GetInfo('*', Items.GetBonus(AItem, btLife),
          'Life', 'Rare');
      if (Items.GetBonus(AItem, btMana) > 0) then
        Level := Level + ' ' + Items.GetInfo('*', Items.GetBonus(AItem, btMana),
          'Mana', 'Rare');
      if (Items.GetBonus(AItem, btExtraGold) > 0) then
        Level := Level + ' ' + Items.GetInfo('x',
          Items.GetBonus(AItem, btExtraGold), 'Gold', 'Rare');
    end;
    if (AItem.Bonus[1] > 0) then
    begin
      if (Items.GetBonus(AItem, btStr) > 0) then
        Level := Level + ' ' + Items.GetInfo('*', Items.GetBonus(AItem, btStr),
          'Strength', 'Rare');
      if (Items.GetBonus(AItem, btDex) > 0) then
        Level := Level + ' ' + Items.GetInfo('*', Items.GetBonus(AItem, btDex),
          'Dexterity', 'Rare');
      if (Items.GetBonus(AItem, btWil) > 0) then
        Level := Level + ' ' + Items.GetInfo('*', Items.GetBonus(AItem, btWil),
          'Willpower', 'Rare');
      if (Items.GetBonus(AItem, btPer) > 0) then
        Level := Level + ' ' + Items.GetInfo('*', Items.GetBonus(AItem, btPer),
          'Perception', 'Rare');
    end;
    if (AItem.Bonus[2] > 0) then
    begin
      if (Items.GetBonus(AItem, btReLife) > 0) then
        Level := Level + ' ' + Items.GetInfo('@',
          Items.GetBonus(AItem, btReLife), 'Life', 'Rare');
      if (Items.GetBonus(AItem, btReMana) > 0) then
        Level := Level + ' ' + Items.GetInfo('@',
          Items.GetBonus(AItem, btReMana), 'Mana', 'Rare');
      if (Items.GetBonus(AItem, btLifeAfEachKill) > 0) then
        Level := Level + ' ' + Items.GetInfo('x',
          Items.GetBonus(AItem, btLifeAfEachKill), 'Life', 'Rare');
      if (Items.GetBonus(AItem, btManaAfEachKill) > 0) then
        Level := Level + ' ' + Items.GetInfo('x',
          Items.GetBonus(AItem, btManaAfEachKill), 'Mana', 'Rare');
    end;
    // Durability
    D := '';
    if IT in SmithTypeItems then
    begin
      D := Format('%s%d/%d', [UI.Icon(icHammer), AItem.Durability,
        AItem.MaxDurability]);
      if (AItem.Identify > 0) and
        (TSuffixEnum(AItem.Identify) in DurabilitySuffixes) then
        D := Terminal.Colorize(D, 'Rare');
    end;
    S := S + AddItemInfo([Level, T, D]);

    if ((AItem.Identify = 0) or Player.Look) then
      S := '';
  end;
  Result := Trim(Format('%s %s', [Items.GetName(AItem), S]));
  // Map's item
  if (IsManyItems or (ACount > 0)) then
  begin
    if Player.Look or IsShort then
      S := '';
    S := Game.IfThen(AItem.Amount > 1, GetAmount(), '');
    S := GetCapit(GetDescAn(Trim(Items.GetName(AItem, IsShort) + ' ' + S)));
    if IsManyItems then
    begin
      Result := Format(_('Several items (%dx) are lying here (%s).'),
        [ACount, S]);
    end
    else
      Result := Format(_('%s is lying here.'), [S]);
  end;
  if Mode.Wizard and Game.ShowID then
    Result := Result + Format(' ID: %d', [ID]);
end;

class procedure TItems.Make(ID: UInt; var AItem: Item);
var
  I: UInt;

  function IsIdentify(): Boolean;
  begin
    Result := False;
    if (ItemBase[TItemEnum(ID)].ItemType in IdentTypeItems) then
      Result := (Math.RandomRange(0, 4) = 0) or
        (ItemBase[TItemEnum(ID)].ItemType in AllwaysIdentTypeItems)
  end;

begin
  Items_Clear_Item(AItem);
  AItem.ItemID := ID;
  AItem.SlotID := Ord(ItemBase[TItemEnum(ID)].SlotType);
  AItem.Stack := ItemBase[TItemEnum(ID)].MaxStack;
  AItem.Level := ItemBase[TItemEnum(ID)].Level;
  // Color
  AItem.Color := ItemBase[TItemEnum(ID)].Color;
  // AItem.Color := Math.RandomRange($FF888888, $FFFFFFFF);
  // Effects
  AItem.Effects := ItemBase[TItemEnum(ID)].Effects;
  // Value
  AItem.Value := ItemBase[TItemEnum(ID)].Value;
  // Defense
  if (AItem.Stack = 1) and (ItemBase[TItemEnum(ID)].Defense.Min > 0) then
    AItem.Defense := Math.EnsureRange
      (Math.RandomRange(ItemBase[TItemEnum(ID)].Defense.Min,
      ItemBase[TItemEnum(ID)].Defense.Max + 1), 1, UIntMax)
  else
    AItem.Defense := 0;
  // Damage
  if (AItem.Stack = 1) and (ItemBase[TItemEnum(ID)].Damage.MinDamage.Min > 0)
  then
    AItem.MinDamage := Math.EnsureRange
      (Math.RandomRange(ItemBase[TItemEnum(ID)].Damage.MinDamage.Min,
      ItemBase[TItemEnum(ID)].Damage.MinDamage.Max + 1), 1, UIntMax - 1)
  else
    AItem.MinDamage := 0;
  if (AItem.Stack = 1) and (ItemBase[TItemEnum(ID)].Damage.MaxDamage.Min > 0)
  then
    AItem.MaxDamage := Math.EnsureRange
      (Math.RandomRange(ItemBase[TItemEnum(ID)].Damage.MaxDamage.Min,
      ItemBase[TItemEnum(ID)].Damage.MaxDamage.Max + 1), 2, UIntMax)
  else
    AItem.MaxDamage := 0;
  // Durability
  if (AItem.Stack = 1) then
    AItem.MaxDurability := Math.EnsureRange
      (Math.RandomRange(ItemBase[TItemEnum(ID)].MaxDurability - 5,
      ItemBase[TItemEnum(ID)].MaxDurability + 6), 10, UIntMax)
  else
    AItem.MaxDurability := 0;
  AItem.Durability := AItem.MaxDurability;
  // Clear Bonuses
  for I := 0 to BonusCount - 1 do
    AItem.Bonus[I] := 0;
  // Price
  CalcItem(AItem);
  // Affix
  AItem.Identify := Math.IfThen(IsIdentify(), 0, -1);
end;

procedure TItems.Add(AZ: TMapEnum; AX: Int = -1; AY: Int = -1; AID: Int = -1;
  IsRare: Boolean = False);
var
  I, ID, FX, FY: UInt;
  FItem: Item;
  Value: Int;
  IT: TItemType;
begin
  I := 0;
  repeat
    if (AID >= 0) then
      ID := AID
    else
      ID := Math.RandomRange(0, Ord(High(TItemEnum)) + 1);
    if (AX >= 0) then
      FX := AX
    else
      FX := Math.RandomRange(1, UIntMax - 1);
    if (AY >= 0) then
      FY := AY
    else
      FY := Math.RandomRange(1, UIntMax - 1);
    if (I >= UIntMax) then
    begin
      ID := Ord(ivGold);
      Break;
    end;
    Inc(I);
  until (Map.GetTileEnum(FX, FY, AZ) in SpawnTiles) and
    (AZ in ItemBase[TItemEnum(ID)].Deep);
  IT := ItemBase[TItemEnum(ID)].ItemType;
  if ((AID < 0) and (IT in NotDropTypeItems)) then
    Exit;
  // Rare
  if not IsRare and ItemBase[TItemEnum(ID)].Rare then
  begin
    Add(AZ, AX, AY, AID, Math.RandomRange(0, Math.IfThen(Mode.Wizard,
      1, 9)) = 0);
    if not Mode.Wizard then
      Exit;
  end;
  //
  Make(ID, FItem);
  FItem.Amount := 1;
  FItem.MapID := Ord(AZ);
  case IT of
    itCoin:
      begin
        Value := Ord(AZ) + 1;
        FItem.Amount := Math.RandomRange(Value * Value,
          Value * Value * (5 - Ord(Game.Difficulty))) + 1;
        if Player.Talents.IsTalent(tlMiser) then
          FItem.Amount := FItem.Amount * 2;
        // Extra Gold from Monsters
        if (Player.Attributes.Attrib[atExtraGold].Value > 0) then
        begin
          Value := FItem.Amount + Player.Attributes.Attrib[atExtraGold].Value;
          FItem.Amount := Value.Percent(FItem.Amount);
        end;
      end;
  end;
  if ((FItem.Stack = 1) and (IT in WeaponTypeItems + ArmorTypeItems)) then
    FItem.Durability := Math.RandomRange(FItem.MaxDurability div 4,
      FItem.MaxDurability) + 1;
  FItem.X := FX;
  FItem.Y := FY;
  FItem.Equipment := 0;
  AddItemToDungeon(FItem);
end;

procedure TItems.Loot(const AX, AY: UInt; AItemEnum: TItemEnum);
begin
  Add(Map.Current, AX, AY, Ord(AItemEnum));
end;

procedure TItems.Loot(const AX, AY: UInt; AIsBoss: Boolean);
var
  V, I: UInt;
const
  M = 10;
begin
  V := Math.IfThen(AIsBoss, Ord(Map.Current) + 9, Ord(Map.Current) + 2);
  for I := 1 to V do
  begin
    // Gold
    if (Math.RandomRange(0, M) >= 6) then
      LootGold(AX, AY);
    { // Potion
      if ((Math.RandomRange(0, M) >= 7) or AIsBoss) then
      Loot(AX, AY, TItemEnum(Math.RandomRange(Ord(ivLesser_Healing_Potion),
      Ord(ivPotion_of_Full_Mana) + 1))); }
    // Scroll
    if ((Math.RandomRange(0, M) >= 8) or AIsBoss) then
      Loot(AX, AY, TItemEnum(Math.RandomRange(Ord(ivScroll_of_Minor_Healing),
        Ord(ivScroll_of_Town_Portal) + 1)));
    // Item
    if (Math.RandomRange(0, M) >= 9) then
      Add(Map.Current, AX, AY, -1, AIsBoss);
  end;
end;

procedure TItems.Render(AX, AY: UInt);
var
  MapID: UInt;
  I, Count: Int;
  FColor: Cardinal;
  FSymbol: Char;
  FItem: Item;
begin
  MapID := Ord(Map.Current);
  Count := Items_Dungeon_GetMapCount(MapID);
  for I := Count - 1 downto 0 do
  begin
    FItem := Items_Dungeon_GetMapItem(MapID, I);
    if not Map.InView(FItem.X, FItem.Y) or
      (not Mode.Wizard and not Map.GetFOV(FItem.X, FItem.Y)) then
      Continue;
    X := FItem.X - Player.X + AX + View.Left;
    Y := FItem.Y - Player.Y + AY + View.Top;
    if not Mode.Wizard and (Player.GetDist(FItem.X, FItem.Y) > Player.Vision)
    then
    begin
      FColor := clFog;
      FSymbol := '?';
    end
    else
    begin
      FColor := FItem.Color; // ItemBase[TItemEnum(FItem.ItemID)].Color;
      FSymbol := ItemBase[TItemEnum(FItem.ItemID)].Symbol;
    end;
    Terminal.Print(X, Y, FSymbol, FColor, 0);
  end;
end;

constructor TItems.Create;
var
  I: TItemEnum;
  P: Pointer;
begin
  Items_Open();
  P := TypeInfo(TItemEnum);
  for I := Low(TItemEnum) to High(TItemEnum) do
    FItemName[I] := GetEnumName(P, Ord(I)).GetName('iv');
end;

destructor TItems.Destroy;
begin
  Items_Close();
  inherited;
end;

function TItems.StrToItemEnum(const S: string): TItemEnum;
var
  P: Pointer;
begin
  P := TypeInfo(TItemEnum);
  Result := TItemEnum(GetEnumValue(P, 'iv' + S));
end;

function TItems.GetName(I: TItemEnum): string;
begin
  Result := FItemName[I];
end;

function TItems.GetSlotName(const SlotType: TSlotType): string;
const
  SlotName: array [TSlotType] of string = ('', 'Head', 'Torso', 'Hands', 'Feet',
    'Main Hand', 'Off-Hand', 'Neck', 'Finger', 'In Hands');
begin
  Result := Terminal.Colorize(Format('{%s}', [SlotName[SlotType]]),
    Terminal.GetColorFromIni('Equip'));
end;

function TItems.GetPrice(Price: UInt; F: Boolean = False): string;
var
  Color: string;
begin
  if (F or (Player.Gold >= Price)) then
    Color := 'lighter yellow'
  else
    Color := 'light red';
  Result := Terminal.Colorize(UI.Icon(icGold) + Price.ToString, Color);
end;

function TItems.GetLevel(L: UInt): string;
var
  Color: string;
begin
  if (L > Player.Attributes.Attrib[atLev].Value) then
    Color := 'Light Red'
  else
    Color := 'Gray';
  Result := Terminal.Colorize(Format('%s%d', [UI.Icon(icElixir), L]), Color);
end;

function TItems.GetInfo(Sign: string; Value: UInt; Color: string;
  RareColor: string = ''): string;
var
  S, P: string;
begin
  S := '';
  P := '';
  Result := '';
  if (Sign = '*') then
    Sign := '';
  if (Sign = 'x') then
  begin
    Sign := '';
    S := UI.Icon(icPlus);
  end;
  if (Sign = '@') then
  begin
    Sign := '';
    S := UI.Icon(icElixir);
  end;
  if (Color = 'Life') then
    S := S + UI.Icon(icLife);
  if (Color = 'Mana') then
  begin
    if ((Player.Attributes.Attrib[atMana].Value < Value) and (Sign <> '+')) then
      Color := 'NoMana';
    S := S + UI.Icon(icMana);
  end;
  if (Color = 'Food') then
    S := UI.Icon(icFood);
  if (Color = 'Ident') then
    S := UI.Icon(icQuestion);
  if (Color = 'Portal') then
    S := UI.Icon(icFlag);
  if (Color = 'Poison') then
    S := UI.Icon(icDrop);
  if (Color = 'Vision') then
    S := S + UI.Icon(icVision);
  if (Color = 'Repair') then
    S := UI.Icon(icHammer);
  if (Color = 'Strength') then
    S := UI.Icon(icStr);
  if (Color = 'Dexterity') then
    S := UI.Icon(icDex);
  if (Color = 'Willpower') then
    S := UI.Icon(icBook);
  if (Color = 'Perception') then
    S := UI.Icon(icLeaf);
  if (Color = 'Gold') then
  begin
    S := S + UI.Icon(icGold);
    P := '%';
  end;
  if (RareColor <> '') then
    Color := RareColor;
  if (Sign = '&') then
  begin
    Result := Trim(Terminal.Colorize(S, Color));
    Exit;
  end;
  if (Value > 0) then
    Result := Trim(Terminal.Colorize(Format('%s%s%d%s', [S, Sign, Value, P]
      ), Color));
end;

function TItems.RenderInvItem(const AX, AY, I: Int; AItem: Item;
  IsAdvInfo: Boolean = False; IsRender: Boolean = True;
  PriceType: TPriceType = ptNone): string;
var
  S: string;
  D: TItemBase;
  RepairCost: UInt;

const
  T = '------';
  L = Length(T) + 1;

  function GetRedPrice(Price: UInt): string;
  begin
    Result := Terminal.Colorize(UI.Icon(icGold) + Price.ToString, 'Light Red');
  end;

begin
  Result := '';
  D := ItemBase[TItemEnum(AItem.ItemID)];
  Terminal.Print(AX - 4, AY + I, UI.KeyToStr(Chr(I + Ord('A')), '',
    Game.IfThen(AItem.Equipment > 0, 'Equip', 'Key')));

  if IsRender then
  begin
    Terminal.ForegroundColor(AItem.Color);
    Terminal.Print(AX, AY + I, D.Symbol);
  end
  else
    Result := Result + D.Symbol + ' ';

  Terminal.ForegroundColor(clLightGray);
  if (IsAdvInfo and (Game.Timer = 0)) then
  begin
    S := '';
    if (D.SlotType <> stNone) and (AItem.Equipment > 0) then
      S := GetSlotName(D.SlotType);
  end;
  if (S <> '') then
    S := Items.GetItemInfo(AItem, False, 0, True) + ' ' + S
  else
    S := Trim(Items.GetItemInfo(AItem, False, 0, True));

  if IsRender then
  begin
    Terminal.Print(AX + 2, AY + I, S);
    S := '';
    case PriceType of
      ptSell:
        begin
          S := T;
          if ((AItem.Price > 1) and not ChItem(AItem)) then
          begin
            S := GetPrice(AItem.Price div 4, True);
            if (AItem.Equipment > 0) then
              S := GetRedPrice(AItem.Price div 4);
          end;
        end;
      ptBuy:
        begin
          S := GetPrice(AItem.Price);
        end;
      ptRepair:
        begin
          S := T;
          if ((AItem.Stack = 1) and (AItem.Amount = 1) and (Items.Index = 0))
          then
          begin
            RepairCost := (AItem.MaxDurability - AItem.Durability) * 10;
            if (RepairCost > 0) then
              S := GetPrice(RepairCost);
          end;
        end;
    else
      if ((AItem.Price > 0) and Game.GetOption(apShPrice)) then
        S := GetPrice(AItem.Price, True);
    end;
    if Game.Timer > 0 then
      Terminal.Print(Status.Left - L, AY + I, S)
    else
      Terminal.Print(Screen.Width - L, AY + I, S);
  end
  else
    Result := Result + S;
end;

function TItems.GetBonus(const AItem: Item; const BonusType: TBonusType): UInt8;
begin
  case BonusType of
    btLife:
      Result := UInt8(AItem.Bonus[0] shr 24);
    btMana:
      Result := UInt8(AItem.Bonus[0] shr 16);
    btVis:
      Result := UInt8(AItem.Bonus[0] shr 8);
    btExtraGold:
      Result := UInt8(AItem.Bonus[0]);
    btStr:
      Result := UInt8(AItem.Bonus[1] shr 24);
    btDex:
      Result := UInt8(AItem.Bonus[1] shr 16);
    btWil:
      Result := UInt8(AItem.Bonus[1] shr 8);
    btPer:
      Result := UInt8(AItem.Bonus[1]);
    btReLife:
      Result := UInt8(AItem.Bonus[2] shr 24);
    btReMana:
      Result := UInt8(AItem.Bonus[2] shr 16);
    btLifeAfEachKill:
      Result := UInt8(AItem.Bonus[2] shr 8);
    btManaAfEachKill:
      Result := UInt8(AItem.Bonus[2]);
  else
    Result := 0;
  end;
end;

procedure TItems.SetBonus(var AItem: Item; const BonusType: TBonusType;
  const Value: UInt8);
var
  V: array [0 .. 3] of UInt8;
  I: Cardinal;
begin
  if (BonusType in [btStr .. btPer]) then
  begin
    V[0] := GetBonus(AItem, btStr);
    V[1] := GetBonus(AItem, btDex);
    V[2] := GetBonus(AItem, btWil);
    V[3] := GetBonus(AItem, btPer);
  end;
  if (BonusType in [btLife .. btExtraGold]) then
  begin
    V[0] := GetBonus(AItem, btLife);
    V[1] := GetBonus(AItem, btMana);
    V[2] := GetBonus(AItem, btVis);
    V[3] := GetBonus(AItem, btExtraGold);
  end;
  if (BonusType in [btReLife .. btManaAfEachKill]) then
  begin
    V[0] := GetBonus(AItem, btReLife);
    V[1] := GetBonus(AItem, btReMana);
    V[2] := GetBonus(AItem, btLifeAfEachKill);
    V[3] := GetBonus(AItem, btManaAfEachKill);
  end;

  case BonusType of
    btLife, btStr, btReLife:
      V[0] := Value;
    btMana, btDex, btReMana:
      V[1] := Value;
    btVis, btWil, btLifeAfEachKill:
      V[2] := Value;
    btExtraGold, btPer, btManaAfEachKill:
      V[3] := Value;
  end;

  I := (V[0] shl 24) or (V[1] shl 16) or (V[2] shl 8) or V[3];

  case BonusType of
    btLife .. btExtraGold:
      AItem.Bonus[0] := I;
    btStr .. btPer:
      AItem.Bonus[1] := I;
    btReLife .. btManaAfEachKill:
      AItem.Bonus[2] := I;
  end;
end;

function TItems.AddItemInfo(V: array of string): string;
var
  I: UInt;
begin
  Result := '';
  for I := 0 to Length(V) - 1 do
    Result := Result + Trim(V[I]) + ' ';
  Result := Trim(Result);
  if (Result <> '') then
    Result := '[[' + Result + ']]';
  Result := SysUtils.StringReplace(Result, '  ', ' ', [rfReplaceAll]);
end;

procedure TItems.AddItemToDungeon(AItem: Item);
var
  FItem: Item;
  I, FCount: Int;
begin
  // Add an item
  Items_Dungeon_AppendItem(AItem);
  // Sort all items on the tile
  FCount := Items_Dungeon_GetMapCountXY(AItem.MapID, AItem.X, AItem.Y);
  if (FCount = 1) then
    Exit;
  for I := 0 to FCount - 1 do
  begin
    FItem := Items_Dungeon_GetMapItemXY(AItem.MapID, I, AItem.X, AItem.Y);
    if (ItemBase[TItemEnum(FItem.ItemID)].ItemType in CorpseTypeItems) then
      if (Items_Dungeon_DeleteMapItemXY(AItem.MapID, I, AItem.X, AItem.Y,
        FItem) > 0) then
        Items_Dungeon_AppendItem(FItem);
  end;
end;

procedure TItems.AddItemToInv(AItemEnum: TItemEnum; AAmount: UInt = 1;
  EqFlag: Boolean = False; IdFlag: Boolean = False; SufID: UInt = 0);
var
  FItem: Item;
begin
  if (AAmount = 0) then
    Exit;
  Make(Ord(AItemEnum), FItem);
  FItem.Amount := AAmount;
  FItem.Equipment := IfThen(EqFlag, 1, 0);
  if IdFlag and (FItem.Identify = 0) then
    Items.Identify(FItem, True, False, SufID);
  Items_Inventory_AppendItem(FItem);
end;

procedure TItems.AddItemToInv(Index: Int = 0; AFlag: Boolean = False);
var
  FItem: Item;
  MapID: Int;
begin
  MapID := Ord(Map.Current);
  FItem := Items_Dungeon_GetMapItemXY(MapID, Index, Player.X, Player.Y);
  if (FItem.Stack > 1) and (FItem.Amount > 1) and not AFlag then
  begin
    Player.SetAmountScene(False, Index, FItem.Amount);
    Exit;
  end;
  if (Items_Dungeon_DeleteMapItemXY(MapID, Index, Player.X, Player.Y, FItem) > 0)
  then
  begin
    Items_Inventory_AppendItem(FItem);
    if (FItem.Amount = 1) then
      MsgLog.Add(Format(_('You picked up %s.'), [Items.GetNameThe(FItem)]))
    else
      MsgLog.Add(Format(_('You picked up %s (%dx).'), [Items.GetNameThe(FItem),
        FItem.Amount]));
    Player.Wait;
    Player.Calc;
  end;
end;

function TItems.GetInventory: string;
var
  SL: TStringList;
  I, FCount: Int;
  FItem: Item;
  S: string;
begin
  Result := '';
  SL := TStringList.Create;
  try
    FCount := Items_Inventory_GetCount().InRange(ItemMax);
    for I := 0 to FCount - 1 do
    begin
      FItem := Items_Inventory_GetItem(I);
      S := Game.IfThen(FItem.Amount > 1, Format(' (%dx)', [FItem.Amount]), '');
      S := GetCapit(GetDescAn(Trim(Items.GetName(FItem, True) + S)));
      SL.Append(S);
    end;
    Result := SL.Text;
  finally
    FreeAndNil(SL);
  end;
end;

function TItems.GetItemEnum(AItemID: Int): TItemEnum;
begin
  Result := TItemEnum(AItemID);
end;

procedure TItems.LootGold(const AX, AY: UInt);
begin
  Loot(AX, AY, ivGold);
  if (Math.RandomRange(0, 4) = 0) then
  begin
    X := Map.EnsureRange(AX + (Math.RandomRange(0, 3) - 1));
    Y := Map.EnsureRange(AY + (Math.RandomRange(0, 3) - 1));
    if (Map.GetTileEnum(X, Y, Map.Current) in SpawnTiles) then
      Loot(X, Y, ivGold);
  end;
end;

procedure TItems.RenderInventory(PriceType: TPriceType = ptNone);
var
  I, FCount: Int;
begin
  FCount := Items_Inventory_GetCount().InRange(ItemMax);
  for I := 0 to FCount - 1 do
    Items.RenderInvItem(5, 2, I, Items_Inventory_GetItem(I), True, True,
      PriceType);
end;

function TItems.Identify(var AItem: Item; IsNew: Boolean = False;
  IsRare: Boolean = False; Index: UInt = 0): Boolean;
var
  I: UInt;
  SB: TSuffixBase;
begin
  Result := False;
  if (AItem.Identify = 0) then
  begin
    repeat
      // Random suffix
      I := Math.RandomRange(1, Ord(High(TSuffixEnum)) + 1);
      if (Index > 0) then
        I := Index;
      SB := SuffixBase[TSuffixEnum(I)];
      // Level
      { if (ItemBase[TItemEnum(AItem.ItemID)].ItemType in OilTypeItems) then
        begin
        Lev := Player.Attributes.Attrib[atLev].Value;
        AItem.Level := Math.EnsureRange(Math.RandomRange(Lev - 1,
        Lev + 2), 1, 15);
        AItem.Price := AItem.Price * AItem.Level;
        end; }
      if ((AItem.Level < SB.Level.Min) or (AItem.Level > SB.Level.Max)) then
        Continue;
      //
      if not(ItemBase[TItemEnum(AItem.ItemID)].ItemType in SB.Occurence) then
        Continue;
      // Rare
      if not IsRare and SB.Rare then
      begin
        Identify(AItem, IsNew, Math.RandomRange(0, Math.IfThen(Mode.Wizard, 1,
          9)) = 0, Index);
        if not Mode.Wizard then
          Exit;
      end;
      //
      AItem.Identify := I;
      Affixes.DoSuffix(AItem);
      if IsNew then
      begin
        AItem.Durability := AItem.MaxDurability;
      end;
    until (AItem.Identify > 0);
    Result := True;
  end;
end;

function TItems.GetName(AItem: Item; IsShort: Boolean = False): string;
var
  N, S: string;
begin
  N := GetName(TItemEnum(AItem.ItemID));
  case AItem.Identify of
    0:
      begin
        if IsShort then
          S := ''
        else
          S := ' [[' + _('Unidentified') + ']]';
        Result := Terminal.Colorize(N + S, 'Unidentified');
      end;
    1 .. UIntMax:
      begin
        Result := N + ' ' + Affixes.GetSuffixName(TSuffixEnum(AItem.Identify));
        if (AItem.SlotID > 0) then
          Result := Terminal.Colorize(Result, 'Rare')
        else
          Result := Terminal.Colorize(Result, 'Flask');
      end
  else
    Result := N;
  end;
  { if ItemBase[TItemEnum(AItem.ItemID)].ItemType in OilTypeItems then
    case AItem.Effect of
    tfCursed:
    Result := Terminal.Colorize(_('Cursed') + ' ' + GetPureText(Result),
    'Cursed');
    tfBlessed:
    Result := Terminal.Colorize(_('Blessed') + ' ' + GetPureText(Result),
    'Blessed');
    end; }
end;

function TItems.GetNameThe(AItem: Item): string;
begin
  Result := GetDescThe(GetPureText(Items.GetName(AItem, True)));
end;

procedure TItems.DelCorpses;
var
  I: Int;
  M: TMapEnum;
  FItem: Item;
begin
  for M := Low(TMapEnum) to High(TMapEnum) do
  begin
    if (M = Map.Current) then
      Continue;
    for I := Items_Dungeon_GetMapCount(Ord(M)) - 1 downto 0 do
    begin
      FItem := Items_Dungeon_GetMapItem(Ord(M), I);
      if (ItemBase[TItemEnum(FItem.ItemID)].ItemType in CorpseTypeItems +
        FoodTypeItems) then
        if (Items_Dungeon_DeleteMapItem(Ord(M), I, FItem) > 0) then
          Continue;
    end;
  end;
end;

procedure TItems.AddPlants;
var
  I, FCount: Int;
  M: TMapEnum;
  FItem: Item;
begin
  for M := Low(TMapEnum) to High(TMapEnum) do
  begin
    FCount := Items_Dungeon_GetMapCount(Ord(M));
    for I := 0 to FCount - 1 do
    begin
      FItem := Items_Dungeon_GetMapItem(Ord(M), I);
      if (ItemBase[TItemEnum(FItem.ItemID)].ItemType in PlantTypeItems) then
      begin
        X := FItem.X + Math.RandomRange(0, 2);
        Y := FItem.Y + Math.RandomRange(0, 2);
        if (Map.InMap(X, Y) and (Map.GetTileEnum(X, Y, M) in SpawnTiles) and
          (M in ItemBase[TItemEnum(FItem.ItemID)].Deep)) then
          Loot(X, Y, TItemEnum(FItem.ItemID));
      end;
    end;
  end;
end;

initialization

Items := TItems.Create;

finalization

FreeAndNil(Items);

end.
