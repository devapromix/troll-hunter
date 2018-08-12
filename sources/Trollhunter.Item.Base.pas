unit Trollhunter.Item.Base;

interface

uses
  uBearLibItemsCommon,
  Trollhunter.Types,
  Trollhunter.Item.Types,
  Trollhunter.Player.Types,
  Trollhunter.Game,
  Trollhunter.Map,
  Trollhunter.Creature;

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

type
  ItemBase = class(TObject)
  public
    class function GetItem(const Value: Int): TItemBase; overload;
    class function GetItem(const Value: Item): TItemBase; overload;
    class function GetItem(const Value: TItemEnum): TItemBase; overload;
    class function Count: UInt;
  end;

implementation

const
  Base: array [TItemEnum] of TItemBase = (
    /// / == All maps == ////

    // None
    (Symbol: ' '; ItemType: itNone; SlotType: stNone; MaxStack: 1; MaxDurability: 0; Level: 0; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 0; Color: clGray; Deep: []),
    // Corpse
    (Symbol: '%'; ItemType: itCorpse; SlotType: stNone; MaxStack: 1; MaxDurability: 0; Level: 0; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 0; Color: clGray; Deep: [deDark_Wood .. deDrom]),
    // Gold
    (Symbol: '$'; ItemType: itCoin; SlotType: stNone; MaxStack: 10000; MaxDurability: 0; Level: 0; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 0; Color: clYellow; Deep: [deDark_Wood .. deDrom]),

    // Ruby Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 25; Color: clWhite; Deep: [deDark_Wood]; Value: 10;),
    // Amethyst Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 50; Color: clWhite; Deep: [deDark_Wood]; Value: 20;),
    // Bismuth Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1; MaxDurability: 0; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 75; Color: clWhite; Deep: [deDark_Wood]; Value: 30;),
    // Silver Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1; MaxDurability: 0; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 100; Color: clWhite; Deep: [deDark_Wood]; Value: 40;),
    // Aquamarine Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1; MaxDurability: 0; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 125; Color: clWhite; Deep: [deGray_Cave]; Value: 50;),
    // Sapphire Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1; MaxDurability: 0; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 150; Color: clWhite; Deep: [deGray_Cave]; Value: 60;),
    // Quicksilver Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1; MaxDurability: 0; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 175; Color: clWhite; Deep: [deGray_Cave]; Value: 70;),
    // Topaz Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1; MaxDurability: 0; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 200; Color: clWhite; Deep: [deGray_Cave]; Value: 80;),
    // Sulphur Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1; MaxDurability: 0; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 225; Color: clWhite; Deep: [deDeep_Cave]; Value: 90;),
    // Granite Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1; MaxDurability: 0; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 250; Color: clWhite; Deep: [deDeep_Cave]; Value: 100;),
    // Quartz Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1; MaxDurability: 0; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 275; Color: clWhite; Deep: [deDeep_Cave]; Value: 110;),
    // Sacred Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1; MaxDurability: 0; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 300; Color: clWhite; Deep: [deDeep_Cave]; Value: 120;),
    // Jade Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1; MaxDurability: 0; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 325; Color: clWhite; Deep: [deBlood_Cave]; Value: 130;),
    // Hallowed Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1; MaxDurability: 0; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 350; Color: clWhite; Deep: [deBlood_Cave]; Value: 140;),
    // Coruscating Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1; MaxDurability: 0; Level: 8; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 375; Color: clWhite; Deep: [deBlood_Cave]; Value: 150;),
    // Sanctified Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1; MaxDurability: 0; Level: 8; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 400; Color: clWhite; Deep: [deBlood_Cave]; Value: 160;),
    // Divine Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1; MaxDurability: 0; Level: 9; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 425; Color: clWhite; Deep: [deDrom]; Value: 170;),
    // Gold Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1; MaxDurability: 0; Level: 9; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 450; Color: clWhite; Deep: [deDrom]; Value: 180;),
    // Diamond Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1; MaxDurability: 0; Level: 10; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 475; Color: clWhite; Deep: [deDrom]; Value: 190;),
    // Eternal Flask
    (Symbol: '!'; ItemType: itFlask; SlotType: stNone; MaxStack: 1; MaxDurability: 0; Level: 10; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 500; Color: clWhite; Deep: [deDrom]; Value: 200;),

    // Potion of Minor Healing
    (Symbol: '!'; ItemType: itPotion; SlotType: stNone; MaxStack: 8; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 50; Color: clLightestRed; Deep: [deDark_Wood .. deGray_Cave];
    Effects: [efLife]; Value: 50;),
    // Potion of Lesser Healing
    (Symbol: '!'; ItemType: itPotion; SlotType: stNone; MaxStack: 8; MaxDurability: 0; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 100; Color: clLightRed; Deep: [deGray_Cave .. deDeep_Cave];
    Effects: [efLife]; Value: 100;),
    // Potion of Greater Healing
    (Symbol: '!'; ItemType: itPotion; SlotType: stNone; MaxStack: 8; MaxDurability: 0; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 150; Color: clRed; Deep: [deDeep_Cave .. deBlood_Cave];
    Effects: [efLife]; Value: 150;),
    // Potion of Full Healing
    (Symbol: '!'; ItemType: itPotion; SlotType: stNone; MaxStack: 8; MaxDurability: 0; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 200; Color: clDarkRed; Deep: [deBlood_Cave .. deDrom];
    Effects: [efLife]; Value: 200;),

    // Potion of Minor Mana
    (Symbol: '!'; ItemType: itPotion; SlotType: stNone; MaxStack: 8; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 40; Color: clLightestBlue; Deep: [deDark_Wood .. deGray_Cave];
    Effects: [efMana]; Value: 60;),
    // Potion of Lesser Mana
    (Symbol: '!'; ItemType: itPotion; SlotType: stNone; MaxStack: 8; MaxDurability: 0; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 80; Color: clLightBlue; Deep: [deGray_Cave .. deDeep_Cave];
    Effects: [efMana]; Value: 120;),
    // Potion of Greater Mana
    (Symbol: '!'; ItemType: itPotion; SlotType: stNone; MaxStack: 8; MaxDurability: 0; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 120; Color: clBlue; Deep: [deDeep_Cave .. deBlood_Cave];
    Effects: [efMana]; Value: 180;),
    // Potion of Full Mana
    (Symbol: '!'; ItemType: itPotion; SlotType: stNone; MaxStack: 8; MaxDurability: 0; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 160; Color: clDarkBlue; Deep: [deBlood_Cave .. deDrom];
    Effects: [efMana]; Value: 240;),

    // Stone of
    (Symbol: '8'; ItemType: itStone; SlotType: stNone; MaxStack: 32; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 25; Color: clRed; Deep: [deDark_Wood .. deDrom]; Effects: [efLife];
    Value: 25;),
    // Stone of
    (Symbol: '8'; ItemType: itStone; SlotType: stNone; MaxStack: 32; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 20; Color: clBlue; Deep: [deDark_Wood .. deDrom]; Effects: [efMana];
    Value: 25;),
    // Stone of
    (Symbol: '8'; ItemType: itStone; SlotType: stNone; MaxStack: 32; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 25; Color: clYellow; Deep: [deDark_Wood .. deDrom];
    Effects: [efLife, efMana]; Value: 25;),

    // Scroll of Minor healing
    (Symbol: '?'; ItemType: itScroll; SlotType: stNone; MaxStack: 16; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 40; Color: clLightestBlue; Deep: [deDark_Wood .. deDeep_Cave];
    Effects: [efLife]; Value: 50; ManaCost: 20;),
    // Scroll of Lesser Healing
    (Symbol: '?'; ItemType: itScroll; SlotType: stNone; MaxStack: 16; MaxDurability: 0; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 80; Color: clLightBlue; Deep: [deGray_Cave .. deBlood_Cave];
    Effects: [efLife]; Value: 100; ManaCost: 30;),
    // Scroll of Greater Healing
    (Symbol: '?'; ItemType: itScroll; SlotType: stNone; MaxStack: 16; MaxDurability: 0; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 160; Color: clBlue; Deep: [deDeep_Cave .. deDrom]; Effects: [efLife];
    Value: 200; ManaCost: 40;),
    // Scroll of Full Healing
    (Symbol: '?'; ItemType: itScroll; SlotType: stNone; MaxStack: 16; MaxDurability: 0; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 300; Color: clDarkBlue; Deep: [deBlood_Cave .. deDrom];
    Effects: [efLife]; Value: 250; ManaCost: 50;),

    // Scroll of Hunger
    (Symbol: '?'; ItemType: itScroll; SlotType: stNone; MaxStack: 16; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 100; Color: clDarkYellow; Deep: [deDark_Wood .. deDrom];
    Effects: [efFood]; Value: 350; ManaCost: 25;),

    // Scroll of Sidestepping
    (Symbol: '?'; ItemType: itScroll; SlotType: stNone; MaxStack: 16; MaxDurability: 0; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 750; Color: clLightRed; Deep: [deDark_Wood .. deDeep_Cave];
    Effects: [efTeleportation]; Value: 3; ManaCost: 50;),
    // Scroll of Phasing
    (Symbol: '?'; ItemType: itScroll; SlotType: stNone; MaxStack: 16; MaxDurability: 0; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1000; Color: clRed; Deep: [deGray_Cave .. deBlood_Cave];
    Effects: [efTeleportation]; Value: 6; ManaCost: 100;),
    // Scroll of Teleportation
    (Symbol: '?'; ItemType: itScroll; SlotType: stNone; MaxStack: 16; MaxDurability: 0; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1250; Color: clDarkRed; Deep: [deDeep_Cave .. deDrom];
    Effects: [efTeleportation]; Value: 10; ManaCost: 150;),
    // Scroll of Disappearing
    (Symbol: '?'; ItemType: itScroll; SlotType: stNone; MaxStack: 16; MaxDurability: 0; Level: 8; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1500; Color: clDarkRed; Deep: [deBlood_Cave .. deDrom];
    Effects: [efTeleportation]; Value: 15; ManaCost: 200;),

    // Scroll of Town Portal
    (Symbol: '?'; ItemType: itScroll; SlotType: stNone; MaxStack: 16; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 75; Color: clLightGreen; Deep: [deDark_Wood .. deDrom];
    Effects: [efTownPortal]; Value: 0; ManaCost: 20;),
    // Scroll of Bloodlust
    (Symbol: '?'; ItemType: itScroll; SlotType: stNone; MaxStack: 16; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 100; Color: clLightRed; Deep: [deDark_Wood .. deDrom];
    Effects: [efBloodlust]; Value: 10; ManaCost: 25;),
    // Scroll of Identify
    (Symbol: '?'; ItemType: itScroll; SlotType: stNone; MaxStack: 16; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 35; Color: clLightYellow; Deep: [deDark_Wood .. deDrom];
    Effects: [efIdentification]; Value: 1; ManaCost: 5;),
    // Scroll of Full Identify
    (Symbol: '?'; ItemType: itScroll; SlotType: stNone; MaxStack: 16; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 75; Color: clLightYellow; Deep: [deDark_Wood .. deDrom];
    Effects: [efAllIdentification]; Value: 1; ManaCost: 15;),
    // Scroll of Enchant Item
    (Symbol: '?'; ItemType: itScroll; SlotType: stNone; MaxStack: 16; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 750; Color: clLightBlue; Deep: [deDark_Wood .. deDrom];
    Effects: [efEnchantItem]; Value: 1; ManaCost: 75; Rare: True;),

    // Rune of minor healing
    (Symbol: '*'; ItemType: itRune; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1000; Color: clLightestRed; Deep: [deDark_Wood .. deGray_Cave];
    Effects: [efLife]; Value: 75; ManaCost: 20; Rare: True;),
    // Rune of lesser healing
    (Symbol: '*'; ItemType: itRune; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1500; Color: clLightRed; Deep: [deGray_Cave .. deDeep_Cave];
    Effects: [efLife]; Value: 150; ManaCost: 30; Rare: True;),
    // Rune of greater healing
    (Symbol: '*'; ItemType: itRune; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 2000; Color: clRed; Deep: [deDeep_Cave .. deBlood_Cave];
    Effects: [efLife]; Value: 250; ManaCost: 40; Rare: True;),
    // Rune of full healing
    (Symbol: '*'; ItemType: itRune; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 9; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 2500; Color: clDarkRed; Deep: [deBlood_Cave .. deDrom];
    Effects: [efLife]; Value: 250; ManaCost: 50; Rare: True;),

    // Rune of teleportation
    (Symbol: '*'; ItemType: itRune; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 7000; Color: clDarkRed; Deep: [deDeep_Cave .. deBlood_Cave];
    Effects: [efTeleportation]; Value: 10; ManaCost: 150; Rare: True;),
    // Rune of town portal
    (Symbol: '*'; ItemType: itRune; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 4800; Color: clLightGreen; Deep: [deDark_Wood .. deDrom];
    Effects: [efTownPortal]; Value: 0; ManaCost: 50; Rare: True;),

    // Bread ration
    (Symbol: ':'; ItemType: itFood; SlotType: stNone; MaxStack: 16; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 200; Color: clYellow; Deep: [deDark_Wood .. deDrom];
    Effects: [efFood]; Value: 600;),
    // Valley root
    (Symbol: ':'; ItemType: itPlant; SlotType: stNone; MaxStack: 16; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 100; Color: clLightestYellow; Deep: [deDark_Wood .. deDrom];
    Effects: [efFood]; Value: 300;),
    // Rat pod
    (Symbol: ':'; ItemType: itPlant; SlotType: stNone; MaxStack: 16; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 150; Color: clLightestGreen; Deep: [deDark_Wood .. deDrom];
    Effects: [efFood]; Value: 450;),
    // Kobold bulb
    (Symbol: ':'; ItemType: itPlant; SlotType: stNone; MaxStack: 16; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 50; Color: clLightestGreen; Deep: [deDark_Wood .. deDrom];
    Effects: [efFood]; Value: 150;),
    // Hunk of Meat
    (Symbol: ':'; ItemType: itFood; SlotType: stNone; MaxStack: 16; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 300; Color: clLightestYellow; Deep: [deDark_Wood .. deDrom];
    Effects: [efFood]; Value: 900;),

    // Key
    (Symbol: '`'; ItemType: itKey; SlotType: stNone; MaxStack: 16; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 50; Color: clYellow; Deep: [deDark_Wood .. deDrom];),

    // Torch
    (Symbol: 'i'; ItemType: itTorch; SlotType: stTorch; MaxStack: 1; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 60; Color: clLighterRed; Deep: [deDark_Wood .. deDrom]; Value: 100;),
    // Oil Lamp
    (Symbol: 'O'; ItemType: itTorch; SlotType: stTorch; MaxStack: 1; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 90; Color: clLighterYellow; Deep: [deDark_Wood .. deDrom];
    Value: 150;),

    // Light Orb
    (Symbol: 'o'; ItemType: itOrb; SlotType: stNone; MaxStack: 10; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 75; Color: clLightYellow; Deep: [deDark_Wood .. deDrom];
    Effects: [efLight]; Value: 150;),
    // Life Orb
    (Symbol: 'o'; ItemType: itOrb; SlotType: stNone; MaxStack: 10; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 55; Color: clLightRed; Deep: [deDark_Wood .. deDrom];
    Effects: [efLife, efCurePoison]; Value: 45;),
    // Mana Orb
    (Symbol: 'o'; ItemType: itOrb; SlotType: stNone; MaxStack: 10; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 25; Color: clLightBlue; Deep: [deDark_Wood .. deDrom];
    Effects: [efMana]; Value: 35;),

    // Healing Herb
    (Symbol: ':'; ItemType: itPlant; SlotType: stNone; MaxStack: 16; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 15; Color: clYellow; Deep: [deDark_Wood .. deDrom];
    Effects: [efLife, efFood]; Value: 25;),

    // Ruby #1
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 750; Color: clRed; Deep: [deDark_Wood]; Effects: [efCraftStr];
    Value: 0; Rare: True;),
    // Ruby #2
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1000; Color: clRed; Deep: [deDark_Wood .. deGray_Cave];
    Effects: [efCraftStr]; Value: 1; Rare: True;),
    // Ruby #3
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1250; Color: clRed; Deep: [deGray_Cave .. deDeep_Cave];
    Effects: [efCraftStr]; Value: 2; Rare: True;),
    // Ruby #4
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1500; Color: clRed; Deep: [deDeep_Cave .. deBlood_Cave];
    Effects: [efCraftStr]; Value: 3; Rare: True;),
    // Ruby #5
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1750; Color: clRed; Deep: [deBlood_Cave .. deDrom];
    Effects: [efCraftStr]; Value: 4; Rare: True;),
    // Ruby #6
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 2000; Color: clRed; Deep: [deBlood_Cave .. deDrom];
    Effects: [efCraftStr]; Value: 5; Rare: True;),
    // Ruby #7
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 2500; Color: clRed; Deep: [deDrom]; Effects: [efCraftStr]; Value: 6;
    Rare: True;),

    // Topaz #1
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 750; Color: clYellow; Deep: [deDark_Wood]; Effects: [efCraftDex];
    Value: 0; Rare: True;),
    // Topaz #2
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1000; Color: clYellow; Deep: [deDark_Wood .. deGray_Cave];
    Effects: [efCraftDex]; Value: 1; Rare: True;),
    // Topaz #3
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1250; Color: clYellow; Deep: [deGray_Cave .. deDeep_Cave];
    Effects: [efCraftDex]; Value: 2; Rare: True;),
    // Topaz #4
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1500; Color: clYellow; Deep: [deDeep_Cave .. deBlood_Cave];
    Effects: [efCraftDex]; Value: 3; Rare: True;),
    // Topaz #5
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1750; Color: clYellow; Deep: [deBlood_Cave .. deDrom];
    Effects: [efCraftDex]; Value: 4; Rare: True;),
    // Topaz #6
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 2000; Color: clYellow; Deep: [deBlood_Cave .. deDrom];
    Effects: [efCraftDex]; Value: 5; Rare: True;),
    // Topaz #7
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 2500; Color: clYellow; Deep: [deDrom]; Effects: [efCraftDex];
    Value: 6; Rare: True;),

    // Sapphire #1
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 750; Color: clBlue; Deep: [deDark_Wood]; Effects: [efCraftWil];
    Value: 0; Rare: True;),
    // Sapphire #2
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1000; Color: clBlue; Deep: [deDark_Wood .. deGray_Cave];
    Effects: [efCraftWil]; Value: 1; Rare: True;),
    // Sapphire #3
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1250; Color: clBlue; Deep: [deGray_Cave .. deDeep_Cave];
    Effects: [efCraftWil]; Value: 2; Rare: True;),
    // Sapphire #4
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1500; Color: clBlue; Deep: [deDeep_Cave .. deBlood_Cave];
    Effects: [efCraftWil]; Value: 3; Rare: True;),
    // Sapphire #5
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1750; Color: clBlue; Deep: [deBlood_Cave .. deDrom];
    Effects: [efCraftWil]; Value: 4; Rare: True;),
    // Sapphire #6
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 2000; Color: clBlue; Deep: [deBlood_Cave .. deDrom];
    Effects: [efCraftWil]; Value: 5; Rare: True;),
    // Sapphire #7
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 2500; Color: clBlue; Deep: [deDrom]; Effects: [efCraftWil]; Value: 6;
    Rare: True;),

    // Emerald #1
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 750; Color: clGreen; Deep: [deDark_Wood]; Effects: [efCraftPer];
    Value: 0; Rare: True;),
    // Emerald #2
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1000; Color: clGreen; Deep: [deDark_Wood .. deGray_Cave];
    Effects: [efCraftPer]; Value: 1; Rare: True;),
    // Emerald #3
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1250; Color: clGreen; Deep: [deGray_Cave .. deDeep_Cave];
    Effects: [efCraftPer]; Value: 2; Rare: True;),
    // Emerald #4
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1500; Color: clGreen; Deep: [deDeep_Cave .. deBlood_Cave];
    Effects: [efCraftPer]; Value: 3; Rare: True;),
    // Emerald #5
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1750; Color: clGreen; Deep: [deBlood_Cave .. deDrom];
    Effects: [efCraftPer]; Value: 4; Rare: True;),
    // Emerald #6
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 2000; Color: clGreen; Deep: [deBlood_Cave .. deDrom];
    Effects: [efCraftPer]; Value: 5; Rare: True;),
    // Emerald #7
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 2500; Color: clGreen; Deep: [deDrom]; Effects: [efCraftPer];
    Value: 6; Rare: True;),

    // Diamond #1
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1000; Color: clWhite; Deep: [deDark_Wood]; Effects: [efCraftAtr];
    Value: 0; Rare: True;),
    // Diamond #2
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1500; Color: clWhite; Deep: [deDark_Wood .. deGray_Cave];
    Effects: [efCraftAtr]; Value: 1; Rare: True;),
    // Diamond #3
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 2000; Color: clWhite; Deep: [deGray_Cave .. deDeep_Cave];
    Effects: [efCraftAtr]; Value: 2; Rare: True;),
    // Diamond #4
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 2500; Color: clWhite; Deep: [deDeep_Cave .. deBlood_Cave];
    Effects: [efCraftAtr]; Value: 3; Rare: True;),
    // Diamond #5
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 3000; Color: clWhite; Deep: [deBlood_Cave .. deDrom];
    Effects: [efCraftAtr]; Value: 4; Rare: True;),
    // Diamond #6
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 3500; Color: clWhite; Deep: [deBlood_Cave .. deDrom];
    Effects: [efCraftAtr]; Value: 5; Rare: True;),
    // Diamond #7
    (Symbol: '$'; ItemType: itGem; SlotType: stNone; MaxStack: 3; MaxDurability: 0; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 4000; Color: clWhite; Deep: [deDrom]; Effects: [efCraftAtr];
    Rare: True;),

    // Ring #1
    (Symbol: '='; ItemType: itRing; SlotType: stFinger; MaxStack: 1; MaxDurability: 15; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1000; Color: clYellow; Deep: [deDark_Wood];),
    // Ring #2
    (Symbol: '='; ItemType: itRing; SlotType: stFinger; MaxStack: 1; MaxDurability: 20; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1250; Color: clYellow; Deep: [deDark_Wood];),
    // Ring #3
    (Symbol: '='; ItemType: itRing; SlotType: stFinger; MaxStack: 1; MaxDurability: 25; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1500; Color: clYellow; Deep: [deGray_Cave];),
    // Ring #4
    (Symbol: '='; ItemType: itRing; SlotType: stFinger; MaxStack: 1; MaxDurability: 30; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1750; Color: clYellow; Deep: [deGray_Cave];),
    // Ring #5
    (Symbol: '='; ItemType: itRing; SlotType: stFinger; MaxStack: 1; MaxDurability: 35; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 2000; Color: clYellow; Deep: [deDeep_Cave];),
    // Ring #6
    (Symbol: '='; ItemType: itRing; SlotType: stFinger; MaxStack: 1; MaxDurability: 40; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1250; Color: clYellow; Deep: [deDeep_Cave];),
    // Ring #7
    (Symbol: '='; ItemType: itRing; SlotType: stFinger; MaxStack: 1; MaxDurability: 45; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1500; Color: clYellow; Deep: [deBlood_Cave];),
    // Ring #8
    (Symbol: '='; ItemType: itRing; SlotType: stFinger; MaxStack: 1; MaxDurability: 50; Level: 8; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1750; Color: clYellow; Deep: [deBlood_Cave];),
    // Ring #9
    (Symbol: '='; ItemType: itRing; SlotType: stFinger; MaxStack: 1; MaxDurability: 55; Level: 9; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 2000; Color: clYellow; Deep: [deDrom];),
    // Ring #10
    (Symbol: '='; ItemType: itRing; SlotType: stFinger; MaxStack: 1; MaxDurability: 60; Level: 10; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 2500; Color: clYellow; Deep: [deDrom];),

    // Amulet #1
    (Symbol: ''''; ItemType: itAmulet; SlotType: stNeck; MaxStack: 1; MaxDurability: 35; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1200; Color: clYellow; Deep: [deDark_Wood];),
    // Amulet #2
    (Symbol: ''''; ItemType: itAmulet; SlotType: stNeck; MaxStack: 1; MaxDurability: 45; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1400; Color: clYellow; Deep: [deDark_Wood];),
    // Amulet #3
    (Symbol: ''''; ItemType: itAmulet; SlotType: stNeck; MaxStack: 1; MaxDurability: 55; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1600; Color: clYellow; Deep: [deGray_Cave];),
    // Amulet #4
    (Symbol: ''''; ItemType: itAmulet; SlotType: stNeck; MaxStack: 1; MaxDurability: 65; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1800; Color: clYellow; Deep: [deGray_Cave];),
    // Amulet #5
    (Symbol: ''''; ItemType: itAmulet; SlotType: stNeck; MaxStack: 1; MaxDurability: 75; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 2000; Color: clYellow; Deep: [deDeep_Cave];),
    // Amulet #6
    (Symbol: ''''; ItemType: itAmulet; SlotType: stNeck; MaxStack: 1; MaxDurability: 85; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 2200; Color: clYellow; Deep: [deDeep_Cave];),
    // Amulet #7
    (Symbol: ''''; ItemType: itAmulet; SlotType: stNeck; MaxStack: 1; MaxDurability: 95; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 2400; Color: clYellow; Deep: [deBlood_Cave];),
    // Amulet #8
    (Symbol: ''''; ItemType: itAmulet; SlotType: stNeck; MaxStack: 1; MaxDurability: 105; Level: 8; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 2600; Color: clYellow; Deep: [deBlood_Cave];),
    // Amulet #9
    (Symbol: ''''; ItemType: itAmulet; SlotType: stNeck; MaxStack: 1; MaxDurability: 115; Level: 9; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 2800; Color: clYellow; Deep: [deDrom];),
    // Amulet #10
    (Symbol: ''''; ItemType: itAmulet; SlotType: stNeck; MaxStack: 1; MaxDurability: 125; Level: 10; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 3000; Color: clYellow; Deep: [deDrom];),

    // Talisman #1
    (Symbol: ''''; ItemType: itTalisman; SlotType: stNeck; MaxStack: 1; MaxDurability: 40; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1000; Color: clYellow; Deep: [deDark_Wood];),
    // Talisman #2
    (Symbol: ''''; ItemType: itTalisman; SlotType: stNeck; MaxStack: 1; MaxDurability: 50; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1300; Color: clYellow; Deep: [deDark_Wood];),
    // Talisman #3
    (Symbol: ''''; ItemType: itTalisman; SlotType: stNeck; MaxStack: 1; MaxDurability: 60; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1600; Color: clYellow; Deep: [deGray_Cave];),
    // Talisman #4
    (Symbol: ''''; ItemType: itTalisman; SlotType: stNeck; MaxStack: 1; MaxDurability: 70; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1900; Color: clYellow; Deep: [deGray_Cave];),
    // Talisman #5
    (Symbol: ''''; ItemType: itTalisman; SlotType: stNeck; MaxStack: 1; MaxDurability: 80; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 2200; Color: clYellow; Deep: [deDeep_Cave];),
    // Talisman #6
    (Symbol: ''''; ItemType: itTalisman; SlotType: stNeck; MaxStack: 1; MaxDurability: 90; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 2500; Color: clYellow; Deep: [deDeep_Cave];),
    // Talisman #7
    (Symbol: ''''; ItemType: itTalisman; SlotType: stNeck; MaxStack: 1; MaxDurability: 100; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 2800; Color: clYellow; Deep: [deBlood_Cave];),
    // Talisman #8
    (Symbol: ''''; ItemType: itTalisman; SlotType: stNeck; MaxStack: 1; MaxDurability: 110; Level: 8; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 3100; Color: clYellow; Deep: [deBlood_Cave];),
    // Talisman #9
    (Symbol: ''''; ItemType: itTalisman; SlotType: stNeck; MaxStack: 1; MaxDurability: 120; Level: 9; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 3500; Color: clYellow; Deep: [deDrom];),
    // Talisman #10
    (Symbol: ''''; ItemType: itTalisman; SlotType: stNeck; MaxStack: 1; MaxDurability: 130; Level: 10; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 4000; Color: clYellow; Deep: [deDrom];),

    // Wand #1
    (Symbol: '-'; ItemType: itWand; SlotType: stMainHand; MaxStack: 1; MaxDurability: 10; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 1; Max: 2;); MaxDamage: (Min: 3; Max: 4;)); Price: 550; Color: clDarkBlue; Deep: [deDark_Wood]; ManaCost: 2;),
    // Wand #2
    (Symbol: '-'; ItemType: itWand; SlotType: stMainHand; MaxStack: 1; MaxDurability: 12; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 5; Max: 6;); MaxDamage: (Min: 7; Max: 8;)); Price: 600; Color: clDarkBlue; Deep: [deDark_Wood]; ManaCost: 2;),
    // Wand #3
    (Symbol: '-'; ItemType: itWand; SlotType: stMainHand; MaxStack: 1; MaxDurability: 14; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 11; Max: 12;); MaxDamage: (Min: 13; Max: 14;)); Price: 650; Color: clDarkBlue; Deep: [deGray_Cave]; ManaCost: 2;),
    // Wand #4
    (Symbol: '-'; ItemType: itWand; SlotType: stMainHand; MaxStack: 1; MaxDurability: 16; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 15; Max: 16;); MaxDamage: (Min: 17; Max: 18;)); Price: 700; Color: clDarkBlue; Deep: [deGray_Cave]; ManaCost: 3;),
    // Wand #5
    (Symbol: '-'; ItemType: itWand; SlotType: stMainHand; MaxStack: 1; MaxDurability: 18; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 21; Max: 22;); MaxDamage: (Min: 23; Max: 24;)); Price: 750; Color: clDarkBlue; Deep: [deDeep_Cave]; ManaCost: 3;),
    // Wand #6
    (Symbol: '-'; ItemType: itWand; SlotType: stMainHand; MaxStack: 1; MaxDurability: 20; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 25; Max: 26;); MaxDamage: (Min: 27; Max: 28;)); Price: 800; Color: clDarkBlue; Deep: [deDeep_Cave]; ManaCost: 3;),
    // Wand #7
    (Symbol: '-'; ItemType: itWand; SlotType: stMainHand; MaxStack: 1; MaxDurability: 22; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 31; Max: 32;); MaxDamage: (Min: 33; Max: 34;)); Price: 850; Color: clDarkBlue; Deep: [deBlood_Cave]; ManaCost: 4;),
    // Wand #8
    (Symbol: '-'; ItemType: itWand; SlotType: stMainHand; MaxStack: 1; MaxDurability: 24; Level: 8; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 35; Max: 36;); MaxDamage: (Min: 37; Max: 38;)); Price: 900; Color: clDarkBlue; Deep: [deBlood_Cave]; ManaCost: 4;),
    // Wand #9
    (Symbol: '-'; ItemType: itWand; SlotType: stMainHand; MaxStack: 1; MaxDurability: 27; Level: 9; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 41; Max: 42;); MaxDamage: (Min: 43; Max: 44;)); Price: 950; Color: clDarkBlue; Deep: [deDrom]; ManaCost: 4;),
    // Wand #10
    (Symbol: '-'; ItemType: itWand; SlotType: stMainHand; MaxStack: 1; MaxDurability: 30; Level: 10; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 45; Max: 46;); MaxDamage: (Min: 47; Max: 48;)); Price: 1000; Color: clDarkBlue; Deep: [deDrom]; ManaCost: 5;),

    // Quarterstaff (Battle Staff #1)
    (Symbol: '|'; ItemType: itBattleStaff; SlotType: stMainHand; MaxStack: 1; MaxDurability: 20; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 2; Max: 3;); MaxDamage: (Min: 3; Max: 4;)); Price: 170; Color: clDarkGreen; Deep: [deDark_Wood];),

    // Short Staff (Staff #1)
    (Symbol: '|'; ItemType: itStaff; SlotType: stMainHand; MaxStack: 1; MaxDurability: 12; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 1; Max: 2;); MaxDamage: (Min: 2; Max: 3;)); Price: 300; Color: clDarkGreen; Deep: [deDark_Wood]; ManaCost: 12;),
    // (Staff #2)
    (Symbol: '|'; ItemType: itStaff; SlotType: stMainHand; MaxStack: 1; MaxDurability: 15; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 2; Max: 3;); MaxDamage: (Min: 4; Max: 5;)); Price: 400; Color: clDarkGreen; Deep: [deDark_Wood]; ManaCost: 2;),
    // (Staff #3)
    (Symbol: '|'; ItemType: itStaff; SlotType: stMainHand; MaxStack: 1; MaxDurability: 18; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 3; Max: 4;); MaxDamage: (Min: 4; Max: 5;)); Price: 500; Color: clDarkGreen; Deep: [deGray_Cave]; ManaCost: 3;),
    // (Staff #4)
    (Symbol: '|'; ItemType: itStaff; SlotType: stMainHand; MaxStack: 1; MaxDurability: 21; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 4; Max: 5;); MaxDamage: (Min: 6; Max: 7;)); Price: 600; Color: clDarkGreen; Deep: [deGray_Cave]; ManaCost: 3;),
    // (Staff #5)
    (Symbol: '|'; ItemType: itStaff; SlotType: stMainHand; MaxStack: 1; MaxDurability: 24; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 5; Max: 6;); MaxDamage: (Min: 6; Max: 7;)); Price: 700; Color: clDarkGreen; Deep: [deDeep_Cave]; ManaCost: 4;),
    // (Staff #6)
    (Symbol: '|'; ItemType: itStaff; SlotType: stMainHand; MaxStack: 1; MaxDurability: 27; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 6; Max: 7;); MaxDamage: (Min: 8; Max: 9;)); Price: 800; Color: clDarkGreen; Deep: [deDeep_Cave]; ManaCost: 4;),
    // (Staff #7)
    (Symbol: '|'; ItemType: itStaff; SlotType: stMainHand; MaxStack: 1; MaxDurability: 30; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 7; Max: 8;); MaxDamage: (Min: 8; Max: 9;)); Price: 900; Color: clDarkGreen; Deep: [deBlood_Cave]; ManaCost: 5;),
    // (Staff #8)
    (Symbol: '|'; ItemType: itStaff; SlotType: stMainHand; MaxStack: 1; MaxDurability: 33; Level: 8; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 8; Max: 9;); MaxDamage: (Min: 10; Max: 11;)); Price: 1000; Color: clDarkGreen; Deep: [deBlood_Cave]; ManaCost: 6;),
    // (Staff #9)
    (Symbol: '|'; ItemType: itStaff; SlotType: stMainHand; MaxStack: 1; MaxDurability: 36; Level: 9; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 9; Max: 10;); MaxDamage: (Min: 11; Max: 12;)); Price: 1100; Color: clDarkGreen; Deep: [deDrom]; ManaCost: 7;),
    // (Staff #10)
    (Symbol: '|'; ItemType: itStaff; SlotType: stMainHand; MaxStack: 1; MaxDurability: 40; Level: 10; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 11; Max: 12;); MaxDamage: (Min: 13; Max: 14;)); Price: 1200; Color: clDarkGreen; Deep: [deDrom]; ManaCost: 8;),

    // Dagger (Dagger #1)
    (Symbol: '-'; ItemType: itDagger; SlotType: stMainHand; MaxStack: 1; MaxDurability: 13; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 1; Max: 2;); MaxDamage: (Min: 3; Max: 4;)); Price: 125; Color: clDarkBlue; Deep: [deDark_Wood];),
    // Kris (Dagger #2)
    (Symbol: '-'; ItemType: itDagger; SlotType: stMainHand; MaxStack: 1; MaxDurability: 16; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 5; Max: 6;); MaxDamage: (Min: 7; Max: 8;)); Price: 150; Color: clDarkBlue; Deep: [deDark_Wood];),
    // Sacrificial Kris (Dagger #3)
    (Symbol: '-'; ItemType: itDagger; SlotType: stMainHand; MaxStack: 1; MaxDurability: 19; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 11; Max: 12;); MaxDamage: (Min: 13; Max: 14;)); Price: 175; Color: clDarkBlue; Deep: [deGray_Cave];),
    // Leafblade (Dagger #4)
    (Symbol: '-'; ItemType: itDagger; SlotType: stMainHand; MaxStack: 1; MaxDurability: 22; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 15; Max: 16;); MaxDamage: (Min: 17; Max: 18;)); Price: 200; Color: clDarkBlue; Deep: [deGray_Cave];),
    // Long Dagger (Dagger #5)
    (Symbol: '-'; ItemType: itDagger; SlotType: stMainHand; MaxStack: 1; MaxDurability: 25; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 21; Max: 22;); MaxDamage: (Min: 23; Max: 24;)); Price: 225; Color: clDarkBlue; Deep: [deDeep_Cave];),
    // Jagged Dagger (Dagger #6)
    (Symbol: '-'; ItemType: itDagger; SlotType: stMainHand; MaxStack: 1; MaxDurability: 28; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 25; Max: 26;); MaxDamage: (Min: 27; Max: 28;)); Price: 250; Color: clDarkBlue; Deep: [deDeep_Cave];),
    // Trident Dagger (Dagger #7)
    (Symbol: '-'; ItemType: itDagger; SlotType: stMainHand; MaxStack: 1; MaxDurability: 31; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 31; Max: 32;); MaxDamage: (Min: 33; Max: 34;)); Price: 275; Color: clDarkBlue; Deep: [deBlood_Cave];),
    // Divine Dagger (Dagger #8)
    (Symbol: '-'; ItemType: itDagger; SlotType: stMainHand; MaxStack: 1; MaxDurability: 34; Level: 8; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 35; Max: 36;); MaxDamage: (Min: 37; Max: 38;)); Price: 300; Color: clDarkBlue; Deep: [deBlood_Cave];),
    // Blood Dagger (Dagger #9)
    (Symbol: '-'; ItemType: itDagger; SlotType: stMainHand; MaxStack: 1; MaxDurability: 37; Level: 9; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 41; Max: 42;); MaxDamage: (Min: 43; Max: 44;)); Price: 325; Color: clDarkBlue; Deep: [deDrom];),
    // Shadow Dagger (Dagger #10)
    (Symbol: '-'; ItemType: itDagger; SlotType: stMainHand; MaxStack: 1; MaxDurability: 40; Level: 10; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 45; Max: 46;); MaxDamage: (Min: 47; Max: 48;)); Price: 350; Color: clDarkBlue; Deep: [deDrom];),

    // Short Bow (Bow #1)
    (Symbol: ')'; ItemType: itBow; SlotType: stMainHand; MaxStack: 1; MaxDurability: 35; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 1; Max: 2;); MaxDamage: (Min: 3; Max: 4;)); Price: 200; Color: clDarkBlue; Deep: [deDark_Wood];),
    // Hunter's Bow (Bow #2)
    (Symbol: ')'; ItemType: itBow; SlotType: stMainHand; MaxStack: 1; MaxDurability: 40; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 5; Max: 6;); MaxDamage: (Min: 7; Max: 8;)); Price: 250; Color: clDarkBlue; Deep: [deDark_Wood];),
    // Long Bow (Bow #3)
    (Symbol: ')'; ItemType: itBow; SlotType: stMainHand; MaxStack: 1; MaxDurability: 45; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 11; Max: 12;); MaxDamage: (Min: 13; Max: 14;)); Price: 300; Color: clDarkBlue; Deep: [deGray_Cave];),
    // Hero Bow (Bow #4)
    (Symbol: ')'; ItemType: itBow; SlotType: stMainHand; MaxStack: 1; MaxDurability: 50; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 15; Max: 16;); MaxDamage: (Min: 17; Max: 18;)); Price: 350; Color: clDarkBlue; Deep: [deGray_Cave];),
    // Battle Bow (Bow #5)
    (Symbol: ')'; ItemType: itBow; SlotType: stMainHand; MaxStack: 1; MaxDurability: 55; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 21; Max: 22;); MaxDamage: (Min: 23; Max: 24;)); Price: 400; Color: clDarkBlue; Deep: [deDeep_Cave];),
    // Composite Bow (Bow #6)
    (Symbol: ')'; ItemType: itBow; SlotType: stMainHand; MaxStack: 1; MaxDurability: 60; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 25; Max: 26;); MaxDamage: (Min: 27; Max: 28;)); Price: 450; Color: clDarkBlue; Deep: [deDeep_Cave];),
    // Imperial Bow (Bow #7)
    (Symbol: ')'; ItemType: itBow; SlotType: stMainHand; MaxStack: 1; MaxDurability: 65; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 31; Max: 32;); MaxDamage: (Min: 33; Max: 34;)); Price: 500; Color: clDarkBlue; Deep: [deBlood_Cave];),
    // Revenant Bow (Bow #8)
    (Symbol: ')'; ItemType: itBow; SlotType: stMainHand; MaxStack: 1; MaxDurability: 70; Level: 8; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 35; Max: 36;); MaxDamage: (Min: 37; Max: 38;)); Price: 550; Color: clDarkBlue; Deep: [deBlood_Cave];),
    // Ancient Bow (Bow #9)
    (Symbol: ')'; ItemType: itBow; SlotType: stMainHand; MaxStack: 1; MaxDurability: 75; Level: 9; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 41; Max: 42;); MaxDamage: (Min: 43; Max: 44;)); Price: 600; Color: clDarkBlue; Deep: [deDrom];),
    // Dragonbone Bow (Bow #10)
    (Symbol: ')'; ItemType: itBow; SlotType: stMainHand; MaxStack: 1; MaxDurability: 80; Level: 10; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 45; Max: 46;); MaxDamage: (Min: 47; Max: 48;)); Price: 700; Color: clDarkBlue; Deep: [deDrom];),

    // Quilted Armor (Body Armor #1)
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1; MaxDurability: 25; Level: 1; Defense: (Min: 3; Max: 6);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 300; Color: clWhite; Deep: [deDark_Wood];),
    // Leather Armor (Body Armor #2)
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1; MaxDurability: 50; Level: 2; Defense: (Min: 8; Max: 11);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 500; Color: clWhite; Deep: [deDark_Wood];),
    // HardLeather Armor (Body Armor #3)
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1; MaxDurability: 75; Level: 3; Defense: (Min: 12; Max: 15);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 700; Color: clWhite; Deep: [deGray_Cave];),
    // Battle Armor (Body Armor #4)
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1; MaxDurability: 100; Level: 4; Defense: (Min: 17; Max: 20);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 900; Color: clWhite; Deep: [deGray_Cave];),
    // Brigantine Armor (Body Armor #5)
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1; MaxDurability: 125; Level: 5; Defense: (Min: 21; Max: 25);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1000; Color: clWhite; Deep: [deDeep_Cave];),
    // Ring Mail (Body Armor #6)
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1; MaxDurability: 150; Level: 6; Defense: (Min: 26; Max: 30);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1200; Color: clWhite; Deep: [deDeep_Cave];),
    // Chain Mail (Body Armor #7)
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1; MaxDurability: 175; Level: 7; Defense: (Min: 31; Max: 35);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1300; Color: clWhite; Deep: [deBlood_Cave];),
    // Scale Mail (Body Armor #8)
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1; MaxDurability: 200; Level: 8; Defense: (Min: 36; Max: 40);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1600; Color: clWhite; Deep: [deBlood_Cave];),
    // Splint Mail (Body Armor #9)
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1; MaxDurability: 225; Level: 9; Defense: (Min: 41; Max: 45);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1900; Color: clWhite; Deep: [deDrom];),
    // Plate Mail (Body Armor #10)
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1; MaxDurability: 250; Level: 10; Defense: (Min: 46; Max: 50);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 2500; Color: clWhite; Deep: [deDrom];),

    // Light Clothes
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1; MaxDurability: 20; Level: 1; Defense: (Min: 1; Max: 2);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 600; Color: clLightestBlue; Deep: [deDark_Wood];),
    // Leather Apron
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1; MaxDurability: 40; Level: 2; Defense: (Min: 3; Max: 4);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 800; Color: clLightestBlue; Deep: [deDark_Wood];),
    // Fancy Clothes
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1; MaxDurability: 60; Level: 3; Defense: (Min: 5; Max: 6);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1000; Color: clLightestBlue; Deep: [deGray_Cave];),
    // Robe
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1; MaxDurability: 75; Level: 4; Defense: (Min: 7; Max: 8);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1300; Color: clLightestBlue; Deep: [deGray_Cave];),
    // Light Furs
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1; MaxDurability: 85; Level: 5; Defense: (Min: 9; Max: 10);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1500; Color: clLightestBlue; Deep: [deDeep_Cave];),
    // Clean Robe
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1; MaxDurability: 100; Level: 6; Defense: (Min: 11; Max: 12);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1700; Color: clLightestBlue; Deep: [deDeep_Cave];),
    // Thick Furs
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1; MaxDurability: 120; Level: 7; Defense: (Min: 13; Max: 14);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1900; Color: clLightestBlue; Deep: [deBlood_Cave];),
    // Hard Robe
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1; MaxDurability: 150; Level: 8; Defense: (Min: 15; Max: 16);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 2100; Color: clLightestBlue; Deep: [deBlood_Cave];),
    // Moloch Robe
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1; MaxDurability: 180; Level: 9; Defense: (Min: 17; Max: 18);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 2600; Color: clLightestBlue; Deep: [deDrom];),
    // Boneweave Hauberk
    (Symbol: '&'; ItemType: itBodyArmor; SlotType: stTorso; MaxStack: 1; MaxDurability: 200; Level: 10; Defense: (Min: 19; Max: 20);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 3000; Color: clLightestBlue; Deep: [deDrom];),

    // Cap
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1; MaxDurability: 15; Level: 1; Defense: (Min: 1; Max: 2);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 100; Color: clWhite; Deep: [deDark_Wood];),
    // War Cap
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1; MaxDurability: 20; Level: 2; Defense: (Min: 3; Max: 4);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 150; Color: clWhite; Deep: [deDark_Wood];),
    // Helm
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1; MaxDurability: 25; Level: 3; Defense: (Min: 4; Max: 6);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 300; Color: clWhite; Deep: [deGray_Cave];),
    // Grand Helm
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1; MaxDurability: 30; Level: 4; Defense: (Min: 6; Max: 8);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 400; Color: clWhite; Deep: [deGray_Cave];),
    // Great Helm
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1; MaxDurability: 35; Level: 5; Defense: (Min: 8; Max: 10);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 500; Color: clWhite; Deep: [deDeep_Cave];),
    // Full Helm
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1; MaxDurability: 40; Level: 6; Defense: (Min: 10; Max: 12);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 750; Color: clWhite; Deep: [deDeep_Cave];),
    // Horned Helmet
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1; MaxDurability: 45; Level: 7; Defense: (Min: 12; Max: 14);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 850; Color: clWhite; Deep: [deBlood_Cave];),
    // Spired Helm
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1; MaxDurability: 50; Level: 8; Defense: (Min: 14; Max: 16);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1000; Color: clWhite; Deep: [deBlood_Cave];),
    // Casque
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1; MaxDurability: 60; Level: 9; Defense: (Min: 16; Max: 18);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1300; Color: clWhite; Deep: [deDrom];),
    // Winged Helm
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1; MaxDurability: 75; Level: 10; Defense: (Min: 18; Max: 20);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1500; Color: clWhite; Deep: [deDrom];),

    // Hood
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1; MaxDurability: 10; Level: 1; Defense: (Min: 1; Max: 2);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 200; Color: clLightestBlue; Deep: [deDark_Wood];),
    // Red Hat
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1; MaxDurability: 12; Level: 2; Defense: (Min: 2; Max: 3);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 300; Color: clLightRed; Deep: [deDark_Wood];),
    // Leather Cap
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1; MaxDurability: 15; Level: 3; Defense: (Min: 2; Max: 3);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 600; Color: clLightestBlue; Deep: [deGray_Cave];),
    // Mask
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1; MaxDurability: 18; Level: 4; Defense: (Min: 3; Max: 4);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 700; Color: clLightestBlue; Deep: [deGray_Cave];),
    // Bone Helmet
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1; MaxDurability: 20; Level: 5; Defense: (Min: 4; Max: 5);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1000; Color: clLightestBlue; Deep: [deDeep_Cave];),
    // Wizard Hat
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1; MaxDurability: 25; Level: 6; Defense: (Min: 5; Max: 6);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1200; Color: clLightestBlue; Deep: [deDeep_Cave];),
    // Diadem
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1; MaxDurability: 30; Level: 7; Defense: (Min: 6; Max: 7);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1300; Color: clLightestBlue; Deep: [deBlood_Cave];),
    // Tiara
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1; MaxDurability: 35; Level: 8; Defense: (Min: 7; Max: 8);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1700; Color: clLightestBlue; Deep: [deBlood_Cave];),
    // Magic Helmet
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1; MaxDurability: 40; Level: 9; Defense: (Min: 8; Max: 10);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 2300; Color: clLightestBlue; Deep: [deDrom];),
    // Crown
    (Symbol: '^'; ItemType: itHeadgear; SlotType: stHead; MaxStack: 1; MaxDurability: 50; Level: 10; Defense: (Min: 10; Max: 12);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 2800; Color: clLightestBlue; Deep: [deDrom];),

    // Leather Gloves
    (Symbol: '%'; ItemType: itHands; SlotType: stHands; MaxStack: 1; MaxDurability: 10; Level: 1; Defense: (Min: 1; Max: 2);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 100; Color: clWhite; Deep: [deDark_Wood];),
    // Hide Gloves
    (Symbol: '%'; ItemType: itHands; SlotType: stHands; MaxStack: 1; MaxDurability: 15; Level: 2; Defense: (Min: 3; Max: 4);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 150; Color: clWhite; Deep: [deDark_Wood];),
    // Kobold Gloves
    (Symbol: '%'; ItemType: itHands; SlotType: stHands; MaxStack: 1; MaxDurability: 22; Level: 3; Defense: (Min: 5; Max: 6);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 200; Color: clWhite; Deep: [deGray_Cave];),
    // Chain Gloves
    (Symbol: '%'; ItemType: itHands; SlotType: stHands; MaxStack: 1; MaxDurability: 29; Level: 4; Defense: (Min: 7; Max: 8);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 250; Color: clWhite; Deep: [deGray_Cave];),
    // Etched Gloves
    (Symbol: '%'; ItemType: itHands; SlotType: stHands; MaxStack: 1; MaxDurability: 35; Level: 5; Defense: (Min: 9; Max: 10);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 300; Color: clWhite; Deep: [deDeep_Cave];),
    // Heavy Gloves
    (Symbol: '%'; ItemType: itHands; SlotType: stHands; MaxStack: 1; MaxDurability: 40; Level: 6; Defense: (Min: 11; Max: 12);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 350; Color: clWhite; Deep: [deDeep_Cave];),
    // Battle Gauntlets
    (Symbol: '%'; ItemType: itHands; SlotType: stHands; MaxStack: 1; MaxDurability: 45; Level: 7; Defense: (Min: 13; Max: 14);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 400; Color: clWhite; Deep: [deBlood_Cave];),
    // War Gauntlets
    (Symbol: '%'; ItemType: itHands; SlotType: stHands; MaxStack: 1; MaxDurability: 50; Level: 8; Defense: (Min: 15; Max: 16);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 450; Color: clWhite; Deep: [deBlood_Cave];),
    // Troll Gauntlets
    (Symbol: '%'; ItemType: itHands; SlotType: stHands; MaxStack: 1; MaxDurability: 55; Level: 9; Defense: (Min: 17; Max: 18);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 500; Color: clWhite; Deep: [deDrom];),
    // Plated Gauntlets
    (Symbol: '%'; ItemType: itHands; SlotType: stHands; MaxStack: 1; MaxDurability: 60; Level: 10; Defense: (Min: 19; Max: 20);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 600; Color: clWhite; Deep: [deDrom];),

    // Shoes (Boots #1)
    (Symbol: ';'; ItemType: itFeet; SlotType: stFeet; MaxStack: 1; MaxDurability: 15; Level: 1; Defense: (Min: 1; Max: 3);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 200; Color: clWhite; Deep: [deDark_Wood];),
    // Leather Boots (Boots #2)
    (Symbol: ';'; ItemType: itFeet; SlotType: stFeet; MaxStack: 1; MaxDurability: 20; Level: 2; Defense: (Min: 4; Max: 6);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 250; Color: clWhite; Deep: [deDark_Wood];),
    // Mesh Boots (Boots #3)
    (Symbol: ';'; ItemType: itFeet; SlotType: stFeet; MaxStack: 1; MaxDurability: 30; Level: 3; Defense: (Min: 7; Max: 9);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 300; Color: clWhite; Deep: [deGray_Cave];),
    // Heavy Boots (Boots #4)
    (Symbol: ';'; ItemType: itFeet; SlotType: stFeet; MaxStack: 1; MaxDurability: 40; Level: 4; Defense: (Min: 10; Max: 12);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 400; Color: clWhite; Deep: [deGray_Cave];),
    // Greaves (Boots #5)
    (Symbol: ';'; ItemType: itFeet; SlotType: stFeet; MaxStack: 1; MaxDurability: 50; Level: 5; Defense: (Min: 13; Max: 15);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 500; Color: clWhite; Deep: [deDeep_Cave];),
    // Boneweave Boots (Boots #6)
    (Symbol: ';'; ItemType: itFeet; SlotType: stFeet; MaxStack: 1; MaxDurability: 60; Level: 6; Defense: (Min: 16; Max: 18);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 650; Color: clWhite; Deep: [deDeep_Cave];),
    // Chain Boots (Boots #7)
    (Symbol: ';'; ItemType: itFeet; SlotType: stFeet; MaxStack: 1; MaxDurability: 70; Level: 7; Defense: (Min: 19; Max: 21);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 700; Color: clWhite; Deep: [deBlood_Cave];),
    // War Boots (Boots #8)
    (Symbol: ';'; ItemType: itFeet; SlotType: stFeet; MaxStack: 1; MaxDurability: 80; Level: 8; Defense: (Min: 22; Max: 24);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 850; Color: clWhite; Deep: [deBlood_Cave];),
    // Battle Boots (Boots #9)
    (Symbol: ';'; ItemType: itFeet; SlotType: stFeet; MaxStack: 1; MaxDurability: 90; Level: 9; Defense: (Min: 25; Max: 27);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 900; Color: clWhite; Deep: [deDrom];),
    // Plate Boots (Boots #10)
    (Symbol: ';'; ItemType: itFeet; SlotType: stFeet; MaxStack: 1; MaxDurability: 100; Level: 10; Defense: (Min: 28; Max: 30);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1000; Color: clWhite; Deep: [deDrom];),

    // Buckler (Shield #1)
    (Symbol: '+'; ItemType: itShield; SlotType: stOffHand; MaxStack: 1; MaxDurability: 25; Level: 1; Defense: (Min: 3; Max: 6);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 120; Color: clLightBlue; Deep: [deDark_Wood];),
    // Targe Shield (Shield #2)
    (Symbol: '+'; ItemType: itShield; SlotType: stOffHand; MaxStack: 1; MaxDurability: 30; Level: 2; Defense: (Min: 7; Max: 10);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 200; Color: clLightBlue; Deep: [deDark_Wood];),
    // Small Shield (Shield #3)
    (Symbol: '+'; ItemType: itShield; SlotType: stOffHand; MaxStack: 1; MaxDurability: 35; Level: 3; Defense: (Min: 10; Max: 12);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 275; Color: clLightBlue; Deep: [deGray_Cave];),
    // Kite Shield (Shield #4)
    (Symbol: '+'; ItemType: itShield; SlotType: stOffHand; MaxStack: 1; MaxDurability: 40; Level: 4; Defense: (Min: 13; Max: 15);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 380; Color: clLightBlue; Deep: [deGray_Cave];),
    // Bone Shield (Shield #5)
    (Symbol: '+'; ItemType: itShield; SlotType: stOffHand; MaxStack: 1; MaxDurability: 45; Level: 5; Defense: (Min: 16; Max: 18);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 450; Color: clLightBlue; Deep: [deDeep_Cave];),
    // Heater Shield (Shield #6)
    (Symbol: '+'; ItemType: itShield; SlotType: stOffHand; MaxStack: 1; MaxDurability: 50; Level: 6; Defense: (Min: 19; Max: 21);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 590; Color: clLightBlue; Deep: [deDeep_Cave];),
    // Heavy Shield (Shield #7)
    (Symbol: '+'; ItemType: itShield; SlotType: stOffHand; MaxStack: 1; MaxDurability: 60; Level: 7; Defense: (Min: 22; Max: 24);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 740; Color: clLightBlue; Deep: [deBlood_Cave];),
    // Large Shield (Shield #8)
    (Symbol: '+'; ItemType: itShield; SlotType: stOffHand; MaxStack: 1; MaxDurability: 75; Level: 8; Defense: (Min: 25; Max: 27);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 860; Color: clLightBlue; Deep: [deBlood_Cave];),
    // Tower Shield (Shield #9)
    (Symbol: '+'; ItemType: itShield; SlotType: stOffHand; MaxStack: 1; MaxDurability: 100; Level: 9; Defense: (Min: 28; Max: 30);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 920; Color: clLightBlue; Deep: [deDrom];),
    // Gothic Shield (Shield #10)
    (Symbol: '+'; ItemType: itShield; SlotType: stOffHand; MaxStack: 1; MaxDurability: 150; Level: 10; Defense: (Min: 31; Max: 35);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;)); Price: 1000; Color: clLightBlue; Deep: [deDrom];),

    // Rusty Sword
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1; MaxDurability: 30; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 3; Max: 5;); MaxDamage: (Min: 6; Max: 9;)); Price: 185; Color: clDarkRed; Deep: [deDark_Wood];),
    // Short Sword
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1; MaxDurability: 35; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 5; Max: 7;); MaxDamage: (Min: 10; Max: 12;)); Price: 210; Color: clWhite; Deep: [deDark_Wood];),
    // Hatchet
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1; MaxDurability: 30; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 3; Max: 5;); MaxDamage: (Min: 6; Max: 8;)); Price: 165; Color: clDarkRed; Deep: [deDark_Wood];),
    // Battle Axe
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1; MaxDurability: 35; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 7; Max: 9;); MaxDamage: (Min: 11; Max: 12;)); Price: 195; Color: clDarkRed; Deep: [deDark_Wood];),
    // Short Spear
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1; MaxDurability: 30; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 1; Max: 3;); MaxDamage: (Min: 4; Max: 5;)); Price: 150; Color: clDarkRed; Deep: [deDark_Wood];),
    // Spear
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1; MaxDurability: 35; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 2; Max: 4;); MaxDamage: (Min: 5; Max: 7;)); Price: 180; Color: clDarkRed; Deep: [deDark_Wood];),
    // Slag Hammer
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1; MaxDurability: 30; Level: 1; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 2; Max: 4;); MaxDamage: (Min: 5; Max: 6;)); Price: 175; Color: clDarkRed; Deep: [deDark_Wood];),
    // Spiked Cudgel
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1; MaxDurability: 35; Level: 2; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 3; Max: 5;); MaxDamage: (Min: 7; Max: 9;)); Price: 220; Color: clDarkRed; Deep: [deDark_Wood];),

    /// / == Gray Cave == ////

    // Broad Sword
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1; MaxDurability: 40; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 11; Max: 14;); MaxDamage: (Min: 19; Max: 21;)); Price: 345; Color: clDarkRed; Deep: [deGray_Cave];),
    // Long Sword
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1; MaxDurability: 45; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 14; Max: 18;); MaxDamage: (Min: 23; Max: 26;)); Price: 385; Color: clDarkRed; Deep: [deGray_Cave];),
    // Meat Axe
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1; MaxDurability: 40; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 12; Max: 14;); MaxDamage: (Min: 16; Max: 19;)); Price: 330; Color: clDarkRed; Deep: [deGray_Cave];),
    // Flesh Tearer
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1; MaxDurability: 45; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 16; Max: 18;); MaxDamage: (Min: 21; Max: 24;)); Price: 355; Color: clDarkRed; Deep: [deGray_Cave];),
    // Javelin
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1; MaxDurability: 40; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 4; Max: 6;); MaxDamage: (Min: 10; Max: 12;)); Price: 320; Color: clDarkRed; Deep: [deGray_Cave];),
    // Fuscina
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1; MaxDurability: 45; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 5; Max: 7;); MaxDamage: (Min: 16; Max: 19;)); Price: 360; Color: clDarkRed; Deep: [deGray_Cave];),
    // Warhammer
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1; MaxDurability: 40; Level: 3; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 6; Max: 9;); MaxDamage: (Min: 11; Max: 13;)); Price: 345; Color: clDarkRed; Deep: [deGray_Cave];),
    // War Mace
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1; MaxDurability: 45; Level: 4; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 8; Max: 10;); MaxDamage: (Min: 17; Max: 19;)); Price: 410; Color: clDarkRed; Deep: [deGray_Cave];),

    /// / == Deep Cave == ////

    // Moon Blade
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1; MaxDurability: 50; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 17; Max: 20;); MaxDamage: (Min: 27; Max: 31;)); Price: 570; Color: clDarkRed; Deep: [deDeep_Cave];),
    // Scimitar
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1; MaxDurability: 55; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 21; Max: 25;); MaxDamage: (Min: 35; Max: 38;)); Price: 600; Color: clDarkRed; Deep: [deDeep_Cave];),
    // War Axe
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1; MaxDurability: 50; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 21; Max: 24;); MaxDamage: (Min: 26; Max: 30;)); Price: 560; Color: clDarkRed; Deep: [deDeep_Cave];),
    // Dark Axe
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1; MaxDurability: 55; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 24; Max: 27;); MaxDamage: (Min: 30; Max: 33;)); Price: 585; Color: clDarkRed; Deep: [deDeep_Cave];),
    // War Spear
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1; MaxDurability: 50; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 6; Max: 10;); MaxDamage: (Min: 25; Max: 28;)); Price: 540; Color: clDarkRed; Deep: [deDeep_Cave];),
    // Harpoon
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1; MaxDurability: 55; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 7; Max: 11;); MaxDamage: (Min: 35; Max: 39;)); Price: 575; Color: clDarkRed; Deep: [deDeep_Cave];),
    // Flanged Mace
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1; MaxDurability: 50; Level: 5; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 11; Max: 14;); MaxDamage: (Min: 22; Max: 25;)); Price: 590; Color: clDarkRed; Deep: [deDeep_Cave];),
    // War Gavel
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1; MaxDurability: 55; Level: 6; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 15; Max: 18;); MaxDamage: (Min: 30; Max: 33;)); Price: 650; Color: clDarkRed; Deep: [deDeep_Cave];),

    /// / == Blood Cave == ////

    // Bastard Sword
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1; MaxDurability: 60; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 24; Max: 27;); MaxDamage: (Min: 39; Max: 43;)); Price: 770; Color: clDarkRed; Deep: [deBlood_Cave];),
    // Great Sword
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1; MaxDurability: 65; Level: 8; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 27; Max: 30;); MaxDamage: (Min: 44; Max: 48;)); Price: 820; Color: clDarkRed; Deep: [deBlood_Cave];),
    // Berserker Axe
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1; MaxDurability: 60; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 28; Max: 32;); MaxDamage: (Min: 36; Max: 38;)); Price: 750; Color: clDarkRed; Deep: [deDeep_Cave];),
    // Marauder Axe
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1; MaxDurability: 65; Level: 8; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 32; Max: 35;); MaxDamage: (Min: 41; Max: 45;)); Price: 885; Color: clDarkRed; Deep: [deBlood_Cave];),
    // Silvan Whisper
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1; MaxDurability: 60; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 8; Max: 11;); MaxDamage: (Min: 50; Max: 52;)); Price: 720; Color: clDarkRed; Deep: [deBlood_Cave];),
    // Impaler
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1; MaxDurability: 65; Level: 8; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 9; Max: 11;); MaxDamage: (Min: 65; Max: 67;)); Price: 790; Color: clDarkRed; Deep: [deBlood_Cave];),
    // Barbarous Mace
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1; MaxDurability: 60; Level: 7; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 19; Max: 22;); MaxDamage: (Min: 38; Max: 41;)); Price: 780; Color: clDarkRed; Deep: [deBlood_Cave];),
    // Adept Hammer
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1; MaxDurability: 65; Level: 8; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 24; Max: 27;); MaxDamage: (Min: 48; Max: 51;)); Price: 850; Color: clDarkRed; Deep: [deBlood_Cave];),

    /// / == Drom == ////

    // Rune Sword
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1; MaxDurability: 70; Level: 9; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 30; Max: 33;); MaxDamage: (Min: 50; Max: 54;)); Price: 930; Color: clDarkRed; Deep: [deDrom];),
    // Troll Slayer,
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1; MaxDurability: 75; Level: 10; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 33; Max: 37;); MaxDamage: (Min: 55; Max: 60;)); Price: 990; Color: clDarkRed; Deep: [deDrom];),
    // Chopper
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1; MaxDurability: 70; Level: 9; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 36; Max: 39;); MaxDamage: (Min: 45; Max: 49;)); Price: 940; Color: clDarkRed; Deep: [deDrom];),
    // Demon Axe,
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1; MaxDurability: 75; Level: 10; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 40; Max: 45;); MaxDamage: (Min: 50; Max: 55;)); Price: 980; Color: clDarkRed; Deep: [deDrom];),
    // Soul Reaver
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1; MaxDurability: 70; Level: 9; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 10; Max: 12;); MaxDamage: (Min: 80; Max: 89;)); Price: 940; Color: clDarkRed; Deep: [deDrom];),
    // Honed Spear,
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1; MaxDurability: 75; Level: 10; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 10; Max: 15;); MaxDamage: (Min: 90; Max: 100;)); Price: 970; Color: clDarkRed; Deep: [deDrom];),
    // War Maul
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1; MaxDurability: 70; Level: 9; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 30; Max: 35;); MaxDamage: (Min: 65; Max: 69;)); Price: 950; Color: clDarkRed; Deep: [deDrom];),
    // Doom Hammer
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1; MaxDurability: 75; Level: 10; Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 36; Max: 40;); MaxDamage: (Min: 70; Max: 80;)); Price: 1000; Color: clDarkRed; Deep: [deDrom];)

    );

class function ItemBase.GetItem(const Value: Int): TItemBase;
begin
  Result := Base[TItemEnum(Value)];
end;

class function ItemBase.GetItem(const Value: Item): TItemBase;
begin
  Result := Base[TItemEnum(Value.ItemID)];
end;

class function ItemBase.GetItem(const Value: TItemEnum): TItemBase;
begin
  Result := Base[Value];
end;

class function ItemBase.Count: UInt;
begin
  Result := Ord(Length(Base));
end;

end.
