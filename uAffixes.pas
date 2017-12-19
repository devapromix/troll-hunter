unit uAffixes;

interface

uses uCreature, uItem, uBeaRLibItemsCommon;

type
  TSetOfItem = set of TItemType;

  TSuffixBase = record
    Level: TMinMax;
    Price: Word;
    Occurence: TSetOfItem;
    MaxDurability: TMinMax;
    Defense: TMinMax;
    Damage: TBaseDamage;
    Life: TMinMax;
    Mana: TMinMax;
    Strength: TMinMax;
    Dexterity: TMinMax;
    Willpower: TMinMax;
    Perception: TMinMax;
    Value: TMinMax;
    PrmValue: Byte;
    Rare: Boolean;
    Effects: TEffects;
  end;

type
  TSuffixEnum = (
    // None
    None,
    // Vision I
    of_Radiance,
    // Potions, Antidote, Extracts
    of_Healing, of_Minor_Mana, of_Mana, of_Rejuvenation, of_Antidote,
    of_Soothing, of_Poultice, of_the_Troll, of_the_Unicorn,
    // Oil I - V
    of_Blacksmith, of_Mastery, of_Sharpness, of_Fortitude, of_Permanence,
    // Life I - VII
    of_Life1, of_Life2, of_Life3, of_Life4, of_Life5, of_Life6, of_Life7,
    // Mana I - VII
    of_Mana1, of_Mana2, of_Mana3, of_Mana4, of_Mana5, of_Mana6, of_Mana7,
    // Life and Mana I - VII
    of_Atr1, of_Atr2, of_Atr3, of_Atr4, of_Atr5, of_Atr6, of_Atr7,
    // Defense I - VII
    of_Defense1, of_Defense2, of_Defense3, of_Defense4, of_Defense5,
    of_Defense6, of_Defense7,
    // Damage I - VII
    of_Damage1, of_Damage2, of_Damage3, of_Damage4, of_Damage5, of_Damage6,
    of_Damage7,
    // Durability I - VII
    of_Craftmanship, of_Durability, of_Sturdiness, of_Structure, of_Endurance,
    of_the_Ages, of_Permanance,
    // Strength I - VII
    of_Strength1, of_Strength2, of_Strength3, of_Strength4, of_Strength5,
    of_Strength6, of_Strength7,
    // Dexterity I - VII
    of_the_Mongoose, of_the_Fox, of_the_Lynx, of_the_Falcon, of_the_Panther,
    of_the_Leopard, of_the_Jaguar,
    // Willpower I - VII
    of_Willpower1, of_Willpower2, of_Willpower3, of_Willpower4, of_Willpower5,
    of_Willpower6, of_Willpower7,
    // Perception I - VII
    of_Perception1, of_Perception2, of_Perception3, of_Perception4,
    of_Perception5, of_Perception6, of_Perception7,
    // Additional All Attributes I - VII
    of_the_Sky, of_the_Meteor, of_the_Comet, of_the_Heavens, of_the_Galaxy,
    of_the_Universe, of_the_Infinite,
    // Mage I - VII
    of_Magic1, of_Magic2, of_Magic3, of_Magic4, of_Magic5, of_Magic6, of_Magic7
    //
    );

  // of the Greatwolf
  //

const
  DefenseSuffixes = [of_Defense1 .. of_Defense7];
  DamageSuffixes = [of_Damage1 .. of_Damage7];
  DurabilitySuffixes = [of_Craftmanship .. of_Permanance];

const
  SuffixBase: array [TSuffixEnum] of TSuffixBase = (
    // None
    (),
    // of Radiance (Vision I)
    (Level: (Min: 1; Max: 15); Price: 1000; Occurence: JewelryTypeItems),

    // of Healing (Flask I - XII)
    (Level: (Min: 1; Max: 15); Price: 0; Occurence: FlaskTypeItems;
    Value: (Min: 1; Max: 9); Effects: [efLife];),
    // of Minor Mana (Flask I)
    (Level: (Min: 1; Max: 1); Price: 0; Occurence: FlaskTypeItems;
    Effects: [efMana];),
    // of Mana (Flask I - XII)
    (Level: (Min: 1; Max: 15); Price: 0; Occurence: FlaskTypeItems;
    Value: (Min: 1; Max: 9); Effects: [efMana];),
    // of Rejuvenation (Flask I - XII)
    (Level: (Min: 1; Max: 15); Price: 35; Occurence: FlaskTypeItems;
    Value: (Min: 1; Max: 9); Effects: [efLife, efMana];),
    // of Antidote (Flask I - XII)
    (Level: (Min: 1; Max: 15); Price: 65; Occurence: FlaskTypeItems;
    Value: (Min: 1; Max: 9); Effects: [efCurePoison];),
    // of Soothing (Flask I - XII)
    (Level: (Min: 1; Max: 15); Price: 20; Occurence: FlaskTypeItems;
    Value: (Min: 1; Max: 9); Effects: [efLife, efMana, efFood];),
    // of Poultice (Flask I - XII)
    (Level: (Min: 1; Max: 15); Price: 45; Occurence: FlaskTypeItems;
    Value: (Min: 1; Max: 9); Effects: [efLife, efMana, efCurePoison];),
    // of the Troll (Flask I - XII)
    (Level: (Min: 1; Max: 15); Price: 2000; Occurence: FlaskTypeItems;
    PrmValue: 1; Rare: True; Effects: [efPrmLife];),
    // of the Unicorn (Flask I - XII)
    (Level: (Min: 1; Max: 15); Price: 2000; Occurence: FlaskTypeItems;
    PrmValue: 1; Rare: True; Effects: [efPrmMana];),

    // of Blacksmith (Oil I)
    (Level: (Min: 1; Max: 3); Price: 100; Occurence: FlaskTypeItems;
    Value: (Min: 5; Max: 9); Effects: [efRepair];),
    // of Mastery (Oil II)
    (Level: (Min: 2; Max: 6); Price: 200; Occurence: FlaskTypeItems;
    Value: (Min: 10; Max: 14); Effects: [efRepair];),
    // of Sharpness (Oil III)
    (Level: (Min: 3; Max: 9); Price: 300; Occurence: FlaskTypeItems;
    Value: (Min: 15; Max: 19); Effects: [efRepair];),
    // of Fortitude (Oil VI)
    (Level: (Min: 4; Max: 12); Price: 400; Occurence: FlaskTypeItems;
    Value: (Min: 20; Max: 24); Effects: [efRepair];),
    // of Permanence (Oil V)
    (Level: (Min: 5; Max: 15); Price: 500; Occurence: FlaskTypeItems;
    Value: (Min: 25; Max: 30); Effects: [efRepair];),

    // (Life I)
    (Level: (Min: 1; Max: 3); Price: 100; Occurence: DefenseTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Life: (Min: 1; Max: 4);),
    // (Life II)
    (Level: (Min: 2; Max: 5); Price: 200; Occurence: DefenseTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Life: (Min: 5; Max: 8);),
    // (Life III)
    (Level: (Min: 3; Max: 7); Price: 300; Occurence: DefenseTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Life: (Min: 9; Max: 12);),
    // (Life IV)
    (Level: (Min: 4; Max: 9); Price: 400; Occurence: DefenseTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Life: (Min: 13; Max: 16);),
    // (Life V)
    (Level: (Min: 5; Max: 11); Price: 500; Occurence: DefenseTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Life: (Min: 17; Max: 20);),
    // (Life VI)
    (Level: (Min: 6; Max: 13); Price: 750; Occurence: DefenseTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Life: (Min: 21; Max: 25);),
    // (Life VII)
    (Level: (Min: 7; Max: 15); Price: 1000; Occurence: DefenseTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Life: (Min: 26; Max: 30);),

    // (Mana I)
    (Level: (Min: 1; Max: 3); Price: 100; Occurence: DefenseTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Life: (Min: 0; Max: 0); Mana: (Min: 1; Max: 4);),
    // (Mana II)
    (Level: (Min: 2; Max: 5); Price: 200; Occurence: DefenseTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Life: (Min: 0; Max: 0); Mana: (Min: 5; Max: 8);),
    // (Mana III)
    (Level: (Min: 3; Max: 7); Price: 300; Occurence: DefenseTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Life: (Min: 0; Max: 0); Mana: (Min: 9; Max: 12);),
    // (Mana IV)
    (Level: (Min: 4; Max: 9); Price: 400; Occurence: DefenseTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Life: (Min: 0; Max: 0); Mana: (Min: 13; Max: 16);),
    // (Mana V)
    (Level: (Min: 5; Max: 11); Price: 500; Occurence: DefenseTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Life: (Min: 0; Max: 0); Mana: (Min: 17; Max: 20);),
    // (Mana VI)
    (Level: (Min: 6; Max: 13); Price: 750; Occurence: DefenseTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Life: (Min: 0; Max: 0); Mana: (Min: 21; Max: 25);),
    // (Mana VII)
    (Level: (Min: 7; Max: 15); Price: 1000; Occurence: DefenseTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Life: (Min: 0; Max: 0); Mana: (Min: 26; Max: 30);),

    // (Life and Mana I)
    (Level: (Min: 1; Max: 3); Price: 150; Occurence: DefenseTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Life: (Min: 1; Max: 3); Mana: (Min: 1; Max: 3);),
    // (Life and Mana II)
    (Level: (Min: 2; Max: 5); Price: 300; Occurence: DefenseTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Life: (Min: 4; Max: 6); Mana: (Min: 4; Max: 6);),
    // (Life and Mana III)
    (Level: (Min: 3; Max: 7); Price: 500; Occurence: DefenseTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Life: (Min: 7; Max: 9); Mana: (Min: 7; Max: 9);),
    // (Life and Mana IV)
    (Level: (Min: 4; Max: 9); Price: 700; Occurence: DefenseTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Life: (Min: 10; Max: 12); Mana: (Min: 10; Max: 12);),
    // (Life and Mana V)
    (Level: (Min: 5; Max: 11); Price: 1000; Occurence: DefenseTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Life: (Min: 13; Max: 17); Mana: (Min: 13; Max: 17);),
    // (Life and Mana VI)
    (Level: (Min: 6; Max: 13); Price: 1500; Occurence: DefenseTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Life: (Min: 18; Max: 23); Mana: (Min: 18; Max: 23);),
    // (Life and Mana VII)
    (Level: (Min: 7; Max: 15); Price: 2000; Occurence: DefenseTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 0; Max: 0;); MaxDamage: (Min: 0; Max: 0;));
    Life: (Min: 24; Max: 30); Mana: (Min: 23; Max: 30);),

    // (Defense I)
    (Level: (Min: 1; Max: 3); Price: 100; Occurence: DefenseTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 1; Max: 4);),
    // (Defense II)
    (Level: (Min: 2; Max: 5); Price: 200; Occurence: DefenseTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 5; Max: 8);),
    // (Defense III)
    (Level: (Min: 3; Max: 7); Price: 300; Occurence: DefenseTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 9; Max: 12);),
    // (Defense IV)
    (Level: (Min: 4; Max: 9); Price: 400; Occurence: DefenseTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 11; Max: 16);),
    // (Defense V)
    (Level: (Min: 5; Max: 11); Price: 500; Occurence: DefenseTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 17; Max: 20);),
    // (Defense VI)
    (Level: (Min: 6; Max: 13); Price: 750; Occurence: DefenseTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 20; Max: 25);),
    // (Defense VII)
    (Level: (Min: 7; Max: 15); Price: 1000; Occurence: DefenseTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 25; Max: 30);),

    // (Damage I)
    (Level: (Min: 1; Max: 3); Price: 250; Occurence: DamageTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 1; Max: 3); MaxDamage: (Min: 3; Max: 5));),
    // (Damage II)
    (Level: (Min: 2; Max: 5); Price: 500; Occurence: DamageTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 3; Max: 6); MaxDamage: (Min: 6; Max: 10));),
    // (Damage III)
    (Level: (Min: 3; Max: 7); Price: 800; Occurence: DamageTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 6; Max: 9); MaxDamage: (Min: 9; Max: 15));),
    // (Damage IV)
    (Level: (Min: 4; Max: 9); Price: 1000; Occurence: DamageTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 9; Max: 12); MaxDamage: (Min: 12; Max: 20));),
    // (Damage V)
    (Level: (Min: 5; Max: 11); Price: 1500; Occurence: DamageTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 12; Max: 15); MaxDamage: (Min: 15; Max: 25));),
    // (Damage VI)
    (Level: (Min: 6; Max: 13); Price: 2000; Occurence: DamageTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 15; Max: 18); MaxDamage: (Min: 18; Max: 30));),
    // (Damage VII)
    (Level: (Min: 7; Max: 15); Price: 2500; Occurence: DamageTypeItems;
    MaxDurability: (Min: 0; Max: 0); Defense: (Min: 0; Max: 0);
    Damage: (MinDamage: (Min: 18; Max: 20); MaxDamage: (Min: 21; Max: 35));),

    // of Craftmanship (Durability I)
    (Level: (Min: 1; Max: 3); Price: 100; Occurence: SmithTypeItems;
    MaxDurability: (Min: 10; Max: 20);),
    // of Durability (Durability II)
    (Level: (Min: 2; Max: 5); Price: 200; Occurence: SmithTypeItems;
    MaxDurability: (Min: 20; Max: 30);),
    // of Sturdiness (Durability III)
    (Level: (Min: 3; Max: 7); Price: 300; Occurence: SmithTypeItems;
    MaxDurability: (Min: 30; Max: 40);),
    // of Structure (Durability IV)
    (Level: (Min: 4; Max: 9); Price: 400; Occurence: SmithTypeItems;
    MaxDurability: (Min: 40; Max: 50);),
    // of Endurance (Durability V)
    (Level: (Min: 5; Max: 11); Price: 500; Occurence: SmithTypeItems;
    MaxDurability: (Min: 50; Max: 60);),
    // of the Ages (Durability VI)
    (Level: (Min: 6; Max: 13); Price: 750; Occurence: SmithTypeItems;
    MaxDurability: (Min: 60; Max: 70);),
    // of Permanance (Durability VII)
    (Level: (Min: 7; Max: 15); Price: 1000; Occurence: SmithTypeItems;
    MaxDurability: (Min: 70; Max: 80);),

    // (Strength I)
    (Level: (Min: 1; Max: 3); Price: 200; Occurence: SmithTypeItems;
    Strength: (Min: 1; Max: 2);),
    // (Strength II)
    (Level: (Min: 2; Max: 5); Price: 400; Occurence: SmithTypeItems;
    Strength: (Min: 3; Max: 5);),
    // (Strength III)
    (Level: (Min: 3; Max: 7); Price: 600; Occurence: SmithTypeItems;
    Strength: (Min: 6; Max: 9);),
    // (Strength IV)
    (Level: (Min: 4; Max: 9); Price: 800; Occurence: SmithTypeItems;
    Strength: (Min: 10; Max: 14);),
    // (Strength V)
    (Level: (Min: 5; Max: 11); Price: 1000; Occurence: SmithTypeItems;
    Strength: (Min: 15; Max: 20);),
    // (Strength VI)
    (Level: (Min: 6; Max: 13); Price: 1500; Occurence: SmithTypeItems;
    Strength: (Min: 21; Max: 25);),
    // (Strength VII)
    (Level: (Min: 7; Max: 15); Price: 2000; Occurence: SmithTypeItems;
    Strength: (Min: 26; Max: 30);),

    // of the Mongoose (Dexterity I)
    (Level: (Min: 1; Max: 3); Price: 200; Occurence: SmithTypeItems;
    Dexterity: (Min: 1; Max: 2);),
    // of the Fox (Dexterity II)
    (Level: (Min: 2; Max: 5); Price: 400; Occurence: SmithTypeItems;
    Dexterity: (Min: 3; Max: 5);),
    // of the Lynx (Dexterity III)
    (Level: (Min: 3; Max: 7); Price: 600; Occurence: SmithTypeItems;
    Dexterity: (Min: 6; Max: 9);),
    // of the Falcon (Dexterity IV)
    (Level: (Min: 4; Max: 9); Price: 800; Occurence: SmithTypeItems;
    Dexterity: (Min: 10; Max: 14);),
    // of the Panther (Dexterity V)
    (Level: (Min: 5; Max: 11); Price: 1000; Occurence: SmithTypeItems;
    Dexterity: (Min: 15; Max: 20);),
    // of the Leopard (Dexterity VI)
    (Level: (Min: 6; Max: 13); Price: 1500; Occurence: SmithTypeItems;
    Dexterity: (Min: 21; Max: 25);),
    // of the Jaguar (Dexterity VII)
    (Level: (Min: 7; Max: 15); Price: 2000; Occurence: SmithTypeItems;
    Dexterity: (Min: 26; Max: 30);),

    // (Willpower I)
    (Level: (Min: 1; Max: 3); Price: 200; Occurence: SmithTypeItems;
    Willpower: (Min: 1; Max: 2);),
    // (Willpower II)
    (Level: (Min: 2; Max: 5); Price: 400; Occurence: SmithTypeItems;
    Willpower: (Min: 3; Max: 5);),
    // (Willpower III)
    (Level: (Min: 3; Max: 7); Price: 600; Occurence: SmithTypeItems;
    Willpower: (Min: 6; Max: 9);),
    // (Willpower IV)
    (Level: (Min: 4; Max: 9); Price: 800; Occurence: SmithTypeItems;
    Willpower: (Min: 10; Max: 14);),
    // (Willpower V)
    (Level: (Min: 5; Max: 11); Price: 1000; Occurence: SmithTypeItems;
    Willpower: (Min: 15; Max: 20);),
    // (Willpower VI)
    (Level: (Min: 6; Max: 13); Price: 1500; Occurence: SmithTypeItems;
    Willpower: (Min: 21; Max: 25);),
    // (Willpower VII)
    (Level: (Min: 7; Max: 15); Price: 2000; Occurence: SmithTypeItems;
    Willpower: (Min: 26; Max: 30);),

    // (Perception I)
    (Level: (Min: 1; Max: 3); Price: 200; Occurence: SmithTypeItems;
    Perception: (Min: 1; Max: 2);),
    // (Perception II)
    (Level: (Min: 2; Max: 5); Price: 400; Occurence: SmithTypeItems;
    Perception: (Min: 3; Max: 5);),
    // (Perception III)
    (Level: (Min: 3; Max: 7); Price: 600; Occurence: SmithTypeItems;
    Perception: (Min: 6; Max: 9);),
    // (Perception IV)
    (Level: (Min: 4; Max: 9); Price: 800; Occurence: SmithTypeItems;
    Perception: (Min: 10; Max: 14);),
    // (Perception V)
    (Level: (Min: 5; Max: 11); Price: 1000; Occurence: SmithTypeItems;
    Perception: (Min: 15; Max: 20);),
    // (Perception VI)
    (Level: (Min: 6; Max: 13); Price: 1500; Occurence: SmithTypeItems;
    Perception: (Min: 21; Max: 25);),
    // (Perception VII)
    (Level: (Min: 7; Max: 15); Price: 2000; Occurence: SmithTypeItems;
    Perception: (Min: 26; Max: 30);),

    // of the Sky (Additional All Attributes I)
    (Level: (Min: 1; Max: 3); Price: 400; Occurence: JewelryTypeItems;),
    // of the Meteor (Additional All Attributes II)
    (Level: (Min: 2; Max: 5); Price: 800; Occurence: JewelryTypeItems;),
    // of the Comet (Additional All Attributes III)
    (Level: (Min: 3; Max: 7); Price: 1200; Occurence: JewelryTypeItems;),
    // of the Heavens (Additional All Attributes IV)
    (Level: (Min: 4; Max: 9); Price: 1600; Occurence: JewelryTypeItems;),
    // of the Galaxy (Additional All Attributes V)
    (Level: (Min: 5; Max: 11); Price: 2000; Occurence: JewelryTypeItems;),
    // of the Universe (Additional All Attributes VI)
    (Level: (Min: 6; Max: 13); Price: 2500; Occurence: JewelryTypeItems;),
    // of the Infinite (Additional All Attributes VII)
    (Level: (Min: 7; Max: 15); Price: 3000; Occurence: JewelryTypeItems;),

    // (Staves and Wands I)
    (Level: (Min: 1; Max: 3); Price: 600; Occurence: MagicWeaponTypeItems;
    Mana: (Min: 5; Max: 10);),
    // (Staves and Wands II)
    (Level: (Min: 2; Max: 5); Price: 900; Occurence: MagicWeaponTypeItems;
    Mana: (Min: 15; Max: 20);),
    // (Staves and Wands III)
    (Level: (Min: 3; Max: 7); Price: 1200; Occurence: MagicWeaponTypeItems;
    Mana: (Min: 25; Max: 30);),
    // (Staves and Wands IV)
    (Level: (Min: 4; Max: 9); Price: 1500; Occurence: MagicWeaponTypeItems;
    Mana: (Min: 35; Max: 40);),
    // (Staves and Wands V)
    (Level: (Min: 5; Max: 11); Price: 1800; Occurence: MagicWeaponTypeItems;
    Mana: (Min: 45; Max: 50);),
    // (Staves and Wands VI)
    (Level: (Min: 6; Max: 13); Price: 2100; Occurence: MagicWeaponTypeItems;
    Mana: (Min: 55; Max: 60);),
    // (Staves and Wands VII)
    (Level: (Min: 7; Max: 15); Price: 2400; Occurence: MagicWeaponTypeItems;
    Mana: (Min: 65; Max: 70);)

    //
    );

type
  TAffixes = class(TObject)
  private
    FSuffixName: array [TSuffixEnum] of string;
  public
    constructor Create();
    function GetSuffixName(const SuffixEnum: TSuffixEnum): string;
    procedure DoSuffix(var AItem: Item);
    procedure DoCraft(const Effect: TEffect; const Index: Byte);
    function Amount: Byte;
  end;

var
  Affixes: TAffixes = nil;

implementation

uses SysUtils, TypInfo, Math, uTerminal, uGame;

function TAffixes.Amount: Byte;
begin
  Result := Ord(High(TSuffixEnum)) + 1;
end;

constructor TAffixes.Create();
var
  I: TSuffixEnum;
  P: Pointer;
begin
  P := TypeInfo(TSuffixEnum);
  for I := Low(TSuffixEnum) to High(TSuffixEnum) do
    FSuffixName[I] := StringReplace(GetEnumName(P, Ord(I)), '_', ' ',
      [rfReplaceAll]);
end;

procedure TAffixes.DoCraft(const Effect: TEffect; const Index: Byte);
begin
  case Effect of
    efCraftStr:
      Items.Index := Ord(TSuffixEnum(of_Strength1)) + Index;
    efCraftDex:
      Items.Index := Ord(TSuffixEnum(of_the_Mongoose)) + Index;
    efCraftWil:
      Items.Index := Ord(TSuffixEnum(of_Willpower1)) + Index;
    efCraftPer:
      Items.Index := Ord(TSuffixEnum(of_Perception1)) + Index;
    efCraftAtr:
      Items.Index := Ord(TSuffixEnum(of_the_Sky)) + Index;
  end;
end;

procedure TAffixes.DoSuffix(var AItem: Item);
var
  SB: TSuffixBase;
  BT: TBonusType;
  Value: Byte;

  procedure SetLife();
  begin
    if (SB.Life.Min > 0) then
    begin
      Value := Math.EnsureRange(Items.GetBonus(AItem, btLife) +
        Math.RandomRange(SB.Life.Min, SB.Life.Max + 1), 1, High(Byte));
      Items.SetBonus(AItem, btLife, Value);
    end;
  end;

  procedure SetMana();
  begin
    if (SB.Mana.Min > 0) then
    begin
      Value := Math.EnsureRange(Items.GetBonus(AItem, btMana) +
        Math.RandomRange(SB.Mana.Min, SB.Mana.Max + 1), 1, High(Byte));
      Items.SetBonus(AItem, btMana, Value);
    end;
  end;

  procedure SetAtr(const ABType: TBonusType; const AMin, AMax: Byte);
  var
    Value: Byte;
  begin
    Value := Math.EnsureRange(Items.GetBonus(AItem, ABType) +
      Math.RandomRange(AMin, AMax + 1), 1, High(Byte));
    Items.SetBonus(AItem, ABType, Value);
  end;

begin
  SB := SuffixBase[TSuffixEnum(AItem.Identify)];
  case TSuffixEnum(AItem.Identify) of
    // Vision
    of_Radiance:
      begin
        Value := Items.GetBonus(AItem, btVis) + 1;
        Items.SetBonus(AItem, btVis, Value);
      end;
    // Healing
    of_Healing:
      begin

      end;
    // Oil
    of_Blacksmith .. of_Permanence:
      begin
        // Cursed or Blessed
        // AItem.Effect := Math.RandomRange(tfCursed * 5, tfBlessed * 5 + 1);
        // Repair Durability
        // AItem.Durability := Math.RandomRange(SB.MaxDurability.Min,
        // SB.MaxDurability.Max + 1);
        // AItem.Value := Math.RandomRange(SB.MaxDurability.Min,
        // SB.MaxDurability.Max + 1);
      end;
    // Life
    of_Life1 .. of_Life7:
      SetLife();
    // Mana
    of_Mana1 .. of_Mana7, of_Magic1 .. of_Magic7:
      SetMana();
    // Life and Mana
    of_Atr1 .. of_Atr7:
      begin
        SetLife();
        SetMana();
      end;
    // Strength
    of_Strength1 .. of_Strength7:
      SetAtr(btStr, SB.Strength.Min, SB.Strength.Max);
    // Dexterity
    of_the_Mongoose .. of_the_Jaguar:
      SetAtr(btDex, SB.Dexterity.Min, SB.Dexterity.Max);
    // Willpower
    of_Willpower1 .. of_Willpower7:
      SetAtr(btWil, SB.Willpower.Min, SB.Willpower.Max);
    // Perception
    of_Perception1 .. of_Perception7:
      SetAtr(btPer, SB.Perception.Min, SB.Perception.Max);
    // Additional All Attributes
    of_the_Sky .. of_the_Infinite:
      for BT := btStr to btPer do
        SetAtr(BT, SB.Level.Min, SB.Level.Max);
    // Defense
    of_Defense1 .. of_Defense7:
      begin
        if (SB.Defense.Min > 0) then
          AItem.Defense := AItem.Defense + Math.EnsureRange
            (Math.RandomRange(SB.Defense.Min, SB.Defense.Max + 1), 1,
            High(Byte));
      end;
    // Damage
    of_Damage1 .. of_Damage7:
      begin
        if (SB.Damage.MinDamage.Min > 0) then
          AItem.MinDamage := AItem.MinDamage +
            Math.EnsureRange(Math.RandomRange(SB.Damage.MinDamage.Min,
            SB.Damage.MinDamage.Max + 1), 1, High(Byte) - 1);
        if (SB.Damage.MaxDamage.Min > 0) then
          AItem.MaxDamage := AItem.MaxDamage +
            Math.EnsureRange(Math.RandomRange(SB.Damage.MaxDamage.Min,
            SB.Damage.MaxDamage.Max + 1), 2, High(Byte));
      end;
    // Durability
    of_Craftmanship .. of_Permanance:
      begin
        if (SB.MaxDurability.Min > 0) then
        begin
          AItem.MaxDurability := AItem.MaxDurability +
            Math.EnsureRange(Math.RandomRange(SB.MaxDurability.Min,
            SB.MaxDurability.Max + 1), 1, High(Byte));
          case Game.Difficulty of
            dfEasy:
              AItem.Durability := AItem.MaxDurability;
            dfHell:
              AItem.Durability := Math.RandomRange(0, 5) + 1;
          end;
        end;
      end;
  end;
  // Effects
  AItem.Effects := SB.Effects;
  if (SB.Value.Min > 0) then
    AItem.Value := Math.EnsureRange(AItem.Value + Math.RandomRange(SB.Value.Min,
      SB.Value.Max + 1), 0, High(Byte));
  if (SB.PrmValue > 0) then
    AItem.Value := SB.PrmValue;
  // Price
  uItem.TItems.CalcItem(AItem);
end;

function TAffixes.GetSuffixName(const SuffixEnum: TSuffixEnum): string;
begin
  Result := FSuffixName[SuffixEnum];
end;

initialization

Affixes := TAffixes.Create;

finalization

FreeAndNil(Affixes);

end.
