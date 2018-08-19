unit Trollhunter.Mob.Base;

interface

uses
  Trollhunter.Types,
  Trollhunter.Map,
  Trollhunter.Ability,
  Trollhunter.Creature,
  Trollhunter.Mob.Types;

type
  TMobBase = record
    Symbol: Char;
    Boss: Boolean;
    Maps: set of TMapEnum;
    MaxLife: UInt;
    Level: UInt;
    PV: UInt;
    DV: UInt;
    MaxCount: UInt;
    Damage: TDamage;
    Color: Cardinal;
    NPCType: set of TNPCType;
    Abilities: TSetOfAbility;
  end;

type
  MobBase = class(TObject)
  public
    class function GetMob(const Value: TMobEnum): TMobBase; overload;
    class function GetMob(const Value: UInt): TMobBase; overload;
    class function Count: UInt;
  end;

implementation

uses Trollhunter.Game;

const
  Base: array [TMobEnum] of TMobBase = (
    // == Dark _Wood == //

    // Big Rat
    (Symbol: 'r'; Boss: False; Maps: [deDark_Wood]; MaxLife: 5; Level: 1; PV: 0; DV: 4; MaxCount: 9; Damage: (Min: 1; Max: 2;); Color: $FF249988;
    NPCType: []; Abilities: [abDiseased];),
    // Spiny Frog
    (Symbol: 'f'; Boss: False; Maps: [deDark_Wood]; MaxLife: 7; Level: 1; PV: 0; DV: 5; MaxCount: 7; Damage: (Min: 1; Max: 3;); Color: $FF33FF66;
    NPCType: []; Abilities: [abPoisoned, abDiseased];),
    // Giant Gecko
    (Symbol: 'g'; Boss: False; Maps: [deDark_Wood]; MaxLife: 8; Level: 1; PV: 2; DV: 6; MaxCount: 5; Damage: (Min: 2; Max: 3;); Color: $FF993377;
    NPCType: []; Abilities: [abPoisoned, abStunned, abBlinded];),
    // Jackal
    (Symbol: 'j'; Boss: False; Maps: [deDark_Wood]; MaxLife: 9; Level: 1; PV: 4; DV: 7; MaxCount: 4; Damage: (Min: 2; Max: 3;); Color: $FF9955FF;
    NPCType: []; Abilities: [];),
    // Black Bear
    (Symbol: 'b'; Boss: False; Maps: [deDark_Wood]; MaxLife: 10; Level: 2; PV: 5; DV: 8; MaxCount: 1; Damage: (Min: 4; Max: 5;); Color: $FF444444;
    NPCType: []; Abilities: [abStunned, abAfraid, abArmor_Reduction];),
    // Grizzly Bear
    (Symbol: 'b'; Boss: False; Maps: [deDark_Wood]; MaxLife: 14; Level: 2; PV: 5; DV: 9; MaxCount: 1; Damage: (Min: 2; Max: 5;); Color: $FFAAAAAA;
    NPCType: []; Abilities: [abStunned, abAfraid, abArmor_Reduction];),
    // Anaconda
    (Symbol: 's'; Boss: False; Maps: [deDark_Wood]; MaxLife: 18; Level: 2; PV: 3; DV: 9; MaxCount: 1; Damage: (Min: 1; Max: 3;); Color: $FF339955;
    NPCType: []; Abilities: [abPoisoned, abDiseased, abDrunk];),
    // Wolf
    (Symbol: 'w'; Boss: False; Maps: [deDark_Wood]; MaxLife: 22; Level: 3; PV: 4; DV: 10; MaxCount: 4; Damage: (Min: 2; Max: 4;); Color: $FF666666;
    NPCType: []; Abilities: [];),
    // Hound
    (Symbol: 'h'; Boss: False; Maps: [deDark_Wood]; MaxLife: 23; Level: 3; PV: 5; DV: 12; MaxCount: 3; Damage: (Min: 3; Max: 4;); Color: $FFCC9988;
    NPCType: []; Abilities: [abBurning];),

    // == Gray Cave == //

    // Kobold
    (Symbol: 'k'; Boss: False; Maps: [deGray_Cave]; MaxLife: 25; Level: 3; PV: 5; DV: 12; MaxCount: 7; Damage: (Min: 1; Max: 4;); Color: $FF777700;
    NPCType: []; Abilities: [abBloodlust, abCursed];),
    // Big Kobold
    (Symbol: 'k'; Boss: False; Maps: [deGray_Cave]; MaxLife: 25; Level: 3; PV: 6; DV: 12; MaxCount: 5; Damage: (Min: 2; Max: 4;); Color: $FF777700;
    NPCType: []; Abilities: [abBloodlust, abCursed];),
    // Red Kobold
    (Symbol: 'k'; Boss: False; Maps: [deGray_Cave]; MaxLife: 30; Level: 3; PV: 7; DV: 13; MaxCount: 5; Damage: (Min: 3; Max: 4;); Color: $FF777700;
    NPCType: []; Abilities: [abBurning, abBloodlust, abCursed];),
    // Gnoll
    (Symbol: 'g'; Boss: False; Maps: [deGray_Cave]; MaxLife: 32; Level: 4; PV: 4; DV: 14; MaxCount: 3; Damage: (Min: 2; Max: 4;); Color: $FF777700;
    NPCType: []; Abilities: [abCursed];),
    // Basilisk
    (Symbol: 'b'; Boss: False; Maps: [deGray_Cave]; MaxLife: 35; Level: 4; PV: 5; DV: 15; MaxCount: 1; Damage: (Min: 2; Max: 5;); Color: $FF777700;
    NPCType: []; Abilities: [abPoisoned, abStunned, abBurning, abAfraid, abDrunk];),
    // Wisp
    (Symbol: 'w'; Boss: False; Maps: [deGray_Cave]; MaxLife: 38; Level: 4; PV: 5; DV: 16; MaxCount: 3; Damage: (Min: 2; Max: 3;); Color: $FF777700;
    NPCType: []; Abilities: [abBlinded, abDiseased];),
    // Worm
    (Symbol: 'w'; Boss: False; Maps: [deGray_Cave]; MaxLife: 40; Level: 5; PV: 5; DV: 18; MaxCount: 3; Damage: (Min: 3; Max: 5;); Color: $FF777700;
    NPCType: []; Abilities: [abBlinded, abDiseased, abAfraid];),
    // Naga
    (Symbol: 'n'; Boss: False; Maps: [deGray_Cave]; MaxLife: 42; Level: 5; PV: 7; DV: 18; MaxCount: 1; Damage: (Min: 3; Max: 5;); Color: $FF7777CC;
    NPCType: []; Abilities: [abPoisoned, abArmor_Reduction];),
    // Fire Vortex
    (Symbol: 'v'; Boss: False; Maps: [deGray_Cave]; MaxLife: 43; Level: 5; PV: 9; DV: 20; MaxCount: 1; Damage: (Min: 4; Max: 5;); Color: $FF299AFF;
    NPCType: []; Abilities: [abBurning, abBlinded];),

    // == Deep Cave == //

    // Scorpion
    (Symbol: 's'; Boss: False; Maps: [deDeep_Cave]; MaxLife: 45; Level: 5; PV: 10; DV: 21; MaxCount: 7; Damage: (Min: 3; Max: 5;); Color: $FF992233;
    NPCType: []; Abilities: [abPoisoned, abDiseased, abAfraid, abDrunk];),
    // Wasp
    (Symbol: 'w'; Boss: False; Maps: [deDeep_Cave]; MaxLife: 48; Level: 5; PV: 5; DV: 21; MaxCount: 5; Damage: (Min: 4; Max: 5;); Color: $FF992233;
    NPCType: []; Abilities: [abPoisoned, abDiseased, abDrunk];),
    // Ant
    (Symbol: 'a'; Boss: False; Maps: [deDeep_Cave]; MaxLife: 50; Level: 5; PV: 6; DV: 22; MaxCount: 9; Damage: (Min: 2; Max: 6;); Color: $FF992233;
    NPCType: []; Abilities: [abPoisoned, abDiseased];),
    // Soldier Ant
    (Symbol: 'a'; Boss: False; Maps: [deDeep_Cave]; MaxLife: 55; Level: 6; PV: 9; DV: 22; MaxCount: 9; Damage: (Min: 2; Max: 7;); Color: $FF992233;
    NPCType: []; Abilities: [abPoisoned, abDiseased];),
    // Scarab
    (Symbol: 's'; Boss: False; Maps: [deDeep_Cave]; MaxLife: 60; Level: 6; PV: 15; DV: 23; MaxCount: 7; Damage: (Min: 3; Max: 6;); Color: $FF992233;
    NPCType: []; Abilities: [abPoisoned, abDiseased];),
    // Big Spider
    (Symbol: 's'; Boss: False; Maps: [deDeep_Cave]; MaxLife: 65; Level: 6; PV: 12; DV: 25; MaxCount: 4; Damage: (Min: 1; Max: 7;); Color: $FF992233;
    NPCType: []; Abilities: [abPoisoned, abDiseased, abAfraid];),
    // Fire Crab
    (Symbol: 's'; Boss: False; Maps: [deDeep_Cave]; MaxLife: 70; Level: 7; PV: 25; DV: 26; MaxCount: 8; Damage: (Min: 3; Max: 5;); Color: $FF992233;
    NPCType: []; Abilities: [abBurning, abBlinded];),
    // Dire Wolf
    (Symbol: 'w'; Boss: False; Maps: [deDeep_Cave]; MaxLife: 70; Level: 7; PV: 10; DV: 26; MaxCount: 3; Damage: (Min: 6; Max: 7;); Color: $FF888888;
    NPCType: []; Abilities: [abStunned, abAfraid];),
    // Pan
    (Symbol: 'p'; Boss: False; Maps: [deDeep_Cave]; MaxLife: 72; Level: 7; PV: 10; DV: 28; MaxCount: 1; Damage: (Min: 7; Max: 8;); Color: $FF992233;
    NPCType: []; Abilities: [abBurning, abCursed];),
    // Faun
    (Symbol: 'f'; Boss: False; Maps: [deDeep_Cave]; MaxLife: 73; Level: 7; PV: 10; DV: 30; MaxCount: 1; Damage: (Min: 7; Max: 9;); Color: $FF992233;
    NPCType: []; Abilities: [abBlinded, abArmor_Reduction];),

    // == Blood Cave == //

    // Goblin
    (Symbol: 'g'; Boss: False; Maps: [deBlood_Cave]; MaxLife: 75; Level: 7; PV: 25; DV: 31; MaxCount: 9; Damage: (Min: 6; Max: 8;); Color: $FF00AA00;
    NPCType: []; Abilities: [abBloodlust, abCursed];),
    // Dark Goblin
    (Symbol: 'g'; Boss: False; Maps: [deBlood_Cave]; MaxLife: 75; Level: 7; PV: 30; DV: 32; MaxCount: 7; Damage: (Min: 7; Max: 9;); Color: $FF116610;
    NPCType: []; Abilities: [abBloodlust, abCursed];),
    // Black Goblin
    (Symbol: 'g'; Boss: False; Maps: [deBlood_Cave]; MaxLife: 78; Level: 7; PV: 45; DV: 32; MaxCount: 5; Damage: (Min: 8; Max: 10;); Color: $FF445544;
    NPCType: []; Abilities: [abBloodlust, abCursed];),
    // Hobgoblin
    (Symbol: 'g'; Boss: False; Maps: [deBlood_Cave]; MaxLife: 75; Level: 7; PV: 50; DV: 33; MaxCount: 9; Damage: (Min: 7; Max: 10;); Color: $FF55AA55;
    NPCType: []; Abilities: [abBloodlust, abCursed];),
    // Gargoyle
    (Symbol: 'g'; Boss: False; Maps: [deBlood_Cave]; MaxLife: 80; Level: 7; PV: 100; DV: 34; MaxCount: 1; Damage: (Min: 8; Max: 10;);
    Color: $FF445544; NPCType: []; Abilities: [abPoisoned, abBurning, abBlinded, abAfraid];),
    // Warg
    (Symbol: 'w'; Boss: False; Maps: [deBlood_Cave]; MaxLife: 82; Level: 8; PV: 30; DV: 35; MaxCount: 4; Damage: (Min: 9; Max: 11;); Color: $FF445544;
    NPCType: []; Abilities: [abDiseased, abAfraid];),
    // Werewolf
    (Symbol: 'w'; Boss: False; Maps: [deBlood_Cave]; MaxLife: 90; Level: 8; PV: 35; DV: 35; MaxCount: 2; Damage: (Min: 10; Max: 12;);
    Color: $FF777733; NPCType: []; Abilities: [abDiseased, abPoisoned, abAfraid];),
    // Draconian
    (Symbol: 'd'; Boss: False; Maps: [deBlood_Cave]; MaxLife: 85; Level: 8; PV: 50; DV: 35; MaxCount: 1; Damage: (Min: 10; Max: 14;);
    Color: $FF445544; NPCType: []; Abilities: [abStunned, abBurning, abBloodlust, abAfraid];),
    // Orc
    (Symbol: 'o'; Boss: False; Maps: [deBlood_Cave]; MaxLife: 88; Level: 8; PV: 60; DV: 35; MaxCount: 5; Damage: (Min: 10; Max: 15;);
    Color: $FF445544; NPCType: []; Abilities: [abBloodlust];),
    // Orc Brute
    (Symbol: 'o'; Boss: False; Maps: [deBlood_Cave]; MaxLife: 90; Level: 8; PV: 70; DV: 38; MaxCount: 5; Damage: (Min: 11; Max: 15;);
    Color: $FF445544; NPCType: []; Abilities: [abBloodlust];),
    // Orc Warrior
    (Symbol: 'o'; Boss: False; Maps: [deBlood_Cave]; MaxLife: 90; Level: 9; PV: 80; DV: 39; MaxCount: 4; Damage: (Min: 11; Max: 15;);
    Color: $FF445544; NPCType: []; Abilities: [abBloodlust];),
    // Orc Warlord
    (Symbol: 'o'; Boss: False; Maps: [deBlood_Cave]; MaxLife: 90; Level: 9; PV: 30; DV: 40; MaxCount: 3; Damage: (Min: 11; Max: 15;);
    Color: $FF445544; NPCType: []; Abilities: [abBurning, abBloodlust, abArmor_Reduction];),

    // == Drom == //

    // Zombie
    (Symbol: 'z'; Boss: False; Maps: [deDrom]; MaxLife: 90; Level: 9; PV: 58; DV: 42; MaxCount: 9; Damage: (Min: 15; Max: 16;); Color: $FF00BB00;
    NPCType: []; Abilities: [abPoisoned, abDiseased];),
    // Ogre
    (Symbol: 'o'; Boss: False; Maps: [deDrom]; MaxLife: 92; Level: 9; PV: 55; DV: 43; MaxCount: 3; Damage: (Min: 15; Max: 17;); Color: $FF559977;
    NPCType: []; Abilities: [abStunned, abBloodlust];),
    // Mummy
    (Symbol: 'm'; Boss: False; Maps: [deDrom]; MaxLife: 95; Level: 9; PV: 50; DV: 44; MaxCount: 5; Damage: (Min: 15; Max: 17;); Color: $FF223333;
    NPCType: []; Abilities: [abPoisoned, abDiseased];),
    // Ghoul
    (Symbol: 'g'; Boss: False; Maps: [deDrom]; MaxLife: 97; Level: 10; PV: 65; DV: 44; MaxCount: 5; Damage: (Min: 12; Max: 16;); Color: $FF223333;
    NPCType: []; Abilities: [abPoisoned, abBlinded, abBloodlust, abDrunk];),
    // Vampire
    (Symbol: 'v'; Boss: False; Maps: [deDrom]; MaxLife: 98; Level: 10; PV: 45; DV: 45; MaxCount: 3; Damage: (Min: 14; Max: 18;); Color: $FF773333;
    NPCType: []; Abilities: [abBurning, abBloodlust, abCursed, abDiseased, abArmor_Reduction];),
    // Vulture
    (Symbol: 'v'; Boss: False; Maps: [deDrom]; MaxLife: 100; Level: 10; PV: 50; DV: 45; MaxCount: 2; Damage: (Min: 15; Max: 19;); Color: $FFAA3333;
    NPCType: []; Abilities: [abBurning, abBloodlust, abCursed, abDiseased];),
    // Cyclops
    (Symbol: 'c'; Boss: False; Maps: [deDrom]; MaxLife: 100; Level: 10; PV: 120; DV: 46; MaxCount: 1; Damage: (Min: 19; Max: 23;); Color: $FF223333;
    NPCType: []; Abilities: [abStunned, abAfraid];),
    // Skeleton
    (Symbol: 'c'; Boss: False; Maps: [deDrom]; MaxLife: 100; Level: 10; PV: 25; DV: 46; MaxCount: 9; Damage: (Min: 10; Max: 14;); Color: $FF223333;
    NPCType: []; Abilities: [abDiseased];),
    // Wraith
    (Symbol: 'w'; Boss: False; Maps: [deDrom]; MaxLife: 100; Level: 10; PV: 19; DV: 47; MaxCount: 9; Damage: (Min: 12; Max: 15;); Color: $FF22FFFF;
    NPCType: []; Abilities: [abBurning, abBlinded, abCursed, abDiseased, abAfraid];),
    // Lich
    (Symbol: 'l'; Boss: False; Maps: [deDrom]; MaxLife: 100; Level: 10; PV: 20; DV: 48; MaxCount: 1; Damage: (Min: 22; Max: 25;); Color: $FF223333;
    NPCType: []; Abilities: [abBlinded, abCursed, abAfraid, abDrunk, abArmor_Reduction];),
    // Phantom
    (Symbol: 'p'; Boss: False; Maps: [deDrom]; MaxLife: 100; Level: 10; PV: 10; DV: 49; MaxCount: 1; Damage: (Min: 23; Max: 30;); Color: $FF223333;
    NPCType: []; Abilities: [abBurning, abBlinded];),
    // Troll Brute
    (Symbol: 't'; Boss: False; Maps: [deDrom]; MaxLife: 100; Level: 10; PV: 85; DV: 50; MaxCount: 1; Damage: (Min: 25; Max: 30;); Color: $FF223333;
    NPCType: []; Abilities: [abStunned, abBloodlust];),

    // == Bosses == //

    // Black Hound
    (Symbol: 'h'; Boss: True; Maps: [deDark_Wood]; MaxLife: 45; Level: 3; PV: 30; DV: 25; MaxCount: 1; Damage: (Min: 8; Max: 10;); Color: $FFCC8899;
    NPCType: []; Abilities: [abBurning, abBlinded];),
    // Giant Newt
    (Symbol: 'n'; Boss: True; Maps: [deDark_Wood]; MaxLife: 50; Level: 3; PV: 45; DV: 30; MaxCount: 1; Damage: (Min: 9; Max: 11;); Color: $FF66DD99;
    NPCType: []; Abilities: [abPoisoned, abStunned, abBlinded, abDiseased, abAfraid, abDrunk];),
    // Iguana
    (Symbol: 'i'; Boss: True; Maps: [deDark_Wood]; MaxLife: 55; Level: 3; PV: 55; DV: 30; MaxCount: 1; Damage: (Min: 10; Max: 12;); Color: $FF44FF77;
    NPCType: []; Abilities: [abPoisoned, abBlinded, abDiseased, abDrunk];),
    // Kobold King
    (Symbol: 'k'; Boss: True; Maps: [deGray_Cave]; MaxLife: 60; Level: 5; PV: 60; DV: 32; MaxCount: 1; Damage: (Min: 10; Max: 15;); Color: $FFAA77CC;
    NPCType: []; Abilities: [abStunned, abBurning, abBloodlust, abCursed, abArmor_Reduction];),
    // Swamp Worm
    (Symbol: 'w'; Boss: True; Maps: [deGray_Cave]; MaxLife: 63; Level: 5; PV: 80; DV: 35; MaxCount: 1; Damage: (Min: 12; Max: 18;); Color: $FF6699BB;
    NPCType: []; Abilities: [abPoisoned, abBlinded, abDiseased, abAfraid, abDrunk];),
    // Giant Slug
    (Symbol: 's'; Boss: True; Maps: [deGray_Cave]; MaxLife: 67; Level: 5; PV: 90; DV: 38; MaxCount: 1; Damage: (Min: 14; Max: 20;); Color: $FFCCAADD;
    NPCType: []; Abilities: [abPoisoned, abBlinded, abDiseased, abAfraid, abDrunk];),
    // Centaur
    (Symbol: 'c'; Boss: True; Maps: [deDeep_Cave]; MaxLife: 70; Level: 7; PV: 55; DV: 40; MaxCount: 1; Damage: (Min: 18; Max: 23;); Color: $FF77CCAA;
    NPCType: []; Abilities: [];),
    // Satyr
    (Symbol: 's'; Boss: True; Maps: [deDeep_Cave]; MaxLife: 75; Level: 7; PV: 45; DV: 45; MaxCount: 1; Damage: (Min: 20; Max: 25;); Color: $FF3388AA;
    NPCType: []; Abilities: [abBurning, abBlinded];),
    // Titan
    (Symbol: 't'; Boss: True; Maps: [deDeep_Cave]; MaxLife: 95; Level: 8; PV: 150; DV: 48; MaxCount: 1; Damage: (Min: 22; Max: 25;); Color: $FFAABB77;
    NPCType: []; Abilities: [abStunned, abBurning, abAfraid];),
    // Hill Giant
    (Symbol: 'g'; Boss: True; Maps: [deBlood_Cave]; MaxLife: 96; Level: 9; PV: 160; DV: 50; MaxCount: 1; Damage: (Min: 23; Max: 25;);
    Color: $FF2233FF; NPCType: []; Abilities: [abStunned, abBurning, abAfraid];),
    // Stone Giant
    (Symbol: 'g'; Boss: True; Maps: [deBlood_Cave]; MaxLife: 99; Level: 9; PV: 180; DV: 54; MaxCount: 1; Damage: (Min: 24; Max: 25;);
    Color: $FF22FF33; NPCType: []; Abilities: [abStunned, abAfraid];),
    // Two-Headed Ogre
    (Symbol: 'o'; Boss: True; Maps: [deBlood_Cave]; MaxLife: 100; Level: 10; PV: 190; DV: 57; MaxCount: 1; Damage: (Min: 25; Max: 30;);
    Color: $FF223333; NPCType: []; Abilities: [abStunned, abBloodlust, abAfraid, abArmor_Reduction];),
    // Troll King
    (Symbol: 't'; Boss: True; Maps: [deDrom]; MaxLife: 200; Level: 15; PV: 200; DV: 60; MaxCount: 1; Damage: (Min: 50; Max: 75;); Color: $FFDD7711;
    NPCType: []; Abilities: [abBurning, abBloodlust, abAfraid, abArmor_Reduction];),

    // == NPC == //

    // Magic Trader
    (Symbol: '@'; Boss: False; Maps: [deDark_Wood]; MaxLife: 100; Level: 10; PV: 50; DV: 50; MaxCount: 1; Damage: (Min: 10; Max: 15;); Color: clBlue;
    NPCType: [ntScrTrader_A, ntPotManaTrader_B, ntJewTrader_C, ntRuneTrader_D]),

    // Armor Trader
    (Symbol: '@'; Boss: False; Maps: [deDark_Wood]; MaxLife: 100; Level: 10; PV: 50; DV: 50; MaxCount: 1; Damage: (Min: 10; Max: 15;); Color: clWhite;
    NPCType: [ntHelmTrader_A, ntGlovesTrader_B, ntBootsTrader_C, ntQuest_D]),

    // Blacksmith
    (Symbol: '@'; Boss: False; Maps: [deDark_Wood]; MaxLife: 100; Level: 10; PV: 50; DV: 50; MaxCount: 1; Damage: (Min: 10; Max: 15;); Color: clRed;
    NPCType: [ntBlacksmith_A, ntSmithTrader_B, ntGemTrader_C]),

    // Tavern Owner
    (Symbol: '@'; Boss: False; Maps: [deDark_Wood]; MaxLife: 100; Level: 10; PV: 50; DV: 50; MaxCount: 1; Damage: (Min: 10; Max: 15;);
    Color: clLightYellow; NPCType: [ntFoodTrader_A, ntTavTrader_B]),

    // Weapons and Armors Trader
    (Symbol: '@'; Boss: False; Maps: [deDark_Wood]; MaxLife: 100; Level: 10; PV: 50; DV: 50; MaxCount: 1; Damage: (Min: 10; Max: 15;);
    Color: clLightestGreen; NPCType: [ntArmTrader_A, ntWpnTrader_B, ntSell_C]),

    // Shield Trader
    (Symbol: '@'; Boss: False; Maps: [deDark_Wood]; MaxLife: 100; Level: 10; PV: 50; DV: 50; MaxCount: 1; Damage: (Min: 10; Max: 15;);
    Color: clLightBlue; NPCType: [ntShTrader_A]),

    // Healer
    (Symbol: '@'; Boss: False; Maps: [deDark_Wood]; MaxLife: 100; Level: 10; PV: 50; DV: 50; MaxCount: 1; Damage: (Min: 10; Max: 15;); Color: clGreen;
    NPCType: [ntHealer_A, ntHealTrader_B])

    );

class function MobBase.GetMob(const Value: TMobEnum): TMobBase;
begin
  Result := Base[Value];
end;

class function MobBase.GetMob(const Value: UInt): TMobBase;
begin
  Result := Base[TMobEnum(Value)];
end;

class function MobBase.Count;
begin
  Result := Length(Base);
end;

end.
