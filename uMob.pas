unit uMob;

interface

uses uGame, uMap, uEntity, uCreature, uAbility;

type
  TMobRaceEnum = (mrAnimal, mrHumanoid, mrGoblinoid, mrDemon, mrUndead, mrElemental,
    mrGiant, mrPlant);

type
  TMobSize = (msTiny, msSmall, msMedium, msLarge, msHuge, msGargantuan, msColossal);

type
  TNPCType = (ntSell_C, ntJewTrader_C, ntHealer_A, ntBlacksmith_A, ntWpnTrader_B,
    ntSmithTrader_B, ntArmTrader_A, ntGemTrader_C, ntShTrader_A, ntHelmTrader_A, ntPotTrader_B,
    ntHealTrader_B, ntGlovesTrader_B, ntBootsTrader_C, ntTavTrader_B, ntPotManaTrader_B, ntScrTrader_A,
    ntFoodTrader_A, ntRuneTrader_D);

type
  TMobBase = record
    Symbol: Char;
    Boss: Boolean;
    Maps: set of TMapEnum;
    MaxLife: Word;
    Level: Byte;
    PV: Byte;
    DV: Byte;
    MaxCount: Byte;
    Damage: TDamage;
    Color: Cardinal;
    NPCType: set of TNPCType;
    Abilities: TSetOfAbility;
  end;

type
  TMobEnum = (
    // Dark Wood
    mBigRat, mSpinyFrog, mGiantGecko, mJackal, mBlackBear, mGrizzlyBear,
    mAnaconda, mWolf, mHound,
    // Gray Cave
    mKobold, mBigKobold, mRedKobold, mGnoll, mBasilisk, mWisp, mWorm, mNaga,
    mFireVortex,
    // Deep Cave
    mScorpion, mWasp, mAnt, mSoldierAnt, mScarab, mBigSpider, mFireCrab,
    mDireWolf, mPan, mFaun,
    // Blood Cave
    mGoblin, mDarkGoblin, mBlackGoblin, mHobgoblin, mGargoyle, mWarg, mWerewolf,
    mDraconian, mOrc, mOrcBrute, mOrcWarrior, mOrcWarlord,
    // Drom
    mZombie, mOgre, mMummy, mGhoul, mVampire, mVulture, mCyclops, mSkeleton, mWraith,
    mLich, mPhantom, mTrollBrute,
    // Bosses
    mBlackHound, mGiantNewt, mIguana, // Dark Wood
    mKoboldKing, mSwampWorm, mGiantSlug, // Gray Cave
    mCentaur, mSatyr, mTitan, // Deep Cave
    mHillGiant, mStoneGiant, mTwoHeadedOgre, // Blood Cave
    mTrollKing, // Drom

    npcNPC1, npcNPC2, npcNPC3, npcNPC4, npcNPC5, npcNPC6, npcNPC7);

  // {Black Bear (B)}, {Grizzly Bear (B)}, {Big Rat (R)}
  // Black Viper (S), Ball Python (S), {Anaconda (S)},
  // {Jackal (J)}, {Hound (H)}, {Wolf (W)},
  // {Spiny Frog (F)}, Giant Toad (F), {Giant Newt (N)}, {Iguana (I)}, {Giant Gecko (G),}

  // {Kobold (K)}, {Big Kobold (K)}, {Red Kobold} (K), {Gnoll (G)}, {Basilisk (B)}
  // {Worm (W)}, {Swamp Worm (W)}, {Wisp (W)}, {Fire Vortex (V)}, {Giant Slug (S)}
  // {Naga (N)}, Greater Naga (N), Naga Warrior (N), Jelly (J), Acid Blob (B)

  // {Scorpion (S)}, {Wasp (W)}, {Pan (P)}, {Satyr (S)}, {Faun (F)}, {Centaur (C)},
  // {Ant (A)}, {Soldier Ant (A)}, {Scarab (S)}, {Fire Crab (C)}, {Big Spider (S)}

  // {Goblin (G)}, {Dark Goblin (G)}, {Black Goblin (G)}, {Gargoyle (G)}, {Warg (W)}
  // Air Elemental (E), Fire Elemental (E), Water Elemental (E), Earth Elemental (E),
  // {Orc (O)}, {Orc Warrior (O)}, {Orc Warlord (O)}, {Draconian (D)}, {Titan (T)}
  // {Hill Giant (G)}, {Stone Giant (G)}

  // {Mummy (M)}, {Ghoul (G)}, {Vampire (V)}, {Zombie (Z)}, {Skeleton (S)}, Burning Dead (d),
  // {Lich (L)}, {Phantom (P)},
  // Stone Golem (G), Fire Golem (G), Frost Golem (G),
  // {Ogre (O)}, {Two-Headed Ogre (O)}, {Cyclops (C)}, {Troll King (T)}

const
  MobBase: array [TMobEnum] of TMobBase = (
    // == Dark Wood == //

    // Big Rat
    (Symbol: 'r'; Boss: False; Maps: [deDarkWood]; MaxLife: 5; Level: 1;
    PV: 0; DV: 4; MaxCount: 9; Damage: (Min: 1; Max: 2;); Color: $FF249988;
    NPCType: []; Abilities: [abDiseased];),
    // Spiny Frog
    (Symbol: 'f'; Boss: False; Maps: [deDarkWood]; MaxLife: 7; Level: 1;
    PV: 0; DV: 5; MaxCount: 7; Damage: (Min: 1; Max: 3;); Color: $FF33FF66;
    NPCType: []; Abilities: [abPoisoned, abDiseased];),
    // Giant Gecko
    (Symbol: 'g'; Boss: False; Maps: [deDarkWood]; MaxLife: 8; Level: 1;
    PV: 2; DV: 6; MaxCount: 5; Damage: (Min: 2; Max: 3;); Color: $FF993377;
    NPCType: []; Abilities: [abPoisoned, abStunned, abBlinded];),
    // Jackal
    (Symbol: 'j'; Boss: False; Maps: [deDarkWood]; MaxLife: 9; Level: 1;
    PV: 4; DV: 7; MaxCount: 4; Damage: (Min: 2; Max: 3;); Color: $FF9955FF;
    NPCType: []; Abilities: [];),
    // Black Bear
    (Symbol: 'b'; Boss: False; Maps: [deDarkWood]; MaxLife: 10; Level: 2;
    PV: 5; DV: 8; MaxCount: 1; Damage: (Min: 4; Max: 5;); Color: $FF444444;
    NPCType: []; Abilities: [abStunned, abAfraid];),
    // Grizzly Bear
    (Symbol: 'b'; Boss: False; Maps: [deDarkWood]; MaxLife: 14; Level: 2;
    PV: 5; DV: 9; MaxCount: 1; Damage: (Min: 2; Max: 5;); Color: $FFAAAAAA;
    NPCType: []; Abilities: [abStunned, abAfraid];),
    // Anaconda
    (Symbol: 's'; Boss: False; Maps: [deDarkWood]; MaxLife: 18; Level: 2;
    PV: 3; DV: 9; MaxCount: 1; Damage: (Min: 1; Max: 3;); Color: $FF339955;
    NPCType: []; Abilities: [abPoisoned, abDiseased, abDrunk];),
    // Wolf
    (Symbol: 'w'; Boss: False; Maps: [deDarkWood]; MaxLife: 22; Level: 3;
    PV: 4; DV: 10; MaxCount: 4; Damage: (Min: 2; Max: 4;); Color: $FF666666;
    NPCType: []; Abilities: [];),
    // Hound
    (Symbol: 'h'; Boss: False; Maps: [deDarkWood]; MaxLife: 23; Level: 3;
    PV: 5; DV: 12; MaxCount: 3; Damage: (Min: 3; Max: 4;); Color: $FFCC9988;
    NPCType: []; Abilities: [abBurning];),

    // == Gray Cave == //

    // Kobold
    (Symbol: 'k'; Boss: False; Maps: [deGrayCave]; MaxLife: 25; Level: 3;
    PV: 5; DV: 12; MaxCount: 7; Damage: (Min: 1; Max: 4;); Color: $FF777700;
    NPCType: []; Abilities: [abBloodlust, abCursed];),
    // Big Kobold
    (Symbol: 'k'; Boss: False; Maps: [deGrayCave]; MaxLife: 25; Level: 3;
    PV: 6; DV: 12; MaxCount: 5; Damage: (Min: 2; Max: 4;); Color: $FF777700;
    NPCType: []; Abilities: [abBloodlust, abCursed];),
    // Red Kobold
    (Symbol: 'k'; Boss: False; Maps: [deGrayCave]; MaxLife: 30; Level: 3;
    PV: 7; DV: 13; MaxCount: 5; Damage: (Min: 3; Max: 4;); Color: $FF777700;
    NPCType: []; Abilities: [abBurning, abBloodlust, abCursed];),
    // Gnoll
    (Symbol: 'g'; Boss: False; Maps: [deGrayCave]; MaxLife: 32; Level: 4;
    PV: 4; DV: 14; MaxCount: 3; Damage: (Min: 2; Max: 4;); Color: $FF777700;
    NPCType: []; Abilities: [abCursed];),
    // Basilisk
    (Symbol: 'b'; Boss: False; Maps: [deGrayCave]; MaxLife: 35; Level: 4;
    PV: 5; DV: 15; MaxCount: 1; Damage: (Min: 2; Max: 5;); Color: $FF777700;
    NPCType: []; Abilities: [abPoisoned, abStunned, abBurning, abAfraid, abDrunk];),
    // Wisp
    (Symbol: 'w'; Boss: False; Maps: [deGrayCave]; MaxLife: 38; Level: 4;
    PV: 5; DV: 16; MaxCount: 3; Damage: (Min: 2; Max: 3;); Color: $FF777700;
    NPCType: []; Abilities: [abBlinded, abDiseased];),
    // Worm
    (Symbol: 'w'; Boss: False; Maps: [deGrayCave]; MaxLife: 40; Level: 5;
    PV: 5; DV: 18; MaxCount: 3; Damage: (Min: 3; Max: 5;); Color: $FF777700;
    NPCType: []; Abilities: [abBlinded, abDiseased, abAfraid];),
    // Naga
    (Symbol: 'n'; Boss: False; Maps: [deGrayCave]; MaxLife: 42; Level: 5;
    PV: 7; DV: 18; MaxCount: 1; Damage: (Min: 3; Max: 5;); Color: $FF7777CC;
    NPCType: []; Abilities: [abPoisoned];),
    // Fire Vortex
    (Symbol: 'v'; Boss: False; Maps: [deGrayCave]; MaxLife: 43; Level: 5;
    PV: 9; DV: 20; MaxCount: 1; Damage: (Min: 4; Max: 5;); Color: $FF299AFF;
    NPCType: []; Abilities: [abBurning, abBlinded];),

    // == Deep Cave == //

    // Scorpion
    (Symbol: 's'; Boss: False; Maps: [deDeepCave]; MaxLife: 45; Level: 5;
    PV: 10; DV: 21; MaxCount: 7; Damage: (Min: 3; Max: 5;); Color: $FF992233;
    NPCType: []; Abilities: [abPoisoned, abDiseased, abAfraid, abDrunk];),
    // Wasp
    (Symbol: 'w'; Boss: False; Maps: [deDeepCave]; MaxLife: 48; Level: 5;
    PV: 5; DV: 21; MaxCount: 5; Damage: (Min: 4; Max: 5;); Color: $FF992233;
    NPCType: []; Abilities: [abPoisoned, abDiseased, abDrunk];),
    // Ant
    (Symbol: 'a'; Boss: False; Maps: [deDeepCave]; MaxLife: 50; Level: 5;
    PV: 6; DV: 22; MaxCount: 9; Damage: (Min: 2; Max: 6;); Color: $FF992233;
    NPCType: []; Abilities: [abPoisoned, abDiseased];),
    // Soldier Ant
    (Symbol: 'a'; Boss: False; Maps: [deDeepCave]; MaxLife: 55; Level: 6;
    PV: 9; DV: 22; MaxCount: 9; Damage: (Min: 2; Max: 7;); Color: $FF992233;
    NPCType: []; Abilities: [abPoisoned, abDiseased];),
    // Scarab
    (Symbol: 's'; Boss: False; Maps: [deDeepCave]; MaxLife: 60; Level: 6;
    PV: 15; DV: 23; MaxCount: 7; Damage: (Min: 3; Max: 6;); Color: $FF992233;
    NPCType: []; Abilities: [abPoisoned, abDiseased];),
    // Big Spider
    (Symbol: 's'; Boss: False; Maps: [deDeepCave]; MaxLife: 65; Level: 6;
    PV: 12; DV: 25; MaxCount: 4; Damage: (Min: 1; Max: 7;); Color: $FF992233;
    NPCType: []; Abilities: [abPoisoned, abDiseased, abAfraid];),
    // Fire Crab
    (Symbol: 's'; Boss: False; Maps: [deDeepCave]; MaxLife: 70; Level: 7;
    PV: 25; DV: 26; MaxCount: 8; Damage: (Min: 3; Max: 5;); Color: $FF992233;
    NPCType: []; Abilities: [abBurning, abBlinded];),
    // Dire Wolf
    (Symbol: 'w'; Boss: False; Maps: [deDeepCave]; MaxLife: 70; Level: 7;
    PV: 10; DV: 26; MaxCount: 3; Damage: (Min: 6; Max: 7;); Color: $FF888888;
    NPCType: []; Abilities: [abStunned, abAfraid];),
    // Pan
    (Symbol: 'p'; Boss: False; Maps: [deDeepCave]; MaxLife: 72; Level: 7;
    PV: 10; DV: 28; MaxCount: 1; Damage: (Min: 7; Max: 8;); Color: $FF992233;
    NPCType: []; Abilities: [abBurning, abCursed];),
    // Faun
    (Symbol: 'f'; Boss: False; Maps: [deDeepCave]; MaxLife: 73; Level: 7;
    PV: 10; DV: 30; MaxCount: 1; Damage: (Min: 7; Max: 9;); Color: $FF992233;
    NPCType: []; Abilities: [abBlinded];),

    // == Blood Cave == //

    // Goblin
    (Symbol: 'g'; Boss: False; Maps: [deBloodCave]; MaxLife: 75; Level: 7;
    PV: 25; DV: 31; MaxCount: 9; Damage: (Min: 6; Max: 8;); Color: $FF00AA00;
    NPCType: []; Abilities: [abBloodlust, abCursed];),
    // Dark Goblin
    (Symbol: 'g'; Boss: False; Maps: [deBloodCave]; MaxLife: 75; Level: 7;
    PV: 30; DV: 32; MaxCount: 7; Damage: (Min: 7; Max: 9;); Color: $FF116610;
    NPCType: []; Abilities: [abBloodlust, abCursed];),
    // Black Goblin
    (Symbol: 'g'; Boss: False; Maps: [deBloodCave]; MaxLife: 78; Level: 7;
    PV: 45; DV: 32; MaxCount: 5; Damage: (Min: 8; Max: 10;); Color: $FF445544;
    NPCType: []; Abilities: [abBloodlust, abCursed];),
    // Hobgoblin
    (Symbol: 'g'; Boss: False; Maps: [deBloodCave]; MaxLife: 75; Level: 7;
    PV: 50; DV: 33; MaxCount: 9; Damage: (Min: 7; Max: 10;); Color: $FF55AA55;
    NPCType: []; Abilities: [abBloodlust, abCursed];),
    // Gargoyle
    (Symbol: 'g'; Boss: False; Maps: [deBloodCave]; MaxLife: 80; Level: 7;
    PV: 100; DV: 34; MaxCount: 1; Damage: (Min: 8; Max: 10;);
    Color: $FF445544; NPCType: []; Abilities: [abPoisoned, abBurning, abBlinded, abAfraid];),
    // Warg
    (Symbol: 'w'; Boss: False; Maps: [deBloodCave]; MaxLife: 82; Level: 8;
    PV: 30; DV: 35; MaxCount: 4; Damage: (Min: 9; Max: 11;);
    Color: $FF445544; NPCType: []; Abilities: [abDiseased, abAfraid];),
    // Werewolf
    (Symbol: 'w'; Boss: False; Maps: [deBloodCave]; MaxLife: 90; Level: 8;
    PV: 35; DV: 35; MaxCount: 2; Damage: (Min: 10; Max: 12;);
    Color: $FF777733; NPCType: []; Abilities: [abDiseased, abPoisoned, abAfraid];),
    // Draconian
    (Symbol: 'd'; Boss: False; Maps: [deBloodCave]; MaxLife: 85; Level: 8;
    PV: 50; DV: 35; MaxCount: 1; Damage: (Min: 10; Max: 14;);
    Color: $FF445544; NPCType: []; Abilities: [abStunned, abBurning, abBloodlust, abAfraid];),
    // Orc
    (Symbol: 'o'; Boss: False; Maps: [deBloodCave]; MaxLife: 88; Level: 8;
    PV: 60; DV: 35; MaxCount: 5; Damage: (Min: 10; Max: 15;);
    Color: $FF445544; NPCType: []; Abilities: [abBloodlust];),
    // Orc Brute
    (Symbol: 'o'; Boss: False; Maps: [deBloodCave]; MaxLife: 90; Level: 8;
    PV: 70; DV: 38; MaxCount: 5; Damage: (Min: 11; Max: 15;);
    Color: $FF445544; NPCType: []; Abilities: [abBloodlust];),
    // Orc Warrior
    (Symbol: 'o'; Boss: False; Maps: [deBloodCave]; MaxLife: 90; Level: 9;
    PV: 80; DV: 39; MaxCount: 4; Damage: (Min: 11; Max: 15;);
    Color: $FF445544; NPCType: []; Abilities: [abBloodlust];),
    // Orc Warlord
    (Symbol: 'o'; Boss: False; Maps: [deBloodCave]; MaxLife: 90; Level: 9;
    PV: 30; DV: 40; MaxCount: 3; Damage: (Min: 11; Max: 15;);
    Color: $FF445544; NPCType: []; Abilities: [abBurning, abBloodlust];),

    // == Drom == //

    // Zombie
    (Symbol: 'z'; Boss: False; Maps: [deDrom]; MaxLife: 90; Level: 9; PV: 58;
    DV: 42; MaxCount: 9; Damage: (Min: 15; Max: 16;); Color: $FF00BB00;
    NPCType: []; Abilities: [abPoisoned, abDiseased];),
    // Ogre
    (Symbol: 'o'; Boss: False; Maps: [deDrom]; MaxLife: 92; Level: 9; PV: 55;
    DV: 43; MaxCount: 3; Damage: (Min: 15; Max: 17;); Color: $FF559977;
    NPCType: []; Abilities: [abStunned, abBloodlust];),
    // Mummy
    (Symbol: 'm'; Boss: False; Maps: [deDrom]; MaxLife: 95; Level: 9; PV: 50;
    DV: 44; MaxCount: 5; Damage: (Min: 15; Max: 17;); Color: $FF223333;
    NPCType: []; Abilities: [abPoisoned, abDiseased];),
    // Ghoul
    (Symbol: 'g'; Boss: False; Maps: [deDrom]; MaxLife: 97; Level: 10;
    PV: 65; DV: 44; MaxCount: 5; Damage: (Min: 12; Max: 16;);
    Color: $FF223333; NPCType: []; Abilities: [abPoisoned, abBlinded, abBloodlust, abDrunk];),
    // Vampire
    (Symbol: 'v'; Boss: False; Maps: [deDrom]; MaxLife: 98; Level: 10;
    PV: 45; DV: 45; MaxCount: 3; Damage: (Min: 14; Max: 18;);
    Color: $FF773333; NPCType: []; Abilities: [abBurning, abBloodlust, abCursed, abDiseased];),
    // Vulture
    (Symbol: 'v'; Boss: False; Maps: [deDrom]; MaxLife: 100; Level: 10;
    PV: 50; DV: 45; MaxCount: 2; Damage: (Min: 15; Max: 19;);
    Color: $FFAA3333; NPCType: []; Abilities: [abBurning, abBloodlust, abCursed, abDiseased];),
    // Cyclops
    (Symbol: 'c'; Boss: False; Maps: [deDrom]; MaxLife: 100; Level: 10;
    PV: 120; DV: 46; MaxCount: 1; Damage: (Min: 19; Max: 23;);
    Color: $FF223333; NPCType: []; Abilities: [abStunned, abAfraid];),
    // Skeleton
    (Symbol: 'c'; Boss: False; Maps: [deDrom]; MaxLife: 100; Level: 10;
    PV: 25; DV: 46; MaxCount: 9; Damage: (Min: 10; Max: 14;);
    Color: $FF223333; NPCType: []; Abilities: [abDiseased];),
    // Wraith
    (Symbol: 'w'; Boss: False; Maps: [deDrom]; MaxLife: 100; Level: 10;
    PV: 19; DV: 47; MaxCount: 9; Damage: (Min: 12; Max: 15;);
    Color: $FF22FFFF; NPCType: []; Abilities: [abBurning, abBlinded, abCursed, abDiseased, abAfraid];),
    // Lich
    (Symbol: 'l'; Boss: False; Maps: [deDrom]; MaxLife: 100; Level: 10;
    PV: 20; DV: 48; MaxCount: 1; Damage: (Min: 22; Max: 25;);
    Color: $FF223333; NPCType: []; Abilities: [abBlinded, abCursed, abAfraid, abDrunk];),
    // Phantom
    (Symbol: 'p'; Boss: False; Maps: [deDrom]; MaxLife: 100; Level: 10;
    PV: 10; DV: 49; MaxCount: 1; Damage: (Min: 23; Max: 30;);
    Color: $FF223333; NPCType: []; Abilities: [abBurning, abBlinded];),
    // Troll Brute
    (Symbol: 't'; Boss: False; Maps: [deDrom]; MaxLife: 100; Level: 10;
    PV: 85; DV: 50; MaxCount: 1; Damage: (Min: 25; Max: 30;);
    Color: $FF223333; NPCType: []; Abilities: [abStunned, abBloodlust];),

    // == Bosses == //

    // Black Hound
    (Symbol: 'h'; Boss: True; Maps: [deDarkWood]; MaxLife: 45; Level: 3;
    PV: 30; DV: 25; MaxCount: 1; Damage: (Min: 8; Max: 10;); Color: $FFCC8899;
    NPCType: []; Abilities: [abBurning, abBlinded];),
    // Giant Newt
    (Symbol: 'n'; Boss: True; Maps: [deDarkWood]; MaxLife: 50; Level: 3;
    PV: 45; DV: 30; MaxCount: 1; Damage: (Min: 9; Max: 11;); Color: $FF66DD99;
    NPCType: []; Abilities: [abPoisoned, abStunned, abBlinded, abDiseased, abAfraid, abDrunk];),
    // Iguana
    (Symbol: 'i'; Boss: True; Maps: [deDarkWood]; MaxLife: 55; Level: 3;
    PV: 55; DV: 30; MaxCount: 1; Damage: (Min: 10; Max: 12;);
    Color: $FF44FF77; NPCType: []; Abilities: [abPoisoned, abBlinded, abDiseased, abDrunk];),
    // Kobold King
    (Symbol: 'k'; Boss: True; Maps: [deGrayCave]; MaxLife: 60; Level: 5;
    PV: 60; DV: 32; MaxCount: 1; Damage: (Min: 10; Max: 15;);
    Color: $FFAA77CC; NPCType: []; Abilities: [abStunned, abBurning, abBloodlust, abCursed];),
    // Swamp Worm
    (Symbol: 'w'; Boss: True; Maps: [deGrayCave]; MaxLife: 63; Level: 5;
    PV: 80; DV: 35; MaxCount: 1; Damage: (Min: 12; Max: 18;);
    Color: $FF6699BB; NPCType: []; Abilities: [abPoisoned, abBlinded, abDiseased, abAfraid, abDrunk];),
    // Giant Slug
    (Symbol: 's'; Boss: True; Maps: [deGrayCave]; MaxLife: 67; Level: 5;
    PV: 90; DV: 38; MaxCount: 1; Damage: (Min: 14; Max: 20;);
    Color: $FFCCAADD; NPCType: []; Abilities: [abPoisoned, abBlinded, abDiseased, abAfraid, abDrunk];),
    // Centaur
    (Symbol: 'c'; Boss: True; Maps: [deDeepCave]; MaxLife: 70; Level: 7;
    PV: 55; DV: 40; MaxCount: 1; Damage: (Min: 18; Max: 23;);
    Color: $FF77CCAA; NPCType: []; Abilities: [];),
    // Satyr
    (Symbol: 's'; Boss: True; Maps: [deDeepCave]; MaxLife: 75; Level: 7;
    PV: 45; DV: 45; MaxCount: 1; Damage: (Min: 20; Max: 25;);
    Color: $FF3388AA; NPCType: []; Abilities: [abBurning, abBlinded];),
    // Titan
    (Symbol: 't'; Boss: True; Maps: [deDeepCave]; MaxLife: 95; Level: 8;
    PV: 150; DV: 48; MaxCount: 1; Damage: (Min: 22; Max: 25;);
    Color: $FFAABB77; NPCType: []; Abilities: [abStunned, abBurning, abAfraid];),
    // Hill Giant
    (Symbol: 'g'; Boss: True; Maps: [deBloodCave]; MaxLife: 96; Level: 9;
    PV: 160; DV: 50; MaxCount: 1; Damage: (Min: 23; Max: 25;);
    Color: $FF2233FF; NPCType: []; Abilities: [abStunned, abBurning, abAfraid];),
    // Stone Giant
    (Symbol: 'g'; Boss: True; Maps: [deBloodCave]; MaxLife: 99; Level: 9;
    PV: 180; DV: 54; MaxCount: 1; Damage: (Min: 24; Max: 25;);
    Color: $FF22FF33; NPCType: []; Abilities: [abStunned, abAfraid];),
    // Two-Headed Ogre
    (Symbol: 'o'; Boss: True; Maps: [deBloodCave]; MaxLife: 100; Level: 10;
    PV: 190; DV: 57; MaxCount: 1; Damage: (Min: 25; Max: 30;);
    Color: $FF223333; NPCType: []; Abilities: [abStunned, abBloodlust, abAfraid];),
    // Troll King
    (Symbol: 't'; Boss: True; Maps: [deDrom]; MaxLife: 200; Level: 15;
    PV: 200; DV: 60; MaxCount: 1; Damage: (Min: 50; Max: 75;);
    Color: $FFDD7711; NPCType: []; Abilities: [abBurning, abBloodlust, abAfraid];),

    // == NPC == //

    // Magic Trader
    (Symbol: '@'; Boss: False; Maps: [deDarkWood]; MaxLife: 100; Level: 10;
    PV: 50; DV: 50; MaxCount: 1; Damage: (Min: 10; Max: 15;); Color: clBlue;
    NPCType: [ntScrTrader_A, ntPotManaTrader_B, ntJewTrader_C, ntRuneTrader_D]),

    // Armor Trader
    (Symbol: '@'; Boss: False; Maps: [deDarkWood]; MaxLife: 100; Level: 10;
    PV: 50; DV: 50; MaxCount: 1; Damage: (Min: 10; Max: 15;); Color: clWhite;
    NPCType: [ntHelmTrader_A, ntGlovesTrader_B, ntBootsTrader_C]),

    // Blacksmith
    (Symbol: '@'; Boss: False; Maps: [deDarkWood]; MaxLife: 100; Level: 10;
    PV: 50; DV: 50; MaxCount: 1; Damage: (Min: 10; Max: 15;); Color: clRed;
    NPCType: [ntBlacksmith_A, ntSmithTrader_B, ntGemTrader_C]),

    // Tavern Owner
    (Symbol: '@'; Boss: False; Maps: [deDarkWood]; MaxLife: 100; Level: 10;
    PV: 50; DV: 50; MaxCount: 1; Damage: (Min: 10; Max: 15;);
    Color: clLightYellow; NPCType: [ntFoodTrader_A, ntTavTrader_B]),

    // Weapons and Armors Trader
    (Symbol: '@'; Boss: False; Maps: [deDarkWood]; MaxLife: 100; Level: 10;
    PV: 50; DV: 50; MaxCount: 1; Damage: (Min: 10; Max: 15;);
    Color: clLightestGreen; NPCType: [ntArmTrader_A, ntWpnTrader_B, ntSell_C]),

    // Shield Trader
    (Symbol: '@'; Boss: False; Maps: [deDarkWood]; MaxLife: 100; Level: 10;
    PV: 50; DV: 50; MaxCount: 1; Damage: (Min: 10; Max: 15;);
    Color: clLightBlue; NPCType: [ntShTrader_A]),

    // Healer
    (Symbol: '@'; Boss: False; Maps: [deDarkWood]; MaxLife: 100; Level: 10;
    PV: 50; DV: 50; MaxCount: 1; Damage: (Min: 10; Max: 15;); Color: clGreen;
    NPCType: [ntHealer_A, ntHealTrader_B])

    );

type
  TForce = (fcAlly, fcEnemy, fcNPC);

type
  TMob = class(TCreature)
  private
    FID: Byte;
    FForce: TForce;
    Maps: TMapEnum;
    Boss: Boolean;
    FColor: Cardinal;
    FAlive: Boolean;
    function GetVision: Byte;
  public
    constructor Create();
    destructor Destroy; override;
    procedure Add(AZ: TMapEnum; AX: Integer = -1; AY: Integer = -1;
      AID: Integer = -1; AForce: TForce = fcEnemy);
    procedure AddNPC(AX, AY: Byte; AZ: TMapEnum; ANPCID: Byte);
    procedure Process;
    procedure Render(AX, AY: Byte);
    procedure Walk(AX, AY: Byte; PX: Byte = 0; PY: Byte = 0);
    procedure Attack;
    procedure Defeat;
    procedure DropItems;
    property ID: Byte read FID write FID;
    property Force: TForce read FForce write FForce;
    property Color: Cardinal read FColor;
    property Vision: Byte read GetVision;
    property Alive: Boolean read FAlive write FAlive;
  end;

type
  TMobs = class(TEntity)
  private
    FMob: array of TMob;
    function GetMob(I: Integer): TMob;
    procedure SetMob(I: Integer; const Value: TMob);
    function GetName(I: TMobEnum): string;
    function ChMob(I: Integer; AX, AY: Byte): Boolean;
  public
    constructor Create();
    destructor Destroy; override;
    procedure Add(AZ: TMapEnum; AX: Integer = -1; AY: Integer = -1;
      AForce: TForce = fcEnemy; AID: Integer = -1);
    procedure AddGroup(AZ: TMapEnum);
    function Count: Integer;
    procedure Process;
    procedure Render(AX, AY: Byte);
    function GetFreeTile(AX, AY: Byte): Boolean;
    function GetIndex(AX, AY: Byte): Integer;
    property Mob[I: Integer]: TMob read GetMob write SetMob;
    property Name[I: TMobEnum]: string read GetName;
  end;

type
  TGetXYVal = function(X, Y: Integer): Boolean; stdcall;

var
  Mobs: TMobs = nil;

implementation

uses Math, SysUtils, uTerminal, uPlayer, uMsgLog, uLanguage,
  uItem, uSkill, uStatistic, uAttribute, uPathFind;

//function DoAStar(MapX, MapY, FromX, FromY, ToX, ToY: Integer;
//  Callback: TGetXYVal; var TargetX, TargetY: Integer): Boolean;
//  external 'BeaRLibPF.dll';

function MyCallback(X, Y: Integer): Boolean; stdcall;
begin
  Result := (Map.GetTileEnum(X, Y, Map.Current) in FreeTiles);
end;

{ TMob }

procedure TMob.AddNPC(AX, AY: Byte; AZ: TMapEnum; ANPCID: Byte);
begin
  Self.Clear;
  X := AX;
  Y := AY;
  Maps := AZ;
  ID := ANPCID;
  Boss := False;
  Alive := True;
  Force := fcNPC;
  MaxLife := 100;
  Life := MaxLife;
end;

function ChMapTile(AMobID, AX, AY: Byte; AZ: TMapEnum): Boolean;
begin
  Result := (Map.GetTileEnum(AX, AY, AZ) in SpawnTiles)
    and (Player.X <> AX) and (Player.Y <> AY) and Mobs.GetFreeTile(AX, AY) and
    (AZ in MobBase[TMobEnum(AMobID)].Maps);
end;

procedure TMob.Add(AZ: TMapEnum; AX: Integer = -1; AY: Integer = -1;
  AID: Integer = -1; AForce: TForce = fcEnemy);
var
  I, V, FX, FY: Byte;
begin
  I := 0;
  repeat
    if (AID >= 0) then
      ID := AID
    else
      ID := Math.RandomRange(0, Ord(mTrollKing) + 1);
    if (AX >= 0) then
      FX := AX
    else
      FX := Math.RandomRange(1, High(Byte) - 1);
    if (AY >= 0) then
      FY := AY
    else
      FY := Math.RandomRange(1, High(Byte) - 1);
    if (AForce <> fcEnemy) then
      Break;
    if (I >= High(Byte)) then
      Exit;
    Inc(I);
  until ChMapTile(ID, FX, FY, AZ);
  if (MobBase[TMobEnum(ID)].Boss and IsBoss) then
    Add(AZ);
  X := FX;
  Y := FY;
  Self.Clear;
  Maps := AZ;
  Boss := False;
  Alive := True;
  Force := AForce;
  Abilities.Modify(abSleeping, 1);
  // Color
  FColor := MobBase[TMobEnum(ID)].Color;
  // Life
  V := Math.EnsureRange(IfThen(MobBase[TMobEnum(ID)].Boss, (MobBase[TMobEnum(ID)].Level + Ord(Game.Difficulty)) * 25, 0), 0, High(Byte));
  MaxLife := Math.RandomRange(MobBase[TMobEnum(ID)].MaxLife + V, MobBase[TMobEnum(ID)].MaxLife +
    (Ord(Game.Difficulty) * MobBase[TMobEnum(ID)].Level) + V);
  Life := MaxLife;
  // DV
  V := MobBase[TMobEnum(ID)].DV + (Ord(Game.Difficulty) * 5);
  Attributes.SetValue(atDV, Math.EnsureRange(Math.RandomRange(V - 10, V + 10), 5, DVMax - 10));
  // PV
  V := MobBase[TMobEnum(ID)].PV + (Ord(Game.Difficulty) * 10);
  Attributes.SetValue(atPV, Math.EnsureRange(Math.RandomRange(V, V * 2), 0, PVMax - 10));
  // Boss
  if MobBase[TMobEnum(ID)].Boss then
  begin
    if Game.Wizard then
      Game.Log(Format('%s [%d:%d:%d]', [Mobs.GetName(TMobEnum(ID)), X, Y,
        Ord(AZ)]));
    Boss := True;
    IsBoss := True;
    // PV
    Attributes.SetValue(atPV, Math.EnsureRange(Math.RandomRange(Attributes.Attrib[atPV].Value,
      Attributes.Attrib[atPV].Value + (MobBase[TMobEnum(ID)].Level * Ord(Game.Difficulty))),
      Attributes.Attrib[atPV].Value, PVMax - 10));
  end;
end;

procedure TMob.Attack;
var
  The: string;
  Dam: Word;
  L: Byte;

  procedure Miss();
  begin
    // Miss
    MsgLog.Add(Format(_('%s misses you.'), [The]));
    // MsgLog.Add(Format(_('%s hits you, but your armor protects you.'), [The]));
  end;

begin
  if IsDead or Player.IsDead or (Force <> fcEnemy) then Exit;
  The := GetCapit(GetDescThe(Mobs.GetName(TMobEnum(ID))));
  if (Player.Attributes.Attrib[atDV].Value < Math.RandomRange(0, 100)) then
  begin
    Game.ShowEffects := False;

    // Bloodlust (10%)
    if (abBloodlust in MobBase[TMobEnum(ID)].Abilities) and
      (Math.RandomRange(0, 10) = 0) then
    begin
      L := MobBase[TMobEnum(ID)].Level;
      Dam := Math.EnsureRange(Math.RandomRange(L + 10, L + 25), 0, High(Byte));
      Abilities.Modify(abBloodlust, Dam);
      MsgLog.Add
        (Format(Terminal.Colorize(_('%s feel lust for blood (%d).'),
        Abilities.GetColor(abBloodlust)), [The, Dam]));
      Exit;
    end;

    // Drunk (5%)
    if (abDrunk in MobBase[TMobEnum(ID)].Abilities) and
      (Math.RandomRange(0, 20) = 0) then
    begin
      L := MobBase[TMobEnum(ID)].Level;
      Dam := Math.RandomRange(10, L + 10);
      Player.Abilities.Modify(abBlinded, Dam);
      MsgLog.Add(Format(Terminal.Colorize(_('You are drunk (%d).'),
        Abilities.GetColor(abDrunk)), [Dam]));
      Game.ShowEffects := True;
      Exit;
    end;
    // Blinded (5%)
    if (abBlinded in MobBase[TMobEnum(ID)].Abilities) and
      (Math.RandomRange(0, 20) = 0) then
    begin
      L := MobBase[TMobEnum(ID)].Level;
      Dam := Math.RandomRange(1, L);
      Player.Abilities.Modify(abBlinded, Dam);
      MsgLog.Add(Format(Terminal.Colorize(_('%s blinded you (%d).'), Abilities.GetColor(abBlinded)),
        [The, Dam]));
      Game.ShowEffects := True;
      Exit;
    end;
    // Stunned (5%)
    if (abStunned in MobBase[TMobEnum(ID)].Abilities) and
      (Math.RandomRange(0, 20) = 0) then
    begin
      L := MobBase[TMobEnum(ID)].Level;
      Dam := Math.EnsureRange(Math.RandomRange(1, L), 0, High(Byte));
      Player.Abilities.Modify(abStunned, Dam);
      MsgLog.Add(Format(Terminal.Colorize(_('%s is stuns you (%d).'),
        Abilities.GetColor(abStunned)), [The, Dam]));
      Game.ShowEffects := True;
      Exit;
    end;
    // Weak (20%)
    if (abWeak in MobBase[TMobEnum(ID)].Abilities) and
      (Math.RandomRange(0, 5) = 0) then
    begin
      L := MobBase[TMobEnum(ID)].Level;
      Dam := Math.EnsureRange(Math.RandomRange(10, L * 10), 0, High(Byte));
      Player.Abilities.Modify(abWeak, Dam);
      MsgLog.Add(Format(Terminal.Colorize(_('%s has weakened you (%d).'),
        Abilities.GetColor(abWeak)), [The, Dam]));
      Game.ShowEffects := True;
      Exit;
    end;
    // Diseased (20%)
    if (abDiseased in MobBase[TMobEnum(ID)].Abilities) and
      (Math.RandomRange(0, 5) = 0) then
    begin
      L := MobBase[TMobEnum(ID)].Level;
      Dam := Math.EnsureRange(Math.RandomRange(L * (Ord(Game.Difficulty) + 5),
        L * (Ord(Game.Difficulty) + 9)), 0, High(Byte));
      if MobBase[TMobEnum(ID)].Boss then Dam := Math.EnsureRange(Dam * 3, 0, High(Byte));
      Player.Abilities.Modify(abDiseased, Dam);
      MsgLog.Add(Format(Terminal.Colorize(_('%s has infected you (%d).'),
        Abilities.GetColor(abDiseased)), [The, Dam]));
      Game.ShowEffects := True;
      Exit;
    end;
    // Poisoned (10%)
    if (abPoisoned in MobBase[TMobEnum(ID)].Abilities) and
      (Math.RandomRange(0, 10) = 0) then
    begin
      L := MobBase[TMobEnum(ID)].Level;
      Dam := Math.EnsureRange(Math.RandomRange(L * 5, L * 15), 0, High(Byte));
      Player.Abilities.Modify(abPoisoned, Dam);
      MsgLog.Add(Format(Terminal.Colorize(_('%s is poisoning you (%d).'),
        Abilities.GetColor(abPoisoned)), [The, Dam]));
      Game.ShowEffects := True;
      Exit;
    end;
    // Afraid (10%)
    if (abAfraid in MobBase[TMobEnum(ID)].Abilities) and
      (Math.RandomRange(0, 10) = 0) then
    begin
      L := MobBase[TMobEnum(ID)].Level;
      Dam := Math.EnsureRange(Math.RandomRange(L * 10, L * 20), 0, High(Byte));
      Player.Abilities.Modify(abAfraid, Dam);
      MsgLog.Add(Format(Terminal.Colorize(_('%s scared you (%d).'),
        Abilities.GetColor(abAfraid)), [The, Dam]));
      Game.ShowEffects := True;
      Exit;
    end;
    // Cursed (10%)
    if (abCursed in MobBase[TMobEnum(ID)].Abilities) and
      (Math.RandomRange(0, 10) = 0) then
    begin
      L := MobBase[TMobEnum(ID)].Level;
      Dam := Math.EnsureRange(Math.RandomRange(L * (Ord(Game.Difficulty) + 3),
        L * (Ord(Game.Difficulty) + 5)), 0, High(Byte));
      if MobBase[TMobEnum(ID)].Boss then Dam := Math.EnsureRange(Dam * 3, 0, High(Byte));
      Player.Abilities.Modify(abCursed, Dam);
      MsgLog.Add(Format(Terminal.Colorize(_('%s has cursed you (%d).'),
        Abilities.GetColor(abCursed)), [The, Dam]));
      Game.ShowEffects := True;
      Exit;
    end;
    // Burning (5%)
    if (abBurning in MobBase[TMobEnum(ID)].Abilities) and
      (Math.RandomRange(0, 20) = 0) then
    begin
      L := MobBase[TMobEnum(ID)].Level;
      Dam := Math.EnsureRange(Math.RandomRange(L + 2, L + 5), 0, High(Byte));
      Player.Abilities.Modify(abBurning, Dam);
      MsgLog.Add(Format(Terminal.Colorize(_('%s has burnt you (%d).'),
        Abilities.GetColor(abBurning)), [The, Dam]));
      Game.ShowEffects := True;
      Exit;
    end;
    // Attack
    Dam := EnsureRange(RandomRange(MobBase[TMobEnum(ID)].Damage.Min +
      Ord(Game.Difficulty), MobBase[TMobEnum(ID)].Damage.Max +
      (Ord(Game.Difficulty) * 3)), 0, High(Word));
    // Abilities
    if Abilities.IsAbility(abBloodlust) then
      Inc(Dam, (Dam div 3));
    // PV
    Dam := GetRealDamage(Dam, Player.Attributes.Attrib[atPV].Value);
    if (Dam = 0) then
    begin
      Miss();
      Exit;
    end;
    // Damage
    Player.Life := EnsureRange(Player.Life - Dam, 0, Player.Life);
    MsgLog.Add(Format(_('%s hits you (%d).'), [The, Dam]));
    if ((Math.RandomRange(0, 9 - Ord(Game.Difficulty)) = 0) and not Game.Wizard) then
    case Math.RandomRange(0, 7) of
      0:
        Player.BreakItem(stHead);
      1:
        Player.BreakItem(stTorso);
      2:
        Player.BreakItem(stHands);
      3:
        Player.BreakItem(stFeet);
      4:
        Player.BreakItem(stOffHand);
      5:
        Player.BreakItem(stNeck);
      6:
        Player.BreakItem(stFinger);
    end;
    if (Player.Life = 0) then
      Player.Defeat(Mobs.GetName(TMobEnum(ID)));
  end
  else Miss();
end;

constructor TMob.Create;
begin
  inherited;
end;

procedure TMob.Defeat;
var
  S, The: string;
  V: Byte;
begin
  Self.Alive := False;
  The := GetDescThe(Mobs.GetName(TMobEnum(ID)));
  case Math.RandomRange(0, 2) of
    0:
      S := Format(_('You kill %s.'), [The]);
  else
    S := Format(_('%s dies.'), [GetCapit(The)]);
  end;
  if Boss then
    S := Terminal.Colorize(S, clAlarm);
  MsgLog.Add(S);
  Player.Statictics.Inc(stKills);

  if Boss then
    V := 25
  else
    V := 1;
  Player.Statictics.Inc(stScore, MobBase[TMobEnum(ID)].Level * V);
  Self.DropItems;
  // Boss
  if (Boss and (Map.Current = FinalDungeon) and (TMobEnum(ID) = mTrollKing))
  then
  begin
    if not Game.Wizard then
      Game.Won := True;
    MsgLog.Add(Terminal.Colorize(_('You have won!!!'), clAlarm));
    Player.Statictics.Inc(stScore, 2000);
    Game.Screenshot := Terminal.GetTextScreenshot();
  end;
end;

destructor TMob.Destroy;
begin

  inherited;
end;

procedure TMob.DropItems;
begin
  Items.Loot(Self.X, Self.Y, Boss);
  if Game.LCorpses then
    Items.Loot(Self.X, Self.Y, iCorpse);
end;

function TMob.GetVision: Byte;
begin
  Result := EnsureRange(VisionMax - (Player.Skills.Skill[skStealth]
    .Value div 6), 3, VisionMax);
end;

procedure TMob.Process;
var
  NX, NY, Dist: Integer;
  The: string;
begin
  if (Force = fcNPC) then
    Exit;
  Dist := GetDist(Player.X, Player.Y);
  if (Dist > GetVision) then
    Exit;
  if Abilities.IsAbility(abSleeping) then
  begin
    if (Math.RandomRange(0, 99) <= 15) then
    begin
      Abilities.Ability[abSleeping] := 0;
      Player.Skills.DoSkill(skStealth);
      if (Player.Attributes.Attrib[atPer].Value > Math.RandomRange(0, 100)) then
      begin
        The := GetCapit(GetDescThe(Mobs.GetName(TMobEnum(ID))));
        MsgLog.Add(Format(_('%s notices you!'), [The]));
      end;
      Exit;
    end;
    Exit;
  end;
  if (Dist <= 2) and Player.IsRest then
    Player.IsRest := False;
  // A*
  if not PathFind(High(Byte) + 1, High(Byte) + 1, X, Y, Player.X, Player.Y,
    @MyCallback, NX, NY) then
    Exit;
  if (NX = Player.X) and (NY = Player.Y) then
  begin
    Self.Attack();
  end
  else if (Mobs.GetFreeTile(NX, NY)) then
  begin
    X := NX;
    Y := NY;
  end
  else
    Self.Walk(X, Y, Player.X, Player.Y);
  OnTurn();
  if (Self.Life = 0) then
    Self.Defeat;
end;

procedure TMob.Render(AX, AY: Byte);
var
  C: Char;
begin
  if not Map.InView(X, Y) or (not Game.Wizard and not Map.GetFOV(X, Y)) then
    Exit;
  if not Game.Wizard and (Player.GetDist(X, Y) > Player.Vision) then
    Exit;
  C := MobBase[TMobEnum(ID)].Symbol;
  if (Self.Boss) then
    C := Chr(Ord(C) - 32);
  if Player.Look then
    Terminal.Print(X - Player.X + AX + View.Left, Y - Player.Y + AY + View.Top, C, Color)
  else
    if Self.Force = fcEnemy then
    Terminal.Print(X - Player.X + AX + View.Left, Y - Player.Y + AY + View.Top, C,
      Color, clBkMob)
      else
    Terminal.Print(X - Player.X + AX + View.Left, Y - Player.Y + AY + View.Top, C,
      Color, clBkPlayer);
end;

procedure TMob.Walk(AX, AY: Byte; PX: Byte = 0; PY: Byte = 0);
var
  NX, NY: ShortInt;
  FX, FY: Byte;
begin
  NX := 0;
  NY := 0;
  //
  if (AX < PX) then
  begin
    case Math.RandomRange(0, 4) of
      0:
        begin
          NY := -1;
        end;
      1:
        begin
          NY := +1;
        end;
      2:
        begin
          NX := +1;
          NY := -1;
        end;
      3:
        begin
          NX := +1;
          NY := +1;
        end;
    end;
  end;
  if (AX > PX) then
  begin
    case Math.RandomRange(0, 4) of
      0:
        begin
          NY := -1;
        end;
      1:
        begin
          NY := +1;
        end;
      2:
        begin
          NX := -1;
          NY := +1;
        end;
      3:
        begin
          NX := -1;
          NY := -1;
        end;
    end;
  end;
  //
  if (AY < PY) then
  begin
    case Math.RandomRange(0, 4) of
      0:
        begin
          NX := +1;
        end;
      1:
        begin
          NX := -1;
        end;
      2:
        begin
          NX := -1;
          NY := +1;
        end;
      3:
        begin
          NX := +1;
          NY := +1;
        end;
    end;
  end;
  if (AY > PY) then
  begin
    case Math.RandomRange(0, 4) of
      0:
        begin
          NX := +1;
        end;
      1:
        begin
          NX := -1;
        end;
      2:
        begin
          NX := -1;
          NY := -1;
        end;
      3:
        begin
          NX := +1;
          NY := -1;
        end;
    end;
  end;
  //
  FX := Map.EnsureRange(X + NX);
  FY := Map.EnsureRange(Y + NY);
  if Mobs.GetFreeTile(FX, FY) and (Map.GetTileEnum(FX, FY, Map.Current)
    in FreeTiles) then
  begin
    X := FX;
    Y := FY;
  end;
end;

{ TMobs }

procedure TMobs.Add(AZ: TMapEnum; AX: Integer = -1; AY: Integer = -1;
  AForce: TForce = fcEnemy; AID: Integer = -1);
var
  I: Integer;

  procedure AddMob();
  begin
    FMob[I].Add(AZ, AX, AY, AID, AForce);
  end;

begin
  for I := 0 to Self.Count - 1 do
    if not FMob[I].Alive then
    begin
      AddMob();
      Exit;
    end;
  SetLength(FMob, Length(FMob) + 1);
  I := Length(FMob) - 1;
  FMob[I] := TMob.Create;
  AddMob();
end;

procedure TMobs.AddGroup(AZ: TMapEnum);
var
  ID, FX, FY, FCount: Byte;
  I: Integer;
begin
  repeat
    ID := Math.RandomRange(0, Ord(mTrollKing) + 1);
    repeat
      FX := Math.RandomRange(1, High(Byte) - 1);
      FY := Math.RandomRange(1, High(Byte) - 1);
      if (Ord(AZ) > 0) then
        Break;
    until (Player.GetDist(FX, FY) > 50);
  until ChMapTile(ID, FX, FY, AZ);
  FCount := MobBase[TMobEnum(ID)].MaxCount;
  FCount := Math.EnsureRange(Math.RandomRange(FCount div 2, FCount), 1, FCount);
  if (FCount > 1) then
    FCount := Math.RandomRange(FCount, FCount + (Ord(Game.Difficulty) * 2));
  for I := 1 to FCount do
  begin
    repeat
      FX := Math.EnsureRange(FX + (RandomRange(0, 3) - 1), 1, High(Byte) - 1);
      FY := Math.EnsureRange(FY + (RandomRange(0, 3) - 1), 1, High(Byte) - 1);
    until ChMapTile(ID, FX, FY, AZ);
    Self.Add(AZ, FX, FY, fcEnemy, ID);
  end;
end;

function TMobs.Count: Integer;
begin
  Result := Length(FMob)
end;

constructor TMobs.Create;
begin
  SetLength(FMob, 0)
end;

destructor TMobs.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    FMob[I].Free;
    FMob[I] := nil;
  end;
  inherited;
end;

function TMobs.ChMob(I: Integer; AX, AY: Byte): Boolean;
begin
  with FMob[I] do
    Result := Alive and (Maps = Map.Current) and (AX = X) and (AY = Y)
end;

function TMobs.GetFreeTile(AX, AY: Byte): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Count - 1 do
    if ChMob(I, AX, AY) then
    begin
      Result := False;
      Exit;
    end;
end;

function TMobs.GetIndex(AX, AY: Byte): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if ChMob(I, AX, AY) then
    begin
      Result := I;
      Exit;
    end;
end;

function TMobs.GetMob(I: Integer): TMob;
begin
  Result := FMob[I]
end;

procedure TMobs.Process;
var
  I: Integer;
begin
  if (Count > 0) then
    for I := 0 to Count - 1 do
      if FMob[I].Alive and (FMob[I].Maps = Map.Current) then
        FMob[I].Process;
end;

procedure TMobs.Render(AX, AY: Byte);
var
  I: Integer;
begin
  if (Count > 0) then
    for I := 0 to Count - 1 do
      if FMob[I].Alive and (FMob[I].Maps = Map.Current) then
        FMob[I].Render(AX, AY);
end;

procedure TMobs.SetMob(I: Integer; const Value: TMob);
begin
  FMob[I] := Value;
end;

function TMobs.GetName(I: TMobEnum): string;
begin
  case I of

    // == Dark Wood == //

    mBigRat:
      Result := _('Big Rat');
    mSpinyFrog:
      Result := _('Spiny Frog');
    mGiantGecko:
      Result := _('Giant Gecko');
    mJackal:
      Result := _('Jackal');
    mBlackBear:
      Result := _('Black Bear');
    mGrizzlyBear:
      Result := _('Grizzly Bear');
    mAnaconda:
      Result := _('Anaconda');
    mWolf:
      Result := _('Wolf');
    mHound:
      Result := _('Hound');

    // == Gray Cave == //

    mKobold:
      Result := _('Kobold');
    mBigKobold:
      Result := _('Big Kobold');
    mRedKobold:
      Result := _('Red Kobold');
    mGnoll:
      Result := _('Gnoll');
    mBasilisk:
      Result := _('Basilisk');
    mWisp:
      Result := _('Wisp');
    mWorm:
      Result := _('Worm');
    mNaga:
      Result := _('Naga');
    mFireVortex:
      Result := _('Fire Vortex');

    // == Deep Cave == //

    mScorpion:
      Result := _('Scorpion');
    mWasp:
      Result := _('Wasp');
    mAnt:
      Result := _('Ant');
    mSoldierAnt:
      Result := _('Soldier Ant');
    mScarab:
      Result := _('Scarab');
    mBigSpider:
      Result := _('Big Spider');
    mFireCrab:
      Result := _('Fire Crab');
    mDireWolf:
      Result := _('Dire Wolf');
    mPan:
      Result := _('Pan');
    mFaun:
      Result := _('Faun');

    // == Blood Cave == //

    mGoblin:
      Result := _('Goblin');
    mDarkGoblin:
      Result := _('Dark Goblin');
    mBlackGoblin:
      Result := _('Black Goblin');
    mHobgoblin:
      Result := _('Hobgoblin');
    mGargoyle:
      Result := _('Gargoyle');
    mWarg:
      Result := _('Warg');
    mWerewolf:
      Result := _('Werewolf');
    mDraconian:
      Result := _('Draconian');
    mOrc:
      Result := _('Orc');
    mOrcBrute:
      Result := _('Orc Brute');
    mOrcWarrior:
      Result := _('Orc Warrior');
    mOrcWarlord:
      Result := _('Orc Warlord');

    // == Drom == //

    mZombie:
      Result := _('Zombie');
    mOgre:
      Result := _('Ogre');
    mMummy:
      Result := _('Mummy');
    mGhoul:
      Result := _('Ghoul');
    mVampire:
      Result := _('Vampire');
    mVulture:
      Result := _('Vulture');
    mCyclops:
      Result := _('Cyclops');
    mSkeleton:
      Result := _('Skeleton');
    mWraith:
      Result := _('Wraith');
    mLich:
      Result := _('Lich');
    mPhantom:
      Result := _('Phantom');
    mTrollBrute:
      Result := _('Troll Brute');

    // == Bosses == //

    // Dark Wood
    mBlackHound:
      Result := _('Black Hound');
    mGiantNewt:
      Result := _('Giant Newt');
    mIguana:
      Result := _('Iguana');
    // Gray Cave
    mKoboldKing:
      Result := _('Kobold King');
    mSwampWorm:
      Result := _('Swamp Worm');
    mGiantSlug:
      Result := _('Giant Slug');
    // Deep Cave
    mCentaur:
      Result := _('Centaur');
    mSatyr:
      Result := _('Satyr');
    mTitan:
      Result := _('Titan');
    // Blood Cave
    mHillGiant:
      Result := _('Hill Giant');
    mStoneGiant:
      Result := _('Stone Giant');
    mTwoHeadedOgre:
      Result := _('Two-Headed Ogre');
    // Drom
    mTrollKing:
      Result := _('Troll King');

    // == NPC == //

    npcNPC1:
      Result := _('Eldan (the magic trader)');

    npcNPC2:
      Result := _('Petra (the trader)');

    npcNPC3:
      Result := _('Bran (the blacksmith)');

    npcNPC4:
      Result := _('Tarn (the tavern owner)');

    npcNPC5:
      Result := _('Sirius (the trader)');

    npcNPC6:
      Result := _('Thor (the trader)');

    npcNPC7:
      Result := _('Virna (the healer)');
  end;
end;

initialization

Mobs := TMobs.Create;

finalization

FreeAndNil(Mobs);

end.
