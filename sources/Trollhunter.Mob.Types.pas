unit Trollhunter.Mob.Types;

interface

type
  TMobRaceEnum = (mrAnimal, mrHumanoid, mrGoblinoid, mrDemon, mrUndead, mrElemental, mrGiant, mrPlant);

type
  TMobSize = (msTiny, msSmall, msMedium, msLarge, msHuge, msGargantuan, msColossal);

type
  TNPCType = (ntSell_C, ntJewTrader_C, ntHealer_A, ntBlacksmith_A, ntWpnTrader_B, ntSmithTrader_B, ntArmTrader_A, ntGemTrader_C, ntShTrader_A,
    ntHelmTrader_A, ntPotTrader_B, ntHealTrader_B, ntGlovesTrader_B, ntBootsTrader_C, ntTavTrader_B, ntPotManaTrader_B, ntScrTrader_A, ntFoodTrader_A,
    ntRuneTrader_D, ntQuest_D);

type
  TMobEnum = (
    // Dark _Wood
    mbBig_Rat, mbSpiny_Frog, mbGiant_Gecko, mbJackal, mbBlack_Bear, mbGrizzly_Bear, mbAnaconda, mbWolf, mbHound,
    // Gray Cave
    mbKobold, mbBig_Kobold, mbRed_Kobold, mbGnoll, mbBasilisk, mbWisp, mbWorm, mbNaga, mbFire_Vortex,
    // Deep Cave
    mbScorpion, mbWasp, mbAnt, mbSoldier_Ant, mbScarab, mbBig_Spider, mbFire_Crab, mbDire_Wolf, mbPan, mbFaun,
    // Blood Cave
    mbGoblin, mbDark_Goblin, mbBlack_Goblin, mbHobgoblin, mbGargoyle, mbWarg, mbWerewolf, mbDraconian, mbOrc, mbOrc_Brute, mbOrc_Warrior,
    mbOrc_Warlord,
    // Drom
    mbZombie, mbOgre, mbMummy, mbGhoul, mbVampire, mbVulture, mbCyclops, mbSkeleton, mbWraith, mbLich, mbPhantom, mbTroll_Brute,
    // Dark _Wood (Bosses)
    mbBlack_Hound, mbGiant_Newt, mbIguana,
    // Gray Cave (Bosses)
    mbKobold_King, mbSwamp_Worm, mbGiant_Slug,
    // Deep Cave (Bosses)
    mbCentaur, mbSatyr, mbTitan,
    // Blood Cave (Bosses)
    mbHill_Giant, mbStone_Giant, mbTwo4Headed_Ogre,
    // Drom (Bosses)
    mbTroll_King,

    // NPC
    mbEldan_2the_magic_trader3, mbPetra_2the_trader3, mbBran_2the_blacksmith3, mbTarn_2the_tavern_owner3, mbSirius_2the_trader3, mbThor_2the_trader3,
    mbVirna_2the_healer3);



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

type
  TForce = (fcAlly, fcEnemy, fcNPC);

implementation

end.
