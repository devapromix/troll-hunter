unit Trollhunter.Race;

interface

uses
  Trollhunter.Skill;

const
  RacesCount = 10;

type
  TItemProp = record
    ID: string;
    Count: Byte;
  end;

type
  TRaceSkill = record
    Skill: TSkillEnum;
    Level: Byte;
  end;

type
  TRaceSkills = array [0..1] of TRaceSkill;

type
  TRaceRec = record
    NameLangID: Word;
    Strength: Integer;
    Dexterity: Integer;
    Will: Integer;
    Speed: Integer;
    RHWeapon: TItemProp;
    LHWeapon: TItemProp;
    Skills: TRaceSkills;
  end;

const
  Race: array [0..RacesCount - 1] of TRaceRec = (
  ( // Human
  NameLangID: 182;
  Strength  : 0;
  Dexterity : 0;
  Will      : 0;
  Speed     : 0;
  RHWeapon  : (ID: 'SHORTSWORD';    Count: 1);
  LHWeapon  : (ID: 'SMALLSHIELD';   Count: 1);
  Skills    : ((Skill: skSword; Level: 10), (Skill: skShield; Level: 10));
  ),
  ( // Halfling
  NameLangID: 183;
  Strength  : -3;
  Dexterity : 4;
  Will      : -2;
  Speed     : 1;
  RHWeapon  : (ID: 'SHORTSWORD';    Count: 1);
  LHWeapon  : (ID: '';              Count: 0);
  Skills    : ((Skill: skDagger; Level: 10), (Skill: skTrap; Level: 10));
  ),
  ( // Gnome
  NameLangID: 184;
  Strength  : 1;
  Dexterity : 1;
  Will      : 0;
  Speed     : -2;
  RHWeapon  : (ID: 'HATCHET';       Count: 1);
  LHWeapon  : (ID: 'SMALLSHIELD';   Count: 1);
  Skills    : ((Skill: skAxe; Level: 10), (Skill: skShield; Level: 10));
  ),
  ( // Gray Dwarf
  NameLangID: 185;
  Strength  : 2;
  Dexterity : 0;
  Will      : 0;
  Speed     : -2;
  RHWeapon  : (ID: 'HATCHET';       Count: 1);
  LHWeapon  : (ID: 'SMALLSHIELD';   Count: 1);
  Skills    : ((Skill: skAxe; Level: 10), (Skill: skShield; Level: 10));
  ),
  ( // Orc
  NameLangID: 186;
  Strength  : 6;
  Dexterity : -2;
  Will      : -2;
  Speed     : -2;
  RHWeapon  : (ID: 'STONEHAMMER';   Count: 1);
  LHWeapon  : (ID: '';              Count: 0);
  Skills    : ((Skill: skMace; Level: 20), (Skill: skShield; Level: 0));
  ),
  ( // High Elf
  NameLangID: 187;
  Strength  : -4;
  Dexterity : 0;  
  Will      : 4;
  Speed     : 0;
  RHWeapon  : (ID: 'SHORTSWORD';    Count: 1);
  LHWeapon  : (ID: '';              Count: 0);
  Skills    : ((Skill: skMagic; Level: 10), (Skill: skTrap; Level: 10));
  ),
  ( // Night Elf
  NameLangID: 188;
  Strength  : 1;
  Dexterity : 2;
  Will      : -2;
  Speed     : -1;
  RHWeapon  : (ID: 'HUNTBOW';       Count: 1);
  LHWeapon  : (ID: 'ARROW';         Count: 75);
  Skills    : ((Skill: skBow; Level: 10), (Skill: skTrap; Level: 10));
  ),
  ( // Dark Elf
  NameLangID: 189;
  Strength  : -3;
  Dexterity : 1;
  Will      : 1;
  Speed     : 1;
  RHWeapon  : (ID: 'LIGHTCROSSBOW'; Count: 1);
  LHWeapon  : (ID: 'BOLT';          Count: 75);
  Skills    : ((Skill: skCrossBow; Level: 10), (Skill: skTrap; Level: 10));
  ),
  ( // Deep Dwarf
  NameLangID: 190;
  Strength  : 1;
  Dexterity : 1;
  Will      : 0;
  Speed     : -2;
  RHWeapon  : (ID: 'HATCHET';       Count: 1);
  LHWeapon  : (ID: 'SMALLSHIELD';   Count: 1);
  Skills    : ((Skill: skAxe; Level: 10), (Skill: skShield; Level: 10));
  ),
  ( // Cave Dwarf
  NameLangID: 191;
  Strength  : -1;
  Dexterity : 4;
  Will      : -2;
  Speed     : -1;
  RHWeapon  : (ID: 'STONEHAMMER';       Count: 1);
  LHWeapon  : (ID: '';   Count: 0);
  Skills    : ((Skill: skMace; Level: 15), (Skill: skTrap; Level: 5));
  )
  );
  
implementation

end.
