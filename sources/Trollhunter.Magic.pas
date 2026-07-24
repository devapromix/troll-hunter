unit Trollhunter.Magic;

interface

uses
  Trollhunter.Types,
  Trollhunter.Creature;

type
  TSpellSchool = (
    scArcane,
    scDivine,
    scNature,
    scShadow,
    scElemental
  );

const
  CSpellSchoolName: array [TSpellSchool] of string = (
    'Arcane',
    'Divine',
    'Nature',
    'Shadow',
    'Elemental'
  );

const
  CSpellSchoolColor: array [TSpellSchool] of string = (
    'lighter yellow',
    'light blue',
    'light green',
    'light gray',
    'light red'
  );

type
  TSpellEnum = (
    spNone,
    spManaShield,
    spHeal,
    spRegeneration,
    spCurse,
    spFireArrow,
    spTeleport,
    spTownPortal,
    spIdentify,
    spInfusion,
    spCurePoison,
    spCureWeakness
  );

type
  TSpellData = record
    Name: string;
    School: TSpellSchool;
    Level: UInt;
    ManaCost: UInt;
    Effects: TEffects;
    Description: string;
    Value: Int;
  end;

const
  SpellData: array[TSpellEnum] of TSpellData = (

  // None
  (Name: '';
   School: scArcane;
   Level: 0;
   ManaCost: 0;
   Effects: [];
   Description: '';
   Value: 0;
   ),

   // Arcane
   (Name: 'Mana Shield';
    School: scArcane;
    Level: 1;
    ManaCost: 25;
    Effects: [];
    Description: 'Absorbs damage using mana';
    Value: 0;
    ),

    // Divine
    (Name: 'Heal';
     School: scDivine;
     Level: 1;
     ManaCost: 15;
     Effects: [efLife];
     Description: 'Restores health';
     Value: 35;
     ),

    // Nature
    (Name: 'Regeneration';
     School: scNature;
     Level: 2;
     ManaCost: 22;
     Effects: [efRegeneration];
     Description: 'Gradually restores health over time';
     Value: 0;
     ),

    // Shadow
    (Name: 'Curse';
     School: scShadow;
     Level: 2;
     ManaCost: 30;
     Effects: [];
     Description: 'Weakens the target';
     Value: 0;
     ),

    // Elemental
    (Name: 'Fire Arrow';
     School: scElemental;
     Level: 1;
     ManaCost: 2;
     Effects: [];
     Description: 'Hurls a flaming arrow at the target';
     Value: 0;
     ),

    // Arcane
    (Name: 'Teleport';
     School: scArcane;
     Level: 1;
     ManaCost: 20;
     Effects: [efTeleportation];
     Description: 'Teleports you to a random nearby location';
     Value: 0;
     ),

    (Name: 'Town Portal';
     School: scArcane;
     Level: 1;
     ManaCost: 25;
     Effects: [efTownPortal];
     Description: 'Opens a portal back to town';
     Value: 0;
     ),

    (Name: 'Identify';
     School: scArcane;
     Level: 1;
     ManaCost: 10;
     Effects: [efIdentification];
     Description: 'Identifies an unknown item';
     Value: 0;
     ),

    (Name: 'Infusion';
     School: scArcane;
     Level: 1;
     ManaCost: 30;
     Effects: [efCraftAtr];
     Description: 'Imbues an item with a random enchantment';
     Value: 0;
     ),

    // Divine
    (Name: 'Cure Poison';
     School: scDivine;
     Level: 1;
     ManaCost: 15;
     Effects: [efCurePoison];
     Description: 'Neutralizes poison in your body';
     Value: 15;
     ),

    (Name: 'Cure Weakness';
     School: scDivine;
     Level: 1;
     ManaCost: 12;
     Effects: [efCureWeak];
     Description: 'Cures weakness';
     Value: 0;
     )
  );

implementation

end.
