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
    spManaShield,
    spHeal,
    spRegeneration,
    spCurse,
    spFireArrow
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

    // Arcane
    (Name: 'Mana Shield';
     School: scArcane;
     Level: 2;
     ManaCost: 25;
     Effects: [];
     Description: 'Absorbs damage using mana;';
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
     Level: 1;
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
     )
  );

implementation

end.
