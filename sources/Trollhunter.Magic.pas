unit Trollhunter.Magic;

interface

uses
  Trollhunter.Types;

type
  TSpellSchool = (
    scArcane,
    scDivine,
    scNature,
    scShadow,
    scElemental
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
    ManaCost: Byte;
    Description: string;
  end;

const
  SpellData: array[TSpellEnum] of TSpellData = (

    // Arcane
    (Name: 'Mana Shield';
     School: scArcane;
     Level: 1;
     ManaCost: 25;
     Description: 'Absorbs damage using mana'),

    // Divine
    (Name: 'Heal';
     School: scDivine;
     Level: 1;
     ManaCost: 15;
     Description: 'Restores health'),

    // Nature
    (Name: 'Regeneration';
     School: scNature;
     Level: 1;
     ManaCost: 22;
     Description: 'Gradually restores health over time'),

    // Shadow
    (Name: 'Curse';
     School: scShadow;
     Level: 1;
     ManaCost: 30;
     Description: 'Weakens the target'),

    // Elemental
    (Name: 'Fire Arrow';
     School: scElemental;
     Level: 1;
     ManaCost: 2;
     Description: 'Hurls a flaming arrow at the target')
  );

implementation

end.
