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
    spCurePoison,
    spVerdantSpear,

    spTownPortal,
    spCureWeakness,
    spIdentify,
    spInfusion
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
    MinDamage: UInt;
    MaxDamage: UInt;
    IsTargeted: boolean;
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

   // Arcane: Mana Shield
   (Name: 'Mana Shield';
    School: scArcane;
    Level: 1;
    ManaCost: 25;
    Effects: [];
    Description: 'Absorbs damage using mana';
    Value: 0;
    ),

    // Divine: Heal
    (Name: 'Heal';
     School: scDivine;
     Level: 1;
     ManaCost: 20;
     Effects: [efLife];
     Description: 'Restores health';
     Value: 25;
     ),

    // Nature: Regeneration
    (Name: 'Regeneration';
     School: scNature;
     Level: 2;
     ManaCost: 22;
     Effects: [efRegeneration];
     Description: 'Gradually restores health over time';
     Value: 0;
     ),

    // Shadow: Curse
    (Name: 'Curse';
     School: scShadow;
     Level: 2;
     ManaCost: 30;
     Effects: [];
     Description: 'Weakens the target';
     Value: 0;
     ),

    // Elemental: Fire Arrow
    (Name: 'Fire Arrow';
     School: scElemental;
     Level: 1;
     ManaCost: 1;
     Effects: [];
     Description: 'Hurls a flaming arrow at the target';
     Value: 0;
     MinDamage: 2;
     MaxDamage: 4;
     IsTargeted: True;
     ),

    // Arcane: Teleport
    (Name: 'Teleport';
     School: scArcane;
     Level: 4;
     ManaCost: 45;
     Effects: [efTeleportation];
     Description: 'Teleports you to a random nearby location';
     Value: 0;
     ),

     // Divine: Cure Poison
     (Name: 'Cure Poison';
      School: scDivine;
      Level: 3;
      ManaCost: 35;
      Effects: [efCurePoison];
      Description: 'Neutralizes poison in your body';
      Value: 15;
      ),

      // Nature: Verdant Spear
      (Name: 'Verdant Spear';
       School: scNature;
       Level: 4;
       ManaCost: 2;
       Effects: [];
       Description: 'Conjures a spear of pure nature energy';
       Value: 0;
       MinDamage: 3;
       MaxDamage: 9;
       IsTargeted: True;
       ),

     // Shadow:

     // Elemental:


     // Arcane: Town Portal
    (Name: 'Town Portal';
     School: scArcane;
     Level: 6;
     ManaCost: 75;
     Effects: [efTownPortal];
     Description: 'Opens a portal back to town';
     Value: 0;
     ),

     // Divine: Cure Weakness
    (Name: 'Cure Weakness';
     School: scDivine;
     Level: 5;
     ManaCost: 40;
     Effects: [efCureWeak];
     Description: 'Cures weakness';
     Value: 0;
    ) ,

     // Arcane: Identify
    (Name: 'Identify';
     School: scArcane;
     Level: 8;
     ManaCost: 80;
     Effects: [efIdentification];
     Description: 'Identifies an unknown item';
     Value: 0;
     ),

     // Arcane: Infusion
    (Name: 'Infusion';
     School: scArcane;
     Level: 10;
     ManaCost: 100;
     Effects: [efCraftAtr];
     Description: 'Imbues an item with a random enchantment';
     Value: 0;
     )
  );

implementation

end.
