unit Trollhunter.Spell;

interface

uses
  Trollhunter.Types,
  Trollhunter.Effect,
  Trollhunter.Spell.School,
  Trollhunter.Projectile.Types;

type
  TSpellEnum = (
    spNone,

    spManaShield,    {Arcane I}
    spHeal,          {Divine I}
    spRegeneration,  {Nature I}
    spCurse,         {Shadow I}
    spFireArrow,     {Elemental I}

    spTeleport,      {Arcane II}
    spCurePoison,    {Divine II}
    spVerdantSpear,  {Nature II}
    spDrainLife,     {Shadow II}
    spIgnite,        {Elemental II}

    spTownPortal,    {Arcane III}
    spCureWeakness,  {Divine III}
                     {Nature III}
    spBlind,         {Shadow III}
                     {Elemental III}

    spIdentify,      {Arcane IV}
                     {Divine IV}
                     {Nature IV}
                     {Shadow IV}
                     {Elemental IV}

    spInfusion       {Arcane V}
                     {Divine V}
                     {Nature V}
                     {Shadow V}
                     {Elemental V}
    );

type
  TSpellData = record
    Name: string;
    School: TSpellSchoolEnum;
    Level: UInt;
    ManaCost: UInt;
    Effects: TEffects;
    Description: string;
    Value: Int;
    MinDamage: UInt;
    MaxDamage: UInt;
    Projectile: TProjectileEnum;
  end;

function GetSpellData(const ASpellEnum: TSpellEnum): TSpellData;

implementation

const
  CSpellData: array[TSpellEnum] of TSpellData = (

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
    ManaCost: 75;
    Effects: [efManaShield];
    Description: 'Absorbs damage using mana';
    Value: 5;
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
    Value: 20;
    ),
    // Shadow: Curse
    (Name: 'Curse';
    School: scShadow;
    Level: 2;
    ManaCost: 30;
    Effects: [efWeaken];
    Description: 'Weakens the target';
    Value: 15;
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
    Projectile: prFireArrow;
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
    Projectile: prVerdantSpear;
    ),
    // Shadow: Drain Life
    (Name: 'Drain Life';
    School: scShadow;
    Level: 4;
    ManaCost: 35;
    Effects: [efDrain];
    Description: 'Drains the life force of the target';
    Value: 15;
    MinDamage: 4;
    MaxDamage: 8;
    Projectile: prFireArrow;
    ),
    // Elemental: Ignite
    (Name: 'Ignite';
    School: scElemental;
    Level: 3;
    ManaCost: 12;
    Effects: [efBurn];
    Description: 'Sets the target ablaze';
    Value: 5;
    MinDamage: 3;
    MaxDamage: 7;
    Projectile: prFireArrow;
    ),

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
    ),
    // Shadow: Blind
    (Name: 'Blind';
    School: scShadow;
    Level: 6;
    ManaCost: 55;
    Effects: [];
    Description: '';
    Value: 10;
    ),

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

function GetSpellData(const ASpellEnum: TSpellEnum): TSpellData;
begin
  Result := CSpellData[ASpellEnum];
end;

end.
