unit Trollhunter.Spell.School;

interface

type
  TSpellSchoolEnum = (
    scArcane,
    scDivine,
    scNature,
    scShadow,
    scElemental
    );

type
  TSpellSchoolData = record
    Name: string;
    Color: string;
  end;

function GetSpellSchoolData(const ASpellSchoolEnum: TSpellSchoolEnum): TSpellSchoolData;

implementation

const
  CSpellSchoolData: array[TSpellSchoolEnum] of TSpellSchoolData = (
    // Arcane
    (Name: 'Arcane'; Color: 'lighter yellow'),
    // Divine
    (Name: 'Divine'; Color: 'light blue'),
    // Nature
    (Name: 'Nature'; Color: 'light green'),
    // Shadow
    (Name: 'Shadow'; Color: 'light gray'),
    // Elemental
    (Name: 'Elemental'; Color: 'lighter red'));

function GetSpellSchoolData(const ASpellSchoolEnum: TSpellSchoolEnum): TSpellSchoolData;
begin
  Result := CSpellSchoolData[ASpellSchoolEnum];
end;

end.
