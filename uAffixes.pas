unit uAffixes;

interface

uses uEntity, uItem, BeaRLibItems;

type
  TSuffixBase = record
    Level: TMinMax;
    Color: Cardinal;
    Price: Word;
    Occurence: set of TItemType;
    MaxDurability: TMinMax;
    Defense: TMinMax;
    Damage: TBaseDamage;
    Life: TMinMax;
    Mana: TMinMax;
    Strength: TMinMax;
    Dexterity: TMinMax;
    Willpower: TMinMax;
    Perception: TMinMax;
  end;

type
  TSuffixEnum = (aNone,
    aLife1, aLife2, aLife3, aLife4, aLife5, aLife6, aLife7,
    aMana1, aMana2, aMana3, aMana4, aMana5, aMana6, aMana7,
    aDefense1, aDefense2, aDefense3, aDefense4, aDefense5, aDefense6, aDefense7,
    aDamage1, aDamage2, aDamage3, aDamage4, aDamage5, aDamage6, aDamage7,
    aDurability1, aDurability2, aDurability3, aDurability4, aDurability5,
    aDurability6, aDurability7);

const
  SuffixBase: array [TSuffixEnum] of TSuffixBase = (
    // None
    (),
    // Life
    (Level: (Min: 1; Max: 3); Price: 100; Occurence: DefenseTypeItems; Life: (Min: 1; Max: 5);),
    (Level: (Min: 2; Max: 5); Price: 200; Occurence: DefenseTypeItems; Life: (Min: 6; Max: 10);),
    (Level: (Min: 3; Max: 7); Price: 300; Occurence: DefenseTypeItems; Life: (Min: 11; Max: 15);),
    (Level: (Min: 4; Max: 9); Price: 400; Occurence: DefenseTypeItems; Life: (Min: 16; Max: 20);),
    (Level: (Min: 5; Max: 11); Price: 500; Occurence: DefenseTypeItems; Life: (Min: 21; Max: 25);),
    (Level: (Min: 6; Max: 13); Price: 750; Occurence: DefenseTypeItems; Life: (Min: 26; Max: 30);),
    (Level: (Min: 7; Max: 15); Price: 1000; Occurence: DefenseTypeItems; Life: (Min: 31; Max: 35);),
    // Mana
    (Level: (Min: 1; Max: 3); Price: 100; Occurence: DefenseTypeItems; Mana: (Min: 1; Max: 5);),
    (Level: (Min: 2; Max: 5); Price: 200; Occurence: DefenseTypeItems; Mana: (Min: 6; Max: 10);),
    (Level: (Min: 3; Max: 7); Price: 300; Occurence: DefenseTypeItems; Mana: (Min: 11; Max: 15);),
    (Level: (Min: 4; Max: 9); Price: 400; Occurence: DefenseTypeItems; Mana: (Min: 16; Max: 20);),
    (Level: (Min: 5; Max: 11); Price: 500; Occurence: DefenseTypeItems; Mana: (Min: 21; Max: 25);),
    (Level: (Min: 6; Max: 13); Price: 750; Occurence: DefenseTypeItems; Mana: (Min: 26; Max: 30);),
    (Level: (Min: 7; Max: 15); Price: 1000; Occurence: DefenseTypeItems; Mana: (Min: 31; Max: 35);),
    // Defense
    (Level: (Min: 1; Max: 3); Price: 100; Occurence: DefenseTypeItems; Defense: (Min: 1; Max: 4);),
    (Level: (Min: 2; Max: 5); Price: 200; Occurence: DefenseTypeItems; Defense: (Min: 5; Max: 8);),
    (Level: (Min: 3; Max: 7); Price: 300; Occurence: DefenseTypeItems; Defense: (Min: 9; Max: 12);),
    (Level: (Min: 4; Max: 9); Price: 400; Occurence: DefenseTypeItems; Defense: (Min: 11; Max: 16);),
    (Level: (Min: 5; Max: 11); Price: 500; Occurence: DefenseTypeItems; Defense: (Min: 17; Max: 20);),
    (Level: (Min: 6; Max: 13); Price: 750; Occurence: DefenseTypeItems; Defense: (Min: 20; Max: 25);),
    (Level: (Min: 7; Max: 15); Price: 1000; Occurence: DefenseTypeItems; Defense: (Min: 25; Max: 30);),
    // Damage
    (Level: (Min: 1; Max: 3); Price: 100; Occurence: DamageTypeItems; Damage: (MinDamage: (Min: 1; Max: 3); MaxDamage: (Min: 3; Max: 5));),
    (Level: (Min: 2; Max: 5); Price: 200; Occurence: DamageTypeItems; Damage: (MinDamage: (Min: 3; Max: 6); MaxDamage: (Min: 6; Max: 10));),
    (Level: (Min: 3; Max: 7); Price: 300; Occurence: DamageTypeItems; Damage: (MinDamage: (Min: 6; Max: 9); MaxDamage: (Min: 9; Max: 15));),
    (Level: (Min: 4; Max: 9); Price: 400; Occurence: DamageTypeItems; Damage: (MinDamage: (Min: 9; Max: 12); MaxDamage: (Min: 12; Max: 20));),
    (Level: (Min: 5; Max: 11); Price: 500; Occurence: DamageTypeItems; Damage: (MinDamage: (Min: 12; Max: 15); MaxDamage: (Min: 15; Max: 25));),
    (Level: (Min: 6; Max: 13); Price: 750; Occurence: DamageTypeItems; Damage: (MinDamage: (Min: 15; Max: 18); MaxDamage: (Min: 18; Max: 30));),
    (Level: (Min: 7; Max: 15); Price: 1000; Occurence: DamageTypeItems; Damage: (MinDamage: (Min: 18; Max: 20); MaxDamage: (Min: 21; Max: 35));),
    // Durability
    (Level: (Min: 1; Max: 3); Price: 100; Occurence: IdentTypeItems; MaxDurability: (Min: 10; Max: 20);),
    (Level: (Min: 2; Max: 5); Price: 200; Occurence: IdentTypeItems; MaxDurability: (Min: 20; Max: 30);),
    (Level: (Min: 3; Max: 7); Price: 300; Occurence: IdentTypeItems; MaxDurability: (Min: 30; Max: 40);),
    (Level: (Min: 4; Max: 9); Price: 400; Occurence: IdentTypeItems; MaxDurability: (Min: 40; Max: 50);),
    (Level: (Min: 5; Max: 11); Price: 500; Occurence: IdentTypeItems; MaxDurability: (Min: 50; Max: 60);),
    (Level: (Min: 6; Max: 13); Price: 750; Occurence: IdentTypeItems; MaxDurability: (Min: 60; Max: 70);),
    (Level: (Min: 7; Max: 15); Price: 1000; Occurence: IdentTypeItems; MaxDurability: (Min: 70; Max: 80);)
    );

type
  TAffixes = class
  public
    function GetSuffixName(SuffixEnum: TSuffixEnum): string;
    procedure DoSuffix(var AItem: Item);
  end;

var
  Affixes: TAffixes = nil;

implementation

uses SysUtils, Math, uTerminal, GNUGetText;

procedure TAffixes.DoSuffix(var AItem: Item);
var
  SB: TSuffixBase;
begin
  SB := SuffixBase[TSuffixEnum(AItem.Identify)];
  case TSuffixEnum(AItem.Identify) of
    // Life
    aLife1..aLife7:
      begin

      end;
    // Mana
    aMana1..aMana7:
      begin

      end;
    // Defense
    aDefense1..aDefense7:
      begin
        if (SB.Defense.Min > 0) then
          AItem.Defense := AItem.Defense +
            Math.EnsureRange(Math.RandomRange(SB.Defense.Min,
            SB.Defense.Max + 1), 1, High(Byte));
      end;
    // Damage
    aDamage1..aDamage7:
      begin
        if (SB.Damage.MinDamage.Min > 0) then
          AItem.MinDamage := AItem.MinDamage +
            Math.EnsureRange(Math.RandomRange(SB.Damage.MinDamage.Min,
            SB.Damage.MinDamage.Max + 1), 1, High(Byte) - 1);
        if (SB.Damage.MaxDamage.Min > 0) then
          AItem.MaxDamage := AItem.MaxDamage +
            Math.EnsureRange(Math.RandomRange(SB.Damage.MaxDamage.Min,
            SB.Damage.MaxDamage.Max + 1), 2, High(Byte));
      end;
    // Durability
    aDurability1..aDurability7:
      begin
        if (SB.MaxDurability.Min > 0) then
          AItem.MaxDurability := AItem.MaxDurability +
            Math.EnsureRange(Math.RandomRange(SB.MaxDurability.Min,
            SB.MaxDurability.Max + 1), 1, High(Byte));
      end;
  end;
  // Price
  TItems.CalcItem(AItem, SB.Price);
end;

function TAffixes.GetSuffixName(SuffixEnum: TSuffixEnum): string;
begin
  case SuffixEnum of
    aLife1:
      Result := _(' of Life I');
    aLife2:
      Result := _(' of Life II');
    aLife3:
      Result := _(' of Life III');
    aLife4:
      Result := _(' of Life IV');
    aLife5:
      Result := _(' of Life V');
    aLife6:
      Result := _(' of Life VI');
    aLife7:
      Result := _(' of Life VII');
    aMana1:
      Result := _(' of Mana I');
    aMana2:
      Result := _(' of Mana II');
    aMana3:
      Result := _(' of Mana III');
    aMana4:
      Result := _(' of Mana IV');
    aMana5:
      Result := _(' of Mana V');
    aMana6:
      Result := _(' of Mana VI');
    aMana7:
      Result := _(' of Mana VII');
    aDefense1:
      Result := _(' of Defense I');
    aDefense2:
      Result := _(' of Defense II');
    aDefense3:
      Result := _(' of Defense III');
    aDefense4:
      Result := _(' of Defense IV');
    aDefense5:
      Result := _(' of Defense V');
    aDefense6:
      Result := _(' of Defense VI');
    aDefense7:
      Result := _(' of Defense VII');
    aDamage1:
      Result := _(' of Damage I');
    aDamage2:
      Result := _(' of Damage II');
    aDamage3:
      Result := _(' of Damage III');
    aDamage4:
      Result := _(' of Damage IV');
    aDamage5:
      Result := _(' of Damage V');
    aDamage6:
      Result := _(' of Damage VI');
    aDamage7:
      Result := _(' of Damage VII');
    aDurability1:
      Result := _(' of Durability I');
    aDurability2:
      Result := _(' of Durability II');
    aDurability3:
      Result := _(' of Durability III');
    aDurability4:
      Result := _(' of Durability IV');
    aDurability5:
      Result := _(' of Durability V');
    aDurability6:
      Result := _(' of Durability VI');
    aDurability7:
      Result := _(' of Durability VII');
  else
    Result := '';
  end;
end;

initialization

Affixes := TAffixes.Create;

finalization

FreeAndNil(Affixes);

end.
