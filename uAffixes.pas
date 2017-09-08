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
  end;

type
  TSuffixEnum = (aNone, aDefense1, aDefense2, aDefense3, aDefense4, aDefense5,
    aDefense6, aDefense7,
    aDamage1, aDamage2, aDamage3, aDamage4, aDamage5, aDamage6, aDamage7,
    aDurability1, aDurability2, aDurability3, aDurability4, aDurability5,
    aDurability6, aDurability7);

const
  SuffixBase: array [TSuffixEnum] of TSuffixBase = (
    // None
    (),
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
    aDefense1:
      Result := ' of Defense I';
    aDefense2:
      Result := ' of Defense II';
    aDefense3:
      Result := ' of Defense III';
    aDefense4:
      Result := ' of Defense IV';
    aDefense5:
      Result := ' of Defense V';
    aDefense6:
      Result := ' of Defense VI';
    aDefense7:
      Result := ' of Defense VII';
    aDamage1:
      Result := ' of Damage I';
    aDamage2:
      Result := ' of Damage II';
    aDamage3:
      Result := ' of Damage III';
    aDamage4:
      Result := ' of Damage IV';
    aDamage5:
      Result := ' of Damage V';
    aDamage6:
      Result := ' of Damage VI';
    aDamage7:
      Result := ' of Damage VII';
    aDurability1:
      Result := ' of Durability I';
    aDurability2:
      Result := ' of Durability II';
    aDurability3:
      Result := ' of Durability III';
    aDurability4:
      Result := ' of Durability IV';
    aDurability5:
      Result := ' of Durability V';
    aDurability6:
      Result := ' of Durability VI';
    aDurability7:
      Result := ' of Durability VII';
  else
    Result := '';
  end;
end;

initialization

Affixes := TAffixes.Create;

finalization

FreeAndNil(Affixes);

end.
