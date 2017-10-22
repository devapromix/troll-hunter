unit uAffixes;

interface

uses uCreature, uItem, uBeaRLibItemsCommon;

type
  TSuffixBase = record
    Name: string;
    Level: TMinMax;
    // Color: Cardinal;
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
  TSuffixEnum = (aNone, aVision, aLife1, aLife2, aLife3, aLife4, aLife5, aLife6,
    aLife7, aMana1, aMana2, aMana3, aMana4, aMana5, aMana6, aMana7, aAtr1,
    aAtr2, aAtr3, aAtr4, aAtr5, aAtr6, aAtr7, aDefense1, aDefense2, aDefense3,
    aDefense4, aDefense5, aDefense6, aDefense7, aDamage1, aDamage2, aDamage3,
    aDamage4, aDamage5, aDamage6, aDamage7, aDurability1, aDurability2,
    aDurability3, aDurability4, aDurability5, aDurability6, aDurability7);

const
  DefenseSuffixes = [aDefense1 .. aDefense7];
  DamageSuffixes = [aDamage1 .. aDamage7];
  DurabilitySuffixes = [aDurability1 .. aDurability7];

const
  SuffixBase: array [TSuffixEnum] of TSuffixBase = (
    // None
    (),
    // of Radiance (Vision I)
    (Name: 'of Radiance'; Level: (Min: 1; Max: 15); Price: 1000;
    Occurence: JewelryTypeItems),
    // (Life I)
    (Name: 'of Life I'; Level: (Min: 1; Max: 3); Price: 100;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 1; Max: 4);),
    // (Life II)
    (Name: 'of Life II'; Level: (Min: 2; Max: 5); Price: 200;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 5; Max: 8);),
    // (Life III)
    (Name: 'of Life III'; Level: (Min: 3; Max: 7); Price: 300;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 9; Max: 12);),
    // (Life IV)
    (Name: 'of Life IV'; Level: (Min: 4; Max: 9); Price: 400;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 13; Max: 16);),
    // (Life V)
    (Name: 'of Life V'; Level: (Min: 5; Max: 11); Price: 500;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 17; Max: 20);),
    // (Life VI)
    (Name: 'of Life VI'; Level: (Min: 6; Max: 13); Price: 750;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 21; Max: 25);),
    // (Life VII)
    (Name: 'of Life VII'; Level: (Min: 7; Max: 15); Price: 1000;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 26; Max: 30);),
    // (Mana I)
    (Name: 'of Mana I'; Level: (Min: 1; Max: 3); Price: 100;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 0; Max: 0);
    Mana: (Min: 1; Max: 4);),
    // (Mana II)
    (Name: 'of Mana II'; Level: (Min: 2; Max: 5); Price: 200;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 0; Max: 0);
    Mana: (Min: 5; Max: 8);),
    // (Mana III)
    (Name: 'of Mana III'; Level: (Min: 3; Max: 7); Price: 300;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 0; Max: 0);
    Mana: (Min: 9; Max: 12);),
    // (Mana IV)
    (Name: 'of Mana IV'; Level: (Min: 4; Max: 9); Price: 400;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 0; Max: 0);
    Mana: (Min: 13; Max: 16);),
    // (Mana V)
    (Name: 'of Mana V'; Level: (Min: 5; Max: 11); Price: 500;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 0; Max: 0);
    Mana: (Min: 17; Max: 20);),
    // (Mana VI)
    (Name: 'of Mana VI'; Level: (Min: 6; Max: 13); Price: 750;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 0; Max: 0);
    Mana: (Min: 21; Max: 25);),
    // (Mana VII)
    (Name: 'of Mana VII'; Level: (Min: 7; Max: 15); Price: 1000;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 0; Max: 0);
    Mana: (Min: 26; Max: 30);),
    // (Life and Mana I)
    (Name: 'of Atr I'; Level: (Min: 1; Max: 3); Price: 150;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 1; Max: 3);
    Mana: (Min: 1; Max: 3);),
    // (Life and Mana II)
    (Name: 'of Atr II'; Level: (Min: 2; Max: 5); Price: 300;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 4; Max: 6);
    Mana: (Min: 4; Max: 6);),
    // (Life and Mana III)
    (Name: 'of Atr III'; Level: (Min: 3; Max: 7); Price: 500;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 7; Max: 9);
    Mana: (Min: 7; Max: 9);),
    // (Life and Mana IV)
    (Name: 'of Atr IV'; Level: (Min: 4; Max: 9); Price: 700;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 10; Max: 12);
    Mana: (Min: 10; Max: 12);),
    // (Life and Mana V)
    (Name: 'of Atr V'; Level: (Min: 5; Max: 11); Price: 1000;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 13; Max: 17);
    Mana: (Min: 13; Max: 17);),
    // (Life and Mana VI)
    (Name: 'of Atr VI'; Level: (Min: 6; Max: 13); Price: 1500;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 18; Max: 23);
    Mana: (Min: 18; Max: 23);),
    // (Life and Mana VII)
    (Name: 'of Atr VII'; Level: (Min: 7; Max: 15); Price: 2000;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 24; Max: 30);
    Mana: (Min: 23; Max: 30);),
    // (Defense I)
    (Name: 'of Defense I'; Level: (Min: 1; Max: 3); Price: 100;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 1; Max: 4);),
    // (Defense II)
    (Name: 'of Defense II'; Level: (Min: 2; Max: 5); Price: 200;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 5; Max: 8);),
    // (Defense III)
    (Name: 'of Defense III'; Level: (Min: 3; Max: 7); Price: 300;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 9; Max: 12);),
    // (Defense IV)
    (Name: 'of Defense IV'; Level: (Min: 4; Max: 9); Price: 400;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 11; Max: 16);),
    // (Defense V)
    (Name: 'of Defense V'; Level: (Min: 5; Max: 11); Price: 500;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 17; Max: 20);),
    // (Defense VI)
    (Name: 'of Defense VI'; Level: (Min: 6; Max: 13); Price: 750;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 20; Max: 25);),
    // (Defense VII)
    (Name: 'of Defense VII'; Level: (Min: 7; Max: 15); Price: 1000;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 25; Max: 30);),
    // (Damage I)
    (Name: 'of Damage I'; Level: (Min: 1; Max: 3); Price: 250;
    Occurence: DamageTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 1; Max: 3);
    MaxDamage: (Min: 3; Max: 5));),
    // (Damage II)
    (Name: 'of Damage II'; Level: (Min: 2; Max: 5); Price: 500;
    Occurence: DamageTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 3; Max: 6);
    MaxDamage: (Min: 6; Max: 10));),
    // (Damage III)
    (Name: 'of Damage III'; Level: (Min: 3; Max: 7); Price: 800;
    Occurence: DamageTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 6; Max: 9);
    MaxDamage: (Min: 9; Max: 15));),
    // (Damage IV)
    (Name: 'of Damage IV'; Level: (Min: 4; Max: 9); Price: 1000;
    Occurence: DamageTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 9; Max: 12);
    MaxDamage: (Min: 12; Max: 20));),
    // (Damage V)
    (Name: 'of Damage V'; Level: (Min: 5; Max: 11); Price: 1500;
    Occurence: DamageTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 12; Max: 15);
    MaxDamage: (Min: 15; Max: 25));),
    // (Damage VI)
    (Name: 'of Damage VI'; Level: (Min: 6; Max: 13); Price: 2000;
    Occurence: DamageTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 15; Max: 18);
    MaxDamage: (Min: 18; Max: 30));),
    // (Damage VII)
    (Name: 'of Damage VII'; Level: (Min: 7; Max: 15); Price: 2500;
    Occurence: DamageTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 18; Max: 20);
    MaxDamage: (Min: 21; Max: 35));),

    // of Craftmanship (Durability I)
    (Name: 'of Craftmanship'; Level: (Min: 1; Max: 3); Price: 100;
    Occurence: SmithTypeItems; MaxDurability: (Min: 10; Max: 20);),
    // of Durability (Durability II)
    (Name: 'of Durability'; Level: (Min: 2; Max: 5); Price: 200;
    Occurence: SmithTypeItems; MaxDurability: (Min: 20; Max: 30);),
    // of Sturdiness (Durability III)
    (Name: 'of Sturdiness'; Level: (Min: 3; Max: 7); Price: 300;
    Occurence: SmithTypeItems; MaxDurability: (Min: 30; Max: 40);),
    // of Structure (Durability IV)
    (Name: 'of Structure'; Level: (Min: 4; Max: 9); Price: 400;
    Occurence: SmithTypeItems; MaxDurability: (Min: 40; Max: 50);),
    // of Endurance (Durability V)
    (Name: 'of Endurance'; Level: (Min: 5; Max: 11); Price: 500;
    Occurence: SmithTypeItems; MaxDurability: (Min: 50; Max: 60);),
    // of the Ages (Durability VI)
    (Name: 'of the Ages'; Level: (Min: 6; Max: 13); Price: 750;
    Occurence: SmithTypeItems; MaxDurability: (Min: 60; Max: 70);),
    // of Permanance (Durability VII)
    (Name: 'of Permanance'; Level: (Min: 7; Max: 15); Price: 1000;
    Occurence: SmithTypeItems; MaxDurability: (Min: 70; Max: 80);));

type
  TAffixes = class(TObject)
  public
    function GetSuffixName(const SuffixEnum: TSuffixEnum): string;
    procedure DoSuffix(var AItem: Item);
  end;

var
  Affixes: TAffixes = nil;

implementation

uses SysUtils, Math, uTerminal, uLanguage, uGame, uPlayer;

const
  SuffixName: array [TSuffixEnum] of string = ('', 'of Radiance',
    // Life
    'of Life I', 'of Life II', 'of Life III', 'of Life IV', 'of Life V',
    'of Life VI', 'of Life VII',
    // Mana
    'of Mana I', 'of Mana II', 'of Mana III', 'of Mana IV', 'of Mana V',
    'of Mana VI', 'of Mana VII',
    // Mana and Life
    'of Atr I', 'of Atr II', 'of Atr III', 'of Atr IV', 'of Atr V', 'of Atr VI',
    'of Atr VII',
    // Defense
    'of Defense I', 'of Defense II', 'of Defense III', 'of Defense IV',
    'of Defense V', 'of Defense VI', 'of Defense VII',
    // Damage
    'of Damage I', 'of Damage II', 'of Damage III', 'of Damage IV',
    'of Damage V', 'of Damage VI', 'of Damage VII',
    // Durability
    'of Craftmanship', 'of Sturdiness', 'of Durability', 'of Structure',
    'of the Ages', 'of Endurance', 'of Permanance');

procedure TAffixes.DoSuffix(var AItem: Item);
var
  SB: TSuffixBase;
  Value: Byte;

  procedure SetLife();
  begin
    if (SB.Life.Min > 0) then
    begin
      Value := Items.GetBonus(AItem, btLife) +
        Math.EnsureRange(Math.RandomRange(SB.Life.Min, SB.Life.Max + 1), 1,
        High(Byte));
      Items.SetBonus(AItem, btLife, Value, False);
    end;
  end;

  procedure SetMana();
  begin
    if (SB.Mana.Min > 0) then
    begin
      Value := Items.GetBonus(AItem, btMana) +
        Math.EnsureRange(Math.RandomRange(SB.Mana.Min, SB.Mana.Max + 1), 1,
        High(Byte));
      Items.SetBonus(AItem, btMana, Value, False);
    end;
  end;

begin
  SB := SuffixBase[TSuffixEnum(AItem.Identify)];
  case TSuffixEnum(AItem.Identify) of
    // Vision
    aVision:
      begin
        Value := Items.GetBonus(AItem, btVis) + 1;
        Items.SetBonus(AItem, btVis, Value, False);
      end;
    // Life
    aLife1 .. aLife7:
      SetLife();
    // Mana
    aMana1 .. aMana7:
      SetMana();
    // Life and Mana
    aAtr1 .. aAtr7:
      begin
        SetLife();
        SetMana();
      end;
    // Defense
    aDefense1 .. aDefense7:
      begin
        if (SB.Defense.Min > 0) then
          AItem.Defense := AItem.Defense + Math.EnsureRange
            (Math.RandomRange(SB.Defense.Min, SB.Defense.Max + 1), 1,
            High(Byte));
      end;
    // Damage
    aDamage1 .. aDamage7:
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
    aDurability1 .. aDurability7:
      begin
        if (SB.MaxDurability.Min > 0) then
        begin
          AItem.MaxDurability := AItem.MaxDurability +
            Math.EnsureRange(Math.RandomRange(SB.MaxDurability.Min,
            SB.MaxDurability.Max + 1), 1, High(Byte));
          case Game.Difficulty of
            dfEasy:
              AItem.Durability := AItem.MaxDurability;
            dfHell:
              AItem.Durability := Math.RandomRange(0, 5) + 1;
          end;
        end;
      end;
  end;
  // Price
  uItem.TItems.CalcItem(AItem, SB.Price);
end;

function TAffixes.GetSuffixName(const SuffixEnum: TSuffixEnum): string;
begin
  Result := SuffixName[SuffixEnum];
end;

initialization

Affixes := TAffixes.Create;

finalization

FreeAndNil(Affixes);

end.
