unit uAffixes;

interface

uses uCreature, uItem, uBeaRLibItemsCommon;

type
  TSuffixBase = record
    Level: TMinMax;
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
  TSuffixEnum = (
    // None
    None,
    // Vision
    of_Radiance,
    // Life I - VII
    of_Life1, of_Life2, of_Life3, of_Life4, of_Life5, of_Life6, of_Life7,
    // Mana I - VII
    of_Mana1, of_Mana2, of_Mana3, of_Mana4, of_Mana5, of_Mana6, of_Mana7,
    // Life and Mana I - VII
    of_Atr1, of_Atr2, of_Atr3, of_Atr4, of_Atr5, of_Atr6, of_Atr7,
    // Defense I - VII
    of_Defense1, of_Defense2, of_Defense3, of_Defense4, of_Defense5,
    of_Defense6, of_Defense7,
    // Damage I - VII
    of_Damage1, of_Damage2, of_Damage3, of_Damage4, of_Damage5, of_Damage6,
    of_Damage7,
    // Durability I - VII
    of_Craftmanship, of_Durability, of_Sturdiness, of_Structure,
    of_Endurance, of_the_Ages, of_Permanance);

const
  DefenseSuffixes = [of_Defense1 .. of_Defense7];
  DamageSuffixes = [of_Damage1 .. of_Damage7];
  DurabilitySuffixes = [of_Craftmanship .. of_Permanance];

const
  SuffixBase: array [TSuffixEnum] of TSuffixBase = (
    // None
    (),
    // of Radiance (Vision I)
    (Level: (Min: 1; Max: 15); Price: 1000;
    Occurence: JewelryTypeItems),
    // (Life I)
    (Level: (Min: 1; Max: 3); Price: 100;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 1; Max: 4);),
    // (Life II)
    (Level: (Min: 2; Max: 5); Price: 200;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 5; Max: 8);),
    // (Life III)
    (Level: (Min: 3; Max: 7); Price: 300;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 9; Max: 12);),
    // (Life IV)
    (Level: (Min: 4; Max: 9); Price: 400;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 13; Max: 16);),
    // (Life V)
    (Level: (Min: 5; Max: 11); Price: 500;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 17; Max: 20);),
    // (Life VI)
    (Level: (Min: 6; Max: 13); Price: 750;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 21; Max: 25);),
    // (Life VII)
    (Level: (Min: 7; Max: 15); Price: 1000;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 26; Max: 30);),
    // (Mana I)
    (Level: (Min: 1; Max: 3); Price: 100;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 0; Max: 0);
    Mana: (Min: 1; Max: 4);),
    // (Mana II)
    (Level: (Min: 2; Max: 5); Price: 200;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 0; Max: 0);
    Mana: (Min: 5; Max: 8);),
    // (Mana III)
    (Level: (Min: 3; Max: 7); Price: 300;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 0; Max: 0);
    Mana: (Min: 9; Max: 12);),
    // (Mana IV)
    (Level: (Min: 4; Max: 9); Price: 400;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 0; Max: 0);
    Mana: (Min: 13; Max: 16);),
    // (Mana V)
    (Level: (Min: 5; Max: 11); Price: 500;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 0; Max: 0);
    Mana: (Min: 17; Max: 20);),
    // (Mana VI)
    (Level: (Min: 6; Max: 13); Price: 750;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 0; Max: 0);
    Mana: (Min: 21; Max: 25);),
    // (Mana VII)
    (Level: (Min: 7; Max: 15); Price: 1000;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 0; Max: 0);
    Mana: (Min: 26; Max: 30);),
    // (Life and Mana I)
    (Level: (Min: 1; Max: 3); Price: 150;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 1; Max: 3);
    Mana: (Min: 1; Max: 3);),
    // (Life and Mana II)
    (Level: (Min: 2; Max: 5); Price: 300;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 4; Max: 6);
    Mana: (Min: 4; Max: 6);),
    // (Life and Mana III)
    (Level: (Min: 3; Max: 7); Price: 500;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 7; Max: 9);
    Mana: (Min: 7; Max: 9);),
    // (Life and Mana IV)
    (Level: (Min: 4; Max: 9); Price: 700;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 10; Max: 12);
    Mana: (Min: 10; Max: 12);),
    // (Life and Mana V)
    (Level: (Min: 5; Max: 11); Price: 1000;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 13; Max: 17);
    Mana: (Min: 13; Max: 17);),
    // (Life and Mana VI)
    (Level: (Min: 6; Max: 13); Price: 1500;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 18; Max: 23);
    Mana: (Min: 18; Max: 23);),
    // (Life and Mana VII)
    (Level: (Min: 7; Max: 15); Price: 2000;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 0; Max: 0;);
    MaxDamage: (Min: 0; Max: 0;)); Life: (Min: 24; Max: 30);
    Mana: (Min: 23; Max: 30);),
    // (Defense I)
    (Level: (Min: 1; Max: 3); Price: 100;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 1; Max: 4);),
    // (Defense II)
    (Level: (Min: 2; Max: 5); Price: 200;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 5; Max: 8);),
    // (Defense III)
    (Level: (Min: 3; Max: 7); Price: 300;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 9; Max: 12);),
    // (Defense IV)
    (Level: (Min: 4; Max: 9); Price: 400;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 11; Max: 16);),
    // (Defense V)
    (Level: (Min: 5; Max: 11); Price: 500;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 17; Max: 20);),
    // (Defense VI)
    (Level: (Min: 6; Max: 13); Price: 750;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 20; Max: 25);),
    // (Defense VII)
    (Level: (Min: 7; Max: 15); Price: 1000;
    Occurence: DefenseTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 25; Max: 30);),
    // (Damage I)
    (Level: (Min: 1; Max: 3); Price: 250;
    Occurence: DamageTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 1; Max: 3);
    MaxDamage: (Min: 3; Max: 5));),
    // (Damage II)
    (Level: (Min: 2; Max: 5); Price: 500;
    Occurence: DamageTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 3; Max: 6);
    MaxDamage: (Min: 6; Max: 10));),
    // (Damage III)
    (Level: (Min: 3; Max: 7); Price: 800;
    Occurence: DamageTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 6; Max: 9);
    MaxDamage: (Min: 9; Max: 15));),
    // (Damage IV)
    (Level: (Min: 4; Max: 9); Price: 1000;
    Occurence: DamageTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 9; Max: 12);
    MaxDamage: (Min: 12; Max: 20));),
    // (Damage V)
    (Level: (Min: 5; Max: 11); Price: 1500;
    Occurence: DamageTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 12; Max: 15);
    MaxDamage: (Min: 15; Max: 25));),
    // (Damage VI)
    (Level: (Min: 6; Max: 13); Price: 2000;
    Occurence: DamageTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 15; Max: 18);
    MaxDamage: (Min: 18; Max: 30));),
    // (Damage VII)
    (Level: (Min: 7; Max: 15); Price: 2500;
    Occurence: DamageTypeItems; MaxDurability: (Min: 0; Max: 0);
    Defense: (Min: 0; Max: 0); Damage: (MinDamage: (Min: 18; Max: 20);
    MaxDamage: (Min: 21; Max: 35));),

    // of Craftmanship (Durability I)
    (Level: (Min: 1; Max: 3); Price: 100;
    Occurence: SmithTypeItems; MaxDurability: (Min: 10; Max: 20);),
    // of Durability (Durability II)
    (Level: (Min: 2; Max: 5); Price: 200;
    Occurence: SmithTypeItems; MaxDurability: (Min: 20; Max: 30);),
    // of Sturdiness (Durability III)
    (Level: (Min: 3; Max: 7); Price: 300;
    Occurence: SmithTypeItems; MaxDurability: (Min: 30; Max: 40);),
    // of Structure (Durability IV)
    (Level: (Min: 4; Max: 9); Price: 400;
    Occurence: SmithTypeItems; MaxDurability: (Min: 40; Max: 50);),
    // of Endurance (Durability V)
    (Level: (Min: 5; Max: 11); Price: 500;
    Occurence: SmithTypeItems; MaxDurability: (Min: 50; Max: 60);),
    // of The Ages (Durability VI)
    (Level: (Min: 6; Max: 13); Price: 750;
    Occurence: SmithTypeItems; MaxDurability: (Min: 60; Max: 70);),
    // of Permanance (Durability VII)
    (Level: (Min: 7; Max: 15); Price: 1000;
    Occurence: SmithTypeItems; MaxDurability: (Min: 70; Max: 80);));

type
  TAffixes = class(TObject)
  public
    SuffixName: array [TSuffixEnum] of string;
    constructor Create();
    function GetSuffixName(const SuffixEnum: TSuffixEnum): string;
    procedure DoSuffix(var AItem: Item);
  end;

var
  Affixes: TAffixes = nil;

implementation

uses SysUtils, TypInfo, Math, uTerminal, uLanguage, uGame, uPlayer;

constructor TAffixes.Create();
var
  I: TSuffixEnum;
  P: Pointer;
begin
  P := TypeInfo(TSuffixEnum);
  for I := Low(TSuffixEnum) to High(TSuffixEnum) do
    SuffixName[I] := StringReplace(GetEnumName(P, Ord(I)), '_', ' ', [rfReplaceAll]);
end;

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
    of_Radiance:
      begin
        Value := Items.GetBonus(AItem, btVis) + 1;
        Items.SetBonus(AItem, btVis, Value, False);
      end;
    // Life
    of_Life1 .. of_Life7:
      SetLife();
    // Mana
    of_Mana1 .. of_Mana7:
      SetMana();
    // Life and Mana
    of_Atr1 .. of_Atr7:
      begin
        SetLife();
        SetMana();
      end;
    // Defense
    of_Defense1 .. of_Defense7:
      begin
        if (SB.Defense.Min > 0) then
          AItem.Defense := AItem.Defense + Math.EnsureRange
            (Math.RandomRange(SB.Defense.Min, SB.Defense.Max + 1), 1,
            High(Byte));
      end;
    // Damage
    of_Damage1 .. of_Damage7:
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
    of_Craftmanship .. of_Permanance:
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
