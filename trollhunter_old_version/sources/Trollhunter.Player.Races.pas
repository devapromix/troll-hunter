unit Trollhunter.Player.Races;

interface

uses
  Trollhunter.Types,
  Trollhunter.Creature,
  Trollhunter.Attribute;

type
  TRaceEnum = (rcHuman, rcHalfling, rcElf, rcGnome, rcDwarf);

type
  TRaceArray<T> = array [TRaceEnum] of T;

  // TRaceEnumHelper = record helper for TRaceEnum
  // end;

type
  TRaceProp = record
    Description: string;
    Age: TMinMax;
    Height: TMinMax;
    Weight: TMinMax;
    Metabolism: TMinMax;
    Strength: TMinMax;
    Dexterity: TMinMax;
    Willpower: TMinMax;
    Perception: TMinMax;
    Life: TMinMax;
    Mana: TMinMax;
  end;

const
  dsHuman = 'Люди - самая молодая раса. Они легко ко всему приспосабливаются. ' +
    'Благодаря своим способностям, часто правят империями, которыми другим расам управлять сложно. ' +
    'Живут мало, но у них разнообразные религии, вкусы, мораль и привычки.';
  dsHalfling = 'Низкорослая раса восхитительных и знаменитых взломщиков и карманников. ' +
    'Хафлинги крепки и трудолюбивы, в основном тихие и мирные. В целом они предпочитают домашний уют опасностям приключений. ' +
    'Обитают во всех известных городах и селениях мира.';
  dsElf = 'Самая древняя и самая таинственная раса. Эльфы оставили неизгладимый след в истории мира, ' +
    'большинство героев разных эпох из их числа. Быстры, ловки, а об эльфийской магии, которой в совершенстве ' +
    'владеет каждый эльф, ходят легенды.';
  dsGnome = 'Раньше гномы жили дикими племенами высоко в горах, но спустя несколько веков научились ' +
    'самым простым ремеслам и торговле. В наше время они все так же ведут уединенный образ жизни вдали от ' +
    'цивиллизаций. Славятся большими магическими способностями.';
  dsDwarf = 'Дворфы селятся в горах, разрабатывают там месторождения полезных ископаемых и строят целые ' +
    'подземные города. Невысоки, приземисты, крепко сложены. Дворфы честны и прямолинейны, что часто приводит ' +
    'их к конфликтам с другими разумными расами.';

const
  RaceProp: TRaceArray<TRaceProp> = (
    // Human
    (Description: dsHuman; Age: (Min: 16; Max: 50;); Height: (Min: 160; Max: 180;); Weight: (Min: 70; Max: 110;); Metabolism: (Min: 80; Max: 85;);
    Strength: (Min: 1; Max: 2;); Dexterity: (Min: 1; Max: 2;); Willpower: (Min: 1; Max: 2;); Perception: (Min: 1; Max: 2;); Life: (Min: 5; Max: 8;);
    Mana: (Min: 5; Max: 8;);),
    // Halfling
    (Description: dsHalfling; Age: (Min: 18; Max: 120;); Height: (Min: 80; Max: 120;); Weight: (Min: 30; Max: 60;); Metabolism: (Min: 50; Max: 75;);
    Strength: (Min: 1; Max: 1;); Dexterity: (Min: 1; Max: 2;); Willpower: (Min: 1; Max: 2;); Perception: (Min: 2; Max: 4;); Life: (Min: 5; Max: 8;);
    Mana: (Min: 5; Max: 8;);),
    // Elf
    (Description: dsElf; Age: (Min: 75; Max: 800;); Height: (Min: 190; Max: 250;); Weight: (Min: 50; Max: 100;); Metabolism: (Min: 60; Max: 65;);
    Strength: (Min: 1; Max: 1;); Dexterity: (Min: 1; Max: 3;); Willpower: (Min: 1; Max: 3;); Perception: (Min: 1; Max: 2;); Life: (Min: 5; Max: 10;);
    Mana: (Min: 10; Max: 15;);),
    // Gnome
    (Description: dsGnome; Age: (Min: 20; Max: 160;); Height: (Min: 90; Max: 130;); Weight: (Min: 70; Max: 110;); Metabolism: (Min: 95; Max: 100;);
    Strength: (Min: 1; Max: 2;); Dexterity: (Min: 1; Max: 2;); Willpower: (Min: 1; Max: 3;); Perception: (Min: 1; Max: 3;); Life: (Min: 5; Max: 10;);
    Mana: (Min: 10; Max: 15;);),
    // Dwarf
    (Description: dsDwarf; Age: (Min: 20; Max: 250;); Height: (Min: 100; Max: 150;); Weight: (Min: 80; Max: 120;); Metabolism: (Min: 115; Max: 120;);
    Strength: (Min: 1; Max: 3;); Dexterity: (Min: 1; Max: 3;); Willpower: (Min: 1; Max: 1;); Perception: (Min: 1; Max: 2;); Life: (Min: 10; Max: 15;);
    Mana: (Min: 5; Max: 10;);)
    /// ///
    );

type
  TRace = record

  end;

type
  TRaces = class(TObject)
  private
    FRaceName: TRaceArray<string>;
  public
    Attrib: TBaseAttribArray<UInt>;
    constructor Create;
    destructor Destroy; override;
    function GetName(I: TRaceEnum): string;
    function GetDescription(I: TRaceEnum): string;
  end;

var
  Races: TRaces;

implementation

uses
  TypInfo,
  SysUtils,
  Trollhunter.Helpers,
  EnumHelper;

{ TRaces }

constructor TRaces.Create;
var
  I: TRaceEnum;
begin
  for I := Low(TRaceEnum) to High(TRaceEnum) do
    FRaceName[I] := Enum<TRaceEnum>.ValueName(I).GetName('rc');
end;

destructor TRaces.Destroy;
begin

  inherited;
end;

function TRaces.GetDescription(I: TRaceEnum): string;
begin
  Result := RaceProp[I].Description;
end;

function TRaces.GetName(I: TRaceEnum): string;
begin
  Result := FRaceName[I]
end;

initialization

Races := TRaces.Create;

finalization

FreeAndNil(Races);

end.
