unit uMob;

interface

uses uMap, uCommon;

type
  TMobBase = record
    Symbol: Char;
    Boss: Boolean;
    Deep: set of TDeepEnum;
    MaxLife: Word;
    Level: Byte;
    Armor: Byte;
    DV: Byte;
    Damage: TDamage;
    Color: Cardinal;
  end;

type
  TMobEnum = (
    // Dark Wood
    mBigRat, mSpinyFrog, mJackal, mBlackBear, mGrizzlyBear, mAnaconda, mWolf,
    // Gray Cave
    mKobold, mBigKobold, mRedKobold, mGnoll, mBasilisk, mWisp, mWorm,
    // Deep Cave
    mScorpion, mWasp, mAnt, mSoldierAnt, mScarab, mBigSpider, mFireCrab,
    // Blood Cave
    mGoblin, mDarkGoblin, mBlackGoblin, mGargoyle, mWarg, mDraconian, mOrc,
    // Dungeon of Doom
    mZombie, mOgre, mMummy, mGhoul, mVampire, mCyclops, mSkeleton,
    // Boss
    mTrollKing);

// {Black Bear (B)}, {Grizzly Bear (B)}, {Big Rat (R)}
// Black Viper (S), Ball Python (S), {Anaconda (S)},
// {Jackal (J)}, Hound (H), {Wolf (W)},
// {Spiny Frog (F)}, Giant Toad (F), Giant Newt (N), Iguana (I), Giant Gecko (G), Komodo Dragon (K)
// Pan (P), Satyr (S), Faun (F), Centaur (C),

// {Kobold (K)}, {Big Kobold (K)}, {Red Kobold} (K), {Gnoll (G)}, {Basilisk (B)}
// {Worm (W)}, Swamp Worm (W), {Wisp (W)}, Fire Vortex (V), Giant Slug (S)
// Naga (N), Greater Naga (N), Naga Warrior (N), Jelly (J), Acid Blob (B)

//
// {Scorpion (S)}, {Wasp (W)},
// {Ant (A)}, {Soldier Ant (A)}, {Scarab (S)}, {Fire Crab (C)}, {Big Spider (S)}

// {Goblin (G)}, {Dark Goblin (G)}, {Black Goblin (G)}, {Gargoyle (G)}, {Warg (W)}
// Air Elemental (E), Fire Elemental (E), Water Elemental (E), Earth Elemental (E),
// {Orc (O)}, Orc Warrior (O), Orc Warlord (O), {Draconian (D)},

// {Mummy (M)}, {Ghoul (G)}, {Vampire (V)}, {Zombie (Z)}, {Skeleton (S)}, Lich (L), Phantom (P)
// Stone Golem (G), Fire Golem (G), Frost Golem (G), Hill Giant (G), Stone Giant (G)
// Titan (T), {Ogre (O)}, Two-Headed Ogre (O), {Cyclops (C)}, {Troll King (T)}

const
  MobBase: array [TMobEnum] of TMobBase = (
    // == Dark Wood == //

    // Big Rat
    (Symbol: 'r'; Boss: False; Deep: [deDarkWood]; MaxLife: 5; Level: 1; Armor: 0;
    DV: 4; Damage: (Min: 2; Max: 3;); Color: $FF249988;),
    // Spiny Frog
    (Symbol: 'f'; Boss: False; Deep: [deDarkWood]; MaxLife: 7; Level: 1; Armor: 0;
    DV: 5; Damage: (Min: 4; Max: 5;); Color: $FF33FF66;),
    // Jackal
    (Symbol: 'j'; Boss: False; Deep: [deDarkWood]; MaxLife: 8; Level: 1; Armor: 0;
    DV: 7; Damage: (Min: 5; Max: 7;); Color: $FF9955FF;),
    // Black Bear
    (Symbol: 'b'; Boss: False; Deep: [deDarkWood]; MaxLife: 10; Level: 2; Armor: 1;
    DV: 8; Damage: (Min: 6; Max: 8;); Color: $FF331155;),
    // Grizzly Bear
    (Symbol: 'b'; Boss: False; Deep: [deDarkWood]; MaxLife: 14; Level: 2; Armor: 1;
    DV: 10; Damage: (Min: 6; Max: 9;); Color: $FF331155;),
    // Anaconda
    (Symbol: 's'; Boss: False; Deep: [deDarkWood]; MaxLife: 18; Level: 2; Armor: 1;
    DV: 10; Damage: (Min: 7; Max: 9;); Color: $FF331155;),
    // Wolf
    (Symbol: 'w'; Boss: False; Deep: [deDarkWood]; MaxLife: 22; Level: 3; Armor: 2;
    DV: 12; Damage: (Min: 8; Max: 9;); Color: $FF331155;),

    // == Gray Cave == //

    // Kobold
    (Symbol: 'k'; Boss: False; Deep: [deGrayCave]; MaxLife: 25; Level: 3; Armor: 2;
    DV: 16; Damage: (Min: 6; Max: 10;); Color: $FF777700;),
    // Big Kobold
    (Symbol: 'k'; Boss: False; Deep: [deGrayCave]; MaxLife: 25; Level: 3; Armor: 3;
    DV: 18; Damage: (Min: 7; Max: 10;); Color: $FF777700;),
    // Red Kobold
    (Symbol: 'k'; Boss: False; Deep: [deGrayCave]; MaxLife: 30; Level: 3; Armor: 4;
    DV: 20; Damage: (Min: 7; Max: 10;); Color: $FF777700;),
    // Gnoll
    (Symbol: 'g'; Boss: False; Deep: [deGrayCave]; MaxLife: 32; Level: 4; Armor: 4;
    DV: 22; Damage: (Min: 8; Max: 11;); Color: $FF777700;),
    // Basilisk
    (Symbol: 'b'; Boss: False; Deep: [deGrayCave]; MaxLife: 35; Level: 4; Armor: 5;
    DV: 25; Damage: (Min: 9; Max: 12;); Color: $FF777700;),
    // Wisp
    (Symbol: 'w'; Boss: False; Deep: [deGrayCave]; MaxLife: 38; Level: 4; Armor: 5;
    DV: 25; Damage: (Min: 10; Max: 13;); Color: $FF777700;),
    // Worm
    (Symbol: 'w'; Boss: False; Deep: [deGrayCave]; MaxLife: 40; Level: 5; Armor: 5;
    DV: 27; Damage: (Min: 10; Max: 14;); Color: $FF777700;),

    // == Deep Cave == //

    // Scorpion
    (Symbol: 's'; Boss: False; Deep: [deDeepCave]; MaxLife: 45; Level: 5; Armor: 5;
    DV: 30; Damage: (Min: 10; Max: 15;); Color: $FF992233;),
    // Wasp
    (Symbol: 'w'; Boss: False; Deep: [deDeepCave]; MaxLife: 48; Level: 5; Armor: 5;
    DV: 30; Damage: (Min: 10; Max: 15;); Color: $FF992233;),
    // Ant
    (Symbol: 'a'; Boss: False; Deep: [deDeepCave]; MaxLife: 50; Level: 5; Armor: 6;
    DV: 30; Damage: (Min: 11; Max: 16;); Color: $FF992233;),
    // Soldier Ant
    (Symbol: 'a'; Boss: False; Deep: [deDeepCave]; MaxLife: 55; Level: 6; Armor: 6;
    DV: 35; Damage: (Min: 12; Max: 17;); Color: $FF992233;),
    // Scarab
    (Symbol: 's'; Boss: False; Deep: [deDeepCave]; MaxLife: 60; Level: 6; Armor: 6;
    DV: 35; Damage: (Min: 12; Max: 18;); Color: $FF992233;),
    // Big Spider
    (Symbol: 's'; Boss: False; Deep: [deDeepCave]; MaxLife: 65; Level: 6; Armor: 7;
    DV: 40; Damage: (Min: 14; Max: 19;); Color: $FF992233;),
    // Fire Crab
    (Symbol: 's'; Boss: False; Deep: [deDeepCave]; MaxLife: 70; Level: 6; Armor: 7;
    DV: 40; Damage: (Min: 15; Max: 20;); Color: $FF992233;),

    // == Blood Cave == //

    // Goblin
    (Symbol: 'g'; Boss: False; Deep: [deBloodCave]; MaxLife: 72; Level: 7; Armor: 8;
    DV: 40; Damage: (Min: 15; Max: 20;); Color: $FF00AA00;),
    // Dark Goblin
    (Symbol: 'g'; Boss: False; Deep: [deBloodCave]; MaxLife: 75; Level: 7; Armor: 8;
    DV: 45; Damage: (Min: 15; Max: 21;); Color: $FF116610;),
    // Black Goblin
    (Symbol: 'g'; Boss: False; Deep: [deBloodCave]; MaxLife: 78; Level: 7; Armor: 8;
    DV: 45; Damage: (Min: 15; Max: 22;); Color: $FF445544;),
    // Gargoyle
    (Symbol: 'g'; Boss: False; Deep: [deBloodCave]; MaxLife: 80; Level: 7; Armor: 9;
    DV: 45; Damage: (Min: 15; Max: 23;); Color: $FF445544;),
    // Warg
    (Symbol: 'w'; Boss: False; Deep: [deBloodCave]; MaxLife: 82; Level: 8; Armor: 10;
    DV: 50; Damage: (Min: 16; Max: 24;); Color: $FF445544;),
    // Draconian
    (Symbol: 'd'; Boss: False; Deep: [deBloodCave]; MaxLife: 85; Level: 8; Armor: 10;
    DV: 50; Damage: (Min: 17; Max: 24;); Color: $FF445544;),
    // Orc
    (Symbol: 'd'; Boss: False; Deep: [deBloodCave]; MaxLife: 88; Level: 8; Armor: 10;
    DV: 50; Damage: (Min: 18; Max: 25;); Color: $FF445544;),

    // == Dungeon of Doom == //

    // Zombie
    (Symbol: 'z'; Boss: False; Deep: [deDungeonOfDoom]; MaxLife: 90; Level: 9; Armor: 12;
    DV: 55; Damage: (Min: 19; Max: 25;); Color: $FF00BB00;),
    // Ogre
    (Symbol: 'o'; Boss: False; Deep: [deDungeonOfDoom]; MaxLife: 92; Level: 9; Armor: 12;
    DV: 55; Damage: (Min: 19; Max: 26;); Color: $FF559977;),
    // Mummy
    (Symbol: 'm'; Boss: False; Deep: [deDungeonOfDoom]; MaxLife: 95; Level: 9; Armor: 12;
    DV: 55; Damage: (Min: 20; Max: 27;); Color: $FF223333;),
    // Ghoul
    (Symbol: 'g'; Boss: False; Deep: [deDungeonOfDoom]; MaxLife: 97; Level: 10; Armor: 15;
    DV: 60; Damage: (Min: 20; Max: 27;); Color: $FF223333;),
    // Vampire
    (Symbol: 'v'; Boss: False; Deep: [deDungeonOfDoom]; MaxLife: 98; Level: 10; Armor: 15;
    DV: 60; Damage: (Min: 20; Max: 28;); Color: $FF223333;),
    // Cyclops
    (Symbol: 'c'; Boss: False; Deep: [deDungeonOfDoom]; MaxLife: 100; Level: 10; Armor: 18;
    DV: 60; Damage: (Min: 21; Max: 29;); Color: $FF223333;),
    // Skeleton
    (Symbol: 'c'; Boss: False; Deep: [deDungeonOfDoom]; MaxLife: 100; Level: 10; Armor: 18;
    DV: 60; Damage: (Min: 22; Max: 30;); Color: $FF223333;),

    // == Bosses == //

    // Troll King
    (Symbol: 'T'; Boss: True; Deep: [deDungeonOfDoom]; MaxLife: 100; Level: 10; Armor: 20;
    DV: 60; Damage: (Min: 25; Max: 35;); Color: $FFFF4400;));

type
  TMob = class(TObject)
    ID: Byte;
    Life: Word;
    X, Y: Integer;
    Deep: TDeepEnum;
    Alive: Boolean;
    Sleep: Boolean;
    procedure AddRandom(ADeep: TDeepEnum);
    procedure Process;
    procedure Render(AX, AY: Byte);
    procedure Walk(AX, AY: Byte; PX: Byte = 0; PY: Byte = 0);
    procedure Attack;
    procedure Defeat;
    function GetRadius: Byte;
    procedure DropItems;
  end;

type
  TMobs = class(TObject)
    FMob: array of TMob;
    constructor Create();
    destructor Destroy; override;
    procedure Add(ADeep: TDeepEnum);
    function Count: Integer;
    procedure Process;
    procedure Render(AX, AY: Byte);
    function GetFreeTile(AX, AY: Byte): Boolean;
    function GetIndex(AX, AY: Byte): Integer;
    function GetName(AMob: TMobEnum): string;
  end;

type
  TGetXYVal = function(X, Y: Integer): Boolean; stdcall;

var
  Mobs: TMobs = nil;

implementation

uses Math, SysUtils, Dialogs, uTerminal, uPlayer, uMsgLog, gnugettext, uGame,
  uItem;

function DoAStar(MapX, MapY, FromX, FromY, ToX, ToY: Integer;
  Callback: TGetXYVal; var TargetX, TargetY: Integer): Boolean;
  external 'BeaRLibPF.dll';

function MyCallback(X, Y: Integer): Boolean; stdcall;
begin
  Result := (Map.GetTileEnum(X, Y, Map.Deep) in FreeTiles)
end;

{ TMob }

procedure TMob.AddRandom(ADeep: TDeepEnum);
var
  FX, FY: Byte;
begin
  repeat
    ID := Math.RandomRange(0, Ord(High(TMobEnum)) + 1);
    FX := Math.RandomRange(0, High(Byte));
    FY := Math.RandomRange(0, High(Byte));
  until (Map.GetTileEnum(FX, FY, ADeep) in SpawnTiles) and (Player.X <> FX) and
    (Player.Y <> FY) and Mobs.GetFreeTile(FX, FY) and
    (ADeep in MobBase[TMobEnum(ID)].Deep);
  if (MobBase[TMobEnum(ID)].Boss and IsBoss) then
    AddRandom(ADeep);
  X := FX;
  Y := FY;
  Deep := ADeep;
  Alive := True;
  Sleep := True;
  Life := MobBase[TMobEnum(ID)].MaxLife;
  // Boss
  if MobBase[TMobEnum(ID)].Boss then
  begin
    IsBoss := True;
    if Game.Wizard then
    begin
      Player.X := Clamp(X - 1, 0, High(Byte));
      Player.Y := Clamp(Y - 1, 0, High(Byte));
    end;
  end;
end;

procedure TMob.Attack;
var
  The: string;
  Dam: Word;
begin
  if (Self.Life = 0) or (Player.Life = 0) then Exit;
  The := GetCapit(GetDescThe(Mobs.GetName(TMobEnum(ID))));
  if (Player.DV < Math.RandomRange(0, 100)) then
  begin
    // Attack
    Dam := Clamp(RandomRange(MobBase[TMobEnum(ID)].Damage.Min, MobBase[TMobEnum(ID)].Damage.Max + 1), 0, High(Word));
    Player.Life := Clamp(Player.Life - Dam, 0, High(Word));
    MsgLog.Add(Format(_('%s hits you (%d).'), [The, Dam]));
    if Player.Life = 0 then
      Player.Defeat(Mobs.GetName(TMobEnum(ID)));
  end
  else
  begin
    // Miss
    MsgLog.Add(Format(_('%s hits you, but your armor protects you.'), [The]));
  end;
end;

procedure TMob.Defeat;
begin
  Self.Alive := False;
  MsgLog.Add(Format(_('You kill %s.'),
    [GetDescThe(Mobs.GetName(TMobEnum(ID)))]));
  Player.Kills := Player.Kills + 1;
  Player.Score := Player.Score + MobBase[TMobEnum(ID)].Level;
  Self.DropItems;
  // Boss
  if (MobBase[TMobEnum(ID)].Boss and (Map.Deep = FinalDungeon)) then
  begin
    if not Game.Wizard then Game.Won := True;
    MsgLog.Add(_('You have won.'));
    Player.Score := Player.Score + 1000;
    Game.Screenshot := GetTextScreenshot();
  end;
end;

procedure TMob.DropItems;
begin
  Items.Drop(Self.X, Self.Y, MobBase[TMobEnum(Self.ID)].Boss);
  Items.Drop(Self.X, Self.Y, iCorpse);
end;

function TMob.GetRadius: Byte;
begin
  Result := Clamp(30 - (Player.GetSkillValue(skStealth) div 3), 5, 30);
end;

procedure TMob.Process;
var
  NX, NY, Dist: Integer;
begin
  Dist := GetDist(X, Y, Player.X, Player.Y);
  if (Dist > GetRadius) then Exit;
  if Sleep then
  begin
    if (Math.RandomRange(0, 99) <= 20) then
    begin
      Sleep := False;
      Exit;
    end;
    Player.Skill(skStealth);
    Exit;
  end;
  if (Dist <= 2) and Player.IsRest then Player.IsRest := False;
  // A*
  if not DoAStar(High(Byte), High(Byte), X, Y, Player.X, Player.Y, @MyCallback,
    NX, NY) then Exit;
  if (NX = Player.X) and (NY = Player.Y) then
  begin
    Self.Attack();  
  end
  else if (Mobs.GetFreeTile(NX, NY)) then
  begin
    X := NX;
    Y := NY;
  end
  else
    Self.Walk(X, Y, Player.X, Player.Y);
end;

procedure TMob.Render(AX, AY: Byte);
begin
  if not Map.InView(X, Y) or (not Game.Wizard and not Map.GetFOV(X, Y)) then
    Exit;
  if not Game.Wizard and (GetDist(Player.X, Player.Y, X, Y) > Player.GetRadius)
  then
    Exit;
  Terminal.Print(X - Player.X + AX + View.Left, Y - Player.Y + AY + View.Top,
    MobBase[TMobEnum(ID)].Symbol, MobBase[TMobEnum(ID)].Color);
end;

procedure TMob.Walk(AX, AY: Byte; PX: Byte = 0; PY: Byte = 0);
var
  NX, NY: ShortInt;
begin
  NX := 0;
  NY := 0;
  case Math.RandomRange(0, 8) + 1 of
    // North
    0:
      begin
        NY := -1;
      end;
    1:
      begin
        NX := +1;
        NY := -1;
      end;
    2:
      begin
        NX := +1;
      end;
    3:
      begin
        NX := +1;
        NY := +1;
      end;
    4:
      begin
        NY := +1;
      end;
    5:
      begin
        NX := -1;
        NY := +1;
      end;
    6:
      begin
        NX := -1;
      end;
    7:
      begin
        NX := -1;
        NY := -1;
      end;
  end;
  if Mobs.GetFreeTile(X + NX, Y + NY) and
    (Map.GetTileEnum(X + NX, Y + NY, Map.Deep) in FreeTiles) then
  begin
    X := X + NX;
    Y := Y + NY;
  end;
end;

{ TMobs }

procedure TMobs.Add(ADeep: TDeepEnum);
var
  I: Integer;
begin
  for I := 0 to Self.Count - 1 do
    if not FMob[I].Alive then
    begin
      FMob[I].AddRandom(ADeep);
      Exit;
    end;
  SetLength(FMob, Length(FMob) + 1);
  I := Length(FMob) - 1;
  FMob[I] := TMob.Create;
  FMob[I].AddRandom(ADeep);
end;

function TMobs.Count: Integer;
begin
  Result := Length(FMob);
end;

constructor TMobs.Create;
begin
  SetLength(FMob, 0);
end;

destructor TMobs.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    FMob[I].Free;
    FMob[I] := nil;
  end;
  inherited;
end;

function TMobs.GetFreeTile(AX, AY: Byte): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Count - 1 do
    with FMob[I] do
      if Alive and (Deep = Map.Deep) and (AX = X) and (AY = Y) then
      begin
        Result := False;
        Exit;
      end;
end;

function TMobs.GetIndex(AX, AY: Byte): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    with FMob[I] do
      if Alive and (Deep = Map.Deep) and (AX = X) and (AY = Y) then
      begin
        Result := I;
        Exit;
      end;
end;

procedure TMobs.Process;
var
  I: Integer;
begin
  if (Count > 0) then
    for I := 0 to Count - 1 do
      if FMob[I].Alive and (FMob[I].Deep = Map.Deep) then
        FMob[I].Process;
end;

procedure TMobs.Render(AX, AY: Byte);
var
  I: Integer;
begin
  if (Count > 0) then
    for I := 0 to Count - 1 do
      if FMob[I].Alive and (FMob[I].Deep = Map.Deep) then
        FMob[I].Render(AX, AY);
end;

function TMobs.GetName(AMob: TMobEnum): string;
begin
  case AMob of
    // == Dark Wood == //

    mBigRat:
      Result := _('Big Rat');
    mSpinyFrog:
      Result := _('Spiny Frog');
    mJackal:
      Result := _('Jackal');
    mBlackBear:
      Result := _('Black Bear');
    mGrizzlyBear:
      Result := _('Grizzly Bear');
    mAnaconda:
      Result := _('Anaconda');
    mWolf:
      Result := _('Wolf');

    // == Gray Cave == //

    mKobold:
      Result := _('Kobold');
    mBigKobold:
      Result := _('Big Kobold');
    mRedKobold:
      Result := _('Red Kobold');
    mGnoll:
      Result := _('Gnoll');
    mBasilisk:
      Result := _('Basilisk');
    mWisp:
      Result := _('Wisp');
    mWorm:
      Result := _('Worm');

    // == Deep Cave == //

    mScorpion:  
      Result := _('Scorpion');
    mWasp:
      Result := _('Wasp');
    mAnt:
      Result := _('Ant');
    mSoldierAnt:
      Result := _('Soldier Ant');
    mScarab:
      Result := _('Scarab');
    mBigSpider:
      Result := _('Big Spider');
    mFireCrab:
      Result := _('Fire Crab');

    // == Blood Cave == //
             
    mGoblin:
      Result := _('Goblin');
    mDarkGoblin:
      Result := _('Dark Goblin');
    mBlackGoblin:
      Result := _('Black Goblin');
    mGargoyle:
      Result := _('Gargoyle');
    mWarg:
      Result := _('Warg');
    mDraconian:
      Result := _('Draconian');
    mOrc:
      Result := _('Orc');

    // == Dungeon of Doom == //

    mZombie:
      Result := _('Zombie');
    mOgre:
      Result := _('Ogre');
    mMummy:
      Result := _('Mummy');
    mGhoul:
      Result := _('Ghoul');
    mVampire:
      Result := _('Vampire');
    mCyclops:
      Result := _('Cyclops');
    mSkeleton:
      Result := _('Skeleton');

    // == Bosses == //

    mTrollKing:
      Result := _('Troll King');
  end;
end;

initialization

Mobs := TMobs.Create;

finalization

FreeAndNil(Mobs);

end.
