unit uMob;

interface

uses uMap, uEntity;

type
  TMobBase = record
    Symbol: Char;
    Boss: Boolean;
    Maps: set of TMapEnum;
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
    mBigRat, mSpinyFrog, mJackal, mBlackBear, mGrizzlyBear, mAnaconda,
    mWolf, mHound,
    // Gray Cave
    mKobold, mBigKobold, mRedKobold, mGnoll, mBasilisk, mWisp, mWorm, mNaga,
    // Deep Cave
    mScorpion, mWasp, mAnt, mSoldierAnt, mScarab, mBigSpider, mFireCrab, mPan,
    // Blood Cave
    mGoblin, mDarkGoblin, mBlackGoblin, mGargoyle, mWarg, mDraconian, mOrc,
    mOrcWarrior,
    // Dungeon of Doom
    mZombie, mOgre, mMummy, mGhoul, mVampire, mCyclops, mSkeleton, mLich,
    // Bosses
    mBlackHound, mGiantNewt, mIguana, mKoboldKing, mSwampWorm, mGiantSlug,
    mCentaur, mSatyr, mTitan, mHillGiant, mStoneGiant, mTwoHeadedOgre,
    mTrollKing);

  // {Black Bear (B)}, {Grizzly Bear (B)}, {Big Rat (R)}
  // Black Viper (S), Ball Python (S), {Anaconda (S)},
  // {Jackal (J)}, {Hound (H)}, {Wolf (W)},
  // {Spiny Frog (F)}, Giant Toad (F), {Giant Newt (N)}, {Iguana (I)}, Giant Gecko (G),

  // {Kobold (K)}, {Big Kobold (K)}, {Red Kobold} (K), {Gnoll (G)}, {Basilisk (B)}
  // {Worm (W)}, {Swamp Worm (W)}, {Wisp (W)}, Fire Vortex (V), {Giant Slug (S)}
  // {Naga (N)}, Greater Naga (N), Naga Warrior (N), Jelly (J), Acid Blob (B)

  // {Scorpion (S)}, {Wasp (W)}, {Pan (P)}, {Satyr (S)}, Faun (F), {Centaur (C)},
  // {Ant (A)}, {Soldier Ant (A)}, {Scarab (S)}, {Fire Crab (C)}, {Big Spider (S)}

  // {Goblin (G)}, {Dark Goblin (G)}, {Black Goblin (G)}, {Gargoyle (G)}, {Warg (W)}
  // Air Elemental (E), Fire Elemental (E), Water Elemental (E), Earth Elemental (E),
  // {Orc (O)}, {Orc Warrior (O)}, Orc Warlord (O), {Draconian (D)}, {Titan (T)}
  // {Hill Giant (G)}, {Stone Giant (G)}

  // {Mummy (M)}, {Ghoul (G)}, {Vampire (V)}, {Zombie (Z)}, {Skeleton (S)},
  // {Lich (L)}, Phantom (P),
  // Stone Golem (G), Fire Golem (G), Frost Golem (G),
  // {Ogre (O)}, {Two-Headed Ogre (O)}, {Cyclops (C)}, {Troll King (T)}

const
  MobBase: array [TMobEnum] of TMobBase = (
    // == Dark Wood == //

    // Big Rat
    (Symbol: 'r'; Boss: False; Maps: [deDarkWood]; MaxLife: 5; Level: 1;
    Armor: 0; DV: 2; Damage: (Min: 1; Max: 2;); Color: $FF249988;),
    // Spiny Frog
    (Symbol: 'f'; Boss: False; Maps: [deDarkWood]; MaxLife: 7; Level: 1;
    Armor: 0; DV: 3; Damage: (Min: 1; Max: 3;); Color: $FF33FF66;),
    // Jackal
    (Symbol: 'j'; Boss: False; Maps: [deDarkWood]; MaxLife: 8; Level: 1;
    Armor: 0; DV: 4; Damage: (Min: 2; Max: 3;); Color: $FF9955FF;),
    // Black Bear
    (Symbol: 'b'; Boss: False; Maps: [deDarkWood]; MaxLife: 10; Level: 2;
    Armor: 1; DV: 5; Damage: (Min: 3; Max: 4;); Color: $FF331155;),
    // Grizzly Bear
    (Symbol: 'b'; Boss: False; Maps: [deDarkWood]; MaxLife: 14; Level: 2;
    Armor: 1; DV: 6; Damage: (Min: 4; Max: 5;); Color: $FF331155;),
    // Anaconda
    (Symbol: 's'; Boss: False; Maps: [deDarkWood]; MaxLife: 18; Level: 2;
    Armor: 1; DV: 7; Damage: (Min: 4; Max: 6;); Color: $FF331155;),
    // Wolf
    (Symbol: 'w'; Boss: False; Maps: [deDarkWood]; MaxLife: 22; Level: 3;
    Armor: 2; DV: 8; Damage: (Min: 5; Max: 6;); Color: $FF331155;),
    // Hound
    (Symbol: 'h'; Boss: False; Maps: [deDarkWood]; MaxLife: 23; Level: 3;
    Armor: 2; DV: 10; Damage: (Min: 5; Max: 7;); Color: $FF249988;),

    // == Gray Cave == //

    // Kobold
    (Symbol: 'k'; Boss: False; Maps: [deGrayCave]; MaxLife: 25; Level: 3;
    Armor: 2; DV: 12; Damage: (Min: 6; Max: 10;); Color: $FF777700;),
    // Big Kobold
    (Symbol: 'k'; Boss: False; Maps: [deGrayCave]; MaxLife: 25; Level: 3;
    Armor: 3; DV: 14; Damage: (Min: 7; Max: 10;); Color: $FF777700;),
    // Red Kobold
    (Symbol: 'k'; Boss: False; Maps: [deGrayCave]; MaxLife: 30; Level: 3;
    Armor: 4; DV: 16; Damage: (Min: 7; Max: 10;); Color: $FF777700;),
    // Gnoll
    (Symbol: 'g'; Boss: False; Maps: [deGrayCave]; MaxLife: 32; Level: 4;
    Armor: 4; DV: 18; Damage: (Min: 8; Max: 11;); Color: $FF777700;),
    // Basilisk
    (Symbol: 'b'; Boss: False; Maps: [deGrayCave]; MaxLife: 35; Level: 4;
    Armor: 5; DV: 20; Damage: (Min: 9; Max: 12;); Color: $FF777700;),
    // Wisp
    (Symbol: 'w'; Boss: False; Maps: [deGrayCave]; MaxLife: 38; Level: 4;
    Armor: 5; DV: 23; Damage: (Min: 10; Max: 13;); Color: $FF777700;),
    // Worm
    (Symbol: 'w'; Boss: False; Maps: [deGrayCave]; MaxLife: 40; Level: 5;
    Armor: 5; DV: 24; Damage: (Min: 10; Max: 14;); Color: $FF777700;),
    // Naga
    (Symbol: 'n'; Boss: False; Maps: [deGrayCave]; MaxLife: 42; Level: 5;
    Armor: 5; DV: 25; Damage: (Min: 10; Max: 14;); Color: $FF7777CC;),

    // == Deep Cave == //

    // Scorpion
    (Symbol: 's'; Boss: False; Maps: [deDeepCave]; MaxLife: 45; Level: 5;
    Armor: 5; DV: 26; Damage: (Min: 10; Max: 15;); Color: $FF992233;),
    // Wasp
    (Symbol: 'w'; Boss: False; Maps: [deDeepCave]; MaxLife: 48; Level: 5;
    Armor: 5; DV: 28; Damage: (Min: 10; Max: 15;); Color: $FF992233;),
    // Ant
    (Symbol: 'a'; Boss: False; Maps: [deDeepCave]; MaxLife: 50; Level: 5;
    Armor: 6; DV: 30; Damage: (Min: 11; Max: 16;); Color: $FF992233;),
    // Soldier Ant
    (Symbol: 'a'; Boss: False; Maps: [deDeepCave]; MaxLife: 55; Level: 6;
    Armor: 6; DV: 35; Damage: (Min: 12; Max: 17;); Color: $FF992233;),
    // Scarab
    (Symbol: 's'; Boss: False; Maps: [deDeepCave]; MaxLife: 60; Level: 6;
    Armor: 6; DV: 35; Damage: (Min: 12; Max: 18;); Color: $FF992233;),
    // Big Spider
    (Symbol: 's'; Boss: False; Maps: [deDeepCave]; MaxLife: 65; Level: 6;
    Armor: 7; DV: 40; Damage: (Min: 14; Max: 19;); Color: $FF992233;),
    // Fire Crab
    (Symbol: 's'; Boss: False; Maps: [deDeepCave]; MaxLife: 70; Level: 7;
    Armor: 7; DV: 40; Damage: (Min: 15; Max: 20;); Color: $FF992233;),
    // Pan
    (Symbol: 'p'; Boss: False; Maps: [deDeepCave]; MaxLife: 70; Level: 7;
    Armor: 7; DV: 42; Damage: (Min: 15; Max: 20;); Color: $FF992233;),

    // == Blood Cave == //

    // Goblin
    (Symbol: 'g'; Boss: False; Maps: [deBloodCave]; MaxLife: 72; Level: 7;
    Armor: 8; DV: 42; Damage: (Min: 15; Max: 20;); Color: $FF00AA00;),
    // Dark Goblin
    (Symbol: 'g'; Boss: False; Maps: [deBloodCave]; MaxLife: 75; Level: 7;
    Armor: 8; DV: 45; Damage: (Min: 15; Max: 21;); Color: $FF116610;),
    // Black Goblin
    (Symbol: 'g'; Boss: False; Maps: [deBloodCave]; MaxLife: 78; Level: 7;
    Armor: 8; DV: 45; Damage: (Min: 15; Max: 22;); Color: $FF445544;),
    // Gargoyle
    (Symbol: 'g'; Boss: False; Maps: [deBloodCave]; MaxLife: 80; Level: 7;
    Armor: 9; DV: 45; Damage: (Min: 15; Max: 23;); Color: $FF445544;),
    // Warg
    (Symbol: 'w'; Boss: False; Maps: [deBloodCave]; MaxLife: 82; Level: 8;
    Armor: 10; DV: 50; Damage: (Min: 16; Max: 24;); Color: $FF445544;),
    // Draconian
    (Symbol: 'd'; Boss: False; Maps: [deBloodCave]; MaxLife: 85; Level: 8;
    Armor: 10; DV: 50; Damage: (Min: 17; Max: 24;); Color: $FF445544;),
    // Orc
    (Symbol: 'o'; Boss: False; Maps: [deBloodCave]; MaxLife: 88; Level: 8;
    Armor: 10; DV: 50; Damage: (Min: 17; Max: 25;); Color: $FF445544;),
    // Orc Warrior
    (Symbol: 'o'; Boss: False; Maps: [deBloodCave]; MaxLife: 90; Level: 9;
    Armor: 11; DV: 53; Damage: (Min: 18; Max: 25;); Color: $FF445544;),

    // == Dungeon of Doom == //

    // Zombie
    (Symbol: 'z'; Boss: False; Maps: [deDungeonOfDoom]; MaxLife: 90; Level: 9;
    Armor: 12; DV: 55; Damage: (Min: 19; Max: 25;); Color: $FF00BB00;),
    // Ogre
    (Symbol: 'o'; Boss: False; Maps: [deDungeonOfDoom]; MaxLife: 92; Level: 9;
    Armor: 12; DV: 55; Damage: (Min: 19; Max: 26;); Color: $FF559977;),
    // Mummy
    (Symbol: 'm'; Boss: False; Maps: [deDungeonOfDoom]; MaxLife: 95; Level: 9;
    Armor: 12; DV: 55; Damage: (Min: 20; Max: 27;); Color: $FF223333;),
    // Ghoul
    (Symbol: 'g'; Boss: False; Maps: [deDungeonOfDoom]; MaxLife: 97; Level: 10;
    Armor: 15; DV: 60; Damage: (Min: 20; Max: 27;); Color: $FF223333;),
    // Vampire
    (Symbol: 'v'; Boss: False; Maps: [deDungeonOfDoom]; MaxLife: 98; Level: 10;
    Armor: 15; DV: 60; Damage: (Min: 20; Max: 28;); Color: $FF223333;),
    // Cyclops
    (Symbol: 'c'; Boss: False; Maps: [deDungeonOfDoom]; MaxLife: 100; Level: 10;
    Armor: 18; DV: 60; Damage: (Min: 21; Max: 29;); Color: $FF223333;),
    // Skeleton
    (Symbol: 'c'; Boss: False; Maps: [deDungeonOfDoom]; MaxLife: 100; Level: 10;
    Armor: 18; DV: 60; Damage: (Min: 22; Max: 30;); Color: $FF223333;),
    // Lich
    (Symbol: 'l'; Boss: False; Maps: [deDungeonOfDoom]; MaxLife: 100; Level: 10;
    Armor: 19; DV: 60; Damage: (Min: 23; Max: 30;); Color: $FF223333;),

    // == Bosses == //

    // Black Hound
    (Symbol: 'h'; Boss: True; Maps: [deDarkWood]; MaxLife: 45; Level: 3;
    Armor: 8; DV: 25; Damage: (Min: 8; Max: 10;); Color: $FFCC8899;),
    // Giant Newt
    (Symbol: 'n'; Boss: True; Maps: [deDarkWood]; MaxLife: 50; Level: 3;
    Armor: 9; DV: 27; Damage: (Min: 9; Max: 11;); Color: $FF66DD99;),
    // Iguana
    (Symbol: 'i'; Boss: True; Maps: [deDarkWood]; MaxLife: 55; Level: 3;
    Armor: 10; DV: 30; Damage: (Min: 10; Max: 12;); Color: $FF44FF77;),
    // Kobold King
    (Symbol: 'k'; Boss: True; Maps: [deGrayCave]; MaxLife: 60; Level: 5;
    Armor: 12; DV: 32; Damage: (Min: 10; Max: 15;); Color: $FFAA77CC;),
    // Swamp Worm
    (Symbol: 'k'; Boss: True; Maps: [deGrayCave]; MaxLife: 63; Level: 5;
    Armor: 14; DV: 35; Damage: (Min: 12; Max: 18;); Color: $FF6699BB;),
    // Giant Slug
    (Symbol: 'k'; Boss: True; Maps: [deGrayCave]; MaxLife: 67; Level: 5;
    Armor: 16; DV: 38; Damage: (Min: 14; Max: 20;); Color: $FFCCAADD;),
    // Centaur
    (Symbol: 'c'; Boss: True; Maps: [deDeepCave]; MaxLife: 70; Level: 7;
    Armor: 25; DV: 40; Damage: (Min: 18; Max: 23;); Color: $FF77CCAA;),
    // Satyr
    (Symbol: 's'; Boss: True; Maps: [deDeepCave]; MaxLife: 75; Level: 7;
    Armor: 27; DV: 45; Damage: (Min: 20; Max: 25;); Color: $FF3388AA;),
    // Titan
    (Symbol: 't'; Boss: True; Maps: [deDeepCave]; MaxLife: 95; Level: 8;
    Armor: 30; DV: 55; Damage: (Min: 22; Max: 25;); Color: $FFAABB77;),
    // Hill Giant
    (Symbol: 'g'; Boss: True; Maps: [deBloodCave]; MaxLife: 96; Level: 9;
    Armor: 18; DV: 60; Damage: (Min: 23; Max: 25;); Color: $FF2233FF;),
    // Stone Giant
    (Symbol: 'g'; Boss: True; Maps: [deBloodCave]; MaxLife: 99; Level: 9;
    Armor: 19; DV: 60; Damage: (Min: 24; Max: 25;); Color: $FF22FF33;),
    // Two-Headed Ogre
    (Symbol: 'o'; Boss: True; Maps: [deBloodCave]; MaxLife: 100; Level: 10;
    Armor: 20; DV: 60; Damage: (Min: 25; Max: 30;); Color: $FF223333;),
    // Troll King
    (Symbol: 't'; Boss: True; Maps: [deDungeonOfDoom]; MaxLife: 20; Level: 15;
    Armor: 40; DV: 60; Damage: (Min: 50; Max: 75;); Color: $FFDD7711;));

type
  TMob = class(TEntity)
  private
    FID: Byte;
    Maps: TMapEnum;
    Sleep: Boolean;
    Boss: Boolean;
  public
    procedure AddRandom(AZ: TMapEnum);
    procedure Process;
    procedure Render(AX, AY: Byte);
    procedure Walk(AX, AY: Byte; PX: Byte = 0; PY: Byte = 0);
    procedure Attack;
    procedure Defeat;
    function GetRadius: Byte;
    procedure DropItems;
    property ID: Byte read FID write FID;
  end;

type
  TMobs = class(TObject)
  private
    FMob: array of TMob;
    function GetMob(I: Integer): TMob;
    procedure SetMob(I: Integer; const Value: TMob);
  public
    constructor Create();
    destructor Destroy; override;
    procedure Add(AZ: TMapEnum);
    function Count: Integer;
    procedure Process;
    procedure Render(AX, AY: Byte);
    function GetFreeTile(AX, AY: Byte): Boolean;
    function GetIndex(AX, AY: Byte): Integer;
    function GetName(AMobEnum: TMobEnum): string;
    property Mob[I: Integer]: TMob read GetMob write SetMob;
  end;

type
  TGetXYVal = function(X, Y: Integer): Boolean; stdcall;

var
  Mobs: TMobs = nil;

implementation

uses Math, SysUtils, Dialogs, uTerminal, uPlayer, uMsgLog, gnugettext, uGame,
  uItem, BearLibTerminal;

function DoAStar(MapX, MapY, FromX, FromY, ToX, ToY: Integer;
  Callback: TGetXYVal; var TargetX, TargetY: Integer): Boolean;
  external 'BeaRLibPF.dll';

function MyCallback(X, Y: Integer): Boolean; stdcall;
begin
  Result := (Map.GetTileEnum(X, Y, Map.Current) in FreeTiles)
end;

{ TMob }

procedure TMob.AddRandom(AZ: TMapEnum);
var
  FX, FY: Byte;
begin
  repeat
    ID := Math.RandomRange(0, Ord(High(TMobEnum)) + 1);
    FX := Math.RandomRange(0, High(Byte));
    FY := Math.RandomRange(0, High(Byte));
  until (Map.GetTileEnum(FX, FY, AZ) in SpawnTiles) and (Player.X <> FX) and
    (Player.Y <> FY) and Mobs.GetFreeTile(FX, FY) and
    (AZ in MobBase[TMobEnum(ID)].Maps);
  if (MobBase[TMobEnum(ID)].Boss and IsBoss) then
    AddRandom(AZ);
  X := FX;
  Y := FY;
  Self.Boss := False;
  Maps := AZ;
  Alive := True;
  Sleep := True;
  MaxLife := MobBase[TMobEnum(ID)].MaxLife;
  Life := MaxLife;
  // Boss
  if MobBase[TMobEnum(ID)].Boss then
  begin
    Game.Log(Format('%s [%d:%d:%d]', [Mobs.GetName(TMobEnum(ID)), X, Y,
      Ord(AZ)]));
    Self.Boss := True;
    IsBoss := True;
    { if Game.Wizard then
      begin
      Player.X := EnsureRange(X - 1, 0, High(Byte));
      Player.Y := EnsureRange(Y - 1, 0, High(Byte));
      end; }
  end;
end;

procedure TMob.Attack;
var
  The: string;
  Dam: Word;
begin
  if (Self.Life = 0) or (Player.Life = 0) then
    Exit;
  The := GetCapit(GetDescThe(Mobs.GetName(TMobEnum(ID))));
  if (Player.DV < Math.RandomRange(0, 100)) then
  begin
    // Attack
    Dam := EnsureRange(RandomRange(MobBase[TMobEnum(ID)].Damage.Min,
      MobBase[TMobEnum(ID)].Damage.Max + 1), 0, High(Word));
    Player.Life := EnsureRange(Player.Life - Dam, 0, High(Word));
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
var
  S: string;
  V: Byte;
begin
  Self.Alive := False;
  S := Format(_('You kill %s.'), [GetDescThe(Mobs.GetName(TMobEnum(ID)))]);
  if Boss then
    S := Format(FC, [clAlarm, S]);
  MsgLog.Add(S);
  Player.Kills := Player.Kills + 1;
  if Boss then
    V := 25
  else
    V := 1;
  Player.Score := Player.Score + (MobBase[TMobEnum(ID)].Level * V);
  Self.DropItems;
  // Boss
  if (Boss and (Map.Current = FinalDungeon) and (TMobEnum(ID) = mTrollKing))
  then
  begin
    if not Game.Wizard then
      Game.Won := True;
    MsgLog.Add(Format(FC, [clAlarm, _('You have won!!!')]));
    Player.Score := Player.Score + 1000;
    Game.Screenshot := Terminal.GetTextScreenshot();
  end;
end;

procedure TMob.DropItems;
begin
  Items.Drop(Self.X, Self.Y, Boss);
  Items.Drop(Self.X, Self.Y, iCorpse);
end;

function TMob.GetRadius: Byte;
begin
  Result := EnsureRange(30 - (Player.GetSkillValue(skStealth) div 3), 5, 30);
end;

procedure TMob.Process;
var
  NX, NY, Dist: Integer;
begin
  Dist := GetDist(Player.X, Player.Y);
  if (Dist > GetRadius) then
    Exit;
  if Sleep then
  begin
    if (Math.RandomRange(0, 99) <= 15) then
    begin
      Sleep := False;
      Exit;
    end;
    Player.Skill(skStealth);
    Exit;
  end;
  if (Dist <= 2) and Player.IsRest then
    Player.IsRest := False;
  // A*
  if not DoAStar(High(Byte), High(Byte), X, Y, Player.X, Player.Y, @MyCallback,
    NX, NY) then
    Exit;
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
var
  C: Char;
begin
  if not Map.InView(X, Y) or (not Game.Wizard and not Map.GetFOV(X, Y)) then
    Exit;
  if not Game.Wizard and (Player.GetDist(X, Y) > Player.GetRadius) then
    Exit;
  C := MobBase[TMobEnum(ID)].Symbol;
  if (Self.Boss) then
    C := Chr(Ord(C) - 32);
  Terminal.Print(X - Player.X + AX + View.Left, Y - Player.Y + AY + View.Top, C,
    MobBase[TMobEnum(ID)].Color, clBkMob);
end;

procedure TMob.Walk(AX, AY: Byte; PX: Byte = 0; PY: Byte = 0);
var
  NX, NY: ShortInt;
  FX, FY: Byte;
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
  FX := EnsureRange(X + NX, 0, High(Byte));
  FY := EnsureRange(Y + NY, 0, High(Byte));
  if Mobs.GetFreeTile(FX, FY) and (Map.GetTileEnum(FX, FY, Map.Current)
    in FreeTiles) then
  begin
    X := FX;
    Y := FY;
  end;
end;

{ TMobs }

procedure TMobs.Add(AZ: TMapEnum);
var
  I: Integer;
begin
  for I := 0 to Self.Count - 1 do
    if not FMob[I].Alive then
    begin
      FMob[I].AddRandom(AZ);
      Exit;
    end;
  SetLength(FMob, Length(FMob) + 1);
  I := Length(FMob) - 1;
  FMob[I] := TMob.Create;
  FMob[I].AddRandom(AZ);
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
      if Alive and (Maps = Map.Current) and (AX = X) and (AY = Y) then
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
      if Alive and (Maps = Map.Current) and (AX = X) and (AY = Y) then
      begin
        Result := I;
        Exit;
      end;
end;

function TMobs.GetMob(I: Integer): TMob;
begin
  Result := FMob[I]
end;

procedure TMobs.Process;
var
  I: Integer;
begin
  if (Count > 0) then
    for I := 0 to Count - 1 do
      if FMob[I].Alive and (FMob[I].Maps = Map.Current) then
        FMob[I].Process;
end;

procedure TMobs.Render(AX, AY: Byte);
var
  I: Integer;
begin
  if (Count > 0) then
    for I := 0 to Count - 1 do
      if FMob[I].Alive and (FMob[I].Maps = Map.Current) then
        FMob[I].Render(AX, AY);
end;

procedure TMobs.SetMob(I: Integer; const Value: TMob);
begin
  FMob[I] := Value;
end;

function TMobs.GetName(AMobEnum: TMobEnum): string;
begin
  case AMobEnum of

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
    mHound:
      Result := _('Hound');

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
    mNaga:
      Result := _('Naga');

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
    mPan:
      Result := _('Pan');

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
    mOrcWarrior:
      Result := _('Orc Warrior');

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
    mLich:
      Result := _('Lich');

    // == Bosses == //

    // Dark Wood
    mBlackHound:
      Result := _('Black Hound');
    mGiantNewt:
      Result := _('Giant Newt');
    mIguana:
      Result := _('Iguana');
    // Gray Cave
    mKoboldKing:
      Result := _('Kobold King');
    mSwampWorm:
      Result := _('Swamp Worm');
    mGiantSlug:
      Result := _('Giant Slug');
    // Deep Cave
    mCentaur:
      Result := _('Centaur');
    mSatyr:
      Result := _('Satyr');
    mTitan:
      Result := _('Titan');
    // Blood Cave
    mHillGiant:
      Result := _('Hill Giant');
    mStoneGiant:
      Result := _('Stone Giant');
    mTwoHeadedOgre:
      Result := _('Two-Headed Ogre');
    // Dungeon of Doom
    mTrollKing:
      Result := _('Troll King');
  end;
end;

initialization

Mobs := TMobs.Create;

finalization

FreeAndNil(Mobs);

end.
