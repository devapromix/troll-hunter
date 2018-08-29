unit Trollhunter.Mob;

interface

uses
  Trollhunter.Types,
  Trollhunter.Game,
  Trollhunter.Map,
  Trollhunter.Entity,
  Trollhunter.Creature,
  Trollhunter.Mob.Types,
  Trollhunter.Ability;

type
  TMob = class(TCreature)
  private
    FID: UInt;
    FForce: TForce;
    Maps: TMapEnum;
    Boss: Boolean;
    FColor: Cardinal;
    FAlive: Boolean;
    function GetVision: UInt;
  public
    constructor Create();
    destructor Destroy; override;
    procedure Add(AZ: TMapEnum; AX: Int = -1; AY: Int = -1; AID: Int = -1; AForce: TForce = fcEnemy);
    procedure AddNPC(AX, AY: UInt; AZ: TMapEnum; ANPCID: UInt);
    procedure Process;
    procedure Render(AX, AY: UInt);
    procedure Walk(AX, AY: UInt; PX: UInt = 0; PY: UInt = 0);
    procedure Attack;
    procedure Defeat;
    procedure DropItems;
    property ID: UInt read FID write FID;
    property Force: TForce read FForce write FForce;
    property Color: Cardinal read FColor;
    property Vision: UInt read GetVision;
    property Alive: Boolean read FAlive write FAlive;
  end;

type
  TMobs = class(TEntity)
  private
    FMobName: array [TMobEnum] of string;
    FMob: TArray<TMob>;
    function GetMob(I: Int): TMob;
    procedure SetMob(I: Int; const Value: TMob);
    function GetName(I: TMobEnum): string; overload;
    function ChMob(I: Int; AX, AY: UInt): Boolean;
  public
    constructor Create();
    destructor Destroy; override;
    procedure Add(AZ: TMapEnum; AX: Int = -1; AY: Int = -1; AForce: TForce = fcEnemy; AID: Int = -1);
    procedure AddGroup(const AZ: TMapEnum); overload;
    procedure AddGroup(const AZ: TMapEnum; const AMobEnum: TMobEnum; const ACount: UInt); overload;
    function Count: Int;
    procedure Process;
    procedure Render(AX, AY: UInt);
    function GetFreeTile(AX, AY: UInt): Boolean;
    function GetIndex(AX, AY: UInt): Int;
    property Mob[I: Int]: TMob read GetMob write SetMob;
    property Name[I: TMobEnum]: string read GetName;
    function GetName(I: UInt): string; overload;
  end;

type
  TGetXYVal = function(X, Y: Int): Boolean; stdcall;

var
  Mobs: TMobs = nil;
  IsBoss: Boolean;
  NPCName: string = '';
  NPCType: set of TNPCType = [];

implementation

uses
  Math,
  SysUtils,
  TypInfo,
  Trollhunter.Terminal,
  Trollhunter.Player,
  Trollhunter.UI.Log,
  Trollhunter.Language,
  Trollhunter.Item,
  Trollhunter.Player.Skills,
  Trollhunter.Statistic,
  Trollhunter.Attribute,
  Trollhunter.PathFind,
  Trollhunter.Player.Quest,
  Trollhunter.Helpers,
  Trollhunter.Item.Types,
  Trollhunter.Mob.Base,
  EnumHelper;

function MyCallback(X, Y: Int): Boolean; stdcall;
begin
  Result := (Map.GetTileEnum(X, Y, Map.Current) in FreeTiles);
end;

{ TMob }

procedure TMob.AddNPC(AX, AY: UInt; AZ: TMapEnum; ANPCID: UInt);
begin
  Self.Clear;
  X := AX;
  Y := AY;
  Maps := AZ;
  ID := ANPCID;
  Boss := False;
  Alive := True;
  Force := fcNPC;
  Attributes.SetValue(atMaxLife, 100);
  Self.Fill;
end;

function ChMapTile(AMobID, AX, AY: UInt; AZ: TMapEnum): Boolean;
begin
  Result := (Map.GetTileEnum(AX, AY, AZ) in SpawnTiles) and (Player.X <> AX) and (Player.Y <> AY) and Mobs.GetFreeTile(AX, AY) and
    (AZ in MobBase.GetMob(AMobID).Maps);
end;

procedure TMob.Add(AZ: TMapEnum; AX: Int = -1; AY: Int = -1; AID: Int = -1; AForce: TForce = fcEnemy);
var
  I, V, FX, FY: UInt;
begin
  I := 0;
  repeat
    if (AID >= 0) then
      ID := AID
    else
      ID := Math.RandomRange(0, Ord(mbTroll_King) + 1);
    if (AX >= 0) then
      FX := AX
    else
      FX := Math.RandomRange(1, UIntMax - 1);
    if (AY >= 0) then
      FY := AY
    else
      FY := Math.RandomRange(1, UIntMax - 1);
    if (AForce <> fcEnemy) then
      Break;
    if (I >= UIntMax) then
      Exit;
    Inc(I);
  until ChMapTile(ID, FX, FY, AZ);
  if (MobBase.GetMob(ID).Boss and IsBoss) then
    Add(AZ);
  X := FX;
  Y := FY;
  Self.Clear;
  Maps := AZ;
  Boss := False;
  Alive := True;
  Force := AForce;
  Abilities.Modify(abSleeping, 1);
  // Color
  FColor := MobBase.GetMob(ID).Color;
  // Life
  V := Game.EnsureRange(IfThen(MobBase.GetMob(ID).Boss, (MobBase.GetMob(ID).Level + Ord(Game.Difficulty)) * 25, 0), UIntMax);
  Attributes.SetValue(atMaxLife, Math.RandomRange(MobBase.GetMob(ID).MaxLife + V,
    MobBase.GetMob(ID).MaxLife + (Ord(Game.Difficulty) * MobBase.GetMob(ID).Level) + V));
  Self.Fill;
  // DV
  V := MobBase.GetMob(ID).DV + (Ord(Game.Difficulty) * 5);
  Attributes.SetValue(atDV, Math.EnsureRange(Math.RandomRange(V - 10, V + 10), 5, DVMax - 10));
  // PV
  V := MobBase.GetMob(ID).PV + (Ord(Game.Difficulty) * 10);
  Attributes.SetValue(atPV, Game.EnsureRange(Math.RandomRange(V, V * 2), PVMax - 10));
  // Boss
  if MobBase.GetMob(ID).Boss then
  begin
    if Mode.Wizard then
      Game.Log(Format('%s [%d:%d:%d]', [Mobs.GetName(ID), X, Y, Ord(AZ)]));
    Boss := True;
    IsBoss := True;
    // PV
    Attributes.SetValue(atPV, Math.EnsureRange(Math.RandomRange(Attributes.Attrib[atPV].Value,
      Attributes.Attrib[atPV].Value + (MobBase.GetMob(ID).Level * Ord(Game.Difficulty))), Attributes.Attrib[atPV].Value, PVMax - 10));
  end;
end;

procedure TMob.Attack;
var
  The: string;
  Dam: UInt;
  L: UInt;

  procedure Miss();
  begin
    // Miss
    MsgLog.Add(Format(_('%s misses you.'), [The]));
    // MsgLog.Add(Format(_('%s hits you, but your armor protects you.'), [The]));
  end;

begin
  if IsDead or Player.IsDead or (Force <> fcEnemy) then
    Exit;
  The := GetCapit(GetDescThe(Mobs.GetName(ID)));
  if (Player.Attributes.Attrib[atDV].Value < Math.RandomRange(0, 100)) then
  begin
    Game.ShowEffects := False;

    // Bloodlust (10%)
    if (abBloodlust in MobBase.GetMob(ID).Abilities) and (Math.RandomRange(0, 10) = 0) then
    begin
      L := MobBase.GetMob(ID).Level;
      Dam := Game.EnsureRange(Math.RandomRange(L + 5, L + 15), UIntMax);
      Abilities.Modify(abBloodlust, Dam);
      MsgLog.Add(Format(Terminal.Colorize(_('%s feel lust for blood (%d).'), Abilities.GetColor(abBloodlust)), [The, Dam]));
      Exit;
    end;

    // Drunk (5%)
    if (abDrunk in MobBase.GetMob(ID).Abilities) and (Math.RandomRange(0, 20) = 0) then
    begin
      L := MobBase.GetMob(ID).Level;
      Dam := Math.RandomRange(7, L + 10);
      Player.Abilities.Modify(abBlinded, Dam);
      MsgLog.Add(Format(Terminal.Colorize(_('You are drunk (%d).'), Abilities.GetColor(abDrunk)), [Dam]));
      Game.ShowEffects := True;
      Exit;
    end;
    // Blinded (5%)
    if (abBlinded in MobBase.GetMob(ID).Abilities) and (Math.RandomRange(0, 20) = 0) then
    begin
      L := MobBase.GetMob(ID).Level;
      Dam := Math.RandomRange(1, L);
      Player.Abilities.Modify(abBlinded, Dam);
      MsgLog.Add(Format(Terminal.Colorize(_('%s blinded you (%d).'), Abilities.GetColor(abBlinded)), [The, Dam]));
      Game.ShowEffects := True;
      Exit;
    end;
    // Stunned (5%)
    if (abStunned in MobBase.GetMob(ID).Abilities) and (Math.RandomRange(0, 20) = 0) then
    begin
      L := MobBase.GetMob(ID).Level;
      Dam := Game.EnsureRange(Math.RandomRange(1, L), UIntMax);
      Player.Abilities.Modify(abStunned, Dam);
      MsgLog.Add(Format(Terminal.Colorize(_('%s is stuns you (%d).'), Abilities.GetColor(abStunned)), [The, Dam]));
      Game.ShowEffects := True;
      Exit;
    end;
    // Weak (20%)
    if (abWeak in MobBase.GetMob(ID).Abilities) and (Math.RandomRange(0, 5) = 0) then
    begin
      L := MobBase.GetMob(ID).Level;
      Dam := Game.EnsureRange(Math.RandomRange(10, L * 10), UIntMax);
      Player.Abilities.Modify(abWeak, Dam);
      MsgLog.Add(Format(Terminal.Colorize(_('%s has weakened you (%d).'), Abilities.GetColor(abWeak)), [The, Dam]));
      Game.ShowEffects := True;
      Exit;
    end;
    // Diseased (20%)
    if (abDiseased in MobBase.GetMob(ID).Abilities) and (Math.RandomRange(0, 5) = 0) then
    begin
      L := MobBase.GetMob(ID).Level;
      Dam := Game.EnsureRange(Math.RandomRange(L * (Ord(Game.Difficulty) + 5), L * (Ord(Game.Difficulty) + 9)), UIntMax);
      if MobBase.GetMob(ID).Boss then
        Dam := Game.EnsureRange(Dam * 3, UIntMax);
      Player.Abilities.Modify(abDiseased, Dam);
      MsgLog.Add(Format(Terminal.Colorize(_('%s has infected you (%d).'), Abilities.GetColor(abDiseased)), [The, Dam]));
      Game.ShowEffects := True;
      Exit;
    end;
    // Poisoned (10%)
    if (abPoisoned in MobBase.GetMob(ID).Abilities) and (Math.RandomRange(0, 10) = 0) then
    begin
      L := MobBase.GetMob(ID).Level;
      Dam := Game.EnsureRange(Math.RandomRange(L * 5, L * 15), UIntMax);
      Player.Abilities.Modify(abPoisoned, Dam);
      MsgLog.Add(Format(Terminal.Colorize(_('%s is poisoning you (%d).'), Abilities.GetColor(abPoisoned)), [The, Dam]));
      Game.ShowEffects := True;
      Exit;
    end;
    // Afraid (10%)
    if (abAfraid in MobBase.GetMob(ID).Abilities) and (Math.RandomRange(0, 10) = 0) then
    begin
      L := MobBase.GetMob(ID).Level;
      Dam := Game.EnsureRange(Math.RandomRange(L * 10, L * 20), UIntMax);
      Player.Abilities.Modify(abAfraid, Dam);
      MsgLog.Add(Format(Terminal.Colorize(_('%s scared you (%d).'), Abilities.GetColor(abAfraid)), [The, Dam]));
      Game.ShowEffects := True;
      Exit;
    end;
    // Armor Reduction (10%)
    if (abArmor_Reduction in MobBase.GetMob(ID).Abilities) and (Math.RandomRange(0, 10) = 0) then
    begin
      L := MobBase.GetMob(ID).Level;
      Dam := Game.EnsureRange(Math.RandomRange(L * 3, L * 7), UIntMax);
      Player.Abilities.Modify(abArmor_Reduction, Dam);
      // MsgLog.Add(Format(Terminal.Colorize(_('%s scared you (%d).'), Abilities.GetColor(abArmor_Reduction)), [The, Dam]));
      Game.ShowEffects := True;
      Player.Calc;
      Exit;
    end;
    // Cursed (10%)
    if (abCursed in MobBase.GetMob(ID).Abilities) and (Math.RandomRange(0, 10) = 0) then
    begin
      L := MobBase.GetMob(ID).Level;
      Dam := Game.EnsureRange(Math.RandomRange(L * (Ord(Game.Difficulty) + 3), L * (Ord(Game.Difficulty) + 5)), UIntMax);
      if MobBase.GetMob(ID).Boss then
        Dam := Game.EnsureRange(Dam * 3, UIntMax);
      Player.Abilities.Modify(abCursed, Dam);
      MsgLog.Add(Format(Terminal.Colorize(_('%s has cursed you (%d).'), Abilities.GetColor(abCursed)), [The, Dam]));
      Game.ShowEffects := True;
      Exit;
    end;
    // Burning (5%)
    if (abBurning in MobBase.GetMob(ID).Abilities) and (Math.RandomRange(0, 20) = 0) then
    begin
      L := MobBase.GetMob(ID).Level;
      Dam := Game.EnsureRange(Math.RandomRange(L + 2, L + 5), UIntMax);
      Player.Abilities.Modify(abBurning, Dam);
      MsgLog.Add(Format(Terminal.Colorize(_('%s has burnt you (%d).'), Abilities.GetColor(abBurning)), [The, Dam]));
      Game.ShowEffects := True;
      Exit;
    end;
    // Attack
    Dam := Game.EnsureRange(RandomRange(MobBase.GetMob(ID).Damage.Min + Ord(Game.Difficulty),
      MobBase.GetMob(ID).Damage.Max + (Ord(Game.Difficulty) * 3)), UIntMax);
    // Abilities
    if Abilities.IsAbility(abBloodlust) then
      Inc(Dam, (Dam div 3));
    // PV
    Dam := GetRealDamage(Dam, Player.Attributes.Attrib[atPV].Value);
    if (Dam = 0) then
    begin
      Miss();
      Exit;
    end;
    // Damage
    Player.Attributes.Modify(atLife, -Dam);
    MsgLog.Add(Format(_('%s hits you (%d).'), [The, Dam]));
    if (((Math.RandomRange(0, 9 - Ord(Game.Difficulty)) = 0) and not Mode.Wizard)) then
      Player.BreakItem();
    if Player.IsDead then
      Player.Defeat(Mobs.GetName(ID));
  end
  else
    Miss();
end;

constructor TMob.Create;
begin
  inherited;
end;

procedure TMob.Defeat;
var
  S, The: string;
  V: UInt;
begin
  // Quests
  Quests.DoQuest(qtKillMobs, FID);
  Self.Alive := False;
  The := GetDescThe(Mobs.GetName(ID));
  case Math.RandomRange(0, 2) of
    0:
      S := Format(_('You kill %s.'), [The]);
  else
    S := Format(_('%s dies.'), [GetCapit(The)]);
  end;
  if Boss then
    S := Terminal.Colorize(S, clAlarm);
  MsgLog.Add(S);
  Player.Statictics.Inc(stKills);
  Player.AddExp(MobBase.GetMob(ID).Level);

  // Mana and Life After Each Kill
  V := Player.Attributes.Attrib[atLifeAfEachKill].Value.InRange(LifeAEKMax);
  if (V > 0) then
    Player.Attributes.Modify(atLife, V);
  V := Player.Attributes.Attrib[atManaAfEachKill].Value.InRange(ManaAEKMax);
  if (V > 0) then
    Player.Attributes.Modify(atMana, V);
  if Boss then
    V := 25
  else
    V := 1;
  Player.Statictics.Inc(stScore, MobBase.GetMob(ID).Level * V);
  Self.DropItems;
  // Boss
  if (Boss and (Map.Current = FinalDungeon) and (ID.MobEnum = mbTroll_King)) then
  begin
    if not Mode.Wizard then
      Game.Won := True;
    MsgLog.Add(Terminal.Colorize(_('You have won!!!'), clAlarm));
    Player.Statictics.Inc(stScore, 2000);
    Game.Screenshot := Terminal.GetTextScreenshot();
  end;
end;

destructor TMob.Destroy;
begin

  inherited;
end;

procedure TMob.DropItems;
begin
  Items.Loot(Self.X, Self.Y, Boss);
  if Game.LCorpses then
    Items.Loot(Self.X, Self.Y, ivCorpse);
end;

function TMob.GetVision: UInt;
begin
  Result := EnsureRange(VisionMax - (Player.Skills.GetSkill(skStealth) div 6), 3, VisionMax);
end;

procedure TMob.Process;
var
  NX, NY: Int;
  The: string;
  Dist: UInt;
begin
  // Exit;

  NX := 0;
  NY := 0;
  if (Force = fcNPC) then
    Exit;
  Dist := GetDist(Player.X, Player.Y);
  if (Dist > GetVision) then
    Exit;
  if Abilities.IsAbility(abSleeping) then
  begin
    if (Math.RandomRange(0, 99) <= 15) then
    begin
      Abilities.Ability[abSleeping] := 0;
      if (Player.Attributes.Attrib[atPer].Value > Math.RandomRange(0, 100)) then
      begin
        The := GetCapit(GetDescThe(Mobs.GetName(ID)));
        MsgLog.Add(Format(_('%s notices you!'), [The]));
      end;
      Exit;
    end;
    Exit;
  end;
  if (Dist <= 2) and Player.IsRest then
    Player.IsRest := False;
  // A*
  if not PathFind(UIntMax + 1, UIntMax + 1, X, Y, Player.X, Player.Y, @MyCallback, NX, NY) then
    Exit;
  if (NX = Int(Player.X)) and (NY = Int(Player.Y)) then
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
  OnTurn();
  if Self.IsDead then
    Self.Defeat;
end;

procedure TMob.Render(AX, AY: UInt);
var
  C: Char;
begin
  if not Map.InView(X, Y) or (not Mode.Wizard and not Map.GetFOV(X, Y)) then
    Exit;
  if not Mode.Wizard and (Player.GetDist(X, Y) > Player.Vision) then
    Exit;
  C := MobBase.GetMob(ID).Symbol;
  if (Self.Boss) then
    C := Chr(Ord(C) - 32);
  if Player.Look then
    Terminal.Print(X - Player.X + AX + View.Left, Y - Player.Y + AY + View.Top, C, Color)
  else if Self.Force = fcEnemy then
    Terminal.Print(X - Player.X + AX + View.Left, Y - Player.Y + AY + View.Top, C, Color, clBkMob)
  else
    Terminal.Print(X - Player.X + AX + View.Left, Y - Player.Y + AY + View.Top, C, Color, clBkPlayer);
end;

procedure TMob.Walk(AX, AY: UInt; PX: UInt = 0; PY: UInt = 0);
var
  NX, NY: ShortInt;
  FX, FY: UInt;
begin
  NX := 0;
  NY := 0;
  //
  if (AX < PX) then
  begin
    case Math.RandomRange(0, 4) of
      0:
        begin
          NY := -1;
        end;
      1:
        begin
          NY := +1;
        end;
      2:
        begin
          NX := +1;
          NY := -1;
        end;
      3:
        begin
          NX := +1;
          NY := +1;
        end;
    end;
  end;
  if (AX > PX) then
  begin
    case Math.RandomRange(0, 4) of
      0:
        begin
          NY := -1;
        end;
      1:
        begin
          NY := +1;
        end;
      2:
        begin
          NX := -1;
          NY := +1;
        end;
      3:
        begin
          NX := -1;
          NY := -1;
        end;
    end;
  end;
  //
  if (AY < PY) then
  begin
    case Math.RandomRange(0, 4) of
      0:
        begin
          NX := +1;
        end;
      1:
        begin
          NX := -1;
        end;
      2:
        begin
          NX := -1;
          NY := +1;
        end;
      3:
        begin
          NX := +1;
          NY := +1;
        end;
    end;
  end;
  if (AY > PY) then
  begin
    case Math.RandomRange(0, 4) of
      0:
        begin
          NX := +1;
        end;
      1:
        begin
          NX := -1;
        end;
      2:
        begin
          NX := -1;
          NY := -1;
        end;
      3:
        begin
          NX := +1;
          NY := -1;
        end;
    end;
  end;
  //
  FX := Map.EnsureRange(X + NX);
  FY := Map.EnsureRange(Y + NY);
  if Mobs.GetFreeTile(FX, FY) and (Map.GetTileEnum(FX, FY, Map.Current) in FreeTiles) then
  begin
    X := FX;
    Y := FY;
  end;
end;

{ TMobs }

procedure TMobs.Add(AZ: TMapEnum; AX: Int = -1; AY: Int = -1; AForce: TForce = fcEnemy; AID: Int = -1);
var
  I: Int;

  procedure AddMob();
  begin
    FMob[I].Add(AZ, AX, AY, AID, AForce);
  end;

begin
  for I := 0 to Self.Count - 1 do
    if not FMob[I].Alive then
    begin
      AddMob();
      Exit;
    end;
  SetLength(FMob, Length(FMob) + 1);
  I := Length(FMob) - 1;
  FMob[I] := TMob.Create;
  AddMob();
end;

procedure TMobs.AddGroup(const AZ: TMapEnum);
var
  ID, FX, FY, FCount: UInt;
  I: Int;
begin
  repeat
    ID := Math.RandomRange(0, Ord(mbTroll_King) + 1);
    repeat
      FX := Math.RandomRange(1, UIntMax - 1);
      FY := Math.RandomRange(1, UIntMax - 1);
      if (Ord(AZ) > 0) then
        Break;
    until (Player.GetDist(FX, FY) > 50);
  until ChMapTile(ID, FX, FY, AZ);
  FCount := MobBase.GetMob(ID).MaxCount;
  FCount := Math.EnsureRange(Math.RandomRange(FCount div 2, FCount), 1, FCount);
  if (FCount > 1) then
    FCount := Math.RandomRange(FCount, FCount + (Ord(Game.Difficulty) * 2));
  for I := 1 to FCount do
  begin
    repeat
      FX := Math.EnsureRange(FX + (RandomRange(0, 3) - 1), 1, UIntMax - 1);
      FY := Math.EnsureRange(FY + (RandomRange(0, 3) - 1), 1, UIntMax - 1);
    until ChMapTile(ID, FX, FY, AZ);
    Self.Add(AZ, FX, FY, fcEnemy, ID);
  end;
end;

procedure TMobs.AddGroup(const AZ: TMapEnum; const AMobEnum: TMobEnum; const ACount: UInt);
var
  ID, FX, FY: Int;
  FCount: UInt;
begin
  ID := Ord(AMobEnum);
  repeat
    FX := Math.RandomRange(1, UIntMax - 1);
    FY := Math.RandomRange(1, UIntMax - 1);
    if (Ord(AZ) > 0) then
      Break;
  until (Player.GetDist(FX, FY) > 50) and ChMapTile(ID, FX, FY, AZ);
  FCount := 0;
  if Mode.Wizard then
    MsgLog.Add(Format('%dx%d', [FX, FY]));
  while (FCount < ACount) do
  begin
    repeat
      FX := Math.EnsureRange(FX + (RandomRange(0, 3) - 1), 1, UIntMax - 1);
      FY := Math.EnsureRange(FY + (RandomRange(0, 3) - 1), 1, UIntMax - 1);
    until ChMapTile(ID, FX, FY, AZ);
    Self.Add(AZ, FX, FY, fcEnemy, ID);
    Inc(FCount);
  end;
end;

function TMobs.Count: Int;
begin
  Result := Length(FMob)
end;

constructor TMobs.Create;
var
  I: TMobEnum;
begin
  SetLength(FMob, 0);
  for I := Low(TMobEnum) to High(TMobEnum) do
  begin
    FMobName[I] := Enum<TMobEnum>.ValueName(I).GetName('mb');
  end;
end;

destructor TMobs.Destroy;
var
  I: Int;
begin
  for I := 0 to Count - 1 do
    FreeAndNil(FMob[I]);
  inherited;
end;

function TMobs.ChMob(I: Int; AX, AY: UInt): Boolean;
begin
  with FMob[I] do
    Result := Alive and (Maps = Map.Current) and (AX = X) and (AY = Y)
end;

function TMobs.GetFreeTile(AX, AY: UInt): Boolean;
var
  I: Int;
begin
  Result := True;
  for I := 0 to Count - 1 do
    if ChMob(I, AX, AY) then
      Exit(False);
end;

function TMobs.GetIndex(AX, AY: UInt): Int;
var
  I: Int;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if ChMob(I, AX, AY) then
    begin
      Result := I;
      Exit;
    end;
end;

function TMobs.GetMob(I: Int): TMob;
begin
  Result := FMob[I]
end;

procedure TMobs.Process;
var
  I: Int;
begin
  if (Count > 0) then
    for I := 0 to Count - 1 do
      if FMob[I].Alive and (FMob[I].Maps = Map.Current) then
        FMob[I].Process;
end;

procedure TMobs.Render(AX, AY: UInt);
var
  I: Int;
begin
  if (Count > 0) then
    for I := 0 to Count - 1 do
      if FMob[I].Alive and (FMob[I].Maps = Map.Current) then
        FMob[I].Render(AX, AY);
end;

procedure TMobs.SetMob(I: Int; const Value: TMob);
begin
  FMob[I] := Value;
end;

function TMobs.GetName(I: UInt): string;
begin
  Result := GetName(I.MobEnum);
end;

function TMobs.GetName(I: TMobEnum): string;
begin
  Result := FMobName[I];
end;

initialization

Mobs := TMobs.Create;

finalization

FreeAndNil(Mobs);

end.
