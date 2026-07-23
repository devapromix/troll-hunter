unit Trollhunter.Player;

interface

uses
  Types,
  Trollhunter.Types,
  Trollhunter.Player.Types,
  Trollhunter.Creature,
  Trollhunter.Mob,
  Trollhunter.Item.Common,
  Trollhunter.Skill,
  Trollhunter.Statistic,
  Trollhunter.Talent,
  Trollhunter.Player.Races,
  Trollhunter.Player.Classes;

const
  // Player
  VisionMax = 15;
  DVMax = 80;
  LevelExpMax = 9;
  ReLifeMax = 10;
  ReManaMax = 20;
  LifeAEKMax = 8;
  ManaAEKMax = 12;
  LifeTurnMax = 150;
  ManaTurnMax = 90;
  // Satiation
  StarvingMax = 500;
  SatiatedMax = 8000;
  EngorgedMax = 15000;
  // Metabolism
  MetabolismMin = 65;
  MetabolismMax = 135;
  // Inventory
  ItemMax = 26;
  StartGold = 250;
  // Talents
  MinPrm = 1;
  TalentPrm = 3;
  AttribPrm = 7;

type

  { TPlayer }

  TPlayer = class(TCreature)
  private
    FLX: UInt;
    FLY: UInt;
    FMaxMap: UInt;
    FLook: boolean;
    FGold: Int;
    FKiller: string;
    FWeaponSkill: TSkillEnum;
    FRace: TRaceEnum;
    FClass: TClassEnum;
    FItemIsDrop: boolean;
    FItemIndex: Int;
    FItemAmount: Int;
    FSatPerTurn: UInt;
    FBackground: string;
    FIsRest: boolean;
    FName: string;
    FStatistics: TStatistics;
    FSex: TSexEnum;
    FTalents: TTalents;
    FSkills: TSkills;
    FFireMode: boolean;
    FFireTargets: array of Int;
    FFireIndex: Int;
    FBowLevel: UInt;
    FBowMinDamage: UInt;
    FBowMaxDamage: UInt;
    procedure GenNPCText;
    function GetVision: UInt;
    procedure Empty;
    function GetEquippedIndex(ASlot: TSlotType): Int;
    function GetQuiverIndex: Int;
    function HasArrows: boolean;
    procedure UseArrow;
    function HasCharges: boolean;
    procedure UseCharge;
  public
    constructor Create;
    destructor Destroy; override;
    property LX: UInt read FLX write FLX;
    property LY: UInt read FLY write FLY;
    property Vision: UInt read GetVision;
    property MaxMap: UInt read FMaxMap write FMaxMap;
    property Look: boolean read FLook write FLook;
    property WeaponSkill: TSkillEnum read FWeaponSkill;
    property FireMode: boolean read FFireMode;
    property FireIndex: Int read FFireIndex;
    property Gold: Int read FGold write FGold;
    property Killer: string read FKiller write FKiller;
    property IsRest: boolean read FIsRest write FIsRest;
    property ItemIsDrop: boolean read FItemIsDrop write FItemIsDrop;
    property ItemIndex: Int read FItemIndex write FItemIndex;
    property ItemAmount: Int read FItemAmount write FItemAmount;
    property SatPerTurn: UInt read FSatPerTurn write FSatPerTurn;
    property Statictics: TStatistics read FStatistics write FStatistics;
    property Background: string read FBackground;
    property Name: string read FName write FName;
    property Skills: TSkills read FSkills write FSkills;
    property HRace: TRaceEnum read FRace write FRace;
    property HClass: TClassEnum read FClass write FClass;
    property Talents: TTalents read FTalents write FTalents;
    procedure SetAmountScene(IsDrop: boolean; Index, Amount: Int);
    property Sex: TSexEnum read FSex write FSex;
    procedure Render(AX, AY: UInt);
    procedure Move(Dir: TDirectionEnum);
    procedure RenderInfo;
    procedure Calc;
    procedure Wait;
    procedure Clear();
    procedure AddTurn;
    procedure GenerateBackground();
    procedure Spawn;
    function GetSatiationStr: string;
    function SaveCharacterDump(AReason: string): string;
    procedure Defeat(AKiller: string = '');
    procedure Attack(Index: Int);
    procedure RangedAttack(Index: Int);
    function CanFire: boolean;
    procedure FireModeEnter;
    procedure FireModeExit;
    procedure FireModeSwitch(ADir: Int);
    function FireModeTarget: Int;
    function FireRange: UInt;
    function RangedMinDamage: UInt;
    function RangedMaxDamage: UInt;
    function CanRangedAttack: boolean;
    procedure ReceiveHealing;
    function HasQuiver: boolean;
    function IsQuiverBroken: boolean;
    function GetArrowsToBuy: Int;
    procedure BuyArrows;
    procedure RechargeWand(Index: Int);
    procedure Buy(Index: Int);
    procedure PickUp;
    procedure PickUpAmount(Index: Int);
    procedure PickUpArrows(const MapID, Index: Int; AItem: Item);
    procedure Drop(Index: Int);
    procedure DropAmount(Index: Int);
    procedure Use(Index: Int);
    procedure DoEffects(const Effects: TEffects; const Value: UInt = 0;
      const Multiplier: UInt = 1);
    procedure Equip(Index: Int);
    procedure UnEquip(Index: Int);
    procedure Sell(Index: Int);
    procedure RepairItem(Index: Int);
    procedure IdentItem(Index: Int);
    procedure IdentAllItems;
    procedure CraftItem(Index: Int);
    procedure BreakItem(Index: Int; Value: UInt = 1); overload;
    procedure BreakItem(ASlot: TSlotType; Value: UInt = 1); overload;
    procedure BreakItem(); overload;
    procedure AddExp(Value: UInt = 1);
    procedure Start;
    procedure DoWeaponSkill;
    procedure Rest(ATurns: UInt);
    procedure Dialog(AMob: TMob);
    procedure RnItem(FItem: Item; const Index: Int);
    procedure AutoPickup();
    procedure RenderWeather(const AX, AY, AWidth: UInt);
    procedure Turn;
    procedure StartEquip;
    procedure StartSkills;
    function GetStartGold: UInt;
    function IsOnStash: boolean;
  end;

var
  Player: TPlayer = nil;

implementation

uses
  Classes,
  SysUtils,
  Math,
  Trollhunter.Game,
  Trollhunter.Map,
  Trollhunter.Scenes,
  Trollhunter.Item,
  Dialogs,
  Trollhunter.Terminal,
  Trollhunter.UI.Log,
  Trollhunter.Calendar,
  Trollhunter.Item.Shop,
  BearLibTerminal,
  Trollhunter.Ability,
  Trollhunter.Item.Affixes,
  Trollhunter.Attribute,
  Trollhunter.Spellbook,
  Trollhunter.UI,
  Trollhunter.Item.Dungeon,
  Trollhunter.Item.Inventory,
  Trollhunter.Helpers,
  Trollhunter.Item.Types,
  Trollhunter.Utils;

  { TPlayer }

// Generate a random player's background (from Kharne and UMoria roguelikes)
procedure TPlayer.GenerateBackground();
var
  I: (cpChild, cpClass, cpParent, cpCredit, cpBackground, cpEyeType,
    cpEyeColour, cpHairStyle, cpHairColour, cpComplexion);
  SL: array [Low(I) .. High(I)] of TStringList;
begin
  Randomize;
  FBackground := '';
  for I := Low(I) to High(I) do
    SL[I] := TStringList.Create;
  try
    SL[cpChild].DelimitedText :=
      '"an only child","one of two children",' +
      '"one of many children","the only surviving child","one of several children",'
      +
      '"the illegitimate but acknowledged child","the illegitimate and unacknowledged child"';
    SL[cpClass].DelimitedText :=
      '"lower-class", "middle-class","upper-class"';
    SL[cpParent].DelimitedText :=
      '"mercenary","merchant","businessman","titled noble",' +
      '"craftsman","soldier","templar","priest","guildsman","townsman"';
    SL[cpBackground].DelimitedText :=
      '"contented","peaceful","troubled","settled","disturbed"';
    SL[cpCredit].DelimitedText :=
      '"a credit to","a disgrace to","the black sheep of"';
    SL[cpEyeType].DelimitedText :=
      '"dull","unusually piercing","piercing","striking","dark"';
    SL[cpEyeColour].DelimitedText :=
      '"grey","violet","green","blue","brown","blue-gray"';
    SL[cpHairStyle].DelimitedText :=
      '"wavy","curly","straight","short","long"';
    SL[cpHairColour].DelimitedText :=
      '"auburn","blonde","black","dark","red","ginger","grey","brown"';
    SL[cpComplexion].DelimitedText :=
      '"an average","a sallow","a fair","a dark","a light"';

    FBackground :=
      Format(Terminal.Colorize(
      'You are %s of a %s %s. You had a %s upbringing and you ' +
      'are %s the family. You have %s %s eyes, %s %s hair, and %s complexion.',
      'Yellow'), [SL[cpChild][Random(SL[cpChild].Count - 1)],
      SL[cpClass][Random(SL[cpClass].Count - 1)],
      SL[cpParent][Random(SL[cpParent].Count - 1)],
      SL[cpBackground][Random(SL[cpBackground].Count - 1)],
      SL[cpCredit][Random(SL[cpCredit].Count - 1)],
      SL[cpEyeType][Random(SL[cpEyeType].Count - 1)],
      SL[cpEyeColour][Random(SL[cpEyeColour].Count - 1)],
      SL[cpHairStyle][Random(SL[cpHairStyle].Count - 1)],
      SL[cpHairColour][Random(SL[cpHairColour].Count - 1)],
      SL[cpComplexion][Random(SL[cpComplexion].Count - 1)]]);
  finally
    for I := Low(I) to High(I) do
      FreeAndNil(SL[I]);
  end;
end;

procedure TPlayer.RnItem(FItem: Item; const Index: Int);
begin
  if (FItem.Durability = 0) then
  begin
    Items_Inventory_DeleteItem(Index, FItem);
    MsgLog.Add(Terminal.Colorize(Format('%s been ruined irreversibly.',
      [Items.GetNameThe(FItem)]), clAlarm));
  end;
end;

procedure TPlayer.AddTurn;
begin
  if IsDead then
    Exit;
  Statictics.Inc(stTurn);
  Calendar.Turn;
  if (Attributes.Attrib[atSat].Value > 0) and
    (Math.RandomRange(0, MetabolismMax) <= Player.Statictics.Get(stMetabolism)) then
    if not Utils.Chance(Player.Talents.GetLevel(tlSurvival) * 10) then
      Attributes.Modify(atSat, -SatPerTurn);
  if Abilities.IsAbility(abWeak) then
    Attributes.Modify(atSat, -10);
  if (Attributes.Attrib[atSat].Value < StarvingMax) then
    Attributes.Modify(atLife, -1);
  Turn;
  if OnTurn() then
    Calc;
  if IsDead then
    Defeat;
  Mobs.Process;
end;

procedure TPlayer.DoWeaponSkill;
begin
  case FWeaponSkill of
    skBlade:
    begin
      Skills.DoSkill(FWeaponSkill, 2);
      Skills.DoSkill(skAthletics, 2);
      Skills.DoSkill(skDodge, 2);
      SatPerTurn := Ord(Game.Difficulty) + 5;
    end;
    skAxe:
    begin
      Skills.DoSkill(FWeaponSkill, 2);
      Skills.DoSkill(skAthletics, 3);
      Skills.DoSkill(skDodge);
      SatPerTurn := Ord(Game.Difficulty) + 6;
    end;
    skSpear, skDagger:
    begin
      Skills.DoSkill(FWeaponSkill, 2);
      Skills.DoSkill(skAthletics);
      Skills.DoSkill(skDodge, 3);
      SatPerTurn := Ord(Game.Difficulty) + 4;
    end;
    skMace:
    begin
      Skills.DoSkill(FWeaponSkill, 2);
      Skills.DoSkill(skAthletics, 4);
      SatPerTurn := Ord(Game.Difficulty) + 7;
    end;
    skStaff, skWand:
    begin
      Skills.DoSkill(FWeaponSkill, 2);
      Skills.DoSkill(skDodge);
      Skills.DoSkill(skConcentration, 3);
      SatPerTurn := Ord(Game.Difficulty) + 8;
    end;
    skBow:
    begin
      Skills.DoSkill(FWeaponSkill, 2);
      Skills.DoSkill(skDodge, 3);
      Skills.DoSkill(skAthletics);
      SatPerTurn := Ord(Game.Difficulty) + 4;
    end;
  end;
end;

procedure TPlayer.Attack(Index: Int);
const
  AccuracyDexDivisor = 2;
var
  V, Ch: UInt;
  Mob: TMob;
  Dam, Cr, TargetDV, AccBonus: UInt;
  CrStr, The: string;

  procedure Miss();
  begin
    MsgLog.Add(Format('You miss %s.', [The]));
    // MsgLog.Add(Format('You fail to hurt %s.', [The]));
    SatPerTurn := Ord(Game.Difficulty) + 3;
  end;

begin
  if (Index < 0) then
    Exit;
  Mob := Mobs.Mob[Index];
  if not Mob.Alive then
    Exit;
  if (Mob.Force <> fcEnemy) then
  begin
    Self.Dialog(Mob);
    GenNPCText;
    Exit;
  end;
  The := GetDescThe(Mobs.Name[TMobEnum(Mob.ID)]);
  TargetDV := Mob.Attributes.Attrib[atDV].Value;
  if Abilities.IsAbility(abBerserk) then
    TargetDV := TargetDV div 2;
  AccBonus := Self.Attributes.Attrib[atDV].Value div AccuracyDexDivisor;
  TargetDV := UInt(Math.Max(0, Int(TargetDV) - Int(AccBonus)));
  if (TargetDV < Math.RandomRange(0, 100)) and not Abilities.IsAbility(abCursed) then
  begin
    CrStr := '';
    // Attack
    Dam := Game.EnsureRange(RandomRange(Self.GetDamage.Min, GetDamage.Max +
      1), UIntMax);
    // Abilities
    if Abilities.IsAbility(abBloodlust) then
      Inc(Dam, Dam div 4);
    if Abilities.IsAbility(abWeak) then
      Dec(Dam, Dam div 3);
    // Critical hits...     .
    Ch := Math.RandomRange(0, 100);
    Cr := Skills.Skill[FWeaponSkill].Value;
    if ((Ch < Cr) and not Abilities.IsAbility(abWeak)) then
    begin
      if (Ch > (Cr div 10)) then
      begin
        V := 2;
        CrStr := 'It was a good hit!';
      end
      else
      begin
        V := 3;
        CrStr := 'It was an excellent hit!';
      end;
      Dam := Dam * V;
      CrStr := CrStr + Format(' (%dx)', [V]);
    end;
    // PV
    Dam := Self.GetRealDamage(Dam, Mob.Attributes.Attrib[atPV].Value);
    if (Dam = 0) then
    begin
      Miss();
      AddTurn;
      Exit;
    end;
    // Attack
    Mob.Attributes.Modify(atLife, -Dam);
    MsgLog.Add(Format('You hit %s (%d).', [The, Dam]));
    // Break weapon
    if ((Math.RandomRange(0, 10 - Ord(Game.Difficulty)) = 0) and not Mode.Wizard) then
      BreakItem(stMainHand);
    if (CrStr <> '') then
      MsgLog.Add(Terminal.Colorize(CrStr, clAlarm));
    DoWeaponSkill;
    // Victory
    if Mob.IsDead then
      Mob.Defeat;
  end
  else
    Miss();
  AddTurn;
end;

function TPlayer.GetEquippedIndex(ASlot: TSlotType): Int;
var
  FItem: Item;
  FCount, I: Int;
begin
  Result := -1;
  FCount := Items_Inventory_GetCount().InRange(ItemMax);
  for I := 0 to FCount - 1 do
  begin
    FItem := Items_Inventory_GetItem(I);
    if (FItem.Equipment > 0) and (ItemBase[TItemEnum(FItem.ItemID)].SlotType =
      ASlot) then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function TPlayer.GetQuiverIndex: Int;
begin
  Result := Self.GetEquippedIndex(stQuiver);
end;

function TPlayer.HasCharges: boolean;
var
  WIndex: Int;
begin
  WIndex := Self.GetEquippedIndex(stRanged);
  Result := (WIndex >= 0) and (Items_Inventory_GetItem(WIndex).Value > 0);
end;

procedure TPlayer.UseCharge;
var
  WIndex: Int;
  FItem: Item;
begin
  WIndex := Self.GetEquippedIndex(stRanged);
  if (WIndex < 0) then
    Exit;
  FItem := Items_Inventory_GetItem(WIndex);
  if (FItem.Value = 0) then
    Exit;
  FItem.Value := Game.EnsureRange(FItem.Value - 1, UIntMax);
  Items_Inventory_SetItem(WIndex, FItem);
end;

function TPlayer.HasQuiver: boolean;
begin
  Result := (Self.GetQuiverIndex >= 0);
end;

function TPlayer.IsQuiverBroken: boolean;
var
  QIndex: Int;
begin
  QIndex := Self.GetQuiverIndex;
  Result := (QIndex >= 0) and (Items_Inventory_GetItem(QIndex).Durability = 0);
end;

function TPlayer.HasArrows: boolean;
var
  QIndex: Int;
  FItem: Item;
begin
  QIndex := Self.GetQuiverIndex;
  if (QIndex < 0) then
    Exit(False);
  FItem := Items_Inventory_GetItem(QIndex);
  Result := (FItem.Durability > 0) and (FItem.Value > 0);
end;

procedure TPlayer.UseArrow;
var
  QIndex: Int;
  FItem: Item;
begin
  QIndex := Self.GetQuiverIndex;
  if (QIndex < 0) then
    Exit;
  FItem := Items_Inventory_GetItem(QIndex);
  if (FItem.Durability = 0) or (FItem.Value = 0) then
    Exit;
  FItem.Value := Game.EnsureRange(FItem.Value - 1, UIntMax);
  Items_Inventory_SetItem(QIndex, FItem);
  if (FItem.Value <= 25) then
    MsgLog.Add(Terminal.Colorize(
      Format('You are running out of arrows (%d left in your quiver).',
      [FItem.Value]), clAlarm));
end;

procedure TPlayer.RangedAttack(Index: Int);
const
  RangeAccuracyPenalty = 3;
  AccuracyDexDivisor = 2;
var
  V, Ch: UInt;
  Mob: TMob;
  Dam, Cr, TargetDV, AccBonus, Dist, RMin, RMax: UInt;
  CrStr, The: string;

  procedure Miss();
  begin
    MsgLog.Add(Format('You miss %s.', [The]));
    SatPerTurn := Ord(Game.Difficulty) + 3;
  end;

begin
  if (Index < 0) then
    Exit;
  Mob := Mobs.Mob[Index];
  if not Mob.Alive then
    Exit;
  if (Mob.Force <> fcEnemy) then
    Exit;
  Dist := Self.GetDist(Mob.X, Mob.Y);
  if (Dist <= 1) then
  begin
    Self.FireModeExit;
    Self.Attack(Index);
    Exit;
  end;
  if (FWeaponSkill = skWand) then
  begin
    if not Self.HasCharges then
    begin
      MsgLog.Add('Your wand has no charges left.');
      Self.FireModeExit;
      Exit;
    end;
    Self.UseCharge;
  end;
  if (FWeaponSkill = skBow) then
  begin
    if not Self.HasQuiver then
    begin
      MsgLog.Add('You need a quiver equipped to do that.');
      Self.FireModeExit;
      Exit;
    end;
    if Self.IsQuiverBroken then
    begin
      MsgLog.Add('Your quiver is broken and can''t hold arrows.');
      Self.FireModeExit;
      Exit;
    end;
    if not Self.HasArrows then
    begin
      MsgLog.Add('You have no arrows left in your quiver.');
      Self.FireModeExit;
      Exit;
    end;
    Self.UseArrow;
  end;
  The := GetDescThe(Mobs.Name[TMobEnum(Mob.ID)]);
  TargetDV := Mob.Attributes.Attrib[atDV].Value;
  if Abilities.IsAbility(abBerserk) then
    TargetDV := TargetDV div 2;
  if (Dist > 1) then
    Inc(TargetDV, (Dist - 1) * RangeAccuracyPenalty);
  AccBonus := Self.Attributes.Attrib[atDV].Value div AccuracyDexDivisor;
  TargetDV := UInt(Math.Max(0, Int(TargetDV) - Int(AccBonus)));
  if (TargetDV < Math.RandomRange(0, 100)) and not Abilities.IsAbility(abCursed) then
  begin
    CrStr := '';
    RMin := EnsureRange(FBowMinDamage + Attributes.Attrib[atDex]
      .Value div 5, 1, UIntMax - 1);
    RMax := EnsureRange(FBowMaxDamage + Attributes.Attrib[atDex].Value div
      3, 2, UIntMax);
    Dam := Game.EnsureRange(RandomRange(RMin, RMax + 1), UIntMax);
    // Abilities
    if Abilities.IsAbility(abBloodlust) then
      Inc(Dam, Dam div 4);
    if Abilities.IsAbility(abWeak) then
      Dec(Dam, Dam div 3);
    // Critical hits...
    Ch := Math.RandomRange(0, 100);
    Cr := Skills.Skill[FWeaponSkill].Value;
    if ((Ch < Cr) and not Abilities.IsAbility(abWeak)) then
    begin
      if (Ch > (Cr div 10)) then
      begin
        V := 2;
        CrStr := 'It was a good hit!';
      end
      else
      begin
        V := 3;
        CrStr := 'It was an excellent hit!';
      end;
      Dam := Dam * V;
      CrStr := CrStr + Format(' (%dx)', [V]);
    end;
    // PV
    Dam := Self.GetRealDamage(Dam, Mob.Attributes.Attrib[atPV].Value);
    if (Dam = 0) then
    begin
      Miss();
      AddTurn;
      Exit;
    end;
    // Attack
    Mob.Attributes.Modify(atLife, -Dam);
    if (FWeaponSkill = skWand) then
      MsgLog.Add(Format('Your charge hits %s (%d).', [The, Dam]))
    else if (FWeaponSkill = skBow) then
      MsgLog.Add(Format('Your arrow hits %s (%d).', [The, Dam]));
    // Break weapon
    if ((Math.RandomRange(0, 15 - Ord(Game.Difficulty)) = 0) and not Mode.Wizard) then
      BreakItem(stRanged)
    else
    if (FWeaponSkill <> skWand) and
      ((Math.RandomRange(0, 20 - Ord(Game.Difficulty)) = 0) and not Mode.Wizard) then
      BreakItem(stQuiver);
    if (CrStr <> '') then
      MsgLog.Add(Terminal.Colorize(CrStr, clAlarm));
    DoWeaponSkill;
    // Victory
    if Mob.IsDead then
      Mob.Defeat;
  end
  else
    Miss();
  AddTurn;
end;

function TPlayer.CanFire: boolean;
begin
  Result := (FWeaponSkill = skBow) or (FWeaponSkill = skWand);
end;

procedure TPlayer.FireModeEnter;
var
  I, J: Int;
  Tmp: Int;
  PrevTarget: Int;

  function TargetDist(Idx: Int): Int;
  begin
    Result := Self.GetDist(Mobs.Mob[FFireTargets[Idx]].X,
      Mobs.Mob[FFireTargets[Idx]].Y);
  end;

begin
  PrevTarget := Self.FireModeTarget;
  SetLength(FFireTargets, 0);
  FFireIndex := -1;
  if not CanFire then
  begin
    FFireMode := False;
    MsgLog.Add('You need a bow or wand equipped to do that.');
    Exit;
  end;
  if (FWeaponSkill <> skWand) then
  begin
    if not Self.HasQuiver then
    begin
      FFireMode := False;
      MsgLog.Add('You need a quiver equipped to do that.');
      Exit;
    end;
    if Self.IsQuiverBroken then
    begin
      FFireMode := False;
      MsgLog.Add('Your quiver is broken and can''t hold arrows.');
      Exit;
    end;
    if not Self.HasArrows then
    begin
      FFireMode := False;
      MsgLog.Add('You have no arrows left in your quiver.');
      Exit;
    end;
  end
  else if not Self.HasCharges then
  begin
    FFireMode := False;
    MsgLog.Add('Your wand has no charges left.');
    Exit;
  end;
  for I := 0 to Mobs.Count - 1 do
    if Mobs.Mob[I].Alive and (Mobs.Mob[I].Force = fcEnemy) and
      (Mobs.Mob[I].MapZone = Map.Current) and
      Map.InView(Mobs.Mob[I].X, Mobs.Mob[I].Y) and
      (Self.GetDist(Mobs.Mob[I].X, Mobs.Mob[I].Y) > 1) and
      (Mode.Wizard or (Map.GetFOV(Mobs.Mob[I].X, Mobs.Mob[I].Y) and
      (Self.GetDist(Mobs.Mob[I].X, Mobs.Mob[I].Y) <= Self.FireRange))) then
    begin
      SetLength(FFireTargets, Length(FFireTargets) + 1);
      FFireTargets[High(FFireTargets)] := I;
    end;
  if (Length(FFireTargets) = 0) then
  begin
    FFireMode := False;
    MsgLog.Add('There is no one in sight to shoot at.');
    Exit;
  end;
  for I := 1 to High(FFireTargets) do
  begin
    J := I;
    while (J > 0) and (TargetDist(J) < TargetDist(J - 1)) do
    begin
      Tmp := FFireTargets[J];
      FFireTargets[J] := FFireTargets[J - 1];
      FFireTargets[J - 1] := Tmp;
      Dec(J);
    end;
  end;
  FFireIndex := 0;
  if (PrevTarget >= 0) then
    for I := 0 to High(FFireTargets) do
      if (FFireTargets[I] = PrevTarget) then
      begin
        FFireIndex := I;
        Break;
      end;
  FFireMode := True;
end;

function TPlayer.FireRange: UInt;
begin
  Result := Math.Min(Self.Vision, 4 + ((FBowLevel + 1) div 2) +
    Talents.GetLevel(tlLong_Range) + Talents.GetLevel(tlElven_Marksman));
end;

function TPlayer.RangedMinDamage: UInt;
begin
  Result := EnsureRange(FBowMinDamage + Attributes.Attrib[atDex].Value div 5,
    1, UIntMax - 1);
end;

function TPlayer.RangedMaxDamage: UInt;
begin
  Result := EnsureRange(FBowMaxDamage + Attributes.Attrib[atDex].Value div 3,
    2, UIntMax);
end;

function TPlayer.CanRangedAttack: boolean;
begin
  Result := Self.CanFire and (((FWeaponSkill = skWand) and Self.HasCharges) or
    (Self.HasQuiver and not Self.IsQuiverBroken and Self.HasArrows));
end;

procedure TPlayer.FireModeExit;
begin
  FFireMode := False;
  FFireIndex := -1;
  SetLength(FFireTargets, 0);
end;

procedure TPlayer.FireModeSwitch(ADir: Int);
begin
  if not FFireMode or (Length(FFireTargets) = 0) then
    Exit;
  FFireIndex := (FFireIndex + ADir + Length(FFireTargets)) mod Length(FFireTargets);
end;

function TPlayer.FireModeTarget: Int;
begin
  if FFireMode and (FFireIndex >= 0) and (FFireIndex < Length(FFireTargets)) then
    Result := FFireTargets[FFireIndex]
  else
    Result := -1;
end;

procedure TPlayer.AutoPickup;
var
  Index, FCount: Int;
  ItemType: TItemType;
  FItem: Item;
begin
  if Self.IsDead then
    Exit;
  FCount := Items_Dungeon_GetMapCountXY(Ord(Map.Current), X, Y).InRange(ItemMax);
  for Index := FCount - 1 downto 0 do
  begin
    FItem := Items_Dungeon_GetMapItemXY(Ord(Map.Current), Index, X, Y);
    ItemType := ItemBase[TItemEnum(FItem.ItemID)].ItemType;
    if (ItemType in AutoPickupItems) then
    begin
      if ((ItemType in CoinTypeItems) and not Game.GetOption(apCoin)) then
        Exit;
      if ((ItemType in FoodTypeItems) and not Game.GetOption(apFood)) then
        Exit;
      if ((ItemType in PlantTypeItems) and not Game.GetOption(apPlant)) then
        Exit;
      if ((ItemType in PotionTypeItems) and not Game.GetOption(apPotion)) then
        Exit;
      if ((ItemType in MagicTypeItems) and not Game.GetOption(apMagic)) then
        Exit;
      if ((ItemType in FlaskTypeItems) and not Game.GetOption(apFlask)) then
        Exit;
      if ((ItemType in ScrollTypeItems) and not Game.GetOption(apScroll)) then
        Exit;
      if ((ItemType in RuneTypeItems) and not Game.GetOption(apRune)) then
        Exit;
      if ((ItemType in BookTypeItems) and not Game.GetOption(apBook)) then
        Exit;
      if ((ItemType in GemTypeItems) and not Game.GetOption(apGem)) then
        Exit;
      if ((ItemType in KeyTypeItems) and not Game.GetOption(apKey)) then
        Exit;
      if ((ItemType in ArrowTypeItems) and not Game.GetOption(apArrow)) then
        Exit;
      Items.AddItemToInv(Index, True);
      Wait;
    end;
  end;
end;

procedure TPlayer.Calc;
var
  FAttrib: array [TAttribEnum] of UInt;
  Attrib: TAttribEnum;
  I, FCount: Int;
  ID: TItemEnum;
  FItem: Item;

  procedure AddAttrib(const AAttrib: TAttribEnum; const Value: UInt);
  begin
    FAttrib[AAttrib] := FAttrib[AAttrib] + Value;
  end;

  procedure ClearAttrib();
  var
    I: TAttribEnum;
  begin
    for I := Low(TAttribEnum) to High(TAttribEnum) do
      FAttrib[I] := 0;
  end;

  function GetSkill(const Value: TItemType): TSkillEnum;
  begin
    case Value of
      itBlade:
        Result := skBlade;
      itAxe:
        Result := skAxe;
      itSpear:
        Result := skSpear;
      itMace:
        Result := skMace;
      itStaff:
        Result := skStaff;
      itWand:
        Result := skWand;
      itDagger:
        Result := skDagger;
      itBow:
        Result := skBow;
      else
        Result := skNone;
    end;
  end;

  procedure LoAttrib(Attrib: TAttribEnum);
  begin
    Attributes.SetValue(Attrib, Attributes.Attrib[Attrib].Value div 2);
  end;

  procedure HiAttrib(Attrib: TAttribEnum);
  begin
    Attributes.SetValue(Attrib, Attributes.Attrib[Attrib].Value * 2);
  end;

begin
  Light := 0;
  ClearAttrib();
  FCount := Items_Inventory_GetCount().InRange(ItemMax);
  for I := 0 to FCount - 1 do
  begin
    FItem := Items_Inventory_GetItem(I);
    if (FItem.Equipment > 0) then
    begin
      if (FItem.Identify = 0) then
        Continue;
      ID := TItemEnum(FItem.ItemID);
      AddAttrib(atDef, FItem.Defense);
      AddAttrib(atMinDamage, FItem.MinDamage);
      AddAttrib(atMaxDamage, FItem.MaxDamage);
      if (FItem.Bonus[0] > 0) then
      begin
        AddAttrib(atMaxLife, Items.GetBonus(FItem, btLife));
        AddAttrib(atMaxMana, Items.GetBonus(FItem, btMana));
        AddAttrib(atVision, Items.GetBonus(FItem, btVis));
        AddAttrib(atExtraGold, Items.GetBonus(FItem, btExtraGold));
      end;
      if (FItem.Bonus[1] > 0) then
      begin
        AddAttrib(atStr, Items.GetBonus(FItem, btStr));
        AddAttrib(atDex, Items.GetBonus(FItem, btDex));
        AddAttrib(atWil, Items.GetBonus(FItem, btWil));
        AddAttrib(atPer, Items.GetBonus(FItem, btPer));
      end;
      if (FItem.Bonus[2] > 0) then
      begin
        AddAttrib(atReLife, Items.GetBonus(FItem, btReLife));
        AddAttrib(atReMana, Items.GetBonus(FItem, btReMana));
        AddAttrib(atLifeAfEachKill, Items.GetBonus(FItem, btLifeAfEachKill));
        AddAttrib(atManaAfEachKill, Items.GetBonus(FItem, btManaAfEachKill));
      end;
      if (ItemBase[ID].SlotType = stMainHand) or
        (ItemBase[ID].SlotType = stRanged) then
        FWeaponSkill := GetSkill(ItemBase[ID].ItemType);
      if (ItemBase[ID].SlotType = stRanged) then
      begin
        FBowLevel := FItem.Level;
        FBowMinDamage := FItem.MinDamage;
        FBowMaxDamage := FItem.MaxDamage;
      end;
      if (ItemBase[ID].SlotType = stTorch) then
      begin
        Light := Light + FItem.Value;
        FItem.Value := FItem.Value - 1;
        Items_Inventory_SetItem(I, FItem);
        if (FItem.Value <= 0) then
        begin
          Items_Inventory_DeleteItem(I, FItem);
          Light := 0;
        end;
      end;
    end;
  end;

  Gold := Items_Inventory_GetItemAmount(Ord(itmGold));

  Attributes.SetValue(atStr, EnsureRange(Round(Skills.Skill[skAthletics].Value *
    1.2) + Round(Skills.Skill[skToughness].Value * 0.2) + FAttrib[atStr] +
    Attributes.Attrib[atStr].Prm, 1, AttribMax));
  Attributes.SetValue(atDex,
    EnsureRange(Round(Skills.Skill[skDodge].Value * 1.4) + FAttrib[atDex] +
    Attributes.Attrib[atDex].Prm, 1, AttribMax));
  Attributes.SetValue(atWil,
    EnsureRange(Round(Skills.Skill[skConcentration].Value * 1.4) +
    FAttrib[atWil] + Attributes.Attrib[atWil].Prm, 1, AttribMax));
  Attributes.SetValue(atPer, EnsureRange(Round(Skills.Skill[skToughness].Value *
    0.3) + Round(Skills.Skill[skAwareness].Value * 1.4) + FAttrib[atPer] +
    Attributes.Attrib[atPer].Prm, 1, AttribMax));

  if (Abilities.IsAbility(abBerserk)) then
  begin
    HiAttrib(atStr);
    HiAttrib(atDex);
  end;
  if (Abilities.IsAbility(abWeak)) then
  begin
    LoAttrib(atStr);
    LoAttrib(atDex);
  end;
  if Abilities.IsAbility(abAfraid) then
    LoAttrib(atWil);
  if Abilities.IsAbility(abDrunk) then
    LoAttrib(atPer);
  // DV
  Attributes.SetValue(atDV,
    Game.EnsureRange(Round(Attributes.Attrib[atDex].Value * (DVMax / AttribMax)) +
    Attributes.Attrib[atDV].Prm, DVMax));
  // PV
  Attributes.SetValue(atPV,
    Game.EnsureRange(Round(Skills.Skill[skToughness].Value / 1.4) -
    4 + FAttrib[atDef] + Attributes.Attrib[atPV].Prm, PVMax));
  if Abilities.IsAbility(abArmor_Reduction) then
    LoAttrib(atPV);
  // Life
  Attributes.SetValue(atMaxLife, Round(Attributes.Attrib[atStr].Value * 3.6) +
    Round(Attributes.Attrib[atDex].Value * 2.3) + FAttrib[atMaxLife] +
    Attributes.Attrib[atMaxLife].Prm);
  // Mana
  Attributes.SetValue(atMaxMana, Round(Attributes.Attrib[atWil].Value * 4.2) +
    Round(Attributes.Attrib[atDex].Value * 0.4) + FAttrib[atMaxMana] +
    Attributes.Attrib[atMaxMana].Prm);
  // Light
  if Abilities.IsAbility(abLight) then
    Light := Light + Abilities.Ability[abLight];
  // Vision
  Attributes.SetValue(atVision, Round(Attributes.Attrib[atPer].Value / 8.3) +
    FAttrib[atVision] + Light);

  Attributes.SetValue(atExtraGold, FAttrib[atExtraGold].InRange(ExtraGoldMax));
  Self.SetDamage(EnsureRange(FAttrib[atMinDamage] + Attributes.Attrib[atStr]
    .Value div 3, 1, UIntMax - 1),
    EnsureRange(FAttrib[atMaxDamage] + Attributes.Attrib[atStr].Value div
    2, 2, UIntMax));
  for Attrib := AttrLow to AttrHigh do
    Attributes.SetValue(Attrib, FAttrib[Attrib]);

  // if Abilities.IsAbility(abWeightless) then;
  // Your pack seems featherweight! -- Your pack seems much heavier!
end;

procedure TPlayer.Clear();
var
  PlayerName: string;
begin
  inherited Clear();
  Skills.Clear();
  Spellbook.Clear();
  Items_Inventory_Clear();
  Self.Empty;
  Attributes.SetValue(atSat, SatiatedMax);
  Gold := 0;
  MaxMap := 0;
  PlayerName := Trim(Terminal_Get('ini.player.name'));
  if (PlayerName = '') then
    Name := 'PLAYER'
  else
    Name := PlayerName;
  FWeaponSkill := skNone;
  FBowLevel := 0;
  FBowMinDamage := 0;
  FBowMaxDamage := 0;
  FFireMode := False;
  FFireIndex := -1;
  SetLength(FFireTargets, 0);
  Attributes.SetValue(atLev, 1);
  GenerateBackground();
  Calc();
  Fill();
end;

procedure TPlayer.CraftItem(Index: Int);
var
  FItem: Item;
begin
  FItem := Items_Inventory_GetItem(Index);
  if ((FItem.Stack > 1) or (FItem.Amount > 1) or (FItem.Identify > -1)) then
    Exit;
  if (ItemBase[TItemEnum(FItem.ItemID)].ItemType in SmithTypeItems) then
  begin
    FItem.Identify := Items.Index;
    Affixes.DoSuffix(FItem);
    if (Items_Inventory_SetItem(Index, FItem) > 0) then
    begin
      MsgLog.Add(Format('You crafted %s.', [Items.GetNameThe(FItem)]));
      Statictics.Inc(stItCrafted);
      Skills.DoSkill(skEnchant_Item, FItem.Level);
      Scenes.SetScene(scInv);
    end;
    Self.Calc;
  end;
end;

constructor TPlayer.Create;
begin
  inherited;
  FStatistics := TStatistics.Create;
  Talents := TTalents.Create;
  Skills := TSkills.Create;
  Self.Clear;
end;

procedure TPlayer.Defeat(AKiller: string = '');
begin
  Killer := AKiller;
  MsgLog.Add(Terminal.Colorize('You die...', 'Light Red'));
  MsgLog.Add(Terminal.Colorize('Better luck next time!', 'Light Yellow'));
  if (Game.Difficulty < dfHard) then
    MsgLog.Add(Format('Press %s to try again...', [UI.KeyToStr('SPACE')]))
  else
    MsgLog.Add(Format('Press %s to exit...', [UI.KeyToStr('SPACE')]));
  Game.Screenshot := Terminal.GetTextScreenshot();
end;

destructor TPlayer.Destroy;
begin
  FreeAndNil(FSkills);
  FreeAndNil(FTalents);
  FreeAndNil(FStatistics);
  inherited;
end;

procedure TPlayer.Dialog(AMob: TMob);
begin
  Game.Timer := UIntMax;
  NPCName := Mobs.Name[TMobEnum(AMob.ID)];
  NPCType := MobBase[TMobEnum(AMob.ID)].NPCType;
  Scenes.SetScene(scDialog);
end;

procedure TPlayer.GenNPCText;
var
  S: string;
begin
  case Math.RandomRange(0, 3) of
    0:
      S := 'What can I do for you?';
    1:
      S := 'What can I get you today?';
    else
      S := 'Good day!';
  end;
  MsgLog.Add(Format('%s says: "%s"', [NPCName, S]));
end;

function TPlayer.GetVision: UInt;
begin
  Result := Game.EnsureRange((Attributes.Attrib[atVision].Value -
    Abilities.Ability[abBlinded]) + 3, VisionMax);
  Result := Math.IfThen(Calendar.IsDay, Result, Result div 2);
end;

function TPlayer.GetSatiationStr: string;
begin
  Result := '';
  case Attributes.Attrib[atSat].Value of
    0 .. StarvingMax:
      Result := 'Starving';
    StarvingMax + 1 .. 1500:
      Result := 'Near starving';
    1501 .. 2000:
      Result := 'Very hungry';
    2001 .. 2500:
      Result := 'Hungry';
    SatiatedMax + 1 .. 10000:
      Result := 'Full';
    10001 .. 11000:
      Result := 'Very full';
    11001 .. EngorgedMax:
      Result := 'Engorged';
  end;
  if Mode.Wizard then
  begin
    if (Result = '') then
      Result := 'Satiated';
    Result := Result + Format(' (%d)', [Attributes.Attrib[atSat].Value]);
  end;
  case Attributes.Attrib[atSat].Value of
    0 .. StarvingMax:
      Result := Terminal.Colorize(Result, 'Light Red');
    StarvingMax + 1 .. SatiatedMax:
      Result := Terminal.Colorize(Result, 'Light Yellow');
    else
      Result := Terminal.Colorize(Result, 'Light Green');
  end;
end;

procedure TPlayer.Move(Dir: TDirectionEnum);
var
  FX, FY: UInt;
begin
  if Look then
  begin
    if Map.InMap(LX + Direction[Dir].X, LY + Direction[Dir].Y) and
      ((Map.InView(LX + Direction[Dir].X, LY + Direction[Dir].Y) and
      not Map.GetFog(LX + Direction[Dir].X, LY + Direction[Dir].Y)) or
      Mode.Wizard) then
    begin
      LX := Map.EnsureRange(Math.EnsureRange(LX + Direction[Dir].X,
        X - (View.Width div 2), X + (View.Width div 2 - 1)));
      LY := Map.EnsureRange(Math.EnsureRange(LY + Direction[Dir].Y,
        Y - (View.Height div 2), Y + (View.Height div 2 - 1)));
    end;
  end
  else
  begin
    if IsDead then
      Exit;
    FX := Map.EnsureRange(X + Direction[Dir].X);
    FY := Map.EnsureRange(Y + Direction[Dir].Y);
    if (Map.GetTileEnum(FX, FY, Map.Current) in StopTiles) and not Mode.Wizard then
      Exit;
    // Stunned or burning
    if (Self.Abilities.IsAbility(abStunned) or
      Self.Abilities.IsAbility(abBurning)) then
    begin
      AddTurn;
      Exit;
    end;

    if not Mobs.GetFreeTile(FX, FY) then
    begin
      Self.Attack(Mobs.GetIndex(FX, FY));
    end
    else
    begin
      X := FX;
      Y := FY;
      if ((Direction[Dir].X <> 0) or (Direction[Dir].Y <> 0)) then
      begin
        SatPerTurn := 2;
        AutoPickup;
      end;
      AddTurn;
    end;
  end;
end;

procedure TPlayer.Use(Index: Int);
var
  FItem: Item;
  I: TItemEnum;
  T: TItemType;
begin
  if IsDead then
    Exit;
  FItem := Items_Inventory_GetItem(Index);
  // Unidentified
  if FItem.Identify = 0 then
  begin
    MsgLog.Add('You can not use this yet (unidentified)!');
    Self.Calc;
    Exit;
  end;
  // Need level
  if (Attributes.Attrib[atLev].Value < FItem.Level) and not Mode.Wizard then
  begin
    MsgLog.Add(Format('You can not use this yet (need level %d)!', [FItem.Level]));
    Self.Calc;
    Exit;
  end;
  I := TItemEnum(FItem.ItemID);
  T := ItemBase[I].ItemType;
  // No mana
  if (Player.Attributes.Attrib[atMana].Value < ItemBase[I].ManaCost) then
  begin
    MsgLog.Add(Format('You need more mana!', [FItem.Level]));
    Self.Calc;
    Exit;
  end;
  if (T in NotEquipTypeItems) then
  begin
    if (T in UseTypeItems) then
    begin
      if not (T in RuneTypeItems) then
        FItem.Amount := FItem.Amount - 1;
      if (T in PotionTypeItems) then
      begin
        MsgLog.Add(Format('You drink %s.', [Items.GetNameThe(FItem)]));
        Statictics.Inc(stPotDrunk);
      end;
      if (T in RuneTypeItems + BookTypeItems + ScrollTypeItems) then
      begin
        MsgLog.Add(Format('You read %s.', [Items.GetNameThe(FItem)]));
      end;
      if (T in FoodTypeItems + PlantTypeItems) then
      begin
        MsgLog.Add(Format('You ate %s.', [Items.GetNameThe(FItem)]));
        Statictics.Inc(stFdEat);
      end;
      if (T in MagicTypeItems + FlaskTypeItems) then
      begin
        MsgLog.Add(Format('You use %s.', [Items.GetNameThe(FItem)]));
        Statictics.Inc(stItUsed);
      end;

      if (T in ScrollTypeItems) then
      begin
        Statictics.Inc(stScrRead);
      end;
      if not (T in RuneTypeItems) then
      begin
        Items_Inventory_SetItem(Index, FItem);
      end;
      if (T in ScrollTypeItems + RuneTypeItems) then
      begin
        if (Attributes.Attrib[atMana].Value >= ItemBase[I].ManaCost) then
        begin
          Skills.DoSkill(skConcentration);
          Attributes.Modify(atMana, -ItemBase[I].ManaCost);
          Statictics.Inc(stSpCast);
        end
        else
        begin
          MsgLog.Add('You need more mana!');
          Self.Calc;
          Wait;
          Exit;
        end;
      end;
      DoEffects(FItem.Effects, FItem.Value);
      Self.Calc;
      Wait;
    end;
  end
  else
  begin
    // Equip or unequip an item
    case FItem.Equipment of
      0:
        Self.Equip(Index);
      1:
        Self.UnEquip(Index);
    end;
  end;
  // MsgLog.Add(Format(_('You don''t know how to use %s.'), [The]));
end;

procedure TPlayer.Equip(Index: Int);
var
  FItem: Item;
  I: Int;
begin
  // Need level
  FItem := Items_Inventory_GetItem(Index);
  if (Attributes.Attrib[atLev].Value < FItem.Level) and not Mode.Wizard then
  begin
    MsgLog.Add(Format('You can not use this yet (need level %d)!', [FItem.Level]));
    Self.Calc;
    Exit;
  end;
  if (FItem.Identify = 0) and not Mode.Wizard then
  begin
    MsgLog.Add('You can not use this yet (unidentified item)!');
    Self.Calc;
    Exit;
  end;
  // Replace
  I := Items_Inventory_EquipItem(Index);
  if (I > -1) then
    UnEquip(I);
  // Equip
  MsgLog.Add(Format('You equip %s.', [Items.GetNameThe(FItem)]));
  Self.Calc;
  Wait;
end;

procedure TPlayer.UnEquip(Index: Int);
var
  FItem: Item;
begin
  if (Items_Inventory_UnEquipItem(Index) > 0) then
  begin
    FItem := Items_Inventory_GetItem(Index);
    MsgLog.Add(Format('You unequip %s.', [Items.GetNameThe(FItem)]));
    Self.Calc;
    Wait;
  end;
end;

procedure TPlayer.Sell(Index: Int);
var
  Value: Int;
  FItem: Item;
begin
  FItem := Items_Inventory_GetItem(Index);
  if ((FItem.Equipment > 0) or Items.ChItem(FItem)) then
    Exit;
  if (Items_Inventory_DeleteItem(Index, FItem) > 0) then
  begin
    Value := FItem.Price div 4;
    Items.AddItemToInv(itmGold, Value);
    MsgLog.Add(Format('You sold %s (+%d gold).', [Items.GetNameThe(FItem), Value]));
  end;
  Self.Calc;
  Wait;
end;

procedure TPlayer.BreakItem();
begin
  case Math.RandomRange(0, 7) of
    0:
      Player.BreakItem(stHead);
    1:
      Player.BreakItem(stTorso);
    2:
      Player.BreakItem(stHands);
    3:
      Player.BreakItem(stFeet);
    4:
      Player.BreakItem(stOffHand);
    5:
      Player.BreakItem(stNeck);
    else
      Player.BreakItem(stFinger);
  end;
end;

procedure TPlayer.Buy(Index: Int);
var
  FItem: Item;
begin
  FItem := Shops.Shop[Shops.Current].GetItem(Index);
  if (Items_Inventory_DeleteItemAmount(Ord(itmGold), FItem.Price) > 0) then
  begin
    MsgLog.Add(Format('You bought %s (-%d gold).',
      [Items.GetNameThe(FItem), FItem.Price]));
    Items_Inventory_AppendItem(FItem);
    Self.Calc;
    // The %s just frowns. Maybe you'll return when you have enough gold?
  end
  else
    MsgLog.Add('You need more gold.');
end;

procedure TPlayer.ReceiveHealing;
var
  Cost: UInt;
begin
  Cost := Round((Attributes.Attrib[atMaxLife].Value - Attributes.Attrib[atLife]
    .Value) * 1.6);
  if (Self.Gold >= Cost) then
  begin
    if (Items_Inventory_DeleteItemAmount(Ord(itmGold), Cost) > 0) then
    begin
      Attributes.SetValue(atLife, atMaxLife);
      MsgLog.Add(Format('You feel better (-%d gold).', [Cost]));
    end;
  end
  else
    MsgLog.Add('You need more gold.');
  Self.Calc;
end;

function TPlayer.GetArrowsToBuy: Int;
var
  QIndex: Int;
  FItem: Item;
begin
  Result := 0;
  QIndex := Self.GetQuiverIndex;
  if (QIndex < 0) then
    Exit;
  FItem := Items_Inventory_GetItem(QIndex);
  if (FItem.Durability = 0) then
    Exit;
  Result := Int(ItemBase[TItemEnum(FItem.ItemID)].Value) +
    Int(Items.GetBonus(FItem, btQuiverCap)) - Int(FItem.Value);
end;

procedure TPlayer.BuyArrows;
var
  QIndex: Int;
  FItem: Item;
  Cost: UInt;
begin
  if not Self.HasQuiver then
  begin
    MsgLog.Add('You need a quiver equipped to do that.');
    Exit;
  end;
  if Self.IsQuiverBroken then
  begin
    MsgLog.Add('Your quiver is broken and can''t hold arrows.');
    Exit;
  end;
  QIndex := Self.GetQuiverIndex;
  FItem := Items_Inventory_GetItem(QIndex);
  Cost := (ItemBase[TItemEnum(FItem.ItemID)].Value +
    Items.GetBonus(FItem, btQuiverCap)) - FItem.Value;
  if (Cost = 0) then
  begin
    MsgLog.Add('Your quiver is already full.');
    Exit;
  end;
  if (Self.Gold >= Cost) then
  begin
    if (Items_Inventory_DeleteItemAmount(Ord(itmGold), Cost) > 0) then
    begin
      FItem.Value := ItemBase[TItemEnum(FItem.ItemID)].Value +
        Items.GetBonus(FItem, btQuiverCap);
      Items_Inventory_SetItem(QIndex, FItem);
      MsgLog.Add(Format('You bought %d arrows (-%d gold).', [Cost, Cost]));
    end;
  end
  else
    MsgLog.Add('You need more gold.');
  Self.Calc;
end;

procedure TPlayer.IdentAllItems;
var
  FItem: Item;
  FCount, I: Int;
  F: boolean;
begin
  F := False;
  FCount := Items_Inventory_GetCount().InRange(ItemMax);
  for I := 0 to FCount - 1 do
  begin
    FItem := Items_Inventory_GetItem(I);
    if (FItem.Identify = 0) then
    begin
      Self.IdentItem(I);
      F := True;
    end;
  end;
  if F then
    Self.Calc;
end;

procedure TPlayer.IdentItem(Index: Int);
var
  FItem: Item;
begin
  FItem := Items_Inventory_GetItem(Index);
  if ((FItem.Stack > 1) or (FItem.Amount > 1)) then
    Exit;
  if (Items.Identify(FItem) and (FItem.Identify > 0) and
    (Items_Inventory_SetItem(Index, FItem) > 0)) then
  begin
    MsgLog.Add(Format('You identified %s.', [Items.GetNameThe(FItem)]));
    Statictics.Inc(stItIdent);
    Scenes.SetScene(scInv);
  end;
  Self.Calc;
  Wait;
end;

procedure TPlayer.RechargeWand(Index: Int);
var
  FItem: Item;
  MaxCharges: UInt;
begin
  FItem := Items_Inventory_GetItem(Index);
  if (ItemBase[TItemEnum(FItem.ItemID)].ItemType <> itWand) then
    Exit;
  MaxCharges := ItemBase[TItemEnum(FItem.ItemID)].Value +
    Items.GetBonus(FItem, btWandCap);
  if (FItem.Value >= MaxCharges) then
  begin
    MsgLog.Add(Format('%s is already fully charged.',
      [GetCapit(Items.GetNameThe(FItem))]));
    Exit;
  end;
  FItem.Value := MaxCharges;
  Items_Inventory_SetItem(Index, FItem);
  MsgLog.Add(Format('You recharge %s.', [Items.GetNameThe(FItem)]));
  Scenes.SetScene(scInv);
  Self.Calc;
end;

procedure TPlayer.RepairItem(Index: Int);
var
  RepairCost: UInt;
  FItem: Item;
begin
  FItem := Items_Inventory_GetItem(Index);
  if ((FItem.Stack > 1) or (FItem.Identify = 0) or (FItem.Amount > 1)) then
    Exit;
  // Oil
  if (Items.Index > 0) then
  begin
    { case Items.CurrentItem.Effect of
      // Cursed
      - 1:
      Dec(FItem.MaxDurability);
      // Blessed
      1:
      Inc(FItem.MaxDurability);
      end; }
    FItem.Durability := Math.EnsureRange(FItem.Durability + Items.Index, 1,
      FItem.MaxDurability);
    if (Items_Inventory_SetItem(Index, FItem) > 0) then
    begin
      MsgLog.Add(Format('You repaired %s.', [Items.GetNameThe(FItem)]));
      Statictics.Inc(stItRep);
      Calc;
    end;
    Scenes.SetScene(scInv);
    Exit;
  end;
  // Smith
  RepairCost := (FItem.MaxDurability - FItem.Durability) * 10;
  if (RepairCost > 0) then
  begin
    if (Gold < RepairCost) then
    begin
      MsgLog.Add('You need more gold.');
      Exit;
    end;
    if (FItem.MaxDurability > 0) then
    begin
      if (Game.Difficulty > dfEasy) then
      begin
        if (Game.Difficulty = dfHell) then
          FItem.MaxDurability :=
            Math.EnsureRange(FItem.MaxDurability - Math.RandomRange(2, 4), 0,
            FItem.MaxDurability)
        else
          Dec(FItem.MaxDurability);
        if (FItem.MaxDurability = 0) then
        begin
          RnItem(FItem, Index);
          Exit;
        end;
      end;
      FItem.Durability := FItem.MaxDurability;
      if ((Items_Inventory_DeleteItemAmount(Ord(itmGold), RepairCost) > 0) and
        (Items_Inventory_SetItem(Index, FItem) > 0)) then
        MsgLog.Add(Format('You repaired %s (-%d gold).',
          [Items.GetNameThe(FItem), RepairCost]));
      Statictics.Inc(stItRep);
    end;
  end;
  Self.Calc;
end;

procedure TPlayer.BreakItem(Index: Int; Value: UInt = 1);
var
  FItem: Item;
begin
  FItem := Items_Inventory_GetItem(Index);
  if ((FItem.Stack > 1) or (FItem.Amount > 1)) then
    Exit;
  FItem.Durability := Game.EnsureRange(FItem.Durability - Value, UIntMax);
  if ((FItem.Durability > 0) and (FItem.Durability <
    (FItem.MaxDurability div 4))) then
    MsgLog.Add(Terminal.Colorize(
      Format('%s soon will be totally broken (%d/%d).',
      [GetCapit(Items.GetNameThe(FItem)), FItem.Durability, FItem.MaxDurability]),
      clAlarm));
  Items_Inventory_SetItem(Index, FItem);
  RnItem(FItem, Index);
  Self.Calc;
end;

procedure TPlayer.BreakItem(ASlot: TSlotType; Value: UInt = 1);
var
  Index: Int;
begin
  Index := Self.GetEquippedIndex(ASlot);
  if (Index >= 0) then
    BreakItem(Index, Value);
end;

procedure TPlayer.Drop(Index: Int);
var
  AItem: Item;

  procedure DeleteItem;
  begin
    if (Items_Inventory_DeleteItem(Index, AItem) > 0) then
    begin
      AItem.X := X;
      AItem.Y := Y;
      AItem.Equipment := 0;
      AItem.MapID := Ord(Map.Current);
      Items.AddItemToDungeon(AItem);
      if IsOnStash then
        MsgLog.Add(Format('You put %s into the stash.', [Items.GetNameThe(AItem)]))
      else
        MsgLog.Add(Format('You drop %s.', [Items.GetNameThe(AItem)]));
      Wait();
    end;
  end;

begin
  if IsDead then
    Exit;
  AItem := Items_Inventory_GetItem(Index);
  if (AItem.Equipment > 0) then
    Exit;
  if not ((AItem.Stack > 1) and (AItem.Amount > 1)) then
    DeleteItem()
  else
    SetAmountScene(True, Index, 1);
  Self.Calc();
end;

procedure TPlayer.DropAmount(Index: Int);
var
  FItem: Item;
begin
  FItem := Items_Inventory_GetItem(Index);
  FItem.Amount := FItem.Amount - ItemAmount;
  Items_Inventory_SetItem(Index, FItem);
  FItem.X := X;
  FItem.Y := Y;
  FItem.Equipment := 0;
  FItem.MapID := Ord(Map.Current);
  FItem.Amount := ItemAmount;
  Items.AddItemToDungeon(FItem);
  if IsOnStash then
  begin
    if (FItem.Amount > 1) then
      MsgLog.Add(Format('You put %s (%dx) into the stash.',
        [Items.GetNameThe(FItem), FItem.Amount]))
    else
      MsgLog.Add(Format('You put %s into the stash.', [Items.GetNameThe(FItem)]));
    Scenes.SetScene(scStore);
  end
  else
  begin
    if (FItem.Amount > 1) then
      MsgLog.Add(Format('You drop %s (%dx).', [Items.GetNameThe(FItem),
        FItem.Amount]))
    else
      MsgLog.Add(Format('You drop %s.', [Items.GetNameThe(FItem)]));
    Scenes.SetScene(scDrop);
  end;
  Wait();
end;

procedure TPlayer.PickUpArrows(const MapID, Index: Int; AItem: Item);
var
  QIndex: Int;
  QItem: Item;
  Capacity, Space, Picked, Remaining: Int;
  GroundItem: Item;
begin
  QIndex := Self.GetQuiverIndex;
  if (QIndex < 0) then
  begin
    MsgLog.Add('You need a quiver equipped to pick up arrows.');
    Exit;
  end;
  QItem := Items_Inventory_GetItem(QIndex);
  Capacity := ItemBase[TItemEnum(QItem.ItemID)].Value +
    Items.GetBonus(QItem, btQuiverCap);
  Space := Math.Max(0, Capacity - QItem.Value);
  if (Space <= 0) then
  begin
    MsgLog.Add('Your quiver is full.');
    Exit;
  end;
  Picked := Math.Min(Space, AItem.Amount);
  QItem.Value := QItem.Value + Picked;
  Items_Inventory_SetItem(QIndex, QItem);
  Remaining := AItem.Amount - Picked;
  if (Remaining > 0) then
  begin
    GroundItem := AItem;
    GroundItem.Amount := Remaining;
    Items_Dungeon_SetMapItemXY(MapID, Index, X, Y, GroundItem);
    MsgLog.Add(Format('You picked up %d arrows (your quiver is full).',
      [Picked]));
  end
  else
  begin
    Items_Dungeon_DeleteMapItemXY(MapID, Index, X, Y, GroundItem);
    if (Picked = 1) then
      MsgLog.Add('You picked up an arrow.')
    else
      MsgLog.Add(Format('You picked up %d arrows.', [Picked]));
  end;
  Self.Wait;
  Self.Calc;
end;

procedure TPlayer.PickUp();
var
  FCount: Int;
begin
  Statictics.Inc(stFound);
  /// / Your backpack is full!
  FCount := Items_Dungeon_GetMapCountXY(Ord(Map.Current), X, Y);
  if Player.IsOnStash then
  begin
    // Stash scene
    Game.Timer := UIntMax;
    Scenes.SetScene(scStash);
  end
  else
  if (FCount > 0) then
  begin
    if (FCount = 1) then
    begin
      // Pickup an item
      Items.AddItemToInv(0);
    end
    else
    begin
      // Items scene
      Game.Timer := UIntMax;
      Scenes.SetScene(scItems);
    end;
  end
  else
    MsgLog.Add('There is nothing here to pick up.');
end;

procedure TPlayer.PickUpAmount(Index: Int);
var
  FItem: Item;
begin
  FItem := Items_Dungeon_GetMapItemXY(Ord(Map.Current), Index, X, Y);
  FItem.Amount := FItem.Amount - ItemAmount;
  Items_Dungeon_SetMapItemXY(Ord(Map.Current), Index, X, Y, FItem);
  FItem.Amount := ItemAmount;
  Items_Inventory_AppendItem(FItem);
  if (FItem.Amount > 1) then
    MsgLog.Add(Format('You picked up %s (%dx).',
      [Items.GetNameThe(FItem), FItem.Amount]))
  else
    MsgLog.Add(Format('You picked up %s.', [Items.GetNameThe(FItem)]));
  Scenes.SetScene(scItems);
  Wait();
end;

procedure TPlayer.Render(AX, AY: UInt);
begin
  if IsDead then
    Terminal.Print(AX + View.Left, AY + View.Top, '%', clCorpse)
  else
  begin
    if Look then
      Terminal.Print(AX + View.Left, AY + View.Top, '@', clPlayer)
    else
      Terminal.Print(AX + View.Left, AY + View.Top, '@', clPlayer, clBkPlayer);
  end;
end;

procedure TPlayer.RenderInfo;
const
  F = '%s %d/%d';
var
  I: TAbilityEnum;
  S: string;
begin
  Terminal.ForegroundColor(clDefault);
  // Info
  Terminal.Print(Status.Left - 1, Status.Top + 1, ' ' + UI.Icon(icLife, 'Life') +
    ' ' + Terminal.Colorize(Format(F, ['Life', Attributes.Attrib[atLife].Value,
    Attributes.Attrib[atMaxLife].Value]), 'Life'));
  Terminal.Print(Status.Left - 1, Status.Top + 2, ' ' + UI.Icon(icMana, 'Mana') +
    ' ' + Terminal.Colorize(Format(F, ['Mana', Self.Attributes.Attrib[atMana].Value,
    Self.Attributes.Attrib[atMaxMana].Value]), 'Mana'));
  // Bars
  UI.Bar(Status.Left, 15, Status.Top + 1, Status.Width - 16,
    Attributes.Attrib[atLife].Value, Attributes.Attrib[atMaxLife].Value, clLife,
    clDarkGray);
  UI.Bar(Status.Left, 15, Status.Top + 2, Status.Width - 16,
    Self.Attributes.Attrib[atMana].Value, Self.Attributes.Attrib[atMaxMana]
    .Value, clMana, clDarkGray);
  case Game.ShowEffects of
    False:
    begin
      Terminal.Print(Status.Left - 1, Status.Top + 3,
        ' ' + Format('%s%d %s%d %s%d-%d %s%d %s',
        [UI.Icon(icFlag), Statictics.Get(stTurn), UI.Icon(icGold),
        Gold, UI.Icon(icSword), GetDamage.Min, GetDamage.Max,
        UI.Icon(icShield), Attributes.Attrib[atPV].Value, GetSatiationStr()]));
      Self.RenderWeather(Status.Left + (Status.Width div 2), Status.Top + 5,
        Status.Width);
    end;
    else
    begin
      S := '';
      for I := Low(TAbilityEnum) to High(TAbilityEnum) do
        if Abilities.IsAbility(I) then
          S := S + Terminal.Colorize(Format(' %s (%d)',
            [Abilities.GetName(I), Abilities.Ability[I]]), Abilities.GetColor(I));
      Terminal.Print(Status.Left, Status.Top + 3, Log.Width, 2, S,
        TK_ALIGN_TOP);
    end;
  end;
end;

procedure TPlayer.RenderWeather(const AX, AY, AWidth: UInt);
var
  SunOrMoonGlyphColor, SunOrMoonGlyph, SunOrMoon, SkyColor, SkyBef, SkyAft: string;
  Left: UInt;

  procedure Add(const ASunOrMoonGlyph, ASunOrMoonGlyphColor, ASkyColor: string);
  begin
    SunOrMoonGlyph := ASunOrMoonGlyph;
    SunOrMoonGlyphColor := ASunOrMoonGlyphColor;
    SkyColor := ASkyColor;
  end;

begin
  if Calendar.IsDay then
    Add(UI.Icon(icSun), 'Light Yellow', 'Lightest Blue')
  else
    Add(UI.Icon(icMoon), 'Light White', 'Darker Gray');
  Left := Round(Calendar.Hour / 24 * AWidth);
  SunOrMoon := Terminal.Colorize(SunOrMoonGlyph, SunOrMoonGlyphColor);
  SkyBef := Terminal.Colorize(StringOfChar('_', Left), SkyColor);
  SkyAft := Terminal.Colorize(StringOfChar('_', AWidth - Left - 1), SkyColor);
  Terminal.Print(AX, AY, SkyBef + SunOrMoon + SkyAft, TK_ALIGN_CENTER);
end;

procedure TPlayer.Empty;
begin
  Killer := '';
  Look := False;
  IsRest := False;
  SatPerTurn := 2;
end;

function TPlayer.SaveCharacterDump(AReason: string): string;
var
  MorgueFileName: string;
  SL: TStringList;

  function GetDateTime(DateSep: char = '.'; TimeSep: char = ':'): string;
  begin
    Result := DateToStr(Date) + '-' + TimeToStr(Time);
    Result := StringReplace(Result, '.', DateSep, [rfReplaceAll]);
    Result := StringReplace(Result, ':', TimeSep, [rfReplaceAll]);
  end;

begin
  Result := '';
  if Mode.Wizard then
    Exit;
  SL := TStringList.Create;
  try
    SL.Append(Format(FT, [Game.GetTitle]));
    SL.Append('');
    SL.Append(GetDateTime);
    SL.Append(Format('%s: %s.',
      ['Difficulty', GetPureText(Game.GetStrDifficulty)]));
    SL.Append('');
    SL.Append(Player.Name);
    SL.Append(AReason);
    if IsDead then
      SL.Append(Format('He scored %d points.', [Statictics.Get(stScore)]))
    else
      SL.Append(Format('He has scored %d points so far.',
        [Statictics.Get(stScore)]));
    SL.Append('');
    SL.Append(Format(FT, ['Statistics']));
    SL.Append(Format('Game time: %d turns.', [Statictics.Get(stTurn)]));
    SL.Append('');
    SL.Append(Format(FT, ['Screenshot']));
    SL.Append(Game.Screenshot);
    SL.Append(Format(FT, ['Defeated foes']));
    SL.Append('');
    SL.Append(Format('Total: %d creatures defeated.', [Statictics.Get(stKills)]));
    SL.Append('');
    SL.Append(Format(FT, ['Last messages']));
    SL.Append('');
    SL.Append(GetPureText(MsgLog.GetLastMsg(10)));
    SL.Append(Format(FT, ['Inventory']));
    SL.Append('');
    SL.Append(GetPureText(Items.GetInventory));
    SL.Append(Format('%s: %d', ['Gold', Gold]));
    ForceDirectories(Utils.GetPath('morgue'));
    MorgueFileName := Format('%s-%s-character-dump.txt',
      [Player.Name, GetDateTime('-', '-')]);
    SL.SaveToFile(Utils.GetPath('morgue') + MorgueFileName
      {$IFNDEF FPC}
      ,
      TEncoding.UTF8
      {$ENDIF}
      );
  finally
    FreeAndNil(SL);
  end;
end;

procedure TPlayer.SetAmountScene(IsDrop: boolean; Index, Amount: Int);
begin
  ItemIsDrop := IsDrop;
  ItemIndex := Index;
  ItemAmount := Amount;
  Scenes.SetScene(scAmount);
end;

procedure TPlayer.Spawn;
begin
  X := Game.Spawn.X;
  Y := Game.Spawn.Y;
  Map.Current := deDark_Wood;
  Self.Abilities.Clear;
  MsgLog.Clear;
  Self.Empty();
  // ShowMessage('');
end;

procedure TPlayer.AddExp(Value: UInt = 1);
begin
  Attributes.Modify(atExp, Value);
  if (Attributes.Attrib[atExp].Value >= LevelExpMax) then
  begin
    Attributes.Modify(atExp, -LevelExpMax);
    Attributes.Modify(atLev, 1);
    // You leveled up! You are now level %d!
    MsgLog.Add(Terminal.Colorize(Format('You advance to level %d!',
      [Attributes.Attrib[atLev].Value]), clAlarm));
    if (Attributes.Attrib[atLev].Value mod 2 = 1) then
    begin
      Talents.IsPoint := True;
      MsgLog.Add(Terminal.Colorize('You gained 1 talent point.', clAlarm));
      Statictics.Inc(stScore);
    end
    else
      Talents.IsPoint := False;
    Statictics.Inc(stTurn, Attributes.Attrib[atLev].Value *
      Attributes.Attrib[atLev].Value);
  end;
end;

procedure TPlayer.Wait;
begin
  if not Map.GetVis(Map.Current) then
  begin
    MsgLog.Add(Terminal.Colorize(Format('You have opened a new territory: %s.',
      [Map.Name]), clAlarm));
    Map.SetVis(Map.Current, True);
    if (Ord(Map.Current) > 0) then
      Statictics.Inc(stScore, Ord(Map.Current) * 15);
    MaxMap := MaxMap + 1;
  end;
  SatPerTurn := 1;
  Move(drOrigin);
end;

procedure TPlayer.Rest(ATurns: UInt);
var
  T: UInt;
begin
  IsRest := True;
  MsgLog.Add(Format('Start rest (%d turns)!', [ATurns]));
  for T := 1 to ATurns do
  begin
    if not IsRest then
      Break;
    Wait();
  end;
  MsgLog.Add(Format('Finish rest (%d turns)!', [T - 1]));
  Abilities.Ability[abWeak] := 0;
  if (Math.RandomRange(0, 9) = 0) then
    Abilities.Ability[abDrunk] := 0;
  IsRest := False;
end;

procedure TPlayer.Start();
begin
  Items.AddItemToInv(itmArcane_Orb, 10);
  Exit;
  // ShowMessage('');
  // Add armors
  if Mode.Wizard then
  begin
    Items.AddItemToInv(itmWinged_Helm, 1, True, False);
    Items.AddItemToInv(itmPlate_Mail, 1, True, False);
    Items.AddItemToInv(itmPlated_Gauntlets, 1, True, False);
    Items.AddItemToInv(itmPlate_Boots, 1, True, False);
  end
  else
  begin
    if (Game.Difficulty < dfHard) then
    begin
      Items.AddItemToInv(itmCap, 1, True, True);
      Items.AddItemToInv(itmQuilted_Armor, 1, True, True);
    end;
    if (Game.Difficulty < dfNormal) then
    begin
      Items.AddItemToInv(itmLeather_Gloves, 1, True, True);
      Items.AddItemToInv(itmShoes, 1, True, True);
    end;
  end;
  { // Add weapon
    if Mode.Wizard then
    begin
    case Math.RandomRange(0, 4) of
    0:
    Items.AddItemToInv(ivTroll_Slayer, 1, True, False);
    1:
    Items.AddItemToInv(ivDemon_Axe, 1, True, False);
    2:
    Items.AddItemToInv(ivHoned_Spear, 1, True, False);
    3:
    Items.AddItemToInv(ivDoom_Hammer, 1, True, False);
    end;
    end
    else
    begin
    case Math.RandomRange(0, 4) of
    0:
    Items.AddItemToInv(ivRusty_Sword, 1, True, True);
    1:
    Items.AddItemToInv(ivHatchet, 1, True, True);
    2:
    Items.AddItemToInv(ivShort_Spear, 1, True, True);
    3:
    Items.AddItemToInv(ivSlag_Hammer, 1, True, True);
    end;
    end; }
  // Add runes, potions and scrolls
  if Mode.Wizard then
  begin
    Items.AddItemToInv(itmRune_of_Full_Healing);
    Items.AddItemToInv(itmPotion_of_Full_Healing, 10);
    Items.AddItemToInv(itmPotion_of_Full_Mana, 10);
    // Items.AddItemToInv(ivAntidote, 10);
    Items.AddItemToInv(itmScroll_of_Town_Portal, 10);
    Items.AddItemToInv(itmScroll_of_Identify, 10);
    Items.AddItemToInv(itmScroll_of_Full_Identify, 5);
    Items.AddItemToInv(itmScroll_of_Bloodlust, 10);
    Items.AddItemToInv(itmScroll_of_Enchant_Item, 10);
  end
  else
  begin
    // Items.AddItemToInv(ivLesser_Healing_Potion, 5);
    // Items.AddItemToInv(ivLesser_Mana_Potion, 5);
    // Items.AddItemToInv(ivAntidote, 3);
    Items.AddItemToInv(itmScroll_of_Town_Portal);
    Items.AddItemToInv(itmScroll_of_Identify);
  end;
  { Items.AddItemToInv(ivMoonstone_Ring);
    Items.AddItemToInv(ivMoonstone_Ring);
    Items.AddItemToInv(ivMoonstone_Ring);
    Items.AddItemToInv(ivMoonstone_Ring);
    Items.AddItemToInv(ivMoonstone_Ring);
    Items.AddItemToInv(ivMoonstone_Ring);
    Items.AddItemToInv(ivFlawed_Diamond);
    Items.AddItemToInv(ivCap, 1, True, True, Ord(of_LifeAfEachKill1));
  }  { // Flasks
    D := Math.IfThen(Mode.Wizard, 3, 3);
    for I := 1 to D do
    begin
    Items.AddItemToInv(ivBasalt_Flask0, 1, False, True);
    end;
    for I := 1 to D do
    begin
    Items.AddItemToInv(ivBasalt_Flask, 1, False, True);
    end;
    for I := 1 to D do
    begin
    Items.AddItemToInv(ivEternal_Flask, 1, False, True);
    end;
    for I := 1 to D do
    begin
    Items.AddItemToInv(ivEternal_Flask2, 1, False, True);
    end; }
end;

procedure TPlayer.StartEquip;
var
  J: TSlotType;
  LGold: UInt;
begin
  // Equipment
  for J := Low(ClassProp[HClass].Item) to High(ClassProp[HClass].Item) do
    if (ClassProp[HClass].Item[J] <> TItemEnum.itmNone) then
      Items.AddItemToInv(ClassProp[HClass].Item[J], 1, True, True);
  // Add foods
  Items.AddItemToInv(itmBread_Ration, IfThen(Mode.Wizard, 9, 5));
  Items.AddItemToInv(itmTorch, IfThen(Mode.Wizard, 9, 3));
  // Add coins
  LGold := GetStartGold();
  Items.AddItemToInv(itmGold, IfThen(Mode.Wizard, RandomRange(3333, 9999), LGold));
  // Calc
  Calc();
  Fill();
end;

procedure TPlayer.StartSkills;
var
  I: TClassSkillEnum;
begin
  // Skills
  for I := Low(TClassSkillEnum) to High(TClassSkillEnum) do
    Skills.Modify(ClassProp[Player.HClass].Skill[I],
      Trollhunter.Player.Classes.Classes.GetSkillBeginValue(I));
  // Calc
  Calc();
  Fill();
end;

function TPlayer.GetStartGold: UInt;
begin
  case Game.Difficulty of
    dfEasy:
      Result := 400;
    dfNormal:
      Result := StartGold;
    dfHard:
      Result := Round(StartGold * 0.5);
    dfHell:
      Result := 50;
    else
      Result := StartGold;
  end;
end;

function TPlayer.IsOnStash: boolean;
begin
  Result := (Map.GetTileEnum(X, Y, Map.Current) = teStash);
end;

procedure TPlayer.DoEffects(const Effects: TEffects; const Value: UInt = 0;
  const Multiplier: UInt = 1);
var
  V, VX, VY: UInt;
  Ef: TEffect;
  WIndex: Int;
  WItem: Item;
  MaxCharges: UInt;
  RndValue: UInt;
const
  F = '%s +%d.';

  procedure PrmSkill(ASkill: TSkillEnum);
  begin
    Skills.Modify(ASkill, StartSkill * Multiplier);
    Calc();
    Fill();
  end;

  procedure PrmTalentSkill(ASkill: TSkillEnum);
  begin
    Skills.Modify(ASkill, TalentSkill * Multiplier);
    Calc();
    Fill();
  end;

  procedure PrmValue(AEffect: TEffect; Value: UInt);
  begin
    case AEffect of
      efPrmLife:
        Attributes.Modify(atMaxLife, 0, Value);
      efPrmMana:
        Attributes.Modify(atMaxMana, 0, Value);
      efPrmPV:
        Attributes.Modify(atPV, 0, Value);
      efPrmDV:
        Attributes.Modify(atDV, 0, Value);
      efPrmStr:
        Attributes.Modify(atStr, 0, Value);
      efPrmDex:
        Attributes.Modify(atDex, 0, Value);
      efPrmWil:
        Attributes.Modify(atWil, 0, Value);
      efPrmPer:
        Attributes.Modify(atPer, 0, Value);
    end;
    Calc();
    Fill();
  end;

begin
  // Life
  if (efLife in Effects) then
  begin
    V := Value;
    case RandomRange(0, 3) of
      0:
        MsgLog.Add('You feel healthy.');
      1:
        MsgLog.Add('You feel a bit better.');
      2:
        MsgLog.Add('You feel a wee bit better.');
    end;
    MsgLog.Add(Format(F, ['Life', Min(Attributes.Attrib[atMaxLife].Value -
      Attributes.Attrib[atLife].Value, V)]));
    Attributes.Modify(atLife, V);
  end;
  // Mana
  if (efMana in Effects) then
  begin
    V := Skills.Skill[skConcentration].Value + Value;
    MsgLog.Add('You feel magical energies restoring.');
    MsgLog.Add(Format(F, ['Mana', Min(Self.Attributes.Attrib[atMaxMana].Value -
      Self.Attributes.Attrib[atMana].Value, V)]));
    Self.Attributes.Modify(atMana, V);
    Skills.DoSkill(skConcentration);
  end;
  // Charges
  if (efCharges in Effects) then
  begin
    WIndex := Self.GetEquippedIndex(stRanged);
    if (WIndex < 0) or
      (ItemBase[TItemEnum(Items_Inventory_GetItem(WIndex).ItemID)]
      .ItemType <> itWand) then
      MsgLog.Add('You have no wand equipped.')
    else
    begin
      WItem := Items_Inventory_GetItem(WIndex);
      MaxCharges := ItemBase[TItemEnum(WItem.ItemID)].Value +
        Items.GetBonus(WItem, btWandCap);
      if (WItem.Value >= MaxCharges) then
        MsgLog.Add(Format('%s is already fully charged.',
          [GetCapit(Items.GetNameThe(WItem))]))
      else
      begin
        RndValue := UInt(Math.EnsureRange(Integer(Value) +
          Math.RandomRange(-2, 3), 0, MaxInt));
        V := Min(MaxCharges - WItem.Value, RndValue);
        WItem.Value := WItem.Value + V;
        Items_Inventory_SetItem(WIndex, WItem);
        MsgLog.Add(Format('You recharge %s by %d.',
          [Items.GetNameThe(WItem), V]));
      end;
    end;
  end;
  // Food
  if (efFood in Effects) then
  begin
    Attributes.Modify(atSat, Value);
    MsgLog.Add(Format('You have sated %d hunger.', [Value]));
  end;
  // Identification
  if (efIdentification in Effects) then
    Scenes.SetScene(scIdentification);
  if (efAllIdentification in Effects) then
    Player.IdentAllItems;
  // Craft
  for Ef := CraftEffLow to CraftEffHigh do
    if (Ef in Effects) then
    begin
      Affixes.DoCraft(Ef, Value);
      Scenes.SetScene(scEnchant);
    end;
  // Enchant Item
  if (efEnchantItem in Effects) then
  begin
    Affixes.DoCraft(TEffect(Math.RandomRange(0, 4) + Ord(efCraftStr)),
      Math.EnsureRange(Player.Skills.Skill[skEnchant_Item].Value div 10, 0, 7));
    Scenes.SetScene(scEnchant);
  end;
  // Recharge Wand
  if (efRechargeWand in Effects) then
    Scenes.SetScene(scRecharge);
  // Repair
  if (efRepair in Effects) then
  begin
    Items.Index := Value;
    Scenes.SetScene(scRepair, scInv);
  end;
  // Teleportation
  if (efTeleportation in Effects) then
  begin
    VX := Math.RandomRange(Value, Self.Skills.Skill[skConcentration]
      .Value + Value);
    VY := Math.RandomRange(Value, Self.Skills.Skill[skConcentration]
      .Value + Value);
    X := Map.EnsureRange(X + (Math.RandomRange(0, VX * 2 + 1) - VX));
    Y := Map.EnsureRange(Y + (Math.RandomRange(0, VY * 2 + 1) - VY));
    MsgLog.Add('You have teleported into new place!');
    Scenes.SetScene(scGame);
  end;
  // Town Portal
  if (efTownPortal in Effects) then
  begin
    Map.SetTileEnum(Game.Portal.X, Game.Portal.Y, Game.PortalMap,
      Game.PortalTile);
    if ((X = Game.Spawn.X) and (Y = Game.Spawn.Y)) then
      Exit;
    Game.PortalTile := Map.GetTileEnum(X, Y, Map.Current);
    Game.PortalMap := Map.Current;
    Game.Portal.X := X;
    Game.Portal.Y := Y;
    Map.SetTileEnum(X, Y, Map.Current, tePortal);
    Map.SetTileEnum(Game.Spawn.X, Game.Spawn.Y, deDark_Wood, teTownPortal);
    Scenes.SetScene(scGame);
  end;
  // Magic Eye
  if (efMagicEye in Effects) then
  begin

  end;
  // Light
  if (efLight in Effects) then
  begin
    Abilities.Modify(abLight, Value);
    // MsgLog.Add(Format('You feel lust for blood (%d).', [Value]));
    Self.Calc;
  end;
  // Berserk
  if (efBerserk in Effects) then
  begin
    Abilities.Modify(abBerserk, Value);
    MsgLog.Add(Format('You feel a sudden urge to kill things. (%d).', [Value]));
  end;
  // Bloodlust
  if (efBloodlust in Effects) then
  begin
    V := Math.RandomRange(Value, Skills.Skill[skConcentration].Value + Value);
    Abilities.Modify(abBloodlust, V);
    MsgLog.Add(Format('You feel lust for blood (%d).', [V]));
  end;
  // Cure poison
  if (efCurePoison in Effects) then
  begin
    if Abilities.IsAbility(abPoisoned) then
    begin
      V := Value;
      Abilities.Ability[abPoisoned] :=
        Math.EnsureRange(Abilities.Ability[abPoisoned] - V, 0, UIntMax);
      if Abilities.IsAbility(abPoisoned) then
        MsgLog.Add('You feel better.')
      else
        MsgLog.Add('You are better now.');
    end;
  end;
  // Cure weak
  if (efCureWeak in Effects) then
  begin
    if Abilities.IsAbility(abWeak) then
    begin
      Abilities.Ability[abWeak] := 0;
      MsgLog.Add('You are better now.');
    end;
  end;
  // Athletics
  if (efPrmAthletics in Effects) then
    PrmTalentSkill(skAthletics);
  // Dodge
  if (efPrmDodge in Effects) then
    PrmTalentSkill(skDodge);
  // Awareness
  if (efPrmAwareness in Effects) then
    PrmTalentSkill(skAwareness);
  // Concentration
  if (efPrmConcentration in Effects) then
    PrmTalentSkill(skConcentration);
  // Toughness
  if (efPrmToughness in Effects) then
    PrmTalentSkill(skToughness);
  // Blade
  if (efPrmBlade in Effects) then
    PrmTalentSkill(skBlade);
  // Axe
  if (efPrmAxe in Effects) then
    PrmTalentSkill(skAxe);
  // Spear
  if (efPrmSpear in Effects) then
    PrmTalentSkill(skSpear);
  // Mace
  if (efPrmMace in Effects) then
    PrmTalentSkill(skMace);
  // Staff
  if (efPrmStaff in Effects) then
    PrmTalentSkill(skStaff);
  // Wand
  if (efPrmWand in Effects) then
    PrmTalentSkill(skWand);
  // Dagger
  if (efPrmDagger in Effects) then
    PrmTalentSkill(skDagger);
  // Bow
  if (efPrmBow in Effects) then
    PrmTalentSkill(skBow);
  // Bodybuilding
  if (efPrmBodybuilding in Effects) then
    PrmTalentSkill(skBodybuilding);
  // Meditation
  if (efPrmMeditation in Effects) then
    PrmTalentSkill(skMeditation);
  // Enchant Item
  if (efPrmEnchant_Item in Effects) then
    PrmTalentSkill(skEnchant_Item);
  // Stealth
  if (efPrmStealth in Effects) then
    PrmTalentSkill(skStealth);
  // Life
  if (efPrmLife in Effects) then
  begin
    PrmValue(efPrmLife, IfThen(Value = 0, AttribPrm * Multiplier, Value));
    MsgLog.Add('You increased your amount of life.');
  end;
  // Mana
  if (efPrmMana in Effects) then
  begin
    PrmValue(efPrmMana, IfThen(Value = 0, AttribPrm * Multiplier, Value));
    MsgLog.Add('You increased your amount of mana.');
  end;
  // DV
  if (efPrmDV in Effects) then
  begin
    PrmValue(efPrmDV, IfThen(Value = 0, TalentPrm * Multiplier, Value));
    MsgLog.Add('You increased a defense level');
  end;
  // PV
  if (efPrmPV in Effects) then
  begin
    PrmValue(efPrmPV, IfThen(Value = 0, TalentPrm * Multiplier, Value));
    MsgLog.Add('You increased a protection level');
  end;
  // Strength
  if (efPrmStr in Effects) then
  begin
    PrmValue(efPrmStr, IfThen(Value = 0, MinPrm * Multiplier, Value));
    MsgLog.Add(Format('Strength +%d', [Value]));
  end;
  // Dexterity
  if (efPrmDex in Effects) then
  begin
    PrmValue(efPrmDex, IfThen(Value = 0, MinPrm * Multiplier, Value));
    MsgLog.Add(Format('Dexterity +%d', [Value]));
  end;
  // Willpower
  if (efPrmWil in Effects) then
  begin
    PrmValue(efPrmWil, IfThen(Value = 0, MinPrm * Multiplier, Value));
    MsgLog.Add(Format('Willpower +%d', [Value]));
  end;
  // Perception
  if (efPrmPer in Effects) then
  begin
    PrmValue(efPrmPer, IfThen(Value = 0, MinPrm * Multiplier, Value));
    MsgLog.Add(Format('Perception +%d', [Value]));
  end;
  // Treasure Hunter or Gold Finder
  if (efPrmTreasureHunter in Effects) or (efGoldFinder in Effects) then
  begin
    MsgLog.Add('You increased the amount of gold dropped by monsters');
  end;
  // Survival
  if (efPrmSurvival in Effects) then
  begin
    MsgLog.Add('You have mastered the basics of survival');
  end;
end;

procedure TPlayer.Turn();
var
  Turns: UInt;
begin
  // Regen
  if Abilities.IsAbility(abRegen) then
  begin
    Attributes.Modify(atLife);
    Attributes.Modify(atMana, Math.RandomRange(0, 3) + 1);
  end;
  if not Abilities.IsAbility(abDiseased) then
  begin
    // Replenish Life
    Turns := LifeTurnMax - Skills.Skill[skBodybuilding].Value;
    if (Statictics.Get(stTurn) mod Turns = 0) then
      Attributes.Modify(atLife, Skills.Skill[skBodybuilding].Value);
    // Regenerate Mana
    Turns := ManaTurnMax - Skills.Skill[skMeditation].Value;
    if (Statictics.Get(stTurn) mod Turns = 0) then
      Attributes.Modify(atMana, Skills.Skill[skMeditation].Value);
  end;
end;

initialization

  Player := TPlayer.Create;

finalization

  FreeAndNil(Player);

end.
