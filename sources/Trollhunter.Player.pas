unit Trollhunter.Player;

interface

uses
  Trollhunter.Types,
  Trollhunter.Player.Types,
  Trollhunter.Creature,
  Trollhunter.Mob,
  Trollhunter.Item.Common,
  Trollhunter.Skill,
  Trollhunter.Effect,
  Trollhunter.Spell,
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
  // Ranged damage
  RangedMinDamageMax = 90;
  RangedMaxDamageMax = 100;
  // Spell damage
  SpellMinDamageMax = 79;
  SpellMaxDamageMax = 110;
  // Blacksmith
  CIdentifyAllItemsCost = 100;
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
    FIsRest: boolean;
    FName: string;
    FStatistics: TStatistics;
    FGender: TGender;
    FTalents: TTalents;
    FSkills: TSkills;
    FFireMode: boolean;
    FMagicMode: boolean;
    FFireTargets: array of Int;
    FFireIndex: Int;
    FBowLevel: UInt;
    FBowMinDamage: UInt;
    FBowMaxDamage: UInt;
    FManaShieldPercent: UInt;
    function GetVision: UInt;
    procedure Empty;
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
    property MagicMode: boolean read FMagicMode;
    property FireIndex: Int read FFireIndex;
    property Gold: Int read FGold write FGold;
    property Killer: string read FKiller write FKiller;
    property IsRest: boolean read FIsRest write FIsRest;
    property ItemIsDrop: boolean read FItemIsDrop write FItemIsDrop;
    property ItemIndex: Int read FItemIndex write FItemIndex;
    property ItemAmount: Int read FItemAmount write FItemAmount;
    property SatPerTurn: UInt read FSatPerTurn write FSatPerTurn;
    property Statictics: TStatistics read FStatistics write FStatistics;
    property Name: string read FName write FName;
    property Skills: TSkills read FSkills write FSkills;
    property HRace: TRaceEnum read FRace write FRace;
    property HClass: TClassEnum read FClass write FClass;
    property Talents: TTalents read FTalents write FTalents;
    procedure SetAmountScene(IsDrop: boolean; Index, Amount: Int);
    property Gender: TGender read FGender write FGender;
    procedure Render(AX, AY: UInt);
    procedure Move(Dir: TDirectionEnum);
    procedure RenderInfo;
    procedure Calc;
    procedure Wait;
    procedure Clear();
    procedure AddTurn;
    procedure Spawn;
    procedure Defeat(AKiller: string = '');
    procedure MeleeAttack(Index: Int);
    procedure Backstab(Index: Int);
    procedure ApplyWeaponPoison(AMob: TMob; const AThe: string);
    procedure BreakStealth;
    procedure UseCharge;
    procedure BuildFireTargets(ARange: UInt);
    procedure RangedAttack(Index: Int);
    procedure MagicAttack(Index: Int; ASpellEnum: TSpellEnum);
    procedure MagicSplashAttack(Index: Int; ASpellEnum: TSpellEnum);
    function CanFire: boolean;
    procedure FireModeEnter;
    procedure MagicFireModeEnter;
    procedure FireModeExit;
    procedure FireModeSwitch(ADir: Int);
    function FireModeTarget: Int;
    function FireRange: UInt;
    function RangedMinDamage: UInt;
    function RangedMaxDamage: UInt;
    function CanRangedAttack: boolean;
    procedure ReceiveHealing;
    procedure BuyArrows;
    procedure RechargeWand(Index: Int);
    procedure Buy(Index: Int);
    procedure PickUp;
    procedure PickUpAmount(Index: Int);
    procedure PickUpArrows(const MapID, Index: Int; AItem: Item);
    procedure Drop(Index: Int);
    procedure DropAmount(Index: Int);
    procedure UseItem(Index: Int);
    procedure DoEffects(const Effects: TEffects; const Value: UInt = 0;
      const Multiplier: UInt = 1);
    function AbsorbManaShieldDamage(const ADamage: UInt): UInt;
    procedure Equip(Index: Int);
    procedure UnEquip(Index: Int);
    procedure Sell(Index: Int);
    procedure RepairItem(Index: Int);
    procedure PoisonItem(Index: Int);
    procedure DisenchantItem(Index: Int);
    procedure IdentItem(Index: Int);
    function IdentAllItems: boolean;
    function HasUnidentifiedItems: boolean;
    procedure IdentifyAllItems;
    procedure CraftItem(Index: Int);
    procedure BreakItem(Index: Int; Value: UInt = 1); overload;
    procedure BreakItem(ASlot: TSlotType; Value: UInt = 1); overload;
    procedure BreakItem(); overload;
    procedure AddExp(Value: UInt = 1);
    procedure Rest(ATurns: UInt);
    procedure RestUntilHealed;
    function HasVisibleEnemy: boolean;
    procedure Dialog(AMob: TMob);
    procedure RnItem(FItem: Item; const Index: Int);
    procedure AutoPickup();
    procedure RenderWeather(const AX, AY, AWidth: UInt);
    procedure Turn;
    function IsOnStash: boolean;
    function SpellMinDamage(ASpellEnum: TSpellEnum): UInt;
    function SpellMaxDamage(ASpellEnum: TSpellEnum): UInt;
    function QuickSpellMinDamage: UInt;
    function QuickSpellMaxDamage: UInt;
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
  Trollhunter.Terminal,
  Trollhunter.UI.Log,
  Trollhunter.Calendar,
  Trollhunter.Weather,
  Trollhunter.Item.Shop,
  BearLibTerminal,
  Trollhunter.Ability,
  Trollhunter.Creature.NPC,
  Trollhunter.Projectile.Types,
  Trollhunter.Item.Affixes,
  Trollhunter.Attribute,
  Trollhunter.Spellbook,
  Trollhunter.UI,
  Trollhunter.Player.Name,
  Trollhunter.Item.Dungeon,
  Trollhunter.Item.Inventory,
  Trollhunter.Helpers,
  Trollhunter.Item.Types,
  Trollhunter.StatusEffect,
  Trollhunter.Player.Helpers,
  Trollhunter.Player.Background,
  Trollhunter.Utils;

  { TPlayer }

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
var
  LWasAiming: boolean;
begin
  if IsDead then
    Exit;
  Statictics.Inc(stTurn);
  Calendar.Turn;
  if (Attributes.Attrib[atSat].Value > 0) and
    (Math.RandomRange(0, MetabolismMax) <= Player.Statictics.Get(stMetabolism)) then
    if not Utils.Chance(Player.Talents.GetLevel(tlSurvival) * 10) then
      Attributes.Modify(atSat, -SatPerTurn);
  if StatusEffects.IsStatusEffect(seWeak) then
    Attributes.Modify(atSat, -10);
  if (Attributes.Attrib[atSat].Value < StarvingMax) then
    Attributes.Modify(atLife, -1);
  Turn;
  LWasAiming := StatusEffects.IsStatusEffect(seAiming);
  if OnTurn() then
    Calc;
  if LWasAiming and not StatusEffects.IsStatusEffect(seAiming) then
    MsgLog.Add(Terminal.Colorize(
      'You lower your bow - the aiming effect fades away.',
      StatusEffects.GetColor(seAiming)));
  if IsDead then
    Defeat;
  Mobs.Process;
end;

procedure TPlayer.BreakStealth;
begin
  if StatusEffects.IsStatusEffect(seStealth) then
  begin
    StatusEffects.StatusEffect[seStealth] := 0;
    MsgLog.Add('You are no longer hidden in the shadows.');
  end;
end;

procedure TPlayer.ApplyWeaponPoison(AMob: TMob; const AThe: string);
const
  CThiefPoisonSkillBonus = 2;
var
  LWeaponIndex: Int;
  LWeapon: Item;
begin
  LWeaponIndex := GetEquippedIndex(stMainHand);
  if (LWeaponIndex < 0) then
    Exit;
  LWeapon := Items_Inventory_GetItem(LWeaponIndex);
  if not ((ItemBase[TItemEnum(LWeapon.ItemID)].ItemType in DaggerTypeItems) and
    (LWeapon.Value > 0)) then
    Exit;
  AMob.StatusEffects.Modify(sePoisoned, Skills.Skill[skPoisoning].Value);
  if (FClass = clThief) then
    Skills.DoSkill(skPoisoning, CThiefPoisonSkillBonus)
  else
    Skills.DoSkill(skPoisoning);
  LWeapon.Value := Game.EnsureRange(LWeapon.Value - 1, UIntMax);
  Items_Inventory_SetItem(LWeaponIndex, LWeapon);
  MsgLog.Add(Format('You poison %s.', [AThe]));
end;

procedure TPlayer.Backstab(Index: Int);
const
  CBackstabDamageMultiplier = 2;
var
  Dam: UInt;
  Mob: TMob;
  The: string;
begin
  Mob := Mobs.Mob[Index];
  The := GetDescThe(Mobs.Name[TMobEnum(Mob.ID)]);
  Dam := Game.EnsureRange(RandomRange(Self.GetDamage.Min, GetDamage.Max +
    1), UIntMax);
  if StatusEffects.IsStatusEffect(seBloodlust) then
    Inc(Dam, Dam div 4);
  if StatusEffects.IsStatusEffect(seWeak) then
    Dec(Dam, Dam div 3);
  Dam := Dam * CBackstabDamageMultiplier;
  Dam := Self.GetRealDamage(Dam, Mob.Attributes.Attrib[atPV].Value);
  if (Dam = 0) then
  begin
    MsgLog.Add(Format('You miss %s.', [The]));
    SatPerTurn := Ord(Game.Difficulty) + 9;
    AddTurn;
    Exit;
  end;
  Mob.Attributes.Modify(atLife, -Dam);
  MsgLog.Add(Format('You backstab %s (%d).', [The, Dam]));
  ApplyWeaponPoison(Mob, The);
  Skills.DoSkill(skStealth);
  Skills.DoSkill(skDagger);
  if ((Math.RandomRange(0, 7 - Ord(Game.Difficulty)) = 0) and not Mode.Wizard) then
    BreakItem(stMainHand);
  if Mob.IsDead then
    Mob.Defeat;
  AddTurn;
end;

procedure TPlayer.MeleeAttack(Index: Int);
const
  AccuracyDexDivisor = 2;
  CStealthDamageBonusDivisor = 4;
var
  V, Ch: UInt;
  Mob: TMob;
  Dam, Cr, TargetDV, AccBonus: UInt;
  CrStr, The: string;
  LWasStealthed: boolean;

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
    GenRandomNPCWelcomeText;
    Exit;
  end;
  LWasStealthed := StatusEffects.IsStatusEffect(seStealth);
  BreakStealth;
  if LWasStealthed and (FWeaponSkill = skDagger) then
  begin
    Backstab(Index);
    Exit;
  end;
  The := GetDescThe(Mobs.Name[TMobEnum(Mob.ID)]);
  TargetDV := Mob.Attributes.Attrib[atDV].Value;
  if StatusEffects.IsStatusEffect(seBerserk) then
    TargetDV := TargetDV div 2;
  AccBonus := Self.Attributes.Attrib[atDV].Value div AccuracyDexDivisor;
  TargetDV := UInt(Math.Max(0, Int(TargetDV) - Int(AccBonus)));
  if (TargetDV < Math.RandomRange(0, 100)) and not
    StatusEffects.IsStatusEffect(seCursed) then
  begin
    CrStr := '';
    // Attack
    Dam := Game.EnsureRange(RandomRange(Self.GetDamage.Min, GetDamage.Max +
      1), UIntMax);
    // Status Effects
    if StatusEffects.IsStatusEffect(seBloodlust) then
      Inc(Dam, Dam div 4);
    if StatusEffects.IsStatusEffect(seWeak) then
      Dec(Dam, Dam div 3);
    if LWasStealthed then
      Inc(Dam, Dam div CStealthDamageBonusDivisor);
    // Critical hits...     .
    Ch := Math.RandomRange(0, 100);
    Cr := Skills.Skill[FWeaponSkill].Value;
    if ((Ch < Cr) and not StatusEffects.IsStatusEffect(seWeak)) then
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
    // Poison
    ApplyWeaponPoison(Mob, The);
    // Break weapon
    if ((Math.RandomRange(0, 10 - Ord(Game.Difficulty)) = 0) and not Mode.Wizard) then
      BreakItem(stMainHand);
    if (CrStr <> '') then
      MsgLog.Add(Terminal.Colorize(CrStr, clAlarm));
    Skills.DoWeaponSkill;
    // Victory
    if Mob.IsDead then
      Mob.Defeat;
  end
  else
    Miss();
  AddTurn;
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

procedure TPlayer.RangedAttack(Index: Int);
const
  RangeAccuracyPenalty = 3;
  AccuracyDexDivisor = 2;
  CSlowedTurns = 10;
  CStealthDamageBonusDivisor = 4;
  CRangedCritMultiplier = 2;
var
  V, Ch: UInt;
  Mob: TMob;
  Dam, Cr, TargetDV, AccBonus, Dist, RMin, RMax: UInt;
  CrStr, The: string;
  LWasStealthed: boolean;

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
  LWasStealthed := StatusEffects.IsStatusEffect(seStealth);
  BreakStealth;
  Dist := Self.GetDist(Mob.X, Mob.Y);
  if (Dist <= 1) then
  begin
    Self.FireModeExit;
    Self.MeleeAttack(Index);
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
  if StatusEffects.IsStatusEffect(seBerserk) then
    TargetDV := TargetDV div 2;
  if (Dist > 1) then
    Inc(TargetDV, (Dist - 1) * RangeAccuracyPenalty);
  AccBonus := Self.Attributes.Attrib[atDV].Value div AccuracyDexDivisor;
  TargetDV := UInt(Math.Max(0, Int(TargetDV) - Int(AccBonus)));
  if (TargetDV < Math.RandomRange(0, 100)) and not
    StatusEffects.IsStatusEffect(seCursed) then
  begin
    CrStr := '';
    RMin := EnsureRange(FBowMinDamage + Attributes.Attrib[atDex].Value div 7,
      1, UIntMax - 1);
    RMax := EnsureRange(FBowMaxDamage + Attributes.Attrib[atDex].Value div 5,
      2, UIntMax);
    Dam := Game.EnsureRange(RandomRange(RMin, RMax + 1), UIntMax);
    // Status Effects
    if StatusEffects.IsStatusEffect(seBloodlust) then
      Inc(Dam, Dam div 4);
    if StatusEffects.IsStatusEffect(seWeak) then
      Dec(Dam, Dam div 3);
    if LWasStealthed then
      Inc(Dam, Dam div CStealthDamageBonusDivisor);
    // Critical hit (ranged weapons only ever land a "good hit")
    Ch := Math.RandomRange(0, 100);
    Cr := Skills.Skill[FWeaponSkill].Value;
    if (Ch < Cr) and not StatusEffects.IsStatusEffect(seWeak) then
    begin
      V := CRangedCritMultiplier;
      Dam := Dam * V;
      CrStr := Format('It was a good hit! (%dx)', [V]);
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
    begin
      MsgLog.Add(Format('Your arrow hits %s (%d).', [The, Dam]));
      if StatusEffects.IsStatusEffect(seAiming) then
      begin
        Mob.StatusEffects.Modify(seSlowed, CSlowedTurns);
        MsgLog.Add(Terminal.Colorize(
          Format('%s is crippled.', [GetCapit(The)]),
          Mob.StatusEffects.GetColor(seSlowed)));
        StatusEffects.StatusEffect[seAiming] := 0;
      end;
    end;
    // Break weapon
    if ((Math.RandomRange(0, 15 - Ord(Game.Difficulty)) = 0) and not Mode.Wizard) then
      BreakItem(stRanged)
    else if (FWeaponSkill <> skWand) and
      ((Math.RandomRange(0, 20 - Ord(Game.Difficulty)) = 0) and not Mode.Wizard) then
      BreakItem(stQuiver);
    if (CrStr <> '') then
      MsgLog.Add(Terminal.Colorize(CrStr, clAlarm));
    Skills.DoWeaponSkill;
    // Victory
    if Mob.IsDead then
      Mob.Defeat;
  end
  else
    Miss();
  AddTurn;
end;

procedure TPlayer.MagicAttack(Index: Int; ASpellEnum: TSpellEnum);
const
  RangeAccuracyPenalty = 3;
  AccuracyWilDivisor = 2;
  CStealthDamageBonusDivisor = 4;
var
  Mob: TMob;
  Dam, TargetDV, AccBonus, Dist, MMin, MMax, V: UInt;
  The: string;
  LSpell: TSpellData;
  LWasStealthed: boolean;

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
  LWasStealthed := StatusEffects.IsStatusEffect(seStealth);
  BreakStealth;
  LSpell := GetSpellData(ASpellEnum);
  if (Self.Attributes.Attrib[atMana].Value < LSpell.ManaCost) then
  begin
    MsgLog.Add(Format('You don''t have enough mana to cast %s.', [LSpell.Name]));
    Self.FireModeExit;
    Exit;
  end;
  Self.Attributes.Modify(atMana, -Int(LSpell.ManaCost));
  Self.Statictics.Inc(stSpCast);
  Dist := Self.GetDist(Mob.X, Mob.Y);
  The := GetDescThe(Mobs.Name[TMobEnum(Mob.ID)]);
  TargetDV := Mob.Attributes.Attrib[atDV].Value;
  if StatusEffects.IsStatusEffect(seBerserk) then
    TargetDV := TargetDV div 2;
  if (Dist > 1) then
    Inc(TargetDV, (Dist - 1) * RangeAccuracyPenalty);
  AccBonus := Self.Attributes.Attrib[atWil].Value div AccuracyWilDivisor;
  TargetDV := UInt(Math.Max(0, Int(TargetDV) - Int(AccBonus)));
  if (TargetDV < Math.RandomRange(0, 100)) and not
    StatusEffects.IsStatusEffect(seCursed) then
  begin
    MMin := EnsureRange(LSpell.MinDamage + Attributes.Attrib[atWil].Value div
      7, 1, UIntMax - 1);
    MMax := EnsureRange(LSpell.MaxDamage + Attributes.Attrib[atWil].Value div
      5, 2, UIntMax);
    Dam := Game.EnsureRange(RandomRange(MMin, MMax + 1), UIntMax);
    if StatusEffects.IsStatusEffect(seBloodlust) then
      Inc(Dam, Dam div 4);
    if StatusEffects.IsStatusEffect(seWeak) then
      Dec(Dam, Dam div 3);
    if LWasStealthed then
      Inc(Dam, Dam div CStealthDamageBonusDivisor);
    Dam := Self.GetRealDamage(Dam, Mob.Attributes.Attrib[atPV].Value);
    if (Dam = 0) then
    begin
      Miss();
      AddTurn;
      Exit;
    end;
    Mob.Attributes.Modify(atLife, -Dam);
    MsgLog.Add(Format('Your %s hits %s (%d).', [LowerCase(LSpell.Name), The, Dam]));
    Skills.TryAwarenessSkill;
    if Mob.IsDead then
      Mob.Defeat
    else if (efWeaken in LSpell.Effects) then
    begin
      V := Skills.Skill[skConcentration].Value + LSpell.Value;
      Mob.StatusEffects.Modify(seWeak, V);
      MsgLog.Add(Format('%s looks weakened (%d).', [The, V]));
    end
    else if (efBurn in LSpell.Effects) then
    begin
      V := Skills.Skill[skConcentration].Value + LSpell.Value;
      Mob.StatusEffects.Modify(seBurning, V);
      MsgLog.Add(Format('%s is engulfed in flames (%d).', [The, V]));
    end
    else if (efDrain in LSpell.Effects) then
    begin
      V := (Dam * (Skills.Skill[skConcentration].Value + LSpell.Value)) div 100;
      Attributes.Modify(atLife, V);
      Skills.DoSkill(skConcentration);
      MsgLog.Add(Format('You drain %d life from %s.', [V, The]));
    end;
  end
  else
    Miss();
  AddTurn;
end;

procedure TPlayer.MagicSplashAttack(Index: Int; ASpellEnum: TSpellEnum);
var
  LMob: TMob;
  LDam, LMin, LMax, LPercent: UInt;
  LSpell: TSpellData;
  LThe: string;
begin
  if (Index < 0) then
    Exit;
  LMob := Mobs.Mob[Index];
  if not LMob.Alive or (LMob.Force <> fcEnemy) then
    Exit;
  LSpell := GetSpellData(ASpellEnum);
  LThe := GetDescThe(Mobs.Name[TMobEnum(LMob.ID)]);
  LPercent := EnsureRange(Skills.Skill[skConcentration].Value, SkillMin, SkillMax);
  LMin := EnsureRange((LSpell.MinDamage * LPercent) div 100, 1, UIntMax - 1);
  LMax := EnsureRange((LSpell.MaxDamage * LPercent) div 100, 2, UIntMax);
  LDam := Game.EnsureRange(RandomRange(LMin, LMax + 1), UIntMax);
  LDam := Self.GetRealDamage(LDam, LMob.Attributes.Attrib[atPV].Value);
  if (LDam = 0) then
    Exit;
  LMob.Attributes.Modify(atLife, -LDam);
  MsgLog.Add(Format('The lightning arcs to %s (%d).', [LThe, LDam]));
  if LMob.IsDead then
    LMob.Defeat;
end;

function TPlayer.CanFire: boolean;
begin
  Result := (FWeaponSkill = skBow) or (FWeaponSkill = skWand);
end;

procedure TPlayer.BuildFireTargets(ARange: UInt);
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
  for I := 0 to Mobs.Count - 1 do
    if Mobs.Mob[I].Alive and (Mobs.Mob[I].Force = fcEnemy) and
      (Mobs.Mob[I].MapZone = Map.Current) and
      Map.InView(Mobs.Mob[I].X, Mobs.Mob[I].Y) and
      (Self.GetDist(Mobs.Mob[I].X, Mobs.Mob[I].Y) > 1) and
      (Mode.Wizard or (Map.GetFOV(Mobs.Mob[I].X, Mobs.Mob[I].Y) and
      (Self.GetDist(Mobs.Mob[I].X, Mobs.Mob[I].Y) <= ARange))) then
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

procedure TPlayer.FireModeEnter;
begin
  FMagicMode := False;
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
  Self.BuildFireTargets(Self.FireRange);
end;

procedure TPlayer.MagicFireModeEnter;
begin
  FMagicMode := True;
  if not (Spellbook.GetSpell(Spellbook.GetLastSelectedSpell).Enable and
    (Spellbook.GetSpell(Spellbook.GetLastSelectedSpell).Spell.Projectile <>
    prNone)) then
  begin
    FFireMode := False;
    MsgLog.Add('No quick spell selected.');
    Exit;
  end;
  if (Self.Attributes.Attrib[atMana].Value < Spellbook.GetSpell(
    Spellbook.GetLastSelectedSpell).Spell.ManaCost) then
  begin
    FFireMode := False;
    MsgLog.Add('You need more mana!');
    Exit;
  end;
  Self.BuildFireTargets(Self.Vision);
end;

function TPlayer.FireRange: UInt;
begin
  Result := Math.Min(Self.Vision, 4 + ((FBowLevel + 1) div 2) +
    Talents.GetLevel(tlLong_Range) + Talents.GetLevel(tlElven_Marksman));
end;

function TPlayer.RangedMinDamage: UInt;
begin
  Result := EnsureRange(FBowMinDamage + Attributes.Attrib[atDex].Value div 5,
    1, RangedMinDamageMax);
end;

function TPlayer.RangedMaxDamage: UInt;
begin
  Result := EnsureRange(FBowMaxDamage + Attributes.Attrib[atDex].Value div 3,
    2, RangedMaxDamageMax);
end;

function TPlayer.CanRangedAttack: boolean;
begin
  Result := Self.CanFire and (((FWeaponSkill = skWand) and Self.HasCharges) or
    (Self.HasQuiver and not Self.IsQuiverBroken and Self.HasArrows));
end;

procedure TPlayer.FireModeExit;
begin
  FFireMode := False;
  FMagicMode := False;
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
  if (Items_Inventory_GetCount() >= ItemMax) then
    Exit;
  FCount := Items_Dungeon_GetMapCountXY(Ord(Map.Current), X, Y).InRange(ItemMax);
  for Index := FCount - 1 downto 0 do
  begin
    if (Items_Inventory_GetCount() >= ItemMax) then
      Exit;
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

  if (StatusEffects.IsStatusEffect(seBerserk)) then
  begin
    HiAttrib(atStr);
    HiAttrib(atDex);
  end;
  if (StatusEffects.IsStatusEffect(seWeak)) then
  begin
    LoAttrib(atStr);
    LoAttrib(atDex);
  end;
  if StatusEffects.IsStatusEffect(seAfraid) then
    LoAttrib(atWil);
  if StatusEffects.IsStatusEffect(seDrunk) then
    LoAttrib(atPer);
  // DV
  Attributes.SetValue(atDV,
    Game.EnsureRange(Round(Attributes.Attrib[atDex].Value * (DVMax / AttribMax)) +
    Attributes.Attrib[atDV].Prm, DVMax));
  // PV
  Attributes.SetValue(atPV,
    Game.EnsureRange(Round(Skills.Skill[skToughness].Value / 1.4) -
    4 + FAttrib[atDef] + Attributes.Attrib[atPV].Prm, PVMax));
  if StatusEffects.IsStatusEffect(seArmor_Reduction) then
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
  if StatusEffects.IsStatusEffect(seLight) then
    Light := Light + StatusEffects.StatusEffect[seLight];
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

  // if StatusEffects.IsStatusEffect(abWeightless) then;
  // Your pack seems featherweight! -- Your pack seems much heavier!
end;

procedure TPlayer.Clear();
begin
  inherited Clear();
  Skills.Clear();
  Spellbook.Clear();
  Items_Inventory_Clear();
  Self.Empty;
  Attributes.SetValue(atSat, SatiatedMax);
  Gold := 0;
  MaxMap := 0;
  FWeaponSkill := skNone;
  FBowLevel := 0;
  FBowMinDamage := 0;
  FBowMaxDamage := 0;
  FFireMode := False;
  FMagicMode := False;
  FFireIndex := -1;
  SetLength(FFireTargets, 0);
  Attributes.SetValue(atLev, 1);
  GeneratePlayerBackground();
  Calc();
  Fill();
end;

procedure TPlayer.CraftItem(Index: Int);
var
  FItem: Item;
begin
  if not Items_Inventory_IndexInRange(Index) then Exit;
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
  if (Game.Difficulty < dfNormal) then
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

function TPlayer.GetVision: UInt;
begin
  Result := Game.EnsureRange((Attributes.Attrib[atVision].Value -
    StatusEffects.StatusEffect[seBlinded]) + 3, VisionMax);
  Result := Math.IfThen(Calendar.IsDay, Result, Result div 2);
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
    if (Self.StatusEffects.IsStatusEffect(seStunned) or
      Self.StatusEffects.IsStatusEffect(seBurning)) then
    begin
      AddTurn;
      Exit;
    end;

    if not Mobs.GetFreeTile(FX, FY) then
    begin
      Self.MeleeAttack(Mobs.GetIndex(FX, FY));
    end
    else
    begin
      X := FX;
      Y := FY;
      if ((Direction[Dir].X <> 0) or (Direction[Dir].Y <> 0)) then
      begin
        SatPerTurn := 2;
        if not IsOnStash then
          AutoPickup;
      end;
      AddTurn;
    end;
  end;
end;

procedure TPlayer.UseItem(Index: Int);
var
  FItem: Item;
  I: TItemEnum;
  T: TItemType;
begin
  if IsDead or not Items_Inventory_IndexInRange(Index) then
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
      if (T in BookTypeItems) then
      begin
        if Spellbook.GetSpell(TSpellEnum(FItem.Value)).Enable then
          MsgLog.Add(Format('You already know %s.',
            [GetSpellData(TSpellEnum(FItem.Value)).Name]))
        else
        begin
          Spellbook.AddSpell(TSpellEnum(FItem.Value));
          MsgLog.Add(Format('You learn %s.',
            [GetSpellData(TSpellEnum(FItem.Value)).Name]));
        end;
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
  if not Items_Inventory_IndexInRange(Index) then Exit;
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
  if not Items_Inventory_IndexInRange(Index) then Exit;
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
  if (Items_Inventory_GetCount() >= ItemMax) then
  begin
    MsgLog.Add('Your backpack is full!');
    Exit;
  end;
  FItem := Shops.Shop[Shops.Current].GetItem(Index);
  if (Items_Inventory_DeleteItemAmount(Ord(itmGold), FItem.Price) > 0) then
  begin
    MsgLog.Add(Format('You bought %s (-%d gold).',
      [Items.GetNameThe(FItem), FItem.Price]));
    Items_Inventory_AppendItem(FItem);
    Self.Calc;
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

function TPlayer.IdentAllItems: boolean;
var
  FItem: Item;
  FCount, I: Int;
begin
  Result := False;
  FCount := Items_Inventory_GetCount().InRange(ItemMax);
  for I := 0 to FCount - 1 do
  begin
    FItem := Items_Inventory_GetItem(I);
    if (FItem.Identify = 0) then
    begin
      Self.IdentItem(I);
      Result := True;
    end;
  end;
  if Result then
    Self.Calc;
end;

function TPlayer.HasUnidentifiedItems: boolean;
var
  LCount, LIndex: Int;
begin
  Result := False;
  LCount := Items_Inventory_GetCount().InRange(ItemMax);
  for LIndex := 0 to LCount - 1 do
    if (Items_Inventory_GetItem(LIndex).Identify = 0) then
      Exit(True);
end;

procedure TPlayer.IdentifyAllItems;
begin
  if not Self.HasUnidentifiedItems then
  begin
    MsgLog.Add('You have nothing to identify.');
    Exit;
  end;
  if (Self.Gold < CIdentifyAllItemsCost) then
  begin
    MsgLog.Add('You need more gold.');
    Exit;
  end;
  if (Items_Inventory_DeleteItemAmount(Ord(itmGold), CIdentifyAllItemsCost) > 0) then
  begin
    Self.IdentAllItems;
    MsgLog.Add(Format('You identify all items (-%d gold).',
      [CIdentifyAllItemsCost]));
  end;
end;

procedure TPlayer.IdentItem(Index: Int);
var
  FItem: Item;
begin
  if not Items_Inventory_IndexInRange(Index) then Exit;
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
  if not Items_Inventory_IndexInRange(Index) then Exit;
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
  if not Items_Inventory_IndexInRange(Index) then Exit;
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

procedure TPlayer.PoisonItem(Index: Int);
var
  LItem: Item;
  LCharges: UInt;
begin
  LItem := Items_Inventory_GetItem(Index);
  if (LItem.ItemID < 0) then
    Exit;
  if not (ItemBase[TItemEnum(LItem.ItemID)].ItemType in DaggerTypeItems) then
  begin
    MsgLog.Add('You can only smear a dagger with poison.');
    Exit;
  end;
  LCharges := Items.Index + Skills.Skill[skPoisoning].Value;
  LItem.Value := Game.EnsureRange(LItem.Value + LCharges, UIntMax);
  if (Items_Inventory_SetItem(Index, LItem) > 0) then
  begin
    MsgLog.Add(Format('You smear %s with poison (+%d).',
      [Items.GetNameThe(LItem), LCharges]));
    Calc;
  end;
  Scenes.SetScene(scInv);
end;

procedure TPlayer.DisenchantItem(Index: Int);
var
  FItem: Item;
  Amount: UInt;
begin
  if not Items_Inventory_IndexInRange(Index) then Exit;
  FItem := Items_Inventory_GetItem(Index);
  if not (ItemBase[TItemEnum(FItem.ItemID)].ItemType in DisenchantTypeItems) then
  begin
    MsgLog.Add('You cannot disenchant this item.');
    Exit;
  end;
  if (FItem.Equipment > 0) then
  begin
    MsgLog.Add('You cannot disenchant an equipped item.');
    Exit;
  end;
  Amount := ItemBase[TItemEnum(FItem.ItemID)].Level;
  if (Items_Inventory_DeleteItem(Index, FItem) > 0) then
  begin
    Items.AddItemToInv(itmArcane_Orb, Amount);
    MsgLog.Add(Format('%s dissolves into pure arcane energy.',
      [Items.GetNameThe(FItem)]));
    if Amount = 1 then
      MsgLog.Add('You receive Arcane Orb.')
    else
      MsgLog.Add(Format('You receive %d Arcane Orbs.', [Amount]));
    Scenes.SetScene(scInv);
  end;
  Self.Calc;
  Wait;
end;

procedure TPlayer.BreakItem(Index: Int; Value: UInt = 1);
var
  FItem: Item;
begin
  if not Items_Inventory_IndexInRange(Index) then Exit;
  FItem := Items_Inventory_GetItem(Index);
  if ((FItem.Stack > 1) or (FItem.Amount > 1)) then
    Exit;
  FItem.Durability := Game.EnsureRange(FItem.Durability - Value, UIntMax);
  if ((FItem.Durability > 0) and (FItem.Durability <
    (FItem.MaxDurability div 4))) then
    MsgLog.Add(Terminal.Colorize(Format('%s soon will be totally broken (%d/%d).',
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
  FCount: Int;

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
        MsgLog.Add(Format('You put %s into the stash.',
          [Items.GetNameThe(AItem)]))
      else
        MsgLog.Add(Format('You drop %s.', [Items.GetNameThe(AItem)]));
      Wait();
    end;
  end;

begin
  if IsDead or not Items_Inventory_IndexInRange(Index) then
    Exit;
  AItem := Items_Inventory_GetItem(Index);
  if (AItem.Equipment > 0) then
    Exit;

  FCount := Items_Dungeon_GetMapCountXY(Ord(Map.Current), X, Y);
  if (FCount >= ItemMax) then
  begin
    if IsOnStash then
      MsgLog.Add('The stash is full!')
    else
      MsgLog.Add('There is no space here.');
    Exit;
  end;

  if not ((AItem.Stack > 1) and (AItem.Amount > 1)) then
    DeleteItem()
  else
    SetAmountScene(True, Index, 1);
  Self.Calc();
end;

procedure TPlayer.DropAmount(Index: Int);
var
  FItem: Item;
  FCount: Int;
begin
  if not Items_Inventory_IndexInRange(Index) then Exit;
  FItem := Items_Inventory_GetItem(Index);

  if (FItem.Stack <= 1) then
  begin
    Drop(Index);
    Exit;
  end;

  ItemAmount := Math.EnsureRange(ItemAmount, 1, FItem.Amount);

  FCount := Items_Dungeon_GetMapCountXY(Ord(Map.Current), X, Y);
  if (FCount >= ItemMax) then
  begin
    if IsOnStash then
      MsgLog.Add('The stash is full!')
    else
      MsgLog.Add('There is no space here.');
    Exit;
  end;

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
    MsgLog.Add(Format('You picked up %d arrows (your quiver is full).', [Picked]));
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
  FCount := Items_Dungeon_GetMapCountXY(Ord(Map.Current), X, Y);
  if Player.IsOnStash then
  begin
    Game.Timer := UIntMax;
    Scenes.SetScene(scStash);
  end
  else if (FCount > 0) then
  begin
    if (Items_Inventory_GetCount() >= ItemMax) then
    begin
      MsgLog.Add('Your backpack is full!');
      Exit;
    end;
    if (FCount = 1) then
    begin
      Items.AddItemToInv(0);
    end
    else
    begin
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
  if (Items_Inventory_GetCount() >= ItemMax) then
  begin
    MsgLog.Add('Your backpack is full!');
    Exit;
  end;
  FItem := Items_Dungeon_GetMapItemXY(Ord(Map.Current), Index, X, Y);

  if (FItem.Stack <= 1) then
  begin
    Items.AddItemToInv(Index, True);
    Exit;
  end;

  ItemAmount := Math.EnsureRange(ItemAmount, 1, FItem.Amount);

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
var
  LColor: cardinal;
begin
  if IsDead then
    Terminal.Print(AX + View.Left, AY + View.Top, '%', clCorpse)
  else
  begin
    if StatusEffects.IsStatusEffect(seStealth) then
      LColor := clDarkGray
    else
      LColor := clPlayer;
    if Look then
      Terminal.Print(AX + View.Left, AY + View.Top, '@', LColor)
    else
      Terminal.Print(AX + View.Left, AY + View.Top, '@', LColor, clBkPlayer);
  end;
end;

procedure TPlayer.RenderInfo;
const
  F = '%s %d/%d';
var
  I: TStatusEffectEnum;
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
        UI.Icon(icShield), Attributes.Attrib[atPV].Value, Satiation]));
      Self.RenderWeather(Status.Left + (Status.Width div 2), Status.Top + 5,
        Status.Width);
      if Spellbook.GetQuickSpell.Enable then
        Terminal.Print(Status.Left, Status.Top + 4, UI.Icon(icBook) +
          ' ' + Spellbook.GetQuickSpell.Spell.Name);
    end;
    else
    begin
      S := '';
      for I := Low(TStatusEffectEnum) to High(TStatusEffectEnum) do
        if StatusEffects.IsStatusEffect(I) then
          S := S + Terminal.Colorize(Format(' %s (%d)',
            [StatusEffects.GetName(I), StatusEffects.StatusEffect[I]]),
            StatusEffects.GetColor(I));
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
  if (Map.Current <> deDark_Wood) then
    Exit;
  case Weather.Weather of
    wtCloudy:
      if Calendar.IsDay then
        Add(UI.Icon(icSunnyAndCloudy), 'Light Gray', 'Lightest Blue')
      else
        Add(UI.Icon(icCloudy), 'Light Gray', 'Darker Gray');
    wtRain:
      Add(UI.Icon(icRain), 'Light Blue', 'Darker Gray');
  else
    if Calendar.IsDay then
      Add(UI.Icon(icSun), 'Light Yellow', 'Lightest Blue')
    else
      Add(UI.Icon(icMoon), 'Light White', 'Darker Gray');
  end;
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
  Self.StatusEffects.Clear;
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
    Map.SetVis(Map.Current, True);
    if (Ord(Map.Current) > 0) then
      Statictics.Inc(stScore, Ord(Map.Current) * 15);
    MaxMap := MaxMap + 1;
  end;
  SatPerTurn := 1;
  Move(drOrigin);
end;

function TPlayer.HasVisibleEnemy: boolean;
var
  I: Int;
begin
  Result := False;
  for I := 0 to Mobs.Count - 1 do
    if Mobs.Mob[I].Alive and (Mobs.Mob[I].Force = fcEnemy) and
      (Mobs.Mob[I].MapZone = Map.Current) and
      Map.InView(Mobs.Mob[I].X, Mobs.Mob[I].Y) and
      (Mode.Wizard or Map.GetFOV(Mobs.Mob[I].X, Mobs.Mob[I].Y)) then
    begin
      Result := True;
      Exit;
    end;
end;

procedure TPlayer.Rest(ATurns: UInt);
var
  T: UInt;
  LLifeBefore: Int;
  LInterruptReason: string;
  LInterruptColor: string;
begin
  if Player.IsDead then
    Exit;
  IsRest := True;
  LInterruptReason := '';
  LInterruptColor := clAlarm;
  T := 0;
  MsgLog.Add(Terminal.Colorize(
    'You settle down and let your thoughts drift into a quiet rest...',
    'Light Green'));
  while (T < ATurns) and IsRest do
  begin
    Inc(T);
    LLifeBefore := Attributes.Attrib[atLife].Value;
    Wait();

    if IsDead then
    begin
      IsRest := False;
      Exit;
    end;

    if (Attributes.Attrib[atLife].Value < LLifeBefore) then
    begin
      LInterruptReason := 'A sudden pain jolts you awake!';
      Break;
    end;
    if HasVisibleEnemy then
    begin
      LInterruptReason := 'A shadow stirs nearby - you snap awake, weapon in hand!';
      Break;
    end;
    if StatusEffects.IsStatusEffect(seStunned) or
      StatusEffects.IsStatusEffect(seBurning) or
      StatusEffects.IsStatusEffect(sePoisoned) then
    begin
      LInterruptReason := 'Your body screams in protest - rest is impossible like this!';
      Break;
    end;
    if (Attributes.Attrib[atSat].Value < StarvingMax) then
    begin
      LInterruptReason := 'Hunger gnaws at your belly, banishing all thought of rest!';
      Break;
    end;
    if (Attributes.Attrib[atLife].Value >= Attributes.Attrib[atMaxLife].Value)
      and (Attributes.Attrib[atMana].Value >= Attributes.Attrib[atMaxMana].Value) then
    begin
      LInterruptColor := 'Light Green';
      Break; // fully rested - normal completion, not an interruption
    end;
  end;
  StatusEffects.StatusEffect[seWeak] := 0;
  if (Math.RandomRange(0, 9) = 0) then
    StatusEffects.StatusEffect[seDrunk] := 0;
  IsRest := False;
  if Player.IsDead then
    Exit;
  if (LInterruptReason <> '') then
    MsgLog.Add(Terminal.Colorize(LInterruptReason, LInterruptColor))
  else
    MsgLog.Add(Terminal.Colorize(Format(
      'You rise, refreshed and steady once more (%d turns passed).',
      [T]), 'Light Green'));
end;

procedure TPlayer.RestUntilHealed;
const
  CMaxRestTurns = 10000;
begin
  if Player.IsDead then
    Exit;
  if (Attributes.Attrib[atLife].Value >= Attributes.Attrib[atMaxLife].Value)
    and (Attributes.Attrib[atMana].Value >= Attributes.Attrib[atMaxMana].Value) then
  begin
    MsgLog.Add('You are already fully rested.');
    Exit;
  end;
  Rest(CMaxRestTurns);
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
  // Regeneration
  if (efRegeneration in Effects) then
  begin
    V := Value + Skills.Skill[skConcentration].Value;
    StatusEffects.Modify(seRegen, V);
    MsgLog.Add('A soothing green light wraps around you, sealing your wounds.');
    Skills.DoSkill(skConcentration);
  end;
  // Charges
  if (efCharges in Effects) then
  begin
    WIndex := Self.GetEquippedIndex(stRanged);
    if (WIndex < 0) or (ItemBase[TItemEnum(Items_Inventory_GetItem(WIndex).ItemID)].ItemType <> itWand) then
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
        RndValue := UInt(Math.EnsureRange(integer(Value) +
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
      Math.EnsureRange(Player.Skills.Skill[skEnchant_Item].Value div 11, 0, 6));
    Scenes.SetScene(scEnchant);
  end;
  // Recharge Wand
  if (efRechargeWand in Effects) then
    Scenes.SetScene(scRecharge);
  // Disenchant
  if (efDisenchant in Effects) then
    Scenes.SetScene(scDisenchant);
  // Repair
  if (efRepair in Effects) then
  begin
    Items.Index := Value;
    Scenes.SetScene(scRepair, scInv);
  end;
  // Poison Weapon
  if (efPoisonWeapon in Effects) then
  begin
    Items.Index := Value;
    Scenes.SetScene(scPoison, scInv);
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
  // Light
  if (efLight in Effects) then
  begin
    StatusEffects.Modify(seLight, Value);
    Self.Calc;
  end;
  // Berserk
  if (efBerserk in Effects) then
  begin
    StatusEffects.Modify(seBerserk, Value);
    MsgLog.Add(Format('You feel a sudden urge to kill things. (%d).', [Value]));
  end;
  // Mana Shield
  if (efManaShield in Effects) then
  begin
    V := Skills.Skill[skConcentration].Value;
    FManaShieldPercent := V + Value;
    StatusEffects.Modify(seMana_Shield, V);
    MsgLog.Add
    (Format('A shimmering shield of mana surrounds you (%d%%, %d turns).',
      [FManaShieldPercent, V]));
  end;
  // Bloodlust
  if (efBloodlust in Effects) then
  begin
    V := Math.RandomRange(Value, Skills.Skill[skConcentration].Value + Value);
    StatusEffects.Modify(seBloodlust, V);
    MsgLog.Add(Format('You feel lust for blood (%d).', [V]));
  end;
  // Cure poison
  if (efCurePoison in Effects) then
  begin
    if StatusEffects.IsStatusEffect(sePoisoned) then
    begin
      V := Value;
      StatusEffects.StatusEffect[sePoisoned] :=
        Math.EnsureRange(StatusEffects.StatusEffect[sePoisoned] - V, 0, UIntMax);
      if StatusEffects.IsStatusEffect(sePoisoned) then
        MsgLog.Add('You feel better.')
      else
        MsgLog.Add('You are better now.');
    end;
  end;
  // Cure weak
  if (efCureWeak in Effects) then
  begin
    if StatusEffects.IsStatusEffect(seWeak) then
    begin
      StatusEffects.StatusEffect[seWeak] := 0;
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
  // Poisoning
  if (efPrmPoisoning in Effects) then
    PrmTalentSkill(skPoisoning);
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
  if (efPrmTreasureHunter in Effects) or (efPrmGoldFinder in Effects) then
  begin
    MsgLog.Add('You increased the amount of gold dropped by monsters');
  end;
  // Survival
  if (efPrmSurvival in Effects) then
  begin
    MsgLog.Add('You have mastered the basics of survival');
  end;
end;

function TPlayer.AbsorbManaShieldDamage(const ADamage: UInt): UInt;
var
  LManaDamage: UInt;
  LAbsorbed: UInt;
begin
  Result := ADamage;
  if StatusEffects.IsStatusEffect(seMana_Shield) then
  begin
    LManaDamage := (ADamage * FManaShieldPercent) div 100;
    LAbsorbed := Math.Min(LManaDamage, Attributes.Attrib[atMana].Value);
    if (LAbsorbed > 0) then
    begin
      Attributes.Modify(atMana, -LAbsorbed);
      Result := ADamage - LAbsorbed;
      MsgLog.Add(Format('Your mana shield absorbs %d damage.', [LAbsorbed]));
    end;
  end;
end;

procedure TPlayer.Turn();
var
  Turns: UInt;
begin
  // Regen
  if StatusEffects.IsStatusEffect(seRegen) then
  begin
    Attributes.Modify(atLife);
    Attributes.Modify(atMana, Math.RandomRange(0, 3) + 1);
  end;
  if not StatusEffects.IsStatusEffect(seDiseased) then
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

function TPlayer.SpellMinDamage(ASpellEnum: TSpellEnum): UInt;
begin
  Result := EnsureRange(Spellbook.GetSpell(ASpellEnum).Spell.MinDamage +
    Attributes.Attrib[atWil].Value div 5, 1, SpellMinDamageMax);
end;

function TPlayer.SpellMaxDamage(ASpellEnum: TSpellEnum): UInt;
begin
  Result := EnsureRange(Spellbook.GetSpell(ASpellEnum).Spell.MaxDamage +
    Attributes.Attrib[atWil].Value div 3, 2, SpellMaxDamageMax);
end;

function TPlayer.QuickSpellMinDamage: UInt;
begin
  Result := SpellMinDamage(Spellbook.GetQuickSpellEnum);
end;

function TPlayer.QuickSpellMaxDamage: UInt;
begin
  Result := SpellMaxDamage(Spellbook.GetQuickSpellEnum);
end;

initialization

  Player := TPlayer.Create;

finalization

  FreeAndNil(Player);

end.
