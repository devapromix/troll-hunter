unit Trollhunter.Skill;

interface

uses
  Trollhunter.Types;

type
  TSkillEnum = (
    skNone,

    skStealth,
    // Attributes skills
    skAthletics, skDodge, skConcentration, skToughness, skAwareness,

    skBodybuilding, skMeditation, skEnchant_Item, skPoisoning,
    // Weapon skills
    skBlade, skAxe, skSpear, skMace, skDagger, skStaff, skWand, skBow);

type
  TSkill = record
    Value: Int;
    Exp: Int;
  end;

type

  { TSkills }

  TSkills = class(TObject)
  private
    FSkillName: array [TSkillEnum] of string;
    FSkill: array [TSkillEnum] of TSkill;
    function GetSkill(I: TSkillEnum): TSkill;
    procedure SetSkill(I: TSkillEnum; const Value: TSkill);
    procedure TryWeaponSkill(ASkill: TSkillEnum; AValue, AChance: UInt);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Skill[I: TSkillEnum]: TSkill read GetSkill write SetSkill;
    procedure DoSkill(ASkill: TSkillEnum; AExpValue: UInt = 1);
    procedure DoSkillChance(ASkill: TSkillEnum; AChance: UInt = 1);
    procedure DoWeaponSkill;
    procedure TryAwarenessSkill;
    procedure TryDodgeSkill;
    procedure TryAthleticsSkill;
    procedure TryConcentrationSkill;
    procedure TryBodybuildingSkill;
    procedure TryMeditationSkill;
    procedure Modify(I: TSkillEnum; Value: Int);
    function GetName(I: TSkillEnum): string;
    class function GetSkillExpMax: UInt;
  end;

const
  SkillMin = 5;
  SkillMax = 75;
  BeginSkill = 10;
  StartSkill = 5;
  TalentSkill = 1;
  CFullSkillChance = 100;

implementation

uses
  SysUtils,
  TypInfo,
  Math,
  Trollhunter.Terminal,
  Trollhunter.Player,
  Trollhunter.Game,
  Trollhunter.UI.Log,
  Trollhunter.Statistic,
  Trollhunter.Helpers,
  Trollhunter.Player.Classes,
  Trollhunter.Player.Races,
  Trollhunter.Utils;

type
  TSkillInfo = record
    Value: UInt;
    Chance: UInt;
  end;

type
  TWeaponSkillInfo = record
    Athletics: TSkillInfo;
    Dodge: TSkillInfo;
    Concentration: TSkillInfo;
    Bodybuilding: TSkillInfo;
    Meditation: TSkillInfo;
  end;

const
  CWeaponSkillTable: array [skBlade .. skBow] of TWeaponSkillInfo = (
    // Blade
    (Athletics: (Value: 2; Chance: 100); Dodge: (Value: 2; Chance: 90);
     Concentration: (Value: 0; Chance: 0); Bodybuilding: (Value: 1; Chance: 15);
     Meditation: (Value: 0; Chance: 0)),
    // Axe
    (Athletics: (Value: 3; Chance: 90); Dodge: (Value: 1; Chance: 30);
     Concentration: (Value: 0; Chance: 0); Bodybuilding: (Value: 1; Chance: 10);
     Meditation: (Value: 0; Chance: 0)),
    // Spear
    (Athletics: (Value: 1; Chance: 15); Dodge: (Value: 3; Chance: 100);
     Concentration: (Value: 0; Chance: 0); Bodybuilding: (Value: 1; Chance: 10);
     Meditation: (Value: 0; Chance: 0)),
    // Mace
    (Athletics: (Value: 3; Chance: 80); Dodge: (Value: 1; Chance: 25);
     Concentration: (Value: 0; Chance: 0); Bodybuilding: (Value: 1; Chance: 4);
     Meditation: (Value: 0; Chance: 0)),
    // Dagger
    (Athletics: (Value: 1; Chance: 40); Dodge: (Value: 3; Chance: 100);
     Concentration: (Value: 0; Chance: 0); Bodybuilding: (Value: 1; Chance: 12);
     Meditation: (Value: 0; Chance: 0)),
    // Staff
    (Athletics: (Value: 1; Chance: 7); Dodge: (Value: 1; Chance: 5);
     Concentration: (Value: 2; Chance: 100); Bodybuilding: (Value: 0; Chance: 0);
     Meditation: (Value: 1; Chance: 5)),
    // Wand
    (Athletics: (Value: 1; Chance: 1); Dodge: (Value: 1; Chance: 5);
     Concentration: (Value: 1; Chance: 90); Bodybuilding: (Value: 0; Chance: 0);
     Meditation: (Value: 1; Chance: 1)),
    // Bow
    (Athletics: (Value: 1; Chance: 60); Dodge: (Value: 3; Chance: 100);
     Concentration: (Value: 0; Chance: 0); Bodybuilding: (Value: 1; Chance: 20);
     Meditation: (Value: 0; Chance: 0))
  );

  { TSkills }

procedure TSkills.Clear;
var
  I: TSkillEnum;
begin
  for I := Low(TSkillEnum) to High(TSkillEnum) do
    with FSkill[I] do
    begin
      Value := SkillMin;
      Exp := 0;
    end;
end;

constructor TSkills.Create;
var
  I: TSkillEnum;
  P: Pointer;
begin
  Self.Clear;
  P := TypeInfo(TSkillEnum);
  for I := Low(TSkillEnum) to High(TSkillEnum) do
    FSkillName[I] := GetEnumName(P, Ord(I)).GetName('sk');
end;

destructor TSkills.Destroy;
begin

  inherited;
end;

procedure TSkills.DoSkill(ASkill: TSkillEnum; AExpValue: UInt);
begin
  if AExpValue = 0 then
    Exit;
  if (Skill[ASkill].Value < SkillMax) and (ASkill <> skNone) then
  begin
    FSkill[ASkill].Exp := FSkill[ASkill].Exp + Math.RandomRange(0,
      AExpValue + 1) + 1;
    if (Skill[ASkill].Exp >= GetSkillExpMax) then
    begin
      FSkill[ASkill].Exp := FSkill[ASkill].Exp - GetSkillExpMax;
      Inc(FSkill[ASkill].Value);
      FSkill[ASkill].Value := EnsureRange(FSkill[ASkill].Value, SkillMin,
        SkillMax);
      // Add message {!!!}
      MsgLog.Add(Terminal.Colorize(Format('Your skill %s has raised to %d!',
        [GetName(ASkill), FSkill[ASkill].Value]), clAlarm));
      // Add exp
      Player.AddExp();
      // Add scores
      if (FSkill[ASkill].Value = SkillMax) then
        Player.Statictics.Inc(stScore, 50);
      Player.Calc;
    end;
  end;
end;

procedure TSkills.DoSkillChance(ASkill: TSkillEnum; AChance: UInt);
begin
  if Utils.Chance(AChance) then
    DoSkill(ASkill);
end;

procedure TSkills.TryAwarenessSkill;
const
  CBaseAwarenessSkillExp = 3;
  CRangerAwarenessSkillBonus = 1;
  CThiefAwarenessSkillBonus = 2;
  CGnomeAwarenessSkillBonus = 1;
  CHellAwarenessFailChance = 25; {25%}
var
  LExpValue: UInt;
begin
  if (Game.Difficulty = dfHell) and Utils.Chance(CHellAwarenessFailChance) then
    Exit;
  LExpValue := Math.EnsureRange(CBaseAwarenessSkillExp - Ord(Game.Difficulty),
    1, CBaseAwarenessSkillExp);
  if (Player.HClass = clRanger) then
    Inc(LExpValue, CRangerAwarenessSkillBonus);
  if (Player.HClass = clThief) then
    Inc(LExpValue, CThiefAwarenessSkillBonus);
  if (Player.HRace = rcGnome) then
    Inc(LExpValue, CGnomeAwarenessSkillBonus);
  Self.DoSkill(skAwareness, LExpValue);
end;

procedure TSkills.TryWeaponSkill(ASkill: TSkillEnum; AValue, AChance: UInt);
begin
  if (AValue = 0) or (AChance = 0) then
    Exit;
  if Utils.Chance(AChance) then
    Self.DoSkill(ASkill, AValue);
end;

procedure TSkills.TryDodgeSkill;
const
  CElfDodgeSkillBonus = 1;
var
  LValue, LChance: UInt;
begin
  if not (Player.WeaponSkill in [skBlade .. skBow]) then
    Exit;
  with CWeaponSkillTable[Player.WeaponSkill].Dodge do
  begin
    LValue := Value;
    LChance := Chance;
  end;
  if (Player.HRace = rcElf) then
    if (LChance <> CFullSkillChance) then
      Inc(LChance)
    else
      Inc(LValue, CElfDodgeSkillBonus);
  if (Player.HRace = rcDwarf) and (LChance = CFullSkillChance) and
    (LValue >= 2) then
    Dec(LValue);
  Self.TryWeaponSkill(skDodge, LValue, LChance);
end;

procedure TSkills.TryAthleticsSkill;
const
  CDwarfAthleticsSkillBonus = 1;
var
  LValue, LChance: UInt;
begin
  if not (Player.WeaponSkill in [skBlade .. skBow]) then
    Exit;
  with CWeaponSkillTable[Player.WeaponSkill].Athletics do
  begin
    LValue := Value;
    LChance := Chance;
  end;
  if (Player.HRace = rcElf) and (LChance = CFullSkillChance) and
    (LValue >= 2) then
    Dec(LValue);
  if (Player.HRace = rcDwarf) then
    if (LChance <> CFullSkillChance) then
      Inc(LChance, CDwarfAthleticsSkillBonus)
    else
      Inc(LValue, CDwarfAthleticsSkillBonus);
  Self.TryWeaponSkill(skAthletics, LValue, LChance);
end;

procedure TSkills.TryConcentrationSkill;
begin
  if Player.WeaponSkill in [skBlade .. skBow] then
    with CWeaponSkillTable[Player.WeaponSkill].Concentration do
      Self.TryWeaponSkill(skConcentration, Value, Chance);
end;

procedure TSkills.TryBodybuildingSkill;
begin
  if Player.WeaponSkill in [skBlade .. skBow] then
    with CWeaponSkillTable[Player.WeaponSkill].Bodybuilding do
      Self.TryWeaponSkill(skBodybuilding, Value, Chance);
end;

procedure TSkills.TryMeditationSkill;
begin
  if Player.WeaponSkill in [skBlade .. skBow] then
    with CWeaponSkillTable[Player.WeaponSkill].Meditation do
      Self.TryWeaponSkill(skMeditation, Value, Chance);
end;

procedure TSkills.DoWeaponSkill;
begin
  Self.DoSkill(Player.WeaponSkill, 2);
  Self.TryAthleticsSkill;
  Self.TryConcentrationSkill;
  Self.TryDodgeSkill;
  Self.TryAwarenessSkill;
  Self.TryBodybuildingSkill;
  Self.TryMeditationSkill;
end;

function TSkills.GetName(I: TSkillEnum): string;
begin
  Result := FSkillName[I];
end;

function TSkills.GetSkill(I: TSkillEnum): TSkill;
begin
  Result := FSkill[I];
end;

procedure TSkills.Modify(I: TSkillEnum; Value: Int);
begin
  FSkill[I].Value := Math.EnsureRange(FSkill[I].Value + Value, SkillMin, SkillMax);
end;

procedure TSkills.SetSkill(I: TSkillEnum; const Value: TSkill);
begin
  FSkill[I] := Value;
end;

class function TSkills.GetSkillExpMax: UInt;
begin
  Result := 50 + (Ord(Game.Difficulty) * 50 div 3);
end;

end.
