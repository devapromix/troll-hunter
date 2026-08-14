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

type
  TDodgeSkillInfo = record
    Value: UInt;
    Chance: UInt;
  end;

const
  SkillMin = 5;
  SkillMax = 75;
  BeginSkill = 10;
  StartSkill = 5;
  TalentSkill = 1;

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

const
  CAthleticsSkill: array [skBlade .. skBow] of TDodgeSkillInfo = (
    (Value: 2; Chance: 0),    // skBlade
    (Value: 3; Chance: 0),    // skAxe
    (Value: 0; Chance: 15),   // skSpear
    (Value: 0; Chance: 10),   // skDagger
    (Value: 4; Chance: 0),    // skMace
    (Value: 0; Chance: 2),    // skStaff
    (Value: 0; Chance: 2),    // skWand
    (Value: 0; Chance: 60));  // skBow
  CDodgeSkill: array [skBlade .. skBow] of TDodgeSkillInfo = (
    (Value: 2; Chance: 0),    // skBlade
    (Value: 0; Chance: 30),   // skAxe
    (Value: 3; Chance: 0),    // skSpear
    (Value: 0; Chance: 15),   // skMace
    (Value: 3; Chance: 0),    // skDagger
    (Value: 0; Chance: 5),    // skStaff
    (Value: 0; Chance: 5),    // skWand
    (Value: 3; Chance: 0));   // skBow
  CConcentrationSkill: array [skBlade .. skBow] of TDodgeSkillInfo = (
    (Value: 0; Chance: 0),    // skBlade
    (Value: 0; Chance: 0),    // skAxe
    (Value: 0; Chance: 0),    // skSpear
    (Value: 0; Chance: 0),    // skDagger
    (Value: 0; Chance: 0),    // skMace
    (Value: 2; Chance: 0),    // skStaff
    (Value: 1; Chance: 0),    // skWand
    (Value: 0; Chance: 0));   // skBow
  CBodybuildingSkill: array [skBlade .. skBow] of UInt = (8, 10, 10, 4, 12, 0, 0, 10);
  CMeditationSkill: array [skBlade .. skBow] of UInt = (0, 0, 0, 0, 0, 5, 1, 0);

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

procedure TSkills.TryDodgeSkill;
const
  CElfDodgeSkillBonus = 1;
var
  LValue, LChance: UInt;
begin
  if not (Player.WeaponSkill in [skBlade .. skBow]) then
    Exit;
  with CDodgeSkill[Player.WeaponSkill] do
  begin
    LValue := Value;
    LChance := Chance;
    if (Player.HRace = rcElf) then
      if (LChance <> 0) then
        Inc(LChance)
      else
        Inc(LValue, CElfDodgeSkillBonus);
    if (Player.HRace = rcDwarf) and (LChance = 0) and (LValue >= 2) then
      Dec(LValue);
  end;
  if LChance <> 0 then
    Self.DoSkillChance(skDodge, LChance)
  else
    Self.DoSkill(skDodge, LValue);
end;

procedure TSkills.TryAthleticsSkill;
const
  CDwarfAthleticsSkillBonus = 1;
var
  LValue, LChance: UInt;
begin
  if not (Player.WeaponSkill in [skBlade .. skBow]) then
    Exit;
  with CAthleticsSkill[Player.WeaponSkill] do
  begin
    LValue := Value;
    LChance := Chance;
  end;
  if (Player.HRace = rcElf) and (LChance = 0) and (LValue >= 2) then
    Dec(LValue);
  if (Player.HRace = rcDwarf) then
    if (LChance <> 0) then
      Inc(LChance, CDwarfAthleticsSkillBonus)
    else
      Inc(LValue, CDwarfAthleticsSkillBonus);
  if LChance <> 0 then
    Self.DoSkillChance(skAthletics, LChance)
  else
    Self.DoSkill(skAthletics, LValue);
end;

procedure TSkills.TryConcentrationSkill;
begin
  if Player.WeaponSkill in [skBlade .. skBow] then
    with CConcentrationSkill[Player.WeaponSkill] do
      if Chance <> 0 then
        Self.DoSkillChance(skConcentration, Chance)
      else
        Self.DoSkill(skConcentration, Value);
end;

procedure TSkills.TryBodybuildingSkill;
begin
  if Player.WeaponSkill in [skBlade .. skBow] then
    Self.DoSkillChance(skBodybuilding, CBodybuildingSkill[Player.WeaponSkill]);
end;

procedure TSkills.TryMeditationSkill;
begin
  if Player.WeaponSkill in [skBlade .. skBow] then
    Self.DoSkillChance(skMeditation, CMeditationSkill[Player.WeaponSkill]);
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
