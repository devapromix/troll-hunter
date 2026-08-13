unit Trollhunter.Skill;

interface

uses
  Trollhunter.Types;

type
  TSkillEnum = (
    skNone,
    //
    skStealth,
    // Attributes skills
    skAthletics, skDodge, skConcentration, skToughness, skAwareness,
    //
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
    procedure DoAwarenessSkill;
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

procedure TSkills.DoAwarenessSkill;
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

procedure TSkills.DoWeaponSkill;
begin
  Self.DoAwarenessSkill;
  case Player.WeaponSkill of
    skBlade:
    begin
      Self.DoSkill(Player.WeaponSkill, 2);
      Self.DoSkill(skAthletics, 2);
      Self.DoSkill(skDodge, 2);
      Self.DoSkillChance(skBodybuilding, 8);
      Player.SatPerTurn := Ord(Game.Difficulty) + 5;
    end;
    skAxe:
    begin
      Self.DoSkill(Player.WeaponSkill, 2);
      Self.DoSkill(skAthletics, 3);
      Self.DoSkillChance(skDodge, 75);
      Self.DoSkillChance(skBodybuilding, 10);
      Player.SatPerTurn := Ord(Game.Difficulty) + 6;
    end;
    skSpear:
    begin
      Self.DoSkill(Player.WeaponSkill, 2);
      Self.DoSkillChance(skAthletics, 15);
      Self.DoSkill(skDodge, 2);
      Self.DoSkillChance(skBodybuilding, 10);
      Player.SatPerTurn := Ord(Game.Difficulty) + 4;
    end;
    skDagger:
    begin
      Self.DoSkill(Player.WeaponSkill, 2);
      Self.DoSkillChance(skAthletics, 10);
      Self.DoSkill(skDodge, 3);
      Self.DoSkillChance(skBodybuilding, 4);
      Player.SatPerTurn := Ord(Game.Difficulty) + 3;
    end;
    skMace:
    begin
      Self.DoSkill(Player.WeaponSkill, 2);
      Self.DoSkill(skAthletics, 4);
      Self.DoSkillChance(skBodybuilding, 12);
      Player.SatPerTurn := Ord(Game.Difficulty) + 7;
    end;
    skStaff:
    begin
      Self.DoSkill(Player.WeaponSkill, 2);
      Self.DoSkillChance(skDodge, 25);
      Self.DoSkill(skConcentration, 2);
      Self.DoSkillChance(skMeditation, 5);
      Player.SatPerTurn := Ord(Game.Difficulty) + 8;
    end;
    skWand:
    begin
      Self.DoSkill(Player.WeaponSkill, 2);
      Self.DoSkillChance(skDodge, 5);
      Self.DoSkill(skConcentration);
      Self.DoSkillChance(skMeditation);
      Player.SatPerTurn := Ord(Game.Difficulty) + 6;
    end;
    skBow:
    begin
      Self.DoSkill(Player.WeaponSkill, 2);
      Self.DoSkill(skDodge, 3);
      Self.DoSkillChance(skAthletics, 60);
      Self.DoSkillChance(skBodybuilding, 10);
      Player.SatPerTurn := Ord(Game.Difficulty) + 4;
    end;
  end;
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
