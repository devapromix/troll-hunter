unit Trollhunter.Player.Skills;

interface

uses
  Trollhunter.Types;

type
  TSkillEnum = (
    //
    skNone,
    //
    skStealth,
    // Attributes skills
    skAthletics, skDodge, skConcentration, skToughness,
    //
    skTactics, skBodybuilding, skMeditation, skEnchant_Item,
    // Weapon skills
    skBlade, skAxe, skSpear, skMace, skDagger, skStaff, skWand, skBow, skCrossbow);

type
  TSkill = UInt;

type
  TSkillArray<T> = array [TSkillEnum] of T;

type
  TSkills = class(TObject)
  private
    FSkillName: TSkillArray<string>;
    FSkill: TSkillArray<TSkill>;
    procedure SetSkill(I: TSkillEnum; const Value: TSkill);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function GetSkill(I: TSkillEnum): TSkill;
    property Skill[I: TSkillEnum]: TSkill read GetSkill write SetSkill;
    procedure Modify(I: TSkillEnum; Value: Int = 1);
    function GetName(I: TSkillEnum): string;
  end;

implementation

uses
  Math,
  SysUtils,
  EnumHelper,
  Trollhunter.Player,
  Trollhunter.Game,
  Trollhunter.Helpers;

{ TSkills }

procedure TSkills.Clear;
var
  I: TSkillEnum;
begin
  for I := Low(TSkillEnum) to High(TSkillEnum) do
    FSkill[I] := 0;
end;

constructor TSkills.Create;
var
  I: TSkillEnum;
begin
  Self.Clear;
  for I := Low(TSkillEnum) to High(TSkillEnum) do
    FSkillName[I] := Enum<TSkillEnum>.ValueName(I).GetName('sk');
end;

destructor TSkills.Destroy;
begin

  inherited;
end;

function TSkills.GetName(I: TSkillEnum): string;
begin
  Result := FSkillName[I];
end;

function TSkills.GetSkill(I: TSkillEnum): TSkill;
begin
  Result := FSkill[I]
end;

procedure TSkills.Modify(I: TSkillEnum; Value: Int);
begin
  FSkill[I] := Math.EnsureRange(FSkill[I] + Value, SkillMin, SkillMax);
end;

procedure TSkills.SetSkill(I: TSkillEnum; const Value: TSkill);
begin
  FSkill[I] := Value
end;

end.
