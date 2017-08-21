unit uSkill;

interface

type
  TSkillEnum = (skLearning,
    // Attributes skills
    skAthletics, skDodge, skConcentration, skToughness,
    // Weapon skills
    skBlade, skAxe, skSpear, skMace,
    // Skills
    skStealth, skHealing);

type
  TSkill = record
    Value: Integer;
    Exp: Integer;
  end;

type
  TSkills = class(TObject)
  private
    FSkill: array [TSkillEnum] of TSkill;
    function GetSkill(I: TSkillEnum): TSkill;
    procedure SetSkill(I: TSkillEnum; const Value: TSkill);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Skill[I: TSkillEnum]: TSkill read GetSkill write SetSkill;
    function GetName(I: TSkillEnum): string;
  end;

const
  SkillMin = 5;
  SkillMax = 75;
  SkillExpMax = 50;
  StartSkill = 5;

implementation

uses SysUtils, GNUGetText;

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
begin
  Self.Clear
end;

destructor TSkills.Destroy;
begin

  inherited;
end;

function TSkills.GetName(I: TSkillEnum): string;
begin
  case I of
    skLearning:
      Result := _('Learning');
    // Attributes skills
    skAthletics:
      Result := _('Athletics');
    skDodge:
      Result := _('Dodge');
    skConcentration:
      Result := _('Concentration');
    skToughness:
      Result := _('Toughness');
    // Weapon skills
    skBlade:
      Result := _('Blade');
    skAxe:
      Result := _('Axe');
    skSpear:
      Result := _('Spear');
    skMace:
      Result := _('Mace');
    // Skills
    skStealth:
      Result := _('Stealth');
    skHealing:
      Result := _('Healing');
  end;
end;

function TSkills.GetSkill(I: TSkillEnum): TSkill;
begin
  Result := FSkill[I]
end;

procedure TSkills.SetSkill(I: TSkillEnum; const Value: TSkill);
begin
  FSkill[I] := Value
end;

end.
