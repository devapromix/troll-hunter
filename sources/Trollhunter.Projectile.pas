unit Trollhunter.Projectile;

interface

uses
  Trollhunter.Game,
  Trollhunter.Skill,
  Trollhunter.Spell,
  Trollhunter.Projectile.Types;

type
  TSymbol = record
    Symbol: char;
    Color: cardinal;
  end;

type
  TProjectileData = record
    Color: cardinal;
    Symbol: char; // ignored for prArrow, whose symbol depends on direction
  end;

const
  ProjectileData: array [TProjectileEnum] of TProjectileData = (
    (Color: 0;            Symbol: #0), // prNone
    (Color: clRed;        Symbol: '*'), // prWandBolt
    (Color: clYellow;     Symbol: '/'), // prArrow
    (Color: clRed;        Symbol: '~'), // prFireArrow
    (Color: clLightGreen; Symbol: '*')  // prVerdantSpear
  );

  { TProjectile }

  type
  TProjectile = class
  private
  public
    constructor Create;
    destructor Destroy; override;
    function GetSymbol(const AX, AY: integer; const ASkillEnum: TSkillEnum): TSymbol;
    function GetSpellSymbol(const ASpellEnum: TSpellEnum): TSymbol;
  end;

var
  Projectile: TProjectile;

implementation

uses
  SysUtils;

  { TProjectile }

constructor TProjectile.Create;
begin
  inherited Create;
end;

destructor TProjectile.Destroy;
begin
  inherited Destroy;
end;

function SkillToProjectile(const ASkillEnum: TSkillEnum): TProjectileEnum;
begin
  case ASkillEnum of
    skWand: Result := prWandBolt;
    skBow:  Result := prArrow;
  else
    Result := prNone;
  end;
end;

function TProjectile.GetSymbol(const AX, AY: integer;
  const ASkillEnum: TSkillEnum): TSymbol;
var
  LProjectileEnum: TProjectileEnum;
begin
  LProjectileEnum := SkillToProjectile(ASkillEnum);
  Result.Color := ProjectileData[LProjectileEnum].Color;
  if LProjectileEnum = prArrow then
  begin
    // Arrow symbol depends on the direction it's flying, not a fixed glyph
    if (AX = 0) then
      Result.Symbol := '|'
    else if (AY = 0) then
      Result.Symbol := '-'
    else if (AX = AY) then
      Result.Symbol := '\'
    else
      Result.Symbol := '/';
  end
  else
    Result.Symbol := ProjectileData[LProjectileEnum].Symbol;
end;

function TProjectile.GetSpellSymbol(const ASpellEnum: TSpellEnum): TSymbol;
var
  LProjectileEnum: TProjectileEnum;
begin
  LProjectileEnum := GetSpellData(ASpellEnum).Projectile;
  Result.Color := ProjectileData[LProjectileEnum].Color;
  Result.Symbol := ProjectileData[LProjectileEnum].Symbol;
end;

initialization
  Projectile := TProjectile.Create;

finalization
  FreeAndNil(Projectile);

end.
