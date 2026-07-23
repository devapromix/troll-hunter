unit Trollhunter.Projectile;

interface

uses
  Trollhunter.Skill;

type
  TSymbol = record
    Symbol: char;
    Color: cardinal;
  end;

  { TProjectile }

  TProjectile = class
  private
  public
    constructor Create;
    destructor Destroy; override;
    function GetSymbol(const AX, AY: integer; const ASkillEnum: TSkillEnum): TSymbol;
  end;

var
  Projectile: TProjectile;

implementation

uses
  Math,
  SysUtils,
  BearLibTerminal,
  Trollhunter.Game,
  Trollhunter.Map;

  { TProjectile }

constructor TProjectile.Create;
begin
  inherited Create;
end;

destructor TProjectile.Destroy;
begin
  inherited Destroy;
end;

function TProjectile.GetSymbol(const AX, AY: integer;
  const ASkillEnum: TSkillEnum): TSymbol;
begin
  if ASkillEnum = skWand then
  begin
    Result.Color := clRed;
    Result.Symbol := '*';
  end;
  if ASkillEnum = skBow then
  begin
    Result.Color := clYellow;
    if (aX = 0) then
      Result.Symbol := '|'
    else if (aY = 0) then
      Result.Symbol := '-'
    else if (aX = aY) then
      Result.Symbol := '\'
    else
      Result.Symbol := '/';
  end;
end;

initialization
  Projectile := TProjectile.Create;

finalization
  FreeAndNil(Projectile);

end.
