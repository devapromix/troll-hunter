unit Trollhunter.Trap;

interface

uses
  Trollhunter.Entity;

type
  TBaseTrap = class(TEntity)
  private

  public

  end;

type
  TTrap = class(TBaseTrap)
  private
    procedure Miss;
    procedure Damage(ADamage: Integer);
  public
    procedure Fire;
    procedure Lightning;
  end;

implementation

uses
  SysUtils,
  Trollhunter.Map,
  Trollhunter.Graph,
  Trollhunter.Creatures,
  Trollhunter.Utils,
  Trollhunter.Log,
  Trollhunter.Lang,
  Trollhunter.Decorator,
  Trollhunter.Projectiles;

procedure TTrap.Fire;
var
  D: Integer;
begin
  if (Map.Level > 0) then
  begin
    D := (Rand(1, Map.Level * 3) * (100 - Creatures.PC.Prop.Protect)) div 100;
    if (D > 0) then
      Damage(D)
    else
      Miss;
  end;
end;

procedure TTrap.Lightning;
var
  D: Integer;
begin
  if (Map.Level > 0) then
  begin
    D := (Rand(1, Map.Level * 5) * (100 - Creatures.PC.Prop.Protect)) div 100;
    if (D > 0) then
      Damage(D)
    else
      Miss;
  end;
end;

procedure TTrap.Miss;
begin
  Log.Add(GetLang(78));
end;

procedure TTrap.Damage(ADamage: Integer);
begin
  if (ADamage < 0) then
    ADamage := 0;
  with TAnimNumber.Create(-ADamage) do
    Free;
  Creatures.PC.Life.Dec(ADamage);
  Log.Add(Format(GetLang(77), [ADamage]));
end;

end.
