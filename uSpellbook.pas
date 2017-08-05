unit uSpellbook;

interface

type
  TSpellEnum = (spHeal, spCurePoison, spTeleportation);

type
  TSpellBase = record
    Level: Byte;
    ManaCost: Byte;
  end;

const
  SpellBase: array [TSpellEnum] of TSpellBase = (
  // Heal
  (Level: 1; ManaCost: 20;),
  // Cure Poison
  (Level: 2; ManaCost: 30;),
  // Teleportation
  (Level: 3; ManaCost: 40;)
  );

type
  TSpell = record
    Enable: Boolean;
    Level: Byte;
    ManaCost: Byte;
  end;

type
  TSpellbook = class(TObject)
  private
    FSpell: array[TSpellEnum] of TSpell;
  public
    procedure Clear;
    function GetSpellName(ASpellEnum: TSpellEnum): string;
    procedure AddSpell(ASpellEnum: TSpellEnum);
    function GetSpell(ASpellEnum: TSpellEnum): TSpell;
    procedure Start;
  end;

var
  Spellbook: TSpellbook = nil;

implementation

uses Math, SysUtils, GNUGetText, uGame;

{ TSpellbook }

procedure TSpellbook.AddSpell(ASpellEnum: TSpellEnum);
begin
  FSpell[ASpellEnum].Enable := True;
end;

procedure TSpellbook.Clear;
var
  I: TSpellEnum;
begin
  for I := Low(TSpellEnum) to High(TSpellEnum) do
    FSpell[I].Enable := False;
end;

function TSpellbook.GetSpell(ASpellEnum: TSpellEnum): TSpell;
begin
  Result := FSpell[ASpellEnum];
end;

function TSpellbook.GetSpellName(ASpellEnum: TSpellEnum): string;
begin
  case ASpellEnum of
    spHeal:
      Result := _('Heal');
    spCurePoison:
      Result := _('Cure poison');
    spTeleportation:
      Result := _('Teleportation');
  end;
end;

procedure TSpellbook.Start;
var
  I: TSpellEnum;
begin
  Self.Clear;
  for I := Low(TSpellEnum) to High(TSpellEnum) do
  with FSpell[I] do
  begin
    Level := SpellBase[I].Level;
    ManaCost := SpellBase[I].ManaCost + Math.RandomRange(Ord(Game.Difficulty),
      Ord(Game.Difficulty) * Ord(Game.Difficulty));
  end;
end;

initialization

Spellbook := TSpellbook.Create;

finalization

FreeAndNil(Spellbook);

end.
