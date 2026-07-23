unit Trollhunter.Spellbook;

interface

uses
  Trollhunter.Types,
  Trollhunter.Magic,
  Trollhunter.Creature;

type
  TSpell = record
    Enable: boolean;
    Spell: TSpellData;
  end;

type

  { TSpellbook }

  TSpellbook = class(TObject)
  private
    FSpell: array [TSpellEnum] of TSpell;
  public
    procedure Clear;
    function GetSpellName(ASpellEnum: TSpellEnum): string;
    procedure AddSpell(ASpellEnum: TSpellEnum);
    function GetSpell(ASpellEnum: TSpellEnum): TSpell;
    procedure Start;
    procedure DoSpell(Index: UInt);
    function GetQuickSpell: TSpell;
  end;

var
  Spellbook: TSpellbook = nil;

implementation

uses
  SysUtils,
  Trollhunter.Player,
  Trollhunter.UI.Log,
  Trollhunter.Statistic,
  Trollhunter.UI,
  Trollhunter.Attribute;

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
    FSpell[I].Enable := True;
end;

procedure TSpellbook.DoSpell(Index: UInt);
var
  C: UInt;
  I: TSpellEnum;
begin
  C := 0;
  for I := Low(TSpellEnum) to High(TSpellEnum) do
    if FSpell[I].Enable then
    begin
      if (Index = C) then
      begin
        if (Player.Attributes.Attrib[atMana].Value >= FSpell[I].Spell.ManaCost)
        then
        begin
          Player.Statictics.Inc(stSpCast);
          Player.Attributes.Modify(atMana, -FSpell[I].Spell.ManaCost);
          //Player.DoEffects(FSpell[I].Spell.Effects, FSpell[I].Spell.Value);
        end
        else
        begin
          MsgLog.Add('You need more mana!');
          Player.Calc;
          Player.Wait;
        end;
        Exit;
      end;
      Inc(C);
    end;
end;

function TSpellbook.GetQuickSpell: TSpell;
begin
  //No quick spell selected.
end;

function TSpellbook.GetSpell(ASpellEnum: TSpellEnum): TSpell;
begin
  Result := FSpell[ASpellEnum];
end;

function TSpellbook.GetSpellName(ASpellEnum: TSpellEnum): string;
begin
  Result := SpellData[ASpellEnum].Name;
end;

procedure TSpellbook.Start;
var
  I: TSpellEnum;
begin
  Self.Clear;
end;

initialization

  Spellbook := TSpellbook.Create;

finalization

  FreeAndNil(Spellbook);

end.
