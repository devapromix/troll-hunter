unit Trollhunter.Spellbook;

interface

uses
  Trollhunter.Types,
  Trollhunter.Spell,
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
    FQuickSpell: TSpellEnum;
    FHasQuickSpell: boolean;
    FLastSelectedSpell: TSpellEnum;
    procedure CastSpell(ASpellEnum: TSpellEnum);
  public
    procedure Clear;
    procedure AddSpell(ASpellEnum: TSpellEnum);
    function GetSpell(ASpellEnum: TSpellEnum): TSpell;
    procedure SetQuickSpell(ASpellEnum: TSpellEnum);
    procedure ClearQuickSpell;
    function GetSpellByIndex(Index: UInt): TSpellEnum;
    function GetActiveSpellCount: UInt;
    procedure DoSpell(Index: UInt);
    procedure DoQuickSpell;
    function GetQuickSpell: TSpell;
    function GetQuickSpellEnum: TSpellEnum;
    function GetLastSelectedSpell: TSpellEnum;
  end;

var
  Spellbook: TSpellbook = nil;

implementation

uses
  SysUtils,
  Trollhunter.Player,
  Trollhunter.UI.Log,
  Trollhunter.Statistic,
  Trollhunter.Projectile.Types,
  Trollhunter.UI,
  Trollhunter.Attribute;

  { TSpellbook }

procedure TSpellbook.AddSpell(ASpellEnum: TSpellEnum);
begin
  FSpell[ASpellEnum].Enable := True;
  FSpell[ASpellEnum].Spell := GetSpellData(ASpellEnum);
end;

procedure TSpellbook.Clear;
var
  I: TSpellEnum;
begin
  for I := Low(TSpellEnum) to High(TSpellEnum) do
    FSpell[I].Enable := False;
  FQuickSpell := Low(TSpellEnum);
  FHasQuickSpell := False;
  FLastSelectedSpell := Low(TSpellEnum);
end;

procedure TSpellbook.CastSpell(ASpellEnum: TSpellEnum);
begin
  FLastSelectedSpell := ASpellEnum;
  if (FSpell[ASpellEnum].Spell.Projectile <> prNone) then
  begin
    Player.MagicFireModeEnter;
    Exit;
  end;
  if (Player.Attributes.Attrib[atMana].Value >= FSpell[ASpellEnum].Spell.ManaCost)
  then
  begin
    Player.Statictics.Inc(stSpCast);
    Player.Attributes.Modify(atMana, -FSpell[ASpellEnum].Spell.ManaCost);
    Player.DoEffects(FSpell[ASpellEnum].Spell.Effects, FSpell[ASpellEnum].Spell.Value);
  end
  else
  begin
    MsgLog.Add('You need more mana!');
    Player.Calc;
    Player.Wait;
  end;
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
        CastSpell(I);
        Exit;
      end;
      Inc(C);
    end;
end;

procedure TSpellbook.DoQuickSpell;
begin
  if GetQuickSpell.Enable then
    CastSpell(FQuickSpell)
  else
    MsgLog.Add('No quick spell selected.');
end;

function TSpellbook.GetSpellByIndex(Index: UInt): TSpellEnum;
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
        Result := I;
        Exit;
      end;
      Inc(C);
    end;
  Result := Low(TSpellEnum);
end;

function TSpellbook.GetActiveSpellCount: UInt;
var
  I: TSpellEnum;
begin
  Result := 0;
  for I := Low(TSpellEnum) to High(TSpellEnum) do
    if FSpell[I].Enable then
      Inc(Result);
end;

procedure TSpellbook.SetQuickSpell(ASpellEnum: TSpellEnum);
begin
  if FSpell[ASpellEnum].Enable then
  begin
    FQuickSpell := ASpellEnum;
    FHasQuickSpell := True;
  end;
end;

procedure TSpellbook.ClearQuickSpell;
begin
  FHasQuickSpell := False;
end;

function TSpellbook.GetQuickSpell: TSpell;
begin
  Result := FSpell[FQuickSpell];
  Result.Enable := FHasQuickSpell and FSpell[FQuickSpell].Enable;
end;

function TSpellbook.GetQuickSpellEnum: TSpellEnum;
begin
  Result := FQuickSpell;
end;

function TSpellbook.GetLastSelectedSpell: TSpellEnum;
begin
  Result := FLastSelectedSpell;
end;

function TSpellbook.GetSpell(ASpellEnum: TSpellEnum): TSpell;
begin
  Result := FSpell[ASpellEnum];
end;

initialization

  Spellbook := TSpellbook.Create;

finalization

  FreeAndNil(Spellbook);

end.
