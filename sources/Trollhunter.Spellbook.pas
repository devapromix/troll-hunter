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
    FPendingSpell: TSpellEnum;
    FPendingScrollIndex: Int;
    FPendingScrollItemID: Int;
    procedure CastSpell(ASpellEnum: TSpellEnum);
  public
    procedure Clear;
    procedure CancelPendingSpell;
    procedure BeginIdentifyScroll(Index: Int);
    function CanCompletePendingSpell(ASpell: TSpellEnum): Boolean;
    procedure CompletePendingSpell;
    function HasPendingSpell: Boolean;
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
  Trollhunter.Player.Helpers,
  Trollhunter.UI.Log,
  Trollhunter.Statistic,
  Trollhunter.Projectile.Types,
  Trollhunter.UI,
  Trollhunter.Attribute,
  Trollhunter.StatusEffect,
  Trollhunter.Effect,
  Trollhunter.Map,
  Trollhunter.Game,
  Trollhunter.Scenes,
  Trollhunter.Item,
  Trollhunter.Item.Common,
  Trollhunter.Item.Inventory,
  Trollhunter.Item.Types,
  Trollhunter.Skill;

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
  CancelPendingSpell;
  FLastSelectedSpell := Low(TSpellEnum);
end;

procedure TSpellbook.CancelPendingSpell;
begin
  FPendingSpell := spNone;
  FPendingScrollIndex := -1;
  FPendingScrollItemID := -1;
end;

function TSpellbook.HasPendingSpell: Boolean;
begin
  Result := FPendingSpell <> spNone;
end;

procedure TSpellbook.BeginIdentifyScroll(Index: Int);
var
  LScroll: Item;
begin
  if not Items_Inventory_IndexInRange(Index) then
    Exit;
  LScroll := Items_Inventory_GetItem(Index);
  if not (ItemBase[TItemEnum(LScroll.ItemID)].ItemType in ScrollTypeItems) or
    not (efIdentification in LScroll.Effects) then
    Exit;
  CancelPendingSpell;
  FPendingSpell := spIdentify;
  FPendingScrollIndex := Index;
  FPendingScrollItemID := LScroll.ItemID;
  Scenes.SetScene(scIdentification);
end;

function TSpellbook.CanCompletePendingSpell(ASpell: TSpellEnum): Boolean;
var
  LScroll: Item;
  LManaCost: Int;
begin
  Result := True;
  if not HasPendingSpell then
    Exit;
  Result := not Player.IsDead and (FPendingSpell = ASpell);
  if not Result then
    Exit;
  if Player.StatusEffects.IsStatusEffect(seStunned) then
  begin
    Result := False;
    MsgLog.Add('You cannot cast spells while stunned.');
    Exit;
  end;
  if FPendingScrollIndex >= 0 then
  begin
    Result := False;
    if not Items_Inventory_IndexInRange(FPendingScrollIndex) then
    begin
      MsgLog.Add('The selected scroll is no longer available.');
      Exit;
    end;
    LScroll := Items_Inventory_GetItem(FPendingScrollIndex);
    if (LScroll.ItemID <> FPendingScrollItemID) or (LScroll.Amount <= 0) or
      (LScroll.Identify = 0) or not (efIdentification in LScroll.Effects) then
    begin
      MsgLog.Add('The selected scroll is no longer available.');
      Exit;
    end;
    LManaCost := ItemBase[TItemEnum(LScroll.ItemID)].ManaCost;
  end
  else
  begin
    if not Player.HasSpellbook then
    begin
      Result := False;
      MsgLog.Add('You don''t have a spellbook equipped!');
      Exit;
    end;
    LManaCost := GetSpellData(FPendingSpell).ManaCost;
  end;
  Result := Player.Attributes.Attrib[atMana].Value >= LManaCost;
  if not Result then
    MsgLog.Add('You need more mana!');
end;

procedure TSpellbook.CompletePendingSpell;
var
  LScroll: Item;
  LManaCost: Int;
  LScrollName: string;
begin
  if not HasPendingSpell then
    Exit;
  if FPendingScrollIndex >= 0 then
  begin
    LScroll := Items_Inventory_GetItem(FPendingScrollIndex);
    LManaCost := ItemBase[TItemEnum(LScroll.ItemID)].ManaCost;
    LScrollName := Items.GetNameThe(LScroll);
    Dec(LScroll.Amount);
    if Items_Inventory_SetItem(FPendingScrollIndex, LScroll) <= 0 then
      Exit;
    MsgLog.Add(Format('You read %s.', [LScrollName]));
    Player.Statictics.Inc(stScrRead);
    Player.Skills.DoSkill(skConcentration);
  end
  else
    LManaCost := GetSpellData(FPendingSpell).ManaCost;
  CancelPendingSpell;
  Player.Attributes.Modify(atMana, -LManaCost);
  Player.Statictics.Inc(stSpCast);
  Player.Calc;
  Player.Wait;
end;

procedure TSpellbook.CastSpell(ASpellEnum: TSpellEnum);
var
  LSpell: TSpellData;
  LStatus: TStatusEffectEnum;
  LTeleportX, LTeleportY: Int;
  LPreviousDuration: array [TStatusEffectEnum] of Int;
begin
  if Player.IsDead or not FSpell[ASpellEnum].Enable then
    Exit;
  if Player.StatusEffects.IsStatusEffect(seStunned) then
  begin
    MsgLog.Add('You cannot cast spells while stunned.');
    Exit;
  end;
  if not Player.HasSpellbook then
  begin
    MsgLog.Add('You don''t have a spellbook equipped!');
    Exit;
  end;
  CancelPendingSpell;
  FLastSelectedSpell := ASpellEnum;
  LSpell := FSpell[ASpellEnum].Spell;
  if (LSpell.Projectile <> prNone) or (ASpellEnum in TargetedStatusSpells) then
  begin
    Player.MagicFireModeEnter;
    Exit;
  end;
  if Player.Attributes.Attrib[atMana].Value < LSpell.ManaCost then
  begin
    MsgLog.Add('You need more mana!');
    Exit;
  end;
  if ASpellEnum = spTownPortal then
  begin
    if Map.GetTileEnum(Player.X, Player.Y, Map.Current) = teStash then
    begin
      MsgLog.Add('You cannot open a portal on a stash.');
      Exit;
    end;
    if (Map.Current = deDark_Wood) and
      (Player.X = Game.Spawn.X) and (Player.Y = Game.Spawn.Y) then
    begin
      MsgLog.Add('You cannot open a portal at the town arrival point.');
      Exit;
    end;
  end;
  if ASpellEnum in [spIdentify, spInfusion] then
  begin
    FPendingSpell := ASpellEnum;
    Player.DoEffects(LSpell.Effects, LSpell.Value);
    Exit;
  end;
  LTeleportX := -1;
  LTeleportY := -1;
  if (efTeleportation in LSpell.Effects) then
    if not Player.TryGetTeleportTarget(LSpell.Value, LTeleportX, LTeleportY) then
      Exit;
  for LStatus := Low(TStatusEffectEnum) to High(TStatusEffectEnum) do
    LPreviousDuration[LStatus] := Player.StatusEffects.StatusEffect[LStatus];
  Player.Attributes.Modify(atMana, -Int(LSpell.ManaCost));
  Player.Statictics.Inc(stSpCast);
  Player.DoEffects(LSpell.Effects, LSpell.Value, 1, LTeleportX, LTeleportY);
  // The casting turn must not shorten a newly applied timed effect.
  for LStatus := Low(TStatusEffectEnum) to High(TStatusEffectEnum) do
    if (Player.StatusEffects.StatusEffect[LStatus] > LPreviousDuration[LStatus]) or
      ((LStatus = seNatureEye) and (efVision in LSpell.Effects)) then
      Player.StatusEffects.Modify(LStatus, 1);
  Player.Calc;
  Scenes.SetScene(scGame);
  Player.Wait;
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
