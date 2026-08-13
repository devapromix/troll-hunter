unit Trollhunter.Ability;

interface

uses
  Trollhunter.Types,
  Trollhunter.Item.Types;

{ #todo : Hotkeys 1-9 for abilities. }
{ #todo : Scene with abilities. }
{ #todo : Talents with abilities. }
{ TODO -cMage : Arcane Focus
  Mage active ability.

  - Grants the Arcane Focus status effect for 70 turns.
  - While active, the caster regenerates additional mana at the end of each turn.
  - Mana regeneration may scale with the skill level
    (e.g. +1...+5 MP per turn).
  - Recasting the ability refreshes the duration instead of stacking.
  - When the effect expires, the bonus mana regeneration ends.

  Possible upgrades:
  - Increase duration.
  - Increase mana regenerated per turn.
  - Reduce spell mana costs while Arcane Focus is active.
  - Chance for spells to consume no mana.
  - Temporarily increase maximum mana while the effect lasts.
}

type
  TAbilityEnum = (abFind_Item, abConjure_Mana_Orb, abCripling_Blow, abStealth);

type
  TAbilityBase = record
    Name: string;
    ManaCost: UInt;
  end;

const
  AbilityBase: array [TAbilityEnum] of TAbilityBase = (
    // Warrior - Find Item
    (Name: 'Find item'; ManaCost: 12;),
    // Mage - Conjure Mana Orb
    (Name: 'Conjure Mana Orb'; ManaCost: 65;),
    // Ranger - Cripling Blow
    (Name: 'Cripling Blow'; ManaCost:15;),
    // Thief - Stealth
    (Name: 'Stealth'; ManaCost: 35;)
    );

type

  { TAbility }

  TAbility = class
  private
    function TrySpendMana(AAbility: TAbilityEnum): boolean;
  public
    procedure UseAbility;
    procedure FindItem;
    procedure ConjureManaOrb;
    procedure CriplingBlow;
    procedure Stealth;
  end;

var
  Ability: TAbility;

implementation

uses
  Math,
  SysUtils,
  Trollhunter.Game,
  Trollhunter.Map,
  Trollhunter.Attribute,
  Trollhunter.Player,
  Trollhunter.Player.Classes,
  Trollhunter.Item,
  Trollhunter.Item.Common,
  Trollhunter.Item.Dungeon,
  Trollhunter.StatusEffect,
  Trollhunter.Terminal,
  Trollhunter.UI.Log;

  { TAbility }

procedure TAbility.UseAbility;
begin
  if Player.IsDead then
    Exit;
  case Player.HClass of
    clWarrior:
      FindItem;
    clMage:
      ConjureManaOrb;
    clRanger:
      CriplingBlow;
    clThief:
      Stealth;
  end;
end;

function TAbility.TrySpendMana(AAbility: TAbilityEnum): boolean;
begin
  Result := Player.Attributes.Attrib[atMana].Value >= AbilityBase[AAbility].ManaCost;
  if Result then
    Player.Attributes.Modify(atMana, -AbilityBase[AAbility].ManaCost)
  else
    MsgLog.Add('You don''t have enough mana to use this ability.');
end;

procedure TAbility.FindItem;
var
  I, LCount: Int;
  LItem: Item;
  LHasCorpse: boolean;
begin
  LHasCorpse := False;
  LCount := Items_Dungeon_GetMapCountXY(Ord(Map.Current), Player.X, Player.Y);
  for I := LCount - 1 downto 0 do
  begin
    LItem := Items_Dungeon_GetMapItemXY(Ord(Map.Current), I, Player.X, Player.Y);
    if (ItemBase[TItemEnum(LItem.ItemID)].ItemType in CorpseTypeItems) then
    begin
      LHasCorpse := True;
      Break;
    end;
  end;

  if not LHasCorpse then
  begin
    MsgLog.Add('There is no corpse here to examine.');
    Exit;
  end;

  if not TrySpendMana(abFind_Item) then
    Exit;

  Items_Dungeon_DeleteMapItemXY(Ord(Map.Current), I, Player.X, Player.Y, LItem);

  if (Math.RandomRange(0, 5) = 0) then
  begin
    Items.Add(Map.Current, Player.X, Player.Y);
    MsgLog.Add('You found something.');
  end
  else
    MsgLog.Add('You didn''t find anything.');

  Player.Wait;
end;

procedure TAbility.ConjureManaOrb;
begin
  if not TrySpendMana(abConjure_Mana_Orb) then
    Exit;

  Items.Loot(Player.X, Player.Y, itmMana_Orb);
  MsgLog.Add('You have conjured a mana orb.');

  Player.Wait;
end;

procedure TAbility.CriplingBlow;
const
  CAimingTurns = 18;
begin
  if not TrySpendMana(abCripling_Blow) then
    Exit;

  Player.StatusEffects.Modify(seAiming, CAimingTurns);
  MsgLog.Add('Your aim sharpens.');

  Player.Wait;
end;

procedure TAbility.Stealth;
const
  CStealthTurns = 15;
begin
  if Player.StatusEffects.IsStatusEffect(seStealth) then
  begin
    Player.StatusEffects.StatusEffect[seStealth] := 0;
    MsgLog.Add('You step out of the shadows.');
    Exit;
  end;

  if not TrySpendMana(abStealth) then
    Exit;

  Player.StatusEffects.Modify(seStealth, CStealthTurns);
  MsgLog.Add('You hide in the shadows.');

  Player.Wait;
end;

initialization

  Ability := TAbility.Create;

finalization

  FreeAndNil(Ability);

end.
