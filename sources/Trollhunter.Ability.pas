unit Trollhunter.Ability;

interface

uses
  Trollhunter.Types,
  Trollhunter.Item.Types;

type
  TAbilityEnum = (abFind_Item, abConjure_Mana_Orb);

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
    (Name: 'Conjure Mana Orb'; ManaCost: 65;)
    //
    );

type

  { TAbility }

  TAbility = class
  public
    procedure UseAbility();
    procedure FindItem();
    procedure ConjureManaOrb;
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
  Trollhunter.Terminal,
  Trollhunter.UI.Log;

  { TAbility }

procedure TAbility.UseAbility();
begin
  if Player.IsDead then
    Exit;
  case Player.HClass of
    clWarrior:
      FindItem;
    clMage:
      ConjureManaOrb;
    clRanger:
    begin

    end;
    clThief:
    begin

    end;
  end;
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

  if (Player.Attributes.Attrib[atMana].Value < AbilityBase[abFind_Item].ManaCost) then
  begin
    MsgLog.Add('You don''t have enough mana to use this ability.');
    Exit;
  end;

  Items_Dungeon_DeleteMapItemXY(Ord(Map.Current), I, Player.X, Player.Y, LItem);
  Player.Attributes.Modify(atMana, -AbilityBase[abFind_Item].ManaCost);

  if (Math.RandomRange(0, 5) = 0) then
  begin
    Items.Add(Map.Current, Player.X, Player.Y);
    MsgLog.Add(Terminal.Colorize('You found something.', clAlarm));
  end
  else
    MsgLog.Add('You didn''t find anything.');

  Player.Wait;
end;

procedure TAbility.ConjureManaOrb;
begin
  if (Player.Attributes.Attrib[atMana].Value <
    AbilityBase[abConjure_Mana_Orb].ManaCost) then
  begin
    MsgLog.Add('You don''t have enough mana to use this ability.');
    Exit;
  end;

  Player.Attributes.Modify(atMana, -AbilityBase[abConjure_Mana_Orb].ManaCost);
  Items.Loot(Player.X, Player.Y, itmMana_Orb);
  MsgLog.Add('You have conjured a mana orb.');

  Player.Wait;
end;

initialization

  Ability := TAbility.Create;

finalization

  FreeAndNil(Ability);

end.
