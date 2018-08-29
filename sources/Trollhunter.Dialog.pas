unit Trollhunter.Dialog;

interface

uses
  Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneDialog = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

{ TSceneDialog }

uses
  BearLibTerminal,
  Trollhunter.Terminal,
  Trollhunter.Mob,
  Trollhunter.Attribute,
  Trollhunter.Item,
  Trollhunter.Item.Helpers,
  Trollhunter.Item.Shop,
  Trollhunter.Player,
  Trollhunter.Player.Quest,
  Trollhunter.UI,
  Trollhunter.UI.Log,
  Trollhunter.Language,
  Trollhunter.Mob.Types;

procedure TSceneDialog.Render;
var
  V: Int;
  S: string;

  procedure Add(S: string);
  begin
    Inc(Y);
    Terminal.Print(1, Y, UI.KeyToStr(Chr(Y + 95)) + ' ' + S, TK_ALIGN_LEFT);
  end;

begin
  inherited;
  UI.Title(NPCName + ' ' + UI.GoldLeft(Player.Gold));

  UI.FromAToZ;
  Y := 1;

  // Heal
  if (ntHealer_A in NPCType) then
  begin
    V := Player.Attributes.Attrib[atMaxLife].Value - Player.Attributes.Attrib[atLife].Value;
    if (V > 0) then
      S := ' (' + Items.GetInfo('+', V, 'Life') + ' ' + Items.GetItemPrice(Round(V * 1.6)) + ')'
    else
      S := '';
    Add(_('Receive healing') + S);
  end;
  // Shops
  if (ntScrTrader_A in NPCType) then
    Add(_('Buy items (scrolls)'));
  if (ntArmTrader_A in NPCType) then
    Add(_('Buy items (armors)'));
  if (ntShTrader_A in NPCType) then
    Add(_('Buy items (shields)'));
  if (ntHelmTrader_A in NPCType) then
    Add(_('Buy items (helms)'));
  if (ntFoodTrader_A in NPCType) then
    Add(_('Buy items (foods)'));
  if (ntBlacksmith_A in NPCType) then
    Add(_('Repair items'));
  if (ntSmithTrader_B in NPCType) then
    Add(_('Buy items (blacksmith)'));
  if (ntHealTrader_B in NPCType) then
    Add(_('Buy items (healing)'));
  if (ntPotManaTrader_B in NPCType) then
    Add(_('Buy items (items of mana)'));
  if (ntPotTrader_B in NPCType) then
    Add(_('Buy items (potions)'));
  if (ntGlovesTrader_B in NPCType) then
    Add(_('Buy items (gloves)'));
  if (ntTavTrader_B in NPCType) then
    Add(_('Buy items (tavern)'));
  if (ntWpnTrader_B in NPCType) then
    Add(_('Buy items (weapons)'));
  if (ntGemTrader_C in NPCType) then
    Add(_('Buy items (gems)'));
  if (ntJewTrader_C in NPCType) then
    Add(_('Buy items (amulets and rings)'));
  if (ntBootsTrader_C in NPCType) then
    Add(_('Buy items (boots)'));
  if (ntSell_C in NPCType) then
    Add(_('Sell items'));
  if (ntRuneTrader_D in NPCType) then
    Add(_('Buy items (runes)'));
  // Quests
  if (ntQuest_D in NPCType) then
    Add(_('The Hunt (quest)'));
  MsgLog.Render(2, True);

  AddKey('Esc', _('Close'), True);
end;

procedure TSceneDialog.Update(var Key: UInt);

  procedure AddShop(AShop: TShopEnum);
  begin
    Shops.Current := AShop;
    Scenes.SetScene(scBuy, scDialog);
  end;

  procedure AddQuest(AQuest: TQuestEnum);
  begin
    Quests.Current := AQuest;
    Scenes.SetScene(scQuest, scDialog);
  end;

begin
  case Key of
    TK_ESCAPE:
      // Close
      Scenes.SetScene(scGame);
    TK_A: //
      begin
        if (ntHealer_A in NPCType) then
          Player.ReceiveHealing;
        if (ntBlacksmith_A in NPCType) then
        begin
          Items.Index := 0;
          Scenes.SetScene(scRepair, scDialog);
        end;
        if (ntFoodTrader_A in NPCType) then
          AddShop(shFoods);
        if (ntShTrader_A in NPCType) then
          AddShop(shShields);
        if (ntHelmTrader_A in NPCType) then
          AddShop(shHelms);
        if (ntScrTrader_A in NPCType) then
          AddShop(shScrolls);
        if (ntArmTrader_A in NPCType) then
          AddShop(shArmors);
      end;
    TK_B:
      begin
        if (ntSmithTrader_B in NPCType) then
          AddShop(shSmith);
        if (ntGlovesTrader_B in NPCType) then
          AddShop(shGloves);
        if (ntTavTrader_B in NPCType) then
          AddShop(shTavern);
        if (ntHealTrader_B in NPCType) then
          AddShop(shHealer);
        if (ntPotManaTrader_B in NPCType) then
          AddShop(shMana);
        if (ntPotTrader_B in NPCType) then
          AddShop(shPotions);
        if (ntWpnTrader_B in NPCType) then
          AddShop(shWeapons);
      end;
    TK_C:
      begin
        if (ntSell_C in NPCType) then
          Scenes.SetScene(scSell);
        if (ntJewTrader_C in NPCType) then
          AddShop(shJewelry);
        if (ntBootsTrader_C in NPCType) then
          AddShop(shBoots);
        if (ntGemTrader_C in NPCType) then
          AddShop(shGem);
      end;
    TK_D:
      begin
        if (ntRuneTrader_D in NPCType) then
          AddShop(shRunes);
        if (ntQuest_D in NPCType) then
          AddQuest(qeKillNBears);
      end;
  end;
end;

end.
