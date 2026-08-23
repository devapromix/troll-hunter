unit Trollhunter.Scene.Dialog;

interface

uses
  Classes,
  Trollhunter.UI,
  Trollhunter.UI.Log,
  Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneDialog = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

uses
  SysUtils,
  BearLibTerminal,
  Trollhunter.Mob,
  Trollhunter.Quest,
  Trollhunter.Item,
  Trollhunter.Dialog,
  Trollhunter.Item.Shop,
  Trollhunter.Attribute,
  Trollhunter.Player,
  Trollhunter.Player.Helpers,
  Trollhunter.Terminal;

{ TSceneDialog }

procedure TSceneDialog.Render;
var
  LValue: Int;
  S: string;

  procedure Add(const ALetter: char; const S: string);
  begin
    Inc(Y);
    Terminal.Print(1, Y, UI.KeyToStr(ALetter) + ' ' + S, TK_ALIGN_LEFT);
  end;

begin
  UI.Title(NPCName + ' ' + UI.GoldLeft(Player.Gold));
  UI.FromAToZ;
  Y := 1;

  // Heal
  if (ntHealer_A in NPCType) then
  begin
    LValue := Player.Attributes.Attrib[atMaxLife].Value -
      Player.Attributes.Attrib[atLife].Value;
    if (LValue > 0) then
      S := ' (' + Items.GetIcon(LValue, 'Life') + ' ' +
        Items.GetPrice(Round(LValue * 1.6)) + ')'
    else
      S := '';
    Add('A', 'Heal me, please' + S);
  end;
  // Shops
  if (ntScrTrader_A in NPCType) then
    Add('A', AskAbout('scrolls'));
  if (ntArmTrader_A in NPCType) then
    Add('A', AskAbout('armor'));
  if (ntShTrader_A in NPCType) then
    Add('A', AskAbout('shields'))
  else if (ntShTrader_B in NPCType) then
    Add('B', AskAbout('shields'));
  if (ntHelmTrader_A in NPCType) then
    Add('A', AskAbout('helmets'));
  if (ntFoodTrader_A in NPCType) then
    Add('A', AskAbout('food'));
  if (ntBowTrader_A in NPCType) then
    Add('A', AskAbout('bows'));
  if (ntDaggerTrader_A in NPCType) then
    Add('A', AskAbout('daggers'));
  if (ntVenomTrader_B in NPCType) then
    Add('B', AskAbout('venoms'));
  if (ntBlacksmith_A in NPCType) then
    Add('A', 'Can you repair my gear?');
  if (ntSmithTrader_B in NPCType) then
    Add('B', 'What do you have for sale?');
  if (ntHealTrader_B in NPCType) then
    Add('B', AskAbout('healing items'));
  if (ntPotManaTrader_B in NPCType) then
    Add('B', AskAbout('mana potions'));
  if (ntPotTrader_B in NPCType) then
    Add('B', AskAbout('potions'));
  if (ntGlovesTrader_B in NPCType) then
    Add('B', AskAbout('gloves'));
  if (ntTavTrader_B in NPCType) then
    Add('B', 'What''s on the menu?');
  if (ntWpnTrader_A in NPCType) then
    Add('A', AskAbout('weapons'))
  else if (ntWpnTrader_B in NPCType) then
    Add('B', AskAbout('weapons'))
  else if (ntWpnTrader_C in NPCType) then
    Add('C', AskAbout('weapons'));
  if (ntQvrTrader_B in NPCType) then
    Add('B', AskAbout('quivers'));
  if (ntGemTrader_A in NPCType) then
    Add('A', AskAbout('gems'))
  else if (ntGemTrader_C in NPCType) then
    Add('C', AskAbout('gems'));
  // Identify all items
  if (ntIdentify_D in NPCType) then
  begin
    if Player.HasUnidentifiedItems then
      S := ' (' + Items.GetPrice(CIdentifyAllItemsCost) + ')'
    else
      S := '';
    Add('D', 'Can you identify my items?' + S);
  end;
  if (ntJewTrader_C in NPCType) then
    Add('C', AskAbout('jewelry'));
  if (ntBootsTrader_C in NPCType) then
    Add('C', AskAbout('boots'));
  if (ntSell_C in NPCType) then
    Add('C', 'I want to sell something')
  else if (ntSell_D in NPCType) then
    Add('D', 'I want to sell something');
  // Arrows
  if (ntArrTrader_C in NPCType) then
  begin
    LValue := Player.GetArrowsToBuy;
    if (LValue > 0) then
      S := ' (' + Items.GetIcon(LValue, 'Arrow') + ' ' + Items.GetPrice(LValue) + ')'
    else
      S := '';
    Add('C', 'I need more arrows' + S);
  end;
  if (ntRuneTrader_D in NPCType) then
    Add('D', AskAbout('runes'));
  if (ntStaffTrader_A in NPCType) then
    Add('A', AskAbout('staves'));
  if (ntWandTrader_B in NPCType) then
    Add('B', AskAbout('wands'));
  if (ntBookTrader_C in NPCType) then
    Add('C', AskAbout('books'));
  // Quests
  {if (ntQuest_D in NPCType) then
    Add('D', 'The Hunt (quest)'); }
  MsgLog.Render(2, True);

  AddKey('Esc', 'Close', True);
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
    TK_A:
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
      if (ntHelmTrader_A in NPCType) then
        AddShop(shHelms);
      if (ntScrTrader_A in NPCType) then
        AddShop(shScrolls);
      if (ntArmTrader_A in NPCType) then
        AddShop(shArmors);
      if (ntShTrader_A in NPCType) then
        AddShop(shShields);
      if (ntGemTrader_A in NPCType) then
        AddShop(shGem);
      if (ntStaffTrader_A in NPCType) then
        AddShop(shStaves);
      if (ntWpnTrader_A in NPCType) then
        AddShop(shWeapons);
      if (ntBowTrader_A in NPCType) then
        AddShop(shBows);
      if (ntDaggerTrader_A in NPCType) then
        AddShop(shDaggers);
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
      if (ntQvrTrader_B in NPCType) then
        AddShop(shQuivers);
      if (ntWandTrader_B in NPCType) then
        AddShop(shWands);
      if (ntShTrader_B in NPCType) then
        AddShop(shShields);
      if (ntVenomTrader_B in NPCType) then
        AddShop(shVenoms);
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
      if (ntArrTrader_C in NPCType) then
        Player.BuyArrows;
      if (ntBookTrader_C in NPCType) then
        AddShop(shBooks);
      if (ntWpnTrader_C in NPCType) then
        AddShop(shWeapons);
    end;
    TK_D:
    begin
      if (ntRuneTrader_D in NPCType) then
        AddShop(shRunes);
      if (ntSell_D in NPCType) then
        Scenes.SetScene(scSell);
      if (ntQuest_D in NPCType) then
        AddQuest(qeKillNBears);
      if (ntIdentify_D in NPCType) then
        Player.IdentifyAllItems;
    end;
  end;
end;

end.
