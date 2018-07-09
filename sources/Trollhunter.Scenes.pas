unit Trollhunter.Scenes;

interface

uses
  Classes,
  Types,
  uBearLibItemsCommon,
  Trollhunter.Types,
  Trollhunter.Mob,
  Trollhunter.Game,
  Trollhunter.Attribute;

type
  TSceneEnum = (scTitle, scLoad, scHelp, scGame, scQuit, scWin, scDef, scInv,
    scDrop, scItems, scAmount, scPlayer, scMessages, scStatistics, scDialog,
    scQuest, scSell, scRepair, scBuy, scCalendar, scDifficulty, scRest, scName,
    scSpellbook, scOptions, scTalents, scIdentification, scBackground,
    scEnchant, scClass, scRace);

type
  TScene = class(TObject)
  private
    KStr: string;
  public
    CX, CY: Int;
    X, Y: Int;
    constructor Create;
    procedure Render; virtual; abstract;
    procedure AddLine(AHotKey, AText: string);
    procedure AddOption(AHotKey, AText: string; AOption: Boolean;
      AColor: Cardinal = $FFAAAAAA); overload;
    procedure Add(); overload;
    procedure Add(AText: string; AValue: Int); overload;
    procedure Add(AText: string; AValue: string;
      AColor: Cardinal = $FF00FF00); overload;
    procedure Update(var Key: UInt); virtual; abstract;
    procedure AddKey(AKey, AStr: string; IsRender: Boolean = False); overload;
    procedure AddKey(AKey, AStr, AAdvStr: string;
      IsRender: Boolean = False); overload;
    procedure Title(S: string; F: Boolean = True);
  end;

type
  TScenes = class(TScene)
  private
    FSceneEnum: TSceneEnum;
    FScene: array [TSceneEnum] of TScene;
    FPrevSceneEnum: TSceneEnum;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Render; override;
    procedure Update(var Key: UInt); override;
    property SceneEnum: TSceneEnum read FSceneEnum write FSceneEnum;
    property PrevSceneEnum: TSceneEnum read FPrevSceneEnum;
    function GetScene(I: TSceneEnum): TScene;
    procedure SetScene(ASceneEnum: TSceneEnum); overload;
    procedure SetScene(ASceneEnum, CurrSceneEnum: TSceneEnum); overload;
    property PrevScene: TSceneEnum read FPrevSceneEnum write FPrevSceneEnum;
    procedure GoBack;
  end;

var
  Scenes: TScenes = nil;

type
  TSceneSpellbook = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

type
  TSceneCalendar = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

type
  TSceneDialog = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

type
  TSceneBuy = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

type
  TSceneTalents = class(TScene)
  private
    FTalent: UInt;
  public
    property Talent: UInt read FTalent write FTalent;
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

type
  TSceneSell = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

type
  TSceneRepair = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

type
  TSceneQuit = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

type
  TSceneDef = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

type
  TSceneWin = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

type
  TSceneInv = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

type
  TSceneDrop = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

type
  TSceneAmount = class(TScene)
  public
    MaxAmount: Int;
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

type
  TSceneItems = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

type
  TSceneMessages = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

var
  NPCName: string = '';
  NPCType: set of TNPCType = [];

implementation

uses
  SysUtils,
  Math,
  BearLibTerminal,
  uBearLibItemsDungeon,
  uBearLibItemsInventory,
  Trollhunter.Terminal,
  Trollhunter.Player,
  Trollhunter.Map,
  Trollhunter.Item,
  Trollhunter.Language,
  Trollhunter.Corpse,
  Trollhunter.Calendar,
  Trollhunter.Player.Spellbook,
  Trollhunter.Talent,
  Trollhunter.Player.Skills,
  Trollhunter.UI,
  Trollhunter.UI.Log,
  Trollhunter.UI.Logo,
  Trollhunter.Entity,
  Trollhunter.Creature,
  Trollhunter.Statistic,
  Trollhunter.Player.Quest,
  Trollhunter.Item.Types,
  Trollhunter.Item.Affixes,
  Trollhunter.Item.Shop,
  Trollhunter.Helpers,
  Trollhunter.Player.Types,
  Trollhunter.Player.Races,
  Trollhunter.Player.Classes,
  Trollhunter.Player.Helpers,
  Trollhunter.Scene.Enchant,
  Trollhunter.Scene.Name,
  Trollhunter.Scene.Rest,
  Trollhunter.Scene.Races,
  Trollhunter.Scene.Classes,
  Trollhunter.Scene.Quest,
  Trollhunter.Scene.Background,
  Trollhunter.Scene.Statistics,
  Trollhunter.Scene.Options,
  Trollhunter.Scene.Help,
  Trollhunter.Scene.Title,
  Trollhunter.Item.Helpers,
  Trollhunter.Scene.Difficulty,
  Trollhunter.Scene.Load,
  Trollhunter.Scene.Player,
  Trollhunter.Scene.Game,
  Trollhunter.Scene.Identification;

{ TScene }

procedure TScene.Add;
begin
  Inc(X);
  if (X > 2) then
  begin
    X := 1;
    Inc(Y);
  end;
end;

procedure TScene.AddOption(AHotKey, AText: string; AOption: Boolean;
  AColor: Cardinal);
begin
  Self.Add();
  Terminal.ForegroundColor(AColor);
  Terminal.Print(IfThen(X = 1, 2, CX + 2), Y, UI.KeyToStr(AHotKey) + ' ' + AText
    + ':', TK_ALIGN_LEFT);
  Terminal.ForegroundColor(clLightestBlue);
  Terminal.Print(Math.IfThen(X = 1, CX - 2, CX + (CX - 2)), Y,
    '[[' + Game.IfThen(AOption, 'X', ' ') + ']]', TK_ALIGN_RIGHT);
end;

constructor TScene.Create;
begin
  KStr := '';
end;

procedure TScene.Title(S: string; F: Boolean);
begin
  X := 0;
  if not F then
    Inc(Y, 2);
  UI.Title(S, Y);
  Inc(Y, 2);
end;

procedure TScene.AddKey(AKey, AStr: string; IsRender: Boolean = False);
begin
  KStr := KStr + UI.KeyToStr(AKey, AStr) + ' ';
  if (IsRender and (KStr <> '')) then
  begin
    Terminal.ForegroundColor(clDefault);
    Terminal.Print(Terminal.Window.Width div 2, Terminal.Window.Height - 2,
      Trim(Self.KStr), TK_ALIGN_CENTER);
    KStr := '';
  end;
end;

procedure TScene.AddKey(AKey, AStr, AAdvStr: string; IsRender: Boolean = False);
var
  S: string;
begin
  if Mode.Game then
    S := AStr
  else
    S := AAdvStr;
  AddKey(AKey, S, IsRender);
end;

procedure TScene.AddLine(AHotKey, AText: string);
begin
  Self.Add();
  Terminal.Print(Math.IfThen(X = 1, 5, CX + 5), Y, UI.KeyToStr(AHotKey, AText),
    TK_ALIGN_LEFT);
end;

procedure TScene.Add(AText: string; AValue: Int);
begin
  Self.Add();
  Terminal.ForegroundColor(clWhite);
  Terminal.Print(IfThen(X = 1, 2, CX + 2), Y, AText + ':', TK_ALIGN_LEFT);
  Terminal.ForegroundColor(clGreen);
  Terminal.Print(IfThen(X = 1, CX - 2, CX + (CX - 2)), Y, AValue.ToString(),
    TK_ALIGN_RIGHT);
end;

procedure TScene.Add(AText: string; AValue: string; AColor: Cardinal);
begin
  Self.Add();
  Terminal.ForegroundColor(clWhite);
  Terminal.Print(IfThen(X = 1, 2, CX + 2), Y, AText + ':', TK_ALIGN_LEFT);
  Terminal.ForegroundColor(AColor);
  Terminal.Print(IfThen(X = 1, CX - 2, CX + (CX - 2)), Y, AValue,
    TK_ALIGN_RIGHT);
end;

{ TScenes }

constructor TScenes.Create;
var
  I: TSceneEnum;
begin
  for I := Low(TSceneEnum) to High(TSceneEnum) do
    case I of
      scTitle:
        FScene[I] := TSceneTitle.Create;
      scLoad:
        FScene[I] := TSceneLoad.Create;
      scHelp:
        FScene[I] := TSceneHelp.Create;
      scGame:
        FScene[I] := TSceneGame.Create;
      scQuit:
        FScene[I] := TSceneQuit.Create;
      scWin:
        FScene[I] := TSceneWin.Create;
      scDef:
        FScene[I] := TSceneDef.Create;
      scInv:
        FScene[I] := TSceneInv.Create;
      scDrop:
        FScene[I] := TSceneDrop.Create;
      scPlayer:
        FScene[I] := TScenePlayer.Create;
      scAmount:
        FScene[I] := TSceneAmount.Create;
      scItems:
        FScene[I] := TSceneItems.Create;
      scMessages:
        FScene[I] := TSceneMessages.Create;
      scStatistics:
        FScene[I] := TSceneStatistics.Create;
      scDialog:
        FScene[I] := TSceneDialog.Create;
      scBuy:
        FScene[I] := TSceneBuy.Create;
      scSell:
        FScene[I] := TSceneSell.Create;
      scRepair:
        FScene[I] := TSceneRepair.Create;
      scCalendar:
        FScene[I] := TSceneCalendar.Create;
      scDifficulty:
        FScene[I] := TSceneDifficulty.Create;
      scRest:
        FScene[I] := TSceneRest.Create;
      scName:
        FScene[I] := TSceneName.Create;
      scOptions:
        FScene[I] := TSceneOptions.Create;
      scSpellbook:
        FScene[I] := TSceneSpellbook.Create;
      scTalents:
        FScene[I] := TSceneTalents.Create;
      scIdentification:
        FScene[I] := TSceneIdentification.Create;
      scBackground:
        FScene[I] := TSceneBackground.Create;
      scQuest:
        FScene[I] := TSceneQuest.Create;
      scEnchant:
        FScene[I] := TSceneEnchant.Create;
      scRace:
        FScene[I] := TSceneRace.Create;
      scClass:
        FScene[I] := TSceneClass.Create;
    end;
end;

destructor TScenes.Destroy;
var
  I: TSceneEnum;
begin
  for I := Low(TSceneEnum) to High(TSceneEnum) do
    FreeAndNil(FScene[I]);
  inherited;
end;

function TScenes.GetScene(I: TSceneEnum): TScene;
begin
  Result := FScene[I];
end;

procedure TScenes.GoBack;
begin
  Self.SceneEnum := FPrevSceneEnum;
end;

procedure TScenes.Render;
begin
  Terminal.BackgroundColor(clBackground);
  Terminal.ForegroundColor(clDefault);
  Terminal.Clear;
  if (FScene[SceneEnum] <> nil) then
  begin
    FScene[SceneEnum].CX := Terminal.Window.Width div 2;
    FScene[SceneEnum].CY := Terminal.Window.Height div 2;
    FScene[SceneEnum].Render;
  end;
end;

procedure TScenes.SetScene(ASceneEnum: TSceneEnum);
begin
  Game.Timer := UIntMax;
  Game.ShowEffects := False;
  Self.SceneEnum := ASceneEnum;
  Render;
end;

procedure TScenes.SetScene(ASceneEnum, CurrSceneEnum: TSceneEnum);
begin
  FPrevSceneEnum := CurrSceneEnum;
  SetScene(ASceneEnum);
end;

procedure TScenes.Update(var Key: UInt);
begin
  if (FScene[SceneEnum] <> nil) then
  begin
    FScene[SceneEnum].Update(Key);
  end;
  case Key of
    TK_CLOSE:
      begin
        if (SceneEnum = scTitle) then
          Game.CanClose := True;
        if Mode.Game and not(SceneEnum in [scWin, scDef, scQuit]) and
          not Player.IsDead then
          SetScene(scQuit, SceneEnum);
      end;
  end;
end;

{ TSceneQuit }

procedure TSceneQuit.Render;
begin
  Logo.Render(False);
  Terminal.Print(CX, CY + 3, Format(_('Do you wish to quit? %s/%s'),
    [UI.KeyToStr('Y'), UI.KeyToStr('N')]), TK_ALIGN_CENTER);
end;

procedure TSceneQuit.Update(var Key: UInt);
begin
  case Key of
    TK_Y:
      begin
        Player.SaveCharacterDump(_('Quit the game'));
        Game.CanClose := True;
      end;
    TK_ESCAPE, TK_N:
      Scenes.GoBack;
  end;
end;

{ TSceneDef }

procedure TSceneDef.Render;
begin
  Logo.Render(False);
  Terminal.Print(CX, CY + 1, UpperCase(_('Game over!!!')), TK_ALIGN_CENTER);
  if (Player.Killer = '') then
    Terminal.Print(CX, CY + 3, Format(_('You dead. Press %s'),
      [UI.KeyToStr('ENTER')]), TK_ALIGN_CENTER)
  else
    Terminal.Print(CX, CY + 3, Format(_('You were slain by %s. Press %s'),
      [Terminal.Colorize(Player.Killer, clAlarm), UI.KeyToStr('ENTER')]),
      TK_ALIGN_CENTER);
  if Mode.Wizard then
    Terminal.Print(CX, CY + 5, Format(_('Press %s to continue...'),
      [UI.KeyToStr('SPACE')]), TK_ALIGN_CENTER);

end;

procedure TSceneDef.Update(var Key: UInt);
begin
  case Key of
    TK_ENTER, TK_KP_ENTER:
      begin
        Player.SaveCharacterDump(Format(_('Killed by %s'), [Player.Killer]));
        Game.CanClose := True;
      end;
    TK_SPACE:
      if Mode.Wizard then
      begin
        Player.Fill;
        Scenes.SetScene(scGame);
      end;
  end;
end;

{ TSceneWin }

procedure TSceneWin.Render;
begin
  Logo.Render(False);
  Terminal.Print(CX, CY + 1, UpperCase(_('Congratulations!!!')),
    TK_ALIGN_CENTER);
  Terminal.Print(CX, CY + 3, Format(_('You have won. Press %s'),
    [UI.KeyToStr('ENTER')]), TK_ALIGN_CENTER);
end;

procedure TSceneWin.Update(var Key: UInt);
begin
  case Key of
    TK_ENTER, TK_KP_ENTER:
      begin
        Player.SaveCharacterDump(_('Won the game'));
        Game.CanClose := True;
      end;
  end;
end;

{ TSceneInv }

{ TODO -cПредметы: Нажатие в инвентаре кнопки не сущ. предмета вызывает сообщение, что предмет не идентифицирован }

procedure TSceneInv.Render;
begin
  UI.Title(Format('%s [[%s%d %s%d/%d]]', [_('Inventory'), UI.Icon(icGold),
    Player.Gold, UI.Icon(icFlag), Items_Inventory_GetCount(), ItemMax]));

  UI.FromAToZ(ItemMax);
  Items.RenderInventory;
  MsgLog.Render(2, True);

  AddKey('Esc', _('Close'));
  AddKey('?', _('Help'), True);

end;

procedure TSceneInv.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE: // Close
      Scenes.SetScene(scGame);
    TK_TAB: // Drop
      Scenes.SetScene(scDrop, scInv);
    TK_ENTER, TK_KP_ENTER:
      begin

      end;
    TK_SLASH:
      Scenes.SetScene(scHelp, scInv);
    TK_SPACE: // Player
      Scenes.SetScene(scPlayer);
    TK_A .. TK_Z: // Use an item
      Player.Use(Key - TK_A);
  else
    Game.Timer := UIntMax;
  end;
end;

{ TSceneDrop }

procedure TSceneDrop.Render;
begin
  UI.Title(_('Choose the item you wish to drop'), 1, clDarkestRed);

  UI.FromAToZ;
  Items.RenderInventory;
  MsgLog.Render(2, True);

  AddKey('A-Z', _('Drop an item'));
  AddKey('Esc', _('Close'), True);
end;

procedure TSceneDrop.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      // Close
      Scenes.GoBack;
    TK_A .. TK_Z: // Drop an item
      Player.Drop(Key - TK_A);
  else
    Game.Timer := UIntMax;
  end;
end;

{ TSceneAmount }

procedure TSceneAmount.Render;
var
  FItem: Item;
begin
  UI.Title(_('Enter amount'));

  if Player.ItemIsDrop then
    FItem := Items_Inventory_GetItem(Player.ItemIndex)
  else
    FItem := Items_Dungeon_GetMapItemXY(Ord(Map.Current), Player.ItemIndex,
      Player.X, Player.Y);

  MaxAmount := FItem.Amount;

  Terminal.Print(CX, CY, Format('%d/%dx', [Player.ItemAmount, FItem.Amount]),
    TK_ALIGN_LEFT);

  AddKey('Esc', _('Close'));
  AddKey('UP/W', _('More'));
  AddKey('DOWN/X', _('Less'));
  AddKey('Enter', _('Apply'), True);
end;

procedure TSceneAmount.Update(var Key: UInt);

  procedure ChAmount(Value: Int);
  begin
    Player.ItemAmount := EnsureRange(Value, 1, MaxAmount);
    Render;
  end;

begin
  case Key of
    TK_ESCAPE: // Close
      Scenes.SetScene(scGame);
    TK_ENTER, TK_KP_ENTER:
      begin
        if Player.ItemIsDrop then
          Player.DropAmount(Player.ItemIndex)
        else
          Player.PickUpAmount(Player.ItemIndex);
      end;
    TK_UP, TK_KP_8, TK_W:
      ChAmount(Player.ItemAmount + 1);
    TK_DOWN, TK_KP_2, TK_X:
      ChAmount(Player.ItemAmount - 1);
  end;
end;

{ TSceneItems }

procedure TSceneItems.Render;
var
  I, FCount, MapID: Int;
  FItem: Item;
begin
  MapID := Ord(Map.Current);
  UI.Title(_('Pick up an item'));

  UI.FromAToZ;
  FCount := Items_Dungeon_GetMapCountXY(MapID, Player.X, Player.Y)
    .InRange(ItemMax);
  for I := 0 to FCount - 1 do
  begin
    FItem := Items_Dungeon_GetMapItemXY(MapID, I, Player.X, Player.Y);
    Items.RenderInvItem(5, 2, I, FItem);
  end;

  MsgLog.Render(2, True);

  AddKey('Esc', _('Close'));
  AddKey('Space', _('Pick up all items'));
  AddKey('A-Z', _('Pick up an item'), True);

  if (FCount <= 0) then
    Scenes.SetScene(scGame);
end;

procedure TSceneItems.Update(var Key: UInt);
var
  I, FCount: Int;
begin
  case Key of
    TK_ESCAPE: // Close
      Scenes.SetScene(scGame);
    TK_SPACE:
      begin
        FCount := Items_Dungeon_GetMapCountXY(Ord(Map.Current), Player.X,
          Player.Y).InRange(ItemMax);
        for I := 0 to FCount - 1 do
          Items.AddItemToInv;
      end;
    TK_A .. TK_Z:
      // Pick up
      Items.AddItemToInv(Key - TK_A);
  else
    Game.Timer := UIntMax;
  end;
end;

{ TSceneMessages }

procedure TSceneMessages.Render;
begin
  UI.Title(_('Last messages'));
  MsgLog.RenderAllMessages;
  AddKey('Esc', _('Close'), True);
end;

procedure TSceneMessages.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      // Close
      Scenes.SetScene(scGame);
  end;
end;

{ TSceneDialog }

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
  UI.Title(NPCName + ' ' + UI.GoldLeft(Player.Gold));

  UI.FromAToZ;
  Y := 1;

  // Heal
  if (ntHealer_A in NPCType) then
  begin
    V := Player.Attributes.Attrib[atMaxLife].Value - Player.Attributes.Attrib
      [atLife].Value;
    if (V > 0) then
      S := ' (' + Items.GetInfo('+', V, 'Life') + ' ' +
        Items.GetItemPrice(Round(V * 1.6)) + ')'
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

{ TSceneSell }

procedure TSceneSell.Render;
begin
  UI.Title(_('Selling items') + ' ' + UI.GoldLeft(Player.Gold));

  UI.FromAToZ;
  Items.RenderInventory(ptSell);
  MsgLog.Render(2, True);

  AddKey('A-Z', _('Selling an item'));
  AddKey('Esc', _('Close'), True);
end;

procedure TSceneSell.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      // Close
      Scenes.SetScene(scDialog);
    TK_A .. TK_Z: // Selling an item
      Player.Sell(Key - TK_A);
  else
    Game.Timer := UIntMax;
  end;
end;

{ TSceneBuy }

procedure TSceneBuy.Render;
begin
  UI.Title(Format(_('Buying at %s'), [NPCName]) + ' ' +
    UI.GoldLeft(Player.Gold));

  UI.FromAToZ;
  Shops.Render;
  MsgLog.Render(2, True);

  AddKey('A-Z', _('Buy an item'));
  AddKey('Esc', _('Close'), True);
end;

procedure TSceneBuy.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      // Close
      Scenes.SetScene(scDialog);
    TK_A .. TK_Z: // Buy items
      Player.Buy(Key - TK_A);
  else
    Game.Timer := UIntMax;
  end;
end;

{ TSceneRepair }

procedure TSceneRepair.Render;
begin
  UI.Title(_('Repairing items') + ' ' + UI.GoldLeft(Player.Gold), 1,
    clDarkestRed);

  UI.FromAToZ;
  Items.RenderInventory(ptRepair);
  MsgLog.Render(2, True);

  AddKey('A-Z', _('Repairing an item'));
  AddKey('Esc', _('Close'), True);
end;

procedure TSceneRepair.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      // Close
      Scenes.GoBack();
    TK_A .. TK_Z: // Repairing an item
      Player.RepairItem(Key - TK_A);
  else
    Game.Timer := UIntMax;
  end;
end;

{ TSceneCalendar }

procedure TSceneCalendar.Render;

  procedure Add(const AText: string; AValue: string;
    AAdvValue: string = ''); overload;
  var
    S: string;
    X: UInt;
  begin
    X := Screen.Width div 3;
    S := '';
    if (AAdvValue <> '') then
      S := AAdvValue;
    Terminal.ForegroundColor(clWhite);
    Terminal.Print(X, Y, AText, TK_ALIGN_LEFT);
    Terminal.ForegroundColor(clGreen);
    Terminal.Print(X + 10, Y, AValue, TK_ALIGN_LEFT);
    if (S <> '') then
    begin
      Terminal.ForegroundColor(clLightBlue);
      Terminal.Print(X + 20, Y, AAdvValue, TK_ALIGN_LEFT);
    end;
    Inc(Y);
  end;

  procedure Add(const AText: string; AValue: Int;
    AAdvValue: string = ''); overload;
  begin
    Add(AText, AValue.ToString(), AAdvValue);
  end;

begin
  UI.Title(_('Calendar'));

  Y := 10;
  Player.RenderWeather(CX, Y - 6, CX);
  Add(_('Turn'), Player.Statictics.Get(stTurn));
  Add(_('Time'), Calendar.GetTime, Calendar.GetTimeStr);
  Add(_('Day'), Calendar.Day, Calendar.GetDayName);
  Add(_('Month'), Calendar.Month, Calendar.GetMonthName);
  Add(_('Year'), Calendar.Year);
  Add(_('Map'), Map.Name);
  Add(_('Wind'), '');
  Add(_('Weather'), '');

  AddKey('Esc', _('Close'), True);
end;

procedure TSceneCalendar.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      // Close
      Scenes.SetScene(scGame);
  end;
end;

{ TSceneSpells }

procedure TSceneSpellbook.Render;
var
  I: TSpellEnum;
  V: UInt;

  function IsSpell(I: TSpellEnum): Boolean;
  begin
    Result := Spellbook.GetSpell(I).Enable;
    if Mode.Wizard then
      Result := True;
  end;

begin
  UI.Title(_('Spellbook'));

  V := 0;
  Y := 2;
  UI.FromAToZ;
  for I := Low(TSpellEnum) to High(TSpellEnum) do
    if IsSpell(I) then
    begin
      Terminal.Print(1, Y, UI.KeyToStr(Chr(V + Ord('A'))));
      Terminal.ForegroundColor(clGray);
      Terminal.Print(5, Y, Format('(%s) %s %s',
        [Items.GetItemLevel(Spellbook.GetSpell(I).Spell.Level),
        Spellbook.GetSpellName(I), Items.GetInfo('-',
        Spellbook.GetSpell(I).Spell.ManaCost, 'Mana')]));
      Inc(Y);
      Inc(V);
    end;
  MsgLog.Render(2, True);

  AddKey('A-Z', _('Cast spell'));
  AddKey('Esc', _('Close'), True);
end;

procedure TSceneSpellbook.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      Scenes.SetScene(scGame);
    TK_A .. TK_Z:
      Spellbook.DoSpell(Key - TK_A);
  end
end;

{ TSceneTalents }

procedure TSceneTalents.Render;
var
  V, I: UInt;
  T: TTalentEnum;

  procedure Add(const S, H: string; F: Boolean = True); overload;
  var
    C: Char;
  begin
    C := Chr(V + Ord('A'));
    if F then
      Terminal.Print(1, Y, UI.KeyToStr(C))
    else
    begin
      Terminal.ForegroundColor(clWhite);
      Terminal.Print(1, Y, '[[' + C + ']]');
    end;
    Terminal.ForegroundColor(clWhite);
    Terminal.Print(5, Y, S);
    Terminal.ForegroundColor(clGray);
    Terminal.Print(30, Y, H);
    Inc(Y);
    Inc(V);
  end;

  procedure Add(); overload;
  begin
    if (Player.Talents.Talent[V].Enum <> tlNone) then
    begin
      Terminal.ForegroundColor(clWhite);
      with Player.Talents do
        Terminal.Print(CX + (CX div 2), Y, GetName(Talent[V].Enum));
    end;
    Inc(Y);
    Inc(V);
  end;

begin
  UI.Title(_('Choose a talent'));

  V := 0;
  Y := 2;
  UI.FromAToZ;

  Terminal.ForegroundColor(clGray);
  for T := Succ(Low(TTalentEnum)) to High(TTalentEnum) do
    if (TalentBase[T].Level = Player.Attributes.Attrib[atLev].Value) then
      Add(Player.Talents.GetName(T), Player.Talents.GetHint(T),
        Player.Talents.IsPoint);

  V := 0;
  Y := 2;
  for I := 0 to TalentMax - 1 do
    Add();

  if Mode.Game then
    MsgLog.Render(2, True);

  if Player.Talents.IsPoint then
  begin
    AddKey('A-Z', _('Select a talent'));
    AddKey('Esc', _('Close'), _('Back'), True);
  end
  else
    AddKey('Esc', _('Close'), _('Back'), True);
end;

procedure TSceneTalents.Update(var Key: UInt);
var
  K: UInt;
begin
  case Key of
    TK_ESCAPE:
      begin
        if not Mode.Game then
          Player.Talents.Clear;
        Scenes.GoBack;
      end;
    TK_A .. TK_Z, TK_ENTER, TK_KP_ENTER:
      begin
        if Player.Talents.IsPoint then
        begin
          case Key of
            TK_A .. TK_Z:
              begin
                K := Key - TK_A;
                if Mode.Game then
                  Player.Talents.DoTalent(K)
                else if (K <= 5) then
                begin
                  Self.Talent := K;
                  Scenes.SetScene(scName);
                end;
              end;
            TK_ENTER, TK_KP_ENTER:
              if Mode.Wizard and not Mode.Game then
              begin
                Self.Talent := Math.RandomRange(0, 5);
                Scenes.SetScene(scName);
              end;
          end;
        end;
      end;
  end
end;

initialization

Scenes := TScenes.Create();

finalization

FreeAndNil(Scenes);

end.
