unit Trollhunter.Scenes;

interface

uses
  Classes,
  Trollhunter.Types,
  Trollhunter.Item.Common,
  Trollhunter.Mob,
  Trollhunter.Game,
  Trollhunter.Attribute;

type
  TSceneEnum = (scTitle, scLoad, scHelp, scGame, scQuit, scWin, scDef, scInv,
    scDrop, scItems, scAmount, scPlayer, scMessages, scStatistics, scDialog,
    scQuest, scSell, scRepair, scBuy, scCalendar, scDifficulty, scRest, scName,
    scSpellbook, scOptions, scTalents, scLearnedTalents, scIdentification, scBackground,
    scEnchant, scRecharge, scClass, scRace, scStash, scStore, scDisenchant);

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
    procedure AddOption(AHotKey, AText: string; AOption: boolean;
      AColor: cardinal = $FFAAAAAA); overload;
    procedure Add(); overload;
    procedure Add(AText: string; AValue: Int); overload;
    procedure Add(AText: string; AValue: string; AColor: cardinal = $FF00FF00);
      overload;
    procedure Update(var Key: UInt); virtual; abstract;
    procedure AddKey(AKey, AStr: string; IsRender: boolean = False); overload;
    procedure AddKey(AKey, AStr, AAdvStr: string; IsRender: boolean = False);
      overload;
    procedure Title(S: string; F: boolean = True);
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
  TSceneTitle = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
    procedure RenderHeroes();
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
  TSceneLoad = class(TScene)
  public
    procedure Render; override;
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
  TSceneAmount = class(TScene)
  public
    MaxAmount: Int;
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

type
  TSceneMessages = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

type
  TSceneIdentification = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

type
  TSceneDisenchant = class(TScene)
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
  Trollhunter.Terminal,
  Trollhunter.Player,
  BearLibTerminal,
  Trollhunter.Map,
  Trollhunter.Magic,
  Trollhunter.UI.Log,
  Trollhunter.Item,
  Trollhunter.Calendar,
  Trollhunter.Item.Shop,
  Trollhunter.Spellbook,
  Trollhunter.UI.Logo,
  Trollhunter.Entity,
  Trollhunter.Creature,
  Trollhunter.Statistic,
  Trollhunter.UI,
  Trollhunter.Item.Dungeon,
  Trollhunter.Item.Inventory,
  Trollhunter.Quest,
  Trollhunter.Item.Affixes,
  Trollhunter.Helpers,
  Trollhunter.Scene.Enchant,
  Trollhunter.Scene.Recharge,
  Trollhunter.Scene.Name,
  Trollhunter.Scene.Rest,
  Trollhunter.Scene.Drop,
  Trollhunter.Scene.Items,
  Trollhunter.Scene.RacesAndClasses,
  Trollhunter.Scene.Difficulty,
  Trollhunter.Scene.Quest,
  Trollhunter.Scene.Background,
  Trollhunter.Item.Types,
  Trollhunter.Scene.Statistics,
  Trollhunter.Scene.Player,
  Trollhunter.Scene.Options,
  Trollhunter.Scene.Game,
  Trollhunter.Scene.Stash,
  Trollhunter.Scene.Store,
  Trollhunter.Scene.Talents,
  Trollhunter.Scene.LearnedTalents,
  Trollhunter.Scene.Spellbook,
  Trollhunter.Scene.Help;

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

procedure TScene.AddOption(AHotKey, AText: string; AOption: boolean; AColor: cardinal);
begin
  Self.Add();
  Terminal.ForegroundColor(AColor);
  Terminal.Print(IfThen(X = 1, 2, CX + 2), Y, UI.KeyToStr(AHotKey) +
    ' ' + AText + ':', TK_ALIGN_LEFT);
  Terminal.ForegroundColor(clLightestBlue);
  Terminal.Print(Math.IfThen(X = 1, CX - 2, CX + (CX - 2)), Y,
    '[[' + Game.IfThen(AOption, 'X', ' ') + ']]', TK_ALIGN_RIGHT);
end;

constructor TScene.Create;
begin
  KStr := '';
end;

procedure TScene.Title(S: string; F: boolean);
begin
  X := 0;
  if not F then
    Inc(Y, 2);
  UI.Title(S, Y);
  Inc(Y, 2);
end;

procedure TScene.AddKey(AKey, AStr: string; IsRender: boolean = False);
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

procedure TScene.AddKey(AKey, AStr, AAdvStr: string; IsRender: boolean = False);
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

procedure TScene.Add(AText: string; AValue: string; AColor: cardinal);
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
      scLearnedTalents:
        FScene[I] := TSceneLearnedTalents.Create;
      scIdentification:
        FScene[I] := TSceneIdentification.Create;
      scBackground:
        FScene[I] := TSceneBackground.Create;
      scQuest:
        FScene[I] := TSceneQuest.Create;
      scEnchant:
        FScene[I] := TSceneEnchant.Create;
      scRecharge:
        FScene[I] := TSceneRecharge.Create;
      scRace:
        FScene[I] := TSceneRace.Create;
      scClass:
        FScene[I] := TSceneClass.Create;
      scStash:
        FScene[I] := TSceneStash.Create;
      scStore:
        FScene[I] := TSceneStore.Create;
      scDisenchant:
        FScene[I] := TSceneDisenchant.Create;
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
      if Mode.Game and not (SceneEnum in [scWin, scDef, scQuit]) and
        not Player.IsDead then
        SetScene(scQuit, SceneEnum);
    end;
  end;
end;

{ TSceneTitle }

procedure TSceneTitle.Render;
begin
  Logo.Render(True);
  Terminal.Print(Screen.Width - ((Screen.Width div 2) - (Logo.Width div 2) + 2),
    14, Format('by Apromix v.%s', [Game.GetVersion]), TK_ALIGN_RIGHT);
  // RenderHeroes;
  if Mode.Wizard then
  begin
    Self.AddKey('Space', 'Create a new hero');
    Self.AddKey('Z', Terminal.Colorize('Turn Wizard Mode Off', 'Red'), True);
  end
  else
    Self.AddKey('Space', 'Create a new hero', True);
end;

type
  TAJ = 'A' .. 'J';

procedure TSceneTitle.RenderHeroes;
const
  L = 12;
  T = 15;
var
  J: UInt;
  V: TAJ;
begin
  Terminal.ForegroundColor(clWhite);
  Terminal.Print(L + 4, T, 'Which hero shall you play?');

  for V := 'A' to 'J' do
  begin
    J := Ord(V) - 65;
    Terminal.Print(L, T + J + 2, UI.KeyToStr(V, J.ToString));
  end;
end;

procedure TSceneTitle.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      Game.CanClose := True;
    TK_A .. TK_J:
      ;
    TK_SPACE:
      Scenes.SetScene(scDifficulty);
    TK_ENTER, TK_KP_ENTER:
      if Mode.Wizard then
        Scenes.SetScene(scDifficulty);
    TK_Z:
      Mode.Wizard := False;
  end;
end;

{ TSceneLoad }

procedure TSceneLoad.Render;
begin
  Terminal.Print(CX, CY, 'Creating the world, please wait...',
    TK_ALIGN_CENTER);
end;

{ TSceneQuit }

procedure TSceneQuit.Render;
begin
  Logo.Render(False);
  Terminal.Print(CX, CY + 3, Format('Do you wish to quit? %s/%s',
    [UI.KeyToStr('Y'), UI.KeyToStr('N')]), TK_ALIGN_CENTER);
end;

procedure TSceneQuit.Update(var Key: UInt);
begin
  case Key of
    TK_Y:
    begin
      Player.SaveCharacterDump('Quit the game');
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
  Terminal.Print(CX, CY + 1, UpperCase('Game over!!!'), TK_ALIGN_CENTER);
  if (Player.Killer = '') then
    Terminal.Print(CX, CY + 3, Format('You dead. Press %s', [UI.KeyToStr('ENTER')]),
      TK_ALIGN_CENTER)
  else
    Terminal.Print(CX, CY + 3, Format('You were slain by %s. Press %s',
      [Terminal.Colorize(Player.Killer, clAlarm), UI.KeyToStr('ENTER')]),
      TK_ALIGN_CENTER);
  if Mode.Wizard then
    Terminal.Print(CX, CY + 5, Format('Press %s to continue...',
      [UI.KeyToStr('SPACE')]), TK_ALIGN_CENTER);

end;

procedure TSceneDef.Update(var Key: UInt);
begin
  case Key of
    TK_ENTER, TK_KP_ENTER:
    begin
      Player.SaveCharacterDump(Format('Killed by %s', [Player.Killer]));
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
  Terminal.Print(CX, CY + 1, UpperCase('Congratulations!!!'),
    TK_ALIGN_CENTER);
  Terminal.Print(CX, CY + 3, Format('You have won. Press %s', [UI.KeyToStr('ENTER')]),
    TK_ALIGN_CENTER);
end;

procedure TSceneWin.Update(var Key: UInt);
begin
  case Key of
    TK_ENTER, TK_KP_ENTER:
    begin
      Player.SaveCharacterDump('Won the game');
      Game.CanClose := True;
    end;
  end;
end;

{ TSceneInv }

procedure TSceneInv.Render;
begin
  UI.Title(Format('%s [[%s%d %s%d/%d]]', ['Inventory', UI.Icon(icGold),
    Player.Gold, UI.Icon(icFlag), Items_Inventory_GetCount(), ItemMax]));

  UI.FromAToZ(ItemMax);
  Items.RenderInventory;
  MsgLog.Render(2, True);

  AddKey('Esc', 'Close');
  AddKey('?', 'Help', True);

end;

procedure TSceneInv.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      Scenes.SetScene(scGame);
    TK_TAB:
    begin
      if Player.IsDead then
        Exit;
      if Player.IsOnStash then
        Scenes.SetScene(scStore, scInv)
      else
        Scenes.SetScene(scDrop, scInv);
    end;
    TK_ENTER, TK_KP_ENTER:
    begin

    end;
    TK_SLASH:
      Scenes.SetScene(scHelp, scInv);
    TK_SPACE:
      Scenes.SetScene(scPlayer);
    TK_A .. TK_Z:
      Player.Use(Key - TK_A);
    else
      Game.Timer := UIntMax;
  end;
end;

{ TSceneAmount }

procedure TSceneAmount.Render;
var
  FItem: Item;
begin
  UI.Title('Enter amount');

  if Player.ItemIsDrop then
    FItem := Items_Inventory_GetItem(Player.ItemIndex)
  else
    FItem := Items_Dungeon_GetMapItemXY(Ord(Map.Current), Player.ItemIndex,
      Player.X, Player.Y);

  MaxAmount := FItem.Amount;

  Terminal.Print(CX, CY, Format('%d/%dx', [Player.ItemAmount, FItem.Amount]),
    TK_ALIGN_LEFT);

  AddKey('Esc', 'Close');
  AddKey('UP/W', 'More');
  AddKey('DOWN/X', 'Less');
  AddKey('Enter', 'Apply', True);
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

{ TSceneMessages }

procedure TSceneMessages.Render;
begin
  UI.Title('Last messages');
  MsgLog.RenderAllMessages;
  AddKey('Esc', 'Close', True);
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
    V := Player.Attributes.Attrib[atMaxLife].Value -
      Player.Attributes.Attrib[atLife].Value;
    if (V > 0) then
      S := ' (' + Items.GetInfo('+', V, 'Life') + ' ' +
        Items.GetPrice(Round(V * 1.6)) + ')'
    else
      S := '';
    Add('Receive healing' + S);
  end;
  // Shops
  if (ntScrTrader_A in NPCType) then
    Add('Buy items (scrolls)');
  if (ntArmTrader_A in NPCType) then
    Add('Buy items (armors)');
  if (ntShTrader_A in NPCType) then
    Add('Buy items (shields)');
  if (ntHelmTrader_A in NPCType) then
    Add('Buy items (helms)');
  if (ntFoodTrader_A in NPCType) then
    Add('Buy items (foods)');
  if (ntBlacksmith_A in NPCType) then
    Add('Repair items');
  if (ntSmithTrader_B in NPCType) then
    Add('Buy items (blacksmith)');
  if (ntHealTrader_B in NPCType) then
    Add('Buy items (healing)');
  if (ntPotManaTrader_B in NPCType) then
    Add('Buy items (items of mana)');
  if (ntPotTrader_B in NPCType) then
    Add('Buy items (potions)');
  if (ntGlovesTrader_B in NPCType) then
    Add('Buy items (gloves)');
  if (ntTavTrader_B in NPCType) then
    Add('Buy items (tavern)');
  if (ntWpnTrader_B in NPCType) then
    Add('Buy items (weapons)');
  if (ntQvrTrader_B in NPCType) then
    Add('Buy items (quivers)');
  if (ntGemTrader_C in NPCType) then
    Add('Buy items (gems)');
  if (ntJewTrader_C in NPCType) then
    Add('Buy items (amulets and rings)');
  if (ntBootsTrader_C in NPCType) then
    Add('Buy items (boots)');
  if (ntSell_C in NPCType) then
    Add('Sell items');
  // Arrows
  if (ntArrTrader_C in NPCType) then
  begin
    V := Player.GetArrowsToBuy;
    if (V > 0) then
      S := ' (' + Items.GetInfo('+', V, 'Arrows') + ' ' + Items.GetPrice(V) + ')'
    else
      S := '';
    Add('Buy arrows' + S);
  end;
  if (ntRuneTrader_D in NPCType) then
    Add('Buy items (runes)');
  // Quests
  {if (ntQuest_D in NPCType) then
    Add('The Hunt (quest)'); }
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
      if (ntQvrTrader_B in NPCType) then
        AddShop(shQuivers);
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
  UI.Title('Selling items' + ' ' + UI.GoldLeft(Player.Gold));

  UI.FromAToZ;
  Items.RenderInventory(ptSell);
  MsgLog.Render(2, True);

  AddKey('A-Z', 'Selling an item');
  AddKey('Esc', 'Close', True);
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
  UI.Title(Format('Buying at %s', [NPCName]) + ' ' + UI.GoldLeft(Player.Gold));

  UI.FromAToZ;
  Shops.Render;
  MsgLog.Render(2, True);

  AddKey('A-Z', 'Buy an item');
  AddKey('Esc', 'Close', True);
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
  UI.Title('Repairing items' + ' ' + UI.GoldLeft(Player.Gold), 1,
    clDarkestRed);

  UI.FromAToZ;
  Items.RenderInventory(ptRepair);
  MsgLog.Render(2, True);

  AddKey('A-Z', 'Repairing an item');
  AddKey('Esc', 'Close', True);
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

  procedure Add(const AText: string; AValue: string; AAdvValue: string = ''); overload;
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

  procedure Add(const AText: string; AValue: Int; AAdvValue: string = ''); overload;
  begin
    Add(AText, AValue.ToString(), AAdvValue);
  end;

begin
  UI.Title('Calendar');

  Y := 10;
  Player.RenderWeather(CX, Y - 6, CX);
  Add('Turn', Player.Statictics.Get(stTurn));
  Add('Time', Calendar.GetTime, Calendar.GetTimeStr);
  Add('Day', Calendar.Day, Calendar.GetDayName);
  Add('Month', Calendar.Month, Calendar.GetMonthName);
  Add('Year', Calendar.Year);
  Add('Map', Map.Name);
  Add('Wind', '');
  Add('Weather', '');

  AddKey('Esc', 'Close', True);
end;

procedure TSceneCalendar.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      // Close
      Scenes.SetScene(scGame);
  end;
end;

{ TSceneIdentification }

procedure TSceneIdentification.Render;
begin
  UI.Title('Identification', 1, clDarkestRed);

  UI.FromAToZ();
  Items.RenderInventory();
  MsgLog.Render(2, True);

  AddKey('A-Z', 'Select an item');
  AddKey('Esc', 'Close', True);
end;

procedure TSceneIdentification.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      Scenes.SetScene(scInv);
    TK_A .. TK_Z:
      Player.IdentItem(Key - TK_A);
    else
      Game.Timer := UIntMax;
  end;
end;

{ TSceneDisenchant }

procedure TSceneDisenchant.Render;
begin
  UI.Title('Disenchanting items', 1, clDarkestRed);

  UI.FromAToZ();
  Items.RenderInventory();
  MsgLog.Render(2, True);

  AddKey('A-Z', 'Select an item to disenchant');
  AddKey('Esc', 'Close', True);
end;

procedure TSceneDisenchant.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      Scenes.SetScene(scInv);
    TK_A .. TK_Z:
      Player.DisenchantItem(Key - TK_A);
    else
      Game.Timer := UIntMax;
  end;
end;

initialization

  Scenes := TScenes.Create();

finalization

  FreeAndNil(Scenes);

end.
