unit Trollhunter.Scenes;

interface

uses
  Classes,
  uBearLibItemsCommon,
  Trollhunter.Mob,
  Trollhunter.Game,
  Trollhunter.Types,
  Trollhunter.Attribute;

type
  TSceneEnum = (scTitle, scLoad, scHelp, scGame, scQuit, scWin, scDef, scInv,
    scDrop, scItems, scAmount, scPlayer, scMessages, scStatistics, scDialog,
    scQuest, scSell, scRepair, scBuy, scCalendar, scDifficulty, scRest, scName,
    scSpellbook, scOptions, scIdentification, scBackground, scEnchant, scClass,
    scRace, scItemInfo);

type
  TScene = class(TObject)
  private
    KStr: string;
  public
    X, Y: Int;
    CX, CY: Int;
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

implementation

uses
  Math,
  SysUtils,
  StrUtils,
  BearLibTerminal,
  uBearLibItemsDungeon,
  uBearLibItemsInventory,
  Trollhunter.Terminal,
  Trollhunter.Player,
  Trollhunter.Map,
  Trollhunter.Item,
  Trollhunter.Item.Helpers,
  Trollhunter.Item.Types,
  Trollhunter.Item.Affixes,
  Trollhunter.Item.Shop,
  Trollhunter.Language,
  Trollhunter.Corpse,
  Trollhunter.Calendar,
  Trollhunter.Helpers,
  Trollhunter.UI,
  Trollhunter.UI.Log,
  Trollhunter.UI.Logo,
  Trollhunter.Entity,
  Trollhunter.Creature,
  Trollhunter.Statistic,
  Trollhunter.Player.Quest,
  Trollhunter.Player.Races,
  Trollhunter.Player.Classes,
  Trollhunter.Player.Spellbook,
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
  Trollhunter.Scene.Difficulty,
  Trollhunter.Scene.Load,
  Trollhunter.Scene.Player,
  Trollhunter.Scene.Game,
  Trollhunter.Scene.Identification,
  Trollhunter.Scene.Spellbook,
  Trollhunter.Scene.Inventory,
  Trollhunter.Scene.Messages,
  Trollhunter.Scene.Calendar,
  Trollhunter.Scene.Item.Amount,
  Trollhunter.Scene.Item.Drop,
  Trollhunter.Scene.Items,
  Trollhunter.Scene.Item.Repair,
  Trollhunter.Dialog,
  Trollhunter.Scene.Item.Information;

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
    '[[' + IfThen(AOption, 'X', ' ') + ']]', TK_ALIGN_RIGHT);
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
  Terminal.Print(Math.IfThen(X = 1, 5, CX + 5), Y, '+++' + UI.KeyToStr(AHotKey,
    AText), TK_ALIGN_LEFT);
  Self.Add();
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
      scItemInfo:
        FScene[I] := TSceneItemInfo.Create;
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
  begin
    Terminal.Print(CX, CY + 5, Terminal.Colorize(_('Wizard Mode'), 'Red') + ' '
      + Format(_('Press %s to continue...'), [UI.KeyToStr('SPACE')]),
      TK_ALIGN_CENTER);
  end;
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

initialization

Scenes := TScenes.Create();

finalization

FreeAndNil(Scenes);

end.
