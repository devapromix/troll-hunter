unit uScenes;

interface

uses
  Classes, BeaRLibItems, uMob, Types;

type
  TSceneEnum = (scTitle, scLoad, scHelp, scGame, scQuit, scWin, scDef, scInv,
    scDrop, scItems, scAmount, scPlayer, scMessages, scStatistics, scDialog,
    scSell, scRepair, scBuy, scCalendar, scDifficulty, scRest, scName, scSpellbook,
    scOptions, scTalent, scTalents);
  // scClasses, scRaces, scIdentification

type
  TScene = class(TObject)
  private
    KStr: string;
    CX, CY: Byte;
  public
    procedure Render; virtual; abstract;
    procedure Update(var Key: Word); virtual; abstract;
    procedure RenderBar(X, LM, Y, Wd: Byte; Cur, Max: Word;
      AColor, DarkColor: Cardinal);
    class function KeyStr(AKey: string; AStr: string = ''): string;
    class procedure Title(ATitleStr: string; AY: Byte = 1);
    procedure AddKey(AKey, AStr: string; IsClear: Boolean = False;
      IsRender: Boolean = False);
    procedure FromAToZ;
    procedure AddKeyLabel(AKey: string; S: string; AColor: Cardinal = $FFFFFFFF);
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
    procedure Update(var Key: Word); override;
    property SceneEnum: TSceneEnum read FSceneEnum write FSceneEnum;
    function GetScene(I: TSceneEnum): TScene;
    procedure SetScene(SceneEnum: TSceneEnum); overload;
    procedure SetScene(SceneEnum, CurrSceneEnum: TSceneEnum); overload;
    property PrevScene: TSceneEnum read FPrevSceneEnum write FPrevSceneEnum;
    procedure GoBack;
  end;

var
  Scenes: TScenes = nil;

type
  TSceneTitle = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TSceneStatistics = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TSceneOptions = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TSceneSpellbook = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TSceneDifficulty = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TSceneCalendar = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TSceneRest = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TSceneDialog = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TSceneBuy = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TSceneTalent = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TSceneTalents = class(TSceneTalent)
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TSceneSell = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TSceneRepair = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TSceneName = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TSceneLoad = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TSceneQuit = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TSceneDef = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TSceneWin = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TSceneInv = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TSceneDrop = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TSceneAmount = class(TScene)
  public
    MaxAmount: Integer;
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TSceneItems = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TSceneHelp = class(TScene)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TSceneGame = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TScenePlayer = class(TScene)
  private
    procedure RenderPlayer;
    procedure RenderSkills;
  public
    constructor Create;
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TSceneMessages = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

var
  NPCName: string = '';
  NPCType: set of TNPCType = [];

implementation

uses
  SysUtils, Dialogs, Math, uTerminal, uPlayer, BearLibTerminal,
  uMap, uMsgLog, uItem, GNUGetText, uGame, uCorpse, uCalendar, uShop,
  uSpellbook, uTalent;

{ TScene }

procedure TScene.AddKey(AKey, AStr: string; IsClear: Boolean = False;
  IsRender: Boolean = False);
begin
  if IsClear then
    KStr := '';
  KStr := KStr + KeyStr(AKey, AStr) + ' ';
  if IsRender and (KStr <> '') then
  begin
    Terminal.ForegroundColor(clDefault);
    Terminal.Print(Terminal.Window.Width div 2, Terminal.Window.Height - 2,
      Trim(Self.KStr), TK_ALIGN_CENTER);
  end;
end;

class function TScene.KeyStr(AKey: string; AStr: string = ''): string;
begin
  Result := Trim(Format(FC + ' %s', [Terminal.GetColorFromIni('Key'),
    Format('[[%s]]', [UpperCase(AKey)]), AStr]));
end;

procedure TScene.RenderBar(X, LM, Y, Wd: Byte; Cur, Max: Word;
  AColor, DarkColor: Cardinal);
var
  I, L, W: Byte;
begin
  L := Wd;
  W := Round(Cur / Max * L);
  for I := 0 to L do
  begin
    Terminal.BackgroundColor(DarkColor);
    if (I <= W) and (Cur > 0) then
      Terminal.BackgroundColor(AColor);
    Terminal.Print(X + I + LM, Y, ' ');
    Terminal.BackgroundColor(0); // Clear background
  end;
end;

class procedure TScene.Title(ATitleStr: string; AY: Byte);
var
  X: Byte;
begin
  X := Terminal.Window.Width div 2;
  Terminal.ForegroundColor(Terminal.GetColorFromIni('Title', 'Yellow'));
  Terminal.Print(X, AY, Format(FT, [ATitleStr]), TK_ALIGN_CENTER);
  Terminal.ForegroundColor(clDefault);
end;

procedure TScene.AddKeyLabel(AKey, S: string; AColor: Cardinal);
begin

end;

procedure TScene.FromAToZ;
var
  I: Byte;
begin
  if Game.Wizard then
    for I := 0 to ItemMax - 1 do
    begin
      Terminal.ForegroundColor(clGray);
      Terminal.Print(1, I + 2, '[[' + Chr(I + Ord('A')) + ']]', TK_ALIGN_LEFT);
    end;
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
      scTalent:
        FScene[I] := TSceneTalent.Create;
      scTalents:
        FScene[I] := TSceneTalents.Create;
    end;
end;

destructor TScenes.Destroy;
var
  I: TSceneEnum;
begin
  for I := Low(TSceneEnum) to High(TSceneEnum) do
    FScene[I].Free;
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

procedure TScenes.SetScene(SceneEnum: TSceneEnum);
begin
  Self.SceneEnum := SceneEnum;
  Render;
end;

procedure TScenes.SetScene(SceneEnum, CurrSceneEnum: TSceneEnum);
begin
  FPrevSceneEnum := CurrSceneEnum;
  SetScene(SceneEnum);
end;

procedure TScenes.Update(var Key: Word);
begin
  if (FScene[SceneEnum] <> nil) then
    FScene[SceneEnum].Update(Key);
  case Key of
    TK_CLOSE:
      begin
        if Game.IsMode and not(SceneEnum in [scWin, scDef, scQuit]) and
          (Player.Life > 0) then
          SetScene(scQuit, SceneEnum);
      end;
  end;
end;

{ TSceneTitle }

procedure TSceneTitle.Render;
begin
  Terminal.Print(CX, CY - 3, Format('%s v.%s', [Game.GetTitle, Game.GetVersion]
    ), TK_ALIGN_CENTER);
  Terminal.Print(CX, CY - 1, 'by Apromix <bees@meta.ua>', TK_ALIGN_CENTER);
  Terminal.Print(CX, CY + 1, Format(_('Press %s to continue...'),
    [KeyStr('ENTER')]), TK_ALIGN_CENTER);
end;

procedure TSceneTitle.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE:
      Game.CanClose := True;
    TK_ENTER, TK_KP_ENTER:
      Scenes.SetScene(scDifficulty);
  end;
end;

{ TSceneHelp }

constructor TSceneHelp.Create;
begin

end;

destructor TSceneHelp.Destroy;
begin

  inherited;
end;

procedure TSceneHelp.Render;
var
  Y, KX, KY: Byte;

  procedure AddKey(Key, Text: string);
  begin
    Terminal.Print(KX + 10, Y + KY, KeyStr(Key, Text), TK_ALIGN_LEFT);
    Inc(KX, CX - 5);
    if (KX > CX + 11) then
    begin
      KX := 0;
      Inc(KY);
    end;
  end;

begin
  Y := 1;
  KX := 0;
  KY := 14;
  Self.Title(_('Help'));

  Terminal.Print(CX, Y + 2,
    _('The land Elvion is surrounded by mountains. In the center of this land'),
    TK_ALIGN_CENTER);
  Terminal.Print(CX, Y + 3,
    _('there is village, Dork. The land is in danger, because The Troll King and'),
    TK_ALIGN_CENTER);
  Terminal.Print(CX, Y + 4,
    _('his armies are coming. Only a legendary hero can kill the monster.'),
    TK_ALIGN_CENTER);

  Terminal.Print(CX, Y + 6,
    _('You play as a lonely hero who has to slay trolls to save your land Elvion.'),
    TK_ALIGN_CENTER);
  Terminal.Print(CX, Y + 7,
    _('You can gather equipment, fight enemies and try to survive for your final'),
    TK_ALIGN_CENTER);
  Terminal.Print(CX, Y + 8, _('confrontation with boss. Good luck!'),
    TK_ALIGN_CENTER);

  Self.Title(_('Keybindings'), Y + 10);

  Terminal.Print(CX, Y + 12, Format('%s: %s, %s, %s %s: %s, %s',
    [_('Move'), KeyStr('arrow keys'), KeyStr('numpad'), KeyStr('QWEADZXC'),
    _('Wait'), KeyStr('5'), KeyStr('S')]), TK_ALIGN_CENTER);

  AddKey('<', _('Go upstairs'));
  AddKey('>', _('Go downstairs'));
  AddKey('G', _('Pickup an item'));
  AddKey('F', _('Drop an item'));
  AddKey('L', _('Look mode'));
  AddKey('R', _('Rest'));
  AddKey('M', _('Last messages'));
//  AddKey('B', _('Spellbook'));
  AddKey('T', _('Talents'));
  AddKey('N', _('Statistics'));
  AddKey('O', _('Options'));
  AddKey('I', _('Inventory'));
  AddKey('P', _('Skills and attributes'));
  AddKey('K', _('Calendar'));
  AddKey('?', _('Help (this page)'));

  Self.Title(_('Character dump'), Terminal.Window.Height - Y - 5);
  Terminal.Print(CX, Terminal.Window.Height - Y - 3,
    Format(_('The game saves a character dump to %s file.'),
    [KeyStr('*-character-dump.txt')]), TK_ALIGN_CENTER);

  Self.AddKey('Esc', _('Close'), True, True);
end;

procedure TSceneHelp.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE: // Close
      Scenes.SetScene(scGame);
  end;
end;

{ TSceneGame }

procedure TSceneGame.Render;
var
  I, X, Y, PX, PY, DX, DY, R: Integer;
  T: TTile;
  Min, Max: TPoint;
  S: string;

  procedure RenderLook(X, Y: Byte; T: TTile; IsMob: Boolean);
  var
    S: string;
    C: Integer;
    FItem: Item;
  begin
    S := '';
    Terminal.BackgroundColor(0);
    Terminal.ForegroundColor(clDefault);
    S := S + T.Name + '. ';
    if Corpses.IsCorpse(X, Y) then
      S := S + _('Corpse') + '. ';
    C := Items_Dungeon_GetMapCountXY(Ord(Map.Current), X, Y);
    if (C > 0) then
    begin
      FItem := Items_Dungeon_GetMapItemXY(Ord(Map.Current), 0, X, Y);
      S := S + Items.GetItemInfo(FItem, (C > 1), C) + ' ';
    end;
    if IsMob then
    begin
      C := Mobs.GetIndex(X, Y);
      if (C > -1) then
      begin
        S := S + Format('%s (%d/%d). ', [Mobs.Name[TMobEnum(Mobs.Mob[C].ID)],
          Mobs.Mob[C].Life, Mobs.Mob[C].MaxLife]);
      end;
    end;
    //
    Terminal.Print(Info.Left, Info.Top, Info.Width, Info.Height, S,
      TK_ALIGN_TOP);
  end;

  procedure AddTo(X, Y: Integer);
  var
    I, L: Integer;
    AX, AY: Byte;
    LR: Real;
  begin
    L := Math.Max(Abs(Player.X - X), Abs(Player.Y - Y)) + 1;
    for I := 1 to L do
    begin
      LR := I / L;
      AX := Map.EnsureRange(Player.X + Trunc((X - Player.X) * LR));
      AY := Map.EnsureRange(Player.Y + Trunc((Y - Player.Y) * LR));
      Map.SetFOV(AX, AY, True);
      if (Map.GetTileEnum(AX, AY, Map.Current) in StopTiles) then
        Exit;
    end;
  end;

begin
  // Map
  R := Player.Radius;
  if not Game.Wizard then
  begin
    Min.X := Player.X - R;
    Max.X := Player.X + R;
    Min.Y := Player.Y - R;
    Max.Y := Player.Y + R;
    Map.ClearFOV;
    for I := Min.X to Max.X do
      AddTo(I, Min.Y);
    for I := Min.Y to Max.Y do
      AddTo(Max.X, I);
    for I := Max.X downto Min.X do
      AddTo(I, Max.Y);
    for I := Max.Y downto Min.Y do
      AddTo(Min.X, I);
  end;
  Terminal.BackgroundColor(clBackground);
  PX := View.Width div 2;
  PY := View.Height div 2;
  if Game.ShowMap then
  for DY := 0 to View.Height - 1 do
    for DX := 0 to View.Width - 1 do
    begin
      X := DX - PX + Player.X;
      Y := DY - PY + Player.Y;
      if not Map.InMap(X, Y) then
        Continue;
      if not Game.Wizard then
        if (Player.GetDist(X, Y) > R) and Map.GetFog(X, Y) then
          Continue;
      T := Map.GetTile(X, Y);
      if (Player.Look and (Player.LX = X) and (Player.LY = Y)) then
      begin
        Terminal.BackgroundColor(clLook);
        Terminal.Print(DX + View.Left, DY + View.Top, ' ');
        RenderLook(X, Y, T, True);
      end;
      if (not Player.Look) and (Player.X = X) and (Player.Y = Y) then
        RenderLook(X, Y, T, False);
      if not Game.Wizard then
      begin
        if (Player.GetDist(X, Y) <= R) then
        begin
          if not Map.GetFog(X, Y) then
            Terminal.ForegroundColor(clFog);
          if Map.GetFOV(X, Y) then
          begin
            Terminal.ForegroundColor(T.Color);
            Map.SetFog(X, Y, False);
          end;
        end
        else
        begin
          if not Map.GetFog(X, Y) then
            Terminal.ForegroundColor(clFog);
        end;
      end
      else
        Terminal.ForegroundColor(T.Color);
      if Game.Wizard or not Map.GetFog(X, Y) then
        Terminal.Print(DX + View.Left, DY + View.Top, T.Symbol);
    end;
  // Items, player's corpses, player, mobs
  Items.Render(PX, PY);
  Corpses.Render(PX, PY);
  Player.Render(PX, PY);
  Mobs.Render(PX, PY);
  // Player info
  Terminal.BackgroundColor(clBackground);
  Terminal.ForegroundColor(clDefault);
  Terminal.Print(Status.Left, Status.Top, Player.Name);
  if Game.Wizard then
    S := Format('%s (%d:%d)', [Map.Name, Player.X, Player.Y])
  else
    S := Map.Name;
  Terminal.Print(Status.Left + Status.Width - 1, Status.Top, S, TK_ALIGN_RIGHT);
  Terminal.ForegroundColor(clDefault);
  // Log
  MsgLog.Render;
end;

procedure TSceneGame.Update(var Key: Word);
begin
  MsgLog.Turn;
  MsgLog.Msg := '';
  if Player.IsDead then
  begin
    if (Game.Difficulty = dfEasy) then
    begin
      Player.Fill;
      Player.X := Game.Spawn.X;
      Player.Y := Game.Spawn.Y;
      Map.Current := deDarkWood;
      Exit;
    end;
    Scenes.SetScene(scDef);
    Exit;
  end;
  if Game.Won then
  begin
    Scenes.SetScene(scWin);
    Exit;
  end;
  case Key of
    TK_LEFT, TK_KP_4, TK_A:
      Player.Move(-1, 0);
    TK_RIGHT, TK_KP_6, TK_D:
      Player.Move(1, 0);
    TK_UP, TK_KP_8, TK_W:
      Player.Move(0, -1);
    TK_DOWN, TK_KP_2, TK_X:
      Player.Move(0, 1);
    TK_KP_7, TK_Q:
      Player.Move(-1, -1);
    TK_KP_9, TK_E:
      Player.Move(1, -1);
    TK_KP_1, TK_Z:
      Player.Move(-1, 1);
    TK_KP_3, TK_C:
      Player.Move(1, 1);
    TK_KP_5, TK_S:
      Player.Wait;
    TK_L: // Look
      begin
        Player.LX := Player.X;
        Player.LY := Player.Y;
        Player.Look := not Player.Look;
      end;
    TK_KP_PLUS:
      if Game.Wizard then
        if (Map.Current < High(TMapEnum)) then
        begin
          Map.Current := Succ(Map.Current);
          Player.Wait;
        end;
    TK_KP_MINUS:
      if Game.Wizard then
        if (Map.Current > Low(TMapEnum)) then
        begin
          Map.Current := Pred(Map.Current);
          Player.Wait;
        end;
    TK_COMMA:
      if (Map.GetTileEnum(Player.X, Player.Y, Map.Current) = teUpStairs) then
        if (Map.Current > Low(TMapEnum)) then
        begin
          Map.Current := Pred(Map.Current);
          Player.Wait;
        end;
    TK_PERIOD:
      begin
      // Portal in town
      if (Map.GetTileEnum(Player.X, Player.Y, Map.Current) = tePortal) then
      begin
        Player.X := Game.Spawn.X;
        Player.Y := Game.Spawn.Y;
        Map.Current := deDarkWood;
        Scenes.SetScene(scGame);
        Exit;
      end;
      // Portal
      if (Map.GetTileEnum(Player.X, Player.Y, Map.Current) = teTownPortal) then
      begin
        Map.SetTileEnum(Player.X, Player.Y, deDarkWood, teStoneFloor);
        Player.X := Game.Portal.X;
        Player.Y := Game.Portal.Y;
        Map.Current := Game.PortalMap;
        Map.SetTileEnum(Player.X, Player.Y, Game.PortalMap, Game.PortalTile);
        Scenes.SetScene(scGame);
        Exit;
      end;
      // Down stairs
      if (Map.GetTileEnum(Player.X, Player.Y, Map.Current) = teDnStairs) then
        if (Map.Current < High(TMapEnum)) then
        begin
          Map.Current := Succ(Map.Current);
          Player.Wait;
        end;
      end;
    TK_KP_MULTIPLY:
      if Game.Wizard then
        Player.Fill;
    TK_ESCAPE:
      begin
        if Player.Look then
        begin
          Player.Look := False;
          Exit;
        end;
        if (Player.Life = 0) then
          Exit;
        Game.Screenshot := Terminal.GetTextScreenshot();
        Scenes.SetScene(scQuit, Scenes.SceneEnum);
      end;
    TK_K:
      Scenes.SetScene(scCalendar);
    TK_R:
      begin
        Game.Timer := High(Byte);
        Scenes.SetScene(scRest);
      end;
    TK_G:
      Player.Pickup;
    TK_I:
      begin
        Game.Timer := High(Byte);
        Scenes.SetScene(scInv);
      end;
    TK_M:
      Scenes.SetScene(scMessages);
    TK_F:
      begin
        Game.Timer := High(Byte);
        Scenes.SetScene(scDrop);
      end;
    TK_P:
      Scenes.SetScene(scPlayer);
    TK_N:
      Scenes.SetScene(scStatistics);
    TK_O:
      Scenes.SetScene(scOptions);
//    TK_B:
//      Scenes.SetScene(scSpellbook);
    TK_T:
      Scenes.SetScene(scTalents);
    TK_SLASH:
      Scenes.SetScene(scHelp);
    TK_Y:
      if Game.Wizard then
        Shops.New;
  end;
end;

{ TSceneLoad }

procedure TSceneLoad.Render;
begin
  Terminal.Print(CX, CY, _('Creating the world, please wait...'),
    TK_ALIGN_CENTER);
end;

procedure TSceneLoad.Update(var Key: Word);
begin

end;

{ TSceneQuit }

procedure TSceneQuit.Render;
begin
  Terminal.Print(Terminal.Window.Width div 2, CY - 1,
    UpperCase(_('Are you sure?')), TK_ALIGN_CENTER);
  Terminal.Print(Terminal.Window.Width div 2, CY + 1,
    Format(_('Wish to leave? %s/%s'), [KeyStr('Y'), KeyStr('N')]),
    TK_ALIGN_CENTER);
end;

procedure TSceneQuit.Update(var Key: Word);
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
  Terminal.Print(CX, CY - 1, UpperCase(_('Game over!!!')), TK_ALIGN_CENTER);
  Terminal.Print(CX, CY + 1, Format(_('Killed by %s. Press %s'),
    [Format(FC, [clAlarm, Player.Killer]), KeyStr('ENTER')]), TK_ALIGN_CENTER);
  if Game.Wizard then
    Terminal.Print(CX, CY + 3, Format(_('Press %s to continue...'),
      [KeyStr('SPACE')]), TK_ALIGN_CENTER);

end;

procedure TSceneDef.Update(var Key: Word);
begin
  case Key of
    TK_ENTER, TK_KP_ENTER:
      begin
        Player.SaveCharacterDump(Format(_('Killed by %s'), [Player.Killer]));
        Game.CanClose := True;
      end;
    TK_SPACE:
      if Game.Wizard then
      begin
        Player.Fill;
        Scenes.SetScene(scGame);
      end;
  end;
end;

{ TSceneWin }

procedure TSceneWin.Render;
begin
  Terminal.Print(CX, CY - 1, UpperCase(_('Congratulations!!!')),
    TK_ALIGN_CENTER);
  Terminal.Print(CX, CY + 1, Format(_('You have won. Press %s'),
    [KeyStr('ENTER')]), TK_ALIGN_CENTER);
end;

procedure TSceneWin.Update(var Key: Word);
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

procedure TSceneInv.Render;
begin
  Self.Title(_('Inventory'));

  Self.FromAToZ;
  Items.RenderInventory;
  MsgLog.Render(2, True);

  AddKey('Esc', _('Close'), True);
  AddKey('Space', _('Skills and attributes'));
  AddKey('A-Z', _('Use an item'), False, True);
end;

procedure TSceneInv.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE: // Close
      Scenes.SetScene(scGame);
    TK_SPACE: // Player
      Scenes.SetScene(scPlayer);
    TK_A .. TK_Z: // Use an item
      Player.Use(Key - TK_A);
  else
    Game.Timer := High(Byte);
  end;
end;

{ TSceneDrop }

procedure TSceneDrop.Render;
begin
  Self.Title(_('Drop an item'));

  Self.FromAToZ;
  Items.RenderInventory;
  MsgLog.Render(2, True);

  AddKey('Esc', _('Close'), True, False);
  AddKey('A-Z', _('Drop an item'), False, True);
end;

procedure TSceneDrop.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE: // Close
      Scenes.SetScene(scGame);
    TK_A .. TK_Z: // Drop an item
      Player.Drop(Key - TK_A);
  else
    Game.Timer := High(Byte);
  end;
end;

{ TScenePlayer }

constructor TScenePlayer.Create;
begin

end;

procedure TScenePlayer.Render;
begin
  Self.Title(Player.Name);

  Self.RenderPlayer;
  Self.RenderSkills;

  AddKey('Esc', _('Close'), True);
  AddKey('Space', _('Inventory'), False, True);
end;

procedure TScenePlayer.RenderPlayer;
var
  X, Y, W: Byte;
begin
  Y := 3;
  X := Terminal.Window.Width div 4;
  W := X * 2 - 3;
  Terminal.Print(X, Y, Format(FT, [_('Attributes')]), TK_ALIGN_CENTER);
  RenderBar(1, 0, Y + 2, W, Player.Exp, LevelExpMax, clDarkRed, clDarkGray);
  Terminal.Print(X, Y + 2, Format('%s %d', [_('Level'), Player.Level]),
    TK_ALIGN_CENTER);
  RenderBar(1, 0, Y + 4, W, Player.Strength, AtrMax, clDarkRed, clDarkGray);
  Terminal.Print(X, Y + 4, Format('%s %d/%d', [_('Strength'), Player.Strength,
    AtrMax]), TK_ALIGN_CENTER);
  RenderBar(1, 0, Y + 6, W, Player.Dexterity, AtrMax, clDarkRed, clDarkGray);
  Terminal.Print(X, Y + 6, Format('%s %d/%d', [_('Dexterity'), Player.Dexterity,
    AtrMax]), TK_ALIGN_CENTER);
  RenderBar(1, 0, Y + 8, W, Player.Willpower, AtrMax, clDarkRed, clDarkGray);
  Terminal.Print(X, Y + 8, Format('%s %d/%d', [_('Willpower'), Player.Willpower,
    AtrMax]), TK_ALIGN_CENTER);
  RenderBar(1, 0, Y + 10, W, Player.Perception, AtrMax, clDarkRed, clDarkGray);
  Terminal.Print(X, Y + 10, Format('%s %d/%d', [_('Perception'),
    Player.Perception, AtrMax]), TK_ALIGN_CENTER);

  RenderBar(1, 0, Y + 14, W, Player.DV, DVMax, clDarkGreen, clDarkGray);
  Terminal.Print(X, Y + 14, Format('%s %d/%d', [_('Defensive Value (DV)'),
    Player.DV, DVMax]), TK_ALIGN_CENTER);
  RenderBar(1, 0, Y + 16, W, Player.PV, PVMax, clDarkGreen, clDarkGray);
  Terminal.Print(X, Y + 16, Format('%s %d/%d', [_('Protection Value (PV)'),
    Player.PV, PVMax]), TK_ALIGN_CENTER);
  RenderBar(1, 0, Y + 18, W, Player.Life, Player.MaxLife, clLife,
    clDarkGray);
  Terminal.Print(X, Y + 18, Format('%s %d/%d', [_('Life'), Player.Life,
    Player.MaxLife]), TK_ALIGN_CENTER);
  RenderBar(1, 0, Y + 20, W, Player.Mana, Player.MaxMana, clMana,
    clDarkGray);
  Terminal.Print(X, Y + 20, Format('%s %d/%d', [_('Mana'), Player.Mana,
    Player.MaxMana]), TK_ALIGN_CENTER);
  RenderBar(1, 0, Y + 22, W, Player.Radius, RadiusMax, clGray, clDarkGray);
  Terminal.Print(X, Y + 22, Format('%s %d/%d', [_('Radius'), Player.Radius,
    RadiusMax]), TK_ALIGN_CENTER);
end;

procedure TScenePlayer.RenderSkills;
var
  I: TSkillEnum;
  A, B, X, Y, D: Byte;
begin
  Y := 3;
  X := Terminal.Window.Width div 2;
  A := Terminal.Window.Width div 4;
  B := A * 3;
  Terminal.Print(B, Y, Format(FT, [_('Skills')]), TK_ALIGN_CENTER);
  for I := Low(TSkillEnum) to High(TSkillEnum) do
  begin
    D := (Ord(I) * 2) + Y + 2;
    RenderBar(X, 0, D, X - 2, Player.GetSkill(I).Value, SkillMax, clDarkRed,
      clDarkGray);
    Terminal.Print(B, D, Format('%s %d/%d', [Player.GetSkillName(I),
      Player.GetSkill(I).Value, SkillMax]), TK_ALIGN_CENTER);
  end;
end;

procedure TScenePlayer.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE: // Close
      Scenes.SetScene(scGame);
    TK_SPACE: // Inventory
      begin
        Game.Timer := High(Byte);
        Scenes.SetScene(scInv);
      end;
  end;
end;

{ TSceneAmount }

procedure TSceneAmount.Render;
var
  FItem: Item;
begin
  Self.Title(_('Enter amount'));

  if Player.ItemIsDrop then
    FItem := Items_Inventory_GetItem(Player.ItemIndex)
  else
    FItem := Items_Dungeon_GetMapItemXY(Ord(Map.Current), Player.ItemIndex,
      Player.X, Player.Y);

  MaxAmount := FItem.Amount;

  Terminal.Print(CX, CY, Format('%d/%dx', [Player.ItemAmount, FItem.Amount]),
    TK_ALIGN_LEFT);

  AddKey('Esc', _('Close'), True, False);
  AddKey('W', _('More'), False, False);
  AddKey('X', _('Less'), False, False);
  AddKey('Enter', _('Apply'), False, True);
end;

procedure TSceneAmount.Update(var Key: Word);

  procedure ChAmount(Value: Integer);
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
  I, FCount, MapID: Integer;
  FItem: Item;
begin
  MapID := Ord(Map.Current);
  Self.Title(_('Pick up an item'));

  Self.FromAToZ;
  FCount := EnsureRange(Items_Dungeon_GetMapCountXY(MapID, Player.X, Player.Y),
    0, ItemMax);
  for I := 0 to FCount - 1 do
  begin
    FItem := Items_Dungeon_GetMapItemXY(MapID, I, Player.X, Player.Y);
    Items.RenderInvItem(5, 2, I, FItem);
  end;

  MsgLog.Render(2, True);

  AddKey('Esc', _('Close'), True, False);
  AddKey('Space', _('Pick up all items'), False, False);
  AddKey('A-Z', _('Pick up an item'), False, True);

  if (FCount <= 0) then
    Scenes.SetScene(scGame);
end;

procedure TSceneItems.Update(var Key: Word);
var
  I, FCount: Integer;
begin
  case Key of
    TK_ESCAPE: // Close
      Scenes.SetScene(scGame);
    TK_SPACE:
    begin
      FCount := EnsureRange(Items_Dungeon_GetMapCountXY(Ord(Map.Current), Player.X, Player.Y),
        0, ItemMax);
      for I := 0 to FCount - 1 do
        Items.AddItemToInv;
    end;
    TK_A .. TK_Z: // Pick up
      Items.AddItemToInv(Key - TK_A);
  else
    Game.Timer := High(Byte);
  end;
end;

{ TSceneMessages }

procedure TSceneMessages.Render;
begin
  Self.Title(_('Last messages'));
  MsgLog.RenderAllMessages;
  AddKey('Esc', _('Close'), True, True);
end;

procedure TSceneMessages.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE: // Close
      Scenes.SetScene(scGame);
  end;
end;

{ TSceneStatistics }

procedure TSceneStatistics.Render;
var
  X, Y: Byte;

  procedure Add(); overload;
  begin
    Inc(X);
    if (X > 2) then
    begin
      X := 1;
      Inc(Y);
    end;
  end;

  procedure Add(AText: string; AValue: Integer); overload;
  begin
    Terminal.ForegroundColor(clWhite);
    Terminal.Print(IfThen(X = 1, 3, CX + 3), Y, AText + ':', TK_ALIGN_LEFT);
    Terminal.ForegroundColor(clGreen);
    Terminal.Print(IfThen(X = 1, CX - 1, CX + (CX - 1)), Y, IntToStr(AValue),
      TK_ALIGN_RIGHT);
    Add();
  end;

  procedure Add(AText: string; AValue: string;
    AColor: Cardinal = clGreen); overload;
  begin
    Terminal.ForegroundColor(clWhite);
    Terminal.Print(IfThen(X = 1, 3, CX + 3), Y, AText + ':', TK_ALIGN_LEFT);
    Terminal.ForegroundColor(AColor);
    Terminal.Print(IfThen(X = 1, CX - 1, CX + (CX - 1)), Y, AValue,
      TK_ALIGN_RIGHT);
    Add();
  end;

begin
  Self.Title(_('Statistics'));
  X := 1;
  Y := 3;

  Add(_('Name'), Player.Name);
  Add(_('Difficulty'), Game.GetStrDifficulty);
  Add(_('Scores'), Player.Score);
//  Add(_('Talent'), Player.GetTalentName(Player.GetTalent(0)));
  Add(_('Tiles Moved'), Player.Turn);
  Add(_('Monsters Killed'), Player.Kills);
  Add(_('Items Found'), Player.Found);
  // Add(_('Chests Found'), );
  // Add(_('Doors Opened'), );
  Add(_('Potions Drunk'), Player.PotDrunk);
  Add(_('Scrolls Read'), Player.ScrRead);
  Add(_('Spells Cast'), Player.SpCast);
  // Add(_('Foods Eaten'), );
  // Add(_('Melee Attack Performed'), );
  // Add(_('Ranged Attack Performed'), );
  // Add(_('Unarmed Attack Performed'), );
  // Add(_('Times Fallen Into Pit'), );
  // Add(_('Items Sold'), );
  // Add(_('Items Identified'), );
  // Add(_('Items Crafted'), );
  // Add(_('Gold from Sales'), );
  // Add(_(''), );

  // Version
  X := 1;
  Y := Y + 3;
  Self.Title(_('Version'), Y - 1);
  Y := Y + 1;
  Add(_('Game version'), Game.GetVersion);
  Add(_('BeaRLibTerminal'), BearLibTerminal.terminal_get('version'));
  Add();
  Add(_('BeaRLibItems'), BeaRLibItems.Items_GetVersion);

  if Game.Wizard then
  begin
    X := 1;
    Y := Y + 3;
    Self.Title(_('Wizard Mode'), Y - 1);
    Y := Y + 1;
    Add(_('Monsters'), Ord(Length(MobBase)) - (13 + 7));
    Add(_('Bosses'), 13);
    Add(_('NPC'), 7);
    Add();
    Add(_('Items'), Ord(Length(ItemBase)));
    Add(_('Shops'), Shops.Count);
  end;

  AddKey('Esc', _('Close'), True, True);
end;

procedure TSceneStatistics.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE: // Close
      Scenes.SetScene(scGame);
  end;
end;

{ TSceneDialog }

procedure TSceneDialog.Render;
var
  V: Integer;
  S: string;
  Y: Byte;
begin
  Self.Title(Format('%s ' + _('(%d gold left)'), [NPCName, Player.Gold]));

  Self.FromAToZ;
  Y := 1;

  // Heal
  if (ntHealer_A in NPCType) then
  begin
    Inc(Y);
    V := Player.MaxLife - Player.Life;
    if (V > 0) then
      S := ' (-' + Items.GetPrice(V) + ')'
    else
      S := '';
    Terminal.Print(1, Y, KeyStr(Chr(Y + 95)) + ' ' + _('Receive healing') + S,
      TK_ALIGN_LEFT);
  end;

  if (ntScrTrader_A in NPCType) then
  begin
    Inc(Y);
    Terminal.Print(1, Y, KeyStr(Chr(Y + 95)) + ' ' + _('Buy items (scrolls)'),
      TK_ALIGN_LEFT);
  end;

  if (ntArmTrader_A in NPCType) then
  begin
    Inc(Y);
    Terminal.Print(1, Y, KeyStr(Chr(Y + 95)) + ' ' + _('Buy items (armors)'),
      TK_ALIGN_LEFT);
  end;

  if (ntShTrader_A in NPCType) then
  begin
    Inc(Y);
    Terminal.Print(1, Y, KeyStr(Chr(Y + 95)) + ' ' + _('Buy items (shields)'),
      TK_ALIGN_LEFT);
  end;

  if (ntHelmTrader_A in NPCType) then
  begin
    Inc(Y);
    Terminal.Print(1, Y, KeyStr(Chr(Y + 95)) + ' ' + _('Buy items (helms)'),
      TK_ALIGN_LEFT);
  end;

  if (ntFoodTrader_A in NPCType) then
  begin
    Inc(Y);
    Terminal.Print(1, Y, KeyStr(Chr(Y + 95)) + ' ' + _('Buy items (foods)'),
      TK_ALIGN_LEFT);
  end;

  // Repair
  if (ntBlacksmith_A in NPCType) then
  begin
    Inc(Y);
    Terminal.Print(1, Y, KeyStr(Chr(Y + 95)) + ' ' + _('Repair items'),
      TK_ALIGN_LEFT);
  end;

  if (ntSmithTrader_B in NPCType) then
  begin
    Inc(Y);
    Terminal.Print(1, Y, KeyStr(Chr(Y + 95)) + ' ' +
      _('Buy items (blacksmith)'), TK_ALIGN_LEFT);
  end;

  if (ntHealTrader_B in NPCType) then
  begin
    Inc(Y);
    Terminal.Print(1, Y, KeyStr(Chr(Y + 95)) + ' ' + _('Buy items (healing)'),
      TK_ALIGN_LEFT);
  end;

  if (ntPotManaTrader_B in NPCType) then
  begin
    Inc(Y);
    Terminal.Print(1, Y, KeyStr(Chr(Y + 95)) + ' ' +
      _('Buy items (potions of mana)'), TK_ALIGN_LEFT);
  end;

  if (ntPotTrader_B in NPCType) then
  begin
    Inc(Y);
    Terminal.Print(1, Y, KeyStr(Chr(Y + 95)) + ' ' + _('Buy items (potions)'),
      TK_ALIGN_LEFT);
  end;

  if (ntTavTrader_B in NPCType) then
  begin
    Inc(Y);
    Terminal.Print(1, Y, KeyStr(Chr(Y + 95)) + ' ' + _('Buy items (tavern)'),
      TK_ALIGN_LEFT);
  end;

  if (ntWpnTrader_B in NPCType) then
  begin
    Inc(Y);
    Terminal.Print(1, Y, KeyStr(Chr(Y + 95)) + ' ' + _('Buy items (weapons)'),
      TK_ALIGN_LEFT);
  end;

  // Sell
  if (ntSell_C in NPCType) then
  begin
    Inc(Y);
    Terminal.Print(1, Y, KeyStr(Chr(Y + 95)) + ' ' + _('Sell items'),
      TK_ALIGN_LEFT);
  end;

  MsgLog.Render(2, True);

  AddKey('Esc', _('Close'), True, True);
end;

procedure TSceneDialog.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE: // Close
      Scenes.SetScene(scGame);
    TK_A: //
      begin
        if (ntHealer_A in NPCType) then
        begin
          Player.ReceiveHealing;
        end;
        if (ntBlacksmith_A in NPCType) then
        begin
          Game.Timer := High(Byte);
          Scenes.SetScene(scRepair);
        end;
        if (ntFoodTrader_A in NPCType) then
        begin
          Game.Timer := High(Byte);
          Shops.Current := shFoods;
          Scenes.SetScene(scBuy);
        end;
        if (ntShTrader_A in NPCType) then
        begin
          Game.Timer := High(Byte);
          Shops.Current := shShields;
          Scenes.SetScene(scBuy);
        end;
        if (ntHelmTrader_A in NPCType) then
        begin
          Game.Timer := High(Byte);
          Shops.Current := shHelms;
          Scenes.SetScene(scBuy);
        end;
        if (ntScrTrader_A in NPCType) then
        begin
          Game.Timer := High(Byte);
          Shops.Current := shScrolls;
          Scenes.SetScene(scBuy);
        end;
        if (ntArmTrader_A in NPCType) then
        begin
          Game.Timer := High(Byte);
          Shops.Current := shArmors;
          Scenes.SetScene(scBuy);
        end;
      end;
    TK_B:
      begin
        if (ntSmithTrader_B in NPCType) then
        begin
          Game.Timer := High(Byte);
          Shops.Current := shSmith;
          Scenes.SetScene(scBuy);
        end;
        if (ntTavTrader_B in NPCType) then
        begin
          Game.Timer := High(Byte);
          Shops.Current := shTavern;
          Scenes.SetScene(scBuy);
        end;
        if (ntHealTrader_B in NPCType) then
        begin
          Game.Timer := High(Byte);
          Shops.Current := shHealer;
          Scenes.SetScene(scBuy);
        end;
        if (ntPotManaTrader_B in NPCType) then
        begin
          Game.Timer := High(Byte);
          Shops.Current := shMana;
          Scenes.SetScene(scBuy);
        end;
        if (ntPotTrader_B in NPCType) then
        begin
          Game.Timer := High(Byte);
          Shops.Current := shPotions;
          Scenes.SetScene(scBuy);
        end;
        if (ntWpnTrader_B in NPCType) then
        begin
          Game.Timer := High(Byte);
          Shops.Current := shWeapons;
          Scenes.SetScene(scBuy);
        end;
      end;
    TK_C: // Selling items
      begin
        if (ntSell_C in NPCType) then
        begin
          Game.Timer := High(Byte);
          Scenes.SetScene(scSell);
        end;
      end;
  end;
end;

{ TSceneSell }

procedure TSceneSell.Render;
begin
  Self.Title(Format(_('Selling items') + ' ' + _('(%d gold left)'),
    [Player.Gold]));

  Self.FromAToZ;
  Items.RenderInventory(ptSell);
  MsgLog.Render(2, True);

  AddKey('Esc', _('Close'), True, False);
  AddKey('A-Z', _('Selling an item'), False, True);
end;

procedure TSceneSell.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE: // Close
      Scenes.SetScene(scDialog);
    TK_A .. TK_Z: // Selling an item
      Player.Sell(Key - TK_A);
  else
    Game.Timer := High(Byte);
  end;
end;

{ TSceneBuy }

procedure TSceneBuy.Render;
begin
  Self.Title(Format(_('Buying at %s') + ' ' + _('(%d gold left)'),
    [NPCName, Player.Gold]));

  Self.FromAToZ;
  Shops.Render;
  MsgLog.Render(2, True);

  AddKey('Esc', _('Close'), True, False);
  AddKey('A-Z', _('Buy an item'), False, True);
end;

procedure TSceneBuy.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE: // Close
      Scenes.SetScene(scDialog);
    TK_A .. TK_Z: // Buy items
      Player.Buy(Key - TK_A);
  else
    Game.Timer := High(Byte);
  end;
end;

{ TSceneRepair }

procedure TSceneRepair.Render;
begin
  Self.Title(Format(_('Repairing items') + ' ' + _('(%d gold left)'),
    [Player.Gold]));

  Self.FromAToZ;
  Items.RenderInventory(ptRepair);
  MsgLog.Render(2, True);

  AddKey('Esc', _('Close'), True, False);
  AddKey('A-Z', _('Repairing an item'), False, True);
end;

procedure TSceneRepair.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE: // Close
      Scenes.SetScene(scDialog);
    TK_A .. TK_Z: // Repairing an item
      Player.Repair(Key - TK_A);
  else
    Game.Timer := High(Byte);
  end;
end;

{ TSceneCalendar }

procedure TSceneCalendar.Render;
var
  Y: Byte;

  procedure Add(const AText: string; AValue: string;
    AAdvValue: string = ''); overload;
  var
    S: string;
    X: Word;
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

  procedure Add(const AText: string; AValue: Integer;
    AAdvValue: string = ''); overload;
  begin
    Add(AText, IntToStr(AValue), AAdvValue);
  end;

begin
  Self.Title(_('Calendar'));

  Y := 12;
  Add(_('Turn'), Player.Turn);
  Add(_('Time'), Calendar.GetTime, Calendar.GetTimeStr);
  Add(_('Day'), Calendar.Day, Calendar.GetDayName);
  Add(_('Month'), Calendar.Month, Calendar.GetMonthName);
  Add(_('Year'), Calendar.Year);
  Add(_('Map'), Map.Name);

  AddKey('Esc', _('Close'), True, True);
end;

procedure TSceneCalendar.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE: // Close
      Scenes.SetScene(scGame);
  end;
end;

{ TSceneDifficulty }

procedure TSceneDifficulty.Render;
begin
  Self.Title(_('Difficulty'));

  Terminal.Print(CX - 5, CY - 3, Format('%s %s', [KeyStr('A'), _('Easy')]),
    TK_ALIGN_LEFT);
  Terminal.Print(CX - 5, CY - 1, Format('%s %s', [KeyStr('B'), _('Normal')]),
    TK_ALIGN_LEFT);
  Terminal.Print(CX - 5, CY + 1, Format('%s %s', [KeyStr('C'), _('Hard')]),
    TK_ALIGN_LEFT);
  Terminal.Print(CX - 5, CY + 3, Format('%s %s', [KeyStr('D'), _('Hell')]),
    TK_ALIGN_LEFT);

  AddKey('Esc', _('Back'), True, True);
end;

procedure TSceneDifficulty.Update(var Key: Word);
begin
  case Key of
    TK_A .. TK_D, TK_ENTER, TK_KP_ENTER:
      begin
        case Key of
          TK_A:
            Game.Difficulty := dfEasy;
          TK_B:
            Game.Difficulty := dfNormal;
          TK_C:
            Game.Difficulty := dfHard;
          TK_D:
            Game.Difficulty := dfHell;
          TK_ENTER, TK_KP_ENTER:
            if Game.Wizard then
              Game.Difficulty := dfNormal
            else
              Exit;
        end;
        Scenes.SetScene(scTalent);
      end;
    TK_ESCAPE:
      Scenes.SetScene(scTitle);
  end;
end;

{ TSceneRest }

procedure TSceneRest.Render;
var
  Y: Byte;
begin
  Self.Title(_('Rest'));

  Self.FromAToZ;
  Y := 1;

  Inc(Y);
  Terminal.Print(1, Y, KeyStr(Chr(Y + 95)) + ' ' + _('Rest for 10 turns'),
    TK_ALIGN_LEFT);
  Inc(Y);
  Terminal.Print(1, Y, KeyStr(Chr(Y + 95)) + ' ' + _('Rest for 100 turns'),
    TK_ALIGN_LEFT);
  Inc(Y);
  Terminal.Print(1, Y, KeyStr(Chr(Y + 95)) + ' ' + _('Rest for 1000 turns'),
    TK_ALIGN_LEFT);

  MsgLog.Render(2, True);

  AddKey('Esc', _('Back'), True, True);
end;

procedure TSceneRest.Update(var Key: Word);
begin
  case Key of
    TK_A, TK_B, TK_C:
      Player.Rest(StrToInt('1' + StringOfChar('0', Key - TK_A + 1)));
    TK_ESCAPE:
      Scenes.SetScene(scGame);
  end
end;

{ TSceneName }

procedure TSceneName.Render;
begin
  Self.Title(_('Name'));

  Terminal.Print(CX - 10, CY, _('Name') + ': ' + Player.Name + Game.GetCursor, TK_ALIGN_LEFT);

  AddKey('Esc', _('Back'), True, True);
end;

procedure TSceneName.Update(var Key: Word);
begin
  case Key of
    TK_BACKSPACE:
    begin
      if (Player.Name <> '') then
        Player.Name := Copy(Player.Name, 1, Length(Player.Name) - 1);
    end;
    TK_ENTER, TK_KP_ENTER:
    begin
      if (Player.Name = '') then
        Player.Name := _('PLAYER');
      Scenes.SetScene(scLoad);
      Terminal.Refresh;
      Terminal_Delay(1000);
      Map.Gen;
      Game.Start();
    end;
    TK_A..TK_Z:
    begin
      if (Length(Player.Name) < 10) then
        Player.Name := Player.Name + Chr(Key - TK_A + 65);
    end;
    TK_ESCAPE:
      Scenes.SetScene(scTalent);
  end;
end;

{ TSceneOptions }

procedure TSceneOptions.Render;
var
  X, Y: Byte;

  procedure Add(); overload;
  begin
    Inc(X);
    if (X > 2) then
    begin
      X := 1;
      Inc(Y);
    end;
  end;

  procedure Add(AHotKey, AText: string; AOption: Boolean; AColor: Cardinal = clWhite); overload;
  begin
    Terminal.ForegroundColor(AColor);
    Terminal.Print(IfThen(X = 1, 3, CX + 3), Y, KeyStr(AHotKey) + ' ' + AText + ':', TK_ALIGN_LEFT);
    Terminal.ForegroundColor(clLightBlue);
    Terminal.Print(Math.IfThen(X = 1, CX - 1, CX + (CX - 1)), Y, '[['
      + Game.IfThen(AOption, 'X', ' ') + ']]', TK_ALIGN_RIGHT);
    Add();
  end;

begin
  Self.Title(_('Options'));

  X := 1;
  Y := 3;
  Add('C', _('Auto pickup coins'), Game.APCoin);
  Add('F', _('Auto pickup foods'), Game.APFood);
  Add('P', _('Auto pickup potions'), Game.APPotion);
  Add('S', _('Auto pickup scrolls'), Game.APScroll);
  Add('R', _('Auto pickup runes'), Game.APRune);
  Add('B', _('Auto pickup books'), Game.APBook);

  if Game.Wizard then
  begin
    X := 1;
    Y := Y + 3;
    Self.Title(_('Wizard Mode'), Y - 1);
    Y := Y + 1;
    Add('W', _('Wizard Mode'), Game.Wizard, clRed);
    Add('M', _('Show map'), Game.ShowMap);
    Add('T', _('Reload all shops'), False);
    Add('L', _('Leave corpses'), Game.LCorpses);
  end;

  AddKey('Esc', _('Back'), True, True);
end;

procedure TSceneOptions.Update(var Key: Word);
begin
  case Key of
    TK_C:
      Game.APCoin := not Game.APCoin;
    TK_F:
      Game.APFood := not Game.APFood;
    TK_P:
      Game.APPotion := not Game.APPotion;
    TK_S:
      Game.APScroll := not Game.APScroll;
    TK_R:
      Game.APRune := not Game.APRune;
    TK_B:
      Game.APBook := not Game.APBook;
    TK_W:
      Game.Wizard := False;
    TK_M:
      if Game.Wizard then Game.ShowMap := not Game.ShowMap;
    TK_L:
      if Game.Wizard then Game.LCorpses := not Game.LCorpses;
    TK_T:
      if Game.Wizard then Shops.New;
    TK_ESCAPE:
      Scenes.SetScene(scGame);
  end
end;

{ TSceneSpells }

procedure TSceneSpellbook.Render;
var
  I: TSpellEnum;
  V, Y: Byte;

  function IsSpell(I: TSpellEnum): Boolean;
  begin
    Result := Spellbook.GetSpell(I).Enable;
    if Game.Wizard then Result := True;
  end;

begin
  Self.Title(_('Spellbook'));

  V := 0;
  Y := 2;
  Self.FromAToZ;
  for I := Low(TSpellEnum) to High(TSpellEnum) do
  if IsSpell(I) then
  begin
    Terminal.Print(1, Y, TScene.KeyStr(Chr(V + Ord('A'))));
    Terminal.ForegroundColor(clGray);
    Terminal.Print(5, Y, Format('(%s) %s %s', [
      Items.GetLevel(Spellbook.GetSpell(I).Spell.Level),
      Spellbook.GetSpellName(I), Items.GetMana('-', Spellbook.GetSpell(I).Spell.ManaCost)]));
    Inc(Y);
    Inc(V);
  end;
  MsgLog.Render(2, True);

  AddKey('Esc', _('Close'), True, False);
  AddKey('A-Z', _('Cast spell'), False, True);
end;

procedure TSceneSpellbook.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE:
      Scenes.SetScene(scGame);
    TK_A..TK_Z:
      Spellbook.DoSpell(Key - TK_A);
  end
end;

{ TSceneTalent }

procedure TSceneTalent.Render;
var
  V, Y: Byte;

  procedure Add(const S, H: string);
  begin
    Terminal.Print(1, Y, TScene.KeyStr(Chr(V + Ord('A'))));
    Terminal.ForegroundColor(clWhite);
    Terminal.Print(5, Y, S);
    Terminal.ForegroundColor(clGray);
    Terminal.Print(20, Y, H);
    Inc(Y);
    Inc(V);
  end;

begin
  Self.Title(_('Talent'));

  V := 0;
  Y := 2;
  Self.FromAToZ;

{  Add(Player.GetTalentName(tlStrong), Player.GetTalentHint(tlStrong));
  Add(Player.GetTalentName(tlDextrous), Player.GetTalentHint(tlDextrous));
  Add(Player.GetTalentName(tlMage), Player.GetTalentHint(tlMage));
  Add(Player.GetTalentName(tlTough), Player.GetTalentHint(tlTough));
  Add(Player.GetTalentName(tlWealthy), Player.GetTalentHint(tlWealthy));}

  AddKey('Esc', _('Back'), True, True);
end;

procedure TSceneTalent.Update(var Key: Word);
begin
  case Key of
    TK_A .. TK_Z, TK_ENTER, TK_KP_ENTER:
      begin
        {case Key of
          TK_ENTER, TK_KP_ENTER:
            if Game.Wizard then
              Player.Talent := tlNone;
          TK_A:
            begin
              Player.Talent := tlStrong;
            end;
          TK_B:
            begin
              Player.Talent := tlDextrous;
            end;
          TK_C:
            begin
              Player.Talent := tlMage;
            end;
          TK_D:
            begin
              Player.Talent := tlTough;
            end;
          TK_E:
            begin
              Player.Talent := tlWealthy;
            end;
        end;}
        Scenes.SetScene(scName);
      end;
    TK_ESCAPE:
      Scenes.SetScene(scDifficulty);
  end;
end;

{ TSceneTalents }

procedure TSceneTalents.Render;
var
  I, V, Y: Byte;

  procedure Add(const S, H: string; F: Boolean = True);
  begin
    if F then
      Terminal.Print(1, Y, TScene.KeyStr(Chr(V + Ord('A'))))
        else begin
          Terminal.ForegroundColor(clWhite);
          Terminal.Print(1, Y, '[[' + Chr(V + Ord('A')) + ']]');
        end;
    Terminal.ForegroundColor(clWhite);
    Terminal.Print(5, Y, S);
    Terminal.ForegroundColor(clGray);
    Terminal.Print(20, Y, H);
    Inc(Y);
    Inc(V);
  end;

begin
  Self.Title(_('Talents'));

  V := 0;
  Y := 2;
  Self.FromAToZ;

  Terminal.ForegroundColor(clGray);

  for I := 0 to TalentMax - 1 do
    if (Talents.Talent[I].Enum <> tlNone) then
      Add(Talents.GetName(TTalentEnum(0)), Talents.GetHint(TTalentEnum(0)), False);

  MsgLog.Render(2, True);

  AddKey('Esc', _('Close'), True, False);
  AddKey('A-Z', _('Teach talent'), False, True);
end;

procedure TSceneTalents.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE:
      Scenes.SetScene(scGame);
    TK_A..TK_Z:
      ;
  end
end;

initialization

Scenes := TScenes.Create;

finalization

FreeAndNil(Scenes);

end.
