unit uScenes;

interface

uses
  Classes, BeaRLibItems;

type
  TSceneEnum = (scTitle, scLoad, scHelp, scGame, scQuit, scWin, scDef, scInv,
    scDrop, scItems, scAmount, scPlayer, scMessages);

type
  TScene = class(TObject)
  private
    KStr: string;
  public
    procedure Render; virtual; abstract;
    procedure Update(var Key: Word); virtual; abstract;
    procedure RenderBar(X, LM, Y, Wd: Byte; Cur, Max: Word;
      AColor, DarkColor: Cardinal);
    class function KeyStr(AKey: string; AStr: string = ''): string;
    class procedure Title(ATitleStr: string; AY: Byte = 1);
    procedure AddKey(AKey, AStr: string; IsClear: Boolean = False; IsRender: Boolean = False);
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
    property Scene: TSceneEnum read FSceneEnum write FSceneEnum;
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

implementation

uses
  SysUtils, Types, Dialogs, Math, uCommon, uTerminal, uPlayer, BearLibTerminal,
  uMap, uMob, uMsgLog, uItem, gnugettext, uGame, uVillage;

{ TScene }

procedure TScene.AddKey(AKey, AStr: string; IsClear: Boolean = False; IsRender: Boolean = False);
begin
  if IsClear then KStr := '';
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
  Result := Trim(Format('[color=%s][[%s]][/color] %s', [LowerCase(terminal_get('ini.colors.key')), UpperCase(AKey), AStr]));
end;

procedure TScene.RenderBar(X, LM, Y, Wd: Byte; Cur, Max: Word;
  AColor, DarkColor: Cardinal);
var
  I, L, W: Byte;
begin
  L := Wd;
  W := BarWidth(Cur, Max, L);
  for I := 0 to L do
  begin
    Terminal.BackgroundColor(DarkColor);
    if (I <= W) then
    begin
      if (Cur > 0) then
      begin
        Terminal.BackgroundColor(AColor);
      end;
    end;
    Terminal.Print(X + I + LM, Y, ' ');
    Terminal.BackgroundColor(0); // Clear background
  end;
end;

class procedure TScene.Title(ATitleStr: string; AY: Byte);
var
  X: Byte;
begin
  X := Terminal.Window.Width div 2;
  Terminal.ForegroundColor(color_from_name(LowerCase(terminal_get('ini.colors.title'))));
  Terminal.Print(X, AY, Format(FT, [ATitleStr]), TK_ALIGN_CENTER);
  Terminal.ForegroundColor(clDefault);
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
  Self.Scene := FPrevSceneEnum;
end;

procedure TScenes.Render;
begin
  Terminal.BackgroundColor(clBackground);
  Terminal.ForegroundColor(clDefault);
  Terminal.Clear;
  if (FScene[Scene] <> nil) then
    FScene[Scene].Render;
end;

procedure TScenes.SetScene(SceneEnum: TSceneEnum);
begin
  Self.Scene := SceneEnum;
  Render;
end;

procedure TScenes.SetScene(SceneEnum, CurrSceneEnum: TSceneEnum);
begin
  FPrevSceneEnum := CurrSceneEnum;
  SetScene(SceneEnum);
end;

procedure TScenes.Update(var Key: Word);
begin
  if (FScene[Scene] <> nil) then
    FScene[Scene].Update(Key);
  case Key of
    TK_CLOSE:
      begin
        if Game.IsMode and not(Scene in [scWin, scDef, scQuit]) and
          (Player.Life > 0) then
          SetScene(scQuit, Scene);
      end;
  end;
end;

{ TSceneTitle }

procedure TSceneTitle.Render;
var
  X, Y: Byte;
begin
  X := Terminal.Window.Width div 2;
  Y := Terminal.Window.Height div 2;
  if Game.Wizard then
    Terminal.Print(X, Y - 5, '1 [color=red] 2 [color=green] 3 [/color] 2 [/color] 1', TK_ALIGN_CENTER);
  Terminal.Print(X, Y - 3, Format('%s v.%s', [_('Trollhunter'), Version]), TK_ALIGN_CENTER);
  Terminal.Print(X, Y - 1, 'by Apromix <bees@meta.ua>', TK_ALIGN_CENTER);
  Terminal.Print(X, Y + 1,
    Format(_('Press %s to continue...'), [KeyStr('ENTER')]), TK_ALIGN_CENTER);
end;

procedure TSceneTitle.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE:
      Game.CanClose := True;
    TK_ENTER, TK_KP_ENTER:
      begin
        Scenes.SetScene(scLoad);
        Terminal.Refresh;
        Map.Gen;
        terminal_delay(1000);
        Game.Start();
      end;
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
  X, Y, KX, KY: Byte;

  procedure AddKey(Key, Text: string);
  begin
    Terminal.Print(KX + 10, Y + KY, KeyStr(Key, Text), TK_ALIGN_LEFT);
    Inc(KX, X - 5);
    if (KX > X + 11) then
    begin
      KX := 0;
      Inc(KY);
    end;
  end;

begin
  Y := 1;
  KX := 0;
  KY := 14;
  X := Terminal.Window.Width div 2;
  Self.Title(_('Trollhunter'));

  Terminal.Print(X, Y + 2,
    _('The land Elvion is surrounded by mountains. In the center of this land'),
    TK_ALIGN_CENTER);
  Terminal.Print(X, Y + 3,
    _('there is village, Dork. The land is in danger, because The Troll King and'),
    TK_ALIGN_CENTER);
  Terminal.Print(X, Y + 4,
    _('his armies are coming. Only a legendary hero can kill the monster.'),
    TK_ALIGN_CENTER);

  Terminal.Print(X, Y + 6,
    _('You play as a lonely hero who has to slay trolls to save your land Elvion.'),
    TK_ALIGN_CENTER);
  Terminal.Print(X, Y + 7,
    _('You can gather equipment, fight enemies and try to survive for your final'),
    TK_ALIGN_CENTER);
  Terminal.Print(X, Y + 8, _('confrontation with boss. Good luck!'),
    TK_ALIGN_CENTER);

  Self.Title(_('Keybindings'), Y + 10);

  Terminal.Print(KX + 10, Y + 12,
    Format('%s: %s, %s, %s Wait: %s, %s', [_('Move'), KeyStr('arrow keys'), KeyStr('numpad'),
    KeyStr('QWEADZXC'), KeyStr('5'), KeyStr('S')]), TK_ALIGN_LEFT);

  AddKey('<', _('Up staris'));
  AddKey('>', _('Down staris'));
  AddKey('G', _('Pickup an item'));
  AddKey('F', _('Drop an item'));
  AddKey('L', _('Look mode'));
  AddKey('R', _('Rest'));
  AddKey('M', _('Last messages'));
  AddKey('I', _('Inventory'));
  AddKey('P', _('Skills and attributes'));
  AddKey('?', _('Help'));

  Self.Title(_('Character dump'), Terminal.Window.Height - Y - 5);
  Terminal.Print(X, Terminal.Window.Height - Y - 3,
    Format(_('The game saves a character dump to %s file.'), [KeyStr('*-character-dump.txt')]),
    TK_ALIGN_CENTER);

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

  procedure RenderLook(X, Y: Byte; T: TTile; IsMob: Boolean);
  var
    S: string;
    C: Integer;
    FItem: Item;
  begin
    S := '';
    Terminal.BackgroundColor(clBackground);
    Terminal.ForegroundColor(clDefault);
    S := S + T.Name + '. ';
    C := Items_Dungeon_GetMapCountXY(Ord(Map.Deep), X, Y);
    if (C > 0) then
    begin
      FItem := Items_Dungeon_GetMapItemXY(Ord(Map.Deep), 0, X, Y);
      S := S + Items.GetMapItemInfo(FItem, (C > 1), C) + ' ';
    end;
    if IsMob then
    begin
      C := Mobs.GetIndex(X, Y);
      if (C > -1) then
      begin
        S := S + Format('%s (%d/%d). ',
          [Mobs.GetName(TMobEnum(Mobs.FMob[C].ID)), Mobs.FMob[C].Life,
          MobBase[TMobEnum(Mobs.FMob[C].ID)].MaxLife]);
      end;
    end;

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
      AX := Clamp(Player.X + Trunc((X - Player.X) * LR), 0, High(Byte));
      AY := Clamp(Player.Y + Trunc((Y - Player.Y) * LR), 0, High(Byte));
      Map.SetFOV(AX, AY, True);
      if (Map.GetTileEnum(AX, AY, Map.Deep) in StopTiles) then
        Exit;
    end;
  end;

begin
  // Map
  R := Player.GetRadius;
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
  for DY := 0 to View.Height - 1 do
    for DX := 0 to View.Width - 1 do
    begin
      X := DX - PX + Player.X;
      Y := DY - PY + Player.Y;
      if not Map.InMap(X, Y) then
        Continue;
      if not Game.Wizard then
        if (GetDist(Player.X, Player.Y, X, Y) > R) and
          Map.GetFog(X, Y) then
          Continue;
      T := Map.GetTile(X, Y);
      if (Player.Look and (Player.LX = X) and (Player.LY = Y)) then
      begin
        Terminal.BackgroundColor($88FFFF00);
        Terminal.Print(DX + View.Left, DY + View.Top, ' ');
        RenderLook(X, Y, T, True);
      end;
      if (not Player.Look) and (Player.X = X) and (Player.Y = Y) then
        RenderLook(X, Y, T, False);
      if not Game.Wizard then
      begin
        if (GetDist(Player.X, Player.Y, X, Y) <= R) then
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
  // Items, player, mobs
  Items.Render(PX, PY);
  Player.Render(PX, PY);
  Mobs.Render(PX, PY);
  // Player info
  Terminal.BackgroundColor(clBackground);
  Terminal.ForegroundColor(clDefault);
  Terminal.Print(Status.Left, Status.Top, _('Trollhunter'));
  Terminal.Print(Status.Left + Status.Width - 1, Status.Top,
    Format('%s (%d:%d)', [Map.GetName, Player.X, Player.Y]), TK_ALIGN_RIGHT);
  Terminal.ForegroundColor(clDefault);
  Terminal.Print(Status.Left, Status.Top + 1,
    Format('%s %d/%d', [_('Life'), Player.Life, Player.MaxLife]));
  Terminal.Print(Status.Left, Status.Top + 2,
    Format('%s %d/%d', [_('Mana'), Player.Mana, Player.MaxMana]));
  Terminal.ForegroundColor(clDefault);
  Terminal.Print(Status.Left, Status.Top + 3,
    Format(_('Turn: %d Gold: %d Food: %d Score: %d'),
    [Player.Turn, Player.Gold, Player.Food, Player.Score]));
  Terminal.Print(Status.Left, Status.Top + 4, Format(_('Damage: %d-%d Protection: %d'),
    [Player.Damage.Min, Player.Damage.Max, Player.PV]));
  // Bars
  Self.RenderBar(Status.Left, 13, Status.Top + 1, Status.Width - 14,
    Player.Life, Player.MaxLife, clDarkRed, clDarkGray);
  Self.RenderBar(Status.Left, 13, Status.Top + 2, Status.Width - 14,
    Player.Mana, Player.MaxMana, clDarkBlue, clDarkGray);
  // Log
  MsgLog.Render;
end;

procedure TSceneGame.Update(var Key: Word);
begin
  MsgLog.Turn;
  MsgLog.Msg := '';
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
        if (Map.Deep < High(TDeepEnum)) then
        begin
          Map.Deep := succ(Map.Deep);
          Player.Wait;
        end;
    TK_KP_MINUS:
      if Game.Wizard then
        if (Map.Deep > Low(TDeepEnum)) then
        begin
          Map.Deep := pred(Map.Deep);
          Player.Wait;
        end;
    TK_COMMA:
      if (Map.GetTileEnum(Player.X, Player.Y, Map.Deep) = teUpStairs) then
        if (Map.Deep > Low(TDeepEnum)) then
        begin
          Map.Deep := pred(Map.Deep);
          Player.Wait;
        end;
    TK_PERIOD:
      if (Map.GetTileEnum(Player.X, Player.Y, Map.Deep) = teDnStairs) then
        if (Map.Deep < High(TDeepEnum)) then
        begin
          Map.Deep := succ(Map.Deep);
          Player.Wait;
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
        Game.Screenshot := GetTextScreenshot();
        Scenes.SetScene(scQuit, Scenes.Scene);
      end;
    TK_R:
      Player.Rest(100);
    TK_G:
      Player.Pickup;
    TK_I:
      Scenes.SetScene(scInv);
    TK_M:
      Scenes.SetScene(scMessages);
    TK_F:
      Scenes.SetScene(scDrop);
    TK_P:
      Scenes.SetScene(scPlayer);
    TK_V:
      if Game.Wizard then
        Scenes.SetScene(scWin);
    TK_B:
      if Game.Wizard then
        Scenes.SetScene(scDef);
    TK_SLASH:
      Scenes.SetScene(scHelp);
  end;
end;

{ TSceneLoad }

procedure TSceneLoad.Render;
begin
  Terminal.Print(Terminal.Window.Width div 2, Terminal.Window.Height div 2,
    _('Creating the world, please wait...'), TK_ALIGN_CENTER);
end;

procedure TSceneLoad.Update(var Key: Word);
begin

end;

{ TSceneQuit }

procedure TSceneQuit.Render;
var
  Y: Byte;
begin
  Y := Terminal.Window.Height div 2;
  Terminal.Print(Terminal.Window.Width div 2, Y - 1, _('Are you sure?'), TK_ALIGN_CENTER);
  Terminal.Print(Terminal.Window.Width div 2, Y + 1, Format(_('Quit? %s/%s'), [KeyStr('Y'), KeyStr('N')]), TK_ALIGN_CENTER);
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
var
  X, Y: Byte;
begin
  X := Terminal.Window.Width div 2;
  Y := Terminal.Window.Height div 2;
  Terminal.Print(X, Y - 1, _('GAME OVER!!!'), TK_ALIGN_CENTER);
  Terminal.Print(X, Y + 1,
    Format(_('Killed by [color=white]%s[/color]. Press %s'),
    [Player.Killer, KeyStr('ENTER')]), TK_ALIGN_CENTER);
  if Game.Wizard then
    Terminal.Print(X, Y + 3, Format(_('Press %s to continue...'),
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
var
  X, Y: Byte;
begin
  X := Terminal.Window.Width div 2;
  Y := Terminal.Window.Height div 2;
  Terminal.Print(X, Y - 1, _('CONGRATULATIONS!!!'), TK_ALIGN_CENTER);
  Terminal.Print(X, Y + 1,
    Format(_('You have won. Press %s'), [KeyStr('ENTER')]),
    TK_ALIGN_CENTER);
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
var
  I, FCount: Integer;
  FItem: Item;
  X: Byte;
  S: string;
begin
  X := Terminal.Window.Width div 2;
  Self.Title(_('Inventory'));

  if Game.Wizard then
  for I := 0 to 25 do
  begin
    Terminal.ForegroundColor(clGray);
    Terminal.Print(1, I + 2, '[[' + Chr(I + Ord('A')) + ']]', TK_ALIGN_LEFT);
  end;

  FCount := Clamp(Items_Inventory_GetCount(), 0, 26);
  for I := 0 to FCount - 1 do
  begin
    FItem := Items_Inventory_GetItem(I);
    Items.RenderInvItem(5, 2, I, FItem, True);
  end;

  AddKey('Esc', _('Close'), True);
  AddKey('Space', _('Skills and attributes'));
  AddKey('A-Z', _('Select an item'), False, True);
end;

procedure TSceneInv.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE:  // Close
      Scenes.SetScene(scGame);
    TK_SPACE:   // Player
      Scenes.SetScene(scPlayer);
    TK_A..TK_Z: // Use an item
      Player.Use(Key - TK_A);
  end;
end;

{ TSceneDrop }

procedure TSceneDrop.Render;
var
  I, FCount: Integer;
  FItem: Item;
  X: Byte;
begin
  X := Terminal.Window.Width div 2;
  Self.Title(_('Drop an item'));

  if Game.Wizard then
  for I := 0 to 25 do
  begin
    Terminal.ForegroundColor(clGray);
    Terminal.Print(1, I + 2, '[[' + Chr(I + Ord('A')) + ']]', TK_ALIGN_LEFT);
  end;

  FCount := Clamp(Items_Inventory_GetCount(), 0, 26);
  for I := 0 to FCount - 1 do
  begin
    FItem := Items_Inventory_GetItem(I);
    Items.RenderInvItem(5, 2, I, FItem);
  end;

  AddKey('Esc', _('Close'), True, False);
  AddKey('A-Z', _('Drop an item'), False, True);
end;

procedure TSceneDrop.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE:  // Close
      Scenes.SetScene(scGame);
    TK_A..TK_Z: // Drop an item
      Player.Drop(Key - TK_A);
  end;
end;

{ TScenePlayer }

constructor TScenePlayer.Create;
begin

end;

procedure TScenePlayer.Render;
var
  X: Byte;
begin
  X := Terminal.Window.Width div 2;
  Self.Title(_('Trollhunter'));

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
  RenderBar(1, 0, Y + 2, W, Player.Exp, ExpMax, clDarkRed, clDarkGray);
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

  RenderBar(1, 0, Y + 14, W, Player.GetDV, DVMax, clDarkGreen, clDarkGray);
  Terminal.Print(X, Y + 14, Format('%s %d/%d', [_('Defensive Value (DV)'),
    Player.GetDV, DVMax]), TK_ALIGN_CENTER);
  RenderBar(1, 0, Y + 16, W, Player.GetPV, PVMax, clDarkGreen, clDarkGray);
  Terminal.Print(X, Y + 16, Format('%s %d/%d', [_('Protection Value (PV)'),
    Player.GetPV, PVMax]), TK_ALIGN_CENTER);
  RenderBar(1, 0, Y + 18, W, Player.Life, Player.MaxLife, clDarkRed,
    clDarkGray);
  Terminal.Print(X, Y + 18, Format('%s %d/%d', [_('Life'), Player.Life,
    Player.MaxLife]), TK_ALIGN_CENTER);
  RenderBar(1, 0, Y + 20, W, Player.Mana, Player.MaxMana, clDarkBlue,
    clDarkGray);
  Terminal.Print(X, Y + 20, Format('%s %d/%d', [_('Mana'), Player.Mana,
    Player.MaxMana]), TK_ALIGN_CENTER);
  RenderBar(1, 0, Y + 22, W, Player.GetRadius, RadiusMax, clGray, clDarkGray);
  Terminal.Print(X, Y + 22, Format('%s %d/%d', [_('Radius'), Player.GetRadius,
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
      Scenes.SetScene(scInv);
  end;
end;

{ TSceneAmount }

procedure TSceneAmount.Render;
var
  I, FCount: Integer;
  FItem: Item;
  X, Y: Byte;
begin
  Y := 1;
  X := Terminal.Window.Width div 2;
  Self.Title(_('Enter amount'));

{  FCount := Items_Inventory_GetCount();
  for I := 0 to FCount - 1 do
  begin
    FItem := Items_Inventory_GetItem(I);
    Items.RenderInvItem(5, 2, I, FItem);
  end;  }

  AddKey('Esc', _('Close'), True, True);
end;

procedure TSceneAmount.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE: // Close
      Scenes.SetScene(scGame);
  end;
end;

{ TSceneItems }

procedure TSceneItems.Render;
var
  I, FCount, MapID: Integer;
  FItem: Item;
  X, Y: Byte;
begin
  Y := 1;
  MapID := Ord(Map.Deep);
  X := Terminal.Window.Width div 2;
  Self.Title(_('Pick up an item'));

  if Game.Wizard then
  for I := 0 to 25 do
  begin
    Terminal.ForegroundColor(clGray);
    Terminal.Print(1, I + 2, '[[' + Chr(I + Ord('A')) + ']]', TK_ALIGN_LEFT);
  end;

  FCount := Clamp(Items_Dungeon_GetMapCountXY(MapID, Player.X, Player.Y), 0, 26);
  for I := 0 to FCount - 1 do
  begin
    FItem := Items_Dungeon_GetMapItemXY(MapID, I, Player.X, Player.Y);
    Items.RenderInvItem(5, 2, I, FItem);
  end;

  AddKey('Esc', _('Close'), True, False);
  AddKey('A-Z', _('Pick up an item'), False, True);
end;

procedure TSceneItems.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE: // Close
      Scenes.SetScene(scGame);
    TK_A..TK_Z:
      Items.AddItemToInv(Key - TK_A);
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

initialization

Scenes := TScenes.Create;

finalization

FreeAndNil(Scenes);

end.
