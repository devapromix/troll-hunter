unit uScenes;

interface

uses
  Classes, Types, uTypes, uBearLibItemsCommon, uMob, uGame;

type
  TSceneEnum = (scTitle, scLoad, scHelp, scGame, scQuit, scWin, scDef, scInv, scDrop, scItems, scAmount, scPlayer,
    scMessages, scStatistics, scDialog, scQuest, scSell, scRepair, scBuy, scCalendar, scDifficulty, scRest, scName,
    scSpellbook, scOptions, scTalents, scIdentification, scBackground, scCraft, scClass, scRace);

type
  TScene = class(TObject)
  private
    KStr: string;
    X, Y, CX, CY: Int;
    procedure AddOption(AHotKey, AText: string; AOption: Boolean; AColor: Cardinal = $FFAAAAAA); overload;
    procedure AddLine(AHotKey, AText: string);
    procedure Add(); overload;
    procedure Add(AText: string; AValue: Int); overload;
    procedure Add(AText: string; AValue: string; AColor: Cardinal = $FF00FF00); overload;
  public
    constructor Create;
    procedure Render; virtual; abstract;
    procedure Update(var Key: UInt); virtual; abstract;
    procedure AddKey(AKey, AStr: string; IsRender: Boolean = False); overload;
    procedure AddKey(AKey, AStr, AAdvStr: string; IsRender: Boolean = False); overload;
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
  TSceneStatistics = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

type
  TSceneOptions = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

type
  TSceneSpellbook = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

type
  TSceneDifficulty = class(TScene)
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
  TSceneQuest = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

type
  TSceneRest = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

type
  TSceneBackground = class(TScene)
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
  TSceneName = class(TScene)
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
  TSceneHelp = class(TScene)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

type
  TSceneGame = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

type
  TScenePlayer = class(TScene)
  private
    D, W: UInt;
    FSkillCursorTop: ShortInt;
    FRenderInfo: Boolean;
    procedure RenderPlayer;
    procedure RenderInfo;
    procedure RenderSkills;
    procedure Add(const AStr, AIcons, ABarColor: string; const ACur, AMax: Int);
  public
    constructor Create;
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
  TSceneCraft = class(TScene)
  public
    Suffix: UInt;
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

type
  TSceneRace = class(TScene)
  public
    procedure ReRoll;
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

type
  TSceneClass = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

var
  NPCName: string = '';
  NPCType: set of TNPCType = [];

implementation

uses
  SysUtils, Math, uTerminal, uPlayer, BearLibTerminal,
  uMap, uMsgLog, uItem, uLanguage, uCorpse, uCalendar, uShop,
  uSpellbook, uTalent, uSkill, uLogo, uEntity, uCreature, uStatistic,
  uAttribute, uUI, uBearLibItemsDungeon, uBearLibItemsInventory, uQuest,
  uAffixes, uHelpers, uRace;

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

procedure TScene.AddOption(AHotKey, AText: string; AOption: Boolean; AColor: Cardinal);
begin
  Terminal.ForegroundColor(AColor);
  Terminal.Print(IfThen(X = 1, 2, CX + 2), Y, UI.KeyToStr(AHotKey) + ' ' + AText + ':', TK_ALIGN_LEFT);
  Terminal.ForegroundColor(clLightestBlue);
  Terminal.Print(Math.IfThen(X = 1, CX - 2, CX + (CX - 2)), Y, '[[' + Game.IfThen(AOption, 'X', ' ') + ']]',
    TK_ALIGN_RIGHT);
  Self.Add();
end;

constructor TScene.Create;
begin
  KStr := '';
end;

procedure TScene.AddKey(AKey, AStr: string; IsRender: Boolean = False);
begin
  KStr := KStr + UI.KeyToStr(AKey, AStr) + ' ';
  if (IsRender and (KStr <> '')) then
  begin
    Terminal.ForegroundColor(clDefault);
    Terminal.Print(Terminal.Window.Width div 2, Terminal.Window.Height - 2, Trim(Self.KStr), TK_ALIGN_CENTER);
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
  Terminal.Print(Math.IfThen(X = 1, 5, CX + 5), Y, UI.KeyToStr(AHotKey, AText), TK_ALIGN_LEFT);
  Self.Add();
end;

procedure TScene.Add(AText: string; AValue: Int);
begin
  Terminal.ForegroundColor(clWhite);
  Terminal.Print(IfThen(X = 1, 2, CX + 2), Y, AText + ':', TK_ALIGN_LEFT);
  Terminal.ForegroundColor(clGreen);
  Terminal.Print(IfThen(X = 1, CX - 2, CX + (CX - 2)), Y, AValue.ToString(), TK_ALIGN_RIGHT);
  Self.Add();
end;

procedure TScene.Add(AText: string; AValue: string; AColor: Cardinal);
begin
  Terminal.ForegroundColor(clWhite);
  Terminal.Print(IfThen(X = 1, 2, CX + 2), Y, AText + ':', TK_ALIGN_LEFT);
  Terminal.ForegroundColor(AColor);
  Terminal.Print(IfThen(X = 1, CX - 2, CX + (CX - 2)), Y, AValue, TK_ALIGN_RIGHT);
  Self.Add();
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
      scCraft:
        FScene[I] := TSceneCraft.Create;
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
        if Mode.Game and not(SceneEnum in [scWin, scDef, scQuit]) and not Player.IsDead then
          SetScene(scQuit, SceneEnum);
      end;
  end;
end;

{ TSceneTitle }

procedure TSceneTitle.Render;
begin
  Logo.Render(True);
  Terminal.Print(Screen.Width - ((Screen.Width div 2) - (Logo.Width div 2) + 2), 14,
    Format('by Apromix v.%s', [Game.GetVersion]), TK_ALIGN_RIGHT);
  // RenderHeroes;
  if Mode.Wizard then
  begin
    Self.AddKey('Space', _('Create a new hero'));
    Self.AddKey('Z', _('Turn Wizard Mode Off'), True);
  end
  else
    Self.AddKey('Space', _('Create a new hero'), True);
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
  Terminal.Print(L + 4, T, _('Which hero shall you play?'));

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

{ TSceneHelp }

constructor TSceneHelp.Create;
begin

end;

destructor TSceneHelp.Destroy;
begin

  inherited;
end;

procedure TSceneHelp.Render;
begin
  UI.Title(_('Help'));

  case Scenes.FPrevSceneEnum of
    scInv:
      begin
        Terminal.Print(CX, 3, Format(_('To drop an item, press the %s key and then press %s key to drop it.'),
          [UI.KeyToStr('TAB'), UI.KeyToStr('A-Z')]), TK_ALIGN_CENTER);

        UI.Title(_('Keybindings'), 5);
        X := 1;
        Y := 7;
        AddLine('Tab', _('Drop an item to the floor'));
        AddLine('Space', _('Character Screen'));
        AddLine('A-Z', _('Use an item'));
      end;
    scPlayer:
      begin
        UI.Title(_('Keybindings'), 5);

        X := 1;
        Y := 8;
        AddLine('Right/Left', _('Change tab'));
        AddLine('Up/Down', _('Scroll skills'));
        AddLine('Tab', _('Show Background'));
        AddLine('Space', _('Show Inventory'));
      end;
    scGame:
      begin
        Terminal.Print(CX, 3,
          _('Far away in an uncharted region of the Earth land Elvion lies surrounded by mountains.'), TK_ALIGN_CENTER);
        Terminal.Print(CX, 4, _('In the center of this land there is a village named Dork. It''s people are in'),
          TK_ALIGN_CENTER);
        Terminal.Print(CX, 5, _('grave danger as the Troll King and his armies are marching to lay waste on all of'),
          TK_ALIGN_CENTER);
        Terminal.Print(CX, 6, _('its inhabitants. Unless a hero will rise to take a stand against the forces of evil.'),
          TK_ALIGN_CENTER);

        Terminal.Print(CX, 8, _('You are the hero who departs on a quest to stop the enemies and save your homeland,'),
          TK_ALIGN_CENTER);
        Terminal.Print(CX, 9, _('Elvion. Survive, gather equipment, fight adversaries and be ready for the final'),
          TK_ALIGN_CENTER);
        Terminal.Print(CX, 10, _('confrontation. Good luck! You will need it.'), TK_ALIGN_CENTER);

        UI.Title(_('Keybindings'), 12);

        Terminal.Print(CX, 14, Format('%s: %s, %s, %s %s: %s, %s %s: %s', [_('Move'), UI.KeyToStr('arrow keys'),
          UI.KeyToStr('numpad'), UI.KeyToStr('QWEADZXC'), _('Wait'), UI.KeyToStr('5'), UI.KeyToStr('S'), _('Effects'),
          UI.KeyToStr('TAB')]), TK_ALIGN_CENTER);

        X := 1;
        Y := 16;
        AddLine('<', _('Go up stairs'));
        AddLine('>', _('Go down stairs'));
        AddLine('G', _('Pick up an item from the floor'));
        AddLine('F', _('Drop an item to the floor'));
        AddLine('L', _('Look mode'));
        AddLine('R', _('Rest'));
        AddLine('M', _('View messages'));
        // AddLine('B', _('Spellbook'));
        AddLine('T', _('Talents'));
        AddLine('N', _('Show Statistics'));
        AddLine('O', _('Options'));
        AddLine('I', _('Show Inventory'));
        AddLine('P', _('Character Screen'));
        AddLine('K', _('Calendar'));
        AddLine('?', _('Show this Help Screen'));

        UI.Title(_('Character dump'), Terminal.Window.Height - 6);
        Terminal.Print(CX, Terminal.Window.Height - 4, Format(_('The game saves a character dump to %s file.'),
          [UI.KeyToStr('*-character-dump.txt')]), TK_ALIGN_CENTER);
      end;
  end;
  Self.AddKey('Esc', _('Close'), True);
end;

procedure TSceneHelp.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      // Close
      Scenes.GoBack;
  end;
end;

{ TSceneGame }

procedure TSceneGame.Render;
var
  I, PX, PY, DX, DY: Int;
  R: UInt;
  T: TTile;
  Min, Max: TPoint;
  S: string;

  procedure RenderLook(X, Y: UInt; T: TTile; IsMob: Boolean);
  var
    S: string;
    C: Int;
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
      S := S + Items.GetItemInfo(FItem, (C > 1), C, True) + ' ';
    end;
    if IsMob then
    begin
      C := Mobs.GetIndex(X, Y);
      if (C > -1) then
      begin
        S := S + Format('%s (%s%d/%d). ', [Mobs.Name[TMobEnum(Mobs.Mob[C].ID)], UI.Icon(icLife),
          Mobs.Mob[C].Attributes.Attrib[atLife].Value, Mobs.Mob[C].Attributes.Attrib[atMaxLife].Value]);
      end;
    end;
    //
    Terminal.Print(Info.Left, Info.Top, Info.Width, Info.Height, S, TK_ALIGN_TOP);
  end;

  procedure AddTo(X, Y: Int);
  var
    I, L: Int;
    AX, AY: UInt;
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
  R := Player.Vision;
  if not Mode.Wizard then
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
        if Player.Look then
          Terminal.BackgroundColor($FF333333);
        X := DX - PX + Player.X;
        Y := DY - PY + Player.Y;
        if not Map.InMap(X, Y) then
          Continue;
        if not Mode.Wizard then
          if (Player.GetDist(X, Y) > R) and Map.GetFog(X, Y) then
            Continue;
        T := Map.GetTile(X, Y);
        if (Player.Look and (Player.LX = X) and (Player.LY = Y)) then
        begin
          Terminal.BackgroundColor(clRed);
          // Terminal.Print(DX + View.Left, DY + View.Top, ' ');
          RenderLook(X, Y, T, True);
        end;
        if (not Player.Look) and (Player.X = X) and (Player.Y = Y) then
          RenderLook(X, Y, T, False);
        if not Mode.Wizard then
        begin
          if (Player.GetDist(X, Y) <= R) then
          begin
            if not Map.GetFog(X, Y) then
              Terminal.ForegroundColor(clFog);
            if Map.GetFOV(X, Y) then
            begin
              if (Player.Light > 0) then
                Terminal.ForegroundColor(clLightestYellow)
              else
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
        if Mode.Wizard or not Map.GetFog(X, Y) then
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
  if Mode.Wizard then
    S := Format('%s (%d:%d)', [Map.Name, Player.X, Player.Y])
  else
    S := Map.Name;
  Terminal.Print(Status.Left + Status.Width - 1, Status.Top, S, TK_ALIGN_RIGHT);
  Terminal.ForegroundColor(clDefault);
  // Log
  MsgLog.Render;
end;

procedure TSceneGame.Update(var Key: UInt);
begin
  MsgLog.Turn;
  MsgLog.Msg := '';
  if Game.Won then
  begin
    Scenes.SetScene(scWin);
    Exit;
  end;
  case Key of
    TK_LEFT, TK_KP_4, TK_A:
      Player.Move(drWest);
    TK_RIGHT, TK_KP_6, TK_D:
      Player.Move(drEast);
    TK_UP, TK_KP_8, TK_W:
      Player.Move(drNorth);
    TK_DOWN, TK_KP_2, TK_X:
      Player.Move(drSouth);
    TK_KP_7, TK_Q:
      Player.Move(drNorthWest);
    TK_KP_9, TK_E:
      Player.Move(drNorthEast);
    TK_KP_1, TK_Z:
      Player.Move(drSouthWest);
    TK_KP_3, TK_C:
      Player.Move(drSouthEast);
    TK_KP_5, TK_S:
      Player.Wait;
    TK_L: // Look
      begin
        Player.LX := Player.X;
        Player.LY := Player.Y;
        Player.Look := not Player.Look;
      end;
    TK_KP_PLUS:
      if Mode.Wizard then
        if (Map.Current < High(TMapEnum)) then
        begin
          Map.Current := Succ(Map.Current);
          Player.Wait;
        end;
    TK_KP_MINUS:
      if Mode.Wizard then
        if (Map.Current > Low(TMapEnum)) then
        begin
          Map.Current := Pred(Map.Current);
          Player.Wait;
        end;
    TK_COMMA:
      begin
        if Player.IsDead then
          Exit;
        if (Map.GetTileEnum(Player.X, Player.Y, Map.Current) = teUpStairs) then
        begin
          if (Map.Current > Low(TMapEnum)) then
          begin
            MsgLog.Add(_('You climb up the ladder...'));
            Map.Current := Pred(Map.Current);
            Player.Wait;
          end;
        end
        else
          MsgLog.Add(_('You cannot climb up here.'));
      end;
    TK_PERIOD:
      begin
        if Player.IsDead then
          Exit;
        // Portal in town
        if (Map.GetTileEnum(Player.X, Player.Y, Map.Current) = tePortal) then
        begin
          Player.X := Game.Spawn.X;
          Player.Y := Game.Spawn.Y;
          Map.Current := deDark_Wood;
          Scenes.SetScene(scGame);
          Exit;
        end;
        // Portal
        if (Map.GetTileEnum(Player.X, Player.Y, Map.Current) = teTownPortal) then
        begin
          Map.SetTileEnum(Player.X, Player.Y, deDark_Wood, teStoneFloor);
          Player.X := Game.Portal.X;
          Player.Y := Game.Portal.Y;
          Map.Current := Game.PortalMap;
          Map.SetTileEnum(Player.X, Player.Y, Game.PortalMap, Game.PortalTile);
          Scenes.SetScene(scGame);
          Exit;
        end;
        // Down stairs
        if (Map.GetTileEnum(Player.X, Player.Y, Map.Current) = teDnStairs) then
        begin
          if (Map.Current < High(TMapEnum)) then
          begin
            MsgLog.Add(_('You climb down the ladder...'));
            Map.Current := Succ(Map.Current);
            Player.Wait;
          end;
        end
        else
          MsgLog.Add(_('You cannot climb down here.'));
      end;
    TK_KP_MULTIPLY:
      if Mode.Wizard then
      begin
        Player.Fill;
      end;
    TK_SPACE:
      if Player.IsDead then
      begin
        if (Game.Difficulty = dfEasy) or (Game.Difficulty = dfNormal) then
        begin
          Player.Spawn;
          Player.Fill;
          Exit;
        end;
        Scenes.SetScene(scDef);
        Exit;
      end;
    TK_ESCAPE:
      begin
        if Player.Look then
        begin
          Player.Look := False;
          Exit;
        end;
        if Player.IsDead then
          Exit;
        Game.Screenshot := Terminal.GetTextScreenshot();
        Scenes.SetScene(scQuit, Scenes.SceneEnum);
      end;
    TK_TAB:
      Game.ShowEffects := not Game.ShowEffects;
    TK_K:
      Scenes.SetScene(scCalendar);
    TK_R:
      begin
        if Player.IsDead then
          Exit;
        Scenes.SetScene(scRest);
      end;
    TK_G:
      begin
        if Player.IsDead then
          Exit;
        Player.Pickup;
      end;
    TK_I:
      Scenes.SetScene(scInv);
    TK_M:
      Scenes.SetScene(scMessages);
    TK_F:
      begin
        if Player.IsDead then
          Exit;
        Scenes.SetScene(scDrop, scGame);
      end;
    TK_P:
      Scenes.SetScene(scPlayer);
    TK_N:
      Scenes.SetScene(scStatistics);
    TK_O:
      Scenes.SetScene(scOptions);
    // TK_B:
    // Scenes.SetScene(scSpellbook);
    TK_Y:
      if Mode.Wizard then
      begin
        Quests.Add(qeKillNBears);

      end;

    // if Game.Wizard then Items.DelCorpses;
    // ShowMessage(IntToStr(Player.GetRealDamage(1000, 250)));
    // if Game.Wizard then Player.AddExp(LevelExpMax);
    TK_T:
      Scenes.SetScene(scTalents, scGame);
    TK_SLASH:
      Scenes.SetScene(scHelp, scGame);
  end;
end;

{ TSceneLoad }

procedure TSceneLoad.Render;
begin
  Terminal.Print(CX, CY, _('Creating the world, please wait...'), TK_ALIGN_CENTER);
end;

{ TSceneQuit }

procedure TSceneQuit.Render;
begin
  Logo.Render(False);
  Terminal.Print(CX, CY + 3, Format(_('Do you wish to quit? %s/%s'), [UI.KeyToStr('Y'), UI.KeyToStr('N')]),
    TK_ALIGN_CENTER);
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
    Terminal.Print(CX, CY + 3, Format(_('You dead. Press %s'), [UI.KeyToStr('ENTER')]), TK_ALIGN_CENTER)
  else
    Terminal.Print(CX, CY + 3, Format(_('You were slain by %s. Press %s'), [Terminal.Colorize(Player.Killer, clAlarm),
      UI.KeyToStr('ENTER')]), TK_ALIGN_CENTER);
  if Mode.Wizard then
    Terminal.Print(CX, CY + 5, Format(_('Press %s to continue...'), [UI.KeyToStr('SPACE')]), TK_ALIGN_CENTER);

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
  Terminal.Print(CX, CY + 1, UpperCase(_('Congratulations!!!')), TK_ALIGN_CENTER);
  Terminal.Print(CX, CY + 3, Format(_('You have won. Press %s'), [UI.KeyToStr('ENTER')]), TK_ALIGN_CENTER);
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

procedure TSceneInv.Render;
begin
  UI.Title(Format('%s [[%s%d %s%d/%d]]', [_('Inventory'), UI.Icon(icGold), Player.Gold, UI.Icon(icFlag),
    Items_Inventory_GetCount(), ItemMax]));

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

{ TScenePlayer }

constructor TScenePlayer.Create;
begin
  FRenderInfo := False;
  FSkillCursorTop := 0;
end;

procedure TScenePlayer.Render;
begin
  D := 2;
  Y := 0;
  X := Math.EnsureRange(Terminal.Window.Width div 4, 10, UIntMax);

  if Mode.Wizard then
    UI.Title(Format('%s, %s (%s), %s', [Player.Name, Races.GetName(Player.Race), Game.IfThen(Player.Sex = sxMale,
      _('Male'), _('Female')), 'Class']))
  else
    UI.Title(Player.Name);

  if FRenderInfo then
    RenderInfo()
  else
    Self.RenderPlayer();
  Self.RenderSkills();

  AddKey('Esc', _('Close'));
  AddKey('?', _('Help'), True);
end;

procedure TScenePlayer.Add(const AStr, AIcons, ABarColor: string; const ACur, AMax: Int);
begin
  W := X * 2 - 3;
  UI.Bar(1, 0, Y + (D * 2), W, ACur, AMax, color_from_name(LowerCase(ABarColor)), clDarkGray);
  Terminal.Print(X, Y + (D * 2), Format('%s %d/%d', [AIcons + ' ' + AStr, ACur, AMax]), TK_ALIGN_CENTER);
  Inc(D);
end;

procedure TScenePlayer.RenderPlayer;
begin
  Terminal.Print(X, Y + 2, Format(FT, [_('Attributes') + ' (1/2)']), TK_ALIGN_CENTER);
  // Level
  Add(Format('%s %d', [_('Level'), Player.Attributes.Attrib[atLev].Value]), UI.Icon(icElixir), 'Gold',
    Player.Attributes.Attrib[atExp].Value, LevelExpMax);
  // Attributes
  Add('Strength', UI.Icon(icStr), 'Strength', Player.Attributes.Attrib[atStr].Value, AttribMax);
  Add('Dexterity', UI.Icon(icDex), 'Dexterity', Player.Attributes.Attrib[atDex].Value, AttribMax);
  Add('Willpower', UI.Icon(icBook), 'Willpower', Player.Attributes.Attrib[atWil].Value, AttribMax);
  Add('Perception', UI.Icon(icLeaf), 'Perception', Player.Attributes.Attrib[atPer].Value, AttribMax);
  // Damage
  Add('Min Damage', UI.Icon(icSword), 'Darker Yellow', Player.Attributes.Attrib[atMinDamage].Value, MinDamMax);
  Add('Max Damage', UI.Icon(icSword), 'Darker Yellow', Player.Attributes.Attrib[atMaxDamage].Value, MaxDamMax);
  // DV and PV
  Add('Defensive Value (DV)', UI.Icon(icDex), 'Darkest Green', Player.Attributes.Attrib[atDV].Value, DVMax);
  Add('Protection Value (PV)', UI.Icon(icShield), 'Darkest Green', Player.Attributes.Attrib[atPV].Value, PVMax);
  // Life and Mana
  Add('Life', UI.Icon(icLife), 'Life', Player.Attributes.Attrib[atLife].Value,
    Player.Attributes.Attrib[atMaxLife].Value);
  Add('Mana', UI.Icon(icMana), 'Mana', Player.Attributes.Attrib[atMana].Value,
    Player.Attributes.Attrib[atMaxMana].Value);
  // Vision radius
  Add('Vision radius', UI.Icon(icVision), 'Vision', Player.Vision, VisionMax);
end;

procedure TScenePlayer.RenderInfo;
begin
  Terminal.Print(X, Y + 2, Format(FT, [_('Attributes') + ' (2/2)']), TK_ALIGN_CENTER);
  //
  Add('Replenish Life', UI.Icon(icElixir) + UI.Icon(icLife), 'Life', Player.Attributes.Attrib[atReLife].Value,
    ReLifeMax);
  Add('Regeneration Mana', UI.Icon(icElixir) + UI.Icon(icMana), 'Mana', Player.Attributes.Attrib[atReMana].Value,
    ReManaMax);
  //
  Add('To Life after each Kill', UI.Icon(icPlus) + UI.Icon(icLife), 'Life',
    Player.Attributes.Attrib[atLifeAfEachKill].Value, LifeAEKMax);
  Add('To Mana after each Kill', UI.Icon(icPlus) + UI.Icon(icLife), 'Mana',
    Player.Attributes.Attrib[atManaAfEachKill].Value, ManaAEKMax);
  //
  Add('Extra Gold from Monsters (%)', UI.Icon(icPlus) + UI.Icon(icGold), 'Gold',
    Player.Attributes.Attrib[atExtraGold].Value, ExtraGoldMax);
  //
  Add('Satiation', UI.Icon(icFood), 'Food', Player.Attributes.Attrib[atSat].Value, EngorgedMax);
end;

const
  ScrMax = 12;

procedure TScenePlayer.RenderSkills;
var
  I: TSkillEnum;
  A, B, J, D: UInt;
begin
  Y := 2;
  X := Terminal.Window.Width div 2;
  A := Terminal.Window.Width div 4;
  B := A * 3;
  Terminal.Print(B, Y, Format(FT, [Format(_('Skills (%d-%d)'), [FSkillCursorTop + 1, FSkillCursorTop + ScrMax])]),
    TK_ALIGN_CENTER);
  for J := 1 to ScrMax do
  begin
    I := TSkillEnum(FSkillCursorTop + J);
    D := ((J - 1) * 2) + Y + 2;
    UI.Bar(X, 0, D, X - 2, Player.Skills.Skill[I].Value, SkillMax, clDarkRed, clDarkGray);
    Terminal.Print(B, D, Format('%s %d/%d', [Player.Skills.GetName(I), Player.Skills.Skill[I].Value, SkillMax]),
      TK_ALIGN_CENTER);
  end;
end;

procedure TScenePlayer.Update(var Key: UInt);
begin
  case Key of
    // Close
    TK_ESCAPE:
      Scenes.SetScene(scGame);
    // Background
    TK_TAB:
      Scenes.SetScene(scBackground, scPlayer);
    // Information
    TK_LEFT, TK_A, TK_KP_4:
      FRenderInfo := False;
    TK_RIGHT, TK_D, TK_KP_6:
      FRenderInfo := True;
    // Inventory
    TK_SPACE:
      begin
        Game.Timer := UIntMax;
        Scenes.SetScene(scInv);
      end;
    TK_UP, TK_KP_8, TK_W:
      begin
        if (FSkillCursorTop > 0) then
          Dec(FSkillCursorTop);
      end;
    TK_DOWN, TK_KP_2, TK_X:
      begin
        if (FSkillCursorTop < Ord(High(TSkillEnum)) - ScrMax) then
          Inc(FSkillCursorTop);
      end;
    TK_SLASH:
      Scenes.SetScene(scHelp, scPlayer);
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
    FItem := Items_Dungeon_GetMapItemXY(Ord(Map.Current), Player.ItemIndex, Player.X, Player.Y);

  MaxAmount := FItem.Amount;

  Terminal.Print(CX, CY, Format('%d/%dx', [Player.ItemAmount, FItem.Amount]), TK_ALIGN_LEFT);

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
  FCount := Items_Dungeon_GetMapCountXY(MapID, Player.X, Player.Y).InRange(ItemMax);
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
        FCount := Items_Dungeon_GetMapCountXY(Ord(Map.Current), Player.X, Player.Y).InRange(ItemMax);
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

{ TSceneStatistics }

procedure TSceneStatistics.Render;
begin
  UI.Title(_('Statistics'));
  X := 1;
  Y := 3;

  Add(_('Name'), Player.Name);
  Add(_('Level'), Player.Attributes.Attrib[atLev].Value);
  Add(_('Race'), Format('%s (%s)', [Races.GetName(Player.Race), Game.IfThen(Player.Sex = sxMale, _('Male'),
    _('Female'))]));
  Add(_('Class'), 'Warrior');
  Add(_('Game Difficulty'), Game.GetStrDifficulty);
  Add(_('Scores'), Player.Statictics.Get(stScore));
  Add(_('Age'), Player.Statictics.Get(stAge));
  Add(_('Weight'), Player.Statictics.Get(stWeight));
  Add(_('Height'), Player.Statictics.Get(stHeight));
  // Add(_('Talent'), Player.GetTalentName(Player.GetTalent(0)));
  Add(_('Tiles Moved'), Player.Statictics.Get(stTurn));
  Add(_('Monsters Killed'), Player.Statictics.Get(stKills));
  Add(_('Items Found'), Player.Statictics.Get(stFound));
  // Add(_('Chests Found'), );
  // Add(_('Doors Opened'), );
  Add(_('Potions Drunk'), Player.Statictics.Get(stPotDrunk));
  Add(_('Scrolls Read'), Player.Statictics.Get(stScrRead));
  Add(_('Spells Cast'), Player.Statictics.Get(stSpCast));
  Add(_('Foods Eaten'), Player.Statictics.Get(stFdEat));
  // Add(_('Melee Attack Performed'), );
  // Add(_('Ranged Attack Performed'), );
  // Add(_('Unarmed Attack Performed'), );
  // Add(_('Times Fallen Into Pit'), );
  // Add(_('Items Sold'), );
  Add(_('Items Used'), Player.Statictics.Get(stItUsed));
  Add(_('Items Repaired'), Player.Statictics.Get(stItRep));
  Add(_('Items Identified'), Player.Statictics.Get(stItIdent));
  Add(_('Items Crafted'), Player.Statictics.Get(stItCrafted));
  // Add(_('Gold from Sales'), );
  // Add(_(''), );

  // Version
  X := 1;
  Y := Y + 2;
  UI.Title(_('Version'), Y - 1);
  Y := Y + 1;
  Add(_('Game Version'), Game.GetVersion);
  Add(_('BeaRLibTerminal'), BearLibTerminal.terminal_get('version'));
  Self.Add();
  Add(_('BeaRLibItems'), Items_GetVersion);

  if Mode.Wizard then
  begin
    X := 1;
    Y := Y + 2;
    UI.Title(_('Wizard Mode'), Y - 1);
    Y := Y + 1;
    Add(_('Monsters'), Ord(Length(MobBase)) - (13 + 7));
    Add(_('Bosses'), 13);
    Add(_('NPCs'), 7);
    Add(_('Items'), Ord(Length(ItemBase)));
    Add(_('Shops'), Shops.Count);
    Add(_('Quests'), Quests.Amount);
    Add(_('Talents'), Player.Talents.Amount);
    Add(_('Affixes'), Affixes.Amount);
    Add(_('Item Types'), Ord(High(TItemType)));
    Add(_('Skills'), Ord(High(TSkillEnum)));
  end;

  AddKey('Esc', _('Close'), True);
end;

procedure TSceneStatistics.Update(var Key: UInt);
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
    V := Player.Attributes.Attrib[atMaxLife].Value - Player.Attributes.Attrib[atLife].Value;
    if (V > 0) then
      S := ' (' + Items.GetInfo('+', V, 'Life') + ' ' + Items.GetPrice(Round(V * 1.6)) + ')'
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
  UI.Title(Format(_('Buying at %s'), [NPCName]) + ' ' + UI.GoldLeft(Player.Gold));

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
  UI.Title(_('Repairing items') + ' ' + UI.GoldLeft(Player.Gold), 1, clDarkestRed);

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

{ TSceneDifficulty }

procedure TSceneDifficulty.Render;
begin
  UI.Title(_('Choose a difficulty'));

  Terminal.Print(CX - 5, CY - 3, Format('%s %s', [UI.KeyToStr('A'), _('Easy')]), TK_ALIGN_LEFT);
  Terminal.Print(CX - 5, CY - 1, Format('%s %s', [UI.KeyToStr('B'), _('Normal')]), TK_ALIGN_LEFT);
  Terminal.Print(CX - 5, CY + 1, Format('%s %s', [UI.KeyToStr('C'), _('Hard')]), TK_ALIGN_LEFT);
  Terminal.Print(CX - 5, CY + 3, Format('%s %s', [UI.KeyToStr('D'), _('Hell')]), TK_ALIGN_LEFT);

  AddKey('Esc', _('Back'), True);
end;

procedure TSceneDifficulty.Update(var Key: UInt);
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
            if Mode.Wizard then
              Game.Difficulty := dfNormal
            else
              Exit;
        end;
        Game.Start();
        Scenes.SetScene(scRace, scDifficulty);
        (Scenes.GetScene(scRace) as TSceneRace).ReRoll;
      end;
    TK_ESCAPE:
      Scenes.SetScene(scTitle);
  end;
end;

{ TSceneRest }

procedure TSceneRest.Render;
begin
  UI.Title(_('Rest'));

  UI.FromAToZ;
  Y := 1;

  Inc(Y);
  Terminal.Print(1, Y, UI.KeyToStr(Chr(Y + 95)) + ' ' + _('Rest for 10 turns'), TK_ALIGN_LEFT);
  Inc(Y);
  Terminal.Print(1, Y, UI.KeyToStr(Chr(Y + 95)) + ' ' + _('Rest for 100 turns'), TK_ALIGN_LEFT);
  Inc(Y);
  Terminal.Print(1, Y, UI.KeyToStr(Chr(Y + 95)) + ' ' + _('Rest for 1000 turns'), TK_ALIGN_LEFT);

  MsgLog.Render(2, True);

  AddKey('Esc', _('Back'), True);
end;

procedure TSceneRest.Update(var Key: UInt);
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
  UI.Title(_('Choose a name'));

  Terminal.Print(CX - 10, CY, _('Enter your player''s name') + ': ' + Player.Name + Game.GetCursor, TK_ALIGN_LEFT);

  AddKey('Enter', _('Confirm'));
  AddKey('Esc', _('Back'), True);
end;

procedure TSceneName.Update(var Key: UInt);
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
        Scenes.SetScene(scBackground, scName);
      end;
    TK_A .. TK_Z:
      begin
        if (Length(Player.Name) < 10) then
          Player.Name := Player.Name + Chr(Key - TK_A + 65);
      end;
    TK_ESCAPE:
      begin
        Player.Talents.Clear;
        Scenes.SetScene(scTalents, scClass);
      end;
  end;
end;

{ TSceneOptions }

procedure TSceneOptions.Render;
begin
  // Options
  UI.Title(_('Options'));
  X := 1;
  Y := 3;
  AddOption('C', _('Auto pick up coins'), Game.GetOption(apCoin));
  AddOption('G', _('Auto pick up gems'), Game.GetOption(apGem));
  AddOption('F', _('Auto pick up food'), Game.GetOption(apFood));
  AddOption('Y', _('Auto pick up plants'), Game.GetOption(apPlant));
  AddOption('P', _('Auto pick up potions and flasks'), Game.GetOption(apPotion));
  AddOption('U', _('Auto pick up flasks'), Game.GetOption(apFlask));
  AddOption('O', _('Auto pick up magic items'), Game.GetOption(apMagic));
  AddOption('S', _('Auto pick up scrolls'), Game.GetOption(apScroll));
  AddOption('R', _('Auto pick up runes'), Game.GetOption(apRune));
  AddOption('B', _('Auto pick up books'), Game.GetOption(apBook));
  AddOption('K', _('Auto pick up keys'), Game.GetOption(apKey));
  AddOption('D', _('Show items price in inventory'), Game.GetOption(apShPrice));

  // Settings
  X := 1;
  Y := Y + 3;
  UI.Title(_('Settings'), Y - 1);
  Y := Y + 1;
  AddOption('W', _('Fullscreen'), Game.GetOption(apFullscreen), clLightBlue);

  // Wizard mode
  if Mode.Wizard then
  begin
    X := 1;
    Y := Y + 3;
    UI.Title(_('Wizard Mode'), Y - 1);
    Y := Y + 1;
    AddOption('Z', _('Turn Wizard Mode Off'), Mode.Wizard, clRed);
    AddOption('M', _('Show map'), Game.ShowMap);
    AddOption('T', _('Reload all shops'), False);
    // AddOption('J', _(''), False);
    AddOption('L', _('Leave corpses'), Game.LCorpses);
    AddOption('I', _('Show ID of items'), Game.ShowID);
    AddOption('N', _('Hide level of an item'), Game.GetOption(apHdLevOfItem));
  end;

  AddKey('Esc', _('Back'), True);
end;

procedure TSceneOptions.Update(var Key: UInt);
begin
  case Key of
    // Options
    TK_C:
      Game.ChOption(apCoin);
    TK_G:
      Game.ChOption(apGem);
    TK_F:
      Game.ChOption(apFood);
    TK_Y:
      Game.ChOption(apPlant);
    TK_P:
      Game.ChOption(apPotion);
    TK_O:
      Game.ChOption(apMagic);
    TK_U:
      Game.ChOption(apFlask);
    TK_S:
      Game.ChOption(apScroll);
    TK_R:
      Game.ChOption(apRune);
    TK_K:
      Game.ChOption(apKey);
    TK_B:
      Game.ChOption(apBook);
    TK_D:
      Game.ChOption(apShPrice);
    // Settings
    TK_W:
      begin
        Game.ChOption(apFullscreen);
        Game.ChScreen;
      end;
    // Wizard mode
    TK_Z:
      Mode.Wizard := False;
    TK_M:
      if Mode.Wizard then
        Game.ShowMap := not Game.ShowMap;
    TK_L:
      if Mode.Wizard then
        Game.LCorpses := not Game.LCorpses;
    TK_T:
      if Mode.Wizard then
      begin
        Shops.New;
        Scenes.SetScene(scGame);
      end;
    TK_I:
      if Mode.Wizard then
        Game.ShowID := not Game.ShowID;
    TK_N:
      if Mode.Wizard then
        Game.ChOption(apHdLevOfItem);
    // TK_J:
    // if Mode.Wizard then
    // ;
    TK_ESCAPE:
      Scenes.SetScene(scGame);
  end
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
      Terminal.Print(5, Y, Format('(%s) %s %s', [Items.GetLevel(Spellbook.GetSpell(I).Spell.Level),
        Spellbook.GetSpellName(I), Items.GetInfo('-', Spellbook.GetSpell(I).Spell.ManaCost, 'Mana')]));
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
      Add(Player.Talents.GetName(T), Player.Talents.GetHint(T), Player.Talents.IsPoint);

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

{ TSceneIdentification }

procedure TSceneIdentification.Render;
begin
  UI.Title(_('Identification'), 1, clDarkestRed);

  UI.FromAToZ();
  Items.RenderInventory();
  MsgLog.Render(2, True);

  AddKey('A-Z', _('Select an item'));
  AddKey('Esc', _('Close'), True);
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
  end
end;

{ TSceneCraft }

procedure TSceneCraft.Render;
begin
  UI.Title(_('Enchant an item'), 1, clDarkestRed);

  UI.FromAToZ();
  Items.RenderInventory();
  MsgLog.Render(2, True);

  AddKey('A-Z', _('Select an item'));
  AddKey('Esc', _('Close'), True);
end;

procedure TSceneCraft.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      Scenes.SetScene(scInv);
    TK_A .. TK_Z:
      Player.CraftItem(Key - TK_A);
  else
    Game.Timer := UIntMax;
  end
end;

{ TSceneQuest }

procedure TSceneQuest.Render;
begin
  UI.Title(Quests.GetName(Quests.Current), 1);

  AddKey('Enter', _('Accept'));
  AddKey('Esc', _('Decline'), True);
end;

procedure TSceneQuest.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      Scenes.GoBack();
    TK_ENTER, TK_KP_ENTER:
      begin
        Quests.Add(Quests.Current);
        Scenes.GoBack();
      end
  else
    Game.Timer := UIntMax;
  end
end;

{ TSceneBackground }

procedure TSceneBackground.Render;
begin
  UI.Title(_('Character Background'));

  Terminal.ForegroundColor(clGray);
  Terminal.Print(CX - (CX div 2), CY - (CY div 2), CX, CY, Player.Background, TK_ALIGN_BOTTOM);

  if not Mode.Game then
  begin
    AddKey('Enter', _('Start game'));
    AddKey('Space', _('Re-roll'));
  end;
  AddKey('Esc', _('Close'), _('Back'), True);
end;

procedure TSceneBackground.Update(var Key: UInt);
begin
  case Key of
    TK_ENTER, TK_KP_ENTER:
      if not Mode.Game then
      begin
        Scenes.SetScene(scLoad);
        Terminal.Refresh;
        Terminal_Delay(1000);
        Map.Gen;
        Mode.Game := True;
        Player.Talents.DoTalent(TSceneTalents(Scenes.GetScene(scTalents)).Talent);
        Scenes.SetScene(scGame);
      end;
    TK_SPACE:
      if not Mode.Game then
        Player.GenerateBackground();
    TK_ESCAPE:
      Scenes.GoBack();
  end;
end;

{ TSceneRace }

procedure TSceneRace.Render;
var
  I: UInt;
  R: TRaceEnum;

  procedure Add(const AName: string);
  var
    C: Char;
  begin
    C := Chr(I + Ord('A'));
    Terminal.ForegroundColor(clWhite);
    Terminal.Print(1, Y, UI.KeyToStr(C));
    if (R = Player.Race) then
      Terminal.ForegroundColor(clYellow)
    else
      Terminal.ForegroundColor(clWhite);
    Terminal.Print(5, Y, AName);
    Inc(I);
    Inc(Y);
  end;

begin
  UI.Title(_('Choose a race'));
  I := 0;
  Y := 2;
  for R := Low(TRaceEnum) to High(TRaceEnum) do
    Add(Races.GetName(R));

  Terminal.ForegroundColor(clWhite);
  Terminal.Print(CX, 3, _('Age') + ': ' + Terminal.Colorize(Player.Statictics.Get(stAge), 'Lush'));
  Terminal.Print(CX, 4, _('Height') + ': ' + Terminal.Colorize(Player.Statictics.Get(stHeight), 'Lush'));
  Terminal.Print(CX, 5, _('Weight') + ': ' + Terminal.Colorize(Player.Statictics.Get(stWeight), 'Lush'));
  Terminal.Print(CX, 6, _('Sex') + ': ' + Terminal.Colorize(Game.IfThen(Player.Sex = sxMale, _('Male'), _('Female')
    ), 'Lush'));

  Terminal.Print(CX, 8, _('Strength') + ': ' + Terminal.Colorize(Player.Attributes.Attrib[atStr].Prm, 'Strength'));
  Terminal.Print(CX, 9, _('Dexterity') + ': ' + Terminal.Colorize(Player.Attributes.Attrib[atDex].Prm, 'Dexterity'));
  Terminal.Print(CX, 10, _('Willpower') + ': ' + Terminal.Colorize(Player.Attributes.Attrib[atWil].Prm, 'Willpower'));
  Terminal.Print(CX, 11, _('Perception') + ': ' + Terminal.Colorize(Player.Attributes.Attrib[atPer].Prm, 'Perception'));

  AddKey('A-Z', _('Select a race'));
  AddKey('Tab', _('Choose a sex'));
  AddKey('Space', _('Re-roll'));
  AddKey('Enter', _('Confirm'));
  AddKey('Esc', _('Back'));
  AddKey('?', _('Help'), True);
end;

procedure TSceneRace.ReRoll;
begin
  case Player.Race of
    rcHuman:
      begin
        Player.Statictics.SetValue(stAge, Math.RandomRange(20, 50));
      end;
    rcElf:
      begin
        Player.Statictics.SetValue(stAge, Math.RandomRange(20, 500));
        Player.Statictics.SetValue(stHeight, Math.RandomRange(190, 230));
      end;
    rcDwarf:
      begin
        Player.Statictics.SetValue(stAge, Math.RandomRange(20, 200));
        Player.Statictics.SetValue(stHeight, Math.RandomRange(110, 140));
      end;
  end;
  Player.Statictics.SetValue(stHeight, Math.RandomRange(RaceHeight[Player.Race].Min, RaceHeight[Player.Race].Max));
end;

procedure TSceneRace.Update(var Key: UInt);
begin
  case Key of
    TK_TAB:
      begin
        if (Player.Sex = sxMale) then
          Player.Sex := sxFemale
        else
          Player.Sex := sxMale;
        ReRoll;
      end;
    TK_A .. TK_Z:
      begin
        Player.Race := TRaceEnum(Math.EnsureRange(Ord(Key) - Ord(TK_A), 0, Ord(High(TRaceEnum))));
        ReRoll;
      end;
    TK_ENTER, TK_KP_ENTER:
      begin
        Scenes.SetScene(scClass, scRace);
      end;
    TK_ESCAPE:
      begin
        Scenes.SetScene(scDifficulty);
      end;
    TK_SLASH:
      Scenes.SetScene(scHelp, scRace);
    TK_SPACE:
      ReRoll;
  end;
end;

{ TSceneClass }

procedure TSceneClass.Render;
begin
  UI.Title(_('Choose a class'));

  AddKey('Enter', _('Confirm'));
  AddKey('Esc', _('Back'), True);
end;

procedure TSceneClass.Update(var Key: UInt);
begin
  case Key of
    TK_ENTER, TK_KP_ENTER:
      begin
        Scenes.SetScene(scTalents, scClass);
      end;
    TK_ESCAPE:
      begin
        Scenes.SetScene(scRace);
      end;
  end;
end;

initialization

Scenes := TScenes.Create();

finalization

FreeAndNil(Scenes);

end.
