unit Trollhunter.Scene.Game;

interface

uses
  Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneGame = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

{ TSceneGame }

uses
  Math,
  Types,
  SysUtils,
  BearLibTerminal,
  uBearLibItemsCommon,
  uBearLibItemsDungeon,
  Trollhunter.Map,
  Trollhunter.Terminal,
  Trollhunter.Corpse,
  Trollhunter.Game,
  Trollhunter.Item,
  Trollhunter.Mob,
  Trollhunter.UI,
  Trollhunter.UI.Log,
  Trollhunter.Attribute,
  Trollhunter.Language,
  Trollhunter.Player,
  Trollhunter.Player.Quest,
  Trollhunter.Player.Types,
  Trollhunter.Scene.Final,
  Trollhunter.Mob.Types;

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
        S := S + Format('%s (%s%d/%d). ', [Mobs.Name[TMobEnum(Mobs.Mob[C].ID)],
        UI.Icon(icLife), Mobs.Mob[C].Attributes.Attrib[atLife].Value,
          Mobs.Mob[C].Attributes.Attrib[atMaxLife].Value]);
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
    Scenes.SetScene(scFinal, feWon);
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
    TK_KP_5:
      Player.Wait;
    TK_L: // Look
      begin
        Player.LX := Player.X;
        Player.LY := Player.Y;
        Player.Look := not Player.Look;
        Player.Shoot := False;
      end;
    TK_S: // Shoot/Spell
      if Player.CanShootOrCastSpell then
      begin
        Player.LX := Player.X;
        Player.LY := Player.Y;
        Player.Look := not Player.Look;
        Player.Shoot := Player.Look;
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
        // Up stairs
        if Player.IsDead then
          Exit;
        if (Map.GetTileEnum(Player.X, Player.Y, Map.Current) = teUpStairs) then
        begin
          if (Map.Current > Low(TMapEnum)) then
          begin
            Player.Wait;
            MsgLog.Add(_('You climb up the ladder...'));
            Map.Current := Pred(Map.Current);
          end;
        end
        else
          MsgLog.Add(_('You cannot climb up here.'));
      end;
    TK_ENTER: // Shoot
      begin
        if Player.Shoot and Player.CanShootOrCastSpell and not Mobs.GetFreeTile(Player.LX, Player.LY) then
        begin
          Player.DistAttack(Mobs.GetIndex(Player.LX, Player.LY));
        end;
      end;
    TK_PERIOD:
      begin
        // Portal in town
        if Player.IsDead then
          Exit;
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
            Player.Wait;
            MsgLog.Add(_('You climb down the ladder...'));
            Map.Current := Succ(Map.Current);
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
        Scenes.SetScene(scFinal, feDefeat);
        Exit;
      end
      else
        Player.Wait;
    TK_ESCAPE:
      begin
        if Player.Look then
        begin
          Player.Look := False;
          Player.Shoot := False;
          Exit;
        end;
        if Player.IsDead then
          Exit;
        Game.Screenshot := Terminal.GetTextScreenshot();
        Scenes.SetScene(scFinal, Scenes.SceneEnum, feQuit);
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
      ;
    TK_SLASH:
      Scenes.SetScene(scHelp, scGame);
  end;
end;

end.
