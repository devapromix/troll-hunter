unit Trollhunter.Scene.Game;

interface

uses
  Classes,
  Types,
  Trollhunter.UI,
  Trollhunter.UI.Log,
  Trollhunter.Spell,
  Trollhunter.Types,
  Trollhunter.Projectile,
  Trollhunter.Item.Common,
  Trollhunter.Mob,
  Trollhunter.Game,
  Trollhunter.Scenes,
  Trollhunter.Attribute;

type
  TLightningGeometry = record
    Path: array of TPoint;
    BranchAttach: array of Int;
    BranchPoints: array of TPoint;
  end;

  { TSceneGame }

  TSceneGame = class(TScene)
  private
    procedure FireArrow;
    procedure CastTargetedSpell;
    procedure CastSpell();
    procedure RenderProjectileFly(const ATX, ATY: Int; const ASymbol: TSymbol);
    function BuildLightning(const ATX, ATY: Int): TLightningGeometry;
    procedure RenderLightning(const AGeometry: TLightningGeometry;
      const ASymbol: TSymbol);
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

uses
  Math,
  SysUtils,
  BearLibTerminal,
  Trollhunter.Quest,
  Trollhunter.Terminal,
  Trollhunter.Player,
  Trollhunter.Player.Helpers,
  Trollhunter.Player.Types,
  Trollhunter.Projectile.Types,
  Trollhunter.Item.Dungeon,
  Trollhunter.Item.Inventory,
  Trollhunter.Statistic,
  Trollhunter.Item,
  Trollhunter.Item.Types,
  Trollhunter.Map,
  Trollhunter.Ability,
  Trollhunter.Spellbook;

  { TSceneGame }

procedure TSceneGame.Render;
var
  I, PX, PY, DX, DY: Int;
  R: UInt;
  T: TTile;
  Min, Max: TPoint;
  S, ActionLabel: string;

  procedure RenderLook(X, Y: UInt; T: TTile; IsMob: boolean);
  var
    S: string;
    C: Int;
    FItem: Item;
  begin
    S := '';
    Terminal.BackgroundColor(0);
    Terminal.ForegroundColor(clDefault);
    S := S + T.Name + '. ';
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

    Terminal.Print(Info.Left, Info.Top, Info.Width, Info.Height, S,
      TK_ALIGN_TOP);
  end;

  procedure AddTo(X, Y: Int);
  var
    I, L: Int;
    AX, AY: UInt;
    LR: real;
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
  Player.Render(PX, PY);
  Mobs.Render(PX, PY);
  // Fire mode
  if Player.FireMode then
  begin
    I := Player.FireModeTarget;
    if (I > -1) and Mobs.Mob[I].Alive then
    begin
      DX := Mobs.Mob[I].X - Player.X + PX;
      DY := Mobs.Mob[I].Y - Player.Y + PY;
      Terminal.BackgroundColor(clRed);
      Terminal.ForegroundColor(Mobs.Mob[I].Color);
      Terminal.Print(DX + View.Left, DY + View.Top,
        MobBase[TMobEnum(Mobs.Mob[I].ID)].Symbol);
      Terminal.BackgroundColor(clBackground);
      Terminal.ForegroundColor(clDefault);
      if Player.MagicMode then
        ActionLabel := 'cast'
      else
        ActionLabel := 'shoot';
      Terminal.Print(Info.Left, Info.Top, Info.Width, Info.Height,
        Format('%s (%s%d/%d). [<-/->] target, [Enter] %s, [V/Esc] cancel',
        [Mobs.Name[TMobEnum(Mobs.Mob[I].ID)], UI.Icon(icLife),
        Mobs.Mob[I].Attributes.Attrib[atLife].Value,
        Mobs.Mob[I].Attributes.Attrib[atMaxLife].Value, ActionLabel]),
        TK_ALIGN_TOP);
    end;
  end;
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

procedure TSceneGame.RenderProjectileFly(const ATX, ATY: Int; const ASymbol: TSymbol);
var
  PX, PY, L, I, AX, AY: Int;
  LR: real;
begin
  PX := View.Width div 2;
  PY := View.Height div 2;
  L := Math.Max(Abs(Int(Player.X) - ATX), Abs(Int(Player.Y) - ATY));
  if (L = 0) then
    L := 1;
  for I := 1 to L do
  begin
    LR := I / L;
    AX := Int(Player.X) + Round((ATX - Int(Player.X)) * LR);
    AY := Int(Player.Y) + Round((ATY - Int(Player.Y)) * LR);
    Self.Render;
    Terminal.ForegroundColor(ASymbol.Color);
    Terminal.Print(AX - Int(Player.X) + PX + View.Left,
      AY - Int(Player.Y) + PY + View.Top, ASymbol.Symbol);
    terminal_refresh();
    terminal_delay(15);
  end;
end;

function TSceneGame.BuildLightning(const ATX, ATY: Int): TLightningGeometry;
const
  CBranchLen = 2;
var
  L, I, J, LBranchCount, LDX, LDY, LPX, LPY, LSign: Int;
  LR: real;
begin
  L := Math.Max(Abs(Int(Player.X) - ATX), Abs(Int(Player.Y) - ATY));
  if (L = 0) then
    L := 1;
  SetLength(Result.Path, L + 1);
  Result.Path[0] := Point(Int(Player.X), Int(Player.Y));
  for I := 1 to L do
  begin
    LR := I / L;
    Result.Path[I].X := Int(Player.X) + Round((ATX - Int(Player.X)) * LR);
    Result.Path[I].Y := Int(Player.Y) + Round((ATY - Int(Player.Y)) * LR);
    if (I < L) then
      if (RandomRange(0, 2) = 0) then
        Inc(Result.Path[I].X, RandomRange(-1, 2))
      else
        Inc(Result.Path[I].Y, RandomRange(-1, 2));
  end;

  LBranchCount := RandomRange(2, 4);
  SetLength(Result.BranchAttach, LBranchCount);
  SetLength(Result.BranchPoints, LBranchCount);
  for J := 0 to LBranchCount - 1 do
  begin
    I := RandomRange(1, L);
    LDX := Result.Path[I].X - Result.Path[I - 1].X;
    LDY := Result.Path[I].Y - Result.Path[I - 1].Y;
    if (RandomRange(0, 2) = 0) then
      LSign := 1
    else
      LSign := -1;
    LPX := Result.Path[I].X + LSign * (-LDY) * RandomRange(1, CBranchLen + 1);
    LPY := Result.Path[I].Y + LSign * LDX * RandomRange(1, CBranchLen + 1);
    Result.BranchAttach[J] := I;
    Result.BranchPoints[J] := Point(LPX, LPY);
  end;

end;

procedure TSceneGame.RenderLightning(const AGeometry: TLightningGeometry;
  const ASymbol: TSymbol);
var
  L, I, J, PX, PY: Int;
begin
  PX := View.Width div 2;
  PY := View.Height div 2;
  L := High(AGeometry.Path);
  for I := 1 to L do
  begin
    Self.Render;
    Terminal.ForegroundColor(ASymbol.Color);
    for J := 1 to I do
      Terminal.Print(AGeometry.Path[J].X - Int(Player.X) + PX + View.Left,
        AGeometry.Path[J].Y - Int(Player.Y) + PY + View.Top,
        Projectile.GetDirSymbol(Math.Sign(AGeometry.Path[J].X - AGeometry.Path[J - 1].X),
        Math.Sign(AGeometry.Path[J].Y - AGeometry.Path[J - 1].Y)));
    for J := 0 to High(AGeometry.BranchPoints) do
      if (AGeometry.BranchAttach[J] <= I) then
        Terminal.Print(AGeometry.BranchPoints[J].X - Int(Player.X) + PX + View.Left,
          AGeometry.BranchPoints[J].Y - Int(Player.Y) + PY + View.Top,
          Projectile.GetDirSymbol(
          Math.Sign(AGeometry.BranchPoints[J].X - AGeometry.Path[AGeometry.BranchAttach[J]].X),
          Math.Sign(AGeometry.BranchPoints[J].Y - AGeometry.Path[AGeometry.BranchAttach[J]].Y)));
    terminal_refresh();
    terminal_delay(15);
  end;

end;

procedure TSceneGame.FireArrow;
var
  Index, TX, TY, SX, SY: Int;
  LSymbol: TSymbol;
begin
  Index := Player.FireModeTarget;
  if (Index < 0) or not Mobs.Mob[Index].Alive then
  begin
    Player.FireModeExit;
    Exit;
  end;
  TX := Mobs.Mob[Index].X;
  TY := Mobs.Mob[Index].Y;
  SX := Math.Sign(TX - Int(Player.X));
  SY := Math.Sign(TY - Int(Player.Y));
  LSymbol := Projectile.GetSymbol(SX, SY, Player.WeaponSkill);
  RenderProjectileFly(TX, TY, LSymbol);
  Player.RangedAttack(Index);
  Player.FireModeEnter;
end;

procedure TSceneGame.CastTargetedSpell;
var
  Index, TX, TY, I: Int;
  LSymbol: TSymbol;
  LSpellEnum: TSpellEnum;
  LGeometry: TLightningGeometry;
  LSplashTiles: array of TPoint;
  LResult: TSpellAttackResult;
begin
  Index := Player.FireModeTarget;
  if (Index < 0) or (Index >= Mobs.Count) or not Mobs.Mob[Index].Alive then
  begin
    Player.FireModeExit;
    Exit;
  end;
  LSpellEnum := Spellbook.GetLastSelectedSpell;
  TX := Mobs.Mob[Index].X;
  TY := Mobs.Mob[Index].Y;
  if GetSpellData(LSpellEnum).Projectile = prLightning then
  begin
    LGeometry := BuildLightning(TX, TY);
    SetLength(LSplashTiles, High(LGeometry.Path) + Length(LGeometry.BranchPoints));
    for I := 1 to High(LGeometry.Path) do
      LSplashTiles[I - 1] := LGeometry.Path[I];
    for I := 0 to High(LGeometry.BranchPoints) do
      LSplashTiles[High(LGeometry.Path) + I] := LGeometry.BranchPoints[I];
  end;
  LResult := Player.MagicAttack(Index, LSpellEnum, LSplashTiles);
  if LResult = sarRejected then
    Exit;
  if GetSpellData(LSpellEnum).Projectile <> prNone then
  begin
    LSymbol := Projectile.GetSpellSymbol(LSpellEnum);
    if (GetSpellData(LSpellEnum).Projectile = prLightning) and (LResult = sarHit) then
      RenderLightning(LGeometry, LSymbol)
    else
      RenderProjectileFly(TX, TY, LSymbol);
  end;
  Player.AddTurn;
  if Player.FireMode then
    Player.MagicFireModeEnter;
end;

procedure TSceneGame.CastSpell();
begin
  if Player.IsDead then
    Exit;
  Spellbook.DoQuickSpell;
end;

procedure TSceneGame.Update(var Key: UInt);
begin
  MsgLog.Turn;
  MsgLog.Msg := '';
  if Player.FireMode then
  begin
    case Key of
      TK_LEFT, TK_KP_4:
        Player.FireModeSwitch(-1);
      TK_RIGHT, TK_KP_6:
        Player.FireModeSwitch(1);
      TK_RETURN, TK_KP_ENTER:
        if Player.MagicMode then
          CastTargetedSpell
        else
          FireArrow;
      TK_F, TK_ESCAPE:
        Player.FireModeExit;
    end;
    Exit;
  end;
  case Key of
    TK_LEFT, TK_KP_4:
      Player.Move(drWest);
    TK_RIGHT, TK_KP_6:
      Player.Move(drEast);
    TK_UP, TK_KP_8:
      Player.Move(drNorth);
    TK_DOWN, TK_KP_2:
      Player.Move(drSouth);
    TK_KP_7:
      Player.Move(drNorthWest);
    TK_KP_9:
      Player.Move(drNorthEast);
    TK_KP_1:
      Player.Move(drSouthWest);
    TK_KP_3:
      Player.Move(drSouthEast);
    TK_KP_5:
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
      if (Map.GetTileEnum(Player.X, Player.Y, Map.Current) in UpStairsTiles) then
      begin
        if (Map.Current > Low(TMapEnum)) then
        begin
          MsgLog.Add('You climb up the ladder...');
          Map.Current := Pred(Map.Current);
          Player.Wait;
        end;
      end
      else
        MsgLog.Add('You cannot climb up here.');
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
      if (Map.GetTileEnum(Player.X, Player.Y, Map.Current) in DnStairsTiles) then
      begin
        if (Map.Current < High(TMapEnum)) then
        begin
          MsgLog.Add('You climb down the ladder...');
          Map.Current := Succ(Map.Current);
          Player.Wait;
        end;
      end
      else
        MsgLog.Add('You cannot climb down here.');
    end;
    TK_KP_MULTIPLY:
      if Mode.Wizard then
      begin
        Player.Fill;
      end;
    TK_SPACE:
      if Player.IsDead then
      begin
        if (Game.Difficulty < dfNormal) then
        begin
          Player.Spawn;
          Player.Fill;
          Exit;
        end;
        Scenes.SetScene(scDeath);
        Exit;
      end
      else if Game.Won then
      begin
        Scenes.SetScene(scWin);
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
    TK_D:
    begin
      if Player.IsDead then
        Exit;
      if Player.IsOnStash then
        Scenes.SetScene(scStore, scGame)
      else
        Scenes.SetScene(scDrop, scGame);
    end;
    TK_F:
    begin
      if Player.IsDead then
        Exit;
      if Player.CanFire then
        Player.FireModeEnter
      else
        MsgLog.Add('You need to equip a bow or wand to use ranged fire mode.');
    end;
    TK_P:
      Scenes.SetScene(scPlayer);
    TK_N:
      Scenes.SetScene(scStatistics);
    TK_O:
      Scenes.SetScene(scOptions);
    TK_B:
    begin
      if Player.HasSpellbook then
        Scenes.SetScene(scSpellbook)
      else
        MsgLog.Add('You don''t have a spellbook equipped!');
    end;
    TK_C:
      CastSpell();
    TK_A:
      Ability.UseAbility();
    TK_T:
      Scenes.SetScene(scTalents, scGame);
    TK_SLASH:
      Scenes.SetScene(scHelp, scGame);
//    TK_Z:
//      Scenes.SetScene(scWin, scGame);
  end;
end;


end.
