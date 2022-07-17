unit Trollhunter.Scene.Game;

interface

uses
  Classes,
  Graphics,
  Trollhunter.Scene,
  Trollhunter.Map,
  Trollhunter.Map.Tiles;

var
  TT, MaxTT: Cardinal;

type
  TSceneGame = class(TScene)
  private
    procedure Go(T: Tiles);
  public
    procedure GoToPrevMap;
    procedure GoToNextMap;
    procedure GoToAltNextMap;
    procedure Render(); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Info();
    constructor Create;
    destructor Destroy; override;
  end;

var
  SceneGame: TSceneGame;

implementation

uses
  Windows,
  Types,
  SysUtils,
  Trollhunter.MainForm,
  Trollhunter.Graph,
  Trollhunter.Creatures,
  Trollhunter.Scenes,
  Trollhunter.Utils,
  Trollhunter.Trap,
  Trollhunter.Log,
  Trollhunter.Game,
  Trollhunter.Item,
  Trollhunter.Color,
  Trollhunter.Error,
  Trollhunter.Scene.Menu,
  Trollhunter.Scene.Inv,
  Trollhunter.Projectiles,
  Trollhunter.Scene.Item,
  Trollhunter.Decorator,
  Trollhunter.Scene.Char,
  Trollhunter.Lang,
  Trollhunter.Light,
  Trollhunter.Look,
  Trollhunter.Scene.Help,
  Trollhunter.Scene.LevelUp,
  Trollhunter.Skill,
  Trollhunter.Scene.Statistics,
  Trollhunter.Statistics;

{ TGame }

constructor TSceneGame.Create;
begin

end;

destructor TSceneGame.Destroy;
begin
  inherited;
end;

procedure TSceneGame.Go(T: Tiles);
var
  X, Y: Word;
begin
  for X := 0 to Map.Width - 1 do
    for Y := 0 to Map.Height - 1 do
      if (T = Map.Cell[Y][X].Tile) then
      begin
        Creatures.PC.SetPosition(X, Y);
        Exit;
      end;
end;

procedure TSceneGame.GoToPrevMap;
var
  I: Integer;
  B: Boolean;
begin
  if (Map.Info.PrevMap <> '') then
  begin
    Graph.Messagebar.Clear;
    Log.Add(GetLang(290) + ' ' + GetMapLang(Map.Info.PrevMap, True) + '.');
    Log.Apply;
    Game.Save;
    B := Map.Info.IsAltMapEnt;
    I := Map.GetMapIndex(Map.Info.PrevMap);
    if (I < 0) then
      I := 0;
    Game.Load(I);
    if B then
      Go(tlAltNextDungeon)
    else
      Go(tlNextDungeon);
    Scenes.Render;
  end;
end;

procedure TSceneGame.GoToNextMap;
begin
  if (Map.Info.NextMap <> '') then
  begin
    Graph.Messagebar.Clear;
    Log.Add(GetLang(290) + ' ' + GetMapLang(Map.Info.NextMap, True) + '.');
    Log.Apply;
    Game.Save;
    Game.Load(Map.GetMapIndex(Map.Info.NextMap));
    Go(tlPrevDungeon);
    Scenes.Render;
  end;
end;

procedure TSceneGame.GoToAltNextMap;
begin
  if (Map.Info.AltNextMap <> '') then
  begin
    Graph.Messagebar.Clear;
    Log.Add(GetLang(290) + ' ' + GetMapLang(Map.Info.AltNextMap, True) + '.');
    Log.Apply;
    Game.Save;
    Game.Load(Map.GetMapIndex(Map.Info.AltNextMap));
    Go(tlPrevDungeon);
    Scenes.Render;
  end;
end;

procedure TSceneGame.Info();
var
  T: Tiles;
  F: Boolean;
  I, J, K: Integer;
  S: string;
begin
  F := False;
  T := tlMin;
  case CursorMode of
    cmNone:
      T := Map.Cell[Creatures.PC.Pos.Y][Creatures.PC.Pos.X].Tile;
    cmLook, cmShoot:
      begin
        if (Length(Creatures.Enemy) > 0) then
          for I := 0 to High(Creatures.Enemy) do
            if (Creatures.PC.Look.X = Creatures.Enemy[I].Pos.X) and
              (Creatures.PC.Look.Y = Creatures.Enemy[I].Pos.Y) then
            begin
              Graph.Messagebar.Add
                ('#r' + GetCreatureLang(Creatures.Enemy[I].Name) +
                Format(' (%d/%d).$', [Creatures.Enemy[I].Life.Cur,
                Creatures.Enemy[I].Life.Max]));
              Exit;
            end;
        T := Map.Cell[Creatures.PC.Look.Y][Creatures.PC.Look.X].Tile;
      end;
  end;
  case T of
    tlLockedDoor:
      Graph.Messagebar.Add(GetLang(58));
    tlClosedDoor:
      Graph.Messagebar.Add(GetLang(59));
    tlPrevDungeon:
      Graph.Messagebar.Add(Format(GetLang(83), [GetMapLang(Map.Info.PrevMap)]));
    tlNextDungeon:
      Graph.Messagebar.Add(Format(GetLang(84), [GetMapLang(Map.Info.NextMap)]));
    tlAltNextDungeon:
      Graph.Messagebar.Add(Format(GetLang(84),
        [GetMapLang(Map.Info.AltNextMap)]));
    tlEmptyShrine:
      Graph.Messagebar.Add(GetLang(85));
    tlLifeShrine:
      Graph.Messagebar.Add(GetLang(86));
    tlManaShrine:
      Graph.Messagebar.Add(GetLang(86));
    tlMegaShrine:
      Graph.Messagebar.Add(GetLang(86));
    tlClosedBarrel:
      Graph.Messagebar.Add(GetLang(57));
    tlClosedWoodChest:
      Graph.Messagebar.Add(GetLang(87));
    tlLockedWoodChest:
      Graph.Messagebar.Add(GetLang(88));
    tlLockedBestChest:
      Graph.Messagebar.Add(GetLang(88));
  end;
  // Item
  if (Length(Items.Item) > 0) then
    for I := 0 to High(Items.Item) do
      if (Creatures.PC.Pos.X = Items.Item[I].Pos.X) and
        (Creatures.PC.Pos.Y = Items.Item[I].Pos.Y) then
      begin
        Graph.Messagebar.Clear;
        S := GetItemLang(Items.Item[I].Prop.Sprite);
        if (Items.Item[I].Prop.IsStack) and (Items.Item[I].Count > 1) then
          S := S + Format('$ (#r%dx$)', [Items.Item[I].Count]);
        if (Items.CellItemsCount(Creatures.PC.Pos.X, Creatures.PC.Pos.Y) > 1)
        then
          J := 0
        else
          J := 1;
        case T of
          tlOpenWoodChest, tlOpenBestChest:
            begin
              K := 51;
              F := True;
            end;
          tlOpenBarrel:
            begin
              K := 101;
              F := True;
            end;
        else
          begin
            K := 53;
            F := True;
          end;
        end;
        Graph.Messagebar.Add(Format(GetLang(K + J), [S]));
        Break;
      end;
  if F then
    Exit;
  case T of
    tlOpenBarrel:
      Graph.Messagebar.Add(GetLang(56));
    tlOpenWoodChest, tlOpenBestChest:
      Graph.Messagebar.Add(GetLang(47));
  end;
end;

procedure TSceneGame.KeyDown(var Key: Word; Shift: TShiftState);
var
  I, X, Y: Integer;
  T: Tiles;

  procedure OpenDoor(AX, AY: Integer);
  begin
    Map.Cell[Creatures.PC.Pos.Y + AY][Creatures.PC.Pos.X + AX].Tile :=
      tlOpenDoor;
    Log.Add(GetLang(46));
  end;

  procedure DoMove();
  var
    Trap: TTrap;
    TX, TY: Integer;
  begin
    Graph.Messagebar.Clear;
    // Look
    if (CursorMode <> cmNone) then
    begin
      if (GetDist(Creatures.PC.Pos.X, Creatures.PC.Pos.Y,
        Creatures.PC.Look.X + X, Creatures.PC.Look.Y + Y) >
        Creatures.PC.Prop.Distance) then
        Exit;
      if Map.Cell[Creatures.PC.Look.Y + Y, Creatures.PC.Look.X + X].FOV then
        Creatures.PC.IncLook(X, Y);
      Scenes.Render;
      Exit;
    end
    else
      // Move
      if Creatures.PC.FreeCell(Creatures.PC.Pos.X + X, Creatures.PC.Pos.Y + Y)
      then
      begin
        TX := Creatures.PC.Pos.X + X;
        TY := Creatures.PC.Pos.X + Y;
        Creatures.PC.Move(X, Y);
        if (TX <> Creatures.PC.Pos.X + X) or (TY <> Creatures.PC.Pos.X + Y) then
          Creatures.PC.Statistics.Inc(stTilesMoved);
      end
      else
        case Map.Cell[Creatures.PC.Pos.Y + Y][Creatures.PC.Pos.X + X].Tile of
          // Open door
          tlClosedDoor:
            OpenDoor(X, Y);
          // Trap in door
          tlClosedDoorWithFireTrap:
            begin
              OpenDoor(X, Y);
              Trap := TTrap.Create;
              Trap.Fire;
              Trap.Free;
            end;
          tlClosedDoorWithLightningTrap:
            begin
              OpenDoor(X, Y);
              Trap := TTrap.Create;
              Trap.Lightning;
              Trap.Free;
            end;
          // Locked door
          tlLockedDoor:
            begin
              if (Creatures.PC.Inv.GetCount('KEY') > 0) then
              begin
                Log.Add(GetLang(44));
                Creatures.PC.Inv.Del('KEY');
                OpenDoor(X, Y);
              end
              else
                Log.Add(GetLang(10));
            end;
          // Hidden door
          tlHiddenDoor:
            begin
              case Rand(1, 9) of
                1 .. 4:
                  T := tlClosedDoor;
                5 .. 6:
                  T := tlClosedDoorWithFireTrap;
                7 .. 8:
                  T := tlClosedDoorWithLightningTrap;
                9:
                  T := tlLockedDoor;
              end;
              Map.Cell[Creatures.PC.Pos.Y + Y][Creatures.PC.Pos.X + X]
                .Tile := T;
              Log.Add(GetLang(43));
            end;
        end;
    Creatures.Move;
    Log.Apply;
    Scenes.Render;
  end;

  procedure Move(AX, AY: Integer);
  begin
    X := AX;
    Y := AY;
    with Creatures.PC do
      if IsValidCell(Pos.X + AX, Pos.Y + AY) then
        DoMove();
  end;

  procedure Run(AX, AY: Integer);
  begin

  end;

begin
  inherited;
  try
    TransKeys(Key);
    if Creatures.PC.Life.IsMin then
    begin
      Creatures.PC.Defeat;
      Exit;
    end;
    T := tlMin;
    X := 0;
    Y := 0;

    // Move //
    if ssAlt in Shift then
    begin
      if ((GetKeyState(VK_LEFT) and 128) = 128) and
        ((GetKeyState(VK_DOWN) and 128) = 128) then
        Move(-1, 1)
      else if ((GetKeyState(VK_RIGHT) and 128) = 128) and
        ((GetKeyState(VK_DOWN) and 128) = 128) then
        Move(1, 1)
      else if ((GetKeyState(VK_LEFT) and 128) = 128) and
        ((GetKeyState(VK_UP) and 128) = 128) then
        Move(-1, -1)
      else if ((GetKeyState(VK_RIGHT) and 128) = 128) and
        ((GetKeyState(VK_UP) and 128) = 128) then
        Move(1, -1);
      Exit;
    end
    else
      case Key of
        13:
          case CursorMode of
            cmShoot:
              begin
                if (Length(Creatures.Enemy) > 0) then
                  with Creatures.PC do
                    for I := 0 to High(Creatures.Enemy) do
                      if not Creatures.Enemy[I].Life.IsMin and
                        (Look.X = Creatures.Enemy[I].Pos.X) and
                        (Look.Y = Creatures.Enemy[I].Pos.Y) then
                      begin
                        Ranged(I);
                        Graph.Messagebar.Clear;
                        Exit;
                      end;
              end;
          end;
        // Look //
        ord('L'):
          begin
            if CursorMode <> cmNone then
              CursorMode := cmNone
            else
            begin
              Creatures.PC.Look := Creatures.PC.Pos;
              CursorMode := cmLook;
            end;
            Scenes.Render;
          end;
        // Shoot //
        ord('S'):
          begin
            if CursorMode <> cmNone then
              CursorMode := cmNone
            else if Items.IsRangedWeapon then
            begin
              Creatures.PC.Look := Creatures.PC.Pos;
              CursorMode := cmShoot;
            end;
            Scenes.Render;
          end;
        // Debug //
        ord('T'):
          if ParamDebug then
          begin
            // Creatures.PC.AddStrength;
            // Inc(Creatures.PC.Turns);
            // Items.Grow;
            // Creatures.PC.TrainSkill(skTrap);
            // Inc(Creatures.PC.Dungeon);
            // Game.Load(1);

            // Scenes.Scene := SceneLevelUp;
            Scenes.Render;
          end;
        // Detect traps //
        ord('D'):
          begin
            Creatures.PC.Wait;
            Creatures.PC.DoDetectTraps;
            Scenes.Render;
          end;
        ord('M'):
          if ParamDebug then
          begin
            Inc(Creatures.PC.Prop.Radius);
            Scenes.Render;
          end;
        ord('N'):
          if ParamDebug then
          begin
            Dec(Creatures.PC.Prop.Radius);
            Scenes.Render;
          end;
        27, 123:
          begin
            Game.Save;
            Scenes.Scene := SceneMenu;
            CursorMode := cmNone;
          end;
        ord('I'):
          begin
            SceneInv.RedrawPCIcon;
            SceneInv.ItemUseID := '';
            Scenes.Scene := SceneInv;
          end;
        ord('C'):
          begin
            SceneInv.RedrawPCIcon;
            Scenes.Scene := SceneChar;
          end;
        ord('Y'):
          begin
            Scenes.Scene := SceneStatistics;
          end;
        // Rest //
        ord('R'):
          begin

          end;
        // Move //
        ord('W'), 12, 101, 53:
          Creatures.PC.Wait;
        35, 97, 49:
          if ssShift in Shift then
            Run(-1, 1)
          else
            Move(-1, 1);
        40, 98, 50:
          if ssShift in Shift then
            Run(0, 1)
          else
            Move(0, 1);
        34, 99, 51:
          if ssShift in Shift then
            Run(1, 1)
          else
            Move(1, 1);
        37, 100, 52:
          if ssShift in Shift then
            Run(-1, 0)
          else
            Move(-1, 0);
        39, 102, 54:
          if ssShift in Shift then
            Run(1, 0)
          else
            Move(1, 0);
        36, 103, 55:
          if ssShift in Shift then
            Run(-1, -1)
          else
            Move(-1, -1);
        38, 104, 56:
          if ssShift in Shift then
            Run(0, -1)
          else
            Move(0, -1);
        33, 105, 57:
          if ssShift in Shift then
            Run(1, -1)
          else
            Move(1, -1);
        // Use object //
        32, ord('U'):
          begin
            case Map.Cell[Creatures.PC.Pos.Y][Creatures.PC.Pos.X].Tile of
              tlLifeShrine:
                begin
                  Graph.Messagebar.Clear;
                  with TAnimNumber.Create(Creatures.PC.Life.Max -
                    Creatures.PC.Life.Cur) do
                    Free;
                  Creatures.PC.Life.SetToMax;
                  Map.Cell[Creatures.PC.Pos.Y][Creatures.PC.Pos.X].Tile :=
                    tlEmptyShrine;
                  Creatures.PC.Rating := Creatures.PC.Rating + 25;
                  // Log.Add('.');
                  Scenes.Render;
                end;
              tlManaShrine:
                begin
                  Graph.Messagebar.Clear;
                  with TAnimNumber.Create(Creatures.PC.Mana.Max -
                    Creatures.PC.Mana.Cur) do
                    Free;
                  Creatures.PC.Mana.SetToMax;
                  Map.Cell[Creatures.PC.Pos.Y][Creatures.PC.Pos.X].Tile :=
                    tlEmptyShrine;
                  Creatures.PC.Rating := Creatures.PC.Rating + 25;
                  // Log.Add('.');
                  Scenes.Render;
                end;
              tlMegaShrine:
                begin
                  Graph.Messagebar.Clear;
                  with TAnimNumber.Create(Creatures.PC.Life.Max -
                    Creatures.PC.Life.Cur) do
                    Free;
                  with TAnimNumber.Create(Creatures.PC.Mana.Max -
                    Creatures.PC.Mana.Cur) do
                    Free;
                  Creatures.PC.Fill;
                  Map.Cell[Creatures.PC.Pos.Y][Creatures.PC.Pos.X].Tile :=
                    tlEmptyShrine;
                  Creatures.PC.Rating := Creatures.PC.Rating + 50;
                  // Log.Add('.');
                  Scenes.Render;
                end;
              tlPrevDungeon:
                GoToPrevMap;
              tlNextDungeon:
                GoToNextMap;
              tlAltNextDungeon:
                GoToAltNextMap;
              tlClosedBarrel:
                begin
                  Graph.Messagebar.Clear;
                  OpenChest(False);
                  Log.Apply;
                  Scenes.Render;
                end;
              tlClosedWoodChest:
                begin
                  Graph.Messagebar.Clear;
                  OpenChest(False);
                  Log.Apply;
                  Scenes.Render;
                end;
              tlLockedWoodChest, tlLockedBestChest:
                begin
                  Graph.Messagebar.Clear;
                  if (Map.Cell[Creatures.PC.Pos.Y][Creatures.PC.Pos.X].Tile
                    in [tlLockedWoodChest, tlLockedBestChest]) then
                  begin
                    if (Creatures.PC.Inv.GetCount('KEY') > 0) then
                    begin
                      Creatures.PC.Inv.Del('KEY');
                      OpenChest(True);
                    end
                    else
                      Log.Add(GetLang(10));
                    Log.Apply;
                    Scenes.Render;
                  end;
                  Scenes.Render;
                end;
            end;
          end;
        // Pick up
        ord('G'):
          Items.Pickup;
        112: // Help
          Scenes.Scene := SceneHelp;
        ord('H'):
          if ParamDebug then
            GoToPrevMap;
        ord('J'):
          if ParamDebug then
            GoToNextMap;
        ord('K'):
          if ParamDebug then
            GoToAltNextMap;
        ord('F'):
          if ParamDebug then
            Scenes.Scene := SceneLevelUp;
      end;
  except
    on E: Exception do
      Error.Add('SceneGame.KeyDown (#' + IntToStr(Key) + ')', E.Message);
  end;
end;

procedure TSceneGame.KeyPress(var Key: Char);
begin
  inherited;

end;

procedure TSceneGame.Render;
begin
  inherited;
  TT := GetTickCount;
  Graph.Clear(0);
  Graph.Bars.Render;
  Log.Render;
  Info();
  Map.Render;
  TT := GetTickCount - TT;
  if (MaxTT < TT) then
    MaxTT := TT;
  if ParamDebug then
    with Graph.Surface.Canvas do
    begin
      Brush.Style := bsClear;
      Font.Color := cWhiteYel;
      TextOut(0, Graph.CharHeight, IntToStr(TT) + '(' + IntToStr(MaxTT) + ')');
      TextOut(0, Graph.CharHeight * 2, Format('X%d:Y%d', [Creatures.PC.Pos.X,
        Creatures.PC.Pos.Y]));
      TextOut(0, Graph.CharHeight * 3, Format('LX%d:LY%d', [Creatures.PC.Look.X,
        Creatures.PC.Look.Y]));
    end;
  Graph.Messagebar.Render;
  Map.MiniMap.Render;
  Creatures.PC.Effects.Render;
  Graph.Render;
end;

initialization

finalization

SceneGame.Free;

end.
