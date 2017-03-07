unit uScenes;

interface

uses
  Classes;

type
  TSceneEnum = (scTitle, scLoad, scHelp, scGame, scQuit, scWin, scDef, scInv,
    scDrop);

type
  TScene = class(TObject)
    procedure Render; virtual; abstract;
    procedure Update(var Key: Word); virtual; abstract;
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
  TSceneHelp = class(TScene)
  private
    SL: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

type
  TSceneGame = class(TScene)
  private
    procedure RenderLifeBar(X, Y: Byte);
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

implementation

uses
  SysUtils, Types, Dialogs, Math, uCommon, uTerminal, uPlayer, BearLibTerminal,
  uMap;

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
  Terminal.BackgroundColor(0);
  Terminal.ForegroundColor(clYellow);
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
      if GameMode and not (Scene in [scWin, scDef, scQuit]) then
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
  Terminal.Print(X, Y - 3, 'Trollhunter v.' + Version, TK_ALIGN_CENTER);
  Terminal.Print(X, Y - 1, 'by Apromix <bees@meta.ua>', TK_ALIGN_CENTER);
  Terminal.Print(X, Y + 1, 'Press [[SPACE]] to continue...', TK_ALIGN_CENTER);
end;

procedure TSceneTitle.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE:
      CanClose := True;
    TK_SPACE:
    begin
      Scenes.SetScene(scLoad);
      Terminal.Refresh;
      Map.Gen;
      terminal_delay(1000);
      GameMode := True;
      Scenes.SetScene(scGame);
    end;
  end;
end;

{ TSceneHelp }

constructor TSceneHelp.Create;
begin
  SL := TStringList.Create;
  SL.LoadFromFile(HelpFileName);
end;

destructor TSceneHelp.Destroy;
begin
  SL.Free;
  inherited;
end;

procedure TSceneHelp.Render;
var
  I, X, Y: Byte;
begin
  X := Terminal.Window.Width div 2;
  Y := (Terminal.Window.Height div 2) - ((SL.Count + 4) div 2);
  for I := 0 to SL.Count - 1 do
  Terminal.Print(X, Y + I, SL[I], TK_ALIGN_CENTER);
  Terminal.Print(X, Terminal.Window.Height - Y - 1,
    '[color=red][[ESC]][/color] Close', TK_ALIGN_CENTER);
end;

procedure TSceneHelp.Update(var Key: Word);
begin
  case Key of
    TK_R: // Refresh
      if WizardMode then
      begin
        SL.LoadFromFile(HelpFileName);
      end;
    TK_ESCAPE: // Close
      Scenes.SetScene(scGame);
  end;
end;

{ TSceneGame }

procedure TSceneGame.Render;
var
  I, X, Y, PX, PY, DX, DY: Integer;
  T: TTile;
  Min, Max: TPoint;

  procedure RenderLook(T: TTile);
  var
    S: string;
  begin
    S := '';
    Terminal.BackgroundColor(0);
    Terminal.ForegroundColor(clYellow);
    S := S + T.Name + '. ';
    Terminal.Print(Info.Left, Info.Top, S);
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
      if Map.GetTileEnum(AX, AY, Map.Deep) in StopTiles then Exit;
    end;
  end;

begin
  // Map
  if not WizardMode then
  begin
    Min.X := Player.X - Player.GetRadius;
    Max.X := Player.X + Player.GetRadius;
    Min.Y := Player.Y - Player.GetRadius;
    Max.Y := Player.Y + Player.GetRadius;
    Map.ClearFOV;
    for I := Min.X to Max.X do AddTo(I, Min.Y);
    for I := Min.Y to Max.Y do AddTo(Max.X, I);
    for I := Max.X downto Min.X do AddTo(I, Max.Y);
    for I := Max.Y downto Min.Y do AddTo(Min.X, I);
  end;
  Terminal.BackgroundColor(0);
  PX := View.Width div 2;
  PY := View.Height div 2;
  for DY := 0 to View.Height - 1 do
    for DX := 0 to View.Width - 1 do
    begin
      X := DX - PX + Player.X;
      Y := DY - PY + Player.Y;
      if not Map.InMap(X, Y) then Continue;
      if not WizardMode then
        if (GetDist(Player.X, Player.Y, X, Y) > Player.GetRadius)
          and Map.GetFog(X, Y) then Continue;
      T := Map.GetTile(X, Y);
      if (Player.Look and (Player.LX = X) and (Player.LY = Y))  then
      begin
        Terminal.BackgroundColor($88FFFF00);
        Terminal.Print(DX + View.Left, DY + View.Top, ' ');
        RenderLook(T);
      end;
      if (not Player.Look and (Player.X = X) and (Player.Y = Y)) then
        RenderLook(T);
      if not WizardMode then
      begin
        if (GetDist(Player.X, Player.Y, X, Y) <= Player.GetRadius) then
        begin
          if not Map.GetFog(X, Y) then
            Terminal.ForegroundColor(clFog);
          if Map.GetFOV(X, Y) then
          begin
            Terminal.ForegroundColor(T.Color);
            Map.SetFog(X, Y, False);
          end;
        end else begin
          if not Map.GetFog(X, Y) then
            Terminal.ForegroundColor(clFog);
        end;
      end else Terminal.ForegroundColor(T.Color);
      if WizardMode or not Map.GetFog(X, Y) then
        Terminal.Print(DX + View.Left, DY + View.Top, T.Symbol);
    end;
  Terminal.ForegroundColor(clDarkRed);
  Terminal.Print(PX + View.Left, PY + View.Top, '@');
  // Player
  Terminal.BackgroundColor(0);
  Terminal.ForegroundColor(clYellow);
  Terminal.Print(Status.Left, Status.Top, 'Trollhunter');
  Terminal.Print(Status.Left + Status.Width - 1, Status.Top, Format('%s (%d:%d)',
    [Map.GetName, Player.X, Player.Y]), TK_ALIGN_RIGHT);
  Terminal.ForegroundColor(clYellow);
  Terminal.Print(Status.Left, Status.Top + 1, Format('Life %d/%d', [Player.Life, Player.MaxLife]));
  Terminal.ForegroundColor(clYellow);
  Terminal.Print(Status.Left, Status.Top + 2, Format('Turn %d', [Player.Turn]));
  // Lifebar
  RenderLifeBar(Status.Left ,Status.Top + 1);
  // Log
  for Y := 0 to Log.Height - 1 do
    for X := 0 to Log.Width - 1 do
    begin
      Terminal.BackgroundColor($FF550055);
      Terminal.ForegroundColor($FFFFFF00);
      Terminal.Print(X + Log.Left, Y + Log.Top, ' ');
    end;
end;

procedure TSceneGame.RenderLifeBar(X, Y: Byte);
var
  I, L, W: Byte;
begin
  L := Status.Width - 14;
  W := BarWidth(Player.Life, Player.MaxLife, L);
  for I := 0 to L do
  begin
    Terminal.BackgroundColor(clDarkGray);
    if (I <= W) then
    begin
      if (Player.Life > 0) then
      begin
        Terminal.BackgroundColor(clDarkRed);
      end;
    end;
    Terminal.Print(X + I + 13, Y, ' ');
  end;
end;

procedure TSceneGame.Update(var Key: Word);
begin
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
      if WizardMode then
        if (Map.Deep < High(TDeepEnum)) then
          Map.Deep := succ(Map.Deep);
    TK_KP_MINUS:
      if WizardMode then
        if (Map.Deep > Low(TDeepEnum)) then
          Map.Deep := pred(Map.Deep);
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
    TK_SLASH:
      Scenes.SetScene(scHelp);
    TK_ESCAPE:
      Scenes.SetScene(scQuit, Scenes.Scene);
    TK_I:
      Scenes.SetScene(scInv);
    TK_F:
      Scenes.SetScene(scDrop);
    TK_V:
      if WizardMode then
        Scenes.SetScene(scWin);
    TK_B:
      if WizardMode then
        Scenes.SetScene(scDef);
  end;
end;

{ TSceneLoad }

procedure TSceneLoad.Render;
begin
  Terminal.Print(Terminal.Window.Width div 2, Terminal.Window.Height div 2, 'Creating the world, please wait...', TK_ALIGN_CENTER);
end;

procedure TSceneLoad.Update(var Key: Word);
begin

end;

{ TSceneQuit }

procedure TSceneQuit.Render;
begin
  Terminal.Print(Terminal.Window.Width div 2, Terminal.Window.Height div 2, 'Quit? [[Y/N]]', TK_ALIGN_CENTER);
end;

procedure TSceneQuit.Update(var Key: Word);
begin
  case Key of
    TK_Y:
    begin
      Player.SaveCharacterDump('Quit the game');
      CanClose := True;
    end;
    TK_ESCAPE, TK_N:
      Scenes.GoBack;
  end;
end;

{ TSceneDef }

procedure TSceneDef.Render;
begin
  Terminal.Print(Terminal.Window.Width div 2, Terminal.Window.Height div 2,
    Format('Game over: killed by %s. Press ENTER', ['MOB']), TK_ALIGN_CENTER);
end;

procedure TSceneDef.Update(var Key: Word);
begin
  case Key of
    TK_ENTER:
    begin
      Player.SaveCharacterDump(Format('Killed by %s', ['MOB']));
      CanClose := True;
    end;
  end;
end;

{ TSceneWin }

procedure TSceneWin.Render;
begin
  Terminal.Print(Terminal.Window.Width div 2, Terminal.Window.Height div 2,
    'Congratulations! You have won. Press ENTER', TK_ALIGN_CENTER);
end;

procedure TSceneWin.Update(var Key: Word);
begin
  case Key of
    TK_ENTER:
    begin
      Player.SaveCharacterDump('Won the game');
      CanClose := True;
    end;
  end;
end;

{ TSceneInv }

procedure TSceneInv.Render;
var
  X, Y: Byte;
begin
  Y := 1;
  X := Terminal.Window.Width div 2;
  Terminal.Print(X, Y, 'Inventory', TK_ALIGN_CENTER);

  Terminal.Print(X, Terminal.Window.Height - Y - 1,
    '[color=red][[ESC]][/color] Close', TK_ALIGN_CENTER);
end;

procedure TSceneInv.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE: // Close
      Scenes.SetScene(scGame);
  end;
end;

{ TSceneDrop }

procedure TSceneDrop.Render;
var
  X, Y: Byte;
begin
  Y := 1;
  X := Terminal.Window.Width div 2;
  Terminal.Print(X, Y, 'Select an item to drop', TK_ALIGN_CENTER);

  Terminal.Print(X, Terminal.Window.Height - Y - 1,
    '[color=red][[ESC]][/color] Close', TK_ALIGN_CENTER);
end;

procedure TSceneDrop.Update(var Key: Word);
begin
  case Key of
    TK_ESCAPE: // Close
      Scenes.SetScene(scGame);
  end;
end;

initialization
  Scenes := TScenes.Create;
  Scenes.SetScene(scTitle);

finalization
  Scenes.Free;
  Scenes := nil;

end.
