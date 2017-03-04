unit uScenes;

interface

type
  TSceneEnum = (scGame);

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
  TSceneGame = class(TScene)
  private
    procedure RenderLifeBar(X, Y: Byte);
  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

implementation

uses SysUtils, Math, uCommon, uTerminal, uPlayer, BearLibTerminal, uMap;

{ TScenes }

constructor TScenes.Create;
var
  I: TSceneEnum;
begin
  for I := Low(TSceneEnum) to High(TSceneEnum) do
    case I of
      scGame:
        FScene[I] := TSceneGame.Create;
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
end;

{ TSceneGame }

procedure TSceneGame.Render;
var
  X, Y, PX, PY, DX, DY: Integer;
  T: TTile;
begin
  // Map
  Terminal.BackgroundColor(0);
  PX := View.Width div 2;
  PY := View.Height div 2;
  for DY := 0 to View.Height - 1 do
    for DX := 0 to View.Width - 1 do
    begin
      X := DX - PX + Player.X;
      Y := DY - PY + Player.Y;
      if (X < Low(Byte)) or (Y < Low(Byte))
        or (X > High(Byte)) or (Y > High(Byte)) then Continue;
      T := Map.GetTile(X, Y);
      Terminal.ForegroundColor(T.Color);
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
      Player.Move(0, 0);
    TK_KP_PLUS:
      if (Player.Life < Player.MaxLife) then
        Player.Life := Player.Life + 1;
    TK_KP_MINUS:
      if (Player.Life > 0) then
        Player.Life := Player.Life - 1;
  end;
end;

initialization
  Scenes := TScenes.Create;
  Scenes.SetScene(scGame);

finalization
  Scenes.Free;
  Scenes := nil;

end.
