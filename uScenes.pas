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

  public
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

implementation

uses uCommon, uTerminal;

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
const
  PlayerX = 5;
  PlayerY = 5;
begin
  PX := View.Width div 2;
  PY := View.Height div 2;
  for DY := 0 to View.Height - 1 do
    for DX := 0 to View.Width - 1 do
    begin
      X := DX - PX + PlayerX;
      Y := DY - PY + PlayerY;
      if (X < Low(Byte)) or (Y < Low(Byte))
        or (X > High(Byte)) or (Y > High(Byte)) then Continue;
      Terminal.BackgroundColor($FF550055);
      //Terminal.Clear;
      Terminal.ForegroundColor($FFFFFF00);
      Terminal.Print(DX + View.Left, DY + View.Top, '.');
      //Terminal.Refresh;
    end;
  Terminal.Print(PX + View.Left, PY + View.Top, '@');
  for Y := 0 to Status.Height - 1 do
    for X := 0 to Status.Width - 1 do
    begin
      Terminal.BackgroundColor($FF550055);
      //Terminal.Clear;
      Terminal.ForegroundColor($FFFFFF00);
      Terminal.Print(X + Status.Left, Y + Status.Top, ' ');
      //Terminal.Refresh;
    end;
  for Y := 0 to Log.Height - 1 do
    for X := 0 to Log.Width - 1 do
    begin
      Terminal.BackgroundColor($FF550055);
      //Terminal.Clear;
      Terminal.ForegroundColor($FFFFFF00);
      Terminal.Print(X + Log.Left, Y + Log.Top, ' ');
      //Terminal.Refresh;
    end;
end;

procedure TSceneGame.Update(var Key: Word);
begin
  inherited;

end;

initialization
  Scenes := TScenes.Create;
  Scenes.SetScene(scGame);

finalization
  Scenes.Free;
  Scenes := nil;

end.
