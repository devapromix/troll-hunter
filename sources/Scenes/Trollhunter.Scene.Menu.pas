unit Trollhunter.Scene.Menu;

interface

uses
  Classes,
  Trollhunter.Scene;

type
  TSceneMenu = class(TScene)
  private
    Count, P, T: Integer;
    CursorPos: Integer;
    procedure MenuItem(S: string);
    procedure DrawCopyright;
    procedure DrawLogo;
  public
    procedure Render(); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    constructor Create;
    destructor Destroy; override;
  end;

var
  SceneMenu: TSceneMenu;

implementation

uses
  Graphics,
  SysUtils,
  Trollhunter.Graph,
  Trollhunter.Color,
  Trollhunter.Scene.Name,
  Trollhunter.Scenes,
  Trollhunter.MainForm,
  Trollhunter.Scene.Records,
  Trollhunter.Creatures,
  Trollhunter.Game,
  Trollhunter.Scene.Load,
  Trollhunter.Error,
  Trollhunter.Utils,
  Trollhunter.Scene.Settings,
  Trollhunter.Lang,
  Trollhunter.Settings,
  Trollhunter.Item,
  Trollhunter.Creature,
  Trollhunter.Map;

{ TSceneMenu }

procedure TSceneMenu.DrawCopyright;
var
  I, T: Integer;
  S: string;
begin
  S := '';
  T := Graph.Height div Graph.CharHeight;
  if ParamDebug then
  begin
    for I := 1 to ParamCount do
      S := S + ParamStr(I) + #32;
    S := Trim(S);
  end;
  with Graph.Surface.Canvas do
  begin
    Font.Style := [];
    Brush.Color := 0;
  end;
  with Graph do
  begin
    if ParamDebug and (S <> '') then
    begin
      Surface.Canvas.Font.Color := cRdGray;
      Text.TextCenter(T - 3, Format('[Items: %d, Enemies: %d, Maps: %d]',
        [ItemsCount, CreaturesCount, MapsCount]));
      Text.TextCenter(T - 4, '[' + S + ']');
    end;
  end;
end;

constructor TSceneMenu.Create;
begin
  CursorPos := 0;
  Count := 5;
end;

destructor TSceneMenu.Destroy;
begin

  inherited;
end;

procedure TSceneMenu.KeyDown(var Key: Word; Shift: TShiftState);
var
  S: TSettings;
begin
  try
    case Key of
      38, 40:
        begin
          CursorPos := CursorPos + (Key - 39);
          CursorPos := ClampCycle(CursorPos, 0, Count - 1);
          Render;
        end;
      13:
        case CursorPos of
          0:
            begin
              with Creatures do
                if (PC.Name = '') then
                begin
                  S := TSettings.Create;
                  try
                    PC.Name := S.Read('Settings', 'LastName', '');
                  finally
                    S.Free;
                  end;
                end;
              Scenes.Scene := SceneName;
            end;
          1:
            begin
              SceneLoad.ReadSaveDir;
              Scenes.Scene := SceneLoad;
            end;
          2:
            begin
              Scenes.Scene := SceneSettings;
            end;
          3:
            begin
              Scenes.Scene := SceneRecords;
            end;
          4:
            MainForm.Close;
        end;
    end;
  except
    on E: Exception do
      Error.Add('SceneMenu.KeyDown (#' + IntToStr(Key) + ')', E.Message);
  end;
end;

procedure TSceneMenu.KeyPress(var Key: Char);
begin
  inherited;

end;

procedure TSceneMenu.MenuItem(S: string);
begin
  try
    with Graph.Surface.Canvas do
    begin
      if (CursorPos = P) then
      begin
        Font.Color := cAcColor;
        Font.Style := [fsBold];
        Graph.RenderMenu(P, T, cDkGray);
      end
      else
      begin
        Font.Color := cBgColor;
        Font.Style := [];
      end;
      TextOut((Graph.Width div 2) - (TextWidth(S) div 2),
        (P * Graph.CharHeight) + T, S);
      Inc(P);
    end;
  except
    on E: Exception do
      Error.Add('SceneMenu.MenuItem', E.Message);
  end;
end;

procedure TSceneMenu.Render;
var
  I, N: Byte;
begin
  inherited;
  try
    P := 0;
    Graph.Clear(0);
    T := (Graph.Height div 2) - ((Count - 1) * Graph.CharHeight div 2);
    for I := 0 to Count - 1 do
    begin
      if (I < Count - 1) then
        N := I
      else
        N := 9;
      MenuItem(GetLang(N));
    end;
    IsGame := False;
    DrawLogo();
    DrawCopyright();
    Graph.Render();
  except
    on E: Exception do
      Error.Add('SceneMenu.Render', E.Message);
  end;
end;

procedure TSceneMenu.DrawLogo;
var
  X, Y, H: Word;
const
  Logo: array [0 .. 17] of string =
    (' ______________                                                                                  ',
    '| ____  . ____ |              ___ ___ ___                               ___                       ',
    '|/    |. |    \|              \ .\\ .\\. \                              \ .\                      ',
    '      | .|                    | .||. || .|                              | .|                      ',
    '      |. |____  ___   ______  |. || .||. | ____ ____   ____ ____  ____  |. |__   ______ ____  ___ ',
    '      |::|\:::|/:::\ /::::::\ |::||::||::|/::::\\:::\  \:::|\:::|/::::\ |:::::| /::::::\\:::|/:::\',
    '      |xx| |xx|  \x||xx/  \xx||xx||xx||xx|   \xx\|xx|   |xx| |xx|   \xx\|xx|   |xx/__\xx||xx|  \x|',
    '      |xx| |xx|     |xx|  |xx||xx||xx||xx|   |xx||xx|   |xx| |xx|   |xx||xx|   |xx|xxxxx||xx|     ',
    '      |XX| |XX|     |XX\__/XX||XX||XX||XX|   |XX||XX\___|XX| |XX|   |XX||XX\___|XX\_____ |XX|     ',
    '      |XX| \XX\      \XXXXXX/ \XX\\XX\\XX\   \XX\ \XXXX/|XX\ \XX\   \XX\ \XXXX/ \XXXXXX/ \XX\     ',
    '      |XX|                                                                                        ',
    '      |XX|                                                                                        ',
    '      |XX|                                                                                        ',
    '     _|XX|                                                                                        ',
    '     \XXX|                                                                                        ',
    '      \XX|                                                                                        ',
    '       \X|                                                                                        ',
    '        \|                                                                                        ');

begin
  with Graph.Surface.Canvas do
  begin
    H := High(Logo) * Graph.CharHeight;
    X := (Graph.Surface.Width div 2) -
      ((Length(Logo[0]) * Graph.CharWidth) div 2);
    for Y := 0 to High(Logo) do
    begin
      Font.Style := [fsBold];
      Font.Color := DarkColor(cLtRed, Y * 5);
      TextOut(X, (Y * Graph.CharHeight) + ((T div 2) - (H div 2)), Logo[Y]);
    end;
  end;
end;

initialization

SceneMenu := TSceneMenu.Create;

finalization

SceneMenu.Free;

end.
