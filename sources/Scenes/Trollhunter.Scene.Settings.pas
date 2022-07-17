unit Trollhunter.Scene.Settings;

interface

uses
  Classes,
  Trollhunter.Scene,
  Trollhunter.Scene.BaseMenu;

type
  TSceneSettings = class(TSceneBaseMenu)
  private
    Count, P, T: Integer;
    CursorPos: Integer;
    procedure SettingsItem(A, B: string);
  public
    procedure Render(); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    constructor Create;
    destructor Destroy; override;
  end;

var
  SceneSettings: TSceneSettings;

implementation

uses
  Graphics,
  SysUtils,
  Trollhunter.Graph,
  Trollhunter.Color,
  Trollhunter.Scenes,
  Trollhunter.MainForm,
  Trollhunter.Scene.Menu,
  Trollhunter.Error,
  Trollhunter.Utils,
  Trollhunter.Lang,
  Trollhunter.Settings;

{ TSceneSettings }

constructor TSceneSettings.Create;
begin
  inherited Create(2);
  Count := 3;
  CursorPos := 0;
end;

destructor TSceneSettings.Destroy;
begin

  inherited;
end;

procedure TSceneSettings.KeyDown(var Key: Word; Shift: TShiftState);
var
  S: TSettings;

  procedure Use(I: Integer);
  begin
    case CursorPos of
      0:
        begin
          ChangeLanguage;
          Render;
        end;
      1:
        begin
          with Graph.Surface.Canvas.Font do
          begin
            Size := Size + I;
            Size := Clamp(Size, 10, 20);
          end;
          Graph.Default;
          Render;
        end;
      2:
        begin
          TileSize := TileSize + (I * 16);
          TileSize := Clamp(TileSize, BaseTileSize, 64);
          Graph.Default;
          Render;
        end;
    end;
  end;

begin
  inherited;
  try
    case Key of
      27, 123:
        begin
          S := TSettings.Create;
          try
            S.Write('Settings', 'Language', LangID);
            S.Write('Settings', 'FontSize', Graph.Surface.Canvas.Font.Size);
            S.Write('Settings', 'TileSize', (TileSize - BaseTileSize) div 16);
          finally
            S.Free;
          end;
        end;
      38, 40:
        begin
          CursorPos := CursorPos + (Key - 39);
          CursorPos := ClampCycle(CursorPos, 0, Count - 1);
          Render;
        end;
      37, 39:
        Use(Key - 38);
    end;
  except
    on E: Exception do
      Error.Add('SceneSettings.KeyDown (#' + IntToStr(Key) + ')', E.Message);
  end;
end;

procedure TSceneSettings.KeyPress(var Key: Char);
begin
  inherited;

end;

procedure TSceneSettings.SettingsItem(A, B: string);
var
  S, L: string;
  I, D: Byte;
begin
  try
    with Graph.Surface.Canvas do
    begin
      L := #32;
      B := '<< ' + B + ' >>';
      D := 30 - Length(A + L + B);
      for I := 0 to D do
        L := L + #32;
      S := A + L + B;
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
      Error.Add('SceneSettings.SettingsItem', E.Message);
  end;
end;

procedure TSceneSettings.Render;
begin
  inherited;
  try
    P := 0;
    with Graph.Surface.Canvas do
    begin
      T := (Graph.Height div 2) - ((Count - 1) * Graph.CharHeight div 2);
      SettingsItem(GetLang('Language', 'Язык'), LanguageName);
      SettingsItem(GetLang('Font Size', 'Размер Шрифта'), IntToStr(Font.Size));
      SettingsItem(GetLang('Tile Size', 'Размер Тайла'), IntToStr(TileSize));
    end;
    Graph.Render;
  except
    on E: Exception do
      Error.Add('SceneSettings.Render', E.Message);
  end;
end;

initialization

SceneSettings := TSceneSettings.Create;

finalization

SceneSettings.Free;

end.
