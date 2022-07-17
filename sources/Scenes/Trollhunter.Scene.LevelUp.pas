unit Trollhunter.Scene.LevelUp;

interface

uses
  Classes,
  Trollhunter.Scene;

type
  TSceneLevelUp = class(TScene)
  private
    Count, T, CursorPos, P: Integer;
    procedure AtrItem(I: Integer; S: string);
  public
    procedure Render(); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    constructor Create;
    destructor Destroy; override;
  end;

var
  SceneLevelUp: TSceneLevelUp;

implementation

uses
  Graphics,
  SysUtils,
  Trollhunter.Graph,
  Trollhunter.Creatures,
  Trollhunter.Scenes,
  Trollhunter.Scene.Game,
  Trollhunter.Color,
  Trollhunter.Error,
  Trollhunter.Log,
  Trollhunter.Lang,
  Trollhunter.Utils;

{ TSceneLevelUp }

constructor TSceneLevelUp.Create;
begin
  CursorPos := 0;
  Count := 4;
end;

procedure TSceneLevelUp.AtrItem(I: Integer; S: string);
begin
  try
    with Graph.Surface.Canvas do
    begin
      case I of
        0:
          S := S + #32 + IntToStr(Creatures.PC.Prop.Strength);
        1:
          S := S + #32 + IntToStr(Creatures.PC.Prop.Dexterity);
        2:
          S := S + #32 + IntToStr(Creatures.PC.Prop.Will);
        3:
          S := S + #32 + IntToStr(Creatures.PC.Prop.Speed);
      end;
      if (CursorPos = P) then
      begin
        Font.Color := cAcColor;
        Font.Style := [fsBold];
        Graph.RenderMenu(P + 1, T, cDkGray);
      end
      else
      begin
        Font.Color := cBgColor;
        Font.Style := [];
      end;
      TextOut((Graph.Width div 2) - (TextWidth(S) div 2),
        (P * Graph.CharHeight) + T + Graph.CharHeight, S);
      Font.Color := cLtBlue;
      if (P = CursorPos) then
        TextOut((Graph.Width div 2) - (TextWidth(S) div 2) +
          ((Length(S) + 1) * Graph.CharWidth), (P * Graph.CharHeight) + T +
          Graph.CharHeight, '+ 1');
      Inc(P);
    end;
  except
    on E: Exception do
      Error.Add('SceneLevelUp.AtrItem', E.Message);
  end;
end;

destructor TSceneLevelUp.Destroy;
begin

  inherited;
end;

procedure TSceneLevelUp.KeyDown(var Key: Word; Shift: TShiftState);
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
        with Creatures.PC do
        begin
          case CursorPos of
            0:
              AddStrength;
            1:
              AddDexterity;
            2:
              AddWill;
            3:
              AddSpeed;
          end;
          Calc;
          Log.Apply;
          Scenes.Scene := SceneGame;
        end;
    end;
  except
    on E: Exception do
      Error.Add('SceneLevelUp.KeyDown (#' + IntToStr(Key) + ')', E.Message);
  end;
end;

procedure TSceneLevelUp.KeyPress(var Key: Char);
begin
  inherited;

end;

procedure TSceneLevelUp.Render;
var
  I: Byte;
begin
  inherited;
  try
    P := 0;
    Graph.Clear(0);
    T := (Graph.Height div 2) - (Count * Graph.CharHeight div 2);
    with Graph.Surface.Canvas do
    begin
      for I := 0 to Count - 1 do
        AtrItem(I, GetLang(I + 15));
      T := T div Graph.CharHeight;
      Font.Style := [];
      Font.Color := cBgColor;
      Graph.Text.TextCenter(T - 3, GetLang(60));
      Graph.Text.TextCenter(T - 2, GetLang(62));
      Graph.Text.TextCenter(T - 1, GetLang(63));
      Font.Style := [];
    end;
    Graph.Render;
  except
    on E: Exception do
      Error.Add('SceneLevelUp.Render', E.Message);
  end;
end;

initialization

SceneLevelUp := TSceneLevelUp.Create;

finalization

SceneLevelUp.Free;

end.
