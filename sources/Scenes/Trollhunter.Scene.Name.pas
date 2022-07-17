unit Trollhunter.Scene.Name;

interface

uses
  Classes,
  Trollhunter.Scene,
  Trollhunter.Scene.BaseMenu;

type
  TSceneName = class(TSceneBaseMenu)
  private
    procedure AddChar(S: string);
  public
    procedure Render(); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    constructor Create;
    destructor Destroy; override;
  end;

var
  SceneName: TSceneName;

implementation

uses
  SysUtils,
  Graphics,
  Trollhunter.Creatures,
  Trollhunter.Scenes,
  Trollhunter.Scene.Game,
  Trollhunter.Graph,
  Trollhunter.Color,
  Trollhunter.Game,
  Trollhunter.MainForm,
  Trollhunter.Scene.Menu,
  Trollhunter.Name,
  Trollhunter.Utils,
  Trollhunter.Error,
  Trollhunter.Lang,
  Trollhunter.Scene.Race;

{ TSceneName }

procedure TSceneName.AddChar(S: string);
begin
  if (Length(Creatures.PC.Name) < 15) then
  begin
    Creatures.PC.Name := Creatures.PC.Name + S;
    Render;
  end;
end;

constructor TSceneName.Create;
begin
  inherited Create(4);
end;

destructor TSceneName.Destroy;
begin

  inherited;
end;

procedure TSceneName.KeyDown(var Key: Word; Shift: TShiftState);
var
  N: string;
  Name: TName;
begin
  inherited;
  try
    Name := TName.Create;
    try
      case Key of
        8:
          begin
            if (Length(Creatures.PC.Name) > 0) then
            begin
              N := Creatures.PC.Name;
              Delete(N, Length(N), 1);
              Creatures.PC.Name := N;
              Render();
            end;
          end;
        13:
          begin
            if (Creatures.PC.Name <> '') then
            begin
              SceneGame.Free;
              SceneGame := TSceneGame.Create;
              if FileExists(Path + 'save\' + Creatures.PC.Name + '.sav') then
              begin
                Game.Load();
                Graph.Messagebar.Add(Format(GetLang(20), [Creatures.PC.Name,
                  MainForm.Caption]));
                Scenes.Scene := SceneGame;
              end
              else
              begin
                N := Creatures.PC.Name;
                Game.New;
                Creatures.PC.Name := N;
                SceneRace.MakePC;
                Scenes.Scene := SceneRace;
              end;
            end
            else
            begin
              Creatures.PC.Name := Name.Gen();
              Render();
            end;
          end;
        32:
          begin
            Creatures.PC.Name := Name.Gen();
            Render();
          end;
      end;
    finally
      Name.Free;
    end;
  except
    on E: Exception do
      Error.Add('SceneName.KeyDown (#' + IntToStr(Key) + ')', E.Message);
  end;
end;

procedure TSceneName.Render;
var
  S, N: string;
  W, H: Integer;
  P: TPCInfo;
begin
  inherited;
  try
    with Graph.Surface.Canvas do
    begin
      N := 'space';
      Font.Color := cBgColor;
      S := GetLang(38) + #32 + Creatures.PC.Name + '_';
      W := TextWidth(S);
      S := GetLang(38);
      Font.Style := [];
      H := TextWidth(S);
      TextOut((Graph.Width div 2) - (W div 2), (Graph.Height div 2) -
        (Graph.CharHeight div 2), S);
      Font.Style := [fsBold];
      Font.Color := cAcColor;
      S := #32 + Creatures.PC.Name + '_';
      TextOut((Graph.Width div 2) - (W div 2) + H, (Graph.Height div 2) -
        (Graph.CharHeight div 2), S);
      if (Creatures.PC.Name = '') then
        N := 'enter, ' + N;
      Graph.Text.BarOut(N, 48);
      if FileExists(Path + 'save\' + Creatures.PC.Name + '.sav') then
      begin
        P := Game.GetPCInfo(Path + 'save\' + Creatures.PC.Name + '.sav');
        S := GetLang(30) + ': ' + IntToStr(P.Level) + ' | ' + GetLang(36) + ': '
          + IntToStr(P.Rating) + ' | ' + GetLang(140) + ': ' +
          GetFileDate(Path + 'save\' + Creatures.PC.Name + '.sav');
        Font.Color := cBgColor;
        Graph.Text.TextCenter(((Graph.Height div 2) div Graph.CharHeight) + 2,
          AnsiLowerCase(S));
        Graph.Text.BarOut('enter', 1);
      end;
    end;
    Graph.Render;
  except
    on E: Exception do
      Error.Add('SceneName.Render', E.Message);
  end;
end;

procedure TSceneName.KeyPress(var Key: Char);
const
  C1 = 'abcdefghijklmnopqrstuvwxyz';
  C2 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  C3 = '-';
  Chars = C1 + C2 + C3;
begin
  if (Pos(Key, Chars) > 0) then
    AddChar(Key);
end;

initialization

SceneName := TSceneName.Create;

finalization

SceneName.Free;

end.
