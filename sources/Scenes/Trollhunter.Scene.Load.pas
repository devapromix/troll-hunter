unit Trollhunter.Scene.Load;

interface

uses
  Classes,
  Trollhunter.Scene,
  Trollhunter.Scene.BaseMenu;

type
  TSceneLoad = class(TSceneBaseMenu)
  private
    CursorPos: Integer;
    SS: TStringList;
    function Count: Byte;
  public
    procedure Render(); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure ReadSaveDir();
    constructor Create;
    destructor Destroy; override;
  end;

var
  SceneLoad: TSceneLoad;

implementation

uses
  SysUtils,
  Graphics,
  Trollhunter.Graph,
  Trollhunter.Scenes,
  Trollhunter.Scene.Menu,
  Trollhunter.Utils,
  Trollhunter.Color,
  Trollhunter.MainForm,
  Trollhunter.Game,
  Trollhunter.Creatures,
  Trollhunter.Scene.Game,
  Trollhunter.Error,
  Trollhunter.Lang;

{ TSceneLoad }

function TSceneLoad.Count: Byte;
begin
  Result := SS.Count;
  if (Result > 26) then
    Result := 26;
end;

constructor TSceneLoad.Create;
begin
  inherited Create(1);
  SS := TStringList.Create;
  CursorPos := 1;
end;

destructor TSceneLoad.Destroy;
begin
  SS.Free;
  inherited;
end;

procedure TSceneLoad.KeyDown(var Key: Word; Shift: TShiftState);
var
  I: Byte;
  K: Word;
  C: Integer;
begin
  inherited;
  try
    case Key of
      38, 40:
        begin
          C := Count;
          if (C > 0) then
          begin
            CursorPos := CursorPos + (Key - 39);
            CursorPos := ClampCycle(CursorPos, 1, C);
            Render;
          end;
        end;
      13:
        begin
          C := Count;
          if (C > 0) then
          begin
            K := (ord('A') + CursorPos) - 1;
            KeyDown(K, Shift);
          end;
        end;
      ord('A') .. ord('Z'):
        begin
          I := Key - (ord('A'));
          if (I < Count) then
          begin
            SceneGame.Free;
            SceneGame := TSceneGame.Create;
            Creatures.PC.Name := SS[I];
            Game.Load();
            Graph.Messagebar.Clear;
            Graph.Messagebar.Add(Format(GetLang(20), [Creatures.PC.Name,
              MainForm.Caption]));
            Scenes.Scene := SceneGame;
          end;
        end;
    end;
  except
    on E: Exception do
      Error.Add('SceneLoad.KeyDown (#' + IntToStr(Key) + ')', E.Message);
  end;
end;

procedure TSceneLoad.KeyPress(var Key: Char);
begin
  inherited;

end;

procedure TSceneLoad.ReadSaveDir;
var
  SR: TSearchRec;
  S: string;
begin
  try
    SS.Clear;
    if (FindFirst(Path + '\save\*.sav', faAnyFile, SR) = 0) then
    begin
      repeat
        S := Trim(SR.Name);
        if (S = '') then
          Continue;
        Delete(S, Length(S) - 3, 4);
        SS.Append(S);
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;
    SS.Sort;
  except
    on E: Exception do
      Error.Add('SceneLoad.ReadSaveDir', E.Message);
  end;
end;

procedure TSceneLoad.Render;
var
  C, I, Y: Integer;
  S: string;
  P: TPCInfo;
begin
  inherited;
  try
    with Graph.Surface.Canvas do
    begin
      C := Count;
      for I := 0 to C - 1 do
      begin
        if (I > 25) then
          Break;
        Y := (I + 3) * Graph.CharHeight;
        S := Chr(I + 97) + '.';
        CursorPos := Clamp(CursorPos, 1, C);
        if (CursorPos = I + 1) then
        begin
          Font.Style := [fsBold];
          Font.Color := cAcColor;
          Graph.RenderMenu(I + 3, 0);
        end
        else
        begin
          Font.Style := [];
          Font.Color := cBgColor;
        end;
        P := Game.GetPCInfo(Path + 'save\' + SS[I] + '.sav');
        TextOut((Graph.CharWidth * 3) - TextWidth(S), Y, S);
        TextOut(Graph.CharWidth * 3, Y, SS[I]);
        TextOut(Graph.CharWidth * 20, Y, IntToStr(P.Level));
        TextOut(Graph.CharWidth * 30, Y, IntToStr(P.Rating));
        TextOut(Graph.CharWidth * 40, Y, GetMapLang(P.Dungeon));
        TextOut(Graph.CharWidth * 70, Y,
          GetFileDate(Path + 'save\' + SS[I] + '.sav'));
      end;
      Font.Style := [];
      if (Count > 0) then
      begin
        Font.Color := cDkYellow;
        TextOut(Graph.CharWidth, Graph.CharHeight * 2, '#');
        TextOut(Graph.CharWidth * 3, Graph.CharHeight * 2, GetLang(37));
        TextOut(Graph.CharWidth * 20, Graph.CharHeight * 2, GetLang(30));
        TextOut(Graph.CharWidth * 30, Graph.CharHeight * 2, GetLang(36));
        TextOut(Graph.CharWidth * 40, Graph.CharHeight * 2, GetLang(39));
        TextOut(Graph.CharWidth * 70, Graph.CharHeight * 2, GetLang(140));
      end;
      if (Count = 1) then
        Graph.Text.BarOut('a', GetLang(28), False)
      else if (Count > 1) then
        Graph.Text.BarOut('a-' + Chr(96 + Count), GetLang(28), False)
    end;
    Graph.Render;
  except
    on E: Exception do
      Error.Add('SceneLoad.Render', E.Message);
  end;
end;

initialization

SceneLoad := TSceneLoad.Create;

finalization

SceneLoad.Free;

end.
