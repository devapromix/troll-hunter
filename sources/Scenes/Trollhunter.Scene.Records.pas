unit Trollhunter.Scene.Records;

interface

uses
  Classes,
  Trollhunter.Scene,
  Trollhunter.Scene.BaseMenu;

type
  TSceneRecords = class(TSceneBaseMenu)
  private
    CursorPos, MaxItems: Integer;
  public
    procedure Render(); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    constructor Create;
    destructor Destroy; override;
  end;

var
  SceneRecords: TSceneRecords;

implementation

uses
  SysUtils,
  Graphics,
  Trollhunter.Utils,
  Trollhunter.Graph,
  Trollhunter.Scene.Menu,
  Trollhunter.Scenes,
  Trollhunter.Color,
  Trollhunter.Error,
  Trollhunter.Lang,
  Trollhunter.Game;

{ TSceneRecords }

const
  ColCount = 6;

constructor TSceneRecords.Create;
begin
  inherited Create(3);
  CursorPos := 0;
  MaxItems := 0;
end;

destructor TSceneRecords.Destroy;
begin

  inherited;
end;

procedure TSceneRecords.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  try
    case Key of
      27, 123:
        begin
          Game.Scores.Line := -1;
        end;
      38, 40:
        begin
          CursorPos := CursorPos + (Key - 39);
          CursorPos := ClampCycle(CursorPos, 0, MaxItems - 1);
          Render;
        end;
    end;
  except
    on E: Exception do
      Error.Add('SceneRecords.KeyDown (#' + IntToStr(Key) + ')', E.Message);
  end;
end;

procedure TSceneRecords.KeyPress(var Key: Char);
begin
  inherited;

end;

procedure TSceneRecords.Render;
const
  N: array [0 .. ColCount - 1] of Byte = (36, 37, 140, 30, 39, 35);
var
  K: array [0 .. ColCount - 1] of Word;
  I, J, Y, L, Max: Integer;
  S: string;
  R: Byte;
begin
  inherited;
  try
    R := 1;
    with Graph.Surface.Canvas do
    begin
      Max := (Graph.Height div Graph.CharHeight) - 6;
      if (Game.Scores.MaxCount < Max) then
        Max := Game.Scores.MaxCount;
      // Col Width
      for J := 0 to High(K) do
      begin
        K[J] := (Length(GetLang(N[J])) + 1) * Graph.CharWidth;
        for I := 0 to Max do
        begin
          if (I >= Game.Scores.MaxCount) or (I >= Game.Scores.Count) then
            Break;
          L := (Length(Game.Scores.GetValue(J, I)) + 1) * Graph.CharWidth;
          if (L > K[J]) then
            K[J] := L;
        end;
      end;
      L := (Graph.CharWidth * (ColCount - 1)) + (Graph.CharWidth * 4);
      for J := 1 to High(K) do
        Inc(L, K[J]);
      if (Graph.Width > L) then
      begin
        L := ((Graph.Width - L) div (ColCount - 1));
        for J := 1 to High(K) do
          Inc(K[J], L);
      end;
      if (Game.Scores.Count > 0) then
      begin
        // Col Name
        Font.Color := cDkYellow;
        TextOut(Graph.CharWidth, Graph.CharHeight * 2, '##');
        //
        L := Graph.CharWidth * 4;
        for J := 1 to High(K) do
        begin
          TextOut(L, Graph.CharHeight * 2, GetLang(N[J]));
          Inc(L, K[J]);
        end;
        TextOut(Graph.Width - (K[0] + Graph.CharWidth), Graph.CharHeight * 2,
          GetLang(N[0]));
      end;
      // Col Value
      for I := 0 to Max do
      begin
        if (I >= Game.Scores.MaxCount) or (I >= Game.Scores.Count) then
          Break;
        if (Game.Scores.Line = I) or ((CursorPos = I)) then
        begin
          Font.Style := [fsBold];
          if (Game.Scores.Line = I) then
          begin
            Graph.RenderMenu(R + 2, 0, cDkRed);
            Font.Color := cDkYellow;
          end
          else
          begin
            Graph.RenderMenu(R + 2, 0, cDkGray);
            Font.Color := cAcColor;
          end;
        end
        else
        begin
          Font.Color := cBgColor;
        end;
        Y := (I + 3) * Graph.CharHeight;
        S := IntToStr(R) + '.';
        TextOut((Graph.CharWidth * 4) - TextWidth(S), Y, S);
        L := Graph.CharWidth * 4;
        for J := 1 to High(K) do
        begin
          TextOut(L, Y, Game.Scores.GetValue(J, I));
          Inc(L, K[J]);
        end;
        TextOut(Graph.Width - (K[0] + Graph.CharWidth), Y,
          Game.Scores.GetValue(0, I));
        Font.Style := [];
        Inc(R);
      end;
    end;
    Graph.Render;
    MaxItems := R - 1;
  except
    on E: Exception do
      Error.Add('SceneRecords.Render', E.Message);
  end;
end;

initialization

SceneRecords := TSceneRecords.Create;

finalization

SceneRecords.Free;

end.
