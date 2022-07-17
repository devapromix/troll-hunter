unit Trollhunter.Scene.Items;

interface

uses
  Classes,
  Graphics,
  Windows,
  Trollhunter.Scene,
  Trollhunter.Scene.BaseGame;

type
  TSceneItems = class(TSceneBaseGame)
  private
    CursorPos: Integer;
    Icon: Graphics.TBitmap;
  public
    procedure KeyPress(var Key: Char); override;
    procedure Render(); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    constructor Create;
    destructor Destroy; override;
  end;

var
  SceneItems: TSceneItems;

implementation

uses
  SysUtils,
  Trollhunter.Scene.Game,
  Trollhunter.Scenes,
  Trollhunter.Error,
  Trollhunter.Graph,
  Trollhunter.Item,
  Trollhunter.Creatures,
  Trollhunter.Color,
  Trollhunter.Utils,
  Trollhunter.Lang,
  Trollhunter.Scene.Inv;

{ TSceneItem }

constructor TSceneItems.Create;
begin
  inherited Create(21);
  Icon := Graphics.TBitmap.Create;
  CursorPos := 1;
end;

destructor TSceneItems.Destroy;
begin
  Icon.Free;
  inherited;
end;

procedure TSceneItems.KeyDown(var Key: Word; Shift: TShiftState);
var
  I, C: Integer;
  K: Word;
begin
  inherited;
  try
    case Key of
      38, 40:
        begin
          C := Items.CellItemsCount(Creatures.PC.Pos.X, Creatures.PC.Pos.Y);
          if (C > 0) then
          begin
            CursorPos := CursorPos + (Key - 39);
            CursorPos := ClampCycle(CursorPos, 1, C);
            Render;
          end;
        end;
      32:
        begin
          Items.PickupAll;
          Scenes.Scene := SceneGame
        end;
      8:
        Scenes.Scene := SceneInv;
      13:
        begin
          C := Items.CellItemsCount(Creatures.PC.Pos.X, Creatures.PC.Pos.Y);
          if (C > 0) then
          begin
            K := (ord('A') + CursorPos) - 1;
            KeyDown(K, Shift);
          end;
        end;
      ord('A') .. ord('Z'):
        begin
          I := (Key - (ord('A'))) + 1;
          if (I <= Items.CellItemsCount(Creatures.PC.Pos.X, Creatures.PC.Pos.Y))
          then
          begin
            Items.Pickup(I);
            if (Items.CellItemsCount(Creatures.PC.Pos.X, Creatures.PC.Pos.Y) = 0)
            then
              Scenes.Scene := SceneGame
            else
              Render;
          end;
        end;
    end;
  except
    on E: Exception do
      Error.Add('SceneItems.KeyDown (#' + IntToStr(Key) + ')', E.Message);
  end;
end;

procedure TSceneItems.KeyPress(var Key: Char);
begin
  inherited;

end;

procedure TSceneItems.Render;
var
  Tileset: Graphics.TBitmap;
  I, Y, C, V: Integer;
  S, ID: string;
begin
  inherited;
  Tileset := Graphics.TBitmap.Create;
  try
    Y := 2;
    with Graph.Surface.Canvas do
    begin
      C := Items.CellItemsCount(Creatures.PC.Pos.X, Creatures.PC.Pos.Y);
      if (C > 0) and (Length(Items.Item) > 0) then
        for I := 0 to High(Items.Item) do
          if (Items.Item[I].Pos.X = Creatures.PC.Pos.X) and
            (Items.Item[I].Pos.Y = Creatures.PC.Pos.Y) then
          begin
            CursorPos := Clamp(CursorPos, 1, C);
            ID := Items.Item[I].Name;
            if (ID = '') then
              Continue;
            V := Items.ItemIndex(ID);
            S := Chr((Y - 1) + 96) + '.';
            Font.Color := clSilver;
            if (CursorPos = Y - 1) then
            begin
              Font.Style := [fsBold];
              Graph.RenderMenu(Y, 0, cDkGray);
            end
            else
            begin
              Font.Style := [];
            end;
            Graph.Text.DrawOut(1, Y, S);
            if (DungeonItems[V].AdvSprite = '') then
              Tileset.Handle := Windows.LoadBitmap(hInstance, PChar(ID))
            else
              Tileset.Handle := Windows.LoadBitmap(hInstance,
                PChar(DungeonItems[V].AdvSprite));
            Graph.BitmapFromTileset(Icon, Tileset, 0);
            Items.Colors(Icon, V);
            ScaleBmp(Icon, Graph.CharHeight, Graph.CharHeight);
            Icon.Transparent := True;
            Draw(Graph.CharWidth * 3, Y * Graph.CharHeight, Icon);
            Items.SetColor(V);
            if ((DungeonItems[V].MaxTough > 0) and
              (Items.Item[I].Prop.Tough <= 0)) then
              Font.Color := cRed;
            Graph.Text.DrawText(5, Y, GetItemLang(DungeonItems[V].Sprite) +
              Items.GetItemProp(Items.Item[I].Count, Items.Item[I].Prop.Tough,
              I, V) + Items.GetWeight(V));
            Inc(Y);
          end;
      Items.RenderPCInvStat(Y);
      if (C = 1) then
        Graph.Text.BarOut('enter, a', GetLang(26), False)
      else if (C > 1) then
        Graph.Text.BarOut('enter, a-' + Chr(96 + C), GetLang(26), False);
      Graph.Text.BarOut('backspace', GetLang(25), False);
      Graph.Text.BarOut('space', GetLang(50), False);
    end;
    Graph.Render;
    Tileset.Free;
  except
    on E: Exception do
      Error.Add('SceneItems.Render', E.Message);
  end;
end;

initialization

SceneItems := TSceneItems.Create;

finalization

SceneItems.Free;

end.
