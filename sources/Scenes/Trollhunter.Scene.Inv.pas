unit Trollhunter.Scene.Inv;

interface

uses
  Classes,
  Graphics,
  Trollhunter.Scene,
  Trollhunter.Scenes,
  Trollhunter.Inv,
  Trollhunter.Item,
  Trollhunter.Creatures,
  Trollhunter.Scene.BaseGame;

type
  TSceneInv = class(TSceneBaseGame)
  private
    CursorPos: Integer;
    Icon: Graphics.TBitmap;
  public
    ItemUseID: string;
    Hero, Item: Graphics.TBitmap;
    procedure RedrawPCIcon;
    procedure RenderUseIcon;
    procedure Render(); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    constructor Create;
    destructor Destroy; override;
  end;

var
  SceneInv: TSceneInv;

implementation

uses
  Windows,
  Types,
  SysUtils,
  Trollhunter.Error,
  Trollhunter.Graph,
  Trollhunter.Color,
  Trollhunter.Game,
  Trollhunter.Utils,
  Trollhunter.Log,
  Trollhunter.Lang,
  Trollhunter.Scene.Game,
  Trollhunter.Scene.Item,
  Trollhunter.Scene.Char,
  Trollhunter.Scene.Items;

{ TSceneInv }

constructor TSceneInv.Create;
begin
  inherited Create(25);
  Icon := Graphics.TBitmap.Create;
  Hero := Graphics.TBitmap.Create;
  Hero.Transparent := True;
  Item := Graphics.TBitmap.Create;
  Item.Transparent := True;
  CursorPos := 1;
end;

destructor TSceneInv.Destroy;
begin
  Icon.Free;
  Hero.Free;
  Item.Free;
  inherited;
end;

procedure TSceneInv.KeyDown(var Key: Word; Shift: TShiftState);
var
  I, C: Integer;
  K: Word;
begin
  inherited;
  try
    case Key of
      38, 40:
        begin
          C := Creatures.PC.Inv.Count;
          if (C > 0) then
          begin
            CursorPos := CursorPos + (Key - 39);
            CursorPos := ClampCycle(CursorPos, 1, C);
            Render;
          end;
        end;
      8:
        begin
          C := Items.CellItemsCount(Creatures.PC.Pos.X, Creatures.PC.Pos.Y);
          if (C > 0) then
            Scenes.Scene := SceneItems;
        end;
      32:
        Scenes.Scene := SceneChar;
      13:
        begin
          C := Creatures.PC.Inv.Count;
          if (C > 0) then
          begin
            K := (ord('A') + CursorPos) - 1;
            KeyDown(K, Shift);
          end;
        end;
      27, 123:
        begin
          CursorMode := cmNone;
          Log.Apply;
          Game.Save;
          ItemUseID := '';
          Creatures.PC.Redraw;
          Graph.Messagebar.Clear;
          Scenes.Render;
          Exit;
        end;
      ord('A') .. ord('Z'):
        begin
          I := (Key - (ord('A'))) + 1;
          if (I <= Creatures.PC.Inv.Count) then
          begin
            if (ItemUseID <> '') then
            begin
              Items.Use(ItemUseID, DungeonItems[SceneItem.KeyIDToInvItemID(I)
                ].Sprite, I);
              ItemUseID := '';
              Log.Apply;
              Game.Save;
              Creatures.PC.Redraw;
              Scenes.Render;
              Exit;
            end;
            SceneItem.CursorPos := 0;
            SceneItem.ItemIndex := I;
            Scenes.Scene := SceneItem;
          end;
        end;
    end;
  except
    on E: Exception do
      Error.Add('SceneInv.KeyDown (#' + IntToStr(Key) + ')', E.Message);
  end;
end;

procedure TSceneInv.KeyPress(var Key: Char);
begin
  inherited;

end;

procedure TSceneInv.RedrawPCIcon;
begin
  try
    Creatures.PC.Redraw;
    Hero.Assign(Creatures.PC.Image);
    ScaleBmp(Hero, 64, 64);
    Hero.Transparent := True;
  except
    on E: Exception do
      Error.Add('SceneInv.RedrawPCIcon', E.Message);
  end;
end;

procedure TSceneInv.RenderUseIcon;
var
  B, R: string;

  procedure DrawIcon(G: string; Y: Byte);
  var
    I: Integer;
    Tileset: Graphics.TBitmap;
  begin
    Tileset := Graphics.TBitmap.Create;
    try
      I := Items.ItemIndex(G);
      if (DungeonItems[I].AdvSprite = '') then
        Tileset.Handle := Windows.LoadBitmap(hInstance, PChar(G))
      else
        Tileset.Handle := Windows.LoadBitmap(hInstance,
          PChar(DungeonItems[I].AdvSprite));
      Graph.BitmapFromTileset(Item, Tileset, 0);
      Items.Colors(Item, I);
      ScaleBmp(Item, 64, 64);
      Item.Transparent := True;
      Graph.Surface.Canvas.Draw(Graph.Surface.Width - 72, Y, Item);
    finally
      Tileset.Free;
    end;
  end;

begin
  DrawIcon(ItemUseID, 96);
  B := DungeonItems[Items.ItemIndex(CursorPos)].Sprite;
  DrawIcon(B, 176);
  R := Items.Craft(ItemUseID, B);
  if (R <> '') then
    DrawIcon(R, 255);
end;

procedure TSceneInv.Render;
var
  S: string;
  I, ID, C, Y: Integer;
  Tileset: Graphics.TBitmap;
begin
  inherited;
  Tileset := Graphics.TBitmap.Create;
  try
    Y := 2;
    with Graph.Surface.Canvas do
    begin
      C := Creatures.PC.Inv.Count;
      for I := 1 to C do
      begin
        CursorPos := ClampCycle(CursorPos, 1, C);
        ID := Items.ItemIndex(I);
        S := Chr(I + 96) + '.';
        Font.Color := clSilver;
        if (CursorPos = I) then
        begin
          Font.Style := [fsBold];
          Graph.RenderMenu(Y, 0);
        end
        else
        begin
          Font.Style := [];
        end;
        Graph.Text.DrawOut(1, Y, S);

        if (DungeonItems[ID].AdvSprite = '') then
          Tileset.Handle := Windows.LoadBitmap(hInstance,
            PChar(Creatures.PC.Inv.GetID(I)))
        else
          Tileset.Handle := Windows.LoadBitmap(hInstance,
            PChar(DungeonItems[ID].AdvSprite));
        Graph.BitmapFromTileset(Icon, Tileset, 0);
        Items.Colors(Icon, ID);
        ScaleBmp(Icon, Graph.CharHeight, Graph.CharHeight);
        Icon.Transparent := True;
        Draw(Graph.CharWidth * 3, Y * Graph.CharHeight, Icon);

        Items.SetColor(ID);
        if Creatures.PC.Inv.GetDoll(I) then
        begin
          Font.Color := cRdYellow;
          Font.Style := [fsBold];
        end
        else if (Font.Style <> [fsBold]) then
          Font.Style := [];
        if ((DungeonItems[ID].MaxTough > 0) and
          (Creatures.PC.Inv.GetTough(I) <= 0)) then
          Font.Color := cRed;
        Graph.Text.DrawText(5, Y, Trim(GetItemLang(DungeonItems[ID].Sprite) +
          Items.GetItemProp(Creatures.PC.Inv.GetCount(I),
          Creatures.PC.Inv.GetTough(I), I, ID) + Items.GetWeight(ID) +
          Items.GetDollText(I, ID)));
        Inc(Y);
      end;
      Items.RenderPCInvStat(Y);
      if (C = 1) then
        Graph.Text.BarOut('enter, a', GetLang(24), False)
      else if (C > 1) then
        Graph.Text.BarOut('enter, a-' + Chr(96 + C), GetLang(24), False);
      Graph.Text.BarOut('space', GetLang(8), False);
      if (Items.CellItemsCount(Creatures.PC.Pos.X, Creatures.PC.Pos.Y) > 0) then
        Graph.Text.BarOut('backspace', GetLang(111), False);
      Draw(Graph.Surface.Width - 72, Graph.CharHeight, Hero);
      if (ItemUseID <> '') then
        RenderUseIcon;
    end;
    Graph.Render;
    Tileset.Free;
  except
    on E: Exception do
      Error.Add('SceneInv.Render', E.Message);
  end;
end;

initialization

SceneInv := TSceneInv.Create;

finalization

SceneInv.Free;

end.
