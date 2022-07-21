unit Trollhunter.Projectiles;

interface

uses
  Graphics,
  Trollhunter.Creature,
  Trollhunter.Creatures;

type
  TProjectile = class(TObject)
  private
    FTileset: Graphics.TBitmap;
    FX1, FY1, FX2, FY2: Integer;
    FBack, FImage: Graphics.TBitmap;
  public
    constructor Create(ACreature: TCreature; FromX, FromY, ToX, ToY: Integer);
  end;

type
  TAnimNumber = class(TObject)
  private
    FBack: Graphics.TBitmap;
  public
    constructor Create(Number, TX, TY: Integer); overload;
    constructor Create(Number: Integer); overload;
  end;

implementation

uses
  Windows,
  SysUtils,
  Math,
  Types,
  Trollhunter.Graph,
  Trollhunter.Utils,
  Trollhunter.Color;

{ TProjectile }

constructor TProjectile.Create(ACreature: TCreature;
  FromX, FromY, ToX, ToY: Integer);
var
  I, L, AX, AY, RX, RY: Integer;
  LR: Real;

  procedure Render();
  begin
    Graph.Surface.Canvas.Draw(RX + 7, RY + 7, FImage);
    Graph.Render();
  end;

  procedure BmpFromTileset(Tile, Tileset: Graphics.TBitmap; Index: Integer);
  var
    Col, Row, ColCount, RowCount: Integer;
  begin
    if (Index < 0) then
      Index := 0;
    ColCount := Tileset.Width div 18;
    RowCount := Tileset.Height div 18;
    if (Index > (ColCount * RowCount) - 1) then
      Index := (ColCount * RowCount) - 1;
    Tile.Width := 18;
    Tile.Height := 18;
    Tile.PixelFormat := pf24bit;
    Col := Index mod ColCount;
    Row := Index div ColCount;
    Tile.Canvas.CopyRect(Bounds(0, 0, 18, 18), Tileset.Canvas,
      Bounds(Col * 18, Row * 18, 18, 18));
  end;

  procedure LoadBitmap(Index: Integer);
  begin
    FImage.Free;
    FImage := Graphics.TBitmap.Create;
    FImage.Width := 18;
    FImage.Height := 18;
    FImage.PixelFormat := pf24bit;
    BmpFromTileset(FImage, FTileset, Index);
    FImage.Transparent := True;
  end;

  procedure LoadShotBitmap();
  var
    Z, H: Integer;
  begin
    H := (FTileset.Height div 18) - 1;
    Z := Ord(ACreature.Prop.Projectile) - 1;
    if (Z < 0) then
      Z := 0;
    if (Z > H) then
      Z := H;
    if (FromX = ToX) then
    begin
      if (FromY > ToY) then
        LoadBitmap(Z * 10 + 0)
      else
        LoadBitmap(Z * 10 + 4);
      Exit;
    end;
    if (FromY = ToY) then
    begin
      if (FromX > ToX) then
        LoadBitmap(Z * 10 + 6)
      else
        LoadBitmap(Z * 10 + 2);
      Exit;
    end;
    if (FromY > ToY) then
    begin
      if (FromX > ToX) then
        LoadBitmap(Z * 10 + 7)
      else
        LoadBitmap(Z * 10 + 1);
    end
    else
    begin
      if (FromX > ToX) then
        LoadBitmap(Z * 10 + 5)
      else
        LoadBitmap(Z * 10 + 3);
    end;
  end;

begin
  RX := 0;
  RY := 0;
  if (ACreature = Creatures.PC) then
  begin
    FX1 := ToX;
    FY1 := ToY;
    FX2 := FromX;
    FY2 := FromY;
  end
  else
  begin
    FX1 := FromX;
    FY1 := FromY;
    FX2 := ToX;
    FY2 := ToY;
  end;
  FTileset := Graphics.TBitmap.Create;
  FTileset.Handle := Windows.LoadBitmap(hInstance, 'MISSILES');
  FBack := Graphics.TBitmap.Create;
  FBack.Assign(Graph.Surface);
  L := GetDist(FX1, FY1, FX2, FY2);
  LoadShotBitmap();
  for I := 1 to L do
  begin
    LR := (I / L);
    AX := (FX1 + Trunc((FX2 - FX1) * LR));
    AY := (FY1 + Trunc((FY2 - FY1) * LR));
    if (ACreature = Creatures.PC) then
    begin
      RX := ((ToX - AX + Graph.RW) * TileSize);
      RY := ((ToY - AY + Graph.RH) * TileSize) + Graph.CharHeight;
    end
    else
    begin
      RX := ((AX - ToX + Graph.RW) * TileSize);
      RY := ((AY - ToY + Graph.RH) * TileSize) + Graph.CharHeight;
    end;
    Graph.Surface.Assign(FBack);
    Render();
    Sleep(15);
    // if (I > ACreature.Prop.Radius) or
    // not Creatures.PC.FreeCell(AX, AY) then Break;
  end;
  Render();
  FImage.Free;
  FBack.Free;
  FTileset.Free;
end;

{ TAnimDamage }

constructor TAnimNumber.Create(Number, TX, TY: Integer);
const
  A = 0.2;
var
  DX, DY, I: Integer;
begin
  Graph.Surface.Canvas.Brush.Style := bsClear;
  Graph.Surface.Canvas.Font.Style := [fsBold];
  if (Number > 0) then
    Graph.Surface.Canvas.Font.Color := cLtGreen
  else
    Graph.Surface.Canvas.Font.Color := cLtRed;
  FBack := Graphics.TBitmap.Create;
  if (Number = 0) then
    Exit;
  try
    FBack.Assign(Graph.Surface);
    for I := 0 to 9 do
    begin
      DX := Round(Sin(I * A) * 2 * TileSize + I * 2) + TileSize;
      DY := Round(-2.0 * 2 * TileSize / 20 * I - TileSize);
      Graph.Surface.Assign(FBack);
      Graph.Surface.Canvas.TextOut(TX + DX, TY + TileSize + DY,
        IntToStr(Number));
      Graph.Render();
      Sleep(15);
    end;
  finally
    FBack.Free;
    Graph.Surface.Canvas.Font.Style := [];
  end;
end;

constructor TAnimNumber.Create(Number: Integer);
begin
  Create(Number, Graph.RW * TileSize, (Graph.RH * TileSize) + Graph.CharHeight);
end;

end.
