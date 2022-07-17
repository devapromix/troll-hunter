unit Trollhunter.Light;

interface

const
  LightMin = 0.5;

type
  TLight = class(TObject)
  private
    procedure Make(AX, AY, AR: Integer);
  public
    procedure Render(X, Y, DX, DY: Integer);
    procedure Add(FX, FY, FD: Integer);
    procedure Clear();
  end;

var
  Light: TLight;

implementation

uses
  Graphics,
  SysUtils,
  Types,
  Trollhunter.Utils,
  Trollhunter.Graph,
  Trollhunter.Creatures,
  Trollhunter.Decorator,
  Trollhunter.Map,
  Trollhunter.Map.Tiles;

procedure TLight.Render(X, Y, DX, DY: Integer);
var
  B: Graphics.TBitmap;
  D, H: Double;
begin
  if (Creatures.PC.Pos.X = X) and (Creatures.PC.Pos.Y = Y) then
    Exit;
  B := Graphics.TBitmap.Create;
  B.Width := TileSize;
  B.Height := TileSize;
  B.Canvas.CopyRect(Bounds(0, 0, TileSize, TileSize), Graph.Surface.Canvas,
    Bounds(DX, DY, TileSize, TileSize));
  D := Map.Cell[Y][X].Light / 10;
  H := 1 - (GetDist(Creatures.PC.Pos.X, Creatures.PC.Pos.Y, X, Y) / 10);
  if (D < H) then
    D := H;
  if (D < LightMin) then
    D := LightMin;
  Gamma(B, D);
  Graph.Surface.Canvas.Draw(DX, DY, B);
  B.Free;
  /// //
  // Graph.Surface.Canvas.Brush.Style := bsClear;
  // Graph.Surface.Canvas.Font.Size := 11;
  // Graph.Surface.Canvas.TextOut(DX, DY, IntToStr(Map.Cell[Y][X].Light));
  // Graph.Surface.Canvas.TextOut(DX, DY, IntToStr(Round(D * 10)));
end;

procedure TLight.Clear;
var
  X, Y: Word;
begin
  for Y := 0 to Map.Height - 1 do
    for X := 0 to Map.Width do
      Map.Cell[Y][X].Light := 0;
end;

procedure TLight.Make(AX, AY, AR: Integer);
const
  Max = 9;
var
  X, Y, D: Integer;
begin
  if (AR < 0) then
    AR := 0;
  if (AR > Max) then
    AR := Max;
  for X := AX - AR to AX + AR do
    for Y := AY - AR to AY + AR do
      with Map do
      begin
        if not isValidCell(X, Y) then
          Exit;
        LineFOV2(AX, AY, X, Y, False);
        if Cell[Y][X].FOV then
        begin
          D := Max - GetDist(AX, AY, X, Y);
          if (D < Max - AR) then
            D := Max - AR;
          if (D < Round(LightMin * 10)) then
            D := Round(LightMin * 10);
          if (Cell[Y][X].Light < D) then
            Cell[Y][X].Light := D;
        end;
      end;
end;

procedure TLight.Add(FX, FY, FD: Integer);
var
  X, Y: Integer;
begin
  if (Decors[FD].Light > 0) then
    for Y := FY - 1 to FY + 1 do
      for X := FX - 1 to FX + 1 do
        if (Map.Cell[Y][X].Tile in FloorSet) or ((Y = FY) and (X = FX)) then
          Make(X, Y, Decors[FD].Light);
end;

initialization

Light := TLight.Create;

finalization

Light.Free;

end.
