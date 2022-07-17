unit Trollhunter.MiniMap;

interface

uses
  Graphics,
  Trollhunter.CustomMap;

type
  TMiniMap = class(TCustomMap)
  private
    Surface: Graphics.TBitmap;
    procedure RenderCell(X, Y: Integer);
    procedure PSet(X, Y: Integer); overload;
    procedure PSet(X, Y, AColor: Integer); overload;
  public
    procedure Make;
    procedure Clear; override;
    procedure Render; override;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  Types,
  Trollhunter.Map,
  Trollhunter.Map.Tiles,
  Trollhunter.Creatures,
  Trollhunter.Utils,
  Trollhunter.Color,
  Trollhunter.Graph;

{ TMiniMap }

procedure TMiniMap.Clear;
begin
  Surface.Canvas.Brush.Color := 0;
  Surface.Canvas.FillRect(Rect(0, 0, Surface.Width, Surface.Height));
end;

constructor TMiniMap.Create;
begin
  inherited;
  Surface := Graphics.TBitmap.Create;
  Surface.Width := Width * 2;
  Surface.Height := Height * 2;
end;

destructor TMiniMap.Destroy;
begin
  Surface.Free;
  inherited;
end;

procedure TMiniMap.Make;
var
  X, Y: Integer;
begin
  with Surface.Canvas do
    for X := 0 to Width - 1 do
      for Y := 0 to Height - 1 do
        RenderCell(X, Y);
end;

procedure TMiniMap.PSet(X, Y, AColor: Integer);
begin
  Surface.Canvas.Pixels[X * 2, Y * 2] := AColor;
  Surface.Canvas.Pixels[X * 2 + 1, Y * 2] := AColor;
  Surface.Canvas.Pixels[X * 2, Y * 2 + 1] := AColor;
  Surface.Canvas.Pixels[X * 2 + 1, Y * 2 + 1] := AColor;
end;

procedure TMiniMap.PSet(X, Y: Integer);
begin
  PSet(X, Y, Tile[Map.Cell[Y][X].Tile].Color);
end;

procedure TMiniMap.Render;
var
  J, X, Y: Integer;
begin
  begin
    if ParamTest then
    begin
      for X := 0 to Width - 1 do
        for Y := 0 to Height - 1 do
          PSet(X, Y);
    end
    else
    begin
      for X := Creatures.PC.Pos.X - 7 to Creatures.PC.Pos.X + 7 do
        for Y := Creatures.PC.Pos.Y - 7 to Creatures.PC.Pos.Y + 7 do
          RenderCell(X, Y);
    end;
  end;

  // Show all enemies on map. "Глаз Чародея".
  with Creatures do
    if PC.TempSys.IsVar('WizardEye') then
      for J := 0 to High(Enemy) do
        if not Enemy[J].Life.IsMin then
          if (GetDist(PC.Pos.X, PC.Pos.Y, Enemy[J].Pos.X, Enemy[J].Pos.Y) <=
            PC.TempSys.Power('WizardEye')) then
            PSet(Enemy[J].Pos.X, Enemy[J].Pos.Y, clRed);

  PSet(Creatures.PC.Pos.X, Creatures.PC.Pos.Y, cWhite);
  Graph.Surface.Canvas.Draw(Graph.DL, Graph.CharHeight * 6, Surface);
end;

procedure TMiniMap.RenderCell(X, Y: Integer);
begin
  if IsValidCell(X, Y) and Map.Cell[Y, X].Viz then
    PSet(X, Y);
end;

end.
