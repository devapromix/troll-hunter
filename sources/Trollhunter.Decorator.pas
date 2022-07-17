unit Trollhunter.Decorator;

interface

uses
  Graphics;

type
  TDecorType = (dtNone, dtBlood, dtBone, dtStone, dtWeb, dtSlime, dtTrap);

  TDecorSurf = (dsNone, dsFloor, dsWall, dsBoth);

  TDecoratorRec = record
    SpriteID, Light: Byte;
    DecorSurf: TDecorSurf;
  end;

const
  Decors: array [0 .. 99] of TDecoratorRec = ((SpriteID: 0; Light: 0;
    DecorSurf: dsFloor;), (SpriteID: 1; Light: 0; DecorSurf: dsBoth;
    ), (SpriteID: 2; Light: 0; DecorSurf: dsBoth;), (SpriteID: 3; Light: 0;
    DecorSurf: dsBoth;), (SpriteID: 4; Light: 0; DecorSurf: dsBoth;
    ), (SpriteID: 5; Light: 0; DecorSurf: dsFloor;), (SpriteID: 6; Light: 0;
    DecorSurf: dsFloor;), (SpriteID: 7; Light: 0; DecorSurf: dsFloor;
    ), (SpriteID: 8; Light: 0; DecorSurf: dsBoth;), (SpriteID: 9; Light: 0;
    DecorSurf: dsBoth;), (SpriteID: 10; Light: 0; DecorSurf: dsFloor;
    ), (SpriteID: 11; Light: 0; DecorSurf: dsFloor;), (SpriteID: 12; Light: 0;
    DecorSurf: dsFloor;), (SpriteID: 13; Light: 0; DecorSurf: dsFloor;
    ), (SpriteID: 14; Light: 0; DecorSurf: dsFloor;), (SpriteID: 15; Light: 0;
    DecorSurf: dsFloor;), (SpriteID: 16; Light: 0; DecorSurf: dsFloor;
    ), (SpriteID: 17; Light: 0; DecorSurf: dsFloor;), (SpriteID: 18; Light: 0;
    DecorSurf: dsWall;), (SpriteID: 19; Light: 0; DecorSurf: dsWall;
    ), (SpriteID: 20; Light: 5; DecorSurf: dsWall;), (SpriteID: 21; Light: 6;
    DecorSurf: dsWall;), (SpriteID: 22; Light: 7; DecorSurf: dsWall;
    ), (SpriteID: 23; Light: 6; DecorSurf: dsWall;), (SpriteID: 24; Light: 0;
    DecorSurf: dsWall;), (SpriteID: 25; Light: 0; DecorSurf: dsWall;
    ), (SpriteID: 26; Light: 0; DecorSurf: dsWall;), (SpriteID: 27; Light: 0;
    DecorSurf: dsWall;), (SpriteID: 28; Light: 3; DecorSurf: dsWall;
    ), (SpriteID: 29; Light: 0; DecorSurf: dsWall;), (SpriteID: 30; Light: 0;
    DecorSurf: dsWall;), (SpriteID: 31; Light: 0; DecorSurf: dsWall;
    ), (SpriteID: 32; Light: 0; DecorSurf: dsWall;), (SpriteID: 33; Light: 0;
    DecorSurf: dsWall;), (SpriteID: 34; Light: 0; DecorSurf: dsWall;
    ), (SpriteID: 35; Light: 0; DecorSurf: dsWall;), (SpriteID: 36; Light: 0;
    DecorSurf: dsFloor;), (SpriteID: 37; Light: 0; DecorSurf: dsFloor;
    ), (SpriteID: 38; Light: 0; DecorSurf: dsFloor;), (SpriteID: 39; Light: 4;
    DecorSurf: dsFloor;), (SpriteID: 40; Light: 0; DecorSurf: dsFloor;
    ), (SpriteID: 41; Light: 0; DecorSurf: dsFloor;), (SpriteID: 42; Light: 0;
    DecorSurf: dsFloor;), (SpriteID: 43; Light: 0; DecorSurf: dsFloor;
    ), (SpriteID: 44; Light: 0; DecorSurf: dsFloor;), (SpriteID: 45; Light: 0;
    DecorSurf: dsFloor;), (SpriteID: 46; Light: 0; DecorSurf: dsFloor;
    ), (SpriteID: 47; Light: 0; DecorSurf: dsFloor;), (SpriteID: 48; Light: 0;
    DecorSurf: dsFloor;), (SpriteID: 49; Light: 0; DecorSurf: dsFloor;
    ), (SpriteID: 50; Light: 0; DecorSurf: dsFloor;), (SpriteID: 51; Light: 0;
    DecorSurf: dsFloor;), (SpriteID: 52; Light: 0; DecorSurf: dsFloor;
    ), (SpriteID: 53; Light: 0; DecorSurf: dsFloor;), (SpriteID: 54; Light: 0;
    DecorSurf: dsFloor;), (SpriteID: 55; Light: 0; DecorSurf: dsFloor;
    ), (SpriteID: 56; Light: 0; DecorSurf: dsFloor;), (SpriteID: 57; Light: 0;
    DecorSurf: dsFloor;), (SpriteID: 58; Light: 0; DecorSurf: dsFloor;
    ), (SpriteID: 59; Light: 0; DecorSurf: dsFloor;), (SpriteID: 60; Light: 0;
    DecorSurf: dsFloor;), (SpriteID: 61; Light: 0; DecorSurf: dsFloor;
    ), (SpriteID: 62; Light: 0; DecorSurf: dsFloor;), (SpriteID: 63; Light: 0;
    DecorSurf: dsFloor;), (SpriteID: 64; Light: 0; DecorSurf: dsFloor;
    ), (SpriteID: 65; Light: 0; DecorSurf: dsFloor;), (SpriteID: 66; Light: 0;
    DecorSurf: dsFloor;), (SpriteID: 67; Light: 0; DecorSurf: dsFloor;
    ), (SpriteID: 68; Light: 0; DecorSurf: dsFloor;), (SpriteID: 69; Light: 0;
    DecorSurf: dsFloor;), (SpriteID: 70; Light: 0; DecorSurf: dsFloor;
    ), (SpriteID: 71; Light: 0; DecorSurf: dsFloor;), (SpriteID: 72; Light: 0;
    DecorSurf: dsFloor;), (SpriteID: 73; Light: 0; DecorSurf: dsFloor;
    ), (SpriteID: 74; Light: 0; DecorSurf: dsFloor;), (SpriteID: 75; Light: 0;
    DecorSurf: dsFloor;), (SpriteID: 76; Light: 0; DecorSurf: dsFloor;
    ), (SpriteID: 77; Light: 0; DecorSurf: dsFloor;), (SpriteID: 78; Light: 0;
    DecorSurf: dsFloor;), (SpriteID: 79; Light: 0; DecorSurf: dsFloor;
    ), (SpriteID: 80; Light: 0; DecorSurf: dsFloor;), (SpriteID: 81; Light: 0;
    DecorSurf: dsFloor;), (SpriteID: 82; Light: 0; DecorSurf: dsFloor;
    ), (SpriteID: 83; Light: 0; DecorSurf: dsFloor;), (SpriteID: 84; Light: 0;
    DecorSurf: dsFloor;), (SpriteID: 85; Light: 0; DecorSurf: dsFloor;
    ), (SpriteID: 86; Light: 0; DecorSurf: dsFloor;), (SpriteID: 87; Light: 0;
    DecorSurf: dsBoth;), (SpriteID: 88; Light: 0; DecorSurf: dsFloor;
    ), (SpriteID: 89; Light: 0; DecorSurf: dsFloor;), (SpriteID: 90; Light: 0;
    DecorSurf: dsFloor;), (SpriteID: 91; Light: 0; DecorSurf: dsFloor;
    ), (SpriteID: 92; Light: 0; DecorSurf: dsFloor;), (SpriteID: 93; Light: 0;
    DecorSurf: dsFloor;), (SpriteID: 94; Light: 0; DecorSurf: dsFloor;
    ), (SpriteID: 95; Light: 0; DecorSurf: dsFloor;), (SpriteID: 96; Light: 0;
    DecorSurf: dsFloor;), (SpriteID: 97; Light: 0; DecorSurf: dsFloor;
    ), (SpriteID: 98; Light: 0; DecorSurf: dsFloor;), (SpriteID: 99; Light: 0;
    DecorSurf: dsFloor;));

type
  TDecorators = class(TObject)
  private
    Image: array [0 .. High(Decors)] of Graphics.TBitmap;
  public
    constructor Create;
    destructor Destroy; override;
    function IsDecorSurf(I, X, Y: Integer): Boolean;
    procedure Insert(DT: TDecorType; X, Y: Integer);
    procedure Render(X, Y, DX, DY: Integer);
    procedure AddID(ID, X, Y: Integer);
    procedure Add(X, Y, Z: Integer);
  end;

function StrToDecorType(S: string): TDecorType;
function DecorTypeToStr(DT: TDecorType): string;

var
  Decorators: TDecorators;

const
  dWeb = 87;
  dSlimeMin = 80;
  dSlimeMax = 84;
  dTrap = 90;
  dTrapDet = 91;
  dTrapMin = 92;
  dTrapMag = 93;
  dTrapPsn = 94;
  dTrapSum = 97;
  dTrapWeb = 98;
  dTrapMax = 99;

implementation

uses
  SysUtils,
  Windows,
  Trollhunter.Graph,
  Trollhunter.Error,
  Trollhunter.Light,
  Trollhunter.Map,
  Trollhunter.Map.Tiles,
  Trollhunter.Utils;

{ TDecorators }

procedure TDecorators.Add(X, Y, Z: Integer);
var
  ID: Integer;
begin
  ID := Rand(0, High(Decors) - 9);
  if (Z = 1) then
    ID := dTrap;
  case ID of
    dSlimeMin .. dSlimeMax:
      Exit;
  end;
  if not Map.Info.Underground then
    case ID of
      48, 49, 58, 59, 68, 69, 78, 79:
        ;
    else
      Exit;
    end;
  if Decorators.IsDecorSurf(ID, X, Y) then
  begin
    Map.Cell[Y][X].Decor := ID;
    if (Decors[ID].Light > 0) then
      Light.Add(X, Y, ID);
  end;
end;

procedure TDecorators.AddID(ID, X, Y: Integer);
begin
  if (Map.Cell[Y][X].Tile in FloorSet) and (Map.Cell[Y][X].Decor < 0) and
    ((Decors[ID].DecorSurf = dsFloor) or (Decors[ID].DecorSurf = dsBoth)) then
    Map.Cell[Y][X].Decor := ID;
end;

constructor TDecorators.Create;
var
  I: Byte;
  Tileset: Graphics.TBitmap;
begin
  Tileset := Graphics.TBitmap.Create;
  Tileset.Handle := Windows.LoadBitmap(hInstance, 'DECORATORS');
  for I := 0 to High(Decors) do
  begin
    Image[I] := Graphics.TBitmap.Create;
    Graph.BitmapFromTileset(Image[I], Tileset, Decors[I].SpriteID);
    Image[I].Transparent := True;
  end;
  Tileset.Free;
end;

destructor TDecorators.Destroy;
var
  I: Byte;
begin
  for I := 0 to High(Decors) do
    Image[I].Free;
  inherited;
end;

function TDecorators.IsDecorSurf(I, X, Y: Integer): Boolean;
begin
  Result := (I >= 0) and ((Map.Cell[Y][X].Tile in FloorSet) and
    ((Decors[I].DecorSurf = dsFloor) or (Decors[I].DecorSurf = dsBoth)) or
    (Map.Cell[Y][X].Tile in [tlWall]) and ((Decors[I].DecorSurf = dsWall) or
    (Decors[I].DecorSurf = dsBoth)))
end;

procedure TDecorators.Render(X, Y, DX, DY: Integer);
begin
  try
    if IsDecorSurf(Map.Cell[Y][X].Decor, X, Y) then
      Graph.Surface.Canvas.Draw(DX, DY,
        Image[Decors[Map.Cell[Y][X].Decor].SpriteID]);
  except
    on E: Exception do
      Error.Add('Decorators.Render', E.Message);
  end;
end;

procedure TDecorators.Insert(DT: TDecorType; X, Y: Integer);
var
  ID: Byte;
begin
  case DT of
    dtBlood:
      begin
        AddID(Rand(0, 19), X, Y);
      end;
    dtBone:
      begin
        ID := Rand(43, 77);
        case ID of
          43, 45, 51, 57, 62, 63, 73 .. 77:
            AddID(ID, X, Y);
        end;
      end;
    dtTrap:
      begin
        AddID(Rand(dTrapMin, dTrapMax), X, Y);
      end;
    dtWeb:
      begin
        AddID(dWeb, X, Y);
      end;
    dtSlime:
      begin
        AddID(Rand(dSlimeMin, dSlimeMax), X, Y);
      end;
    dtStone:
      begin
        ID := Rand(4, 8);
        ID := ID * 10;
        ID := ID + Rand(0, 1) + 8;
        AddID(ID, X, Y);
      end;
  end;
end;

function StrToDecorType(S: string): TDecorType;
begin
  Result := dtNone;
  if (S = 'dtNone') or (S = '') then
    Exit;
  if (S = 'dtBlood') then
    Result := dtBlood;
  if (S = 'dtStone') then
    Result := dtStone;
  if (S = 'dtSlime') then
    Result := dtSlime;
  if (S = 'dtBone') then
    Result := dtBone;
  if (S = 'dtTrap') then
    Result := dtTrap;
  if (S = 'dtWeb') then
    Result := dtWeb;
end;

function DecorTypeToStr(DT: TDecorType): string;
begin
  Result := 'dtNone';
  case DT of
    dtBlood:
      Result := 'dtBlood';
    dtStone:
      Result := 'dtStone';
    dtSlime:
      Result := 'dtSlime';
    dtBone:
      Result := 'dtBone';
    dtTrap:
      Result := 'dtTrap';
    dtWeb:
      Result := 'dtWeb';
  end;
end;

end.
