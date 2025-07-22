unit uMap;

interface

uses
  Trollhunter.Types;

type
  TMapEnum = (deDark_Wood, deGray_Cave, deDeep_Cave, deBlood_Cave, deDrom);

const
  FinalDungeon = deDrom;

type
  TTile = record
    Symbol: Char;
    Name: string;
    Color: Cardinal;
  end;

type
  TTileEnum = (teDefaultFloor, teDefaultWall, teRock, teFloor1, teFloor2,
    teFloor3, teUpStairs, teDnStairs, teWater, teStoneWall, teWoodenWall,
    teStoneFloor, teWoodenFloor, teDoor, teGate, tePortal, teTownPortal);

const
  StopTiles = [teDefaultWall, teStoneWall, teWoodenWall];
  FreeTiles = [teDefaultFloor, teRock, teFloor1, teFloor2, teFloor3, teUpStairs,
    teDnStairs, teWater];
  VillageTiles = [teStoneWall, teWoodenWall, teStoneFloor, teWoodenFloor,
    teDoor, teGate];
  SpawnTiles = [teDefaultFloor, teRock, teFloor1, teFloor2, teFloor3, teWater];

var
  Tile: array [TTileEnum, TMapEnum] of TTile;

type
  MapSize = System.Byte;

type
  TMap = class(TObject)
  private
    FCurrent: TMapEnum;
    FMapName: array [TMapEnum] of string;
    FVis: array [TMapEnum] of Boolean;
    FMap: array [MapSize, MapSize, TMapEnum] of TTileEnum;
    FFog: array [MapSize, MapSize, TMapEnum] of Boolean;
    FFOV: array [MapSize, MapSize] of Boolean;
    procedure AddSpot(AX, AY: UInt; ASize: UInt; AZ: TMapEnum;
      ABaseTileEnum, ATileEnum: TTileEnum);
    procedure AddTiles(AX, AY: UInt; AZ: TMapEnum; AType: UInt; ADen: UInt;
      ABaseTileEnum, ATileEnum: TTileEnum);
    procedure AddTile(ASymbol: Char; AName: string; AColor: Cardinal;
      ATile: TTileEnum; AZ: TMapEnum);
    procedure InitTiles;
    function GetName: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetVis(const AZ: TMapEnum; const Value: Boolean);
    function GetVis(const AZ: TMapEnum): Boolean;
    procedure Clear(Z: TMapEnum; ATileEnum: TTileEnum);
    procedure Gen;
    property Current: TMapEnum read FCurrent write FCurrent;
    function InMap(AX, AY: Int): Boolean;
    function InView(AX, AY: Int): Boolean;
    function GetFog(AX, AY: UInt): Boolean;
    procedure SetFog(AX, AY: UInt; AFlag: Boolean);
    procedure ClearFOV;
    function GetFOV(AX, AY: UInt): Boolean;
    procedure SetFOV(AX, AY: UInt; AFlag: Boolean);
    function GetTile(AX, AY: UInt): TTile; overload;
    function GetTile(ATileEnum: TTileEnum): TTile; overload;
    procedure SetTileEnum(AX, AY: UInt; AZ: TMapEnum; ATileEnum: TTileEnum);
    function GetTileEnum(AX, AY: UInt; AZ: TMapEnum): TTileEnum;
    property Name: string read GetName;
    function EnsureRange(Value: Int): UInt;
  end;

var
  IsBoss: Boolean = False;
  IsRare: Boolean = False;

var
  Map: TMap = nil;

implementation

uses
  SysUtils,
  Math,
  Types,
  TypInfo,
  Trollhunter.Player,
  uMob,
  uItem,
  uLanguage,
  Trollhunter.Terminal,
  Trollhunter.Game,
  uCreature,
  uAttribute,
  uScenes,
  Trollhunter.Helpers;

{ TMap }

procedure TMap.InitTiles;
begin
  // DefaultFloor
  AddTile('"', _('Grass'), $FF113311, teDefaultFloor, deDark_Wood);
  AddTile(':', _('Dirt'), $FF331133, teDefaultFloor, deGray_Cave);
  AddTile('.', _('Stone'), $FF222111, teDefaultFloor, deDeep_Cave);
  AddTile(';', _('Stone'), $FF330000, teDefaultFloor, deBlood_Cave);
  AddTile('~', _('Stone'), $FF002200, teDefaultFloor, deDrom);
  // DefaultWall
  AddTile('T', _('Tree'), $FF006622, teDefaultWall, deDark_Wood);
  AddTile('#', _('Wall'), $FF444422, teDefaultWall, deGray_Cave);
  AddTile('#', _('Wall'), $FF222133, teDefaultWall, deDeep_Cave);
  AddTile('#', _('Wall'), $FF322118, teDefaultWall, deBlood_Cave);
  AddTile('#', _('Wall'), $FF112211, teDefaultWall, deDrom);
  // Rock
  AddTile('^', _('Rock'), $FF556655, teRock, deDark_Wood);
  AddTile(':', _('Wall'), $FF444422, teRock, deGray_Cave);
  AddTile(':', _('Wall'), $FF222133, teRock, deDeep_Cave);
  AddTile(':', _('Wall'), $FF322118, teRock, deBlood_Cave);
  AddTile(':', _('Wall'), $FF112233, teRock, deDrom);
  // Floor #1
  AddTile('"', _('Grass'), $FF446644, teFloor1, deDark_Wood);
  AddTile('"', _('Grass'), $FF99AA99, teFloor1, deGray_Cave);
  AddTile('.', _('Stone'), $FF224422, teFloor1, deDeep_Cave);
  AddTile(';', _('Stone'), $FF225533, teFloor1, deBlood_Cave);
  AddTile('~', _('Stone'), $FF228833, teFloor1, deDrom);
  // Floor #2
  AddTile('"', _('Grass'), $FF447755, teFloor2, deDark_Wood);
  AddTile('"', _('Grass'), $FF779977, teFloor2, deGray_Cave);
  AddTile('.', _('Stone'), $FF22CC44, teFloor2, deDeep_Cave);
  AddTile(';', _('Stone'), $FF333322, teFloor2, deBlood_Cave);
  AddTile('~', _('Stone'), $FF334422, teFloor2, deDrom);
  // Floor #3
  AddTile('"', _('Grass'), $FF778866, teFloor3, deDark_Wood);
  AddTile('"', _('Grass'), $FF668866, teFloor3, deGray_Cave);
  AddTile('.', _('Stone'), $FF338855, teFloor3, deDeep_Cave);
  AddTile(';', _('Stone'), $FF223333, teFloor3, deBlood_Cave);
  AddTile('~', _('Stone'), $FF226622, teFloor3, deDrom);
  // Up Stairs
  AddTile('*', _('Stairs'), $FFFFFF00, teUpStairs, deDark_Wood);
  AddTile('<', _('Stairs'), $FFEEEE00, teUpStairs, deGray_Cave);
  AddTile('<', _('Stairs'), $FFDDDD00, teUpStairs, deDeep_Cave);
  AddTile('<', _('Stairs'), $FFCCCC00, teUpStairs, deBlood_Cave);
  AddTile('<', _('Stairs'), $FFBBBB00, teUpStairs, deDrom);
  // Down Stairs
  AddTile('*', _('Stairs'), $FFFFFF00, teDnStairs, deDark_Wood);
  AddTile('>', _('Stairs'), $FFEEEE00, teDnStairs, deGray_Cave);
  AddTile('>', _('Stairs'), $FFDDDD00, teDnStairs, deDeep_Cave);
  AddTile('>', _('Stairs'), $FFCCCC00, teDnStairs, deBlood_Cave);
  AddTile('>', _('Stairs'), $FFBBBB00, teDnStairs, deDrom);
  // Water
  AddTile('=', _('Water'), $FF333388, teWater, deDark_Wood);
  AddTile('=', _('Water'), $FF333377, teWater, deGray_Cave);
  AddTile('=', _('Water'), $FF222266, teWater, deDeep_Cave);
  AddTile('=', _('Water'), $FF222255, teWater, deBlood_Cave);
  AddTile('=', _('Water'), $FF222244, teWater, deDrom);
  // Stone Wall
  AddTile('#', _('Stone Wall'), $FF818F95, teStoneWall, deDark_Wood);
  AddTile('#', _('Stone Wall'), $FF818F95, teStoneWall, deGray_Cave);
  AddTile('#', _('Stone Wall'), $FF818F95, teStoneWall, deDeep_Cave);
  AddTile('#', _('Stone Wall'), $FF818F95, teStoneWall, deBlood_Cave);
  AddTile('#', _('Stone Wall'), $FF818F95, teStoneWall, deDrom);
  // _Wooden Wall
  AddTile('#', _('Wooden Wall'), $FF776735, teWoodenWall, deDark_Wood);
  AddTile('#', _('Wooden Wall'), $FF776735, teWoodenWall, deGray_Cave);
  AddTile('#', _('Wooden Wall'), $FF776735, teWoodenWall, deDeep_Cave);
  AddTile('#', _('Wooden Wall'), $FF776735, teWoodenWall, deBlood_Cave);
  AddTile('#', _('Wooden Wall'), $FF776735, teWoodenWall, deDrom);
  // Stone Floor
  AddTile('.', _('Stone Floor'), $FF818F95, teStoneFloor, deDark_Wood);
  AddTile('.', _('Stone Floor'), $FF818F95, teStoneFloor, deGray_Cave);
  AddTile('.', _('Stone Floor'), $FF818F95, teStoneFloor, deDeep_Cave);
  AddTile('.', _('Stone Floor'), $FF818F95, teStoneFloor, deBlood_Cave);
  AddTile('.', _('Stone Floor'), $FF818F95, teStoneFloor, deDrom);
  // _Wooden Floor
  AddTile('.', _('Wooden Floor'), $FF776735, teWoodenFloor, deDark_Wood);
  AddTile('.', _('Wooden Floor'), $FF776735, teWoodenFloor, deGray_Cave);
  AddTile('.', _('Wooden Floor'), $FF776735, teWoodenFloor, deDeep_Cave);
  AddTile('.', _('Wooden Floor'), $FF776735, teWoodenFloor, deBlood_Cave);
  AddTile('.', _('Wooden Floor'), $FF776735, teWoodenFloor, deDrom);
  // Door
  AddTile('+', _('Door'), $FF675725, teDoor, deDark_Wood);
  AddTile('+', _('Door'), $FF675725, teDoor, deGray_Cave);
  AddTile('+', _('Door'), $FF675725, teDoor, deDeep_Cave);
  AddTile('+', _('Door'), $FF675725, teDoor, deBlood_Cave);
  AddTile('+', _('Door'), $FF675725, teDoor, deDrom);
  // Gate
  AddTile('+', _('Gate'), $FF515F55, teGate, deDark_Wood);
  AddTile('+', _('Gate'), $FF515F55, teGate, deGray_Cave);
  AddTile('+', _('Gate'), $FF515F55, teGate, deDeep_Cave);
  AddTile('+', _('Gate'), $FF515F55, teGate, deBlood_Cave);
  AddTile('+', _('Gate'), $FF515F55, teGate, deDrom);
  // Portal
  AddTile('O', _('Portal'), $FF9999FF, tePortal, deDark_Wood);
  AddTile('O', _('Portal'), $FF9999FF, tePortal, deGray_Cave);
  AddTile('O', _('Portal'), $FF9999FF, tePortal, deDeep_Cave);
  AddTile('O', _('Portal'), $FF9999FF, tePortal, deBlood_Cave);
  AddTile('O', _('Portal'), $FF9999FF, tePortal, deDrom);
  // Town Portal
  AddTile('O', _('Portal'), $FF9999FF, teTownPortal, deDark_Wood);
  AddTile('O', _('Portal'), $FF9999FF, teTownPortal, deGray_Cave);
  AddTile('O', _('Portal'), $FF9999FF, teTownPortal, deDeep_Cave);
  AddTile('O', _('Portal'), $FF9999FF, teTownPortal, deBlood_Cave);
  AddTile('O', _('Portal'), $FF9999FF, teTownPortal, deDrom);
end;

procedure TMap.AddSpot(AX, AY: UInt; ASize: UInt; AZ: TMapEnum;
  ABaseTileEnum, ATileEnum: TTileEnum);
var
  Z: TMapEnum;
  I, X, Y: UInt;
begin
  X := AX;
  Y := AY;
  Z := AZ;
  ASize := Math.EnsureRange(ASize, 49, 9999);
  for I := 0 to ASize do
  begin
    if (Round(Random(6)) = 1) and (X > 0) then
    begin
      X := X - 1;
      if (GetTileEnum(X, Y, Z) <> ABaseTileEnum) then
        Continue;
      SetTileEnum(X, Y, Z, ATileEnum);
    end;
    if (Round(Random(6)) = 1) and (X < UIntMax) then
    begin
      X := X + 1;
      if (GetTileEnum(X, Y, Z) <> ABaseTileEnum) then
        Continue;
      SetTileEnum(X, Y, Z, ATileEnum);
    end;
    if (Round(Random(6)) = 1) and (Y > 0) then
    begin
      Y := Y - 1;
      if (GetTileEnum(X, Y, Z) <> ABaseTileEnum) then
        Continue;
      SetTileEnum(X, Y, Z, ATileEnum);
    end;
    if (Round(Random(6)) = 1) and (Y < UIntMax) then
    begin
      Y := Y + 1;
      if (GetTileEnum(X, Y, Z) <> ABaseTileEnum) then
        Continue;
      SetTileEnum(X, Y, Z, ATileEnum);
    end;
  end;
end;

procedure TMap.AddTile(ASymbol: Char; AName: string; AColor: Cardinal;
  ATile: TTileEnum; AZ: TMapEnum);
begin
  with Tile[ATile, AZ] do
  begin
    Symbol := ASymbol;
    Name := AName;
    Color := AColor;
  end;
end;

procedure TMap.AddTiles(AX, AY: UInt; AZ: TMapEnum; AType: UInt; ADen: UInt;
  ABaseTileEnum, ATileEnum: TTileEnum);
var
  K, X, Y: UInt;
  Z: TMapEnum;

  procedure ModTile(const X, Y: UInt);
  begin
    if (GetTileEnum(X, Y, Z) = ABaseTileEnum) then
      SetTileEnum(X, Y, Z, ATileEnum);
  end;

begin
  X := AX;
  Y := AY;
  Z := AZ;
  AType := Math.EnsureRange(AType, 2, 9);
  for K := 0 to ADen do
  begin
    if (Round(Random(AType)) = 1) and (X > 0) then
    begin
      X := X - 1;
      ModTile(X, Y);
    end;
    if (Round(Random(AType)) = 1) and (X < UIntMax) then
    begin
      X := X + 1;
      ModTile(X, Y);
    end;
    if (Round(Random(AType)) = 1) and (Y > 0) then
    begin
      Y := Y - 1;
      ModTile(X, Y);
    end;
    if (Round(Random(AType)) = 1) and (Y < UIntMax) then
    begin
      Y := Y + 1;
      ModTile(X, Y);
    end;
  end;
end;

procedure TMap.ClearFOV;
var
  X, Y: Int;
  Vision: UInt;
begin
  Vision := Player.Attributes.Attrib[atVision].Value.InRange(VisionMax);
  for Y := Player.Y - Vision to Player.Y + Vision do
    for X := Player.X - Vision to Player.X + Vision do
      FFOV[Self.EnsureRange(X)][Self.EnsureRange(Y)] := False;
end;

procedure TMap.Clear(Z: TMapEnum; ATileEnum: TTileEnum);
var
  X, Y: UInt;
begin
  for Y := 0 to UIntMax do
    for X := 0 to UIntMax do
    begin
      FMap[X][Y][Z] := ATileEnum;
      FFog[X][Y][Z] := True;
    end;
end;

constructor TMap.Create;
var
  I: TMapEnum;
  P: Pointer;
begin
  Self.Current := deDark_Wood;
  P := TypeInfo(TMapEnum);
  for I := Low(TMapEnum) to High(TMapEnum) do
    FMapName[I] := GetEnumName(P, Ord(I)).GetName('de');
end;

destructor TMap.Destroy;
begin

  inherited;
end;

function TMap.EnsureRange(Value: Int): UInt;
begin
  Result := Value.InRange(UIntMax);
end;

var
  BNPC: array [0 .. 6] of Boolean;

procedure TMap.Gen;
var
  GatePos: TPoint;
  I, X, Y: UInt;
  Z: TMapEnum;

const
  Pd = 11;

  procedure GenCave(D: UInt; C, V: UInt);
  var
    I: UInt;
  begin
    for I := 0 to C do
    begin
      repeat
        X := Math.RandomRange(Pd, UIntMax - Pd);
        Y := Math.RandomRange(Pd, UIntMax - Pd);
      until (GetTileEnum(X, Y, pred(Z)) = teDefaultFloor);
      Self.AddTiles(X, Y, Z, D, V, teDefaultWall, teDefaultFloor);
      SetTileEnum(X, Y, pred(Z), teDnStairs);
      SetTileEnum(X, Y, Z, teUpStairs);
    end;
  end;

  procedure AddArea(ADeep: TMapEnum; ABaseTileEnum, ATileEnum: TTileEnum);
  var
    X, Y: UInt;
  begin
    repeat
      X := Math.RandomRange(Pd, UIntMax - Pd);
      Y := Math.RandomRange(Pd, UIntMax - Pd);
    until (GetTileEnum(X, Y, ADeep) = ABaseTileEnum);
    AddSpot(X, Y, Math.RandomRange(49, UIntMax), ADeep, ABaseTileEnum,
      ATileEnum);
  end;

  procedure AddFrame(AX, AY, AW, AH: UInt; ABaseTileEnum: TTileEnum);
  var
    X, Y: UInt;
    PX, PY: UInt;
  begin
    PX := AX - (AW div 2);
    PY := AY - (AH div 2);
    for X := PX to PX + AW do
      for Y := PY to PY + AH do
        if not(((X > PX) and (X < (PX + AW))) and ((Y > PY) and (Y < (PY + AH))))
        then
          SetTileEnum(X, Y, Z, ABaseTileEnum);
  end;

  procedure AddRect(AX, AY, AW, AH: UInt;
    AFloorTileEnum, AWallTileEnum: TTileEnum; IsFog: Boolean = False);
  var
    X, Y: UInt;
    PX, PY: UInt;
  begin
    PX := AX - (AW div 2);
    PY := AY - (AH div 2);
    for X := PX to PX + AW do
      for Y := PY to PY + AH do
      begin
        if IsFog then
          Self.SetFog(X, Y, False);
        if (((X > PX) and (X < (PX + AW))) and ((Y > PY) and (Y < (PY + AH))))
        then
          SetTileEnum(X, Y, Z, AFloorTileEnum)
        else
          SetTileEnum(X, Y, Z, AWallTileEnum);
      end;
  end;

  procedure AddNPC(AX, AY: UInt);
  var
    I: UInt;
  begin
    repeat
      I := Math.RandomRange(0, 7);
    until not BNPC[I];
    Mobs.Add(Self.Current, AX, AY, fcNPC, Ord(mbEldan_2the_magic_trader3) + I);
    BNPC[I] := True;
  end;

  procedure AddHouse(AX, AY, CX, CY, D: UInt; AV: Boolean; F: Boolean);
  var
    W, H: UInt;
    IsDoor: Boolean;

    procedure AddDoor(X, Y: UInt);
    begin
      if IsDoor then
        Exit;
      SetTileEnum(X, Y, Z, teDoor);
      IsDoor := True;
      AddNPC(AX, AY);
    end;

  begin
    IsDoor := False;
    W := IfThen(AV, 8, RandomRange(2, 5) * 2);
    H := IfThen(AV, 8, RandomRange(2, 5) * 2);
    AddRect(AX, AY, W, H, teWoodenFloor, teWoodenWall);
    // Add door
    if AV then
    begin
      case D of
        4:
          AddDoor(AX, AY - (H div 2));
        5:
          AddDoor(AX + (H div 2), AY);
        6:
          AddDoor(AX - (H div 2), AY);
        7:
          AddDoor(AX, AY + (H div 2));
      end;
      Exit;
    end;
    if F then
      if (AX <= CX) then
        AddDoor(AX + (W div 2), AY)
      else
        AddDoor(AX - (W div 2), AY)
    else if (AY <= CY) then
      AddDoor(AX, AY + (H div 2))
    else
      AddDoor(AX, AY - (H div 2));
  end;

  procedure AddVillage(AX, AY: UInt);
  var
    I, J, T, X, Y, PX, PY: UInt;
    HP: array [0 .. 7] of Boolean;
  const
    House: array [0 .. 7] of TPoint = ((X: - 10; Y: - 10;), (X: 10; Y: - 10;
      ), (X: - 10; Y: 10;), (X: 10; Y: 10;), (X: 0; Y: 10;), (X: - 10; Y: 0;
      ), (X: 10; Y: 0;), (X: 0; Y: - 10;));

    procedure AddGate(AX, AY: UInt; SX, SY: ShortInt);
    begin
      SetTileEnum(AX + SX, AY + SY, Z, teGate);
      GatePos := Point(AX + SX, AY + SY);
      if (SX = 0) then
      begin
        SetTileEnum(AX + 1, AY + SY, Z, teGate);
        SetTileEnum(AX - 1, AY + SY, Z, teGate);
      end;
      if (SY = 0) then
      begin
        SetTileEnum(AX + SX, AY + 1, Z, teGate);
        SetTileEnum(AX + SX, AY - 1, Z, teGate);
      end;
    end;

  begin
    // Save to log
    Game.Log(Format('Village: %dx%d', [AX, AY]));
    Player.X := AX;
    Player.Y := AY;
    //
    AddFrame(AX, AY, 34, 34, teDefaultFloor);
    AddRect(AX, AY, 32, 32, teStoneFloor, teStoneWall, True);
    for I := 0 to High(House) do
      HP[I] := False;
    // Add gate
    J := Math.RandomRange(4, 8);
    case J of
      4:
        AddGate(AX, AY, 0, -16);
      5:
        AddGate(AX, AY, 16, 0);
      6:
        AddGate(AX, AY, -16, 0);
      7:
        AddGate(AX, AY, 0, 16);
    end;
    PX := AX - House[J].X;
    PY := AY - House[J].Y;
    AddRect(PX, PY, 10, 10, teStoneFloor, teStoneFloor);
    HP[J] := True;
    // Add houses
    T := 0;
    while (T < High(House)) do
    begin
      I := Math.RandomRange(0, 8);
      X := AX - House[I].X;
      Y := AY - House[I].Y;
      if not HP[I] then
      begin
        AddHouse(X, Y, AX, AY, J, I = (10 - J + 1), (J = 4) or (J = 7));
        HP[I] := True;
        Inc(T);
      end;
    end;
  end;

begin
  for I := 0 to 6 do
    BNPC[I] := False;
  InitTiles();
  for Z := Low(TMapEnum) to High(TMapEnum) do
  begin
    Self.SetVis(Z, False);
    case Z of
      deDark_Wood:
        begin
          Self.SetVis(Z, True);
          Self.Clear(Z, teDefaultFloor);
          for I := 0 to 9999 do
            Self.SetTileEnum(Math.RandomRange(0, UIntMax),
              Math.RandomRange(0, UIntMax), Z, teDefaultWall);
          Game.Spawn.X := RandomRange(25, UIntMax - 25);
          Game.Spawn.Y := RandomRange(25, UIntMax - 25);
          Game.Portal.X := Game.Spawn.X;
          Game.Portal.Y := Game.Spawn.Y;
          AddVillage(Game.Spawn.X, Game.Spawn.Y);
        end;
      deGray_Cave:
        begin
          Self.Clear(Z, teDefaultWall);
          GenCave(9, 49, 4999);
        end;
      deDeep_Cave:
        begin
          Self.Clear(Z, teDefaultWall);
          GenCave(6, 39, 3999);
        end;
      deBlood_Cave:
        begin
          Self.Clear(Z, teDefaultWall);
          GenCave(3, 29, 2999);
        end;
      deDrom:
        begin
          Self.Clear(Z, teDefaultWall);
          GenCave(2, 19, 1999);
        end;
    end;
    for I := 0 to 9 do
      AddArea(Z, teDefaultFloor, teWater);
    for I := 0 to 19 do
      AddArea(Z, teDefaultFloor, teRock);
    for I := 0 to 29 do
      AddArea(Z, teDefaultFloor, teFloor1);
    for I := 0 to 39 do
      AddArea(Z, teDefaultFloor, teFloor2);
    for I := 0 to 49 do
      AddArea(Z, teDefaultFloor, teFloor3);
  end;

  for Z := Low(TMapEnum) to High(TMapEnum) do
  begin
    // Add mobs
    IsBoss := False;
    for I := 0 to UIntMax do
      Mobs.AddGroup(Z);
  end;
end;

function TMap.GetTile(ATileEnum: TTileEnum): TTile;
begin
  Result := Tile[ATileEnum][Current];
end;

function TMap.GetTile(AX, AY: UInt): TTile;
begin
  Result := Tile[FMap[AX][AY][Current]][Current];
end;

function TMap.GetName: string;
begin
  if (GetTileEnum(Player.X, Player.Y, Current) in VillageTiles) then
  begin
    case Current of
      deDark_Wood:
        Result := _('Village Dork');
    end;
    Exit;
  end;
  Result := _(FMapName[Current]);
end;

function TMap.GetTileEnum(AX, AY: UInt; AZ: TMapEnum): TTileEnum;
begin
  Result := FMap[AX][AY][AZ];
end;

function TMap.GetVis(const AZ: TMapEnum): Boolean;
begin
  Result := FVis[AZ];
end;

procedure TMap.SetTileEnum(AX, AY: UInt; AZ: TMapEnum; ATileEnum: TTileEnum);
begin
  FMap[AX][AY][AZ] := ATileEnum;
end;

function TMap.GetFog(AX, AY: UInt): Boolean;
begin
  Result := FFog[AX][AY][Current];
end;

procedure TMap.SetFog(AX, AY: UInt; AFlag: Boolean);
begin
  FFog[AX][AY][Current] := AFlag;
end;

function TMap.InMap(AX, AY: Int): Boolean;
begin
  Result := (AX >= 0) and (AY >= 0) and (AX <= UIntMax) and (AY <= UIntMax)
end;

function TMap.InView(AX, AY: Int): Boolean;
var
  PX, PY: Int;
begin
  PX := View.Width div 2;
  PY := View.Height div 2;
  Result := (AX >= Player.X - PX) and (AY >= Player.Y - PY) and
    (AX <= Player.X + PX - 1) and (AY <= Player.Y + PY - 1);
end;

function TMap.GetFOV(AX, AY: UInt): Boolean;
begin
  Result := FFOV[AX][AY];
end;

procedure TMap.SetFOV(AX, AY: UInt; AFlag: Boolean);
begin
  FFOV[AX][AY] := AFlag;
end;

procedure TMap.SetVis(const AZ: TMapEnum; const Value: Boolean);
begin
  FVis[AZ] := Value;
end;

initialization

Map := TMap.Create;

finalization

FreeAndNil(Map);

end.
