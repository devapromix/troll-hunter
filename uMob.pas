unit uMob;

interface

type
  TMob = class(TObject)
    X, Y: Integer;
    Alive: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure AddRandom;
    procedure Process;
    procedure Render(AX, AY: Byte);
  end;

type
  TMobs = class(TObject)
    FMob: array of TMob;
    constructor Create();
    destructor Destroy; override;
    procedure Add;
    function Count: Integer;
    procedure Process;
    procedure Render(AX, AY: Byte);
  end;

type
  TGetXYVal = function(X, Y: integer): boolean;stdcall;

var
  Mobs: TMobs = nil;

implementation

uses Math, uTerminal, uMap, uPlayer, uCommon;

function DoAStar(MapX, MapY, FromX, FromY, ToX, ToY: Integer; Callback: TGetXYVal; var TargetX, TargetY: integer): boolean;external 'BeaRLibPF.dll';

function MyCallback(X, Y: Integer): Boolean; stdcall;
begin
  Result := Map.GetTileEnum(X, Y, Map.Deep) in FreeTiles;
end;

{ TMob }

procedure TMob.AddRandom;
begin
  Create;
  repeat
    X := Math.RandomRange(0, High(Byte));
    Y := Math.RandomRange(0, High(Byte));
  until (Map.GetTileEnum(X, Y, Map.Deep) in FreeTiles)
    and (Player.X <> X) and (Player.Y <> Y);
  Map.SetTileEnum(X, Y, Map.Deep, teWater);  
end;

constructor TMob.Create;
begin
  Alive := True;
  X := 0;
  Y := 0;
end;

destructor TMob.Destroy;
begin

  inherited;
end;

procedure TMob.Process;
var
  NX, NY: Integer;          
begin
  if (GetDist(X, Y, Player.X, Player.Y) > 20) then Exit;
  if not DoAStar(High(Byte), High(Byte), X, Y, Player.X,
    Player.Y, @MyCallback, NX, NY)then Exit;
  if (NX = Player.X) and (NY = Player.Y) then AddRandom else
  begin
    X := NX;
    Y := NY;
  end;
end;

procedure TMob.Render(AX, AY: Byte);
begin
  if not Map.InView(X, Y) then Exit;
  Terminal.ForegroundColor(clDarkRed);
  Terminal.Print(X - Player.X + AX + View.Left,
    Y - Player.Y + AY + View.Top, '@');
end;

{ TMobs }

procedure TMobs.Add;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if not FMob[I].Alive then
    begin
      FMob[I].AddRandom;
      Exit;
    end;
  SetLength(FMob, Length(FMob) + 1);
  I := Length(FMob) - 1;
  FMob[I] := TMob.Create;
  FMob[I].AddRandom;
end;

function TMobs.Count: Integer;
begin
  Result := Length(FMob);
end;

constructor TMobs.Create;
begin
  SetLength(FMob, 0);
end;

destructor TMobs.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    FMob[I].Free;
    FMob[I] := nil;
  end;
  inherited;
end;

procedure TMobs.Process;
var
  I: Integer;
begin
  if (Count > 0) then
    for I := 0 to Count - 1 do
      if FMob[I].Alive then
        FMob[I].Process;
end;

procedure TMobs.Render(AX, AY: Byte);
var
  I: Integer;
begin
  if (Count > 0) then
    for I := 0 to Count - 1 do
      if FMob[I].Alive then
        FMob[I].Render(AX, AY);
end;

initialization
  Mobs := TMobs.Create;

finalization
  Mobs.Free;
  Mobs := nil;

end.
