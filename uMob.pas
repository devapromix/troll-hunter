unit uMob;

interface

uses uCommon;

type
  TMobBase = record
    Symbol: Char;
    Name: string;
    MaxLife: Word;
    Color: Cardinal;
  end;

const
  MobCount = 3;

const
  MobBase: array [0..MobCount - 1] of TMobBase = (
  (Symbol: 'r'; Name: 'Rat';    MaxLife: 5;  Color: $FF249988;),
  (Symbol: 'k'; Name: 'Kobold'; MaxLife: 15; Color: $FF777700;),
  (Symbol: 'g'; Name: 'Goblin'; MaxLife: 20; Color: $FF00AA00;)
  );

type
  TMob = class(TObject)
    ID: Byte;
    Life: Word;
    X, Y: Integer;
    Alive: Boolean;
    procedure AddRandom;
    procedure Process;
    procedure Render(AX, AY: Byte);
    procedure Attack;
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
    function FreeTile(AX, AY: Byte): Boolean;
  end;

type
  TGetXYVal = function(X, Y: Integer): Boolean; stdcall;

var
  Mobs: TMobs = nil;

implementation

uses Math, uTerminal, uMap, uPlayer;

function DoAStar(MapX, MapY, FromX, FromY, ToX, ToY: Integer; Callback: TGetXYVal; var TargetX, TargetY: integer): boolean;external 'BeaRLibPF.dll';

function MyCallback(X, Y: Integer): Boolean; stdcall;
begin
  Result := (Map.GetTileEnum(X, Y, Map.Deep) in FreeTiles)
end;

{ TMob }

procedure TMob.AddRandom;
var
  FX, FY: Byte;
begin
  repeat
    FX := Math.RandomRange(0, High(Byte));
    FY := Math.RandomRange(0, High(Byte));
  until (Map.GetTileEnum(FX, FY, Map.Deep) in FreeTiles)
    and (Player.X <> FX) and (Player.Y <> FY)
    and Mobs.FreeTile(FX, FY);
  X := FX;
  Y := FY;
  Alive := True;
  ID := Math.RandomRange(0, MobCount);
  Life := MobBase[ID].MaxLife;
end;

procedure TMob.Attack;
begin
  Player.Life := Clamp(Player.Life - 5, 0, High(Word));
  if Player.Life = 0 then Player.Defeat(MobBase[ID].Name);
end;

procedure TMob.Process;
var
  NX, NY: Integer;
begin
  if (GetDist(X, Y, Player.X, Player.Y) > 20) then Exit;
  if not DoAStar(High(Byte), High(Byte), X, Y, Player.X,
    Player.Y, @MyCallback, NX, NY)then Exit;
  if (NX = Player.X) and (NY = Player.Y) then
  begin
    Self.Attack();
  end else
  if (Mobs.FreeTile(NX, NY)) then
  begin
    X := NX;
    Y := NY;
  end;
end;

procedure TMob.Render(AX, AY: Byte);
begin
  if not Map.InView(X, Y) or (not WizardMode and not Map.GetFOV(X, Y)) then Exit;
  Terminal.ForegroundColor(MobBase[ID].Color);
  Terminal.Print(X - Player.X + AX + View.Left,
    Y - Player.Y + AY + View.Top, MobBase[ID].Symbol);
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

function TMobs.FreeTile(AX, AY: Byte): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Count - 1 do
    with FMob[I] do  
      if Alive and (AX = X) and (AY = Y)then
      begin
        Result := False;
        Exit;
      end;
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
