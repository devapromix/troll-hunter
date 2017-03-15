unit uMob;

interface

uses uMap, uCommon;

type
  TMobBase = record
    Symbol: Char;
    Name: string;
    Boss: Boolean;
    Deep: TDeepEnum;
    MaxLife: Word;
    Level: Byte;
    Armor: Byte;
    DV: Byte;
    Damage: Word;
    Color: Cardinal;
  end;

const
  MobCount = 7;

const
  MobBase: array [0..MobCount - 1] of TMobBase = (
  // Dark Wood
  (Symbol: 'r'; Name: 'Rat';        Boss: False; Deep: deDarkWood;      MaxLife:   5; Level:  1; Armor:  0;  DV:  4; Damage:  2; Color: $FF249988;),
  (Symbol: 'f'; Name: 'Frog';       Boss: False; Deep: deDarkWood;      MaxLife:   7; Level:  1; Armor:  0;  DV:  5; Damage:  2; Color: $FF33FF66;),
  // Gray Cave
  (Symbol: 'k'; Name: 'Kobold';     Boss: False; Deep: deGrayCave;      MaxLife:  15; Level:  1; Armor:  1;  DV:  6; Damage:  4; Color: $FF777700;),
  // Deep Cave
  (Symbol: 'g'; Name: 'Goblin';     Boss: False; Deep: deDeepCave;      MaxLife:  20; Level:  2; Armor:  2;  DV: 12; Damage:  5; Color: $FF00AA00;),
  // Blood Cave
  (Symbol: 'z'; Name: 'Zombie';     Boss: False; Deep: deBloodCave;     MaxLife:  25; Level:  2; Armor:  2;  DV:  9; Damage:  3; Color: $FF00BB00;),
  // Dungeon of Doom
  (Symbol: 'O'; Name: 'Ogre';       Boss: False; Deep: deDungeonOfDoom; MaxLife:  80; Level: 10; Armor:  12; DV: 60; Damage: 30; Color: $FF559977;),
  // Boss
  (Symbol: 'T'; Name: 'Troll King'; Boss: True;  Deep: deDungeonOfDoom; MaxLife: 10; Level: 10; Armor:  14; DV: 60; Damage: 35; Color: $FFFF4400;)
  );

type
  TMob = class(TObject)
    ID: Byte;
    Life: Word;
    X, Y: Integer;
    Deep: TDeepEnum;
    Alive: Boolean;
    procedure AddRandom(ADeep: TDeepEnum);
    procedure Process;
    procedure Render(AX, AY: Byte);
    procedure Attack;
    procedure Defeat;
  end;

type
  TMobs = class(TObject)
    FMob: array of TMob;
    constructor Create();
    destructor Destroy; override;
    procedure Add(ADeep: TDeepEnum);
    function Count: Integer;
    procedure Process;
    procedure Render(AX, AY: Byte);
    function GetFreeTile(AX, AY: Byte): Boolean;
    function GetIndex(AX, AY: Byte): Integer;
  end;

type  
  TGetXYVal = function(X, Y: Integer): Boolean; stdcall;

var
  Mobs: TMobs = nil;

implementation

uses Math, SysUtils, Dialogs, uTerminal, uPlayer, uMsgLog;

function DoAStar(MapX, MapY, FromX, FromY, ToX, ToY: Integer; Callback: TGetXYVal; var TargetX, TargetY: integer): boolean;external 'BeaRLibPF.dll';

function MyCallback(X, Y: Integer): Boolean; stdcall;
begin
  Result := (Map.GetTileEnum(X, Y, Map.Deep) in FreeTiles)
end;

{ TMob }

procedure TMob.AddRandom(ADeep: TDeepEnum);
var
  FX, FY: Byte;
begin
  repeat
    ID := Math.RandomRange(0, MobCount);
    FX := Math.RandomRange(0, High(Byte));
    FY := Math.RandomRange(0, High(Byte));
  until (Map.GetTileEnum(FX, FY, ADeep) in SpawnTiles)
    and (Player.X <> FX)
    and (Player.Y <> FY)
    and Mobs.GetFreeTile(FX, FY)
    and (MobBase[ID].Deep = ADeep);
  if (MobBase[ID].Boss and IsBoss) then AddRandom(ADeep);
  X := FX;
  Y := FY;
  Deep := ADeep;
  Alive := True;
  Life := MobBase[ID].MaxLife;
  // Boss
  if MobBase[ID].Boss then
  begin
    IsBoss := True;
    if WizardMode then
    begin
      Player.X := X - 1;
      Player.Y := Y - 1;
    end;
  end;
end;    

procedure TMob.Attack;
var
  The: string;
  Dam: Word;
begin
  if (Self.Life = 0) or (Player.Life = 0) then Exit;
  The := GetCapit(GetDescThe(MobBase[ID].Name));
  if (Player.DV < Math.RandomRange(0, 100)) then
  begin
    // Attack
    Dam := Clamp(MobBase[ID].Damage, 0, High(Word));
    Player.Life := Clamp(Player.Life - Dam, 0, High(Word));
    MsgLog.Add(Format('%s hits you (%d).', [The, Dam]));
    if Player.Life = 0 then
      Player.Defeat(MobBase[ID].Name);
  end else begin
    // Miss
    MsgLog.Add(Format('%s hits you, but your armor protects you.', [The]));
  end;
end;

procedure TMob.Defeat;
begin
  Self.Alive := False;
  MsgLog.Add(Format('You kill %s.', [GetDescThe(MobBase[ID].Name)]));
  if (MobBase[ID].Boss and (Map.Deep = FinalDungeon)) then
  begin
    WonGame := True;
    MsgLog.Add('You have won.');
    TextScreenshot := GetTextScreenshot();
  end;
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
  if (Mobs.GetFreeTile(NX, NY)) then
  begin
    X := NX;
    Y := NY;
  end;
end;

procedure TMob.Render(AX, AY: Byte);
begin
  if not Map.InView(X, Y) or (not WizardMode
    and not Map.GetFOV(X, Y)) then Exit;
  if not WizardMode
    and (GetDist(Player.X, Player.Y, X, Y) > Player.GetRadius) then Exit;
  Terminal.Print(X - Player.X + AX + View.Left,
    Y - Player.Y + AY + View.Top, MobBase[ID].Symbol,
    MobBase[ID].Color);
end;

{ TMobs }

procedure TMobs.Add(ADeep: TDeepEnum);
var
  I: Integer;
begin
  for I := 0 to Self.Count - 1 do
    if not FMob[I].Alive then
    begin
      FMob[I].AddRandom(ADeep);
      Exit;
    end;
  SetLength(FMob, Length(FMob) + 1);  
  I := Length(FMob) - 1;
  FMob[I] := TMob.Create;
  FMob[I].AddRandom(ADeep);
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

function TMobs.GetFreeTile(AX, AY: Byte): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Count - 1 do
    with FMob[I] do
      if Alive and (Deep = Map.Deep) and (AX = X) and (AY = Y)then
      begin
        Result := False;
        Exit;
      end;
end;

function TMobs.GetIndex(AX, AY: Byte): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    with FMob[I] do
      if Alive and (Deep = Map.Deep) and (AX = X) and (AY = Y)then
      begin
        Result := I;
        Exit;
      end;
end;

procedure TMobs.Process;
var
  I: Integer;
begin
  if (Count > 0) then
    for I := 0 to Count - 1 do
      if FMob[I].Alive and (FMob[I].Deep = Map.Deep) then
        FMob[I].Process;
end;

procedure TMobs.Render(AX, AY: Byte);
var
  I: Integer;
begin
  if (Count > 0) then
    for I := 0 to Count - 1 do
      if FMob[I].Alive and (FMob[I].Deep = Map.Deep) then
        FMob[I].Render(AX, AY);
end;

initialization
  Mobs := TMobs.Create;

finalization
  Mobs.Free;
  Mobs := nil;

end.
