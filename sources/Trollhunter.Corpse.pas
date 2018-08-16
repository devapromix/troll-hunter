unit Trollhunter.Corpse;

interface

uses
  Trollhunter.Types;

const
  CorpseMax = 10;

type
  TCorpse = record
    X, Y, Z: UInt;
  end;

  TCorpses = class(TObject)
  private
    FCorpse: array [0 .. CorpseMax - 1] of TCorpse;
    procedure Save(Index, AX, AY, AZ: UInt);
    procedure Load(Index: UInt);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Render(AX, AY: UInt);
    function IsCorpse(AX, AY: UInt): Boolean;
    procedure Append();
    procedure DelCorpse(AX, AY: UInt);
  end;

var
  Corpses: TCorpses = nil;

implementation

uses
  SysUtils,
  Trollhunter.Player,
  Trollhunter.Map,
  Trollhunter.Game,
  Trollhunter.Terminal;

{ TCorpses }

procedure TCorpses.Save(Index, AX, AY, AZ: UInt);
var
  // F: TIniFile;
  S: string;
begin
  { F := TIniFile.Create(Game.GetPath() + 'morgue.thi');
    try
    S := IntToStr(Index);
    F.EraseSection(S);
    if ((AX > 0) and (AY > 0)) then
    begin
    F.WriteInt(S, 'X', AX);
    F.WriteInt(S, 'Y', AY);
    F.WriteInt(S, 'Z', AZ);
    end;
    finally
    F.Free;
    end; }
end;

procedure TCorpses.Load(Index: UInt);
var
  // F: TIniFile;
  S: string;
begin
  { F := TIniFile.Create(Game.GetPath() + 'morgue.thi');
    try
    S := IntToStr(Index);
    FCorpse[Index].X := F.ReadInt(S, 'X', 0);
    FCorpse[Index].Y := F.ReadInt(S, 'Y', 0);
    FCorpse[Index].Z := F.ReadInt(S, 'Z', 0);
    finally
    F.Free;
    end; }
end;

procedure TCorpses.Append;
var
  // F: TIniFile;
  I: UInt;
  S: string;
begin
  if (Player.X = 0) or (Player.Y = 0) or (Player.X = UIntMax) or (Player.Y = UIntMax) then
    Exit;
  { F := TIniFile.Create(Game.GetPath() + 'morgue.thi');
    try
    for I := 0 to CorpseMax - 1 do
    begin
    S := IntToStr(I);
    if ((FCorpse[I].X = 0) or (FCorpse[I].Y = 0)) then
    F.EraseSection(S);
    if not F.SectionExists(S) then
    begin
    Save(I, Player.X, Player.Y, Ord(Map.Current));
    Exit;
    end;
    end;
    finally
    F.Free;
    end; }
end;

constructor TCorpses.Create;
// var
// F: TIniFile;
// I: UInt;
begin
  { F := TIniFile.Create(Game.GetPath() + 'morgue.thi');
    try
    for I := 0 to CorpseMax - 1 do
    Load(I);
    finally
    F.Free;
    end; }
end;

procedure TCorpses.DelCorpse(AX, AY: UInt);
var
  I: UInt;
begin
  for I := 0 to CorpseMax - 1 do
  begin
    if (UInt(Ord(Map.Current)) <> FCorpse[I].Z) then
      Continue;
    if ((FCorpse[I].X = AX) and (FCorpse[I].Y = AY)) then
    begin
      Save(I, 0, 0, 0);
      Load(I);
      Exit;
    end;
  end;
end;

destructor TCorpses.Destroy;
begin

  inherited;
end;

function TCorpses.IsCorpse(AX, AY: UInt): Boolean;
var
  I: UInt;
begin
  Result := False;
  for I := 0 to CorpseMax - 1 do
  begin
    if (UInt(Ord(Map.Current)) <> FCorpse[I].Z) then
      Continue;
    if ((FCorpse[I].X = AX) and (FCorpse[I].Y = AY)) then
      Exit(True);
  end;
end;

procedure TCorpses.Render(AX, AY: UInt);
var
  Color: Cardinal;
  X, Y, I: UInt;
begin
  for I := 0 to CorpseMax - 1 do
  begin
    if (UInt(Ord(Map.Current)) <> FCorpse[I].Z) then
      Continue;
    if not Map.InView(FCorpse[I].X, FCorpse[I].Y) or (not Mode.Wizard and not Map.GetFOV(FCorpse[I].X, FCorpse[I].Y)) then
      Continue;
    if ((FCorpse[I].X = 0) or (FCorpse[I].Y = 0)) then
      Continue;
    X := FCorpse[I].X - Player.X + AX + View.Left;
    Y := FCorpse[I].Y - Player.Y + AY + View.Top;
    if not Mode.Wizard and (Player.GetDist(FCorpse[I].X, FCorpse[I].Y) > Player.Vision) then
      Color := clFog
    else
      Color := clCorpse;
    Terminal.Print(X, Y, '%', Color);
  end;
end;

initialization

Corpses := TCorpses.Create;

finalization

FreeAndNil(Corpses);

end.
