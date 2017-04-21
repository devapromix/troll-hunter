unit uCorpse;

interface

const
  CorpseMax = 10;

type
  TCorpse = record
    X, Y, Z: Byte;
  end;

  TCorpses = class
  private
    FCorpse: array [0..CorpseMax - 1] of TCorpse;
    procedure Save(Index, AX, AY, AZ: Byte);
    procedure Load(Index: Byte);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Render(AX, AY: Byte);
    function IsCorpse(AX, AY: Byte): Boolean;
    procedure Append();
    procedure DelCorpse(AX, AY: Byte);
  end;

var
  Corpses: TCorpses = nil;

implementation

uses SysUtils, IniFiles, uPlayer, uMap, uGame, uTerminal;

{ TCorpses }

procedure TCorpses.Save(Index, AX, AY, AZ: Byte);
var
  F: TIniFile;
  S: string;
begin
  F := TIniFile.Create(Game.GetPath() + 'morgue.thi');
  try
    S := IntToStr(Index);
    F.EraseSection(S);
    if ((AX > 0) and (AY > 0)) then
    begin
      F.WriteInteger(S, 'X', AX);
      F.WriteInteger(S, 'Y', AY);
      F.WriteInteger(S, 'Z', AZ);
    end; 
  finally
    F.Free;
  end;
end;

procedure TCorpses.Load(Index: Byte);
var
  F: TIniFile;  
  S: string;
begin
  F := TIniFile.Create(Game.GetPath() + 'morgue.thi');
  try
    S := IntToStr(Index);
    FCorpse[Index].X := F.ReadInteger(S, 'X', 0);
    FCorpse[Index].Y := F.ReadInteger(S, 'Y', 0);
    FCorpse[Index].Z := F.ReadInteger(S, 'Z', 0);
  finally
    F.Free;
  end;
end;

procedure TCorpses.Append;
var
  F: TIniFile;
  I: Byte;
  S: string;
begin
  if (Player.X = 0) or (Player.Y = 0) or (Player.X = High(Byte)) or
    (Player.Y = High(Byte)) then Exit;
  F := TIniFile.Create(Game.GetPath() + 'morgue.thi');
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
  end;
end;

constructor TCorpses.Create;
var
  F: TIniFile;
  I: Byte;
begin
  F := TIniFile.Create(Game.GetPath() + 'morgue.thi');
  try
    for I := 0 to CorpseMax - 1 do Load(I);
  finally
    F.Free;
  end;
end;

procedure TCorpses.DelCorpse(AX, AY: Byte);
var
  I: Byte;
begin
  for I := 0 to CorpseMax - 1 do
  begin
    if (Byte(Ord(Map.Current)) <> FCorpse[I].Z) then Continue;
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

function TCorpses.IsCorpse(AX, AY: Byte): Boolean;
var
  I: Byte;
begin
  Result := False;
  for I := 0 to CorpseMax - 1 do
  begin
    if (Byte(Ord(Map.Current)) <> FCorpse[I].Z) then Continue;
    if ((FCorpse[I].X = AX) and (FCorpse[I].Y = AY)) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TCorpses.Render(AX, AY: Byte);
var
  Color: Cardinal;
  X, Y, I: Byte;
begin
  for I := 0 to CorpseMax - 1 do
  begin
    if (Byte(Ord(Map.Current)) <> FCorpse[I].Z) then Continue;
    if not Map.InView(FCorpse[I].X, FCorpse[I].Y) or
      (not Game.Wizard and not Map.GetFOV(FCorpse[I].X, FCorpse[I].Y)) then
      Continue;
    if ((FCorpse[I].X = 0) or (FCorpse[I].Y = 0)) then Continue;
    X := FCorpse[I].X - Player.X + AX + View.Left;
    Y := FCorpse[I].Y - Player.Y + AY + View.Top;
    if not Game.Wizard and (Player.GetDist(FCorpse[I].X, FCorpse[I].Y) > Player.GetRadius) then
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
