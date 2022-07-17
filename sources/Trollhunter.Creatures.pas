unit Trollhunter.Creatures;

interface

uses
  Graphics,
  Trollhunter.Creature,
  Trollhunter.PC,
  Trollhunter.Enemy;

type
  TEnemies = array of TEnemy;

  TCreatures = class(TObject)
  private
    FPC: TPC;
    LBAR: Graphics.TBitmap;
    MBAR: Graphics.TBitmap;
    LIFEBAR: Graphics.TBitmap;
    MANABAR: Graphics.TBitmap;
    FEnemy: TEnemies;
    procedure SetPC(const Value: TPC);
    procedure SetEnemy(const Value: TEnemies);
  public
    procedure Move;
    procedure Clear;
    function EmptyCell(X, Y: Integer): Boolean;
    procedure Add(const X, Y: Integer; AName: string);
    procedure Render(X, Y, DX, DY: Integer; IsSpot: Boolean);
    procedure SetDamage(ACreature: TCreature; EnemyName: string;
      ADamage: Integer; IsTrap: Boolean = False);
    procedure SetManaDamage(ACreature: TCreature; EnemyName: string;
      ADamage: Integer; IsTrap: Boolean = False);
    function GetMapEnemiesCount(): Integer;
    procedure Summon();
    procedure Teleport(ToEnemy: Boolean);
    function CreatureIndex(ID: string): Integer;
    procedure Insert(AX, AY: Integer; CreatureID: string; Chance: Byte = 1);
    function GetDamage(ACreature: TCreature; AProtect: Integer): Integer;
    property PC: TPC read FPC write SetPC;
    property Enemy: TEnemies read FEnemy write SetEnemy;
    constructor Create;
    destructor Destroy; override;
  end;

const
  MaxDistance = 12;

var
  Creatures: TCreatures;

implementation

uses
  Windows,
  Types,
  SysUtils,
  Trollhunter.Utils,
  Trollhunter.Error,
  Trollhunter.Graph,
  Trollhunter.Projectiles,
  Trollhunter.Item,
  Trollhunter.Log,
  Trollhunter.Lang,
  Trollhunter.Decorator,
  Trollhunter.Map,
  Trollhunter.Map.Tiles;

{ TCreatures }

function TCreatures.EmptyCell(X, Y: Integer): Boolean;
var
  J: Integer;
begin
  Result := False;
  if (X = Creatures.PC.Pos.X) and (Y = Creatures.PC.Pos.Y) then
    Exit;
  if not Creatures.PC.FreeCell(X, Y) then
    Exit;
  for J := 0 to High(Creatures.Enemy) do
    if not Creatures.Enemy[J].Life.IsMin and (X = Creatures.Enemy[J].Pos.X) and
      (Y = Creatures.Enemy[J].Pos.Y) then
      Exit;
  Result := True;
end;

function TCreatures.GetDamage(ACreature: TCreature; AProtect: Integer): Integer;
begin
  Result := 0;
  try
    if (ACreature.Prop.MinDamage < 1) then
      ACreature.Prop.MinDamage := 1;
    if (ACreature.Prop.MaxDamage < 2) then
      ACreature.Prop.MaxDamage := 2;
    Result := Rand(ACreature.Prop.MinDamage, ACreature.Prop.MaxDamage);
    if (Result < 1) then
      Result := 1;
    Result := (Result * (100 - AProtect)) div 100;
    if ((ACreature.Prop.Strength div 10) > Rand(1, 100)) then
      Result := Result + ACreature.Prop.Strength;
  except
    on E: Exception do
      Error.Add('Creatures.GetDamage', E.Message);
  end;
end;

procedure TCreatures.SetDamage(ACreature: TCreature; EnemyName: string;
  ADamage: Integer; IsTrap: Boolean = False);
var
  CX, CY: Integer;
begin
  try
    if (ADamage < 1) then
      ADamage := 1;

    CX := (ACreature.Pos.X - (Creatures.PC.Pos.X - Graph.RW)) * TileSize;
    CY := (ACreature.Pos.Y - (Creatures.PC.Pos.Y - Graph.RH)) * TileSize +
      Graph.CharHeight;
    with TAnimNumber.Create(-ADamage, CX, CY) do
      Free;

    if (ACreature = Creatures.PC) then
    begin
      // The %s hits you %d.
      Items.Damage(ArmorSet, 7);
      Log.Add(Format(GetLang(68), [EnemyName, ADamage]));
    end
    else
    begin
      if IsTrap then
      begin
        // The trap hits the %s %d.
        Log.Add(Format(GetLang(104), [EnemyName, ADamage]));
      end
      else
      begin
        // You hit the %s %d.
        Items.Damage(WeaponSet, 5);
        Log.Add(Format(GetLang(69), [EnemyName, ADamage]));
      end;
    end;

    ACreature.Life.Dec(ADamage);
    if (Rand(0, 4) = 0) then
      Decorators.Insert(ACreature.Prop.Decor, ACreature.Pos.X, ACreature.Pos.Y);
  except
    on E: Exception do
      Error.Add('Creatures.SetDamage', E.Message);
  end;
end;

procedure TCreatures.SetManaDamage(ACreature: TCreature; EnemyName: string;
  ADamage: Integer; IsTrap: Boolean);
begin
  try
    if (ADamage < 1) then
      ADamage := 1;

    if (ACreature = Creatures.PC) then
    begin
      // The %s hits your mana %d.
      Log.Add(Format(GetLang(107), [EnemyName, ADamage]));
    end
    else
    begin
      if IsTrap then
      begin
        // The trap hits the %s %d.
        Log.Add(Format(GetLang(106), [EnemyName, ADamage]));
      end
      else
      begin
        // You hit the %s %d.
        Log.Add(Format(GetLang(108), [EnemyName, ADamage]));
      end;
    end;

    ACreature.Mana.Dec(ADamage);
  except
    on E: Exception do
      Error.Add('Creatures.SetManaDamage', E.Message);
  end;
end;

procedure TCreatures.Insert(AX, AY: Integer; CreatureID: string;
  Chance: Byte = 1);
var
  X, Y, Z: Integer;
begin
  for Z := 1 to 25 do
  begin
    X := AX + Rand(-1, 1);
    Y := AY + Rand(-1, 1);
    if (AX = X) and (AY = Y) then
      Continue;
    if EmptyCell(X, Y) then
    begin
      if (Rand(1, Chance) <= 1) then
        Add(X, Y, CreatureID);
      Exit;
    end;
  end;
end;

constructor TCreatures.Create;
begin
  FPC := TPC.Create;
  LBAR := Graphics.TBitmap.Create;
  LIFEBAR := Graphics.TBitmap.Create;
  LIFEBAR.Handle := LoadBitmap(hInstance, 'LIFEBAR');
  MBAR := Graphics.TBitmap.Create;
  MANABAR := Graphics.TBitmap.Create;
  MANABAR.Handle := LoadBitmap(hInstance, 'MANABAR');
end;

destructor TCreatures.Destroy;
var
  I: Word;
begin
  if (Length(Enemy) > 0) then
    for I := 0 to Length(Enemy) - 1 do
      Enemy[I].Free;
  FPC.Free;
  LBAR.Free;
  LIFEBAR.Free;
  MBAR.Free;
  MANABAR.Free;
  inherited;
end;

procedure TCreatures.Clear;
begin
  SetLength(FEnemy, 0);
end;

procedure TCreatures.Move;
var
  I, J: Integer;
begin
  try
    if (Length(Enemy) > 0) then
      for J := 0 to PC.AP.Max do
        for I := 0 to High(Enemy) do
          if not Enemy[I].Life.IsMin and not PC.Life.IsMin then
          begin
            with Enemy[I] do
              if AP.IsMin then
              begin
                Process;
                AP.SetToMax;
              end
              else
                AP.Dec;
          end;
  except
    on E: Exception do
      Error.Add('Creatures.Move', E.Message);
  end;
end;

procedure TCreatures.Render(X, Y, DX, DY: Integer; IsSpot: Boolean);
var
  I: Integer;
begin
  try
    with Graph.Surface.Canvas do
      if (Length(Creatures.Enemy) > 0) then
      begin
        if IsSpot then
        begin
          for I := 0 to High(Creatures.Enemy) do
            if (X = Creatures.Enemy[I].Pos.X) and (Y = Creatures.Enemy[I].Pos.Y)
            then
              if Creatures.Enemy[I].Life.IsMin then
                Draw(DX, DY, Creatures.Enemy[I].Spot);
        end
        else
          for I := 0 to High(Creatures.Enemy) do
            if (X = Creatures.Enemy[I].Pos.X) and (Y = Creatures.Enemy[I].Pos.Y)
            then
              if not Creatures.Enemy[I].Life.IsMin then
              begin
                Draw(DX, DY, Creatures.Enemy[I].Image);
                LBAR.Assign(LIFEBAR);
                LBAR.Width := BarWidth(Creatures.Enemy[I].Life.Cur,
                  Creatures.Enemy[I].Life.Max, 30);
                Draw(DX + 1, DY, LBAR);
                if not Creatures.Enemy[I].Mana.IsMin then
                begin
                  MBAR.Assign(MANABAR);
                  MBAR.Width := BarWidth(Creatures.Enemy[I].Mana.Cur,
                    Creatures.Enemy[I].Mana.Max, 30);
                  Draw(DX + 1, DY + 2, MBAR);
                end;
              end;
      end;
  except
    on E: Exception do
      Error.Add('Creatures.Render', E.Message);
  end;
end;

procedure TCreatures.SetEnemy(const Value: TEnemies);
begin
  FEnemy := Value;
end;

procedure TCreatures.SetPC(const Value: TPC);
begin
  FPC := Value;
end;

procedure TCreatures.Add(const X, Y: Integer; AName: string);
var
  A, I, P: Integer;
  B: Graphics.TBitmap;
begin
  try
    B := Graphics.TBitmap.Create;
    try
      AName := UpperCase(Trim(AName));
      if (AName = '') then
        Exit;
      P := CreatureIndex(AName);
      if (P < 0) or (P > High(DungeonCreatures)) then
        Exit;
      I := Length(FEnemy) + 1;
      SetLength(FEnemy, I);
      FEnemy[I - 1] := TEnemy.Create(X, Y);
      with FEnemy[I - 1] do
      begin
        A := 0;
        Look := Point(X, Y);
        Name := AName;
        Prop := DungeonCreatures[P];
        B.Handle := Windows.LoadBitmap(hInstance, PChar(Name));
        Graph.BitmapFromTileset(Image, B, 0);
        Image.Transparent := True;
        Graph.BitmapFromTileset(Spot, B, 1);
        Spot.Transparent := True;
        if (Prop.Level < PC.Dungeon) then
          A := PC.Dungeon - Prop.Level;
        with Prop do
          if (A > 0) then
          begin
            Inc(Level, A);
            if (Strength >= Dexterity) and (Strength >= Will) then
              Inc(Strength, A);
            if (Dexterity >= Strength) and (Dexterity >= Will) then
              Inc(Dexterity, A);
            if (Will >= Strength) and (Will >= Dexterity) then
              Inc(Will, A);
            Inc(MaxDamage, Strength div 2);
            Inc(Protect, Strength div 10);
            Inc(Radius, Dexterity div 5);
          end;
        if (Prop.AIType = aiBerserk) then
          Prop.Protect := 0;
        Calc();
        Fill();
        AP.SetToMax;
        Map.Cell[Y][X].Tile := tlFloor; { ? }
      end;
    finally
      B.Free;
    end;
  except
    on E: Exception do
      Error.Add('Creatures.Add', E.Message);
  end;
end;

function TCreatures.CreatureIndex(ID: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  try
    for I := 0 to High(DungeonCreatures) do
      if (Trim(ID) = DungeonCreatures[I].Sprite) then
      begin
        Result := I;
        Break;
      end;
  except
    on E: Exception do
      Error.Add('Creatures.CreatureIndex', E.Message);
  end;
end;

function TCreatures.GetMapEnemiesCount: Integer;
var
  I, C: Integer;
begin
  C := 0;
  if (Length(Creatures.Enemy) > 0) then
    for I := 0 to High(Creatures.Enemy) do
      if not Creatures.Enemy[I].Life.IsMin then
        Inc(C);
  Result := C;
end;

procedure TCreatures.Summon;
var
  I, PX, PY: Integer;
begin
  try
    if (GetMapEnemiesCount() = 0) then
      Exit;
    repeat
      I := Rand(0, High(Enemy));
      PX := Rand(PC.Pos.X - 1, PC.Pos.X + 1);
      PY := Rand(PC.Pos.Y - 1, PC.Pos.Y + 1);
    until (not Enemy[I].Life.IsMin) and (Map.Cell[PY][PX].Tile in FloorSet) and
      not((PX = PC.Pos.X) and (PY = PC.Pos.Y)) and
      (Creatures.EmptyCell(PX, PY));;
    Enemy[I].SetPosition(PX, PY);
    PC.Wait();
  except
    on E: Exception do
      Error.Add('Creatures.Summon', E.Message);
  end;
end;

procedure TCreatures.Teleport(ToEnemy: Boolean);
var
  I, PX, PY: Integer;
begin
  try
    if (Creatures.GetMapEnemiesCount() = 0) or not ToEnemy then
    begin
      repeat
        PX := Rand(2, Map.Width - 3);
        PY := Rand(2, Map.Height - 3);
      until (Map.Cell[PY][PX].Tile in FloorSet) and
        (Creatures.EmptyCell(PX, PY));
    end
    else
    begin
      repeat
        I := Rand(0, High(Creatures.Enemy));
        PX := Rand(Creatures.Enemy[I].Pos.X - 1, Creatures.Enemy[I].Pos.X + 1);
        PY := Rand(Creatures.Enemy[I].Pos.Y - 1, Creatures.Enemy[I].Pos.Y + 1);
      until (not Creatures.Enemy[I].Life.IsMin) and
        (Map.Cell[PY][PX].Tile in FloorSet) and (Creatures.EmptyCell(PX, PY));
    end;
    Creatures.PC.SetPosition(PX, PY);
    PC.Wait();
  except
    on E: Exception do
      Error.Add('Creatures.Teleport', E.Message);
  end;
end;

initialization

Creatures := TCreatures.Create;

finalization

Creatures.Free;

end.
