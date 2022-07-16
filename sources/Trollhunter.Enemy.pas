unit Trollhunter.Enemy;

interface

uses
  Trollhunter.Creature;

type
  TEnemy = class(TCreature)
  private
  public
    procedure Move(AX, AY: Integer);
    procedure Process;
    procedure Process2;
    procedure Melee;
    procedure Ranged;
    procedure SetEffects;
    procedure Poison;
    procedure Blind;
    constructor Create(AX, AY: Integer);
    destructor Destroy; override;
  end;

implementation

uses
  Types,
  SysUtils,
  Trollhunter.Creatures,
  Trollhunter.Utils,
  Trollhunter.Log,
  Trollhunter.Lang,
  Trollhunter.Error,
  Trollhunter.Projectiles,
  Trollhunter.Decorator,
  Trollhunter.AStar,
  Trollhunter.Map;

procedure TEnemy.Poison;
begin
  with Creatures.PC.TempSys do
    if (Rand(1, 3) = 1) then
    begin
      Add('Poison', Prop.Poison.Power, Prop.Poison.Duration);
      Log.Add(Format(GetLang(75), [GetCreatureLang(Prop.Sprite),
        Power('Poison'), Duration('Poison')]));
    end;
end;

procedure TEnemy.Blind;
begin
  with Creatures.PC.TempSys do
    if (Rand(1, 5) = 1) then
    begin
      Add('Blind', Prop.Blind.Power, Prop.Blind.Duration);
      Log.Add(GetLang(148));
    end;
end;

procedure TEnemy.SetEffects;
begin
  Poison;
  Blind;
end;

procedure TEnemy.Melee;
var
  D: Integer;
begin
  try
    D := Creatures.GetDamage(Self, Creatures.PC.Prop.Protect);
    if (D > 0) and (Rand(1, Prop.Dexterity + Creatures.PC.Prop.Dexterity) <=
      Prop.Dexterity) then
    begin
      Creatures.SetDamage(Creatures.PC, GetCreatureLang(Prop.Sprite), D);
      Self.SetEffects();
      if Creatures.PC.Life.IsMin then
        Log.Add(GetLang(72));
    end
    else
      Log.Add(Format(GetLang(76), [GetCreatureLang(Prop.Sprite)]));
    // The %s misses you!
  except
    on E: Exception do
      Error.Add('Enemy.Melee', E.Message);
  end;
end;

procedure TEnemy.Ranged;
var
  D: Integer;
  P: TProjectile;

  procedure AddCr(CrID: string; ManaCost: Byte);
  begin
    if (Mana.Cur >= ManaCost) and (Rand(1, 3) = 1) then
    begin
      Mana.Dec(ManaCost);
      Creatures.Insert(Pos.X, Pos.Y, CrID);
    end;
  end;

begin
  try
    case Prop.AIType of
      aiNecro:
        begin
          AddCr('SKELETON', 10);
          Exit;
        end;
    end;
    if Mana.IsMin then
      Exit
    else
      Mana.Dec;
    P := TProjectile.Create(Self, Pos.X, Pos.Y, Creatures.PC.Pos.X,
      Creatures.PC.Pos.Y);
    try
      D := Creatures.GetDamage(Self, Creatures.PC.Prop.Protect);
      if (D > 0) and (Rand(1, Prop.Dexterity + Creatures.PC.Prop.Dexterity) <=
        Prop.Dexterity) then
      begin
        Creatures.SetDamage(Creatures.PC, GetCreatureLang(Prop.Sprite), D);
        Self.SetEffects();
        if Creatures.PC.Life.IsMin then
          Log.Add(GetLang(72));
        // The %s misses you!
      end
      else
        Log.Add(Format(GetLang(76), [GetCreatureLang(Prop.Sprite)]));
    finally
      P.Free;
    end;
  except
    on E: Exception do
      Error.Add('Enemy.Ranged', E.Message);
  end;
end;

procedure TEnemy.Move(AX, AY: Integer);
var
  I: Integer;
begin
  try
    for I := 0 to High(Creatures.Enemy) do
      if not Creatures.Enemy[I].Life.IsMin and
        (Pos.X + AX = Creatures.Enemy[I].Pos.X) and
        (Pos.Y + AY = Creatures.Enemy[I].Pos.Y) then
        Exit;
    if (Pos.X + AX = Creatures.PC.Pos.X) and (Pos.Y + AY = Creatures.PC.Pos.Y)
    then
      Melee
    else
      inherited Move(AX, AY, (Prop.Decor <> dtWeb) or (Prop.Decor <> dtSlime));
  except
    on E: Exception do
      Error.Add('Enemy.Move', E.Message);
  end;
end;

constructor TEnemy.Create(AX, AY: Integer);
begin
  inherited Create();
  SetPosition(AX, AY);
end;

destructor TEnemy.Destroy;
begin
  inherited;
end;

function IsFreeCell(AX, AY: Integer): Boolean; stdcall;
begin
  Result := Creatures.PC.FreeCell(AX, AY);
end;

procedure TEnemy.Process2;
begin
  if ParamMove or (GetDist(Creatures.PC.Pos.X, Creatures.PC.Pos.Y, Pos.X, Pos.Y)
    > MaxDistance) then
    Exit;
  try

  except
    on E: Exception do
      Error.Add('Enemy.Process2', E.Message);
  end;
end;

procedure TEnemy.Process;
var
  I, J, JX, JY, NX, NY: Integer;
  L: TPoint;
begin
  if ParamMove or (GetDist(Creatures.PC.Pos.X, Creatures.PC.Pos.Y, Pos.X, Pos.Y)
    > MaxDistance) then
    Exit;
  try
    case Prop.AIType of
      aiSimple:
        begin
          if Creatures.PC.Pos.X > Pos.X then
            NX := 1
          else if Creatures.PC.Pos.X < Pos.X then
            NX := -1
          else
            NX := 0;
          if Creatures.PC.Pos.Y > Pos.Y then
            NY := 1
          else if Creatures.PC.Pos.Y < Pos.Y then
            NY := -1
          else
            NY := 0;
          if NX <> 0 then
            if (not FreeCell(Pos.X + NX, Pos.Y)) then
              NX := 0;
          if NY <> 0 then
            if (not FreeCell(Pos.X, Pos.Y + NY)) then
              NY := 0;
          if GetDist(Creatures.PC.Pos.X, Creatures.PC.Pos.Y, Pos.X, Pos.Y) >
            Prop.Radius then
            Exit;
          if NX = 0 then
            Move(0, NY)
          else if NY = 0 then
            Move(NX, 0)
          else if (Rand(1, 2) = 1) then
            Move(0, NY)
          else
            Move(NX, 0);
        end;
      aiGoblin, aiMelee, aiRanged, aiBerserk, aiSlug, aiBigSlime,
        aiSlime, aiNecro:
        begin
          if ((Prop.AIType = aiRanged) or (Prop.AIType = aiNecro)) and
            (GetDist(Creatures.PC.Pos.X, Creatures.PC.Pos.Y, Pos.X, Pos.Y) <=
            Prop.Distance) and LineDist(Creatures.PC.Pos.X, Creatures.PC.Pos.Y,
            Pos.X, Pos.Y) then
          begin
            // Box(GetDist(Creatures.PC.X, Creatures.PC.Y, X, Y));
            Self.Ranged;
            Exit;
          end;

          L := Creatures.PC.Pos;
          if (Prop.AIType <> aiBerserk) and (AI = aiRun) then
          begin
            if (Mana.Cur < Mana.Max) and (Rand(1, 4) = 1) then
              Mana.Inc;
            if (Life.Cur = Life.Max) then
              AI := aiCombat;
            if ((L.X = Look.X) and (L.X = Look.Y)) or (Rand(1, 15) = 1) then
            begin
              AI := aiCombat;
              Process;
            end
            else
            begin
              {
                if (Rand(1, 9) = 1) then
                repeat
                WX := Rand(Map.Width + 5, Map.Width - 6);
                WY := Rand(Map.Height + 5, Map.Height - 6);
                until Map.Cell[WY][WX].Tile in FloorSet;
              }
              L := Look;
              if not Life.IsMin and (Life.Cur < Life.Max) and (Rand(1, 3) = 1)
              then
                if not Mana.IsMin then
                begin
                  Mana.Dec;
                  Life.Inc;
                end
                else if (Mana.Cur < Mana.Max) then
                  Mana.Inc;
            end;
          end;
          //
          if not DoAStar(Map.Width, Map.Height, Pos.X, Pos.Y, L.X, L.Y,
            @IsFreeCell, NX, NY) then
            Exit;
          //
          begin
            if (NX <= 0) or (NY <= 0) then
              Exit;
            if (GetDist(Creatures.PC.Pos.X, Creatures.PC.Pos.Y, Pos.X, Pos.Y) >
              Prop.Radius) then
              Exit;
            for I := 0 to High(Creatures.Enemy) do
              if not Creatures.Enemy[I].Life.IsMin and
                (NX = Creatures.Enemy[I].Pos.X) and
                (NY = Creatures.Enemy[I].Pos.Y) then
              begin
                JX := Rand(NX - 1, NX + 1);
                JY := Rand(NY - 1, NY + 1);
                if Creatures.PC.FreeCell(JX, JY) and
                  not((JX = Creatures.PC.Pos.X) and (JY = Creatures.PC.Pos.Y))
                then
                begin
                  for J := 0 to High(Creatures.Enemy) do
                    if not Creatures.Enemy[J].Life.IsMin and
                      (JX = Creatures.Enemy[J].Pos.X) and
                      (JY = Creatures.Enemy[J].Pos.Y) then
                      Exit;
                  Creatures.Enemy[I].SetPosition(JX, JY);
                end;
                Exit;
              end;
            if (NX = Creatures.PC.Pos.X) and (NY = Creatures.PC.Pos.Y) then
              Melee()
            else
              Move(NX - Pos.X, NY - Pos.Y);
          end;
        end;
    end;
  except
    on E: Exception do
      Error.Add('Enemy.Process', E.Message);
  end;
end;

end.
