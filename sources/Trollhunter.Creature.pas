unit Trollhunter.Creature;

interface

uses
  Types,
  Graphics,
  Trollhunter.BaseCreature,
  Trollhunter.Decorator,
  Trollhunter.TempSys;

const
  CreaturesCount = 19;

type
  TAIType = (aiSimple, aiGoblin, aiMelee, aiRanged, aiBerserk, aiSlug,
    aiBigSlime, aiSlime, aiNecro);

  TCreatureProjectileType = (ptNone, ptPoisonball, ptBolt, ptFireball,
    ptArrow, ptIce);

  TCreatureRec = record
    Sprite: string;
    AIType: TAIType;
    Decor: TDecorType;
    Level: Integer;
    Exp: Integer;
    MinDamage: Integer;
    MaxDamage: Integer;
    Protect: Integer;
    Radius: Integer;
    Distance: Integer;
    Projectile: TCreatureProjectileType;
    Strength: Integer;
    Dexterity: Integer;
    Will: Integer;
    Speed: Integer;
    Poison: TTempSysItem;
    Blind: TTempSysItem;
  end;

const
  DungeonCreatures: array [0 .. CreaturesCount - 1] of TCreatureRec = (

{$I Enemies/Enemies.crt     }
  );

type
  TAI = (aiNone, aiCombat, aiRun);

  // Creature
  TCreature = class(TBaseCreature)
  private
    FLook: TPoint;
    FAI: TAI;
    procedure SetLook(const Value: TPoint);
    procedure SetAI(const Value: TAI);
  public
    Prop: TCreatureRec;
    Spot: Graphics.TBitmap;
    procedure Calc;
    property AI: TAI read FAI write SetAI;
    function FreeCell(AX, AY: Integer): Boolean;
    procedure Move(AX, AY: Integer; B: Boolean = True);
    constructor Create();
    destructor Destroy; override;
    property Look: TPoint read FLook write SetLook;
    procedure IncLook(X, Y: Integer);
  end;

implementation

uses
  Math,
  SysUtils,
  Trollhunter.Map,
  Trollhunter.Map.Tiles,
  Trollhunter.Utils,
  Trollhunter.Creatures,
  Trollhunter.Lang,
  Trollhunter.Log,
  Trollhunter.Error,
  Trollhunter.Graph,
  Trollhunter.Formulas;

{ TCreature }

constructor TCreature.Create();
begin
  inherited;
  AI := aiNone;
  Spot := Graphics.TBitmap.Create;
  Spot.Transparent := True;
  Prop.Decor := dtNone;
  Prop.Protect := 0;
  Prop.Radius := 5;
  Prop.Distance := 0;
  Prop.Level := 1;
  Prop.Exp := 0;
  Prop.Poison := TempSysItem(0, 0);
  Prop.Blind := TempSysItem(0, 0);
end;

destructor TCreature.Destroy;
begin
  Spot.Free;
  inherited;
end;

procedure TCreature.Calc;
begin
  AP.SetMax(GetMaxAP(Prop.Speed));
  Life.SetMax(GetMaxLife(Prop.Strength));
  Mana.SetMax(GetMaxMana(Prop.Will));
end;

function TCreature.FreeCell(AX, AY: Integer): Boolean;
begin
  Result := Map.Cell[AY][AX].Tile in FloorSet + [tlStone] +
    [tlOpenDoor, tlPrevDungeon .. tlAltNextDungeon,
    tlEmptyShrine .. tlMegaShrine, tlOpenWoodChest .. tlClosedBarrel];
end;

procedure TCreature.Move(AX, AY: Integer; B: Boolean = True);

  procedure TrapDamage(ID: Byte);
  var
    D: Integer;
    S: string;
  begin
    try
      D := Rand(1, 3);
      if (Self = Creatures.PC) then
        S := GetLang(103)
      else
        S := GetCreatureLang(Prop.Sprite);
      Creatures.SetDamage(Self, S, D, True);
    except
      on E: Exception do
        Error.Add('Creature.Move.TrapDamage', E.Message);
    end;
  end;

  procedure TrapPoison();
  begin
    with Creatures.PC.TempSys do
      try
        Add('Poison', Map.Level, Rand(Map.Level * 2, Map.Level * 4));
        Log.Add(Format(GetLang(105), [Power('Poison'), Duration('Poison')]));
      except
        on E: Exception do
          Error.Add('Creature.Move.TrapPoison', E.Message);
      end;
  end;

  procedure TrapWeb();
  begin
    with Creatures.PC.TempSys do
      try
        Add('Webbed', Map.Level, Rand(Map.Level * 3, Map.Level * 5));
        Log.Add(Format(GetLang(113), [Power('Webbed'), Duration('Webbed')]));
      except
        on E: Exception do
          Error.Add('Creature.Move.TrapWeb', E.Message);
      end;
  end;

  procedure TrapMagic();
  var
    D: Word;
    S: string;
  begin
    try
      D := Rand(Map.Level * 3, Map.Level * 5);
      with Creatures.PC do
      begin
        if (Mana.Cur < D) then
          D := Mana.Cur;
        if (Self = Creatures.PC) then
          S := GetLang(103)
        else
          S := GetCreatureLang(Prop.Sprite);
        if (D < 1) then
          D := Rand(1, 3);
        Creatures.SetManaDamage(Self, S, D, True);
      end;
    except
      on E: Exception do
        Error.Add('Creature.Move.TrapMagic', E.Message);
    end;
  end;

  procedure DoTrap(ID: Word);
  begin
    Exit;
    try
      case ID of
        dTrapPsn:
          TrapPoison();
        dTrapMag:
          TrapMagic();
        dTrapSum:
          Creatures.Summon();
        dTrapWeb:
          TrapWeb();
        dTrapMax:
          Creatures.Teleport(True);
      else
        TrapDamage(ID);
      end;
      Log.Apply;
      Graph.Render;
    except
      on E: Exception do
        Error.Add('Creature.Move.DoTrap', E.Message);
    end;
  end;

begin
  try
    SetPosition(Pos.X + AX, Pos.Y + AY);
    if (Mana.Cur < Mana.Max) and (Rand(1, 3) = 1) then
      Mana.Inc;
    case Map.Cell[Pos.Y][Pos.X].Decor of
      // Traps
      dTrap, dTrapDet:
        begin
          Map.Cell[Pos.Y][Pos.X].Decor := Rand(dTrapMin, dTrapMax);
          DoTrap(Map.Cell[Pos.Y][Pos.X].Decor);
        end;
      dTrapMin .. dTrapMax:
        DoTrap(Map.Cell[Pos.Y][Pos.X].Decor);
      dWeb:
        TrapWeb();
    end;
  except
    on E: Exception do
      Error.Add('Creature.Move', E.Message);
  end;
end;

procedure TCreature.SetLook(const Value: TPoint);
begin
  FLook := Value;
end;

procedure TCreature.IncLook(X, Y: Integer);
begin
  Inc(FLook.X, X);
  Inc(FLook.Y, Y);
end;

procedure TCreature.SetAI(const Value: TAI);
begin
  FAI := Value;
end;

end.
