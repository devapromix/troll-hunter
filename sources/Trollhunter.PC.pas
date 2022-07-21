unit Trollhunter.PC;

interface

uses
  Classes,
  Trollhunter.Effect,
  Trollhunter.Creature,
  Trollhunter.Inv,
  Trollhunter.Skill,
  Trollhunter.TempSys,
  Trollhunter.RandItems,
  Trollhunter.GlobalMap,
  Trollhunter.Statistics;

type
  TPC = class(TCreature)
  private
    FInv: TAdvInv;
    FSkill: TSkill;
    FTempSys: TTempSys;
    FRace: Integer;
    FDungeon: Integer;
    FLastTurns: Integer;
    FTurns: Integer;
    FRating: Integer;
    FKills: Integer;
    FF: TStringList;
    FDay: Integer;
    FMonth: Integer;
    FWeek: Integer;
    FYear: Integer;
    FEffects: TEffects;
    FScrolls: TRandItems;
    FPotions: TRandItems;
    FWorld: TGlobalMap;
    FStatistics: TStatistics;
    function GetText: string;
    procedure SetInv(const Value: TAdvInv);
    procedure SetSkill(const Value: TSkill);
    procedure SetTempSys(const Value: TTempSys);
    procedure SetRace(const Value: Integer);
    procedure SetDungeon(const Value: Integer);
    procedure SetLastTurns(const Value: Integer);
    procedure SetKills(const Value: Integer);
    procedure SetRating(const Value: Integer);
    procedure SetTurns(const Value: Integer);
    procedure SetDay(const Value: Integer);
    procedure SetMonth(const Value: Integer);
    procedure SetWeek(const Value: Integer);
    procedure SetYear(const Value: Integer);
    procedure SetText(const Value: string);
    procedure SetEffects(const Value: TEffects);
    procedure SetScrolls(const Value: TRandItems);
    procedure SetPotions(const Value: TRandItems);
  public
    procedure Calc;
    procedure Save;
    procedure Load;
    procedure Defeat;
    procedure Render;
    procedure Redraw;
    procedure DoTime();
    procedure AddStrength;
    procedure AddDexterity;
    procedure AddWill;
    procedure AddSpeed;
    procedure DetectTraps;
    procedure DoDetectTraps;
    procedure TrainSkill; overload;
    procedure TrainSkill(ID: TSkillEnum); overload;
    procedure Melee(I: Integer);
    procedure Ranged(I: Integer);
    function MaxExp(ALevel: Integer = 0): Integer;
    function AddExp(Value: Word): Boolean;
    procedure Move(AX, AY: Integer);
    procedure Wait;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    property Statistics: TStatistics read FStatistics write FStatistics;
    property Inv: TAdvInv read FInv write SetInv;
    property Skill: TSkill read FSkill write SetSkill;
    property Scrolls: TRandItems read FScrolls write SetScrolls;
    property Potions: TRandItems read FPotions write SetPotions;
    property World: TGlobalMap read FWorld write FWorld;
    property TempSys: TTempSys read FTempSys write SetTempSys;
    property Race: Integer read FRace write SetRace;
    property Dungeon: Integer read FDungeon write SetDungeon;
    property LastTurns: Integer read FLastTurns write SetLastTurns;
    property Rating: Integer read FRating write SetRating;
    property Turns: Integer read FTurns write SetTurns;
    property Kills: Integer read FKills write SetKills;
    property Day: Integer read FDay write SetDay;
    property Week: Integer read FWeek write SetWeek;
    property Month: Integer read FMonth write SetMonth;
    property Year: Integer read FYear write SetYear;
    property Text: string read GetText write SetText;
    property Effects: TEffects read FEffects write SetEffects;
    function GetRadius: Integer;
    procedure Portal;
    function GetSpeed: Integer;

  end;

implementation

uses
  Math,
  Windows,
  SysUtils,
  Graphics,
  Trollhunter.Error,
  Trollhunter.Graph,
  Trollhunter.Log,
  Trollhunter.Decorator,
  Trollhunter.Lang,
  Trollhunter.Item,
  Trollhunter.Scenes,
  Trollhunter.Scene.LevelUp,
  Trollhunter.Creatures,
  Trollhunter.Projectiles,
  Trollhunter.Utils,
  Trollhunter.Map,
  Trollhunter.Scene.Item,
  Trollhunter.Scene.Game,
  Trollhunter.Time,
  Trollhunter.Settings,
  Trollhunter.Screenshot,
  Trollhunter.Game,
  Trollhunter.Scene.Records,
  Trollhunter.Formulas;

{ TPC }

procedure TPC.Clear;
begin
  Prop.Decor := dtBlood;
  Prop.MinDamage := 1;
  Prop.MaxDamage := 2;
  Prop.Protect := 0;
  Prop.Radius := 7;
  Prop.Distance := 7;
  Prop.Strength := 15;
  Prop.Dexterity := 5;
  Prop.Will := 8;
  Prop.Speed := 10;
  Dungeon := 0;
  Race := 0;
  Rating := 0;
  Turns := 0;
  LastTurns := 0;
  Kills := 0;
  Day := 1;
  Week := 1;
  Month := 1;
  Year := 1;
  Inv.Clear;
  Skill.Clear;
  Statistics.Clear;
  Calc();
  Fill();
end;

constructor TPC.Create;
begin
  inherited Create;
  FF := TStringList.Create;
  TempSys := TTempSys.Create;
  World := TGlobalMap.Create;
  Inv := TAdvInv.Create;
  Skill := TSkill.Create;
  Statistics := TStatistics.Create;
  Effects := TEffects.Create;
  Scrolls := TRandItems.Create(RandomScrollsCount);
  Potions := TRandItems.Create(RandomPotionsCount);
  Clear;
end;

destructor TPC.Destroy;
begin
  World.Free;
  Potions.Free;
  Scrolls.Free;
  Effects.Free;
  TempSys.Free;
  Statistics.Free;
  Skill.Free;
  Inv.Free;
  FF.Free;
  inherited;
end;

procedure TPC.Redraw;
var
  I: Integer;
  L: TCategory;
  B, D: Graphics.TBitmap;
begin
  try
    with Graph.Surface.Canvas do
    begin
      B := Graphics.TBitmap.Create;
      B.Handle := Windows.LoadBitmap(hInstance, 'PC');
      Graph.BitmapFromTileset(Image, B, Race);
      B.Free;
      for L := Low(L) to High(L) do
        for I := 1 to Inv.Count do
          if Inv.GetDoll(I) and (DungeonItems[Items.ItemIndex(I)].Category
            in WeapArmSet) and (DungeonItems[Items.ItemIndex(I)].Category = L)
          then
          begin
            D := Graphics.TBitmap.Create;
            B := Graphics.TBitmap.Create;
            B.Handle := Windows.LoadBitmap(hInstance, PChar(Inv.GetID(I)));
            Graph.BitmapFromTileset(D, B, 1);
            D.Transparent := True;
            Image.Canvas.Draw(0, 0, D);
            B.Free;
            D.Free;
          end;
      Image.Transparent := True;
    end;
  except
    on E: Exception do
      Error.Add('PC.Render', E.Message);
  end;
end;

procedure TPC.Load;
var
  ID, X, Y: Word;

  function GetName: string;
  begin
    Result := FF[ID];
    Inc(ID);
  end;

  function Get: Integer;
  begin
    Result := StrToInt(FF[ID]);
    Inc(ID);
  end;

begin
  try
    ID := 0;
    Name := GetName;
    X := Get;
    Y := Get;
    SetPosition(X, Y);
    Life.SetCur(Get);
    Mana.SetCur(Get);
    Dungeon := Get;
    Race := Get;
    Prop.Strength := Get;
    Prop.Dexterity := Get;
    Prop.Will := Get;
    Prop.Speed := Get;
    Prop.Level := Get;
    Prop.Exp := Get;
    Prop.MinDamage := Get;
    Prop.MaxDamage := Get;
    Prop.Protect := Get;
    Rating := Get;
    Turns := Get;
    LastTurns := Get;
    Kills := Get;
    Day := Get;
    Week := Get;
    Month := Get;
    Year := Get;
    Calc;
  except
    on E: Exception do
      Error.Add('PC.Load', E.Message);
  end;
end;

procedure TPC.Save;

  procedure Add(V: Integer); overload;
  begin
    FF.Append(IntToStr(V));
  end;

  procedure Add(V: string); overload;
  begin
    FF.Append(V)
  end;

begin
  try
    FF.Clear;
    //
    Add(Name);
    Add(Pos.X);
    Add(Pos.Y);
    Add(Life.Cur);
    Add(Mana.Cur);
    Add(Dungeon);
    Add(Race);
    //
    Add(Prop.Strength);
    Add(Prop.Dexterity);
    Add(Prop.Will);
    Add(Prop.Speed);
    //
    Add(Prop.Level);
    Add(Prop.Exp);
    Add(Prop.MinDamage);
    Add(Prop.MaxDamage);
    Add(Prop.Protect);
    //
    Add(Rating);
    Add(Turns);
    Add(LastTurns);
    Add(Kills);
    //
    Add(Day);
    Add(Week);
    Add(Month);
    Add(Year);
  except
    on E: Exception do
      Error.Add('PC.Save', E.Message);
  end;
end;

function TPC.MaxExp(ALevel: Integer): Integer;
var
  L: Integer;
begin
  if (ALevel = 0) then
    L := Prop.Level
  else
    L := ALevel;
  Result := L * ((L * 3) + 30);
end;

function TPC.AddExp(Value: Word): Boolean;
begin
  Result := False;
  try
    Prop.Exp := Prop.Exp + Value;
    Log.Add(Format(GetLang(64), [Value]));
    Self.Statistics.Inc(stKills);
    with Prop do
      while (Exp >= MaxExp) do
      begin
        Result := True;
        Level := Level + 1;
        Rating := Rating + (Level * 10);
        Log.Add(GetLang(60));
        Log.Add(Format(GetLang(61), [Level]));
        Scenes.Scene := SceneLevelUp;
      end;
  except
    on E: Exception do
      Error.Add('PC.AddExp', E.Message);
  end;
end;

procedure TPC.Move(AX, AY: Integer);
var
  I, V: Integer;
begin
  with Creatures do
    try
      // Move
      for I := 0 to High(Enemy) do
        if not Enemy[I].Life.IsMin and (Pos.X + AX = Enemy[I].Pos.X) and
          (Pos.Y + AY = Enemy[I].Pos.Y) then
        begin
          if not Items.IsRangedWeapon then
            Melee(I);
          Exit;
        end;
      inherited Move(AX, AY);
      if ((AX <> 0) or (AY <> 0)) then
        Look := Pos;
      if TempSys.IsVar('Poison') then
      begin
        V := TempSys.Power('Poison');
        with TAnimNumber.Create(-V) do
          Free;
        Life.Dec(V);
        Log.Add(Format(GetLang(70), [V, TempSys.Duration('Poison')]));
        if (TempSys.Duration('Poison') <= 1) then
          Log.Add(GetLang(71));
      end;
      if TempSys.IsVar('VialOfLife') and not Life.IsMax then
      begin
        V := TempSys.Power('VialOfLife');
        with TAnimNumber.Create(V) do
          Free;
        Life.Inc(V);
      end;
      if TempSys.IsVar('VialOfMana') and not Mana.IsMax then
      begin
        V := TempSys.Power('VialOfMana');
        with TAnimNumber.Create(V) do
          Free;
        Mana.Inc(V);
      end;
      Self.DetectTraps;
      Self.DoTime();
      Self.TempSys.Move;
      if Self.Life.IsMin then
        Log.Add(GetLang(72)); // You die.
    except
      on E: Exception do
        Error.Add('PC.Move', E.Message);
    end;
end;

procedure TPC.Melee(I: Integer);
var
  D, J: Integer;
  N: string;
begin
  with Creatures do
    try
      D := GetDamage(PC, Enemy[I].Prop.Protect);
      if (D > 0) and (Rand(1, Prop.Dexterity + Enemy[I].Prop.Dexterity) <=
        Prop.Dexterity) then
      begin
        N := GetCreatureLang(Enemy[I].Prop.Sprite);
        SetDamage(Enemy[I], N, D);
        Enemy[I].AI := aiCombat;
        TrainSkill();
        if Enemy[I].Life.IsMin then
        begin
          case Enemy[I].Prop.AIType of
            aiBigSlime:
              begin
                for J := 1 to 3 do
                  Insert(Enemy[I].Pos.X, Enemy[I].Pos.Y, 'SLIME');
              end;
            aiSlime:
              begin
                for J := 1 to 3 do
                  Insert(Enemy[I].Pos.X, Enemy[I].Pos.Y, 'SMALLSLIME');
              end;
          end;
          if (Rand(0, 9) = 0) then
            Items.Add(Enemy[I].Pos.X, Enemy[I].Pos.Y, Map.GetRandItemID);
          Log.Add(Format(GetLang(73), [N])); // The %s dies.
          if PC.AddExp(Enemy[I].Prop.Exp) then
            Log.Add(Format(GetLang(65), [PC.Prop.Level]));
          with PC do
            Rating := Rating + Enemy[I].Prop.Exp;
        end;
        if (Enemy[I].Prop.AIType = aiGoblin) then
        begin
          for D := 0 to High(Enemy) do
            if (Rand(1, 3) <= 2) then
              Enemy[D].AI := aiRun;
          Exit;
        end;
      end
      else
      begin
        Log.Add(Format(GetLang(74), [GetCreatureLang(Enemy[I].Prop.Sprite)]));
        // You miss the %s.
        case Enemy[I].Prop.AIType of
          aiSlug:
            begin
              Insert(Enemy[I].Pos.X, Enemy[I].Pos.Y, Enemy[I].Name, 5);
            end;
        end;
      end;
      if (Enemy[I].Prop.AIType in [aiGoblin, aiMelee, aiRanged]) then
        if (Enemy[I].AI <> aiRun) and (Rand(1, 2) = 1) and
          (Enemy[I].Life.Cur < (Enemy[I].Life.Max div 2)) and
          ((Enemy[I].Pos.X <> Enemy[I].Look.X) and
          (Enemy[I].Pos.X <> Enemy[I].Look.Y)) then
          Enemy[I].AI := aiRun;
    except
      on E: Exception do
        Error.Add('PC.Melee', E.Message);
    end;
end;

procedure TPC.Ranged(I: Integer);
var
  C, EX, EY: Integer;
  ProjID: string;
  P: TProjectile;

  procedure Rang(ProjID: string);
  var
    J: Integer;
  begin
    with Creatures.PC do
      for J := 1 to Inv.Count do
        if Inv.GetDoll(J) and (Inv.GetID(J) = ProjID) then
        begin
          if (Inv.GetCount(J) = 1) then
          begin
            SceneItem.UnEquip(J, False);
            CursorMode := cmNone;
            Calc();
            Redraw;
          end;
          Exit;
        end;
  end;

begin
  try
    with Items do
      with Creatures do
        if IsRangedWeapon then
        begin
          ProjID := GetDollItemID(ArmorSet);
          C := PC.Inv.GetCount(ProjID);
          if IsBow() then
            PC.Prop.Projectile := ptArrow;
          if IsCrossBow() then
            PC.Prop.Projectile := ptBolt;
          if (C > 0) then
          begin
            EX := Enemy[I].Pos.X;
            EY := Enemy[I].Pos.Y;
            P := TProjectile.Create(Self, Pos.X, Pos.Y, EX, EY);
            try
              // if (Rand(1, Prop.Dexterity + Enemy[I].Prop.Dexterity)
              // <= Prop.Dexterity) then
              // точность зависит от расстояния
              Melee(I);
              if (C = 1) then
                Rang(ProjID);
              PC.Inv.Del(ProjID);
              Wait();
              Exit;
            finally
              P.Free;
            end;
          end;
        end;
  except
    on E: Exception do
      Error.Add('PC.Ranged', E.Message);
  end;
end;

procedure TPC.SetText(const Value: string);
begin
  FF.Text := Value;
  Self.Load;
end;

procedure TPC.SetEffects(const Value: TEffects);
begin
  FEffects := Value;
end;

procedure TPC.SetScrolls(const Value: TRandItems);
begin
  FScrolls := Value;
end;

procedure TPC.SetPotions(const Value: TRandItems);
begin
  FPotions := Value;
end;

procedure TPC.Portal;
begin
  SceneGame.GoToPrevMap;
end;

procedure TPC.SetTurns(const Value: Integer);
begin
  FTurns := Value;
end;

procedure TPC.SetDay(const Value: Integer);
begin
  FDay := Value;
end;

procedure TPC.SetMonth(const Value: Integer);
begin
  FMonth := Value;
end;

procedure TPC.SetWeek(const Value: Integer);
begin
  FWeek := Value;
end;

procedure TPC.SetYear(const Value: Integer);
begin
  FYear := Value;
end;

function TPC.GetText: string;
begin
  Self.Save;
  Result := FF.Text;
end;

procedure TPC.SetTempSys(const Value: TTempSys);
begin
  FTempSys := Value;
end;

procedure TPC.SetRace(const Value: Integer);
begin
  FRace := Value;
end;

procedure TPC.SetDungeon(const Value: Integer);
begin
  FDungeon := Value;
end;

procedure TPC.SetLastTurns(const Value: Integer);
begin
  FLastTurns := Value;
end;

procedure TPC.SetKills(const Value: Integer);
begin
  FKills := Value;
end;

procedure TPC.SetRating(const Value: Integer);
begin
  FRating := Value;
end;

procedure TPC.DoDetectTraps;
var
  AX, AY: Integer;
begin
  for AX := Pos.X - 1 to Pos.X + 1 do
    for AY := Pos.Y - 1 to Pos.Y + 1 do
      if (Map.Cell[AY][AX].Decor = dTrap) then
      begin
        Map.Cell[AY][AX].Decor := dTrapDet;
        Log.Add(GetLang(109));
        TrainSkill(skTrap);
      end;
end;

procedure TPC.DetectTraps;
begin
  if (Rand(0, 100) < Skill.GetSkill(skTrap, True)) then
    DoDetectTraps;
end;

procedure TPC.SetInv(const Value: TAdvInv);
begin
  FInv := Value;
end;

procedure TPC.SetSkill(const Value: TSkill);
begin
  FSkill := Value;
end;

procedure TPC.TrainSkill(ID: TSkillEnum);
begin
  Skill.Up(ID);
end;

procedure TPC.Wait;
begin
  Move(0, 0);
  Creatures.Move;
  Log.Apply;
  Scenes.Render;
end;

procedure TPC.DoTime;
begin
  Turns := Turns + 1;
  Trollhunter.Time.DoTime();
end;

procedure TPC.TrainSkill;
var
  ID: string;
  CS: TSubCSet;
begin
  ID := Trim(Items.GetDollItemID(WeaponSet));
  if (ID = '') then
    Exit;
  CS := DungeonItems[Items.ItemIndex(ID)].SubCats;
  ///
  if (scDagger in CS) then
    Skill.Up(skDagger);
  if (scAxe in CS) then
    Skill.Up(skAxe);
  if (scSword in CS) then
    Skill.Up(skSword);
  if (scMace in CS) then
    Skill.Up(skMace);
  if (scSpear in CS) then
    Skill.Up(skSpear);
  if (scBow in CS) then
    Skill.Up(skBow);
  if (scCrossBow in CS) then
    Skill.Up(skCrossBow);
  if (scShield in CS) then
    Skill.Up(skShield);

  {
    '+Daggers and knives':3,
    '+Swords':3,
    '+Spears':3,
    '+Clubs, hammers and maces':3,
    '+Axes':3,
    '+Bows':2,
    '+Crossbows':2,
    '+Shield use':2
    'Dodge':1,
    'Two weapon fighting':2,
    'Whips':3,
    'Unarmed fighting':3,
    'Throwing':2,
  }

end;

procedure TPC.AddDexterity;
begin
  Inc(Prop.Dexterity);
  Log.Add(Format('%s +1 (%d).', [GetLang(16), Prop.Dexterity]));
  Calc;
end;

procedure TPC.AddSpeed;
begin
  Inc(Prop.Speed);
  Log.Add(Format('%s +1 (%d).', [GetLang(18), Prop.Speed]));
  Calc;
end;

procedure TPC.AddStrength;
begin
  Inc(Prop.Strength);
  Log.Add(Format('%s +1 (%d).', [GetLang(15), Prop.Strength]));
  Calc;
end;

procedure TPC.AddWill;
begin
  Inc(Prop.Will);
  Log.Add(Format('%s +1 (%d).', [GetLang(17), Prop.Will]));
  Calc;
end;

procedure TPC.Calc;
begin
  inherited Calc;
  AP.SetMax(GetMaxAP(GetSpeed));
  Inv.MaxCount := GetMaxCount(Prop.Strength);
  Inv.MaxWeight := GetMaxWeight(Prop.Strength);
  Mana.SetMax(GetMaxMana(Prop.Will) +
    GetAdvMana(Skill.GetSkill(skMagic, True)));
end;

procedure TPC.Render;
var
  TX, TY: Integer;
begin
  try
    with Graph do
    begin
      TX := TileSize * RW;
      TY := TileSize * RH + CharHeight;
      Surface.Canvas.Draw(TX, TY, Image);
      // Surface.Canvas.Draw(TX, TY, Effects.Image[0]);
    end;
  except
    on E: Exception do
      Error.Add('PC.Render', E.Message);
  end;
end;

function TPC.GetRadius: Integer;
begin
  Prop.Radius := Clamp(Prop.Radius, 0, 9);
  Result := Prop.Radius;
  if TempSys.IsVar('Blind') then
    Result := 1;
end;

function TPC.GetSpeed: Integer;
begin
  Result := Prop.Speed;
  if TempSys.IsVar('Webbed') then
    Result := Math.EnsureRange(Prop.Speed - TempSys.Power('Webbed'), 3,
      Prop.Speed);
end;

procedure TPC.Defeat;
var
  S: TSettings;
begin
  try
    Life.SetToMin;
    Mana.SetToMin;
    TakeScreenShot(False);
    Rating := Rating + (Creatures.PC.Turns div 100);
    if (Rating > 0) then
      Game.Scores.Add(Rating, Name, DateTimeToStr(Now), Prop.Level,
        Dungeon, Turns);
    DelFile(Name);
    Self.Clear;
    // Create;  {!!!!!!!!!!!!!!!!!!}
    Scenes.Scene := SceneRecords;
    S := TSettings.Create;
    try
      S.Write('Settings', 'LastName', '');
    finally
      S.Free;
    end;
  except
    on E: Exception do
      Error.Add('PC.Defeat', E.Message);
  end;
end;

end.
