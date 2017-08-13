unit uPlayer;

interface

uses uEntity, uMob;

type
  TSkillEnum = (skLearning,
    // Attributes skills
    skAthletics, skDodge, skConcentration, skToughness,
    // Weapon skills
    skBlade, skAxe, skSpear, skMace,
    // Skills
    skStealth, skHealing);

type
  TSkill = record
    Value: Integer;
    Exp: Integer;
  end;

type
  TEffect = (efHeal, efFullHeal, efMana, efFullMana, efFood, efTeleportation,
    efTownPortal, efMagicEye, efCurePoison);
  TEffects = set of TEffect;

type
  TTalentEnum = (tlNone, tlStrong {Сильный}, tlDextrous {Ловкий}, tlTough {Тяжелый},
    tlWealthy {Богатый}, tlMiser {Скряга});

const
  // Player
  AtrMax = 100;
  RadiusMax = 15;
  DVMax = 80;
  PVMax = 250;
  LevelExpMax = 8;
  // Skills
  SkillMin = 5;
  SkillMax = 75;
  SkillExpMax = 50;
  // Satiation
  StarvingMax = 500;
  SatiatedMax = 8000;
  EngorgedMax = 15000;
  // Inventory
  ItemMax = 26;
  StartGold = 250;

type
  TPlayer = class(TEntity)
  private
    FLX: Byte;
    FLY: Byte;
    FTurn: Word;
    FSatiation: Word;
    FLevel: Byte;
    FMana: Word;
    FMaxMana: Word;
    FRadius: Byte;
    FDV: Byte;
    FPV: Byte;
    FExp: Byte;
    FMaxMap: Byte;
    FLook: Boolean;
    FStrength: Byte;
    FDexterity: Byte;
    FWillpower: Byte;
    FPerception: Byte;
    FGold: Integer;
    FScore: Word;
    FKills: Word;
    FSpCast: Word;
    FFound: Word;
    FPotDrunk: Word;
    FScrRead: Word;
    FKiller: string;
    FSkill: array [TSkillEnum] of TSkill;
    FWeaponSkill: TSkillEnum;
    FItemIsDrop: Boolean;
    FItemIndex: Integer;
    FItemAmount: Integer;
    FSatPerTurn: Byte;
    FIsRest: Boolean;
    FTalent: TTalentEnum;
    FName: string;
    procedure GenNPCText;
    function GetDV: Byte;
    function GetPV: Byte;
    function GetRadius: Byte;
    function GetSatiation: Word;
  public
    constructor Create;
    destructor Destroy; override;
    property LX: Byte read FLX write FLX;
    property LY: Byte read FLY write FLY;
    property Turn: Word read FTurn write FTurn;
    property Satiation: Word read GetSatiation write FSatiation; // Nutrition
    property Level: Byte read FLevel write FLevel;
    property Mana: Word read FMana write FMana;
    property MaxMana: Word read FMaxMana write FMaxMana;
    property Radius: Byte read GetRadius write FRadius;
    property DV: Byte read GetDV write FDV;
    property PV: Byte read GetPV write FPV;
    property Exp: Byte read FExp write FExp;
    property MaxMap: Byte read FMaxMap write FMaxMap;
    property Look: Boolean read FLook write FLook;
    property Strength: Byte read FStrength write FStrength;
    property Dexterity: Byte read FDexterity write FDexterity;
    property Willpower: Byte read FWillpower write FWillpower;
    property Perception: Byte read FPerception write FPerception;
    property Gold: Integer read FGold write FGold;
    property Score: Word read FScore write FScore;
    property Kills: Word read FKills write FKills;
    property Found: Word read FFound write FFound;
    property Talent: TTalentEnum read FTalent write FTalent;
    property PotDrunk: Word read FPotDrunk write FPotDrunk;
    property ScrRead: Word read FScrRead write FScrRead;
    property Killer: string read FKiller write FKiller;
    property SpCast: Word read FSpCast write FSpCast;
    property IsRest: Boolean read FIsRest write FIsRest;
    property ItemIsDrop: Boolean read FItemIsDrop write FItemIsDrop;
    property ItemIndex: Integer read FItemIndex write FItemIndex;
    property ItemAmount: Integer read FItemAmount write FItemAmount;
    property SatPerTurn: Byte read FSatPerTurn write FSatPerTurn;
    property Name: string read FName write FName;
    procedure SetAmountScene(IsDrop: Boolean; Index, Amount: Integer);
    procedure Render(AX, AY: Byte);
    procedure Move(AX, AY: ShortInt);
    procedure Calc;
    procedure Fill;
    procedure Wait;
    procedure AddTurn;
    function GetSatiationStr: string;
    function SaveCharacterDump(AReason: string): string;
    procedure Skill(ASkill: TSkillEnum; AExpValue: Byte = 1);
    function GetSkill(ASkill: TSkillEnum): TSkill;
    function GetSkillName(ASkill: TSkillEnum): string;
    function GetSkillValue(ASkill: TSkillEnum): Byte;
    function GetTalentName(ATalent: TTalentEnum): string;
    procedure Defeat(AKiller: string);
    procedure Attack(Index: Integer);
    procedure ReceiveHealing;
    procedure Buy(Index: Integer);
    procedure PickUp;
    procedure PickUpAmount(Index: Integer);
    procedure Drop(Index: Integer);
    procedure DropAmount(Index: Integer);
    procedure Use(Index: Integer);
    procedure DoEffects(const Effects: TEffects; const Value: Word);
    procedure Equip(Index: Integer);
    procedure UnEquip(Index: Integer);
    procedure Sell(Index: Integer);
    procedure Repair(Index: Integer);
    procedure AddExp(Value: Byte = 1);
    procedure SkillSet;
    procedure StarterSet;
    procedure Rest(ATurns: Word);
    procedure Dialog(AMob: TMob);
    procedure AutoPickup();
  end;

var
  Player: TPlayer = nil;

implementation

uses Classes, SysUtils, Dialogs, Math, IniFiles, uGame, uMap, uScenes,
  uTerminal, uMsgLog, GNUGetText, BeaRLibItems, uItem, uCorpse, uCalendar,
  uShop;

{ TPlayer }

procedure TPlayer.AddTurn;
var
  V: Byte;
begin
  Turn := Turn + 1;
  Calendar.Turn;
  if (Satiation > 0) then
    Satiation := Satiation - SatPerTurn;
  if (Satiation < StarvingMax) then
  begin
    Life := EnsureRange(Life - 1, 0, MaxLife);;
  end
  else
  begin
    V := EnsureRange(100 - Player.GetSkillValue(skHealing), 25, 100);
    if (Turn mod V = 0) then
      Life := EnsureRange(Life + Player.GetSkillValue(skHealing), 0, MaxLife);
    V := EnsureRange(100 - Player.GetSkillValue(skConcentration), 25, 100);
    if (Turn mod V = 0) then
      Mana := EnsureRange(Mana + Player.GetSkillValue(skConcentration),
        0, MaxMana);
  end;
  Mobs.Process;
end;

procedure TPlayer.Attack(Index: Integer);
var
  V, Ch: Byte;
  Mob: TMob;
  Dam, Cr: Word;
  CrStr, The: string;
begin
  if (Index < 0) then
    Exit;
  Mob := Mobs.Mob[Index];
  if not Mob.Alive then
    Exit;
  if (Mob.Force <> fcEnemy) then
  begin
    Self.Dialog(Mob);
    GenNPCText;
    Exit;
  end;
  The := GetDescThe(Mobs.Name[TMobEnum(Mob.ID)]);
  if (MobBase[TMobEnum(Mob.ID)].DV < Math.RandomRange(0, 100)) then
  begin
    CrStr := '';
    // Attack
    Dam := EnsureRange(RandomRange(Self.Damage.Min, Self.Damage.Max + 1), 0,
      High(Word));
    // Critical hits...     .
    Ch := Math.RandomRange(0, 100);
    Cr := Self.GetSkillValue(FWeaponSkill);
    if (Ch < Cr) then
    begin
      if (Ch > (Cr div 10)) then
      begin
        V := 2;
        CrStr := _('It was a good hit!');
      end
      else
      begin
        V := 3;
        CrStr := _('It was an excellent hit!');
      end;
      Dam := Dam * V;
      CrStr := CrStr + Format(' (%dx)', [V]);
    end;
    // Attack
    Mob.Life := EnsureRange(Mob.Life - Dam, 0, High(Word));
    MsgLog.Add(Format(_('You hit %s (%d).'), [The, Dam]));
    if (CrStr <> '') then
      MsgLog.Add(Format(FC, [clAlarm, CrStr]));
    case FWeaponSkill of
      skBlade:
        begin
          Skill(FWeaponSkill, 2);
          Skill(skAthletics, 2);
          Skill(skDodge, 2);
          SatPerTurn := 5;
        end;
      skAxe:
        begin
          Skill(FWeaponSkill, 2);
          Skill(skAthletics, 3);
          Skill(skDodge);
          SatPerTurn := 6;
        end;
      skSpear:
        begin
          Skill(FWeaponSkill, 2);
          Skill(skAthletics);
          Skill(skDodge, 3);
          SatPerTurn := 4;
        end;
      skMace:
        begin
          Skill(FWeaponSkill, 2);
          Skill(skAthletics, 4);
          SatPerTurn := 7;
        end;
    end;
    // Victory
    if (Mob.Life = 0) then
    begin
      Mob.Defeat;
    end;
  end
  else
  begin
    // Miss
    MsgLog.Add(Format(_('You miss %s.'), [The]));
    // MsgLog.Add(Format(_('You fail to hurt %s.'), [The]));
    SatPerTurn := 3;
  end;
  AddTurn;
end;

procedure TPlayer.AutoPickup;
var
  Index, FCount: Integer;
  ItemType: TItemType;
  FItem: Item;
begin
  FCount := EnsureRange(Items_Dungeon_GetMapCountXY(Ord(Map.Current), X, Y),
    0, ItemMax);
  for Index := FCount - 1 downto 0 do
  begin
    FItem := Items_Dungeon_GetMapItemXY(Ord(Map.Current), Index, X, Y);
    ItemType := ItemBase[TItemEnum(FItem.ItemID)].ItemType;
    if (ItemType in AutoPickupItems) then
    begin
      if ((ItemType = itCoin) and not Game.APCoin) then
        Exit;
      if ((ItemType = itFood) and not Game.APFood) then
        Exit;
      if ((ItemType = itPotion) and not Game.APPotion) then
        Exit;
      if ((ItemType = itScroll) and not Game.APScroll) then
        Exit;
      Items.AddItemToInv(Index, True);
      Wait;
    end;
  end;
end;

procedure TPlayer.Calc;
var
  I, FCount, Def: Integer;
  Dam: TDamage;
  FI: TItemEnum;
  FItem: Item;
begin
  Dam.Min := 0;
  Dam.Max := 0;
  Def := 0;
  FCount := EnsureRange(Items_Inventory_GetCount(), 0, ItemMax);
  for I := 0 to FCount - 1 do
  begin
    FItem := Items_Inventory_GetItem(I);
    if (FItem.Equipment > 0) then
    begin
      FI := TItemEnum(FItem.ItemID);
      Dam.Min := Dam.Min + FItem.MinDamage;
      Dam.Max := Dam.Max + FItem.MaxDamage;
      Def := Def + FItem.Defense;
      if (ItemBase[FI].SlotType = stMainHand) then
        case ItemBase[FI].ItemType of
          itBlade:
            FWeaponSkill := skBlade;
          itAxe:
            FWeaponSkill := skAxe;
          itSpear:
            FWeaponSkill := skSpear;
          itMace:
            FWeaponSkill := skMace;
        else
          FWeaponSkill := skLearning;
        end;
    end;
  end;
  //
  Self.Gold := EnsureRange(Items_Inventory_GetItemAmount(Ord(iGold)), 0,
    High(Integer));
  //
  Strength := EnsureRange(Round(FSkill[skAthletics].Value * 0.5) +
    Round(FSkill[skToughness].Value * 0.9), 1, AtrMax);
  Dexterity := EnsureRange(Round(FSkill[skDodge].Value * 1.4), 1, AtrMax);
  Willpower := EnsureRange(Round(FSkill[skConcentration].Value * 1.4),
    1, AtrMax);
  Perception := EnsureRange(Round(FSkill[skToughness].Value * 1.4), 1, AtrMax);
  //
  DV := EnsureRange(Round(Dexterity * (DVMax / AtrMax)), 0, DVMax);
  PV := EnsureRange(Round(FSkill[skToughness].Value / 1.4) - 4 + Def, 0, PVMax);
  MaxLife := Round(Strength * 3.6) + Round(Dexterity * 2.3);
  MaxMana := Round(Willpower * 4.2) + Round(Dexterity * 0.4);
  Radius := Round(Perception / 8.3);
  //
  Self.SetDamage(EnsureRange(Dam.Min + Strength div 3, 1, High(Byte) - 1),
    EnsureRange(Dam.Max + Strength div 2, 2, High(Byte)));
end;

constructor TPlayer.Create;
var
  I: TSkillEnum;
begin
  inherited;
  Exp := 0;
  Turn := 0;
  Gold := 0;
  Score := 0;
  Kills := 0;
  SpCast := 0;
  Found := 0;
  PotDrunk := 0;
  ScrRead := 0;
  Level := 1;
  MaxMap := 0;
  Killer := '';
  Alive := True;
  Look := False;
  IsRest := False;
  Name := _('PLAYER');
  SatPerTurn := 2;
  Talent := tlNone;
  Satiation := SatiatedMax;
  FWeaponSkill := skLearning;
  for I := Low(TSkillEnum) to High(TSkillEnum) do
    with FSkill[I] do
    begin
      Value := SkillMin;
      Exp := 0;
    end;
  Self.Calc;
  Self.Fill;
end;

procedure TPlayer.Defeat(AKiller: string);
begin
  Killer := AKiller;
  MsgLog.Add(Format(FC, [clAlarm, _('You die...')]));
  Corpses.Append();
  Game.Screenshot := Terminal.GetTextScreenshot();
end;

destructor TPlayer.Destroy;
begin

  inherited;
end;

procedure TPlayer.Dialog(AMob: TMob);
begin
  Game.Timer := High(Byte);
  NPCName := Mobs.Name[TMobEnum(AMob.ID)];
  NPCType := MobBase[TMobEnum(AMob.ID)].NPCType;
  Scenes.SetScene(scDialog);
end;

procedure TPlayer.Fill;
begin
  Life := MaxLife;
  Mana := MaxMana;
end;

procedure TPlayer.GenNPCText;
var
  S: string;
begin
  case Math.RandomRange(0, 3) of
    0:
      S := _('What can I do for you?');
    1:
      S := _('What can I get you today?');
  else
    S := _('Good day!');
  end;
  MsgLog.Add(Format(_('%s says: "%s"'), [NPCName, S]));
end;

function TPlayer.GetDV: Byte;
begin
  Result := EnsureRange(FDV, 0, DVMax);
end;

function TPlayer.GetPV: Byte;
begin
  Result := EnsureRange(FPV, 0, PVMax);
end;

function TPlayer.GetRadius: Byte;
begin
  Result := EnsureRange(FRadius + 3, 1, RadiusMax);
end;

function TPlayer.GetSatiationStr: string;
begin
  Result := '';
  case Satiation of
    0 .. StarvingMax:
      Result := _('Starving');
    StarvingMax + 1 .. 1500:
      Result := _('Near starving');
    1501 .. 2000:
      Result := _('Very hungry');
    2001 .. 2500:
      Result := _('Hungry');
    SatiatedMax + 1 .. 10000:
      Result := _('Full');
    10001 .. 11000:
      Result := _('Very full');
    11001 .. EngorgedMax:
      Result := _('Engorged');
  end;
  if Game.Wizard then
  begin
    if (Result = '') then
      Result := _('Satiated');
    Result := Result + Format(' (%d)', [Satiation]);
  end;
  case Satiation of
    0 .. StarvingMax:
      Result := Format('[color=light red]%s[/color]', [Result]);
    StarvingMax + 1 .. SatiatedMax:
      Result := Format('[color=light yellow]%s[/color]', [Result]);
  else
    Result := Format('[color=light green]%s[/color]', [Result]);
  end;
end;

function TPlayer.GetSatiation: Word;
begin
  Result := EnsureRange(FSatiation, 0, EngorgedMax);
end;

function TPlayer.GetSkill(ASkill: TSkillEnum): TSkill;
begin
  Result := FSkill[ASkill];
end;

function TPlayer.GetSkillName(ASkill: TSkillEnum): string;
begin
  case ASkill of
    skLearning:
      Result := _('Learning');
    // Attributes skills
    skAthletics:
      Result := _('Athletics');
    skDodge:
      Result := _('Dodge');
    skConcentration:
      Result := _('Concentration');
    skToughness:
      Result := _('Toughness');
    // Weapon skills
    skBlade:
      Result := _('Blade');
    skAxe:
      Result := _('Axe');
    skSpear:
      Result := _('Spear');
    skMace:
      Result := _('Mace');
    // Skills
    skStealth:
      Result := _('Stealth');
    skHealing:
      Result := _('Healing');
  end;
end;

function TPlayer.GetSkillValue(ASkill: TSkillEnum): Byte;
begin
  try
    Result := FSkill[ASkill].Value;
  except
    Result := 0;
  end;
end;

function TPlayer.GetTalentName(ATalent: TTalentEnum): string;
begin
  case ATalent of
    tlStrong:
      Result := _('Strong');
    tlDextrous:
      Result := _('Dextrous');
    tlTough:
      Result := _('Tough');
    tlWealthy:
      Result := _('Wealthy');
    tlMiser:
      Result := _('Miser');
    else
      Result := '-';
  end;
end;

procedure TPlayer.Move(AX, AY: ShortInt);
var
  FX, FY: Byte;
begin
  if Look then
  begin
    if Map.InMap(LX + AX, LY + AY) and
      ((Map.InView(LX + AX, LY + AY) and not Map.GetFog(LX + AX, LY + AY)) or
      Game.Wizard) then
    begin
      LX := Map.EnsureRange(LX + AX);
      LY := Map.EnsureRange(LY + AY);
    end;
  end
  else
  begin
    FX := Map.EnsureRange(X + AX);
    FY := Map.EnsureRange(Y + AY);
    if (Map.GetTileEnum(FX, FY, Map.Current) in StopTiles) and not Game.Wizard
    then
      Exit;
    if not Mobs.GetFreeTile(FX, FY) then
    begin
      Self.Attack(Mobs.GetIndex(FX, FY));
    end
    else
    begin
      X := FX;
      Y := FY;
      if ((AX <> 0) or (AY <> 0)) then
      begin
        SatPerTurn := 2;
        AutoPickup;
      end;
      AddTurn;
    end;
  end;
end;

procedure TPlayer.Use(Index: Integer);
var
  The: string;
  AItem: Item;
  I: TItemEnum;
  T: TItemType;
  ItemLevel: Byte;
begin
  AItem := Items_Inventory_GetItem(Index);
  // Need level
  ItemLevel := ItemBase[TItemEnum(AItem.ItemID)].Level;
  if (Player.Level < ItemLevel) and not Game.Wizard then
  begin
    MsgLog.Add(Format(_('You can not use this yet (need level %d)!'),
      [ItemLevel]));
    Self.Calc;
    Exit;
  end;
  I := TItemEnum(AItem.ItemID);
  T := ItemBase[I].ItemType;
  if (T in NotEquipTypeItems) then
  begin
    if (T in UseTypeItems) then
    begin
      if not (T in RuneTypeItems) then
        AItem.Amount := AItem.Amount - 1;
      The := GetDescThe(Items.Name[I]);
      case T of
        itPotion:
          begin
            MsgLog.Add(Format(_('You drink %s.'), [The]));
            PotDrunk := PotDrunk + 1;
          end;
        itScroll:
          begin
            MsgLog.Add(Format(_('You read %s.'), [The]));
            ScrRead := ScrRead + 1;
          end;
        itFood:
          MsgLog.Add(Format(_('You ate %s.'), [The]));
      end;
      if not (T in RuneTypeItems) then
        Items_Inventory_SetItem(Index, AItem);
      if (T in ScrollTypeItems + RuneTypeItems) then
      begin
        if (Self.Mana >= ItemBase[I].ManaCost) then
        begin
          Player.Skill(skConcentration);
          Self.Mana := Self.Mana - ItemBase[I].ManaCost;
          SpCast := SpCast + 1;
        end
        else
        begin
          MsgLog.Add(_('You need more mana!'));
          Self.Calc;
          Wait;
          Exit;
        end;
      end;
      DoEffects(ItemBase[I].Effects, ItemBase[I].Value);
      Self.Calc;
      Wait;
    end;
  end
  else
  begin
    // Equip or unequip an item
    case AItem.Equipment of
      0:
        Self.Equip(Index);
      1:
        Self.UnEquip(Index);
    end;
  end;
  // MsgLog.Add(Format(_('You don''t know how to use %s.'), [The]));
end;

procedure TPlayer.Equip(Index: Integer);
var
  The: string;
  AItem: Item;
  I: Integer;
  ItemLevel: Byte;
begin
  // Need level
  AItem := Items_Inventory_GetItem(Index);
  ItemLevel := ItemBase[TItemEnum(AItem.ItemID)].Level;
  if (Player.Level < ItemLevel) and not Game.Wizard then
  begin
    MsgLog.Add(Format(_('You can not use this yet (need level %d)!'),
      [ItemLevel]));
    Self.Calc;
    Exit;
  end;
  // Replace
  I := Items_Inventory_EquipItem(Index);
  if (I > -1) then
    UnEquip(I);
  // Equip
  The := GetDescThe(Items.Name[Items.GetItemEnum(AItem.ItemID)]);
  MsgLog.Add(Format(_('You equip %s.'), [The]));
  Self.Calc;
  Wait;
end;

procedure TPlayer.UnEquip(Index: Integer);
var
  The: string;
  AItem: Item;
begin
  if (Items_Inventory_UnEquipItem(Index) > 0) then
  begin
    AItem := Items_Inventory_GetItem(Index);
    The := GetDescThe(Items.Name[Items.GetItemEnum(AItem.ItemID)]);
    MsgLog.Add(Format(_('You unequip %s.'), [The]));
    Self.Calc;
    Wait;
  end;
end;

procedure TPlayer.Sell(Index: Integer);
var
  Value: Integer;
  AItem: Item;
  The: string;
begin
  AItem := Items_Inventory_GetItem(Index);
  if ((AItem.Equipment > 0) or (AItem.Stack > 1) or (AItem.Amount > 1)) then
    Exit;
  if (Items_Inventory_DeleteItem(Index, AItem) > 0) then
  begin
    Value := Items.GetPrice(AItem) div 4;
    Items.AddItemToInv(iGold, Value);
    The := GetDescThe(Items.Name[TItemEnum(AItem.ItemID)]);
    MsgLog.Add(Format(_('You sold %s (+%d gold).'), [The, Value]));
  end;
  Self.Calc;
end;

procedure TPlayer.Buy(Index: Integer);
var
  Price: Word;
  AItem: Item;
  The: string;
begin
  AItem := Shops.Shop[Shops.Current].GetItem(Index);
  Price := Items.GetPrice(AItem);
  if (Items_Inventory_DeleteItemAmount(Ord(iGold), Price) > 0) then
  begin
    The := GetDescThe(Items.Name[TItemEnum(AItem.ItemID)]);
    MsgLog.Add(Format(_('You bought %s (-%d gold).'), [The, Price]));
    Items_Inventory_AppendItem(AItem);
    Self.Calc;
    // The %s just frowns. Maybe you'll return when you have enough gold?
  end
  else
    MsgLog.Add(_('You need more gold.'));
end;

procedure TPlayer.ReceiveHealing;
var
  Cost: Word;
begin
  Cost := MaxLife - Life;
  if (Self.Gold >= Cost) then
  begin
    if (Items_Inventory_DeleteItemAmount(Ord(iGold), Cost) > 0) then
    begin
      Life := MaxLife;
      MsgLog.Add(Format(_('You feel better (-%d gold).'), [Cost]));
    end;
  end
  else
    MsgLog.Add(_('You need more gold.'));
  Self.Calc;
end;

procedure TPlayer.Repair(Index: Integer);
var
  RepairCost: Word;
  AItem: Item;
  The: string;
begin
  AItem := Items_Inventory_GetItem(Index);
  if ((AItem.Stack > 1) or (AItem.Amount > 1)) then
    Exit;
  RepairCost := (AItem.MaxDurability - AItem.Durability) * 10;
  if (RepairCost > 0) then
  begin
    if (Gold < RepairCost) then
    begin
      MsgLog.Add(_('You need more gold.'));
      Exit;
    end;
    AItem.Durability := AItem.MaxDurability;
    if ((Items_Inventory_DeleteItemAmount(Ord(iGold), RepairCost) > 0) and
      (Items_Inventory_SetItem(Index, AItem) > 0)) then
    begin
      The := GetDescThe(Items.Name[TItemEnum(AItem.ItemID)]);
      MsgLog.Add(Format(_('You repaired %s (-%d gold).'), [The, RepairCost]));
    end;
  end;
  Self.Calc;
end;

procedure TPlayer.Drop(Index: Integer);
var
  AItem: Item;

  procedure DeleteItem;
  var
    The: string;
  begin
    if (Items_Inventory_DeleteItem(Index, AItem) > 0) then
    begin
      AItem.X := Player.X;
      AItem.Y := Player.Y;
      AItem.Equipment := 0;
      AItem.MapID := Ord(Map.Current);
      Items_Dungeon_AppendItem(AItem);
      The := GetDescThe(Items.Name[TItemEnum(AItem.ItemID)]);
      MsgLog.Add(Format(_('You drop %s.'), [The]));
      Wait;
    end;
  end;

begin
  AItem := Items_Inventory_GetItem(Index);
  if (AItem.Equipment > 0) then
    Exit;
  if not((AItem.Stack > 1) and (AItem.Amount > 1)) then
    DeleteItem
  else
    Player.SetAmountScene(True, Index, 1);
  Self.Calc;
end;

procedure TPlayer.DropAmount(Index: Integer);
var
  FItem: Item;
  The: string;
begin
  FItem := Items_Inventory_GetItem(Index);
  FItem.Amount := FItem.Amount - Player.ItemAmount;
  Items_Inventory_SetItem(Index, FItem);
  FItem.X := Player.X;
  FItem.Y := Player.Y;
  FItem.Equipment := 0;
  FItem.MapID := Ord(Map.Current);
  FItem.Amount := Player.ItemAmount;
  Items_Dungeon_AppendItem(FItem);
  The := GetDescThe(Items.Name[TItemEnum(FItem.ItemID)]);
  if (FItem.Amount > 1) then
    MsgLog.Add(Format(_('You drop %s (%dx).'), [The, FItem.Amount]))
  else
    MsgLog.Add(Format(_('You drop %s.'), [The]));
  Scenes.SetScene(scDrop);
  Wait;
end;

procedure TPlayer.PickUp;
var
  FCount: Integer;
begin
  Inc(FFound);
  Corpses.DelCorpse(Player.X, Player.Y);
  /// / Your backpack is full!
  FCount := Items_Dungeon_GetMapCountXY(Ord(Map.Current), Player.X, Player.Y);
  if (FCount > 0) then
  begin
    if (FCount = 1) then
    begin
      // Pickup an item
      Items.AddItemToInv(0);
    end
    else
    begin
      // Items scene
      Game.Timer := High(Byte);
      Scenes.SetScene(scItems);
    end;
  end;
end;

procedure TPlayer.PickUpAmount(Index: Integer);
var
  FItem: Item;
  The: string;
begin
  FItem := Items_Dungeon_GetMapItemXY(Ord(Map.Current), Index, Player.X,
    Player.Y);
  FItem.Amount := FItem.Amount - Player.ItemAmount;
  Items_Dungeon_SetMapItemXY(Ord(Map.Current), Index, Player.X,
    Player.Y, FItem);
  FItem.Amount := Player.ItemAmount;
  Items_Inventory_AppendItem(FItem);
  The := GetDescThe(Items.Name[TItemEnum(FItem.ItemID)]);
  if (FItem.Amount > 1) then
    MsgLog.Add(Format(_('You picked up %s (%dx).'), [The, FItem.Amount]))
  else
    MsgLog.Add(Format(_('You picked up %s.'), [The]));
  Scenes.SetScene(scItems);
  Wait;
end;

procedure TPlayer.Render(AX, AY: Byte);
begin
  if (Self.Life = 0) then
    Terminal.Print(AX + View.Left, AY + View.Top, '%', clCorpse)
  else
    Terminal.Print(AX + View.Left, AY + View.Top, '@', clPlayer, clBkPlayer);
end;

function TPlayer.SaveCharacterDump(AReason: string): string;
var
  SL: TStringList;

  function GetDateTime(DateSep: Char = '.'; TimeSep: Char = ':'): string;
  begin
    Result := DateToStr(Date) + '-' + TimeToStr(Time);
    Result := StringReplace(Result, '.', DateSep, [rfReplaceAll]);
    Result := StringReplace(Result, ':', TimeSep, [rfReplaceAll]);
  end;

begin
  if Game.Wizard then
    Exit;
  SL := TStringList.Create;
  try
    SL.Append(Format(FT, [Game.GetTitle]));
    SL.Append('');
    SL.Append(GetDateTime);
    SL.Append('');
    SL.Append(AReason);
    if Player.IsDead then
      SL.Append(Format(_('He scored %d points.'), [Player.Score]))
    else
      SL.Append(Format(_('He has scored %d points so far.'), [Player.Score]));
    SL.Append('');
    SL.Append(Format(FT, [_('Screenshot')]));
    SL.Append(Game.Screenshot);
    SL.Append(Format(FT, [_('Defeated foes')]));
    SL.Append('');
    SL.Append(Format('Total: %d creatures defeated.', [Player.Kills]));
    SL.Append('');
    SL.Append(Format(FT, [_('Last messages')]));
    SL.Append('');
    SL.Append(GetPureText(MsgLog.GetLastMsg(10)));
    SL.Append(Format(FT, [_('Inventory')]));
    SL.Append('');
    SL.Append(GetPureText(Items.GetInventory));
    SL.Append(Format('%s: %d', [_('Gold'), Player.Gold]));
    SL.SaveToFile(GetDateTime('-', '-') + '-character-dump.txt');
  finally
    SL.Free;
  end;
end;

procedure TPlayer.SetAmountScene(IsDrop: Boolean; Index, Amount: Integer);
begin
  ItemIsDrop := IsDrop;
  ItemIndex := Index;
  ItemAmount := Amount;
  Scenes.SetScene(scAmount);
end;

procedure TPlayer.AddExp(Value: Byte = 1);
begin
  Exp := Exp + Value;
  if (Exp >= LevelExpMax) then
  begin
    Exp := Exp - LevelExpMax;
    FLevel := FLevel + 1;
    MsgLog.Add(Format(FC, [clAlarm, Format(_('You advance to level %d!'),
      [FLevel])]));
    Player.Score := Player.Score + (FLevel * FLevel);
  end;
end;

procedure TPlayer.Skill(ASkill: TSkillEnum; AExpValue: Byte = 1);
begin
  if (FSkill[ASkill].Value < SkillMax) then
  begin
    Inc(FSkill[ASkill].Exp, Math.RandomRange(0, AExpValue + 1) + 1);
    if (FSkill[ASkill].Exp >= SkillExpMax) then
    begin
      FSkill[ASkill].Exp := FSkill[ASkill].Exp - SkillExpMax;
      Inc(FSkill[ASkill].Value);
      FSkill[ASkill].Value := EnsureRange(FSkill[ASkill].Value, SkillMin,
        SkillMax);
      // Add message {!!!}
      MsgLog.Add(Format(FC, [clAlarm, Format('Your skill %s has raised to %d!',
        [Self.GetSkillName(ASkill), Self.GetSkillValue(ASkill)])]));
      // Add exp
      AddExp();
      // Add scores
      if (FSkill[ASkill].Value = SkillMax) then
        Player.Score := Player.Score + 50;
      Self.Calc;
    end;
  end;
end;

procedure TPlayer.SkillSet;
var
  I: TSkillEnum;
begin
  if not Game.Wizard then
    Exit;
  for I := Low(TSkillEnum) to High(TSkillEnum) do
    with FSkill[I] do
    begin
      Value := Math.RandomRange(SkillMin, SkillMax);
      Exp := Math.RandomRange(0, SkillExpMax);
    end;
  Self.Calc;
  Self.Fill;
end;

procedure TPlayer.Wait;
begin
  if not Map.GetVis(Map.Current) then
  begin
    MsgLog.Add(Format(FC, [clAlarm,
      Format(_('You have opened a new territory: %s.'), [Map.Name])]));
    Map.SetVis(Map.Current, True);
    if (Ord(Map.Current) > 0) then
      Score := Score + (Ord(Map.Current) * 15);
    MaxMap := MaxMap + 1;
  end;
  SatPerTurn := 1;
  Move(0, 0);
end;

procedure TPlayer.Rest(ATurns: Word);
var
  T: Word;
begin
  IsRest := True;
  MsgLog.Add(Format(_('Start rest (%d turns)!'), [ATurns]));
  for T := 1 to ATurns do
  begin
    if not IsRest then
      Break;
    Wait;
  end;
  MsgLog.Add(Format(_('Finish rest (%d turns)!'), [T - 1]));
  IsRest := False;
end;

procedure TPlayer.StarterSet;
var
  D: Byte;
begin
  // Add armors
  if Game.Wizard then
  begin
    Items.AddItemToInv(iWingedHelm, 1, True);
    Items.AddItemToInv(iPlateMail, 1, True);
  end
  else
  begin
    Items.AddItemToInv(iCap, 1, True);
    Items.AddItemToInv(iQuiltedArmor, 1, True);
  end;
  // Add weapon
  if Game.Wizard then
  begin
    case Math.RandomRange(0, 4) of
      0:
        Items.AddItemToInv(iTrollSlayer, 1, True);
      1:
        Items.AddItemToInv(iDemonAxe, 1, True);
      2:
        Items.AddItemToInv(iHonedSpear, 1, True);
      3:
        Items.AddItemToInv(iDoomHammer, 1, True);
    end;
  end
  else
  begin
    case Math.RandomRange(0, 4) of
      0:
        Items.AddItemToInv(iRustySword, 1, True);
      1:
        Items.AddItemToInv(iHatchet, 1, True);
      2:
        Items.AddItemToInv(iShortSpear, 1, True);
      3:
        Items.AddItemToInv(iSlagHammer, 1, True);
    end;
  end;
  // Add runes, potions and scrolls
  if Game.Wizard then
  begin
    Items.AddItemToInv(iRuneOfFullHealing);
    Items.AddItemToInv(iPotionOfFullHealing, 10);
    Items.AddItemToInv(iPotionOfFullMana, 10);
  end
  else
  begin
    Items.AddItemToInv(iLesserHealingPotion, 5);
    Items.AddItemToInv(iLesserManaPotion, 5);
  end;
  // Add foods
  Items.AddItemToInv(iBreadRation, IfThen(Game.Wizard, 10, 3));
  // Add coins
  D := IfThen(Game.Difficulty <> dfHell, StartGold, 0);
  Items.AddItemToInv(iGold, IfThen(Game.Wizard, RandomRange(6666, 9999), D));
  Self.Calc;
end;

procedure TPlayer.DoEffects(const Effects: TEffects; const Value: Word);
var
  V, VX, VY: Word;
const
  F = '%s +%d.';
begin
  // Heal
  if (efHeal in Effects) then
  begin
    V := Self.GetSkillValue(skHealing) + Value;
    MsgLog.Add(_('You feel healthy.'));
    MsgLog.Add(Format(F, [_('Life'), Min(MaxLife - Life, V)]));
    Self.Life := EnsureRange(Self.Life + V, 0, MaxLife);
    Self.Skill(skHealing, 5);
  end;
  // Full heal
  if (efFullHeal in Effects) then
  begin
    MsgLog.Add(_('You feel like new.'));
    MsgLog.Add(Format(F, [_('Life'), MaxLife - Life]));
    Self.Life := MaxLife;
    Self.Skill(skHealing, 5);
  end;
  // Mana
  if (efMana in Effects) then
  begin
    V := Self.GetSkillValue(skConcentration) + Value;
    MsgLog.Add(_('You feel magical energies restoring.'));
    MsgLog.Add(Format(F, [_('Mana'), Min(MaxMana - Mana, V)]));
    Self.Mana := EnsureRange(Self.Mana + V, 0, MaxMana);
    Self.Skill(skConcentration, 5);
  end;
  // Full mana
  if (efFullMana in Effects) then
  begin
    MsgLog.Add(_('You feel magically fully restored.'));
    MsgLog.Add(Format(F, [_('Mana'), MaxMana - Mana]));
    Self.Mana := MaxMana;
    Self.Skill(skConcentration, 5);
  end;
  // Food
  if (efFood in Effects) then
  begin
    FSatiation := FSatiation + Value;
    MsgLog.Add(Format(_('You have sated %d hunger.'), [Value]));
  end;
  // Teleportation
  if (efTeleportation in Effects) then
  begin
    VX := Math.RandomRange(Value, Self.GetSkillValue(skConcentration) + Value);
    VY := Math.RandomRange(Value, Self.GetSkillValue(skConcentration) + Value);
    X := Map.EnsureRange(X + (Math.RandomRange(0, VX * 2 + 1) - VX));
    Y := Map.EnsureRange(Y + (Math.RandomRange(0, VY * 2 + 1) - VY));
    MsgLog.Add(_('You have teleported into new place!'));
    Scenes.SetScene(scGame);
  end;
  // Town Portal
  if (efTownPortal in Effects) then
  begin
    X := Game.Spawn.X;
    Y := Game.Spawn.Y;
    Map.Current := deDarkWood;
    MsgLog.Add(_('You have teleported into town!'));
    Scenes.SetScene(scGame);
  end;
  if (efMagicEye in Effects) then
  begin

  end;
  if (efCurePoison in Effects) then
  begin
  
  end;
end;

initialization

Player := TPlayer.Create;

finalization

FreeAndNil(Player);

end.
