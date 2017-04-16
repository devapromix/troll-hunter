unit uPlayer;

interface

uses uCommon;

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

const
  SkillMin = 5;
  SkillMax = 75;
  SkillExp = 100;
  AtrMax = 100;
  RadiusMax = 15;
  DVMax = 80;
  PVMax = 250;
  ExpMax = 10;
  FoodMax = 250;

type
  TPlayer = class(TObject)
  private
    FX: Byte;
    FY: Byte;
    FLX: Byte;
    FLY: Byte;
    FTurn: Word;
    FFood: Word;
    FLevel: Byte;
    FLife: Word;
    FMaxLife: Word;
    FMana: Word;
    FMaxMana: Word;
    FRadius: Byte;
    FDV: Byte;
    FPV: Byte;
    FExp: Byte;
    FDamage: TDamage;
    FLook: Boolean;
    FStrength: Byte;
    FDexterity: Byte;
    FWillpower: Byte;
    FPerception: Byte;
    FGold: Integer;
    FScore: Word;
    FKills: Word;
    FKiller: string;
    FSkill: array [TSkillEnum] of TSkill;
    FWeaponSkill: TSkillEnum;
    FIsRest: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property X: Byte read FX write FX;
    property Y: Byte read FY write FY;
    property LX: Byte read FLX write FLX;
    property LY: Byte read FLY write FLY;
    property Turn: Word read FTurn write FTurn;
    property Food: Word read FFood write FFood;
    property Level: Byte read FLevel write FLevel;
    property Life: Word read FLife write FLife;
    property MaxLife: Word read FMaxLife write FMaxLife;
    property Mana: Word read FMana write FMana;
    property MaxMana: Word read FMaxMana write FMaxMana;
    property Radius: Byte read FRadius write FRadius;
    property DV: Byte read FDV write FDV;
    property PV: Byte read FPV write FPV;
    property Exp: Byte read FExp write FExp;
    property Look: Boolean read FLook write FLook;
    property Strength: Byte read FStrength write FStrength;
    property Dexterity: Byte read FDexterity write FDexterity;
    property Willpower: Byte read FWillpower write FWillpower;
    property Perception: Byte read FPerception write FPerception;
    property Gold: Integer read FGold write FGold;
    property Score: Word read FScore write FScore;
    property Kills: Word read FKills write FKills;
    property Killer: string read FKiller write FKiller;
    property IsRest: Boolean read FIsRest write FIsRest;
    procedure Render(AX, AY: Byte);
    procedure Move(AX, AY: ShortInt);
    property Damage: TDamage read FDamage write FDamage;
    procedure Calc;
    procedure Fill;
    procedure Wait;
    procedure AddTurn;
    function GetRadius: Byte;
    function GetDV: Byte;
    function GetPV: Byte;
    function SaveCharacterDump(AReason: string): string;
    procedure Skill(ASkill: TSkillEnum; AExpValue: Byte = 1);
    function GetSkill(ASkill: TSkillEnum): TSkill;
    function GetSkillName(ASkill: TSkillEnum): string;
    function GetSkillValue(ASkill: TSkillEnum): Byte;
    procedure Defeat(AKiller: string);
    procedure Attack(Index: Integer);
    procedure PickUp;
    procedure Drop(Index: Integer);
    procedure Use(Index: Integer);
    procedure Drink(Index: Integer);
    procedure Equip(Index: Integer);
    procedure UnEquip(Index: Integer);
    procedure AddExp(Value: Byte = 1);
    procedure SkillSet;
    procedure StarterSet;
    function IsDead: Boolean;
    procedure Rest(ATurns: Byte);
  end;

var
  Player: TPlayer = nil;

implementation

uses Classes, SysUtils, Dialogs, Math, uGame, uMap, uMob, uScenes,
  uTerminal, uMsgLog, gnugettext, BeaRLibItems, uItem;

{ TPlayer }

procedure TPlayer.AddTurn;
var
  V: Byte;
begin
  Turn := Turn + 1;
  V := Clamp(100 - Player.GetSkillValue(skToughness), 25, 100);
  if (Turn mod v = 0) then
  begin
    Life := Clamp(Life + Player.GetSkillValue(skHealing), 0, MaxLife);
    Mana := Clamp(Life + Player.GetSkillValue(skConcentration), 0, MaxMana);
  end;
  Mobs.Process;
end;

procedure TPlayer.Attack(Index: Integer);
var
  Mob: TMob;
  Dam: Word;
  The: string;
begin
  if (Index < 0) then
    Exit;
  Mob := Mobs.FMob[Index];
  if not Mob.Alive then
    Exit;
  The := GetDescThe(Mobs.GetName(TMobEnum(Mob.ID)));
  if (MobBase[TMobEnum(Mob.ID)].DV < Math.RandomRange(0, 100)) then
  begin
    // Attack
    Dam := Clamp(RandomRange(Self.Damage.Min, Self.Damage.Max + 1), 0, High(Word));
    Mob.Life := Clamp(Mob.Life - Dam, 0, High(Word));
    MsgLog.Add(Format(_('You hit %s (%d).'), [The, Dam]));
    case FWeaponSkill of
      skBlade:
      begin
        Skill(FWeaponSkill);
        Skill(skAthletics, 2);
        Skill(skDodge, 2);
      end;
      skAxe:
      begin
        Skill(FWeaponSkill);
        Skill(skAthletics, 3);
        Skill(skDodge);
      end;
      skSpear:
      begin
        Skill(FWeaponSkill);
        Skill(skAthletics);
        Skill(skDodge, 3);
      end;
      skMace:
      begin
        Skill(FWeaponSkill);
        Skill(skAthletics, 4);
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
    MsgLog.Add(Format(_('You fail to hurt %s.'), [The]));
  end;
  AddTurn;
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
  FCount := Clamp(Items_Inventory_GetCount(), 0, 26);
  for I := 0 to FCount - 1 do
  begin
    FItem := Items_Inventory_GetItem(I);
    if (FItem.Equipment > 0) then
    begin
      FI := TItemEnum(FItem.ItemID);   
      Dam.Min := Dam.Min + ItemBase[FI].Damage.Min;
      Dam.Max := Dam.Max + ItemBase[FI].Damage.Max;
      Def := Def + ItemBase[FI].Defense;
      if (ItemBase[FI].SlotType = stMainHand) then
        case ItemBase[FI].ItemType of
          itBlade: FWeaponSkill := skBlade;
          itAxe  : FWeaponSkill := skAxe;
          itSpear: FWeaponSkill := skSpear;
          itMace : FWeaponSkill := skMace;
        end;
    end;
  end;
  //
  Self.Gold := Clamp(Items_Inventory_GetItemAmount(Ord(iGold)), 0, High(Integer));
  //
  Strength := Clamp(Round(FSkill[skAthletics].Value * 0.5) +
    Round(FSkill[skToughness].Value * 0.9), 1, AtrMax);
  Dexterity := Clamp(Round(FSkill[skDodge].Value * 1.4), 1, AtrMax);
  Willpower := Clamp(Round(FSkill[skConcentration].Value * 1.4), 1, AtrMax);
  Perception := Clamp(Round(FSkill[skToughness].Value * 1.4), 1, AtrMax);
  //
  DV := Clamp(Round(Dexterity * (DVMax / AtrMax)), 0, DVMax);
  PV := Clamp(Round(FSkill[skToughness].Value / 1.4) - 4 + Def,
    0, PVMax);
  MaxLife := Round(Strength * 3.6) + Round(Dexterity * 2.3);
  MaxMana := Round(Willpower * 4.2) + Round(Dexterity * 0.4);
  Radius := Round(Perception / 8.3);
  //
  FDamage.Min := Clamp(Dam.Min + Strength div 3, 1, High(Byte) - 1);
  FDamage.Max := Clamp(Dam.Max + Strength div 2, 2, High(Byte));
end;

constructor TPlayer.Create;
var
  I: TSkillEnum;
begin
  Exp := 0;
  Turn := 0;
  Food := FoodMax;
  Gold := 0;
  Score := 0;
  Kills := 0;
  Level := 1;
  Killer := '';
  Look := False;
  IsRest := False;
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
  MsgLog.Add(_('You die...'));
  Game.Screenshot := GetTextScreenshot();
end;

destructor TPlayer.Destroy;
begin

  inherited;
end;

procedure TPlayer.Fill;
begin
  Life := MaxLife;
  Mana := MaxMana;
end;

function TPlayer.GetDV: Byte;
begin
  Result := Clamp(Self.DV, 0, DVMax);
end;

function TPlayer.GetPV: Byte;
begin
  Result := Clamp(Self.PV, 0, PVMax);
end;

function TPlayer.GetRadius: Byte;
begin
  Result := Clamp(Self.Radius + 3, 1, RadiusMax);
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
  Result := FSkill[ASkill].Value;
end;

function TPlayer.IsDead: Boolean;
begin
  Result := Self.Life = 0;
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
      LX := Clamp(LX + AX, 0, High(Byte));
      LY := Clamp(LY + AY, 0, High(Byte));
    end;
  end
  else
  begin
    if Self.IsDead then
    begin
      Scenes.SetScene(scDef);
      Exit;
    end;
    if Game.Won then
    begin
      Scenes.SetScene(scWin);
      Exit;
    end;
    FX := Clamp(X + AX, 0, High(Byte));
    FY := Clamp(Y + AY, 0, High(Byte));
    if (Map.GetTileEnum(FX, FY, Map.Deep) in StopTiles) and not Game.Wizard then
      Exit;
    if not Mobs.GetFreeTile(FX, FY) then
    begin
      Self.Attack(Mobs.GetIndex(FX, FY));
    end
    else
    begin
      X := FX;
      Y := FY;
      AddTurn;
    end;
  end;
end;

procedure TPlayer.Use(Index: Integer);
var
  The: string;
  AItem: Item;
  FCount: Integer;
begin
  AItem := Items_Inventory_GetItem(Index);
  The := GetDescThe(Items.GetName(TItemEnum(AItem.ItemID)));
  FCount := Items_Inventory_GetItemCount(AItem.ItemID);
  if (Items.GetItemEnum(AItem.ItemID) in NotEquipItems) then
  begin
    // Drink a potion
    if (Items.GetItemEnum(AItem.ItemID) in DrinkItems) then
      Self.Drink(Index);
  end else begin
    // Equip or unequip an item
    case AItem.Equipment of
      0: Self.Equip(Index);
      1: Self.UnEquip(Index);
    end;
  end;
//  MsgLog.Add(Format(_('You don''t know how to use %s.'), [The]));
end;

procedure TPlayer.Equip(Index: Integer);
var
  The: string;
  AItem, AUnEquipItem: Item;
  I: Integer;
begin
  // Replace
  I := Items_Inventory_EquipItem(Index);
  if (I > -1) then
  begin
    AUnEquipItem := Items_Inventory_GetItem(I);
    //Items.GetItemEnum(AUnEquipItem.ItemID)
    The := GetDescThe(Items.GetName(Items.GetItemEnum(AUnEquipItem.ItemID)));
    MsgLog.Add(Format(_('You unequip %s.'), [The]));
    Self.Calc;
    Wait;
  end;
  // Equip
  AItem := Items_Inventory_GetItem(Index);
  The := GetDescThe(Items.GetName(Items.GetItemEnum(AItem.ItemID)));
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
    The := GetDescThe(Items.GetName(Items.GetItemEnum(AItem.ItemID)));
    MsgLog.Add(Format(_('You unequip %s.'), [The]));
    Self.Calc;
    Wait;
  end;
end;

procedure TPlayer.Drink(Index: Integer);
var
  The: string;
  AItem: Item;
  Value: Word;
const
  F = '%s +%d.';
begin
  AItem := Items_Inventory_GetItem(Index);
  begin
    AItem.Amount := AItem.Amount - 1;
    The := GetDescThe(Items.GetName(Items.GetItemEnum(AItem.ItemID)));
    MsgLog.Add(Format(_('You drink %s.'), [The]));
    case Items.GetItemEnum(AItem.ItemID) of
      iPotionOfHealth:
      begin
        Player.Score := Player.Score + 1;
        Value := Self.GetSkillValue(skHealing) + 100;
        MsgLog.Add(Format(F, [_('Life'), Min(MaxLife - Life, Value)]));
        Self.Life := Clamp(Self.Life + Value, 0, MaxLife);
        Self.Skill(skHealing, 5);
      end;
      iPotionOfMana:
      begin
        Player.Score := Player.Score + 1;
        Value := Self.GetSkillValue(skConcentration) + 100;
        MsgLog.Add(Format(F, [_('Mana'), Min(MaxMana - Mana, Value)]));
        Self.Mana := Clamp(Self.Mana + Value, 0, MaxMana);
        Self.Skill(skConcentration, 5);
      end;
    end;
    Items_Inventory_SetItem(Index, AItem);
    Self.Calc;
    Wait;
  end;
end;

procedure TPlayer.Drop(Index: Integer);
var
  AItem: Item;
  MapID, FCount: Integer;

  procedure DeleteItem;
  var
    The: string;
  begin
    if (Items_Inventory_DeleteItem(Index, AItem) > 0) then
    begin
      AItem.X := Player.X;
      AItem.Y := Player.Y;
      AItem.MapID := Ord(Map.Deep);
      Items_Dungeon_AppendItem(AItem);
      The := GetDescThe(Items.GetName(TItemEnum(AItem.ItemID)));
      MsgLog.Add(Format(_('You drop %s.'), [The]));
      Wait;
    end;
  end;

begin
  MapID := Ord(Map.Deep);
  AItem := Items_Inventory_GetItem(Index);
  FCount := Items_Inventory_GetItemCount(AItem.ItemID);
  if (AItem.Stack > 1) and (AItem.Amount > 1) then
  begin

    Self.Calc;
    Exit;
  end else DeleteItem;
  Self.Calc;
end;

procedure TPlayer.PickUp;
var
  The: string;
  MapID, FCount, Index: Integer;
  FItem: Item;
begin
  //// Your backpack is full!
  MapID := Ord(Map.Deep);
  FCount := Items_Dungeon_GetMapCountXY(MapID, Player.X, Player.Y);
//  if (FItem.Stack > 1) and (FItem.Amount > 1) then
  if (FCount > 0) then
  begin
    if (FCount = 1) then
    begin
      // Pickup an item
      Items.AddItemToInv(0);
    end else begin
      // Items scene
      Scenes.SetScene(scItems);
    end;
  end;
end;

procedure TPlayer.Render(AX, AY: Byte);
begin
  if (Self.Life = 0) then
    Terminal.Print(AX + View.Left, AY + View.Top, '%', clDarkGray)
  else
    Terminal.Print(AX + View.Left, AY + View.Top, '@', clDarkBlue);
end;

function TPlayer.SaveCharacterDump(AReason: string): string;
var
  SL: TStringList;
begin
  if Game.Wizard then
    Exit;
  SL := TStringList.Create;
  try
    SL.Append(Format(FT, [_('Trollhunter')]));
    SL.Append('');
    SL.Append(GetDateTime);
    SL.Append('');
    SL.Append(AReason);
    if Player.IsDead then
      SL.Append(Format(_('He scored %d points.'), [Player.Score]))
      else SL.Append(Format(_('He has scored %d points so far.'), [Player.Score]));
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
    SL.Append(Format('%s: %d', [_('Gold'), Player.Gold]));
    SL.SaveToFile(GetDateTime('-', '-') + '-character-dump.txt');
  finally
    SL.Free;
  end;
end;

procedure TPlayer.AddExp(Value: Byte = 1);
begin
  Exp := Exp + Value;
  if (Exp >= ExpMax) then
  begin
    Exp := Exp - ExpMax;
    FLevel := FLevel + 1;
    MsgLog.Add(Format('%s +1.', [_('Level')]));
    Player.Score := Player.Score + (FLevel * FLevel);
  end;
end;

procedure TPlayer.Skill(ASkill: TSkillEnum; AExpValue: Byte = 1);
begin
  if (FSkill[ASkill].Value < SkillMax) then
  begin
    Inc(FSkill[ASkill].Exp, AExpValue);
    if (FSkill[ASkill].Exp >= SkillExp) then
    begin
      AddExp();
      FSkill[ASkill].Exp := FSkill[ASkill].Exp - SkillExp;
      Inc(FSkill[ASkill].Value);
      // Add message {!!!}
      MsgLog.Add(Format('%s %s +1.', [_('Skill'), Self.GetSkillName(ASkill)]));
      FSkill[ASkill].Value := Clamp(FSkill[ASkill].Value, SkillMin, SkillMax);
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
  if not Game.Wizard then Exit;
  for I := Low(TSkillEnum) to High(TSkillEnum) do
    with FSkill[I] do
    begin
      Value := Math.RandomRange(SkillMin, SkillMax);
      Exp := Math.RandomRange(0, SkillExp);
    end;
  Self.Calc;
  Self.Fill;
end;

procedure TPlayer.Wait;
begin
  if not DeepVis[Map.Deep] then
  begin
    MsgLog.Add(Format(_('You have opened a new territory: %s.'),
      [Map.GetName]));
    DeepVis[Map.Deep] := True;
    if (Ord(Map.Deep) > 0) then
    Player.Score := Player.Score + (Ord(Map.Deep) * 15);
  end;
  Move(0, 0);
end;

procedure TPlayer.Rest(ATurns: Byte);
var
  T: Byte;

  procedure FinRest;
  begin
    MsgLog.Add(Format(_('Finish rest (%d turns)!'), [T - 1]));
    IsRest := False;  
  end;

begin
  if (Player.Food = 0) then 
  begin
    MsgLog.Add(_('Need food!'));
    Exit;
  end;
  IsRest := True;
  MsgLog.Add(Format(_('Start rest (%d turns)!'), [ATurns]));
  for T := 1 to ATurns do
  begin
    if not IsRest or (Player.Food = 0) then 
    begin
      FinRest;
      Exit;
    end;
    Player.Food := Clamp(Player.Food - 1, 0, FoodMax);
    Wait;
  end;  
  FinRest;
end;

procedure TPlayer.StarterSet;
var
  G: Word;
begin
  // Add weapon and armor
  if Game.Wizard then
  begin
    Items.AddItemToInv(iTrollSlayer, 1, True);
  end else begin
    Items.AddItemToInv(iSlagHammer, 1, True);
  end;
  // Add potions and scrolls
  if Game.Wizard then
  begin
    Items.AddItemToInv(iPotionOfHealth, ItemBase[iPotionOfHealth].MaxStack);
    Items.AddItemToInv(iPotionOfMana, ItemBase[iPotionOfMana].MaxStack);
  end else begin
    Items.AddItemToInv(iPotionOfHealth, 5);
    Items.AddItemToInv(iPotionOfMana, 5);
  end;
  // Add coins
  G := IfThen(Game.Wizard, RandomRange(6666, 9999), 50); // :)
  Items.AddItemToInv(iGold, G);
  Self.Calc;
end;

initialization

Player := TPlayer.Create;

finalization

FreeAndNil(Player);

end.
