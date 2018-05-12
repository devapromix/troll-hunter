unit uPlayer;

interface

uses Types, uTypes, uCreature, uMob, uBearLibItemsCommon, uSkill, uStatistic,
  uTalent, uRace, uClass;

type
  TSlotType = (stNone, stHead, stTorso, stHands, stFeet, stMainHand, stOffHand,
    stNeck, stFinger, stTorch);

const
  // Player
  VisionMax = 15;
  DVMax = 80;
  LevelExpMax = 9;
  ReLifeMax = 10;
  ReManaMax = 20;
  LifeAEKMax = 8;
  ManaAEKMax = 12;
  LifeTurnMax = 150;
  ManaTurnMax = 90;
  // Satiation
  StarvingMax = 500;
  SatiatedMax = 8000;
  EngorgedMax = 15000;
  // Inventory
  ItemMax = 26;
  StartGold = 250;
  // Talents
  MinPrm = 1;
  TalentPrm = 3;
  AttribPrm = 7;

type
  TDirectionEnum = (drEast, drWest, drSouth, drNorth, drSouthEast, drSouthWest,
    drNorthEast, drNorthWest, drOrigin);

const
  Direction: array [TDirectionEnum] of TPoint = ((X: 1; Y: 0), (X: - 1; Y: 0),
    (X: 0; Y: 1), (X: 0; Y: - 1), (X: 1; Y: 1), (X: - 1; Y: 1), (X: 1; Y: - 1),
    (X: - 1; Y: - 1), (X: 0; Y: 0));

type
  TSexEnum = (sxMale, sxFemale);

type
  TPlayer = class(TCreature)
  private
    FLX: UInt;
    FLY: UInt;
    FMaxMap: UInt;
    FLook: Boolean;
    FGold: Int;
    FKiller: string;
    FWeaponSkill: TSkillEnum;
    FRace: TRaceEnum;
    FClass: TClassEnum;
    FItemIsDrop: Boolean;
    FItemIndex: Int;
    FItemAmount: Int;
    FSatPerTurn: UInt;
    FBackground: string;
    FIsRest: Boolean;
    FName: string;
    FStatistics: TStatistics;
    FSex: TSexEnum;
    FTalents: TTalents;
    FSkills: TSkills;
    procedure GenNPCText;
    function GetVision: UInt;
  public
    constructor Create;
    destructor Destroy; override;
    property LX: UInt read FLX write FLX;
    property LY: UInt read FLY write FLY;
    property Vision: UInt read GetVision;
    property MaxMap: UInt read FMaxMap write FMaxMap;
    property Look: Boolean read FLook write FLook;
    property Gold: Int read FGold write FGold;
    property Killer: string read FKiller write FKiller;
    property IsRest: Boolean read FIsRest write FIsRest;
    property ItemIsDrop: Boolean read FItemIsDrop write FItemIsDrop;
    property ItemIndex: Int read FItemIndex write FItemIndex;
    property ItemAmount: Int read FItemAmount write FItemAmount;
    property SatPerTurn: UInt read FSatPerTurn write FSatPerTurn;
    property Statictics: TStatistics read FStatistics write FStatistics;
    property Background: string read FBackground;
    property Name: string read FName write FName;
    property Skills: TSkills read FSkills write FSkills;
    property HRace: TRaceEnum read FRace write FRace;
    property HClass: TClassEnum read FClass write FClass;
    property Talents: TTalents read FTalents write FTalents;
    procedure SetAmountScene(IsDrop: Boolean; Index, Amount: Int);
    property Sex: TSexEnum read FSex write FSex;
    procedure Render(AX, AY: UInt);
    procedure Move(Dir: TDirectionEnum);
    procedure RenderInfo;
    procedure Calc;
    procedure Wait;
    procedure Clear();
    procedure AddTurn;
    procedure GenerateBackground();
    procedure Spawn;
    function GetSatiationStr: string;
    function SaveCharacterDump(AReason: string): string;
    procedure Defeat(AKiller: string = '');
    procedure Attack(Index: Int);
    procedure ReceiveHealing;
    procedure Buy(Index: Int);
    procedure PickUp;
    procedure PickUpAmount(Index: Int);
    procedure Drop(Index: Int);
    procedure DropAmount(Index: Int);
    procedure Use(Index: Int);
    procedure DoEffects(const Effects: TEffects; const Value: UInt = 0);
    procedure Equip(Index: Int);
    procedure UnEquip(Index: Int);
    procedure Sell(Index: Int);
    procedure RepairItem(Index: Int);
    procedure IdentItem(Index: Int);
    procedure IdentAllItems;
    procedure CraftItem(Index: Int);
    procedure BreakItem(Index: Int; Value: UInt = 1); overload;
    procedure BreakItem(ASlot: TSlotType; Value: UInt = 1); overload;
    procedure BreakItem(); overload;
    procedure AddExp(Value: UInt = 1);
    procedure Start;
    procedure DoWeaponSkill;
    procedure Rest(ATurns: UInt);
    procedure Dialog(AMob: TMob);
    procedure RnItem(FItem: Item; const Index: Int);
    procedure AutoPickup();
    procedure RenderWeather(const AX, AY, AWidth: UInt);
    procedure Turn;
    procedure StartEquip;
    procedure StartSkills;
  end;

var
  Player: TPlayer = nil;

implementation

uses Classes, SysUtils, Math, uGame, uMap, uScenes, uItem, Dialogs,
  uTerminal, uMsgLog, uLanguage, uCorpse, uCalendar,
  uShop, BearLibTerminal, uAbility, uAffixes, uAttribute, uSpellbook, uUI,
  uBearLibItemsDungeon, uBearLibItemsInventory, uHelpers;

const
  ClassMainWeapon: array [TClassEnum] of TItemEnum = (ivRusty_Sword, ivStaff1,
    ivBow1, ivDagger1);
  ClassWeaponSkill: array [TClassEnum] of TSkillEnum = (skBlade, skStaff, skBow,
    skDagger);
  ClassMainSkill: array [TClassEnum] of TSkillEnum = (skAthletics, skConcentration,
    skDodge, skToughness);
  ClassAddSkill: array [TClassEnum] of TSkillEnum = (skBodybuilding, skMeditation,
    skDodge, skStealth);

  { TPlayer }

  // Generate a random player's background (from Kharne and UMoria roguelikes)
procedure TPlayer.GenerateBackground();
var
  I: (cpChild, cpClass, cpParent, cpCredit, cpBackground, cpEyeType,
    cpEyeColour, cpHairStyle, cpHairColour, cpComplexion);
  SL: array [Low(I) .. High(I)] of TStringList;
begin
  Randomize;
  FBackground := '';
  for I := Low(I) to High(I) do
    SL[I] := TStringList.Create;
  try
    SL[cpChild].DelimitedText := _('"an only child","one of two children",' +
      '"one of many children","the only surviving child","one of several children",'
      + '"the illegitimate but acknowledged child","the illegitimate and unacknowledged child"');
    SL[cpClass].DelimitedText :=
      _('"lower-class", "middle-class","upper-class"');
    SL[cpParent].DelimitedText :=
      _('"mercenary","merchant","businessman","titled noble",' +
      '"craftsman","soldier","templar","priest","guildsman","townsman"');
    SL[cpBackground].DelimitedText :=
      _('"contented","peaceful","troubled","settled","disturbed"');
    SL[cpCredit].DelimitedText :=
      _('"a credit to","a disgrace to","the black sheep of"');
    SL[cpEyeType].DelimitedText :=
      _('"dull","unusually piercing","piercing","striking","dark"');
    SL[cpEyeColour].DelimitedText :=
      _('"grey","violet","green","blue","brown","blue-gray"');
    SL[cpHairStyle].DelimitedText :=
      _('"wavy","curly","straight","short","long"');
    SL[cpHairColour].DelimitedText :=
      _('"auburn","blonde","black","dark","red","ginger","grey","brown"');
    SL[cpComplexion].DelimitedText :=
      _('"an average","a sallow","a fair","a dark","a light"');

    FBackground :=
      Format(Terminal.Colorize
      (_('You are %s of a %s %s. You had a %s upbringing and you ' +
      'are %s the family. You have %s %s eyes, %s %s hair, and %s complexion.'),
      'Yellow'), [SL[cpChild][Random(SL[cpChild].Count - 1)],
      SL[cpClass][Random(SL[cpClass].Count - 1)],
      SL[cpParent][Random(SL[cpParent].Count - 1)],
      SL[cpBackground][Random(SL[cpBackground].Count - 1)],
      SL[cpCredit][Random(SL[cpCredit].Count - 1)],
      SL[cpEyeType][Random(SL[cpEyeType].Count - 1)],
      SL[cpEyeColour][Random(SL[cpEyeColour].Count - 1)],
      SL[cpHairStyle][Random(SL[cpHairStyle].Count - 1)],
      SL[cpHairColour][Random(SL[cpHairColour].Count - 1)],
      SL[cpComplexion][Random(SL[cpComplexion].Count - 1)]]);
  finally
    for I := Low(I) to High(I) do
      FreeAndNil(SL[I]);
  end;
end;

procedure TPlayer.RnItem(FItem: Item; const Index: Int);
begin
  if (FItem.Durability = 0) then
  begin
    Items_Inventory_DeleteItem(Index, FItem);
    MsgLog.Add(Terminal.Colorize(Format(_('%s been ruined irreversibly.'),
      [Items.GetNameThe(FItem)]), clAlarm));
  end;
end;

procedure TPlayer.AddTurn;
begin
  if IsDead then
    Exit;
  Statictics.Inc(stTurn);
  Calendar.Turn;
  if (Attributes.Attrib[atSat].Value > 0) then
    Attributes.Modify(atSat, -SatPerTurn);
  if Abilities.IsAbility(abWeak) then
    Attributes.Modify(atSat, -10);
  if (Attributes.Attrib[atSat].Value < StarvingMax) then
    Attributes.Modify(atLife, -1);
  Turn;
  if OnTurn() then
    Calc;
  if IsDead then
    Defeat;
  Mobs.Process;
end;

procedure TPlayer.DoWeaponSkill;
begin
  case FWeaponSkill of
    skBlade:
      begin
        Skills.DoSkill(FWeaponSkill, 2);
        Skills.DoSkill(skAthletics, 2);
        Skills.DoSkill(skDodge, 2);
        SatPerTurn := Ord(Game.Difficulty) + 5;
      end;
    skAxe:
      begin
        Skills.DoSkill(FWeaponSkill, 2);
        Skills.DoSkill(skAthletics, 3);
        Skills.DoSkill(skDodge);
        SatPerTurn := Ord(Game.Difficulty) + 6;
      end;
    skSpear, skDagger:
      begin
        Skills.DoSkill(FWeaponSkill, 2);
        Skills.DoSkill(skAthletics);
        Skills.DoSkill(skDodge, 3);
        SatPerTurn := Ord(Game.Difficulty) + 4;
      end;
    skMace:
      begin
        Skills.DoSkill(FWeaponSkill, 2);
        Skills.DoSkill(skAthletics, 4);
        SatPerTurn := Ord(Game.Difficulty) + 7;
      end;
    skStaff, skWand:
      begin
        Skills.DoSkill(FWeaponSkill, 2);
        Skills.DoSkill(skDodge);
        Skills.DoSkill(skConcentration, 3);
        SatPerTurn := Ord(Game.Difficulty) + 8;
      end;
    skBow:
      begin
        Skills.DoSkill(FWeaponSkill, 2);
        Skills.DoSkill(skDodge, 4);
        SatPerTurn := Ord(Game.Difficulty) + 4;
      end;
  end;
end;

procedure TPlayer.Attack(Index: Int);
var
  V, Ch: UInt;
  Mob: TMob;
  Dam, Cr: UInt;
  CrStr, The: string;

  procedure Miss();
  begin
    MsgLog.Add(Format(_('You miss %s.'), [The]));
    // MsgLog.Add(Format(_('You fail to hurt %s.'), [The]));
    SatPerTurn := Ord(Game.Difficulty) + 3;
  end;

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
  if (Mob.Attributes.Attrib[atDV].Value < Math.RandomRange(0, 100)) and
    not Abilities.IsAbility(abCursed) then
  begin
    CrStr := '';
    // Attack
    Dam := Game.EnsureRange(RandomRange(Self.GetDamage.Min,
      GetDamage.Max + 1), UIntMax);
    // Abilities
    if Abilities.IsAbility(abBloodlust) then
      Dec(Dam, Dam div 3);
    // Critical hits...     .
    Ch := Math.RandomRange(0, 100);
    Cr := Skills.Skill[FWeaponSkill].Value;
    if ((Ch < Cr) and not Abilities.IsAbility(abWeak)) then
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
    // PV
    Dam := Self.GetRealDamage(Dam, Mob.Attributes.Attrib[atPV].Value);
    if (Dam = 0) then
    begin
      Miss();
      AddTurn;
      Exit;
    end;
    // Attack
    Mob.Attributes.Modify(atLife, -Dam);
    MsgLog.Add(Format(_('You hit %s (%d).'), [The, Dam]));
    // Break weapon
    if ((Math.RandomRange(0, 10 - Ord(Game.Difficulty)) = 0) and not Mode.Wizard)
    then
      BreakItem(stMainHand);
    if (CrStr <> '') then
      MsgLog.Add(Terminal.Colorize(CrStr, clAlarm));
    DoWeaponSkill;
    // Victory
    if Mob.IsDead then
      Mob.Defeat;
  end
  else
    Miss();
  AddTurn;
end;

procedure TPlayer.AutoPickup;
var
  Index, FCount: Int;
  ItemType: TItemType;
  FItem: Item;
begin
  if Self.IsDead then
    Exit;
  FCount := Items_Dungeon_GetMapCountXY(Ord(Map.Current), X, Y)
    .InRange(ItemMax);
  for Index := FCount - 1 downto 0 do
  begin
    FItem := Items_Dungeon_GetMapItemXY(Ord(Map.Current), Index, X, Y);
    ItemType := ItemBase[TItemEnum(FItem.ItemID)].ItemType;
    if (ItemType in AutoPickupItems) then
    begin
      if ((ItemType in CoinTypeItems) and not Game.GetOption(apCoin)) then
        Exit;
      if ((ItemType in FoodTypeItems) and not Game.GetOption(apFood)) then
        Exit;
      if ((ItemType in PlantTypeItems) and not Game.GetOption(apPlant)) then
        Exit;
      if ((ItemType in PotionTypeItems) and not Game.GetOption(apPotion)) then
        Exit;
      if ((ItemType in MagicTypeItems) and not Game.GetOption(apMagic)) then
        Exit;
      if ((ItemType in FlaskTypeItems) and not Game.GetOption(apFlask)) then
        Exit;
      if ((ItemType in ScrollTypeItems) and not Game.GetOption(apScroll)) then
        Exit;
      if ((ItemType in RuneTypeItems) and not Game.GetOption(apRune)) then
        Exit;
      if ((ItemType in BookTypeItems) and not Game.GetOption(apBook)) then
        Exit;
      if ((ItemType in GemTypeItems) and not Game.GetOption(apGem)) then
        Exit;
      if ((ItemType in KeyTypeItems) and not Game.GetOption(apKey)) then
        Exit;
      Items.AddItemToInv(Index, True);
      Wait;
    end;
  end;
end;

procedure TPlayer.Calc;
var
  FAttrib: array [TAttribEnum] of UInt;
  Attrib: TAttribEnum;
  I, FCount: Int;
  ID: TItemEnum;
  FItem: Item;

  procedure AddAttrib(const AAttrib: TAttribEnum; const Value: UInt);
  begin
    FAttrib[AAttrib] := FAttrib[AAttrib] + Value;
  end;

  procedure ClearAttrib();
  var
    I: TAttribEnum;
  begin
    for I := Low(TAttribEnum) to High(TAttribEnum) do
      FAttrib[I] := 0;
  end;

  function GetSkill(const Value: TItemType): TSkillEnum;
  begin
    case Value of
      itBlade:
        Result := skBlade;
      itAxe:
        Result := skAxe;
      itSpear:
        Result := skSpear;
      itMace:
        Result := skMace;
      itStaff:
        Result := skStaff;
      itWand:
        Result := skWand;
      itDagger:
        Result := skDagger;
      itBow:
        Result := skBow;
    else
      Result := skNone;
    end;
  end;

  procedure LoAttrib(Attrib: TAttribEnum);
  begin
    Attributes.SetValue(Attrib, Attributes.Attrib[Attrib].Value div 2);
  end;

  procedure HiAttrib(Attrib: TAttribEnum);
  begin
    Attributes.SetValue(Attrib, Attributes.Attrib[Attrib].Value * 2);
  end;

begin
  Light := 0;
  ClearAttrib();
  FCount := Items_Inventory_GetCount().InRange(ItemMax);
  for I := 0 to FCount - 1 do
  begin
    FItem := Items_Inventory_GetItem(I);
    if (FItem.Equipment > 0) then
    begin
      if (FItem.Identify = 0) then
        Continue;
      ID := TItemEnum(FItem.ItemID);
      AddAttrib(atDef, FItem.Defense);
      AddAttrib(atMinDamage, FItem.MinDamage);
      AddAttrib(atMaxDamage, FItem.MaxDamage);
      if (FItem.Bonus[0] > 0) then
      begin
        AddAttrib(atMaxLife, Items.GetBonus(FItem, btLife));
        AddAttrib(atMaxMana, Items.GetBonus(FItem, btMana));
        AddAttrib(atVision, Items.GetBonus(FItem, btVis));
        AddAttrib(atExtraGold, Items.GetBonus(FItem, btExtraGold));
      end;
      if (FItem.Bonus[1] > 0) then
      begin
        AddAttrib(atStr, Items.GetBonus(FItem, btStr));
        AddAttrib(atDex, Items.GetBonus(FItem, btDex));
        AddAttrib(atWil, Items.GetBonus(FItem, btWil));
        AddAttrib(atPer, Items.GetBonus(FItem, btPer));
      end;
      if (FItem.Bonus[2] > 0) then
      begin
        AddAttrib(atReLife, Items.GetBonus(FItem, btReLife));
        AddAttrib(atReMana, Items.GetBonus(FItem, btReMana));
        AddAttrib(atLifeAfEachKill, Items.GetBonus(FItem, btLifeAfEachKill));
        AddAttrib(atManaAfEachKill, Items.GetBonus(FItem, btManaAfEachKill));
      end;
      if (ItemBase[ID].SlotType = stMainHand) then
        FWeaponSkill := GetSkill(ItemBase[ID].ItemType);
      if (ItemBase[ID].SlotType = stTorch) then
      begin
        Light := Light + FItem.Value;
        FItem.Value := FItem.Value - 1;
        Items_Inventory_SetItem(I, FItem);
        if (FItem.Value <= 0) then
        begin
          Items_Inventory_DeleteItem(I, FItem);
          Light := 0;
        end;
      end;
    end;
  end;
  //
  Gold := Items_Inventory_GetItemAmount(Ord(ivGold));
  //
  Attributes.SetValue(atStr, EnsureRange(Round(Skills.Skill[skAthletics].Value *
    1.2) + Round(Skills.Skill[skToughness].Value * 0.2) + FAttrib[atStr] +
    Attributes.Attrib[atStr].Prm, 1, AttribMax));
  Attributes.SetValue(atDex,
    EnsureRange(Round(Skills.Skill[skDodge].Value * 1.4) + FAttrib[atDex] +
    Attributes.Attrib[atDex].Prm, 1, AttribMax));
  Attributes.SetValue(atWil,
    EnsureRange(Round(Skills.Skill[skConcentration].Value * 1.4) +
    FAttrib[atWil] + Attributes.Attrib[atWil].Prm, 1, AttribMax));
  Attributes.SetValue(atPer, EnsureRange(Round(Skills.Skill[skToughness].Value *
    1.4) + FAttrib[atPer] + Attributes.Attrib[atPer].Prm, 1, AttribMax));
  //
  if (Abilities.IsAbility(abBerserk)) then
  begin
    HiAttrib(atStr);
    HiAttrib(atDex);
  end;
  if (Abilities.IsAbility(abWeak)) then
  begin
    LoAttrib(atStr);
    LoAttrib(atDex);
  end;
  if Abilities.IsAbility(abAfraid) then
    LoAttrib(atWil);
  if Abilities.IsAbility(abDrunk) then
    LoAttrib(atPer);
  // DV
  Attributes.SetValue(atDV,
    Game.EnsureRange(Round(Attributes.Attrib[atDex].Value * (DVMax / AttribMax))
    + Attributes.Attrib[atDV].Prm, DVMax));
  // PV
  Attributes.SetValue(atPV,
    Game.EnsureRange(Round(Skills.Skill[skToughness].Value / 1.4) - 4 +
    FAttrib[atDef] + Attributes.Attrib[atPV].Prm, PVMax));
  if Abilities.IsAbility(abArmor_Reduction) then
    LoAttrib(atPV);
  // Life
  Attributes.SetValue(atMaxLife, Round(Attributes.Attrib[atStr].Value * 3.6) +
    Round(Attributes.Attrib[atDex].Value * 2.3) + FAttrib[atMaxLife] +
    Attributes.Attrib[atMaxLife].Prm);
  // Mana
  Attributes.SetValue(atMaxMana, Round(Attributes.Attrib[atWil].Value * 4.2) +
    Round(Attributes.Attrib[atDex].Value * 0.4) + FAttrib[atMaxMana] +
    Attributes.Attrib[atMaxMana].Prm);
  // Light
  if Abilities.IsAbility(abLight) then
    Light := Light + Abilities.Ability[abLight];
  // Vision
  Attributes.SetValue(atVision, Round(Attributes.Attrib[atPer].Value / 8.3) +
    FAttrib[atVision] + Light);
  //
  Attributes.SetValue(atExtraGold, FAttrib[atExtraGold].InRange(ExtraGoldMax));
  Self.SetDamage(EnsureRange(FAttrib[atMinDamage] + Attributes.Attrib[atStr]
    .Value div 3, 1, UIntMax - 1),
    EnsureRange(FAttrib[atMaxDamage] + Attributes.Attrib[atStr].Value div 2, 2,
    UIntMax));
  for Attrib := AttrLow to AttrHigh do
    Attributes.SetValue(Attrib, FAttrib[Attrib]);
end;

procedure TPlayer.Clear();
var
  PlayerName: string;
begin
  inherited Clear();
  Skills.Clear();
  Spellbook.Clear();
  Items_Inventory_Clear();
  Killer := '';
  Look := False;
  IsRest := False;
  SatPerTurn := 2;
  Attributes.SetValue(atSat, SatiatedMax);
  Gold := 0;
  MaxMap := 0;
  PlayerName := Trim(Terminal_Get('ini.player.name'));
  if (PlayerName = '') then
    Name := _('PLAYER')
  else
    Name := PlayerName;
  FWeaponSkill := skNone;
  Attributes.SetValue(atLev, 1);
  GenerateBackground();
  Calc();
  Fill();
end;

procedure TPlayer.CraftItem(Index: Int);
var
  FItem: Item;
begin
  FItem := Items_Inventory_GetItem(Index);
  if ((FItem.Stack > 1) or (FItem.Amount > 1) or (FItem.Identify > -1)) then
    Exit;
  if (ItemBase[TItemEnum(FItem.ItemID)].ItemType in SmithTypeItems) then
  begin
    FItem.Identify := Items.Index;
    Affixes.DoSuffix(FItem);
    if (Items_Inventory_SetItem(Index, FItem) > 0) then
    begin
      MsgLog.Add(Format(_('You crafted %s.'), [Items.GetNameThe(FItem)]));
      Statictics.Inc(stItCrafted);
      Skills.DoSkill(skEnchant_Item, FItem.Level);
      Scenes.SetScene(scInv);
    end;
    Self.Calc;
  end;
end;

constructor TPlayer.Create;
begin
  inherited;
  FStatistics := TStatistics.Create;
  Talents := TTalents.Create;
  Skills := TSkills.Create;
  Self.Clear;
end;

procedure TPlayer.Defeat(AKiller: string = '');
begin
  Killer := AKiller;
  MsgLog.Add(Terminal.Colorize(_('You die...'), 'Light Red'));
  if (Game.Difficulty < dfHard) then
    MsgLog.Add(Format(_('Press %s to try again...'), [UI.KeyToStr('SPACE')]))
  else
    MsgLog.Add(Format(_('Press %s to exit...'), [UI.KeyToStr('SPACE')]));
  Corpses.Append();
  Game.Screenshot := Terminal.GetTextScreenshot();
end;

destructor TPlayer.Destroy;
begin
  FreeAndNil(FSkills);
  FreeAndNil(FTalents);
  FreeAndNil(FStatistics);
  inherited;
end;

procedure TPlayer.Dialog(AMob: TMob);
begin
  Game.Timer := UIntMax;
  NPCName := Mobs.Name[TMobEnum(AMob.ID)];
  NPCType := MobBase[TMobEnum(AMob.ID)].NPCType;
  Scenes.SetScene(scDialog);
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

function TPlayer.GetVision: UInt;
begin
  Result := Game.EnsureRange((Attributes.Attrib[atVision].Value -
    Abilities.Ability[abBlinded]) + 3, VisionMax);
  Result := Math.IfThen(Calendar.IsDay, Result, Result div 2);
end;

function TPlayer.GetSatiationStr: string;
begin
  Result := '';
  case Attributes.Attrib[atSat].Value of
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
  if Mode.Wizard then
  begin
    if (Result = '') then
      Result := _('Satiated');
    Result := Result + Format(' (%d)', [Attributes.Attrib[atSat].Value]);
  end;
  case Attributes.Attrib[atSat].Value of
    0 .. StarvingMax:
      Result := Terminal.Colorize(Result, 'Light Red');
    StarvingMax + 1 .. SatiatedMax:
      Result := Terminal.Colorize(Result, 'Light Yellow');
  else
    Result := Terminal.Colorize(Result, 'Light Green');
  end;
end;

procedure TPlayer.Move(Dir: TDirectionEnum);
var
  FX, FY: UInt;
begin
  if Look then
  begin
    if Map.InMap(LX + Direction[Dir].X, LY + Direction[Dir].Y) and
      ((Map.InView(LX + Direction[Dir].X, LY + Direction[Dir].Y) and
      not Map.GetFog(LX + Direction[Dir].X, LY + Direction[Dir].Y)) or
      Mode.Wizard) then
    begin
      LX := Map.EnsureRange(Math.EnsureRange(LX + Direction[Dir].X,
        X - (View.Width div 2), X + (View.Width div 2 - 1)));
      LY := Map.EnsureRange(Math.EnsureRange(LY + Direction[Dir].Y,
        Y - (View.Height div 2), Y + (View.Height div 2 - 1)));
    end;
  end
  else
  begin
    if IsDead then
      Exit;
    FX := Map.EnsureRange(X + Direction[Dir].X);
    FY := Map.EnsureRange(Y + Direction[Dir].Y);
    if (Map.GetTileEnum(FX, FY, Map.Current) in StopTiles) and not Mode.Wizard
    then
      Exit;
    // Stunned or burning
    if (Self.Abilities.IsAbility(abStunned) or
      Self.Abilities.IsAbility(abBurning)) then
    begin
      AddTurn;
      Exit;
    end;
    //
    if not Mobs.GetFreeTile(FX, FY) then
    begin
      Self.Attack(Mobs.GetIndex(FX, FY));
    end
    else
    begin
      X := FX;
      Y := FY;
      if ((Direction[Dir].X <> 0) or (Direction[Dir].Y <> 0)) then
      begin
        SatPerTurn := 2;
        AutoPickup;
      end;
      AddTurn;
    end;
  end;
end;

procedure TPlayer.Use(Index: Int);
var
  FItem: Item;
  I: TItemEnum;
  T: TItemType;
begin
  if IsDead then
    Exit;
  FItem := Items_Inventory_GetItem(Index);
  // Unidentified
  if FItem.Identify = 0 then
  begin
    MsgLog.Add(_('You can not use this yet (unidentified)!'));
    Self.Calc;
    Exit;
  end;
  // Need level
  if (Attributes.Attrib[atLev].Value < FItem.Level) and not Mode.Wizard then
  begin
    MsgLog.Add(Format(_('You can not use this yet (need level %d)!'),
      [FItem.Level]));
    Self.Calc;
    Exit;
  end;
  I := TItemEnum(FItem.ItemID);
  T := ItemBase[I].ItemType;
  // No mana
  if (Player.Attributes.Attrib[atMana].Value < ItemBase[I].ManaCost) then
  begin
    MsgLog.Add(Format(_('You need more mana!'), [FItem.Level]));
    Self.Calc;
    Exit;
  end;
  if (T in NotEquipTypeItems) then
  begin
    if (T in UseTypeItems) then
    begin
      if not(T in RuneTypeItems) then
        FItem.Amount := FItem.Amount - 1;
      if (T in PotionTypeItems) then
      begin
        MsgLog.Add(Format(_('You drink %s.'), [Items.GetNameThe(FItem)]));
        Statictics.Inc(stPotDrunk);
      end;
      if (T in RuneTypeItems + BookTypeItems + ScrollTypeItems) then
      begin
        MsgLog.Add(Format(_('You read %s.'), [Items.GetNameThe(FItem)]));
      end;
      if (T in FoodTypeItems + PlantTypeItems) then
      begin
        MsgLog.Add(Format(_('You ate %s.'), [Items.GetNameThe(FItem)]));
        Statictics.Inc(stFdEat);
      end;
      if (T in MagicTypeItems + FlaskTypeItems) then
      begin
        MsgLog.Add(Format(_('You use %s.'), [Items.GetNameThe(FItem)]));
        Statictics.Inc(stItUsed);
      end;
      //
      if (T in ScrollTypeItems) then
      begin
        Statictics.Inc(stScrRead);
      end;
      if not(T in RuneTypeItems) then
      begin
        Items_Inventory_SetItem(Index, FItem);
      end;
      if (T in ScrollTypeItems + RuneTypeItems) then
      begin
        if (Attributes.Attrib[atMana].Value >= ItemBase[I].ManaCost) then
        begin
          Skills.DoSkill(skConcentration);
          Attributes.Modify(atMana, -ItemBase[I].ManaCost);
          Statictics.Inc(stSpCast);
        end
        else
        begin
          MsgLog.Add(_('You need more mana!'));
          Self.Calc;
          Wait;
          Exit;
        end;
      end;
      DoEffects(FItem.Effects, FItem.Value);
      Self.Calc;
      Wait;
    end;
  end
  else
  begin
    // Equip or unequip an item
    case FItem.Equipment of
      0:
        Self.Equip(Index);
      1:
        Self.UnEquip(Index);
    end;
  end;
  // MsgLog.Add(Format(_('You don''t know how to use %s.'), [The]));
end;

procedure TPlayer.Equip(Index: Int);
var
  FItem: Item;
  I: Int;
begin
  // Need level
  FItem := Items_Inventory_GetItem(Index);
  if (Attributes.Attrib[atLev].Value < FItem.Level) and not Mode.Wizard then
  begin
    MsgLog.Add(Format(_('You can not use this yet (need level %d)!'),
      [FItem.Level]));
    Self.Calc;
    Exit;
  end;
  if (FItem.Identify = 0) and not Mode.Wizard then
  begin
    MsgLog.Add(_('You can not use this yet (unidentified item)!'));
    Self.Calc;
    Exit;
  end;
  // Replace
  I := Items_Inventory_EquipItem(Index);
  if (I > -1) then
    UnEquip(I);
  // Equip
  MsgLog.Add(Format(_('You equip %s.'), [Items.GetNameThe(FItem)]));
  Self.Calc;
  Wait;
end;

procedure TPlayer.UnEquip(Index: Int);
var
  FItem: Item;
begin
  if (Items_Inventory_UnEquipItem(Index) > 0) then
  begin
    FItem := Items_Inventory_GetItem(Index);
    MsgLog.Add(Format(_('You unequip %s.'), [Items.GetNameThe(FItem)]));
    Self.Calc;
    Wait;
  end;
end;

procedure TPlayer.Sell(Index: Int);
var
  Value: Int;
  FItem: Item;
begin
  FItem := Items_Inventory_GetItem(Index);
  if ((FItem.Equipment > 0) or Items.ChItem(FItem)) then
    Exit;
  if (Items_Inventory_DeleteItem(Index, FItem) > 0) then
  begin
    Value := FItem.Price div 4;
    Items.AddItemToInv(ivGold, Value);
    MsgLog.Add(Format(_('You sold %s (+%d gold).'),
      [Items.GetNameThe(FItem), Value]));
  end;
  Self.Calc;
  Wait;
end;

procedure TPlayer.BreakItem();
begin
  case Math.RandomRange(0, 7) of
    0:
      Player.BreakItem(stHead);
    1:
      Player.BreakItem(stTorso);
    2:
      Player.BreakItem(stHands);
    3:
      Player.BreakItem(stFeet);
    4:
      Player.BreakItem(stOffHand);
    5:
      Player.BreakItem(stNeck);
    6:
      Player.BreakItem(stFinger);
  end;
end;

procedure TPlayer.Buy(Index: Int);
var
  FItem: Item;
begin
  FItem := Shops.Shop[Shops.Current].GetItem(Index);
  if (Items_Inventory_DeleteItemAmount(Ord(ivGold), FItem.Price) > 0) then
  begin
    MsgLog.Add(Format(_('You bought %s (-%d gold).'), [Items.GetNameThe(FItem),
      FItem.Price]));
    Items_Inventory_AppendItem(FItem);
    Self.Calc;
    // The %s just frowns. Maybe you'll return when you have enough gold?
  end
  else
    MsgLog.Add(_('You need more gold.'));
end;

procedure TPlayer.ReceiveHealing;
var
  Cost: UInt;
begin
  Cost := Round((Attributes.Attrib[atMaxLife].Value - Attributes.Attrib[atLife]
    .Value) * 1.6);
  if (Self.Gold >= Cost) then
  begin
    if (Items_Inventory_DeleteItemAmount(Ord(ivGold), Cost) > 0) then
    begin
      Attributes.SetValue(atLife, atMaxLife);
      MsgLog.Add(Format(_('You feel better (-%d gold).'), [Cost]));
    end;
  end
  else
    MsgLog.Add(_('You need more gold.'));
  Self.Calc;
end;

procedure TPlayer.IdentAllItems;
var
  FItem: Item;
  FCount, I: Int;
  F: Boolean;
begin
  F := False;
  FCount := Items_Inventory_GetCount().InRange(ItemMax);
  for I := 0 to FCount - 1 do
  begin
    FItem := Items_Inventory_GetItem(I);
    if (FItem.Identify = 0) then
    begin
      Self.IdentItem(I);
      F := True;
    end;
  end;
  if F then
    Self.Calc;
end;

procedure TPlayer.IdentItem(Index: Int);
var
  FItem: Item;
begin
  FItem := Items_Inventory_GetItem(Index);
  if ((FItem.Stack > 1) or (FItem.Amount > 1)) then
    Exit;
  if (Items.Identify(FItem) and (FItem.Identify > 0) and
    (Items_Inventory_SetItem(Index, FItem) > 0)) then
  begin
    MsgLog.Add(Format(_('You identified %s.'), [Items.GetNameThe(FItem)]));
    Statictics.Inc(stItIdent);
    Scenes.SetScene(scInv);
  end;
  Self.Calc;
  Wait;
end;

procedure TPlayer.RepairItem(Index: Int);
var
  RepairCost: UInt;
  FItem: Item;
begin
  FItem := Items_Inventory_GetItem(Index);
  if ((FItem.Stack > 1) or (FItem.Identify = 0) or (FItem.Amount > 1)) then
    Exit;
  // Oil
  if (Items.Index > 0) then
  begin
    { case Items.CurrentItem.Effect of
      // Cursed
      - 1:
      Dec(FItem.MaxDurability);
      // Blessed
      1:
      Inc(FItem.MaxDurability);
      end; }
    FItem.Durability := Math.EnsureRange(FItem.Durability + Items.Index, 1,
      FItem.MaxDurability);
    if (Items_Inventory_SetItem(Index, FItem) > 0) then
    begin
      MsgLog.Add(Format(_('You repaired %s.'), [Items.GetNameThe(FItem)]));
      Statictics.Inc(stItRep);
      Calc;
    end;
    Scenes.SetScene(scInv);
    Exit;
  end;
  // Smith
  RepairCost := (FItem.MaxDurability - FItem.Durability) * 10;
  if (RepairCost > 0) then
  begin
    if (Gold < RepairCost) then
    begin
      MsgLog.Add(_('You need more gold.'));
      Exit;
    end;
    if (FItem.MaxDurability > 0) then
    begin
      if (Game.Difficulty > dfEasy) then
      begin
        Dec(FItem.MaxDurability);
        if (FItem.MaxDurability = 0) then
        begin
          RnItem(FItem, Index);
          Exit;
        end;
      end;
      FItem.Durability := FItem.MaxDurability;
      if ((Items_Inventory_DeleteItemAmount(Ord(ivGold), RepairCost) > 0) and
        (Items_Inventory_SetItem(Index, FItem) > 0)) then
        MsgLog.Add(Format(_('You repaired %s (-%d gold).'),
          [Items.GetNameThe(FItem), RepairCost]));
      Statictics.Inc(stItRep);
    end;
  end;
  Self.Calc;
end;

procedure TPlayer.BreakItem(Index: Int; Value: UInt = 1);
var
  FItem: Item;
begin
  FItem := Items_Inventory_GetItem(Index);
  if ((FItem.Stack > 1) or (FItem.Amount > 1)) then
    Exit;
  FItem.Durability := Game.EnsureRange(FItem.Durability - Value, UIntMax);
  if ((FItem.Durability > 0) and
    (FItem.Durability < (FItem.MaxDurability div 4))) then
    MsgLog.Add(Terminal.Colorize
      (Format(_('%s soon will be totally broken (%d/%d).'),
      [GetCapit(Items.GetNameThe(FItem)), FItem.Durability, FItem.MaxDurability]
      ), clAlarm));
  Items_Inventory_SetItem(Index, FItem);
  RnItem(FItem, Index);
  Self.Calc;
end;

procedure TPlayer.BreakItem(ASlot: TSlotType; Value: UInt = 1);
var
  FItem: Item;
  FCount, I: Int;
  FI: TItemEnum;
begin
  FCount := Items_Inventory_GetCount().InRange(ItemMax);
  for I := 0 to FCount - 1 do
  begin
    FItem := Items_Inventory_GetItem(I);
    if (FItem.Equipment > 0) then
    begin
      FI := TItemEnum(FItem.ItemID);
      if (ItemBase[FI].SlotType = ASlot) then
      begin
        BreakItem(I, Value);
        Exit;
      end;
    end;
  end;
end;

procedure TPlayer.Drop(Index: Int);
var
  AItem: Item;

  procedure DeleteItem;
  begin
    if (Items_Inventory_DeleteItem(Index, AItem) > 0) then
    begin
      AItem.X := X;
      AItem.Y := Y;
      AItem.Equipment := 0;
      AItem.MapID := Ord(Map.Current);
      Items.AddItemToDungeon(AItem);
      MsgLog.Add(Format(_('You drop %s.'), [Items.GetNameThe(AItem)]));
      Wait();
    end;
  end;

begin
  if IsDead then
    Exit;
  AItem := Items_Inventory_GetItem(Index);
  if (AItem.Equipment > 0) then
    Exit;
  if not((AItem.Stack > 1) and (AItem.Amount > 1)) then
    DeleteItem()
  else
    SetAmountScene(True, Index, 1);
  Self.Calc();
end;

procedure TPlayer.DropAmount(Index: Int);
var
  FItem: Item;
begin
  FItem := Items_Inventory_GetItem(Index);
  FItem.Amount := FItem.Amount - ItemAmount;
  Items_Inventory_SetItem(Index, FItem);
  FItem.X := X;
  FItem.Y := Y;
  FItem.Equipment := 0;
  FItem.MapID := Ord(Map.Current);
  FItem.Amount := ItemAmount;
  Items.AddItemToDungeon(FItem);
  if (FItem.Amount > 1) then
    MsgLog.Add(Format(_('You drop %s (%dx).'), [Items.GetNameThe(FItem),
      FItem.Amount]))
  else
    MsgLog.Add(Format(_('You drop %s.'), [Items.GetNameThe(FItem)]));
  Scenes.SetScene(scDrop);
  Wait();
end;

procedure TPlayer.PickUp();
var
  FCount: Int;
begin
  Statictics.Inc(stFound);
  Corpses.DelCorpse(X, Y);
  /// / Your backpack is full!
  FCount := Items_Dungeon_GetMapCountXY(Ord(Map.Current), X, Y);
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
      Game.Timer := UIntMax;
      Scenes.SetScene(scItems);
    end;
  end
  else
    MsgLog.Add(_('There is nothing here to pick up.'));
end;

procedure TPlayer.PickUpAmount(Index: Int);
var
  FItem: Item;
begin
  FItem := Items_Dungeon_GetMapItemXY(Ord(Map.Current), Index, X, Y);
  FItem.Amount := FItem.Amount - ItemAmount;
  Items_Dungeon_SetMapItemXY(Ord(Map.Current), Index, X, Y, FItem);
  FItem.Amount := ItemAmount;
  Items_Inventory_AppendItem(FItem);
  if (FItem.Amount > 1) then
    MsgLog.Add(Format(_('You picked up %s (%dx).'), [Items.GetNameThe(FItem),
      FItem.Amount]))
  else
    MsgLog.Add(Format(_('You picked up %s.'), [Items.GetNameThe(FItem)]));
  Scenes.SetScene(scItems);
  Wait();
end;

procedure TPlayer.Render(AX, AY: UInt);
begin
  if IsDead then
    Terminal.Print(AX + View.Left, AY + View.Top, '%', clCorpse)
  else
  begin
    if Look then
      Terminal.Print(AX + View.Left, AY + View.Top, '@', clPlayer)
    else
      Terminal.Print(AX + View.Left, AY + View.Top, '@', clPlayer, clBkPlayer);
  end;
end;

procedure TPlayer.RenderInfo;
const
  F = '%s %d/%d';
var
  I: TAbilityEnum;
  S: string;
begin
  Terminal.ForegroundColor(clDefault);
  // Info
  Terminal.Print(Status.Left - 1, Status.Top + 1, ' ' + UI.Icon(icLife, 'Life')
    + ' ' + Terminal.Colorize(Format(F, [_('Life'),
    Attributes.Attrib[atLife].Value, Attributes.Attrib[atMaxLife].Value]
    ), 'Life'));
  Terminal.Print(Status.Left - 1, Status.Top + 2, ' ' + UI.Icon(icMana, 'Mana')
    + ' ' + Terminal.Colorize(Format(F, [_('Mana'),
    Self.Attributes.Attrib[atMana].Value, Self.Attributes.Attrib[atMaxMana]
    .Value]), 'Mana'));
  // Bars
  UI.Bar(Status.Left, 15, Status.Top + 1, Status.Width - 16,
    Attributes.Attrib[atLife].Value, Attributes.Attrib[atMaxLife].Value, clLife,
    clDarkGray);
  UI.Bar(Status.Left, 15, Status.Top + 2, Status.Width - 16,
    Self.Attributes.Attrib[atMana].Value, Self.Attributes.Attrib[atMaxMana]
    .Value, clMana, clDarkGray);
  case Game.ShowEffects of
    False:
      begin
        Terminal.Print(Status.Left - 1, Status.Top + 3,
          ' ' + Format('%s%d %s%d %s%d-%d %s%d %s', [UI.Icon(icFlag),
          Statictics.Get(stTurn), UI.Icon(icGold), Gold, UI.Icon(icSword),
          GetDamage.Min, GetDamage.Max, UI.Icon(icShield),
          Attributes.Attrib[atPV].Value, GetSatiationStr()]));
        Self.RenderWeather(Status.Left + (Status.Width div 2), Status.Top + 5,
          Status.Width);
      end;
  else
    begin
      S := '';
      for I := Low(TAbilityEnum) to High(TAbilityEnum) do
        if Abilities.IsAbility(I) then
          S := S + Terminal.Colorize(Format(' %s (%d)', [Abilities.GetName(I),
            Abilities.Ability[I]]), Abilities.GetColor(I));
      Terminal.Print(Status.Left, Status.Top + 3, Log.Width, 2, S,
        TK_ALIGN_TOP);
    end;
  end;
end;

procedure TPlayer.RenderWeather(const AX, AY, AWidth: UInt);
var
  SunOrMoonGlyphColor, SunOrMoonGlyph, SunOrMoon, SkyColor, SkyBef,
    SkyAft: string;
  Left: UInt;

  procedure Add(const ASunOrMoonGlyph, ASunOrMoonGlyphColor, ASkyColor: string);
  begin
    SunOrMoonGlyph := ASunOrMoonGlyph;
    SunOrMoonGlyphColor := ASunOrMoonGlyphColor;
    SkyColor := ASkyColor;
  end;

begin
  if Calendar.IsDay then
    Add(UI.Icon(icSun), 'Light Yellow', 'Lightest Blue')
  else
    Add(UI.Icon(icMoon), 'Light White', 'Darker Gray');
  Left := Round(Calendar.Hour / 24 * AWidth);
  SunOrMoon := Terminal.Colorize(SunOrMoonGlyph, SunOrMoonGlyphColor);
  SkyBef := Terminal.Colorize(StringOfChar('_', Left), SkyColor);
  SkyAft := Terminal.Colorize(StringOfChar('_', AWidth - Left - 1), SkyColor);
  Terminal.Print(AX, AY, SkyBef + SunOrMoon + SkyAft, TK_ALIGN_CENTER);
end;

function TPlayer.SaveCharacterDump(AReason: string): string;
var
  MorgueFileName: string;
  SL: TStringList;

  function GetDateTime(DateSep: Char = '.'; TimeSep: Char = ':'): string;
  begin
    Result := DateToStr(Date) + '-' + TimeToStr(Time);
    Result := StringReplace(Result, '.', DateSep, [rfReplaceAll]);
    Result := StringReplace(Result, ':', TimeSep, [rfReplaceAll]);
  end;

begin
  Result := '';
  if Mode.Wizard then
    Exit;
  SL := TStringList.Create;
  try
    SL.Append(Format(FT, [Game.GetTitle]));
    SL.Append('');
    SL.Append(GetDateTime);
    SL.Append(Format('%s: %s.', [_('Difficulty'),
      GetPureText(Game.GetStrDifficulty)]));
    SL.Append('');
    SL.Append(Player.Name);
    SL.Append(AReason);
    if IsDead then
      SL.Append(Format(_('He scored %d points.'), [Statictics.Get(stScore)]))
    else
      SL.Append(Format(_('He has scored %d points so far.'),
        [Statictics.Get(stScore)]));
    SL.Append('');
    SL.Append(Format(FT, [_('Statistics')]));
    SL.Append(Format(_('Game time: %d turns.'), [Statictics.Get(stTurn)]));
    SL.Append('');
    SL.Append(Format(FT, [_('Screenshot')]));
    SL.Append(Game.Screenshot);
    SL.Append(Format(FT, [_('Defeated foes')]));
    SL.Append('');
    SL.Append(Format('Total: %d creatures defeated.',
      [Statictics.Get(stKills)]));
    SL.Append('');
    SL.Append(Format(FT, [_('Last messages')]));
    SL.Append('');
    SL.Append(GetPureText(MsgLog.GetLastMsg(10)));
    SL.Append(Format(FT, [_('Inventory')]));
    SL.Append('');
    SL.Append(GetPureText(Items.GetInventory));
    SL.Append(Format('%s: %d', [_('Gold'), Gold]));
    ForceDirectories(Game.GetPath('morgue'));
    MorgueFileName := Format('%s-%s-character-dump.txt',
      [Player.Name, GetDateTime('-', '-')]);
    SL.SaveToFile(Game.GetPath('morgue') + MorgueFileName{$IFNDEF FPC},
      TEncoding.UTF8{$ENDIF});
  finally
    FreeAndNil(SL);
  end;
end;

procedure TPlayer.SetAmountScene(IsDrop: Boolean; Index, Amount: Int);
begin
  ItemIsDrop := IsDrop;
  ItemIndex := Index;
  ItemAmount := Amount;
  Scenes.SetScene(scAmount);
end;

procedure TPlayer.Spawn;
begin
  X := Game.Spawn.X;
  Y := Game.Spawn.Y;
  Map.Current := deDark_Wood;
  MsgLog.Clear;
end;

procedure TPlayer.AddExp(Value: UInt = 1);
begin
  Attributes.Modify(atExp, Value);
  if (Attributes.Attrib[atExp].Value >= LevelExpMax) then
  begin
    Attributes.Modify(atExp, -LevelExpMax);
    Attributes.Modify(atLev, 1);
    // You leveled up! You are now level %d!
    MsgLog.Add(Terminal.Colorize(Format(_('You advance to level %d!'),
      [Attributes.Attrib[atLev].Value]), clAlarm));
    if (Attributes.Attrib[atLev].Value mod 2 = 1) then
    begin
      Talents.IsPoint := True;
      MsgLog.Add(Terminal.Colorize(_('You gained 1 talent point.'), clAlarm));
      Statictics.Inc(stScore)
    end
    else
      Talents.IsPoint := False;
    Statictics.Inc(stTurn, Attributes.Attrib[atLev].Value * Attributes.Attrib
      [atLev].Value);
  end;
end;

procedure TPlayer.Wait;
begin
  if not Map.GetVis(Map.Current) then
  begin
    MsgLog.Add(Terminal.Colorize
      (Format(_('You have opened a new territory: %s.'), [Map.Name]), clAlarm));
    Map.SetVis(Map.Current, True);
    if (Ord(Map.Current) > 0) then
      Statictics.Inc(stScore, Ord(Map.Current) * 15);
    MaxMap := MaxMap + 1;
  end;
  SatPerTurn := 1;
  Move(drOrigin);
end;

procedure TPlayer.Rest(ATurns: UInt);
var
  T: UInt;
begin
  IsRest := True;
  MsgLog.Add(Format(_('Start rest (%d turns)!'), [ATurns]));
  for T := 1 to ATurns do
  begin
    if not IsRest then
      Break;
    Wait();
  end;
  MsgLog.Add(Format(_('Finish rest (%d turns)!'), [T - 1]));
  Abilities.Ability[abWeak] := 0;
  if (Math.RandomRange(0, 9) = 0) then
    Abilities.Ability[abDrunk] := 0;
  IsRest := False;
end;

procedure TPlayer.Start();
begin
  Exit;
  // ShowMessage('');
  // Add armors
  if Mode.Wizard then
  begin
    Items.AddItemToInv(ivWinged_Helm, 1, True, False);
    Items.AddItemToInv(ivPlate_Mail, 1, True, False);
    Items.AddItemToInv(ivPlated_Gauntlets, 1, True, False);
    Items.AddItemToInv(ivPlate_Boots, 1, True, False);
  end
  else
  begin
    if (Game.Difficulty < dfHard) then
    begin
      Items.AddItemToInv(ivCap, 1, True, True);
      Items.AddItemToInv(ivQuilted_Armor, 1, True, True);
    end;
    if (Game.Difficulty < dfNormal) then
    begin
      Items.AddItemToInv(ivLeather_Gloves, 1, True, True);
      Items.AddItemToInv(ivShoes, 1, True, True);
    end;
  end;
  { // Add weapon
    if Mode.Wizard then
    begin
    case Math.RandomRange(0, 4) of
    0:
    Items.AddItemToInv(ivTroll_Slayer, 1, True, False);
    1:
    Items.AddItemToInv(ivDemon_Axe, 1, True, False);
    2:
    Items.AddItemToInv(ivHoned_Spear, 1, True, False);
    3:
    Items.AddItemToInv(ivDoom_Hammer, 1, True, False);
    end;
    end
    else
    begin
    case Math.RandomRange(0, 4) of
    0:
    Items.AddItemToInv(ivRusty_Sword, 1, True, True);
    1:
    Items.AddItemToInv(ivHatchet, 1, True, True);
    2:
    Items.AddItemToInv(ivShort_Spear, 1, True, True);
    3:
    Items.AddItemToInv(ivSlag_Hammer, 1, True, True);
    end;
    end; }
  // Add runes, potions and scrolls
  if Mode.Wizard then
  begin
    Items.AddItemToInv(ivRune_of_Full_Healing);
    Items.AddItemToInv(ivPotion_of_Full_Healing, 10);
    Items.AddItemToInv(ivPotion_of_Full_Mana, 10);
    // Items.AddItemToInv(ivAntidote, 10);
    Items.AddItemToInv(ivScroll_of_Town_Portal, 10);
    Items.AddItemToInv(ivScroll_of_Identify, 10);
    Items.AddItemToInv(ivScroll_of_Full_Identify, 5);
    Items.AddItemToInv(ivScroll_of_Bloodlust, 10);
    Items.AddItemToInv(ivScroll_of_Enchant_Item, 10);
  end
  else
  begin
    // Items.AddItemToInv(ivLesser_Healing_Potion, 5);
    // Items.AddItemToInv(ivLesser_Mana_Potion, 5);
    // Items.AddItemToInv(ivAntidote, 3);
    Items.AddItemToInv(ivScroll_of_Town_Portal);
    Items.AddItemToInv(ivScroll_of_Identify);
  end;
  { Items.AddItemToInv(ivMoonstone_Ring);
    Items.AddItemToInv(ivMoonstone_Ring);
    Items.AddItemToInv(ivMoonstone_Ring);
    Items.AddItemToInv(ivMoonstone_Ring);
    Items.AddItemToInv(ivMoonstone_Ring);
    Items.AddItemToInv(ivMoonstone_Ring);
    Items.AddItemToInv(ivFlawed_Diamond);
    Items.AddItemToInv(ivCap, 1, True, True, Ord(of_LifeAfEachKill1));
  }  { // Flasks
    D := Math.IfThen(Mode.Wizard, 3, 3);
    for I := 1 to D do
    begin
    Items.AddItemToInv(ivBasalt_Flask0, 1, False, True);
    end;
    for I := 1 to D do
    begin
    Items.AddItemToInv(ivBasalt_Flask, 1, False, True);
    end;
    for I := 1 to D do
    begin
    Items.AddItemToInv(ivEternal_Flask, 1, False, True);
    end;
    for I := 1 to D do
    begin
    Items.AddItemToInv(ivEternal_Flask2, 1, False, True);
    end; }
end;

procedure TPlayer.StartEquip;
var
  D: UInt;
begin
  // Main Weapon
  Items.AddItemToInv(ClassMainWeapon[HClass], 1, True, True);

  // Add foods
  Items.AddItemToInv(ivBread_Ration, IfThen(Mode.Wizard, 9, 5));
  Items.AddItemToInv(ivTorch, IfThen(Mode.Wizard, 9, 3));
  // Add coins
  D := IfThen(Game.Difficulty <> dfHell, StartGold, 0);
  Items.AddItemToInv(ivGold, IfThen(Mode.Wizard, RandomRange(3333, 9999), D));
end;

procedure TPlayer.StartSkills;
begin
  // Weapon
  Skills.Modify(ClassWeaponSkill[HClass], StartSkill);
  // Main
  Skills.Modify(ClassMainSkill[HClass], StartSkill);
  // Add
  Skills.Modify(ClassAddSkill[HClass], StartSkill);
  // Calc
  Calc;
  Fill;
end;

procedure TPlayer.DoEffects(const Effects: TEffects; const Value: UInt = 0);
var
  V, VX, VY: UInt;
  Ef: TEffect;
const
  F = '%s +%d.';

  procedure PrmSkill(ASkill: TSkillEnum);
  begin
    Skills.Modify(ASkill, StartSkill);
    Calc();
    Fill();
  end;

  procedure PrmValue(AEffect: TEffect; Value: UInt);
  begin
    case AEffect of
      efPrmLife:
        Attributes.Modify(atMaxLife, 0, Value);
      efPrmMana:
        Attributes.Modify(atMaxMana, 0, Value);
      efPrmPV:
        Attributes.Modify(atPV, 0, Value);
      efPrmDV:
        Attributes.Modify(atDV, 0, Value);
      efPrmStr:
        Attributes.Modify(atStr, 0, Value);
      efPrmDex:
        Attributes.Modify(atDex, 0, Value);
      efPrmWil:
        Attributes.Modify(atWil, 0, Value);
      efPrmPer:
        Attributes.Modify(atPer, 0, Value);
    end;
    Calc();
    Fill();
  end;

begin
  // Life
  if (efLife in Effects) then
  begin
    V := Value;
    case RandomRange(0, 3) of
      0:
        MsgLog.Add(_('You feel healthy.'));
      1:
        MsgLog.Add(_('You feel a bit better.'));
      2:
        MsgLog.Add(_('You feel a wee bit better.'));
    end;
    MsgLog.Add(Format(F, [_('Life'), Min(Attributes.Attrib[atMaxLife].Value -
      Attributes.Attrib[atLife].Value, V)]));
    Attributes.Modify(atLife, V);
  end;
  // Mana
  if (efMana in Effects) then
  begin
    V := Skills.Skill[skConcentration].Value + Value;
    MsgLog.Add(_('You feel magical energies restoring.'));
    MsgLog.Add(Format(F, [_('Mana'), Min(Self.Attributes.Attrib[atMaxMana].Value
      - Self.Attributes.Attrib[atMana].Value, V)]));
    Self.Attributes.Modify(atMana, V);
    Skills.DoSkill(skConcentration);
  end;
  // Food
  if (efFood in Effects) then
  begin
    Attributes.Modify(atSat, Value);
    MsgLog.Add(Format(_('You have sated %d hunger.'), [Value]));
  end;
  // Identification
  if (efIdentification in Effects) then
    Scenes.SetScene(scIdentification);
  if (efAllIdentification in Effects) then
    Player.IdentAllItems;
  // Craft
  for Ef := CraftEffLow to CraftEffHigh do
    if (Ef in Effects) then
    begin
      Affixes.DoCraft(Ef, Value);
      Scenes.SetScene(scCraft);
    end;
  // Enchant Item
  if (efEnchantItem in Effects) then
  begin
    Affixes.DoCraft(TEffect(Math.RandomRange(0, 4) + Ord(efCraftStr)),
      Math.EnsureRange(Player.Skills.Skill[skEnchant_Item].Value div 10, 0, 7));
    Scenes.SetScene(scCraft);
  end;
  // Repair
  if (efRepair in Effects) then
  begin
    Items.Index := Value;
    Scenes.SetScene(scRepair, scInv);
  end;
  // Teleportation
  if (efTeleportation in Effects) then
  begin
    VX := Math.RandomRange(Value, Self.Skills.Skill[skConcentration]
      .Value + Value);
    VY := Math.RandomRange(Value, Self.Skills.Skill[skConcentration]
      .Value + Value);
    X := Map.EnsureRange(X + (Math.RandomRange(0, VX * 2 + 1) - VX));
    Y := Map.EnsureRange(Y + (Math.RandomRange(0, VY * 2 + 1) - VY));
    MsgLog.Add(_('You have teleported into new place!'));
    Scenes.SetScene(scGame);
  end;
  // Town Portal
  if (efTownPortal in Effects) then
  begin
    Map.SetTileEnum(Game.Portal.X, Game.Portal.Y, Game.PortalMap,
      Game.PortalTile);
    if ((X = Game.Spawn.X) and (Y = Game.Spawn.Y)) then
      Exit;
    Game.PortalTile := Map.GetTileEnum(X, Y, Map.Current);
    Game.PortalMap := Map.Current;
    Game.Portal.X := X;
    Game.Portal.Y := Y;
    Map.SetTileEnum(X, Y, Map.Current, tePortal);
    Map.SetTileEnum(Game.Spawn.X, Game.Spawn.Y, deDark_Wood, teTownPortal);
    Scenes.SetScene(scGame);
  end;
  // Magic Eye
  if (efMagicEye in Effects) then
  begin

  end;
  // Light
  if (efLight in Effects) then
  begin
    Abilities.Modify(abLight, Value);
    // MsgLog.Add(Format(_('You feel lust for blood (%d).'), [Value]));
    Self.Calc;
  end;
  // Berserk
  if (efBerserk in Effects) then
  begin
    Abilities.Modify(abBerserk, Value);
    MsgLog.Add(Format(_('You feel a sudden urge to kill things. (%d).'),
      [Value]));
  end;
  // Bloodlust
  if (efBloodlust in Effects) then
  begin
    V := Math.RandomRange(Value, Skills.Skill[skConcentration].Value + Value);
    Abilities.Modify(abBloodlust, V);
    MsgLog.Add(Format(_('You feel lust for blood (%d).'), [V]));
  end;
  // Cure poison
  if (efCurePoison in Effects) then
  begin
    if Abilities.IsAbility(abPoisoned) then
    begin
      V := Value;
      Abilities.Ability[abPoisoned] :=
        Math.EnsureRange(Abilities.Ability[abPoisoned] - V, 0, UIntMax);
      if Abilities.IsAbility(abPoisoned) then
        MsgLog.Add(_('You feel better.'))
      else
        MsgLog.Add(_('You are better now.'));
    end;
  end;
  // Cure weak
  if (efCureWeak in Effects) then
  begin
    if Abilities.IsAbility(abWeak) then
    begin
      Abilities.Ability[abWeak] := 0;
      MsgLog.Add(_('You are better now.'));
    end;
  end;
  // Gold
  if (efPrmGold in Effects) then
  begin
    Items.AddItemToInv(ivGold, StartGold);
    Player.Calc;
  end;
  // Athletics
  if (efPrmAthletics in Effects) then
    PrmSkill(skAthletics);
  // Dodge
  if (efPrmDodge in Effects) then
    PrmSkill(skDodge);
  // Concentration
  if (efPrmConcentration in Effects) then
    PrmSkill(skConcentration);
  // Toughness
  if (efPrmToughness in Effects) then
    PrmSkill(skToughness);
  // Blade
  if (efPrmBlade in Effects) then
    PrmSkill(skBlade);
  // Axe
  if (efPrmAxe in Effects) then
    PrmSkill(skAxe);
  // Spear
  if (efPrmSpear in Effects) then
    PrmSkill(skSpear);
  // Mace
  if (efPrmMace in Effects) then
    PrmSkill(skMace);
  // Staff
  if (efPrmStaff in Effects) then
    PrmSkill(skStaff);
  // Wand
  if (efPrmWand in Effects) then
    PrmSkill(skWand);
  // Dagger
  if (efPrmDagger in Effects) then
    PrmSkill(skDagger);
  // Bow
  if (efPrmBow in Effects) then
    PrmSkill(skBow);
  // Bodybuilding
  if (efPrmBodybuilding in Effects) then
    PrmSkill(skBodybuilding);
  // Meditation
  if (efPrmMeditation in Effects) then
    PrmSkill(skMeditation);
  // Enchant Item
  if (efPrmEnchant_Item in Effects) then
    PrmSkill(skEnchant_Item);
  // 2x to gold
  if (ef2xGold in Effects) then
  begin

  end;
  // Life
  if (efPrmLife in Effects) then
  begin
    PrmValue(efPrmLife, IfThen(Value = 0, AttribPrm, Value));
    MsgLog.Add(_('You increased your amount of life.'));
  end;
  // Mana
  if (efPrmMana in Effects) then
  begin
    PrmValue(efPrmMana, IfThen(Value = 0, AttribPrm, Value));
    MsgLog.Add(_('You increased your amount of mana.'));
  end;
  // DV
  if (efPrmDV in Effects) then
  begin
    PrmValue(efPrmDV, IfThen(Value = 0, TalentPrm, Value));
    MsgLog.Add(_('You increased a defense level'));
  end;
  // PV
  if (efPrmPV in Effects) then
  begin
    PrmValue(efPrmPV, IfThen(Value = 0, TalentPrm, Value));
    MsgLog.Add(_('You increased a protection level'));
  end;
  // Strength
  if (efPrmStr in Effects) then
  begin
    PrmValue(efPrmStr, IfThen(Value = 0, MinPrm, Value));
    MsgLog.Add(Format(_('Strength +%d'), [Value]));
  end;
  // Dexterity
  if (efPrmDex in Effects) then
  begin
    PrmValue(efPrmDex, IfThen(Value = 0, MinPrm, Value));
    MsgLog.Add(Format(_('Dexterity +%d'), [Value]));
  end;
  // Willpower
  if (efPrmWil in Effects) then
  begin
    PrmValue(efPrmWil, IfThen(Value = 0, MinPrm, Value));
    MsgLog.Add(Format(_('Willpower +%d'), [Value]));
  end;
  // Perception
  if (efPrmPer in Effects) then
  begin
    PrmValue(efPrmPer, IfThen(Value = 0, MinPrm, Value));
    MsgLog.Add(Format(_('Perception +%d'), [Value]));
  end;
end;

procedure TPlayer.Turn();
var
  Turns: UInt;
begin
  // Regen
  if Abilities.IsAbility(abRegen) then
  begin
    Attributes.Modify(atLife);
    Attributes.Modify(atMana, Math.RandomRange(0, 3) + 1);
  end;
  if not Abilities.IsAbility(abDiseased) then
  begin
    // Replenish Life
    Turns := LifeTurnMax - Skills.Skill[skBodybuilding].Value;
    if (Statictics.Get(stTurn) mod Turns = 0) then
      Attributes.Modify(atLife, Skills.Skill[skBodybuilding].Value);
    // Regenerate Mana
    Turns := ManaTurnMax - Skills.Skill[skMeditation].Value;
    if (Statictics.Get(stTurn) mod Turns = 0) then
      Attributes.Modify(atMana, Skills.Skill[skMeditation].Value);
  end;
end;

initialization

Player := TPlayer.Create;

finalization

FreeAndNil(Player);

end.
