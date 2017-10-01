unit uPlayer;

interface

uses Types, uCreature, uMob, uSkill, uStatistic, uTalent;

type
  TSlotType = (stNone, stHead, stTorso, stHands, stFeet, stMainHand, stOffHand,
    stNeck, stFinger);

const
  // Player
  VisionMax = 15;
  DVMax = 80;
  LevelExpMax = 8;
  // Satiation
  StarvingMax = 500;
  SatiatedMax = 8000;
  EngorgedMax = 15000;
  // Inventory
  ItemMax = 26;
  StartGold = 250;
  // Talents
  TalentPrm = 3;
  AttribPrm = 7;

type
  TDirectionEnum = (drEast, drWest, drSouth, drNorth, drSouthEast, drSouthWest,
    drNorthEast, drNorthWest, drOrigin);

const
  Direction: array [TDirectionEnum] of TPoint = ((X: 1; Y: 0), (X: -1; Y: 0),
    (X: 0; Y: 1), (X: 0; Y: -1), (X: 1; Y: 1), (X: -1; Y: 1), (X: 1; Y: -1),
    (X: -1; Y: -1), (X: 0; Y: 0));

type
  TPlayer = class(TCreature)
  private
    FLX: Byte;
    FLY: Byte;
    FMana: Word;
    FMaxMana: Word;
    FMaxMap: Byte;
    FLook: Boolean;
    FGold: Integer;
    FKiller: string;
    FWeaponSkill: TSkillEnum;
    FItemIsDrop: Boolean;
    FItemIndex: Integer;
    FItemAmount: Integer;
    FSatPerTurn: Byte;
    FBackground: string;
    FIsRest: Boolean;
    FName: string;
    FStatistics: TStatistics;
    FTalents: TTalents;
    FSkills: TSkills;
    procedure GenNPCText;
    function GetVision: Byte;
    function GetSatiation: Word;
    function GenerateBackground(): string;
  public
    constructor Create;
    destructor Destroy; override;
    property LX: Byte read FLX write FLX;
    property LY: Byte read FLY write FLY;
    property Satiation: Word read GetSatiation; // Nutrition
    property Mana: Word read FMana write FMana;
    property MaxMana: Word read FMaxMana write FMaxMana;
    property Vision: Byte read GetVision;
    property MaxMap: Byte read FMaxMap write FMaxMap;
    property Look: Boolean read FLook write FLook;
    property Gold: Integer read FGold write FGold;
    property Killer: string read FKiller write FKiller;
    property IsRest: Boolean read FIsRest write FIsRest;
    property ItemIsDrop: Boolean read FItemIsDrop write FItemIsDrop;
    property ItemIndex: Integer read FItemIndex write FItemIndex;
    property ItemAmount: Integer read FItemAmount write FItemAmount;
    property SatPerTurn: Byte read FSatPerTurn write FSatPerTurn;
    property Statictics: TStatistics read FStatistics write FStatistics;
    property Background: string read FBackground;
    property Name: string read FName write FName;
    property Skills: TSkills read FSkills write FSkills;
    property Talents: TTalents read FTalents write FTalents;
    procedure SetAmountScene(IsDrop: Boolean; Index, Amount: Integer);
    procedure Render(AX, AY: Byte);
    procedure Move(Dir: TDirectionEnum);
    procedure RenderInfo;
    procedure Calc;
    procedure Fill;
    procedure Wait;
    procedure Clear;
    procedure AddTurn;
    procedure Spawn;
    function GetSatiationStr: string;
    function SaveCharacterDump(AReason: string): string;
    procedure Defeat(AKiller: string = '');
    procedure Attack(Index: Integer);
    procedure ReceiveHealing;
    procedure Buy(Index: Integer);
    procedure PickUp;
    procedure PickUpAmount(Index: Integer);
    procedure Drop(Index: Integer);
    procedure DropAmount(Index: Integer);
    procedure Use(Index: Integer);
    procedure DoEffects(const Effects: TEffects; const Value: Word = 0);
    procedure Equip(Index: Integer);
    procedure UnEquip(Index: Integer);
    procedure Sell(Index: Integer);
    procedure RepairItem(Index: Integer);
    procedure IdentItem(Index: Integer);
    procedure BreakItem(Index: Integer; Value: Byte = 1); overload;
    procedure BreakItem(ASlot: TSlotType; Value: Byte = 1); overload;
    procedure AddExp(Value: Byte = 1);
    procedure Start;
    procedure Rest(ATurns: Word);
    procedure Dialog(AMob: TMob);
    procedure AutoPickup();
    procedure RenderWeather(const AX, AY, AWidth: Byte);
  end;

var
  Player: TPlayer = nil;

implementation

uses Classes, SysUtils, Dialogs, Math, IniFiles, uItem, uGame, uMap, uScenes,
  uTerminal, uMsgLog, GNUGetText, BeaRLibItems, uCorpse, uCalendar,
  uShop, BearLibTerminal, uAbility, uAffixes, uAttribute, uSpellbook, uUI;

{ TPlayer }

// Generate a random background (from Kharne roguelike)
function TPlayer.GenerateBackground(): string;
type
  TConPartsEnum = (cpChild, cpClass, cpParent, cpCredit, cpBackground,
    cpEyeType, cpEyeColour, cpHairStyle, cpHairColour, cpComplexion);
var
  I: TConPartsEnum;
  SL: array[TConPartsEnum] of TStringList;
begin
  Randomize;
  Result := '';
  for I := Low(TConPartsEnum) to High(TConPartsEnum) do
    SL[I] := TStringList.Create;
  try
    SL[cpChild].DelimitedText := _('"an only child","one of two children",' +
				'"one of many children","the only surviving child"');
		SL[cpClass].DelimitedText := _('"lower-class", "middle-class",' +
				'"upper-class"');
		SL[cpParent].DelimitedText := _('"mercenary","merchant","businessman",' +
				'"craftsman","soldier","templar","priest"');
		SL[cpBackground].DelimitedText := _('"contented","peaceful",' +
				'"troubled","settled","disturbed"');
		SL[cpCredit].DelimitedText := _('"a credit to","a disgrace to",' +
				'"the black sheep of"');
		SL[cpEyeType].DelimitedText := _('"dull","unusually piercing",' +
				'"piercing","striking"');
		SL[cpEyeColour].DelimitedText := _('"grey","violet","green","blue",' +
				'"brown"');
		SL[cpHairStyle].DelimitedText := _('"wavy","curly","straight","short",' +
				'"long"');
		SL[cpHairColour].DelimitedText := _('"auburn","blonde","black","dark",' +
				'"ginger","grey"');
		SL[cpComplexion].DelimitedText := _('"an average","a sallow","a fair",' +
				'"a dark","a light"');

			Result := Format(Terminal.Colorize(_('You are %s of a %s %s. You had a %s upbringing and you ' +
				'are %s the family. You have %s %s eyes, %s %s hair, and %s complexion.'), 'Yellow'),
				[SL[cpChild][Random(SL[cpChild].Count - 1)],
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
    for I := Low(TConPartsEnum) to High(TConPartsEnum) do
      FreeAndNil(SL[I]);
	end;
end;


procedure TPlayer.AddTurn;
var
  V, C: Byte;
begin
  if IsDead then
    Exit;
  Statictics.Inc(stTurn);  
  Calendar.Turn;
  if (Satiation > 0) then
    Attributes.Modify(atSat, -SatPerTurn);
  if Abilities.IsAbility(abWeak) then
    Attributes.Modify(atSat, -10);
  if (Satiation < StarvingMax) then
  begin
    Life := EnsureRange(Life - 1, 0, MaxLife);
  end
  else if not Abilities.IsAbility(abDiseased) then
  begin
    V := EnsureRange(100 - Skills.Skill[skHealing].Value, 25, 100);
    if (Statictics.Get(stTurn) mod V = 0) then
    begin
      C := Skills.Skill[skHealing].Value;
      if Abilities.IsAbility(abRegen) then
        C := EnsureRange(C * 3, C, High(Byte));
      Life := EnsureRange(Life + C, 0, MaxLife);
    end;
    V := EnsureRange(100 - Skills.Skill[skConcentration].Value, 25, 100);
    if (Statictics.Get(stTurn) mod V = 0) then
    begin
      C := Skills.Skill[skConcentration].Value;
      if Abilities.IsAbility(abRegen) then
        C := EnsureRange(C * 3, C, High(Byte));
      Mana := EnsureRange(Mana + C, 0, MaxMana);
    end;
  end;
  OnTurn();
  if (Life = 0) then
    Self.Defeat;
  Mobs.Process;
end;

procedure TPlayer.Attack(Index: Integer);
var
  V, Ch: Byte;
  Mob: TMob;
  Dam, Cr: Word;
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
  if (Mob.Attributes.Attrib[atDV].Value < Math.RandomRange(0, 100)) and not Abilities.IsAbility(abCursed)
  then
  begin
    CrStr := '';
    // Attack
    Dam := EnsureRange(RandomRange(Self.GetDamage.Min, GetDamage.Max + 1), 0,
      High(Word));
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
    Mob.Life := EnsureRange(Mob.Life - Dam, 0, Mob.Life);
    MsgLog.Add(Format(_('You hit %s (%d).'), [The, Dam]));
    // Break weapon
    if ((Math.RandomRange(0, 10 - Ord(Game.Difficulty)) = 0) and not Game.Wizard)
    then
      BreakItem(stMainHand);
    if (CrStr <> '') then
      MsgLog.Add(Terminal.Colorize(CrStr, clAlarm));
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
      skSpear:
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
    end;
    // Victory
    if (Mob.Life = 0) then
      Mob.Defeat;
  end
  else
    Miss();
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
      if ((ItemType in CoinTypeItems) and not Game.GetOption(apCoin)) then
        Exit;
      if ((ItemType in FoodTypeItems) and not Game.GetOption(apFood)) then
        Exit;
      if ((ItemType in PlantTypeItems) and not Game.GetOption(apPlant)) then
        Exit;
      if ((ItemType in PotionTypeItems) and not Game.GetOption(apPotion)) then
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
  FAttrib: array [TAttribEnum] of Word;
  I, FCount: Integer;
  ID: TItemEnum;
  FItem: Item;

  procedure AddAttrib(const AAttrib: TAttribEnum; const Value: Word);
  begin
    FAttrib[AAttrib] := FAttrib[AAttrib] + Value;
  end;

  procedure ClearAttrib();
  var
    I: TAttribEnum;
  begin
    for I := Low(TAttribEnum) to High(TAttribEnum) do FAttrib[I] := 0;
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
      else
        Result := skNone;
    end;
  end;

begin
  ClearAttrib();
  FCount := EnsureRange(Items_Inventory_GetCount(), 0, ItemMax);
  for I := 0 to FCount - 1 do
  begin
    FItem := Items_Inventory_GetItem(I);
    if (FItem.Equipment > 0) then
    begin
      if (FItem.Identify = 0) then Continue;
      ID := TItemEnum(FItem.ItemID);
      AddAttrib(atDef, FItem.Defense);
      AddAttrib(atMinDamage, FItem.MinDamage);
      AddAttrib(atMaxDamage, FItem.MaxDamage);

//      FLife := FLife + FItem.?;
//      FMana := FMana + FItem.?;
//      FStr := FStr + FItem.?;
//      FDex := FDex + FItem.?;
//      FWil := FWil + FItem.?;
//      FPer := FPer + FItem.?;
      if (ItemBase[ID].SlotType = stMainHand) then
        FWeaponSkill := GetSkill(ItemBase[ID].ItemType);
    end;
  end;
  //
  Self.Gold := EnsureRange(Items_Inventory_GetItemAmount(Ord(iGold)), 0,
    High(Integer));
  //
  Attributes.SetValue(atStr, EnsureRange(Round(Skills.Skill[skAthletics].Value * 1.2) +
    Round(Skills.Skill[skToughness].Value * 0.2) + FAttrib[atStr], 1, AttribMax));
  Attributes.SetValue(atDex, EnsureRange(Round(Skills.Skill[skDodge].Value * 1.4) + FAttrib[atDex], 1, AttribMax));
  Attributes.SetValue(atWil, EnsureRange(Round(Skills.Skill[skConcentration].Value * 1.4) + FAttrib[atWil], 1, AttribMax));
  Attributes.SetValue(atPer, EnsureRange(Round(Skills.Skill[skToughness].Value * 1.4) + FAttrib[atPer], 1, AttribMax));
  //
  if (Abilities.IsAbility(abWeak)) then
  begin
    Attributes.SetValue(atStr, Attributes.Attrib[atStr].Value div 2);
    Attributes.SetValue(atDex, Attributes.Attrib[atDex].Value div 2);
  end;
  if Abilities.IsAbility(abAfraid) then
  begin
    Attributes.SetValue(atWil, Attributes.Attrib[atWil].Value div 3);
  end;
  if Abilities.IsAbility(abDrunk) then
  begin
    Attributes.SetValue(atPer, Attributes.Attrib[atPer].Value div 3);
  end;
  //
  Attributes.SetValue(atDV, EnsureRange(Round(Attributes.Attrib[atDex].Value * (DVMax / AttribMax))
    + Attributes.Attrib[atDV].Prm, 0, DVMax));
  Attributes.SetValue(atPV, EnsureRange(Round(Skills.Skill[skToughness].Value / 1.4) - 4
    + FAttrib[atDef] + Attributes.Attrib[atPV].Prm, 0, PVMax));
  MaxLife := Round(Attributes.Attrib[atStr].Value * 3.6) + Round(Attributes.Attrib[atDex].Value * 2.3) + FAttrib[atLife] + Attributes.Attrib[atMaxLife].Prm;
  MaxMana := Round(Attributes.Attrib[atWil].Value * 4.2) + Round(Attributes.Attrib[atDex].Value * 0.4) + FAttrib[atMana] + Attributes.Attrib[atMaxMana].Prm;
  Attributes.SetValue(atVis, Round(Attributes.Attrib[atPer].Value / 8.3));
  //
  Self.SetDamage(EnsureRange(FAttrib[atMinDamage] + Attributes.Attrib[atStr].Value div 3, 1, High(Byte) - 1),
    EnsureRange(FAttrib[atMaxDamage] + Attributes.Attrib[atStr].Value div 2, 2, High(Byte)));
end;

procedure TPlayer.Clear;
begin
  inherited Clear;
  Skills.Clear;
  Spellbook.Clear;
  Items_Inventory_Clear();
  Killer := '';
  Look := False;
  IsRest := False;
  SatPerTurn := 2;
  Attributes.SetValue(atSat, SatiatedMax);
  Gold := 0;
  MaxMap := 0;
  Name := _('PLAYER');
  FWeaponSkill := skNone;
  Attributes.SetValue(atLev, 1);
  FBackground := GenerateBackground();
  Calc;
  Fill;
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
  MsgLog.Add(Format(_('Press %s to try again...'), [UI.KeyToStr('SPACE')]));
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

function TPlayer.GetVision: Byte;
begin
  Result := EnsureRange((Attributes.Attrib[atVis].Value - Abilities.Ability[abBlinded]) + 3, 0,
    VisionMax);
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
      Result := Terminal.Colorize(Result, 'Light Red');
    StarvingMax + 1 .. SatiatedMax:
      Result := Terminal.Colorize(Result, 'Light Yellow');
  else
    Result := Terminal.Colorize(Result, 'Light Green');
  end;
end;

function TPlayer.GetSatiation: Word;
begin
  Result := EnsureRange(Attributes.Attrib[atSat].Value, 0, EngorgedMax);
end;

procedure TPlayer.Move(Dir: TDirectionEnum);
var
  FX, FY: Byte;
begin
  if Look then
  begin
    if Map.InMap(LX + Direction[Dir].X, LY + Direction[Dir].Y) and
      ((Map.InView(LX + Direction[Dir].X, LY + Direction[Dir].Y) and not Map.GetFog(LX + Direction[Dir].X, LY + Direction[Dir].Y)) or
      Game.Wizard) then
    begin
      LX := Map.EnsureRange(Math.EnsureRange(LX + Direction[Dir].X, X - (View.Width div 2),
        X + (View.Width div 2 - 1)));
      LY := Map.EnsureRange(Math.EnsureRange(LY + Direction[Dir].Y, Y - (View.Height div 2),
        Y + (View.Height div 2 - 1)));
    end;
  end
  else
  begin
    if IsDead then Exit;
    FX := Map.EnsureRange(X + Direction[Dir].X);
    FY := Map.EnsureRange(Y + Direction[Dir].Y);
    if (Map.GetTileEnum(FX, FY, Map.Current) in StopTiles) and not Game.Wizard
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

procedure TPlayer.Use(Index: Integer);
var
  FItem: Item;
  I: TItemEnum;
  T: TItemType;
  ItemLevel: Byte;
begin
  if IsDead then
    Exit;
  FItem := Items_Inventory_GetItem(Index);
  // Need level
  ItemLevel := ItemBase[TItemEnum(FItem.ItemID)].Level;
  if (Attributes.Attrib[atLev].Value < ItemLevel) and not Game.Wizard then
  begin
    MsgLog.Add(Format(_('You can not use this yet (need level %d)!'),
      [ItemLevel]));
    Self.Calc;
    Exit;
  end;
  I := TItemEnum(FItem.ItemID);
  T := ItemBase[I].ItemType;
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
        if (Self.Mana >= ItemBase[I].ManaCost) then
        begin
          Skills.DoSkill(skConcentration);
          Self.Mana := Self.Mana - ItemBase[I].ManaCost;
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
      DoEffects(ItemBase[I].Effects, ItemBase[I].Value);
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

procedure TPlayer.Equip(Index: Integer);
var
  FItem: Item;
  I: Integer;
  ItemLevel: Byte;
begin
  // Need level
  FItem := Items_Inventory_GetItem(Index);
  ItemLevel := ItemBase[TItemEnum(FItem.ItemID)].Level;
  if (Attributes.Attrib[atLev].Value < ItemLevel) and not Game.Wizard then
  begin
    MsgLog.Add(Format(_('You can not use this yet (need level %d)!'),
      [ItemLevel]));
    Self.Calc;
    Exit;
  end;
  if (FItem.Identify = 0) and not Game.Wizard then
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

procedure TPlayer.UnEquip(Index: Integer);
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

procedure TPlayer.Sell(Index: Integer);
var
  Value: Integer;
  FItem: Item;
begin
  FItem := Items_Inventory_GetItem(Index);
  if ((FItem.Equipment > 0) or Items.ChItem(FItem)) then
    Exit;
  if (Items_Inventory_DeleteItem(Index, FItem) > 0) then
  begin
    Value := FItem.Price div 4;
    Items.AddItemToInv(iGold, Value);
    MsgLog.Add(Format(_('You sold %s (+%d gold).'), [Items.GetNameThe(FItem), Value]));
  end;
  Self.Calc;
end;

procedure TPlayer.Buy(Index: Integer);
var
  FItem: Item;
begin
  FItem := Shops.Shop[Shops.Current].GetItem(Index);
  if (Items_Inventory_DeleteItemAmount(Ord(iGold), FItem.Price) > 0) then
  begin
    MsgLog.Add(Format(_('You bought %s (-%d gold).'), [Items.GetNameThe(FItem), FItem.Price]));
    Items_Inventory_AppendItem(FItem);
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
  Cost := Round((MaxLife - Life) * 1.6);
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

procedure TPlayer.IdentItem(Index: Integer);
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
    Scenes.SetScene(scInv);
  end;
  Self.Calc;
end;

procedure TPlayer.RepairItem(Index: Integer);
var
  RepairCost: Word;
  FItem: Item;
begin
  FItem := Items_Inventory_GetItem(Index);
  if ((FItem.Stack > 1) or (FItem.Amount > 1)) then
    Exit;
  RepairCost := (FItem.MaxDurability - FItem.Durability) * 10;
  if (RepairCost > 0) then
  begin
    if (Gold < RepairCost) then
    begin
      MsgLog.Add(_('You need more gold.'));
      Exit;
    end;
    FItem.Durability := FItem.MaxDurability;
    if ((Items_Inventory_DeleteItemAmount(Ord(iGold), RepairCost) > 0) and
      (Items_Inventory_SetItem(Index, FItem) > 0)) then
      MsgLog.Add(Format(_('You repaired %s (-%d gold).'), [Items.GetNameThe(FItem), RepairCost]));
  end;
  Self.Calc;
end;

procedure TPlayer.BreakItem(Index: Integer; Value: Byte = 1);
var
  FItem: Item;
begin
  FItem := Items_Inventory_GetItem(Index);
  if ((FItem.Stack > 1) or (FItem.Amount > 1)) then
    Exit;
  FItem.Durability := Math.EnsureRange(FItem.Durability - Value, 0, High(Byte));
  if ((FItem.Durability > 0) and
    (FItem.Durability < (FItem.MaxDurability div 4))) then
    MsgLog.Add(Terminal.Colorize(Format(_('%s soon will be totally broken (%d/%d).'),
      [Items.GetNameThe(FItem), FItem.Durability, FItem.MaxDurability]), clAlarm));
  Items_Inventory_SetItem(Index, FItem);
  if (FItem.Durability = 0) then
  begin
    Items_Inventory_DeleteItem(Index, FItem);
    MsgLog.Add(Terminal.Colorize(Format(_('%s been ruined irreversibly.'),
      [Items.GetNameThe(FItem)]), clAlarm));
  end;
  Self.Calc;
end;

procedure TPlayer.BreakItem(ASlot: TSlotType; Value: Byte = 1);
var
  FCount, I: Integer;
  FItem: Item;
  FI: TItemEnum;
begin
  FCount := EnsureRange(Items_Inventory_GetCount(), 0, ItemMax);
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

procedure TPlayer.Drop(Index: Integer);
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
  AItem := Items_Inventory_GetItem(Index);
  if (AItem.Equipment > 0) then
    Exit;
  if not((AItem.Stack > 1) and (AItem.Amount > 1)) then
    DeleteItem()
  else
    SetAmountScene(True, Index, 1);
  Self.Calc();
end;

procedure TPlayer.DropAmount(Index: Integer);
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
    MsgLog.Add(Format(_('You drop %s (%dx).'), [Items.GetNameThe(FItem), FItem.Amount]))
  else
    MsgLog.Add(Format(_('You drop %s.'), [Items.GetNameThe(FItem)]));
  Scenes.SetScene(scDrop);
  Wait();
end;

procedure TPlayer.PickUp();
var
  FCount: Integer;
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
      Game.Timer := High(Byte);
      Scenes.SetScene(scItems);
    end;
  end
  else
    MsgLog.Add(_('There is nothing here to pick up.'));
end;

procedure TPlayer.PickUpAmount(Index: Integer);
var
  FItem: Item;
begin
  FItem := Items_Dungeon_GetMapItemXY(Ord(Map.Current), Index, X, Y);
  FItem.Amount := FItem.Amount - ItemAmount;
  Items_Dungeon_SetMapItemXY(Ord(Map.Current), Index, X, Y, FItem);
  FItem.Amount := ItemAmount;
  Items_Inventory_AppendItem(FItem);
  if (FItem.Amount > 1) then
    MsgLog.Add(Format(_('You picked up %s (%dx).'), [Items.GetNameThe(FItem), FItem.Amount]))
  else
    MsgLog.Add(Format(_('You picked up %s.'), [Items.GetNameThe(FItem)]));
  Scenes.SetScene(scItems);
  Wait();
end;

procedure TPlayer.Render(AX, AY: Byte);
begin
  if (Self.Life = 0) then
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
  Terminal.Print(Status.Left - 1, Status.Top + 1,
    ' ' + Terminal.Icon('F8D7', 'Life') + ' ' + Terminal.Colorize(Format(F, [_('Life'), Life, MaxLife]
    ), 'Life'));
  Terminal.Print(Status.Left - 1, Status.Top + 2,
    ' ' + Terminal.Icon('F8D9', 'Mana') + ' ' + Terminal.Colorize(Format(F, [_('Mana'), Mana, MaxMana]
    ), 'Mana'));
  // Bars
  UI.Bar(Status.Left, 15, Status.Top + 1, Status.Width - 16,
    Life, MaxLife, clLife, clDarkGray);
  UI.Bar(Status.Left, 15, Status.Top + 2, Status.Width - 16,
    Mana, MaxMana, clMana, clDarkGray);
  case Game.ShowEffects of
    False:
      begin
        Terminal.Print(Status.Left - 1, Status.Top + 3,
          ' ' + Format(_('Turn: %d Gold: %d %s'), [Statictics.Get(stTurn), Gold,
          GetSatiationStr]));
        Terminal.Print(Status.Left - 1, Status.Top + 4,
          ' ' + Format(_('Damage: %d-%d PV: %d DV: %d'), [GetDamage.Min,
          GetDamage.Max, Attributes.Attrib[atPV].Value, Attributes.Attrib[atDV].Value,
            Satiation]));
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

procedure TPlayer.RenderWeather(const AX, AY, AWidth: Byte);
var
  SunOrMoonGlyphColor, SunOrMoonGlyph, SunOrMoon, SkyColor, SkyBef, SkyAft: string;
  Left: Byte;

  procedure Add(const ASunOrMoonGlyph, ASunOrMoonGlyphColor, ASkyColor: string);
  begin
    SunOrMoonGlyph := ASunOrMoonGlyph;
    SunOrMoonGlyphColor := ASunOrMoonGlyphColor;
    SkyColor := ASkyColor;
  end;

begin
  case Calendar.Hour of
    6..21:
      Add(Terminal.Icon('F8D9'), 'Light Yellow', 'Lightest Blue');
    else
      Add('(', 'Light White', 'Darker Gray');
  end;
  Left := Round(Calendar.Hour / 24 * AWidth);
  SunOrMoon := Terminal.Colorize(SunOrMoonGlyph, SunOrMoonGlyphColor);
  SkyBef := Terminal.Colorize(StringOfChar('_', Left), SkyColor);
  SkyAft := Terminal.Colorize(StringOfChar('_', AWidth - Left - 1), SkyColor);
  Terminal.Print(AX, AY, SkyBef + SunOrMoon + SkyAft, TK_ALIGN_CENTER);
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
    SL.Append(Format(_('%s: %s.'), [_('Difficulty'),
      GetPureText(Game.GetStrDifficulty)]));
    SL.Append('');
    SL.Append(AReason);
    if IsDead then
      SL.Append(Format(_('He scored %d points.'), [Statictics.Get(stScore)]))
    else
      SL.Append(Format(_('He has scored %d points so far.'), [Statictics.Get(stScore)]));
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

procedure TPlayer.Spawn;
begin
  X := Game.Spawn.X;
  Y := Game.Spawn.Y;
  Map.Current := deDarkWood;
  MsgLog.Clear;
end;

procedure TPlayer.AddExp(Value: Byte = 1);
begin
  Attributes.Modify(atExp, Value);
  if (Attributes.Attrib[atExp].Value >= LevelExpMax) then
  begin
    Attributes.Modify(atExp, -LevelExpMax);
    Attributes.Modify(atLev, 1);
    // You leveled up! You are now level %d!
    MsgLog.Add(Terminal.Colorize(Format(_('You advance to level %d!'), [Attributes.Attrib[atLev].Value]),
      clAlarm));
    if (Attributes.Attrib[atLev].Value mod 2 = 1) then
    begin
      Talents.IsPoint := True;
      MsgLog.Add(Terminal.Colorize(_('You gained 1 talent point.'), clAlarm));
      Statictics.Inc(stScore)
    end
    else
      Talents.IsPoint := False;
    Statictics.Inc(stTurn, Attributes.Attrib[atLev].Value * Attributes.Attrib[atLev].Value);
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
    Wait();
  end;
  MsgLog.Add(Format(_('Finish rest (%d turns)!'), [T - 1]));
  Abilities.Ability[abWeak] := 0;
  if (Math.RandomRange(0, 9) = 0) then
    Abilities.Ability[abDrunk] := 0;
  IsRest := False;
end;

procedure TPlayer.Start();
var
  D: Byte;
begin
  
  // Add armors
  if Game.Wizard then
  begin
    Items.AddItemToInv(iWingedHelm, 1, True, False);
    Items.AddItemToInv(iPlateMail, 1, True, False);
    Items.AddItemToInv(iPlatedGauntlets, 1, True, False);
    Items.AddItemToInv(iPlateBoots, 1, True, False);
    Items.AddItemToInv(iRing, 1, True, False);
    Items.AddItemToInv(iAmulet, 1, True, False);
  end
  else
  begin
    if (Game.Difficulty < dfHard) then
    begin
      Items.AddItemToInv(iCap, 1, True, True);
      Items.AddItemToInv(iQuiltedArmor, 1, True, True);
    end;
    if (Game.Difficulty < dfNormal) then
    begin
      Items.AddItemToInv(iLeatherGloves, 1, True, True);
      Items.AddItemToInv(iShoes, 1, True, True);
    end;
  end;
  // Add weapon
  if Game.Wizard then
  begin
    case Math.RandomRange(0, 4) of
      0:
        Items.AddItemToInv(iTrollSlayer, 1, True, False);
      1:
        Items.AddItemToInv(iDemonAxe, 1, True, False);
      2:
        Items.AddItemToInv(iHonedSpear, 1, True, False);
      3:
        Items.AddItemToInv(iDoomHammer, 1, True, False);
    end;
  end
  else
  begin
    case Math.RandomRange(0, 4) of
      0:
        Items.AddItemToInv(iRustySword, 1, True, True);
      1:
        Items.AddItemToInv(iHatchet, 1, True, True);
      2:
        Items.AddItemToInv(iShortSpear, 1, True, True);
      3:
        Items.AddItemToInv(iSlagHammer, 1, True, True);
    end;
  end;
  // Add runes, potions and scrolls
  if Game.Wizard then
  begin
    Items.AddItemToInv(iRuneOfFullHealing);
    Items.AddItemToInv(iPotionOfFullHealing, 10);
    Items.AddItemToInv(iPotionOfFullMana, 10);
    Items.AddItemToInv(iAntidote, 10);
    Items.AddItemToInv(iScrollOfTownPortal, 10);
    Items.AddItemToInv(iScrollOfIdentify, 10);
  end
  else
  begin
    Items.AddItemToInv(iLesserHealingPotion, 5);
    Items.AddItemToInv(iLesserManaPotion, 5);
    Items.AddItemToInv(iAntidote, 3);
    Items.AddItemToInv(iScrollOfTownPortal);
    Items.AddItemToInv(iScrollOfIdentify);
  end;
  // Add foods
  Items.AddItemToInv(iBreadRation, IfThen(Game.Wizard, 10, 3));
  // Add coins
  D := IfThen(Game.Difficulty <> dfHell, StartGold, 0);
  Items.AddItemToInv(iGold, IfThen(Game.Wizard, RandomRange(3333, 9999), D));
  Self.Calc();
end;

procedure TPlayer.DoEffects(const Effects: TEffects; const Value: Word = 0);
var
  V, VX, VY: Word;
const
  F = '%s +%d.';

  procedure PrmSkill(ASkill: TSkillEnum);
  begin
    Skills.Modify(ASkill, StartSkill);
    Calc();
    Fill();
  end;

  procedure PrmValue(AEffect: TEffect; Value: Byte);
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
    end;
    Calc();
    Fill();
  end;

begin
  // Life
  if (efLife in Effects) then
  begin
    V := Skills.Skill[skHealing].Value + Value;
    case RandomRange(0, 3) of
      0: MsgLog.Add(_('You feel healthy.'));
      1: MsgLog.Add(_('You feel a bit better.'));
      2: MsgLog.Add(_('You feel a wee bit better.'));
    end;
    MsgLog.Add(Format(F, [_('Life'), Min(MaxLife - Life, V)]));
    Life := EnsureRange(Self.Life + V, 0, MaxLife);
    Skills.DoSkill(skHealing);
  end;
  // Mana
  if (efMana in Effects) then
  begin
    V := Skills.Skill[skConcentration].Value + Value;
    MsgLog.Add(_('You feel magical energies restoring.'));
    MsgLog.Add(Format(F, [_('Mana'), Min(MaxMana - Mana, V)]));
    Mana := EnsureRange(Mana + V, 0, MaxMana);
    Skills.DoSkill(skConcentration, 5);
  end;
  // Food
  if (efFood in Effects) then
  begin
    Attributes.Modify(atSat, Value);
    MsgLog.Add(Format(_('You have sated %d hunger.'), [Value]));
  end;
  // Identification
  if (efIdentification in Effects) then
  begin
    Scenes.SetScene(scIdentification);
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
    Map.SetTileEnum(Game.Spawn.X, Game.Spawn.Y, deDarkWood, teTownPortal);
    Scenes.SetScene(scGame);
  end;
  // Magic Eye
  if (efMagicEye in Effects) then
  begin

  end;
  // Bloodlust
  if (efBloodlust in Effects) then
  begin
    V := Math.RandomRange(Value, Skills.Skill[skConcentration]
      .Value + Value);
    Abilities.Modify(abBloodlust, V);
    MsgLog.Add(Format(_('You feel lust for blood (%d).'), [V]));
  end;
  // Cure poison
  if (efCurePoison in Effects) then
  begin
    if Abilities.IsAbility(abPoisoned) then
    begin
      V := Self.Skills.Skill[skHealing].Value + Value;
      Abilities.Ability[abPoisoned] :=
        Math.EnsureRange(Abilities.Ability[abPoisoned] - V, 0, High(Word));
      Self.Skills.DoSkill(skHealing);
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
    Items.AddItemToInv(iGold, StartGold);
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
end;

initialization

Player := TPlayer.Create;

finalization

FreeAndNil(Player);

end.
