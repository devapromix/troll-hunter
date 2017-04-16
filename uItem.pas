unit uItem;

interface

uses BearLibItems, uCommon, uMap, uPlayer;

type
  TItemType = (itCoin, itPotion, itBlade, itAxe, itSpear, itMace, itHelm,
    itArmour);

type
  TSlotType = (stNone, stHead, stChest, stFeet, stMainHand, stOffHand,
    stNeck, stFinger);

type
  TItemBase = record
    Symbol: Char;
    ItemType: TItemType;
    SlotType: TSlotType;
    MaxStack: Word;
    MaxDurability: Word;
    Level: Byte;
    Defense: Byte;
    Damage: TDamage;
    Color: Cardinal;
    Deep: TDeepEnum;
  end;

type
  TItemEnum = (
    // All maps
    iGold, iPotionOfHealth, iPotionOfMana,
    // Dark Wood
    iRustySword, iShortSword, // Blade
    iHatchet, iBattleAxe, // Axe
    iShortSpear, iSpear, // Spear
    iSlagHammer, iSpikedCudgel, // Mace
    // Gray Cave
    iBroadSword, iLongSword, // Blade
    iMeatAxe, iFleshTearer, // Axe
    iJavelin, iFuscina, // Spear
    iWarhammer, iWarMace, // Mace
    // Deep Cave
    iMoonBlade, iScimitar, // Blade
    iRubyAxe, iDarkAxe, // Axe
    iWarSpear, iHarpoon, // Spear
    iFlangedMace, iWarGavel, // Mace
    // Blood Cave
    iBastardSword, iGreatSword, // Blade
    iBerserkerAxe, iMarauderAxe, // Axe
    iSilvanWhisper, iImpaler, // Spear
    iBarbarousMace, iAdeptHammer, // Mace
    // Dungeon of Doom
    iRuneSword, iTrollSlayer, // Blade
    iChopper, iDemonAxe, // Axe
    iSoulReaver, iHonedSpear, // Spear
    iWarMaul, iDoomHammer // Mace
    );

const
  NotEquipItems = [iGold, iPotionOfHealth, iPotionOfMana];
  DrinkItems = [iPotionOfHealth, iPotionOfMana];

const
  ItemBase: array [TItemEnum] of TItemBase = (
    // == All maps == //

    // Gold
    (Symbol: '$'; ItemType: itCoin; SlotType: stNone; MaxStack: 1000; MaxDurability: 0;
    Level: 0; Defense: 0; Damage: (Min: 0; Max: 0;); Color: clYellow; Deep: deDarkWood;),
    // Life Potion
    (Symbol: '!'; ItemType: itPotion; SlotType: stNone; MaxStack: 10; MaxDurability: 0;
    Level: 0; Defense: 0; Damage: (Min: 0; Max: 0;); Color: clRed; Deep: deDarkWood;),
    // Mana potion
    (Symbol: '!'; ItemType: itPotion; SlotType: stNone; MaxStack: 10; MaxDurability: 0;
    Level: 0; Defense: 0; Damage: (Min: 0; Max: 0;); Color: clBlue; Deep: deDarkWood;),

    // == Dark Wood == //

    // Rusty Sword
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1; MaxDurability: 30;
    Level: 1; Defense: 1; Damage: (Min: 2; Max: 3;); Color: clDarkRed; Deep: deDarkWood;),
    // Short Sword
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1; MaxDurability: 35;
    Level: 2; Defense: 2; Damage: (Min: 3; Max: 5;); Color: clWhite; Deep: deDarkWood;),
    // Hatchet
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1; MaxDurability: 30;
    Level: 1; Defense: 1; Damage: (Min: 1; Max: 3;); Color: clDarkRed; Deep: deDarkWood;),
    // Battle Axe
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1; MaxDurability: 35;
    Level: 2; Defense: 2; Damage: (Min: 2; Max: 5;); Color: clDarkRed; Deep: deDarkWood;),
    // Short Spear
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1; MaxDurability: 30;
    Level: 1; Defense: 1; Damage: (Min: 1; Max: 3;); Color: clDarkRed; Deep: deDarkWood;),
    // Spear
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1; MaxDurability: 35;
    Level: 2; Defense: 2; Damage: (Min: 1; Max: 5;); Color: clDarkRed; Deep: deDarkWood;),
    // Slag Hammer
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1; MaxDurability: 30;
    Level: 1; Defense: 1; Damage: (Min: 3; Max: 4;); Color: clDarkRed; Deep: deDarkWood;),
    // Spiked Cudgel
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1; MaxDurability: 35;
    Level: 2; Defense: 2; Damage: (Min: 4; Max: 5;); Color: clDarkRed; Deep: deDarkWood;),

    // == Gray Cave == //

    // Broad Sword
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1; MaxDurability: 40;
    Level: 3; Defense: 3; Damage: (Min: 7; Max: 10;); Color: clDarkRed; Deep: deGrayCave;),
    // Long Sword
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1; MaxDurability: 45;
    Level: 4; Defense: 4; Damage: (Min: 10; Max: 14;); Color: clDarkRed; Deep: deGrayCave;),
    // Meat Axe
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1; MaxDurability: 40;
    Level: 3; Defense: 3; Damage: (Min: 3; Max: 7;); Color: clDarkRed; Deep: deGrayCave;),
    // Flesh Tearer
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1; MaxDurability: 45;
    Level: 4; Defense: 4; Damage: (Min: 6; Max: 10;); Color: clDarkRed; Deep: deGrayCave;),
    // Javelin
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1; MaxDurability: 40;
    Level: 3; Defense: 3; Damage: (Min: 1; Max: 10;); Color: clDarkRed; Deep: deGrayCave;),
    // Fuscina
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1; MaxDurability: 45;
    Level: 4; Defense: 4; Damage: (Min: 1; Max: 15;); Color: clDarkRed; Deep: deGrayCave;),
    // Warhammer
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1; MaxDurability: 40;
    Level: 3; Defense: 3; Damage: (Min: 0; Max: 0;); Color: clDarkRed; Deep: deGrayCave;),
    // War Mace
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1; MaxDurability: 45;
    Level: 4; Defense: 4; Damage: (Min: 0; Max: 0;); Color: clDarkRed; Deep: deGrayCave;),

    // == Deep Cave == //

    // Moon Blade
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1; MaxDurability: 50;
    Level: 5; Defense: 5; Damage: (Min: 0; Max: 0;); Color: clDarkRed; Deep: deDeepCave;),
    // Scimitar
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1; MaxDurability: 55;
    Level: 6; Defense: 6; Damage: (Min: 0; Max: 0;); Color: clDarkRed; Deep: deDeepCave;),
    // Ruby Axe
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1; MaxDurability: 50;
    Level: 5; Defense: 5; Damage: (Min: 0; Max: 0;); Color: clDarkRed; Deep: deDeepCave;),
    // Dark Axe
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1; MaxDurability: 55;
    Level: 6; Defense: 6; Damage: (Min: 0; Max: 0;); Color: clDarkRed; Deep: deDeepCave;),
    // War Spear
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1; MaxDurability: 50;
    Level: 5; Defense: 5; Damage: (Min: 1; Max: 25;); Color: clDarkRed; Deep: deDeepCave;),
    // Harpoon
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1; MaxDurability: 55;
    Level: 6; Defense: 6; Damage: (Min: 1; Max: 35;); Color: clDarkRed; Deep: deDeepCave;),
    // Flanged Mace
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1; MaxDurability: 50;
    Level: 5; Defense: 5; Damage: (Min: 0; Max: 0;); Color: clDarkRed; Deep: deDeepCave;),
    // War Gavel
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1; MaxDurability: 55;
    Level: 6; Defense: 6; Damage: (Min: 0; Max: 0;); Color: clDarkRed; Deep: deDeepCave;),

    // == Blood Cave == //

    // Bastard Sword
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1; MaxDurability: 60;
    Level: 7; Defense: 7; Damage: (Min: 0; Max: 0;); Color: clDarkRed; Deep: deBloodCave;),
    // Great Sword
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1; MaxDurability: 65;
    Level: 8; Defense: 8; Damage: (Min: 0; Max: 0;); Color: clDarkRed; Deep: deBloodCave;),
    // Berserker Axe
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1; MaxDurability: 60;
    Level: 7; Defense: 7; Damage: (Min: 0; Max: 0;); Color: clDarkRed; Deep: deDeepCave;),
    // Marauder Axe
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1; MaxDurability: 65;
    Level: 8; Defense: 8; Damage: (Min: 0; Max: 0;); Color: clDarkRed; Deep: deBloodCave;),
    // Silvan Whisper
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1; MaxDurability: 60;
    Level: 7; Defense: 7; Damage: (Min: 1; Max: 50;); Color: clDarkRed; Deep: deBloodCave;),
    // Impaler
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1; MaxDurability: 65;
    Level: 8; Defense: 8; Damage: (Min: 1; Max: 65;); Color: clDarkRed; Deep: deBloodCave;),
    // Barbarous Mace
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1; MaxDurability: 60;
    Level: 7; Defense: 7; Damage: (Min: 0; Max: 0;); Color: clDarkRed; Deep: deBloodCave;),
    // Adept Hammer
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1; MaxDurability: 65;
    Level: 8; Defense: 8; Damage: (Min: 0; Max: 0;); Color: clDarkRed; Deep: deBloodCave;),

    // == Dungeon of Doom == //

    // Rune Sword
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1; MaxDurability: 70;
    Level: 9; Defense: 9; Damage: (Min: 0; Max: 0;); Color: clDarkRed; Deep: deDungeonOfDoom;),
    // Troll Slayer,
    (Symbol: '/'; ItemType: itBlade; SlotType: stMainHand; MaxStack: 1; MaxDurability: 75;
    Level: 10; Defense: 10; Damage: (Min: 0; Max: 0;); Color: clDarkRed; Deep: deDungeonOfDoom;),
    // Chopper
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1; MaxDurability: 70;
    Level: 9; Defense: 9; Damage: (Min: 0; Max: 0;); Color: clDarkRed; Deep: deDungeonOfDoom;),
    // Demon Axe,
    (Symbol: '('; ItemType: itAxe; SlotType: stMainHand; MaxStack: 1; MaxDurability: 75;
    Level: 10; Defense: 10; Damage: (Min: 0; Max: 0;); Color: clDarkRed; Deep: deDungeonOfDoom;),
    // Soul Reaver
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1; MaxDurability: 70;
    Level: 9; Defense: 9; Damage: (Min: 1; Max: 80;); Color: clDarkRed; Deep: deDungeonOfDoom;),
    // Honed Spear,
    (Symbol: '|'; ItemType: itSpear; SlotType: stMainHand; MaxStack: 1; MaxDurability: 75;
    Level: 10; Defense: 10; Damage: (Min: 1; Max: 100;); Color: clDarkRed; Deep: deDungeonOfDoom;),
    // War Maul
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1; MaxDurability: 70;
    Level: 9; Defense: 9; Damage: (Min: 0; Max: 0;); Color: clDarkRed; Deep: deDungeonOfDoom;),
    // Doom Hammer
    (Symbol: ')'; ItemType: itMace; SlotType: stMainHand; MaxStack: 1; MaxDurability: 75;
    Level: 10; Defense: 10; Damage: (Min: 0; Max: 0;); Color: clDarkRed; Deep: deDungeonOfDoom;)

    );

type
  TItems = class(TObject)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Render(AX, AY: Byte);
    procedure Add(ADeep: TDeepEnum);
    function GetName(AItem: TItemEnum): string;
    function GetItemEnum(AItemID: Integer): TItemEnum;
    function GetItemInfo(AItem: Item): string;
    function GetMapItemInfo(AItem: Item; IsManyItems: Boolean;
      ACount: Byte): string;
    function RenderInvItem(X, Y, I: Integer; AItem: Item; IsAdvInfo: Boolean = False; IsRender: Boolean = True): string;
    procedure AddItemToInv(Index: Integer); overload;
    procedure AddItemToInv(AItemEnum: TItemEnum; AAmount: Word = 1; EqFlag: Boolean = False); overload;
    function GetInventory: string;
  end;

var
  Items: TItems = nil;

implementation

uses Math, Classes, SysUtils, uTerminal, gnugettext, uMsgLog, uScenes, uGame;

{ TItems }

function TItems.GetItemInfo(AItem: Item): string;
var
  ID: Integer;
  S: string;
begin
  S := '';
  ID := AItem.ItemID;
  if (AItem.Stack > 1) then
    // Amount
    S := '(' + IntToStr(AItem.Amount) + 'x)'
    // Durability
  else
    S := '(' + IntToStr(AItem.Durability) + '/' +
      IntToStr(ItemBase[TItemEnum(ID)].MaxDurability) + ')';
  Result := Trim(Format('%s %s', [Items.GetName(TItemEnum(ID)), S]));
end;

function TItems.GetMapItemInfo(AItem: Item; IsManyItems: Boolean;
  ACount: Byte): string;
var
  S: string;
  N: Integer;
begin
  S := '';
  N := AItem.ItemID;
  if (AItem.Stack > 1) then
    // Amount
    S := '(' + IntToStr(AItem.Amount) + 'x)'
    // Durability
  else
    S := '(' + IntToStr(AItem.Durability) + '/' +
      IntToStr(ItemBase[TItemEnum(N)].MaxDurability) + ')';
  S := GetCapit(GetDescAn(Trim(Items.GetName(TItemEnum(AItem.ItemID)) +
    ' ' + S)));
  if IsManyItems then
  begin
    Result := Format(_('Several items (%dx) are lying here (%s).'),
      [ACount, S]);
  end
  else
    Result := Format(_('%s is lying here.'), [S]);
end;

procedure Make(ID: Byte; var AItem: Item);
begin
  Items_Clear_Item(AItem);
  AItem.ItemID := ID;
  AItem.SlotID := Ord(ItemBase[TItemEnum(ID)].SlotType);
  AItem.Stack := ItemBase[TItemEnum(ID)].MaxStack;
  AItem.Durability := ItemBase[TItemEnum(ID)].MaxDurability;
end;

procedure TItems.Add(ADeep: TDeepEnum);
var
  ID, FX, FY: Byte;
  FItem: Item;
  Value: Integer;
begin
  repeat
    ID := Math.RandomRange(0, Ord(High(TItemEnum)) + 1);
    FX := Math.RandomRange(0, High(Byte));
    FY := Math.RandomRange(0, High(Byte));
  until (Map.GetTileEnum(FX, FY, ADeep) in SpawnTiles) and
    ((ItemBase[TItemEnum(ID)].MaxStack > 1) or
    (ItemBase[TItemEnum(ID)].Deep = ADeep));
  Make(ID, FItem);
  FItem.MapID := Ord(ADeep);
  FItem.Amount := 1;
  FItem.X := FX;
  FItem.Y := FY;
  case ItemBase[TItemEnum(ID)].ItemType of
    itCoin:
      FItem.Amount := (Math.RandomRange(0, 25) + 1) * (Ord(ADeep) + 1);
    itPotion:
      FItem.Amount := (Math.RandomRange(0, 3) + 1);
  end;
  if (FItem.Stack = 1) then
  begin
    Value := ItemBase[TItemEnum(ID)].MaxDurability;
    FItem.Durability := Math.RandomRange(Value div 4, Value) + 1;
  end;
  FItem.Equipment := 0;
  Items_Dungeon_AppendItem(FItem);
end;

procedure TItems.AddItemToInv(AItemEnum: TItemEnum; AAmount: Word = 1; EqFlag: Boolean = False);
var
  FItem: Item;
begin
  Make(Ord(AItemEnum), FItem);
  FItem.Amount := AAmount;
  FItem.Equipment := IfThen(EqFlag, 1, 0);
  Items_Inventory_AppendItem(FItem);
end;

procedure TItems.Render(AX, AY: Byte);
var
  MapID, X, Y: Byte;
  I, Count: Integer;
  Color: Cardinal;
  FItem: Item;
begin
  MapID := Ord(Map.Deep);
  Count := Items_Dungeon_GetMapCount(MapID);
  for I := Count - 1 downto 0 do
  begin
    FItem := Items_Dungeon_GetMapItem(MapID, I);
    if not Map.InView(FItem.X, FItem.Y) or
      (not Game.Wizard and not Map.GetFOV(FItem.X, FItem.Y)) then
      Continue;
    X := FItem.X - Player.X + AX + View.Left;
    Y := FItem.Y - Player.Y + AY + View.Top;
    if not Game.Wizard and (GetDist(Player.X, Player.Y, FItem.X, FItem.Y) >
      Player.GetRadius) then
      Color := clFog
    else
      Color := ItemBase[TItemEnum(FItem.ItemID)].Color;
    Terminal.Print(X, Y, ItemBase[TItemEnum(FItem.ItemID)].Symbol, Color);
  end;
end;

constructor TItems.Create;
begin
  Items_Open;
end;

destructor TItems.Destroy;
begin
  Items_Close;
  inherited;
end;

// Broadsword, Hilted Sword, Longsword, Bastard Sword
// Combat Sword, War Sword, Claymore, Ebony Sword
// Rusty Iron Wood-Chopping Axe, Battle Axe,

// Phantom Axe, Dwarven Battle Axe, War Axe
// Feathered Spear, Bronze Spear, Rusted Spear
// Small Dagger, a Rusty Dagger, Flying Dagger
// Sharpened Daggers, Gemmed Dagger, Carving Knife
// Boot Knife, Target Knife, Throwing Spike
// Rapier, Sabre,

// Rusty, Chipped
// Low Quality, Medium Quality, High Quality
// Fine, Double Bladed, Enchanted
// Bronze, Iron, Steel

// Short Bow, Battle Bow, Long Bow, Elven Long Bow
// Hunter's Bow, Ranger's Bow, Elvish Longbow, Compound Bow

// Tunic, Chainmail, Platemail
// Padded clothing, Chitin armor, Bone armor, Light armor,
// Medium armor, Mail hauberk, Brigandine,
// Heavy armor, Plate armor,

function TItems.GetName(AItem: TItemEnum): string;
begin
  case AItem of
    // == All maps == //
    // Gold
    iGold:
      Result := _('Gold');
    // Potion of health
    iPotionOfHealth:
      Result := _('Potion of health');
    // Potion of mana
    iPotionOfMana:
      Result := _('Potion of mana');

    // == Dark Wood == //

    // Blade
    iRustySword:
      Result := _('Rusty Sword');
    iShortSword:
      Result := _('Short Sword');
    // Axe
    iHatchet:
      Result := _('Hatchet');
    iBattleAxe:
      Result := _('Battle Axe');
    // Spear
    iShortSpear:
      Result := _('Short Spear');
    iSpear:
      Result := _('Spear');
    // Mace
    iSlagHammer:
      Result := _('Slag Hammer');
    iSpikedCudgel:
      Result := _('Spiked Cudgel');

    // == Gray Cave == //

    // Blade
    iBroadSword:
      Result := _('Broad Sword');
    iLongSword:
      Result := _('Long Sword');
    // Axe
    iMeatAxe:
      Result := _('Meat Axe');
    iFleshTearer:
      Result := _('Flesh Tearer');
    // Spear
    iJavelin:
      Result := _('Javelin');
    iFuscina:
      Result := _('Fuscina');
    // Mace
    iWarhammer:
      Result := _('Warhammer');
    iWarMace:
      Result := _('War Mace');

    // == Deep Cave == //

    // Blade
    iMoonBlade:
      Result := _('Moon Blade');
    iScimitar:
      Result := _('Scimitar');
    // Axe
    iRubyAxe:
      Result := _('Ruby Axe');
    iDarkAxe:
      Result := _('Dark Axe');
    // Spear
    iWarSpear:
      Result := _('War Spear');
    iHarpoon:
      Result := _('Harpoon');
    // Mace
    iFlangedMace:
      Result := _('Flanged Mace');
    iWarGavel:
      Result := _('War Gavel');

    // == Blood Cave == //

    // Blade
    iBastardSword:
      Result := _('Bastard Sword');
    iGreatSword:
      Result := _('Great Sword');
    // Axe
    iBerserkerAxe:
      Result := _('Berserker Axe');
    iMarauderAxe:
      Result := _('Marauder Axe');
    // Spear
    iSilvanWhisper:
      Result := _('Silvan Whisper');
    iImpaler:
      Result := _('Impaler');
    // Mace
    iBarbarousMace:
      Result := _('Barbarous Mace');
    iAdeptHammer:
      Result := _('Adept Hammer');

    // == Dungeon of Doom == //

    // Blade
    iRuneSword:
      Result := _('Rune Sword');
    iTrollSlayer:
      Result := _('Troll Slayer');
    // Axe
    iChopper:
      Result := _('Chopper');
    iDemonAxe:
      Result := _('Demon Axe');
    // Spear
    iSoulReaver:
      Result := _('Soul Reaver');
    iHonedSpear:
      Result := _('Honed Spear');
    // Mace
    iWarMaul:
      Result := _('War Maul');
    iDoomHammer:
      Result := _('Doom Hammer');
  end;
end;

function TItems.RenderInvItem(X, Y, I: Integer; AItem: Item; IsAdvInfo: Boolean = False; IsRender: Boolean = True): string;
var
  S: string;
  D: TItemBase;
begin
  Result := '';
  D := ItemBase[TItemEnum(AItem.ItemID)];
  Terminal.Print(X - 4, Y + I, TScene.KeyStr(Chr(I + Ord('A'))));
  if IsRender then
  begin
    Terminal.ForegroundColor(D.Color);
    Terminal.Print(X, Y + I, D.Symbol);
  end else Result := Result + D.Symbol + ' ';
  Terminal.ForegroundColor(clGray);
  if IsAdvInfo then
  begin
    S := '';
    if (AItem.Equipment > 0) then
    begin
      case D.SlotType of
        stHead:;
        stNeck:;
        stFinger:;
        stMainHand: S := _('in main hand');
        stOffHand:;
        stChest:;
        stFeet:;
      end;
      S := '- ' + S;
    end;
  end;
  S := Items.GetItemInfo(AItem) + ' ' + S;
  if IsRender then
    Terminal.Print(X + 2, Y + I, S)
      else Result := Result + S;
end;

procedure TItems.AddItemToInv(Index: Integer);
var
  FItem: Item;
  MapID: Integer;
  The: string;
begin
  MapID := Ord(Map.Deep);
  FItem := Items_Dungeon_GetMapItemXY(MapID, Index, Player.X, Player.Y);
  if (Items_Dungeon_DeleteItemXY(MapID, Index, Player.X, Player.Y, FItem) > 0) then
  begin
    Items_Inventory_AppendItem(FItem);
    The := GetDescThe(Items.GetName(TItemEnum(FItem.ItemID)));
    MsgLog.Add(Format(_('You pick up %s.'), [The]));
    Player.Calc;
  end;
end;

function TItems.GetInventory: string;
var
  SL: TStringList;
  I, FCount: Integer;
  FItem: Item;
begin
  Result := '';
  SL := TStringList.Create;
  try
    FCount := Clamp(Items_Inventory_GetCount(), 0, 26);
    for I := 0 to FCount - 1 do
    begin
      FItem := Items_Inventory_GetItem(I);
      SL.Append(Items.RenderInvItem(5, 2, I, FItem, False, False));
    end;
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TItems.GetItemEnum(AItemID: Integer): TItemEnum;
begin
  Result := TItemEnum(AItemID);
end;

initialization

Items := TItems.Create;

finalization

FreeAndNil(Items);

end.
