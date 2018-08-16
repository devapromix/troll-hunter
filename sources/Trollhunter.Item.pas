unit Trollhunter.Item;

interface

{ TODO -cПредметы : Свет от лампы днем должен быть на 1 пункт больше макс. для героя, а не макс. как сейчас. }

uses
  uBearLibItemsCommon,
  Trollhunter.Types,
  Trollhunter.Item.Types,
  Trollhunter.Player.Types,
  Trollhunter.Game,
  Trollhunter.Map,
  Trollhunter.Player,
  Trollhunter.Entity,
  Trollhunter.Creature;

type
  TItems = class(TEntity)
  private
    FItemName: array [TItemEnum] of string;
    function GetName(I: TItemEnum): string; overload;
  public
    Index: UInt;
    class procedure Make(ID: UInt; var AItem: Item);
    class procedure CalcItem(var AItem: Item);
    constructor Create;
    destructor Destroy; override;
    procedure Render(AX, AY: UInt);
    procedure Add(AZ: TMapEnum; AX: Int = -1; AY: Int = -1; AID: Int = -1; IsRare: Boolean = False);
    function GetItemEnum(AItemID: Int): TItemEnum;
    function GetItemInfo(AItem: Item; IsManyItems: Boolean = False; ACount: UInt = 0; IsShort: Boolean = False): string;
    function RenderInvItem(const AX, AY, I: Int; AItem: Item; IsAdvInfo: Boolean = False; IsRender: Boolean = True;
      PriceType: TPriceType = ptNone): string;
    procedure AddItemToInv(Index: Int = 0; AFlag: Boolean = False); overload;
    procedure AddItemToInv(AItemEnum: TItemEnum; AAmount: UInt = 1; EqFlag: Boolean = False; IdFlag: Boolean = False; SufID: UInt = 0); overload;
    function GetInventory: string;
    function InvCount: Integer;
    function GetInfo(Sign: string; Value: UInt; Color: string; RareColor: string = ''): string;
    procedure RenderInventory(PriceType: TPriceType = ptNone);
    procedure LootGold(const AX, AY: UInt);
    procedure Loot(const AX, AY: UInt; AItemEnum: TItemEnum); overload;
    procedure Loot(const AX, AY: UInt; AIsBoss: Boolean); overload;
    property Name[I: TItemEnum]: string read GetName;
    function ChItem(AItem: Item): Boolean;
    function Identify(var AItem: Item; IsNew: Boolean = False; IsRare: Boolean = False; Index: UInt = 0): Boolean;
    function GetName(AItem: Item; IsShort: Boolean = False): string; overload;
    function GetNameThe(AItem: Item): string;
    procedure AddItemToDungeon(AItem: Item);
    function AddItemInfo(V: TArray<string>): string;
    function StrToItemEnum(const S: string): TItemEnum;
    procedure SetBonus(var AItem: Item; const BonusType: TBonusType; const Value: UInt8);
    function GetBonus(const AItem: Item; const BonusType: TBonusType): UInt8;
    procedure DelCorpses();
    procedure AddPlants;
  end;

var
  Items: TItems = nil;

implementation

uses
  Math,
  Classes,
  TypInfo,
  StrUtils,
  SysUtils,
  uBearLibItemsDungeon,
  uBearLibItemsInventory,
  Trollhunter.Terminal,
  Trollhunter.Language,
  Trollhunter.UI.Log,
  Trollhunter.Item.Shop,
  Trollhunter.Item.Affixes,
  Trollhunter.Attribute,
  Trollhunter.UI,
  Trollhunter.Helpers,
  Trollhunter.Statistic,
  Trollhunter.Item.Base,
  Trollhunter.Item.Helpers;

{ TItems }

class procedure TItems.CalcItem(var AItem: Item);
var
  SB: TSuffixBase;
begin
  // Suffix
  SB := Affixes.GetSuffix(TSuffixEnum(AItem.Identify));
  // Damage
  if (AItem.MinDamage > 0) and (AItem.MinDamage >= AItem.MaxDamage) then
    AItem.MinDamage := AItem.MaxDamage - 1;
  // Flask
  if (ItemBase.GetItem(AItem).ItemType in FlaskTypeItems) then
  begin
    AItem.Price := ItemBase.GetItem(AItem).Price + SB.Price;
    if not SB.Rare then
      AItem.Price := AItem.Price + AItem.Value;
    Exit;
  end;
  { // Oil
    if ItemBase[TItemEnum(AItem.ItemID)].ItemType in OilTypeItems then
    begin
    AItem.Price := ItemBase[TItemEnum(AItem.ItemID)].Price + APrice +
    Round(AItem.Durability * 4.7);
    case AItem.Effect of
    tfCursed:
    AItem.Price := AItem.Price div 2;
    tfBlessed:
    AItem.Price := AItem.Price * 2;
    end;
    Exit;
    end; }
  // Price
  if (ItemBase.GetItem(AItem).ItemType in IdentTypeItems) then
  begin
    AItem.Price := ItemBase.GetItem(AItem).Price + SB.Price + Round(AItem.MaxDurability * 3.7) + Round(AItem.Defense * 4.8) +
      Round(AItem.MaxDamage * 5.6);
  end
  else
    AItem.Price := ItemBase.GetItem(AItem).Price;
end;

function TItems.ChItem(AItem: Item): Boolean;
begin
  Result := (ItemBase.GetItem(AItem).ItemType in CorpseTypeItems) or (AItem.Stack > 1) or (AItem.Amount > 1) or (AItem.Identify = 0);
end;

function TItems.GetItemInfo(AItem: Item; IsManyItems: Boolean = False; ACount: UInt = 0; IsShort: Boolean = False): string;
var
  ID: Int;
  S, T, Level, D: string;
  ItemType: TItemType;
  V: UInt;

  function GetAmount(): string;
  begin
    Result := Format('(%dx)', [AItem.Amount])
  end;

  procedure AddEffect(const AEffect: TEffect; const Sign, Color: string; const RareColor: string = '');
  var
    Ef: TEffect;

    procedure Add(Color: string);
    var
      V: UInt;
    begin
      V := AItem.Value;
      if ((V > 0) or (Sign = '&')) then
        S := S + Items.GetInfo(Sign, V, Color, RareColor) + ' ';
    end;

  begin
    if (AEffect in AItem.Effects) then
    begin
      if (AEffect = efCraftAtr) then
      begin
        for Ef := CraftEffLow to Pred(CraftEffHigh) do
          Add(EfNameStr[Ef]);
        Exit;
      end;
      Add(Color);
    end;
  end;

begin
  S := '';
  T := '';
  Level := '';
  Result := '';
  ID := AItem.ItemID;
  ItemType := ItemBase.GetItem(ID).ItemType;
  // Level
  if (AItem.Level > Player.Attributes.Attrib[atLev].Value) or not Game.GetOption(apHdLevOfItem) then
    Level := GetItemLevel(AItem.Level);
  // Info
  if not IsManyItems then
  begin
    if (ItemType in RuneTypeItems + ScrollTypeItems) then
    begin
      V := ItemBase.GetItem(ID).ManaCost;
      if (V > 0) then
        S := S + Items.GetInfo('-', V, 'Mana') + ' ';
    end;

    AddEffect(efMana, '+', 'Mana');
    AddEffect(efLife, '+', 'Life');
    AddEffect(efFood, '+', 'Food');
    AddEffect(efCurePoison, '+', 'Poison');
    AddEffect(efBloodlust, '+', 'Poison', 'Blood');
    AddEffect(efIdentification, '&', 'Ident', 'Dark Yellow');
    AddEffect(efAllIdentification, '&', 'Ident', 'Dark Yellow');
    AddEffect(efTownPortal, '&', 'Portal', 'Dark Green');
    AddEffect(efTeleportation, '&', 'Portal', 'Dark Green');
    AddEffect(efRepair, '+', 'Repair', 'Food');
    AddEffect(efCraftStr, '&', 'Strength', 'Strength');
    AddEffect(efCraftDex, '&', 'Dexterity', 'Dexterity');
    AddEffect(efCraftWil, '&', 'Willpower', 'Willpower');
    AddEffect(efCraftPer, '&', 'Perception', 'Perception');
    AddEffect(efCraftAtr, '&', '', '');
    AddEffect(efPrmMana, '*', 'Mana');
    AddEffect(efPrmLife, '*', 'Life');
  end;
  // Amount
  if (AItem.Stack > 1) then
  begin
    if not(ItemType in NotInfoTypeItems) then
      S := AddItemInfo([Level, S]);
    if (AItem.Amount > 1) then
      S := S + GetAmount();
  end
  // Corpse
  else if (TItemEnum(ID) = ivCorpse) then
    S := ''
    // Light (Torch)
  else if (ItemType = itTorch) then
    S := S + Format('(%s%d/%d)', [UI.Icon(icFlag), AItem.Value, ItemBase.GetItem(ID).Value])
  else
  begin
    if (AItem.SlotID = 0) then
    begin
      Level := Level + ' ' + S;
      S := '';
    end;
    // Defense
    if (ItemType in ArmorTypeItems + JewelryTypeItems) then
    begin
      if (AItem.Defense > 0) then
        T := Format('%s%d', [UI.Icon(icShield), AItem.Defense]);
      if (AItem.Identify > 0) and (TSuffixEnum(AItem.Identify) in DefenseSuffixes) then
        T := Terminal.Colorize(T, 'Rare');
    end;
    // Damage
    if (ItemType in WeaponTypeItems + JewelryTypeItems) then
    begin
      if (AItem.MinDamage > 0) then
        T := Format('%s%d-%d', [UI.Icon(icSword), AItem.MinDamage, AItem.MaxDamage]);
      if (AItem.Identify > 0) and (TSuffixEnum(AItem.Identify) in DamageSuffixes) then
        T := Terminal.Colorize(T, 'Rare');
    end;

    if (AItem.Bonus[0] > 0) then
    begin
      if (Items.GetBonus(AItem, btVis) > 0) then
        Level := Level + ' ' + Items.GetInfo('x', Items.GetBonus(AItem, btVis), 'Vision', 'Rare');
      if (Items.GetBonus(AItem, btLife) > 0) then
        Level := Level + ' ' + Items.GetInfo('*', Items.GetBonus(AItem, btLife), 'Life', 'Rare');
      if (Items.GetBonus(AItem, btMana) > 0) then
        Level := Level + ' ' + Items.GetInfo('*', Items.GetBonus(AItem, btMana), 'Mana', 'Rare');
      if (Items.GetBonus(AItem, btExtraGold) > 0) then
        Level := Level + ' ' + Items.GetInfo('x', Items.GetBonus(AItem, btExtraGold), 'Gold', 'Rare');
    end;
    if (AItem.Bonus[1] > 0) then
    begin
      if (Items.GetBonus(AItem, btStr) > 0) then
        Level := Level + ' ' + Items.GetInfo('*', Items.GetBonus(AItem, btStr), 'Strength', 'Rare');
      if (Items.GetBonus(AItem, btDex) > 0) then
        Level := Level + ' ' + Items.GetInfo('*', Items.GetBonus(AItem, btDex), 'Dexterity', 'Rare');
      if (Items.GetBonus(AItem, btWil) > 0) then
        Level := Level + ' ' + Items.GetInfo('*', Items.GetBonus(AItem, btWil), 'Willpower', 'Rare');
      if (Items.GetBonus(AItem, btPer) > 0) then
        Level := Level + ' ' + Items.GetInfo('*', Items.GetBonus(AItem, btPer), 'Perception', 'Rare');
    end;
    if (AItem.Bonus[2] > 0) then
    begin
      if (Items.GetBonus(AItem, btReLife) > 0) then
        Level := Level + ' ' + Items.GetInfo('@', Items.GetBonus(AItem, btReLife), 'Life', 'Rare');
      if (Items.GetBonus(AItem, btReMana) > 0) then
        Level := Level + ' ' + Items.GetInfo('@', Items.GetBonus(AItem, btReMana), 'Mana', 'Rare');
      if (Items.GetBonus(AItem, btLifeAfEachKill) > 0) then
        Level := Level + ' ' + Items.GetInfo('x', Items.GetBonus(AItem, btLifeAfEachKill), 'Life', 'Rare');
      if (Items.GetBonus(AItem, btManaAfEachKill) > 0) then
        Level := Level + ' ' + Items.GetInfo('x', Items.GetBonus(AItem, btManaAfEachKill), 'Mana', 'Rare');
    end;
    // Durability
    D := '';
    if ItemType in SmithTypeItems then
    begin
      D := Format('%s%d/%d', [UI.Icon(icHammer), AItem.Durability, AItem.MaxDurability]);
      if (AItem.Identify > 0) and (TSuffixEnum(AItem.Identify) in DurabilitySuffixes) then
        D := Terminal.Colorize(D, 'Rare');
    end;
    S := S + AddItemInfo([Level, T, D]);

    if ((AItem.Identify = 0) or Player.Look) then
      S := '';
  end;
  Result := Trim(Format('%s %s', [Items.GetName(AItem), S]));
  // Map's item
  if (IsManyItems or (ACount > 0)) then
  begin
    if Player.Look or IsShort then
      S := '';
    S := IfThen(AItem.Amount > 1, GetAmount(), '');
    S := GetCapit(GetDescAn(Trim(Items.GetName(AItem, IsShort) + ' ' + S)));
    if IsManyItems then
    begin
      Result := Format(_('Several items (%dx) are lying here (%s).'), [ACount, S]);
    end
    else
      Result := Format(_('%s is lying here.'), [S]);
  end;
  if Mode.Wizard and Game.ShowID then
    Result := Result + Format(' ID: %d', [ID]);
end;

class procedure TItems.Make(ID: UInt; var AItem: Item);
var
  I: UInt;

  function IsIdentify(): Boolean;
  begin
    Result := False;
    if (ItemBase.GetItem(ID).ItemType in IdentTypeItems) then
      Result := (Math.RandomRange(0, 4) = 0) or (ItemBase.GetItem(ID).ItemType in AllwaysIdentTypeItems)
  end;

begin
  Items_Clear_Item(AItem);
  AItem.ItemID := ID;
  AItem.SlotID := Ord(ItemBase.GetItem(ID).SlotType);
  AItem.Stack := ItemBase.GetItem(ID).MaxStack;
  AItem.Level := ItemBase.GetItem(ID).Level;
  // Color
  AItem.Color := ItemBase.GetItem(ID).Color;
  // AItem.Color := Math.RandomRange($FF888888, $FFFFFFFF);
  // Effects
  AItem.Effects := ItemBase.GetItem(ID).Effects;
  // Value
  AItem.Value := ItemBase.GetItem(ID).Value;
  // Defense
  if (AItem.Stack = 1) and (ItemBase.GetItem(ID).Defense.Min > 0) then
    AItem.Defense := Math.EnsureRange(Math.RandomRange(ItemBase.GetItem(ID).Defense.Min, ItemBase.GetItem(ID).Defense.Max + 1), 1, UIntMax)
  else
    AItem.Defense := 0;
  // Damage
  if (AItem.Stack = 1) and (ItemBase.GetItem(ID).Damage.MinDamage.Min > 0) then
    AItem.MinDamage := Math.EnsureRange(Math.RandomRange(ItemBase.GetItem(ID).Damage.MinDamage.Min, ItemBase.GetItem(ID).Damage.MinDamage.Max + 1), 1,
      UIntMax - 1)
  else
    AItem.MinDamage := 0;
  if (AItem.Stack = 1) and (ItemBase.GetItem(ID).Damage.MaxDamage.Min > 0) then
    AItem.MaxDamage := Math.EnsureRange(Math.RandomRange(ItemBase.GetItem(ID).Damage.MaxDamage.Min, ItemBase.GetItem(ID).Damage.MaxDamage.Max + 1),
      2, UIntMax)
  else
    AItem.MaxDamage := 0;
  // Durability
  if (AItem.Stack = 1) then
    AItem.MaxDurability := Math.EnsureRange(Math.RandomRange(ItemBase.GetItem(ID).MaxDurability - 5, ItemBase.GetItem(ID).MaxDurability + 6),
      10, UIntMax)
  else
    AItem.MaxDurability := 0;
  AItem.Durability := AItem.MaxDurability;
  // Clear Bonuses
  for I := 0 to BonusCount - 1 do
    AItem.Bonus[I] := 0;
  // Price
  CalcItem(AItem);
  // Affix
  AItem.Identify := Math.IfThen(IsIdentify(), 0, -1);
end;

procedure TItems.Add(AZ: TMapEnum; AX: Int = -1; AY: Int = -1; AID: Int = -1; IsRare: Boolean = False);
var
  I, ID, FX, FY: UInt;
  FItem: Item;
  Value: Int;
  ItemType: TItemType;
begin
  I := 0;
  repeat
    if (AID >= 0) then
      ID := AID
    else
      ID := Math.RandomRange(0, Ord(High(TItemEnum)) + 1);
    if (AX >= 0) then
      FX := AX
    else
      FX := Math.RandomRange(1, UIntMax - 1);
    if (AY >= 0) then
      FY := AY
    else
      FY := Math.RandomRange(1, UIntMax - 1);
    if (I >= UIntMax) then
    begin
      ID := Ord(ivGold);
      Break;
    end;
    Inc(I);
  until (Map.GetTileEnum(FX, FY, AZ) in SpawnTiles) and (AZ in ItemBase.GetItem(ID).Deep);
  ItemType := ItemBase.GetItem(ID).ItemType;
  if ((AID < 0) and (ItemType in NotDropTypeItems)) then
    Exit;
  // Rare
  if not IsRare and ItemBase.GetItem(ID).Rare then
  begin
    Add(AZ, AX, AY, AID, Math.RandomRange(0, Math.IfThen(Mode.Wizard, 1, 9)) = 0);
    if not Mode.Wizard then
      Exit;
  end;
  //
  Make(ID, FItem);
  FItem.Amount := 1;
  FItem.MapID := Ord(AZ);
  case ItemType of
    itCoin:
      begin
        Value := Ord(AZ) + 1;
        FItem.Amount := Math.RandomRange(Value * Value, Value * Value * (5 - Ord(Game.Difficulty))) + 1;
        // Extra Gold from Monsters
        if (Player.Attributes.Attrib[atExtraGold].Value > 0) then
        begin
          Value := FItem.Amount + Player.Attributes.Attrib[atExtraGold].Value;
          FItem.Amount := Value.Percent(FItem.Amount);
        end;
      end;
  end;
  if ((FItem.Stack = 1) and (ItemType in WeaponTypeItems + ArmorTypeItems)) then
    FItem.Durability := Math.RandomRange(FItem.MaxDurability div 4, FItem.MaxDurability) + 1;
  FItem.X := FX;
  FItem.Y := FY;
  FItem.Equipment := 0;
  AddItemToDungeon(FItem);
end;

procedure TItems.Loot(const AX, AY: UInt; AItemEnum: TItemEnum);
begin
  Add(Map.Current, AX, AY, Ord(AItemEnum));
end;

procedure TItems.Loot(const AX, AY: UInt; AIsBoss: Boolean);
var
  V, I: UInt;
const
  M = 10;
begin
  V := Math.IfThen(AIsBoss, Ord(Map.Current) + 9, Ord(Map.Current) + 2);
  for I := 1 to V do
  begin
    // Gold
    if (Math.RandomRange(0, M) >= 6) then
      LootGold(AX, AY);
    { // Potion
      if ((Math.RandomRange(0, M) >= 7) or AIsBoss) then
      Loot(AX, AY, TItemEnum(Math.RandomRange(Ord(ivLesser_Healing_Potion),
      Ord(ivPotion_of_Full_Mana) + 1))); }
    // Scroll
    if ((Math.RandomRange(0, M) >= 8) or AIsBoss) then
      Loot(AX, AY, TItemEnum(Math.RandomRange(Ord(ivScroll_of_Minor_Healing), Ord(ivScroll_of_Town_Portal) + 1)));
    // Item
    if (Math.RandomRange(0, M) >= 9) then
      Add(Map.Current, AX, AY, -1, AIsBoss);
  end;
end;

procedure TItems.Render(AX, AY: UInt);
var
  MapID: UInt;
  I, Count: Int;
  FColor: Cardinal;
  FSymbol: Char;
  FItem: Item;
begin
  MapID := Ord(Map.Current);
  Count := Items_Dungeon_GetMapCount(MapID);
  for I := Count - 1 downto 0 do
  begin
    FItem := Items_Dungeon_GetMapItem(MapID, I);
    if not Map.InView(FItem.X, FItem.Y) or (not Mode.Wizard and not Map.GetFOV(FItem.X, FItem.Y)) then
      Continue;
    X := FItem.X - Player.X + AX + View.Left;
    Y := FItem.Y - Player.Y + AY + View.Top;
    if not Mode.Wizard and (Player.GetDist(FItem.X, FItem.Y) > Player.Vision) then
    begin
      FColor := clFog;
      FSymbol := '?';
    end
    else
    begin
      FColor := FItem.Color; // ItemBase[TItemEnum(FItem.ItemID)].Color;
      FSymbol := ItemBase.GetItem(FItem).Symbol;
    end;
    Terminal.Print(X, Y, FSymbol, FColor, 0);
  end;
end;

constructor TItems.Create;
var
  I: TItemEnum;
  P: Pointer;
begin
  Items_Open();
  P := TypeInfo(TItemEnum);
  for I := Low(TItemEnum) to High(TItemEnum) do
    FItemName[I] := GetEnumName(P, Ord(I)).GetName('iv');
end;

destructor TItems.Destroy;
begin
  Items_Close();
  inherited;
end;

function TItems.StrToItemEnum(const S: string): TItemEnum;
var
  P: Pointer;
begin
  P := TypeInfo(TItemEnum);
  Result := TItemEnum(GetEnumValue(P, 'iv' + S));
end;

function TItems.GetName(I: TItemEnum): string;
begin
  Result := FItemName[I];
end;

function TItems.GetInfo(Sign: string; Value: UInt; Color: string; RareColor: string = ''): string;
var
  S, P: string;
begin
  S := '';
  P := '';
  Result := '';
  if (Sign = '*') then
    Sign := '';
  if (Sign = 'x') then
  begin
    Sign := '';
    S := UI.Icon(icPlus);
  end;
  if (Sign = '@') then
  begin
    Sign := '';
    S := UI.Icon(icElixir);
  end;
  if (Color = 'Life') then
    S := S + UI.Icon(icLife);
  if (Color = 'Mana') then
  begin
    if ((Player.Attributes.Attrib[atMana].Value < Value) and (Sign <> '+')) then
      Color := 'NoMana';
    S := S + UI.Icon(icMana);
  end;
  if (Color = 'Food') then
    S := UI.Icon(icFood);
  if (Color = 'Ident') then
    S := UI.Icon(icQuestion);
  if (Color = 'Portal') then
    S := UI.Icon(icFlag);
  if (Color = 'Poison') then
    S := UI.Icon(icDrop);
  if (Color = 'Vision') then
    S := S + UI.Icon(icVision);
  if (Color = 'Repair') then
    S := UI.Icon(icHammer);
  if (Color = 'Strength') then
    S := UI.Icon(icStr);
  if (Color = 'Dexterity') then
    S := UI.Icon(icDex);
  if (Color = 'Willpower') then
    S := UI.Icon(icBook);
  if (Color = 'Perception') then
    S := UI.Icon(icLeaf);
  if (Color = 'Gold') then
  begin
    S := S + UI.Icon(icGold);
    P := '%';
  end;
  if (RareColor <> '') then
    Color := RareColor;
  if (Sign = '&') then
  begin
    Result := Trim(Terminal.Colorize(S, Color));
    Exit;
  end;
  if (Value > 0) then
    Result := Trim(Terminal.Colorize(Format('%s%s%d%s', [S, Sign, Value, P]), Color));
end;

function TItems.RenderInvItem(const AX, AY, I: Int; AItem: Item; IsAdvInfo: Boolean = False; IsRender: Boolean = True;
  PriceType: TPriceType = ptNone): string;
var
  S: string;
  D: TItemBase;
  RepairCost: UInt;

const
  T = '------';
  L = Length(T) + 1;

  function GetRedPrice(Price: UInt): string;
  begin
    Result := Terminal.Colorize(UI.Icon(icGold) + Price.ToString, 'Light Red');
  end;

begin
  Result := '';
  D := ItemBase.GetItem(AItem);
  Terminal.Print(AX - 4, AY + I, UI.KeyToStr(Chr(I + Ord('A')), '', IfThen(AItem.Equipment > 0, 'Equip', 'Key')));

  if IsRender then
  begin
    Terminal.ForegroundColor(AItem.Color);
    Terminal.Print(AX, AY + I, D.Symbol);
  end
  else
    Result := Result + D.Symbol + ' ';

  Terminal.ForegroundColor(clLightGray);
  if (IsAdvInfo and (Game.Timer = 0)) then
  begin
    S := '';
    if (D.SlotType <> stNone) and (AItem.Equipment > 0) then
      S := GetItemSlotName(D.SlotType);
  end;
  if (S <> '') then
    S := Items.GetItemInfo(AItem, False, 0, True) + ' ' + S
  else
    S := Trim(Items.GetItemInfo(AItem, False, 0, True));

  if IsRender then
  begin
    Terminal.Print(AX + 2, AY + I, S);
    S := '';
    case PriceType of
      ptSell:
        begin
          S := T;
          if ((AItem.Price > 1) and not ChItem(AItem)) then
          begin
            S := GetItemPrice(AItem.Price div 4, True);
            if (AItem.Equipment > 0) then
              S := GetRedPrice(AItem.Price div 4);
          end;
        end;
      ptBuy:
        begin
          S := GetItemPrice(AItem.Price);
        end;
      ptRepair:
        begin
          S := T;
          if ((AItem.Stack = 1) and (AItem.Amount = 1) and (Items.Index = 0)) then
          begin
            RepairCost := (AItem.MaxDurability - AItem.Durability) * 10;
            if (RepairCost > 0) then
              S := GetItemPrice(RepairCost);
          end;
        end;
    else
      if ((AItem.Price > 0) and Game.GetOption(apShPrice)) then
        S := GetItemPrice(AItem.Price, True);
    end;
    if Game.Timer > 0 then
      Terminal.Print(Status.Left - L, AY + I, S)
    else
      Terminal.Print(Screen.Width - L, AY + I, S);
  end
  else
    Result := Result + S;
end;

function TItems.GetBonus(const AItem: Item; const BonusType: TBonusType): UInt8;
begin
  case BonusType of
    btLife:
      Result := UInt8(AItem.Bonus[0] shr 24);
    btMana:
      Result := UInt8(AItem.Bonus[0] shr 16);
    btVis:
      Result := UInt8(AItem.Bonus[0] shr 8);
    btExtraGold:
      Result := UInt8(AItem.Bonus[0]);
    btStr:
      Result := UInt8(AItem.Bonus[1] shr 24);
    btDex:
      Result := UInt8(AItem.Bonus[1] shr 16);
    btWil:
      Result := UInt8(AItem.Bonus[1] shr 8);
    btPer:
      Result := UInt8(AItem.Bonus[1]);
    btReLife:
      Result := UInt8(AItem.Bonus[2] shr 24);
    btReMana:
      Result := UInt8(AItem.Bonus[2] shr 16);
    btLifeAfEachKill:
      Result := UInt8(AItem.Bonus[2] shr 8);
    btManaAfEachKill:
      Result := UInt8(AItem.Bonus[2]);
  else
    Result := 0;
  end;
end;

procedure TItems.SetBonus(var AItem: Item; const BonusType: TBonusType; const Value: UInt8);
var
  V: array [0 .. 3] of UInt8;
  I: Cardinal;
begin
  if (BonusType in [btStr .. btPer]) then
  begin
    V[0] := GetBonus(AItem, btStr);
    V[1] := GetBonus(AItem, btDex);
    V[2] := GetBonus(AItem, btWil);
    V[3] := GetBonus(AItem, btPer);
  end;
  if (BonusType in [btLife .. btExtraGold]) then
  begin
    V[0] := GetBonus(AItem, btLife);
    V[1] := GetBonus(AItem, btMana);
    V[2] := GetBonus(AItem, btVis);
    V[3] := GetBonus(AItem, btExtraGold);
  end;
  if (BonusType in [btReLife .. btManaAfEachKill]) then
  begin
    V[0] := GetBonus(AItem, btReLife);
    V[1] := GetBonus(AItem, btReMana);
    V[2] := GetBonus(AItem, btLifeAfEachKill);
    V[3] := GetBonus(AItem, btManaAfEachKill);
  end;

  case BonusType of
    btLife, btStr, btReLife:
      V[0] := Value;
    btMana, btDex, btReMana:
      V[1] := Value;
    btVis, btWil, btLifeAfEachKill:
      V[2] := Value;
    btExtraGold, btPer, btManaAfEachKill:
      V[3] := Value;
  end;

  I := (V[0] shl 24) or (V[1] shl 16) or (V[2] shl 8) or V[3];

  case BonusType of
    btLife .. btExtraGold:
      AItem.Bonus[0] := I;
    btStr .. btPer:
      AItem.Bonus[1] := I;
    btReLife .. btManaAfEachKill:
      AItem.Bonus[2] := I;
  end;
end;

function TItems.AddItemInfo(V: TArray<string>): string;
var
  I: UInt;
  R: string;
begin
  Result := '';
  for R in V do
    Result := Result + Trim(R) + ' ';
  Result := Trim(Result);
  if (Result <> '') then
    Result := '[[' + Result + ']]';
  Result := SysUtils.StringReplace(Result, '  ', ' ', [rfReplaceAll]);
end;

procedure TItems.AddItemToDungeon(AItem: Item);
var
  FItem: Item;
  I, FCount: Int;
begin
  // Add an item
  Items_Dungeon_AppendItem(AItem);
  // Sort all items on the tile
  FCount := Items_Dungeon_GetMapCountXY(AItem.MapID, AItem.X, AItem.Y);
  if (FCount = 1) then
    Exit;
  for I := 0 to FCount - 1 do
  begin
    FItem := Items_Dungeon_GetMapItemXY(AItem.MapID, I, AItem.X, AItem.Y);
    if (ItemBase.GetItem(FItem).ItemType in CorpseTypeItems) then
      if (Items_Dungeon_DeleteMapItemXY(AItem.MapID, I, AItem.X, AItem.Y, FItem) > 0) then
        Items_Dungeon_AppendItem(FItem);
  end;
end;

procedure TItems.AddItemToInv(AItemEnum: TItemEnum; AAmount: UInt = 1; EqFlag: Boolean = False; IdFlag: Boolean = False; SufID: UInt = 0);
var
  FItem: Item;
begin
  if (AAmount = 0) then
    Exit;
  Make(Ord(AItemEnum), FItem);
  if AAmount > ItemBase.GetItem(AItemEnum).MaxStack then
  begin
    FItem.Amount := ItemBase.GetItem(AItemEnum).MaxStack;
    AddItemToInv(AItemEnum, AAmount - ItemBase.GetItem(AItemEnum).MaxStack, EqFlag, IdFlag, SufID);
  end
  else
    FItem.Amount := AAmount;
  FItem.Equipment := IfThen(EqFlag, 1, 0);
  if IdFlag and (FItem.Identify = 0) then
    Items.Identify(FItem, True, False, SufID);
  Items_Inventory_AppendItem(FItem);
end;

procedure TItems.AddItemToInv(Index: Int = 0; AFlag: Boolean = False);
var
  FItem: Item;
  MapID: Int;
begin
  MapID := Ord(Map.Current);
  FItem := Items_Dungeon_GetMapItemXY(MapID, Index, Player.X, Player.Y);
  if (FItem.Stack > 1) and (FItem.Amount > 1) and not AFlag then
  begin
    Player.SetAmountScene(False, Index, FItem.Amount);
    Exit;
  end;
  if (Items_Dungeon_DeleteMapItemXY(MapID, Index, Player.X, Player.Y, FItem) > 0) then
  begin
    Items_Inventory_AppendItem(FItem);
    if (FItem.Amount = 1) then
      MsgLog.Add(Format(_('You picked up %s.'), [Items.GetNameThe(FItem)]))
    else
      MsgLog.Add(Format(_('You picked up %s (%dx).'), [Items.GetNameThe(FItem), FItem.Amount]));
    // Statistics
    case ItemBase.GetItem(FItem).ItemType of
      itCoin:
        Player.Statictics.Inc(stCoinsLooted, FItem.Amount);
    end;
    Player.Wait;
    Player.Calc;
  end;
end;

function TItems.GetInventory: string;
var
  SL: TStringList;
  I, FCount: Int;
  FItem: Item;
  S: string;
begin
  Result := '';
  SL := TStringList.Create;
  try
    FCount := Items_Inventory_GetCount().InRange(ItemMax);
    for I := 0 to FCount - 1 do
    begin
      FItem := Items_Inventory_GetItem(I);
      S := IfThen(FItem.Amount > 1, Format(' (%dx)', [FItem.Amount]), '');
      S := GetCapit(GetDescAn(Trim(Items.GetName(FItem, True) + S)));
      SL.Append(S);
    end;
    Result := SL.Text;
  finally
    FreeAndNil(SL);
  end;
end;

function TItems.GetItemEnum(AItemID: Int): TItemEnum;
begin
  Result := TItemEnum(AItemID);
end;

procedure TItems.LootGold(const AX, AY: UInt);
begin
  Loot(AX, AY, ivGold);
  if (Math.RandomRange(0, 4) = 0) then
  begin
    X := Map.EnsureRange(AX + (Math.RandomRange(0, 3) - 1));
    Y := Map.EnsureRange(AY + (Math.RandomRange(0, 3) - 1));
    if (Map.GetTileEnum(X, Y, Map.Current) in SpawnTiles) then
      Loot(X, Y, ivGold);
  end;
end;

procedure TItems.RenderInventory(PriceType: TPriceType = ptNone);
var
  I, FCount: Int;
begin
  FCount := Items_Inventory_GetCount().InRange(ItemMax);
  for I := 0 to FCount - 1 do
    Items.RenderInvItem(5, 2, I, Items_Inventory_GetItem(I), True, True, PriceType);
end;

function TItems.Identify(var AItem: Item; IsNew: Boolean = False; IsRare: Boolean = False; Index: UInt = 0): Boolean;
var
  I: UInt;
  SB: TSuffixBase;
begin
  Result := False;
  if (AItem.Identify = 0) then
  begin
    repeat
      // Random suffix
      I := Math.RandomRange(1, Ord(High(TSuffixEnum)) + 1);
      if (Index > 0) then
        I := Index;
      SB := Affixes.GetSuffix(TSuffixEnum(I));
      // Level
      { if (ItemBase[TItemEnum(AItem.ItemID)].ItemType in OilTypeItems) then
        begin
        Lev := Player.Attributes.Attrib[atLev].Value;
        AItem.Level := Math.EnsureRange(Math.RandomRange(Lev - 1,
        Lev + 2), 1, 15);
        AItem.Price := AItem.Price * AItem.Level;
        end; }
      if ((AItem.Level < SB.Level.Min) or (AItem.Level > SB.Level.Max)) then
        Continue;
      //
      if not(ItemBase.GetItem(AItem).ItemType in SB.Occurence) then
        Continue;
      // Rare
      if not IsRare and SB.Rare then
      begin
        Identify(AItem, IsNew, Math.RandomRange(0, Math.IfThen(Mode.Wizard, 1, 9)) = 0, Index);
        if not Mode.Wizard then
          Exit;
      end;
      //
      AItem.Identify := I;
      Affixes.DoSuffix(AItem);
      if IsNew then
      begin
        AItem.Durability := AItem.MaxDurability;
      end;
    until (AItem.Identify > 0);
    Result := True;
  end;
end;

function TItems.InvCount: Integer;
begin
  Result := Items_Inventory_GetCount();
end;

function TItems.GetName(AItem: Item; IsShort: Boolean = False): string;
var
  N, S: string;
begin
  N := _(Trim(GetName(TItemEnum(AItem.ItemID))));
  case AItem.Identify of
    0:
      begin
        if IsShort then
          S := ''
        else
          S := ' [[' + _('Unidentified') + ']]';
        Result := Terminal.Colorize(N + S, 'Unidentified');
      end;
    1 .. UIntMax:
      begin
        Result := N + ' ' + _(Affixes.GetSuffixName(TSuffixEnum(AItem.Identify)));
        if (AItem.SlotID > 0) then
          Result := Terminal.Colorize(Result, 'Rare')
        else
          Result := Terminal.Colorize(Result, 'Flask');
      end
  else
    Result := N;
  end;
  { if ItemBase[TItemEnum(AItem.ItemID)].ItemType in OilTypeItems then
    case AItem.Effect of
    tfCursed:
    Result := Terminal.Colorize(_('Cursed') + ' ' + GetPureText(Result),
    'Cursed');
    tfBlessed:
    Result := Terminal.Colorize(_('Blessed') + ' ' + GetPureText(Result),
    'Blessed');
    end; }
end;

function TItems.GetNameThe(AItem: Item): string;
begin
  Result := GetDescThe(GetPureText(Items.GetName(AItem, True)));
end;

procedure TItems.DelCorpses;
var
  I: Int;
  M: TMapEnum;
  FItem: Item;
begin
  for M := Low(TMapEnum) to High(TMapEnum) do
  begin
    if (M = Map.Current) then
      Continue;
    for I := Items_Dungeon_GetMapCount(Ord(M)) - 1 downto 0 do
    begin
      FItem := Items_Dungeon_GetMapItem(Ord(M), I);
      if (ItemBase.GetItem(FItem).ItemType in CorpseTypeItems + FoodTypeItems) then
        if (Items_Dungeon_DeleteMapItem(Ord(M), I, FItem) > 0) then
          Continue;
    end;
  end;
end;

procedure TItems.AddPlants;
var
  I, FCount: Int;
  M: TMapEnum;
  FItem: Item;
begin
  for M := Low(TMapEnum) to High(TMapEnum) do
  begin
    FCount := Items_Dungeon_GetMapCount(Ord(M));
    for I := 0 to FCount - 1 do
    begin
      FItem := Items_Dungeon_GetMapItem(Ord(M), I);
      if (ItemBase.GetItem(FItem).ItemType in PlantTypeItems) then
      begin
        X := FItem.X + Math.RandomRange(0, 2);
        Y := FItem.Y + Math.RandomRange(0, 2);
        if (Map.InMap(X, Y) and (Map.GetTileEnum(X, Y, M) in SpawnTiles) and (M in ItemBase.GetItem(FItem).Deep)) then
          Loot(X, Y, TItemEnum(FItem.ItemID));
      end;
    end;
  end;
end;

initialization

Items := TItems.Create;

finalization

FreeAndNil(Items);

end.
