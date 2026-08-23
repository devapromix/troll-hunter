unit Trollhunter.Item.Shop;

interface

uses
  Trollhunter.Types,
  Trollhunter.Item.Common,
  Trollhunter.Player,
  Trollhunter.Item;

type
  TItemsStore = array [0 .. ItemMax - 1] of Item;

type
  TShop = class
  private
    FItemsStore: TItemsStore;
    FCount: UInt;
  public
    constructor Create;
    procedure Clear;
    property Count: UInt read FCount;
    procedure Add(const AItem: Item);
    function GetItem(const Index: UInt): Item;
  end;

type
  TShops = class
    FCurrent: TShopEnum;
    FShop: array [TShopEnum] of TShop;
    function GetShop(I: TShopEnum): TShop;
    procedure SetShop(I: TShopEnum; const Value: TShop);
  public
    constructor Create;
    destructor Destroy; override;
    procedure New;
    procedure Clear;
    procedure Render;
    function Count: UInt;
    property Current: TShopEnum read FCurrent write FCurrent;
    property Shop[I: TShopEnum]: TShop read GetShop write SetShop;
  end;

var
  Shops: TShops;

implementation

uses
  SysUtils,
  Math,
  Trollhunter.Map,
  Trollhunter.Effect,
  Trollhunter.Creature,
  Trollhunter.Attribute,
  Trollhunter.Helpers,
  Trollhunter.Game,
  Trollhunter.Item.Affixes,
  Trollhunter.Item.Types;

const
  CShopItemTypes: array [TShopEnum] of TSetOfItem = (
    PotionTypeItems,                    // shPotions
    ScrollTypeItems,                    // shScrolls
    [],                                 // shHealer  (перевірка за Effects)
    [],                                 // shMana    (перевірка за Effects)
    SmithTypeItems + RepairTypeItems,   // shSmith
    ArmorTypeItems,                     // shArmors
    GlovesTypeItems,                    // shGloves
    FoodTypeItems + PlantTypeItems,     // shFoods
    WeaponTypeItems,                    // shWeapons
    BootsTypeItems,                     // shBoots
    [],                                 // shTavern  (перевірка за TavernItems)
    ShieldTypeItems,                    // shShields
    HelmTypeItems,                      // shHelms
    JewelryTypeItems,                   // shJewelry
    GemTypeItems,                       // shGem
    RuneTypeItems,                      // shRunes
    QuiverTypeItems,                    // shQuivers
    StaffTypeItems,                     // shStaves
    WandTypeItems,                      // shWands
    BookTypeItems,                      // shBooks
    BowTypeItems,                       // shBows
    DaggerTypeItems,                    // shDaggers
    VenomTypeItems                      // shVenoms
  );

{ TShop }

procedure TShop.Add(const AItem: Item);
begin
  FItemsStore[FCount] := AItem;
  Inc(FCount);
end;

procedure TShop.Clear;
var
  I: UInt;
begin
  for I := Low(FItemsStore) to High(FItemsStore) do
    Items_Clear_Item(FItemsStore[I]);
  FCount := 0;
end;

constructor TShop.Create;
begin
  Self.Clear;
end;

function TShop.GetItem(const Index: UInt): Item;
begin
  Result := FItemsStore[Index.InRange(FCount)];
end;

{ TShops }

procedure TShops.Clear;
var
  S: TShopEnum;
begin
  for S := Low(TShopEnum) to High(TShopEnum) do
    FShop[S].Clear;
end;

function TShops.Count: UInt;
begin
  Result := Length(FShop);
end;

constructor TShops.Create;
var
  S: TShopEnum;
begin
  for S := Low(TShopEnum) to High(TShopEnum) do
    FShop[S] := TShop.Create;
  Current := shPotions;
end;

destructor TShops.Destroy;
var
  S: TShopEnum;
begin
  for S := Low(TShopEnum) to High(TShopEnum) do
    FreeAndNil(FShop[S]);
  inherited;
end;

function TShops.GetShop(I: TShopEnum): TShop;
begin
  Result := FShop[I];
end;

procedure TShops.New;
const
  CMaxShopAffixAttempts = 30;
var
  FItem: Item;
  I, Max: UInt;
  ID: TItemEnum;
  S: TShopEnum;
  LCandidates: array [0 .. Ord(High(TItemEnum))] of TItemEnum;
  LCount: UInt;
  LPlayerLevel: UInt;
  LAttempts: UInt;

  function MatchesShop(const AID: TItemEnum): Boolean;
  begin
    case S of
      shTavern:
        Result := AID in TavernItems;
      shHealer:
        Result := efLife in ItemBase[AID].Effects;
      shMana:
        Result := (efMana in ItemBase[AID].Effects) and
          not(efLife in ItemBase[AID].Effects);
    else
      Result := ItemBase[AID].ItemType in CShopItemTypes[S];
    end;
  end;

  function MatchesLevel(const AID: TItemEnum): Boolean;
  begin
    Result := ItemBase[AID].Level <= LPlayerLevel;
  end;

  procedure RestrictShopAffixes(var AItem: Item);
  var
    LHasPrefix, LHasSuffix, LProtectSuffix: Boolean;
  begin
    LHasPrefix := AItem.Prefix = 0;
    LHasSuffix := AItem.Identify = 0;
    LProtectSuffix := ItemBase[ID].ItemType in JewelryTypeItems;
    case Game.Difficulty of
      dfHell:
        begin
          if LHasPrefix then
            AItem.Prefix := -1;
          if LHasSuffix and not LProtectSuffix then
            AItem.Identify := -1;
        end;
      dfHard:
        if LHasSuffix and not LProtectSuffix then
          AItem.Identify := -1;
      dfNormal:
        if LHasPrefix and LHasSuffix then
          if LProtectSuffix then
            AItem.Prefix := -1
          else if Math.RandomRange(0, 2) = 0 then
            AItem.Prefix := -1
          else
            AItem.Identify := -1;
    end;
  end;

  function HasRareAffix(const AItem: Item): Boolean;
  begin
    Result := ((AItem.Prefix > 0) and
      PrefixBase[TPrefixEnum(AItem.Prefix)].Rare) or
      ((AItem.Identify > 0) and SuffixBase[TSuffixEnum(AItem.Identify)].Rare);
  end;

begin
  LPlayerLevel := Player.Attributes.Attrib[atLev].Value;
  Max := EnsureRange(3 + LPlayerLevel, 4, ItemMax);
  for S := Low(TShopEnum) to High(TShopEnum) do
  begin
    Shops.Shop[S].Clear;
    LCount := 0;
    for ID := Low(TItemEnum) to High(TItemEnum) do
      if MatchesShop(ID) and MatchesLevel(ID) and not ItemBase[ID].Rare then
      begin
        LCandidates[LCount] := ID;
        Inc(LCount);
      end;
    if LCount > 0 then
      for I := 0 to Max - 1 do
      begin
        LAttempts := 0;
        repeat
          ID := LCandidates[Math.RandomRange(0, LCount)];
          Items.Make(Ord(ID), FItem);
          RestrictShopAffixes(FItem);
          Items.Identify(FItem, True);
          Inc(LAttempts);
        until not HasRareAffix(FItem) or (LAttempts >= CMaxShopAffixAttempts);
        Shops.Shop[S].Add(FItem);
      end;
  end;
end;

procedure TShops.Render;
var
  I, C: Int;
begin
  C := Shops.Shop[Shops.Current].Count.InRange(ItemMax);
  for I := 0 to C - 1 do
    Items.RenderInvItem(5, 2, I, Shops.Shop[Shops.Current].GetItem(I), True,
      True, ptBuy);
end;

procedure TShops.SetShop(I: TShopEnum; const Value: TShop);
begin
  FShop[I] := Value;
end;

initialization

Shops := TShops.Create;

finalization

FreeAndNil(Shops);

end.
