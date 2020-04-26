unit Trollhunter.Item.Shop;

interface

uses
  Trollhunter.Types,
  uBeaRLibItemsCommon,
  Trollhunter.Player,
  Trollhunter.Item;

type
  TShopEnum = (shPotions, shScrolls, shHealer, shMana, shSmith, shArmors, shGloves, shFoods, shWeapons, shBoots, shTavern, shShields, shHelms,
    shJewelry, shGem, shRunes);

type
  TItemsStore = array [0 .. ItemMax - 1] of Item;

type
  TShop = class(TObject)
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
  TShops = class(TObject)
    FCurrent: TShopEnum;
    FShop: array [TShopEnum] of TShop;
    function GetShop(I: TShopEnum): TShop;
    procedure SetShop(I: TShopEnum; const Value: TShop);
  public
    constructor Create;
    destructor Destroy; override;
    procedure New;
    procedure Clear;
    function Render: UInt;
    function Count: UInt;
    property Current: TShopEnum read FCurrent write FCurrent;
    property Shop[I: TShopEnum]: TShop read GetShop write SetShop;
  end;

var
  Shops: TShops;

implementation

uses
  Math,
  SysUtils,
  Trollhunter.Map,
  Trollhunter.Creature,
  Trollhunter.Attribute,
  Trollhunter.Helpers,
  Trollhunter.Item.Types,
  Trollhunter.Item.Base;

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
  Result := FItemsStore[Index.InRange(ItemMax)];
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
  I: TShopEnum;
begin
  for I := Low(TShopEnum) to High(TShopEnum) do
    FreeAndNil(FShop[I]);
  inherited;
end;

function TShops.GetShop(I: TShopEnum): TShop;
begin
  Result := FShop[I];
end;

procedure TShops.New;
var
  FItem: Item;
  I, Max: UInt;
  ID: TItemEnum;
  S: TShopEnum;

  function GetItemID(): TItemEnum;
  begin
    Result := TItemEnum(Math.RandomRange(Ord(Low(TItemEnum)), Ord(High(TItemEnum)) + 1));
  end;

  function Check: Boolean;
  var
    Effects: TEffects;
  begin
    ID := GetItemID();
    Effects := ItemBase.GetItem(ID).Effects;
    case S of
      shTavern:
        Result := ID in TavernItems;
      shHealer:
        Result := efLife in Effects;
      shMana:
        Result := (efMana in Effects) and not(efLife in Effects);
      shPotions:
        Result := ItemBase.GetItem(ID).ItemType in PotionTypeItems;
      shScrolls:
        Result := ItemBase.GetItem(ID).ItemType in ScrollTypeItems;
      shArmors:
        Result := ItemBase.GetItem(ID).ItemType in ArmorTypeItems;
      shGloves:
        Result := ItemBase.GetItem(ID).ItemType in GlovesTypeItems;
      shBoots:
        Result := ItemBase.GetItem(ID).ItemType in BootsTypeItems;
      shHelms:
        Result := ItemBase.GetItem(ID).ItemType in HelmTypeItems;
      shShields:
        Result := ItemBase.GetItem(ID).ItemType in ShieldTypeItems;
      shWeapons:
        Result := ItemBase.GetItem(ID).ItemType in WeaponTypeItems;
      shSmith:
        Result := ItemBase.GetItem(ID).ItemType in SmithTypeItems + RepairTypeItems;
      shFoods:
        Result := ItemBase.GetItem(ID).ItemType in FoodTypeItems + PlantTypeItems;
      shGem:
        Result := ItemBase.GetItem(ID).ItemType in GemTypeItems;
      shJewelry:
        Result := ItemBase.GetItem(ID).ItemType in JewelryTypeItems;
      shRunes:
        Result := ItemBase.GetItem(ID).ItemType in RuneTypeItems;
    else
      Result := False;
    end;
  end;

begin
  for S := Low(TShopEnum) to High(TShopEnum) do
  begin
    Shops.Shop[S].Clear;
    Max := EnsureRange(Player.Attributes.Attrib[atLev].Value * 4, 4, ItemMax);
    if S in [shSmith, shTavern, shFoods] then
      Max := EnsureRange(Max + 3, 7, ItemMax);
    for I := 0 to Max - 1 do
    begin
      repeat
        repeat
        until Check;
      until (TMapEnum(Player.Attributes.Attrib[atMaxMap].Value) in ItemBase.GetItem(ID).Deep);
      Items.Make(Ord(ID), FItem);
      Items.Identify(FItem, True);
      Shops.Shop[S].Add(FItem);
    end;
  end;
end;

function TShops.Render: UInt;
var
  I: UInt;
begin
  Result := Shops.Shop[Shops.Current].Count.InRange(ItemMax);
  for I := 0 to Result - 1 do
    Items.RenderInvItem(5, 2, I, Shops.Shop[Shops.Current].GetItem(I), True, True, ptBuy);
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
