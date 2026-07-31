unit Trollhunter.Item.Shop;

interface

uses
  Trollhunter.Types,
  Trollhunter.Item.Common,
  Trollhunter.Player,
  Trollhunter.Item;

type
  TShopEnum = (shPotions, shScrolls, shHealer, shMana, shSmith, shArmors,
    shGloves, shFoods, shWeapons, shBoots, shTavern, shShields, shHelms,
    shJewelry, shGem, shRunes, shQuivers, shStaves, shWands, shBooks, shBows);

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
  Trollhunter.Creature,
  Trollhunter.Attribute,
  Trollhunter.Helpers,
  Trollhunter.Game,
  Trollhunter.Item.Types;

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
var
  FItem: Item;
  I, Max: UInt;
  ID: TItemEnum;
  S: TShopEnum;
  LCandidates: array [0 .. Ord(High(TItemEnum))] of TItemEnum;
  LCount: UInt;

  function MatchesShop(const AID: TItemEnum): Boolean;
  var
    Effects: TEffects;
  begin
    Effects := ItemBase[AID].Effects;
    case S of
      shTavern:
        Result := AID in TavernItems;
      shHealer:
        Result := efLife in Effects;
      shMana:
        Result := (efMana in Effects) and not(efLife in Effects);
      shPotions:
        Result := ItemBase[AID].ItemType in PotionTypeItems;
      shScrolls:
        Result := ItemBase[AID].ItemType in ScrollTypeItems;
      shArmors:
        Result := ItemBase[AID].ItemType in ArmorTypeItems;
      shGloves:
        Result := ItemBase[AID].ItemType in GlovesTypeItems;
      shBoots:
        Result := ItemBase[AID].ItemType in BootsTypeItems;
      shHelms:
        Result := ItemBase[AID].ItemType in HelmTypeItems;
      shShields:
        Result := ItemBase[AID].ItemType in ShieldTypeItems;
      shWeapons:
        Result := ItemBase[AID].ItemType in WeaponTypeItems;
      shSmith:
        Result := ItemBase[AID].ItemType in SmithTypeItems + RepairTypeItems;
      shFoods:
        Result := ItemBase[AID].ItemType in FoodTypeItems + PlantTypeItems;
      shGem:
        Result := ItemBase[AID].ItemType in GemTypeItems;
      shJewelry:
        Result := ItemBase[AID].ItemType in JewelryTypeItems;
      shRunes:
        Result := ItemBase[AID].ItemType in RuneTypeItems;
      shQuivers:
        Result := ItemBase[AID].ItemType in QuiverTypeItems;
      shStaves:
        Result := ItemBase[AID].ItemType in StaffTypeItems;
      shWands:
        Result := ItemBase[AID].ItemType in WandTypeItems;
      shBooks:
        Result := ItemBase[AID].ItemType in BookTypeItems;
      shBows:
        Result := ItemBase[AID].ItemType in BowTypeItems;
    else
      Result := False;
    end;
  end;

  function MatchesLevel(const AID: TItemEnum): Boolean;
  begin
    Result := ItemBase[AID].Level <= Player.Attributes.Attrib[atLev].Value;
  end;

  procedure RestrictShopAffixes(var AItem: Item);
  var
    LHasPrefix, LHasSuffix: Boolean;
  begin
    LHasPrefix := AItem.Prefix = 0;
    LHasSuffix := AItem.Identify = 0;
    case Game.Difficulty of
      dfHell:
        begin
          if LHasPrefix then
            AItem.Prefix := -1;
          if LHasSuffix then
            AItem.Identify := -1;
        end;
      dfHard:
        if LHasSuffix then
          AItem.Identify := -1;
      dfNormal:
        if LHasPrefix and LHasSuffix then
          if Math.RandomRange(0, 2) = 0 then
            AItem.Prefix := -1
          else
            AItem.Identify := -1;
    end;
  end;

begin
  for S := Low(TShopEnum) to High(TShopEnum) do
  begin
    Shops.Shop[S].Clear;
    Max := EnsureRange(Player.Attributes.Attrib[atLev].Value * 4, 4, ItemMax);
    if S = shSmith then
      Max := EnsureRange(Max + 3, 7, ItemMax);
    LCount := 0;
    for ID := Low(TItemEnum) to High(TItemEnum) do
      if MatchesShop(ID) and MatchesLevel(ID) then
      begin
        LCandidates[LCount] := ID;
        Inc(LCount);
      end;
    if LCount > 0 then
      for I := 0 to Max - 1 do
      begin
        ID := LCandidates[Math.RandomRange(0, LCount)];
        Items.Make(Ord(ID), FItem);
        RestrictShopAffixes(FItem);
        Items.Identify(FItem, True);
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
