unit Trollhunter.Item;

interface

uses
  Windows,
  Graphics,
  Classes,
  Trollhunter.Color,
  Trollhunter.Craft,
  Trollhunter.Entity,
  Trollhunter.RandItems;

const
  ItemsCount = 76;

const
  RandomScrollsCount = 9;
  RandomPotionsCount = 8;

type
  TCategory = (dsNone, dsKey, dsGold, dsBody, dsHead, dsFoot, dsRHand, dsLHand,
    dsAmulet, dsRing, dsPotion, dsScroll, dsRepair, dsCraft, dsPlant);

type
  TSubCats = (scNone, scKey, scFill, scDispel, scAntidote, scTeleport, scPortal,
    scSummon, scIdentify, scWizardEye, scStrength, scDexterity, scWill, scSpeed,
    scRepair, scRepair3, scRepair6, scRepair9, scRepair12, scRepair15,
    scRepair25, scRepairAll, scLife, scLife25, scLife50, scLife75, scLife100,
    scLife200, scMana, scMana25, scMana50, scMana75, scMana100, scMana200,
    scDagger, scAxe, scSword, scMace, scSpear, scShield, scBow, scCrossBow);

type
  TCatSet = set of TCategory;

type
  TSubCSet = set of TSubCats;

type
  TRarity = (riNormal, riMagic, riRare, riUnique);

type
  TJewlery = (jwNone, jwSteel, jwBronze, jwCopper, jwBrass, jwSilver, jwGold,
    jwAgate, jwOpal, jwAmethyst, jwRuby, jwEmerald, jwJade, jwPearl, jwQuartz,
    jwSapphire, jwDiamond);

const
  KeySet = [dsKey];
  PotionSet = [dsPotion];
  WeaponSet = [dsRHand];
  ArmorSet = [dsBody, dsHead, dsLHand, dsFoot];
  AmuRingSet = [dsAmulet, dsRing];
  UseSet = [dsPotion, dsRepair, dsCraft];
  CraftSet = [dsCraft];
  FoodSet = [dsPlant];
  RepairSet = [dsRepair];
  ScrollSet = [dsScroll];
  WeapArmSet = WeaponSet + ArmorSet;
  EquipSet = WeapArmSet + AmuRingSet;
  DropSet = EquipSet + PotionSet + RepairSet + KeySet + CraftSet + FoodSet +
    ScrollSet;

type
  TItemRec = record
    Sprite: string;
    AdvSprite: string;
    MinDamage: Integer;
    MaxDamage: Integer;
    Protect: Integer;
    Rarity: TRarity;
    Tough: Integer;
    MaxTough: Integer;
    Weight: Integer;
    ManaCost: Integer;
    IsStack: Boolean;
    MinCount: Integer;
    MaxCount: Integer;
    Category: TCategory;
    SubCats: TSubCSet;
    ColorTag: Integer;
    Color: Integer;
    BonusStrength: Integer;
    BonusDexterity: Integer;
    BonusWill: Integer;
    BonusSpeed: Integer;
    BonusLife: Integer;
    BonusMana: Integer;
    NeedStrength: Integer;
    NeedDexterity: Integer;
    NeedWill: Integer;
    NeedSpeed: Integer;
    NeedMagic: Integer;
  end;

const
  // RandomScroll = 'SCROLLA,SCROLLB,SCROLLC,SCROLLD,SCROLLE,SCROLLF,SCROLLG,SCROLLH,';
  // RandomPotion = 'POTIONA,POTIONB,POTIONC,POTIOND,POTIONE,POTIONF,POTIONG,POTIONH,';

  DefaultItems = 'GOLDCOINS,KEY,SLEDGEHAMMER,ARROW,BOLT,';

  PotionLevel1 =
    'MINIPOTION,MINILIFEPOTION,MINIMANAPOTION,MINIMEGAPOTION,MINIOILPOTION,';
  PotionLevel2 =
    'NORMPOTION,NORMLIFEPOTION,NORMMANAPOTION,NORMMEGAPOTION,NORMOILPOTION,';
  PotionLevel3 =
    'BASEPOTION,BASELIFEPOTION,BASEMANAPOTION,BASEMEGAPOTION,BASEOILPOTION,';
  PotionLevel4 =
    'NANOPOTION,NANOLIFEPOTION,NANOMANAPOTION,NANOMEGAPOTION,NANOOILPOTION,';
  PotionLevel5 =
    'BIGPOTION,BIGLIFEPOTION,BIGMANAPOTION,BIGMEGAPOTION,BIGOILPOTION,';
  PotionLevel6 = '';
  PotionLevel7 = '';

  WeaponLevel1 =
    'STONEHAMMER,HATCHET,SHORTSWORD,HUNTBOW,LIGHTCROSSBOW,SMALLSHIELD,';
  WeaponLevel2 = 'WARAXE,LONGBOW,SIEGECROSSBOW,LARGESHIELD,';
  WeaponLevel3 = 'LARGEAXE,TOWERSHIELD,';
  WeaponLevel4 = 'BROADAXE,GOTHICSHIELD,';
  WeaponLevel5 = 'BATTLEAXE,';
  WeaponLevel6 = 'GREATAXE,';
  WeaponLevel7 = 'GIANTAXE,';

  AmmuntLevel1 = 'LEATHERARMOR,CAP,MESHBOOTS,';
  AmmuntLevel2 = 'STUDDEDLEATHER,HELM,HEAVYBOOTS,';
  AmmuntLevel3 = 'RINGMAIL,';
  AmmuntLevel4 = 'SCALEMAIL,';
  AmmuntLevel5 = '';
  AmmuntLevel6 = '';
  AmmuntLevel7 = '';

  RarityLevel1 = 'EARTHRING,';
  RarityLevel2 = 'FIRERING,';
  RarityLevel3 = '';
  RarityLevel4 = '';
  RarityLevel5 = '';
  RarityLevel6 = '';
  RarityLevel7 = '';

  ScrollLevel1 = '';
  ScrollLevel2 = '';
  ScrollLevel3 = '';
  ScrollLevel4 = '';
  ScrollLevel5 = '';
  ScrollLevel6 = '';
  ScrollLevel7 = '';

const
  DungeonItems: array [0 .. ItemsCount - 1] of TItemRec = (

{$I Items/Items.itm         }
{$I Items/Potions.itm       }
{$I Items/RandomPotions.itm }
{$I Items/RandomScrolls.itm }
{$I Items/Armors.itm        }
{$I Items/Shields.itm       }
{$I Items/Bows.itm          }
{$I Items/Crossbows.itm     }
{$I Items/Axes.itm          }
{$I Items/Swords.itm        }
{$I Items/Maces.itm         }
{$I Items/Rings.itm         }
{$I Items/Amulets.itm       }
{$I Items/Plants.itm        }
  );

var
  RandomScrolls, RandomPotions: string;

type
  TBaseItem = class(TEntity)
  private
    FCount: Integer;
    SmallImage: Graphics.TBitmap;
    HeroImage: Graphics.TBitmap;
    procedure SetCount(const Value: Integer);
  public
    Prop: TItemRec;
    constructor Create(AX, AY: Integer);
    destructor Destroy; override;
    property Count: Integer read FCount write SetCount;
  end;

  TItem = array of TBaseItem;

  TItems = class(TObject)
  private
    FItem: TItem;
    procedure SetItem(const Value: TItem);
  public
    function Use(ItemA, ItemB: string; I: Integer): Boolean;
    property Item: TItem read FItem write SetItem;
    procedure Clear;
    procedure Pickup(Index: Integer = 0);
    procedure PickupAll;
    function Craft(A, B: string): string;
    procedure Damage(const ItemSet: TCatSet; Chance: Byte = 3);
    function IsDollSubCat(SC: TSubCats): Boolean;
    function IsBow: Boolean;
    function IsCrossBow: Boolean;
    function IsRangedWeapon: Boolean;
    function GetDollItemID(ACatSet: TCatSet): string;
    function GetDollItemSubCat(ACatSet: TCatSet): TSubCSet;
    procedure Add(const X, Y: Integer; AName: string); overload;
    procedure Add(ID: string; ACount: Integer = 1); overload;
    procedure AddAndEquip(ID: string; ACount: Integer = 1);
    procedure Render(X, Y, DX, DY: Integer);
    procedure Colors(var Icon: Graphics.TBitmap; ItemIndex: Integer);
    procedure SetColor(const AColor: Integer);
    procedure UseItem(const Index: Integer; Category: TCatSet);
    procedure UseSubCats(const Index: Integer);
    function CellItemsCount(X, Y: Integer): Integer;
    function ItemIndex(ID: string): Integer; overload;
    function ItemIndex(ID: Integer): Integer; overload;
    function GetDollText(I, V: Integer): string;
    procedure RepairAll;
    procedure MakeCraftDoc;
    procedure MakeAlchemyDoc;
    procedure Key;
    procedure Grow;
    procedure Identify;
    function GetItemProp(ACount, ATough, I, V: Integer): string;
    function GetWeight(Index: Integer): string;
    procedure RenderPCInvStat(Y: Integer);
    constructor Create;
    destructor Destroy; override;
  end;

var
  Items: TItems;

implementation

uses
  SysUtils,
  Trollhunter.Utils,
  Trollhunter.Error,
  Trollhunter.Map,
  Trollhunter.Graph,
  Trollhunter.Creatures,
  Trollhunter.Scenes,
  Trollhunter.Log,
  Trollhunter.Scene.Item,
  Trollhunter.Map.Tiles,
  Trollhunter.Scene.Items,
  Trollhunter.Lang,
  Trollhunter.TempSys,
  Trollhunter.Inv,
  Trollhunter.Skill,
  Trollhunter.Formulas;

{ TBaseItem }

constructor TBaseItem.Create(AX, AY: Integer);
begin
  try
    inherited Create;
    SmallImage := Graphics.TBitmap.Create;
    HeroImage := Graphics.TBitmap.Create;
    HeroImage.Transparent := True;
    Count := 1;
    Prop.Tough := 0;
    SetPosition(AX, AY);
  except
    on E: Exception do
      Error.Add('BaseItem.Create', E.Message);
  end;
end;

destructor TBaseItem.Destroy;
begin
  SmallImage.Free;
  HeroImage.Free;
  inherited;
end;

procedure TBaseItem.SetCount(const Value: Integer);
begin
  FCount := Value;
end;

{ TItems }

procedure TItems.Add(const X, Y: Integer; AName: string);
var
  I, P: Integer;
  Tileset: Graphics.TBitmap;
begin
  Tileset := Graphics.TBitmap.Create;
  try
    AName := UpperCase(AName);
    if (AName = '') then
      Exit;
    P := ItemIndex(AName);
    if (P < 0) or (P > High(DungeonItems)) then
      Exit;
    I := Length(FItem) + 1;
    SetLength(FItem, I);
    FItem[I - 1] := TBaseItem.Create(X, Y);
    with FItem[I - 1] do
      with DungeonItems[P] do
      begin
        Name := AName;
        Prop := DungeonItems[P];
        Count := Rand(MinCount, MaxCount);
        if Prop.IsStack and (Category = dsGold) then
          Count := Map.Level * Count;
        Prop.Tough := Rand(Prop.MaxTough div 3, Prop.MaxTough - 1);

        if (AdvSprite = '') then
          Tileset.Handle := Windows.LoadBitmap(hInstance, PChar(Name))
        else
          Tileset.Handle := Windows.LoadBitmap(hInstance, PChar(AdvSprite));

        Graph.BitmapFromTileset(Image, Tileset, 1);
        HeroImage.Assign(Image);
        Graph.BitmapFromTileset(Image, Tileset, 0);
        SmallImage.Assign(Image);
        Colors(SmallImage, P);
        ScaleBmp(SmallImage, TileSize div 2, TileSize div 2);
        SmallImage.Transparent := True;
      end;
    Tileset.Free;
  except
    on E: Exception do
      Error.Add('Items.Add', E.Message);
  end;
end;

procedure TItems.Add(ID: string; ACount: Integer = 1);
var
  I: Integer;
begin
  try
    ID := UpperCase(ID);
    if (ID = '') then
      Exit;
    I := ItemIndex(ID);
    if (I < 0) or (I > High(DungeonItems)) then
      Exit;
    with Creatures.PC do
    begin
      if (ACount > 1) and not DungeonItems[I].IsStack then
        ACount := 1;
      if not Inv.Add(ID, ACount, DungeonItems[I].Weight,
        DungeonItems[I].MaxTough, DungeonItems[I].IsStack) then
        Exit;
    end;
  except
    on E: Exception do
      Error.Add('Items.Add', E.Message);
  end;
end;

function TItems.CellItemsCount(X, Y: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  if (Length(Items.Item) > 0) then
    for I := 0 to High(Items.Item) do
      if (Items.Item[I].Pos.X = X) and (Items.Item[I].Pos.Y = Y) then
        Inc(Result);
end;

procedure TItems.Colors(var Icon: Graphics.TBitmap; ItemIndex: Integer);
begin
  with DungeonItems[ItemIndex] do
    try
      if (ColorTag > 0) then
      begin
        case Category of
          dsPotion:
            Graph.ModTileColor(Icon, AdvSprite,
              Creatures.PC.Potions.GetColor(ColorTag));
          dsScroll:
            Graph.ModTileColor(Icon, AdvSprite,
              Creatures.PC.Scrolls.GetColor(ColorTag));
        end;
        Exit;
      end;
      if (Color = clNone) then
        Exit;
      if (AdvSprite = '') then
        Graph.ModTileColor(Icon, Sprite, Color)
      else
        Graph.ModTileColor(Icon, AdvSprite, Color);
    except
      on E: Exception do
        Error.Add('Items.Colors', E.Message);
    end;
end;

function TItems.Craft(A, B: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(CraftItems) do
    if ((A = CraftItems[I].A) and (B = CraftItems[I].B)) or
      ((A = CraftItems[I].B) and (B = CraftItems[I].A)) then
    begin
      Result := CraftItems[I].C;
      Exit;
    end;
end;

procedure TItems.Damage(const ItemSet: TCatSet; Chance: Byte);
var
  I, J: Integer;
begin
  with Creatures.PC do
    for I := 1 to Inv.Count do
    begin
      if Inv.GetDoll(I) and not(DungeonItems[Items.ItemIndex(I)].IsStack) and
        (DungeonItems[Items.ItemIndex(I)].Category in ItemSet) then
      begin
        if (Rand(1, Chance) > 1) then
          Continue;
        J := Inv.GetTough(I);
        if (J > 0) then
          Dec(J);
        Inv.SetTough(I, J);
        if (Inv.GetTough(I) = 0) then
        begin
          SceneItem.UnEquip(I, False);
          Redraw;
        end;
        Exit;
      end;
    end;
end;

constructor TItems.Create;
var
  I: Byte;
begin
  RandomPotions := '';
  for I := 1 to RandomPotionsCount do
    RandomPotions := RandomPotions + 'POTION' + Chr(I + 64) + ',';
  RandomScrolls := '';
  for I := 1 to RandomScrollsCount do
    RandomScrolls := RandomScrolls + 'SCROLL' + Chr(I + 64) + ',';
end;

destructor TItems.Destroy;
begin

  inherited;
end;

procedure TItems.Key;
begin
  Graph.Messagebar.Clear;
  if (Map.Cell[Creatures.PC.Pos.Y][Creatures.PC.Pos.X].Tile
    in [tlLockedWoodChest, tlLockedBestChest]) then
  begin
    OpenChest(True);
    Log.Apply;
    Scenes.Render;
  end;
  Scenes.Render;
end;

procedure TItems.UseSubCats(const Index: Integer);
begin
  try
    with Creatures.PC do
      with DungeonItems[Index] do
        with TempSys do
        begin
          // Life
          if (scLife in SubCats) then
            Life.SetToMax;
          if (scLife25 in SubCats) then
            Add('VialOfLife', 5, 5);
          if (scLife50 in SubCats) then
            Add('VialOfLife', 10, 5);
          if (scLife75 in SubCats) then
            Add('VialOfLife', 15, 5);
          if (scLife100 in SubCats) then
            Add('VialOfLife', 10, 10);
          if (scLife200 in SubCats) then
            Add('VialOfLife', 20, 10);
          // Mana
          if (scMana in SubCats) then
            Mana.SetToMax;
          if (scMana25 in SubCats) then
            Add('VialOfMana', 5, 5);
          if (scMana50 in SubCats) then
            Add('VialOfMana', 10, 5);
          if (scMana75 in SubCats) then
            Add('VialOfMana', 15, 5);
          if (scMana100 in SubCats) then
            Add('VialOfMana', 10, 10);
          if (scMana200 in SubCats) then
            Add('VialOfMana', 20, 10);
          // Drink Oil
          if (scRepair3 in SubCats) then
            Add('Poison', 3, 10);
          if (scRepair6 in SubCats) then
            Add('Poison', 6, 10);
          if (scRepair9 in SubCats) then
            Add('Poison', 9, 10);
          if (scRepair12 in SubCats) then
            Add('Poison', 12, 10);
          if (scRepair15 in SubCats) then
            Add('Poison', 15, 10);
          // Atr
          if (scStrength in SubCats) then
            AddStrength;
          if (scDexterity in SubCats) then
            AddDexterity;
          if (scWill in SubCats) then
            AddWill;
          if (scSpeed in SubCats) then
            AddSpeed;
          // Misc
          if (scFill in SubCats) then
            Fill;
          if (scAntidote in SubCats) then
            ClearVar('Poison');
          if (scKey in SubCats) then
            Key;
          if (scTeleport in SubCats) then
            Creatures.Teleport(False);
          if (scSummon in SubCats) then
            Creatures.Summon;
          if (scIdentify in SubCats) then
            Identify;
          if (scPortal in SubCats) then
            Portal;
          if (scWizardEye in SubCats) then
            Add('WizardEye', GetWizardEyePower, Mana.Max);
          if (scDispel in SubCats) then
            Clear;
          if (scRepairAll in SubCats) then
            RepairAll;
        end;
  except
    on E: Exception do
      Error.Add('Items.UseSubCats', E.Message);
  end;
end;

procedure TItems.UseItem(const Index: Integer; Category: TCatSet);
begin
  try
    if (DungeonItems[Index].Category in Category) then
      UseSubCats(Index);
  except
    on E: Exception do
      Error.Add('Items.UseItem', E.Message);
  end;
end;

procedure TItems.Clear;
begin
  SetLength(FItem, 0);
end;

function TItems.GetItemProp(ACount, ATough, I, V: Integer): string;
begin
  Result := '';
  with DungeonItems[V] do
  begin
    if (MaxDamage > 0) then
      Result := Result + Format(' [%d-%d]', [MinDamage, MaxDamage]);
    if (Protect > 0) then
      Result := Result + Format(' [%d]', [Protect]);
    if (ACount > 1) then
      Result := Result + Format(' (%dx)', [ACount]);
    if not IsStack and (MaxTough > 0) then
      Result := Result + Format(' <%d/%d>', [ATough, MaxTough]);
  end;
end;

function TItems.GetWeight(Index: Integer): string;
begin
  if (DungeonItems[Index].Weight > 0) then
    Result := Format(' %ds', [DungeonItems[Index].Weight])
  else
    Result := '';
end;

procedure TItems.Grow;
var
  X, Y, Z: Integer;
begin
  if (Length(Items.Item) = 0) or (Rand(1, 3) > 1) then
    Exit;
  try
    for Z := 0 to High(Items.Item) do
      if (DungeonItems[ItemIndex(Items.Item[Z].Name)].Category = dsPlant) then
      begin
        X := Items.Item[Z].Pos.X + Rand(-1, 1);
        Y := Items.Item[Z].Pos.Y + Rand(-1, 1);
        if (Map.Cell[Y, X].Tile in FloorSet) and (Items.CellItemsCount(X, Y) = 0)
        then
          Items.Add(X, Y, Items.Item[Z].Name);
      end;
  except
    on E: Exception do
      Error.Add('Items.Grow', E.Message);
  end;
end;

function TItems.ItemIndex(ID: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(DungeonItems) do
    if (Trim(ID) = DungeonItems[I].Sprite) then
    begin
      Result := I;
      Break;
    end;
end;

function TItems.ItemIndex(ID: Integer): Integer;
var
  S: string;
begin
  Result := -1;
  S := Creatures.PC.Inv.GetID(ID);
  if (S = '') then
    Exit;
  Result := ItemIndex(S);
end;

procedure TItems.MakeAlchemyDoc;
var
  I: Integer;
  L: TStringList;
begin
  L := TStringList.Create;
  try
    try
      L.Append('<html>');
      L.Append('<head><title>Alchemy</title></head>');
      L.Append('<body><h2 align="center">Alchemy</h2>');
      L.Append('<table align="center" width="90%">');
      for I := 0 to High(CraftItems) do
      begin
        L.Append(Format('<tr><td>%s</td><td>%s</td><td>%s</td></tr>',
          [GetItemLang(CraftItems[I].A), GetItemLang(CraftItems[I].B),
          GetItemLang(CraftItems[I].C)]));
      end;
      L.Append('</table></html>');
    finally
      MakeDir('docs');
      L.SaveToFile('docs\alchemy.html');
      L.Free;
    end;
  except
    on E: Exception do
      Error.Add('Items.MakeCraftDoc', E.Message);
  end;
end;

procedure TItems.MakeCraftDoc;
begin

end;

procedure TItems.Pickup(Index: Integer = 0);
var
  I, H: Integer;
  S: string;

  function IsAddToInv(I: Integer): Boolean;
  begin
    with Items.Item[I] do
      Result := Creatures.PC.Inv.Add(Name, Count, Prop.Weight, Prop.Tough,
        Prop.IsStack);
  end;

  procedure RenderText();
  begin
    Log.Add(Format(GetLang(12), [S]));
    Log.Apply;
    Scenes.Render;
  end;

  procedure Add(I: Integer);
  var
    J: Integer;
  begin
    if not IsAddToInv(I) then
    begin
      Log.Add(GetLang(11));
      Log.Apply;
      Scenes.Render;
      Exit;
    end;
    Graph.Messagebar.Clear;
    S := GetItemLang(Items.Item[I].Prop.Sprite);
    if (Items.Item[I].Count > 1) then
      S := S + ' (' + IntToStr(Items.Item[I].Count) + ')';
    if (Length(Items.Item) > 1) then
    begin
      for J := I to High(Items.Item) - 1 do
        Items.Item[J] := Items.Item[J + 1];
      SetLength(FItem, Length(Items.Item) - 1);
    end
    else
      Self.Clear;
    RenderText();
  end;

begin
  try
    H := 0;
    if (Index > 0) then
    begin
      if (Length(Items.Item) > 0) then
        for I := 0 to High(Items.Item) do
          if (Creatures.PC.Pos.X = Items.Item[I].Pos.X) and
            (Creatures.PC.Pos.Y = Items.Item[I].Pos.Y) then
          begin
            Inc(H);
            if (Index > 0) and (Index = H) then
            begin
              Add(I);
              Exit;
            end;
          end;
    end;
    if (Items.CellItemsCount(Creatures.PC.Pos.X, Creatures.PC.Pos.Y) > 1) then
    begin
      Graph.Messagebar.Clear;
      Scenes.Scene := SceneItems;
    end
    else if (Length(Items.Item) > 0) then
      for I := 0 to High(Items.Item) do
        if (Creatures.PC.Pos.X = Items.Item[I].Pos.X) and
          (Creatures.PC.Pos.Y = Items.Item[I].Pos.Y) then
        begin
          Add(I);
          Exit;
        end;
  except
    on E: Exception do
      Error.Add('Items.Pickup', E.Message);
  end;
end;

procedure TItems.PickupAll;
var
  C, I: Integer;
begin
  try
    C := Items.CellItemsCount(Creatures.PC.Pos.X, Creatures.PC.Pos.Y);
    for I := 0 to C do
      Pickup(I);
  except
    on E: Exception do
      Error.Add('Items.PickupAll', E.Message);
  end;
end;

procedure TItems.Render(X, Y, DX, DY: Integer);
var
  I: Integer;
begin
  try
    with Graph.Surface.Canvas do
      if (Length(Items.Item) > 0) then
      begin
        for I := High(Items.Item) downto 0 do
          if (X = Items.Item[I].Pos.X) and (Y = Items.Item[I].Pos.Y) then
          begin
            if (Map.Cell[Y][X].Tile in [tlOpenWoodChest, tlOpenBestChest,
              tlOpenBarrel]) then
              Continue;
            Draw(DX + (TileSize div 4), DY + (TileSize div 4),
              Items.Item[I].SmallImage);
            // TextOut(DX, DY, IntToStr(X) + IntToStr(Y));
            // LBAR.Assign(LIFEBAR);
            // LBAR.Width := BarWidth(Creatures.Enemy[I].Life, Creatures.Enemy[I].MaxLife);
            // Draw(DX + 1, DY, LBAR);
          end;
      end;
  except
    on E: Exception do
      Error.Add('Items.Render', E.Message);
  end;
end;

procedure TItems.RenderPCInvStat(Y: Integer);
begin
  with Graph.Surface.Canvas do
  begin
    Font.Style := [fsBold];
    Font.Color := cRdYellow;
    with Creatures.PC.Inv do
      Trollhunter.Graph.Graph.Text.TextCenter(Y,
        Format('%s: %d/%d | %s: %d/%ds', [GetLang(41), Count, MaxCount,
        GetLang(42), Weight, MaxWeight]));
  end;
end;

procedure TItems.SetColor(const AColor: Integer);
begin
  with Graph.Surface.Canvas do
    case DungeonItems[AColor].Category of
      dsGold:
        Font.Color := clYellow;
    else
      Font.Color := clSkyBlue;
    end;
end;

procedure TItems.SetItem(const Value: TItem);
begin
  FItem := Value;
end;

function TItems.Use(ItemA, ItemB: string; I: Integer): Boolean;
var
  A, B, C, D, MaxD: Integer;
  ItemC: string;
begin
  Result := False;
  // if (ItemA = ItemB) then Exit; {?}
  with Creatures.PC.Inv do
  begin
    if (GetCount(ItemA) > 0) and (GetCount(ItemB) > 0) then
    begin
      A := ItemIndex(ItemA);
      B := ItemIndex(ItemB);
      C := ItemIndex(ItemC);

      // Repair item (oils, hammers)
      if ((DungeonItems[A].Category in PotionSet) or
        (DungeonItems[A].Category in RepairSet)) and
        ((scRepair in DungeonItems[A].SubCats) or
        (scRepair3 in DungeonItems[A].SubCats) or
        (scRepair6 in DungeonItems[A].SubCats) or
        (scRepair9 in DungeonItems[A].SubCats) or
        (scRepair12 in DungeonItems[A].SubCats) or
        (scRepair15 in DungeonItems[A].SubCats) or
        (scRepair25 in DungeonItems[A].SubCats)) and
        ((DungeonItems[B].Category in WeaponSet) or
        (DungeonItems[B].Category in ArmorSet)) then
      begin
        D := GetTough(I);
        MaxD := DungeonItems[B].MaxTough;
        if (D < MaxD) then
        begin
          if (scRepair in DungeonItems[A].SubCats) then
            SetTough(I, MaxD);
          if (scRepair3 in DungeonItems[A].SubCats) then
            SetTough(I, D + 3);
          if (scRepair6 in DungeonItems[A].SubCats) then
            SetTough(I, D + 6);
          if (scRepair9 in DungeonItems[A].SubCats) then
            SetTough(I, D + 9);
          if (scRepair12 in DungeonItems[A].SubCats) then
            SetTough(I, D + 12);
          if (scRepair15 in DungeonItems[A].SubCats) then
            SetTough(I, D + 15);
          if (scRepair25 in DungeonItems[A].SubCats) then
            SetTough(I, D + 25);
          if (GetTough(I) > MaxD) then
            SetTough(I, MaxD);
          Del(ItemA);
          Result := True;
        end;
        Exit;
      end;

      // Mix potions
      if (DungeonItems[A].Category in UseSet) or
        (DungeonItems[B].Category in PotionSet) then
      begin
        ItemC := Craft(ItemA, ItemB);
        if (ItemC <> '') then
        begin
          Del(ItemA);
          Del(ItemB);
          Add(ItemC, 1, DungeonItems[C].Weight, 0, True);
          Result := True;
        end;
        Exit;
      end;

    end;
  end;
end;

function TItems.GetDollItemID(ACatSet: TCatSet): string;
var
  I: Integer;
begin
  Result := '';
  with Creatures.PC do
    for I := 1 to Inv.Count do
      if Inv.GetDoll(I) and (DungeonItems[Items.ItemIndex(I)].Category
        in ACatSet) then
      begin
        Result := Inv.GetID(I);
        Exit;
      end;
end;

function TItems.IsDollSubCat(SC: TSubCats): Boolean;
var
  K, J: Integer;
begin
  Result := False;
  with Creatures.PC do
  begin
    for K := 1 to Inv.Count do
    begin
      J := ItemIndex(K);
      if Inv.GetDoll(K) then
      begin
        if (SC in DungeonItems[J].SubCats) then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
  end;
end;

function TItems.GetDollItemSubCat(ACatSet: TCatSet): TSubCSet;
begin
  Result := DungeonItems[ItemIndex(GetDollItemID(ACatSet))].SubCats;
end;

function TItems.IsBow: Boolean;
begin
  Result := (scBow in GetDollItemSubCat(WeaponSet)) and
    (scBow in GetDollItemSubCat(ArmorSet))
end;

function TItems.IsCrossBow: Boolean;
begin
  Result := (scCrossBow in GetDollItemSubCat(WeaponSet)) and
    (scCrossBow in GetDollItemSubCat(ArmorSet))
end;

function TItems.IsRangedWeapon: Boolean;
begin
  Result := (GetDollItemID(WeaponSet) <> '') and (GetDollItemID(ArmorSet) <> '')
    and (IsBow or IsCrossBow);
end;

procedure TItems.Identify;
var
  I, ID, Tag: Integer;
begin
  try
    with Creatures.PC do
      for I := 1 to Inv.Count do
      begin
        ID := ItemIndex(I);
        Tag := DungeonItems[ID].ColorTag;
        if (Tag > 0) and (DungeonItems[ID].Category = dsPotion) and
          not Potions.IsDefined(Tag) then
          Potions.SetDefined(Tag);
        if (Tag > 0) and (DungeonItems[ID].Category = dsScroll) and
          not Scrolls.IsDefined(Tag) then
          Scrolls.SetDefined(Tag);
      end;
  except
    on E: Exception do
      Error.Add('Items.Identify', E.Message);
  end;
end;

procedure TItems.RepairAll;
var
  I: 1 .. 26;
  M: Word;
begin
  try
    with Creatures.PC.Inv do
      for I := 1 to Count do
      begin
        M := DungeonItems[ItemIndex(I)].MaxTough;
        if (GetTough(I) < M) then
          SetTough(I, M);
      end;
  except
    on E: Exception do
      Error.Add('Items.RepairAll', E.Message);
  end;
end;

procedure TItems.AddAndEquip(ID: string; ACount: Integer = 1);
begin
  ID := Trim(ID);
  if (ID = '') then
    Exit;
  Add(ID, ACount);
  SceneItem.Equip(Creatures.PC.Inv.Count, False);
end;

function TItems.GetDollText(I, V: Integer): string;
var
  S: string;
begin
  Result := '';
  case DungeonItems[V].Category of
    dsLHand:
      S := GetLang('on left hand', 'в левой руке');
    dsRHand:
      S := GetLang('on right hand', 'в правой руке');
    dsBody:
      S := GetLang('on body', 'на торсе');
    dsHead:
      S := GetLang('on head', 'на голове');
    dsFoot:
      S := GetLang('on feet', 'на ногах');
    dsRing:
      S := GetLang('on finger', 'на пальце');
    dsAmulet:
      S := GetLang('on neck', 'на шее');
  else
    S := '';
  end;
  if Creatures.PC.Inv.GetDoll(I) then
    Result := S;
  if (Result <> '') then
    Result := ' - ' + Result;
end;

initialization

Items := TItems.Create;

finalization

Items.Free;

end.
