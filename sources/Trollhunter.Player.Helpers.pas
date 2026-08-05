unit Trollhunter.Player.Helpers;

interface

uses
  Trollhunter.Types,
  Trollhunter.Player;

type

  { TPlayerHelper }

  TPlayerHelper = class helper for TPlayer
    function FullName: string;
    function GenderStr: string;
    function GenderIcon: string;
    function Satiation: string;
    function StartGold: UInt;
    function HasSpellbook: boolean;
    function HasQuiver: boolean;
    function IsQuiverBroken: boolean;
    function HasArrows: boolean;
    function HasCharges: boolean;
    function GetArrowsToBuy: Int;
    procedure UseArrow;
    procedure StartSkills;
    procedure StartItems;
  end;

implementation

uses
  Math,
  SysUtils,
  Trollhunter.Game,
  Trollhunter.Terminal,
  Trollhunter.Attribute,
  Trollhunter.Item,
  Trollhunter.Item.Types,
  Trollhunter.Item.Common,
  Trollhunter.Item.Inventory,
  Trollhunter.UI,
  Trollhunter.UI.Log,
  Trollhunter.Player.Types,
  Trollhunter.Player.Races,
  Trollhunter.Player.Classes;

  { TPlayerHelper }

function TPlayerHelper.FullName: string;
begin
  Result := Format('%s, %s %s, %s', [Player.Name, Races.GetName(Player.HRace),
    Player.GenderIcon, Trollhunter.Player.Classes.Classes.GetName(Player.HClass)]);
end;

function TPlayerHelper.GenderStr: string;
begin
  if (Player.Gender = gdMale) then
    Result := 'Male ' + UI.Icon(icMale)
  else
    Result := 'Female ' + UI.Icon(icFemale);
end;

function TPlayerHelper.GenderIcon: string;
begin
  if (Player.Gender = gdMale) then
    Result := UI.Icon(icMale)
  else
    Result := UI.Icon(icFemale);
end;

function TPlayerHelper.Satiation: string;
begin
  case Attributes.Attrib[atSat].Value of
    0 .. StarvingMax:
      Result := 'Starving';
    StarvingMax + 1 .. 1500:
      Result := 'Near starving';
    1501 .. 2000:
      Result := 'Very hungry';
    2001 .. 2500:
      Result := 'Hungry';
    SatiatedMax + 1 .. 10000:
      Result := 'Full';
    10001 .. 11000:
      Result := 'Very full';
    11001 .. EngorgedMax:
      Result := 'Engorged';
    else
      Result := '';
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

function TPlayerHelper.StartGold: UInt;
const
  CGold: array [TDifficulty] of UInt = (400, 250, 100, 50);
begin
  Result := CGold[Game.Difficulty];
end;

function TPlayerHelper.HasSpellbook: boolean;
begin
  Result := Self.GetEquippedIndex(stSpellbook) >= 0;
end;

function TPlayerHelper.HasQuiver: boolean;
begin
  Result := (Self.GetQuiverIndex >= 0);
end;

function TPlayerHelper.IsQuiverBroken: boolean;
var
  QIndex: Int;
begin
  QIndex := Self.GetQuiverIndex;
  Result := (QIndex >= 0) and (Items_Inventory_GetItem(QIndex).Durability = 0);
end;

function TPlayerHelper.HasArrows: boolean;
var
  QIndex: Int;
  FItem: Item;
begin
  QIndex := Self.GetQuiverIndex;
  if (QIndex < 0) then
    Exit(False);
  FItem := Items_Inventory_GetItem(QIndex);
  Result := (FItem.Durability > 0) and (FItem.Value > 0);
end;

function TPlayerHelper.HasCharges: boolean;
var
  WIndex: Int;
begin
  WIndex := Self.GetEquippedIndex(stRanged);
  Result := (WIndex >= 0) and (Items_Inventory_GetItem(WIndex).Value > 0);
end;

function TPlayerHelper.GetArrowsToBuy: Int;
var
  QIndex: Int;
  FItem: Item;
begin
  Result := 0;
  QIndex := Self.GetQuiverIndex;
  if (QIndex < 0) then
    Exit;
  FItem := Items_Inventory_GetItem(QIndex);
  if (FItem.Durability = 0) then
    Exit;
  Result := Int(ItemBase[TItemEnum(FItem.ItemID)].Value) +
    Int(Items.GetBonus(FItem, btQuiverCap)) - Int(FItem.Value);
end;

procedure TPlayerHelper.UseArrow;
var
  QIndex: Int;
  FItem: Item;
begin
  QIndex := Self.GetQuiverIndex;
  if (QIndex < 0) then
    Exit;
  FItem := Items_Inventory_GetItem(QIndex);
  if (FItem.Durability = 0) or (FItem.Value = 0) then
    Exit;
  FItem.Value := Game.EnsureRange(FItem.Value - 1, UIntMax);
  Items_Inventory_SetItem(QIndex, FItem);
  if (FItem.Value <= 25) then
    MsgLog.Add(Terminal.Colorize(
      Format('You are running out of arrows (%d left in your quiver).',
      [FItem.Value]), clAlarm));
end;

procedure TPlayerHelper.StartSkills;
var
  I: TClassSkillEnum;
begin
  // Skills
  for I := Low(TClassSkillEnum) to High(TClassSkillEnum) do
    Skills.Modify(ClassProp[Player.HClass].Skill[I],
      Trollhunter.Player.Classes.Classes.GetSkillBeginValue(I));
  // Calc
  Calc();
  Fill();
end;

procedure TPlayerHelper.StartItems;
var
  J: TSlotType;
  I: integer;
begin
  // Equipment
  for J := Low(ClassProp[HClass].EquipItem) to High(ClassProp[HClass].EquipItem) do
    if (ClassProp[HClass].EquipItem[J] <> TItemEnum.itmNone) then
      Items.AddItemToInv(ClassProp[HClass].EquipItem[J], 1, True, True);
  // Add class items
  for I := 0 to Length(ClassProp[HClass].ClassItem) - 1 do
    if ClassProp[HClass].ClassItem[I] <> itmNone then
      Items.AddItemToInv(ClassProp[HClass].ClassItem[I]);
  // Add foods
  Items.AddItemToInv(itmBread_Ration, IfWizard(9, 3));
  Items.AddItemToInv(itmTorch, IfWizard(3, 1));
  // Add coins
  Items.AddItemToInv(itmGold, IfWizard(RandomRange(3333, 9999),
    StartGold));
  // Calc
  Calc();
  Fill();
  // Wizard
  if Mode.Wizard then
  begin
    Items.AddItemToInv(itmNature_Book_of_Verdant_Spear, 1);
  end;
end;

end.
