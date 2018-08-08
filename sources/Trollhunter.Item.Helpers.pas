unit Trollhunter.Item.Helpers;

interface

uses
  Trollhunter.Item,
  Trollhunter.Types,
  Trollhunter.Player.Types;

type
  TItemHelper = class helper for TItems
    function GetItemSlotName(const SlotType: TSlotType): string;
    function GetItemPrice(Price: UInt; F: Boolean = False): string;
    function GetItemLevel(L: UInt): string;
  end;

implementation

uses
  SysUtils,
  Trollhunter.Terminal,
  Trollhunter.Language,
  Trollhunter.UI,
  Trollhunter.Player,
  Trollhunter.Helpers,
  Trollhunter.Attribute;

{ TItemHelper }

function TItemHelper.GetItemLevel(L: UInt): string;
var
  Color: string;
begin
  if (L > Player.Attributes.Attrib[atLev].Value) then
    Color := 'Light Red'
  else
    Color := 'Gray';
  Result := Terminal.Colorize(Format('%s%d', [UI.Icon(icElixir), L]), Color);
end;

function TItemHelper.GetItemPrice(Price: UInt; F: Boolean): string;
var
  Color: string;
begin
  if (F or (Player.Gold >= Price)) then
    Color := 'lighter yellow'
  else
    Color := 'light red';
  Result := Terminal.Colorize(UI.Icon(icGold) + Price.ToString, Color);
end;

function TItemHelper.GetItemSlotName(const SlotType: TSlotType): string;
const
  SlotName: array [TSlotType] of string = ('', 'Head', 'Torso', 'Hands', 'Feet',
    'Main Hand', 'Off-Hand', 'Neck', 'Finger', 'In Hands');
begin
  Result := Terminal.Colorize(Format('{%s}', [_(SlotName[SlotType])]),
    Terminal.GetColorFromIni('Equip'));
end;

end.
