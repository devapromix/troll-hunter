unit Trollhunter.Scene.Item.Amount;

interface

uses
  Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneAmount = class(TScene)
  public const
    MinAmount = 1;
  public
    MaxAmount: Int;
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

{ TSceneAmount }

uses
  Math,
  SysUtils,
  BearLibTerminal,
  uBearLibItemsCommon,
  uBearLibItemsDungeon,
  uBearLibItemsInventory,
  Trollhunter.Terminal,
  Trollhunter.Player,
  Trollhunter.Map,
  Trollhunter.UI,
  Trollhunter.Language;

procedure TSceneAmount.Render;
var
  FItem: Item;
begin
  inherited;
  UI.Title(_('Enter amount'));

  if Player.ItemIsDrop then
    FItem := Items_Inventory_GetItem(Player.ItemIndex)
  else
    FItem := Items_Dungeon_GetMapItemXY(Ord(Map.Current), Player.ItemIndex, Player.X, Player.Y);

  MaxAmount := FItem.Amount;

  Terminal.Print(CX, CY, Format('%d/%dx', [Player.ItemAmount, FItem.Amount]), TK_ALIGN_LEFT);

  AddKey('Enter', _('Confirm'));
  AddKey('Esc', _('Back'));
  AddKey('?', _('Help'), True);
end;

procedure TSceneAmount.Update(var Key: UInt);

  procedure ChAmount(Value: Int);
  begin
    Player.ItemAmount := EnsureRange(Value, MinAmount, MaxAmount);
    Render;
  end;

begin
  case Key of
    TK_ESCAPE: // Close
      Scenes.SetScene(scGame);
    TK_ENTER, TK_KP_ENTER:
      begin
        if Player.ItemIsDrop then
          Player.DropAmount(Player.ItemIndex)
        else
          Player.PickUpAmount(Player.ItemIndex);
      end;
    TK_LEFT, TK_KP_4, TK_A:
      ChAmount(MinAmount);
    TK_UP, TK_KP_8, TK_W:
      ChAmount(Player.ItemAmount + 1);
    TK_DOWN, TK_KP_2, TK_X:
      ChAmount(Player.ItemAmount - 1);
    TK_RIGHT, TK_KP_6, TK_D:
      ChAmount(MaxAmount);
    TK_SLASH:
      Scenes.SetScene(scHelp, scAmount);
  end;
end;

end.
