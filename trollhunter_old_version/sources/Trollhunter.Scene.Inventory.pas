unit Trollhunter.Scene.Inventory;

interface

uses
  Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneInv = class(TScene)
  private
    FIsEquipment: Boolean;
  public
    constructor Create;
    procedure Render; override;
    procedure Update(var Key: UInt); override;
    property IsEquipment: Boolean read FIsEquipment write FIsEquipment;
  end;

implementation

{ TSceneInv }

uses
  SysUtils,
  BearLibTerminal,
  Trollhunter.UI,
  Trollhunter.UI.Log,
  Trollhunter.Player,
  Trollhunter.Item,
  Trollhunter.Language,
  Trollhunter.Game,
  Trollhunter.Item.Types;

constructor TSceneInv.Create;
begin
  inherited Create;
  IsEquipment := False;
end;

procedure TSceneInv.Render;
begin
  inherited;
  UI.Title(Format('%s [[%s%d %s%d/%d]]', [_('Inventory'), UI.Icon(icGold), Player.Gold, UI.Icon(icFlag), Items.InvCount, ItemMax]));

  UI.FromAToZ(ItemMax);
  Items.RenderInventory(ptNone, IsEquipment);
  MsgLog.Render(2, True);

  AddKey('Esc', _('Close'));
  AddKey('?', _('Help'), True);

end;

procedure TSceneInv.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE: // Close
      Scenes.SetScene(scGame);
    TK_TAB: // Drop
      Scenes.SetScene(scDrop);
    TK_ENTER, TK_KP_ENTER:
      Scenes.SetScene(scItems);
    TK_1:
      begin
        IsEquipment := not IsEquipment;
        Render;
      end;
    TK_SLASH:
      Scenes.SetScene(scHelp, scInv);
    TK_SPACE: // Player
      Scenes.SetScene(scPlayer);
    TK_BACKSPACE: // Info about item
      Scenes.SetScene(scItemInfo, scInv);
    TK_A .. TK_Z: // Use an item
    begin
      IsEquipment := False;
      Player.Use(Key - TK_A);
    end
  else
    Game.Timer := UIntMax;
  end;
end;

end.
