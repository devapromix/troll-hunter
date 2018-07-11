unit Trollhunter.Scene.Inventory;

interface

uses Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneInv = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

{ TSceneInv }

uses SysUtils,
  BearLibTerminal,
  Trollhunter.UI,
  Trollhunter.UI.Log,
  Trollhunter.Player,
  Trollhunter.Item,
  Trollhunter.Language,
  Trollhunter.Game;

procedure TSceneInv.Render;
begin
  UI.Title(Format('%s [[%s%d %s%d/%d]]', [_('Inventory'), UI.Icon(icGold),
    Player.Gold, UI.Icon(icFlag), Items.InvCount, ItemMax]));

  UI.FromAToZ(ItemMax);
  Items.RenderInventory;
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
      Scenes.SetScene(scDrop, scInv);
    TK_ENTER, TK_KP_ENTER:
      ;
    TK_SLASH:
      Scenes.SetScene(scHelp, scInv);
    TK_SPACE: // Player
      Scenes.SetScene(scPlayer);
    TK_BACKSPACE: // Info about item
      Scenes.SetScene(scItemInfo, scInv);
    TK_A .. TK_Z: // Use an item
      Player.Use(Key - TK_A);
  else
    Game.Timer := UIntMax;
  end;
end;

end.
