unit Trollhunter.Scene.Sell;

interface

uses
  Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneSell = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

uses
  SysUtils,
  BearLibTerminal,
  Trollhunter.UI,
  Trollhunter.UI.Log,
  Trollhunter.Player,
  Trollhunter.Game,
  Trollhunter.Language,
  Trollhunter.Item.Shop,
  Trollhunter.Item,
  Trollhunter.Item.Types;

{ TSceneSell }

procedure TSceneSell.Render;
var
  C: UInt;
begin
  UI.Title(_('Selling items') + ' ' + UI.GoldLeft(Player.Gold));

  UI.FromAToZ;
  C := Items.RenderInventory(ptSell);
  MsgLog.Render(2, True);

  AddKey(C, _('Selling an item'));
  AddKey('Esc', _('Close'), True);
end;

procedure TSceneSell.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      // Close
      Scenes.SetScene(scDialog);
    TK_A .. TK_Z: // Selling an item
      Player.Sell(Key - TK_A);
  else
    Game.Timer := UIntMax;
  end;
end;

end.
