unit Trollhunter.Scene.Enchant;

interface

uses
  Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneEnchant = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

uses
  BearLibTerminal,
  Trollhunter.UI,
  Trollhunter.Language,
  Trollhunter.Item,
  Trollhunter.UI.Log,
  Trollhunter.Player,
  Trollhunter.Game;

{ TSceneEnchant }

procedure TSceneEnchant.Render;
var
  C: UInt;
begin
  inherited;
  UI.Title(_('Enchant an item'), 1, clDarkestRed);

  UI.FromAToZ();
  C := Items.RenderInventory();
  MsgLog.Render(2, True);

  AddKey(C, _('Select an item'));
  AddKey('Esc', _('Close'), True);
end;

procedure TSceneEnchant.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      Scenes.SetScene(scInv);
    TK_A .. TK_Z:
      Player.CraftItem(Key - TK_A);
  else
    Game.Timer := UIntMax;
  end
end;

end.
