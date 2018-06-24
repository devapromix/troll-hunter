unit Trollhunter.Scene.Enchant;

interface

uses uTypes, uScenes;

type
  TSceneEnchant = class(TScene)
  public
    Suffix: UInt;
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

uses Trollhunter.UI, uLanguage, uItem, Trollhunter.UI.Log, uTerminal, BearLibTerminal,
  uPlayer, uGame;

{ TSceneEnchant }

procedure TSceneEnchant.Render;
begin
  UI.Title(_('Enchant an item'), 1, clDarkestRed);

  UI.FromAToZ();
  Items.RenderInventory();
  MsgLog.Render(2, True);

  AddKey('A-Z', _('Select an item'));
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
