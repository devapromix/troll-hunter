unit Trollhunter.Scene.Enchant;

interface

uses
  Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneEnchant = class(TScene)
  public
    Suffix: UInt;
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

uses
  Trollhunter.UI,
  uItem,
  Trollhunter.UI.Log,
  Trollhunter.Terminal,
  BearLibTerminal,
  Trollhunter.Player,
  Trollhunter.Game;

  { TSceneEnchant }

procedure TSceneEnchant.Render;
begin
  UI.Title('Enchant an item', 1, clDarkestRed);

  UI.FromAToZ();
  Items.RenderInventory();
  MsgLog.Render(2, True);

  AddKey('A-Z', 'Select an item');
  AddKey('Esc', 'Close', True);
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
  end;
end;

end.
