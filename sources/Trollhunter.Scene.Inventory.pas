unit Trollhunter.Scene.Inventory;

interface

uses
  Trollhunter.Scenes,
  Trollhunter.Types;
  
type
  TSceneInv = class(TScene)
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
  Trollhunter.Item,
  Trollhunter.Item.Inventory,
  Trollhunter.Game;
  
{ TSceneInv }

procedure TSceneInv.Render;
begin
  UI.Title(Format('%s [[%s%d %s%d/%d]]', ['Inventory', UI.Icon(icGold),
    Player.Gold, UI.Icon(icSack), Items_Inventory_GetCount(), ItemMax]));

  UI.FromAToZ(ItemMax);
  Items.RenderInventory;
  MsgLog.Render(2, True);

  AddKey('Esc', 'Close');
  AddKey('?', 'Help', True);

end;

procedure TSceneInv.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      Scenes.SetScene(scGame);
    TK_TAB:
    begin
      if Player.IsDead then
        Exit;
      if Player.IsOnStash then
        Scenes.SetScene(scStore, scInv)
      else
        Scenes.SetScene(scDrop, scInv);
    end;
    TK_SLASH:
      Scenes.SetScene(scHelp, scInv);
    TK_SPACE:
      if Player.IsOnStash then
        Scenes.SetScene(scStash)
      else
        Scenes.SetScene(scPlayer);
    TK_A .. TK_Z:
      Player.UseItem(Key - TK_A);
    else
      Game.Timer := UIntMax;
  end;
end;

end.
