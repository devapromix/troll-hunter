unit Trollhunter.Scene.Item.Repair;

interface

uses
  Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneRepair = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

{ TSceneRepair }

uses
  Math,
  SysUtils,
  BearLibTerminal,
  Trollhunter.Game,
  Trollhunter.Player,
  Trollhunter.Language,
  Trollhunter.UI,
  Trollhunter.UI.Log,
  Trollhunter.Item,
  Trollhunter.Item.Types;

procedure TSceneRepair.Render;
var
  C: UInt;
begin
  inherited;
  UI.Title(_('Repair items') + ' ' + UI.GoldLeft(Player.Gold), 1, clDarkestRed);

  UI.FromAToZ;
  C := Items.RenderInventory(ptRepair);
  MsgLog.Render(2, True);

  AddKey(C, _('Repair an item'));
  AddKey('Esc', _('Close'), True);
end;

procedure TSceneRepair.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      // Close
      Scenes.GoBack();
    TK_A .. TK_Z:
      Player.RepairItem(Key - TK_A);
  else
    Game.Timer := UIntMax;
  end;
end;

end.
