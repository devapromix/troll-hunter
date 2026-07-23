unit Trollhunter.Scene.Recharge;

interface

uses
  Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneRecharge = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

uses
  BearLibTerminal,
  Trollhunter.UI,
  Trollhunter.Item,
  Trollhunter.UI.Log,
  Trollhunter.Terminal,
  Trollhunter.Player,
  Trollhunter.Game;

  { TSceneRecharge }

procedure TSceneRecharge.Render;
begin
  UI.Title('Recharge a wand', 1, clDarkestRed);

  UI.FromAToZ();
  Items.RenderInventory();
  MsgLog.Render(2, True);

  AddKey('A-Z', 'Select a wand');
  AddKey('Esc', 'Close', True);
end;

procedure TSceneRecharge.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      Scenes.SetScene(scInv);
    TK_A .. TK_Z:
      Player.RechargeWand(Key - TK_A);
    else
      Game.Timer := UIntMax;
  end;
end;

end.