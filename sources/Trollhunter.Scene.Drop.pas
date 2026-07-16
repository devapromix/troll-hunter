unit Trollhunter.Scene.Drop;

interface

uses
  Trollhunter.Scenes,
  Trollhunter.Types;

type
  TSceneDrop = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

{ TSceneDrop }

uses
  BearLibTerminal,
  Trollhunter.UI,
  Trollhunter.UI.Log,
  Trollhunter.Player,
  Trollhunter.Item,
  Trollhunter.Game;

procedure TSceneDrop.Render;
begin
  if Player.IsOnStash then
    UI.Title('Choose the item you wish to store', 1, clDarkestGreen)
  else
    UI.Title('Choose the item you wish to drop', 1, clDarkestRed);

  UI.FromAToZ;
  Items.RenderInventory;
  MsgLog.Render(2, True);

  if Player.IsOnStash then
  begin
    AddKey('A-Z', 'Store an item');
    AddKey('Esc', 'Close', True);
  end
  else
  begin
    AddKey('A-Z', 'Drop an item');
    AddKey('Esc', 'Close', True);
  end;
end;

procedure TSceneDrop.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      // Close
      Scenes.GoBack;
    TK_A .. TK_Z:
      Player.Drop(Key - TK_A);
    else
      Game.Timer := UIntMax;
  end;
end;

end.
