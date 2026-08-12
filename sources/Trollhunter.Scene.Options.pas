unit Trollhunter.Scene.Options;

interface

uses
  Trollhunter.Scenes,
  Trollhunter.Types;

type
  TSceneOptions = class(TScene)
  private
    procedure RenderOptions;
    procedure UpdateOptions(const Key: UInt);
    procedure RenderWizard;
    procedure UpdateWizard(const Key: UInt);
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

uses
  Trollhunter.Game,
  Trollhunter.UI,
  BearLibTerminal, Trollhunter.Item.Shop;

var
  Wizard: boolean = False;

{ TSceneOptions }

procedure TSceneOptions.RenderOptions;
begin
  // Options
  Title('Options');

  AddOption('C', 'Auto pick up coins', Game.GetOption(apCoin));
  AddOption('G', 'Auto pick up gems', Game.GetOption(apGem));
  AddOption('F', 'Auto pick up food', Game.GetOption(apFood));
  AddOption('Y', 'Auto pick up plants', Game.GetOption(apPlant));
  AddOption('P', 'Auto pick up potions and flasks',
    Game.GetOption(apPotion));
  AddOption('U', 'Auto pick up flasks', Game.GetOption(apFlask));
  AddOption('O', 'Auto pick up magic items', Game.GetOption(apMagic));
  AddOption('S', 'Auto pick up scrolls', Game.GetOption(apScroll));
  AddOption('R', 'Auto pick up runes', Game.GetOption(apRune));
  AddOption('B', 'Auto pick up books', Game.GetOption(apBook));
  AddOption('K', 'Auto pick up keys', Game.GetOption(apKey));
  AddOption('A', 'Auto pick up arrows', Game.GetOption(apArrow));
  AddOption('D', 'Show items price in inventory', Game.GetOption(apShPrice));

  // Settings
  Title('Settings', False);
  AddOption('W', 'Fullscreen', Game.GetOption(apFullscreen), clLightBlue);
end;

procedure TSceneOptions.UpdateOptions(const Key: UInt);
begin
  case Key of
    // Options
    TK_C:
      Game.ChOption(apCoin);
    TK_G:
      Game.ChOption(apGem);
    TK_F:
      Game.ChOption(apFood);
    TK_Y:
      Game.ChOption(apPlant);
    TK_P:
      Game.ChOption(apPotion);
    TK_O:
      Game.ChOption(apMagic);
    TK_U:
      Game.ChOption(apFlask);
    TK_S:
      Game.ChOption(apScroll);
    TK_R:
      Game.ChOption(apRune);
    TK_K:
      Game.ChOption(apKey);
    TK_A:
      Game.ChOption(apArrow);
    TK_B:
      Game.ChOption(apBook);
    TK_D:
      Game.ChOption(apShPrice);
    // Settings
    TK_W:
      begin
        Game.ChOption(apFullscreen);
        Game.ChScreen;
      end;
  end;
end;

procedure TSceneOptions.RenderWizard;
begin
  Title('Wizard Mode');

  AddOption('X', 'Turn Wizard Mode Off', Mode.Wizard, clRed);
  AddOption('M', 'Show map', Game.ShowMap);
  AddOption('T', 'Reload all shops', False);
  AddOption('L', 'Leave corpses', Game.LCorpses);
  AddOption('I', 'Show ID of items', Game.ShowID);
  AddOption('N', 'Hide level of an item', Game.GetOption(apHdLevOfItem));
end;

procedure TSceneOptions.UpdateWizard(const Key: UInt);
begin
  case Key of
    TK_X:
      Mode.Wizard := False;
    TK_M:
      Game.ShowMap := not Game.ShowMap;
    TK_L:
      Game.LCorpses := not Game.LCorpses;
    TK_T:
      begin
        Shops.New;
        Scenes.SetScene(scGame);
      end;
    TK_I:
      Game.ShowID := not Game.ShowID;
    TK_N:
      Game.ChOption(apHdLevOfItem);
  end;
end;

procedure TSceneOptions.Render;
begin
  Y := 1;
  if Mode.Wizard and Wizard then
    RenderWizard
  else
    RenderOptions;

  AddKey('Esc', 'Back', not Mode.Wizard);
  if Mode.Wizard then
    if Wizard then
      AddKey('Z', 'Back', True)
    else
      AddKey('Z', 'Wizard Mode', True);
end;

procedure TSceneOptions.Update(var Key: UInt);
begin
  case Key of
    TK_Z:
      if Mode.Wizard then
        Wizard := not Wizard;
    TK_ESCAPE:
      Scenes.SetScene(scGame);
  else
    if Mode.Wizard and Wizard then
      UpdateWizard(Key)
    else
      UpdateOptions(Key);
  end
end;

end.
