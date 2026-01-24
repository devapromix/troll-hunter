unit Trollhunter.Scene.Options;

interface

uses
  uScenes,
  Trollhunter.Types;

type
  TSceneOptions = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

{ TSceneOptions }

uses
  BearLibTerminal,
  Trollhunter.Game,
  Trollhunter.UI,
  Trollhunter.Item.Shop;

procedure TSceneOptions.Render;
begin
  Y := 1;
  // Options
  Title('Options');

  AddOption('C', 'Auto pick up coins', Game.GetOption(apCoin));
  AddOption('G', 'Auto pick up gems', Game.GetOption(apGem));
  AddOption('F', 'Auto pick up food', Game.GetOption(apFood));
  AddOption('Y', 'Auto pick up plants', Game.GetOption(apPlant));
  AddOption('P', 'Auto pick up potions and flasks', Game.GetOption(apPotion));
  AddOption('U', 'Auto pick up flasks', Game.GetOption(apFlask));
  AddOption('O', 'Auto pick up magic items', Game.GetOption(apMagic));
  AddOption('S', 'Auto pick up scrolls', Game.GetOption(apScroll));
  AddOption('R', 'Auto pick up runes', Game.GetOption(apRune));
  AddOption('B', 'Auto pick up books', Game.GetOption(apBook));
  AddOption('K', 'Auto pick up keys', Game.GetOption(apKey));
  AddOption('D', 'Show items price in inventory', Game.GetOption(apShPrice));

  // Settings
  Title('Settings', False);
  AddOption('W', 'Fullscreen', Game.GetOption(apFullscreen), clLightBlue);

  // Wizard mode
  if Mode.Wizard then
  begin
    Title('Wizard Mode', False);

    AddOption('Z', 'Turn Wizard Mode Off', Mode.Wizard, clRed);
    AddOption('M', 'Show map', Game.ShowMap);
    AddOption('T', 'Reload all shops', False);
    // AddOption('J', _(''), False);
    AddOption('L', 'Leave corpses', Game.LCorpses);
    AddOption('I', 'Show ID of items', Game.ShowID);
    AddOption('N', 'Hide level of an item', Game.GetOption(apHdLevOfItem));
  end;

  AddKey('Esc', 'Back', True);
end;

procedure TSceneOptions.Update(var Key: UInt);
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
    // Wizard mode
    TK_Z:
      Mode.Wizard := False;
    TK_M:
      if Mode.Wizard then
        Game.ShowMap := not Game.ShowMap;
    TK_L:
      if Mode.Wizard then
        Game.LCorpses := not Game.LCorpses;
    TK_T:
      if Mode.Wizard then
      begin
        Shops.New;
        Scenes.SetScene(scGame);
      end;
    TK_I:
      if Mode.Wizard then
        Game.ShowID := not Game.ShowID;
    TK_N:
      if Mode.Wizard then
        Game.ChOption(apHdLevOfItem);
    TK_ESCAPE:
      Scenes.SetScene(scGame);
  end
end;

end.
