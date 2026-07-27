unit Trollhunter.Scene.Help;

interface

uses
  Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneHelp = class(TScene)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

{ TSceneHelp }

uses
  SysUtils,
  Trollhunter.UI,
  BearLibTerminal,
  Trollhunter.Terminal;

constructor TSceneHelp.Create;
begin
  //
end;

destructor TSceneHelp.Destroy;
begin
  inherited;
end;

procedure TSceneHelp.Render;
begin
  UI.Title('Help');

  case Scenes.PrevSceneEnum of
    scClass:
    begin
      UI.Title('Keybindings', 5);
      X := 1;
      Y := 7;
      AddLine('Space',     'Re-roll');
      AddLine('Backspace', 'Random');
      AddLine('A-Z',       'Select a class');
    end;

    scRace:
    begin
      UI.Title('Keybindings', 5);
      X := 1;
      Y := 7;
      AddLine('Tab',       'Choose a sex');
      AddLine('Space',     'Re-roll');
      AddLine('Backspace', 'Random');
      AddLine('A-Z',       'Select a race');
    end;

    scInv:
    begin
      Terminal.Print(CX, 3, 'This screen displays all items currently carried by your character.', TK_ALIGN_CENTER);
      Terminal.Print(CX, 4, 'Equippable items are automatically placed into the appropriate equipment slot when used.', TK_ALIGN_CENTER);
      Terminal.Print(CX, 5, 'If the slot is already occupied, the currently equipped item will be replaced.', TK_ALIGN_CENTER);
      Terminal.Print(CX, 6, 'Your inventory has limited capacity. If it is full, you cannot pick up additional', TK_ALIGN_CENTER);
      Terminal.Print(CX, 7, 'items until at least one slot becomes available.', TK_ALIGN_CENTER);

      UI.Title('Stash', 9);
      Terminal.Print(CX, 11, 'While standing on a Stash tile, you can transfer items between', TK_ALIGN_CENTER);
      Terminal.Print(CX, 12, 'your inventory and your personal stash.', TK_ALIGN_CENTER);
      Terminal.Print(CX, 13, Format('To drop/store an item, press the %s key and then press %s key to drop/store it.',
        [UI.KeyToStr('TAB'), UI.KeyToStr('A-Z')]), TK_ALIGN_CENTER);

      UI.Title('Keybindings', 15);
      X := 1;
      Y := 17;
      AddLine('Tab',   'Drop/Store an item');
      AddLine('Space', 'Character/Stash');
      AddLine('A-Z',   'Use an item');
    end;

    scPlayer:
    begin
      Terminal.Print(CX, 3, 'The Player screen provides a detailed overview of your character. Here you can inspect', TK_ALIGN_CENTER);
      Terminal.Print(CX, 4, 'your attributes, combat statistics, skills, talents, resistances, and other important', TK_ALIGN_CENTER);
      Terminal.Print(CX, 5, 'information that affects your performance throughout the game.', TK_ALIGN_CENTER);

      UI.Title('Skills', 7);
      Terminal.Print(CX, 9, 'Skills represent your character''s proficiency in various disciplines,', TK_ALIGN_CENTER);
      Terminal.Print(CX, 10, 'such as weapon mastery, magic, survival, and exploration.', TK_ALIGN_CENTER);
      Terminal.Print(CX, 11, 'Many skills improve automatically as your character advances,', TK_ALIGN_CENTER);
      Terminal.Print(CX, 12, 'while others are enhanced through talents or equipment.', TK_ALIGN_CENTER);
      Terminal.Print(CX, 13, 'Higher skill levels provide greater bonuses and unlock your character''s full potential.', TK_ALIGN_CENTER);

      UI.Title('Talents', 15);
      Terminal.Print(CX, 17, 'Talents are permanent upgrades chosen as your character levels up.', TK_ALIGN_CENTER);
      Terminal.Print(CX, 18, 'Each talent grants a unique bonus, such as increasing attributes, improving combat', TK_ALIGN_CENTER);
      Terminal.Print(CX, 19, 'performance, strengthening magic, or enhancing survival abilities.', TK_ALIGN_CENTER);
      Terminal.Print(CX, 20, 'Choose talents carefully, as they define your character''s long-term development.', TK_ALIGN_CENTER);

      UI.Title('Keybindings', 22);
      X := 1;
      Y := 24;
      AddLine('Right/Left', 'Change tab');
      AddLine('Up/Down',    'Scroll skills');
      AddLine('T',          'Show Learned Talents');
      AddLine('Space',      'Show Inventory');
    end;

    scGame:
    begin
      Terminal.Print(CX, 3, 'Far away in an uncharted region of the Earth land Elvion lies surrounded by mountains.', TK_ALIGN_CENTER);
      Terminal.Print(CX, 4, 'In the center of this land there is a village named Dork. Its people are in', TK_ALIGN_CENTER);
      Terminal.Print(CX, 5, 'grave danger as the Troll King and his armies are marching to lay waste on all of', TK_ALIGN_CENTER);
      Terminal.Print(CX, 6, 'its inhabitants. Unless a hero will rise to take a stand against the forces of evil.', TK_ALIGN_CENTER);

      Terminal.Print(CX, 8, 'You are the hero who departs on a quest to stop the enemies and save your homeland,', TK_ALIGN_CENTER);
      Terminal.Print(CX, 9, 'Elvion. Survive, gather equipment, fight adversaries and be ready for the final', TK_ALIGN_CENTER);
      Terminal.Print(CX, 10, 'confrontation. Good luck! You will need it.', TK_ALIGN_CENTER);

      UI.Title('Keybindings', 12);

      Terminal.Print(CX, 14, Format('%s: %s, %s   %s: %s   %s: %s',
        ['Move', UI.KeyToStr('arrow keys'), UI.KeyToStr('numpad'),
         'Wait', UI.KeyToStr('5'),
         'Effects', UI.KeyToStr('TAB')]), TK_ALIGN_CENTER);

      X := 1;
      Y := 16;
      AddLine('<', 'Go up stairs');
      AddLine('>', 'Go down stairs');
      AddLine('G', 'Pick up / Open stash');
      AddLine('F', 'Drop / Store an item');
      AddLine('L', 'Look mode');
      AddLine('R', 'Rest');
      AddLine('M', 'View messages');
      AddLine('B', 'Spellbook');
      AddLine('Y', 'Cast Quick Spell');
      AddLine('T', 'Talents');
      AddLine('N', 'Show Statistics');
      AddLine('O', 'Options');
      AddLine('I', 'Show Inventory');
      AddLine('P', 'Character Screen');
      AddLine('K', 'Calendar');
      AddLine('?', 'Show this Help Screen');
    end;
  end;

  Self.AddKey('Esc', 'Close', True);
end;

procedure TSceneHelp.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      Scenes.GoBack;
  end;
end;

end.
