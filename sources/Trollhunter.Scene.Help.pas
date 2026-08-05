unit Trollhunter.Scene.Help;

interface

uses
  Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneHelp = class(TScene)
  private
    procedure AddLine(const AText: string); overload;
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
  Trollhunter.Game,
  Trollhunter.Terminal;

constructor TSceneHelp.Create;
begin
  //
end;

destructor TSceneHelp.Destroy;
begin
  inherited;
end;

procedure TSceneHelp.AddLine(const AText: string);
begin
  Terminal.ForegroundColor(clLightGray);
  Terminal.Print(5, Y, AText);
  Inc(Y);
end;

procedure TSceneHelp.Render;
begin
  UI.Title('Help');

  case Scenes.PrevSceneEnum of
    scClass:
    begin
      Y := 3;
      AddLine('The Class Selection screen determines your starting equipment, skills, abilities');
      AddLine('and preferred combat style. Each class offers a distinct approach to the challenges');
      AddLine('of Elvion — some excel in melee combat, others in magic, ranged attacks or support roles.');
      AddLine('');
      AddLine('As you gain levels you will unlock talents and improve your skills, allowing you to');
      AddLine('develop a unique build that suits your playstyle. Choose carefully — your class shapes');
      AddLine('the early and mid game significantly.');

      UI.Title('Keybindings', 12);
      X := 1;
      Y := 14;
      AddLine('Space',     'Re-roll');
      inherited AddLine('Backspace', 'Random');
      inherited AddLine('A-Z',       'Select a class');
    end;

    scRace:
    begin
      Y := 3;
      AddLine('The Race Selection screen lets you choose your character''s race. Each race possesses');
      AddLine('unique starting attributes, physical traits, strengths, weaknesses and available talents');
      AddLine('that influence how your hero develops throughout the entire adventure.');
      AddLine('');
      AddLine('Your racial choice also affects base statistics such as age, height, weight and metabolism.');
      AddLine('Once the game begins this decision is permanent and cannot be changed, so pick the race');
      AddLine('that best matches the kind of hero you want to play.');

      UI.Title('Keybindings', 13);
      X := 1;
      Y := 15;
      inherited AddLine('Tab',       'Choose a sex');
      inherited AddLine('Space',     'Re-roll');
      inherited AddLine('Backspace', 'Random');
      inherited AddLine('A-Z',       'Select a race');
    end;

    scInv:
    begin
      Y := 3;
      AddLine('This screen displays all items currently carried by your character. Equippable items are');
      AddLine('automatically placed into the appropriate equipment slot when used. If the slot is already');
      AddLine('occupied, the currently equipped item will be replaced. Your inventory has limited capacity.');
      AddLine('If it is full, you cannot pick up additional items until at least one slot becomes available.');

      UI.Title('Stash', 9);
      Y := 11;
      AddLine('While standing on a Stash tile, you can transfer items between your inventory and your');
      AddLine('personal stash.');
      AddLine(Format('To drop/store an item, press the %s key and then press %s key to drop/store it.',
        [UI.KeyToStr('TAB'), UI.KeyToStr('A-Z')]));

      UI.Title('Keybindings', 15);
      X := 1;
      Y := 17;
      inherited AddLine('Tab',   'Drop/Store an item');
      inherited AddLine('Space', 'Character/Stash');
      inherited AddLine('A-Z',   'Use an item');
    end;

    scPlayer:
    begin
      Y := 3;
      AddLine('The Player screen provides a detailed overview of your character. Here you can inspect');
      AddLine('your attributes, combat statistics, skills, talents, resistances, and other important');
      AddLine('information that affects your performance throughout the game.');

      UI.Title('Skills', 7);
      Y := 9;
      AddLine('Skills represent your character''s proficiency in various disciplines, such as weapon');
      AddLine('mastery, magic, survival, and exploration. Many skills improve automatically as your');
      AddLine('character advances, while others are enhanced through talents or equipment. Higher skill');
      AddLine('levels provide greater bonuses and unlock your character''s full potential.');

      UI.Title('Talents', 15);
      Y := 17;
      AddLine('Talents are permanent upgrades chosen as your character levels up. Each talent grants a');
      AddLine('unique bonus, such as increasing attributes, improving combat performance, strengthening');
      AddLine('magic, or enhancing survival abilities. Choose talents carefully, as they define your');
      AddLine('character''s long-term development.');

      UI.Title('Keybindings', 22);
      X := 1;
      Y := 24;
      inherited AddLine('Right/Left', 'Change tab');
      inherited AddLine('Up/Down',    'Scroll skills');
      inherited AddLine('T',          'Show Learned Talents');
      inherited AddLine('Space',      'Show Inventory');
    end;

    scGame:
    begin
      Y := 3;
      AddLine('Far away in an uncharted region of the Earth land Elvion lies surrounded by mountains.');
      AddLine('In the center of this land there is a village named Dork. Its people are in grave danger');
      AddLine('as the Troll King and his armies are marching to lay waste on all of its inhabitants.');
      AddLine('Unless a hero will rise to take a stand against the forces of evil.');
      AddLine('');
      AddLine('You are the hero who departs on a quest to stop the enemies and save your homeland, Elvion.');
      AddLine('Survive, gather equipment, fight adversaries and be ready for the final confrontation.');
      AddLine('Good luck! You will need it.');

      UI.Title('Keybindings', 12);

      Terminal.Print(CX, 14, Format('%s: %s, %s   %s: %s   %s: %s',
        ['Move', UI.KeyToStr('arrow keys'), UI.KeyToStr('numpad'),
         'Wait', UI.KeyToStr('5'),
         'Effects', UI.KeyToStr('TAB')]), TK_ALIGN_CENTER);

      X := 1;
      Y := 16;
      inherited AddLine('<', 'Go up stairs');
      inherited AddLine('>', 'Go down stairs');
      inherited AddLine('G', 'Pick up / Open stash');
      inherited AddLine('D', 'Drop / Store');
      inherited AddLine('L', 'Look mode');
      inherited AddLine('R', 'Rest');
      inherited AddLine('M', 'View messages');
      inherited AddLine('B', 'Spellbook (only Mage)');
      inherited AddLine('C', 'Cast quick spell');
      inherited AddLine('T', 'Talents');
      inherited AddLine('F', 'Ranged fire mode');
      inherited AddLine('N', 'Show statistics');
      inherited AddLine('O', 'Options');
      inherited AddLine('I', 'Show inventory');
      inherited AddLine('P', 'Character screen');
      inherited AddLine('K', 'Calendar');
      inherited AddLine('?', 'Show this help screen');
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
