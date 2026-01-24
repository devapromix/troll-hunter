unit Trollhunter.Scene.Help;

interface

uses
  Trollhunter.Types,
  uScenes;

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
        AddLine('Space', 'Re-roll');
        AddLine('Backspace', 'Random');
        AddLine('A-Z', 'Select a class');
      end;
    scRace:
      begin
        UI.Title('Keybindings', 5);
        X := 1;
        Y := 7;
        AddLine('Tab', 'Choose a sex');
        AddLine('Space', 'Re-roll');
        AddLine('Backspace', 'Random');
        AddLine('A-Z', 'Select a race');
      end;
    scInv:
      begin
        Terminal.Print(CX, 3,
          Format('To drop an item, press the %s key and then press %s key to drop it.',
          [UI.KeyToStr('TAB'), UI.KeyToStr('A-Z')]), TK_ALIGN_CENTER);

        UI.Title('Keybindings', 5);
        X := 1;
        Y := 7;
        AddLine('Tab', 'Drop an item to the floor');
        AddLine('Space', 'Character Screen');
        AddLine('A-Z', 'Use an item');
      end;
    scPlayer:
      begin
        UI.Title('Keybindings', 5);

        X := 1;
        Y := 8;
        AddLine('Right/Left', 'Change tab');
        AddLine('Up/Down', 'Scroll skills');
        AddLine('Tab', 'Show Background');
        AddLine('Space', 'Show Inventory');
      end;
    scGame:
      begin
        Terminal.Print(CX, 3,
          'Far away in an uncharted region of the Earth land Elvion lies surrounded by mountains.',
          TK_ALIGN_CENTER);
        Terminal.Print(CX, 4,
          'In the center of this land there is a village named Dork. It''s people are in',
          TK_ALIGN_CENTER);
        Terminal.Print(CX, 5,
          'grave danger as the Troll King and his armies are marching to lay waste on all of',
          TK_ALIGN_CENTER);
        Terminal.Print(CX, 6,
          'its inhabitants. Unless a hero will rise to take a stand against the forces of evil.',
          TK_ALIGN_CENTER);

        Terminal.Print(CX, 8,
          'You are the hero who departs on a quest to stop the enemies and save your homeland,',
          TK_ALIGN_CENTER);
        Terminal.Print(CX, 9,
          'Elvion. Survive, gather equipment, fight adversaries and be ready for the final',
          TK_ALIGN_CENTER);
        Terminal.Print(CX, 10, 'confrontation. Good luck! You will need it.',
          TK_ALIGN_CENTER);

        UI.Title('Keybindings', 12);

        Terminal.Print(CX, 14, Format('%s: %s, %s, %s %s: %s, %s %s: %s',
          ['Move', UI.KeyToStr('arrow keys'), UI.KeyToStr('numpad'),
          UI.KeyToStr('QWEADZXC'), 'Wait', UI.KeyToStr('5'), UI.KeyToStr('S'),
          'Effects', UI.KeyToStr('TAB')]), TK_ALIGN_CENTER);

        X := 1;
        Y := 16;
        AddLine('<', 'Go up stairs');
        AddLine('>', 'Go down stairs');
        AddLine('G', 'Pick up an item from the floor');
        AddLine('F', 'Drop an item to the floor');
        AddLine('L', 'Look mode');
        AddLine('R', 'Rest');
        AddLine('M', 'View messages');
        // AddLine('B', 'Spellbook');
        AddLine('T', 'Talents');
        AddLine('N', 'Show Statistics');
        AddLine('O', 'Options');
        AddLine('I', 'Show Inventory');
        AddLine('P', 'Character Screen');
        AddLine('K', 'Calendar');
        AddLine('?', 'Show this Help Screen');

        UI.Title('Character dump', Terminal.Window.Height - 6);
        Terminal.Print(CX, Terminal.Window.Height - 4,
          Format('The game saves a character dump to %s file.',
          [UI.KeyToStr('*-character-dump.txt')]), TK_ALIGN_CENTER);
      end;
  end;
  Self.AddKey('Esc', 'Close', True);
end;

procedure TSceneHelp.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      // Close
      Scenes.GoBack;
  end;
end;

end.
