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
  BearLibTerminal,
  Trollhunter.Language,
  Trollhunter.UI,
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
  inherited;
  UI.Title(_('Help'));

  case Scenes.PrevSceneEnum of
    scDifficulty:
      begin
        UI.Title(_('Keybindings'), 5);
        X := 1;
        Y := 7;
        AddLine('Space', _('Random'));
        AddLine('A-Z, Up/Down', _('Select a difficulty'));
      end;
    scClass:
      begin
        UI.Title(_('Keybindings'), 5);
        X := 1;
        Y := 7;
        AddLine('Space', _('Re-roll'));
        AddLine('Backspace', _('Random'));
        AddLine('A-Z, Up/Down', _('Select a class'));
      end;
    scRace:
      begin
        UI.Title(_('Keybindings'), 5);
        X := 1;
        Y := 7;
        AddLine('Tab', _('Choose a sex'));
        AddLine('Space', _('Re-roll'));
        AddLine('Backspace', _('Random'));
        AddLine('A-Z, Up/Down', _('Select a race'));
      end;
    scInv:
      begin
        Terminal.Print(CX, 3, Format(_('To drop an item, press the %s key and then press %s key to drop it.'),
          [UI.KeyToStr('TAB'), UI.KeyToStr('A-Z')]), TK_ALIGN_CENTER);

        UI.Title(_('Keybindings'), 5);
        X := 1;
        Y := 7;
        AddLine('Tab', _('Drop an item to the floor'));
        AddLine('Backspace', _('Show information about item'));
        AddLine('Space', _('Character Screen'));
        AddLine('A-Z', _('Use an item'));
      end;
    scPlayer:
      begin
        UI.Title(_('Keybindings'), 5);

        X := 1;
        Y := 7;
        AddLine('Right/Left', _('Change tab'));
        AddLine('Up/Down', _('Scroll skills'));
        AddLine('Tab', _('Show Background'));
        AddLine('Space', _('Show Inventory'));
      end;
    scAmount:
      begin
        UI.Title(_('Keybindings'), 5);

        X := 1;
        Y := 7;
        AddLine('UP/W', _('More'));
        AddLine('DOWN/X', _('Less'));
        AddLine('Left/A', _('Min'));
        AddLine('Right/D', _('Max'));
      end;
    scGame:
      begin
        Terminal.Print(CX, 3, _('Far away in an uncharted region of the Earth land Elvion lies surrounded by mountains.'), TK_ALIGN_CENTER);
        Terminal.Print(CX, 4, _('In the center of this land there is a village named Dork. It''s people are in'), TK_ALIGN_CENTER);
        Terminal.Print(CX, 5, _('grave danger as the Troll King and his armies are marching to lay waste on all of'), TK_ALIGN_CENTER);
        Terminal.Print(CX, 6, _('its inhabitants. Unless a hero will rise to take a stand against the forces of evil.'), TK_ALIGN_CENTER);

        Terminal.Print(CX, 8, _('You are the hero who departs on a quest to stop the enemies and save your homeland,'), TK_ALIGN_CENTER);
        Terminal.Print(CX, 9, _('Elvion. Survive, gather equipment, fight adversaries and be ready for the final'), TK_ALIGN_CENTER);
        Terminal.Print(CX, 10, _('confrontation. Good luck! You will need it.'), TK_ALIGN_CENTER);

        UI.Title(_('Keybindings'), 12);

        Terminal.Print(CX, 14, Format('%s: %s, %s, %s %s: %s, %s %s: %s', [_('Move'), UI.KeyToStr('arrow keys'), UI.KeyToStr('numpad'),
          UI.KeyToStr('QWEADZXC'), _('Wait'), UI.KeyToStr('5'), UI.KeyToStr('SPACE'), _('Effects'), UI.KeyToStr('TAB')]), TK_ALIGN_CENTER);

        X := 1;
        Y := 16;
        AddLine('<', _('Go up stairs'));
        AddLine('>', _('Go down stairs'));
        AddLine('G', _('Pick up an item from the floor'));
        AddLine('F', _('Drop an item to the floor'));
        AddLine('L', _('Look mode'));
        AddLine('R', _('Rest'));
        AddLine('M', _('View messages'));
        // AddLine('B', _('Spellbook'));
        AddLine('N', _('Show Statistics'));
        AddLine('O', _('Options'));
        AddLine('I', _('Show Inventory'));
        AddLine('P', _('Character Screen'));
        AddLine('K', _('Calendar'));
        AddLine('S', _('Shoot'));
        AddLine('?', _('Show this Help Screen'));

        UI.Title(_('Character dump'), Terminal.Window.Height - 6);
        Terminal.Print(CX, Terminal.Window.Height - 4, Format(_('The game saves a character dump to %s file.'), [UI.KeyToStr('*-character-dump.txt')]
          ), TK_ALIGN_CENTER);
      end;
  end;
  Self.AddKey('Esc', _('Close'), True);
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
