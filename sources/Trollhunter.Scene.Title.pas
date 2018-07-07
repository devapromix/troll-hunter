unit Trollhunter.Scene.Title;

interface

uses Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneTitle = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
    procedure RenderHeroes();
  end;

implementation

{ TSceneTitle }

uses BearLibTerminal,
  SysUtils,
  Trollhunter.Terminal,
  Trollhunter.Game,
  Trollhunter.UI,
  Trollhunter.Helpers,
  Trollhunter.UI.Logo,
  Trollhunter.Language;

type
  TAJ = 'A' .. 'J';

procedure TSceneTitle.Render;
begin
  Logo.Render(True);
  Terminal.Print(Screen.Width - ((Screen.Width div 2) - (Logo.Width div 2) + 2),
    14, Format('by Apromix v.%s', [Game.GetVersion]), TK_ALIGN_RIGHT);
  // RenderHeroes;
  if Mode.Wizard then
  begin
    Self.AddKey('Space', _('Create a new hero'));
    Self.AddKey('Z', Terminal.Colorize(_('Turn Wizard Mode Off'), 'Red'), True);
  end
  else
    Self.AddKey('Space', _('Create a new hero'), True);
end;

procedure TSceneTitle.RenderHeroes;
const
  L = 12;
  T = 15;
var
  J: UInt;
  V: TAJ;
begin
  Terminal.ForegroundColor(clWhite);
  Terminal.Print(L + 4, T, _('Which hero shall you play?'));

  for V := 'A' to 'J' do
  begin
    J := Ord(V) - 65;
    Terminal.Print(L, T + J + 2, UI.KeyToStr(V, J.ToString));
  end;
end;

procedure TSceneTitle.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      Game.CanClose := True;
    TK_A .. TK_J:
      ;
    TK_SPACE:
      Scenes.SetScene(scDifficulty);
    TK_ENTER, TK_KP_ENTER:
      if Mode.Wizard then
        Scenes.SetScene(scDifficulty);
    TK_Z:
      Mode.Wizard := False;
  end;
end;

end.
