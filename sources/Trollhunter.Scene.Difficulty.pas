unit Trollhunter.Scene.Difficulty;

interface

uses Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneDifficulty = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

uses SysUtils,
  BearLibTerminal,
  Trollhunter.UI,
  Trollhunter.UI.Log,
  Trollhunter.Language,
  Trollhunter.Item,
  Trollhunter.Terminal,
  Trollhunter.Player,
  Trollhunter.Game,
  Trollhunter.Scene.Races,
  Trollhunter.Scene.Classes;

{ TSceneDifficulty }

procedure TSceneDifficulty.Render;
begin
  UI.Title(_('Choose a difficulty'));

  Terminal.Print(CX - 5, CY - 3, Format('%s %s', [UI.KeyToStr('A'), _('Easy')]),
    TK_ALIGN_LEFT);
  Terminal.Print(CX - 5, CY - 1, Format('%s %s', [UI.KeyToStr('B'), _('Normal')]
    ), TK_ALIGN_LEFT);
  Terminal.Print(CX - 5, CY + 1, Format('%s %s', [UI.KeyToStr('C'), _('Hard')]),
    TK_ALIGN_LEFT);
  Terminal.Print(CX - 5, CY + 3, Format('%s %s', [UI.KeyToStr('D'), _('Hell')]),
    TK_ALIGN_LEFT);

  AddKey('Esc', _('Back'), True);
end;

procedure TSceneDifficulty.Update(var Key: UInt);
begin
  case Key of
    TK_A .. TK_D, TK_ENTER, TK_KP_ENTER:
      begin
        case Key of
          TK_A:
            Game.Difficulty := dfEasy;
          TK_B:
            Game.Difficulty := dfNormal;
          TK_C:
            Game.Difficulty := dfHard;
          TK_D:
            Game.Difficulty := dfHell;
          TK_ENTER, TK_KP_ENTER:
            if Mode.Wizard then
              Game.Difficulty := dfNormal
            else
              Exit;
        end;
        Game.Start();
        Scenes.SetScene(scRace, scDifficulty);
        (Scenes.GetScene(scRace) as TSceneRace).SelRand;
        (Scenes.GetScene(scRace) as TSceneRace).ReRoll;
        (Scenes.GetScene(scClass) as TSceneClass).SelRand;
        (Scenes.GetScene(scClass) as TSceneClass).ReRoll;
      end;
    TK_ESCAPE:
      Scenes.SetScene(scTitle);
  end;
end;

end.
