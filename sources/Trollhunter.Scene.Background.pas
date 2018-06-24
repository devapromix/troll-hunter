unit Trollhunter.Scene.Background;

interface

uses Trollhunter.Types, uScenes;

type
  TSceneBackground = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

{ TSceneBackground }

uses Trollhunter.UI, BearLibTerminal, uTerminal, uPlayer, uLanguage, uGame,
  uMap;

procedure TSceneBackground.Render;
begin
  UI.Title(_('Character Background'));

  Terminal.ForegroundColor(clGray);
  Terminal.Print(CX - (CX div 2), CY - (CY div 2), CX, CY, Player.Background,
    TK_ALIGN_BOTTOM);

  if not Mode.Game then
  begin
    AddKey('Enter', _('Start game'));
    AddKey('Space', _('Re-roll'));
  end;
  AddKey('Esc', _('Close'), _('Back'), True);
end;

procedure TSceneBackground.Update(var Key: UInt);
begin
  case Key of
    TK_ENTER, TK_KP_ENTER:
      if not Mode.Game then
      begin
        Scenes.SetScene(scLoad);
        Terminal.Refresh;
        Terminal_Delay(1000);
        Map.Gen;
        Mode.Game := True;
        Player.Talents.DoTalent
          (TSceneTalents(Scenes.GetScene(scTalents)).Talent);
        Player.StartEquip;
        Player.StartSkills;
        Scenes.SetScene(scGame);
      end;
    TK_SPACE:
      if not Mode.Game then
        Player.GenerateBackground();
    TK_ESCAPE:
      Scenes.GoBack();
  end;
end;

end.
