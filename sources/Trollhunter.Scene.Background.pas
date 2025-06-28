unit Trollhunter.Scene.Background;

interface

uses 
  Trollhunter.Types,
  uScenes;

type
  TSceneBackground = class(TScene)
  private
	procedure StartGame;
  public
    procedure Render; override;
    procedure Update(var AKey: UInt); override;
  end;

implementation

{ TSceneBackground }

uses
  Trollhunter.UI,
  BearLibTerminal,
  Trollhunter.Terminal,
  Trollhunter.Player,
  uLanguage,
  uGame,
  uMap;

const
  DELAY_MS = 1000;

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

procedure TSceneBackground.StartGame;
begin
  Scenes.SetScene(scLoad);
  Terminal.Refresh;
  Terminal_Delay(DELAY_MS);
  Map.Gen;
  Mode.Game := True;
  Player.Talents.DoTalent(TSceneTalents(Scenes.GetScene(scTalents)).Talent);
  Player.StartEquip;
  Player.StartSkills;
  Scenes.SetScene(scGame);
end;

procedure TSceneBackground.Update(var AKey: UInt);
begin
  case AKey of
    TK_ENTER, TK_KP_ENTER:
      if not Mode.Game then
		StartGame;
    TK_SPACE:
      if not Mode.Game then
        Player.GenerateBackground();
    TK_ESCAPE:
      Scenes.GoBack();
  end;
end;

end.
