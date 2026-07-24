unit Trollhunter.Scene.Name;

interface

uses
  Trollhunter.Scenes,
  Trollhunter.Types;

type
  TSceneName = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

uses
  Trollhunter.UI,
  BearLibTerminal,
  Trollhunter.Terminal,
  Trollhunter.Attribute,
  Trollhunter.Statistic,
  Trollhunter.Player,
  Trollhunter.Game,
  Trollhunter.Scene.Talents,
  Trollhunter.Map;

  { TSceneName }

procedure TSceneName.Render;
begin
  UI.Title('Choose a name');

  Terminal.Print(CX - 14, CY, 'Enter your player''s name' + ': ' +
    Player.Name + Game.GetCursor, TK_ALIGN_LEFT);

  AddKey('Enter', 'Confirm');
  AddKey('Esc', 'Back', True);
end;

procedure TSceneName.Update(var Key: UInt);
begin
  case Key of
    TK_BACKSPACE:
    begin
      if (Player.Name <> '') then
        Player.Name := Copy(Player.Name, 1, Length(Player.Name) - 1);
    end;
    TK_ENTER, TK_KP_ENTER:
    begin
      if (Player.Name = '') then
        Player.Name := 'PLAYER';
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
    end;
    TK_A .. TK_Z:
    begin
      if (Length(Player.Name) < 10) then
        Player.Name := Player.Name + Chr(Key - TK_A + 65);
    end;
    TK_ESCAPE:
    begin
      Player.Talents.Clear;
      Scenes.SetScene(scTalents, scClass);
    end;
  end;
end;

end.
