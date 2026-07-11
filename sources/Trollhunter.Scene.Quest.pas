unit Trollhunter.Scene.Quest;

interface

uses
  Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneQuest = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

{ TSceneQuest }
uses
  BearLibTerminal,
  Trollhunter.UI,
  Trollhunter.Quest,
  Trollhunter.Game;

procedure TSceneQuest.Render;
begin
  UI.Title(Quests.GetName(Quests.Current), 1);

  AddKey('Enter', 'Accept');
  AddKey('Esc', 'Decline', True);
end;

procedure TSceneQuest.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      Scenes.GoBack();
    TK_ENTER, TK_KP_ENTER:
    begin
      Quests.Add(Quests.Current);
      Scenes.GoBack();
    end
    else
      Game.Timer := UIntMax;
  end;
end;

end.
