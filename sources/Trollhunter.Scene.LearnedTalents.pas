unit Trollhunter.Scene.LearnedTalents;

interface

uses
  Trollhunter.Types,
  Trollhunter.Talent,
  Trollhunter.Scenes;

type
  TSceneLearnedTalents = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

uses
  SysUtils,
  BearLibTerminal,
  Trollhunter.Game,
  Trollhunter.Player,
  Trollhunter.UI.Log,
  Trollhunter.Terminal,
  Trollhunter.UI;

procedure TSceneLearnedTalents.Render;
var
  I, V, Y: UInt;
  C: char;
begin
  UI.Title('Learned Talents');

  V := 0;
  Y := 2;
  UI.FromAToZ(ItemMax);
  Terminal.ForegroundColor(clWhite);

  for I := 0 to TalentMax - 1 do
  begin
    if (Player.Talents.Talent[I].Enum <> tlNone) then
    begin
      with Player.Talents do
      begin
        C := Chr(V + Ord('A'));
        Terminal.Print(1, Y, UI.KeyToStr(C, False));
        Terminal.Print(5, Y, GetLevelName(Talent[I].Enum, Talent[I].Level));
        Terminal.ForegroundColor(clGray);
        Terminal.Print(30, Y, GetDescription(Talent[I].Enum));
      end;
      Terminal.ForegroundColor(clWhite);
      Inc(V);
    end;
    Inc(Y);
  end;

  if Mode.Game then
    MsgLog.Render(2, True);

  AddKey('Space', 'Show Talents');
  AddKey('Esc', 'Close', 'Back', True);
end;

procedure TSceneLearnedTalents.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      Scenes.GoBack;
    TK_SPACE:
      Scenes.SetScene(scTalents);
  end;
end;

end.
