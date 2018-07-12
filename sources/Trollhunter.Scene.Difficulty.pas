unit Trollhunter.Scene.Difficulty;

interface

uses Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneDifficulty = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
    procedure SelRand;
  end;

implementation

uses
  Math,
  SysUtils,
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
var
  I: UInt;
  Difficulty: TDifficultyEnum;

  procedure Add(const AName: string);
  var
    C: Char;
  begin
    C := Chr(I + Ord('A'));
    Terminal.ForegroundColor(clWhite);
    Terminal.Print(1, Y, UI.KeyToStr(C));
    if (Difficulty = Game.Difficulty) then
      Terminal.ForegroundColor(clYellow)
    else
      Terminal.ForegroundColor(clWhite);
    Terminal.Print(5, Y, _(AName));
    Inc(I);
    Inc(Y);
  end;

begin
  UI.Title(_('Choose a difficulty'));
  I := 0;
  Y := 2;

  for Difficulty := Low(TDifficultyEnum) to High(TDifficultyEnum) do
    Add(Game.GetDifficultyName(Difficulty));

  AddKey('Enter', _('Confirm'));
  AddKey('Esc', _('Back'));
  AddKey('?', _('Help'), True);
end;

procedure TSceneDifficulty.SelRand;
var
  Difficulty: TDifficultyEnum;
begin
  Difficulty := Game.Difficulty;
  repeat
    Game.Difficulty := TDifficultyEnum(Math.RandomRange(0,
      Ord(High(TDifficultyEnum)) + 1));
  until (Difficulty <> Game.Difficulty);
end;

procedure TSceneDifficulty.Update(var Key: UInt);
var
  I: Int;
begin
  case Key of
    TK_UP, TK_KP_8:
      if Game.Difficulty > Low(TDifficultyEnum) then
        Game.Difficulty := Pred(Game.Difficulty);
    TK_DOWN, TK_KP_2:
      if Game.Difficulty < High(TDifficultyEnum) then
        Game.Difficulty := Succ(Game.Difficulty);
    TK_A .. TK_Z:
      begin
        I := Ord(Key) - Ord(TK_A);
        if (I > Ord(High(TDifficultyEnum))) then
          Exit;
        Game.Difficulty := TDifficultyEnum(Math.EnsureRange(I, 0,
          Ord(High(TDifficultyEnum))));
      end;
    TK_SPACE:
      SelRand;
    TK_SLASH:
      Scenes.SetScene(scHelp, scDifficulty);
    TK_ENTER, TK_KP_ENTER:
      begin
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
