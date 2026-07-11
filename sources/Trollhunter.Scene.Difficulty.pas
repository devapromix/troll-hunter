unit Trollhunter.Scene.Difficulty;

interface

uses
  Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneDifficulty = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

uses
  Math,
  SysUtils,
  BearLibTerminal,
  Trollhunter.Terminal,
  Trollhunter.Game,
  Trollhunter.Player,
  Trollhunter.UI,
  Trollhunter.Scene.RacesAndClasses;

  { TSceneDifficulty }

procedure TSceneDifficulty.Render;
var
  I, DX: UInt;
  D: TDifficulty;
  S: string;

  procedure Add(const AName, ADescription: string);
  var
    C: char;
  begin
    C := Chr(I + Ord('A'));
    Terminal.ForegroundColor(clWhite);
    Terminal.Print(1, Y, UI.KeyToStr(C));
    if (D = Game.Difficulty) then
    begin
      Terminal.ForegroundColor(clYellow);
      S := ADescription;
    end
    else
      Terminal.ForegroundColor(clWhite);
    Terminal.Print(5, Y, AName);
    Inc(I);
    Inc(Y);
  end;

begin
  UI.Title('Choose a difficulty');
  I := 0;
  Y := 3;
  for D := Low(TDifficulty) to High(TDifficulty) do
    case D of
      dfEasy:
        Add('Easy',
          'A gentle path for those who want to enjoy the story and explore the world without the constant threat of death. Enemies are weaker, resources are more plentiful, and mistakes are easier to recover from.');
      dfNormal:
        Add('Normal',
          'The intended trollhunter experience. Enemies fight back with real strength, resources are scarce enough to matter, and every decision carries weight. Recommended for most players.');
      dfHard:
        Add('Hard',
          'A serious challenge meant for experienced adventurers. Enemies hit harder and act smarter, supplies run thin, and careless mistakes can easily prove fatal.');
      dfHell:
        Add('Hell',
          'The trollhunter''s true trial. Every foe is deadly, every resource is precious, and there is little room for error. Only the boldest and most skilled will survive.');
    end;

  DX := CX - (CX div 2);
  Terminal.ForegroundColor(clWhite);
  if Game.Difficulty = dfEasy then
    Terminal.Print(DX, 3, 'Permadeath' + ': ' +
      Terminal.Colorize('Fall, and rise to hunt again.', 'Lush'))
  else
    Terminal.Print(DX, 3, 'Permadeath' + ': ' + Terminal.Colorize(
      'Fall once, and your hunt ends forever."', 'Light Red'));
  if Game.Difficulty = dfEasy then
    Terminal.Print(DX, 4, 'Gear Decay' + ': ' +
      Terminal.Colorize('Your gear mends like new.', 'Lush'))
  else if Game.Difficulty = dfHell then
    Terminal.Print(DX, 4, 'Gear Decay' + ': ' + Terminal.Colorize(
      'Your gear crumbles faster than it can be fixed."', 'Light Red'))
  else
    Terminal.Print(DX, 4, 'Gear Decay' + ': ' + Terminal.Colorize(
      'The smith''s hammer wears your gear thin."', 'Yellow'));
  Terminal.Print(DX, 5, 'Start Gold' + ': ' + Terminal.Colorize(
    IntToStr(Player.GetStartGold()), 'Yellow'));
  Terminal.ForegroundColor(clGray);
  Terminal.Print(CX - (CX div 2), CY - (CY div 2), CX, CY, S, TK_ALIGN_BOTTOM);

  AddKey('Enter', 'Confirm');
  AddKey('Esc', 'Back', True);
end;

procedure TSceneDifficulty.Update(var Key: UInt);
var
  I: Int;
begin
  case Key of
    TK_A .. TK_D:
    begin
      I := Ord(Key) - Ord(TK_A);
      Game.Difficulty := TDifficulty(Math.EnsureRange(I, 0,
        Ord(High(TDifficulty))));
    end;
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
