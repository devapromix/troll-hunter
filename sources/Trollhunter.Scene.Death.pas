unit Trollhunter.Scene.Death;

interface

uses
  Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneDeath = class(TScene)
  private
    FScreenshotTaken: boolean;
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

uses
  SysUtils,
  BearLibTerminal,
  Trollhunter.Game,
  Trollhunter.Terminal,
  Trollhunter.Player,
  Trollhunter.Player.Helpers,
  Trollhunter.Player.Races,
  Trollhunter.Player.Classes,
  Trollhunter.Statistic,
  Trollhunter.Attribute,
  Trollhunter.UI;

const
  CTombstoneWidth = 50;
  CTombstoneHeight = 23;
  CTombstone: array [0..CTombstoneHeight - 1] of string = (
    '                   _____  _____',
    '                  <     `/     |',
    '                   >          (',
    '                  |   _     _  |',
    '                  |  |_) | |_) |',
    '                  |  | \ | |   |',
    '                  |            |',
    '   ______.______%_|            |__________  _____',
    ' _/                                       \|     |',
    '|                                                <',
    '|_____.-._________              ____/|___________|',
    '                  |            |',
    '                  |            |',
    '                  |            |',
    '                  |            |',
    '                  |   _        <',
    '                  |__/         |',
    '                   / `--.      |',
    '                 %|            |%',
    '             |/.%%|          -< @%%%',
    '             `\%`@|     v      |@@%@%%',
    '           .%%%@@@|%    |    % @@@%%@%%%%',
    '      _.%%%%%%@@@@@@%%_/%\_%@@%%@@@@@@@%%%%%%'
  );

{ TSceneDeath }

procedure TSceneDeath.Render;
var
  LI: Int;
  LLeft, LTop, LRight, LY: Int;
  LDeathText: string;

  procedure AddStat(const AText: string; AValue: Int);
  begin
    Terminal.ForegroundColor(clWhite);
    Terminal.Print(LRight, LY, AText + ':', TK_ALIGN_LEFT);
    Terminal.ForegroundColor(clGreen);
    Terminal.Print(LRight + 20, LY, AValue.ToString(), TK_ALIGN_LEFT);
    Inc(LY);
  end;

  procedure AddStrStat(const AText, AValue: string);
  begin
    Terminal.ForegroundColor(clWhite);
    Terminal.Print(LRight, LY, AText + ':', TK_ALIGN_LEFT);
    Terminal.ForegroundColor(clGreen);
    Terminal.Print(LRight + 20, LY, AValue, TK_ALIGN_LEFT);
    Inc(LY);
  end;

begin
  LLeft := 4;
  LTop := CY - (CTombstoneHeight div 2);
  LRight := CX + 8;

  // Tombstone
  Terminal.ForegroundColor(clDarkGray);
  for LI := 0 to CTombstoneHeight - 1 do
    Terminal.Print(LLeft, LTop + LI, CTombstone[LI], TK_ALIGN_LEFT);

  // Epitaph
  Terminal.ForegroundColor(clWhite);
  Terminal.Print(LRight, LTop, UpperCase('Game over!'), TK_ALIGN_LEFT);

  Terminal.ForegroundColor(clDefault);
  Terminal.Print(LRight, LTop + 2, Format('Here lies [color=light green]%s[/color].', [UpperCase(Player.FullName)]),
    TK_ALIGN_LEFT);

  if (Player.Killer = '') then
    LDeathText := 'Death took them quietly.'
  else
    LDeathText := Format('Slain by [color=light red]%s[/color].',
      [Terminal.Colorize(UpperCase(Player.Killer), clAlarm)]);
  Terminal.Print(LRight, LTop + 3, LDeathText, TK_ALIGN_LEFT);

  // Statistics
  LY := LTop + 5;
  AddStat('Level', Player.Attributes.Attrib[atLev].Value);
  AddStat('Score', Player.Statictics.Get(stScore));
  AddStrStat('Difficulty', Game.GetStrDifficulty);
  AddStrStat('Race and Class', Races.GetName(Player.HRace) + ' ' +
    Trollhunter.Player.Classes.Classes.GetName(Player.HClass));
  AddStat('Tiles Moved', Player.Statictics.Get(stTurn));
  AddStat('Monsters Killed', Player.Statictics.Get(stKills));
  AddStat('Items Found', Player.Statictics.Get(stFound));
  AddStat('Potions Drunk', Player.Statictics.Get(stPotDrunk));
  AddStat('Scrolls Read', Player.Statictics.Get(stScrRead));
  AddStat('Spells Cast', Player.Statictics.Get(stSpCast));
  AddStat('Foods Eaten', Player.Statictics.Get(stFdEat));
  AddStat('Items Used', Player.Statictics.Get(stItUsed));
  AddStat('Items Repaired', Player.Statictics.Get(stItRep));
  AddStat('Items Identified', Player.Statictics.Get(stItIdent));
  AddStat('Items Crafted', Player.Statictics.Get(stItCrafted));
  AddStat('Gold', Player.Gold);

  Terminal.ForegroundColor(clDefault);
  Terminal.Print(LRight, LY + 1, Format('Press %s to exit!',
    [UI.KeyToStr('ENTER')]), TK_ALIGN_LEFT);
  if Mode.Wizard then
    Terminal.Print(LRight, LY + 2, Format('Press %s to continue...',
      [UI.KeyToStr('SPACE')]), TK_ALIGN_LEFT);

  if not FScreenshotTaken and not Mode.Wizard then
  begin
    Terminal.SaveTextScreenshot(Player.Name);
    FScreenshotTaken := True;
  end;
end;

procedure TSceneDeath.Update(var Key: UInt);
begin
  case Key of
    TK_ENTER, TK_KP_ENTER:
      Game.CanClose := True;
    TK_SPACE:
      if Mode.Wizard then
      begin
        FScreenshotTaken := False;
        Player.Fill;
        Scenes.SetScene(scGame);
      end;
  end;
end;

end.
