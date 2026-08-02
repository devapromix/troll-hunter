unit Trollhunter.Scene.Victory;

interface

uses
  Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneVictory = class(TScene)
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
  Trollhunter.Statistic,
  Trollhunter.Attribute,
  Trollhunter.UI;

const
  CTrophyWidth = 41;
    CTrophyHeight = 23;
    CTrophy: array [0..22] of string = (
      '                 _________________',
      '                ''.__===========__.''',
      '          .---.--\:            /-.---.',
      '         /    /   \:          /   \   \',
      '        |    |     \:        /  .  |   |',
      '        |    |      \:      /      |   |',
      '        |    |       \:    /       |   |',
      '         \    \   ,   \:  /       /   /',
      '          ''----\       \:/       /---''',
      '                \       :       /',
      '                 \     .:.     /',
      '                  ''.  '':::''  .''',
      '                    ''. '':'' .''',
      '                      \ . /',
      '                       \ /',
      '                       |_|',
      '                      /   \',
      '                     /,    \',
      '                    /_____._\',
      '                    |       |',
      '                    |_______|',
      '                   /_________\',
      '                   ''---------'''
    );

{ TSceneVictory }

procedure TSceneVictory.Render;
var
  LI: Int;
  LLeft, LTop, LRight, LY: Int;

  procedure AddStat(const AText: string; AValue: Int);
  begin
    Terminal.ForegroundColor(clWhite);
    Terminal.Print(LRight, LY, AText + ':', TK_ALIGN_LEFT);
    Terminal.ForegroundColor(clGreen);
    Terminal.Print(LRight + 20, LY, AValue.ToString(), TK_ALIGN_LEFT);
    Inc(LY);
  end;

begin
  LLeft := 4;
  LTop := CY - (CTrophyHeight div 2);
  LRight := CX + 6;

  // Trophy
  Terminal.ForegroundColor(clLightestYellow);
  for LI := 0 to CTrophyHeight - 1 do
    Terminal.Print(LLeft, LTop + LI, CTrophy[LI], TK_ALIGN_LEFT);

  // Victory text
  Terminal.ForegroundColor(clWhite);
  Terminal.Print(LRight, LTop, UpperCase('Victory'), TK_ALIGN_LEFT);

  Terminal.ForegroundColor(clDefault);
  Terminal.Print(LRight, LTop + 1, Format('%s has saved Elvion!',
    [Player.Name]), TK_ALIGN_LEFT);
  Terminal.Print(LRight, LTop + 2, 'The Troll King is no more.', TK_ALIGN_LEFT);

  // Statistics
  LY := LTop + 4;
  AddStat('Level', Player.Attributes.Attrib[atLev].Value);
  AddStat('Score', Player.Statictics.Get(stScore));
  AddStat('Age', Player.Statictics.Get(stAge));
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
  AddStat('Coins Looted', Player.Statictics.Get(stCoinsLooted));
  AddStat('Gold', Player.Gold);

  Terminal.ForegroundColor(clDefault);
  Terminal.Print(LRight, LY + 1, Format('Press %s to exit!',
    [UI.KeyToStr('ENTER')]), TK_ALIGN_LEFT);
end;

procedure TSceneVictory.Update(var Key: UInt);
begin
  case Key of
    TK_ENTER, TK_KP_ENTER:
      Game.CanClose := True;
  end;
end;

end.
