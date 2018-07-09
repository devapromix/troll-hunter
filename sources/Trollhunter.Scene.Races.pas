unit Trollhunter.Scene.Races;

interface

uses Trollhunter.Types,
  Trollhunter.Attribute,
  Trollhunter.Scenes;

type
  TSceneRace = class(TScene)
  public
    DX: UInt;
    PrmAt: array [atStr .. atMana] of UInt;
    procedure ReRoll;
    procedure SelRand;
    procedure Render; override;
    procedure Update(var Key: UInt); override;
    procedure RenderInfo;
  end;

implementation

uses Math,
  BearLibTerminal,
  Trollhunter.Language,
  Trollhunter.Terminal,
  Trollhunter.Player,
  Trollhunter.Game,
  Trollhunter.Player.Races,
  Trollhunter.Player.Classes,
  Trollhunter.UI,
  Trollhunter.Statistic,
  Trollhunter.Player.Types,
  Trollhunter.Player.Helpers, Trollhunter.Scene.Classes;

{ TSceneRace }

procedure TSceneRace.Render;
var
  I: UInt;
  R: TRaceEnum;

  procedure Add(const AName: string);
  var
    C: Char;
  begin
    C := Chr(I + Ord('A'));
    Terminal.ForegroundColor(clWhite);
    Terminal.Print(1, Y, UI.KeyToStr(C));
    if (R = Player.HRace) then
      Terminal.ForegroundColor(clYellow)
    else
      Terminal.ForegroundColor(clWhite);
    Terminal.Print(5, Y, _(AName));
    Inc(I);
    Inc(Y);
  end;

begin
  UI.Title(_('Choose a race'));
  I := 0;
  Y := 2;
  for R := Low(TRaceEnum) to High(TRaceEnum) do
    Add(Races.GetName(R));

  RenderInfo;

  Terminal.ForegroundColor(clGray);
  Terminal.Print(DX, CY - (CY div 2), CX, CY,
    _(Races.GetDescription(Player.HRace)), TK_ALIGN_BOTTOM);

  AddKey('Enter', _('Confirm'));
  AddKey('Esc', _('Back'));
  AddKey('?', _('Help'), True);
end;

procedure TSceneRace.RenderInfo;
begin
  DX := CX - (CX div 2);
  Terminal.ForegroundColor(clWhite);
  Terminal.Print(DX, 3, _('Age') + ': ' + Terminal.Colorize
    (Player.Statictics.Get(stAge), 'Lush'));
  Terminal.Print(DX, 4, _('Height') + ': ' + Terminal.Colorize
    (Player.Statictics.Get(stHeight), 'Lush'));
  Terminal.Print(DX, 5, _('Weight') + ': ' + Terminal.Colorize
    (Player.Statictics.Get(stWeight), 'Lush'));
  Terminal.Print(DX, 6, _('Sex') + ': ' + Terminal.Colorize
    (Player.Gender, 'Lush'));
  Terminal.Print(DX, 7, _('Metabolism') + ': ' +
    Terminal.Colorize(Player.Statictics.Get(stMetabolism), 'Lush'));

  // Attributes
  Terminal.Print(DX, 9, _('Strength') + ': ' +
    Terminal.Colorize(Player.Attributes.Attrib[atStr].Prm, 'Lush'));
  Terminal.Print(DX, 10, _('Dexterity') + ': ' +
    Terminal.Colorize(Player.Attributes.Attrib[atDex].Prm, 'Lush'));
  Terminal.Print(DX, 11, _('Willpower') + ': ' +
    Terminal.Colorize(Player.Attributes.Attrib[atWil].Prm, 'Lush'));
  Terminal.Print(DX, 12, _('Perception') + ': ' +
    Terminal.Colorize(Player.Attributes.Attrib[atPer].Prm, 'Lush'));

  // Life and Mana
  Terminal.Print(DX, 14, _('Life') + ': ' + Terminal.Colorize
    (Player.Attributes.Attrib[atLife].Prm, 'Lush'));
  Terminal.Print(DX, 15, _('Mana') + ': ' + Terminal.Colorize
    (Player.Attributes.Attrib[atMana].Prm, 'Lush'));
end;

procedure TSceneRace.ReRoll;
var
  V: TRaceProp;
  Age, Height, Weight, Metabolism: Integer;
begin
  V := RaceProp[Player.HRace];

  Age := Math.RandomRange(V.Age.Min, V.Age.Max + 1);
  Player.Statictics.SetValue(stAge, Age);

  Height := Math.RandomRange(V.Height.Min, V.Height.Max + 1);
  Player.Statictics.SetValue(stHeight, Height);

  Weight := Math.RandomRange(V.Weight.Min, V.Weight.Max + 1);
  Player.Statictics.SetValue(stWeight, Weight);

  Metabolism := Math.EnsureRange(Math.RandomRange(V.Metabolism.Min,
    V.Metabolism.Max + 1) + Round(Height div 50) + Round(Weight div 15),
    MetabolismMin, MetabolismMax);
  Player.Statictics.SetValue(stMetabolism, Metabolism);

  // Attributes
  Player.Attributes.SetPrm(atStr, Math.RandomRange(V.Strength.Min,
    V.Strength.Max + 1));
  PrmAt[atStr] := Player.Attributes.Attrib[atStr].Prm;
  Player.Attributes.SetPrm(atDex, Math.RandomRange(V.Dexterity.Min,
    V.Dexterity.Max + 1));
  PrmAt[atDex] := Player.Attributes.Attrib[atDex].Prm;
  Player.Attributes.SetPrm(atWil, Math.RandomRange(V.Willpower.Min,
    V.Willpower.Max + 1));
  PrmAt[atWil] := Player.Attributes.Attrib[atWil].Prm;
  Player.Attributes.SetPrm(atPer, Math.RandomRange(V.Perception.Min,
    V.Perception.Max + 1));
  PrmAt[atPer] := Player.Attributes.Attrib[atPer].Prm;

  // Life and Mana
  Player.Attributes.SetPrm(atLife, Math.RandomRange(V.Life.Min,
    V.Life.Max + 1));
  PrmAt[atLife] := Player.Attributes.Attrib[atLife].Prm;
  Player.Attributes.SetPrm(atMana, Math.RandomRange(V.Mana.Min,
    V.Mana.Max + 1));
  PrmAt[atMana] := Player.Attributes.Attrib[atMana].Prm;
end;

procedure TSceneRace.SelRand;
var
  R: TRaceEnum;
begin
  R := Player.HRace;
  repeat
    Player.HRace := TRaceEnum(Math.RandomRange(0, Ord(High(TRaceEnum)) + 1));
  until (R <> Player.HRace);
end;

procedure TSceneRace.Update(var Key: UInt);
var
  I: Int;
begin
  case Key of
    TK_TAB:
      begin
        if (Player.Sex = sxMale) then
          Player.Sex := sxFemale
        else
          Player.Sex := sxMale;
        ReRoll;
      end;
    TK_A .. TK_Z:
      begin
        I := Ord(Key) - Ord(TK_A);
        if (I > Ord(High(TRaceEnum))) then
          Exit;
        Player.HRace := TRaceEnum(Math.EnsureRange(I, 0, Ord(High(TRaceEnum))));
        ReRoll;
      end;
    TK_ENTER, TK_KP_ENTER:
      begin
        (Scenes.GetScene(scClass) as TSceneClass).ReRoll;
        Scenes.SetScene(scClass, scRace);
      end;
    TK_ESCAPE:
      begin
        Scenes.SetScene(scDifficulty);
      end;
    TK_BACKSPACE:
      begin
        SelRand;
        ReRoll;
      end;
    TK_SLASH:
      Scenes.SetScene(scHelp, scRace);
    TK_SPACE:
      ReRoll;
  end;
end;

end.
