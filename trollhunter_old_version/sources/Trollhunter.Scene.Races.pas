unit Trollhunter.Scene.Races;

interface

uses
  Trollhunter.Types,
  Trollhunter.Attribute,
  Trollhunter.Player.Races,
  Trollhunter.Scenes;

const
  dsTop = 22;

type
  TSceneRace = class(TScene)
  private
    procedure PrevScene;
    procedure NextScene;
    procedure SetRace(ARaceEnum: TRaceEnum);
  public
    DX: UInt;
    procedure ReRoll;
    procedure SelRand;
    procedure Render; override;
    procedure Update(var Key: UInt); override;
    procedure RenderInfo;
  end;

implementation

uses
  Math,
  BearLibTerminal,
  Trollhunter.Helpers,
  Trollhunter.Language,
  Trollhunter.Terminal,
  Trollhunter.Game,
  Trollhunter.Player,
  Trollhunter.Player.Classes,
  Trollhunter.Player.Types,
  Trollhunter.Player.Helpers,
  Trollhunter.UI,
  Trollhunter.UI.Images,
  Trollhunter.Statistic,
  Trollhunter.Scene.Classes;

{ TSceneRace }

procedure TSceneRace.NextScene;
begin
  Scenes.SetScene(scClass, scRace);
end;

procedure TSceneRace.PrevScene;
begin
  Scenes.SetScene(scDifficulty);
end;

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

  procedure RenderRacePict;
  var
    I: UInt;
  begin
    for I := 0 to 14 do
      Terminal.Print(60, I + 2, RacePict[Player.HRace][I]);
  end;

begin
  inherited;

  UI.Title(_('Choose a race'));
  I := 0;
  Y := 2;
  for R := Low(TRaceEnum) to High(TRaceEnum) do
    Add(Races.GetName(R));

  RenderInfo;

  // Attributes
  Terminal.Print(DX, 10, _('Strength') + ': ' + Terminal.Colorize(UI.Icon(icStr)
    + BaseAttrib.ToString, 'NoMana') + '  ' + UI.Icon(icPlus, 'Lush') +
    Terminal.Colorize(Races.Attrib[atStr], 'Lush'));
  Terminal.Print(DX, 11, _('Dexterity') + ': ' +
    Terminal.Colorize(UI.Icon(icDex) + BaseAttrib.ToString, 'NoMana') + '  ' +
    UI.Icon(icPlus, 'Lush') + Terminal.Colorize(Races.Attrib[atDex], 'Lush'));
  Terminal.Print(DX, 12, _('Willpower') + ': ' +
    Terminal.Colorize(UI.Icon(icBook) + BaseAttrib.ToString, 'NoMana') + '  ' +
    UI.Icon(icPlus, 'Lush') + Terminal.Colorize(Races.Attrib[atWil], 'Lush'));
  Terminal.Print(DX, 13, _('Perception') + ': ' +
    Terminal.Colorize(UI.Icon(icLeaf) + BaseAttrib.ToString, 'NoMana') + '  ' +
    UI.Icon(icPlus, 'Lush') + Terminal.Colorize(Races.Attrib[atPer], 'Lush'));

  // Life and Mana
  Terminal.Print(DX, 15, _('Life') + ': ' + Terminal.Colorize(UI.Icon(icLife) +
    BaseLife.ToString, 'NoMana') + '  ' + UI.Icon(icPlus, 'Lush') +
    Terminal.Colorize(Races.Attrib[atLife], 'Lush'));
  Terminal.Print(DX, 16, _('Mana') + ': ' + Terminal.Colorize(UI.Icon(icMana) +
    BaseMana.ToString, 'NoMana') + '  ' + UI.Icon(icPlus, 'Lush') +
    Terminal.Colorize(Races.Attrib[atMana], 'Lush'));

  // Description
  Terminal.ForegroundColor(clGray);
  Terminal.Print(DX, dsTop, Round(CX * 1.4), Terminal.Screen.Height - 3,
    _(Races.GetDescription(Player.HRace)), TK_ALIGN_TOP);
  RenderRacePict;

  AddKey('Enter', _('Confirm'));
  AddKey('Esc', _('Back'));
  AddKey('?', _('Help'), True);
end;

procedure TSceneRace.RenderInfo;
begin
  DX := CX - (CX div 2);
  Terminal.ForegroundColor(clWhite);
  Terminal.Print(DX, 3, _('Race') + ': ' + Terminal.Colorize
    (_(Races.GetName(Player.HRace)), 'Lush'));
  Terminal.Print(DX, 4, _('Age') + ': ' + Terminal.Colorize
    (Player.Statictics.Get(stAge), 'Lush'));
  Terminal.Print(DX, 5, _('Height') + ': ' + Terminal.Colorize
    (Player.Statictics.Get(stHeight), 'Lush'));
  Terminal.Print(DX, 6, _('Weight') + ': ' + Terminal.Colorize
    (Player.Statictics.Get(stWeight), 'Lush'));
  Terminal.Print(DX, 7, _('Sex') + ': ' + Terminal.Colorize
    (Player.Gender, 'Lush'));
  Terminal.Print(DX, 8, _('Metabolism') + ': ' +
    Terminal.Colorize(Player.Statictics.Get(stMetabolism), 'Lush'));
end;

procedure TSceneRace.ReRoll;
var
  Prop: TRaceProp;
  Age, Height, Weight, Metabolism: Integer;
begin
  Prop := RaceProp[Player.HRace];

  Age := Math.RandomRange(Prop.Age.Min, Prop.Age.Max + 1);
  Player.Statictics.SetValue(stAge, Age);

  Height := Math.RandomRange(Prop.Height.Min, Prop.Height.Max + 1);
  Player.Statictics.SetValue(stHeight, Height);

  Weight := Math.RandomRange(Prop.Weight.Min, Prop.Weight.Max + 1);
  Player.Statictics.SetValue(stWeight, Weight);

  Metabolism := Math.EnsureRange(Math.RandomRange(Prop.Metabolism.Min,
    Prop.Metabolism.Max + 1) + Round(Height div 50) + Round(Weight div 15),
    MetabolismMin, MetabolismMax);
  Player.Statictics.SetValue(stMetabolism, Metabolism);

  // Attributes
  Races.Attrib[atStr] := Math.RandomRange(Prop.Strength.Min,
    Prop.Strength.Max + 1);
  Races.Attrib[atDex] := Math.RandomRange(Prop.Dexterity.Min,
    Prop.Dexterity.Max + 1);
  Races.Attrib[atWil] := Math.RandomRange(Prop.Willpower.Min,
    Prop.Willpower.Max + 1);
  Races.Attrib[atPer] := Math.RandomRange(Prop.Perception.Min,
    Prop.Perception.Max + 1);

  // Life and Mana
  Races.Attrib[atLife] := Math.RandomRange(Prop.Life.Min, Prop.Life.Max + 1);
  Races.Attrib[atMana] := Math.RandomRange(Prop.Mana.Min, Prop.Mana.Max + 1);
end;

procedure TSceneRace.SelRand;
var
  R: TRaceEnum;
begin
  R := Player.HRace;
  repeat
    SetRace(TRaceEnum(Math.RandomRange(0, Ord(High(TRaceEnum)) + 1)));
  until (R <> Player.HRace);
end;

procedure TSceneRace.SetRace(ARaceEnum: TRaceEnum);
begin
  Player.HRace := ARaceEnum;
  ReRoll;
end;

procedure TSceneRace.Update(var Key: UInt);
var
  I: Int;
begin
  case Key of
    TK_LEFT, TK_KP_4:
      SetRace(Low(TRaceEnum));
    TK_RIGHT, TK_KP_6:
      SetRace(High(TRaceEnum));
    TK_UP, TK_KP_8:
      if Player.HRace > Low(TRaceEnum) then
        SetRace(Pred(Player.HRace));
    TK_DOWN, TK_KP_2:
      if Player.HRace < High(TRaceEnum) then
        SetRace(Succ(Player.HRace));
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
        SetRace(TRaceEnum(Math.EnsureRange(I, 0, Ord(High(TRaceEnum)))));
        NextScene;
      end;
    TK_ENTER, TK_KP_ENTER:
      NextScene;
    TK_ESCAPE:
      PrevScene;
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
