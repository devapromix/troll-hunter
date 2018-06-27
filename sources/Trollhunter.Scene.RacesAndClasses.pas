unit Trollhunter.Scene.RacesAndClasses;

interface

uses Trollhunter.Types, uScenes;

type
  TVScene = class(TScene)
  private
    DX: UInt;
  public
    procedure Render; override;
  end;

type
  TSceneRace = class(TVScene)
  public
    procedure ReRoll;
    procedure SelRand;
    procedure Render; override;
    procedure Update(var Key: UInt); override;
    class procedure RenderInfo;
  end;

type
  TSceneClass = class(TVScene)
  public
    procedure ReRoll;
    procedure SelRand;
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

uses Math, BearLibTerminal, uLanguage, Trollhunter.Terminal, uPlayer, uAttribute, uAbility,
  uGame, uRace, uClass, Trollhunter.UI, Trollhunter.Statistic, uSkill,
  Trollhunter.Player.Types;

var
  PrmAt: array [atStr .. atMana] of UInt;

  { TVScene }

procedure TVScene.Render;
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
    (Game.IfThen(Player.Sex = sxMale, _('Male'), _('Female')), 'Lush'));
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
    Terminal.Print(5, Y, AName);
    Inc(I);
    Inc(Y);
  end;

begin
  UI.Title(_('Choose a race'));
  I := 0;
  Y := 2;
  for R := Low(TRaceEnum) to High(TRaceEnum) do
    Add(Races.GetName(R));

  inherited Render;

  Terminal.ForegroundColor(clGray);
  Terminal.Print(DX, CY - (CY div 2), CX, CY,
    _(Races.GetDescription(Player.HRace)), TK_ALIGN_BOTTOM);

  AddKey('Enter', _('Confirm'));
  AddKey('Esc', _('Back'));
  AddKey('?', _('Help'), True);
end;

class procedure TSceneRace.RenderInfo;
begin

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

{ TSceneClass }

procedure TSceneClass.Render;
var
  I: UInt;
  C: TClassEnum;

  procedure Add(const AName: string);
  var
    L: Char;
  begin
    L := Chr(I + Ord('A'));
    Terminal.ForegroundColor(clWhite);
    Terminal.Print(1, Y, UI.KeyToStr(L));
    if (C = Player.HClass) then
      Terminal.ForegroundColor(clYellow)
    else
      Terminal.ForegroundColor(clWhite);
    Terminal.Print(5, Y, AName);
    Inc(I);
    Inc(Y);
  end;

begin
  UI.Title(_('Choose a class'));
  I := 0;
  Y := 2;
  for C := Low(TClassEnum) to High(TClassEnum) do
    Add(uClass.Classes.GetName(C));

  inherited Render;

  Terminal.Print(DX, 17, _('Items') + ': ' + Terminal.Colorize
    (Classes.GetItems(Player.HClass), 'Lush'));

  Terminal.Print(DX, 19, _('Skills') + ': ' + Terminal.Colorize
    (Classes.GetSkills(Player.HClass), 'Lush'));

  Terminal.ForegroundColor(clGray);
  Terminal.Print(DX, CY - (CY div 2), CX, CY,
    _(Classes.GetDescription(Player.HClass)), TK_ALIGN_BOTTOM);

  AddKey('Enter', _('Confirm'));
  AddKey('Esc', _('Back'));
  AddKey('?', _('Help'), True);
end;

procedure TSceneClass.ReRoll;
var
  V: TClassProp;
begin
  V := ClassProp[Player.HClass];

  // Attributes
  Player.Attributes.SetPrm(atStr, Math.RandomRange(V.Strength.Min,
    V.Strength.Max + 1) + PrmAt[atStr]);
  Player.Attributes.SetPrm(atDex, Math.RandomRange(V.Dexterity.Min,
    V.Dexterity.Max + 1) + PrmAt[atDex]);
  Player.Attributes.SetPrm(atWil, Math.RandomRange(V.Willpower.Min,
    V.Willpower.Max + 1) + PrmAt[atWil]);
  Player.Attributes.SetPrm(atPer, Math.RandomRange(V.Perception.Min,
    V.Perception.Max + 1) + PrmAt[atPer]);

  // Life and Mana
  Player.Attributes.SetPrm(atLife, Math.RandomRange(V.Life.Min, V.Life.Max + 1)
    + PrmAt[atLife]);
  Player.Attributes.SetPrm(atMana, Math.RandomRange(V.Mana.Min, V.Mana.Max + 1)
    + PrmAt[atMana]);
end;

procedure TSceneClass.SelRand;
var
  C: TClassEnum;
begin
  C := Player.HClass;
  repeat
    Player.HClass := TClassEnum(Math.RandomRange(0, Ord(High(TClassEnum)) + 1));
  until (C <> Player.HClass);
end;

procedure TSceneClass.Update(var Key: UInt);
var
  I: Int;
begin
  case Key of
    TK_A .. TK_Z:
      begin
        I := Ord(Key) - Ord(TK_A);
        if (I > Ord(High(TClassEnum))) then
          Exit;
        Player.HClass :=
          TClassEnum(Math.EnsureRange(I, 0, Ord(High(TClassEnum))));
        ReRoll;
      end;
    TK_ENTER, TK_KP_ENTER:
      begin
        Scenes.SetScene(scTalents, scClass);
      end;
    TK_ESCAPE:
      begin
        Scenes.SetScene(scRace);
      end;
    TK_BACKSPACE:
      begin
        SelRand;
        ReRoll;
      end;
    TK_SLASH:
      Scenes.SetScene(scHelp, scClass);
    TK_SPACE:
      ReRoll;
  end;
end;

end.
