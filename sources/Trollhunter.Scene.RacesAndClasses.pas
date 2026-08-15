unit Trollhunter.Scene.RacesAndClasses;

interface

uses
  Trollhunter.Types,
  Trollhunter.Scenes;

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

uses
  Math,
  BearLibTerminal,
  Trollhunter.Terminal,
  Trollhunter.Player,
  Trollhunter.Attribute,
  Trollhunter.Ability,
  Trollhunter.Game,
  Trollhunter.Player.Races,
  Trollhunter.Player.Classes,
  Trollhunter.UI,
  Trollhunter.Statistic,
  Trollhunter.Skill,
  Trollhunter.Player.Name,
  Trollhunter.Player.Types,
  Trollhunter.Player.Background,
  Trollhunter.Player.Helpers;

var
  PrmAt: array [atStr .. atMana] of UInt;

  { TVScene }

procedure TVScene.Render;
var
  LY: UInt;

  procedure Add(const ALabel, AValue: string);
  begin
    Terminal.Print(DX, LY, ALabel + ': ' + Terminal.Colorize(AValue, 'Lush'));
    Inc(LY);
  end;

begin
  DX := CX - (CX div 2);
  Terminal.ForegroundColor(clWhite);
  LY := 2;

  Add('Age', Player.Statictics.Get(stAge));
  Add('Height', Player.Statictics.Get(stHeight));
  Add('Weight', Player.Statictics.Get(stWeight));
  Add('Gender', Player.GenderStr);
  Add('Metabolism', Player.Statictics.Get(stMetabolism));

  // Attributes
  Inc(LY);
  Add('Strength', Player.Attributes.Attrib[atStr].Prm);
  Add('Dexterity', Player.Attributes.Attrib[atDex].Prm);
  Add('Willpower', Player.Attributes.Attrib[atWil].Prm);
  Add('Perception', Player.Attributes.Attrib[atPer].Prm);

  // Life and Mana
  Inc(LY);
  Add('Life', Player.Attributes.Attrib[atLife].Prm);
  Add('Mana', Player.Attributes.Attrib[atMana].Prm);
end;

{ TSceneRace }

procedure TSceneRace.Render;
var
  I: UInt;
  R: TRaceEnum;

  procedure Add(const AName: string);
  var
    C: char;
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
  UI.Title('Choose a race');
  I := 0;
  Y := 2;
  for R := Low(TRaceEnum) to High(TRaceEnum) do
    Add(Races.GetName(R));

  inherited Render;

  Terminal.ForegroundColor(clGray);
  Terminal.Print(CX - (CX div 2), CY - (CY div 2), CX, CY, GetPlayerBackground(),
    TK_ALIGN_BOTTOM);

  AddKey('Enter', 'Confirm');
  AddKey('Esc', 'Back');
  AddKey('?', 'Help', True);
end;

class procedure TSceneRace.RenderInfo;
begin

end;

procedure TSceneRace.ReRoll;
var
  V: TRaceProp;
  Age, Height, Weight, Metabolism: integer;
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
  Player.Attributes.SetPrm(atLife, Math.RandomRange(V.Life.Min, V.Life.Max + 1));
  PrmAt[atLife] := Player.Attributes.Attrib[atLife].Prm;
  Player.Attributes.SetPrm(atMana, Math.RandomRange(V.Mana.Min, V.Mana.Max + 1));
  PrmAt[atMana] := Player.Attributes.Attrib[atMana].Prm;

  GeneratePlayerBackground();

  Player.Name := GetRandomPlayerName();
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
      if (Player.Gender = gdMale) then
        Player.Gender := gdFemale
      else
        Player.Gender := gdMale;
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
  ItemsSize: TSize;

  procedure Add(const AName: string);
  var
    L: char;
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
  UI.Title('Choose a class');
  I := 0;
  Y := 2;
  for C := Low(TClassEnum) to High(TClassEnum) do
    Add(Trollhunter.Player.Classes.Classes.GetName(C));

  inherited Render;

  ItemsSize := Terminal.Print(DX, 16, CX + 15, 2, 'Items' + ': ' + Terminal.Colorize(
    Classes.GetItems(Player.HClass), 'Lush'), TK_ALIGN_TOP);

  Terminal.Print(DX, 18, 'Skills' + ': ' + Terminal.Colorize(
    Classes.GetSkills(Player.HClass), 'Lush'));

  Terminal.Print(DX, 20, 'Ability' + ': ' + Terminal.Colorize(
    Classes.GetAbility(Player.HClass), 'Lush'));

  Terminal.ForegroundColor(clGray);
  Terminal.Print(DX, CY - (CY div 2) + 2, CX, CY,
    Classes.GetDescription(Player.HClass), TK_ALIGN_BOTTOM);

  AddKey('Enter', 'Confirm');
  AddKey('Esc', 'Back');
  AddKey('?', 'Help', True);
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
  Player.Attributes.SetPrm(atLife, Math.RandomRange(V.Life.Min, V.Life.Max + 1) +
    PrmAt[atLife]);
  Player.Attributes.SetPrm(atMana, Math.RandomRange(V.Mana.Min, V.Mana.Max + 1) +
    PrmAt[atMana]);
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
