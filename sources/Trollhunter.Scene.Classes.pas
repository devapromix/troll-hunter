unit Trollhunter.Scene.Classes;

interface

uses
  Trollhunter.Types,
  Trollhunter.Player.Classes,
  Trollhunter.Scenes,
  Trollhunter.Scene.Races;

type
  TSceneClass = class(TSceneRace)
  private
    procedure PrevScene;
    procedure NextScene;
    procedure SetClass(const AClassEnum: TClassEnum);
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
  Trollhunter.Helpers,
  Trollhunter.Language,
  Trollhunter.Terminal,
  Trollhunter.Player,
  Trollhunter.Attribute,
  Trollhunter.Game,
  Trollhunter.Player.Races,
  Trollhunter.UI,
  Trollhunter.UI.Images;

{ TSceneClass }

procedure TSceneClass.NextScene;
begin

  Scenes.SetScene(scName, scClass);
end;

procedure TSceneClass.PrevScene;
begin
  Scenes.SetScene(scRace);
end;

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
    Terminal.Print(5, Y, _(AName));
    Inc(I);
    Inc(Y);
  end;

  procedure RenderClassPict;
  var
    I: UInt;
  begin
    for I := 0 to 14 do
      Terminal.Print(60, I + 2, ClassPict[Player.HClass][I]);
  end;

begin
  inherited;
  Terminal.Clear;
  UI.Title(_('Choose a class'));
  I := 0;
  Y := 2;
  for C := Low(TClassEnum) to High(TClassEnum) do
    Add(PCClasses.GetName(C));

  RenderInfo;

  // Attributes
  Terminal.Print(DX, 10, _('Strength') + ': ' + Terminal.Colorize(UI.Icon(icStr) + BaseAttrib.ToString, 'NoMana') + '  ' + UI.Icon(icPlus, 'NoMana') +
    Terminal.Colorize(Races.Attrib[atStr], 'NoMana') + '  ' + UI.Icon(icPlus, 'Lush') + Terminal.Colorize(PCClasses.Attrib[atStr], 'Lush'));
  Terminal.Print(DX, 11, _('Dexterity') + ': ' + Terminal.Colorize(UI.Icon(icDex) + BaseAttrib.ToString, 'NoMana') + '  ' + UI.Icon(icPlus, 'NoMana')
    + Terminal.Colorize(Races.Attrib[atDex], 'NoMana') + '  ' + UI.Icon(icPlus, 'Lush') + Terminal.Colorize(PCClasses.Attrib[atDex], 'Lush'));
  Terminal.Print(DX, 12, _('Willpower') + ': ' + Terminal.Colorize(UI.Icon(icBook) + BaseAttrib.ToString, 'NoMana') + '  ' + UI.Icon(icPlus, 'NoMana')
    + Terminal.Colorize(Races.Attrib[atWil], 'NoMana') + '  ' + UI.Icon(icPlus, 'Lush') + Terminal.Colorize(PCClasses.Attrib[atWil], 'Lush'));
  Terminal.Print(DX, 13, _('Perception') + ': ' + Terminal.Colorize(UI.Icon(icLeaf) + BaseAttrib.ToString, 'NoMana') + '  ' + UI.Icon(icPlus,
    'NoMana') + Terminal.Colorize(Races.Attrib[atPer], 'NoMana') + '  ' + UI.Icon(icPlus, 'Lush') +
    Terminal.Colorize(PCClasses.Attrib[atPer], 'Lush'));

  // Life and Mana
  Terminal.Print(DX, 15, _('Life') + ': ' + Terminal.Colorize(UI.Icon(icLife) + BaseLife.ToString, 'NoMana') + '  ' + UI.Icon(icPlus, 'NoMana') +
    Terminal.Colorize(Races.Attrib[atLife], 'NoMana') + '  ' + UI.Icon(icPlus, 'Lush') + Terminal.Colorize(PCClasses.Attrib[atLife], 'Lush'));
  Terminal.Print(DX, 16, _('Mana') + ': ' + Terminal.Colorize(UI.Icon(icMana) + BaseMana.ToString, 'NoMana') + '  ' + UI.Icon(icPlus, 'NoMana') +
    Terminal.Colorize(Races.Attrib[atMana], 'NoMana') + '  ' + UI.Icon(icPlus, 'Lush') + Terminal.Colorize(PCClasses.Attrib[atMana], 'Lush'));

  Terminal.Print(DX, 18, _('Items') + ': ' + Terminal.Colorize(PCClasses.GetItems(Player.HClass), 'Lush'));

  Terminal.Print(DX, 20, _('Skills') + ': ' + Terminal.Colorize(PCClasses.GetSkills(Player.HClass), 'Lush'));

  // Description
  Terminal.ForegroundColor(clGray);
  Terminal.Print(DX, dsTop, Round(CX * 1.4), Terminal.Screen.Height - 3, _(PCClasses.GetDescription(Player.HClass)), TK_ALIGN_TOP);
  RenderClassPict;

  AddKey('Enter', _('Confirm'));
  AddKey('Esc', _('Back'));
  AddKey('?', _('Help'), True);
end;

procedure TSceneClass.ReRoll;
var
  Prop: TClassProp;
begin
  Prop := ClassProp[Player.HClass];

  // Attributes
  PCClasses.Attrib[atStr] := Math.RandomRange(Prop.Strength.Min, Prop.Strength.Max + 1);
  PCClasses.Attrib[atDex] := Math.RandomRange(Prop.Dexterity.Min, Prop.Dexterity.Max + 1);
  PCClasses.Attrib[atWil] := Math.RandomRange(Prop.Willpower.Min, Prop.Willpower.Max + 1);
  PCClasses.Attrib[atPer] := Math.RandomRange(Prop.Perception.Min, Prop.Perception.Max + 1);

  // Life and Mana
  PCClasses.Attrib[atLife] := Math.RandomRange(Prop.Life.Min, Prop.Life.Max + 1);
  PCClasses.Attrib[atMana] := Math.RandomRange(Prop.Mana.Min, Prop.Mana.Max + 1);
end;

procedure TSceneClass.SelRand;
var
  C: TClassEnum;
begin
  C := Player.HClass;
  repeat
    SetClass(TClassEnum(Math.RandomRange(0, Ord(High(TClassEnum)) + 1)));
  until (C <> Player.HClass);
end;

procedure TSceneClass.SetClass(const AClassEnum: TClassEnum);
begin
  Player.HClass := AClassEnum;
  ReRoll;
end;

procedure TSceneClass.Update(var Key: UInt);
var
  I: Int;
begin
  case Key of
    TK_LEFT, TK_KP_4:
      SetClass(Low(TClassEnum));
    TK_RIGHT, TK_KP_6:
      SetClass(High(TClassEnum));
    TK_UP, TK_KP_8:
      if Player.HClass > Low(TClassEnum) then
        SetClass(Pred(Player.HClass));
    TK_DOWN, TK_KP_2:
      if Player.HClass < High(TClassEnum) then
        SetClass(Succ(Player.HClass));
    TK_A .. TK_Z:
      begin
        I := Ord(Key) - Ord(TK_A);
        if (I > Ord(High(TClassEnum))) then
          Exit;
        SetClass(TClassEnum(Math.EnsureRange(I, 0, Ord(High(TClassEnum)))));
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
      Scenes.SetScene(scHelp, scClass);
    TK_SPACE:
      ReRoll;
  end;
end;

end.
