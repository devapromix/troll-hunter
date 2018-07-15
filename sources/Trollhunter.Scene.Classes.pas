unit Trollhunter.Scene.Classes;

interface

uses
  Trollhunter.Types,
  Trollhunter.Scenes,
  Trollhunter.Scene.Races;

type
  TSceneClass = class(TSceneRace)
  private
    procedure PrevScene;
    procedure NextScene;
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
  Trollhunter.Language,
  Trollhunter.Terminal,
  Trollhunter.Player,
  Trollhunter.Attribute,
  Trollhunter.Game,
  Trollhunter.Player.Races,
  Trollhunter.Player.Classes,
  Trollhunter.UI;

{ TSceneClass }

procedure TSceneClass.NextScene;
begin
  Scenes.SetScene(scTalents, scClass);
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

begin
  UI.Title(_('Choose a class'));
  I := 0;
  Y := 2;
  for C := Low(TClassEnum) to High(TClassEnum) do
    Add(Trollhunter.Player.Classes.Classes.GetName(C));

  RenderInfo;

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
    TK_UP, TK_KP_8:
      if Player.HClass > Low(TClassEnum) then
        Player.HClass := Pred(Player.HClass);
    TK_DOWN, TK_KP_2:
      if Player.HClass < High(TClassEnum) then
        Player.HClass := Succ(Player.HClass);
    TK_A .. TK_Z:
      begin
        I := Ord(Key) - Ord(TK_A);
        if (I > Ord(High(TClassEnum))) then
          Exit;
        Player.HClass :=
          TClassEnum(Math.EnsureRange(I, 0, Ord(High(TClassEnum))));
        ReRoll;
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
