unit Trollhunter.Scene.Talents;

interface

uses
  Trollhunter.Types,
  Trollhunter.Talent,
  Trollhunter.Scenes;

type
  TSceneTalents = class(TScene)
  private
    FTalent: UInt;
  public
    property Talent: UInt read FTalent write FTalent;
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

uses
  Math,
  SysUtils,
  BearLibTerminal,
  Trollhunter.Game,
  Trollhunter.Player,
  Trollhunter.Attribute,
  Trollhunter.UI.Log,
  Trollhunter.Terminal,
  Trollhunter.UI;

procedure TSceneTalents.Render;
var
  V, I: UInt;
  T: TTalentEnum;

  procedure Add(const S, H: string; F: boolean = True); overload;
  var
    C: char;
  begin
    C := Chr(V + Ord('A'));
    if F then
      Terminal.Print(1, Y, UI.KeyToStr(C))
    else
    begin
      Terminal.ForegroundColor(clWhite);
      Terminal.Print(1, Y, '[[' + C + ']]');
    end;
    Terminal.ForegroundColor(clWhite);
    Terminal.Print(5, Y, S);
    Terminal.ForegroundColor(clGray);
    Terminal.Print(30, Y, H);
    Inc(Y);
    Inc(V);
  end;

begin
  if Player.Talents.IsPoint then
    UI.Title('Choose a talent')
  else
    UI.Title('Talents');

  V := 0;
  Y := 2;
  UI.FromAToZ;

  Terminal.ForegroundColor(clGray);
  for T := Succ(Low(TTalentEnum)) to High(TTalentEnum) do
    if (TalentBase[T].Level = Player.Attributes.Attrib[atLev].Value) and
      Player.Talents.IsAvailable(T) then
      Add(Player.Talents.GetName(T), Player.Talents.GetDescription(T),
        Player.Talents.IsPoint);

  if Mode.Game then
    MsgLog.Render(2, True);

  if Player.Talents.IsPoint then
  begin
    AddKey('A-Z', 'Select a talent');
    if Mode.Game then
      AddKey('Space', 'Show Learned Talents');
    AddKey('Esc', 'Close', 'Back', True);
  end
  else
  begin
    AddKey('Space', 'Show Learned Talents');
    AddKey('Esc', 'Close', 'Back', True);
  end;
end;

procedure TSceneTalents.Update(var Key: UInt);
var
  K: UInt;
begin
  case Key of
    TK_ESCAPE:
    begin
      if not Mode.Game then
        Player.Talents.Clear;
      Scenes.GoBack;
    end;
    TK_SPACE:
      if Mode.Game then
        Scenes.SetScene(scLearnedTalents);
    TK_A .. TK_Z, TK_ENTER, TK_KP_ENTER:
    begin
      if Player.Talents.IsPoint then
      begin
        case Key of
          TK_A .. TK_Z:
          begin
            K := Key - TK_A;
            if Mode.Game then
              Player.Talents.DoTalent(K)
            else if (K <= 5) then
            begin
              Self.Talent := K;
              Scenes.SetScene(scName);
            end;
          end;
          TK_ENTER, TK_KP_ENTER:
            if Mode.Wizard and not Mode.Game then
            begin
              Self.Talent := Math.RandomRange(0, 5);
              Scenes.SetScene(scName);
            end;
        end;
      end;
    end;
  end;
end;

end.
