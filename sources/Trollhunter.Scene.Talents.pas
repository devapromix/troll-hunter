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
    function AvailableCount: UInt;
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
  V: UInt;
  T: TTalentEnum;
  IsMenu: boolean;

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
    if IsMenu and (V = Self.FTalent) then
      Terminal.ForegroundColor(clYellow)
    else
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

  IsMenu := (not Mode.Game) and Player.Talents.IsPoint;

  V := 0;
  Y := 2;
  UI.FromAToZ;

  Terminal.ForegroundColor(clGray);
  for T := Succ(Low(TTalentEnum)) to High(TTalentEnum) do
    if Player.Talents.IsAvailable(T) then
      Add(Player.Talents.GetLevelName(T, Player.Talents.NextLevel(T)),
        Player.Talents.GetDescription(T), Player.Talents.IsPoint);

  if Mode.Game then
    MsgLog.Render(2, True);

  if Player.Talents.IsPoint then
  begin
    AddKey('A-Z', 'Select a talent');
    if Mode.Game then
      AddKey('Space', 'Show Learned Talents');
    if IsMenu then
    begin
      AddKey('Enter', 'Confirm');
      AddKey('Esc', 'Back', True);
    end
    else
      AddKey('Esc', 'Close', 'Back', True);
  end
  else
  begin
    AddKey('Space', 'Show Learned Talents');
    AddKey('Esc', 'Close', 'Back', True);
  end;
end;

function TSceneTalents.AvailableCount: UInt;
var
  T: TTalentEnum;
begin
  Result := 0;
  for T := Succ(Low(TTalentEnum)) to High(TTalentEnum) do
    if Player.Talents.IsAvailable(T) then
      Inc(Result);
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
    TK_A .. TK_Z:
    begin
      if Player.Talents.IsPoint then
      begin
        K := Key - TK_A;
        if K >= Self.AvailableCount then
          Exit;
        if Mode.Game then
          Player.Talents.DoTalent(K)
        else
          Self.Talent := K;
      end;
    end;
    TK_ENTER, TK_KP_ENTER:
      if Player.Talents.IsPoint and not Mode.Game then
        Scenes.SetScene(scName);
    TK_BACKSPACE:
      if Player.Talents.IsPoint and not Mode.Game and Mode.Wizard and
        (Self.AvailableCount > 0) then
        Self.Talent := Math.RandomRange(0, Self.AvailableCount);
  end;
end;

end.
