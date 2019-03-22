unit Trollhunter.Scene.Mods;

interface

uses
  Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneMods = class(TScene)
  private
    FCur: UInt;
    FTop: UInt;
    FCount: UInt;
    procedure PrevScene;
    procedure NextScene;
  public
    constructor Create;
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

uses
  Math,
  SysUtils,
  Dialogs,
  BearLibTerminal,
  Trollhunter.UI,
  Trollhunter.Language,
  Trollhunter.Mods,
  Trollhunter.Terminal,
  Trollhunter.Helpers,
  Trollhunter.Game;

{ TSceneMods }

constructor TSceneMods.Create;
begin
  FCur := 0;
  FTop := 0;
  FCount := 0;
end;

procedure TSceneMods.NextScene;
begin

  Scenes.SetScene(scDifficulty);
end;

procedure TSceneMods.PrevScene;
begin
  Scenes.SetScene(scTitle);
end;

procedure TSceneMods.Render;
var
  I, J: UInt;

  procedure Add(const AName, ADescr: string);
  var
    C: Char;
  begin
    C := Chr(I + Ord('A'));
    Terminal.ForegroundColor(clWhite);
    Terminal.Print(1, Y, UI.KeyToStr(C));
    if (FCur = I) then
      Terminal.ForegroundColor(clYellow)
    else
      Terminal.ForegroundColor(clWhite);
    Terminal.Print(5, Y, _(AName));
    Terminal.ForegroundColor(clWhite);
    Terminal.Print(CX - (CX div 2), 2, _(AName));
    Terminal.Print(CX - (CX div 2), 4, Round(CX * 1.4), CY, ADescr, TK_ALIGN_TOP);
    Inc(I);
    Inc(Y);
  end;

begin
  inherited Render;
  UI.Title(_('Choose a module'));
  I := 0;
  Y := 2;

  FCount := GMods.ModNL.Count;
  for J := 0 to FCount - 1 do
    Add(GMods.ModNL[J], GMods.ModDL[J]);

  AddKey('Enter', _('Confirm'));
  AddKey('Esc', _('Back'), True);

end;

procedure TSceneMods.Update(var Key: UInt);
var
  I: Int;
begin
  case Key of
    TK_ENTER, TK_KP_ENTER:
      NextScene;
    TK_ESCAPE:
      PrevScene;
    TK_A .. TK_Z:
      begin
        I := Ord(Key) - Ord(TK_A);
        if (I > FCount - 1) then
          Exit;
        GMods.SetCurrent(GMods.ModFL[I]);
        NextScene;
      end;
  end;

end;

end.
