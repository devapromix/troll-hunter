unit Trollhunter.Scene.Calendar;

interface

uses
  Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneCalendar = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

{ TSceneCalendar }

uses
  BearLibTerminal,
  Trollhunter.Terminal,
  Trollhunter.Helpers,
  Trollhunter.Game,
  Trollhunter.Player,
  Trollhunter.UI,
  Trollhunter.Language,
  Trollhunter.Map,
  Trollhunter.Statistic,
  Trollhunter.Calendar;

procedure TSceneCalendar.Render;

  procedure Add(const AText: string; AValue: string; AAdvValue: string = ''); overload;
  var
    S: string;
    X: UInt;
  begin
    X := Screen.Width div 3;
    S := '';
    if (AAdvValue <> '') then
      S := AAdvValue;
    Terminal.ForegroundColor(clWhite);
    Terminal.Print(X, Y, AText, TK_ALIGN_LEFT);
    Terminal.ForegroundColor(clGreen);
    Terminal.Print(X + 10, Y, AValue, TK_ALIGN_LEFT);
    if (S <> '') then
    begin
      Terminal.ForegroundColor(clLightBlue);
      Terminal.Print(X + 20, Y, AAdvValue, TK_ALIGN_LEFT);
    end;
    Inc(Y);
  end;

  procedure Add(const AText: string; AValue: Int; AAdvValue: string = ''); overload;
  begin
    Add(AText, AValue.ToString(), AAdvValue);
  end;

begin
  inherited;
  UI.Title(_('Calendar'));

  Y := 10;
  Player.RenderWeather(CX, Y - 6, CX);
  Add(_('Turn'), Player.Statictics.Get(stTurn));
  Add(_('Time'), Calendar.GetTime, Calendar.GetTimeStr);
  Add(_('Day'), Calendar.Day, Calendar.GetDayName);
  Add(_('Month'), Calendar.Month, Calendar.GetMonthName);
  Add(_('Year'), Calendar.Year);
  Add(_('Map'), Map.Name);
  Add(_('Wind'), '');
  Add(_('Weather'), '');

  AddKey('Esc', _('Close'), True);
end;

procedure TSceneCalendar.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      // Close
      Scenes.SetScene(scGame);
  end;
end;

end.
