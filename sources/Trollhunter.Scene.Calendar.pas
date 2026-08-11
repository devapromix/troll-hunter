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

uses
  Math,
  SysUtils,
  BearLibTerminal,
  Trollhunter.Terminal,
  Trollhunter.UI,
  Trollhunter.Map,
  Trollhunter.Statistic,
  Trollhunter.Player,
  Trollhunter.Calendar,
  Trollhunter.Weather,
  Trollhunter.Game;

{ TSceneCalendar }

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
  UI.Title('Calendar');

  Y := 10;
  Player.RenderWeather(CX, Y - 6, CX);
  Add('Turn', Player.Statictics.Get(stTurn));
  Add('Time', Calendar.GetTime, Calendar.GetTimeStr);
  Add('Day', Calendar.Day, Calendar.GetDayName);
  Add('Month', Calendar.Month, Calendar.GetMonthName);
  Add('Year', Calendar.Year);
  Add('Map', Map.Name);
  if (Map.Current = deDark_Wood) then
  begin
    Add('Wind', Weather.GetWindName);
    Add('Weather', Weather.GetWeatherName);
  end
  else
  begin
    Add('Wind', 'Unknown', 'Underground');
    Add('Weather', 'Unknown', 'Underground');
  end;

  AddKey('Esc', 'Close', True);
end;

procedure TSceneCalendar.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      Scenes.SetScene(scGame);
  end;
end;



end.
