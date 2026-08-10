unit Trollhunter.Weather;

interface

type
  TWindEnum = (wdNone, wdN, wdNE, wdE, wdSE, wdS, wdSW, wdW, wdNW);

type
  TWeatherEnum = (wtClear, wtSunnyAndCloudy, wtCloudy, wtRain);

type
  TWeather = class(TObject)
  private
    FWeather: TWeatherEnum;
    FWind: TWindEnum;
  public
    constructor Create;
    property Weather: TWeatherEnum read FWeather;
    property Wind: TWindEnum read FWind;
    function GetWeatherName: string;
    function GetWindName: string;
    procedure Turn;
  end;

var
  Weather: TWeather = nil;

implementation

uses
  SysUtils,
  Math;

  { TWeather }

constructor TWeather.Create;
begin
  FWeather := wtClear;
  FWind := wdNone;
end;

function TWeather.GetWeatherName: string;
const
  CWeatherName: array [TWeatherEnum] of string =
    ('Clear', 'Sunny and Cloudy','Cloudy', 'Rain');
begin
  Result := CWeatherName[FWeather];
end;

function TWeather.GetWindName: string;
const
  CWindName: array [TWindEnum] of string = ('No wind', 'North', 'North-East',
    'East', 'South-East', 'South', 'South-West', 'West', 'North-West');
begin
  Result := CWindName[FWind];
end;

procedure TWeather.Turn;
begin
  FWeather := TWeatherEnum(Math.RandomRange(0, 3));
  FWind := TWindEnum(Math.RandomRange(0, 9));
end;

initialization

  Weather := TWeather.Create;

finalization

  FreeAndNil(Weather);

end.
