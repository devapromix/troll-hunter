unit uCalendar;

interface

type
  TCalendar = class(TObject)
  private
    FMinute: Byte;
    FHour: Byte;
    FDay: Byte;
    FDayOfWeek: Byte;
    FMonth: Byte;
    FYear: Word;
  public
    constructor Create(const ADay, AMonth: Byte; const AYear: Word);
    property Minute: Byte read FMinute;
    property Hour: Byte read FHour;
    property Day: Byte read FDay;
    property DayOfWeek: Byte read FDayOfWeek;
    property Month: Byte read FMonth;
    property Year: Word read FYear;
    function DaysPerMonth(AMonth: Byte): Byte;
    function DaysThisMonth: Byte;
    function GetMonthName(AMonth: Byte = 0): string;
    function GetDayName: string;
    function GetTime: string;
    function GetTimeStr: string;
    procedure OnHour;
    procedure OnDay;
    procedure OnWeek;
    procedure OnMonth;
    procedure OnYear;
    procedure Turn;
  end;

var
  Calendar: TCalendar;

implementation

uses SysUtils, Math, GNUGetText;

{ TCalendar }

constructor TCalendar.Create(const ADay, AMonth: Byte; const AYear: Word);
begin
  FMinute := Math.RandomRange(10, 30);
  FHour := Math.RandomRange(1, 6);
  FDay := ADay;
  FDayOfWeek := ADay;
  FMonth := AMonth;
  FYear := AYear;
end;

function TCalendar.DaysPerMonth(AMonth: Byte): Byte;
const
  DaysInMonth: array[1..12] of Integer = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
begin
  Result := DaysInMonth[AMonth];
end;

function TCalendar.DaysThisMonth: Byte;
begin
  Result := DaysPerMonth(Month)
end;

function TCalendar.GetDayName: string;
begin
  case DayOfWeek of
    1: Result := _('Monday');
    2: Result := _('Tuesday');
    3: Result := _('Wednesday');
    4: Result := _('Thursday');
    5: Result := _('Friday');
    6: Result := _('Saturday');
    else Result := _('Sunday');
  end;
end;

function TCalendar.GetMonthName(AMonth: Byte): string;
begin
  if (AMonth = 0) then AMonth := FMonth;
  case AMonth of
     1: Result := _('January');
     2: Result := _('February');
     3: Result := _('March');
     4: Result := _('April');
     5: Result := _('May');
     6: Result := _('June');
     7: Result := _('July');
     8: Result := _('August');
     9: Result := _('September');
    10: Result := _('October');
    11: Result := _('November');
    else Result := _('December');
  end;
end;

function TCalendar.GetTime: string;
begin
  Result := Format('%d:%d', [Hour, Minute]);
end;

function TCalendar.GetTimeStr: string;
begin
  case Hour of
    6..8: Result := _('Day');
    9..17: Result := _('Day');
    18..20: Result := _('Day');
    else Result := _('Night');
  end;
end;

procedure TCalendar.OnDay;
begin

end;

procedure TCalendar.OnHour;
begin

end;

procedure TCalendar.OnMonth;
begin

end;

procedure TCalendar.OnWeek;
begin

end;

procedure TCalendar.OnYear;
begin

end;

procedure TCalendar.Turn;
begin
  Inc(FMinute, Math.RandomRange(10, 15));
  if (Minute > SysUtils.MinsPerHour - 1) then
  begin
    FMinute := Minute - SysUtils.MinsPerHour;
    Inc(FHour);
    OnHour();
  end;
  if (Hour > SysUtils.HoursPerDay - 1) then
  begin
    FHour := 0;
    Inc(FDay);
    Inc(FDayOfWeek);
    OnDay();
  end;
  if (DayOfWeek > 7) then
  begin
    FDayOfWeek := 1;
    OnWeek();
  end;
  if (Day > DaysThisMonth) then
  begin
    FDay := 1;
    Inc(FMonth);
    OnMonth();
  end;
  if (Month > 12) then
  begin
    FMonth := 1;
    Inc(FYear);
    OnYear();
  end;
end;

initialization
  Calendar := TCalendar.Create(1, 1, 1250);

finalization
  FreeAndNil(Calendar);

end.
