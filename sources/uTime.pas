unit uTime;

interface

procedure DoTime();

implementation

uses uCreatures, uBox;

procedure DoTime();
var
  T: Integer;
begin
  with Creatures.PC do
  begin
    T := Turns div 10;
    if (LastTurns <> T) then
    begin
      LastTurns := T;
      Day := Day + 1;
      if (Day > 7) then
      begin
        Day := 1;
        Week := Week + 1;
        if (Week > 4) then
        begin
          Week := 1;
          Month := Month + 1;
          if (Month > 12) then
          begin
            Month := 1;
            Year := Year + 1;
          end;
        end;
      end;
    end;
  end;
end;

end.
