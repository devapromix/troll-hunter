program Trollhunter;

uses
  SysUtils,
  BearLibTerminal in 'BearLibTerminal.pas',
  uTerminal in 'uTerminal.pas',
  uCommon in 'uCommon.pas',
  uScenes in 'uScenes.pas';

var
  Key: Word = 0;
  Tick: Byte = 0;
  IsRender: Boolean = True;

begin
  repeat
    if IsRender then Stages.Render;
    Key := 0;
    if terminal_has_input() then
    begin
      Key := terminal_read();
      Stages.Update(Key);
      IsRender := True;
      Continue;
    end;
    if (Tick > 99) then
    begin
      Stages.Timer;
      Tick := 0;
      IsRender := True;
      Continue;
    end;
    if IsRender then Terminal.Refresh;
    Tick := Tick + 1;
    terminal_delay(10);
    IsRender := False;
  until (Key = TK_CLOSE);
end.

