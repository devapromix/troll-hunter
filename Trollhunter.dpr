program Trollhunter;

uses
  SysUtils,
  BearLibTerminal in 'BearLibTerminal.pas',
  Common in 'Common.pas',
  Scenes in 'Scenes.pas';

var
  Key: Word = 0;

begin
  terminal_open;
  terminal_refresh;
  repeat
    if terminal_has_input() then
    begin
      Key := terminal_read();
    end;
  until(Key = TK_CLOSE);
  terminal_close;
end.
