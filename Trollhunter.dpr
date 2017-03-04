program Trollhunter;

uses
  SysUtils,
  BearLibTerminal in 'BearLibTerminal.pas',
  uTerminal in 'uTerminal.pas',
  uCommon in 'uCommon.pas',
  uScenes in 'uScenes.pas';

var
  Key: Word = 0;

begin
  repeat
    if terminal_has_input() then
    begin
      Key := terminal_read();
    end;
  until(Key = TK_CLOSE);
end.
