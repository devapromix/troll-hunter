unit uLogo;

interface

type
  TLogo = class(TObject)
  private
    FX: Byte;
    FL: array [0 .. 17] of string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Render;
  end;

var
  Logo: TLogo;

implementation

uses SysUtils, BearLibTerminal, uTerminal, uGame;

const
  T: array [0 .. 17] of string = (
    ' ______________                                                                                     ',
    '| ____  . ____ |              ___  ___  ___                               ___                       ',
    '|/    |. |    \|              \ .\ \ .\ \. \                              \ .\                      ',
    '      | .|                    | .| |. | | .|                              | .|                      ',
    '      |. |____  ___   ______  |. | | .| |. | ____ ____   ____ ____  ____  |. |__   ______ ____  ___ ',
    '      |::|\:::|/:::\ /::::::\ |::| |::| |::|/::::\\:::\  \:::|\:::|/::::\ |:::::| /::::::\\:::|/:::\',
    '      |xx| |xx|  \x||xx(  )xx||xx| |xx| |xx|   \xx\|xx|   |xx| |xx|   \xx\|xx|   |xx(__)xx||xx|  \x|',
    '      |xx| |xx|     |xx|  |xx||xx| |xx| |xx|   |xx||xx|   |xx| |xx|   |xx||xx|   |xx|xxxxx||xx|     ',
    '      |XX| |XX|     |XX(__)XX||XX| |XX| |XX|   |XX||XX\___|XX| |XX|   |XX||XX\___|XX|_____ |XX|     ',
    '      |XX| \XXX\     \XXXXXX/ \XXX\\XXX\\XXX\  \XXX\\XXXX/|XXX\\XXX\  \XXX\\XXXX/ \XXXXXX/ \XXX\    ',
    '      |XX|                                                                                          ',
    '      |XX|                                                                                          ',
    '      |XX|                                                                                          ',
    '     _|XX|                                                                                          ',
    '     \XXX|                                                                                          ',
    '      \XX|                                                                                          ',
    '       \X|                                                                                          ',
    '        \|                                                                                          ');

{ TLogo }

constructor TLogo.Create;
var
  I: Byte;
begin
  for I := 0 to 17 do
  begin
    FL[I] := T[I];
  end;
end;

destructor TLogo.Destroy;
begin

  inherited;
end;

procedure TLogo.Render;
var
  I: Byte;
begin
  FX := Screen.Width div 2;
  for I := 0 to 17 do
    Terminal.Print(FX, I + 3, FL[I], TK_ALIGN_CENTER);
end;

initialization
  Logo := TLogo.Create;

finalization
  FreeAndNil(Logo);

end.
