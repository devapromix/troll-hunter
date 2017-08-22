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
  T: array [0 .. 17] of string =
    (
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
    '        \|                                                                                          '
    );

  { TLogo }

constructor TLogo.Create;
var
  X, Y, L: Integer;
begin
  L := Length(T[0]);
  for Y := 0 to 17 do
  begin
    FL[Y] := '';
    for X := 1 to L do
    begin
      case T[Y][X] of
        '_': FL[Y] := FL[Y] + '[color=light gray]' + T[Y][X] + '[/color]';
        '\': FL[Y] := FL[Y] + '[color=gray]' + T[Y][X] + '[/color]';
        '/': FL[Y] := FL[Y] + '[color=gray]' + T[Y][X] + '[/color]';
        '(': FL[Y] := FL[Y] + '[color=gray]' + T[Y][X] + '[/color]';
        ')': FL[Y] := FL[Y] + '[color=gray]' + T[Y][X] + '[/color]';
        '|': FL[Y] := FL[Y] + '[color=dark gray]' + T[Y][X] + '[/color]';
        '.': FL[Y] := FL[Y] + '[color=dark red]' + T[Y][X] + '[/color]';
        ':': FL[Y] := FL[Y] + '[color=red]' + T[Y][X] + '[/color]';
        'x': FL[Y] := FL[Y] + '[color=orange]' + T[Y][X] + '[/color]';
        'X': FL[Y] := FL[Y] + '[color=dark yellow]' + T[Y][X] + '[/color]';
        else FL[Y] := FL[Y] + T[Y][X];
      end;
    end;
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
