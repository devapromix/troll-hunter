unit Trollhunter.Player.Background;

interface

procedure GeneratePlayerBackground();
function GetPlayerBackground(): string;

implementation

uses
  Math,
  SysUtils,
  Classes,
  Trollhunter.Terminal;

var
  LBackground: string;

// Generate a random player's background (from Kharne and UMoria roguelikes)
procedure GeneratePlayerBackground();
var
  I: (cpChild, cpClass, cpParent, cpCredit, cpBackground, cpEyeType,
    cpEyeColour, cpHairStyle, cpHairColour, cpComplexion);
  SL: array [Low(I) .. High(I)] of TStringList;
begin
  Randomize;
  LBackground := '';
  for I := Low(I) to High(I) do
    SL[I] := TStringList.Create;
  try
    SL[cpChild].DelimitedText :=
      '"an only child","one of two children",' +
      '"one of many children","the only surviving child","one of several children",'
      +
      '"the illegitimate but acknowledged child","the illegitimate and unacknowledged child"';
    SL[cpClass].DelimitedText :=
      '"lower-class", "middle-class","upper-class"';
    SL[cpParent].DelimitedText :=
      '"mercenary","merchant","businessman","titled noble",' +
      '"craftsman","soldier","templar","priest","guildsman","townsman"';
    SL[cpBackground].DelimitedText :=
      '"contented","peaceful","troubled","settled","disturbed"';
    SL[cpCredit].DelimitedText :=
      '"a credit to","a disgrace to","the black sheep of"';
    SL[cpEyeType].DelimitedText :=
      '"dull","unusually piercing","piercing","striking","dark"';
    SL[cpEyeColour].DelimitedText :=
      '"grey","violet","green","blue","brown","blue-gray"';
    SL[cpHairStyle].DelimitedText :=
      '"wavy","curly","straight","short","long"';
    SL[cpHairColour].DelimitedText :=
      '"auburn","blonde","black","dark","red","ginger","grey","brown"';
    SL[cpComplexion].DelimitedText :=
      '"an average","a sallow","a fair","a dark","a light"';

    LBackground :=
      Format(Terminal.Colorize(
      'You are %s of a %s %s. You had a %s upbringing and you ' +
      'are %s the family. You have %s %s eyes, %s %s hair, and %s complexion.',
      'Grey'), [SL[cpChild][Random(SL[cpChild].Count - 1)],
      SL[cpClass][Random(SL[cpClass].Count - 1)],
      SL[cpParent][Random(SL[cpParent].Count - 1)],
      SL[cpBackground][Random(SL[cpBackground].Count - 1)],
      SL[cpCredit][Random(SL[cpCredit].Count - 1)],
      SL[cpEyeType][Random(SL[cpEyeType].Count - 1)],
      SL[cpEyeColour][Random(SL[cpEyeColour].Count - 1)],
      SL[cpHairStyle][Random(SL[cpHairStyle].Count - 1)],
      SL[cpHairColour][Random(SL[cpHairColour].Count - 1)],
      SL[cpComplexion][Random(SL[cpComplexion].Count - 1)]]);
  finally
    for I := Low(I) to High(I) do
      FreeAndNil(SL[I]);
  end;
end;

function GetPlayerBackground(): string;
begin
  Result := LBackground;
end;

end.
