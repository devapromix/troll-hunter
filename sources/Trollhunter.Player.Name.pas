unit Trollhunter.Player.Name;

interface

function GetRandomPlayerName(): string;

implementation

uses
  Math,
  SysUtils,
  Trollhunter.UI.Log;

function GetRandomPlayerName(): string;
const
  Prefix: array[0..29] of string = (
    'Al','Ar','Bel','Bor','Cal','Cor','Da','Dor','El','Fen',
    'Gal','Gar','Hal','Kor','Lor','Mal','Nar','Or','Ra','Syl',
    'Tal','Tor','Ul','Val','Vor','Xa','Za','Ka','Ther','Ith'
  );

  Middle: array[0..19] of string = (
    'a','e','i','o','u',
    'an','en','ar','or','ir',
    'el','al','on','in','ur',
    'ath','eth','orn','ion','yr'
  );

  Suffix: array[0..29] of string = (
    'dor','ric','rin','dan','thor','mir','ion','as','or','an',
    'en','or','is','eth','ar','orn','ain','ius','orn','yr',
    'ald','ric','grim','dor','ven','las','rak','mon','thas','wyn'
  );
begin
  Result :=
    Prefix[Random(Length(Prefix))] +
    Middle[Random(Length(Middle))] +
    Suffix[Random(Length(Suffix))];
end;

end.
