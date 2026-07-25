unit Trollhunter.Player.Name;

interface

uses
  Trollhunter.Player.Types;

function GetRandomPlayerName(const AGender: TSexEnum): string;

implementation

uses
  Math,
  SysUtils,
  Trollhunter.UI.Log;

type
  TGender = (gnMale, gnFemale);

function GetRandomPlayerName(const AGender: TSexEnum): string;
const
  Prefix: array[0..39] of string = (
    'Al','Ar','Bal','Bel','Bor','Cal','Cor','Dar','Dor','Dra',
    'El','Fal','Fen','Gal','Gar','Hal','Jar','Kae','Kel','Kor',
    'Lor','Mal','Mor','Nar','Nor','Or','Per','Qua','Rav','Syl',
    'Tal','Ther','Tor','Ul','Val','Var','Vor','Xa','Yor','Zar'
  );

  Middle: array[0..29] of string = (
    'a','e','i','o','u',
    'ae','ai','ea','ei','ia',
    'an','ar','en','er','el',
    'in','ir','il','on','or',
    'ol','un','ur','ul','ath',
    'eth','ion','orn','yr','end'
  );

  MaleSuffix: array[0..39] of string = (
    'd','dr','g','k','l','m','n','r','th','v',
    'dor','drin','grim','gar','gor','kan','kor','lan','mar','mir',
    'nar','nir','rak','ric','rin','rok','thor','tur','var','vik',
    'vor','wyn','zar','zor','ian','ion','ius','ald','orn','ric'
  );

  FemaleSuffix: array[0..39] of string = (
    'a','ia','ie','ea','elle','enna','eth','iel','ina','ira',
    'issa','lyn','lia','lena','mira','nna','ria','ris','ssa','tha',
    'thea','wen','wyn','yra','yssa','ara','elle','essa','ora','una',
    'arae','etha','lith','riel','siel','via','yana','ylla','issa','iel'
  );

  MaleEnding: array[0..19] of string = (
    '','',
    'n','r','s','th','d','k',
    'or','ar','on','us','as','ric',
    'dor','mir','grim','thor','gar','ion'
  );

  FemaleEnding: array[0..19] of string = (
    '','',
    'a','ia','ra','na','la','sa',
    'elle','ine','iah','ara','ira','essa',
    'wyn','riel','lith','issa','ora','eth'
  );

var
  S: string;
begin
  S := Prefix[Random(Length(Prefix))];

  // У 50% випадків додаємо середній склад
  if Random(2) = 0 then
    S := S + Middle[Random(Length(Middle))];

  if AGender = sxMale then
    S := S +
         MaleSuffix[Random(Length(MaleSuffix))] +
         MaleEnding[Random(Length(MaleEnding))]
  else
    S := S +
         FemaleSuffix[Random(Length(FemaleSuffix))] +
         FemaleEnding[Random(Length(FemaleEnding))];

  S[1] := UpCase(S[1]);

  Result := S;
end;

end.
