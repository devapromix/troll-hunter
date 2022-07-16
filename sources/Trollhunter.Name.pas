unit Trollhunter.Name;

interface

type
  TName = class(TObject)
    function Gen(): string;
  end;

implementation

uses
  SysUtils,
  Classes,
  Trollhunter.Error;

function TName.Gen: string;
var
  S: array [0 .. 2] of TStringList;
  I: 0 .. 2;
begin
  Result := '';
  try
    for I := 0 to 2 do
      S[I] := TStringList.Create;
    S[0].DelimitedText := '"Ab","Ac","Ad","Af","Agr","Ast","As","Al","Adw",' +
      '"Adr","Adk","Ar","B","Br","C","Cr","Ch","Cad","D","Dr","Dw","Ed","Eth",'
      + '"Et","Er","El","Eow","F","Fr","G","Gr","Gw","Gal","Gl","Gn","H","Ha",'
      + '"Ib","Jer","K","Ka","Ked","L","Loth","Lar","Leg","M","Mir","N","Nan",'
      + '"Nyd","Ol","Oc","On","P","Pr","R","Rh","S","Sev","T","Tr","Th","V","Vz",'
      + '"Y", "Z", "W", "Wic", "Wid"';
    S[1].DelimitedText := '"a","ae","au","ao","are","ale","ali","ay","ardo",' +
      '"e","ei","ea","eri","era","ela","eli","enda","erra","i","ia","ie",' +
      '"ire","ira","ila","ili","ira","igo","o","oa","oi","oe","ore","u","y"';
    S[2].DelimitedText := '"a","and","b","bwyn","baen","bard","c","ctred",' +
      '"cred","ch","can","d","dan","din","don","der","dric","dfrid","dus","f","g",'
      + '"gord","gan","l","li","lgrin","lin","lith","lath","loth","ld","ldric",'
      + '"ldan","m","mas","mos","mar","mond","n","nydd","nidd","nnon","nwan",' +
      '"nyth","nad","nn","nnor","nd","p","r","ron","rd","s","son","sh","seth",'
      + '"sean","t","th","tha","tlan","trem","tram","v","vudd","w","wan","win",'
      + '"wid","wod","wyf","wyn","wyr","wys","wyth","wyz"';
    for I := 0 to 2 do
    begin
      Result := Result + S[I][Random(S[I].Count - 1)];
      S[I].Free;
    end;
  except
    on E: Exception do
      Error.Add('Name.Gen', E.Message);
  end;
end;

end.
