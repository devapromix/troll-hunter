program langutil;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  Trollhunter.Language in '..\sources\Trollhunter.Language.pas';

var
  I, J: Integer;
  CurLangFileName: string;
  SL, DefSL, CurSL: TStringList;
  F: Boolean;

procedure ScanLanguagesDir;
var
  SR: TSearchRec;
  S: string;
begin
  try
    SL.Clear;
    if (FindFirst(TLanguage.GetPath('languages') + '*.lng', faAnyFile, SR) = 0)
    then
    begin
      repeat
        S := Trim(SR.Name);
        if (S = '') or (S = TLanguage.DefaultLanguage) then
          Continue;
        SL.Append(S);
      until (FindNext(SR) <> 0);
      FindClose(SR);
    end;
    SL.Sort;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end;

procedure LoadDefaultLanguage();
begin
  DefSL.LoadFromFile(TLanguage.GetPath('languages') + TLanguage.DefaultLanguage,
    TEncoding.UTF8);
end;

function IsLine(const S: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to CurSL.Count - 1 do
    if (S = Copy(CurSL[I], 1, Pos('=', CurSL[I]))) then
      Exit(True);
end;

begin
  SL := TStringList.Create;
  DefSL := TStringList.Create;
  CurSL := TStringList.Create;
  try
    try
      LoadDefaultLanguage();
      ScanLanguagesDir;
      for I := 0 to SL.Count - 1 do
      begin
        F := False;
        CurLangFileName := SL[I];
        Write(CurLangFileName + '... ');
        CurSL.LoadFromFile(TLanguage.GetPath('languages') + CurLangFileName,
          TEncoding.UTF8);
        for J := 0 to DefSL.Count - 1 do
        begin
          if IsLine(Trim(DefSL[J])) then
            Continue;
          CurSL.Append(DefSL[J]);
          F := True;
        end;
        CurSL.Sort;
        CurSL.SaveToFile(TLanguage.GetPath('languages') + CurLangFileName,
          TEncoding.UTF8);
        if F then
          Writeln('UPDATED')
        else
          Writeln('OK');
      end;
    except
      on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
    end;
    Writeln;
    Writeln('Press ENTER to exit...');
    ReadLn;
  finally
    CurSL.Free;
    DefSL.Free;
    SL.Free;
  end;

end.
