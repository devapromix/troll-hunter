unit Trollhunter.Utils;

interface

type

  { Utils }

  Utils = class(TObject)
  public
    class function GetPath(SubDir: string): string;
    class procedure AppStr(var S: string; P: string; IsSep: boolean);
    class function Chance(const AValue: integer): boolean;
  end;

implementation

uses SysUtils;

class procedure Utils.AppStr(var S: string; P: string; IsSep: boolean);
begin
  if IsSep then
    S := S + ', ' + P
  else
    S := S + P;
end;

class function Utils.Chance(const AValue: integer): boolean;
begin
  if (AValue <= 0) then Exit(False);
  if (AValue >= 100) then Exit(True);
  Result := Random(100) < AValue;
end;

class function Utils.GetPath(SubDir: string): string;
begin
  Result := ExtractFilePath(ParamStr(0));
  Result := IncludeTrailingPathDelimiter(Result + SubDir);
end;

end.
