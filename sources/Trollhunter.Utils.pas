unit Trollhunter.Utils;

interface

type
  Utils = class(TObject)
  public
    class function GetPath(SubDir: string): string;
    class procedure AppStr(var S: string; P: string; IsSep: Boolean);
  end;

implementation

uses SysUtils;

class procedure Utils.AppStr(var S: string; P: string; IsSep: Boolean);
begin
  if IsSep then
    S := S + ', ' + P
  else
    S := S + P;
end;

class function Utils.GetPath(SubDir: string): string;
begin
  Result := ExtractFilePath(ParamStr(0));
  Result := IncludeTrailingPathDelimiter(Result + SubDir);
end;

end.
