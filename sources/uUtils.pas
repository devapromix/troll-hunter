unit uUtils;

interface

type
  Utils = class(TObject)
  public
    class function GetPath(SubDir: string): string;
  end;

implementation

uses SysUtils;

class function Utils.GetPath(SubDir: string): string;
begin
  Result := ExtractFilePath(ParamStr(0));
  Result := IncludeTrailingPathDelimiter(Result + SubDir);
end;

end.
