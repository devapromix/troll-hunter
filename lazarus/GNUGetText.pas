unit GNUGetText;

interface

function GetCurrentLanguage(): string;
function _(const S: string): string;

implementation

function GetCurrentLanguage(): string;
begin
  Result := 'en';
end;

function _(const S: string): string;
begin
  Result := S;
end;

end.
