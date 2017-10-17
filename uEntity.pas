unit uEntity;

interface

uses uAbility, uMap;

type
  TEntity = class(TObject)
  private
    FX: Byte;
    FY: Byte;
    FZ: TMapEnum;
  public
    constructor Create;
    destructor Destroy; override;
    property X: Byte read FX write FX;
    property Y: Byte read FY write FY;
    property Z: TMapEnum read FZ write FZ;
    function GetDist(ToX, ToY: Single): Word;
    function GetCapit(S: string): string;
    function GetDescAn(S: string): string;
    function GetDescThe(S: string): string;
    function GetPureText(const S: string): string;
  end;

implementation

uses SysUtils, uGame;

{ TEntity }

constructor TEntity.Create;
begin

end;

destructor TEntity.Destroy;
begin

  inherited;
end;

function TEntity.GetCapit(S: string): string;
begin
  if (Trim(S) <> '') then
    Result := UpCase(S[1]) + Copy(S, 2, Length(S))
      else Result := '(???)' + S;
end;

function TEntity.GetDescAn(S: string): string;
begin
  if (Trim(S) <> '') then
    Result := LowerCase(S)
      else Result := '(???)' + S;
  if (Game.Language.Current <> 'en') then
    Exit;
  if (Result[1] in ['a', 'e', 'i', 'o', 'u']) then
    Result := 'an ' + Result
  else
    Result := 'a ' + Result;
end;

function TEntity.GetDescThe(S: string): string;
begin
  if (Trim(S) <> '') then
    Result := LowerCase(S)
      else Result := '(???)' + S;
  if (Game.Language.Current <> 'en') then
    Exit;
  Result := 'the ' + Result;
end;

function TEntity.GetDist(ToX, ToY: Single): Word;
begin
  Result := Round(Sqrt(Sqr(ToX - X) + Sqr(ToY - Y)));
end;

function TEntity.GetPureText(const S: string): string;
var
  I: Integer;
  B: Boolean;
begin
  B := True;
  Result := '';
  for I := 1 to Length(S) do
  begin
    if (S[I] = '[') then
      B := False;
    if B then
      Result := Result + S[I];
    if (S[I] = ']') then
      B := True;
  end;
end;

end.
