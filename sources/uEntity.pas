unit uEntity;

interface

uses uTypes, uAbility, uMap;

type
  TEntity = class(TObject)
  private
    FX: UInt;
    FY: UInt;
    FZ: TMapEnum;
  public
    constructor Create;
    destructor Destroy; override;
    property X: UInt read FX write FX;
    property Y: UInt read FY write FY;
    property Z: TMapEnum read FZ write FZ;
    function GetDist(ToX, ToY: Single): UInt;
    function GetCapit(S: string): string;
    function GetDescAn(S: string): string;
    function GetDescThe(S: string): string;
    function GetPureText(S: string): string;
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

function TEntity.GetDist(ToX, ToY: Single): UInt;
begin
  Result := Round(Sqrt(Sqr(ToX - X) + Sqr(ToY - Y)));
end;

function TEntity.GetPureText(S: string): string;
var
  I: Int;
  B: Boolean;
begin
  B := True;
  Result := '';
  S := StringReplace(S, '[[', '[', [rfReplaceAll]);
  S := StringReplace(S, ']]', ']', [rfReplaceAll]);
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
