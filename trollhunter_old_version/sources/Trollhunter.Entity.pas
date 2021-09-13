unit Trollhunter.Entity;

interface

uses
  Trollhunter.Types,
  Trollhunter.Map;

type
  IEntity = interface

  end;

type
  TEntity = class(TInterfacedObject, IEntity)
  private
    FX: UInt;
    FY: UInt;
    FZ: TMapEnum;
  public
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

uses
  SysUtils,
  Trollhunter.Game,
  Trollhunter.Language;

{ TEntity }

function TEntity.GetCapit(S: string): string;
begin
  if not S.IsEmpty then
    Result := UpCase(S[1]) + Copy(S, 2, Length(S))
  else
    Result := '(???)' + S;
end;

function TEntity.GetDescAn(S: string): string;
begin
  if not S.IsEmpty then
    Result := S.ToLower
  else
    Result := '(???)' + S;
  if (Language.Current <> 'english') then
    Exit;
  if CharInSet(Result[1], ['a', 'e', 'i', 'o', 'u']) then
    Result := 'an ' + Result
  else
    Result := 'a ' + Result;
end;

function TEntity.GetDescThe(S: string): string;
begin
  if not S.IsEmpty then
    Result := S.ToLower
  else
    Result := '(???)' + S;
  if (Language.Current <> 'english') then
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
  R: string;
begin
  B := True;
  Result := '';
  S := StringReplace(S, '[[', '[', [rfReplaceAll]);
  S := StringReplace(S, ']]', ']', [rfReplaceAll]);
  for R in S do
  begin
    if (R = '[') then
      B := False;
    if B then
      Result := Result + R;
    if (R = ']') then
      B := True;
  end;
end;

end.
