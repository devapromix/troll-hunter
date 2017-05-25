unit uEntity;

interface

type
  TDamage = record
    Min: Word;
    Max: Word;
  end;

type
  TEntity = class(TObject)
  private
    FX: Byte;
    FY: Byte;
    FZ: Byte;
    FLife: Word;
    FMaxLife: Word;
    FDamage: TDamage;
    FAlive: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property X: Byte read FX write FX;
    property Y: Byte read FY write FY;
    property Z: Byte read FZ write FZ;
    property Life: Word read FLife write FLife;
    property MaxLife: Word read FMaxLife write FMaxLife;
    property Damage: TDamage read FDamage write FDamage;
    property Alive: Boolean read FAlive write FAlive;
    function GetDist(ToX, ToY: Single): Word;
    function GetCapit(S: string): string;
    function GetDescAn(S: string): string;
    function GetDescThe(S: string): string;
    function GetPureText(const S: string): string;
    procedure SetDamage(AMin, AMax: Word);
    function IsDead: Boolean;
  end;

implementation

uses SysUtils, GNUGetText;

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
  Result := UpCase(S[1]) + Copy(S, 2, Length(S));
end;

function TEntity.GetDescAn(S: string): string;
begin
  Result := LowerCase(S);
  if (GetCurrentLanguage <> 'en') then
    Exit;
  if (AnsiString(S)[1] in ['a', 'e', 'i', 'o', 'u']) then
    Result := 'an ' + Result
  else
    Result := 'a ' + Result;
end;

function TEntity.GetDescThe(S: string): string;
begin
  Result := LowerCase(S);
  if (GetCurrentLanguage <> 'en') then
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

function TEntity.IsDead: Boolean;
begin
  Result := Life = 0
end;

procedure TEntity.SetDamage(AMin, AMax: Word);
begin
  FDamage.Min := AMin;
  FDamage.Max := AMax;
end;

end.
