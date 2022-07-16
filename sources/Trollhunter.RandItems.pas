unit Trollhunter.RandItems;

interface

uses
  Classes,
  Graphics,
  Trollhunter.Color;

type
  TRandItemRec = record
    Name: string;
    Color: Integer;
    Defined: Integer;
  end;

const
  RandItemCount = 12;

type
  TRandItem = array [1 .. RandItemCount] of TRandItemRec;

const
  AllowColors: array [1 .. RandItemCount] of Integer = (cGolden, cIndigo, cJade,
    cAzure, cLight, cDark, cGray, cBrown, cFxBlack, cFxWhite, cSkyBlue,
    cLtYellow);

type
  TRandItems = class(TObject)
  private
    FCount: Byte;
    FF: TStringList;
    RandItem: TRandItem;
    procedure Gen;
    procedure Save;
    procedure Load;
    procedure Clear;
    function GenName: string;
    function GetText: string;
    procedure SetText(const Value: string);
    function IsThisColor(C: Integer): Boolean;
  public
    constructor Create(ACount: Byte);
    destructor Destroy; override;
    property Text: string read GetText write SetText;
    property Count: Byte read FCount;
    function GetColor(Index: Integer): Integer;
    function GetColorName(Index: Integer): string;
    function GetName(Index: Integer): string;
    function IsDefined(Index: Integer): Boolean;
    procedure SetDefined(Index: Integer);
  end;

implementation

uses
  SysUtils,
  Trollhunter.Utils,
  Trollhunter.Lang;

{ TRandItems }

procedure TRandItems.Clear;
var
  I: Byte;
begin
  for I := 1 to RandItemCount do
    with RandItem[I] do
    begin
      Name := '';
      Color := 0;
      Defined := 0;
    end;
end;

constructor TRandItems.Create(ACount: Byte);
begin
  FF := TStringList.Create;
  FCount := ACount;
  Self.Gen;
end;

destructor TRandItems.Destroy;
begin
  FreeAndNil(FF);
  inherited;
end;

function TRandItems.IsThisColor(C: Integer): Boolean;
var
  I: Byte;
begin
  Result := False;
  for I := 1 to Count do
    if (RandItem[I].Color = C) then
    begin
      Result := True;
      Exit;
    end;
end;

procedure TRandItems.Gen;
var
  I, C: Integer;
begin
  Clear;
  for I := 1 to Count do
  begin
    repeat
      C := AllowColors[Rand(1, RandItemCount)];
    until not IsThisColor(C);
    with RandItem[I] do
    begin
      Name := GenName;
      Color := C;
      Defined := 0;
    end;
  end;
end;

function TRandItems.GenName: string;
const
  S = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
var
  I: Byte;
begin
  Result := '';
  for I := 1 to 7 do
    Result := Result + S[Rand(1, 26)];
end;

function TRandItems.GetColor(Index: Integer): Integer;
begin
  Result := RandItem[Index].Color;
end;

function TRandItems.GetColorName(Index: Integer): string;
begin
  Result := '';
  case GetColor(Index) of
    cGolden:
      Result := GetLang(250);
    cIndigo:
      Result := GetLang(251);
    cJade:
      Result := GetLang(252);
    cAzure:
      Result := GetLang(253);
    cLight:
      Result := GetLang(254);
    cDark:
      Result := GetLang(255);
    cGray:
      Result := GetLang(256);
    cBrown:
      Result := GetLang(257);
    cFxBlack:
      Result := GetLang(258);
    cFxWhite:
      Result := GetLang(259);
    cSkyBlue:
      Result := GetLang(260);
    cLtYellow:
      Result := GetLang(261);
  end;
end;

function TRandItems.GetName(Index: Integer): string;
begin
  Result := RandItem[Index].Name;
end;

function TRandItems.GetText: string;
begin
  Self.Save;
  Result := FF.Text;
end;

function TRandItems.IsDefined(Index: Integer): Boolean;
begin
  Result := RandItem[Index].Defined = 1;
end;

procedure TRandItems.Load;
var
  I, P: Integer;
  E: TExplodeResult;
begin
  Clear;
  P := 1;
  E := nil;
  for I := 0 to FF.Count - 1 do
  begin
    E := Explode('/', FF[I]);
    if (Trim(E[0]) <> '') then
      with RandItem[P] do
      begin
        Name := E[0];
        Color := StrToInt(E[1]);
        Defined := StrToInt(E[2]);
      end;
    Inc(P);
  end;
end;

procedure TRandItems.Save;
var
  I: Byte;
begin
  FF.Clear;
  for I := 1 to Count do
    with RandItem[I] do
      FF.Append(Format('%s/%d/%d', [Name, Color, Defined]));
end;

procedure TRandItems.SetDefined(Index: Integer);
begin
  RandItem[Index].Defined := 1;
end;

procedure TRandItems.SetText(const Value: string);
begin
  FF.Text := Value;
  Self.Load;
end;

end.
