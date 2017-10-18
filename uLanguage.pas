unit uLanguage;

interface

uses Classes;

type
  TLanguage = class(TObject)
  private
    FID: TStringList;
    FValue: TStringList;
    FCurrent: string;
  public
    FSL: TStringList;
    function Get(const AValue: string): string;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure SaveLanguage;
    procedure LoadFromFile(AFileName: string);
    procedure SaveToFile(AFileName: string);
    procedure UseLanguage(ACurrentLanguage: string);
    property Current: string read FCurrent write FCurrent;
  end;

function _(const AValue: string): string;

implementation

uses SysUtils, uGame;

{ TLanguage }

function _(const AValue: string): string;
begin
  if Assigned(Game) then
  begin
    if Game.MkLang then
      Game.Language.FSL.Append(AValue + '=');
    Result := Game.Language.Get(AValue);
  end else Result := AValue;
end;

procedure TLanguage.Clear;
begin
  FID.Clear;
  FValue.Clear;
end;

constructor TLanguage.Create;
begin
  FSL := TStringList.Create;
  FSL.Sorted := True;
  FSL.Duplicates := dupIgnore;
  FSL.LoadFromFile(Game.GetPath('languages') + 'default.lng');
  FID := TStringList.Create;
  FValue := TStringList.Create;
  FCurrent := 'en';
end;

destructor TLanguage.Destroy;
begin
  FreeAndNil(FValue);
  FreeAndNil(FID);
  FreeAndNil(FSL);
  inherited;
end;

procedure TLanguage.LoadFromFile(AFileName: string);
var
  S: string;
  I, J: Integer;
  SL: TStringList;
begin
  if not FileExists(AFileName) then
    Exit;
  SL := TStringList.Create;
  try
    SL.LoadFromFile(AFileName);
    for I := 0 to SL.Count - 1 do
    begin
      S := SL[I];
      J := Pos('=', S);
      FID.Append(Copy(S, 1, J - 1));
      Delete(S, 1, J);
      FValue.Append(S);
    end;
  finally
    FreeAndNil(SL);
  end;
end;

procedure TLanguage.SaveLanguage;
begin
  SaveToFile(Game.GetPath('languages') + 'default.lng');
end;

procedure TLanguage.SaveToFile(AFileName: string);
begin
  FSL.SaveToFile(AFileName);
end;

procedure TLanguage.UseLanguage(ACurrentLanguage: string);
begin
  Clear;
  Current := ACurrentLanguage;
  LoadFromFile(Game.GetPath('languages') + Current + '.lng');
end;

function TLanguage.Get(const AValue: string): string;
var
  I: Integer;
begin
  I := FID.IndexOf(AValue);
  if (I < 0) or (FValue[I] = '') then
    Result := AValue
  else
    Result := FValue[I];
end;

end.
