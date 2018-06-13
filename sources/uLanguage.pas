unit uLanguage;

interface

uses Classes;

type
  TLanguage = class(TObject)
  private
    FID: TStringList;
    FSL: TStringList;
    FValue: TStringList;
    FCurrent: string;
    FUseDefaultLanguage: Boolean;
    function GetPath(SubDir: string): string;
  public
    function Get(const AValue: string): string;
    constructor Create(const AUseDefaultLanguage: Boolean = False);
    destructor Destroy; override;
    procedure Clear;
    procedure SaveDefault;
    procedure LoadFromFile(AFileName: string);
    procedure SaveToFile(AFileName: string);
    procedure UseLanguage(ACurrentLanguage: string);
    property Current: string read FCurrent write FCurrent;
    property UseDefaultLanguage: Boolean read FUseDefaultLanguage;
  end;

function _(const AValue: string): string;

var
  Language: TLanguage;

implementation

uses SysUtils;

{ TLanguage }

function _(const AValue: string): string;
begin
  if Assigned(Language) then
  begin
    if Language.UseDefaultLanguage then
      Language.FSL.Append(AValue + '=');
    Result := Language.Get(AValue) + '+';
  end
  else
    Result := AValue;
end;

procedure TLanguage.Clear;
begin
  FID.Clear;
  FValue.Clear;
end;

constructor TLanguage.Create(const AUseDefaultLanguage: Boolean = False);
var
  F: string;
begin
  FSL := TStringList.Create;
  FSL.Sorted := True;
  FSL.Duplicates := dupIgnore;
  FUseDefaultLanguage := AUseDefaultLanguage;
  F := GetPath('languages') + 'default.lng';
  if FileExists(F) then
    FSL.LoadFromFile(F);
  FID := TStringList.Create;
  FValue := TStringList.Create;
  FCurrent := 'english';
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

procedure TLanguage.SaveDefault;
begin
  if Language.UseDefaultLanguage then
    SaveToFile(GetPath('languages') + 'default.lng');
end;

procedure TLanguage.SaveToFile(AFileName: string);
begin
  FSL.SaveToFile(AFileName);
end;

procedure TLanguage.UseLanguage(ACurrentLanguage: string);
begin
  Clear;
  Current := ACurrentLanguage;
  LoadFromFile(GetPath('languages') + Current + '.lng');
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

function TLanguage.GetPath(SubDir: string): string;
begin
  Result := ExtractFilePath(ParamStr(0));
  Result := IncludeTrailingPathDelimiter(Result + SubDir);
end;

end.
