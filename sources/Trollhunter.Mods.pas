unit Trollhunter.Mods;

interface

uses
  System.Classes;

type
  TMods = class(TObject)
  private const
    Default = 'trollhunter';
  private
    FSL: TStringList;
    FCurrent: string;
    function GetCurValue(const Name: string; DefValue: string): string; overload;
    function GetCurValue(const Name: string; DefValue: Integer): Integer; overload;
  public
    ModFL, ModNL, ModDL: TStringList;
    constructor Create;
    destructor Destroy; override;
    function GetPath(const SubDir, FileName: string): string;
    procedure SetCurrent(const FileName: string);
    procedure LoadMods;
    property Current: string read FCurrent;
    function PlayerX: Integer;
    function PlayerY: Integer;
  end;

var
  GMods: TMods;

implementation

uses
  Dialogs,
  System.SysUtils,
  Trollhunter.Utils;

{ TMods }

constructor TMods.Create;
begin
  FSL := TStringList.Create;
  ModNL := TStringList.Create;
  ModFL := TStringList.Create;
  ModDL := TStringList.Create;
  FCurrent := Default;
end;

destructor TMods.Destroy;
begin
  FreeAndNil(ModDL);
  FreeAndNil(ModFL);
  FreeAndNil(ModNL);
  FreeAndNil(FSL);
  inherited;
end;

function TMods.GetCurValue(const Name: string; DefValue: Integer): Integer;
var
  I: Integer;
begin
  I := FSL.IndexOfName(Name);
  Result := StrToIntDef(FSL.ValueFromIndex[I], DefValue);
end;

function TMods.GetCurValue(const Name: string; DefValue: string): string;
var
  I: Integer;
begin
  I := FSL.IndexOfName(Name);
  Result := FSL.ValueFromIndex[I];
end;

function TMods.GetPath(const SubDir, FileName: string): string;
begin
  Result := Utils.GetPath('mods' + PathDelim + Current + PathDelim + SubDir) + FileName;
  if not FileExists(Result) then
    Result := Utils.GetPath('mods' + PathDelim + Default +PathDelim + SubDir) + FileName;
end;

procedure TMods.LoadMods;
var
  SL: TStringList;
  SR: TSearchRec;
  S: string;
begin
  ForceDirectories(Utils.GetPath('mods'));
  ModFL.Clear;
  ModNL.Clear;
  SL := TStringList.Create;
  try
    if (FindFirst(Utils.GetPath('mods') + '*.txt', faAnyFile, SR) = 0) then
    begin
      repeat
        S := Trim(SR.Name);
        if (S = '') then
          Continue;
        SL.LoadFromFile(Utils.GetPath('mods') + S, TEncoding.UTF8);
        ModNL.Append(SL.Values['Name']);
        ModDL.Append(SL.Values['Descr']);
        ModFL.Append(S);
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;
  finally
    FreeAndNil(SL);
  end;
  ShowMessage(ModFL.Text);
end;

function TMods.PlayerX: Integer;
begin
  Result := GetCurValue('PlayerX', 1);
end;

function TMods.PlayerY: Integer;
begin
  Result := GetCurValue('PlayerY', 1);
end;

procedure TMods.SetCurrent(const FileName: string);
var
  FN: string;
begin
  FCurrent := FileName;
  FSL.LoadFromFile(Utils.GetPath('mods') + FileName, TEncoding.UTF8);
  FN := GetCurValue('Maps', '');
//  ShowMessage(FileName);
end;

initialization

GMods := TMods.Create;

finalization

FreeAndNil(GMods);

end.
