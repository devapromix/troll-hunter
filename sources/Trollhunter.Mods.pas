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
    constructor Create;
    destructor Destroy; override;
    function GetPath(const SubDir, FileName: string): string;
    procedure SetCurrent(const FileName: string);
    property Current: string read FCurrent;
    function PlayerX: Integer;
    function PlayerY: Integer;
  end;

var
  GMods: TMods;

implementation

uses
System.SysUtils,
Trollhunter.Utils;

{ TMods }

constructor TMods.Create;
begin
  FSL := TStringList.Create;
  FCurrent := Default;
end;

destructor TMods.Destroy;
begin
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
  FSL.LoadFromFile(GetPath('', 'mod.txt'), TEncoding.UTF8);
  FN := GetCurValue('Maps', '');
end;

initialization

GMods := TMods.Create;

finalization

FreeAndNil(GMods);

end.

