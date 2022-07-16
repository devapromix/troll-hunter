unit Trollhunter.Settings;

interface

type
  TSettings = class(TObject)
  private
    FFileName: string;
  public
    constructor Create;
    destructor Destroy; override;
    function Read(const Section, Ident, Default: string): string; overload;
    function Read(const Section, Ident: string; Default: Integer)
      : Integer; overload;
    function Read(const FileName, Section, Ident, Default: string)
      : string; overload;
    function Read(const FileName, Section, Ident: string; Default: Integer)
      : Integer; overload;
    procedure Write(const Section, Ident, Value: string); overload;
    procedure Write(const Section, Ident: string; Value: Integer); overload;
    procedure Write(const FileName, Section, Ident, Value: string); overload;
    procedure Write(const FileName, Section, Ident: string;
      Value: Integer); overload;
  end;

implementation

uses
  IniFiles,
  Trollhunter.Utils,
  Trollhunter.Graph,
  Trollhunter.Creatures;

{ TSettings }

constructor TSettings.Create;
begin
  FFileName := Path + 'Save\Settings.cfg';
end;

destructor TSettings.Destroy;
begin

  inherited;
end;

function TSettings.Read(const Section, Ident, Default: string): string;
begin
  Result := Read(FFileName, Section, Ident, Default);
end;

function TSettings.Read(const Section, Ident: string; Default: Integer)
  : Integer;
begin
  Result := Read(FFileName, Section, Ident, Default);
end;

procedure TSettings.Write(const Section, Ident, Value: string);
begin
  Write(FFileName, Section, Ident, Value);
end;

procedure TSettings.Write(const Section, Ident: string; Value: Integer);
begin
  Write(FFileName, Section, Ident, Value);
end;

function TSettings.Read(const FileName, Section, Ident: string;
  Default: Integer): Integer;
var
  I: TIniFile;
begin
  I := TIniFile.Create(FileName);
  try
    Result := I.ReadInteger(Section, Ident, Default);
  finally
    I.Free;
  end;
end;

function TSettings.Read(const FileName, Section, Ident,
  Default: string): string;
var
  I: TIniFile;
begin
  I := TIniFile.Create(FileName);
  try
    Result := I.ReadString(Section, Ident, Default);
  finally
    I.Free;
  end;
end;

procedure TSettings.Write(const FileName, Section, Ident: string;
  Value: Integer);
var
  I: TIniFile;
begin
  I := TIniFile.Create(FileName);
  try
    I.WriteInteger(Section, Ident, Value);
  finally
    I.Free;
  end;
end;

procedure TSettings.Write(const FileName, Section, Ident, Value: string);
var
  I: TIniFile;
begin
  I := TIniFile.Create(FileName);
  try
    I.WriteString(Section, Ident, Value);
  finally
    I.Free;
  end;
end;

end.
