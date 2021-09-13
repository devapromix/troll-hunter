unit uServer;

interface

uses idHTTP;

type
  TServer = class(TObject)
  private
    FURL: string;
    FS: TidHTTP;
  public
    function Get(AURL: string): string;
    constructor Create;
    destructor Destroy; override;
  end;

function IsInternetConnected: Boolean;

implementation

uses Forms, Windows, SysUtils, Wininet, uMain, uBox;

{ TServer }

function IsInternetConnected: Boolean;
var
  ConnectionType: DWORD ;
begin
  Result := False;
  try
    ConnectionType := INTERNET_CONNECTION_MODEM
                    + INTERNET_CONNECTION_LAN
                    + INTERNET_CONNECTION_PROXY;
    Result := InternetGetConnectedState(@ConnectionType, 0);
  except end;
end;

constructor TServer.Create;
begin
  FS := TidHTTP.Create(nil);
  FURL := 'http://hod.rlgclub.ru/hodserver/';
end;

destructor TServer.Destroy;
begin
  FS.Free;
  inherited;
end;

function TServer.Get(AURL: string): string;
begin
  if not IsInternetConnected then
  begin
    Result := '';
    Exit;
  end;
  try
    Result := Trim(FS.Get(FURL + AURL));
  except
    on E: Exception do Result := '';
  end;
end;

end.
