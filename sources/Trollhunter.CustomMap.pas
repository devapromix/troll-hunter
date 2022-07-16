unit Trollhunter.CustomMap;

interface

type
  TCustomMap = class(TObject)
  private
    FHeight: Integer;
    FWidth: Integer;
    function GetHeight: Integer;
    function GetWidth: Integer;
  public
    constructor Create;
    procedure Clear; virtual; abstract;
    procedure Render; virtual; abstract;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  end;

implementation

uses
  Trollhunter.Utils;

{ TCustomMap }

constructor TCustomMap.Create;
begin
  FHeight := MapSide;
  FWidth := MapSide;
end;

function TCustomMap.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TCustomMap.GetWidth: Integer;
begin
  Result := FWidth;
end;

end.
