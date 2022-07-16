unit Trollhunter.Entity;

interface

uses Types, Graphics;

type
  TEntity = class(TObject)
  private
    FImage: TBitmap;
    FPos: TPoint;
    FName: string;
    procedure SetImage(const Value: TBitmap);
    procedure SetPos(const Value: TPoint);
    function GetPos: TPoint;
    procedure SetName(const Value: string);
  public
    procedure SetPosition(const X, Y: Integer);
    constructor Create();
    destructor Destroy; override;
    property Name: string read FName write SetName;
    property Image: TBitmap read FImage write SetImage;
    property Pos: TPoint read GetPos write SetPos;
  end;

implementation

{ TEntity }

constructor TEntity.Create;
begin
  Image := Graphics.TBitmap.Create;
  Image.Transparent := True;
  Name := '';
end;

destructor TEntity.Destroy;
begin
  Image.Free;
  inherited;
end;

function TEntity.GetPos: TPoint;
begin
  Result := FPos;
end;

procedure TEntity.SetImage(const Value: TBitmap);
begin
  FImage := Value;
end;

procedure TEntity.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TEntity.SetPos(const Value: TPoint);
begin
  FPos := Value;
end;

procedure TEntity.SetPosition(const X, Y: Integer);
begin
  Pos := Point(X, Y);
end;

end.
