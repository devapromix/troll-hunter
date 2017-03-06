unit uPlayer;

interface

type
  TPlayer = class(TObject)
  private
    FX: Byte;
    FY: Byte;
    FLX: Byte;
    FLY: Byte;
    FTurn: Word;
    FLife: Word;
    FMaxLife: Word;
    FLook: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property X: Byte read FX write FX;
    property Y: Byte read FY write FY;
    property LX: Byte read FLX write FLX;
    property LY: Byte read FLY write FLY;
    property Turn: Word read FTurn write FTurn;
    property Life: Word read FLife write FLife;
    property MaxLife: Word read FMaxLife write FMaxLife;
    property Look: Boolean read FLook write FLook;
    procedure Move(AX, AY: ShortInt);
    function GetRadius: Byte;
  end;

var
  Player: TPlayer = nil;

implementation

uses Math, uCommon, uMap;

{ TPlayer }

constructor TPlayer.Create;
begin
  Turn := 0;
  Look := False;
end;

destructor TPlayer.Destroy;
begin

  inherited;
end;

function TPlayer.GetRadius: Byte;
begin
  Result := 9;
end;

procedure TPlayer.Move(AX, AY: ShortInt);
begin
  if Look then    
  begin
    if Map.InMap(LX + AX, LY + AY)
      and Map.InView(LX + AX, LY + AY)
      and not Map.GetFog(LX + AX, LY + AY) then
    begin
      LX := Clamp(LX + AX, 0, High(Byte));
      LY := Clamp(LY + AY, 0, High(Byte));
    end;
  end else begin
    X := Clamp(X + AX, 0, High(Byte));
    Y := Clamp(Y + AY, 0, High(Byte));
    Turn := Turn + 1;
  end;
end;

initialization
  Player := TPlayer.Create;
  Player.X := RandomRange(64, High(Byte) - 64);
  Player.Y := 0;
  Player.MaxLife := 100;
  Player.Life := Player.MaxLife;

finalization
  Player.Free;
  Player := nil;

end.
