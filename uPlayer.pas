unit uPlayer;

interface

type
  TPlayer = class(TObject)
  private
    FX: Byte;
    FY: Byte;
    FTurn: Word;
    FLife: Word;
    FMaxLife: Word;
  public
    constructor Create;
    destructor Destroy; override;
    property X: Byte read FX write FX;
    property Y: Byte read FY write FY;
    property Turn: Word read FTurn write FTurn;
    property Life: Word read FLife write FLife;
    property MaxLife: Word read FMaxLife write FMaxLife;
    procedure Move(AX, AY: ShortInt);
  end;

var
  Player: TPlayer = nil;

implementation

uses Math, uCommon;

{ TPlayer }

constructor TPlayer.Create;
begin
  Turn := 0;
end;

destructor TPlayer.Destroy;
begin

  inherited;
end;

procedure TPlayer.Move(AX, AY: ShortInt);
begin
  X := Clamp(X + AX, 0, High(Byte));
  Y := Clamp(Y + AY, 0, High(Byte));
  Turn := Turn + 1;
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
