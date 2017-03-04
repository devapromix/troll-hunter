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
  Player: TPlayer;

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
  Player.X := RandomRange(1, High(Byte));
  Player.Y := RandomRange(1, High(Byte));
  Player.MaxLife := 111;
  Player.Life := Player.MaxLife;

finalization
  Player.Free;
  Player := nil;

end.
