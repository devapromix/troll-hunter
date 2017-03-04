unit uPlayer;

interface

type
  TPlayer = class(TObject)
  private

  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  Player: TPlayer;

implementation

initialization
  Player := TPlayer.Create;

finalization
  Player.Free;
  Player := nil;

end.
