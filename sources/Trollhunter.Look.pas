unit Trollhunter.Look;

interface

type
  TLook = class(TObject)
  private

  public
    X, Y: Integer;
    constructor Create();
    destructor Destroy; override;
  end;

implementation

{ TLook }

constructor TLook.Create;
begin

end;

destructor TLook.Destroy;
begin

  inherited;
end;

end.
