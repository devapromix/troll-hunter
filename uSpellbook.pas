unit uSpellbook;

interface

type
  TSpellbook = class(TObject)
  private

  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  Spellbook: TSpellbook = nil;

implementation

{ TSpellbook }

constructor TSpellbook.Create;
begin

end;

destructor TSpellbook.Destroy;
begin

  inherited;
end;

initialization

Spellbook := TSpellbook.Create;

finalization

FreeAndNil(Spellbook);

end.
