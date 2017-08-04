unit uSpellbook;

interface

type
  TSpellEnum = (spHeal);

type
  TSpell = record
    Enable: Boolean;
  end;

type
  TSpellbook = class(TObject)
  private
    FSpell: array[TSpellEnum] of TSpell;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function GetSpellName(ASpellEnum: TSpellEnum): string;
    procedure AddSpell(ASpellEnum: TSpellEnum);
    function GetSpell(ASpellEnum: TSpellEnum): TSpell;
  end;

var
  Spellbook: TSpellbook = nil;

implementation

uses SysUtils, GNUGetText;

{ TSpellbook }

procedure TSpellbook.AddSpell(ASpellEnum: TSpellEnum);
begin
  FSpell[ASpellEnum].Enable := True;
end;

procedure TSpellbook.Clear;
var
  I: TSpellEnum;
begin
  for I := Low(TSpellEnum) to High(TSpellEnum) do
    with FSpell[I] do
    begin
      Enable := False;
    end;
end;

constructor TSpellbook.Create;
begin
  Self.Clear;
end;

destructor TSpellbook.Destroy;
begin

  inherited;
end;

function TSpellbook.GetSpell(ASpellEnum: TSpellEnum): TSpell;
begin
  Result := FSpell[ASpellEnum];
end;

function TSpellbook.GetSpellName(ASpellEnum: TSpellEnum): string;
begin
  case ASpellEnum of
    spHeal:
      Result := _('Heal');
  end;
end;

initialization

Spellbook := TSpellbook.Create;

finalization

FreeAndNil(Spellbook);

end.
