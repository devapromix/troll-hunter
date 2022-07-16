unit Trollhunter.BaseCreature;

interface

uses
  Trollhunter.Entity,
  Trollhunter.Bar;

type
  TBaseCreature = class(TEntity)
  private
    FLife: TBar;
    FMana: TBar;
    FAP: TBar;
    procedure SetLife(const Value: TBar);
    procedure SetAP(const Value: TBar);
    procedure SetMana(const Value: TBar);
  public
    constructor Create();
    destructor Destroy; override;
    property Life: TBar read FLife write SetLife;
    property Mana: TBar read FMana write SetMana;
    property AP: TBar read FAP write SetAP;
    procedure Fill;
  end;

implementation

{ TBaseCreature }

constructor TBaseCreature.Create;
begin
  inherited;
  Life := TBar.Create;
  Mana := TBar.Create;
  AP := TBar.Create;
end;

destructor TBaseCreature.Destroy;
begin
  Life.Free;
  Mana.Free;
  AP.Free;
  inherited;
end;

procedure TBaseCreature.Fill;
begin
  Life.SetToMax;
  Mana.SetToMax;
end;

procedure TBaseCreature.SetLife(const Value: TBar);
begin
  FLife := Value;
end;

procedure TBaseCreature.SetAP(const Value: TBar);
begin
  FAP := Value;
end;

procedure TBaseCreature.SetMana(const Value: TBar);
begin
  FMana := Value;
end;

end.
