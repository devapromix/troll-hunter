unit uTalent;

interface

uses
  uPlayer;

const
  TalentMax = 10;
  PrmDV = 3;

type
  TTalentEnum = (tlNone, tlStrong { Сильный } , tlDextrous { Ловкий } ,
    tlMage { Маг } , tlTough { Тяжелый } , tlWealthy { Богатый } ,
    tlAffWithSwords, tlAffWithAxes, tlAffWithPolearms, tlAffWithMaces,
    tlMiser { Скряга }, tlCareful { Осторожный } );

type
  TTalentBase = record
    Level: Byte;
    Effects: TEffects;
  end;

const
  TalentBase: array [TTalentEnum] of TTalentBase = (
    // None
    (Level: 1; Effects: [];),
    // Strong
    (Level: 1; Effects: [efPrmAthletics];),
    // Dextrous
    (Level: 1; Effects: [efPrmDodge];),
    // Mage
    (Level: 1; Effects: [efPrmConcentration];),
    // Tough
    (Level: 1; Effects: [efPrmToughness];),
    // Wealthy
    (Level: 1; Effects: [efPrmGold];),
    // Affinity with Swords
    (Level: 3; Effects: [efPrmBlade];),
    // Affinity with Axes
    (Level: 3; Effects: [efPrmAxe];),
    // Affinity with Polearms
    (Level: 3; Effects: [efPrmSpear];),
    // Affinity with Maces
    (Level: 3; Effects: [efPrmMace];),
    // Miser
    (Level: 5; Effects: [ef2xGold];),
    // Careful
    (Level: 5; Effects: [efPrmDV];));

type
  TTalent = record
    Enum: TTalentEnum;
    Level: Byte;
  end;

type
  TTalents = class(TObject)
  private
    FTalent: array [0 .. TalentMax - 1] of TTalent;
    function GetTalent(I: Byte): TTalent;
    procedure SetTalent(I: Byte; const Value: TTalent);
  public
    constructor Create;
    destructor Destroy; override;
    property Talent[I: Byte]: TTalent read GetTalent write SetTalent;
    function GetName(I: TTalentEnum): string;
    function GetHint(I: TTalentEnum): string;
    procedure Add(const ATalent: TTalentEnum);
    function IsTalent(const ATalent: TTalentEnum): Boolean;
    function Count: Byte;
    procedure DoTalent(Key: Byte);
  end;

var
  Talents: TTalents = nil;

implementation

uses SysUtils, Math, GNUGetText, uSkill, uGame, uScenes;

{ TTalents }

procedure TTalents.Add(const ATalent: TTalentEnum);
var
  I: Byte;
begin
  for I := 0 to TalentMax - 1 do
    if (FTalent[I].Enum = tlNone) then
    begin
      FTalent[I].Enum := ATalent;
      Break;
    end;
end;

function TTalents.Count: Byte;
begin
  Result := Length(FTalent);
end;

constructor TTalents.Create;
var
  I: Byte;
begin
  for I := 0 to TalentMax - 1 do
    FTalent[I].Enum := tlNone;
end;

destructor TTalents.Destroy;
begin

  inherited;
end;

procedure TTalents.DoTalent(Key: Byte);
var
  T: TTalentEnum;
  K: Byte;
begin
  K := 0;
  for T := Low(TTalentEnum) to High(TTalentEnum) do
    if ((TalentBase[T].Level = Player.Level)
      and (T <> tlNone)) then
    begin
      if (Key = K) then
      begin
        Self.Add(T);
        Player.TalentPoint := False;
        Player.DoEffects(TalentBase[T].Effects);
        if not Game.IsMode then
          Scenes.SetScene(scName);
        Break;
      end;
      Inc(K);
    end;
end;

function TTalents.GetHint(I: TTalentEnum): string;
const
  F = '+%d to %s';
begin
  case I of
    tlStrong:
      Result := Format(F, [StartSkill, _('Athletics')]);
    tlDextrous:
      Result := Format(F, [StartSkill, _('Dodge')]);
    tlMage:
      Result := Format(F, [StartSkill, _('Concentration')]);
    tlTough:
      Result := Format(F, [StartSkill, _('Toughness')]);
    tlWealthy:
      Result := Format(F, [StartGold, _('Gold')]);
    tlAffWithSwords:
      Result := Format(F, [StartSkill, _('Blade')]);
    tlAffWithAxes:
      Result := Format(F, [StartSkill, _('Axe')]);
    tlAffWithPolearms:
      Result := Format(F, [StartSkill, _('Spear')]);
    tlAffWithMaces:
      Result := Format(F, [StartSkill, _('Mace')]);
    tlMiser:
      Result := _('x2 to Gold');
    tlCareful:
      Result := Format(F, [PrmDV, _('DV')]);
  else
    Result := '-';
  end;
end;

function TTalents.GetName(I: TTalentEnum): string;
begin
  case I of
    tlStrong:
      Result := _('Strong');
    tlDextrous:
      Result := _('Dextrous');
    tlMage:
      Result := _('Mage');
    tlTough:
      Result := _('Tough');
    tlWealthy:
      Result := _('Wealthy');
    tlAffWithSwords:
      Result := _('Affinity with Swords');
    tlAffWithAxes:
      Result := _('Affinity with Axes');
    tlAffWithPolearms:
      Result := _('Affinity with Spears');
    tlAffWithMaces:
      Result := _('Affinity with Maces');
    tlMiser:
      Result := _('Miser');
    tlCareful:
      Result := _('Careful');
  else
    Result := '-';
  end;
end;

function TTalents.GetTalent(I: Byte): TTalent;
begin
  Result := FTalent[I]
end;

function TTalents.IsTalent(const ATalent: TTalentEnum): Boolean;
var
  I: Byte;
begin
  Result := False;
  for I := 0 to TalentMax - 1 do
    if (FTalent[I].Enum = ATalent) then
    begin
      Result := True;
      Exit;
    end;
end;

procedure TTalents.SetTalent(I: Byte; const Value: TTalent);
begin
  FTalent[I] := Value;
end;

initialization

Talents := TTalents.Create;

finalization

FreeAndNil(Talents);

end.
