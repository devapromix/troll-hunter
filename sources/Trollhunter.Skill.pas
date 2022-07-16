unit Trollhunter.Skill;

interface

uses
  Classes;

type
  TSkillEnum = (skDagger, skAxe, skSword, skMace, skSpear, skBow,
    skCrossBow, skShield,

    //
    skTrap, skMagic);

const
  SkillsCount = Ord(High(TSkillEnum));

type
  TSkillRec = record
    Exp: Byte;
  end;

const
  SkillParam: array [0 .. SkillsCount] of TSkillRec = (
    //
    (Exp: 3;), // Daggers and knives
    (Exp: 3;), //
    (Exp: 3;), //
    (Exp: 3;), //
    (Exp: 3;), //
    (Exp: 3;), //
    (Exp: 3;), //
    (Exp: 3;), //

    (Exp: 7;), // Traps
    (Exp: 2;) // Magic
    );

type
  TSkillItem = record
    Level: Integer;
    Exp: Integer;
  end;

const
  SkillMaxValue = 50;
  SkillMaxExp = 100;

type
  TSkillArr = array [0 .. SkillsCount] of TSkillItem;

type
  TSkill = class(TObject)
  private
    FSkill: TSkillArr;
    FF: TStringList;
    procedure Save;
    procedure Load;
    procedure SetSkill(const Value: TSkillArr);
    function GetText: string;
    procedure SetText(const Value: string);
  public
    procedure Clear;
    property Text: string read GetText write SetText;
    procedure Up(SkillID: TSkillEnum); overload;
    procedure Up(SkillID: Byte); overload;
    function Count: Byte;
    property Skill: TSkillArr read FSkill write SetSkill;
    function GetSkill(SkillID: TSkillEnum; IsLevel: Boolean): Integer; overload;
    function GetSkill(SkillID: TSkillEnum): TSkillItem; overload;
    function GetSkill(SkillID: Byte): TSkillItem; overload;
    procedure Add(SkillID: TSkillEnum; ALevel: Byte);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  Trollhunter.Utils,
  Trollhunter.Log,
  Trollhunter.Lang;

{ TSkill }

procedure TSkill.Add(SkillID: TSkillEnum; ALevel: Byte);
begin
  FSkill[Ord(SkillID)].Level := ALevel;
end;

procedure TSkill.Clear;
var
  I: Byte;
begin
  for I := 0 to SkillsCount do
  begin
    FSkill[I].Level := 0;
    FSkill[I].Exp := 0;
  end;
end;

function TSkill.Count: Byte;
begin
  Result := SkillsCount;
end;

constructor TSkill.Create;
begin
  FF := TStringList.Create;
  Self.Clear;
end;

destructor TSkill.Destroy;
begin
  FF.Free;
  inherited;
end;

function TSkill.GetSkill(SkillID: TSkillEnum): TSkillItem;
begin
  Result := FSkill[Ord(SkillID)];
end;

function TSkill.GetSkill(SkillID: Byte): TSkillItem;
begin
  Result := FSkill[SkillID];
end;

function TSkill.GetSkill(SkillID: TSkillEnum; IsLevel: Boolean): Integer;
begin
  if IsLevel then
    Result := FSkill[Ord(SkillID)].Level
  else
    Result := FSkill[Ord(SkillID)].Exp;
end;

function TSkill.GetText: string;
begin
  Self.Save;
  Result := FF.Text;
end;

procedure TSkill.Load;
var
  P, I: Integer;
  E: TExplodeResult;
begin
  P := 0;
  E := nil;
  for I := 0 to FF.Count - 1 do
  begin
    E := Explode('/', FF[I]);
    if (Trim(E[0]) <> '') then
    begin
      FSkill[P].Level := StrToInt(E[0]);
      FSkill[P].Exp := StrToInt(E[1]);
    end;
    Inc(P);
  end;
end;

procedure TSkill.Save;
var
  I: Byte;
begin
  FF.Clear;
  for I := 0 to SkillsCount do
    FF.Append(Format('%d/%d', [FSkill[I].Level, FSkill[I].Exp]));
end;

procedure TSkill.SetSkill(const Value: TSkillArr);
begin
  FSkill := Value;
end;

procedure TSkill.SetText(const Value: string);
begin
  FF.Text := Value;
  Self.Load;
end;

procedure TSkill.Up(SkillID: Byte);
begin
  if (FSkill[SkillID].Level < SkillMaxValue) then
  begin
    Inc(FSkill[SkillID].Exp, SkillParam[SkillID].Exp);
    if (FSkill[SkillID].Exp >= SkillMaxExp) then
    begin
      Dec(FSkill[SkillID].Exp, SkillMaxExp);
      Inc(FSkill[SkillID].Level);
      if (FSkill[SkillID].Level > SkillMaxValue) then
      begin
        FSkill[SkillID].Level := SkillMaxValue;
      end;
      Log.Add(Format('%s +1 (%d).', [GetLang(SkillID + 201),
        FSkill[SkillID].Level]));
    end;
  end;
end;

procedure TSkill.Up(SkillID: TSkillEnum);
begin
  Self.Up(Ord(SkillID));
end;

end.
