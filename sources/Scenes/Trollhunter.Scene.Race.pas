unit Trollhunter.Scene.Race;

interface

uses
  Classes,
  Graphics,
  Trollhunter.Scene,
  Trollhunter.Scene.BaseMenu;

type
  TSceneRace = class(TSceneBaseMenu)
  private
    CursorPos: Integer;
    function Count: Byte;
    function GetAtrValue(A: Byte): Integer;
  public
    procedure MakePC(I: Byte = 0);
    procedure Render(); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    constructor Create;
    destructor Destroy; override;
  end;

var
  SceneRace: TSceneRace;

implementation

uses
  Windows,
  SysUtils,
  Types,
  Trollhunter.Graph,
  Trollhunter.Error,
  Trollhunter.Scene.Game,
  Trollhunter.Lang,
  Trollhunter.Creatures,
  Trollhunter.MainForm,
  Trollhunter.Scenes,
  Trollhunter.Color,
  Trollhunter.Scene.Inv,
  Trollhunter.Race,
  Trollhunter.Utils,
  Trollhunter.Item,
  Trollhunter.Skill;

{ TSceneRace }

function TSceneRace.Count: Byte;
begin
  Result := RacesCount;
  if (Result > 26) then
    Result := 26;
end;

constructor TSceneRace.Create;
begin
  inherited Create(4);
  CursorPos := 1;
end;

destructor TSceneRace.Destroy;
begin

  inherited;
end;

function TSceneRace.GetAtrValue(A: Byte): Integer;
begin
  Result := 0;
  case A of
    1:
      Result := Creatures.PC.Prop.Strength;
    2:
      Result := Creatures.PC.Prop.Dexterity;
    3:
      Result := Creatures.PC.Prop.Will;
    4:
      Result := Creatures.PC.Prop.Speed;
  end;
end;

procedure TSceneRace.KeyDown(var Key: Word; Shift: TShiftState);
var
  I: Byte;
  K: Word;
  C: Integer;
begin
  inherited;
  try
    case Key of
      38, 40:
        begin
          C := Count;
          if (C > 0) then
          begin
            CursorPos := CursorPos + (Key - 39);
            CursorPos := ClampCycle(CursorPos, 1, C);
            MakePC(CursorPos - 1);
            Render;
          end;
        end;
      13:
        begin
          C := Count;
          if (C > 0) then
          begin
            K := (ord('A') + CursorPos) - 1;
            KeyDown(K, Shift);
          end;
        end;
      ord('A') .. ord('Z'):
        begin
          I := Key - (ord('A'));
          if (I < Count) then
          begin
            Creatures.PC.Race := I;
            Graph.Messagebar.Add(Format(GetLang(20), [Creatures.PC.Name,
              MainForm.Caption]));
            Creatures.PC.Redraw;
            Scenes.Scene := SceneGame;
          end;
        end;
    end;
  except
    on E: Exception do
      Error.Add('SceneRace.KeyDown (#' + IntToStr(Key) + ')', E.Message);
  end;
end;

procedure TSceneRace.KeyPress(var Key: Char);
begin
  inherited;

end;

procedure TSceneRace.MakePC(I: Byte);
var
  J: Byte;
begin
  try
    Creatures.PC.Clear;
    with Race[I] do
    begin
      with RHWeapon do
        Items.AddAndEquip(ID, Count);
      with LHWeapon do
        Items.AddAndEquip(ID, Count);
      for J := 0 to High(TRaceSkills) do
        Creatures.PC.Skill.Add(Skills[J].Skill, Skills[J].Level);
    end;

    // Items.Add('SMITH');
    // Items.Add('MINIOILPOTION', 3);
    Items.Add('MINILIFEPOTION', 3);
    Items.Add('MINIMANAPOTION', 3);

    // Items.Add('POTIONA', 5);
    // Items.Add('POTIONB', 5);
    // Items.Add('POTIONC', 5);
    // Items.Add('POTIOND', 5);
    // Items.Add('POTIONE', 5);
    // Items.Add('POTIONF', 5);
    // Items.Add('POTIONG', 5);
    Items.Add('POTIONH', 5);

    // Items.Add('SCROLLA', 5);
    // Items.Add('SCROLLB', 5);
    // Items.Add('SCROLLC', 5);
    // Items.Add('SCROLLD', 5);
    Items.Add('SCROLLE', 5);
    // Items.Add('SCROLLF', 5);
    // Items.Add('SCROLLG', 5);
    // Items.Add('SCROLLH', 5);
    Items.Add('SCROLLI', 5);

    // Items.Add('TAMARILIS', 12);
    Items.Add('KEY', 7);

    Creatures.PC.Prop.Strength := Creatures.PC.Prop.Strength + Race[I].Strength;
    Creatures.PC.Prop.Dexterity := Creatures.PC.Prop.Dexterity +
      Race[I].Dexterity;
    Creatures.PC.Prop.Will := Creatures.PC.Prop.Will + Race[I].Will;
    Creatures.PC.Prop.Speed := Creatures.PC.Prop.Speed + Race[I].Speed;
    Creatures.PC.Calc;
    Creatures.PC.Fill;
  except
    on E: Exception do
      Error.Add('SceneRace.MakePC', E.Message);
  end;
end;

procedure TSceneRace.Render;
var
  T, C, I, J, Y, H, L, K, P, R, V, U: Integer;
  F, S, D, M, Q: string;
  A: ShortInt;
begin
  inherited;
  try
    T := ((Graph.Surface.Width div 5) div Graph.CharWidth);
    with Graph.Surface.Canvas do
    begin
      C := Count;
      for I := 0 to C - 1 do
      begin
        if (I > 25) then
          Break;
        Y := (I + 3) * Graph.CharHeight;
        S := Chr(I + 97) + '.';
        CursorPos := Clamp(CursorPos, 1, C);
        if (CursorPos = I + 1) then
        begin
          Font.Style := [fsBold];
          Font.Color := cAcColor;
          Graph.RenderMenu(I + 3, 0);
        end
        else
        begin
          Font.Style := [];
          Font.Color := cBgColor;
        end;
        TextOut((Graph.CharWidth * 3) - TextWidth(S), Y, S);
        TextOut(Graph.CharWidth * 3, Y, GetLang(Race[I].NameLangID));
      end;
      Font.Style := [];
      if (Count > 0) then
      begin
        Font.Color := cDkYellow;
        Graph.Text.DrawOut(1, 2, '#');
        Graph.Text.DrawOut(3, 2, GetLang(180));
        Graph.Text.DrawOut(T, 2, GetLang(320));
      end;
      H := C + 5;
      Font.Style := [fsBold];
      Q := Creatures.PC.Name + ':';
      Graph.Text.DrawOut(T, H - 1, Q);
      M := '';
      for I := 1 to Length(Q) do
        M := M + '-';
      Graph.Text.DrawOut(T, H, M);
      Font.Style := [];
      { L := 0;
        for I := 1 to 4 do
        begin
        if (Length(GetLang(I + 14)) > L) then
        L := Length(GetLang(I + 14));
        end; }
      L := T;
      for I := 1 to 4 do
      begin
        D := '';
        S := GetLang(I + 14) + ': ' + IntToStr(GetAtrValue(I));
        Font.Color := cBgColor;
        Graph.Text.DrawOut(L, H + I, S);
        A := 0;
        P := CursorPos - 1;
        case I of
          1:
            A := Race[P].Strength;
          2:
            A := Race[P].Dexterity;
          3:
            A := Race[P].Will;
          4:
            A := Race[P].Speed;
        end;
        if (A > 0) then
        begin
          D := ' (+' + IntToStr(A) + ')';
          Font.Color := cRdBlue;
        end;
        if (A < 0) then
        begin
          D := #32 + '(' + IntToStr(A) + ')';
          Font.Color := cRdRed;
        end;
        K := L + Length(S);
        Graph.Text.DrawOut(K, H + I, D);
        U := I + 1;
      end;
      //
      Font.Color := cRdRed;;
      Graph.Text.DrawOut(T, H + U + 1, Format('%s: %d/%d',
        [GetLang(22), Creatures.PC.Life.Max, Creatures.PC.Life.Max]));
      Font.Color := cRdBlue;
      Graph.Text.DrawOut(T, H + U + 2, Format('%s: %d/%d',
        [GetLang(23), Creatures.PC.Mana.Max, Creatures.PC.Mana.Max]));
      Font.Color := cDkYellow;
      Graph.Text.DrawOut(T, H + U + 3, Format('%s: %d-%d',
        [GetLang(32), Creatures.PC.Prop.MinDamage,
        Creatures.PC.Prop.MaxDamage]));
      Font.Color := cDkYellow;
      Graph.Text.DrawOut(T, H + U + 4,
        Format('%s: %d', [GetLang(33), Creatures.PC.Prop.Protect]));

      R := 0;
      Font.Style := [fsBold];
      Q := GetLang(200) + ':';
      Graph.Text.DrawOut(T * 2, H - 1, Q);
      M := '';
      for I := 1 to Length(Q) do
        M := M + '-';
      Graph.Text.DrawOut(T * 2, H, M);
      Font.Style := [];
      for J := 0 to SkillsCount do
        if (Creatures.PC.Skill.GetSkill(J).Level > 0) then
        begin
          Inc(R);
          Font.Color := cDkYellow;
          Graph.Text.DrawOut(T * 2, H + R, GetLang(J + 201) + ': ' +
            IntToStr(Creatures.PC.Skill.GetSkill(J).Level));
        end;

      R := 0;
      Font.Style := [fsBold];
      Q := GetLang(25) + ':';
      Graph.Text.DrawOut(T * 3, H - 1, Q);
      M := '';
      for I := 1 to Length(Q) do
        M := M + '-';
      Graph.Text.DrawOut(T * 3, H, M);
      Font.Style := [];
      for J := 1 to Creatures.PC.Inv.Count do
      begin
        Inc(R);
        Font.Color := cDkYellow;
        V := Items.ItemIndex(J);
        if Creatures.PC.Inv.GetDoll(J) then
          Font.Style := [fsBold]
        else
          Font.Style := [];
        if (Creatures.PC.Inv.GetCount(J) > 1) then
          F := ' (' + IntToStr(Creatures.PC.Inv.GetCount(J)) + 'x)'
        else
          F := '';
        Graph.Text.DrawText(T * 3, H + R, GetItemLang(DungeonItems[V].Sprite) +
          F + Items.GetDollText(J, V));
      end;
      //
      if (Count = 1) then
        Graph.Text.BarOut('a', GetLang(181), False)
      else if (Count > 1) then
        Graph.Text.BarOut('a-' + Chr(96 + Count), GetLang(181), False);
      //
      Creatures.PC.Race := CursorPos - 1;
      SceneInv.RedrawPCIcon;
      Draw((L * Graph.CharWidth) - 72, (H + 1) * Graph.CharHeight,
        SceneInv.Hero);
    end;
    Graph.Render;
  except
    on E: Exception do
      Error.Add('SceneRace.Render', E.Message);
  end;
end;

initialization

SceneRace := TSceneRace.Create;

finalization

SceneRace.Free;

end.
