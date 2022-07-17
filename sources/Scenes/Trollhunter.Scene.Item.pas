unit Trollhunter.Scene.Item;

interface

uses
  Classes,
  Graphics,
  Windows,
  Trollhunter.Scene;

type
  TSceneItem = class(TScene)
  private
    FMenuCount: Integer;
    Icon: Graphics.TBitmap;
    Commands: string;
    FCursorPos: Integer;
    FItemIndex: Integer;
    procedure Drink(I: Integer);
    procedure Drop(I: Integer);
    procedure Read(I: Integer);
    procedure Use(I: Integer);
    procedure SetCursorPos(const Value: Integer);
    procedure SetItemIndex(const Value: Integer);
  public
    property CursorPos: Integer read FCursorPos write SetCursorPos;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    function KeyIDToInvItemID(I: Integer): Integer;
    procedure Equip(I: Integer; IsShowLog: Boolean = True);
    procedure UnEquip(I: Integer; IsShowLog: Boolean = True);
    procedure KeyPress(var Key: Char); override;
    procedure Render(); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    constructor Create;
    destructor Destroy; override;
  end;

var
  SceneItem: TSceneItem;

implementation

uses
  SysUtils,
  Trollhunter.Error,
  Trollhunter.Log,
  Trollhunter.Creatures,
  Trollhunter.Scenes,
  Trollhunter.Scene.Inv,
  Trollhunter.Item,
  Trollhunter.Graph,
  Trollhunter.Game,
  Trollhunter.Scene.Game,
  Trollhunter.Color,
  Trollhunter.Utils,
  Trollhunter.Map,
  Trollhunter.Map.Tiles,
  Trollhunter.Lang,
  Trollhunter.Skill;

{ TSceneItem }

constructor TSceneItem.Create;
begin
  Icon := Graphics.TBitmap.Create;
  FMenuCount := 0;
  CursorPos := 0;
end;

destructor TSceneItem.Destroy;
begin
  Icon.Free;
  inherited;
end;

procedure TSceneItem.Equip(I: Integer; IsShowLog: Boolean = True);
var
  V, J: Integer;
  // B: Boolean;
begin
  try
    V := KeyIDToInvItemID(I);
    { B := True;{((DungeonItems[V].MaxTough > 0)
      and (Creatures.PC.Inv.GetTough(I) > 0))
      or (DungeonItems[V].MaxTough = 0); }
    for J := 1 to 26 do
      if (DungeonItems[V].Category = DungeonItems[KeyIDToInvItemID(J)].Category)
      then
        UnEquip(J);
    // if (B and (DungeonItems[V].Category in WeapArmSet))
    // or (DungeonItems[V].Category in AmuRingSet)
    if (DungeonItems[V].Category in WeapArmSet + AmuRingSet) and
      Creatures.PC.Inv.Equip(I) then
    begin
      Inc(Creatures.PC.Prop.MinDamage, DungeonItems[V].MinDamage);
      Inc(Creatures.PC.Prop.MaxDamage, DungeonItems[V].MaxDamage);
      Inc(Creatures.PC.Prop.Protect, DungeonItems[V].Protect);
      Creatures.PC.Calc;

      if IsShowLog then
        Log.Add(Format(GetLang(96), [GetItemLang(DungeonItems[V].Sprite)]));
      // You equip a %s.
      Scenes.Render;
    end;
  except
    on E: Exception do
      Error.Add('SceneItem.Equip', E.Message);
  end;
end;

procedure TSceneItem.UnEquip(I: Integer; IsShowLog: Boolean = True);
begin
  try
    if Creatures.PC.Inv.UnEquip(I) then
    begin
      Dec(Creatures.PC.Prop.MinDamage, DungeonItems[KeyIDToInvItemID(I)
        ].MinDamage);
      Dec(Creatures.PC.Prop.MaxDamage, DungeonItems[KeyIDToInvItemID(I)
        ].MaxDamage);
      Dec(Creatures.PC.Prop.Protect, DungeonItems[KeyIDToInvItemID(I)].Protect);
      Creatures.PC.Calc;

      if IsShowLog then
        Log.Add(Format(GetLang(97),
          [GetItemLang(DungeonItems[KeyIDToInvItemID(I)].Sprite)]));
      // You unequip a %s.
      Scenes.Render;
    end;
  except
    on E: Exception do
      Error.Add('SceneItem.UnEquip', E.Message);
  end;
end;

procedure TSceneItem.KeyDown(var Key: Word; Shift: TShiftState);
var
  I: Integer;
  K: Word;
  C: Char;
begin
  try
    I := KeyIDToInvItemID(ItemIndex);
    case Key of
      27, 123:
        Scenes.Scene := SceneInv;
      38, 40:
        begin
          CursorPos := CursorPos + (Key - 39);
          CursorPos := ClampCycle(CursorPos, 0, FMenuCount - 1);
          Render;
        end;
      13:
        begin
          if (Length(Commands) > 0) then
          begin
            C := Commands[CursorPos + 1];
            K := ord(C);
            KeyDown(K, Shift);
            Exit;
          end;
        end;
      ord('W'):
        if (DungeonItems[I].Category in EquipSet) then
        begin
          if not Creatures.PC.Inv.GetDoll(ItemIndex) then
            Equip(ItemIndex)
          else
            UnEquip(ItemIndex);
          SceneInv.RedrawPCIcon;
          Scenes.Scene := SceneInv;
        end;
      ord('U'):
        if (DungeonItems[I].Category in UseSet) then
        begin
          Use(ItemIndex);
          Log.Apply;
          Game.Save;
          Scenes.Scene := SceneInv;
        end;
      ord('R'):
        if (DungeonItems[I].Category in ScrollSet) then
        begin
          Read(ItemIndex);
          Log.Apply;
          Game.Save;
          Scenes.Scene := SceneGame;
        end;
      ord('Q'):
        if (DungeonItems[I].Category in PotionSet) then
        begin
          Drink(ItemIndex);
          Log.Apply;
          Game.Save;
          Scenes.Scene := SceneGame;
        end;
      ord('D'):
        if (DungeonItems[I].Category in DropSet) and
          not Creatures.PC.Inv.GetDoll(ItemIndex) then
        begin
          Drop(ItemIndex);
          Log.Apply;
          Game.Save;
          Creatures.PC.Redraw;
          Scenes.Scene := SceneGame;
        end;
    end;
  except
    on E: Exception do
      Error.Add('SceneItem.KeyDown (#' + IntToStr(Key) + ')', E.Message);
  end;
end;

procedure TSceneItem.KeyPress(var Key: Char);
begin
  inherited;

end;

procedure TSceneItem.Render;
var
  Tileset: Graphics.TBitmap;
  P, I, T: Integer;
  ID: string;

  procedure Add(S: string);
  begin
    S := Trim(S);
    if (S = '') then
      Exit;
    Graph.Text.TextCenter(P, S);
    Inc(P);
  end;

  procedure AddMenu(S: string);
  begin
    with Graph.Surface.Canvas do
    begin
      if (CursorPos = FMenuCount) then
      begin
        Font.Color := cAcColor;
        Font.Style := [fsBold];
        Graph.RenderMenu(FMenuCount + P, Graph.CharHeight);
      end
      else
      begin
        Font.Color := cBgColor;
        Font.Style := [];
      end;
      Graph.Text.TextCenter((FMenuCount + P) + 1, S);
      Inc(FMenuCount);
    end;
  end;

  procedure AddCommand(C: Char; S: string);
  begin
    Graph.Text.BarOut(LowerCase(C), AnsiLowerCase(S), False);
    Commands := Commands + UpperCase(C);
    AddMenu(S);
  end;

  procedure RenderItemInfo(I: Integer);
  begin
    with Graph.Surface.Canvas do
    begin
      Font.Color := cWhiteGre;
      with DungeonItems[I] do
      begin
        // Life
        if (scLife in SubCats) then
          Add(GetLang(223));
        if (scLife25 in SubCats) then
          Add(Format(GetLang(81), [25]));
        if (scLife50 in SubCats) then
          Add(Format(GetLang(81), [50]));
        if (scLife75 in SubCats) then
          Add(Format(GetLang(81), [75]));
        if (scLife100 in SubCats) then
          Add(Format(GetLang(81), [100]));
        if (scLife200 in SubCats) then
          Add(Format(GetLang(81), [200]));
        // Mana
        if (scMana in SubCats) then
          Add(GetLang(224));
        if (scMana25 in SubCats) then
          Add(Format(GetLang(82), [25]));
        if (scMana50 in SubCats) then
          Add(Format(GetLang(82), [50]));
        if (scMana75 in SubCats) then
          Add(Format(GetLang(82), [75]));
        if (scMana100 in SubCats) then
          Add(Format(GetLang(82), [100]));
        if (scMana200 in SubCats) then
          Add(Format(GetLang(82), [200]));
        // Atr
        if (scStrength in SubCats) then
          Add(Format('%s +1.', [GetLang(15)]));
        if (scDexterity in SubCats) then
          Add(Format('%s +1.', [GetLang(16)]));
        if (scWill in SubCats) then
          Add(Format('%s +1.', [GetLang(17)]));
        if (scSpeed in SubCats) then
          Add(Format('%s +1.', [GetLang(18)]));
        // Misc
        if (scFill in SubCats) then
          Add(GetLang(80));
        if (scAntidote in SubCats) then
          Add(GetLang(79));
        if (scKey in SubCats) then
          Add(GetLang(112));
        if (scTeleport in SubCats) then
          Add(GetLang(272));
        if (scSummon in SubCats) then
          Add(GetLang(273));
        if (scIdentify in SubCats) then
          Add(GetLang(274));
        if (scPortal in SubCats) then
          Add(GetLang(275));
        if (scWizardEye in SubCats) then
          Add(Format('%s %d.', [GetLang(115),
            Creatures.PC.TempSys.Power('WizardEye')]));
        if (scDispel in SubCats) then
          Add(GetLang(230));
        // Repair
        if (scRepair in SubCats) then
          Add(GetLang(270));
        if (scRepairAll in SubCats) then
          Add(GetLang(271));
        if (scRepair3 in SubCats) then
          Add(Format('%s 3.', [GetLang(89)]));
        if (scRepair6 in SubCats) then
          Add(Format('%s 6.', [GetLang(89)]));
        if (scRepair9 in SubCats) then
          Add(Format('%s 9.', [GetLang(89)]));
        if (scRepair12 in SubCats) then
          Add(Format('%s 12.', [GetLang(89)]));
        if (scRepair15 in SubCats) then
          Add(Format('%s 15.', [GetLang(89)]));
        if (scRepair25 in SubCats) then
          Add(Format('%s 25.', [GetLang(89)]));
      end;
      //
      Font.Color := cSkyBlue;
      if (DungeonItems[I].MaxDamage > 0) then
        Add(Format('%s %d-%d', [GetLang(32), DungeonItems[I].MinDamage,
          DungeonItems[I].MaxDamage]));
      if (DungeonItems[I].Protect > 0) then
        Add(Format('%s %d', [GetLang(33), DungeonItems[I].Protect]));
      // Bonuses
      Font.Color := cSkyBlue;
      if (DungeonItems[I].BonusStrength > 0) then
        Add(Format('%s %d', [GetLang(15), DungeonItems[I].BonusStrength]));
      if (DungeonItems[I].BonusDexterity > 0) then
        Add(Format('%s %d', [GetLang(16), DungeonItems[I].BonusDexterity]));
      if (DungeonItems[I].BonusWill > 0) then
        Add(Format('%s %d', [GetLang(17), DungeonItems[I].BonusWill]));
      if (DungeonItems[I].BonusSpeed > 0) then
        Add(Format('%s %d', [GetLang(18), DungeonItems[I].BonusSpeed]));
      if (DungeonItems[I].BonusLife > 0) then
        Add(Format('%s %d', [GetLang(22), DungeonItems[I].BonusLife]));
      if (DungeonItems[I].BonusMana > 0) then
        Add(Format('%s %d', [GetLang(23), DungeonItems[I].BonusMana]));
      //
      Font.Color := cWhiteGre;
      if (DungeonItems[I].ManaCost > 0) then
        Add(Format('%s -%d (%d/%d)', [GetLang(23), DungeonItems[I].ManaCost,
          Creatures.PC.Mana.Cur, Creatures.PC.Mana.Max]));
      if (DungeonItems[I].NeedMagic > 0) then
        Add(Format('%s %d (%d)', [GetLang(280), DungeonItems[I].NeedMagic,
          Creatures.PC.Skill.GetSkill(skMagic, True)]));
      //
      Font.Color := cSkyBlue;
      if (DungeonItems[I].Weight > 0) then
        Add(GetLang(42) + Items.GetWeight(I));
      if (DungeonItems[I].MaxTough > 0) then
      begin
        if (Creatures.PC.Inv.GetTough(ItemIndex) <= 0) then
          Font.Color := cRdRed;
        Add(Format('%s %d/%d', [GetLang(40),
          Creatures.PC.Inv.GetTough(ItemIndex), DungeonItems[I].MaxTough]));
      end;
    end;
  end;

begin
  Commands := '';
  FMenuCount := 0;
  Tileset := Graphics.TBitmap.Create;
  with Graph do
    try
      I := KeyIDToInvItemID(ItemIndex);
      ID := DungeonItems[I].Sprite;
      P := Height div CharHeight div 2 - 5;
      Clear(0);
      if (DungeonItems[I].AdvSprite = '') then
        Tileset.Handle := Windows.LoadBitmap(hInstance, PChar(ID))
      else
        Tileset.Handle := Windows.LoadBitmap(hInstance,
          PChar(DungeonItems[I].AdvSprite));
      BitmapFromTileset(Icon, Tileset, 0);
      Items.Colors(Icon, I);
      ScaleBmp(Icon, 64, 64);
      Icon.Transparent := True;
      with Surface.Canvas do
      begin
        Draw((Surface.Width div 2) - 32, ((P - 2) * CharHeight) - 64, Icon);
        Text.TitleOut(GetItemLang(DungeonItems[I].Sprite), P - 1);
        Inc(P, 2);
        T := DungeonItems[I].ColorTag;
        if (T = 0) then
          RenderItemInfo(I)
        else
        begin
          if (DungeonItems[I].Category = dsScroll) and
            Creatures.PC.Scrolls.IsDefined(T) then
            RenderItemInfo(I)
          else
            Add(GetLang(23));
          if (DungeonItems[I].Category = dsPotion) and
            Creatures.PC.Potions.IsDefined(T) then
            RenderItemInfo(I)
          else
            Add(GetLang(23));
        end;
      end;
      Text.BarOut('esc', GetLang(49), True);
      if (DungeonItems[I].Category in EquipSet) then
        AddCommand('W', GetLang(95));
      if (DungeonItems[I].Category in UseSet) then
        AddCommand('U', GetLang(98));
      if (DungeonItems[I].Category in PotionSet) then
        AddCommand('Q', GetLang(93));
      if (DungeonItems[I].Category in ScrollSet) then
        AddCommand('R', GetLang(99));
      if (DungeonItems[I].Category in DropSet) and
        not Creatures.PC.Inv.GetDoll(ItemIndex) then
        AddCommand('D', GetLang(90));
      Render;
      Tileset.Free;
    except
      on E: Exception do
        Error.Add('SceneItem.Render', E.Message);
    end;
end;

function TSceneItem.KeyIDToInvItemID(I: Integer): Integer;
begin
  Result := Items.ItemIndex(I);
end;

procedure TSceneItem.Drop(I: Integer);
var
  J, T, C: Integer;
begin
  try
    if not(Map.Cell[Creatures.PC.Pos.Y][Creatures.PC.Pos.X].Tile in FloorSet +
      [tlOpenWoodChest, tlOpenBestChest, tlOpenBarrel]) then
      Exit;
    T := Creatures.PC.Inv.GetTough(I);
    J := KeyIDToInvItemID(I);
    C := Creatures.PC.Inv.GetCount(DungeonItems[J].Sprite);
    if Creatures.PC.Inv.Del(I, C) then
    begin
      if (C = 1) then
      begin
        Log.Add(Format(GetLang(91), [GetItemLang(DungeonItems[J].Sprite)]));
        Items.Add(Creatures.PC.Pos.X, Creatures.PC.Pos.Y,
          DungeonItems[J].Sprite);
        with Items.Item[High(Items.Item)] do
        begin
          Prop.Tough := T;
          Count := 1;
        end;
      end
      else
      begin
        Log.Add(Format(GetLang(92), [GetItemLang(DungeonItems[J].Sprite), C]));
        Items.Add(Creatures.PC.Pos.X, Creatures.PC.Pos.Y,
          DungeonItems[J].Sprite);
        Items.Item[High(Items.Item)].Count := C;
      end;
    end;
  except
    on E: Exception do
      Error.Add('SceneItem.Drop', E.Message);
  end;
end;

procedure TSceneItem.Use(I: Integer);
var
  J: Integer;
begin
  try
    J := KeyIDToInvItemID(I);
    SceneInv.ItemUseID := DungeonItems[J].Sprite;
    Scenes.Scene := SceneInv;
  except
    on E: Exception do
      Error.Add('SceneItem.Use', E.Message);
  end;
end;

procedure TSceneItem.Drink(I: Integer);
var
  J, T: Integer;
begin
  try
    J := KeyIDToInvItemID(I);
    with Creatures.PC do
      if Inv.Del(I, 1) then
      begin
        T := DungeonItems[J].ColorTag;
        Log.Add(Format(GetLang(94), [GetItemLang(DungeonItems[J].Sprite)]));
        if (T > 0) and not Creatures.PC.Potions.IsDefined(T) then
        begin
          Creatures.PC.Potions.SetDefined(T);
          Log.Add(GetLang(225) + ' ' +
            AnsiLowerCase(GetItemLang(DungeonItems[J].Sprite)) + '.');
        end;
        Items.UseItem(J, PotionSet);
      end;
  except
    on E: Exception do
      Error.Add('SceneItem.Drink', E.Message);
  end;
end;

procedure TSceneItem.Read(I: Integer);
var
  J, T: Integer;
begin
  try
    J := KeyIDToInvItemID(I);
    with Creatures.PC do
    begin
      if (Mana.Cur >= DungeonItems[J].ManaCost) then
      begin
        if Inv.Del(I, 1) then
        begin
          T := DungeonItems[J].ColorTag;
          Log.Add(Format(GetLang(100), [GetItemLang(DungeonItems[J].Sprite)]));
          if (T > 0) and not Creatures.PC.Scrolls.IsDefined(T) then
          begin
            Creatures.PC.Scrolls.SetDefined(T);
            Log.Add(GetLang(220) + ' ' +
              AnsiLowerCase(GetItemLang(DungeonItems[J].Sprite)) + '.');
          end;
          Mana.Dec(DungeonItems[J].ManaCost);
          Items.UseItem(J, ScrollSet);
        end;
      end
      else
      begin
        Log.Add(GetLang(67));
      end;
    end;
  except
    on E: Exception do
      Error.Add('SceneItem.Read', E.Message);
  end;
end;

procedure TSceneItem.SetCursorPos(const Value: Integer);
begin
  FCursorPos := Value;
end;

procedure TSceneItem.SetItemIndex(const Value: Integer);
begin
  FItemIndex := Value;
end;

initialization

SceneItem := TSceneItem.Create;

finalization

SceneItem.Free;

end.
