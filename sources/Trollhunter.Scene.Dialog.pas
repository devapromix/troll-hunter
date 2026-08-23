unit Trollhunter.Scene.Dialog;

interface

uses
  Classes,
  Trollhunter.UI,
  Trollhunter.UI.Log,
  Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneDialog = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

uses
  SysUtils,
  BearLibTerminal,
  Trollhunter.Quest,
  Trollhunter.Item,
  Trollhunter.Dialog,
  Trollhunter.Item.Shop,
  Trollhunter.Attribute,
  Trollhunter.Player,
  Trollhunter.Player.Helpers,
  Trollhunter.Terminal;

const
  CLetters: array [0 .. 25] of char = ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
    'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
    'X', 'Y', 'Z');
  CLetterKeys: array [0 .. 25] of UInt = (TK_A, TK_B, TK_C, TK_D, TK_E, TK_F,
    TK_G, TK_H, TK_I, TK_J, TK_K, TK_L, TK_M, TK_N, TK_O, TK_P, TK_Q, TK_R,
    TK_S, TK_T, TK_U, TK_V, TK_W, TK_X, TK_Y, TK_Z);

{ TSceneDialog }

procedure TSceneDialog.Render;
var
  LIndex: Int;
  LAction: TNPCActionEnum;
  LShop: TShopEnum;
  LValue: Int;
  S: string;

  procedure Add(const AText: string);
  begin
    Inc(Y);
    Terminal.Print(1, Y, UI.KeyToStr(CLetters[LIndex]) + ' ' + AText,
      TK_ALIGN_LEFT);
    Inc(LIndex);
  end;

  function GetActionLabel(const AAction: TNPCActionEnum): string;
  begin
    case AAction of
      naHeal:
        begin
          LValue := Player.Attributes.Attrib[atMaxLife].Value -
            Player.Attributes.Attrib[atLife].Value;
          if (LValue > 0) then
            S := ' (' + Items.GetIcon(LValue, 'Life') + ' ' +
              Items.GetPrice(Round(LValue * 1.6)) + ')'
          else
            S := '';
          Result := 'Heal me, please' + S;
        end;
      naRepair:
        Result := 'Can you repair my gear?';
      naSell:
        Result := 'I want to sell something';
      naBuyArrows:
        begin
          LValue := Player.GetArrowsToBuy;
          if (LValue > 0) then
            S := ' (' + Items.GetIcon(LValue, 'Arrow') + ' ' +
              Items.GetPrice(LValue) + ')'
          else
            S := '';
          Result := 'I need more arrows' + S;
        end;
      naIdentify:
        begin
          if Player.HasUnidentifiedItems then
            S := ' (' + Items.GetPrice(CIdentifyAllItemsCost) + ')'
          else
            S := '';
          Result := 'Can you identify my items?' + S;
        end;
    end;
  end;

begin
  UI.Title(NPCName + ' ' + UI.GoldLeft(Player.Gold));
  UI.FromAToZ;
  Y := 1;
  LIndex := 0;

  for LAction := Low(TNPCActionEnum) to High(TNPCActionEnum) do
    if (LAction in NPCActions) then
      Add(GetActionLabel(LAction));

  for LShop := Low(TShopEnum) to High(TShopEnum) do
    if (LShop in NPCShops) then
      Add(NPCShopQuestion[LShop]);

  { if (naQuest in NPCActions) then
      Add('The Hunt (quest)'); }

  MsgLog.Render(2, True);

  AddKey('Esc', 'Close', True);
end;

procedure TSceneDialog.Update(var Key: UInt);
var
  LIndex: Int;
  LAction: TNPCActionEnum;
  LShop: TShopEnum;

  function KeyMatches: Boolean;
  begin
    Result := (Key = CLetterKeys[LIndex]);
    Inc(LIndex);
  end;

  procedure DoAction(const AAction: TNPCActionEnum);
  begin
    case AAction of
      naHeal:
        Player.ReceiveHealing;
      naRepair:
        begin
          Items.Index := 0;
          Scenes.SetScene(scRepair, scDialog);
        end;
      naSell:
        Scenes.SetScene(scSell);
      naBuyArrows:
        Player.BuyArrows;
      naIdentify:
        Player.IdentifyAllItems;
      { naQuest:
          begin
            Quests.Current := qeKillNBears;
            Scenes.SetScene(scQuest, scDialog);
          end; }
    end;
  end;

  procedure DoShop(const AShop: TShopEnum);
  begin
    Shops.Current := AShop;
    Scenes.SetScene(scBuy, scDialog);
  end;

begin
  if (Key = TK_ESCAPE) then
  begin
    Scenes.SetScene(scGame);
    Exit;
  end;

  LIndex := 0;

  for LAction := Low(TNPCActionEnum) to High(TNPCActionEnum) do
    if (LAction in NPCActions) and KeyMatches then
    begin
      DoAction(LAction);
      Exit;
    end;

  for LShop := Low(TShopEnum) to High(TShopEnum) do
    if (LShop in NPCShops) and KeyMatches then
    begin
      DoShop(LShop);
      Exit;
    end;
end;

end.
