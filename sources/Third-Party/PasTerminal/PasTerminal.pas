unit PasTerminal;

interface

uses
  BearLibTerminal, System.Types;

type
  TEntSize = record
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
  end;

type
  TPasTerminal = class(TObject)
  private

  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Refresh;
    function Pick(const AX, AY: Word): Word;
    procedure BackgroundColor(Value: Cardinal); overload;
    procedure ForegroundColor(Value: Cardinal); overload;
    function Colorize(const AStr, AColor: string): string; overload;
    function Colorize(const ANum: Integer; const AColor: string): string; overload;
    function GetColor(Color: Integer): Cardinal;
    function GetColorFromIni(AKey: string): string; overload;
    function GetColorFromIni(AKey: string; ADefault: string): Cardinal; overload;
    procedure Print(AX, AY: Integer; AText: string); overload;
    procedure Print(AX, AY: Integer; AText: string; Align: Integer); overload;
    procedure Print(AX, AY: Integer; AText: string; AForegroundColor: Cardinal; ABackgroundColor: Cardinal); overload;
    procedure Print(ALeft, ATop, AWidth, AHeight: Integer; AText: string; Align: Word); overload;
    function SetEntSize(const ALeft, ATop, AWidth, AHeight: Word): TEntSize;
  end;

implementation

uses
  System.SysUtils;

{ TPasTerminal }

procedure TPasTerminal.BackgroundColor(Value: Cardinal);
begin
  terminal_bkcolor(Value);
end;

procedure TPasTerminal.Clear;
begin
  terminal_clear();
end;

constructor TPasTerminal.Create;
begin
  terminal_open();
  terminal_refresh();
end;

destructor TPasTerminal.Destroy;
begin
  terminal_close();
  inherited;
end;

procedure TPasTerminal.ForegroundColor(Value: Cardinal);
begin
  terminal_color(Value);
end;

procedure TPasTerminal.Refresh;
begin
  terminal_refresh;
end;

function TPasTerminal.Colorize(const ANum: Integer; const AColor: string): string;
begin
  Result := Format('[color=%s]%d[/color]', [LowerCase(AColor), ANum]);
end;

function TPasTerminal.Colorize(const AStr, AColor: string): string;
begin
  Result := Format('[color=%s]%s[/color]', [LowerCase(AColor), AStr]);
end;

function TPasTerminal.GetColor(Color: Integer): Cardinal;
begin
  Result := color_from_argb($FF, Word(Color), Word(Color shr 8), Word(Color shr 16));
end;

function TPasTerminal.GetColorFromIni(AKey: string; ADefault: string): Cardinal;
var
  S: string;
begin
  S := GetColorFromIni(AKey);
  if (S = '') then
    S := ADefault;
  Result := color_from_name(S);
end;

function TPasTerminal.GetColorFromIni(AKey: string): string;
begin
  Result := LowerCase(terminal_get('ini.colors.' + LowerCase(AKey)));
end;

function TPasTerminal.Pick(const AX, AY: Word): Word;
begin
  Result := terminal_pick(AX, AY, 0);
end;

procedure TPasTerminal.Print(AX, AY: Integer; AText: string);
begin
  terminal_print(AX, AY, TK_ALIGN_DEFAULT, AText);
end;

procedure TPasTerminal.Print(AX, AY: Integer; AText: string; Align: Integer);
begin
  terminal_print(AX, AY, Align, AText);
end;

procedure TPasTerminal.Print(ALeft, ATop, AWidth, AHeight: Integer; AText: string; Align: Word);
begin
  terminal_print(ALeft, ATop, AWidth, AHeight, Align, AText);
end;

procedure TPasTerminal.Print(AX, AY: Integer; AText: string; AForegroundColor: Cardinal; ABackgroundColor: Cardinal);
begin
  terminal_bkcolor(ABackgroundColor);
  terminal_color(AForegroundColor);
  terminal_print(AX, AY, TK_ALIGN_DEFAULT, AText);
end;

function TPasTerminal.SetEntSize(const ALeft, ATop, AWidth, AHeight: Word): TEntSize;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Width := AWidth;
  Result.Height := AHeight;
end;

end.
