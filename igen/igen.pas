unit igen;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    RadioGroup1: TRadioGroup;
    RichEdit1: TRichEdit;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  I: Integer;
  S: string;
  Def, DamMin, DamMax: Integer;
begin
  RichEdit1.Clear;
  for I := 1 to 10 do
  begin
    case RadioGroup1.ItemIndex of
    0: begin
         Def := I;
         DamMin := 1 + (I * 3) + Round(I / 4);
         DamMax := 2 + Round(DamMin * 1.7);
         S := Format('Level: %d; Defense: %d; Damage: (Min: %d; Max: %d;);',
           [I, Def, DamMin, DamMax]);
       end;
    1: begin
         Def := I;
         DamMin := 1 + Round(I * 3.9);
         DamMax := 2 + Round(DamMin * 1.3);
         S := Format('Level: %d; Defense: %d; Damage: (Min: %d; Max: %d;);',
           [I, Def, DamMin, DamMax]);
       end;
    2: begin
         Def := 1 + (I div 2);
         DamMin := 1 + I;
         DamMax := 3 + I * I;
         S := Format('Level: %d; Defense: %d; Damage: (Min: %d; Max: %d;);',
           [I, Def, DamMin, DamMax]);
       end;
    3: begin
         Def := I;
         DamMin := 3 + Round((I / 3) * I);
         DamMax := 3 + Round((I / 2) * I) + (I * 2);
         S := Format('Level: %d; Defense: %d; Damage: (Min: %d; Max: %d;);',
           [I, Def, DamMin, DamMax]);
       end;
    end;
    RichEdit1.Lines.Append(S);
  end;
    
end;

end.
