object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 392
  ClientWidth = 494
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 81
    Height = 81
    Caption = 'Weapons'
    ItemIndex = 0
    Items.Strings = (
      'Swords'
      'Axes'
      'Polearms'
      'Hammers')
    TabOrder = 0
  end
  object RichEdit1: TRichEdit
    Left = 8
    Top = 95
    Width = 297
    Height = 289
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Button1: TButton
    Left = 230
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Gen'
    TabOrder = 2
    OnClick = Button1Click
  end
end
