object MainForm: TMainForm
  Left = 126
  Top = 151
  Caption = 'Trollhunter'
  ClientHeight = 61
  ClientWidth = 208
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clCream
  Font.Height = -13
  Font.Name = 'Courier New'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  OnPaint = FormPaint
  OnResize = FormPaint
  PixelsPerInch = 96
  TextHeight = 16
  object Timer1: TTimer
    Interval = 200
    OnTimer = Timer1Timer
    Left = 8
    Top = 8
  end
end
