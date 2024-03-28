object Form1: TForm1
  Left = 192
  Top = 124
  Caption = 'HiveMap'
  ClientHeight = 636
  ClientWidth = 1289
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object imgMain: TImage
    Left = 0
    Top = 0
    Width = 57
    Height = 57
  end
  object pbWorkspace: TPaintBox
    Left = 0
    Top = 88
    Width = 105
    Height = 105
    OnMouseDown = pbWorkspaceMouseDown
    OnMouseLeave = pbWorkspaceMouseLeave
    OnMouseMove = pbWorkspaceMouseMove
    OnMouseUp = pbWorkspaceMouseUp
  end
  object btnLoad: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Load...'
    TabOrder = 0
    OnClick = btnLoadClick
  end
  object memINI: TMemo
    Left = 969
    Top = 0
    Width = 320
    Height = 185
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object dlgLoad: TOpenDialog
    Left = 8
    Top = 40
  end
end
