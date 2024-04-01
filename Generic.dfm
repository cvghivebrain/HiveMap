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
    Top = 70
    Width = 105
    Height = 105
    OnMouseDown = pbWorkspaceMouseDown
    OnMouseLeave = pbWorkspaceMouseLeave
    OnMouseMove = pbWorkspaceMouseMove
    OnMouseUp = pbWorkspaceMouseUp
  end
  object pbPalette: TPaintBox
    Left = 969
    Top = 184
    Width = 320
    Height = 80
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
  object menuZoom: TComboBox
    Left = 104
    Top = 8
    Width = 49
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 2
    Text = '1x'
    OnChange = menuZoomChange
    Items.Strings = (
      '1x'
      '2x'
      '3x'
      '4x'
      '5x')
  end
  object btnSave: TButton
    Left = 8
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 3
    OnClick = btnSaveClick
  end
  object dlgLoad: TOpenDialog
    Left = 296
    Top = 8
  end
end
