object Form1: TForm1
  Left = 192
  Top = 124
  Caption = 'HiveMap 1.0'
  ClientHeight = 602
  ClientWidth = 1128
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  OnResize = FormResize
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
    OnMouseEnter = pbWorkspaceMouseEnter
    OnMouseLeave = pbWorkspaceMouseLeave
    OnMouseMove = pbWorkspaceMouseMove
    OnMouseUp = pbWorkspaceMouseUp
  end
  object pbPalette: TPaintBox
    Left = 807
    Top = 497
    Width = 320
    Height = 80
    OnMouseDown = pbPaletteMouseDown
  end
  object lblGrid: TLabel
    Left = 196
    Top = 35
    Width = 49
    Height = 13
    Caption = 'Show Grid'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblSnap: TLabel
    Left = 196
    Top = 54
    Width = 59
    Height = 13
    Caption = 'Snap to Grid'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblPiece: TLabel
    Left = 807
    Top = 280
    Width = 27
    Height = 13
    Caption = 'Piece'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object pbPiece: TPaintBox
    Left = 807
    Top = 299
    Width = 192
    Height = 192
    OnMouseDown = pbPieceMouseDown
  end
  object lblHi: TLabel
    Left = 1022
    Top = 300
    Width = 56
    Height = 13
    Caption = 'High Priority'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
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
    Left = 807
    Top = 0
    Width = 320
    Height = 185
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
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
      '1x')
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
  object editSprite: TLabeledEdit
    Left = 807
    Top = 208
    Width = 320
    Height = 26
    EditLabel.Width = 27
    EditLabel.Height = 13
    EditLabel.Caption = 'Sprite'
    EditLabel.Color = clWhite
    EditLabel.Font.Charset = DEFAULT_CHARSET
    EditLabel.Font.Color = clWhite
    EditLabel.Font.Height = -11
    EditLabel.Font.Name = 'MS Sans Serif'
    EditLabel.Font.Style = []
    EditLabel.ParentColor = False
    EditLabel.ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    Text = ''
    OnChange = editSpriteChange
  end
  object editGrid: TEdit
    Left = 176
    Top = 8
    Width = 121
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    Text = '40'
    OnChange = editGridChange
  end
  object chkGrid: TCheckBox
    Left = 176
    Top = 35
    Width = 97
    Height = 17
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    TabOrder = 6
    OnClick = chkGridClick
  end
  object chkSnap: TCheckBox
    Left = 176
    Top = 51
    Width = 97
    Height = 17
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    TabOrder = 7
    OnClick = chkGridClick
  end
  object btnDelete: TButton
    Left = 807
    Top = 240
    Width = 69
    Height = 34
    Caption = 'Delete Sprite'
    TabOrder = 8
    OnClick = btnDeleteClick
  end
  object btnDelPieces: TButton
    Left = 882
    Top = 240
    Width = 69
    Height = 33
    Caption = 'Delete Sprite + Pieces'
    TabOrder = 9
    WordWrap = True
    OnClick = btnDelPiecesClick
  end
  object chkHi: TCheckBox
    Left = 1005
    Top = 299
    Width = 97
    Height = 17
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 10
    OnClick = chkHiClick
  end
  object btnHigh: TButton
    Left = 957
    Top = 240
    Width = 69
    Height = 33
    Caption = 'All Pieces High Priority'
    TabOrder = 11
    WordWrap = True
    OnClick = btnHighClick
  end
  object btnLow: TButton
    Left = 1032
    Top = 240
    Width = 69
    Height = 33
    Caption = 'All Pieces Low Priority'
    TabOrder = 12
    WordWrap = True
    OnClick = btnLowClick
  end
  object dlgLoad: TOpenDialog
    Left = 480
    Top = 8
  end
end
