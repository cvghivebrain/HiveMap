unit Generic;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, ExtCtrls, StdCtrls, ScanLineFunc, pngimage, CRCFunc,
  ExplodeFunc, FileFunc, SolveFunc, Math, MiscFunc, Vcl.Mask;

type
  TForm1 = class(TForm)
    imgMain: TImage;
    btnLoad: TButton;
    dlgLoad: TOpenDialog;
    pbWorkspace: TPaintBox;
    memINI: TMemo;
    menuZoom: TComboBox;
    pbPalette: TPaintBox;
    btnSave: TButton;
    editSprite: TLabeledEdit;
    editGrid: TEdit;
    chkGrid: TCheckBox;
    chkSnap: TCheckBox;
    lblGrid: TLabel;
    lblSnap: TLabel;
    lblPiece: TLabel;
    pbPiece: TPaintBox;
    btnDelete: TButton;
    btnDelPieces: TButton;
    chkHi: TCheckBox;
    lblHi: TLabel;
    btnHigh: TButton;
    btnLow: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure pbWorkspaceMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbWorkspaceMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbWorkspaceMouseLeave(Sender: TObject);
    procedure pbWorkspaceMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure menuZoomChange(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure pbWorkspaceMouseEnter(Sender: TObject);
    procedure editSpriteChange(Sender: TObject);
    procedure chkGridClick(Sender: TObject);
    procedure editGridChange(Sender: TObject);
    procedure pbPaletteMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbPieceMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnDelPiecesClick(Sender: TObject);
    procedure chkHiClick(Sender: TObject);
    procedure btnHighClick(Sender: TObject);
    procedure btnLowClick(Sender: TObject);
  private
    { Private declarations }
    procedure LoadPNG;
    procedure LoadINI;
    procedure UpdateDisplay;
    procedure CreatePal;
    procedure GetMousePos(x, y: integer);
    function MatchPal(col: TColor; pal: integer): boolean;
    function FindSprite(x, y: integer): integer;
    function FindPiece(x, y: integer): integer;
    procedure DeleteSprite(i: integer);
    procedure DeletePiece(i: integer);
    function PieceInSprite(p, s: integer): boolean;
    function GetPieceX(p: integer): integer;
    function GetPieceY(p: integer): integer;
    function GetPieceBits(p: integer): integer;
    function GetPiecePal(p: integer): integer;
    function GetPieceHi(p: integer): integer;
    function GetPieceSize(p: integer): integer;
    function GetPieceW(p: integer): integer;
    function GetPieceH(p: integer): integer;
    procedure SetPieceX(p, x: integer);
    procedure SetPieceY(p, y: integer);
    procedure SetPieceBits(p, b: integer);
    procedure SetPiecePal(p, pal: integer);
    procedure SetPieceHi(p, hi: integer);
    procedure SetPieceSize(p, s: integer);
    function GetSpriteX(s: integer): integer;
    function GetSpriteY(s: integer): integer;
    function GetSpriteW(s: integer): integer;
    function GetSpriteH(s: integer): integer;
    procedure SetSpriteX(s, x: integer);
    procedure SetSpriteY(s, y: integer);
    procedure SetSpriteW(s, w: integer);
    procedure SetSpriteH(s, h: integer);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  pngloaded, drag, wheeldelay, hover: boolean;
  pos_x, pos_y, prev_x, prev_y, scale, palused, layer, spritecount, spriteselect,
    spriteside, grid_w, grid_h, mouseimg_x, mouseimg_y, mousewin_x, mousewin_y,
    piececount, pieceselect, colorselect, color_w, color_h: integer;
  pngpath, pngpathrel, inipath: string;
  palarray: array[0..63] of TColor;
  spritenames: array of string;
  spritetable: array of integer;
  piecetable: array of integer;

const
  max_scale: integer = 6;
  side_top: integer = 1;
  side_bottom: integer = 2;
  side_left: integer = 4;
  side_right: integer = 8;
  spritetable_x: integer = 0;
  spritetable_y: integer = 1;
  spritetable_w: integer = 2;
  spritetable_h: integer = 3;
  spritetable_items: integer = 4;
  piecetable_x: integer = 0;
  piecetable_y: integer = 1;
  piecetable_bits: integer = 2;
  piecetable_size: integer = 3;
  piecetable_items: integer = 4;
  piecebits_pal: integer = 3; // %00000011
  piecebits_hi: integer = $10; // %00010000
  piecewidth: array[0..15] of integer = (8,8,8,8,16,16,16,16,24,24,24,24,32,32,32,32);
  pieceheight: array[0..15] of integer = (8,16,24,32,8,16,24,32,8,16,24,32,8,16,24,32);
  tilecount: array[0..15] of integer = (1,2,3,4,2,4,6,8,3,6,9,12,4,8,12,16);
  corner_dim: array[1..6] of integer = (8,8,10,10,12,12);

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var i: integer;
begin
  InitImage(Form1,imgMain); // Set image width & height to match form.
  pos_x := 0; // Image position.
  pos_y := 0;
  scale := 1; // Default zoom.
  spriteselect := -1; // No sprite selected.
  pieceselect := -1; // No piece selected.
  colorselect := -1;
  color_w := pbPalette.Width div 16;
  color_h := pbPalette.Height div 4;
  grid_w := GetGridW(editGrid.Text); // Read grid size from text box (minimum 8).
  grid_h := GetGridH(editGrid.Text);
  for i := 2 to max_scale do menuZoom.Items.Add(IntToStr(i)+'x'); // Populate zoom menu.
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  MatchWindow; // Set boundaries to match window.
  pbWorkspace.Width := Form1.ClientWidth-pbWorkspace.Left-320;
  pbWorkspace.Height := Form1.ClientHeight-pbWorkspace.Top;
  memINI.Left := Form1.ClientWidth-memINI.Width;
  pbPalette.Left := memINI.Left;
  editSprite.Left := memINI.Left;
  lblPiece.Left := memINI.Left;
  pbPiece.Left := memINI.Left;
  btnDelete.Left := memINI.Left;
  btnDelPieces.Left := memINI.Left+btnDelete.Width+6;
  btnHigh.Left := btnDelPieces.Left+btnDelPieces.Width+6;
  btnLow.Left := btnHigh.Left+btnHigh.Width+6;
  chkHi.Left := pbPiece.Left+pbPiece.Width+6;
  lblHi.Left := chkHi.Left+18;
  UpdateDisplay;
end;

procedure TForm1.UpdateDisplay;
var w, h, x, y, i, j, k, p, cnr: integer;
  col: TColor;
const bg: array[0..2] of byte = (40,44,52); // Background colour.
  sc: array[0..2] of byte = (255,255,255); // Sprite colour.
  pc: array[0..11] of byte = (255,0,0, 255,255,0, 0,255,0, 0,0,255); // Piece colours.
  gc: array[0..2] of byte = (255,128,255); // Grid colour.
begin
  // Background.
  FillScreen(bg[0],bg[1],bg[2]);
  if not pngloaded then exit; // Do nothing further if no PNG is loaded.

  // Image.
  w := Min(PNG.Width-pos_x,pbWorkspace.Width div scale);
  h := Min(PNG.Height-pos_y,pbWorkspace.Height div scale);
  DrawPNG(pos_x,pos_y,w+1,h+1,pbWorkspace.Left,pbWorkspace.Top,scale,scale,0,255,255,255,255); // Draw image.

  // Grid.
  if chkGrid.Checked then
    begin
    i := (grid_w-(pos_x mod grid_w))*scale;
    while i < pbWorkspace.Width do
      begin
      DrawLine(gc[0],gc[1],gc[2],128,i+pbWorkspace.Left,pbWorkspace.Top,i+pbWorkspace.Left,pbWorkspace.Height+pbWorkspace.Top); // Draw vertical grid line.
      i := i+(grid_w*scale);
      end;
    i := (grid_h-(pos_y mod grid_h))*scale;
    while i < pbWorkspace.Height do
      begin
      DrawLine(gc[0],gc[1],gc[2],128,pbWorkspace.Left,i+pbWorkspace.Top,pbWorkspace.Width+pbWorkspace.Left,i+pbWorkspace.Top); // Draw horizontal grid line.
      i := i+(grid_h*scale);
      end;
    end;

  // Sprite boxes.
  for i := 0 to spritecount-1 do // Draw sprite boxes.
    begin
    x := ((GetSpriteX(i)-GetSpriteW(i)-pos_x)*scale)+pbWorkspace.Left;
    y := ((GetSpriteY(i)-GetSpriteH(i)-pos_y)*scale)+pbWorkspace.Top;
    w := GetSpriteW(i)*2*scale;
    h := GetSpriteH(i)*2*scale;
    DrawBox(sc[0],sc[1],sc[2],255,x,y,w,h); // Draw box around sprite.
    DrawGrid(sc[0],sc[1],sc[2],128,x,y,w,h,2,2,false); // Draw 2x2 grid.
    if (layer = 1) and (i = spriteselect) then // Highlight corners of selected sprite.
      begin
      cnr := corner_dim[scale];
      DrawRect(sc[0],sc[1],sc[2],255,x,y,cnr,cnr); // Top left.
      DrawRect(sc[0],sc[1],sc[2],255,x+((w-cnr) div 2),y,cnr,cnr); // Top middle.
      DrawRect(sc[0],sc[1],sc[2],255,x+w-cnr,y,cnr,cnr); // Top right.
      DrawRect(sc[0],sc[1],sc[2],255,x,y+((h-cnr) div 2),cnr,cnr); // Middle left.
      DrawRect(sc[0],sc[1],sc[2],255,x+w-cnr,y+((h-cnr) div 2),cnr,cnr); // Middle right.
      DrawRect(sc[0],sc[1],sc[2],255,x,y+h-cnr,cnr,cnr); // Bottom left.
      DrawRect(sc[0],sc[1],sc[2],255,x+((w-cnr) div 2),y+h-cnr,cnr,cnr); // Bottom middle.
      DrawRect(sc[0],sc[1],sc[2],255,x+w-cnr,y+h-cnr,cnr,cnr); // Bottom right.
      end;
    end;

  // Piece boxes.
  for i := 0 to piececount-1 do // Draw piece boxes.
    begin
    x := ((GetPieceX(i)-pos_x)*scale)+pbWorkspace.Left;
    y := ((GetPieceY(i)-pos_y)*scale)+pbWorkspace.Top;
    w := GetPieceW(i)*scale;
    h := GetPieceH(i)*scale;
    p := GetPiecePal(i)*3;
    DrawBoxFill(pc[p],pc[p+1],pc[p+2],255,pc[p],pc[p+1],pc[p+2],64,x,y,w,h); // Draw box around piece.
    if (layer = 2) and (i = pieceselect) then
      DrawBox(pc[p],pc[p+1],pc[p+2],255,x-2,y-2,w+4,h+4); // Draw second box around selected piece.
    if (GetPieceHi(i) <> 0) and (scale > 1) then DrawTriangleFlat(255,255,255,255,x+w-4,y-8,x+w-8,x+w,y,0);
    end;

  // Right menu.
  DrawRect(bg[0],bg[1],bg[2],255,0,0,Form1.ClientWidth,pbWorkspace.Top); // Clear top area.
  DrawRect(bg[0],bg[1],bg[2],255,pbWorkspace.Width,0,Form1.ClientWidth-pbWorkspace.Width,Form1.ClientHeight); // Clear right area.
  for i := 0 to 63 do
    DrawRect(GetRValue(palarray[i]),GetGValue(palarray[i]),GetBValue(palarray[i]),255,
      pbPalette.Left+((i mod 16)*20),pbPalette.Top+((i div 16)*20),20,20); // Draw palette.
  if colorselect <> -1 then DrawBox2(255,255,255,255,pbPalette.Left+((colorselect mod 16)*color_w),pbPalette.Top+((colorselect div 16)*color_h),color_w,color_h,2); // Highlight color.
  if spriteselect = -1 then
    begin
    editSprite.Text := '';
    editSprite.EditLabel.Caption := 'Sprite';
    end
  else
    begin
    editSprite.Text := spritenames[spriteselect]; // Show name of selected sprite.
    j := 0;
    k := 0;
    for i := 0 to piececount-1 do
      if PieceInSprite(i,spriteselect) then
        begin
        Inc(j); // Count pieces in selected sprite.
        k := k+tilecount[GetPieceSize(i)]; // Count tiles in selected sprite.
        end;
    editSprite.EditLabel.Caption := 'Sprite '+IntToStr(spriteselect+1)+'/'+IntToStr(spritecount)+' ['+Quantity(j,'piece')+'; '+Quantity(k,'tile')+']'; // Show sprite number.
    end;
  if pieceselect = -1 then
    begin
    lblPiece.Caption := 'Piece';
    chkHi.Enabled := false;
    chkHi.Checked := false;
    end
  else
    begin
    lblPiece.Caption := 'Piece '+IntToStr(pieceselect+1)+'/'+IntToStr(piececount); // Show piece number.
    DrawRectStriped(64,64,64,255,128,128,128,255,pbPiece.Left,pbPiece.Top,pbPiece.Width,pbPiece.Height,1,1); // Draw empty pixels.
    w := pbPiece.Width div 32;
    h := pbPiece.Height div 32;
    for i := 0 to 31 do
      for j := 0 to 31 do
        begin
        col := PNG.Pixels[GetPieceX(pieceselect)+j,GetPieceY(pieceselect)+i]; // Read pixel.
        if MatchPal(col,GetPiecePal(pieceselect)) then
          DrawRect(GetRValue(col),GetGValue(col),GetBValue(col),255,pbPiece.Left+(j*w),pbPiece.Top+(i*h),w,h); // Draw pixel if in palette.
        end;
    DrawGrid(255,255,255,255,pbPiece.Left,pbPiece.Top,pbPiece.Width,pbPiece.Height,4,4,true);
    p := GetPiecePal(pieceselect)*3;
    DrawBox2(pc[p],pc[p+1],pc[p+2],255,pbPiece.Left,pbPiece.Top,(GetPieceW(pieceselect)*w)+1,(GetPieceH(pieceselect)*h)+1,2); // Highlight piece.
    DrawBox2(255,255,255,255,pbPalette.Left,pbPalette.Top+(GetPiecePal(pieceselect)*color_h),pbPalette.Width,color_h,2); // Highlight palette line.
    chkHi.Enabled := true;
    if GetPieceHi(pieceselect) = 0 then chkHi.Checked := false
    else chkHi.Checked := true;
    end;
  pic.Refresh;
end;

function TForm1.MatchPal(col: TColor; pal: integer): boolean;
var i: integer;
begin
  result := false; // Assume no match.
  if col = palarray[pal*16] then exit; // Colour matches transparent index.
  for i := 1 to 15 do
    if col = palarray[(pal*16)+i] then
      begin
      result := true; // Match found.
      break; // Stop searching.
      end;
end;

procedure TForm1.btnLoadClick(Sender: TObject);
begin
  if not dlgLoad.Execute then exit;
  memINI.Clear;
  SetLength(spritetable,0);
  SetLength(spritenames,0);
  SetLength(piecetable,0);
  spritecount := 0;
  spriteselect := -1;
  piececount := 0;
  pieceselect := -1;
  if ExtractFileExt(dlgLoad.FileName) = '.png' then
    begin
    pngpath := dlgLoad.FileName;
    pngpathrel := ExtractFileName(pngpath);
    LoadPNG;
    CreatePal;
    inipath := ChangeFileExt(pngpath,'.ini');
    if FileExists(inipath) then LoadINI;
    UpdateDisplay;
    end
  else if ExtractFileExt(dlgLoad.FileName) = '.ini' then
    begin
    pngpath := '';
    inipath := dlgLoad.FileName;
    LoadINI;
    if pngpath = '' then
      begin
      pngpath := ChangeFileExt(inipath,'.png');
      pngpathrel := ExtractFileName(pngpath);
      end;
    LoadPNG;
    UpdateDisplay;
    end
  else ShowMessage('Unknown file type.');
end;

procedure TForm1.LoadPNG;
begin
  if (pngpath <> '') and FileExists(pngpath) then
    begin
    LoadSheet(pngpath); // Make PNG available for display.
    pngloaded := true;
    end
  else ShowMessage('PNG not found.');
end;

procedure TForm1.CreatePal;
var i, j, k: integer;
  col: TColor;
  match: boolean;
begin
  for i := 0 to 63 do palarray[i] := 0; // Clear palette.
  palused := 0; // Start position in palette.
  for i := 0 to PNG.Height-1 do
    for j := 0 to PNG.Width-1 do
      begin
      col := PNG.Pixels[j,i]; // Read colour value from pixel.
      match := false;
      for k := 0 to palused-1 do
        if col = palarray[k] then match := true; // Check if colour is already in palette.
      if not match then
        begin
        palarray[palused] := col; // Save new colour.
        Inc(palused);
        end;
      if palused = 64 then exit; // Finish now if palette is full.
      end;
end;

procedure TForm1.LoadINI;
var inifile: textfile;
  s, s2: string;
begin
  AssignFile(inifile,inipath); // Open ini file.
  Reset(inifile);
  while not eof(inifile) do
    begin
    ReadLn(inifile,s);
    if AnsiPos('image=',s) = 1 then
      begin
      pngpathrel := Explode(s,'image=',1); // Save relative path to PNG.
      pngpath := ExtractFilePath(inipath)+pngpathrel; // Save absolute path to PNG.
      end
    else if AnsiPos('palette=',s) = 1 then
      begin
      s2 := Explode(s,'palette=',1);
      palused := 0;
      while Explode(s2,',',palused) <> '' do
        begin
        palarray[palused] := StrToTColor(Explode(s2,',',palused)); // Write palette.
        Inc(palused);
        end;
      end
    else if AnsiPos('sprite=',s) = 1 then
      begin
      s2 := Explode(s,'sprite=',1);
      SetLength(spritenames,Length(spritenames)+1); // Add sprite.
      SetLength(spritetable,Length(spritetable)+spritetable_items);
      spritenames[spritecount] := Explode(s2,',',0); // Sprite name.
      SetSpriteX(spritecount,StrToInt(Explode(s2,',',1))); // Sprite x pos.
      SetSpriteY(spritecount,StrToInt(Explode(s2,',',2))); // Sprite y pos.
      SetSpriteW(spritecount,StrToInt(Explode(s2,',',3))); // Sprite width.
      SetSpriteH(spritecount,StrToInt(Explode(s2,',',4))); // Sprite height.
      Inc(spritecount);
      end
    else if AnsiPos('piece=',s) = 1 then
      begin
      s2 := Explode(s,'piece=',1);
      SetLength(piecetable,Length(piecetable)+piecetable_items); // Add piece.
      SetPieceX(piececount,StrToInt(Explode(s2,',',0))); // Piece x pos.
      SetPieceY(piececount,StrToInt(Explode(s2,',',1))); // Piece y pos.
      SetPieceBits(piececount,StrToInt(Explode(s2,',',2))); // Piece palette.
      SetPieceSize(piececount,StrToInt(Explode(s2,',',3))); // Piece size.
      Inc(piececount);
      end
    else if AnsiPos('grid=',s) = 1 then
      begin
      editGrid.Text := Explode(s,'grid=',1);
      grid_w := GetGridW(editGrid.Text); // Read grid size from text box (minimum 8).
      grid_h := GetGridH(editGrid.Text);
      end
    else if s <> '' then memINI.Lines.Add(s);
    end;
  CloseFile(inifile);
end;

procedure TForm1.btnSaveClick(Sender: TObject);
var inifile: textfile;
  s: string;
  i: integer;
begin
  AssignFile(inifile,inipath); // Open ini file.
  ReWrite(inifile); // Make file editable.
  WriteLn(inifile,memINI.Text);
  WriteLn(inifile,'image='+pngpathrel);
  s := 'palette=';
  for i := 0 to palused-1 do s := s+TColorToStr(palarray[i])+','; // Convert palette to string.
  Delete(s,Length(s),1); // Remove trailing comma.
  WriteLn(inifile,s); // Write palette.
  s := 'sprite=';
  for i := 0 to spritecount-1 do WriteLn(inifile,s+spritenames[i]+','+IntToStr(GetSpriteX(i))+','+
    IntToStr(GetSpriteY(i))+','+IntToStr(GetSpriteW(i))+','+IntToStr(GetSpriteH(i))); // Write sprite table.
  s := 'piece=';
  for i := 0 to piececount-1 do WriteLn(inifile,s+IntToStr(GetPieceX(i))+','+
    IntToStr(GetPieceY(i))+','+IntToStr(GetPieceBits(i))+','+IntToStr(GetPieceSize(i))); // Write piece table.
  WriteLn(inifile,'grid='+editGrid.Text); // Write grid size.
  CloseFile(inifile);
end;

procedure TForm1.menuZoomChange(Sender: TObject);
begin
  scale := menuZoom.ItemIndex+1;
  UpdateDisplay;
end;

procedure TForm1.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if not hover then exit;
  wheeldelay := not wheeldelay;
  if wheeldelay then exit; // Do nothing every other wheel tick.
  if scale = 1 then exit; // Minimum scale.
  Dec(scale);
  pos_x := Max(mouseimg_x-(mousewin_x div scale),0);
  pos_y := Max(mouseimg_y-(mousewin_y div scale),0);
  menuZoom.ItemIndex := scale-1;
  UpdateDisplay;
end;

procedure TForm1.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if not hover then exit;
  wheeldelay := not wheeldelay;
  if wheeldelay then exit; // Do nothing every other wheel tick.
  if scale = max_scale then exit; // Maximum scale.
  menuZoom.ItemIndex := scale;
  Inc(scale);
  pos_x := mouseimg_x-(mousewin_x div scale);
  pos_y := mouseimg_y-(mousewin_y div scale);
  UpdateDisplay;
end;

procedure TForm1.pbPaletteMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if layer = 0 then
    begin
    colorselect := (X div color_w)+((Y div color_h)*16); // Select individual colour.
    UpdateDisplay;
    exit;
    end;
  if pieceselect <> -1 then
    begin
    SetPiecePal(pieceselect,Y div color_h); // Change palette of piece.
    UpdateDisplay;
    end;
end;

procedure TForm1.pbPieceMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if pieceselect <> -1 then
    begin
    SetPieceSize(pieceselect,(Y div (pbPiece.Height div 4))+((X div (pbPiece.Width div 4))*4)); // Change size of piece.
    UpdateDisplay;
    end;
end;

procedure TForm1.pbWorkspaceMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var i, j: integer;
begin
  if colorselect <> -1 then
    begin
    palarray[colorselect] := PNG.Pixels[mouseimg_x,mouseimg_y]; // Get colour at mouse pointer.
    Screen.Cursor := crArrow; // Change mouse pointer back to pointer.
    colorselect := -1; // Unselect colour.
    UpdateDisplay;
    exit;
    end;
  if Button = mbLeft then // Left click.
    begin
    layer := 0; // Assume the background was selected.
    pieceselect := -1; // Assume no piece selected.
    drag := true; // Start dragging whatever is under mouse.
    prev_x := X;
    prev_y := Y;
    if ssCtrl in Shift then
      begin
      UpdateDisplay;
      exit; // Always drag background if CTRL key is held.
      end;
    i := FindSprite(mouseimg_x,mouseimg_y); // Find sprite under mouse pointer.
    if i <> -1 then
      begin
      layer := 1; // Select sprite layer.
      case Screen.Cursor of
        crSizeNWSE:
          if mouseimg_x < GetSpriteX(i) then spriteside := side_top+side_left
          else spriteside := side_bottom+side_right;
        crSizeNESW:
          if mouseimg_x < GetSpriteX(i) then spriteside := side_bottom+side_left
          else spriteside := side_top+side_right;
        crSizeWE:
          if mouseimg_x < GetSpriteX(i) then spriteside := side_left
          else spriteside := side_right;
        crSizeNS:
          if mouseimg_y < GetSpriteY(i) then spriteside := side_top
          else spriteside := side_bottom;
      else spriteside := 0;
      end;
      spriteselect := i;
      end
    else spriteselect := -1; // Background was clicked.
    i := FindPiece(mouseimg_x,mouseimg_y); // Find piece under mouse pointer.
    if i <> -1 then
      begin
      layer := 2; // Select piece layer.
      pieceselect := i;
      end;
    end
  else if Button = mbRight then // Right click.
    begin
    layer := 0;
    i := FindSprite(mouseimg_x,mouseimg_y); // Find sprite under mouse pointer.
    if i <> -1 then
      begin
      layer := 2; // Select piece layer.
      spriteselect := i; // Select clicked sprite.
      j := piececount;
      Inc(piececount); // Add piece.
      SetLength(piecetable,piececount*piecetable_items);
      SetPieceX(j,mouseimg_x-16);
      SetPieceY(j,mouseimg_y-16);
      SetPieceBits(j,0);
      SetPieceSize(j,15);
      pieceselect := j; // Select new piece.
      end;
    if layer = 0 then // Background was right-clicked.
      begin
      i := spritecount;
      Inc(spritecount); // Add sprite.
      SetLength(spritenames,spritecount);
      spritenames[i] := 'sprite'+IntToStr(i); // Give sprite name "sprite#".
      SetLength(spritetable,spritecount*spritetable_items);
      if chkSnap.Checked then
        begin
        SetSpriteX(i,Nearest(mouseimg_x,grid_w));
        SetSpriteY(i,Nearest(mouseimg_y,grid_h));
        end
      else
        begin
        SetSpriteX(i,mouseimg_x);
        SetSpriteY(i,mouseimg_y);
        end;
      SetSpriteW(i,Max(Solve(editGrid.Text)-8,8));
      SetSpriteH(i,Max(Solve(editGrid.Text)-8,8));
      spriteselect := i; // Select new sprite.
      layer := 1;
      end;
    end;
  UpdateDisplay;
end;

function TForm1.FindSprite(x, y: integer): integer;
var i: integer;
begin
  result := -1; // Assume no sprite found.
  for i := spritecount-1 downto 0 do
    if (Abs(x-GetSpriteX(i)) < GetSpriteW(i)) and (Abs(y-GetSpriteY(i)) < GetSpriteH(i)) then
      begin
      result := i;
      break; // Stop searching.
      end;
end;

function TForm1.FindPiece(x, y: integer): integer;
var i: integer;
begin
  result := -1; // Assume no piece found.
  for i := piececount-1 downto 0 do
    if InRect(x,y,GetPieceX(i),GetPieceY(i),GetPieceW(i),GetPieceH(i)) then
      begin
      result := i;
      break; // Stop searching.
      end;
end;

procedure TForm1.pbWorkspaceMouseEnter(Sender: TObject);
begin
  hover := true;
end;

procedure TForm1.pbWorkspaceMouseLeave(Sender: TObject);
begin
  drag := false;
  hover := false;
  Screen.Cursor := crArrow;
end;

procedure TForm1.pbWorkspaceMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbLeft then exit; // Left mouse button only.
  drag := false;
  if (layer = 1) and (spriteside = 0) and chkSnap.Checked then
    begin
    SetSpriteX(spriteselect,Nearest(GetSpriteX(spriteselect),grid_w)); // Snap to grid.
    SetSpriteY(spriteselect,Nearest(GetSpriteY(spriteselect),grid_h));
    UpdateDisplay;
    end;
end;

procedure TForm1.pbWorkspaceMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var dx, dy, spx, spy, spw, sph, cnr, hcnr: integer;
begin
  GetMousePos(X,Y);
  if not drag then Screen.Cursor := crArrow; // Use arrow cursor by default, but keep cursor if dragging.
  if (layer = 1) and (spriteselect <> -1) and (not drag) then
    begin
    spx := (GetSpriteX(spriteselect)-GetSpriteW(spriteselect)-pos_x)*scale; // Get position of sprite on screen.
    spy := (GetSpriteY(spriteselect)-GetSpriteH(spriteselect)-pos_y)*scale;
    spw := GetSpriteW(spriteselect)*scale;
    sph := GetSpriteH(spriteselect)*scale;
    cnr := corner_dim[scale];
    hcnr := cnr div 2;
    if (X >= spx) and (Y >= spy) and (X < spx+cnr) and (Y < spy+cnr) then Screen.Cursor := crSizeNWSE // Top left.
    else if (X >= spx+spw-hcnr) and (Y >= spy) and (X < spx+spw+hcnr) and (Y < spy+cnr) then Screen.Cursor := crSizeNS // Top middle.
    else if (X >= spx+(spw*2)-cnr) and (Y >= spy) and (X < spx+(spw*2)) and (Y < spy+cnr) then Screen.Cursor := crSizeNESW // Top right.
    else if (X >= spx) and (Y >= spy+sph-hcnr) and (X < spx+cnr) and (Y < spy+sph+hcnr) then Screen.Cursor := crSizeWE // Middle left.
    else if (X >= spx+(spw*2)-cnr) and (Y >= spy+sph-hcnr) and (X < spx+(spw*2)) and (Y < spy+sph+hcnr) then Screen.Cursor := crSizeWE // Middle right.
    else if (X >= spx) and (Y >= spy+(sph*2)-cnr) and (X < spx+cnr) and (Y < spy+(sph*2)) then Screen.Cursor := crSizeNESW // Bottom left.
    else if (X >= spx+spw-hcnr) and (Y >= spy+(sph*2)-cnr) and (X < spx+spw+hcnr) and (Y < spy+(sph*2)) then Screen.Cursor := crSizeNS // Bottom middle.
    else if (X >= spx+(spw*2)-cnr) and (Y >= spy+(sph*2)-cnr) and (X < spx+(spw*2)) and (Y < spy+(sph*2)) then Screen.Cursor := crSizeNWSE // Bottom right.
    else Screen.Cursor := crArrow;
    end;
  if colorselect <> -1 then Screen.Cursor := crCross; // Use crosshair cursor if in colour select mode.
  if not drag then exit; // Do nothing if not dragging.
  if not pngloaded then exit; // Do nothing if no PNG is loaded.
  dx := (prev_x div scale)-(X div scale); // Get movement distance.
  dy := (prev_y div scale)-(Y div scale);
  prev_x := X;
  prev_y := Y;
  case layer of
    0: // Background.
      begin
      pos_x := Max(pos_x+dx,0); // New position, always 0 or higher.
      pos_y := Max(pos_y+dy,0);
      end;
    1: // Sprite.
      begin
      if spriteside = 0 then
        begin
        SetSpriteX(spriteselect,GetSpriteX(spriteselect)-dx); // Move sprite box.
        SetSpriteY(spriteselect,GetSpriteY(spriteselect)-dy);
        end;
      if spriteside and side_top <> 0 then
        SetSpriteH(spriteselect,Max(GetSpriteH(spriteselect)+dy,16)); // Resize sprite box.
      if spriteside and side_bottom <> 0 then
        SetSpriteH(spriteselect,Max(GetSpriteH(spriteselect)-dy,16));
      if spriteside and side_left <> 0 then
        SetSpriteW(spriteselect,Max(GetSpriteW(spriteselect)+dx,16));
      if spriteside and side_right <> 0 then
        SetSpriteW(spriteselect,Max(GetSpriteW(spriteselect)-dx,16));
      end;
    2: // Piece.
      begin
      SetPieceX(pieceselect,GetPieceX(pieceselect)-dx); // Move piece box.
      SetPieceY(pieceselect,GetPieceY(pieceselect)-dy);
      end;
  end;
  UpdateDisplay;
end;

procedure TForm1.GetMousePos(x, y: integer);
begin
  mouseimg_x := pos_x+(x div scale); // Get mouse pointer position on image.
  mouseimg_y := pos_y+(y div scale);
  mousewin_x := x; // Get mouse pointer position on workspace.
  mousewin_y := y;
end;

procedure TForm1.editSpriteChange(Sender: TObject);
begin
  if spriteselect = -1 then exit; // Do nothing if no sprite is selected.
  spritenames[spriteselect] := editSprite.Text; // Update name.
end;

procedure TForm1.chkGridClick(Sender: TObject);
begin
  UpdateDisplay;
end;

procedure TForm1.chkHiClick(Sender: TObject);
begin
  if pieceselect = -1 then exit;
  if chkHi.Checked = true then SetPieceHi(pieceselect,$10)
  else SetPieceHi(pieceselect,0);
  UpdateDisplay;
end;

procedure TForm1.editGridChange(Sender: TObject);
begin
  grid_w := GetGridW(editGrid.Text); // Read grid size from text box (minimum 8).
  grid_h := GetGridH(editGrid.Text);
  UpdateDisplay;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then
    begin
    if layer = 1 then DeleteSprite(spriteselect) // Delete selected sprite.
    else if layer = 2 then DeletePiece(pieceselect); // Delete selected piece.
    layer := 0; // Switch to background layer.
    UpdateDisplay;
    end;
end;

procedure TForm1.DeleteSprite(i: integer);
var j: integer;
begin
  if spriteselect = -1 then exit;
  if spritecount = 0 then exit;
  if (i < 0) or (i >= spritecount) then exit; // Do nothing if target is invalid.
  for j := i to spritecount-2 do
    begin
    spritenames[j] := spritenames[j+1]; // Shift names back 1.
    SetSpriteX(j,GetSpriteX(j+1));
    SetSpriteY(j,GetSpriteY(j+1));
    SetSpriteW(j,GetSpriteW(j+1));
    SetSpriteH(j,GetSpriteH(j+1));
    end;
  SetLength(spritenames,Length(spritenames)-1); // Truncate array.
  SetLength(spritetable,Length(spritetable)-spritetable_items);
  Dec(spritecount); // Decrement counter;
  spriteselect := -1; // Deselect sprite.
end;

procedure TForm1.DeletePiece(i: integer);
var j: integer;
begin
  if pieceselect = -1 then exit;
  if piececount = 0 then exit;
  if (i < 0) or (i >= piececount) then exit; // Do nothing if target is invalid.
  for j := i to piececount-2 do
    begin
    SetPieceX(j,GetPieceX(j+1));
    SetPieceY(j,GetPieceY(j+1));
    SetPieceBits(j,GetPieceBits(j+1));
    SetPieceSize(j,GetPieceSize(j+1));
    end;
  SetLength(piecetable,Length(piecetable)-piecetable_items);
  Dec(piececount); // Decrement counter;
  pieceselect := -1; // Deselect piece.
end;

procedure TForm1.btnDeleteClick(Sender: TObject);
begin
  if layer <> 1 then exit; // Do nothing if not on sprite layer.
  DeleteSprite(spriteselect); // Delete selected sprite.
  UpdateDisplay;
end;

procedure TForm1.btnDelPiecesClick(Sender: TObject);
var i: integer;
begin
  if layer <> 1 then exit;
  if spriteselect = -1 then exit;
  i := 0;
  while i < piececount do
    if PieceInSprite(i,spriteselect) then // Check if each piece is in the sprite.
      begin
      pieceselect := i;
      DeletePiece(i); // Delete piece if it's inside the sprite.
      end
    else Inc(i); // Next piece.
  DeleteSprite(spriteselect); // Delete selected sprite.
  UpdateDisplay;
end;

procedure TForm1.btnHighClick(Sender: TObject);
var i: integer;
begin
  if spriteselect = -1 then exit;
  for i := 0 to piececount-1 do
    if PieceInSprite(i,spriteselect) then SetPieceHi(i,$10); // Check if each piece is in the sprite.
  UpdateDisplay;
end;

procedure TForm1.btnLowClick(Sender: TObject);
var i: integer;
begin
  if spriteselect = -1 then exit;
  for i := 0 to piececount-1 do
    if PieceInSprite(i,spriteselect) then SetPieceHi(i,0); // Check if each piece is in the sprite.
  UpdateDisplay;
end;

function TForm1.PieceInSprite(p, s: integer): boolean;
var x, y, w, h: integer;
begin
  result := false; // Assume piece isn't in sprite.
  x := GetSpriteX(s)-GetSpriteW(s); // Get position of sprite.
  y := GetSpriteY(s)-GetSpriteH(s);
  w := GetSpriteW(s)*2; // Get width/height of sprite.
  h := GetSpriteH(s)*2;
  if GetPieceX(p) < x then exit; // Piece is left of sprite.
  if GetPieceY(p) < y then exit; // Piece is above sprite.
  if GetPieceX(p)+GetPieceW(p) >= x+w then exit; // Piece is right of sprite.
  if GetPieceY(p)+GetPieceH(p) >= y+h then exit; // Piece is below sprite.
  result := true; // Piece is inside sprite.
end;

function TForm1.GetPieceX(p: integer): integer;
begin
  result := piecetable[(p*piecetable_items)+piecetable_x];
end;

function TForm1.GetPieceY(p: integer): integer;
begin
  result := piecetable[(p*piecetable_items)+piecetable_y];
end;

function TForm1.GetPieceBits(p: integer): integer;
begin
  result := piecetable[(p*piecetable_items)+piecetable_bits];
end;

function TForm1.GetPiecePal(p: integer): integer;
begin
  result := GetPieceBits(p) and piecebits_pal;
end;

function TForm1.GetPieceHi(p: integer): integer;
begin
  result := GetPieceBits(p) and piecebits_hi;
end;

function TForm1.GetPieceSize(p: integer): integer;
begin
  result := piecetable[(p*piecetable_items)+piecetable_size];
end;

function TForm1.GetPieceW(p: integer): integer;
begin
  result := piecewidth[piecetable[(p*piecetable_items)+piecetable_size]];
end;

function TForm1.GetPieceH(p: integer): integer;
begin
  result := pieceheight[piecetable[(p*piecetable_items)+piecetable_size]];
end;

procedure TForm1.SetPieceX(p, x: integer);
begin
  piecetable[(p*piecetable_items)+piecetable_x] := x;
end;

procedure TForm1.SetPieceY(p, y: integer);
begin
  piecetable[(p*piecetable_items)+piecetable_y] := y;
end;

procedure TForm1.SetPieceBits(p, b: integer);
begin
  piecetable[(p*piecetable_items)+piecetable_bits] := b;
end;

procedure TForm1.SetPiecePal(p, pal: integer);
begin
  if pal > 3 then pal := 3;
  SetPieceBits(p,(GetPieceBits(p) and ($FF-piecebits_pal)) or pal);
end;

procedure TForm1.SetPieceHi(p, hi: integer);
begin
  if hi > 0 then hi := piecebits_hi;
  SetPieceBits(p,(GetPieceBits(p) and ($FF-piecebits_hi)) or hi);
end;

procedure TForm1.SetPieceSize(p, s: integer);
begin
  if s > 15 then s := 15;
  piecetable[(p*piecetable_items)+piecetable_size] := s;
end;

function TForm1.GetSpriteX(s: integer): integer;
begin
  result := spritetable[(s*spritetable_items)+spritetable_x];
end;

function TForm1.GetSpriteY(s: integer): integer;
begin
  result := spritetable[(s*spritetable_items)+spritetable_y];
end;

function TForm1.GetSpriteW(s: integer): integer;
begin
  result := spritetable[(s*spritetable_items)+spritetable_w];
end;

function TForm1.GetSpriteH(s: integer): integer;
begin
  result := spritetable[(s*spritetable_items)+spritetable_h];
end;

procedure TForm1.SetSpriteX(s, x: integer);
begin
  spritetable[(s*spritetable_items)+spritetable_x] := x;
end;

procedure TForm1.SetSpriteY(s, y: integer);
begin
  spritetable[(s*spritetable_items)+spritetable_y] := y;
end;

procedure TForm1.SetSpriteW(s, w: integer);
begin
  spritetable[(s*spritetable_items)+spritetable_w] := w;
end;

procedure TForm1.SetSpriteH(s, h: integer);
begin
  spritetable[(s*spritetable_items)+spritetable_h] := h;
end;

end.
