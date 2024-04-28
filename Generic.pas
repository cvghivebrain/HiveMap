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
  private
    { Private declarations }
    procedure LoadPNG;
    procedure LoadINI;
    procedure UpdateDisplay;
    procedure CreatePal;
    procedure GetMousePos(x, y: integer);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  pngloaded, drag, wheeldelay, hover: boolean;
  pos_x, pos_y, prev_x, prev_y, scale, palused, layer, spritecount, spriteselect,
    spriteside, grid_w, grid_h, mouseimg_x, mouseimg_y, mousewin_x, mousewin_y,
    piececount, pieceselect: integer;
  pngpath, pngpathrel, inipath: string;
  palarray: array[0..63] of TColor;
  spritenames: array of string;
  spritetable: array of integer;
  piecetable: array of integer;

const
  max_scale: integer = 6;
  corner_dim: integer = 6;
  side_top: integer = 1;
  side_bottom: integer = 2;
  side_left: integer = 4;
  side_right: integer = 8;
  piecewidth: array[0..15] of integer = (8,8,8,8,16,16,16,16,24,24,24,24,32,32,32,32);
  pieceheight: array[0..15] of integer = (8,16,24,32,8,16,24,32,8,16,24,32,8,16,24,32);

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
  UpdateDisplay;
end;

procedure TForm1.UpdateDisplay;
var w, h, x, y, i, p: integer;
const bg: array[0..2] of byte = (40,44,52); // Background colour.
  sc: array[0..2] of byte = (255,255,255); // Sprite colour.
  pc: array[0..11] of byte = (255,0,0, 255,255,0, 0,255,0, 0,0,255); // Piece colours.
  gc: array[0..2] of byte = (255,128,255); // Grid colour.
begin
  FillScreen(bg[0],bg[1],bg[2]);
  if not pngloaded then exit; // Do nothing further if no PNG is loaded.
  w := Min(PNG.Width-pos_x,pbWorkspace.Width div scale);
  h := Min(PNG.Height-pos_y,pbWorkspace.Height div scale);
  DrawPNG(pos_x,pos_y,w,h,pbWorkspace.Left,pbWorkspace.Top,scale,scale,0,255,255,255,255); // Draw image.
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
  for i := 0 to spritecount-1 do // Draw sprite boxes.
    begin
    x := ((spritetable[i*4]-spritetable[(i*4)+2]-pos_x)*scale)+pbWorkspace.Left;
    y := ((spritetable[(i*4)+1]-spritetable[(i*4)+3]-pos_y)*scale)+pbWorkspace.Top;
    w := spritetable[(i*4)+2]*2*scale;
    h := spritetable[(i*4)+3]*2*scale;
    DrawBox(sc[0],sc[1],sc[2],255,x,y,w,h); // Draw box around sprite.
    DrawGrid(sc[0],sc[1],sc[2],128,x,y,w,h,2,2,false); // Draw 2x2 grid.
    if (layer = 1) and (i = spriteselect) then
      begin
      DrawRect(sc[0],sc[1],sc[2],255,x,y,corner_dim,corner_dim); // Highlight corners of selected sprite.
      DrawRect(sc[0],sc[1],sc[2],255,x+w-corner_dim,y,corner_dim,corner_dim);
      DrawRect(sc[0],sc[1],sc[2],255,x,y+h-corner_dim,corner_dim,corner_dim);
      DrawRect(sc[0],sc[1],sc[2],255,x+w-corner_dim,y+h-corner_dim,corner_dim,corner_dim);
      end;
    end;
  for i := 0 to piececount-1 do // Draw piece boxes.
    begin
    x := ((piecetable[i*4]-pos_x)*scale)+pbWorkspace.Left;
    y := ((piecetable[(i*4)+1]-pos_y)*scale)+pbWorkspace.Top;
    w := piecewidth[piecetable[(i*4)+3]]*scale;
    h := pieceheight[piecetable[(i*4)+3]]*scale;
    p := piecetable[(i*4)+2]*3;
    DrawBoxFill(pc[p],pc[p+1],pc[p+2],255,pc[p],pc[p+1],pc[p+2],92,x,y,w,h); // Draw box around piece.
    if (layer = 2) and (i = pieceselect) then
      DrawBox(pc[p],pc[p+1],pc[p+2],255,x-2,y-2,w+4,h+4); // Draw second box around selected piece.
    end;
  DrawRect(bg[0],bg[1],bg[2],255,0,0,Form1.ClientWidth,pbWorkspace.Top); // Clear top area.
  DrawRect(bg[0],bg[1],bg[2],255,pbWorkspace.Width,0,Form1.ClientWidth-pbWorkspace.Width,Form1.ClientHeight); // Clear right area.
  for i := 0 to 63 do
    DrawRect(GetRValue(palarray[i]),GetGValue(palarray[i]),GetBValue(palarray[i]),255,
      pbPalette.Left+((i mod 16)*20),pbPalette.Top+((i div 16)*20),20,20); // Draw palette.
  if spriteselect = -1 then
    begin
    editSprite.Text := '';
    editSprite.EditLabel.Caption := 'Sprite';
    end
  else
    begin
    editSprite.Text := spritenames[spriteselect]; // Show name of selected sprite.
    editSprite.EditLabel.Caption := 'Sprite '+IntToStr(spriteselect+1)+'/'+IntToStr(spritecount); // Show sprite number.
    end;
  if pieceselect = -1 then lblPiece.Caption := 'Piece'
  else
    begin
    lblPiece.Caption := 'Piece '+IntToStr(pieceselect+1)+'/'+IntToStr(piececount); // Show piece number.
    DrawRectStriped(64,64,64,255,128,128,128,255,pbPiece.Left,pbPiece.Top,pbPiece.Width,pbPiece.Height,1,1);
    DrawGrid(255,255,255,255,pbPiece.Left,pbPiece.Top,pbPiece.Width,pbPiece.Height,4,4,true);
    DrawBox2(255,255,255,255,pbPalette.Left,pbPalette.Top+(piecetable[(pieceselect*4)+2]*(pbPalette.Height div 4)),pbPalette.Width,pbPalette.Height div 4,2); // Highlight palette line.
    end;
  pic.Refresh;
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
      SetLength(spritetable,Length(spritetable)+4);
      spritenames[spritecount] := Explode(s2,',',0); // Sprite name.
      spritetable[spritecount*4] := StrToInt(Explode(s2,',',1)); // Sprite x pos.
      spritetable[(spritecount*4)+1] := StrToInt(Explode(s2,',',2)); // Sprite y pos.
      spritetable[(spritecount*4)+2] := StrToInt(Explode(s2,',',3)); // Sprite width.
      spritetable[(spritecount*4)+3] := StrToInt(Explode(s2,',',4)); // Sprite height.
      Inc(spritecount);
      end
    else if AnsiPos('piece=',s) = 1 then
      begin
      s2 := Explode(s,'piece=',1);
      SetLength(piecetable,Length(piecetable)+4); // Add piece.
      piecetable[piececount*4] := StrToInt(Explode(s2,',',0)); // Piece x pos.
      piecetable[(piececount*4)+1] := StrToInt(Explode(s2,',',1)); // Piece y pos.
      piecetable[(piececount*4)+2] := StrToInt(Explode(s2,',',2)); // Piece palette.
      piecetable[(piececount*4)+3] := StrToInt(Explode(s2,',',3)); // Piece size.
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
  for i := 0 to spritecount-1 do WriteLn(inifile,s+spritenames[i]+','+IntToStr(spritetable[i*4])+','+
    IntToStr(spritetable[(i*4)+1])+','+IntToStr(spritetable[(i*4)+2])+','+IntToStr(spritetable[(i*4)+3])); // Write sprite table.
  s := 'piece=';
  for i := 0 to piececount-1 do WriteLn(inifile,s+IntToStr(piecetable[i*4])+','+
    IntToStr(piecetable[(i*4)+1])+','+IntToStr(piecetable[(i*4)+2])+','+IntToStr(piecetable[(i*4)+3])); // Write piece table.
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
  if pieceselect <> -1 then
    begin
    piecetable[(pieceselect*4)+2] := Y div (pbPalette.Height div 4); // Change palette of piece.
    UpdateDisplay;
    end;
end;

procedure TForm1.pbWorkspaceMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var i, edgew: integer;
begin
  edgew := corner_dim+4; // Set thickness of clickable edges.
  if Button = mbLeft then // Left click.
    begin
    layer := 0; // Assume the background was selected.
    spriteselect := -1; // Assume no sprite selected.
    pieceselect := -1; // Assume no piece selected.
    drag := true; // Start dragging whatever is under mouse.
    prev_x := X;
    prev_y := Y;
    for i := 0 to spritecount-1 do
      if (Abs(mouseimg_x-spritetable[i*4]) < spritetable[(i*4)+2]) and (Abs(mouseimg_y-spritetable[(i*4)+1]) < spritetable[(i*4)+3]) then
        begin
        layer := 1; // Select sprite layer.
        spriteside := 0; // Assume edge of sprite wasn't clicked.
        if mouseimg_x <= spritetable[i*4]-spritetable[(i*4)+2]+edgew then spriteside := side_left;
        if mouseimg_x >= spritetable[i*4]+spritetable[(i*4)+2]-edgew then spriteside := side_right;
        if mouseimg_y <= spritetable[(i*4)+1]-spritetable[(i*4)+3]+edgew then spriteside := spriteside+side_top;
        if mouseimg_y >= spritetable[(i*4)+1]+spritetable[(i*4)+3]-edgew then spriteside := spriteside+side_bottom;
        spriteselect := i;
        break; // Stop checking sprites.
        end;
    for i := 0 to piececount-1 do
      if InRect(mouseimg_x,mouseimg_y,piecetable[i*4],piecetable[(i*4)+1],piecewidth[piecetable[(i*4)+3]],pieceheight[piecetable[(i*4)+3]]) then
        begin
        layer := 2; // Select piece layer.
        pieceselect := i;
        break; // Stop checking pieces.
        end;
    end
  else if Button = mbRight then // Right click.
    begin
    i := spritecount;
    Inc(spritecount); // Add sprite.
    SetLength(spritenames,spritecount);
    spritenames[i] := 'sprite'+IntToStr(i); // Give sprite name "sprite#".
    SetLength(spritetable,spritecount*4);
    if chkSnap.Checked then
      begin
      spritetable[i*4] := Nearest(mouseimg_x,grid_w);
      spritetable[(i*4)+1] := Nearest(mouseimg_y,grid_h);
      end
    else
      begin
      spritetable[i*4] := mouseimg_x;
      spritetable[(i*4)+1] := mouseimg_y;
      end;
    spritetable[(i*4)+2] := 40;
    spritetable[(i*4)+3] := 40;
    spriteselect := i; // Select new sprite.
    layer := 1;
    end;
  UpdateDisplay;
end;

procedure TForm1.pbWorkspaceMouseEnter(Sender: TObject);
begin
  hover := true;
end;

procedure TForm1.pbWorkspaceMouseLeave(Sender: TObject);
begin
  drag := false;
  hover := false;
end;

procedure TForm1.pbWorkspaceMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbLeft then exit; // Left mouse button only.
  drag := false;
  if (layer = 1) and (spriteside = 0) and chkSnap.Checked then
    begin
    spritetable[spriteselect*4] := Nearest(spritetable[spriteselect*4],grid_w); // Snap to grid.
    spritetable[(spriteselect*4)+1] := Nearest(spritetable[(spriteselect*4)+1],grid_h);
    UpdateDisplay;
    end;
end;

procedure TForm1.pbWorkspaceMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var dx, dy: integer;
begin
  GetMousePos(X,Y);
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
        spritetable[spriteselect*4] := spritetable[spriteselect*4]-dx; // Move sprite box.
        spritetable[(spriteselect*4)+1] := spritetable[(spriteselect*4)+1]-dy;
        end;
      if spriteside and side_top <> 0 then
        spritetable[(spriteselect*4)+3] := Max(spritetable[(spriteselect*4)+3]+dy,16); // Resize sprite box.
      if spriteside and side_bottom <> 0 then
        spritetable[(spriteselect*4)+3] := Max(spritetable[(spriteselect*4)+3]-dy,16);
      if spriteside and side_left <> 0 then
        spritetable[(spriteselect*4)+2] := Max(spritetable[(spriteselect*4)+2]+dx,16);
      if spriteside and side_right <> 0 then
        spritetable[(spriteselect*4)+2] := Max(spritetable[(spriteselect*4)+2]-dx,16);
      end;
    2: // Piece.
      begin
      piecetable[pieceselect*4] := piecetable[pieceselect*4]-dx; // Move piece box.
      piecetable[(pieceselect*4)+1] := piecetable[(pieceselect*4)+1]-dy;
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

procedure TForm1.editGridChange(Sender: TObject);
begin
  grid_w := GetGridW(editGrid.Text); // Read grid size from text box (minimum 8).
  grid_h := GetGridH(editGrid.Text);
  UpdateDisplay;
end;

end.
