unit Generic;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, ExtCtrls, StdCtrls, ScanLineFunc, pngimage, CRCFunc,
  ExplodeFunc, FileFunc, SolveFunc, Math;

type
  TForm1 = class(TForm)
    imgMain: TImage;
    btnLoad: TButton;
    dlgLoad: TOpenDialog;
    pbWorkspace: TPaintBox;
    memINI: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure LoadPNG;
    procedure LoadINI;
    procedure UpdateDisplay;
    procedure pbWorkspaceMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbWorkspaceMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbWorkspaceMouseLeave(Sender: TObject);
    procedure pbWorkspaceMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  pngloaded, drag: boolean;
  pos_x, pos_y, prev_x, prev_y: integer;
  pngpath, inipath: string;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitImage(Form1,imgMain); // Set image width & height to match form.
  pos_x := 0;
  pos_y := 0;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  MatchWindow; // Set boundaries to match window.
  pbWorkspace.Width := Form1.ClientWidth-pbWorkspace.Left-320;
  pbWorkspace.Height := Form1.ClientHeight-pbWorkspace.Top;
  UpdateDisplay;
  memINI.Left := Form1.ClientWidth-memINI.Width;
end;

procedure TForm1.UpdateDisplay;
var w, h: integer;
begin
  FillScreen(40,44,52);
  if not pngloaded then exit; // Do nothing further if no PNG is loaded.
  w := Min(PNG.Width-pos_x,pbWorkspace.Width);
  h := Min(PNG.Height-pos_y,pbWorkspace.Height);
  DrawPNG(pos_x,pos_y,w,h,pbWorkspace.Left,pbWorkspace.Top,1,1,0,255,255,255,255);
  pic.Refresh;
end;

procedure TForm1.btnLoadClick(Sender: TObject);
begin
  if dlgLoad.Execute then
    begin
    if ExtractFileExt(dlgLoad.FileName) = '.png' then
      begin
      pngpath := dlgLoad.FileName;
      LoadPNG;
      inipath := ChangeFileExt(pngpath,'.ini');
      if FileExists(inipath) then LoadINI;
      UpdateDisplay;
      end
    else if ExtractFileExt(dlgLoad.FileName) = '.ini' then
      begin
      pngpath := '';
      inipath := dlgLoad.FileName;
      LoadINI;
      if pngpath = '' then pngpath := ChangeFileExt(inipath,'.png');
      LoadPNG;
      UpdateDisplay;
      end
    else ShowMessage('Unknown file type.');
    end;
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

procedure TForm1.LoadINI;
var inifile: textfile;
  s: string;
begin
  AssignFile(inifile,inipath); // Open ini file.
  Reset(inifile);
  while not eof(inifile) do
    begin
    ReadLn(inifile,s);
    if AnsiPos('image=',s) = 1 then pngpath := ExtractFilePath(inipath)+Explode(s,'image=',1)
    else if s <> '' then memINI.Lines.Add(s);
    end;
  CloseFile(inifile);
end;

procedure TForm1.pbWorkspaceMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  drag := true; // Start dragging whatever is under mouse.
  prev_x := X;
  prev_y := Y;
end;

procedure TForm1.pbWorkspaceMouseLeave(Sender: TObject);
begin
  drag := false;
end;

procedure TForm1.pbWorkspaceMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  drag := false;
end;

procedure TForm1.pbWorkspaceMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var dx, dy: integer;
begin
  if not drag then exit; // Do nothing if not dragging.
  if not pngloaded then exit; // Do nothing if no PNG is loaded.
  dx := prev_x-X; // Get movement distance.
  dy := prev_y-Y;
  prev_x := X;
  prev_y := Y;
  pos_x := Max(pos_x+dx,0); // New position, always 0 or higher.
  pos_y := Max(pos_y+dy,0);
  UpdateDisplay;
end;

end.
