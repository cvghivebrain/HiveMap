unit Generic;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, ExtCtrls, StdCtrls, ScanLineFunc, pngimage, CRCFunc,
  ExplodeFunc, FileFunc, SolveFunc;

type
  TForm1 = class(TForm)
    imgMain: TImage;
    btnLoad: TButton;
    dlgLoad: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure UpdateDisplay;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  pngloaded: boolean;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitImage(Form1,imgMain); // Set image width & height to match form.
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  MatchWindow; // Set boundaries to match window.
  UpdateDisplay;
end;

procedure TForm1.UpdateDisplay;
begin
  FillScreen(40,44,52);
  if not pngloaded then exit; // Do nothing further if no PNG is loaded.
  //DrawPNG(0,0,100,100,0,0,1,1,0,255,255,255,255);
  DrawWholePNG(0,0,1,1,0,255,255,255,255);
  pic.Refresh;
end;

procedure TForm1.btnLoadClick(Sender: TObject);
begin
  if dlgLoad.Execute then
    begin
    if ExtractFileExt(dlgLoad.FileName) = '.png' then
      begin
      LoadSheet(dlgLoad.FileName); // Make PNG available for display.
      pngloaded := true;
      UpdateDisplay;
      end;
    end;
end;

end.
