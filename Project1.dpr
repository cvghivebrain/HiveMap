program Project1;

uses
  Forms,
  Generic in 'Generic.pas' {Form1},
  ScanLineFunc in 'ScanLineFunc.pas',
  CRCFunc in 'CRCFunc.pas',
  ExplodeFunc in 'ExplodeFunc.pas',
  FileFunc in 'FileFunc.pas',
  SolveFunc in 'SolveFunc.pas',
  MiscFunc in 'MiscFunc.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
