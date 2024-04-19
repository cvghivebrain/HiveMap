unit MiscFunc;

interface
uses Graphics, StrUtils, Sysutils, Windows, SolveFunc, Math;

function StrToTColor(str: string): TColor;
function TColorToStr(col: TColor): string;
function GetGrid(str: string): integer;
function Nearest(num, interval: integer): integer;

implementation

{ Convert RGB hex string to TColor. }

function StrToTColor(str: string): TColor;
var c: integer;
begin
  if str = '' then c := 0
    else c := StrToInt('$'+str);
  result := RGB(c shr 16, (c shr 8) and $FF, c and $FF);
end;

{ Convert TColor to RGB hex string. }

function TColorToStr(col: TColor): string;
begin
  result := IntToHex(col and $FF,2)+IntToHex((col shr 8) and $FF,2)+IntToHex((col shr 16) and $FF,2);
end;

{ Convert string to grid size. }

function GetGrid(str: string): integer;
var g: integer;
begin
  g := Solve(str); // Convert string to integer (-1 if invalid).
  result := Max(g,8); // Minimum 8.
end;

{ Round integer to nearest interval. }

function Nearest(num, interval: integer): integer;
begin
  result := Round(num/interval)*interval;
end;

end.