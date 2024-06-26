unit MiscFunc;

interface
uses Graphics, StrUtils, Sysutils, Windows, SolveFunc, Math, ExplodeFunc;

function StrToTColor(str: string): TColor;
function TColorToStr(col: TColor): string;
function GetGridW(str: string): integer;
function GetGridH(str: string): integer;
function Nearest(num, interval: integer): integer;
function InRect(x, y, rx, ry, w, h: integer): boolean;
function Quantity(n: integer; s: string): string;

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

function GetGridW(str: string): integer;
var g: integer;
begin
  if AnsiPos(',',str) <> 0 then g := Solve(Explode(str,',',0)) // Convert string to integer (-1 if invalid).
  else g := Solve(str);
  result := Max(g,8); // Minimum 8.
end;

function GetGridH(str: string): integer;
var g: integer;
begin
  if AnsiPos(',',str) <> 0 then g := Solve(Explode(str,',',1)) // Convert string to integer (-1 if invalid).
  else g := Solve(str);
  result := Max(g,8); // Minimum 8.
end;

{ Round integer to nearest interval. }

function Nearest(num, interval: integer): integer;
begin
  result := Round(num/interval)*interval;
end;

{ Check if a point is inside a rectangle. }

function InRect(x, y, rx, ry, w, h: integer): boolean;
begin
  if (x >= rx) and (x < rx+w) and (y >= ry) and (y < ry+h) then result := true
  else result := false;
end;

{ Show quantity as string and convert string to plural if needed. }

function Quantity(n: integer; s: string): string;
begin
  if n = 1 then result := IntToStr(n)+' '+s
  else result := IntToStr(n)+' '+s+'s';
end;

end.