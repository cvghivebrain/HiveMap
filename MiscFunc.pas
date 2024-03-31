unit MiscFunc;

interface
uses Graphics, StrUtils, Sysutils, Windows;

function StrToTColor(str: string): TColor;

implementation

{ Convert RGB hex string to TColor. }

function StrToTColor(str: string): TColor;
var c: integer;
begin
  if str = '' then c := 0
    else c := StrToInt('$'+str);
  result := RGB(c shr 16, (c shr 8) and $FF, c and $FF);
end;

end.