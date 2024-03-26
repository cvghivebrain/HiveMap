unit SolveFunc;

{$DEFINE GUI_APP} // Remove this line for command line programs.

interface
uses StrUtils,
  SysUtils,
  {$IFDEF GUI_APP}
  Dialogs,
  {$ENDIF}
  Math,
  ExplodeFunc,
  CRCFunc,
  FileFunc;

function DoSum(s: string): int64;
function DoSum2(s: string): int64;
function DoSum3(num1, num2: string; op: char): string;
function BinToInt(s: string): int64;
function Solve(s: string): int64;
function Solve2(s: string): int64;
function SolveStr(s: string): string;
function SolveHex(s: string): string;

var
  val: int64;

implementation

{ Convert an expression (e.g. '1+1') to integer. }

function DoSum(s: string): int64;
label istrue;
begin
  s := ReplaceStr(s,' ',''); // Strip spaces.
  s := ReplaceStr(s,'0x','$'); // Convert C++ hex prefix to Delphi/assembly.
  s := ReplaceStr(s,'<<','L'); // Replace << (shift left) to avoid clash with <.
  s := ReplaceStr(s,'>>','l'); // Replace >> (shift right) to avoid clash with >.
  s := ReplaceStr(s,'**','P'); // Replace ** (exponent) to avoid clash with *.
  if (AnsiPos('=',s) > 0) or (AnsiPos('<',s) > 0) or (AnsiPos('>',s) > 0) then // Check for conditional.
    begin
    result := 1; // Assume condition is satisfied.
    if AnsiPos('>=',s) > 0 then
      begin
      if DoSum2(Explode(s,'>=',0)) >= DoSum2(Explode(s,'>=',1)) then exit; // 1 for greater than or equal.
      end
    else if AnsiPos('<=',s) > 0 then
      begin
      if DoSum2(Explode(s,'<=',0)) <= DoSum2(Explode(s,'<=',1)) then exit; // 1 for less than or equal.
      end
    else if AnsiPos('=',s) > 0 then
      begin
      if DoSum2(Explode(s,'=',0)) = DoSum2(Explode(s,'=',1)) then exit; // 1 for equal.
      end
    else if AnsiPos('<>',s) > 0 then
      begin
      if DoSum2(Explode(s,'<>',0)) <> DoSum2(Explode(s,'<>',1)) then exit; // 1 for inequal.
      end
    else if AnsiPos('>',s) > 0 then
      begin
      if DoSum2(Explode(s,'>',0)) > DoSum2(Explode(s,'>',1)) then exit; // 1 for greater than.
      end
    else if AnsiPos('<',s) > 0 then
      begin
      if DoSum2(Explode(s,'<',0)) < DoSum2(Explode(s,'<',1)) then exit; // 1 for less than.
      end;
    result := 0; // Condition was not satisfied.
    end
  else result := DoSum2(s);
end;

function DoSum2(s: string): int64;
var i: integer;
  num1, num2, op: string;
begin
  num1 := '';
  num2 := '';
  op := '';
  for i := 1 to Length(s) do
    begin
    if AnsiPos(s[i],'0123456789ABCDEFabcdef$') > 0 then // Check if character is digit or operator.
      begin
      if op = '' then num1 := num1+s[i] // Add to first number.
      else num2 := num2+s[i]; // Add to second number if operator is set.
      end
    else if (AnsiPos(s[i],'-%') > 0) and (num1 = '') then num1 := s[i] // Check for number prefix.
    else if (AnsiPos(s[i],'-%') > 0) and (op <> '') and (num2 = '') then num2 := s[i]
    else
      begin
      if op <> '' then
        begin
        num1 := DoSum3(num1,num2,op[1]); // Perform operation and put result in num1.
        num2 := ''; // Clear second number.
        end;
      op := s[i]; // Next operator.
      end;
    end;
  if op <> '' then result := StrToInt64(DoSum3(num1,num2,op[1]))
  else
    begin
    if num1[1] = '%' then result := BinToInt(num1)
    else result := StrToInt64(num1); // Return single number if no operator is present.
    end;
end;

function DoSum3(num1, num2: string; op: char): string;
var n1, n2: int64;
begin
  if num1[1] = '%' then n1 := BinToInt(num1) // Convert from binary.
  else n1 := StrToInt64(num1);
  if num2[1] = '%' then n2 := BinToInt(num2)
  else n2 := StrToInt64(num2);
  case Ord(op) of
  Ord('+'): result := IntToStr(n1+n2);
  Ord('-'): result := IntToStr(n1-n2);
  Ord('*'): result := IntToStr(n1*n2);
  Ord('/'): result := IntToStr(n1 div n2);
  Ord('\'): result := IntToStr(Ceil(n1/n2)); // Division rounding up.
  Ord('R'): result := IntToStr(Ceil(n1/n2)*n2); // Round up to multiple of n2.
  Ord('r'): result := IntToStr((n1 div n2)*n2); // Round down to multiple of n2.
  Ord('&'): result := IntToStr(n1 and n2);
  Ord('|'): result := IntToStr(n1 or n2);
  Ord('^'): result := IntToStr(n1 xor n2);
  Ord('%'): result := IntToStr(n1 mod n2);
  Ord('L'): result := IntToStr(n1 shl n2);
  Ord('l'): result := IntToStr(n1 shr n2);
  Ord('P'): result := IntToStr(Floor(Power(n1,n2)));
  end;
end;

{ Convert binary number with % prefix to integer. }

function BinToInt(s: string): int64;
var i: integer;
begin
  result := 0;
  for i := 2 to Length(s) do
    result := (result shl 1)+Ord(s[i])-48;
end;

{ Convert sum with brackets to integer. }

function Solve(s: string): int64;
var sub, scopy: string;
begin
  scopy := s;
  s := ReplaceStr(s,'{filesize}',IntToStr(Length(filearray))); // Insert file size.
  s := ReplaceStr(s,'{val}',IntToStr(val)); // Insert predefined value.
  try
    while AnsiPos('}',s) <> 0 do
      begin
      sub := ExplodeFull(Explode(s,'}',0),'{',-1); // Get contents of curly brackets.
      s := ReplaceStr(s,'{'+sub+'}',InttoStr(Solve2(sub))); // Solve & remove brackets.
      end;
    while AnsiPos('"',s) <> 0 do
      begin
      sub := Explode(s,'"',1); // Get contents of quotes.
      s := ReplaceStr(s,'"'+sub+'"','$'+CRCString(sub)); // Replace string with CRC32.
      end;
    while AnsiPos(')',s) <> 0 do
      begin
      sub := ExplodeFull(Explode(s,')',0),'(',-1); // Get contents of brackets.
      s := ReplaceStr(s,'('+sub+')',InttoStr(DoSum(sub))); // Solve & remove brackets.
      end;
    result := DoSum(s); // Final sum after brackets are gone.
  except
    {$IFDEF GUI_APP}
    ShowMessage(scopy+' is not a valid expression.');
    {$ELSE}
    WriteLn(scopy+' is not a valid expression.');
    {$ENDIF}
    result := 0;
  end;
end;

function Solve2(s: string): int64; // Get data from file array.
var t, str, param3: string;
  param1, param2: int64;
  i: integer;
begin
  t := Explode(s,':',0); // Get type (e.g. "b" for byte).
  Delete(s,1,Length(t)+1); // Remove type from input string.
  if t = 'b' then result := GetByte(Solve(s)) // Return byte from file array.
  else if t = 'w' then result := GetWord(Solve(s)) // Return word.
  else if t = '_w' then result := GetWordRev(Solve(s)) // Return word (byteswapped).
  else if t = 't' then result := (GetWord(Solve(s)) shl 8)+GetByte(Solve(s)+2) // Return 3 bytes.
  else if t = '_t' then result := GetWordRev(Solve(s))+(GetByte(Solve(s)+2) shl 16) // Return 3 bytes (byteswapped).
  else if t = 'd' then result := GetDWord(Solve(s)) // Return longword.
  else if t = '_d' then result := GetDWordRev(Solve(s)) // Return longword (byteswapped).
  else if t = 's' then
    begin
    param1 := Solve(Explode(s,',',0)); // Get string address.
    param2 := Solve(Explode(s,',',1)); // Get max length.
    param3 := Explode(s,',',2); // Get character width if set.
    if param3 = '' then result := StrtoInt64('$'+CRCString(GetString(param1,param2))) // Return CRC32 of string.
    else result := StrtoInt64('$'+CRCString(GetStringWide(param1,param2,Solve(param3)))); // Return CRC32 of string.
    end
  else if t = 'i' then
    begin
    param1 := Solve(Explode(s,',',0)); // Get string address.
    param2 := Solve(Explode(s,',',1)); // Get max length.
    result := StrtoInt64(GetStrInt(param1,param2)); // Return string as integer.
    end
  else if t = 'find' then
    begin
    param1 := Solve(Explode(s,',',0)); // Get start address.
    param2 := Solve(Explode(s,',',1)); // Get end address.
    str := Explode(s,'"',1); // Get string to search for.
    for i := param1 to param2-Length(str) do
      if GetString(i,Length(str)) = str then
        begin
        result := i; // Return address where string was found.
        exit; // Stop searching.
        end;
    result := -1; // String was not found.
    end
  else result := 0; // Return nothing.
end;

function SolveStr(s: string): string; // Solve with output as string.
begin
  result := IntToStr(Solve(s));
end;

function SolveHex(s: string): string; // Solve with output as (hex) string.
begin
  result := '$'+IntToHex(Solve(s),1);
end;

end.