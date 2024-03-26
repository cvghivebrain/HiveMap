unit FileFunc;

{$DEFINE GUI_APP} // Remove this line for command line programs.

interface
uses Windows,
  {$IFDEF GUI_APP}
  Dialogs,
  {$ENDIF}
  SysUtils;

procedure LoadFile(openthis: string);
procedure SaveFile(savethis: string);
procedure SaveFileOutput(savethis: string);
procedure ClipFile(a, len: integer; clipthisfile: string);
procedure ClipFileOutput(a, len: integer; clipthisfile: string);
procedure NewFile(filelen: integer);
procedure NewFileOutput(filelen: integer);
procedure AddFile(a: integer; addthisfile: string);
procedure EvenFile;
procedure EvenFileOutput;
function GetByte(a: int64): byte;
function GetByteOutput(a: int64): byte;
function GetBit(a: int64; b: integer): byte;
function Bit(i, b: integer): byte;
function GetWord(a: int64): word;
function GetDword(a: int64): longword;
function GetWordRev(a: int64): word;
function GetDwordRev(a: int64): longword;
function GetString(a, maxlength: int64): string;
function GetStringWide(a, maxlength, charw: int64): string;
function GetStrInt(a, maxlength: int64): string;
function GetWordOutput(a: int64): word;
function GetDwordOutput(a: int64): longword;
function GetWordRevOutput(a: int64): word;
function GetDwordRevOutput(a: int64): longword;
function GetStringOutput(a, maxlength: int64): string;
function GetStringWideOutput(a, maxlength, charw: int64): string;
function GetStrIntOutput(a, maxlength: int64): string;
procedure WriteByte(a: int64; b: byte);
procedure WriteWord(a: int64; w: word);
procedure WriteWordRev(a: int64; w: word);
procedure WriteDword(a: int64; d: longword);
procedure WriteDwordRev(a: int64; d: longword);
procedure WriteByteOutput(a: int64; b: byte);
procedure WriteWordOutput(a: int64; w: word);
procedure WriteWordRevOutput(a: int64; w: word);
procedure WriteDwordOutput(a: int64; d: longword);
procedure WriteDwordRevOutput(a: int64; d: longword);
procedure RunCommand(command: string);
function FileInUse(f: string): boolean;
procedure ListFolders(dir: string; subfolders: boolean);
procedure ListFiles(dir: string; subfolders: boolean);
procedure GetBase64(a, a2: int64);
function ByteToBase64(c: byte): byte;

var
  myfile: file;
  filearray, outputarray: array of byte;
  fs, fpos: integer;
  folderlist, filelist: array of string;

implementation

{ Copy file to memory. }

procedure LoadFile(openthis: string);
begin
  if FileExists(openthis) then
    begin
    AssignFile(myfile,openthis); // Get file.
    FileMode := fmOpenRead; // Read only.
    Reset(myfile,1);
    SetLength(filearray,FileSize(myfile)); // Match array size to file size.
    BlockRead(myfile,filearray[0],FileSize(myfile)); // Copy file to memory.
    CloseFile(myfile); // Close file.
    fs := Length(filearray);
    fpos := 0;
    end;
end;

{ Save file from memory to file. }

procedure SaveFile(savethis: string);
begin
  AssignFile(myfile,savethis); // Open file.
  FileMode := fmOpenReadWrite;
  ReWrite(myfile,1);
  BlockWrite(myfile,filearray[0],Length(filearray)); // Copy contents of array to file.
  CloseFile(myfile); // Close file.
end;

procedure SaveFileOutput(savethis: string);
begin
  AssignFile(myfile,savethis); // Open file.
  FileMode := fmOpenReadWrite;
  ReWrite(myfile,1);
  BlockWrite(myfile,outputarray[0],Length(outputarray)); // Copy contents of array to file.
  CloseFile(myfile); // Close file.
end;

{ Save section of file to another file. }

procedure ClipFile(a, len: integer; clipthisfile: string);
var myclipfile: file;
  clipfilearray: array of byte;
begin
  if a+len > fs then len := fs-a; // Don't allow clip to extend outside file.
  if a > fs then len := 0;
  AssignFile(myclipfile,clipthisfile); // Open file.
  FileMode := fmOpenReadWrite;
  ReWrite(myclipfile,1);
  SetLength(clipfilearray,len);
  Move(filearray[a],clipfilearray[0],len); // Copy specified section.
  BlockWrite(myclipfile,clipfilearray[0],len); // Copy contents of array to file.
  CloseFile(myclipfile); // Close file.
end;

procedure ClipFileOutput(a, len: integer; clipthisfile: string);
var myclipfile: file;
  clipfilearray: array of byte;
begin
  if a+len > Length(outputarray) then len := Length(outputarray)-a; // Don't allow clip to extend outside file.
  if a > Length(outputarray) then len := 0;
  AssignFile(myclipfile,clipthisfile); // Open file.
  FileMode := fmOpenReadWrite;
  ReWrite(myclipfile,1);
  SetLength(clipfilearray,len);
  Move(outputarray[a],clipfilearray[0],len); // Copy specified section.
  BlockWrite(myclipfile,clipfilearray[0],len); // Copy contents of array to file.
  CloseFile(myclipfile); // Close file.
end;

{ Create new blank file. }

procedure NewFile(filelen: integer);
begin
  FillChar(filearray[0],fs,0); // Fill existing file with 0.
  SetLength(filearray,filelen);
  fs := filelen;
end;

procedure NewFileOutput(filelen: integer);
begin
  FillChar(outputarray[0],Length(outputarray),0); // Fill existing file with 0.
  SetLength(outputarray,filelen);
end;

{ Add file to existing file array. }

procedure AddFile(a: integer; addthisfile: string);
var myaddfile: file;
begin
  AssignFile(myaddfile,addthisfile); // Get file.
  FileMode := fmOpenRead; // Read only.
  Reset(myaddfile,1);
  if fs < a+FileSize(myaddfile) then SetLength(filearray,a+FileSize(myaddfile)); // Enlarge file if needed.
  BlockRead(myaddfile,filearray[a],FileSize(myaddfile)); // Copy file to array.
  CloseFile(myaddfile); // Close file.
  fs := Length(filearray);
end;

{ Make filesize even. }

procedure EvenFile;
begin
  if Odd(fs) then SetLength(filearray,fs+1); // Add 1 byte to end if odd.
  fs := Length(filearray);
end;

procedure EvenFileOutput;
begin
  if Odd(Length(outputarray)) then SetLength(outputarray,Length(outputarray)+1); // Add 1 byte to end if odd.
end;

{ Get byte from file array. }

function GetByte(a: int64): byte;
begin
  if a < fs then result := filearray[a]
  else result := 0;
  fpos := a+1;
end;

function GetByteOutput(a: int64): byte;
begin
  if a < Length(outputarray) then result := outputarray[a]
  else result := 0;
end;

{ Get bit from file array. }

function GetBit(a: int64; b: integer): byte;
begin
  result := Bit(GetByte(a),b);
end;

{ Get bit from integer. }

function Bit(i, b: integer): byte;
begin
  result := (i and (1 shl b)) shr b;
end;

{ Get word from file array. }

function GetWord(a: int64): word;
begin
  result := (GetByte(a)*$100)+GetByte(a+1);
  fpos := a+2;
end;

function GetWordOutput(a: int64): word;
begin
  result := (GetByteOutput(a)*$100)+GetByteOutput(a+1);
end;

{ Get longword from file array. }

function GetDword(a: int64): longword;
begin
  result := (GetWord(a)*$10000)+GetWord(a+2);
  fpos := a+4;
end;

function GetDwordOutput(a: int64): longword;
begin
  result := (GetWordOutput(a)*$10000)+GetWordOutput(a+2);
end;

{ Get word (little endian) from file array. }

function GetWordRev(a: int64): word;
begin
  result := (GetByte(a+1)*$100)+GetByte(a);
  fpos := a+2;
end;

function GetWordRevOutput(a: int64): word;
begin
  result := (GetByteOutput(a+1)*$100)+GetByteOutput(a);
end;

{ Get longword (little endian) from file array. }

function GetDwordRev(a: int64): longword;
begin
  result := (GetWordRev(a+2)*$10000)+GetWordRev(a);
  fpos := a+4;
end;

function GetDwordRevOutput(a: int64): longword;
begin
  result := (GetWordRevOutput(a+2)*$10000)+GetWordRevOutput(a);
end;

{ Get string from file array. }

function GetString(a, maxlength: int64): string;
begin
  result := '';
  while maxlength > 0 do
    begin
    Dec(maxlength);
    if GetByte(a) in [32..126] then result := result+Chr(GetByte(a)) // Add character to string if valid.
    else maxlength := 0; // Otherwise end the string.
    Inc(a); // Next character.
    fpos := a;
    end;
end;

function GetStringOutput(a, maxlength: int64): string;
begin
  result := '';
  while maxlength > 0 do
    begin
    Dec(maxlength);
    if GetByteOutput(a) in [32..126] then result := result+Chr(GetByteOutput(a)) // Add character to string if valid.
    else maxlength := 0; // Otherwise end the string.
    Inc(a); // Next character.
    end;
end;

{ As above, but allows for spaces between each letter. }

function GetStringWide(a, maxlength, charw: int64): string;
begin
  result := '';
  while maxlength > 0 do
    begin
    Dec(maxlength);
    if GetByte(a) in [32..126] then result := result+Chr(GetByte(a)) // Add character to string if valid.
    else maxlength := 0; // Otherwise end the string.
    a := a+charw; // Next character.
    fpos := a;
    end;
end;

function GetStringWideOutput(a, maxlength, charw: int64): string;
begin
  result := '';
  while maxlength > 0 do
    begin
    Dec(maxlength);
    if GetByteOutput(a) in [32..126] then result := result+Chr(GetByteOutput(a)) // Add character to string if valid.
    else maxlength := 0; // Otherwise end the string.
    a := a+charw; // Next character.
    end;
end;

{ Get string integer from file array. }

function GetStrInt(a, maxlength: int64): string;
var b: byte;
begin
  result := '';
  while maxlength > 0 do
    begin
    Dec(maxlength);
    b := GetByte(a);
    if b in [48..57] then result := result+Chr(b) // Add character to string if valid.
    else if (b = 32) and (result = '') then result := result // Ignore leading spaces.
    else maxlength := 0; // Otherwise end the string.
    Inc(a); // Next character.
    fpos := a;
    end;
  if result = '' then result := '0'; // Return 0 if no string is found.
end;

function GetStrIntOutput(a, maxlength: int64): string;
var b: byte;
begin
  result := '';
  while maxlength > 0 do
    begin
    Dec(maxlength);
    b := GetByteOutput(a);
    if b in [48..57] then result := result+Chr(b) // Add character to string if valid.
    else if (b = 32) and (result = '') then result := result // Ignore leading spaces.
    else maxlength := 0; // Otherwise end the string.
    Inc(a); // Next character.
    end;
  if result = '' then result := '0'; // Return 0 if no string is found.
end;

{ Write single byte to file array. }

procedure WriteByte(a: int64; b: byte);
begin
  if fs < a+1 then SetLength(filearray,a+1); // Enlarge file if needed.
  filearray[a] := b;
  fs := Length(filearray);
  fpos := a+1;
end;

procedure WriteByteOutput(a: int64; b: byte);
begin
  if Length(outputarray) < a+1 then SetLength(outputarray,a+1); // Enlarge file if needed.
  outputarray[a] := b;
end;

{ Write word to file array. }

procedure WriteWord(a: int64; w: word);
begin
  if fs < a+2 then SetLength(filearray,a+2); // Enlarge file if needed.
  filearray[a] := w shr 8;
  filearray[a+1] := w and $FF;
  fs := Length(filearray);
  fpos := a+2;
end;

procedure WriteWordOutput(a: int64; w: word);
begin
  if Length(outputarray) < a+2 then SetLength(outputarray,a+2); // Enlarge file if needed.
  outputarray[a] := w shr 8;
  outputarray[a+1] := w and $FF;
end;

{ Write word (little endian) to file array. }

procedure WriteWordRev(a: int64; w: word);
begin
  if fs < a+2 then SetLength(filearray,a+2); // Enlarge file if needed.
  filearray[a+1] := w shr 8;
  filearray[a] := w and $FF;
  fs := Length(filearray);
  fpos := a+2;
end;

procedure WriteWordRevOutput(a: int64; w: word);
begin
  if Length(outputarray) < a+2 then SetLength(outputarray,a+2); // Enlarge file if needed.
  outputarray[a+1] := w shr 8;
  outputarray[a] := w and $FF;
end;

{ Write longword to file array. }

procedure WriteDword(a: int64; d: longword);
begin
  if fs < a+4 then SetLength(filearray,a+4); // Enlarge file if needed.
  WriteWord(a,d shr 16);
  WriteWord(a+2,d and $FFFF);
  fs := Length(filearray);
  fpos := a+4;
end;

procedure WriteDwordOutput(a: int64; d: longword);
begin
  if Length(outputarray) < a+4 then SetLength(outputarray,a+4); // Enlarge file if needed.
  WriteWordOutput(a,d shr 16);
  WriteWordOutput(a+2,d and $FFFF);
end;

{ Write longword (little endian) to file array. }

procedure WriteDwordRev(a: int64; d: longword);
begin
  if fs < a+4 then SetLength(filearray,a+4); // Enlarge file if needed.
  WriteWordRev(a+2,d shr 16);
  WriteWordRev(a,d and $FFFF);
  fs := Length(filearray);
  fpos := a+4;
end;

procedure WriteDwordRevOutput(a: int64; d: longword);
begin
  if Length(outputarray) < a+4 then SetLength(outputarray,a+4); // Enlarge file if needed.
  WriteWordRevOutput(a+2,d shr 16);
  WriteWordRevOutput(a,d and $FFFF);
end;

{ Run an external program. }

procedure RunCommand(command: string);
var StartInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
begin
  FillChar(StartInfo,SizeOf(TStartupInfo),#0);
  FillChar(ProcInfo,SizeOf(TProcessInformation),#0);
  StartInfo.cb := SizeOf(TStartupInfo);
  if CreateProcess(nil,PChar(command),nil,nil,false,CREATE_NEW_PROCESS_GROUP+NORMAL_PRIORITY_CLASS+CREATE_NO_WINDOW,nil,nil,StartInfo,ProcInfo) then
    begin
    CloseHandle(ProcInfo.hThread);
    WaitForSingleObject(ProcInfo.hProcess,INFINITE);
    CloseHandle(ProcInfo.hProcess);
    end
  else
    {$IFDEF GUI_APP}
    ShowMessage('Failed to execute command: '+SysErrorMessage(GetLastError));
    {$ELSE}
    WriteLn('Failed to execute command: '+SysErrorMessage(GetLastError));
    {$ENDIF}
end;

{ Check if a file is in use by another program. }

function FileInUse(f: string): boolean;
var FileHandle: THandle;
begin
  Result := false;
  try
    FileHandle := FileOpen(f, fmOpenWrite or fmShareExclusive);  // Attempt to open the file in exclusive mode.
    if FileHandle <> THandle(-1) then FileClose(FileHandle) // File is not in use, close the handle.
    else Result := True; // File is in use by another program.
  except
    Result := True; // An exception occurred while opening the file.
  end;
end;

{ List subfolders in a specified folder; the results are stored in folderlist array. }

procedure ListFolders(dir: string; subfolders: boolean);
var rec: TSearchRec;
  i: integer;
begin
  SetLength(folderlist,1);
  folderlist[0] := '\'; // First folder is current one.
  i := 0;
  while i < Length(folderlist) do
    begin
    if FindFirst(dir+'\'+folderlist[i]+'*.*',faDirectory,rec) = 0 then
      begin
      repeat
      if (rec.Name<>'.') and (rec.Name<>'..') and ((rec.attr and faDirectory)=faDirectory) then
        begin
        SetLength(folderlist,Length(folderlist)+1); // Add 1 slot for folder name.
        folderlist[Length(folderlist)-1] := folderlist[i]+rec.Name+'\'; // Add folder name to array.
        end;
      until FindNext(rec) <>0;
      FindClose(rec);
      end;
    Inc(i);
    if not subfolders then exit; // Only run once if subfolders aren't wanted.
    end;
end;

{ List files in a specified folder; the results are stored in filelist array. }

procedure ListFiles(dir: string; subfolders: boolean);
var rec: TSearchRec;
  i: integer;
begin
  SetLength(filelist,0); // Assume no files.
  if subfolders then ListFolders(dir,true) // Create list of folders.
  else
    begin
    SetLength(folderlist,1);
    folderlist[0] := '\'; // Only folder is current one.
    end;
  for i := 0 to Length(folderlist)-1 do
    begin
    if FindFirst(dir+folderlist[i]+'*.*',faAnyFile-faDirectory,rec) = 0 then
      begin
      repeat
        begin
        SetLength(filelist,Length(filelist)+1); // Add slot to filelist.
        filelist[Length(filelist)-1] := folderlist[i]+rec.Name; // Add file name to array.
        end;
      until FindNext(rec) <> 0;
      FindClose(rec);
      end;
    end;
end;

{ Read base64 from file and convert to binary in output file array. }

procedure GetBase64(a, a2: int64);
var b: byte;
  pos: integer;
label loop;
begin
  pos := a2*8; // Set initial position in bits.

loop:
  b := ByteToBase64(GetByte(a));
  if b = 64 then // Stop at invalid or terminator character (=), or end of file.
    begin
    SetLength(outputarray,pos div 8); // Trim excess byte.
    exit;
    end;
  a2 := pos div 8; // Get current output byte.
  case pos mod 8 of
    0:
      begin
      WriteByteOutput(a2,b shl 2);
      end;
    6:
      begin
      WriteByteOutput(a2,(GetByteOutput(a2) and $FC)+(b shr 4));
      WriteByteOutput(a2+1,(b shl 4) and $F0);
      end;
    4:
      begin
      WriteByteOutput(a2,(GetByteOutput(a2) and $F0)+(b shr 2));
      WriteByteOutput(a2+1,(b shl 6) and $C0);
      end;
    2:
      begin
      WriteByteOutput(a2,(GetByteOutput(a2) and $C0)+b);
      end;
  end;
  Inc(a); // Next byte.
  fpos := a;
  pos := pos+6; // Next 6 bits.
  goto loop; // Repeat.
end;

{ Convert a character (as byte) to its base64 value. }

function ByteToBase64(c: byte): byte;
begin
  if (c >= Byte('A')) and (c <= Byte('Z')) then result := c-Byte('A')
  else if (c >= Byte('a')) and (c <= Byte('z')) then result := c-Byte('a')+26
  else if (c >= Byte('0')) and (c <= Byte('9')) then result := c-Byte('0')+52
  else if c = Byte('+') then result := 62
  else if c = Byte('/') then result := 63
  else result := 64;
end;

end.
