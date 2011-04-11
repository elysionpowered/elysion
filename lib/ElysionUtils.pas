unit ElysionUtils;

interface

{$I Elysion.inc}

uses
  {$IFDEF WINDOWS}
  Windows,
    {$IFNDEF FPC}
    ShellAPI, ShlObj,
    {$ENDIF}
  {$ENDIF}
  SysUtils,
  Classes,
  Math;

function IntToString(aValue: Integer; LeadingZero: Boolean; Digits: Integer): String;
function BoolToString(aValue: Boolean): String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function Split(fText: String;fSep: Char;fTrim: Boolean=false;fQuotes: Boolean=false): TStringList;


function GetFilenameExtension(Filename: String): String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function GetFilenameWithoutExt(Filename: String): String; {$IFDEF CAN_INLINE} inline; {$ENDIF}

function GetUserDirectory(): String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function GetAppUserDirectory(AppName: String): String; {$IFDEF CAN_INLINE} inline; {$ENDIF}

{$IFDEF WINDOWS}
function GetWinSpecialDir(FolderID: Cardinal): String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
{$ENDIF}


{$IFDEF WINDOWS}
  const CSIDL_PERSONAL = $0005; //< My Documents (Win 95+)
  const CSIDL_LOCAL_APPDATA = $001c; //< Local Appdata (Windows 2000+)
  const CSIDL_APPDATA = $001a; //< Win 95+ with IE4.0 Shell installed
{$ENDIF}

{$IFNDEF FPC}
  // Assume Delphi
  {$IFDEF WINDOWS}
  const DirectorySeparator = '\';
  {$ELSE}
  const DirectorySeparator = '/';
  {$ENDIF}
{$ENDIF}


implementation

function IntToString(aValue: Integer; LeadingZero: Boolean; Digits: Integer): String;
var
  IntResult, Zeros: String;
  i: Integer;
begin
  Zeros := '';
  IntResult := SysUtils.IntToStr(aValue);
  if LeadingZero then
  begin
    for i := 1 to Digits do
    begin
      if aValue < Power(10, i) then Zeros := Zeros + '0';
    end;
    Result := Zeros + IntResult;
  end else Result := IntResult;
end;

function BoolToString(aValue: Boolean): String;
begin
  if aValue then Result := 'true'
            else Result := 'false';
end;

// See: http://www.delphi-library.de/topic_wie+kann+ich+einen+String+zerteilen_26639,0.html
function Split(fText: String;fSep: Char;fTrim: Boolean=false;fQuotes: Boolean=false): TStringList;
var vI: Integer;
    vBuffer: String;
    vOn: Boolean;
begin
  Result:=TStringList.Create;
  vBuffer:='';
  vOn:=true;
  for vI:=1 to Length(fText) do
  begin
    if (fQuotes and(fText[vI]=fSep)and vOn)or(Not(fQuotes) and (fText[vI]=fSep)) then
    begin
      if fTrim then vBuffer:=Trim(vBuffer);
      if vBuffer[1]=fSep then
        vBuffer:=Copy(vBuffer,2,Length(vBuffer));
      Result.Add(vBuffer);
      vBuffer:='';
    end;
    if fQuotes then
    begin
      if fText[vI]='"' then
      begin
        vOn:=Not(vOn);
        Continue;
      end;
      if (fText[vI]<>fSep)or((fText[vI]=fSep)and(vOn=false)) then
        vBuffer:=vBuffer+fText[vI];
    end else
      if fText[vI]<>fSep then
        vBuffer:=vBuffer+fText[vI];
  end;
  if vBuffer<>'' then
  begin
    if fTrim then vBuffer:=Trim(vBuffer);
    Result.Add(vBuffer);
  end;
end;


function GetFilenameExtension(Filename: String): String;
begin
  Result := UpperCase(Copy(Filename, LastDelimiter('.', Filename) + 1, Length(Filename)));
end;

function GetFilenameWithoutExt(Filename: String): String;
begin
  Result := Copy(Filename, LastDelimiter(DirectorySeparator, Filename) + 1, LastDelimiter('.', Filename) - 1);
end;

{$IFDEF WINDOWS}
function GetWinSpecialDir(FolderID: Cardinal): String;
var
  PIDL: PItemIDList;
  Folder: array[0..MAX_PATH] of Char;
begin
  SHGetSpecialFolderLocation(0, FolderID, PIDL);
  SHGetPathFromIDList(PIDL, Folder);
  Result := Folder;
end;
{$ENDIF}

function GetUserDirectory(): String;
begin
  {$IFDEF UNIX}
    Result := ExpandFileName('~/');
  {$ELSE}
    GetWinSpecialDir(CSIDL_PERSONAL);
  {$ENDIF}
end;

function GetAppUserDirectory(AppName: String): String;
var
  SpecDir: String;
begin
  {$IFDEF WINDOWS}
    SpecDir := GetWinSpecialDir(CSIDL_LOCAL_APPDATA);
    if SpecDir = '' then SpecDir := GetWinSpecialDir(CSIDL_APPDATA);
    // Still no folder found... ok, take the
    if SpecDir = '' then SpecDir := GetUserDirectory();

    Result := SpecDir;
  {$ENDIF}

  {$IFDEF LINUX}
    Result := GetUserDirectory() + '.' + AppName + DirectorySeparator;
  {$ENDIF}

  {$IFDEF DARWIN}
    Result := GetUserDirectory() + 'Application\ Support' + DirectorySeparator + AppName + DirectorySeparator;
  {$ENDIF}
end;

end.
