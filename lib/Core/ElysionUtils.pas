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


function GetFilenameExtension(aFilename: String; isUpperCase: Boolean = true): String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function GetFilenameWithoutExt(aFilename: String): String; {$IFDEF CAN_INLINE} inline; {$ENDIF}

function GetUserDirectory(): String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function GetAppUserDirectory(AppName: String): String; {$IFDEF CAN_INLINE} inline; {$ENDIF}

// Checks if the application is in an application bundle or not (Mac OS X only)
function IsApplicationBundle(LazAppBundle: Boolean = false): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

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


function GetFilenameExtension(aFilename: String; isUpperCase: Boolean = true): String;
var
  lastSlash, lastDot, diffDotSlash, tmpLength: Integer;
begin
  lastSlash := LastDelimiter(DirectorySeparator, aFilename);
  lastDot := LastDelimiter('.', aFilename);
  
  diffDotSlash := lastDot - lastSlash;

  if ((lastDot = 0) or (diffDotSlash <= 0)) then Result := ''
  else begin
    tmpLength := Length(aFilename) - lastDot;

    if isUpperCase then Result := UpperCase(Copy(aFilename, lastDot + 1, tmpLength))
    else Result := LowerCase(Copy(aFilename, lastDot + 1, tmpLength));
  end;
end;

function GetFilenameWithoutExt(aFilename: String): String;
var
  lastSlash, lastDot, diffDotSlash, tmpLength: Integer;
begin
  lastSlash := LastDelimiter(DirectorySeparator, aFilename);
  lastDot := LastDelimiter('.', aFilename);
  
  diffDotSlash := lastDot - lastSlash;

  if ((lastDot = 0) or (diffDotSlash <= 0)) then
    tmpLength := Length(aFilename) - lastSlash
  else
    tmpLength := lastDot - lastSlash - 1;
  
  Result := Copy(aFilename, lastSlash + 1, tmpLength);
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
    // Still no folder found... ok, take the user directory
    if SpecDir = '' then SpecDir := GetUserDirectory();

    if AppName = '' then Result := SpecDir + DirectorySeparator
    else Result := SpecDir + DirectorySeparator + AppName + DirectorySeparator;
  {$ENDIF}

  {$IFDEF LINUX}
    if AppName = '' then Result := GetUserDirectory() + DirectorySeparator
    else Result := GetUserDirectory() + '.' + AppName + DirectorySeparator;
  {$ENDIF}

  {$IFDEF DARWIN}
    if AppName = '' then Result := GetUserDirectory() + DirectorySeparator
    else Result := GetUserDirectory() + 'Library/Application Support' + DirectorySeparator + AppName + DirectorySeparator;
  {$ENDIF}
end;

//returns true if a given directory is empty, false otherwise
function IsDirectoryEmpty(const Directory : String) : Boolean;
var
  searchRec: TSearchRec;
begin
  // If a directory does not exist, it is probably empty ;)
  if not DirectoryExists(Directory) then
  begin
    Result := true;
    Exit;
  end;

  Try
    Result := ((FindFirst(directory + DirectorySeparator + '*.*', faAnyFile, searchRec) = 0) and (FindNext(searchRec) = 0) and (FindNext(searchRec) <> 0));
   finally
     FindClose(searchRec);
   end;
end;

// Checks if application is in an application bundle (Mac OS X only)
function IsApplicationBundle(LazAppBundle: Boolean = false): Boolean;
var
  IsAppBundle: Boolean;
begin

  {$IFNDEF DARWIN}
    Result := false;
  {$ELSE}
    // Check for MacOS folder and PList file
    // Should be enough I guess (especially if you have or want bundles that are not the way Apple has specified)
    IsAppBundle := (DirectoryExists(ExtractFilePath(ParamStr(0)) + '../MacOS') and DirectoryExists(ExtractFilePath(ParamStr(0)) + '../../Contents') and FileExists(ExtractFilePath(ParamStr(0)) + '../Info.plist'));

    if LazAppBundle then
      // Lazarus standard app bundle
      Result := IsAppBundle and (IsDirectoryEmpty((ExtractFilePath(ParamStr(0)) + '../Resources')))
    else
      Result := IsAppBundle;
  {$ENDIF}
end;

end.
