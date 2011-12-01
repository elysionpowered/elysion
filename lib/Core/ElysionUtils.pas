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
  Math,
  ElysionTypes;

function IntToString(aValue: Integer; LeadingZero: Boolean; Digits: Integer): AnsiString;
function BoolToString(aValue: Boolean): AnsiString; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function BoolToInt(aValue: Boolean): Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function BoolToFloat(aValue: Boolean): Single; {$IFDEF CAN_INLINE} inline; {$ENDIF}

function CamelCase(aString: String): String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function HasSuffix(aString, aSubString: AnsiString): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function HasSuffix(aString: AnsiString; aChar: Char): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

function StringWithoutPrefix(anOrgString, aPrefix: AnsiString): AnsiString; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function StringWithoutPrefix(anOrgString: AnsiString; aPrefix: Char): AnsiString; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

function StringWithoutSuffix(anOrgString, aSuffix: AnsiString): AnsiString; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function StringWithoutSuffix(anOrgString: AnsiString; aSuffix: Char): AnsiString; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

function SubStringToIndex(anOrgString: AnsiString; anIndex: Integer): AnsiString; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function SubStringFromIndex(anOrgString: AnsiString; anIndex: Integer): AnsiString; {$IFDEF CAN_INLINE} inline; {$ENDIF}

function HasPrefix(aString, aSubString: AnsiString): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function HasPrefix(aString: AnsiString; aChar: Char): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}


function Split(fText: AnsiString;fSep: Char;fTrim: Boolean=false;fQuotes: Boolean=false): TStringList;


function GetFilenameExtension(aFilename: AnsiString; isUpperCase: Boolean = true): AnsiString; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function GetFilenameWithoutExt(aFilename: AnsiString): AnsiString; {$IFDEF CAN_INLINE} inline; {$ENDIF}

function GetUserDirectory(): AnsiString; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function GetAppUserDirectory(AppName: AnsiString): AnsiString; {$IFDEF CAN_INLINE} inline; {$ENDIF}

// Checks if the application is in an application bundle or not (Mac OS X only)
function IsApplicationBundle(LazAppBundle: Boolean = false): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

{$IFDEF WINDOWS}
function GetWinSpecialDir(FolderID: Cardinal): AnsiString; {$IFDEF CAN_INLINE} inline; {$ENDIF}
{$ENDIF}

function IsInRange(Min, Max, Value: Integer): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function IsInRange(Min, Max, Value: Single): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

function PopulateArray(aKeyIdentArray: array of TKeyIdent): TKeyArray; Overload;
function PopulateArray(aColorArray: array of TelColor): TelColorArray; Overload;
function PopulateArray(aVectorArray: array of TelVector2f): TelVector2fArray; Overload;
function PopulateArray(aVectorArray: array of TelVector2i): TelVector2iArray; Overload;
function PopulateArray(aVectorArray: array of TelVector3f): TelVector3fArray; Overload;
function PopulateArray(aVectorArray: array of TelVector3i): TelVector3iArray; Overload;
function PopulateArray(aSizeArray: array of TelSize): TelSizeArray; Overload;
function PopulateArray(aRectArray: array of TelRect): TelRectArray; Overload;

function HexToInt(HexNum: AnsiString): LongInt; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function StringToColor(aString: AnsiString): TelColor;

function ValuesEnclosedSymbols(anOrgString: AnsiString; aBeginSymbol, anEndSymbol: Char; Seperator: Char; Trim: Boolean = true): TStringList; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function ValuesEnclosedStrings(anOrgString: AnsiString; aBeginSymbol, anEndSymbol: AnsiString; Seperator: Char; Trim: Boolean = true): TStringList; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function ValueEnclosedSymbols(anOrgString: AnsiString; aBeginSymbol, anEndSymbol: Char): AnsiString; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function ValueEnclosedStrings(anOrgString: AnsiString; aBeginSymbol, anEndSymbol: AnsiString): AnsiString; {$IFDEF CAN_INLINE} inline; {$ENDIF}


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

function IntToString(aValue: Integer; LeadingZero: Boolean; Digits: Integer): AnsiString;
var
  IntResult, Zeros: AnsiString;
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

function BoolToString(aValue: Boolean): AnsiString;
begin
  if aValue then Result := 'true'
            else Result := 'false';
end;

function BoolToInt(aValue: Boolean): Integer;
begin
  if aValue then Result := 1
            else Result := 0;
end;

function BoolToFloat(aValue: Boolean): Single;
begin
  if aValue then Result := 1.0
            else Result := 0.0;
end;

function CamelCase(aString: String): String;
var
  tmpString: String;
  i: Integer;
begin
  tmpString := AnsiLowerCase(aString);

  // First letter is uppercase - anywhere, anytime...
  tmpString[1] := UpCase(tmpString[1]);

  for i := 1 to Length(tmpString) - 1 do
  begin
    if ((tmpString[i] = ' ') or (tmpString[i] = '-') or (tmpString[i] = '_')) then
    begin
      Delete(tmpString, i, 1);
      if (tmpString[i] in ['a' .. 'z']) then tmpString[i] := UpCase(tmpString[i]);
    end;
  end;

  Result := tmpString;
end;

function HasSuffix(aString, aSubString: AnsiString): Boolean; Overload;
begin
  Result := (LastDelimiter(aSubString, aString) = Length(aString));
end;

function HasSuffix(aString: AnsiString; aChar: Char): Boolean; Overload;
begin
  Result := (aString[Length(aString)] = aChar);
end;

function StringWithoutPrefix(anOrgString, aPrefix: AnsiString): AnsiString;
begin
  if HasPrefix(anOrgString, aPrefix) then
    Result := Copy(anOrgString, Length(aPrefix) + 1, Length(anOrgString))
  else
    Result := anOrgString;
end;

function StringWithoutPrefix(anOrgString: AnsiString; aPrefix: Char): AnsiString;
begin
  if HasPrefix(anOrgString, aPrefix) then
    Result := Copy(anOrgString, Length(aPrefix) + 1, Length(anOrgString))
  else
    Result := anOrgString;
end;

function StringWithoutSuffix(anOrgString, aSuffix: AnsiString): AnsiString; Overload;
begin
  if HasSuffix(anOrgString, aSuffix) then
    Result := (Copy(anOrgString, 1, Length(anOrgString) - Length(aSuffix)))
  else
    Result := anOrgString;
end;

function StringWithoutSuffix(anOrgString: AnsiString; aSuffix: Char): AnsiString; Overload;
begin
  if HasSuffix(anOrgString, aSuffix) then
    Result := Copy(anOrgString, 1, Length(anOrgString) - 1)
  else
    Result := anOrgString;
end;

function SubStringToIndex(anOrgString: AnsiString; anIndex: Integer): AnsiString;
begin
  Result := Copy(anOrgString, 1, anIndex);
end;

function SubStringFromIndex(anOrgString: AnsiString; anIndex: Integer): AnsiString;
begin
  Result := Copy(anOrgString, anIndex + 1, Length(anOrgString));
end;

function HasPrefix(aString, aSubString: AnsiString): Boolean; Overload;
begin
  Result := (AnsiPos(aSubString, aString) = 1);
end;

function HasPrefix(aString: AnsiString; aChar: Char): Boolean; Overload;
begin
  Result := (aString[1] = aChar);
end;

// See: http://www.delphi-library.de/topic_wie+kann+ich+einen+AnsiString+zerteilen_26639,0.html
function Split(fText: AnsiString;fSep: Char;fTrim: Boolean=false;fQuotes: Boolean=false): TStringList;
var vI: Integer;
    vBuffer: AnsiString;
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


function GetFilenameExtension(aFilename: AnsiString; isUpperCase: Boolean = true): AnsiString;
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

function GetFilenameWithoutExt(aFilename: AnsiString): AnsiString;
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
function GetWinSpecialDir(FolderID: Cardinal): AnsiString;
var
  PIDL: PItemIDList;
  Folder: array[0..MAX_PATH] of Char;
begin
  SHGetSpecialFolderLocation(0, FolderID, PIDL);
  SHGetPathFromIDList(PIDL, Folder);
  Result := Folder;
end;
{$ENDIF}

function GetUserDirectory(): AnsiString;
begin
  {$IFDEF UNIX}
    Result := ExpandFileName('~/');
  {$ELSE}
    Result := GetWinSpecialDir(CSIDL_PERSONAL);
  {$ENDIF}
end;

function GetAppUserDirectory(AppName: AnsiString): AnsiString;
var
  SpecDir: AnsiString;
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
function IsDirectoryEmpty(const Directory : AnsiString) : Boolean;
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
{$IFDEF DARWIN}
var
  IsAppBundle: Boolean;
{$ENDIF}
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

function IsInRange(Min, Max, Value: Integer): Boolean;
begin
  Result := ((Value >= Min) and (Value <= Max));
end;

function IsInRange(Min, Max, Value: Single): Boolean;
begin
  Result := ((Value >= Min) and (Value <= Max));
end;

function PopulateArray(aKeyIdentArray: array of TKeyIdent
  ): TKeyArray;
var
  i: Integer;
  tmpArray: TKeyArray;
begin
  SetLength(tmpArray, Length(aKeyIdentArray));

  for i := 0 to Length(aKeyIdentArray) - 1 do
    tmpArray[i] := aKeyIdentArray[i];

  Result := tmpArray;
end;

function PopulateArray(aColorArray: array of TelColor): TelColorArray;
var
  i: Integer;
  tmpArray: TelColorArray;
begin
  SetLength(tmpArray, Length(aColorArray));

  for i := 0 to Length(aColorArray) - 1 do
    tmpArray[i] := aColorArray[i];

  Result := tmpArray;
end;

function PopulateArray(aVectorArray: array of TelVector2f
  ): TelVector2fArray;
var
  i: Integer;
  tmpArray: TelVector2fArray;
begin
  SetLength(tmpArray, Length(aVectorArray));

  for i := 0 to Length(aVectorArray) - 1 do
    tmpArray[i] := aVectorArray[i];

  Result := tmpArray;
end;

function PopulateArray(aVectorArray: array of TelVector2i
  ): TelVector2iArray;
var
  i: Integer;
  tmpArray: TelVector2iArray;
begin
  SetLength(tmpArray, Length(aVectorArray));

  for i := 0 to Length(aVectorArray) - 1 do
    tmpArray[i] := aVectorArray[i];

  Result := tmpArray;
end;

function PopulateArray(aVectorArray: array of TelVector3f
  ): TelVector3fArray;
var
  i: Integer;
  tmpArray: TelVector3fArray;
begin
  SetLength(tmpArray, Length(aVectorArray));

  for i := 0 to Length(aVectorArray) - 1 do
    tmpArray[i] := aVectorArray[i];

  Result := tmpArray;
end;

function PopulateArray(aVectorArray: array of TelVector3i
  ): TelVector3iArray;
var
  i: Integer;
  tmpArray: TelVector3iArray;
begin
  SetLength(tmpArray, Length(aVectorArray));

  for i := 0 to Length(aVectorArray) - 1 do
    tmpArray[i] := aVectorArray[i];

  Result := tmpArray;
end;

function PopulateArray(aSizeArray: array of TelSize): TelSizeArray;
var
  i: Integer;
  tmpArray: TelSizeArray;
begin
  SetLength(tmpArray, Length(aSizeArray));

  for i := 0 to Length(aSizeArray) - 1 do
    tmpArray[i] := aSizeArray[i];

  Result := tmpArray;
end;

function PopulateArray(aRectArray: array of TelRect): TelRectArray;
var
  i: Integer;
  tmpArray: TelRectArray;
begin
  SetLength(tmpArray, Length(aRectArray));

  for i := 0 to Length(aRectArray) - 1 do
    tmpArray[i] := aRectArray[i];

  Result := tmpArray;
end;

function HexToInt(HexNum: AnsiString): LongInt;
begin
  Result := StrToInt('$' + HexNum);
end;

function StringToColor(aString: AnsiString): TelColor;
var
  tmpString: AnsiString;
  tmpStringList: TStringList;
  tmpR, tmpG, tmpB: Integer;
  tmpA: Single;
begin
  Result := makeCol(0, 0, 0, 255);

  if HasPrefix(aString, '#') then
  begin
    tmpString := StringWithoutPrefix(aString, '#');

    // Only works if hex Ansistring is 3 or 6 characters
    if ((Length(tmpString) / 3) <= 2) then
    begin
      if (Length(tmpString) / 3) = 1 then
      begin
        tmpR := HexToInt(Copy(tmpString, 1, 1) + Copy(tmpString, 1, 1));
        tmpG := HexToInt(Copy(tmpString, 2, 1) + Copy(tmpString, 2, 1));
        tmpB := HexToInt(Copy(tmpString, 3, 1) + Copy(tmpString, 3, 1));
      end else
      begin
        tmpR := HexToInt(Copy(tmpString, 1, 2));
        tmpG := HexToInt(Copy(tmpString, 3, 2));
        tmpB := HexToInt(Copy(tmpString, 5, 2));
      end;

      Result := makeCol(tmpR, tmpG, tmpB, 255);
    end;
  end else
  begin
    if HasPrefix(aString, 'rgb') or HasPrefix(aString, 'rgba') then
    begin
      if HasPrefix(aString, 'rgb') then
      begin
        tmpString := StringWithoutPrefix(aString, 'rgb');

        tmpStringList := ValuesEnclosedSymbols(tmpString, '(', ')', ',');
        if ((tmpStringList = nil) {or (tmpStringList.Count <> 3) or (tmpStringList.Text = '')}) then Exit;

        if not TryStrToInt(tmpStringList[0], tmpR) then tmpR := 0;
        if not TryStrToInt(tmpStringList[1], tmpG) then tmpG := 0;
        if not TryStrToInt(tmpStringList[2], tmpB) then tmpB := 0;

        Result := makeCol(tmpR, tmpG, tmpB, 255);
      end else
      begin
        tmpString := StringWithoutPrefix(aString, 'rgba');

        tmpStringList := ValuesEnclosedSymbols(tmpString, '(', ')', ',');
        if ((tmpStringList = nil) {or (tmpStringList.Count <> 4) or (tmpStringList.Text = '')}) then Exit;

        if not TryStrToInt(tmpStringList[0], tmpR) then tmpR := 0;
        if not TryStrToInt(tmpStringList[1], tmpG) then tmpG := 0;
        if not TryStrToInt(tmpStringList[2], tmpB) then tmpB := 0;
        if not TryStrToFloat(tmpStringList[3], tmpA) then tmpA := 1;

        Result := makeCol(tmpR, tmpG, tmpB, Integer(tmpA * 255));
      end;
    end;
  end;
end;

function ValuesEnclosedSymbols(anOrgString: AnsiString; aBeginSymbol,
  anEndSymbol: Char; Seperator: Char; Trim: Boolean = true): TStringList;
var
  tmpStringList: TStringList;
  tmpValue: AnsiString;
  i: Integer;
begin
  tmpValue := ValueEnclosedSymbols(anOrgString, aBeginSymbol, anEndSymbol);

  if tmpValue = '' then
  begin
    Result := nil;
    Exit;
  end;

  tmpStringList := Split(tmpValue, Seperator, Trim);

  Result := tmpStringList;
end;

function ValuesEnclosedStrings(anOrgString: AnsiString; aBeginSymbol,
  anEndSymbol: AnsiString; Seperator: Char; Trim: Boolean = true): TStringList;
var
  tmpStringList: TStringList;
  tmpValue: AnsiString;
  i: Integer;
begin
  tmpValue := ValueEnclosedStrings(anOrgString, aBeginSymbol, anEndSymbol);

  if tmpValue = '' then
  begin
    Result := nil;
    Exit;
  end;

  tmpStringList := Split(tmpValue, Seperator, Trim);

  Result := tmpStringList;
end;

function ValueEnclosedSymbols(anOrgString: AnsiString; aBeginSymbol,
  anEndSymbol: Char): AnsiString;
var
  tmpString: AnsiString;
begin
  tmpString := Trim(tmpString);

  if (HasPrefix(anOrgString, aBeginSymbol) and HasSuffix(anOrgString, anEndSymbol)) then
    Result := StringWithoutPrefix(StringWithoutSuffix(anOrgString, anEndSymbol), aBeginSymbol)
  else
    Result := '';
end;

function ValueEnclosedStrings(anOrgString: AnsiString; aBeginSymbol,
  anEndSymbol: AnsiString): AnsiString;
var
  tmpString: AnsiString;
begin
  tmpString := Trim(tmpString);

  if (HasPrefix(anOrgString, aBeginSymbol) and HasSuffix(anOrgString, anEndSymbol)) then
    Result := StringWithoutPrefix(StringWithoutSuffix(anOrgString, anEndSymbol), aBeginSymbol)
  else
    Result := '';
end;

end.
