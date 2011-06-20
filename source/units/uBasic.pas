// GetResPath* functions will be obsolete in the near future

unit uBasic;

interface

uses
  SysUtils,
  ElysionUtils;

{$I Elysion.inc}

const
  STD_FONT = 'Cabin.ttf';

  // Use those values if loading from data file failed
  WIDTH = 1024;
  HEIGHT = 600;
  BITS = 32;

  {$IFNDEF FPC}
    // Assume Delphi
    {$IFDEF WINDOWS}
      const DirectorySeparator = '\';
    {$ELSE}
      const DirectorySeparator = '/';
    {$ENDIF}
  {$ENDIF}


type
  //TGameState = (gsMainMenu, gsGame, gsInstructions, gsIntro, gsOptions, gsCredits);
  TLanguage = (laGerman, laEnglish);

function GetResPath(aCustomResName: String = ''): String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function GetResDataPath: String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function GetResImgPath: String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function GetResSndPath: String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function GetResFontPath: String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function GetStdFont: String; {$IFDEF CAN_INLINE} inline; {$ENDIF}

implementation

var
  ResName, PreDir: String;

function GetResPath(aCustomResName: String = ''): String;
begin
  if aCustomResName <> '' then ResName := aCustomResName
  else ResName := 'resources';

  // Resources folder can either be a parent sub-folder or a direct sub-folder
  // relative to the executable
  if DirectoryExists(ExtractFilePath(ParamStr(0)) + '..' + DirectorySeparator + ResName) then PreDir := '..' + DirectorySeparator
  else PreDir := '';

  {$IFDEF DARWIN}
    if IsApplicationBundle(true) then Result := '../../../' + PreDir + ResName + DirectorySeparator
    else begin
      if IsApplicationBundle(false) then Result := '../Resources/'
      else Result := PreDir + ResName + DirectorySeparator;
    end;
  {$ELSE}
    Result := PreDir + ResName + DirectorySeparator;
  {$ENDIF}
end;

function GetResDataPath: String;
begin
  Result := GetResPath + 'data' + DirectorySeparator;
end;

function GetResImgPath: String;
begin
  Result := GetResPath + 'images' + DirectorySeparator;
end;

function GetResSndPath: String;
begin
  Result := GetResPath + 'sounds' + DirectorySeparator;
end;

function GetResFontPath: String;
begin
  Result := GetResPath + 'fonts' + DirectorySeparator;
end;

function GetStdFont: String;
begin
  Result := GetResFontPath + STD_FONT;
end;

end.
