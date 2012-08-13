// GetResPath* functions will be obsolete in the near future

unit uBasic;

interface

uses
  SysUtils,
  ElysionConst,
  ElysionLogger,
  ElysionUtils;

{$I Elysion.inc}

const
  STD_FONT = 'Cabin.ttf';

  // Use those values if loading from data file failed
  WIDTH = 1024;
  HEIGHT = 600;
  BITS = 32;


type
  //TGameState = (gsMainMenu, gsGame, gsInstructions, gsIntro, gsOptions, gsCredits);
  TLanguage = (laGerman, laEnglish);

function GetResPath(aCustomResName: String = ''): String; inline;
function GetResDataPath: String; inline;
function GetResImgPath: String; inline;
function GetResSndPath: String; inline;
function GetResFontPath: String; inline;
function GetStdFont: String; inline;

implementation

var
  ResName, PreDir: String;

function GetResPath(aCustomResName: String = ''): String;
begin
  if aCustomResName <> '' then ResName := aCustomResName
  else ResName := 'assets';

  // Resources folder can either be a parent sub-folder or a direct sub-folder
  // relative to the executable
  if DirectoryExists(ExtractFilePath(ParamStr(0)) + '..' + DirectorySeparator + ResName) then PreDir := '..' + DirectorySeparator
  else if DirectoryExists(ExtractFilePath(ParamStr(0)) + '..' + DirectorySeparator + '..' + DirectorySeparator + ResName) then PreDir := '..' + DirectorySeparator + '..' + DirectorySeparator
  else PreDir := '';

  {$IFDEF DARWIN}
    TelLogger.GetInstance.WriteLog(PreDir);
    if IsApplicationBundle(true) then Result := '../../../../../' + PreDir + ResName + DirectorySeparator
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
