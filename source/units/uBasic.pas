// GetResPath* functions will be obsolete in the near future

unit uBasic;

interface

{$I Elysion.inc}

const
  STD_FONT = 'Cabin.ttf';

  // Use those values if loading from data file failed
  WIDTH = 1024;
  HEIGHT = 600;
  BITS = 32;
  
  
  {$IFDEF WINDOWS}
  RESPATH = '..\resources\';
  {$ELSE}
    {$IFDEF DARWIN}
    RESPATH = '../Resources/';
    {$ELSE}
    RESPATH = '../resources/';
    {$ENDIF}
  {$ENDIF}

  {$IFNDEF FPC}
    // Assume Delphi
    {$IFDEF WINDOWS}
      const DirectorySeparator = '\';
    {$ELSE}
      const DirectorySeparator = '/';
    {$ENDIF}
  {$ENDIF}


type
  TGameState = (gsMainMenu, gsGame, gsInstructions, gsIntro, gsOptions, gsCredits);
  TLanguage = (laGerman, laEnglish);

function GetResPath: String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function GetResDataPath: String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function GetResImgPath: String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function GetResSndPath: String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function GetResFontPath: String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function GetStdFont: String; {$IFDEF CAN_INLINE} inline; {$ENDIF}

implementation

function GetResPath: String;
begin
  Result := RESPATH;
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
