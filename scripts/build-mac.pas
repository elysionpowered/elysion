program buildmac;
{$mode objfpc}

uses
  Classes,
  SysUtils,
  Unix;

const
  COMPILER_OPTIONS = ' -CX -dRelease';

  {$I config.inc}

type
  TShellRemoveOption = (roRecursive, roForce);
  TShellRemoveOptions = set of TShellRemoveOption;

var
  TargetIntel32: Integer;
  TargetIntel64: Integer;
  TargetPPC: Integer;
  AppBundlePath: AnsiString;
  MainExecName: AnsiString;
  SDL_FrameworkPath, SDL_TTF_FrameworkPath, SDL_Mixer_FrameworkPath, SDL_Net_FrameworkPath: AnsiString;


function CmdToString(Command: AnsiString): TStringList;
var
  formattedDateTime: AnsiString = '';
  Filename: AnsiString = '';
  tmpStringList: TStringList;
begin
  // Format date time string & construct filename
  DateTimeToString(formattedDateTime, 'yyyy-mm-dd_hh-nn-ss-z', Now);
  FileName := GetTempDir(true) + formattedDateTime + '.txt';

  // Create temporary file in temporary folder with timestamp as filename
  fpSystem(Command + ' > ' + FileName);

  // Create string list
  tmpStringList := TStringList.Create;

  // Load file into temporary string list
  tmpStringList.LoadFromFile(Filename);

  // Delete file
  DeleteFile(Filename);

  // Return temporary string list
  Result := tmpStringList;
end;

function HasSuffix(aString, aSubString: AnsiString): Boolean; Overload;
begin
  Result := (LastDelimiter(aSubString, aString) = Length(aString));
end;

function StringWithoutSuffix(anOrgString, aSuffix: AnsiString): AnsiString; Overload;
begin
  if HasSuffix(anOrgString, aSuffix) then
    Result := (Copy(anOrgString, 1, Length(anOrgString) - Length(aSuffix)))
  else
    Result := anOrgString;
end;


function GetMacOSVersion: AnsiString;
var
  tmpStringList: TStringList;
begin
  tmpStringList := CmdToString('sw_vers -productVersion');
  Result := tmpStringList.Text;
  tmpStringList.Free;
end;

function GetMacOSMajorVersion: Integer;
begin
  Result := StrToInt(Copy(GetMacOSVersion, 4, 1));
end;

procedure CpShell(aSource, aDest: AnsiString; Recursive: Boolean = true);
var
  RecursiveCmd: AnsiString;
begin
  if ((aSource = '') or (aDest = '')) then Exit;

  if Recursive then RecursiveCmd := ' -R'
    else RecursiveCmd := '';

  fpSystem('cp' + RecursiveCmd + ' "' + aSource + '" "' + aDest + '"');
end;

procedure MvShell(aSource, aDest: AnsiString);
begin
  if ((aSource = '') or (aDest = '')) then Exit;

  fpSystem('mv "' + aSource + '" "' + aDest + '"');
end;

procedure RmShell(aDest: AnsiString; Options: TShellRemoveOptions = [roRecursive, roForce]);
var
  RemoveOptions, RemoveCmd: AnsiString;
  Dash: Char;
begin
  if (aDest = '') then Exit;

  RemoveCmd := '';
  RemoveOptions := '';

  if roRecursive in Options then RemoveOptions := RemoveOptions + 'r';
  if roForce in Options then RemoveOptions := RemoveOptions + 'f';

  if RemoveOptions <> '' then RemoveCmd := ' -' + RemoveOptions
  else RemoveCmd := '';

  fpSystem('rm' + RemoveCmd + ' "' + aDest + '"');
end;

procedure CreateInfoPList(aDest: AnsiString);
var
  tmpStringList: TStringList;
  tmpIntel32: AnsiString;
  tmpIntel64: AnsiString;
  tmpPPC: AnsiString;
  tmpAvailPlatforms: AnsiString;
begin
  tmpStringList := TStringList.Create;

  tmpIntel64 := '';
  tmpIntel32 := '';
  tmpPPC := '';

  if TargetIntel64 <> -1 then
  begin
    tmpIntel64 := '        <key>x86_64</key>' + #13#10 +
 	  '	        <string>10.' + IntToStr(TargetIntel64) + '</string>' + #13#10;
  end;
  if TargetIntel32 <> -1 then
  begin
    tmpIntel32 := '        <key>i386</key>' + #13#10 +
 	  '	        <string>10.' + IntToStr(TargetIntel32) + '</string>' + #13#10;
  end;
  if TargetPPC <> -1 then
  begin
    tmpPPC := '        <key>ppc</key>' + #13#10 +
 	  '	        <string>10.' + IntToStr(TargetPPC) + '</string>' + #13#10;
  end;
  tmpAvailPlatforms := tmpIntel64 + tmpIntel32 + tmpPPC;

  tmpStringList.Text := '<?xml version=''1.0'' encoding=''UTF-8''?>' + #13#10 +
	'<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">' + #13#10 +
	'<plist version="1.0">' + #13#10 +
	'<dict>' + #13#10 +
	'        <key>CFBundleDevelopmentRegion</key>' + #13#10 +
	'        <string>' + BUNDLE_REGION + '</string>' + #13#10 +
	'        <key>CFBundleExecutable</key>' + #13#10 +
	'        <string>' + EXEC_NAME + '</string>' + #13#10 +
	'        <key>CFBundleIconFile</key>' + #13#10 +
	'        <string>' + BUNDLE_ICON + '</string>' + #13#10 +
	'        <key>CFBundleIdentifier</key>' + #13#10 +
	'        <string>' + BUNDLE_IDENT + '</string>' + #13#10 +
	'        <key>CFBundleInfoDictionaryVersion</key>' + #13#10 +
	'        <string>6.0</string>' + #13#10 +
	'        <key>CFBundleName</key>' + #13#10 +
	'        <string>' + APP_NAME + '</string>' + #13#10 +
	'        <key>CFBundlePackageType</key>' + #13#10 +
	'        <string>APPL</string>' + #13#10 +
	'        <key>CFBundleSignature</key>' + #13#10 +
	'        <string>' + BUNDLE_SIGNATURE + '</string>' + #13#10 +
	'        <key>CFBundleVersion</key>' + #13#10 +
	'        <string>' + BUNDLE_VERSION + '</string>' + #13#10 +
	'        <key>CSResourcesFileMapped</key>' + #13#10 +
	'        <true/>' + #13#10 +
  '          <key>LSMinimumSystemVersionByArchitecture</key>' + #13#10 +
 	'        <dict>' + #13#10 + tmpAvailPlatforms +
 	'        </dict>' + #13#10 +
	'</dict>' + #13#10 +
	'</plist>';
  tmpStringList.SaveToFile(aDest);

  tmpStringList.Free;
end;

procedure CreatePkgInfo(aDest: AnsiString);
var
  tmpStringList: TStringList;
begin
  tmpStringList := TStringList.Create;

  tmpStringList.Text := 'APPL' + BUNDLE_SIGNATURE;

  tmpStringList.SaveToFile(aDest);

  tmpStringList.Free;
end;

procedure CopyFrameworks();
begin
  if SDL_FrameworkPath <> '' then CpShell(SDL_FrameworkPath, AppBundlePath + '/Contents/Frameworks/');
  if SDL_Mixer_FrameworkPath <> '' then CpShell(SDL_Mixer_FrameworkPath, AppBundlePath + '/Contents/Frameworks/');
  if SDL_Net_FrameworkPath <> '' then CpShell(SDL_Net_FrameworkPath, AppBundlePath + '/Contents/Frameworks/');
  if SDL_TTF_FrameworkPath <> '' then CpShell(SDL_TTF_FrameworkPath, AppBundlePath + '/Contents/Frameworks/');
end;


begin
  if CmdToString('which fpc').Text = '' then
  begin
    WriteLn('FreePascal compiler not found.');
    Exit;
  end;

  if not DirectoryExists('/Developer/SDKs') then
  begin
    WriteLn('/Developer dirctory not found. Please install Xcode');
  end;

  if DirectoryExists('/Library/Frameworks/SDL.framework') then SDL_FrameworkPath := '/Library/Frameworks/SDL.framework'
    else if DirectoryExists(ExpandFileName('~/Library/Frameworks/SDL.framework')) then SDL_FrameworkPath := '~/Library/Frameworks/SDL.framework'
      else begin
        WriteLn('SDL framework not found. Exiting. Please check: https://github.com/freezedev/elysion/wiki/Setting-up-our-development-environment');
        Exit;
      end;

  if DirectoryExists('/Library/Frameworks/SDL_mixer.framework') then SDL_Mixer_FrameworkPath := '/Library/Frameworks/SDL_mixer.framework'
    else if DirectoryExists(ExpandFileName('~/Library/Frameworks/SDL_mixer.framework')) then SDL_Mixer_FrameworkPath := '~/Library/Frameworks/SDL_mixer.framework';

  if DirectoryExists('/Library/Frameworks/SDL_net.framework') then SDL_Net_FrameworkPath := '/Library/Frameworks/SDL_net.framework'
    else if DirectoryExists(ExpandFileName('~/Library/Frameworks/SDL_net.framework')) then SDL_Net_FrameworkPath := '~/Library/Frameworks/SDL_net.framework';

  if DirectoryExists('/Library/Frameworks/SDL_ttf.framework') then SDL_TTF_FrameworkPath := '/Library/Frameworks/SDL_ttf.framework'
    else if DirectoryExists(ExpandFileName('~/Library/Frameworks/SDL_ttf.framework')) then SDL_TTF_FrameworkPath := '~/Library/Frameworks/SDL_ttf.framework';


  AppBundlePath := BIN_FOLDER + '/' + APP_NAME + '.app';
  MainExecName := StringWithoutSuffix(SRC_MAIN, '.lpr');

  case GetMacOSMajorVersion of
    4, 5:
    begin
      TargetIntel32 := 4;
      TargetIntel64 := -1;
      TargetPPC := 4;
    end;
    6:
    begin
      TargetIntel32 := 5;
      TargetIntel64 := 6;
      TargetPPC := 5;
    end;
    7:
    begin
      TargetIntel32 := 5;
      TargetIntel64 := 6;
      TargetPPC := -1;
    end;
  end;

  WriteLn('Compiling release build');

  // Switch working directory
  SetCurrentDir(SRC_FOLDER);

  if TargetIntel32 <> -1 then
  begin
    if fpSystem('`which ppc386`' + ' @' + CONFIG_FILE + COMPILER_OPTIONS + ' -k"-L' + LIB_FOLDER + '/MacOSX -L/usr/X11R6/lib -macosx_version_min 10.' + IntToStr(TargetIntel32) + '" ' + SRC_MAIN) = 127 then
    begin
      WriteLn('Could not build 32-bit executable. Exiting.');
      Exit;
    end;

    MvShell(BIN_FOLDER + '/' + MainExecName, BIN_FOLDER + '/' + MainExecName + '-intel_x86');
    RmShell(BIN_FOLDER + '/*.o');
    RmShell(BIN_FOLDER + '/*.ppu');
  end;

  if TargetIntel64 <> -1 then
  begin
    if fpSystem('`which ppcx64`' + ' @' + CONFIG_FILE + COMPILER_OPTIONS + ' -k"-L' + LIB_FOLDER + '/MacOSX -L/usr/X11R6/lib -macosx_version_min 10.' + IntToStr(TargetIntel64) + '" ' + SRC_MAIN) = 127 then
    begin
      WriteLn('Could not build 64-bit executable. Exiting.');
      Exit;
    end;

    MvShell(BIN_FOLDER + '/' + MainExecName, BIN_FOLDER + '/' + MainExecName + '-intel_x64');
    RmShell(BIN_FOLDER + '/*.o');
    RmShell(BIN_FOLDER + '/*.ppu');
  end;

  if TargetPPC <> -1 then
  begin
    if fpSystem('`which ppcppc`' + ' @' + CONFIG_FILE + COMPILER_OPTIONS + ' -k"-L' + LIB_FOLDER + '/MacOSX -L/usr/X11R6/lib -macosx_version_min 10.' + IntToStr(TargetPPC) + '" ' + SRC_MAIN) = 127 then
    begin
      WriteLn('Could not build PowerPC executable. Exiting.');
      Exit;
    end;

    MvShell(BIN_FOLDER + '/' + MainExecName, BIN_FOLDER + '/' + MainExecName + '-ppc');
    RmShell(BIN_FOLDER + '/*.o');
    RmShell(BIN_FOLDER + '/*.ppu');
  end;

  if FileExists(BIN_FOLDER + '/' + MainExecName + '-intel_x86') then fpSystem('strip ' + BIN_FOLDER + '/' + MainExecName + '-intel_x86');
  if FileExists(BIN_FOLDER + '/' + MainExecName + '-intel_x64') then fpSystem('strip ' + BIN_FOLDER + '/' + MainExecName + '-intel_x64');
  if FileExists(BIN_FOLDER + '/' + MainExecName + '-ppc') then fpSystem('strip ' + BIN_FOLDER + '/' + MainExecName + '-ppc');

  if FileExists(BIN_FOLDER + '/' + MainExecName + '-intel_x86') and FileExists(BIN_FOLDER + '/' + MainExecName + '-intel_x64') and FileExists(BIN_FOLDER + '/' + MainExecName + '-ppc') then
  begin
    fpSystem('lipo -create "' + BIN_FOLDER + '/' + MainExecName + '-intel_x86" "' + BIN_FOLDER + '/' + MainExecName + '-intel_x64" "' + BIN_FOLDER + '/' + MainExecName + '-ppc" -output "' + BIN_FOLDER + '/' + MainExecName + '"');
    RmShell(BIN_FOLDER + '/' + MainExecName + '-intel_x86');
    RmShell(BIN_FOLDER + '/' + MainExecName + '-intel_x64');
    RmShell(BIN_FOLDER + '/' + MainExecName + '-ppc');
  end else
  begin
    // Intel 32 + PowerPC
    if FileExists(BIN_FOLDER + '/' + MainExecName + '-intel_x86') and FileExists(BIN_FOLDER + '/' + MainExecName + '-ppc') then
    begin
      fpSystem('lipo -create "' + BIN_FOLDER + '/' + MainExecName + '-intel_x86" "' + BIN_FOLDER + '/' + MainExecName + '-ppc" -output "' + BIN_FOLDER + '/' + MainExecName + '"');
      RmShell(BIN_FOLDER + '/' + MainExecName + '-intel_x86');
      RmShell(BIN_FOLDER + '/' + MainExecName + '-ppc');
    end;

    // Intel 32 + Intel 64
    if FileExists(BIN_FOLDER + '/' + MainExecName + '-intel_x86') and FileExists(BIN_FOLDER + '/' + MainExecName + '-intel_x64') then
    begin
      fpSystem('lipo -create "' + BIN_FOLDER + '/' + MainExecName + '-intel_x86" "' + BIN_FOLDER + '/' + MainExecName + '-intel_x64" -output "' + BIN_FOLDER + '/' + MainExecName + '"');
      RmShell(BIN_FOLDER + '/' + MainExecName + '-intel_x86');
      RmShell(BIN_FOLDER + '/' + MainExecName + '-intel_x64');
    end;
  end;


  // Create app bundle
  if DirectoryExists(AppBundlePath) then
  begin
    WriteLn('Removing old app bundle...');
    RmShell(AppBundlePath, [roRecursive]);
  end;

  WriteLn('Creating app bundle');
  MkDir(AppBundlePath);
  MkDir(AppBundlePath + '/Contents');
  MkDir(AppBundlePath + '/Contents/MacOS');
  MkDir(AppBundlePath + '/Contents/Resources');
  MkDir(AppBundlePath + '/Contents/Frameworks');

  MvShell(BIN_FOLDER + '/' + MainExecName, AppBundlePath + '/Contents/MacOS/' + EXEC_NAME);

  CopyFrameworks;

  // Copy resources directory
  WriteLn('Copying resources directory');
  CpShell(RES_FOLDER + '/', AppBundlePath + '/Contents/Resources/');

  //WriteLn(fpSystem('fpc -Mdelphi build-mac.pas'));
  CreateInfoPList(AppBundlePath + '/Contents/Info.plist');
  CreatePkgInfo(AppBundlePath + '/Contents/PkgInfo');
end.
