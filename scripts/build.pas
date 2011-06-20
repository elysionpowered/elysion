#!/usr/bin/env instantfpc

{$mode delphi}

uses
  Classes, SysUtils, INIFiles;

type
  TAppParam = (apUniversal, apMacAppStore, apCurArchOnly, apCustomBundle, apDebug, apRelease, apHelp, apVerbose);
  TAppParams = set of TAppParam;

function GetParam(): TAppParams;
var
  i: Integer;
  tmpResult: TAppParams;

begin
  if ParamCount = 0 then tmpResult := [apHelp]
  else begin
    // Get params
    for i := 0 to ParamCount do
    begin
      if ((ParamStr(i) = 'debug') or (ParamStr(i) = 'dbg') or (ParamStr(i) = '-debug') or (ParamStr(i) = '--debug') or (ParamStr(i) = '-d')) then tmpResult := tmpResult + [apDebug];
      if ((ParamStr(i) = 'release') or (ParamStr(i) = 'rel') or (ParamStr(i) = '-release') or (ParamStr(i) = '--release') or (ParamStr(i) = '-r')) then tmpResult := tmpResult + [apRelease];
      if ((ParamStr(i) = 'universal') or (ParamStr(i) = '-universal') or (ParamStr(i) = '--universal')) then tmpResult := tmpResult + [apUniversal];
      if ((ParamStr(i) = 'macappstore') or (ParamStr(i) = '-macappstore') or (ParamStr(i) = '--macappstore')) then tmpResult := tmpResult + [apMacAppStore];
      if ((ParamStr(i) = 'curarch') or (ParamStr(i) = '-curarch') or (ParamStr(i) = '--curarch')) then tmpResult := tmpResult + [apCurArchOnly];
      if ((ParamStr(i) = 'custombundle') or (ParamStr(i) = 'cbundle') or (ParamStr(i) = '-custombundle') or (ParamStr(i) = '--custombundle')) then tmpResult := tmpResult + [apCustomBundle];

      if ((ParamStr(i) = 'help') or (ParamStr(i) = '-help') or (ParamStr(i) = '--help') or (ParamStr(i) = '-h')) then tmpResult := tmpResult + [apHelp];
      if ((ParamStr(i) = 'verbose') or (ParamStr(i) = '-verbose') or (ParamStr(i) = '--verbose') or (ParamStr(i) = '-v')) then tmpResult := tmpResult + [apVerbose];
    end;



    // Trim
    if ((apDebug in tmpResult) and (apRelease in tmpResult)) then tmpResult := tmpResult - [apDebug];
    if ((apUniversal in tmpResult) and (apCurArchOnly in tmpResult)) then tmpResult := tmpResult - [apCurArchOnly];

    // Add debug or release flag
    if ((not (apDebug in tmpResult)) and (not (apRelease in tmpResult))) then tmpResult := tmpResult + [apDebug];
  end;

  Result := tmpResult;
end;



var
  appParams: TAppParams;
  curBuildStep, maxBuildStep: Integer;

procedure ShowConfig();
begin
  WriteLn('Building application');
  WriteLn('Using configuration: ');
  if apDebug in appParams then WriteLn('  Debug');
  if apRelease in appParams then WriteLn('  Release');
  {$IFDEF DARWIN}
  if apUniversal in appParams then WriteLn('  Universal Binary');
  if apMacAppStore in appParams then WriteLn('  Mac App Store');
  if apCurArchOnly in appParams then WriteLn('  Only current architecture');
  if apCustomBundle in appParams then WriteLn('  Custom application bundle');
  {$ENDIF}
end;

begin
  appParams := GetParam();

  if apHelp in appParams then
  begin
    WriteLn('Command Line parameters:');
    WriteLn('   debug, dbg, -debug, --debug or -d: Debug flag');
    WriteLn('   release, rel, -release, --release or -r: Release flag');
    WriteLn('');
    WriteLn('   universal, -universal, --universal: Tries to build a universal binary - Mac OS X only');
    WriteLn('   macappstore, -macappstore, --macappstore: Builds a Mac App Store compatible application - Mac OS X only');
    WriteLn('   curarch, -curarch, --curarch: Builds only for current architecture (i386) - Mac OS X only');
    WriteLn('   custombundle, cbundle, -custombundle, --custombundle: Allows you to enter bundle information before building - Mac OS X only');
    WriteLn('');
    WriteLn('   help -help, --help or -h: Shows this help screen');
    WriteLn('   verbose, -verbose, --verbose or -v: Prints all information during building');


    Exit();
  end;

  ShowConfig();



  {$IFDEF UNIX}
    {$IFDEF DARWIN}

    {$ELSE}

    {$ENDIF}
  {$ELSE}
  // Windows
  {$ENDIF}
end.
