unit OpenURLUtil;

{$I Elysion.inc}
 
interface 

{$IFDEF FPC}
  {$mode objfpc}{$h+}
{$ENDIF}
 

{$ifdef Windows}
uses Windows, ShellAPI, SysUtils;
{$endif}
{$ifdef LINUX}
uses SysUtils, Unix;
{$endif}
{$ifdef Darwin} // LCLCarbon?
uses MacOSAll;
{$endif}
 
function OpenURLWide(const URLWide: WideString): Boolean;
function OpenURLAnsi(const URLAnsi: AnsiString): Boolean;
function OpenURL(const URLUtf8: String): Boolean;
 
implementation
 
function OpenURLWide(const URLWide: WideString): Boolean;
begin 
  Result := OpenURL(UTF8Encode(URLWide));
end;
 
function OpenURLAnsi(const URLAnsi: AnsiString): Boolean;
begin
  Result := OpenURL(AnsiToUtf8(URLAnsi));
end;
 
{$IFDEF WINDOWS}
function OpenURL(const URLUtf8: String): Boolean;
var
{$IFDEF WinCE}
  Info: SHELLEXECUTEINFO;
{$ELSE}
  ws    : WideString;
  ans   : AnsiString;
{$ENDIF}
begin
  Result := false;
  if URLUtf8 = '' then Exit;

  {$IFDEF WinCE}
  FillChar(Info, SizeOf(Info), 0);
  Info.cbSize := SizeOf(Info);
  Info.fMask := SEE_MASK_FLAG_NO_UI;
  Info.lpVerb := 'open';
  Info.lpFile := PWideChar(UTF8Decode(AURL));
  Result := ShellExecuteEx(@Info);
  {$ELSE}
  if Win32Platform = VER_PLATFORM_WIN32_NT then begin
    ws := UTF8Decode(URLUtf8);
    Result := ShellExecuteW(0, 'open', PWideChar(ws), nil, nil, 0) > 32;
  end else begin
    ans := Utf8ToAnsi(URLUtf8); // utf8 must be converted to Windows Ansi-codepage
    Result := ShellExecute(0, 'open', PAnsiChar(ans), nil, nil, 0) > 32;
  end;
  {$ENDIF}
end;
{$ENDIF}
 
{$IFDEF LINUX}
function OpenURL(const URLUtf8: string): Boolean;
var
  Helper: string;
begin
  Result := True;
  
  try
    Helper := '';
    if fpSystem('which xdg-open') = 0 then
      Helper := 'xdg-open'
    else if FileExists('/etc/alternatives/x-www-browser') then
      Helper := '/etc/alternatives/x-www-browser'
    else if fpSystem('which firefox') = 0 then
      Helper := 'firefox'
    else if fpSystem('which konqueror') = 0 then
      Helper := 'konqueror'
    else if fpSystem('which opera') = 0 then
      Helper := 'opera'
    else if fpSystem('which mozilla') = 0 then
       Helper := 'mozilla';
 
    if Helper <> '' then
      fpSystem(Helper + ' ' + URLUtf8 + '&')
    else
      Result := False;
      
  except
  end;

end;
{$ENDIF}
 
{$IFDEF DARWIN}
function OpenURL(const URLUtf8: string): Boolean;
var
  cf  : CFStringRef;
  url : CFURLRef;
begin
  if URLUtf8 = '' then begin
    Result := false;
    Exit;
  end;

  cf := CFStringCreateWithCString(kCFAllocatorDefault, @URLUtf8[1], kCFStringEncodingUTF8);
  if not Assigned(cf) then
    Exit(False);
  url := CFURLCreateWithString(nil, cf, nil);
  Result := LSOpenCFURLRef(url, nil) = 0;
  CFRelease(url);
  CFRelease(cf);
end;
{$ENDIF}
 
end.
