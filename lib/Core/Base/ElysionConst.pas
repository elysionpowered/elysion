unit ElysionConst;

interface

uses
  SysUtils;

const
  // Version information
  ELYSION_VER_MAJOR = 1;
  ELYSION_VER_MINOR = 5;
  ELYSION_VER_BUILD = 0;
  ELYSION_VER_REVISION = 0;

  ELYSION_VER_CODENAME = 'Boomer'; //< Codename

  {$IFDEF FPC}
  SYSTEM_CPU = {$I %FPCTARGETCPU%};
  SYSTEM_OS = {$I %FPCTARGETOS%};
  {$ELSE}
    // TODO: Add conditions for Delphi
    SYSTEM_CPU = 'Not really sure';
    SYSTEM_OS = 'Probably Windows';
  {$ENDIF}

  {$IFDEF WINDOWS}
  CSIDL_PERSONAL = $0005; //< My Documents (Win 95+)
  CSIDL_LOCAL_APPDATA = $001c; //< Local Appdata (Windows 2000+)
  CSIDL_APPDATA = $001a; //< Win 95+ with IE4.0 Shell installed
  {$ENDIF}

  {$IFNDEF FPC}
    // Assume Delphi
    {$IFDEF WINDOWS}
    DirectorySeparator = '\';
    {$ELSE}
    DirectorySeparator = '/';
    {$ENDIF}
  {$ENDIF}


implementation



end.
