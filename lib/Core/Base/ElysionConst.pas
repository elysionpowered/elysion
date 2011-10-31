unit ElysionConst;

interface

const
  // Version information
  VER_MAJOR = 11;
  VER_MINOR = 06;
  VER_REVISION = 'a';

  VER_CODENAME = 'Echo'; //< Codename
  VER_STABLE = true;

  // Other random stuff mostly regarding version information
  VER_CODENAME_UNIVERSE = 'Dollhouse'; //< From which universe the codename comes from
  VER_CODENAME_RANDOMQUOTE = 'Did I fall asleep?'; //< Random quote from that universe

  // So, in case you didn't get this: Each codename comes from a movie or TV show

  {$IFDEF FPC}
	  // Usage of FreePascal is recommanded
	  {$IFDEF UNIX}
	    {$IFDEF DARWIN}
              {$IFDEF IPHONE}
                      SYS_NAME = 'iPhone-MacOS';
              {$ELSE}
                      SYS_NAME = 'Mac OS X';
              {$ENDIF}
	        {$ELSE}
		      {$IFDEF GP2X}
		        SYS_NAME = 'GP2X-Linux';
		      {$ELSE}
		        SYS_NAME = 'Linux';
		      {$ENDIF}
	        {$ENDIF}
	      {$ELSE}
	        SYS_NAME = 'Windows';
	      {$ENDIF}

	      {$IFDEF CPU64}
	        SYS_BITS = 64;
	      {$ELSE}
	        SYS_BITS = 32;
	      {$ENDIF}
          {$ELSE}
// Assume Delphi
	  SYS_NAME = 'Windows';
	  SYS_BITS = 32;
  {$ENDIF}

implementation

end.
