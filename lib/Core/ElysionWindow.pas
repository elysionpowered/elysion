unit ElysionWindow;

{$I Elysion.inc}

interface

uses
  SDL,
  SDLUtilsLight,
  {$IFDEF USE_VAMPYRE}
  ImagingSDL,
  {$ENDIF}
  {$IFDEF USE_SDL_IMAGE}
  SDL_image,
  {$ENDIF}
  SDLTextures,
  {$IFDEF USE_DGL_HEADER}
  dglOpenGL,
  {$ELSE}
  gl, glu, glext,
  {$ENDIF}

  Classes,
  SysUtils,

  ElysionObject,
  ElysionList,
  ElysionStrings,
  ElysionContent,
  ElysionTypes,
  ElysionTimer,
  ElysionKeyCode,
  ElysionColor,
  ElysionEnums,
  ElysionLogger;

type

  { TelWindow }

  TelWindow = class(TelObject)
  private
    fJoystick: PSDL_Joystick;
    fActive: Boolean;

    FResolution: TelVector3i;
    // Native resolution
    FNativeResolution: TelVector2i;

    FBackgroundColor: TelColor;

    fCursor, fRelCursor, fJoyBall: TelVector2i;
    fResScale: TelVector2f;

    fMouseButtonDownEventList, fMouseButtonUpEventList: TList;

    // VideoFlags
    FVideoFlag: TelVideoFlag;

    FFullscreen: Boolean;
    FHideCursor: Boolean;
    fMouseMotionEvent: Boolean;

    fFlags, fGFlags: Cardinal;
    fFullscreenFlag: Cardinal;

    FFoVY, FZNear, FZFar: Double;


    FBackgroundChange: Boolean;

    fEvent: TSDL_Event;

    fCaption, fIcon: PChar;

    fFrames: Integer;

    fLimitFPS: Boolean;
    fFrameCap: Single;

    fFPSTimer, fDeltaTimer, fCursorReset: TelTimer;

    fMouseDownCount, fMouseUpCount, fMouseMotionCount: Integer;
    fProjection: TelProjectionMode;

    fDeltaTime: Double;

    // Handling key and mouse inputs
    procedure HandleEvents;

    function GetAspectRatio(): Single; inline;

    // Get the window title
    function GetCaption: AnsiString; inline;

    // Sets the window title
    procedure SetCaption(Caption: AnsiString); inline;

    procedure SetFPS(Value: Single); inline;
    function GetFPS: Single; inline;

    function GetDeltaTime: Double; inline;

    function GetWidth: Integer; inline;
    function GetHeight: Integer; inline;
    function GetBitsPerPixel: Integer; inline;

    function GetJoystickCount(): Integer; inline;

    procedure SetNativeResolution(NativeRes: TelVector2i); inline;
    function GetNativeResolution: TelVector2i; inline;

    function GetDisplayOrientation(): TRectOrientation; inline;

    function GetWideScreen(): Boolean; inline;

    // Set projection matrix
    // Expert function: Automatically Ortho mode is being called, this function needs to be used
    // if you want to display 3d objects on the screen, you need to call perspective mode
    procedure SetProjection(ProjectionMode: TelProjectionMode); inline;

  public

      // You can access the SDL surface directly if you wish
      SDL_Surface: PSDL_Surface;

      // Maybe fix: Move input arrays to Input class

      // True, if key is pressed down
      KeyDown: array[0..K_EURO] of Boolean;

      // True, if key is pressed up again
      KeyUp: array[0..K_EURO] of Boolean;

      // True, if key is pressed down
      ModKeyDown: array[KMOD_NONE..KMOD_META] of Boolean;

      // True, if key is pressed up again
      ModKeyUp: array[KMOD_NONE..KMOD_META] of Boolean;

      // True, if mouse button is pressed down
      MouseButtonDown: array[BUTTON_LEFT..BUTTON_WHEELDOWN] of Boolean;

      // True, if mouse button is pressed up again
      MouseButtonUp: array[BUTTON_LEFT..BUTTON_WHEELDOWN] of Boolean;

      // True, if mouse button is pressed down
      JoyButtonDown: array[0..MAX_BUTTONS] of Boolean;

      // True, if mouse button is pressed up again
      JoyButtonUp: array[0..MAX_BUTTONS] of Boolean;

      JoyAxis: array[0..MAX_AXES] of Integer;

      JoyHat: array[HAT_CENTERED..255] of Boolean;


      constructor Create; Override;
      destructor Destroy; Override;

      function SetVideoMode(_Width, _Height, _ColorBits: Integer; _Fullscreen: Boolean; _VideoFlag: TelVideoFlag): Boolean; Overload;

      // Sets video mode for the surface. Use this if you want to change the resolution in-game.
      function SetVideoMode(_Width, _Height, _ColorBits: Integer; _Fullscreen: Boolean = False): Boolean; Overload; inline;


      // Switches between windowed mode and fullscreen mode
      procedure ToggleFullscreen(); inline;

      // Use BeginScene first thing in the game loop
      procedure BeginScene(); inline;

      // Use EndScene last thing before the end of the game loop
      procedure EndScene(); inline;


      // Sets background color
      procedure SetBackgroundColor(Color: TelColor); inline;

      // Sets a window icon
      procedure SetIcon(Filename: AnsiString); Overload;

      // Sets a window icon with color key
      procedure SetIcon(Filename: AnsiString; Mask: TelColor); Overload;

      // Hide the standard black/white cursor. Normally turned on
      procedure HideCursor; inline;

      // Show the cursor
      procedure ShowCursor; inline;

      // Saves the surface into an image
      Procedure TakeScreenshot(aFilename: AnsiString = '');

      // Checks if a point is within a rect
      Function OnPoint(Coord: TelVector2i; Rect: TelRect): Boolean; inline;

      // Checks if surface is active
      //function IsActive: Boolean; inline;

      procedure Quit;

      function GetTicks(): Cardinal; inline;

      property BackgroundColor: TelColor read fBackgroundColor write fBackgroundColor;

      property JoyBall: TelVector2i read fJoyBall;

      property Cursor: TelVector2i read fCursor;
      property RelCursor: TelVector2i read fRelCursor;

      property ResScale: TelVector2f read fResScale write fResScale;
      property NativeResolution: TelVector2i read GetNativeResolution write SetNativeResolution;

  published
    property AspectRatio: Single read GetAspectRatio;

    property Active: Boolean read fActive;

    property MouseButtonDownEventList: TList read fMouseButtonDownEventList;
    property MouseButtonUpEventList: TList read fMouseButtonUpEventList;

    property DisplayOrientation: TRectOrientation read GetDisplayOrientation;
    property WideScreen: Boolean read GetWideScreen;

    // Read-only, set values through SetVideoMode
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Bits: Integer read GetBitsPerPixel;
    property Fullscreen: Boolean read FFullscreen;

    property MouseDownCount: Integer read fMouseDownCount write fMouseDownCount;
    property MouseUpCount: Integer read fMouseUpCount write fMouseUpCount;
    property MouseMotionCount: Integer read fMouseMotionCount write fMouseMotionCount;


    // Setting the caption
    property Caption: AnsiString read GetCaption write SetCaption;

    property DeltaTime: Double read GetDeltaTime;

    property JoystickCount: Integer read GetJoystickCount;


    property FPS: Single read GetFPS write SetFPS;
    property LimitFPS: Boolean read fLimitFPS write fLimitFPS;

    property Projection: TelProjectionMode read fProjection write SetProjection;

    // Values for experts
    property FoVY: Double read FFoVY write FFoVY;
    property ZNear: Double read FZNear write FZNear;
    property ZFar: Double read FZFar write FZFar;
  end;

  TelWindowList = TelObjectList<TelWindow>;




implementation

uses
  ElysionTextureManager;



{
  #############################################################################
  # TelWindow                                                                #
  #############################################################################

  Description:
    TelWindow provides a window to draw the graphical objects on.

  Additional Notes: -

}

constructor TelWindow.Create;
var
  i: Integer;
begin
  inherited Create;

  fActive := true;

  if (SDL_NumJoysticks() > 0) then
    fJoystick := SDL_JoystickOpen(0);


  fCursor.Clear;
  fRelCursor.Clear();
  fResolution.Clear;
  fNativeResolution.Clear;
  fResScale := makeV2f(1.0, 1.0);
  fBackgroundColor := makeCol(0, 0, 0, 255);

  fFrames := 0;
  fMouseMotionEvent := false;


  fFPSTimer := TelTimer.Create;
  fDeltaTimer := TelTimer.Create;
  fCursorReset := TelTimer.Create;
  fCursorReset.Interval := 250;

  FHideCursor := true;

  fLimitFPS := true;
  fFrameCap := 60.0;

  FFullscreenFlag := SDL_FULLSCREEN;

  FVideoFlag := vfNull;

  fCaption := nil;
  fIcon := nil;

  FBackgroundChange := False;
  FFoVY := 60.0;
  FZNear := 0.1;
  FZFar := 128.0;

  fMouseDownCount := 0;
  fMouseUpCount := 0;
  fMouseMotionCount := 0;

  fMouseButtonDownEventList := TList.Create;
  fMouseButtonUpEventList := TList.Create;

  // Reset event arrays
  for i := 0 to High(KeyUp) do
  begin
    KeyDown[i] := false;
    KeyUp[i] := false;
  end;

  for i := 0 to High(MouseButtonUp) do
  begin
    MouseButtonDown[i] := false;
    MouseButtonUp[i] := false;
  end;

  for i := KMOD_NONE to KMOD_META do
  begin
    ModKeyDown[i] := false;
    ModKeyUp[i] := false;
  end;

  for i := 0 to High(JoyButtonUp) do
  begin
    JoyButtonDown[i] := false;
    JoyButtonUp[i] := false;
  end;

  for i := 0 to High(JoyHat) do
  begin
    JoyHat[i] := false;
  end;


  // Probably not needed here...
  if FHideCursor then SDL_ShowCursor(0)
                 else SDL_ShowCursor(1);

end;

destructor TelWindow.Destroy;
begin
  if Self.SDL_Surface <> nil then
  begin
    if (SDL_JoystickOpened(0) = 1) then
      SDL_JoystickClose(fJoystick);

    if FFullscreen then ToggleFullscreen();
    SDL_FreeSurface(Self.SDL_Surface);
  End;

  inherited;
end;

function TelWindow.GetJoystickCount(): Integer;
begin
  Result := SDL_NumJoysticks();
end;

function TelWindow.GetDisplayOrientation(): TRectOrientation;
begin
  if (Trunc(Self.Width / Self.Height) = 0) then Result := roPortrait
  else Result := roLandscape;
end;

function TelWindow.GetWideScreen(): Boolean;
begin
  if Self.AspectRatio >= (16/10) then Result := true
  else Result := false;
end;

function TelWindow.SetVideoMode(_Width, _Height, _ColorBits: Integer; _Fullscreen: Boolean; _VideoFlag: TelVideoFlag): Boolean;
var
  Changed: Boolean;
  VideoInfo: PSDL_VideoInfo;
begin
  Result := false;

  VideoInfo := SDL_GetVideoInfo;

  if VideoInfo = nil then
  begin
    Destroy;
    Exit;
  end;

  if _VideoFlag = vfNull then FVideoFlag := vfAuto;

  if _Width <> FResolution.X then
  begin
    FResolution.X := _Width;
    Changed := true;
  end;
  if _Height <> FResolution.Y then
  begin
    FResolution.Y := _Height;
    Changed := true;
  end;
  if _ColorBits <> FResolution.Z then
  begin
    FResolution.Z := _ColorBits;
    Changed := true;
  end;
  if FVideoFlag <> _VideoFlag then
  begin
    FVideoFlag := _VideoFlag;
    Changed := true;

    if FVideoFlag = vfAuto then
    begin
      if VideoInfo^.hw_available <> 0  then
      begin
        FVideoFlag := vfHardware;
	TelLogger.GetInstance.WriteLog('Using hardware surface.', ltNote);
      end else
      begin
        FVideoFlag := vfSoftware;
	TelLogger.GetInstance.WriteLog('Using software surface.', ltNote);
      end;

      VideoInfo := nil;
    end;
  end;
  if FFullscreen <> _Fullscreen then
  begin
    FFullscreen := _Fullscreen;
    Changed := true;
  end;

  if Changed then
  begin
    SDL_putenv('SDL_VIDEO_CENTERED=center');

    FFlags := SDL_DOUBLEBUF or SDL_HWPALETTE;

    if FVideoFlag = vfHardware then FFlags := FFlags or SDL_HWSURFACE;
    if FVideoFlag = vfSoftware then FFlags := FFlags or SDL_SWSURFACE;

    FFlags := FFlags or SDL_OPENGL;

    if FFullscreen then
    begin
      FGFlags := FFlags or FFullscreenFlag;
      Self.SDL_Surface := SDL_SetVideoMode(FResolution.X, FResolution.Y, FResolution.Z, FGFlags);
    end
      else Self.SDL_Surface := SDL_SetVideoMode(FResolution.X, FResolution.Y, FResolution.Z, FFlags);

    //TelAppProvider.GraphicsProvider.InitVideoMode;

    // Reset attributes (http://sdl.beuc.net/sdl.wiki/SDL_SetVideoMode - User Note 1)
    SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 5);
    SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 5);
    SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 5);
    SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 16);
    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);

    SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, 8);


    glClearDepth(1.0);                                    // Depth Buffer Setup
    glEnable(GL_DEPTH_TEST);                        // Aktiviert Depth Testing
    glDepthFunc(GL_ALWAYS);                        // Bestimmt den Typ des Depth Testing
    glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);// Qualitativ bessere Koordinaten Interpolation
    glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
    glHint(GL_POINT_SMOOTH_HINT, GL_NICEST);

    // Some minor performance improvements on older machines
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);

    TelTextureManager.ReloadAllTextures();

    SetBackgroundColor(TelColor.clFreezeDevBlue);

    glViewport(0, 0, FResolution.X, FResolution.Y);
    //TelAppProvider.GraphicsProvider.SetViewport(makeSize(fResolution.X, fResolution.Y));
    SetProjection(pmOrtho);
    glTranslatef(0, 0, 0);
    //TelAppProvider.GraphicsProvider.TranslateToOrigin();

    if not Assigned(Self.SDL_Surface) then
    begin
      Result := false;
      TelLogger.GetInstance.WriteLog('Surface not assigned', ltError);
            Exit;
            Destroy;
    end else
	begin
	  Self.Caption := Self.Name;
	  Result := true;
	end;
    Changed := false;

    Self.NativeResolution := makeV2i(FResolution.X, FResolution.Y);
  end;



  if (not fFPSTimer.Active) then
  begin
    fFPSTimer.Interval := 1000;
    fFPSTimer.Start();
  end;

  if (not fDeltaTimer.Active) then
    fDeltaTimer.Start();
end;

function TelWindow.SetVideoMode(_Width, _Height, _ColorBits: Integer; _Fullscreen: Boolean = False): Boolean;
begin
  Result := SetVideoMode(_Width, _Height, _ColorBits, _Fullscreen, FVideoFlag);
end;

procedure TelWindow.SetNativeResolution(NativeRes: TelVector2i);
begin
  if (FNativeResolution.X <> NativeRes.X) then FNativeResolution.X := NativeRes.X;
  if (FNativeResolution.Y <> NativeRes.Y) then FNativeResolution.Y := NativeRes.Y;

  // Set resolution scale
  fResScale.X := FResolution.X / NativeRes.X;
  fResScale.Y := FResolution.Y / NativeRes.Y;


  TelLogger.GetInstance.WriteLog('Resolution: %d x %d pixels', [FResolution.X, FResolution.Y], ltNote);
  TelLogger.GetInstance.WriteLog('Native Resolution: %d x %d pixels', [FNativeResolution.X, FNativeResolution.Y], ltNote);
  TelLogger.GetInstance.WriteLog('Scale.X: %f  Scale.Y: %f', [ResScale.X, ResScale.Y], ltNote);
end;

function TelWindow.GetNativeResolution: TelVector2i;
begin
  Result := makeV2i(FNativeResolution.X, FNativeResolution.Y);
end;

procedure TelWindow.ToggleFullscreen();
begin
  FFullscreen := not FFullscreen;

  {$IFDEF LINUX}
  SDL_WM_ToggleFullScreen(Self.SDL_Surface);
  {$ELSE}
  SetVideoMode(FResolution.X, FResolution.Y, FResolution.Z, FFullscreen);
  {$ENDIF}
end;

procedure TelWindow.BeginScene();
begin
  if (fDeltaTimer.getTicks > 0) then fDeltaTime := (fDeltaTimer.getTicks / 1000);
  fDeltaTimer.Start();

  if FBackgroundChange then glClearColor(FBackgroundColor.R / 255, FBackgroundColor.G / 255, FBackgroundColor.B / 255, 1);

  // Clear screen
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  glLoadIdentity;

  // Set resolution
  glScalef(ResScale.X, ResScale.Y, 0.0);

  (*if FBackgroundChange then TelAppProvider.GraphicsProvider.SetBackgroundColor(fBackgroundColor);

  TelAppProvider.GraphicsProvider.OnBeginScene;*)
end;

procedure TelWindow.SetProjection(ProjectionMode: TelProjectionMode);
begin
  fProjection := ProjectionMode;

  glMatrixMode(GL_PROJECTION);

  glLoadIdentity;

  case ProjectionMode of
    pmOrtho: glOrtho(0, FResolution.X, FResolution.Y, 0, FZNear, FZFar);
    pmFrustum: gluPerspective(FFoVY, FResolution.X / FResolution.Y, FZNear, FZFar);
  end;

  glMatrixMode(GL_MODELVIEW);

  //TelAppProvider.GraphicsProvider.SetProjection(makeSize(fResolution.X, fResolution.Y), fProjection, FFoVY, FZNear, FZFar);
end;

procedure TelWindow.EndScene;
begin
  glFlush();

  //TelAppProvider.GraphicsProvider.OnEndScene;

  HandleEvents;

  SDL_GL_SwapBuffers();

  fFrames := fFrames + 1;

  if fLimitFPS then
  begin
    if ( fFPSTimer.GetTicks < (1000 / fFrameCap) ) then
    begin
      SDL_Delay( Abs(Trunc(1000 / fFrameCap) - fFPSTimer.GetTicks) ); // Abs - Dirty Hack!!! TODO: Find an even dirtier hack ;)
    end;
  end;

end;

procedure TelWindow.SetFPS(Value: Single);
begin
  if Value > 0 then fFrameCap := Value;
end;

function TelWindow.GetFPS: Single;
begin
  Result := fFrames * (1000 / fFPSTimer.GetTicks());

  if fFPSTimer.Event then
  begin
    fFrames := 0;
    fFPSTimer.Start();
  end;
end;

function TelWindow.GetDeltaTime: Double;
begin
  if fDeltaTime > 0 then Result := fDeltaTime
    else Result := 0.0001; // < Be careful - Might not get expected results all the time
end;

function TelWindow.GetWidth: Integer;
begin
  if (FNativeResolution.X = FResolution.X) then
    Result := FResolution.X
  else
    Result := FNativeResolution.X;
end;

function TelWindow.GetHeight: Integer;
begin
  if (FNativeResolution.Y = FResolution.Y) then
    Result := FResolution.Y
  else
    Result := FNativeResolution.Y;
end;

function TelWindow.GetBitsPerPixel: Integer;
begin
  Result := FResolution.Z;
end;

function TelWindow.GetAspectRatio(): Single;
begin
  Result := (Width / Height);
end;

procedure TelWindow.SetBackgroundColor(Color: TelColor);
begin
  if fBackgroundColor <> Color then
  begin
    FBackgroundChange := true;
    FBackgroundColor := Color;
  end;
end;

function TelWindow.GetCaption: AnsiString;
begin
  Result := AnsiString(fCaption);
end;

procedure TelWindow.SetCaption(Caption: AnsiString);
begin
  fCaption := PAnsiChar(Caption);
  SDL_WM_SetCaption(fCaption, fCaption);
end;

procedure TelWindow.SetIcon(Filename: AnsiString);
var TMP_Icon: PSDL_Surface;
    Directory: AnsiString;
begin
  if Filename <> '' then
  begin
    Directory := ExtractFilePath(ParamStr(0));

    if FileExists(Directory + Filename) then
    begin
      {$IFDEF USE_SDL_IMAGE}
        TMP_Icon := IMG_Load(PAnsiChar(Directory + Filename));
      {$ENDIF}
      {$IFDEF USE_VAMPYRE}
         TMP_Icon := LoadSDLSurfaceFromFile(Directory + Content.Root + FileName);
      {$ENDIF}
      FIcon := PAnsiChar(Filename);
      SDL_WM_SetIcon(TMP_Icon, 0);
    end
      else TelLogger.GetInstance.WriteLog('File not found: ' + Directory + Content.Root + FileName, ltError);
  end else TelLogger.GetInstance.WriteLog('No filename specified.', ltError);

  if TMP_Icon <> nil then SDL_FreeSurface(TMP_Icon);
end;

procedure TelWindow.SetIcon(Filename: AnsiString; Mask: TelColor);
var TMP_Icon: PSDL_Surface;
    Directory: AnsiString;
begin
  if Filename <> '' then
  begin
    Directory := ExtractFilePath(ParamStr(0));

    if FileExists(Directory + Filename) then
    begin
      {$IFDEF USE_SDL_IMAGE}
        TMP_Icon := IMG_Load(PAnsiChar(Directory + Content.Root + Filename));
      {$ENDIF}
      {$IFDEF USE_VAMPYRE}
        TMP_Icon := LoadSDLSurfaceFromFile(Directory + Content.Root + FileName);
      {$ENDIF}
      if TMP_Icon <> Nil then
        SDL_SetColorKey(TMP_Icon, SDL_SRCCOLORKEY or SDL_RLEACCEL or SDL_HWACCEL,
      SDL_MapRGB(TMP_Icon^.Format, Mask.R, Mask.G, Mask.B));

      FIcon := PAnsiChar(Filename);
      SDL_WM_SetIcon(TMP_Icon, 0);
    end else TelLogger.GetInstance.WriteLog(rsFileNotFound, [Directory + Content.Root + FileName], ltError);
  end else TelLogger.GetInstance.WriteLog(rsFileNotSpecified, ltError);

  if TMP_Icon <> nil then SDL_FreeSurface(TMP_Icon);
end;

procedure TelWindow.HideCursor;
begin
  FHideCursor := true;
  SDL_ShowCursor(0);
end;

procedure TelWindow.ShowCursor;
begin
  FHideCursor := false;
  SDL_ShowCursor(1);
end;

procedure TelWindow.TakeScreenshot(aFilename: AnsiString = '');
var
  tmpSurface: PSDL_Surface;
  rmask, gmask, bmask, amask: Uint32;
  tmpScreenshotCount: Integer;
  tmpFilename, formattedDateTime: AnsiString;
  searchResult: TSearchRec;
  ScreenRect: TSDL_Rect;
begin
  if aFilename = '' then aFilename := ParamStr(0);
  DateTimeToString(formattedDateTime, 'yyyy-mm-dd_hh-nn-ss', Now);

  tmpScreenshotCount := 1;
  tmpFilename := aFilename + '_' + formattedDateTime;

  // If someone is spamming the screenshot button key it should not overwrite older files
  if FindFirst(aFilename + '_' + formattedDateTime + '*.png', faAnyFile, searchResult) = 0 then
  begin
    repeat
      tmpScreenshotCount := tmpScreenshotCount + 1;
    until FindNext(searchResult) <> 0;

    // Must free up resources used by these successful finds
    FindClose(searchResult);

    if tmpScreenshotCount >= 2 then
      tmpFilename := aFilename + '_' + formattedDateTime + ' ' + IntToStr(tmpScreenshotCount);
  end;

  if UpperCase(ExtractFileExt(tmpFilename)) <> '.PNG' then
  begin
    tmpFilename := tmpFilename + '.png';
  end;

  if SDL_BYTEORDER = SDL_BIG_ENDIAN then
  begin
    rmask:=$ff000000;
    gmask:=$00ff0000;
    bmask:=$0000ff00;
    //amask:=$000000ff;
    amask := 0;
  end else
  begin
    rmask:=$000000ff;
    gmask:=$0000ff00;
    bmask:=$00ff0000;
    //amask:=$ff000000;
    amask := 0;
  end;

  tmpSurface := SDL_CreateRGBSurface(SDL_SWSURFACE, Self.Width, Self.Height, 24, rmask, gmask, bmask, amask);

  if not Assigned(tmpSurface) then Exit;

  SDL_LockSurface(tmpSurface);

  //glReadPixels(0, 0, Self.Width, Self.Height, GL_BGR, GL_UNSIGNED_BYTE, tmpSurface^.pixels);

  // Saves OpenGL context into tmpSurface
  //tmpSurface^.pixels := TelAppProvider.GraphicsProvider.ReadPixels(makeRect(0, 0, Self.Width, Self.Height));
  glReadPixels(0, 0, Self.Width, Self.Height, GL_RGB, GL_UNSIGNED_BYTE, tmpSurface^.pixels);

  ScreenRect.x := 0;
  ScreenRect.y := 0;
  ScreenRect.w := Self.Width;
  ScreenRect.h := Self.Height;

  // By default Elysion's origin is at the top-left corner which is not the
  // default setting of OpenGL (default setting of OpenGL: bottom-left)
  // So we need to flip the image (That's all that's happenin' here)
  SDL_FlipRectV(tmpSurface, @ScreenRect);

  SDL_UnlockSurface(tmpSurface);

  // Saves image to file
  {$IFDEF USE_VAMPYRE}
  ImagingSDL.SaveSDLSurfaceToFile(tmpFilename, tmpSurface);
  {$ELSE}
  SDL_SaveBMP(tmpSurface, PAnsiChar(tmpFile));
  {$ENDIF}

  SDL_FreeSurface(tmpSurface);
end;

function TelWindow.OnPoint(Coord: TelVector2i; Rect: TelRect): Boolean;
begin
  if (Coord.X >= Rect.X) and
     (Coord.X >= Rect.Y) and
     (Coord.X < (Rect.X + Rect.W)) and
     (Coord.Y < (Rect.Y + Rect.H)) then Result := true else Result := false;
end;

procedure TelWindow.HandleEvents;
var
  i, mouseX, mouseY: Integer;
  tempEvent: PelButtonEvent;
begin
  fMouseMotionEvent := false;
  mouseX := 0;
  mouseY := 0;

  for i := 0 to High(KeyUp) do
  begin
    if KeyUp[i] then KeyUp[i] := false;
  end;

  for i := KMOD_NONE to KMOD_META do
  begin
    if ModKeyUp[i] then ModKeyUp[i] := false;
  end;

  for i := 0 to High(MouseButtonUp) do
  begin
    if MouseButtonUp[i] then MouseButtonUp[i] := false;
  end;

  for i := 0 to High(JoyButtonUp) do
  begin
    if JoyButtonUp[i] then JoyButtonUp[i] := false;
  end;

  while (SDL_PollEvent(@FEvent) = 1) do
  begin

    case FEvent.type_ of
    SDL_QUITEV:
       begin
         if (Self.SDL_Surface <> nil) then Self.Quit;
       end;

    SDL_KEYUP:
        begin
          KeyDown[FEvent.key.keysym.sym] := false;
          KeyUp[FEvent.key.keysym.sym] := true;

          ModKeyDown[FEvent.key.keysym.modifier] := false;
          ModKeyUp[FEvent.key.keysym.modifier] := true;
        End;

    SDL_KEYDOWN:
        begin
          KeyDown[FEvent.key.keysym.sym] := true;
          KeyUp[FEvent.key.keysym.sym] := false;

          ModKeyDown[FEvent.key.keysym.modifier] := true;
          ModKeyUp[FEvent.key.keysym.modifier] := false;
        end;

    SDL_MOUSEMOTION:
        begin
          fMouseMotionEvent := true;

          fMouseMotionCount := fMouseMotionCount + 1;

          fCursor.X := FEvent.motion.x;
          fCursor.Y := FEvent.motion.y;

          fRelCursor.X := FEvent.motion.xrel;
          fRelCursor.Y := FEvent.motion.yrel;
        end;

    SDL_MOUSEBUTTONDOWN:
        begin
          MouseButtonDown[FEvent.button.button] := true;
          MouseButtonUp[FEvent.button.button] := false;

          fMouseDownCount := fMouseDownCount + 1;

          SDL_GetMouseState(mouseX, mouseY);

          new(tempEvent);

          tempEvent^.Position := makeV2i(mouseX, mouseY);
          tempEvent^.Called := SDL_GetTicks();

          fMouseButtonDownEventList.Add(tempEvent);
        end;

    SDL_MOUSEBUTTONUP:
        begin
          MouseButtonDown[FEvent.button.button] := false;
          MouseButtonUp[FEvent.button.button] := true;

          fMouseUpCount := fMouseUpCount + 1;

          SDL_GetMouseState(mouseX, mouseY);

          new(tempEvent);

          tempEvent^.Position := makeV2i(mouseX, mouseY);
          tempEvent^.Called := SDL_GetTicks();

          fMouseButtonUpEventList.Add(tempEvent);
        end;

    SDL_JOYBUTTONDOWN:
        begin
          JoyButtonDown[FEvent.jbutton.button] := true;
          JoyButtonUp[FEvent.jbutton.button] := false;
        end;

    SDL_JOYBUTTONUP:
        begin
          JoyButtonDown[FEvent.jbutton.button] := false;
          JoyButtonUp[FEvent.jbutton.button] := true;
        end;

    SDL_JOYAXISMOTION:
        begin
          if FEvent.jaxis.which = 0 then
            JoyAxis[FEvent.jaxis.axis] := FEvent.jaxis.value;
        end;

    SDL_JOYBALLMOTION:
        begin
          if FEvent.jaxis.which = 0 then
          begin
            fJoyBall.X := FEvent.jball.xrel;
            fJoyBall.Y := FEvent.jball.yrel;
          end;
        end;

    SDL_JOYHATMOTION:
        begin
          if FEvent.jaxis.which = 0 then
            JoyHat[FEvent.jhat.hat] := true;
        end;

    end;

  end;

  if fMouseMotionEvent then
  begin
    fMouseMotionEvent := false;
    fCursorReset.Start();
  end;

  if fCursorReset.Event then
  begin
    if (fRelCursor.X <> 0) then fRelCursor.X := 0;
    if (fRelCursor.Y <> 0) then fRelCursor.Y := 0;
    fCursorReset.Stop();
  end;
end;

procedure TelWindow.Quit;
begin
  fActive := false;
  //Result := ((SDL_GetAppState = SDL_APPACTIVE) or (SDL_GetAppState = SDL_APPMOUSEFOCUS) or (SDL_GetAppState = SDL_APPINPUTFOCUS));
end;

function TelWindow.GetTicks(): Cardinal;
begin
  Result := SDL_GetTicks();
end;



end.
