(**
  * ElysionApplication.pas
  *
  * Handles application container and windows
  *
  * @author(Johannes Stein and contributors <http://elysionpowered.org>)
  *
  *)
unit ElysionWindow;

{$I Elysion.inc}

interface

uses
  ElysionEnums,
  ElysionTypes,
  ElysionObject,
  ElysionContainer,
  ElysionColor,
  ElysionContent,
  ElysionLogger,
  ElysionTextureManager,
  ElysionTimer,
  ElysionUtils,
  ElysionKeyCode,

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
  SysUtils,
  Classes;

type

// TelVideoFlags
TelVideoFlag =
  (vfNull,      //< vfNull: Use for console applications, no video surface will be created
   vfAuto,      //< vfAuto: Automatically checks if hardware or software render mode are available
   vfHardware,  //< vfHardware: Use hardware surface
   vfSoftware); //< vfSoftware: Use software surface

TelProjectionMode = (pmFrustum, pmOrtho);

{
    @classname @br
    Description: @br
    Provides an application container

}
TAppContainer = class(TelContainer)
  private
    FInitialized: Boolean;

    FRun: Boolean;

    {
        Application.GetUnicodeSupport @br
        Unicode support
        @return

    }
    function GetUnicodeSupport: Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    {
        Application.SetUnicodeSupport

    }
    procedure SetUnicodeSupport(Value: Boolean); {$IFDEF CAN_INLINE} inline; {$ENDIF}
  public
    {
      Application.Create


      Returns: Instance of class Application
    }
    constructor Create; Override;

    {
      Application.Destroy


    }
    destructor Destroy; Override;

    {
      Application.Initialize

      Returns:
    }
    function Initialize: Boolean; Overload; Override;

    // For backward compability purposes
    function Initialize(const aName: String; Width, Height, Bits: Integer; Fullscreen: Boolean; VideoFlag: TelVideoFlag): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Initialize(Width, Height, Bits: Integer; Fullscreen: Boolean = false): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    {
      Application.Finalize
    }
    procedure Finalize(); Override;

    {
      Application.CreateWindow
      @param Name: String
             Width, Height, Bits: Integer
             Fullscreen: Boolean
             VideoFlag: TelVideoFlags

    }
    function CreateWindow(aName: String; Width, Height, Bits: Integer; Fullscreen: Boolean; VideoFlag: TelVideoFlag): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    {
      Application.CreateWindow

      @seealso
    }
    function CreateWindow(aName: String; Width, Height, Bits: Integer; Fullscreen: Boolean = false): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    {
      Application.Quit
      Description:
    }
    procedure Quit; {$IFDEF CAN_INLINE} inline; {$ENDIF}

  published
    property Initialized: Boolean read FInitialized;

    // Use Application.Run for the main loop
    property Run: Boolean read FRun;

    {
      If UnicodeSupport is active, all keydown events are switched to unicode
      and true type fonts will be automatically set for unicode mode
    }
    property UnicodeSupport: Boolean read GetUnicodeSupport write SetUnicodeSupport;
end;

{ TelWindow }

TelWindow = class(TelObject)
  private
    fJoystick: PSDL_Joystick;

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

    UseBackgroundImage: Boolean;

    FBackgroundChange: Boolean;
    FBackgroundImage: PSDL_Surface;

    fEvent: TSDL_Event;

    fCaption, fIcon: PChar;

    fFrames: Integer;

    fLimitFPS: Boolean;
    fFrameCap: Single;

    fFPSTimer, fDeltaTimer, fCursorReset: TelTimer;

    fMouseDownCount, fMouseUpCount, fMouseMotionCount: Integer;
    fProjection: TelProjectionMode;

    fFocused: Boolean;

    fDeltaTime: Double;

    // Handling key and mouse inputs
    procedure HandleEvents;

    function GetAspectRatio(): Single; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    // Get the window title
    function GetCaption: String; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    // Sets the window title
    procedure SetCaption(Caption: String); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    procedure SetFPS(Value: Single); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function GetFPS: Single; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function GetDeltaTime: Double; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function GetWidth: Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function GetHeight: Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function GetBitsPerPixel: Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function GetJoystickCount(): Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    procedure SetNativeResolution(NativeRes: TelVector2i); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function GetNativeResolution: TelVector2i; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function GetDisplayOrientation(): TRectOrientation; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function GetWideScreen(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    // Set projection matrix
    // Expert function: Automatically Ortho mode is being called, this function needs to be used
    // if you want to display 3d objects on the screen, you need to call perspective mode
    procedure SetProjection(ProjectionMode: TelProjectionMode); {$IFDEF CAN_INLINE} inline; {$ENDIF}

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
      function SetVideoMode(_Width, _Height, _ColorBits: Integer; _Fullscreen: Boolean = False): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}


      // Switches between windowed mode and fullscreen mode
      procedure ToggleFullscreen(); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      // Use BeginScene first thing in the game loop
      procedure BeginScene(); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      // Use EndScene last thing before the end of the game loop
      procedure EndScene(); {$IFDEF CAN_INLINE} inline; {$ENDIF}


      // Expert function: Use background image instead of a color
      procedure SetBackgroundImage(Surface: PSDL_Surface); Overload;

      // Sets background image with specifying a filename
      procedure SetBackgroundImage(const Filename: String); Overload;

      // Sets background color
      procedure SetBackgroundColor(Color: TelColor); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      // Sets a window icon
      procedure SetIcon(Filename: String); Overload;

      // Sets a window icon with color key
      procedure SetIcon(Filename: String; Mask: TelColor); Overload;

      // Hide the standard black/white cursor. Normally turned on
      procedure HideCursor; {$IFDEF CAN_INLINE} inline; {$ENDIF}

      // Show the cursor
      procedure ShowCursor; {$IFDEF CAN_INLINE} inline; {$ENDIF}

      // Saves the surface into an image
      Procedure TakeScreenshot(aFilename: String = '');

      // Checks if a point is within a rect
      Function OnPoint(Coord: TelVector2i; Rect: TelRect): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

      // Checks if surface is active
      function IsActive: Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function IsFocused: Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

      function GetTicks(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}

      property BackgroundColor: TelColor read fBackgroundColor write fBackgroundColor;

      property JoyBall: TelVector2i read fJoyBall;

      property Cursor: TelVector2i read fCursor;
      property RelCursor: TelVector2i read fRelCursor;

      property ResScale: TelVector2f read fResScale write fResScale;
      property NativeResolution: TelVector2i read GetNativeResolution write SetNativeResolution;

  published
    property Active: Boolean read IsActive;
    property Focused: Boolean read IsFocused;

    property AspectRatio: Single read GetAspectRatio;

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
    property Caption: String read GetCaption write SetCaption;

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

 // Factory pattern
  TelWindowManager = class(TelObject)
    private
      fWindowList: TList;
      fWindowCount: Integer;

      function GetCurrentWindow(): TelWindow; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    public
      constructor Create; Override;
      destructor Destroy; Override;

      function CreateWindow(const aName: String; Width, Height, Bits: Integer; Fullscreen: Boolean; VideoFlag: TelVideoFlag): TelWindow; Overload;
      function CreateWindow(const aName: String; Width, Height, Bits: Integer; Fullscreen: Boolean = false): TelWindow; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function DestroyWindow(aWindow: TelWindow): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure DestroyAllWindows(); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    published
      property CurrentWindow: TelWindow read GetCurrentWindow;
      property WindowCount: Integer read fWindowCount;
  end;

  { TelEnvironment }

  TelEnvironment = class(TelObject)
    private
      fWidth: Integer;
      fHeight: Integer;
      fColorDepth: Byte;
      fMobile: Boolean;

      function GetAspectRatio(): Single; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function GetBasename: AnsiString;
      function GetWorkingPath: AnsiString;

    public
      constructor Create; Override;
      destructor Destroy; Override;

    published
      property AspectRatio: Single read GetAspectRatio;

      property Width: Integer read fWidth;
      property Height: Integer read fHeight;
      property ColorDepth: Byte read fColorDepth;

      property Mobile: Boolean read fMobile;

      property Basename: AnsiString read GetBasename;
      property WorkingPath: AnsiString read GetWorkingPath;
  end;

{$IFDEF AUTO_INIT}
var
  // Application
  Application: TAppContainer;
  WindowManager: TelWindowManager;
  Environment: TelEnvironment;
{$ENDIF}

  function ActiveWindow: TelWindow; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function TicksNow(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}

implementation

function ActiveWindow: TelWindow; {$IFDEF CAN_INLINE} inline; {$ENDIF}
begin
  Result := WindowManager.CurrentWindow;
end;

function TicksNow(): Cardinal;
begin
  Result := ActiveWindow.GetTicks();
end;

{
  #############################################################################
  # TAppContainer                                                             #
  #############################################################################

  Description:


  Additional Notes: Essential class

}

constructor TAppContainer.Create;
begin
  inherited Create;

  FRun := true;
  FInitialized := false;
  UnicodeSupport := true;
  fDrivername := 'SDL';
end;

destructor TAppContainer.Destroy;
begin
  Finalize();


  if Self.Debug then TelLogger.GetInstance.Dump();

  inherited Destroy;

  Halt(0);
end;

function TAppContainer.Initialize: Boolean; 
begin
  Result := true;

  if SDL_Init(SDL_INIT_EVERYTHING) <> 0 then
  begin
    Result := false;
    TelLogger.GetInstance.WriteLog('Error initializing SDL', ltError);
    Exit;
  end else FInitialized := true;

   SDL_JoystickEventState(SDL_ENABLE);

  // Laden und Initalisieren von OpenGL

  {$IFDEF USE_DGL_HEADER}
    InitOpenGL;
    ReadExtensions;
  {$ENDIF}
    //ReadImplementationProperties;

end;

function TAppContainer.Initialize(const aName: String; Width, Height, Bits: Integer; Fullscreen: Boolean; VideoFlag: TelVideoFlag): Boolean; 
begin
  if not Initialized then Initialize();
  Result := CreateWindow(aName, Width, Height, Bits, Fullscreen, VideoFlag);
end;

function TAppContainer.Initialize(Width, Height, Bits: Integer; Fullscreen: Boolean = false): Boolean; 
begin
  if not Initialized then Initialize();
  Result := CreateWindow('', Width, Height, Bits, Fullscreen);
end;

procedure TAppContainer.Finalize(); 
begin
  WindowManager.DestroyAllWindows();

  if FInitialized then
  begin
    SDL_Quit;
  end;
end;

function TAppContainer.CreateWindow(aName: String; Width, Height, Bits: Integer; Fullscreen: Boolean; VideoFlag: TelVideoFlag): Boolean; 
begin
  (*if Window = nil then
  begin

  end else
  begin
    if isLoggerActive then TelLogger.GetInstance.WriteLog('Elysion Library supports only one window at a time at the moment.');
    Result := false;
  end; *)

  if WindowManager.CreateWindow(aName, Width, Height, Bits, Fullscreen, VideoFlag) <> nil then Result := true
     else Result := false;

end;

function TAppContainer.CreateWindow(aName: String; Width, Height, Bits: Integer; Fullscreen: Boolean = false): Boolean; 
begin
  Result := Self.CreateWindow(aName, Width, Height, Bits, Fullscreen, vfAuto);
end;

procedure TAppContainer.Quit; 
begin
  FRun := false;
end;

function TAppContainer.GetUnicodeSupport: Boolean; 
begin
  if SDL_EnableUNICODE(-1) = 0 then Result := false
                               else Result := true;
end;

procedure TAppContainer.SetUnicodeSupport(Value: Boolean); 
begin
  if Value then SDL_EnableUNICODE(1)
           else SDL_EnableUNICODE(0);
end;

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

  FZNear := 0.0;
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
    SetProjection(pmOrtho);

    glTranslatef(0, 0, 0);

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
end;

procedure TelWindow.EndScene; 
begin
  glFlush();

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

procedure TelWindow.SetBackgroundImage(Surface: PSDL_Surface); deprecated;
begin
  if Surface <> nil then
  begin
    UseBackgroundImage := true;
    fBackgroundImage := Surface;
    FBackgroundChange := true;
  end;
end;

procedure TelWindow.SetBackgroundImage(const Filename: String);
var Directory: String;
begin

  if Filename <> '' then
  begin
    Directory := ExtractFilePath(ParamStr(0));
    if FileExists(Directory+Content.RootDirectory+FileName) then
    begin
      UseBackgroundImage := true;

      {$IFDEF USE_SDL_IMAGE}
        fBackgroundImage := IMG_Load(PChar(Directory+Content.RootDirectory+FileName));
      {$ENDIF}
      {$IFDEF USE_VAMPYRE}
        fBackgroundImage := LoadSDLSurfaceFromFile(Directory+Content.RootDirectory+FileName);
      {$ENDIF}

    end else TelLogger.GetInstance.WriteLog('File not found: '+Directory+Content.RootDirectory+FileName, ltError);
  end else TelLogger.GetInstance.WriteLog('No filename specifies.', ltError);
end;

procedure TelWindow.SetBackgroundColor(Color: TelColor); 
begin
  UseBackgroundImage := false;
  FBackgroundChange := true;
  FBackgroundColor := Color;
end;

function TelWindow.GetCaption: String; 
begin
  Result := String(fCaption);
end;

procedure TelWindow.SetCaption(Caption: String); 
begin
  fCaption := PChar(Caption);
  SDL_WM_SetCaption(fCaption, fCaption);
end;

procedure TelWindow.SetIcon(Filename: String);
var TMP_Icon: PSDL_Surface;
    Directory: String;
begin
  if Filename <> '' then
  begin
    Directory := ExtractFilePath(ParamStr(0));

    if FileExists(Directory + Filename) then
    begin
      {$IFDEF USE_SDL_IMAGE}
        TMP_Icon := IMG_Load(PChar(Directory + Filename));
      {$ENDIF}
      {$IFDEF USE_VAMPYRE}
         TMP_Icon := LoadSDLSurfaceFromFile(Directory+Content.RootDirectory+FileName);
      {$ENDIF}
      FIcon := PChar(Filename);
      SDL_WM_SetIcon(TMP_Icon, 0);
    end
      else TelLogger.GetInstance.WriteLog('File not found: ' + Directory + Content.RootDirectory + FileName, ltError);
  end else TelLogger.GetInstance.WriteLog('No filename specified.', ltError);

  if TMP_Icon <> nil then SDL_FreeSurface(TMP_Icon);
end;

procedure TelWindow.SetIcon(Filename: String; Mask: TelColor);
var TMP_Icon: PSDL_Surface;
    Directory: String;
begin
  if Filename <> '' then
  begin
    Directory := ExtractFilePath(ParamStr(0));

    if FileExists(Directory + Filename) then
    begin
      {$IFDEF USE_SDL_IMAGE}
        TMP_Icon := IMG_Load(PChar(Directory + Content.RootDirectory + Filename));
      {$ENDIF}
      {$IFDEF USE_VAMPYRE}
        TMP_Icon := LoadSDLSurfaceFromFile(Directory+Content.RootDirectory+FileName);
      {$ENDIF}
      if TMP_Icon <> Nil then
        SDL_SetColorKey(TMP_Icon, SDL_SRCCOLORKEY or SDL_RLEACCEL or SDL_HWACCEL,
      SDL_MapRGB(TMP_Icon^.Format, Mask.R, Mask.G, Mask.B));

      FIcon := PChar(Filename);
      SDL_WM_SetIcon(TMP_Icon, 0);
    end else TelLogger.GetInstance.WriteLog('File not found: '+Directory+Content.RootDirectory+FileName, ltError);
  end else TelLogger.GetInstance.WriteLog('No filename specified.', ltError);

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

procedure TelWindow.TakeScreenshot(aFilename: String);
var
  tmpSurface: PSDL_Surface;
  rmask, gmask, bmask, amask: Uint32;
  tmpScreenshotCount: Integer;
  tmpFilename, formattedDateTime: String;
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

  // Saves OpenGL context into tmpSurface
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
  ImagingSDL.SaveSDLSurfaceToFile(Environment.WorkingPath + tmpFilename, tmpSurface);
  {$ELSE}
  SDL_SaveBMP(tmpSurface, PAnsiChar(Environment.WorkingPath + tmpFilename));
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
    SDL_ACTIVEEVENT:
      begin
        fFocused := (FEvent.active.gain = 1);
      end;

    SDL_QUITEV:
       begin
         if (Self.SDL_Surface <> nil) then Application.Quit;
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

function TelWindow.IsActive: Boolean; 
begin
  if ((SDL_GetAppState = SDL_APPACTIVE) or
      (SDL_GetAppState = SDL_APPMOUSEFOCUS) or
      (SDL_GetAppState = SDL_APPINPUTFOCUS)) then Result := true
                                             else Result := false;
end;

function TelWindow.IsFocused: Boolean;
begin
  Result := fFocused;
end;

function TelWindow.GetTicks(): Cardinal;
begin
  Result := SDL_GetTicks();
end;

constructor TelWindowManager.Create;
begin
  inherited Create;

  fWindowCount := 0;

  fWindowList := TList.Create;
end;

destructor TelWindowManager.Destroy;
begin
  if fWindowList <> nil then DestroyAllWindows();

  inherited Destroy;
end;

function TelWindowManager.GetCurrentWindow(): TelWindow; 
begin
  Result := TelWindow(fWindowList.Items[0]);
end;

function TelWindowManager.CreateWindow(const aName: String; Width, Height, Bits: Integer; Fullscreen: Boolean; VideoFlag: TelVideoFlag): TelWindow;
var
  tempWindow: TelWindow;
  tempName: String;
begin

  tempName := aName;
  if tempName = '' then tempName := 'Elysion Application';
  tempWindow := TelWindow.Create;
  tempWindow.ID := WindowCount;
  tempWindow.Name := tempName;
  tempWindow.SetVideoMode(Width, Height, Bits, Fullscreen, VideoFlag);

  fWindowList.Add(tempWindow);
  fWindowCount := fWindowCount + 1;

  Result := tempWindow;
end;

function TelWindowManager.CreateWindow(const aName: String; Width, Height, Bits: Integer; Fullscreen: Boolean = false): TelWindow; 
begin
  Result := Self.CreateWindow(aName, Width, Height, Bits, Fullscreen, vfAuto);
end;

function TelWindowManager.DestroyWindow(aWindow: TelWindow): Boolean; 
begin
  aWindow.Destroy;

  if aWindow = nil then Result := true
     else Result := false;
end;

procedure TelWindowManager.DestroyAllWindows();
begin
  FreeAndNil(fWindowList);
end;

constructor TelEnvironment.Create;
var
  VideoInfo: PSDL_VideoInfo;
begin
  inherited;

  if not Application.Initialized then Application.Initialize();


  VideoInfo := SDL_GetVideoInfo;

  if VideoInfo = nil then
  begin
    Destroy;
    Exit;
  end;

  fWidth := VideoInfo^.current_w;
  fHeight := VideoInfo^.current_h;
  fColorDepth := VideoInfo^.vfmt^.BitsPerPixel;

  fMobile := false;
  {$IFDEF FPC}
    {$IFDEF IPHONEOS}
      fMobile := true;
    {$ENDIF}
    {$IFDEF ARM}
      // In most cases mobile; TODO: add more precise conditions
      fMobile := true;
    {$ENDIF}
  {$ENDIF}
end;

destructor TelEnvironment.Destroy;
begin
  inherited;
end;

function TelEnvironment.GetAspectRatio(): Single;
begin
  Result := (Self.Width / Self.Height);
end;

function TelEnvironment.GetBasename: AnsiString;
begin
  Result := ParamStr(0);
end;

function TelEnvironment.GetWorkingPath: AnsiString;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

{$IFDEF AUTO_INIT}
initialization
  Application := TAppContainer.Create;
  WindowManager := TelWindowManager.Create;
  Environment := TelEnvironment.Create;

finalization
  Environment.Destroy;
  WindowManager.Destroy;
  Application.Destroy;
{$ENDIF}

end.
