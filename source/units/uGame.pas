unit uGame;

interface

uses
  ElysionTypes,
  ElysionApplication,
  ElysionStage,
  ElysionLogger,
  ElysionColor,
  ElysionGraphics,
  ElysionTrueTypeFont,
  ElysionTimer,
  ElysionAudio,
  ElysionInput,
  ElysionUtils,

  SysUtils,
  uBasic,
  uGlobal,
  uConfig,
  uMainMenu,
  uGameScreen,
  uCredits,
  uSettings;

type
  TGame = class(TelGame)
  private
    fMainMenu: TMainMenu;
    fGameScreen: TGameScreen;
    fCredits: TCredits;

    fLoadScreen: TelSprite;
    fSettings: TSettings;
    fIntroMusic: TelSound;
    fMusic: TelMusic;
    fIntroTimer: TelTimer;

    fFont: TelTrueTypeFont;
    fShowFPS, fDebug: Boolean;
    fForceFullscreen, fForceWindow: Boolean;
  public
    constructor Create(); Override;
    destructor Destroy; Override;
    
    procedure Initialize(); Override;

    procedure Render(); Override;
    procedure Update(dt: Double); Override;
    procedure HandleEvents(); Override;
  published
    property Debug: Boolean read fDebug write fDebug;
    property ForceFullscreen: Boolean read fForceFullscreen write fForceFullscreen;
    property ForceWindow: Boolean read fForceWindow write fForceWindow;

    property Font: TelTrueTypeFont read fFont write fFont;

    property MainMenu: TMainMenu read fMainMenu write fMainMenu;
    property GameScreen: TGameScreen read fGameScreen write fGameScreen;
    property Credits: TCredits read fCredits write fCredits;
    property Settings: TSettings read fSettings write fSettings;

    property ShowFPS: Boolean read fShowFPS write fShowFPS;
  end;

implementation

constructor TGame.Create;
var
  tmpFullscreen: Boolean;
begin
  Debug := true;


  fForceFullscreen := false;
  fForceWindow := false;

  fShowFPS := false;
  GameState := gsMainMenu;

  // Check for commandline parameters
  if Self.Param('-debug') then Debug := true;
  if Self.Param('-fullscreen') then ForceFullscreen := true;
  if Self.Param('-window') then ForceWindow := true;

  // Get ya priorities straight
  if Debug then
  begin
    TelLogger.getInstance.Priorities := [ltNote, ltWarning, ltError];
    tmpFullscreen := false
  end else tmpFullscreen := true;

  if (ForceWindow) and (not ForceFullscreen) then tmpFullscreen := false
    else tmpFullscreen := true;

  // Super
  //if ((Desktop.Width = 1024) and (Desktop.Height = 600)) then
    //inherited Create(Desktop.Width, Desktop.Height, AppConfig.Bits, true)
  //else
    inherited Create(AppConfig.Width, AppConfig.Height, AppConfig.Bits, false);

  fLoadScreen := TelSprite.Create;
  fLoadScreen.LoadFromFile(GetResImgPath + 'loadscreen.jpg');

end;

destructor TGame.Destroy;
begin
  inherited;

  fLoadScreen.Destroy;

  MainMenu.Destroy;
  if GameScreen <> nil then GameScreen.Destroy;
end;

procedure TGame.Initialize;
var
  newRatio: Single;
begin

  Audio.Initialize();

  fIntroTimer := TelTimer.Create;
  fIntroTimer.Interval := 5750;
  fIntroTimer.Start();



  if fLoadScreen.AspectRatio = ActiveWindow.AspectRatio then
    fLoadScreen.Scale := makeV2f(ActiveWindow.Width / fLoadScreen.Width, ActiveWindow.Height / fLoadScreen.Height)
  else begin
    if (fLoadScreen.AspectRatio * ActiveWindow.Width) > ActiveWindow.Width then
      fLoadScreen.Left := - (((fLoadScreen.AspectRatio * ActiveWindow.Height) - ActiveWindow.Width) / 2)
    else
      fLoadScreen.Left := (((fLoadScreen.AspectRatio * ActiveWindow.Height) - ActiveWindow.Width) / 2);

    newRatio := ActiveWindow.Height / fLoadScreen.Height;
    fLoadScreen.Scale := makeV2f(newRatio, newRatio);
  end;

  ActiveWindow.BeginScene;
  fLoadScreen.Draw;
  ActiveWindow.EndScene;

  ActiveWindow.Caption := 'Mr. Fire Takes A Walk';
  ActiveWindow.ShowCursor();


  fFont := TelTrueTypeFont.Create;
  fFont.LoadFromFile(GetStdFont, 16);
  fFont.Color := Color.clWhite;
  fFont.RenderStyle := rtBlended;



  //Application.LogDriverInfo();
  Input.DebugInfo();


  MainMenu := TMainMenu.Create;
  Settings := TSettings.Create;


  MainMenu.Background.Scale := fLoadScreen.Scale;
  MainMenu.Background.Left := fLoadScreen.Left;
  Settings.Background.Scale := fLoadScreen.Scale;
  Settings.Background.Left := fLoadScreen.Left;


  Credits := TCredits.Create();

  fIntroMusic := TelSound.Create;
  fIntroMusic.LoadFromFile(GetResSndPath + 'intro.wav');
  if uGlobal.Music then fIntroMusic.Play();

  fMusic := TelMusic.Create;

  fMusic.LoadFromFile(GetResSndPath + 'music.mp3');

end;

procedure TGame.Render;
begin
  if fIntroTimer.Event then
  begin
    if uGlobal.Music then fMusic.Play(-1);
    fIntroTimer.Stop();
  end;

  if MainMenu.NewGameClick then
  begin
    fLoadScreen.Draw;
    ActiveWindow.EndScene;

    if fGameScreen <> nil then FreeAndNil(fGameScreen);
    fGameScreen := TGameScreen.Create;

    MainMenu.NewGameClick := false;
  end;


  if GameState <> gsGame then
    ActiveWindow.ShowCursor();


  case GameState of
    gsMainMenu: MainMenu.Render();
    gsSettings: Settings.Render;
    gsIntro: ; //< As if we would need an intro? ;)
    gsCredits: Credits.Render;
    gsGame: GameScreen.Render;
  end;
  
end;

procedure TGame.Update(dt: Double);
begin
  ActiveWindow.Caption := Format('FPS: %.2f Delta: %.5f', [ActiveWindow.FPS, ActiveWindow.DeltaTime]);

  (*if ShowFPS then
    fFont.TextOut(makeV3f(8, 8, 0), Format('FPS: %.2f Delta: %.5f \n Mouse Abs: %d %d Rel: %d %d',
      [ActiveWindow.FPS, ActiveWindow.DeltaTime,
       Input.Mouse.Cursor.X, Input.Mouse.Cursor.Y,
       Input.Mouse.RelCursor.X, Input.Mouse.RelCursor.Y]));*)


  case GameState of
    gsCredits:
    begin
      Credits.Update(dt);
    end;
    gsSettings:
    begin
      Settings.Update(dt);
    end;
    gsMainMenu:
    begin
      MainMenu.Update(dt);
    end;
    gsGame:
    begin
      GameScreen.Update(dt);
    end;
  end;

  // Update music if necessary
  if uGlobal.Music then
  begin
    if (not fMusic.isPlaying) then fMusic.Play
  end else
  begin
    if (fMusic.isPlaying) then fMusic.Stop
  end;

end;

procedure TGame.HandleEvents();
begin
  case GameState of
    gsCredits:
    begin
      Credits.HandleEvents;
    end;
    gsSettings:
    begin
      Settings.HandleEvents;
    end;
    gsMainMenu:
    begin
      MainMenu.HandleEvents;
    end;
    gsGame:
    begin
      GameScreen.HandleEvents;
    end;
  end;

  // Keyboard Inputs
  if GameState <> gsMainMenu then
  begin
    if Input.Keyboard.isKeyHit(Key.Escape) or Input.XBox360Controller.Back then GameState := gsMainMenu;
  end else
  begin
    if Input.Keyboard.isKeyHit(Key.Escape) or Input.XBox360Controller.Back then Application.Quit();
  end;

  // Fullscreen not working at the moment, needs fixing
  if Input.Keyboard.isKeyHit(Key.F) then
  begin
    ShowFPS := not ShowFPS;
  end;

  if Input.Keyboard.isKeyHit(Key.T) then
  begin
    ActiveWindow.TakeScreenShot('screenshot');
  end;

end;

end.
