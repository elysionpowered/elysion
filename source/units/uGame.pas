unit uGame;

interface

uses
  ElysionTypes,
  ElysionApplication,
  ElysionScene,
  ElysionLogger,
  ElysionColor,
  ElysionSprite,
  ElysionTrueTypeFont,
  ElysionTimer,
  ElysionInput,
  ElysionUtils,

  SysUtils,
  uBasic,
  uGlobal,
  uConfig,
  uMainMenu,
  uGameScreen,
  uCredits,
  uOptions;

type
  TGame = class(TelGame)
  private
    fMainMenu: TMainMenu;
    fGameScreen: TGameScreen;
    fCredits: TCredits;    
    fOptions: TOptions;
    
    fLoadScreen: TelSprite;

    fFont: TelTrueTypeFont;
    fShowFPS, fDebug: Boolean;
    fForceFullscreen, fForceWindow: Boolean;
  public
    constructor Create(); Override;
    destructor Destroy; Override;
    
    procedure Initialize(); Override;

    procedure Render(); Override;
    procedure Update(dt: Double = 0.0); Override;
    procedure HandleEvents(); Override;
  published
    property Debug: Boolean read fDebug write fDebug;
    property ForceFullscreen: Boolean read fForceFullscreen write fForceFullscreen;
    property ForceWindow: Boolean read fForceWindow write fForceWindow;

    property Font: TelTrueTypeFont read fFont write fFont;

    property MainMenu: TMainMenu read fMainMenu write fMainMenu;
    property GameScreen: TGameScreen read fGameScreen write fGameScreen;
    property Credits: TCredits read fCredits write fCredits;
    property Options: TOptions read fOptions write fOptions;

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
  //GameState := gsMainMenu;

  // Check for commandline parameters
  if Self.Param('-debug') then Debug := true;
  if Self.Param('-fullscreen') then ForceFullscreen := true;
  if Self.Param('-window') then ForceWindow := true;

  // Get ya priorities straight

  tmpFullscreen := false;
  if Debug then
    TelLogger.getInstance.Priorities := [ltNote, ltWarning, ltError]
  else tmpFullscreen := true;

  if (ForceWindow) and (not ForceFullscreen) then tmpFullscreen := false
    else tmpFullscreen := true;

  // Super
  if ((Environment.Width = 1024) and (Environment.Height = 600)) then
    inherited Create(Environment.Width, Environment.Height, AppConfig.Bits, true)
  else
    inherited Create(AppConfig.Width, AppConfig.Height, AppConfig.Bits, false);

  fLoadScreen := TelSprite.Create;
  fLoadScreen.LoadFromFile(GetResImgPath + 'loadscreen.jpg');

end;

destructor TGame.Destroy;
begin
  inherited;

  fLoadScreen.Destroy;
end;

procedure TGame.Initialize;
begin

  // Initialize Audio here if you have any sound/music
  //Audio.Initialize();


  ActiveWindow.BeginScene;
  fLoadScreen.Draw;
  ActiveWindow.EndScene;

  ActiveWindow.Caption := 'My Application';
  ActiveWindow.ShowCursor();


  fFont := TelTrueTypeFont.Create;
  fFont.LoadFromFile(GetStdFont, 16);
  fFont.Color := Color.clWhite;
  fFont.RenderStyle := rtBlended;



  //Application.LogDriverInfo();
  Input.DebugInfo();

  // Create scenes
  MainMenu := TMainMenu.Create('mainmenu');
  Options := TOptions.Create('options');
  Credits := TCredits.Create('credits');
  GameScreen := TGameScreen.Create('game');

  // Add scenes to scene director
  SceneDirector.Add(MainMenu);
  SceneDirector.Add(Options);
  SceneDirector.Add(Credits);
  SceneDirector.Add(GameScreen);

  // Switch to main menu
  SceneDirector.SwitchTo('mainmenu');
end;

procedure TGame.Render;
begin
  // This renders all scenes added to the scene director
  inherited Render;
end;

procedure TGame.Update(dt: Double);
begin
  // This updated all scenes added to the scene director
  inherited Update(dt);

  // Shows FPS and debug information
  if ShowFPS then
    fFont.TextOut(makeV3f(8, 8, 0), Format('FPS: %.2f Delta: %.5f \n Mouse Abs: %d %d Rel: %d %d',
      [ActiveWindow.FPS, ActiveWindow.DeltaTime,
       Input.Mouse.Cursor.X, Input.Mouse.Cursor.Y,
       Input.Mouse.RelCursor.X, Input.Mouse.RelCursor.Y]));

end;

procedure TGame.HandleEvents();
begin
  // This renders all scenes added to the scene director
  inherited HandleEvents;

  // Keyboard Inputs
  if SceneDirector.CurrentScene.Name <> 'mainmenu' then
  begin
    if Input.Keyboard.isKeyHit(Key.Escape) or Input.XBox360Controller.Back then SceneDirector.SwitchTo('mainmenu');
  end else
  begin
    if Input.Keyboard.isKeyHit(Key.Escape) or Input.XBox360Controller.Back then Application.Quit();
  end;


  // In-game fullscreen switching only working partially at the moment, needs fixing
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
