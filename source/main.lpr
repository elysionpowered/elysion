
program myapplication;

{$I Elysion.inc}

{$IFDEF WINDOWS}
  {$IFNDEF DEBUG}  
    //{$APPTYPE GUI}
  {$ENDIF}
  
  // Adds icon
  {$R main.res}
{$ELSE}
  // Links automatically to frameworks under Mac OS X
  {$IFDEF DARWIN}
    {$PASCALMAINNAME SDL_main}
    {$linklib SDLmain}

    {$linkframework Cocoa}
    {$linkframework SDL}
    {$linkframework OpenGL}

    // Additional SDL frameworks
    {$linkframework SDL_image}
    {$linkframework SDL_ttf}
    {$linkframework SDL_mixer}
  {$ENDIF}
{$ENDIF}

uses
  ElysionApplication in '../lib/ElysionApplication.pas',
  ElysionWindowManager,
  uGame in 'units/uGame.pas';

var
  Application: TelApplication;
  Game: TGame;

begin
  Randomize();

  Application := TelApplication.Create;
  Application.Debug := true;

  // Create game class
  Game := TGame.Create;

  // Loads images and config files and everything else
  Game.Initialize();
  
  // Game Loop
  while Application.Run do
  begin
    // Clears buffer
    TelWindowManager.CurrentWindow.BeginScene;
    
    // Render procedure
    Game.Render(nil);
	
    // Update procedure
    Game.Update(ActiveWindow.DeltaTime);

    // Handle keyboard, mouse, joystick events
    Game.HandleEvents();
    
    // Flip surface
    TelWindowManager.CurrentWindow.EndScene;
  end;

  Application.Destroy;
  
end.
