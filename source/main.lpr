
program myapplication;

{$I Elysion.inc}

{$IFDEF WINDOWS}
  {$IFNDEF DEBUG}  
<<<<<<< HEAD
    {$APPTYPE GUI}
=======
    //{$APPTYPE GUI}
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1
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
<<<<<<< HEAD
  uGame in 'units/uGame.pas';

var
=======
  ElysionWindowManager,
  uGame in 'units/uGame.pas';

var
  Application: TelApplication;
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1
  Game: TGame;

begin
  Randomize();

<<<<<<< HEAD
=======
  Application := TelApplication.Create;
  Application.Debug := true;

>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1
  // Create game class
  Game := TGame.Create;

  // Loads images and config files and everything else
  Game.Initialize();
  
  // Game Loop
<<<<<<< HEAD
  while Game.Run do
  begin
    // Clears buffer
    ActiveWindow.BeginScene;
=======
  while Application.Run do
  begin
    // Clears buffer
    TelWindowManager.CurrentWindow.BeginScene;
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1
    
    // Render procedure
    Game.Render(nil);
	
    // Update procedure
    Game.Update(ActiveWindow.DeltaTime);

    // Handle keyboard, mouse, joystick events
    Game.HandleEvents();
    
    // Flip surface
<<<<<<< HEAD
    ActiveWindow.EndScene;
  end;
=======
    TelWindowManager.CurrentWindow.EndScene;
  end;

  Application.Destroy;
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1
  
end.
