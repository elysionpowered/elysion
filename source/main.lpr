
program myapplication;

{$I Elysion.inc}

{$IFDEF WINDOWS}
  {$IFNDEF DEBUG}  
    {$APPTYPE GUI}
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
  uGame in 'units/uGame.pas';

var
  Game: TGame;

begin
  Randomize();

  // Create game class
  Game := TGame.Create;

  // Loads images and config files and everything else
  Game.Initialize();
  
  // Game Loop
  while Game.Run do
  begin
    // Clears buffer
    ActiveWindow.BeginScene;
    
    // Render procedure
    Game.Render(nil);
	
    // Update procedure
    Game.Update(ActiveWindow.DeltaTime);

    // Handle keyboard, mouse, joystick events
    Game.HandleEvents();
    
    // Flip surface
    ActiveWindow.EndScene;
  end;
  
end.
