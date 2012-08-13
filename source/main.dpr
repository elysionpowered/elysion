program myapplication;

{$IFDEF WINDOWS}
  {$IFNDEF DEBUG}  
    {$APPTYPE GUI}
  {$ENDIF}
  
  // Adds icon
  {$R main.res}
{$ELSE}
  // Maybe move linker stuff here
{$ENDIF}

{$I Elysion.inc}

uses
  ElysionApplication in '../lib/ElysionApplication.pas',
  uGame in 'units/uGame.pas';

var
  Game: TGame;

begin
  // Create game class
  Game := TGame.Create;

  // Loads images and config files and everything else
  Game.Initialize();
  
  // Game Loop
  while Application.Run do
  begin
    // Clears buffer
    ActiveWindow.BeginScene;
    
    // Render procedure
    Game.Render;
	
    // Update procedure
    Game.Update(ActiveWindow.DeltaTime);

    // Handle keyboard, mouse, joystick events
    Game.HandleEvents();
    
    // Flip surface
    ActiveWindow.EndScene;
  end;
  
end.
