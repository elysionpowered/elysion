unit uGameScreen;

interface

{$I Elysion.inc}

uses
  Classes,
  SysUtils,
  ElysionApplication,
  ElysionTypes,
  ElysionInput,
  ElysionNode,
  ElysionSprite,
  ElysionGUI,
  ElysionTimer,
  ElysionScene,
  ElysionColor,
  ElysionTrueTypeFont,
  ElysionAnimator,
  ElysionStorage,
  ElysionLayer,
  ElysionCamera,
  uGlobal,
  uBasic,
  uConfig;

type
  TGameScreen = class(TelScene)
  private
    fSprite: TelMovingSprite;

    fFont: TelTrueTypeFont;

    fGameOver: Boolean;


    procedure DrawDialog(Title, Text: String); {$IFDEF CAN_INLINE} inline; {$ENDIF}
  public
    constructor Create; Override;
    destructor Destroy; Override;

    procedure Render; Override;
    procedure Update(dt: Double); Override;
    procedure HandleEvents; Override;

    procedure Reset;
  published
    property Font: TelTrueTypeFont read fFont write fFont;

    property GameOver: Boolean read fGameOver;
    
    property Sprite: TelMovingSprite read fSprite write fSprite;
  end;

implementation

constructor TGameScreen.Create;
begin
  inherited;

  Randomize;

  // Create font container
  Font := TelTrueTypeFont.Create;

  // Load standard font (see uBasic.pas)
  Font.LoadFromFile(GetStdFont, 18);

  // Set font color to white (see ElysionColors.pas)
  Font.Color := Color.clWhite;

  // Set font render style to rtBlended, which means the font will be drawn
  // nice and anti-aliased, but it costs performance
  Font.RenderStyle := rtBlended;


  // Create Sprite
  Sprite := TelMovingSprite.Create;

  // Load logo image from disk
  Sprite.LoadFromFile(GetResImgPath + 'logo.png');

  // Set position to
  Sprite.Position := makeV3f(64, 64);

  Self.Add(Sprite);


  Self.SetPauseKey(Key.P());

  (**
    * Add sprite to the scene. We don't need to call Sprite.Draw or Sprite.Update(dt)
    * ourselves, the scene will take care of it if we call the inherited method.
    *
    * You can add any class derived from TelNode to the scene such as TelMenu, TelSprite, TelLabel, TelButton and even custom nodes.
    *
    * Yes, this is very much like Flash or Cocos2D. :) It is intended to be that way.
    * (Do you know how many hours due to thinking I added the Draw procedure for a sprite, but I didn't? Me neither. I lost count... :/ )
    *
    *)

  Reset;
end;

destructor TGameScreen.Destroy;
begin
  Sprite.Destroy;

  Font.Destroy;

  inherited;
end;

(**
  * Helper function which draws a text an information dialog
  * in the middle of the screen
  *
  *)
procedure TGameScreen.DrawDialog(Title, Text: String);
var
  DialogWidth, DialogHeight: Integer;
begin
  DialogWidth := ActiveWindow.Width div 2;
  DialogHeight := ActiveWindow.Height div 2;

  GUI.RoundedBox(makeRect((ActiveWindow.Width - DialogWidth) / 2,
                   (ActiveWindow.Height - DialogHeight) / 2,
                   DialogWidth, DialogHeight), makeCol(0, 0, 0, 192), 20);

  fFont.TextOut(makeV3f((ActiveWindow.Width - fFont.getWidth_Text(Title)) / 2,
                         (ActiveWindow.Height - DialogHeight) / 2 + 30), Title);

  fFont.TextOut(makeV3f((ActiveWindow.Width - DialogWidth) / 2 + 20,
                        (ActiveWindow.Height - DialogHeight) / 2 + 60), Text);
end;

procedure TGameScreen.Reset;
begin
  // Reset game values
  fGameOver := false;
end;

procedure TGameScreen.Render;
begin
  inherited;

  // Draw game paused overlay
  if Paused then
  begin
    DrawDialog('Game information', 'Paused');
  end;

  // Draw game over overlay
  if GameOver then
  begin
    DrawDialog('Game Over', 'Guess you just had bad luck. Try again');
  end;
end;

procedure TGameScreen.Update(dt: Double);
begin

  if not Paused and not GameOver then
  begin
    inherited;

    // This is called if game is running and not paused and game is not over
    //      -> Update game-related stuff here
    
    if Sprite.MouseOver then Sprite.Alpha := 128
    else Sprite.Alpha := 255;
    
    if Sprite.Click then Sprite.Color := makeCol(Random(255), Random(255), Random(255));



  end;

end;

procedure TGameScreen.HandleEvents;
begin
  // Pause game when P has been pressed
  //if Input.Keyboard.isKeyHit(Key.P) then Paused := not Paused;

  // Reset game when hitting the 'R' key or Start on the XBox controller
  if Input.Keyboard.isKeyHit(Key.R) or Input.XBox360Controller.Start() then Self.Reset;
end;

end.

