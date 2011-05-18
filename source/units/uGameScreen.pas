unit uGameScreen;

interface

{$I Elysion.inc}

uses
  Classes,
  SysUtils,
  ElysionApplication,
  ElysionTypes,
  ElysionInput,
  ElysionGraphics,
  ElysionGUI,
  ElysionTimer,
  ElysionScene,
  ElysionColor,
  ElysionTrueTypeFont,
  ElysionAnimator,
  ElysionStorage,
  uGlobal,
  uBasic,
  uConfig;

type
  TGameScreen = class(TelScene)
  private
    fSprite: TelSprite;

    fFont: TelTrueTypeFont;

    fPaused, fGameOver: Boolean;

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

    property Paused: Boolean read fPaused write fPaused;
    property GameOver: Boolean read fGameOver;
    
    property Sprite: TelSprite read fSprite write fSprite;
  end;

implementation

constructor TGameScreen.Create;
begin
  inherited;

  Randomize;

  Font := TelTrueTypeFont.Create;
  Font.LoadFromFile(GetStdFont, 18);
  Font.Color := Color.clWhite;
  Font.RenderStyle := rtBlended;

  
  Sprite := TelSprite.Create;
  Sprite.LoadFromFile(GetResImgPath + 'logo.png');
  
  Sprite.Position := makeV3f(64, 64);
  Sprite.Offset.Rotation := makeV2i(128, 128);

  Reset;
end;

destructor TGameScreen.Destroy;
begin
  inherited;
end;

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
  fPaused := false;
  fGameOver := false;
end;

procedure TGameScreen.Render;
begin
  Sprite.Draw;

  if Paused then
  begin
    DrawDialog('Game information', 'Paused');
  end;

  if GameOver then
  begin
    DrawDialog('Game Over', 'Guess you just had bad luck. Try again');
  end;
end;

procedure TGameScreen.Update(dt: Double);
begin


  if not Paused and not GameOver then
  begin
    // Do not update stuff here or just update the stuff you want
    
    if Sprite.MouseOver then Sprite.Alpha := 128
    else Sprite.Alpha := 255;
    
    if Sprite.Click then Sprite.Color := makeCol(Random(255), Random(255), Random(255));

    if Input.Keyboard.IsKeyDown(Key.Left()) or Input.XBox360Controller.LStick.Left then Sprite.Rotate(7.5 * dt);
    if Input.Keyboard.IsKeyDown(Key.Right()) or Input.XBox360Controller.LStick.Right then Sprite.Rotate(-7.5 * dt);
  end;

end;

procedure TGameScreen.HandleEvents;
begin
  if Input.Keyboard.isKeyHit(Key.P) then Paused := not Paused;
  if Input.Keyboard.isKeyHit(Key.R) or Input.XBox360Controller.Start() then Self.Reset;
end;

end.

