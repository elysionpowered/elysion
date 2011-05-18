(**
  * Elysion Input API
  *
  * @author(Johannes Stein and contributors <http://elysionpowered.org>)
  *
  *)
unit ElysionInput;

interface

{$I Elysion.inc}

uses
  ElysionTypes,
  ElysionApplication,
  ElysionLogger,
  ElysionTimer,
  ElysionKeyCode,

  SDL,
  SysUtils,
  Classes;

type

(*TelAxis = record
  Value: Single;
  Relevance: Boolean;
end;*)

TelAnalogStick = record
  AxisH: Single; //< Horizontal axis
  AxisV: Single; //< Vertical axis

  Click: Boolean; //< Stick click
  
  Motion: Boolean;
  
  Left, Right, Up, Down: Boolean;
end;

TelKeyboardHelper = class
  private
    fLastUsedKey: LongWord;
  public
    constructor Create;
    destructor Destroy; Override;

    function IsKeyDown(Key: Cardinal): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function IsKeyHit(Key: Cardinal): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function IsKeyUp(Key: Cardinal): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function GetLastUsedKey: LongWord; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  published
    property LastUsedKey: LongWord read GetLastUsedKey;
end;

// TODO: Add multiple joystick support
TelJoystickHelper = class
  private
    fLastUsedButton: LongWord;

    function GetBall(): TelVector2i; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  public
    constructor Create;
    destructor Destroy; Override;

    function IsButtonDown(Button: Cardinal): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function IsButtonHit(Button: Cardinal): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function IsButtonUp(Button: Cardinal): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function GetLastUsedButton(): LongWord; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    property Ball: TelVector2i read GetBall;
  published
    property LastUsedButton: LongWord read GetLastUsedButton;
end;

// DPad for GamePads
TelDPad = class
  public
    constructor Create;
    destructor Destroy; Override;

    function Centered(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Up(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Right(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Down(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Left(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function RightUp(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function RightDown(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function LeftUp(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function LeftDown(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
end;

TelXBox360Controller = class
  private
    fControllerName: String;
    fDPad: TelDPad;

    function GetConnected(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function GetLStick(): TelAnalogStick; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function GetRStick(): TelAnalogStick; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function GetTrigger(): TelAnalogStick; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  public
    constructor Create;
    destructor Destroy; Override;

    function A(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function B(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function X(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Y(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function Start(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Back(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function LButton(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function LTrigger(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function RButton(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function RTrigger(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    property LStick: TelAnalogStick read GetLStick;
    property RStick: TelAnalogStick read GetRStick;
  published
    property DPad: TelDPad read fDPad write fDPad;

    property IsConnected: Boolean read GetConnected;
end;

TelPlaystationController = class
  private
    fDPad: TelDPad;
  public
    constructor Create;
    destructor Destroy; Override;

    function Triangle(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Square(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Circle(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Cross(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function Select(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Start(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function LButton(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function LTrigger(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function RButton(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function RTrigger(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  published
    property DPad: TelDPad read fDPad write fDPad;
end;

TelGP2XController = class
  private
    fDPad: TelDPad;

    function GetConnected(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  public
    constructor Create;
    destructor Destroy; Override;

    function A(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function B(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function X(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Y(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function Menu(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Select(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function LButton(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function RButton(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  published
    property IsConnected: Boolean read GetConnected;

    property DPad: TelDPad read fDPad write fDPad;
end;

TelMouseHelper = class
  private
    fDblClickInterval: Integer;
    fDblClicked: Boolean;

    function GetCursor(): TelVector2i; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function GetRelCursor(): TelVector2i; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  public
    constructor Create;
    destructor Destroy; Override;

    function IsButtonDown(Button: LongWord): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function IsButtonHit(Button: LongWord): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function IsButtonUp(Button: LongWord): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function LeftClick(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function RightClick(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function Down(Button: LongWord = BUTTON_LEFT): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Up(Button: LongWord = BUTTON_LEFT): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function DblClick(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function WheelDown(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function WheelUp(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function Motion(): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Motion(Rect: TelRect): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}


    property Cursor: TelVector2i read GetCursor;
    property RelCursor: TelVector2i read GetRelCursor;
  published
    property DblClickInterval: Integer read fDblClickInterval write fDblClickInterval;
end;


{ TelInputHelper }

TelInputHelper = class
  private
    fKeyboard: TelKeyboardHelper;
    fJoystick: TelJoystickHelper;
    fMouse: TelMouseHelper;

    fXBox360Controller: TelXBox360Controller;

    function GetJoystickName(): String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function GetJoystickCount(): Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  public
    constructor Create;
    destructor Destroy; Override;

    procedure DebugInfo();
  published
    property Keyboard: TelKeyboardHelper read fKeyboard write fKeyboard;
    property Joystick: TelJoystickHelper read fJoystick write fJoystick;
    property Mouse: TelMouseHelper read fMouse write fMouse;
    property XBox360Controller: TelXBox360Controller read fXBox360Controller write fXBox360Controller;

    property JoystickName: String read GetJoystickName;
    property JoystickCount: Integer read GetJoystickCount;
end;

TelKeyCode = class
  private
    fKeyList: TStringList;
  public
    constructor Create;
    destructor Destroy; Override;

    function BackSpace(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Tab(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Clear(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Return(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Enter(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Pause(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Escape(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Space(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Exclaim(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function QuoteDbl(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Hash(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Dollar(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Ampersand(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Quote(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function LeftParen(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function RightParen(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Asterisk(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Plus(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Comma(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Minus(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Period(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Slash(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Alpha0(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Alpha1(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Alpha2(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Alpha3(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Alpha4(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Alpha5(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Alpha6(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Alpha7(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Alpha8(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Alpha9(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Colon(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function SemiColon(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Less(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Equals(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Greater(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Question(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function At(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function LeftBracket(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function BackSlash(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function RightBracket(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Caret(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function UnderScore(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function BackQuote(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function A(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function B(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function C(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function D(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function E(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function F(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function G(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function H(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function I(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function J(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function K(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function L(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function M(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function N(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function O(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function P(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Q(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function R(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function S(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function T(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function U(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function V(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function W(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function X(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Y(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Z(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Delete(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function NumPad0(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function NumPad1(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function NumPad2(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function NumPad3(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function NumPad4(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function NumPad5(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function NumPad6(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function NumPad7(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function NumPad8(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function NumPad9(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function NumPad_Period(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function NumPad_Divide(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function NumPad_Multiply(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function NumPad_Minus(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function NumPad_Plus(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function NumPad_Enter(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function NumPad_Equals(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Up(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Down(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Right(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Left(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Insert(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function PageHome(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function PageEnd(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function PageUp(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function PageDown(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function F1(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function F2(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function F3(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function F4(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function F5(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function F6(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function F7(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function F8(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function F9(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function F10(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function F11(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function F12(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function NumLock(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function CapsLock(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function ScrolLock(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function RShift(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function LShift(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function RCtrl(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function LCtrl(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function RAlt(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function LAlt(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function RMeta(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function LMeta(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function LSuper(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function RSuper(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Mode(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF} // Alt Gr
    function Help(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Print(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Break(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Menu(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Euro(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    //procedure SetCustomKeyCode(aKeyCode: String; KeyAscii: Cardinal);
    //function GetCustomKeyCode(aKeyCode: String): Cardinal;
end;

TelButtonCode = class
  public
    constructor Create;
    destructor Destroy; Override;

    function Left(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Right(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Middle(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function WheelUp(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function WheelDown(): Cardinal; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    //procedure SetCustomButton
end;

{$IFDEF AUTO_INIT}
var
  Input: TelInputHelper;
  Key: TelKeyCode;
  Button: TelButtonCode;
{$ENDIF}

implementation

constructor TelKeyboardHelper.Create;
begin
  inherited Create;
end;

destructor TelKeyboardHelper.Destroy;
begin
  FLastUsedKey := 0;

  inherited Destroy;
end;

function TelKeyboardHelper.IsKeyDown(Key: Cardinal): Boolean; 
begin
  fLastUsedKey := Key;

  if SDL_GetModState = Key then Result := ActiveWindow.ModKeyDown[Key]
                           else Result := ActiveWindow.KeyDown[Key];

end;

function TelKeyboardHelper.IsKeyHit(Key: Cardinal): Boolean; 
begin
  fLastUsedKey := Key;

  if isKeyDown(Key) then
  begin
    Result := true;
    if SDL_GetModState = Key then ActiveWindow.ModKeyDown[Key] := false
    else ActiveWindow.KeyDown[Key] := false;
  end else Result := false;
end;

function TelKeyboardHelper.IsKeyUp(Key: Cardinal): Boolean; 
begin
  fLastUsedKey := Key;

  if SDL_GetModState = Key then Result := ActiveWindow.ModKeyUp[Key]
                           else Result := ActiveWindow.KeyUp[Key];

end;

function TelKeyboardHelper.GetLastUsedKey(): LongWord; 
begin
  Result := fLastUsedKey;
end;

constructor TelJoystickHelper.Create;
begin
  inherited Create;
end;

destructor TelJoystickHelper.Destroy;
begin
  inherited Destroy;
end;

function TelJoystickHelper.GetLastUsedButton(): LongWord;
begin
  Result := fLastUsedButton;
end;

function TelJoystickHelper.GetBall(): TelVector2i;
begin
  Result := ActiveWindow.JoyBall;
end;

function TelJoystickHelper.IsButtonDown(Button: Cardinal): Boolean;
begin
  fLastUsedButton := Button;

  Result := ActiveWindow.JoyButtonDown[Button];
end;

function TelJoystickHelper.IsButtonHit(Button: Cardinal): Boolean;
begin
  fLastUsedButton := Button;

  if ActiveWindow.JoyButtonDown[Button] then
  begin
	ActiveWindow.JoyButtonDown[Button] := false;
	Result := true;
  end else Result := false;
end;

function TelJoystickHelper.IsButtonUp(Button: Cardinal): Boolean;
begin
  fLastUsedButton := Button;

  Result := ActiveWindow.JoyButtonUp[Button];
end;

constructor TelDPad.Create;
begin
  inherited;
end;

destructor TelDPad.Destroy;
begin
  inherited;
end;

function TelDPad.Centered(): Boolean;
begin
  if ActiveWindow.JoyHat[HAT_CENTERED] then Result := true
    else Result := false;
end;

function TelDPad.Up(): Boolean;
begin
  if ActiveWindow.JoyHat[HAT_UP] then Result := true
    else Result := false;
end;

function TelDPad.Right(): Boolean;
begin
  if ActiveWindow.JoyHat[HAT_RIGHT] then Result := true
    else Result := false;
end;

function TelDPad.Down(): Boolean;
begin
  if ActiveWindow.JoyHat[HAT_DOWN] then Result := true
    else Result := false;
end;

function TelDPad.Left(): Boolean;
begin
  if ActiveWindow.JoyHat[HAT_LEFT] then Result := true
    else Result := false;
end;

function TelDPad.RightUp(): Boolean;
begin
  if ActiveWindow.JoyHat[HAT_RIGHTUP] then Result := true
    else Result := false;
end;

function TelDPad.RightDown(): Boolean;
begin
  if ActiveWindow.JoyHat[HAT_RIGHTDOWN] then Result := true
    else Result := false;
end;

function TelDPad.LeftUp(): Boolean;
begin
  if ActiveWindow.JoyHat[HAT_LEFTUP] then Result := true
    else Result := false;
end;

function TelDPad.LeftDown(): Boolean;
begin
  if ActiveWindow.JoyHat[HAT_LEFTDOWN] then Result := true
    else Result := false;
end;

constructor TelXBox360Controller.Create;
begin
  inherited;

  DPad := TelDPad.Create;
  
  {$IFDEF WINDOWS}
  fControllerName := 'XBOX';
  {$ELSE}
  fControllerName := 'X-BOX';
  {$ENDIF}
end;

destructor TelXBox360Controller.Destroy;
begin
  DPad.Destroy;

  inherited;
end;

function TelXBox360Controller.GetConnected(): Boolean;
begin
  if AnsiPos(fControllerName, UpperCase(Input.JoystickName)) > 0 then Result := true
    else Result := false;
end;

function TelXBox360Controller.GetLStick(): TelAnalogStick;
var
  tmpStick: TelAnalogStick;
begin
  tmpStick.AxisH := ActiveWindow.JoyAxis[0] / AXIS_MAX;
  tmpStick.AxisV := ActiveWindow.JoyAxis[1] / AXIS_MAX;
  
  if (tmpStick.AxisH < -0.25) then tmpStick.Left := true
    else tmpStick.Left := false;
    
  if (tmpStick.AxisH > 0.25) then tmpStick.Right := true
    else tmpStick.Right := false;


  if (tmpStick.AxisV < -0.25) then tmpStick.Up := true
    else tmpStick.Up := false;
    
  if (tmpStick.AxisV > 0.25) then tmpStick.Down := true
    else tmpStick.Down := false;
  
    
  tmpStick.Motion := tmpStick.Left or tmpStick.Right or tmpStick.Up or tmpStick.Down;

  tmpStick.Click := Input.Joystick.IsButtonHit(8);

  Result := tmpStick;
end;

function TelXBox360Controller.GetRStick(): TelAnalogStick;
var
  tmpStick: TelAnalogStick;
begin
  {$IFDEF WINDOWS}
  tmpStick.AxisH := ActiveWindow.JoyAxis[4] / AXIS_MAX;
  tmpStick.AxisV := ActiveWindow.JoyAxis[3] / AXIS_MAX;
  {$ELSE}
  tmpStick.AxisH := ActiveWindow.JoyAxis[3] / AXIS_MAX;
  tmpStick.AxisV := ActiveWindow.JoyAxis[4] / AXIS_MAX;
  {$ENDIF}

  if (tmpStick.AxisH < -0.25) then tmpStick.Left := true
    else tmpStick.Left := false;
    
  if (tmpStick.AxisH > 0.25) then tmpStick.Right := true
    else tmpStick.Right := false;


  if (tmpStick.AxisV < -0.25) then tmpStick.Up := true
    else tmpStick.Up := false;
    
  if (tmpStick.AxisV > 0.25) then tmpStick.Down := true
    else tmpStick.Down := false;
  
    
  tmpStick.Motion := tmpStick.Left or tmpStick.Right or tmpStick.Up or tmpStick.Down;

  tmpStick.Click := Input.Joystick.IsButtonHit(9);

  Result := tmpStick;
end;

function TelXBox360Controller.GetTrigger(): TelAnalogStick;
var
  tmpStick: TelAnalogStick;
begin
  tmpStick.AxisH := ActiveWindow.JoyAxis[2] / AXIS_MAX;

  if (tmpStick.AxisH < -0.25) then tmpStick.Left := true
    else tmpStick.Left := false;
    
  if (tmpStick.AxisH > 0.25) then tmpStick.Right := true
    else tmpStick.Right := false;
    

  tmpStick.AxisV := 0;
  tmpStick.Up := false;
  tmpStick.Down := false;
  
  tmpStick.Motion := tmpStick.Left or tmpStick.Right;

  (*if (tmpStick.AxisV.Value < -0.25) then tmpStick.AxisV.Left := true
    else tmpStick.AxisV.Top := false;
    
  if (tmpStick.AxisV.Value > 0.25) then tmpStick.AxisV.Right := true
    else tmpStick.AxisV.Bottom := false;
  
  if tmpStick.AxisV.Top or tmpStick.AxisV.Bottom then tmpStick.AxisV.Relevance := true
    else tmpStick.AxisV.Relevance := false;*)

  tmpStick.Click := false;

  Result := tmpStick;
end;

function TelXBox360Controller.A(): Boolean;
begin
  Result := Input.Joystick.IsButtonHit(0);
end;

function TelXBox360Controller.B(): Boolean;
begin
  Result := Input.Joystick.IsButtonHit(1);
end;

function TelXBox360Controller.X(): Boolean;
begin
  Result := Input.Joystick.IsButtonHit(2);
end;

function TelXBox360Controller.Y(): Boolean;
begin
  Result := Input.Joystick.IsButtonHit(3);
end;

function TelXBox360Controller.Start(): Boolean;
begin
  Result := Input.Joystick.IsButtonHit(7);
end;

function TelXBox360Controller.Back(): Boolean;
begin
  Result := Input.Joystick.IsButtonHit(6);
end;

function TelXBox360Controller.LButton(): Boolean;
begin
  Result := Input.Joystick.IsButtonHit(4);
end;

function TelXBox360Controller.LTrigger(): Boolean;
begin
  if GetTrigger().AxisH > 0.8 then Result := true
    else Result := false;
end;

function TelXBox360Controller.RButton(): Boolean;
begin
  Result := Input.Joystick.IsButtonHit(5);
end;

function TelXBox360Controller.RTrigger(): Boolean;
begin
  if GetTrigger().AxisH < -0.8 then Result := true
    else Result := false;
end;

constructor TelPlaystationController.Create;
begin
  inherited;

  DPad := TelDPad.Create;
end;

destructor TelPlaystationController.Destroy;
begin
  DPad.Destroy;

  inherited;
end;

function TelPlaystationController.Triangle(): Boolean;
begin

end;

function TelPlaystationController.Square(): Boolean;
begin

end;

function TelPlaystationController.Circle(): Boolean;
begin

end;

function TelPlaystationController.Cross(): Boolean;
begin

end;

function TelPlaystationController.Start(): Boolean;
begin

end;

function TelPlaystationController.Select(): Boolean;
begin

end;

function TelPlaystationController.LButton(): Boolean;
begin

end;

function TelPlaystationController.LTrigger(): Boolean;
begin

end;

function TelPlaystationController.RButton(): Boolean;
begin
  Result := Input.Joystick.IsButtonHit(5);
end;

function TelPlaystationController.RTrigger(): Boolean;
begin

end;

constructor TelGP2XController.Create;
begin
  inherited;

  DPad := TelDPad.Create;
end;

destructor TelGP2XController.Destroy;
begin
  DPad.Destroy;

  inherited;
end;

function TelGP2XController.GetConnected(): Boolean;
begin
  {$IFDEF FPC}
    {$Note Check for GP2X Joystick name}
  {$ENDIF}
end;

function TelGP2XController.A(): Boolean;
begin

end;

function TelGP2XController.B(): Boolean;
begin

end;

function TelGP2XController.X(): Boolean;
begin

end;

function TelGP2XController.Y(): Boolean;
begin

end;

function TelGP2XController.Menu(): Boolean;
begin

end;

function TelGP2XController.Select(): Boolean;
begin

end;

function TelGP2XController.LButton(): Boolean;
begin

end;

function TelGP2XController.RButton(): Boolean;
begin

end;




constructor TelMouseHelper.Create;
begin
  inherited Create;

  fDblClickInterval := 500;
  fDblClicked := false;
end;

destructor TelMouseHelper.Destroy;
begin
  inherited Destroy;
end;

function TelMouseHelper.GetCursor(): TelVector2i;
begin
  Result := ActiveWindow.Cursor;
end;

function TelMouseHelper.GetRelCursor(): TelVector2i;
begin
  Result := ActiveWindow.RelCursor;
end;

function TelMouseHelper.isButtonDown(Button: LongWord): Boolean; 
begin
  Result := ActiveWindow.MouseButtonDown[Button];
end;

function TelMouseHelper.isButtonHit(Button: LongWord): Boolean; 
begin
  if ActiveWindow.MouseButtonDown[Button] then
  begin
	ActiveWindow.MouseButtonDown[Button] := false;
	Result := true;
  end else Result := false;
end;

function TelMouseHelper.isButtonUp(Button: LongWord): Boolean; 
begin
  Result := ActiveWindow.MouseButtonUp[Button];
end;

function TelMouseHelper.LeftClick: Boolean; 
begin
  Result := Self.Up();
end;

function TelMouseHelper.RightClick: Boolean; 
begin
  Result := Self.Up(BUTTON_RIGHT);
end;

function TelMouseHelper.Down(Button: LongWord = BUTTON_LEFT): Boolean;
begin
  Result := IsButtonDown(Button);
end;

function TelMouseHelper.Up(Button: LongWord = BUTTON_LEFT): Boolean;
begin
  Result := ActiveWindow.MouseButtonUp[Button];
  if (ActiveWindow.MouseButtonUp[Button]) then
    ActiveWindow.MouseButtonUp[Button] := false;
end;

function TelMouseHelper.DblClick: Boolean;
var
  FirstClickEvent, SecondClickEvent: PelButtonEvent;
  FirstClick, SecondClick, Tolerance: Cardinal;
begin
  Result := false;

  if ActiveWindow.MouseButtonUpEventList.Count >= 2 then
  begin
    FirstClickEvent := ActiveWindow.MouseButtonUpEventList.Items[ActiveWindow.MouseButtonUpEventList.Count - 2];
    SecondClickEvent := ActiveWindow.MouseButtonUpEventList.Items[ActiveWindow.MouseButtonUpEventList.Count - 1];

    FirstClick := FirstClickEvent^.Called;
    SecondClick := SecondClickEvent^.Called;


    Tolerance := DblClickInterval div 10;

    if (((SecondClick - FirstClick) <= DblClickInterval) and
        (FirstClickEvent^.Position.X = SecondClickEvent^.Position.X) and
        (FirstClickEvent^.Position.Y = SecondClickEvent^.Position.Y) and
        ((SecondClickEvent^.Called + Tolerance) <= TicksNow)) then
        begin
          Result := true;
          TelLogger.GetInstance.WriteLog('Double clicked');
        end;

  end;

end;

function TelMouseHelper.WheelDown(): Boolean; 
begin
  Result := ActiveWindow.MouseButtonUp[BUTTON_WHEELDOWN];
end;

function TelMouseHelper.WheelUp(): Boolean; 
begin
  Result := ActiveWindow.MouseButtonUp[BUTTON_WHEELUP];
end;

function TelMouseHelper.Motion(): Boolean; 
begin
  Result := false;

  if ((Self.RelCursor.X <> 0) or (Self.RelCursor.Y <> 0)) then Result := true;
end;

function TelMouseHelper.Motion(Rect: TelRect): Boolean; 
begin
  Result := false;

  if (Rect.ContainsVector(Self.Cursor) and Motion()) then Result := true;
end;

{
  #############################################################################
  # TelInputHelper                                                            #
  #############################################################################

  Description:
    Handles keyboard, mouse and joystick inputs

  Additional Notes: -

}

constructor TelInputHelper.Create;
begin
  Keyboard := TelKeyboardHelper.Create;
  Joystick := TelJoystickHelper.Create;
  Mouse := TelMouseHelper.Create;
end;

destructor TelInputHelper.Destroy;
begin
  Keyboard.Destroy;
  Joystick.Destroy;
  Mouse.Destroy;
end;

function TelInputHelper.GetJoystickName(): String;
begin
  Result := SDL_JoystickName(0);
end;

function TelInputHelper.GetJoystickCount(): Integer;
begin
  Result := ActiveWindow.JoystickCount;
end;

procedure TelInputHelper.DebugInfo();
begin
  if IsLoggerActive then TelLogger.GetInstance.WriteLog('Joystick Count: ' + IntToStr(GetJoystickCount()), ltNote);
  if IsLoggerActive then TelLogger.GetInstance.WriteLog('Joystick Name: ' + GetJoystickName(), ltNote);
end;

constructor TelKeyCode.Create;
begin
  inherited;

  fKeyList := TStringList.Create;
end;

destructor TelKeyCode.Destroy;
begin
  fKeyList.Free;

  inherited;
end;

function TelKeyCode.BackSpace(): Cardinal; 
begin
  Result := K_BACKSPACE;
end;

function TelKeyCode.Tab(): Cardinal; 
begin
  Result := K_TAB;
end;

function TelKeyCode.Clear(): Cardinal; 
begin
  Result := K_CLEAR;
end;

function TelKeyCode.Return(): Cardinal; 
begin
  Result := K_RETURN;
end;

function TelKeyCode.Enter(): Cardinal; 
begin
  Result := Self.Return();
end;

function TelKeyCode.Pause(): Cardinal; 
begin
  Result := K_PAUSE;
end;

function TelKeyCode.Escape(): Cardinal; 
begin
  Result := K_ESCAPE;
end;

function TelKeyCode.Space(): Cardinal; 
begin
  Result := K_SPACE;
end;

function TelKeyCode.Exclaim(): Cardinal; 
begin
  Result := K_EXCLAIM;
end;

function TelKeyCode.QuoteDbl(): Cardinal; 
begin
  Result := K_QUOTEDBL;
end;

function TelKeyCode.Hash(): Cardinal; 
begin
  Result := K_HASH;
end;

function TelKeyCode.Dollar(): Cardinal; 
begin
  Result := K_DOLLAR;
end;

function TelKeyCode.Ampersand(): Cardinal; 
begin
  Result := K_AMPERSAND;
end;

function TelKeyCode.Quote(): Cardinal; 
begin
  Result := K_QUOTE;
end;

function TelKeyCode.LeftParen(): Cardinal; 
begin
  Result := K_LEFTPAREN;
end;

function TelKeyCode.RightParen(): Cardinal; 
begin
  Result := K_RIGHTPAREN;
end;

function TelKeyCode.Asterisk(): Cardinal; 
begin
  Result := K_ASTERISK;
end;

function TelKeyCode.Plus(): Cardinal; 
begin
  Result := K_PLUS;
end;

function TelKeyCode.Comma(): Cardinal; 
begin
  Result := K_COMMA;
end;

function TelKeyCode.Minus(): Cardinal; 
begin
  Result := K_MINUS;
end;

function TelKeyCode.Period(): Cardinal; 
begin
  Result := K_PERIOD;
end;

function TelKeyCode.Slash(): Cardinal; 
begin
  Result := K_SLASH;
end;

function TelKeyCode.Alpha0(): Cardinal; 
begin
  Result := K_0;
end;

function TelKeyCode.Alpha1(): Cardinal; 
begin
  Result := K_1;
end;

function TelKeyCode.Alpha2(): Cardinal; 
begin
  Result := K_2;
end;

function TelKeyCode.Alpha3(): Cardinal; 
begin
  Result := K_3;
end;

function TelKeyCode.Alpha4(): Cardinal; 
begin
  Result := K_4;
end;

function TelKeyCode.Alpha5(): Cardinal; 
begin
  Result := K_5;
end;

function TelKeyCode.Alpha6(): Cardinal; 
begin
  Result := K_6;
end;

function TelKeyCode.Alpha7(): Cardinal; 
begin
  Result := K_7;
end;

function TelKeyCode.Alpha8(): Cardinal; 
begin
  Result := K_8;
end;

function TelKeyCode.Alpha9(): Cardinal; 
begin
  Result := K_9;
end;

function TelKeyCode.Colon(): Cardinal; 
begin
  Result := K_COLON;
end;

function TelKeyCode.SemiColon(): Cardinal; 
begin
  Result := K_SEMICOLON;
end;

function TelKeyCode.Less(): Cardinal; 
begin
  Result := K_LESS;
end;

function TelKeyCode.Equals(): Cardinal; 
begin
  Result := K_EQUALS;
end;

function TelKeyCode.Greater(): Cardinal; 
begin
  Result := K_GREATER;
end;

function TelKeyCode.Question(): Cardinal; 
begin
  Result := K_QUESTION;
end;

function TelKeyCode.At(): Cardinal; 
begin
  Result := K_AT;
end;

function TelKeyCode.LeftBracket(): Cardinal; 
begin
  Result := K_LEFTBRACKET;
end;

function TelKeyCode.BackSlash(): Cardinal; 
begin
  Result := K_BACKSLASH;
end;

function TelKeyCode.RightBracket(): Cardinal; 
begin
  Result := K_RIGHTBRACKET;
end;

function TelKeyCode.Caret(): Cardinal; 
begin
  Result := K_CARET;
end;

function TelKeyCode.UnderScore(): Cardinal; 
begin
  Result := K_UNDERSCORE;
end;

function TelKeyCode.BackQuote(): Cardinal; 
begin
  Result := K_BACKQUOTE;
end;

function TelKeyCode.A(): Cardinal; 
begin
  Result := K_a;
end;

function TelKeyCode.B(): Cardinal; 
begin
  Result := K_b;
end;

function TelKeyCode.C(): Cardinal; 
begin
  Result := K_c;
end;

function TelKeyCode.D(): Cardinal; 
begin
  Result := K_d;
end;

function TelKeyCode.E(): Cardinal; 
begin
  Result := K_e;
end;

function TelKeyCode.F(): Cardinal; 
begin
  Result := K_f;
end;

function TelKeyCode.G(): Cardinal; 
begin
  Result := K_g;
end;

function TelKeyCode.H(): Cardinal; 
begin
  Result := K_h;
end;

function TelKeyCode.I(): Cardinal; 
begin
  Result := K_i;
end;

function TelKeyCode.J(): Cardinal; 
begin
  Result := K_j;
end;

function TelKeyCode.K(): Cardinal; 
begin
  Result := K_k;
end;

function TelKeyCode.L(): Cardinal; 
begin
  Result := K_l;
end;

function TelKeyCode.M(): Cardinal; 
begin
  Result := K_m;
end;

function TelKeyCode.N(): Cardinal; 
begin
  Result := K_n;
end;

function TelKeyCode.O(): Cardinal; 
begin
  Result := K_o;
end;

function TelKeyCode.P(): Cardinal; 
begin
  Result := K_p;
end;

function TelKeyCode.Q(): Cardinal; 
begin
  Result := K_q;
end;

function TelKeyCode.R(): Cardinal; 
begin
  Result := K_r;
end;

function TelKeyCode.S(): Cardinal; 
begin
  Result := K_s;
end;

function TelKeyCode.T(): Cardinal; 
begin
  Result := K_t;
end;

function TelKeyCode.U(): Cardinal; 
begin
  Result := K_u;
end;

function TelKeyCode.V(): Cardinal; 
begin
  Result := K_v;
end;

function TelKeyCode.W(): Cardinal; 
begin
  Result := K_w;
end;

function TelKeyCode.X(): Cardinal; 
begin
  Result := K_x;
end;

function TelKeyCode.Y(): Cardinal; 
begin
  Result := K_y;
end;

function TelKeyCode.Z(): Cardinal; 
begin
  Result := K_z;
end;

function TelKeyCode.Delete(): Cardinal; 
begin
  Result := K_DELETE;
end;

function TelKeyCode.NumPad0(): Cardinal; 
begin
  Result := K_KP0;
end;

function TelKeyCode.NumPad1(): Cardinal; 
begin
  Result := K_KP1;
end;

function TelKeyCode.NumPad2(): Cardinal; 
begin
  Result := K_KP2;
end;

function TelKeyCode.NumPad3(): Cardinal; 
begin
  Result := K_KP3;
end;

function TelKeyCode.NumPad4(): Cardinal; 
begin
  Result := K_KP4;
end;

function TelKeyCode.NumPad5(): Cardinal; 
begin
  Result := K_KP5;
end;

function TelKeyCode.NumPad6(): Cardinal; 
begin
  Result := K_KP6;
end;

function TelKeyCode.NumPad7(): Cardinal; 
begin
  Result := K_KP7;
end;

function TelKeyCode.NumPad8(): Cardinal; 
begin
  Result := K_KP8;
end;

function TelKeyCode.NumPad9(): Cardinal; 
begin
  Result := K_KP9;
end;

function TelKeyCode.NumPad_Period(): Cardinal; 
begin
  Result := K_KP_PERIOD;
end;

function TelKeyCode.NumPad_Divide(): Cardinal; 
begin
  Result := K_KP_DIVIDE;
end;

function TelKeyCode.NumPad_Multiply(): Cardinal; 
begin
  Result := K_KP_MULTIPLY;
end;

function TelKeyCode.NumPad_Minus(): Cardinal; 
begin
  Result := K_KP_MINUS;
end;

function TelKeyCode.NumPad_Plus(): Cardinal; 
begin
  Result := K_KP_PLUS;
end;

function TelKeyCode.NumPad_Enter(): Cardinal; 
begin
  Result := K_KP_ENTER;
end;

function TelKeyCode.NumPad_Equals(): Cardinal; 
begin
  Result := K_KP_EQUALS;
end;

function TelKeyCode.Up(): Cardinal; 
begin
  Result := K_UP;
end;

function TelKeyCode.Down(): Cardinal; 
begin
  Result := K_DOWN;
end;

function TelKeyCode.Right(): Cardinal; 
begin
  Result := K_RIGHT;
end;

function TelKeyCode.Left(): Cardinal; 
begin
  Result := K_LEFT;
end;

function TelKeyCode.Insert(): Cardinal; 
begin
  Result := K_INSERT;
end;

function TelKeyCode.PageHome(): Cardinal; 
begin
  Result := K_HOME;
end;

function TelKeyCode.PageEnd(): Cardinal; 
begin
  Result := K_END;
end;

function TelKeyCode.PageUp(): Cardinal; 
begin
  Result := K_PAGEUP;
end;

function TelKeyCode.PageDown(): Cardinal; 
begin
  Result := K_PAGEDOWN;
end;

function TelKeyCode.F1(): Cardinal; 
begin
  Result := K_F1;
end;

function TelKeyCode.F2(): Cardinal; 
begin
  Result := K_F2;
end;

function TelKeyCode.F3(): Cardinal; 
begin
  Result := K_F3;
end;

function TelKeyCode.F4(): Cardinal; 
begin
  Result := K_F4;
end;

function TelKeyCode.F5(): Cardinal; 
begin
  Result := K_F5;
end;

function TelKeyCode.F6(): Cardinal; 
begin
  Result := K_F6;
end;

function TelKeyCode.F7(): Cardinal; 
begin
  Result := K_F7;
end;

function TelKeyCode.F8(): Cardinal; 
begin
  Result := K_F8;
end;

function TelKeyCode.F9(): Cardinal; 
begin
  Result := K_F9;
end;

function TelKeyCode.F10(): Cardinal; 
begin
  Result := K_F10;
end;

function TelKeyCode.F11(): Cardinal; 
begin
  Result := K_F11;
end;

function TelKeyCode.F12(): Cardinal; 
begin
  Result := K_F12;
end;

function TelKeyCode.NumLock(): Cardinal; 
begin
  Result := K_NUMLOCK;
end;

function TelKeyCode.CapsLock(): Cardinal; 
begin
  Result := K_CAPSLOCK;
end;

function TelKeyCode.ScrolLock(): Cardinal; 
begin
  Result := K_SCROLLOCK;
end;

function TelKeyCode.RShift(): Cardinal; 
begin
  Result := RSHIFT;
end;

function TelKeyCode.LShift(): Cardinal; 
begin
  Result := K_LSHIFT;
end;

function TelKeyCode.RCtrl(): Cardinal; 
begin
  Result := K_RCTRL;
end;

function TelKeyCode.LCtrl(): Cardinal; 
begin
  Result := K_LCTRL;
end;

function TelKeyCode.RAlt(): Cardinal; 
begin
  Result := K_RALT;
end;

function TelKeyCode.LAlt(): Cardinal; 
begin
  Result := K_LALT;
end;

function TelKeyCode.RMeta(): Cardinal; 
begin
  Result := K_RMETA;
end;

function TelKeyCode.LMeta(): Cardinal; 
begin
  Result := K_LMETA;
end;

function TelKeyCode.LSuper(): Cardinal; 
begin
  Result := K_LSUPER;
end;

function TelKeyCode.RSuper(): Cardinal; 
begin
  Result := K_RSUPER;
end;

function TelKeyCode.Mode(): Cardinal;  // Alt Gr
begin
  Result := K_MODE;
end;

function TelKeyCode.Help(): Cardinal; 
begin
  Result := K_HELP;
end;

function TelKeyCode.Print(): Cardinal; 
begin
  Result := K_PRINT;
end;

function TelKeyCode.Break(): Cardinal; 
begin
  Result := K_BREAK;
end;

function TelKeyCode.Menu(): Cardinal; 
begin
  Result := K_MENU;
end;

function TelKeyCode.Euro(): Cardinal; 
begin
  Result := K_EURO;
end;

constructor TelButtonCode.Create;
begin
  inherited;
end;

destructor TelButtonCode.Destroy;
begin
  inherited;
end;

function TelButtonCode.Left(): Cardinal; 
begin
  Result := BUTTON_LEFT;
end;

function TelButtonCode.Right(): Cardinal; 
begin
  Result := BUTTON_RIGHT;
end;

function TelButtonCode.Middle(): Cardinal; 
begin
  Result := BUTTON_MIDDLE;
end;

function TelButtonCode.WheelUp(): Cardinal; 
begin
  Result := BUTTON_WHEELUP;
end;

function TelButtonCode.WheelDown(): Cardinal; 
begin
  Result := BUTTON_WHEELDOWN;
end;

{$IFDEF AUTO_INIT}
initialization
  Input := TelInputHelper.Create;
  Key := TelKeyCode.Create;
  Button := TelButtonCode.Create;

finalization
  Input.Destroy;
  Key.Destroy;
  Button.Destroy;
{$ENDIF}

end.
