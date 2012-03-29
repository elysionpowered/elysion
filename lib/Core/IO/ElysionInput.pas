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
  ElysionObject,
  ElysionTypes,
  ElysionLogger,
  ElysionKeyCode,

  SDL,
  SysUtils,
  Classes;

type

(*TelAxis = record
  Value: Single;
  Relevance: Boolean;
end;*)

{ TelAnalogStick }

TelAnalogStick = record
  AxisH: Single; //< Horizontal axis
  AxisV: Single; //< Vertical axis

  Click: Boolean; //< Stick click
  
  Motion: Boolean;
  
  Left, Right, Up, Down: Boolean;


  function ToVector2f(aMultiplicator: Single = 1.0): TelVector2f;
end;

// TODO: Add multiple joystick support
TelJoystick = class
  private
    fLastUsedButton: LongWord;

    function GetBall(): TelVector2i; inline;
  public
    constructor Create;
    destructor Destroy; Override;

    function IsButtonDown(Button: Cardinal): Boolean; inline;
    function IsButtonHit(Button: Cardinal): Boolean; inline;
    function IsButtonUp(Button: Cardinal): Boolean; inline;

    function GetLastUsedButton(): LongWord; inline;

    property Ball: TelVector2i read GetBall;
  published
    property LastUsedButton: LongWord read GetLastUsedButton;
end;


// DPad for GamePads
TelDPad = class
  private
    fUseJoyHat: Boolean;
    fSticky: Boolean;
  public
    constructor Create(UseJoyHat: Boolean = true);
    destructor Destroy; Override;

    function Centered(): Boolean; inline;
    function Up(): Boolean; inline;
    function Right(): Boolean; inline;
    function Down(): Boolean; inline;
    function Left(): Boolean; inline;
    function RightUp(): Boolean; inline;
    function RightDown(): Boolean; inline;
    function LeftUp(): Boolean; inline;
    function LeftDown(): Boolean; inline;
  published
    property Sticky: Boolean read fSticky write fSticky;
end;

ISimpleController = interface
  function IsConnected: Boolean;

  function Start(): Boolean;
  function Back(): Boolean;

  function A(): Boolean;
  function B(): Boolean;
  function X(): Boolean;
  function Y(): Boolean;

  function RButton(): Boolean;
  function LButton(): Boolean;
end;

IController = interface(ISimpleController)
  function GetLStick(): TelAnalogStick;
  function GetRStick(): TelAnalogStick;
  function GetDPad(): TelDPad;

  function LTrigger: Boolean;
  function RTrigger: Boolean;
end;

{ TelXBox360Controller }

TelXBox360Controller = class(TelObject, IController)
  private
    fControllerName: String;

    function GetTrigger(): TelAnalogStick; inline;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    function A(): Boolean; inline;
    function B(): Boolean; inline;
    function X(): Boolean; inline;
    function Y(): Boolean; inline;

    function BigButton(): Boolean; inline;

    function Start(): Boolean; inline;
    function Back(): Boolean; inline;

    function LButton(): Boolean; inline;
    function LTrigger(): Boolean; inline;

    function RButton(): Boolean; inline;
    function RTrigger(): Boolean; inline;

    function IsConnected: Boolean; inline;

    function GetLStick(): TelAnalogStick; inline;
    function GetRStick(): TelAnalogStick; inline;

    function GetDPad(): TelDPad;
  public
    property LStick: TelAnalogStick read GetLStick;
    property RStick: TelAnalogStick read GetRStick;
  published
    property DPad: TelDPad read GetDPad;

    property Connected: Boolean read IsConnected;
end;

(*TelPlaystationController = class
  private
    fDPad: TelDPad;
  public
    constructor Create;
    destructor Destroy; Override;

    function Triangle(): Boolean; inline;
    function Square(): Boolean; inline;
    function Circle(): Boolean; inline;
    function Cross(): Boolean; inline;

    function Select(): Boolean; inline;
    function Start(): Boolean; inline;

    function LButton(): Boolean; inline;
    function LTrigger(): Boolean; inline;

    function RButton(): Boolean; inline;
    function RTrigger(): Boolean; inline;
  published
    property DPad: TelDPad read fDPad write fDPad;
end;

TelGP2XController = class
  private
    fDPad: TelDPad;

    function GetConnected(): Boolean; inline;
  public
    constructor Create;
    destructor Destroy; Override;

    function A(): Boolean; inline;
    function B(): Boolean; inline;
    function X(): Boolean; inline;
    function Y(): Boolean; inline;

    function Menu(): Boolean; inline;
    function Select(): Boolean; inline;

    function LButton(): Boolean; inline;

    function RButton(): Boolean; inline;
  published
    property IsConnected: Boolean read GetConnected;

    property DPad: TelDPad read fDPad write fDPad;
end;*)


{ TelInput }

TelInput = class sealed
  strict private type
    TelKeyboard = class sealed
      private
        fLastUsedKey: LongWord;
      public
        constructor Create;
        destructor Destroy; Override;

        function IsKeyDown(Key: Cardinal): Boolean; inline;
        function IsKeyHit(Key: Cardinal): Boolean; inline;
        function IsKeyUp(Key: Cardinal): Boolean; inline;

        function GetLastUsedKey: LongWord; inline;
      published
        property LastUsedKey: LongWord read GetLastUsedKey;
    end;

    TelMouse = class sealed
      private
        fDblClickInterval: Integer;
        fDblClicked: Boolean;

        function GetCursor(): TelVector2i; inline;
        function GetRelCursor(): TelVector2i; inline;
      public
        constructor Create;
        destructor Destroy; Override;

        function IsButtonDown(Button: LongWord): Boolean; inline;
        function IsButtonHit(Button: LongWord): Boolean; inline;
        function IsButtonUp(Button: LongWord): Boolean; inline;

        function LeftClick(): Boolean; inline;
        function RightClick(): Boolean; inline;

        function Down(Button: LongWord = BUTTON_LEFT): Boolean; inline;
        function Up(Button: LongWord = BUTTON_LEFT): Boolean; inline;

        function DblClick(): Boolean; inline;

        function WheelDown(): Boolean; inline;
        function WheelUp(): Boolean; inline;

        function Motion(): Boolean; Overload; inline;
        function Motion(Rect: TelRect): Boolean; Overload; inline;


        property Cursor: TelVector2i read GetCursor;
        property RelCursor: TelVector2i read GetRelCursor;
      published
        property DblClickInterval: Integer read fDblClickInterval write fDblClickInterval;
    end;

  private
    fKeyboard: TelKeyboard;
    fJoystick: TelJoystick;
    fMouse: TelMouse;

    fXBox360Controller: TelXBox360Controller;

    function GetJoystickName(): AnsiString; inline;
    function GetJoystickCount(): Integer; inline;
  public
    constructor Create;
    destructor Destroy; Override;

    procedure DebugInfo();
  published
    property Keyboard: TelKeyboard read fKeyboard write fKeyboard;
    property Joystick: TelJoystick read fJoystick write fJoystick;
    property Mouse: TelMouse read fMouse write fMouse;
    property XBox360Controller: TelXBox360Controller read fXBox360Controller write fXBox360Controller;

    property JoystickName: AnsiString read GetJoystickName;
    property JoystickCount: Integer read GetJoystickCount;
end;

TelKeyCode = class
  private
    fKeyList: TStringList;
  public
    constructor Create;
    destructor Destroy; Override;

    function BackSpace(): Cardinal; inline;
    function Tab(): Cardinal; inline;
    function Clear(): Cardinal; inline;
    function Return(): Cardinal; inline;
    function Enter(): Cardinal; inline;
    function Pause(): Cardinal; inline;
    function Escape(): Cardinal; inline;
    function Space(): Cardinal; inline;
    function Exclaim(): Cardinal; inline;
    function QuoteDbl(): Cardinal; inline;
    function Hash(): Cardinal; inline;
    function Dollar(): Cardinal; inline;
    function Ampersand(): Cardinal; inline;
    function Quote(): Cardinal; inline;
    function LeftParen(): Cardinal; inline;
    function RightParen(): Cardinal; inline;
    function Asterisk(): Cardinal; inline;
    function Plus(): Cardinal; inline;
    function Comma(): Cardinal; inline;
    function Minus(): Cardinal; inline;
    function Period(): Cardinal; inline;
    function Slash(): Cardinal; inline;
    function Alpha0(): Cardinal; inline;
    function Alpha1(): Cardinal; inline;
    function Alpha2(): Cardinal; inline;
    function Alpha3(): Cardinal; inline;
    function Alpha4(): Cardinal; inline;
    function Alpha5(): Cardinal; inline;
    function Alpha6(): Cardinal; inline;
    function Alpha7(): Cardinal; inline;
    function Alpha8(): Cardinal; inline;
    function Alpha9(): Cardinal; inline;
    function Colon(): Cardinal; inline;
    function SemiColon(): Cardinal; inline;
    function Less(): Cardinal; inline;
    function Equals(): Cardinal; Overload;
    function Greater(): Cardinal; inline;
    function Question(): Cardinal; inline;
    function At(): Cardinal; inline;
    function LeftBracket(): Cardinal; inline;
    function BackSlash(): Cardinal; inline;
    function RightBracket(): Cardinal; inline;
    function Caret(): Cardinal; inline;
    function UnderScore(): Cardinal; inline;
    function BackQuote(): Cardinal; inline;
    function A(): Cardinal; inline;
    function B(): Cardinal; inline;
    function C(): Cardinal; inline;
    function D(): Cardinal; inline;
    function E(): Cardinal; inline;
    function F(): Cardinal; inline;
    function G(): Cardinal; inline;
    function H(): Cardinal; inline;
    function I(): Cardinal; inline;
    function J(): Cardinal; inline;
    function K(): Cardinal; inline;
    function L(): Cardinal; inline;
    function M(): Cardinal; inline;
    function N(): Cardinal; inline;
    function O(): Cardinal; inline;
    function P(): Cardinal; inline;
    function Q(): Cardinal; inline;
    function R(): Cardinal; inline;
    function S(): Cardinal; inline;
    function T(): Cardinal; inline;
    function U(): Cardinal; inline;
    function V(): Cardinal; inline;
    function W(): Cardinal; inline;
    function X(): Cardinal; inline;
    function Y(): Cardinal; inline;
    function Z(): Cardinal; inline;
    function Delete(): Cardinal; inline;
    function NumPad0(): Cardinal; inline;
    function NumPad1(): Cardinal; inline;
    function NumPad2(): Cardinal; inline;
    function NumPad3(): Cardinal; inline;
    function NumPad4(): Cardinal; inline;
    function NumPad5(): Cardinal; inline;
    function NumPad6(): Cardinal; inline;
    function NumPad7(): Cardinal; inline;
    function NumPad8(): Cardinal; inline;
    function NumPad9(): Cardinal; inline;
    function NumPad_Period(): Cardinal; inline;
    function NumPad_Divide(): Cardinal; inline;
    function NumPad_Multiply(): Cardinal; inline;
    function NumPad_Minus(): Cardinal; inline;
    function NumPad_Plus(): Cardinal; inline;
    function NumPad_Enter(): Cardinal; inline;
    function NumPad_Equals(): Cardinal; inline;
    function Up(): Cardinal; inline;
    function Down(): Cardinal; inline;
    function Right(): Cardinal; inline;
    function Left(): Cardinal; inline;
    function Insert(): Cardinal; inline;
    function PageHome(): Cardinal; inline;
    function PageEnd(): Cardinal; inline;
    function PageUp(): Cardinal; inline;
    function PageDown(): Cardinal; inline;
    function F1(): Cardinal; inline;
    function F2(): Cardinal; inline;
    function F3(): Cardinal; inline;
    function F4(): Cardinal; inline;
    function F5(): Cardinal; inline;
    function F6(): Cardinal; inline;
    function F7(): Cardinal; inline;
    function F8(): Cardinal; inline;
    function F9(): Cardinal; inline;
    function F10(): Cardinal; inline;
    function F11(): Cardinal; inline;
    function F12(): Cardinal; inline;
    function NumLock(): Cardinal; inline;
    function CapsLock(): Cardinal; inline;
    function ScrolLock(): Cardinal; inline;
    function RShift(): Cardinal; inline;
    function LShift(): Cardinal; inline;
    function RCtrl(): Cardinal; inline;
    function LCtrl(): Cardinal; inline;
    function RAlt(): Cardinal; inline;
    function LAlt(): Cardinal; inline;
    function RMeta(): Cardinal; inline;
    function LMeta(): Cardinal; inline;
    function LSuper(): Cardinal; inline;
    function RSuper(): Cardinal; inline;
    function Mode(): Cardinal; inline; // Alt Gr
    function Help(): Cardinal; inline;
    function Print(): Cardinal; inline;
    function Break(): Cardinal; inline;
    function Menu(): Cardinal; inline;
    function Euro(): Cardinal; inline;

    //procedure SetCustomKeyCode(aKeyCode: AnsiString; KeyAscii: Cardinal);
    //function GetCustomKeyCode(aKeyCode: AnsiString): Cardinal;
end;

TelButtonCode = class
  public
    constructor Create;
    destructor Destroy; Override;

    function Left(): Cardinal; inline;
    function Right(): Cardinal; inline;
    function Middle(): Cardinal; inline;
    function WheelUp(): Cardinal; inline;
    function WheelDown(): Cardinal; inline;

    //procedure SetCustomButton
end;

{$IFDEF AUTO_INIT}
var
  Input: TelInput;
  Key: TelKeyCode;
  Button: TelButtonCode;
{$ENDIF}

implementation

uses
  ElysionApplication;

{ TelAnalogStick }

function TelAnalogStick.ToVector2f(aMultiplicator: Single = 1.0): TelVector2f;
begin
  Result := makeV2f(Self.AxisH * aMultiplicator, Self.AxisV * aMultiplicator);
end;

constructor TelInput.TelKeyboard.Create;
begin
  inherited Create;
end;

destructor TelInput.TelKeyboard.Destroy;
begin
  FLastUsedKey := 0;

  inherited Destroy;
end;

function TelInput.TelKeyboard.IsKeyDown(Key: Cardinal): Boolean;
begin
  fLastUsedKey := Key;

  if SDL_GetModState = Key then Result := ActiveWindow.ModKeyDown[Key]
                           else Result := ActiveWindow.KeyDown[Key];

end;

function TelInput.TelKeyboard.IsKeyHit(Key: Cardinal): Boolean;
begin
  fLastUsedKey := Key;

  if isKeyDown(Key) then
  begin
    Result := true;
    if SDL_GetModState = Key then ActiveWindow.ModKeyDown[Key] := false
    else ActiveWindow.KeyDown[Key] := false;
  end else Result := false;
end;

function TelInput.TelKeyboard.IsKeyUp(Key: Cardinal): Boolean;
begin
  fLastUsedKey := Key;

  if SDL_GetModState = Key then Result := ActiveWindow.ModKeyUp[Key]
                           else Result := ActiveWindow.KeyUp[Key];

end;

function TelInput.TelKeyboard.GetLastUsedKey(): LongWord;
begin
  Result := fLastUsedKey;
end;

constructor TelJoystick.Create;
begin
  inherited Create;
end;

destructor TelJoystick.Destroy;
begin
  inherited Destroy;
end;

function TelJoystick.GetLastUsedButton(): LongWord;
begin
  Result := fLastUsedButton;
end;

function TelJoystick.GetBall(): TelVector2i;
begin
  Result := ActiveWindow.JoyBall;
end;

function TelJoystick.IsButtonDown(Button: Cardinal): Boolean;
begin
  fLastUsedButton := Button;

  Result := ActiveWindow.JoyButtonDown[Button];
end;

function TelJoystick.IsButtonHit(Button: Cardinal): Boolean;
begin
  fLastUsedButton := Button;

  if ActiveWindow.JoyButtonDown[Button] then
  begin
	ActiveWindow.JoyButtonDown[Button] := false;
	Result := true;
  end else Result := false;
end;

function TelJoystick.IsButtonUp(Button: Cardinal): Boolean;
begin
  fLastUsedButton := Button;

  Result := ActiveWindow.JoyButtonUp[Button];
end;

constructor TelDPad.Create(UseJoyHat: Boolean = true);
begin
  fSticky := true;
  fUseJoyHat := UseJoyHat;
end;

destructor TelDPad.Destroy;
begin
  inherited;
end;

function TelDPad.Centered(): Boolean;
begin
  Result := false;

  (*if fUseJoyHat then
  begin
    if not Sticky then
    begin
      if ActiveWindow.JoyHat[HAT_CENTERED] then
      begin
        Result := true;
        ActiveWindow.JoyHat[HAT_CENTERED] := false;
      end;
    end else
      Result := ActiveWindow.JoyHat[HAT_CENTERED];
  end;*)
end;

function TelDPad.Up(): Boolean;
begin
  Result := false;

  (*if fUseJoyHat then
  begin
    if not Sticky then
    begin
      if ActiveWindow.JoyHat[HAT_UP] then
      begin
        Result := true;
        ActiveWindow.JoyHat[HAT_UP] := false;
      end;
    end else
      Result := ActiveWindow.JoyHat[HAT_UP];
  end else
  begin
    if Sticky then Result := Input.Joystick.IsButtonDown(0)
  else Result := Input.Joystick.IsButtonHit(0);
  end;*)
end;

function TelDPad.Right(): Boolean;
begin
  Result := false;

  (*if fUseJoyHat then
  begin
    if not Sticky then
    begin
      if ActiveWindow.JoyHat[HAT_RIGHT] then
      begin
        Result := true;
        ActiveWindow.JoyHat[HAT_RIGHT] := false;
      end;
    end else
      Result := ActiveWindow.JoyHat[HAT_RIGHT];
  end else
  begin
    if Sticky then Result := Input.Joystick.IsButtonDown(3)
    else Result := Input.Joystick.IsButtonHit(3);
  end;*)
end;

function TelDPad.Down(): Boolean;
begin
  Result := false;

  (*if fUseJoyHat then
  begin
    if not Sticky then
    begin
      if ActiveWindow.JoyHat[HAT_DOWN] then
      begin
        Result := true;
        ActiveWindow.JoyHat[HAT_DOWN] := false;
      end;
    end else
      Result := ActiveWindow.JoyHat[HAT_DOWN];
  end else
  begin
    if Sticky then Result := Input.Joystick.IsButtonDown(1)
    else Result := Input.Joystick.IsButtonHit(1);
  end;*)
end;

function TelDPad.Left(): Boolean;
begin
  Result := false;

  (*if fUseJoyHat then
  begin
    if not Sticky then
    begin
      if ActiveWindow.JoyHat[HAT_LEFT] then
      begin
        Result := true;
        ActiveWindow.JoyHat[HAT_LEFT] := false;
      end;
    end else
      Result := ActiveWindow.JoyHat[HAT_LEFT];
  end else
  begin
    if Sticky then Result := Input.Joystick.IsButtonDown(2)
    else Result := Input.Joystick.IsButtonHit(2);
  end;*)
end;

function TelDPad.RightUp(): Boolean;
begin
  Result := false;

  (*if fUseJoyHat then
  begin
    if not Sticky then
    begin
      if ActiveWindow.JoyHat[HAT_RIGHTUP] then
      begin
        Result := true;
        ActiveWindow.JoyHat[HAT_RIGHTUP] := false;
      end;
    end else
      Result := ActiveWindow.JoyHat[HAT_RIGHTUP];
  end;*)
end;

function TelDPad.RightDown(): Boolean;
begin
  Result := false;

  (*if fUseJoyHat then
  begin
    if not Sticky then
    begin
      if ActiveWindow.JoyHat[HAT_RIGHTDOWN] then
      begin
        Result := true;
        ActiveWindow.JoyHat[HAT_RIGHTDOWN] := false;
      end;
    end else
      Result := ActiveWindow.JoyHat[HAT_RIGHTDOWN];
  end;*)
end;

function TelDPad.LeftUp(): Boolean;
begin
  Result := false;

  (*if fUseJoyHat then
  begin
    if not Sticky then
    begin
      if ActiveWindow.JoyHat[HAT_LEFTUP] then
      begin
        Result := true;
        ActiveWindow.JoyHat[HAT_LEFTUP] := false;
      end;
    end else
      Result := ActiveWindow.JoyHat[HAT_LEFTUP];
  end;*)
end;

function TelDPad.LeftDown(): Boolean;
begin
  Result := false;

  (*if fUseJoyHat then
  begin
    if not Sticky then
    begin
      if ActiveWindow.JoyHat[HAT_LEFTDOWN] then
      begin
        Result := true;
        ActiveWindow.JoyHat[HAT_LEFTDOWN] := false;
      end;
    end else
      Result := ActiveWindow.JoyHat[HAT_LEFTDOWN];
  end;*)
end;

constructor TelXBox360Controller.Create;
begin
  inherited;

  //fDPad := TelDPad.Create({$IFDEF WINDOWS} true {$ELSE} false {$ENDIF});

  // You cannot define variables for keycodes and axis! It will result in evil access violations
  // Keycodes/Axis have to be constants!
  
  {$IFDEF WINDOWS}
  fControllerName := 'XBOX';

  (*fKeyCodeA := 0;
  fKeyCodeB := 1;
  fKeyCodeX := 2;
  fKeyCodeY := 3;
  fKeyCodeStart := 7;
  fKeyCodeBack := 6;
  fKeyCodeLB := 4;
  fKeyCodeRB := 5;
  fKeyCodeBigButton := 10;

  fLAxisH := 0;
  fLAxisV := 1;
  fRAxisH := 4;
  fRAxisV := 3;

  fAxisTrigger := 2;
  fLStickClick := 8;
  fRStickClick := 9;*)
  {$ELSE}
  fControllerName := 'Controller';

  // Mac OS X (needs Tattle Bogle Drivers)
  (*fKeyCodeA := 16;
  fKeyCodeB := 17;
  fKeyCodeX := 18;
  fKeyCodeY := 19;
  fKeyCodeStart := 9;
  fKeyCodeBack := 10;
  fKeyCodeLB := 13;
  fKeyCodeRB := 14;
  fKeyCodeBigButton := 15;

  fLAxisH := 0;
  fLAxisV := 1;
  fRAxisH := 3;
  fRAxisV := 4;

  fAxisTrigger := 2;
  fLStickClick := 11;
  fRStickClick := 12;*)
  {$ENDIF}
end;

destructor TelXBox360Controller.Destroy;
begin
  //fDPad.Destroy;

  inherited;
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

  {$IFDEF WINDOWS}
  tmpStick.Click := Input.Joystick.IsButtonHit(8);
  {$ELSE}
  tmpStick.Click := Input.Joystick.IsButtonHit(6);
  {$ENDIF}

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
  tmpStick.AxisH := ActiveWindow.JoyAxis[2] / AXIS_MAX;
  tmpStick.AxisV := ActiveWindow.JoyAxis[3] / AXIS_MAX;
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

  {$IFDEF WINDOWS}
  tmpStick.Click := Input.Joystick.IsButtonHit(9);
  {$ELSE}
  tmpStick.Click := Input.Joystick.IsButtonHit(7);
  {$ENDIF}

  Result := tmpStick;
end;

function TelXBox360Controller.GetDPad: TelDPad;
begin

end;

function TelXBox360Controller.GetTrigger(): TelAnalogStick;
var
  tmpStick: TelAnalogStick;
begin
  {$IFDEF WINDOWS}
  tmpStick.AxisH := ActiveWindow.JoyAxis[2] / AXIS_MAX;
  {$ELSE}
  tmpStick.AxisH := ActiveWindow.JoyAxis[4] / AXIS_MAX;
  tmpStick.AxisV := ActiveWindow.JoyAxis[5] / AXIS_MAX;
  {$ENDIF}

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
  {$IFDEF WINDOWS}
  Result := Input.Joystick.IsButtonHit(0);
  {$ELSE}
  Result := Input.Joystick.IsButtonHit(11);
  {$ENDIF}
end;

function TelXBox360Controller.B(): Boolean;
begin
  {$IFDEF WINDOWS}
  Result := Input.Joystick.IsButtonHit(1);
  {$ELSE}
  Result := Input.Joystick.IsButtonHit(12);
  {$ENDIF}
end;

function TelXBox360Controller.X(): Boolean;
begin
  {$IFDEF WINDOWS}
  Result := Input.Joystick.IsButtonHit(2);
  {$ELSE}
  Result := Input.Joystick.IsButtonHit(13);
  {$ENDIF}
end;

function TelXBox360Controller.Y(): Boolean;
begin
  {$IFDEF WINDOWS}
  Result := Input.Joystick.IsButtonHit(3);
  {$ELSE}
  Result := Input.Joystick.IsButtonHit(14);
  {$ENDIF}
end;

function TelXBox360Controller.BigButton(): Boolean;
begin
  {$IFDEF WINDOWS}
  Result := Input.Joystick.IsButtonHit(10);
  {$ELSE}
  Result := Input.Joystick.IsButtonHit(10);
  {$ENDIF}
end;

function TelXBox360Controller.Start(): Boolean;
begin
  {$IFDEF WINDOWS}
  Result := Input.Joystick.IsButtonHit(9);
  {$ELSE}
  Result := Input.Joystick.IsButtonHit(4);
  {$ENDIF}
end;

function TelXBox360Controller.Back(): Boolean;
begin
  {$IFDEF WINDOWS}
  Result := Input.Joystick.IsButtonHit(10);
  {$ELSE}
  Result := Input.Joystick.IsButtonHit(5);
  {$ENDIF}
end;

function TelXBox360Controller.LButton(): Boolean;
begin
  {$IFDEF WINDOWS}
  Result := Input.Joystick.IsButtonHit(4);
  {$ELSE}
  Result := Input.Joystick.IsButtonHit(8);
  {$ENDIF}
end;

function TelXBox360Controller.LTrigger(): Boolean;
begin
  if GetTrigger().AxisH > 0.8 then Result := true
    else Result := false;
end;

function TelXBox360Controller.RButton(): Boolean;
begin
  {$IFDEF WINDOWS}
  Result := Input.Joystick.IsButtonHit(5);
  {$ELSE}
  Result := Input.Joystick.IsButtonHit(9);
  {$ENDIF}
end;

function TelXBox360Controller.RTrigger(): Boolean;
begin
  if GetTrigger().AxisH < -0.8 then Result := true
    else Result := false;
end;

function TelXBox360Controller.IsConnected: Boolean;
begin
  Result := (AnsiPos(fControllerName, UpperCase(Input.JoystickName)) > 0);
end;

(*constructor TelPlaystationController.Create;
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

end;*)




constructor TelInput.TelMouse.Create;
begin
  inherited Create;

  fDblClickInterval := 500;
  fDblClicked := false;
end;

destructor TelInput.TelMouse.Destroy;
begin
  inherited Destroy;
end;

function TelInput.TelMouse.GetCursor(): TelVector2i;
begin
  Result := ActiveWindow.Cursor;
end;

function TelInput.TelMouse.GetRelCursor(): TelVector2i;
begin
  Result := ActiveWindow.RelCursor;
end;

function TelInput.TelMouse.isButtonDown(Button: LongWord): Boolean;
begin
  Result := ActiveWindow.MouseButtonDown[Button];
end;

function TelInput.TelMouse.isButtonHit(Button: LongWord): Boolean;
begin
  if ActiveWindow.MouseButtonDown[Button] then
  begin
	ActiveWindow.MouseButtonDown[Button] := false;
	Result := true;
  end else Result := false;
end;

function TelInput.TelMouse.isButtonUp(Button: LongWord): Boolean;
begin
  Result := ActiveWindow.MouseButtonUp[Button];
end;

function TelInput.TelMouse.LeftClick: Boolean;
begin
  Result := Self.Up();
end;

function TelInput.TelMouse.RightClick: Boolean;
begin
  Result := Self.Up(BUTTON_RIGHT);
end;

function TelInput.TelMouse.Down(Button: LongWord = BUTTON_LEFT): Boolean;
begin
  Result := IsButtonDown(Button);
end;

function TelInput.TelMouse.Up(Button: LongWord = BUTTON_LEFT): Boolean;
begin
  Result := ActiveWindow.MouseButtonUp[Button];
  if (ActiveWindow.MouseButtonUp[Button]) then
    ActiveWindow.MouseButtonUp[Button] := false;
end;

function TelInput.TelMouse.DblClick: Boolean;
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

function TelInput.TelMouse.WheelDown(): Boolean;
begin
  Result := ActiveWindow.MouseButtonUp[BUTTON_WHEELDOWN];
end;

function TelInput.TelMouse.WheelUp(): Boolean;
begin
  Result := ActiveWindow.MouseButtonUp[BUTTON_WHEELUP];
end;

function TelInput.TelMouse.Motion(): Boolean;
begin
  Result := false;

  if ((Self.RelCursor.X <> 0) or (Self.RelCursor.Y <> 0)) then Result := true;
end;

function TelInput.TelMouse.Motion(Rect: TelRect): Boolean;
begin
  Result := false;

  if ((Self.Cursor in Rect) and Motion) then Result := true;
end;

{
  #############################################################################
  # TelInput                                                            #
  #############################################################################

  Description:
    Handles keyboard, mouse and joystick inputs

  Additional Notes: -

}

constructor TelInput.Create;
begin
  Keyboard := TelKeyboard.Create;
  Joystick := TelJoystick.Create;
  Mouse := TelMouse.Create;
end;

destructor TelInput.Destroy;
begin
  Keyboard.Destroy;
  Joystick.Destroy;
  Mouse.Destroy;
end;

function TelInput.GetJoystickName(): AnsiString;
begin
  Result := SDL_JoystickName(0);
end;

function TelInput.GetJoystickCount(): Integer;
begin
  Result := ActiveWindow.JoystickCount;
end;

procedure TelInput.DebugInfo();
begin
  TelLogger.GetInstance.WriteLog('Joystick Count: ' + IntToStr(GetJoystickCount()), ltNote);
  TelLogger.GetInstance.WriteLog('Joystick Name: ' + GetJoystickName(), ltNote);
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
  Result := K_RSHIFT;
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
  Input := TelInput.Create;
  Key := TelKeyCode.Create;
  Button := TelButtonCode.Create;

finalization
  Input.Destroy;
  Key.Destroy;
  Button.Destroy;
{$ENDIF}

end.
