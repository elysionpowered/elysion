{%region '--- Unit description ---'}
(**
  *	Elysion Types
  *
  *	Declares all types and enums needed in the Elysion Frameworks
  *
  * All position coordinates are floats as well as the color values
  * 
  *	
  *)
{%endregion}

unit ElysionTypes;

{$I Elysion.inc}

interface

uses
  //ElysionUtils,

  SDLUtils,
  SysUtils;


const
  // Version information
  VER_MAJOR = 11;
  VER_MINOR = 04;
  VER_CODENAME = 'Caprica'; //< Codename
  VER_STABLE = true;

  // Other random stuff mostly regarding version information
  VER_CODENAME_UNIVERSE = 'Battlestar Galactica / Caprica'; //< From which universe the codename comes from
  VER_CODENAME_RANDOMQUOTE = 'A cybernetic lifeform node. A Cylon...'; //< Random quote from that universe

  // So, in case you didn't get this: Each codename comes from a movie or TV show

	{$IFDEF FPC}
		// Usage of FreePascal is recommanded
		{$IFDEF UNIX}
		  {$IFDEF DARWIN}
      	{$IFDEF IPHONE}
        	SYS_NAME = 'iPhone-MacOS';
        {$ELSE}
        	SYS_NAME = 'Mac OS X';
        {$ENDIF}
		  {$ELSE}
			{$IFDEF GP2X}
			  SYS_NAME = 'GP2X-Linux';
			{$ELSE}
			  SYS_NAME = 'Linux';
			{$ENDIF}
		  {$ENDIF}
		{$ELSE}
		  SYS_NAME = 'Windows';
		{$ENDIF}

		{$IFDEF CPU64}
		  SYS_BITS = 64;
		{$ELSE}
		  SYS_BITS = 32;
		{$ENDIF}
	{$ELSE}
    // Assume Delphi
		SYS_NAME = 'Windows';
		SYS_BITS = 32;
	{$ENDIF}
	
	
// ASCII Keymap
// copied from SDL.pas


MAX_JOYSTICKS = 2; // only 2 are supported in the multimedia API
{$EXTERNALSYM MAX_JOYSTICKS}
MAX_AXES = 6; // each joystick can have up to 6 axes
{$EXTERNALSYM MAX_AXES}
MAX_BUTTONS = 32; // and 32 buttons
{$EXTERNALSYM MAX_BUTTONS}
AXIS_MIN = -32768; // minimum value for axis coordinate
{$EXTERNALSYM AXIS_MIN}
AXIS_MAX = 32767; // maximum value for axis coordinate
{$EXTERNALSYM AXIS_MAX}
JOY_AXIS_THRESHOLD = (((AXIS_MAX) - (AXIS_MIN)) / 100); // 1% motion
{$EXTERNALSYM JOY_AXIS_THRESHOLD}


HAT_CENTERED = $00;
{$EXTERNALSYM HAT_CENTERED}
HAT_UP = $01;
{$EXTERNALSYM HAT_UP}
HAT_RIGHT = $02;
{$EXTERNALSYM HAT_RIGHT}
HAT_DOWN = $04;
{$EXTERNALSYM HAT_DOWN}
HAT_LEFT = $08;
{$EXTERNALSYM HAT_LEFT}
HAT_RIGHTUP = HAT_RIGHT or HAT_UP;
{$EXTERNALSYM HAT_RIGHTUP}
HAT_RIGHTDOWN = HAT_RIGHT or HAT_DOWN;
{$EXTERNALSYM HAT_RIGHTDOWN}
HAT_LEFTUP = HAT_LEFT or HAT_UP;
{$EXTERNALSYM HAT_LEFTUP}
HAT_LEFTDOWN = HAT_LEFT or HAT_DOWN;
{$EXTERNALSYM HAT_LEFTDOWN}

  K_UNKNOWN = 0;
{$EXTERNALSYM K_UNKNOWN}
  K_FIRST = 0;
{$EXTERNALSYM K_FIRST}
  K_BACKSPACE = 8;
{$EXTERNALSYM K_BACKSPACE}
  K_TAB = 9;
{$EXTERNALSYM K_TAB}
  K_CLEAR = 12;
{$EXTERNALSYM K_CLEAR}
  K_RETURN = 13;
{$EXTERNALSYM K_RETURN}
  K_PAUSE = 19;
{$EXTERNALSYM K_PAUSE}
  K_ESCAPE = 27;
{$EXTERNALSYM K_ESCAPE}
  K_SPACE = 32;
{$EXTERNALSYM K_SPACE}
  K_EXCLAIM = 33;
{$EXTERNALSYM K_EXCLAIM}
  K_QUOTEDBL = 34;
{$EXTERNALSYM K_QUOTEDBL}
  K_HASH = 35;
{$EXTERNALSYM K_HASH}
  K_DOLLAR = 36;
{$EXTERNALSYM K_DOLLAR}
  K_AMPERSAND = 38;
{$EXTERNALSYM K_AMPERSAND}
  K_QUOTE = 39;
{$EXTERNALSYM K_QUOTE}
  K_LEFTPAREN = 40;
{$EXTERNALSYM K_LEFTPAREN}
  K_RIGHTPAREN = 41;
{$EXTERNALSYM K_RIGHTPAREN}
  K_ASTERISK = 42;
{$EXTERNALSYM K_ASTERISK}
  K_PLUS = 43;
{$EXTERNALSYM K_PLUS}
  K_COMMA = 44;
{$EXTERNALSYM K_COMMA}
  K_MINUS = 45;
{$EXTERNALSYM K_MINUS}
  K_PERIOD = 46;
{$EXTERNALSYM K_PERIOD}
  K_SLASH = 47;
{$EXTERNALSYM K_SLASH}
  K_0 = 48;
{$EXTERNALSYM K_0}
  K_1 = 49;
{$EXTERNALSYM K_1}
  K_2 = 50;
{$EXTERNALSYM K_2}
  K_3 = 51;
{$EXTERNALSYM K_3}
  K_4 = 52;
{$EXTERNALSYM K_4}
  K_5 = 53;
{$EXTERNALSYM K_5}
  K_6 = 54;
{$EXTERNALSYM K_6}
  K_7 = 55;
{$EXTERNALSYM K_7}
  K_8 = 56;
{$EXTERNALSYM K_8}
  K_9 = 57;
{$EXTERNALSYM K_9}
  K_COLON = 58;
{$EXTERNALSYM K_COLON}
  K_SEMICOLON = 59;
{$EXTERNALSYM K_SEMICOLON}
  K_LESS = 60;
{$EXTERNALSYM K_LESS}
  K_EQUALS = 61;
{$EXTERNALSYM K_EQUALS}
  K_GREATER = 62;
{$EXTERNALSYM K_GREATER}
  K_QUESTION = 63;
{$EXTERNALSYM K_QUESTION}
  K_AT = 64;
{$EXTERNALSYM K_AT}

  { Skip uppercase letters }

  K_LEFTBRACKET = 91;
{$EXTERNALSYM K_LEFTBRACKET}
  K_BACKSLASH = 92;
{$EXTERNALSYM K_BACKSLASH}
  K_RIGHTBRACKET = 93;
{$EXTERNALSYM K_RIGHTBRACKET}
  K_CARET = 94;
{$EXTERNALSYM K_CARET}
  K_UNDERSCORE = 95;
{$EXTERNALSYM K_UNDERSCORE}
  K_BACKQUOTE = 96;
{$EXTERNALSYM K_BACKQUOTE}
  K_a = 97;
{$EXTERNALSYM K_a}
  K_b = 98;
{$EXTERNALSYM K_b}
  K_c = 99;
{$EXTERNALSYM K_c}
  K_d = 100;
{$EXTERNALSYM K_d}
  K_e = 101;
{$EXTERNALSYM K_e}
  K_f = 102;
{$EXTERNALSYM K_f}
  K_g = 103;
{$EXTERNALSYM K_g}
  K_h = 104;
{$EXTERNALSYM K_h}
  K_i = 105;
{$EXTERNALSYM K_i}
  K_j = 106;
{$EXTERNALSYM K_j}
  K_k = 107;
{$EXTERNALSYM K_k}
  K_l = 108;
{$EXTERNALSYM K_l}
  K_m = 109;
{$EXTERNALSYM K_m}
  K_n = 110;
{$EXTERNALSYM K_n}
  K_o = 111;
{$EXTERNALSYM K_o}
  K_p = 112;
{$EXTERNALSYM K_p}
  K_q = 113;
{$EXTERNALSYM K_q}
  K_r = 114;
{$EXTERNALSYM K_r}
  K_s = 115;
{$EXTERNALSYM K_s}
  K_t = 116;
{$EXTERNALSYM K_t}
  K_u = 117;
{$EXTERNALSYM K_u}
  K_v = 118;
{$EXTERNALSYM K_v}
  K_w = 119;
{$EXTERNALSYM K_w}
  K_x = 120;
{$EXTERNALSYM K_x}
  K_y = 121;
{$EXTERNALSYM K_y}
  K_z = 122;
{$EXTERNALSYM K_z}
  K_DELETE = 127;
{$EXTERNALSYM K_DELETE}
  // End of ASCII mapped keysyms

  // International keyboard syms
  K_WORLD_0 = 160; // 0xA0
{$EXTERNALSYM K_WORLD_0}
  K_WORLD_1 = 161;
{$EXTERNALSYM K_WORLD_1}
  K_WORLD_2 = 162;
{$EXTERNALSYM K_WORLD_2}
  K_WORLD_3 = 163;
{$EXTERNALSYM K_WORLD_3}
  K_WORLD_4 = 164;
{$EXTERNALSYM K_WORLD_4}
  K_WORLD_5 = 165;
{$EXTERNALSYM K_WORLD_5}
  K_WORLD_6 = 166;
{$EXTERNALSYM K_WORLD_6}
  K_WORLD_7 = 167;
{$EXTERNALSYM K_WORLD_7}
  K_WORLD_8 = 168;
{$EXTERNALSYM K_WORLD_8}
  K_WORLD_9 = 169;
{$EXTERNALSYM K_WORLD_9}
  K_WORLD_10 = 170;
{$EXTERNALSYM K_WORLD_10}
  K_WORLD_11 = 171;
{$EXTERNALSYM K_WORLD_11}
  K_WORLD_12 = 172;
{$EXTERNALSYM K_WORLD_12}
  K_WORLD_13 = 173;
{$EXTERNALSYM K_WORLD_13}
  K_WORLD_14 = 174;
{$EXTERNALSYM K_WORLD_14}
  K_WORLD_15 = 175;
{$EXTERNALSYM K_WORLD_15}
  K_WORLD_16 = 176;
{$EXTERNALSYM K_WORLD_16}
  K_WORLD_17 = 177;
{$EXTERNALSYM K_WORLD_17}
  K_WORLD_18 = 178;
{$EXTERNALSYM K_WORLD_18}
  K_WORLD_19 = 179;
{$EXTERNALSYM K_WORLD_19}
  K_WORLD_20 = 180;
{$EXTERNALSYM K_WORLD_20}
  K_WORLD_21 = 181;
{$EXTERNALSYM K_WORLD_21}
  K_WORLD_22 = 182;
{$EXTERNALSYM K_WORLD_22}
  K_WORLD_23 = 183;
{$EXTERNALSYM K_WORLD_23}
  K_WORLD_24 = 184;
{$EXTERNALSYM K_WORLD_24}
  K_WORLD_25 = 185;
{$EXTERNALSYM K_WORLD_25}
  K_WORLD_26 = 186;
{$EXTERNALSYM K_WORLD_26}
  K_WORLD_27 = 187;
{$EXTERNALSYM K_WORLD_27}
  K_WORLD_28 = 188;
{$EXTERNALSYM K_WORLD_28}
  K_WORLD_29 = 189;
{$EXTERNALSYM K_WORLD_29}
  K_WORLD_30 = 190;
{$EXTERNALSYM K_WORLD_30}
  K_WORLD_31 = 191;
{$EXTERNALSYM K_WORLD_31}
  K_WORLD_32 = 192;
{$EXTERNALSYM K_WORLD_32}
  K_WORLD_33 = 193;
{$EXTERNALSYM K_WORLD_33}
  K_WORLD_34 = 194;
{$EXTERNALSYM K_WORLD_34}
  K_WORLD_35 = 195;
{$EXTERNALSYM K_WORLD_35}
  K_WORLD_36 = 196;
{$EXTERNALSYM K_WORLD_36}
  K_WORLD_37 = 197;
{$EXTERNALSYM K_WORLD_37}
  K_WORLD_38 = 198;
{$EXTERNALSYM K_WORLD_38}
  K_WORLD_39 = 199;
{$EXTERNALSYM K_WORLD_39}
  K_WORLD_40 = 200;
{$EXTERNALSYM K_WORLD_40}
  K_WORLD_41 = 201;
{$EXTERNALSYM K_WORLD_41}
  K_WORLD_42 = 202;
{$EXTERNALSYM K_WORLD_42}
  K_WORLD_43 = 203;
{$EXTERNALSYM K_WORLD_43}
  K_WORLD_44 = 204;
{$EXTERNALSYM K_WORLD_44}
  K_WORLD_45 = 205;
{$EXTERNALSYM K_WORLD_45}
  K_WORLD_46 = 206;
{$EXTERNALSYM K_WORLD_46}
  K_WORLD_47 = 207;
{$EXTERNALSYM K_WORLD_47}
  K_WORLD_48 = 208;
{$EXTERNALSYM K_WORLD_48}
  K_WORLD_49 = 209;
{$EXTERNALSYM K_WORLD_49}
  K_WORLD_50 = 210;
{$EXTERNALSYM K_WORLD_50}
  K_WORLD_51 = 211;
{$EXTERNALSYM K_WORLD_51}
  K_WORLD_52 = 212;
{$EXTERNALSYM K_WORLD_52}
  K_WORLD_53 = 213;
{$EXTERNALSYM K_WORLD_53}
  K_WORLD_54 = 214;
{$EXTERNALSYM K_WORLD_54}
  K_WORLD_55 = 215;
{$EXTERNALSYM K_WORLD_55}
  K_WORLD_56 = 216;
{$EXTERNALSYM K_WORLD_56}
  K_WORLD_57 = 217;
{$EXTERNALSYM K_WORLD_57}
  K_WORLD_58 = 218;
{$EXTERNALSYM K_WORLD_58}
  K_WORLD_59 = 219;
{$EXTERNALSYM K_WORLD_59}
  K_WORLD_60 = 220;
{$EXTERNALSYM K_WORLD_60}
  K_WORLD_61 = 221;
{$EXTERNALSYM K_WORLD_61}
  K_WORLD_62 = 222;
{$EXTERNALSYM K_WORLD_62}
  K_WORLD_63 = 223;
{$EXTERNALSYM K_WORLD_63}
  K_WORLD_64 = 224;
{$EXTERNALSYM K_WORLD_64}
  K_WORLD_65 = 225;
{$EXTERNALSYM K_WORLD_65}
  K_WORLD_66 = 226;
{$EXTERNALSYM K_WORLD_66}
  K_WORLD_67 = 227;
{$EXTERNALSYM K_WORLD_67}
  K_WORLD_68 = 228;
{$EXTERNALSYM K_WORLD_68}
  K_WORLD_69 = 229;
{$EXTERNALSYM K_WORLD_69}
  K_WORLD_70 = 230;
{$EXTERNALSYM K_WORLD_70}
  K_WORLD_71 = 231;
{$EXTERNALSYM K_WORLD_71}
  K_WORLD_72 = 232;
{$EXTERNALSYM K_WORLD_72}
  K_WORLD_73 = 233;
{$EXTERNALSYM K_WORLD_73}
  K_WORLD_74 = 234;
{$EXTERNALSYM K_WORLD_74}
  K_WORLD_75 = 235;
{$EXTERNALSYM K_WORLD_75}
  K_WORLD_76 = 236;
{$EXTERNALSYM K_WORLD_76}
  K_WORLD_77 = 237;
{$EXTERNALSYM K_WORLD_77}
  K_WORLD_78 = 238;
{$EXTERNALSYM K_WORLD_78}
  K_WORLD_79 = 239;
{$EXTERNALSYM K_WORLD_79}
  K_WORLD_80 = 240;
{$EXTERNALSYM K_WORLD_80}
  K_WORLD_81 = 241;
{$EXTERNALSYM K_WORLD_81}
  K_WORLD_82 = 242;
{$EXTERNALSYM K_WORLD_82}
  K_WORLD_83 = 243;
{$EXTERNALSYM K_WORLD_83}
  K_WORLD_84 = 244;
{$EXTERNALSYM K_WORLD_84}
  K_WORLD_85 = 245;
{$EXTERNALSYM K_WORLD_85}
  K_WORLD_86 = 246;
{$EXTERNALSYM K_WORLD_86}
  K_WORLD_87 = 247;
{$EXTERNALSYM K_WORLD_87}
  K_WORLD_88 = 248;
{$EXTERNALSYM K_WORLD_88}
  K_WORLD_89 = 249;
{$EXTERNALSYM K_WORLD_89}
  K_WORLD_90 = 250;
{$EXTERNALSYM K_WORLD_90}
  K_WORLD_91 = 251;
{$EXTERNALSYM K_WORLD_91}
  K_WORLD_92 = 252;
{$EXTERNALSYM K_WORLD_92}
  K_WORLD_93 = 253;
{$EXTERNALSYM K_WORLD_93}
  K_WORLD_94 = 254;
{$EXTERNALSYM K_WORLD_94}
  K_WORLD_95 = 255; // 0xFF
{$EXTERNALSYM K_WORLD_95}

  // Numeric keypad
  K_KP0 = 256;
{$EXTERNALSYM K_KP0}
  K_KP1 = 257;
{$EXTERNALSYM K_KP1}
  K_KP2 = 258;
{$EXTERNALSYM K_KP2}
  K_KP3 = 259;
{$EXTERNALSYM K_KP3}
  K_KP4 = 260;
{$EXTERNALSYM K_KP4}
  K_KP5 = 261;
{$EXTERNALSYM K_KP5}
  K_KP6 = 262;
{$EXTERNALSYM K_KP6}
  K_KP7 = 263;
{$EXTERNALSYM K_KP7}
  K_KP8 = 264;
{$EXTERNALSYM K_KP8}
  K_KP9 = 265;
{$EXTERNALSYM K_KP9}
  K_KP_PERIOD = 266;
{$EXTERNALSYM K_KP_PERIOD}
  K_KP_DIVIDE = 267;
{$EXTERNALSYM K_KP_DIVIDE}
  K_KP_MULTIPLY = 268;
{$EXTERNALSYM K_KP_MULTIPLY}
  K_KP_MINUS = 269;
{$EXTERNALSYM K_KP_MINUS}
  K_KP_PLUS = 270;
{$EXTERNALSYM K_KP_PLUS}
  K_KP_ENTER = 271;
{$EXTERNALSYM K_KP_ENTER}
  K_KP_EQUALS = 272;
{$EXTERNALSYM K_KP_EQUALS}

  // Arrows + Home/End pad
  K_UP = 273;
{$EXTERNALSYM K_UP}
  K_DOWN = 274;
{$EXTERNALSYM K_DOWN}
  K_RIGHT = 275;
{$EXTERNALSYM K_RIGHT}
  K_LEFT = 276;
{$EXTERNALSYM K_LEFT}
  K_INSERT = 277;
{$EXTERNALSYM K_INSERT}
  K_HOME = 278;
{$EXTERNALSYM K_HOME}
  K_END = 279;
{$EXTERNALSYM K_END}
  K_PAGEUP = 280;
{$EXTERNALSYM K_PAGEUP}
  K_PAGEDOWN = 281;
{$EXTERNALSYM K_PAGEDOWN}

  // Function keys
  K_F1 = 282;
{$EXTERNALSYM K_F1}
  K_F2 = 283;
{$EXTERNALSYM K_F2}
  K_F3 = 284;
{$EXTERNALSYM K_F3}
  K_F4 = 285;
{$EXTERNALSYM K_F4}
  K_F5 = 286;
{$EXTERNALSYM K_F5}
  K_F6 = 287;
{$EXTERNALSYM K_F6}
  K_F7 = 288;
{$EXTERNALSYM K_F7}
  K_F8 = 289;
{$EXTERNALSYM K_F8}
  K_F9 = 290;
{$EXTERNALSYM K_F9}
  K_F10 = 291;
{$EXTERNALSYM K_F10}
  K_F11 = 292;
{$EXTERNALSYM K_F11}
  K_F12 = 293;
{$EXTERNALSYM K_F12}
  K_F13 = 294;
{$EXTERNALSYM K_F13}
  K_F14 = 295;
{$EXTERNALSYM K_F14}
  K_F15 = 296;
{$EXTERNALSYM K_F15}

  // Key state modifier keys
  K_NUMLOCK = 300;
{$EXTERNALSYM K_NUMLOCK}
  K_CAPSLOCK = 301;
{$EXTERNALSYM K_CAPSLOCK}
  K_SCROLLOCK = 302;
{$EXTERNALSYM K_SCROLLOCK}
  K_RSHIFT = 303;
{$EXTERNALSYM K_RSHIFT}
  K_LSHIFT = 304;
{$EXTERNALSYM K_LSHIFT}
  K_RCTRL = 305;
{$EXTERNALSYM K_RCTRL}
  K_LCTRL = 306;
{$EXTERNALSYM K_LCTRL}
  K_RALT = 307;
{$EXTERNALSYM K_RALT}
  K_LALT = 308;
{$EXTERNALSYM K_LALT}
  K_RMETA = 309;
{$EXTERNALSYM K_RMETA}
  K_LMETA = 310;
{$EXTERNALSYM K_LMETA}
  K_LSUPER = 311; // Left "Windows" key
{$EXTERNALSYM K_LSUPER}
  K_RSUPER = 312; // Right "Windows" key
{$EXTERNALSYM K_RSUPER}
  K_MODE = 313; // "Alt Gr" key
{$EXTERNALSYM K_MODE}
  K_COMPOSE = 314; // Multi-key compose key
{$EXTERNALSYM K_COMPOSE}

  // Miscellaneous function keys
  K_HELP = 315;
{$EXTERNALSYM K_HELP}
  K_PRINT = 316;
{$EXTERNALSYM K_PRINT}
  K_SYSREQ = 317;
{$EXTERNALSYM K_SYSREQ}
  K_BREAK = 318;
{$EXTERNALSYM K_BREAK}
  K_MENU = 319;
{$EXTERNALSYM K_MENU}
  K_POWER = 320; // Power Macintosh power key
{$EXTERNALSYM K_POWER}
  K_EURO = 321; // Some european keyboards
{$EXTERNALSYM K_EURO}
// End of keys

  // Enumeration of valid key mods (possibly OR'd together)
  KMOD_NONE = $0000;
{$EXTERNALSYM KMOD_NONE}
  KMOD_LSHIFT = $0001;
{$EXTERNALSYM KMOD_LSHIFT}
  KMOD_RSHIFT = $0002;
{$EXTERNALSYM KMOD_RSHIFT}
  KMOD_LCTRL = $0040;
{$EXTERNALSYM KMOD_LCTRL}
  KMOD_RCTRL = $0080;
{$EXTERNALSYM KMOD_RCTRL}
  KMOD_LALT = $0100;
{$EXTERNALSYM KMOD_LALT}
  KMOD_RALT = $0200;
{$EXTERNALSYM KMOD_RALT}
  KMOD_LMETA = $0400;
{$EXTERNALSYM KMOD_LMETA}
  KMOD_RMETA = $0800;
{$EXTERNALSYM KMOD_RMETA}
  KMOD_NUM = $1000;
{$EXTERNALSYM KMOD_NUM}
  KMOD_CAPS = $2000;
{$EXTERNALSYM KMOD_CAPS}
  KMOD_MODE = 44000;
{$EXTERNALSYM KMOD_MODE}
  KMOD_RESERVED = $8000;
{$EXTERNALSYM KMOD_RESERVED}

  KMOD_CTRL = (KMOD_LCTRL or KMOD_RCTRL);
{$EXTERNALSYM KMOD_CTRL}
  KMOD_SHIFT = (KMOD_LSHIFT or KMOD_RSHIFT);
{$EXTERNALSYM KMOD_SHIFT}
  KMOD_ALT = (KMOD_LALT or KMOD_RALT);
{$EXTERNALSYM KMOD_ALT}
  KMOD_META = (KMOD_LMETA or KMOD_RMETA);
{$EXTERNALSYM KMOD_META}


// And here we go with the mouse buttons
  BUTTON_LEFT      = 1;
{$EXTERNALSYM BUTTON_LEFT}
  BUTTON_MIDDLE    = 2;
{$EXTERNALSYM BUTTON_MIDDLE}
  BUTTON_RIGHT     = 3;
{$EXTERNALSYM BUTTON_RIGHT}
  BUTTON_WHEELUP   = 4;
{$EXTERNALSYM BUTTON_WHEELUP}
  BUTTON_WHEELDOWN = 5;
{$EXTERNALSYM BUTTON_WHEELDOWN}
// End of copying from other units


type

  TKey = object
    KeyName: String;
	
  end;

  {$IFDEF FPC}
  TKeyLongString = object
  {$ELSE}
  TKeyLongString = record
  {$ENDIF}
  
  end;
  
  

  {$IFDEF FPC}
  TKeyString = object
  {$ELSE}
  TKeyString = record
  {$ENDIF}
    KeyName: String;
	Value: String;
	
    {$IFDEF CAN_METHODS}
	
	{$ENDIF}
  end;
  
  TKeyInt = record
    KeyName: String;
	Value: Integer;
  end;
  
  TKeyFloat = record
    KeyName: String;
	Value: Single;
  end;

  // Forward declaration
  PelColor = ^TelColor;
  PelVector2f = ^TelVector2f;
  PelVector2i = ^TelVector2i;
  PelVector3f = ^TelVector3f;
  PelVector3i = ^TelVector3i;
  PelSize = ^TelSize;

  {%region '--- TelColor type definition ---'}
    {%region '--- TelColor description ---'}
    (**
      *
      *
      *
      *
      *
      *)
    {%endregion}

  {$IFDEF FPC}
  TelColor = object
  {$ELSE}
  TelColor = record
  {$ENDIF}
    R, G, B, A: Byte;

    {$IFDEF CAN_METHODS}
    procedure Clear();
    procedure Make(aR, aG, aB: Byte; anA: Byte = 255);

    function ToString(): String;
    procedure ToFloat(var floatR: Single; var floatG: Single; var floatB: Single; var floatA: Single);

    // Operators (has to compliant to earlier Delphi versions)
    procedure Add(Color: TelColor);
    procedure Sub(Color: TelColor);
    procedure Multiply(Color: TelColor);
    procedure Divide(Color: TelColor);
    procedure Scale(Factor: Single);
    {$ENDIF}
  end;

  {%endregion}

  {$IFDEF FPC}
  TelVector2f = object
  {$ELSE}
  TelVector2f = record
  {$ENDIF}
    X, Y: Single;

    {$IFDEF CAN_METHODS}
    procedure Clear();

    function GetLength: Double;
    procedure Make(aX, aY: Single);

    function ToString(): String;

    // Convert to other vector types
    function ToVector2i(): PelVector2i;
    function ToVector3f(): PelVector3f;
    function ToVector3i(): PelVector3i;

    // Operators (has to compliant to earlier Delphi versions)
    procedure Add(Vector: TelVector2f);
    procedure Sub(Vector: TelVector2f);
    procedure Multiply(Vector: TelVector2f);
    procedure Divide(Vector: TelVector2f);
    procedure Scale(Factor: Single);

    // Vector stuff
    function DotProduct(Vector: TelVector2f): Single;
    procedure Normalize();
    {$ENDIF}
  end;


  {$IFDEF FPC}
  TelVector2i = object
  {$ELSE}
  TelVector2i = record
  {$ENDIF}
    X, Y: Integer;

    {$IFDEF CAN_METHODS}
    procedure Clear();

    function GetLength(): Double;
    procedure Make(aX, aY: Integer);

    function ToString(): String;

    // Convert to other vector types
    function ToVector2f(): PelVector2f;
    function ToVector3f(): PelVector3f;
    function ToVector3i(): PelVector3i;
    function ToSize(): PelSize;

    // Operators (has to compliant to earlier Delphi versions)
    procedure Add(Vector: TelVector2i);
    procedure Sub(Vector: TelVector2i);
    procedure Multiply(Vector: TelVector2i);
    procedure Divide(Vector: TelVector2i);
    procedure Scale(Factor: Single);

    // Vector stuff
    function DotProduct(Vector: TelVector2i): Integer;
    procedure Normalize();
    {$ENDIF}
  end;

  {$IFDEF FPC}
  TelSize = object
  {$ELSE}
  TelSize = record
  {$ENDIF}
    Width, Height: Integer;

    {$IFDEF CAN_METHODS}
    procedure Clear();

    procedure Make(aWidth, aHeight: Integer);

    function ToString(): String;

    // Convert to other vector types
    function ToVector2i(): PelVector2i;

    // Operators (has to compliant to earlier Delphi versions)
    procedure Add(Size: TelSize);
    procedure Sub(Size: TelSize);
    procedure Multiply(Size: TelSize);
    procedure Divide(Size: TelSize);
    procedure Scale(Factor: Single);
    {$ENDIF}
  end;


  {$IFDEF FPC}
  TelVector3f = object
  {$ELSE}
  TelVector3f = record
  {$ENDIF}
    X, Y, Z: Single;

    {$IFDEF CAN_METHODS}
    procedure Clear();

    function GetLength(): Double;

    procedure Make(aX, aY, aZ: Single);

    function ToString(): String;

    // Convert to other vector types
    function ToVector3i(): PelVector3i;
    function ToVector2f(): PelVector2f;
    function ToVector2i(): PelVector2i;

    // Operators (has to compliant to earlier Delphi versions)
    procedure Add(Vector: TelVector3f);
    procedure Sub(Vector: TelVector3f);
    procedure Multiply(Vector: TelVector3f);
    procedure Divide(Vector: TelVector3f);
    procedure Scale(Factor: Single);

    // Vector stuff
    function DotProduct(Vector: TelVector3f): Single;
    procedure Normalize();

    procedure Zero();
    procedure One();
    procedure Forward();
    procedure Up();
    procedure Right();
    {$ENDIF}
  end;

  {$IFDEF FPC}
  TelVector3i = object
  {$ELSE}
  TelVector3i = record
  {$ENDIF}
    X, Y, Z: Integer;

    {$IFDEF CAN_METHODS}
    procedure Clear();

    function GetLength(): Double;

    procedure Make(aX, aY, aZ: Integer);

    function ToString(): String;

    // Convert to other vector types
    function ToVector3f(EmptyZ: Boolean = False): PelVector3f;
    function ToVector2f(): PelVector2f;
    function ToVector2i(): PelVector2i;

    // Operators (has to compliant to earlier Delphi versions)
    procedure Add(Vector: TelVector3i);
    procedure Sub(Vector: TelVector3i);
    procedure Multiply(Vector: TelVector3i);
    procedure Divide(Vector: TelVector3i);
    procedure Scale(Factor: Single);

    // Vector stuff
    function DotProduct(Vector: TelVector3i): Integer;
    procedure Normalize();

    procedure Zero();
    procedure One();
    procedure Forward();
    procedure Up();
    procedure Right();
    {$ENDIF}
  end;


  // A vertex is basically just a position vector with a color attached to it
  TelVertex = record
    Vector: TelVector3f;
    Color: TelColor;
  end;

  {$IFDEF FPC}
  TelRect = object
  {$ELSE}
  TelRect = record
  {$ENDIF}
    X, Y, W, H: Single;

    {$IFDEF CAN_METHODS}
    procedure Clear();

    procedure Make(aX, aY, aW, aH: Single); Overload;
    procedure Make(aX, aY, aW, aH: Integer); Overload;

    function ContainsVector(aVector: TelVector2i): Boolean; Overload;
    function ContainsVector(aVector: TelVector2f): Boolean; Overload;
    function ContainsRect(aRect: TelRect): Boolean; Overload;
    {$ENDIF}
  end;



  TelImageOffset = record
    Position: TelVector2i;
    Rotation: TelVector2i;
  end;

  TelImageRotation = record
    Angle: Single;
    Vector: TelVector3f;
  end;

  TelShadow = record
    Blur: Integer;
    Color: TelColor;
    Position: TelVector2i;
    Visible: Boolean;
  end;

  PelButtonEvent = ^TelButtonEvent;
  TelButtonEvent = record
    Position: TelVector2i;
    Called: Cardinal;
  end;

  TelBlendMode = (
    bmAdd,    //< Additive blending
    bmNormal, //< Normal blending
    bmSub);   //< Sub blending

  // TelVideoFlags
  TelVideoFlag =
    (vfNull,      //< vfNull: Use for console applications, no video surface will be created
     vfAuto,      //< vfAuto: Automatically checks if hardware or software render mode are available
     vfHardware,  //< vfHardware: Use hardware surface
     vfSoftware); //< vfSoftware: Use software surface

  TelProjectionMode = (pmPerspective, pmOrtho);

  TGradientStyle = SDLUtils.TGradientStyle;
  TColorVertices = array[0..3] of TelColor;

  TelGradient = Record
    StartColor: TelColor;
    EndColor: TelColor;
    GradientStyle: TGradientStyle;
  end;

  TAlignVertical = (avNone, avTop, avBottom, avCenter);
  TAlignHorizontal = (ahNone, ahLeft, ahRight, ahCenter);

  TelAssetType = (atTexture, atSprite, atParallexSprite, atSpriteSheet);

  TelAlignment = record
    Vertical: TAlignVertical;
    Horizontal: TAlignHorizontal;
  end;

  // See: TelGraphicObject.Generate;
  TGenerateMode = (gmAuto, gmRGB, gmRGBA);

  {%region 'Animation types'}
  // Each animator type responds to a node property
  TelAnimationType = (atAlpha, atPosition, atOffset, atRotation, atColor, atScale, atShadow);

  // Different animator transitions: Only linear is working right now, the others are placeholders
  TelAnimationTransition = (atLinear, atEaseIn, atEaseOut, atEaseInOut, atBounce);

  // Animation KeyFrame
  TelAnimationKeyFrame = record
    case AnimType: TelAnimationType of
      atAlpha: (Alpha: Byte);
      atPosition: (Position: TelVector3f);
      atOffset: (Offset: TelImageOffset);
      atRotation: (Rotation: TelImageRotation);
      atColor: (Color: TelColor);
      atScale: (Scale: TelVector2f);
      atShadow: (Shadow: TelShadow);
  end;

  // Animation Property
  TelAnimationProperty = record
    case AnimType: TelAnimationType of
      atAlpha: (StartAlpha, EndAlpha: Byte);
      atPosition: (StartPosition, EndPosition: TelVector3f);
      atOffset: (StartOffset, EndOffset: TelImageOffset);
      atRotation: (StartRotation, EndRotation: TelImageRotation);
      atColor: (StartColor, EndColor: TelColor);
      atScale: (StartScale, EndScale: TelVector2f);
      atShadow: (StartShadow, EndShadow: TelShadow);
  end;
  {%endregion}

  TelEvent = procedure() of object;

  function makeGradient(StartColor: TelColor; EndColor: TelColor; GradientStyle: TGradientStyle = gsVertical): TelGradient; {$IFDEF CAN_INLINE} inline; {$ENDIF}

  function makeV2f(aX, aY: Single): TelVector2f; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function makeV2i(aX, aY: Integer): TelVector2i; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function makeV3f(aX, aY: Single; aZ: Single = 0.0): TelVector3f; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function makeV3i(aX, aY: Integer; aZ: Integer = 0): TelVector3i; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function makeSize(aWidth, aHeight: Integer): TelSize; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function makeRect(aX, aY, aW, aH: Single): TelRect; {$IFDEF CAN_INLINE} inline; {$ENDIF}

  function makeCol(aR, aG, aB: Byte; anA: Byte = 255): TelColor; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function makeCol(aR, aG, aB: Single; anA: Single = 1.0): TelColor; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

  function makeP2D(aX, aY: Integer): TelVector2i; Overload; deprecated;
  function makeP2D(aX, aY: Single): TelVector2f; Overload; deprecated;
  function makeP3D(aX, aY: Integer; aZ: Integer = 0): TelVector3i; Overload; deprecated;
  function makeP3D(aX, aY: Single; aZ: Single = 0.0): TelVector3f; Overload; deprecated;


  function IsRectEmpty(Rect: TelRect): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}


  function VectorEquals(VecOne, VecTwo: TelVector2f): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function VectorEquals(VecOne, VecTwo: TelVector2i): Boolean; Overload;{$IFDEF CAN_INLINE} inline; {$ENDIF}

  function VectorEquals(VecOne, VecTwo: TelVector3f): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function VectorEquals(VecOne, VecTwo: TelVector3i): Boolean; Overload;{$IFDEF CAN_INLINE} inline; {$ENDIF}

  function ColorEquals(ColorOne, ColorTwo: TelColor): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}


  {$IFNDEF CAN_METHODS}
  function RectContainsVector(aRect: TelRect; aVector: TelVector2i): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function RectContainsVector(aRect: TelRect; aVector: TelVector2f): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function RectContainsRect(aRect, bRect: TelRect): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  {$ENDIF}


implementation

function makeGradient(StartColor: TelColor; EndColor: TelColor; GradientStyle: TGradientStyle = gsVertical): TelGradient;
var
  tmpGradient: TelGradient;
begin
  tmpGradient.StartColor := StartColor;
  tmpGradient.EndColor := EndColor;
  tmpGradient.GradientStyle := GradientStyle;

  Result := tmpGradient;
end;

function makeCol(aR, aG, aB: Byte; anA: Byte = 255): TelColor;
var
  tmpCol: TelColor;
begin
  tmpCol.R := aR;
  tmpCol.G := aG;
  tmpCol.B := aB;
  tmpCol.A := anA;

  Result := tmpCol;
end;

function makeCol(aR, aG, aB: Single; anA: Single = 1.0): TelColor;
var
  tR, tG, tB, tA: Byte;
  tmpCol: TelColor;
begin
  if (aR * 255) >= 255 then tR := 255 else tR := Trunc(aR * 255);
  if (aG * 255) >= 255 then tG := 255 else tG := Trunc(aR * 255);
  if (aB * 255) >= 255 then tB := 255 else tB := Trunc(aR * 255);
  if (anA * 255) >= 255 then tA := 255 else tA := Trunc(aR * 255);

  tmpCol.R := tR ;
  tmpCol.G := tG;
  tmpCol.B := tB;
  tmpCol.A := tA;

  Result := tmpCol;
end;

function makeP2D(aX, aY: Integer): TelVector2i;
begin
  Result := makeV2i(aX, aY);
end;

function makeP2D(aX, aY: Single): TelVector2f;
begin
  Result := makeV2f(aX, aY);
end;

function makeP3D(aX, aY: Integer; aZ: Integer = 0): TelVector3i;
begin
  Result := makeV3i(aX, aY, aZ);
end;

function makeP3D(aX, aY: Single; aZ: Single = 0.0): TelVector3f;
begin
  Result := makeV3f(aX, aY, aZ);
end;

function makeV2i(aX, aY: Integer): TelVector2i;
var
  tmpVec: TelVector2i;
begin
  tmpVec.X := aX;
  tmpVec.Y := aY;

  Result := tmpVec;
end;

function makeV2f(aX, aY: Single): TelVector2f;
var
  tmpVec: TelVector2f;
begin
  tmpVec.X := aX;
  tmpVec.Y := aY;

  Result := tmpVec;
end;

function makeV3i(aX, aY: Integer; aZ: Integer = 0): TelVector3i;
var
  tmpVec: TelVector3i;
begin
  tmpVec.X := aX;
  tmpVec.Y := aY;
  tmpVec.Z := aZ;

  Result := tmpVec;
end;

function makeV3f(aX, aY: Single; aZ: Single = 0.0): TelVector3f;
var
  tmpVec: TelVector3f;
begin
  tmpVec.X := aX;
  tmpVec.Y := aY;
  tmpVec.Z := aZ;

  Result := tmpVec;
end;

function makeSize(aWidth, aHeight: Integer): TelSize;
var
  tmpVec: TelSize;
begin
  tmpVec.Width := aWidth;
  tmpVec.Height := aHeight;

  Result := tmpVec;
end;

function makeRect(aX, aY, aW, aH: Single): TelRect;
var
  tmpRect: TelRect;
begin
  tmpRect.X := aX;
  tmpRect.Y := aY;
  tmpRect.W := aW;
  tmpRect.H := aH;

  Result := tmpRect;
end;

function IsRectEmpty(Rect: TelRect): Boolean;
begin
  if ((Rect.X <> 0) and
      (Rect.Y <> 0) and
	  (Rect.W > 0) and
	  (Rect.H > 0)) then Result := false else Result := true;
end;

function VectorEquals(VecOne, VecTwo: TelVector2f): Boolean;
begin
  if (VecOne.X <> VecTwo.X) or
     (VecOne.Y <> VecTwo.Y) then
    Result := false
  else
    Result := true;
end;

function VectorEquals(VecOne, VecTwo: TelVector2i): Boolean;
begin
  if (VecOne.X <> VecTwo.X) or
     (VecOne.Y <> VecTwo.Y) then
    Result := false
  else
    Result := true;
end;

function VectorEquals(VecOne, VecTwo: TelVector3f): Boolean;
begin
  if (VecOne.X <> VecTwo.X) or
     (VecOne.Y <> VecTwo.Y) or
     (VecOne.Z <> VecTwo.Z) then
    Result := false
  else
    Result := true;
end;

function VectorEquals(VecOne, VecTwo: TelVector3i): Boolean;
begin
  if (VecOne.X <> VecTwo.X) or
     (VecOne.Y <> VecTwo.Y) or
     (VecOne.Z <> VecTwo.Z) then
    Result := false
  else
    Result := true;
end;

function ColorEquals(ColorOne, ColorTwo: TelColor): Boolean;
begin
  if (ColorOne.R <> ColorTwo.R) or
     (ColorOne.G <> ColorTwo.G) or
     (ColorOne.B <> ColorTwo.B) or
     (ColorOne.A <> ColorOne.A) then
    Result := false
  else
    Result := true;
end;


{$IFDEF CAN_METHODS}

procedure TelColor.Clear();
begin
  R := 0;
  G := 0;
  B := 0;
  A := 0;
end;

procedure TelColor.Make(aR, aG, aB: Byte; anA: Byte = 255);
begin
  R := aR;
  G := aG;
  B := aB;
  A := anA;
end;

function TelColor.ToString(): String;
begin
  Result := Format('R: %d G: %d B: %d A: %d', [R, G, B, A]);
end;

procedure TelColor.ToFloat(var floatR: Single; var floatG: Single; var floatB: Single; var floatA: Single);
begin
  floatR := R / 255;
  floatG := G / 255;
  floatB := B / 255;
  floatA := A / 255;
end;	

procedure TelColor.Add(Color: TelColor);
begin
  Self.R := Self.R + Color.R;
  Self.G := Self.G + Color.G;
  Self.B := Self.B + Color.B;
  Self.A := Self.A + Color.A;
end;

procedure TelColor.Sub(Color: TelColor);
begin
  Self.R := Self.R - Color.R;
  Self.G := Self.G - Color.G;
  Self.B := Self.B - Color.B;
  Self.A := Self.A - Color.A;
end;

procedure TelColor.Multiply(Color: TelColor);
begin
  Self.R := Self.R * Color.R;
  Self.G := Self.G * Color.G;
  Self.B := Self.B * Color.B;
  Self.A := Self.A * Color.A;
end;

procedure TelColor.Divide(Color: TelColor);
begin
  Self.R := Self.R div Color.R;
  Self.G := Self.G div Color.G;
  Self.B := Self.B div Color.B;
  Self.A := Self.A div Color.A;
end;

procedure TelColor.Scale(Factor: Single);
var
  tR, tG, tB, tA: Byte;
begin
  if (Self.R * Factor) >= 255.0 then tR := 255
     else tR := Trunc(Self.R * Factor);

  if (Self.G * Factor) >= 255.0 then tG := 255
     else tG := Trunc(Self.G * Factor);

  if (Self.B * Factor) >= 255.0 then tB := 255
     else tB := Trunc(Self.B * Factor);

  if (Self.A * Factor) >= 255.0 then tA := 255
     else tA := Trunc(Self.A * Factor);

  Self.R := tR;
  Self.G := tG;
  Self.B := tB;
  Self.A := tA;
end;	


procedure TelVector2f.Clear();
begin
  X := 0.0;
  Y := 0.0;
end;

function TelVector2f.GetLength(): Double;
begin
  Result := sqrt(X * X + Y * Y);
end;
	
procedure TelVector2f.Make(aX, aY: Single);
begin
  X := aX;
  Y := aY;
end;
	
function TelVector2f.ToString(): String;
begin
  Result := Format('X: %.2f Y: %.2f', [X, Y])
end;
	
function TelVector2f.ToVector2i(): PelVector2i;
var
  tmpVec: TelVector2i;
begin
  tmpVec := makeV2i(Trunc(X), Trunc(Y));

  Result := @tmpVec;
end;

function TelVector2f.ToVector3f(): PelVector3f;
var
  tmpVec: TelVector3f;
begin
  tmpVec := makeV3f(X, Y, 0);

  Result := @tmpVec;
end;

function TelVector2f.ToVector3i(): PelVector3i;
var
  tmpVec: TelVector3i;
begin
  tmpVec := makeV3i(Trunc(X), Trunc(Y), 0);

  Result := @tmpVec;
end;
	
procedure TelVector2f.Add(Vector: TelVector2f);
begin
  Self.X := Self.X + Vector.X;
  Self.Y := Self.Y + Vector.Y;
end;

procedure TelVector2f.Sub(Vector: TelVector2f);
begin
  Self.X := Self.Y - Vector.X;
  Self.Y := Self.Y - Vector.Y;
end;

procedure TelVector2f.Multiply(Vector: TelVector2f);
begin
  Self.X := Self.X * Vector.X;
  Self.Y := Self.Y * Vector.Y;
end;

procedure TelVector2f.Divide(Vector: TelVector2f);
begin
  Self.X := Self.X / Vector.X;
  Self.Y := Self.Y / Vector.Y;
end;

procedure TelVector2f.Scale(Factor: Single);
begin
  Self.X := Self.X * Factor;
  Self.Y := Self.Y * Factor;
end;
	
function TelVector2f.DotProduct(Vector: TelVector2f): Single;
begin
  Result := (Self.X * Vector.X) + (Self.Y * Vector.Y);
end;

procedure TelVector2f.Normalize();
begin
  Self.X := Self.X / GetLength;
  Self.Y := Self.Y / GetLength;
end;


procedure TelVector2i.Clear();
begin
  X := 0;
  Y := 0;
end;

function TelVector2i.GetLength(): Double;
begin
  Result := sqrt(X * X + Y * Y);
end;
	
procedure TelVector2i.Make(aX, aY: Integer);
begin
  X := aX;
  Y := aY;
end;
	
function TelVector2i.ToString(): String;
begin
  Result := Format('X: %d Y: %d', [X, Y])
end;

function TelVector2i.ToSize(): PelSize;
var
  tmpVec: TelSize;
begin
  tmpVec := makeSize(X, Y);

  Result := @tmpVec;
end;

function TelVector2i.ToVector2f(): PelVector2f;
var
  tmpVec: TelVector2f;
begin
  tmpVec := makeV2f(X * 1.0, Y * 1.0);

  Result := @tmpVec;
end;

function TelVector2i.ToVector3f(): PelVector3f;
var
  tmpVec: TelVector3f;
begin
  tmpVec := makeV3f(X * 1.0, Y * 1.0, 0);

  Result := @tmpVec;
end;

function TelVector2i.ToVector3i(): PelVector3i;
var
  tmpVec: TelVector3i;
begin
  tmpVec := makeV3i(X, Y, 0);

  Result := @tmpVec;
end;
	
procedure TelVector2i.Add(Vector: TelVector2i);
begin
  Self.X := Self.X + Vector.X;
  Self.Y := Self.Y + Vector.Y;
end;

procedure TelVector2i.Sub(Vector: TelVector2i);
begin
  Self.X := Self.Y - Vector.X;
  Self.Y := Self.Y - Vector.Y;
end;

procedure TelVector2i.Multiply(Vector: TelVector2i);
begin
  Self.X := Self.X * Vector.X;
  Self.Y := Self.Y * Vector.Y;
end;

procedure TelVector2i.Divide(Vector: TelVector2i);
begin
  Self.X := Trunc(Self.X / Vector.X);
  Self.Y := Trunc(Self.Y / Vector.Y);
end;

procedure TelVector2i.Scale(Factor: Single);
begin
  Self.X := Trunc(Self.X * Factor);
  Self.Y := Trunc(Self.Y * Factor);
end;
	
function TelVector2i.DotProduct(Vector: TelVector2i): Integer;
begin
  Result := (Self.X * Vector.X) + (Self.Y * Vector.Y);
end;

procedure TelVector2i.Normalize();
begin
  Self.X := Trunc(Self.X / GetLength);
  Self.Y := Trunc(Self.Y / GetLength);
end;


procedure TelSize.Clear();
begin
  Width := 0;
  Height := 0;
end;

procedure TelSize.Make(aWidth, aHeight: Integer);
begin
  Width := aWidth;
  Height := aHeight;
end;

function TelSize.ToString(): String;
begin
  Result := Format('Width: %d Height: %d', [Width, Height])
end;

function TelSize.ToVector2i(): PelVector2i;
var
  tmpVec: TelVector2i;
begin
  tmpVec := makeV2i(Width, Height);

  Result := @tmpVec;
end;

procedure TelSize.Add(Size: TelSize);
begin
  Self.Width := Self.Width + Size.Width;
  Self.Height := Self.Height + Size.Height;
end;

procedure TelSize.Sub(Size: TelSize);
begin
  Self.Width := Self.Height - Size.Width;
  Self.Height := Self.Height - Size.Height;
end;

procedure TelSize.Multiply(Size: TelSize);
begin
  Self.Width := Self.Width * Size.Width;
  Self.Height := Self.Height * Size.Height;
end;

procedure TelSize.Divide(Size: TelSize);
begin
  Self.Width := Trunc(Self.Width / Size.Width);
  Self.Height := Trunc(Self.Height / Size.Height);
end;

procedure TelSize.Scale(Factor: Single);
begin
  Self.Width := Trunc(Self.Width * Factor);
  Self.Height := Trunc(Self.Height * Factor);
end;


procedure TelVector3f.Clear();
begin
  X := 0.0;
  Y := 0.0;
  Z := 0.0;
end;

function TelVector3f.GetLength(): Double;
begin
  Result := sqrt(X * X + Y * Y + Z * Z);
end;
	
procedure TelVector3f.Make(aX, aY, aZ: Single);
begin
  X := aX;
  Y := aY;
  Z := aZ;
end;
	
function TelVector3f.ToString(): String;
begin
  Result := Format('X: %.3f Y: %.3f Z: %.3f', [X, Y, Z])
end;
	
function TelVector3f.ToVector3i(): PelVector3i;
var
  tmpVec: TelVector3i;
begin
  tmpVec := makeV3i(Trunc(X), Trunc(Y), Trunc(Z));

  Result := @tmpVec;
end;

function TelVector3f.ToVector2f(): PelVector2f;
var
  tmpVec: TelVector2f;
begin
  tmpVec := makeV2f(X, Y);

  Result := @tmpVec;
end;

function TelVector3f.ToVector2i(): PelVector2i;
var
  tmpVec: TelVector2i;
begin
  tmpVec := makeV2i(Trunc(X), Trunc(Y));

  Result := @tmpVec;
end;
	
procedure TelVector3f.Add(Vector: TelVector3f);
begin
  Self.X := Self.X + Vector.X;
  Self.Y := Self.Y + Vector.Y;
end;

procedure TelVector3f.Sub(Vector: TelVector3f);
begin
  Self.X := Self.Y - Vector.X;
  Self.Y := Self.Y - Vector.Y;
end;

procedure TelVector3f.Multiply(Vector: TelVector3f);
begin
  Self.X := Self.X * Vector.X;
  Self.Y := Self.Y * Vector.Y;
end;

procedure TelVector3f.Divide(Vector: TelVector3f);
begin
  Self.X := Self.X / Vector.X;
  Self.Y := Self.Y / Vector.Y;
end;

procedure TelVector3f.Scale(Factor: Single);
begin
  Self.X := Self.X * Factor;
  Self.Y := Self.Y * Factor;
end;
	
function TelVector3f.DotProduct(Vector: TelVector3f): Single;
begin
  Result := (Self.X * Vector.X) + (Self.Y * Vector.Y);
end;

procedure TelVector3f.Normalize();
begin
  Self.X := Self.X / GetLength;
  Self.Y := Self.Y / GetLength;
end;

procedure TelVector3f.Zero();
begin
  Self.X := 0.0;
  Self.Y := 0.0;
  Self.Z := 0.0;
end;

procedure TelVector3f.One();
begin
  Self.X := 1.0;
  Self.Y := 1.0;
  Self.Z := 1.0;
end;

procedure TelVector3f.Forward();
begin
  Self.X := 0.0;
  Self.Y := 0.0;
  Self.Z := 1.0;
end;

procedure TelVector3f.Up();
begin
  Self.X := 0.0;
  Self.Y := 1.0;
  Self.Z := 0.0;
end;

procedure TelVector3f.Right();
begin
  Self.X := 1.0;
  Self.Y := 0.0;
  Self.Z := 0.0;
end;

procedure TelVector3i.Clear();
begin
  X := 0;
  Y := 0;
end;

function TelVector3i.GetLength(): Double;
begin
  Result := sqrt(X * X + Y * Y);
end;
	
procedure TelVector3i.Make(aX, aY, aZ: Integer);
begin
  X := aX;
  Y := aY;
  Z := aZ
end;
	
function TelVector3i.ToString(): String;
begin
  Result := Format('X: %d Y: %d, Z: %d', [X, Y, Z])
end;
	
function TelVector3i.ToVector3f(EmptyZ: Boolean = False): PelVector3f;
var
  tmpVec: TelVector3f;
begin
  if EmptyZ then tmpVec := makeV3f(X * 1.0, Y * 1.0, 0)
     else tmpVec := makeV3f(X * 1.0, Y * 1.0, Z * 1.0);

  Result := @tmpVec;
end;

function TelVector3i.ToVector2f(): PelVector2f;
var
  tmpVec: TelVector2f;
begin
  tmpVec := makeV2f(X * 1.0, Y * 1.0);

  Result := @tmpVec;
end;

function TelVector3i.ToVector2i(): PelVector2i;
var
  tmpVec: TelVector2i;
begin
  tmpVec := makeV2i(X, Y);

  Result := @tmpVec;
end;
	
procedure TelVector3i.Add(Vector: TelVector3i);
begin
  Self.X := Self.X + Vector.X;
  Self.Y := Self.Y + Vector.Y;
end;

procedure TelVector3i.Sub(Vector: TelVector3i);
begin
  Self.X := Self.Y - Vector.X;
  Self.Y := Self.Y - Vector.Y;
end;

procedure TelVector3i.Multiply(Vector: TelVector3i);
begin
  Self.X := Self.X * Vector.X;
  Self.Y := Self.Y * Vector.Y;
end;

procedure TelVector3i.Divide(Vector: TelVector3i);
begin
  Self.X := Trunc(Self.X / Vector.X);
  Self.Y := Trunc(Self.Y / Vector.Y);
end;

procedure TelVector3i.Scale(Factor: Single);
begin
  Self.X := Trunc(Self.X * Factor);
  Self.Y := Trunc(Self.Y * Factor);
end;
	
function TelVector3i.DotProduct(Vector: TelVector3i): Integer;
begin
  Result := (Self.X * Vector.X) + (Self.Y * Vector.Y);
end;

procedure TelVector3i.Normalize();
begin
  Self.X := Trunc(Self.X / GetLength);
  Self.Y := Trunc(Self.Y / GetLength);
end;

procedure TelVector3i.Zero();
begin
  Self.X := 0;
  Self.Y := 0;
  Self.Z := 0;
end;

procedure TelVector3i.One();
begin
  Self.X := 1;
  Self.Y := 1;
  Self.Z := 1;
end;

procedure TelVector3i.Forward();
begin
  Self.X := 0;
  Self.Y := 0;
  Self.Z := 1;
end;

procedure TelVector3i.Up();
begin
  Self.X := 0;
  Self.Y := 1;
  Self.Z := 0;
end;

procedure TelVector3i.Right();
begin
  Self.X := 1;
  Self.Y := 0;
  Self.Z := 0;
end;



procedure TelRect.Clear();
begin
  X := 0;
  Y := 0;
  W := 0;
  H := 0;
end;

procedure TelRect.Make(aX, aY, aW, aH: Integer);
begin
  X := aX * 1.0;
  Y := aY * 1.0;
  W := aW * 1.0;
  H := aH * 1.0;
end;

procedure TelRect.Make(aX, aY, aW, aH: Single);
begin
  X := aX;
  Y := aY;
  W := aW;
  H := aH;
end;

function TelRect.ContainsVector(aVector: TelVector2i): Boolean;
begin
  if (aVector.X >= Self.X) and
     (aVector.Y >= Self.Y) and
     (aVector.X <= (Self.X + Self.W)) and
     (aVector.Y <= (Self.Y + Self.H)) then Result := true else Result := false;
end;

function TelRect.ContainsVector(aVector: TelVector2f): Boolean;
begin
  if (aVector.X >= Self.X) and
     (aVector.Y >= Self.Y) and
     (aVector.X <= (Self.X + Self.W)) and
     (aVector.Y <= (Self.Y + Self.H)) then Result := true else Result := false;
end;

function TelRect.ContainsRect(aRect: TelRect): Boolean;
begin
  if (aRect.X >= Self.X) and
     (aRect.Y >= Self.Y) and
     (aRect.X <= (Self.X + Self.W)) and
     (aRect.Y <= (Self.Y + Self.H)) then Result := true else Result := false;
end;

{$ELSE}

function RectContainsVector(aRect: TelRect; aVector: TelVector2i): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
begin
  if (aVector.X >= aRect.X) and
     (aVector.Y >= aRect.Y) and
     (aVector.X <= (aRect.X + aRect.W)) and
     (aVector.Y <= (aRect.Y + aRect.H)) then Result := true else Result := false;
end;

function RectContainsVector(aRect: TelRect; aVector: TelVector2f): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
begin
  if (aVector.X >= aRect.X) and
     (aVector.Y >= aRect.Y) and
     (aVector.X <= (aRect.X + aRect.W)) and
     (aVector.Y <= (aRect.Y + aRect.H)) then Result := true else Result := false;
end;

function RectContainsRect(aRect, bRect: TelRect): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
begin
  if (aRect.X >= bRect.X) and
     (aRect.Y >= bRect.Y) and
     (aRect.X <= (bRect.X + bRect.W)) and
     (aRect.Y <= (bRect.Y + bRect.H)) then Result := true else Result := false;
end;
{$ENDIF}

end.
