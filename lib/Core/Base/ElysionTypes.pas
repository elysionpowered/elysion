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
  ElysionMath,
  Classes,
  SDLUtils,
  SysUtils;

type

  // Forward declarations
  PelColor = ^TelColor;
  PelVector2f = ^TelVector2f;
  PelVector2i = ^TelVector2i;
  PelVector3f = ^TelVector3f;
  PelVector3i = ^TelVector3i;

  PelRect = ^TelRect;
  PelSize = ^TelSize;


  PKeyIdent = ^TKeyIdent;

  { TKeyIdent }

  {$IFDEF FPC}
  TKeyIdent = object
  {$ELSE}
  TKeyIdent = record
  {$ENDIF}
    Name: String;
    Value: String;

    {$IFDEF CAN_METHODS}
      procedure Clear();

      procedure SetValue(aValue: Integer); Overload;
      procedure SetValue(aValue: Single); Overload;
      procedure SetValue(aValue: Boolean); Overload;

      function ToInt(): Integer;
      function ToFloat(): Single;
      function ToBool(): Boolean;

      function ToColor(): PelColor;
      function ToVector2f(): PelVector2f;
      function ToVector2i(): PelVector2i;
      function ToVector3f(): PelVector3f;
      function ToVector3i(): PelVector3i;
      function ToSize(): PelSize;
      function ToRect(): PelRect;

      function ToString(): String;
      function ToXML(): String;
      function ToJSON(): String;
    {$ENDIF}
  end;

  TKeyArray = array of TKeyIdent;


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
    procedure Clear(anAlpha: Byte = 255);
    procedure Make(aR, aG, aB: Byte; anA: Byte = 255);

    function ToKey(KeyName: String): PKeyIdent;
    function ToString(): String;

    procedure ToFloat(var floatR: Single; var floatG: Single; var floatB: Single; var floatA: Single);

    // Operators (has to compliant to earlier Delphi versions)
    procedure Add(Color: TelColor);
    procedure Sub(Color: TelColor);
    procedure Multiply(Color: TelColor);
    procedure Divide(Color: TelColor);
    procedure Scale(Factor: Single);

    function Equals(aColor: TelColor): Boolean;
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

    // Convert to other types
    function ToVector2i(): PelVector2i;
    function ToVector3f(): PelVector3f;
    function ToVector3i(): PelVector3i;
    function ToSize(): PelSize;
    function ToKey(KeyName: String): PKeyIdent;

    // Operators (has to compliant to earlier Delphi versions)
    procedure Add(Vector: TelVector2f);
    procedure Sub(Vector: TelVector2f);
    procedure Multiply(Vector: TelVector2f);
    procedure Divide(Vector: TelVector2f);
    procedure Scale(Factor: Single);

    // Vector stuff
    function DotProduct(Vector: TelVector2f): Single;
    procedure Normalize();

    function Equals(aVector: TelVector2f): Boolean;
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

    // Convert to other types
    function ToVector2f(): PelVector2f;
    function ToVector3f(): PelVector3f;
    function ToVector3i(): PelVector3i;
    function ToSize(): PelSize;
    function ToKey(KeyName: String): PKeyIdent;

    // Operators (has to compliant to earlier Delphi versions)
    procedure Add(Vector: TelVector2i);
    procedure Sub(Vector: TelVector2i);
    procedure Multiply(Vector: TelVector2i);
    procedure Divide(Vector: TelVector2i);
    procedure Scale(Factor: Single);

    // Vector stuff
    function DotProduct(Vector: TelVector2i): Integer;
    procedure Normalize();

    function Equals(aVector: TelVector2i): Boolean;
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

    // Convert to other types
    function ToVector3i(): PelVector3i;
    function ToVector2f(): PelVector2f;
    function ToVector2i(): PelVector2i;
    function ToSize(): PelSize;
    function ToKey(KeyName: String): PKeyIdent;

    // Operators (has to compliant to earlier Delphi versions)
    procedure Add(Vector: TelVector3f);
    procedure Sub(Vector: TelVector3f);
    procedure Multiply(Vector: TelVector3f);
    procedure Divide(Vector: TelVector3f);
    procedure Scale(Factor: Single);

    // Vector stuff
    function DotProduct(Vector: TelVector3f): Single;
    function CrossProduct(Vector: TelVector3f): PelVector3f;

    procedure Normalize();

    procedure Zero();
    procedure One();
    procedure Forward();
    procedure Up();
    procedure Right();

    function Equals(aVector: TelVector3f): Boolean;
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

    // Convert to other types
    function ToVector3f(EmptyZ: Boolean = False): PelVector3f;
    function ToVector2f(): PelVector2f;
    function ToVector2i(): PelVector2i;
    function ToSize(): PelSize;
    function ToKey(KeyName: String): PKeyIdent;

    // Operators (has to compliant to earlier Delphi versions)
    procedure Add(Vector: TelVector3i);
    procedure Sub(Vector: TelVector3i);
    procedure Multiply(Vector: TelVector3i);
    procedure Divide(Vector: TelVector3i);
    procedure Scale(Factor: Single);

    // Vector stuff
    function DotProduct(Vector: TelVector3i): Integer;
    function CrossProduct(Vector: TelVector3i): PelVector3i;

    procedure Normalize();

    procedure Zero();
    procedure One();
    procedure Forward();
    procedure Up();
    procedure Right();

    function Equals(aVector: TelVector3i): Boolean;
    {$ENDIF}
  end;


  // A vertex is basically just a position vector with a color attached to it
  TelVertex = record
    Vector: TelVector3f;
    Color: TelColor;
  end;

  { TelSize }

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

    // Convert to other types
    function ToVector2i(): PelVector2i;
    function ToVector2f(): PelVector2f;
    function ToVector3i(): PelVector3i;
    function ToVector3f(): PelVector3f;
    function ToKey(KeyName: String): PKeyIdent;

    // Operators (has to compliant to earlier Delphi versions)
    procedure Add(Size: TelSize);
    procedure Sub(Size: TelSize);
    procedure Multiply(Size: TelSize);
    procedure Divide(Size: TelSize);
    procedure Scale(Factor: Single);

    function Center(): PelVector2f; Overload;
    function Center(aRect: PelRect): PelVector2f; Overload;

    function GetAspectRatio(): Single;
    function IsWide(): Boolean;

    function Equals(aSize: TelSize): Boolean;
    {$ENDIF}
  end;

  { TelRect }

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
    procedure Make(aPosition: TelVector2f; aSize: TelSize); Overload;
    procedure Make(aPosition: TelVector2i; aSize: TelSize); Overload;

    function ToString(): String;
    function ToKey(KeyName: String): PKeyIdent;

    function Center(): PelVector2f; Overload;
    function Center(aRect: TelRect): PelVector2f; Overload;

    function ContainsVector(aVector: TelVector2i): Boolean; Overload;
    function ContainsVector(aVector: TelVector2f): Boolean; Overload;
    function ContainsRect(aRect: TelRect): Boolean; Overload;

    function GetAspectRatio(): Single;
    function IsWide(): Boolean;

    function Equals(aRect: TelRect): Boolean;
    function IsEmpty(): Boolean;
    {$ENDIF}
  end;

  // Display orientation (will be renamed in the future)
  TDisplayOrientation = (doLandscape, doPortrait);

  TelImageOffset = record
    Position: TelVector2f;
    Rotation: TelVector2f;
  end;

  TelImageRotation = record
    Angle: Single;
    Vector: TelVector3f;
  end;

  {$IFDEF FPC}

  { TelShadow }

  TelShadow = object
  {$ELSE}
  TelShadow = record
  {$ENDIF}
    Blur: Integer;
    Color: TelColor;
    Position: TelVector2f;
    Visible: Boolean;

    {$IFDEF CAN_METHODS}
    procedure Clear();
    {$ENDIF}
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

  TGradientStyle = SDLUtils.TGradientStyle;
  TColorVertices = array[0..3] of TelColor;

  TelBoundingBox = (bbDefault, bbCustom, bbPixel);

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

  TelElementDecoration = (edMargin, edPadding, edBorder);
  TelElementDecorations = set of TelElementDecoration;



  // See: TelGraphicObject.Generate;
  TGenerateMode = (gmAuto, gmRGB, gmRGBA);

  {%region 'Logger priority types'}
  // Logger message type
  TLogMessageType =
    (ltError,   //< Displays message as an error
     ltWarning, //< Displays message as a warning
     ltNote);   //< Displays message as a note

  TLogMessagePriorities = set of TLogMessageType;
  {%endregion}

  {%region 'General functions'}
  function makeGradient(StartColor: TelColor; EndColor: TelColor; GradientStyle: TGradientStyle = gsVertical): TelGradient; {$IFDEF CAN_INLINE} inline; {$ENDIF}

  function makeV2f(aX, aY: Single): TelVector2f; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function makeV2i(aX, aY: Integer): TelVector2i; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function makeV3f(aX, aY: Single; aZ: Single = 0.0): TelVector3f; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function makeV3i(aX, aY: Integer; aZ: Integer = 0): TelVector3i; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function makeSize(aWidth, aHeight: Integer): TelSize; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function makeRect(aX, aY, aW, aH: Single): TelRect; {$IFDEF CAN_INLINE} inline; {$ENDIF}

  function makeCol(aR, aG, aB: Byte; anA: Byte = 255): TelColor; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  //function makeCol(aR, aG, aB: Single; anA: Single = 1.0): TelColor; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

  function makeP2D(aX, aY: Integer): TelVector2i; Overload; deprecated;
  function makeP2D(aX, aY: Single): TelVector2f; Overload; deprecated;
  function makeP3D(aX, aY: Integer; aZ: Integer = 0): TelVector3i; Overload; deprecated;
  function makeP3D(aX, aY: Single; aZ: Single = 0.0): TelVector3f; Overload; deprecated;

  function makeKey(KeyName, KeyValue: String): TKeyIdent; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function makeKey(KeyName: String; KeyValue: Integer): TKeyIdent; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function makeKey(KeyName: String; KeyValue: Single): TKeyIdent; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  {%endregion}


  function IsRectEmpty(Rect: TelRect): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}


  function VectorEquals(VecOne, VecTwo: TelVector2f): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function VectorEquals(VecOne, VecTwo: TelVector2i): Boolean; Overload;{$IFDEF CAN_INLINE} inline; {$ENDIF}

  function VectorEquals(VecOne, VecTwo: TelVector3f): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function VectorEquals(VecOne, VecTwo: TelVector3i): Boolean; Overload;{$IFDEF CAN_INLINE} inline; {$ENDIF}

  function ColorEquals(ColorOne, ColorTwo: TelColor): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}


  function IsInRange(Min, Max, Value: Integer): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function IsInRange(Min, Max, Value: Single): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

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

(*function makeCol(aR, aG, aB: Single; anA: Single = 1.0): TelColor;
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
end;*)

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

function makeKey(KeyName, KeyValue: String): TKeyIdent;
var
  tmpKey: TKeyIdent;
begin
  tmpKey.Name := KeyName;
  tmpKey.Value := KeyValue;

  Result := tmpKey;
end;

function makeKey(KeyName: String; KeyValue: Integer): TKeyIdent;
var
  tmpKey: TKeyIdent;
begin
  tmpKey.Name := KeyName;
  tmpKey.Value := IntToStr(KeyValue);

  Result := tmpKey;
end;

function makeKey(KeyName: String; KeyValue: Single): TKeyIdent;
var
  tmpKey: TKeyIdent;
begin
  tmpKey.Name := KeyName;
  tmpKey.Value := FloatToStr(KeyValue);

  Result := tmpKey;
end;


function IsRectEmpty(Rect: TelRect): Boolean;
begin
  Result := ((Rect.X = 0) and (Rect.Y = 0) and (Rect.W = 0) and (Rect.H = 0));
end;

function VectorEquals(VecOne, VecTwo: TelVector2f): Boolean;
begin
  Result := ((VecOne.X = VecTwo.X) or (VecOne.Y = VecTwo.Y));
end;

function VectorEquals(VecOne, VecTwo: TelVector2i): Boolean;
begin
  Result := ((VecOne.X = VecTwo.X) or (VecOne.Y = VecTwo.Y));
end;

function VectorEquals(VecOne, VecTwo: TelVector3f): Boolean;
begin
  Result := ((VecOne.X = VecTwo.X) or (VecOne.Y = VecTwo.Y) or (VecOne.Z = VecTwo.Z));
end;

function VectorEquals(VecOne, VecTwo: TelVector3i): Boolean;
begin
  Result := ((VecOne.X = VecTwo.X) or (VecOne.Y = VecTwo.Y) or (VecOne.Z = VecTwo.Z));
end;

function ColorEquals(ColorOne, ColorTwo: TelColor): Boolean;
begin
  Result := ((ColorOne.R = ColorTwo.R) or (ColorOne.G = ColorTwo.G) or (ColorOne.B = ColorTwo.B) or (ColorOne.A = ColorOne.A));
end;

function IsInRange(Min, Max, Value: Integer): Boolean;
begin
  Result :=  ((Value >= Min) and (Value <= Max));
end;

function IsInRange(Min, Max, Value: Single): Boolean;
begin
  Result :=  ((Value >= Min) and (Value <= Max));
end;

{ TelShadow }

procedure TelShadow.Clear();
begin
  Self.Blur := 0;
  Self.Color.Clear();
  Self.Position.Clear();
  Self.Visible := false;
end;

{$IFDEF CAN_METHODS}

procedure TKeyIdent.Clear();
begin
  Value := '';
end;

procedure TKeyIdent.SetValue(aValue: Integer);
begin
  Value := IntToStr(aValue);
end;

procedure TKeyIdent.SetValue(aValue: Single);
begin
  Value := FloatToStr(aValue);
end;

procedure TKeyIdent.SetValue(aValue: Boolean);
begin
  if aValue then Value := 'true'
  else Value := 'false';
end;

function TKeyIdent.ToInt(): Integer;
begin
  try
    Result := StrToInt(Value);
  except
    on Exception: EConvertError do Exit;
  end;
end;

function TKeyIdent.ToFloat(): Single;
begin
  try
    Result := StrToFloat(Value);
  except
    on Exception: EConvertError do Exit;
  end;
end;

function TKeyIdent.ToBool(): Boolean;
begin
  if ((Value = 'true') or (Value = '1')) then Result := true
  else Result := false;
end;

function TKeyIdent.ToColor(): PelColor;
var
  tmpString: String;
  posR, posG, posB, posA: Integer;
  tmpR, tmpG, tmpB, tmpA: Byte;
  tmpColor: TelColor;
begin
  // Remove spaces and double colons
  tmpString := StringReplace(Value, ' ', '', [rfReplaceAll, rfIgnoreCase]);
  tmpString := StringReplace(Value, ':', '', [rfReplaceAll, rfIgnoreCase]);

  posR := Pos('R', tmpString);
  posG := Pos('G', tmpString);
  posB := Pos('B', tmpString);
  posA := Pos('A', tmpString);

  tmpR := StrToInt(Copy(tmpString, posR + 1, posG - (posR + 1)));
  tmpG := StrToInt(Copy(tmpString, posG + 1, posB - (posG + 1)));
  tmpB := StrToInt(Copy(tmpString, posB + 1, posA - (posB + 1)));
  tmpA := StrToInt(Copy(tmpString, posA + 1, Length(tmpString)));

  tmpColor := makeCol(tmpR, tmpG, tmpB, tmpA);

  Result := @tmpColor;
end;

function TKeyIdent.ToVector2f(): PelVector2f;
var
  tmpString: String;
  posX, posY: Integer;
  tmpX, tmpY: Single;
  tmpVec: TelVector2f;
begin
  // Remove spaces and double colons
  tmpString := StringReplace(Value, ' ', '', [rfReplaceAll, rfIgnoreCase]);
  tmpString := StringReplace(Value, ':', '', [rfReplaceAll, rfIgnoreCase]);

  posX := Pos('X', tmpString);
  posY := Pos('Y', tmpString);

  tmpX := StrToInt(Copy(tmpString, posX + 1, posY - (posX + 1)));
  tmpY := StrToInt(Copy(tmpString, posY + 1, Length(tmpString)));

  tmpVec := makeV2f(tmpX, tmpY);

  Result := @tmpVec;
end;

function TKeyIdent.ToVector2i(): PelVector2i;
var
  tmpString: String;
  posX, posY: Integer;
  tmpX, tmpY: Integer;
  tmpVec: TelVector2i;
begin
  // Remove spaces and double colons
  tmpString := StringReplace(Value, ' ', '', [rfReplaceAll, rfIgnoreCase]);
  tmpString := StringReplace(Value, ':', '', [rfReplaceAll, rfIgnoreCase]);

  posX := Pos('X', tmpString);
  posY := Pos('Y', tmpString);

  tmpX := StrToInt(Copy(tmpString, posX + 1, posY - (posX + 1)));
  tmpY := StrToInt(Copy(tmpString, posY + 1, Length(tmpString)));

  tmpVec := makeV2i(tmpX, tmpY);

  Result := @tmpVec;
end;

function TKeyIdent.ToVector3f(): PelVector3f;
var
  tmpString: String;
  posX, posY, posZ: Integer;
  tmpX, tmpY, tmpZ: Single;
  tmpVec: TelVector3f;
begin
  // Remove spaces and double colons
  tmpString := StringReplace(Value, ' ', '', [rfReplaceAll, rfIgnoreCase]);
  tmpString := StringReplace(Value, ':', '', [rfReplaceAll, rfIgnoreCase]);

  posX := Pos('X', tmpString);
  posY := Pos('Y', tmpString);
  posZ := Pos('Z', tmpString);

  tmpX := StrToInt(Copy(tmpString, posX + 1, posY - (posX + 1)));
  tmpY := StrToInt(Copy(tmpString, posY + 1, posZ - (posY + 1)));
  tmpZ := StrToInt(Copy(tmpString, posZ + 1, Length(tmpString)));

  tmpVec := makeV3f(tmpX, tmpY, tmpZ);

  Result := @tmpVec;
end;

function TKeyIdent.ToVector3i(): PelVector3i;
var
  tmpString: String;
  posX, posY, posZ: Integer;
  tmpX, tmpY, tmpZ: Integer;
  tmpVec: TelVector3i;
begin
  // Remove spaces and double colons
  tmpString := StringReplace(Value, ' ', '', [rfReplaceAll, rfIgnoreCase]);
  tmpString := StringReplace(Value, ':', '', [rfReplaceAll, rfIgnoreCase]);

  posX := Pos('X', tmpString);
  posY := Pos('Y', tmpString);
  posZ := Pos('Z', tmpString);

  tmpX := StrToInt(Copy(tmpString, posX + 1, posY - (posX + 1)));
  tmpY := StrToInt(Copy(tmpString, posY + 1, posZ - (posY + 1)));
  tmpZ := StrToInt(Copy(tmpString, posZ + 1, Length(tmpString)));

  tmpVec := makeV3i(tmpX, tmpY, tmpZ);

  Result := @tmpVec;
end;

function TKeyIdent.ToSize(): PelSize;
var
  tmpString: String;
  posW, posH: Integer;
  tmpW, tmpH: Integer;
  tmpSize: TelSize;
begin
  // Remove spaces and double colons
  tmpString := StringReplace(Value, ' ', '', [rfReplaceAll, rfIgnoreCase]);
  tmpString := StringReplace(Value, ':', '', [rfReplaceAll, rfIgnoreCase]);

  posW := Pos('Width', tmpString);
  posH := Pos('Height', tmpString);

  tmpW := StrToInt(Copy(tmpString, posW + 1, posH - (posW + 1)));
  tmpH := StrToInt(Copy(tmpString, posH + 1, Length(tmpString)));

  tmpSize := makeSize(tmpW, tmpH);

  Result := @tmpSize;
end;

function TKeyIdent.ToRect(): PelRect;
var
  tmpString: String;
  posX, posY, posW, posH: Integer;
  tmpX, tmpY, tmpW, tmpH: Single;
  tmpRect: TelRect;
begin
  // Remove spaces and double colons
  tmpString := StringReplace(Value, ' ', '', [rfReplaceAll, rfIgnoreCase]);
  tmpString := StringReplace(Value, ':', '', [rfReplaceAll, rfIgnoreCase]);

  posX := Pos('X', tmpString);
  posY := Pos('Y', tmpString);
  posW := Pos('W', tmpString);
  posH := Pos('H', tmpString);

  tmpX := StrToInt(Copy(tmpString, posX + 1, posY - (posX + 1)));
  tmpY := StrToInt(Copy(tmpString, posY + 1, posW - (posY + 1)));
  tmpW := StrToInt(Copy(tmpString, posW + 1, posH - (posW + 1)));
  tmpH := StrToInt(Copy(tmpString, posH + 1, Length(tmpString)));

  tmpRect := makeRect(tmpX, tmpY, tmpW, tmpH);

  Result := @tmpRect;
end;

function TKeyIdent.ToString(): String;
begin
  Result := Format('Key "%s": %s', [Name, Value]);
end;

function TKeyIdent.ToXML(): String;
begin
  Result := Format('<key name="%s">%s</key>', [Name, Value]);
end;

function TKeyIdent.ToJSON(): String;
begin
  Result := Format('"%s": {"%s"}', [Name, Value]);
end;


procedure TelColor.Clear(anAlpha: Byte = 255);
begin
  R := 0;
  G := 0;
  B := 0;
  A := anAlpha;
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

function TelColor.ToKey(KeyName: String): PKeyIdent;
var
  tmpKey: TKeyIdent;
begin
  tmpKey.Name := KeyName;
  tmpKey.Value := Self.ToString();

  Result := @tmpKey;
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

function TelColor.Equals(aColor: TelColor): Boolean;
begin
  Result := ((Self.R = aColor.R) and (Self.G = aColor.G) and (Self.B = aColor.B) and (Self.A = aColor.A));
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

function TelVector2f.ToSize(): PelSize;
var
  tmpSize: TelSize;
begin
  tmpSize := makeSize(Trunc(X), Trunc(Y));

  Result := @tmpSize;
end;

function TelVector2f.ToKey(KeyName: String): PKeyIdent;
var
  tmpKey: TKeyIdent;
begin
  tmpKey.Name := KeyName;
  tmpKey.Value := Self.ToString();

  Result := @tmpKey;
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

function TelVector2f.Equals(aVector: TelVector2f): Boolean;
begin
  Result := ((Self.X = aVector.X) and (Self.Y = aVector.Y));
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

function TelVector2i.ToKey(KeyName: String): PKeyIdent;
var
  tmpKey: TKeyIdent;
begin
  tmpKey.Name := KeyName;
  tmpKey.Value := Self.ToString();

  Result := @tmpKey;
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

function TelVector2i.Equals(aVector: TelVector2i): Boolean;
begin
  Result := ((Self.X = aVector.X) and (Self.Y = aVector.Y));
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

function TelSize.ToVector2f(): PelVector2f;
var
  tmpVec: TelVector2f;
begin
  tmpVec := makeV2f(Width, Height);

  Result := @tmpVec;
end;

function TelSize.ToVector2i(): PelVector2i;
var
  tmpVec: TelVector2i;
begin
  tmpVec := makeV2i(Width, Height);

  Result := @tmpVec;
end;

function TelSize.ToVector3f(): PelVector3f;
var
  tmpVec: TelVector3f;
begin
  tmpVec := makeV3f(Width, Height);

  Result := @tmpVec;
end;

function TelSize.ToVector3i(): PelVector3i;
var
  tmpVec: TelVector3i;
begin
  tmpVec := makeV3i(Width, Height);

  Result := @tmpVec;
end;

function TelSize.ToKey(KeyName: String): PKeyIdent;
var
  tmpKey: TKeyIdent;
begin
  tmpKey.Name := KeyName;
  tmpKey.Value := Self.ToString();

  Result := @tmpKey;
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

function TelSize.Center(): PelVector2f;
var
  tmpVec: TelVector2f;
begin
  tmpVec := makeV2f(Self.Width / 2, Self.Height / 2);

  Result := @tmpVec;
end;

function TelSize.Center(aRect: PelRect): PelVector2f;
var
  tmpVec: TelVector2f;
begin
  tmpVec := makeV2f(Self.Width - (aRect^.W - aRect^.X) / 2, Self.Height - (aRect^.H - aRect^.Y) / 2);

  Result := @tmpVec;
end;

function TelSize.GetAspectRatio(): Single;
begin
  Result := Width / Height;
end;

function TelSize.IsWide(): Boolean;
begin
  Result := (GetAspectRatio > (4 / 3));
end;

function TelSize.Equals(aSize: TelSize): Boolean;
begin
  Result := ((Self.Width = aSize.Width) and (Self.Height = aSize.Height));
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

function TelVector3f.ToSize(): PelSize;
var
  tmpSize: TelSize;
begin
  tmpSize := makeSize(Trunc(X), Trunc(Y));

  Result := @tmpSize;
end;

function TelVector3f.ToKey(KeyName: String): PKeyIdent;
var
  tmpKey: TKeyIdent;
begin
  tmpKey.Name := KeyName;
  tmpKey.Value := Self.ToString();

  Result := @tmpKey;
end;

procedure TelVector3f.Add(Vector: TelVector3f);
begin
  Self.X := Self.X + Vector.X;
  Self.Y := Self.Y + Vector.Y;
  Self.Z := Self.Z + Vector.Z;
end;

procedure TelVector3f.Sub(Vector: TelVector3f);
begin
  Self.X := Self.Y - Vector.X;
  Self.Y := Self.Y - Vector.Y;
  Self.Z := Self.Z - Vector.Z;
end;

procedure TelVector3f.Multiply(Vector: TelVector3f);
begin
  Self.X := Self.X * Vector.X;
  Self.Y := Self.Y * Vector.Y;
  Self.Z := Self.Z * Vector.Z;
end;

procedure TelVector3f.Divide(Vector: TelVector3f);
begin
  Self.X := Self.X / Vector.X;
  Self.Y := Self.Y / Vector.Y;
  Self.Z := Self.Z / Vector.Z;
end;

procedure TelVector3f.Scale(Factor: Single);
begin
  Self.X := Self.X * Factor;
  Self.Y := Self.Y * Factor;
  Self.Z := Self.Z * Factor;
end;
	
function TelVector3f.DotProduct(Vector: TelVector3f): Single;
begin
  Result := (Self.X * Vector.X) + (Self.Y * Vector.Y) + (Self.Z * Vector.Z);
end;

function TelVector3f.CrossProduct(Vector: TelVector3f): PelVector3f;
var
  tmpVec: TelVector3f;
begin
  tmpVec := makeV3f(Self.Y * Vector.Z - Vector.Y * Self.Z, Self.Z * Vector.X - Vector.Z * Self.X, Self.X * Vector.Y - Vector.X * Self.Y);

  Result := @tmpVec;
end;

procedure TelVector3f.Normalize();
begin
  Self.X := Self.X / GetLength;
  Self.Y := Self.Y / GetLength;
  Self.Z := Self.Z / GetLength;
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

function TelVector3f.Equals(aVector: TelVector3f): Boolean;
begin
  Result := ((Self.X = aVector.X) and (Self.Y = aVector.Y) and (Self.Z = aVector.Z));
end;

procedure TelVector3i.Clear();
begin
  X := 0;
  Y := 0;
  Z := 0;
end;

function TelVector3i.GetLength(): Double;
begin
  Result := sqrt(X * X + Y * Y + Z * Z);
end;
	
procedure TelVector3i.Make(aX, aY, aZ: Integer);
begin
  X := aX;
  Y := aY;
  Z := aZ;
end;
	
function TelVector3i.ToString(): String;
begin
  Result := Format('X: %d Y: %d, Z: %d', [X, Y, Z]);
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

function TelVector3i.ToSize(): PelSize;
var
  tmpSize: TelSize;
begin
  tmpSize := makeSize(X, Y);

  Result := @tmpSize;
end;

function TelVector3i.ToKey(KeyName: String): PKeyIdent;
var
  tmpKey: TKeyIdent;
begin
  tmpKey.Name := KeyName;
  tmpKey.Value := Self.ToString();

  Result := @tmpKey;
end;

procedure TelVector3i.Add(Vector: TelVector3i);
begin
  Self.X := Self.X + Vector.X;
  Self.Y := Self.Y + Vector.Y;
  Self.Z := Self.Z + Vector.Z;
end;

procedure TelVector3i.Sub(Vector: TelVector3i);
begin
  Self.X := Self.Y - Vector.X;
  Self.Y := Self.Y - Vector.Y;
  Self.Z := Self.Z - Vector.Z;
end;

procedure TelVector3i.Multiply(Vector: TelVector3i);
begin
  Self.X := Self.X * Vector.X;
  Self.Y := Self.Y * Vector.Y;
  Self.Z := Self.Z * Vector.Z;
end;

procedure TelVector3i.Divide(Vector: TelVector3i);
begin
  Self.X := Trunc(Self.X / Vector.X);
  Self.Y := Trunc(Self.Y / Vector.Y);
  Self.Z := Trunc(Self.Z / Vector.Z);
end;

procedure TelVector3i.Scale(Factor: Single);
begin
  Self.X := Trunc(Self.X * Factor);
  Self.Y := Trunc(Self.Y * Factor);
  Self.Z := Trunc(Self.Z * Factor);
end;
	
function TelVector3i.DotProduct(Vector: TelVector3i): Integer;
begin
  Result := (Self.X * Vector.X) + (Self.Y * Vector.Y) + (Self.Z * Vector.Z);
end;

function TelVector3i.CrossProduct(Vector: TelVector3i): PelVector3i;
var
  tmpVec: TelVector3i;
begin
  tmpVec := makeV3i(Self.Y * Vector.Z - Vector.Y * Self.Z, Self.Z * Vector.X - Vector.Z * Self.X, Self.X * Vector.Y - Vector.X * Self.Y);

  Result := @tmpVec;
end;

procedure TelVector3i.Normalize();
begin
  Self.X := Trunc(Self.X / GetLength);
  Self.Y := Trunc(Self.Y / GetLength);
  Self.Z := Trunc(Self.Z / GetLength);
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

function TelVector3i.Equals(aVector: TelVector3i): Boolean;
begin
  Result := ((Self.X = aVector.X) and (Self.Y = aVector.Y) and (Self.Z = aVector.Z));
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

procedure TelRect.Make(aPosition: TelVector2f; aSize: TelSize);
begin
  X := aPosition.X;
  Y := aPosition.Y;
  W := aSize.Width * 1.0;
  H := aSize.Height * 1.0;
end;

procedure TelRect.Make(aPosition: TelVector2i; aSize: TelSize);
begin
  X := aPosition.X * 1.0;
  Y := aPosition.Y * 1.0;
  W := aSize.Width * 1.0;
  H := aSize.Height * 1.0;
end;

function TelRect.ToString(): String;
begin
  Result := Format('X: %f Y: %f W: %f H: %f', [X, Y, W, H]);
end;

function TelRect.ToKey(KeyName: String): PKeyIdent;
var
  tmpKey: TKeyIdent;
begin
  tmpKey.Name := KeyName;
  tmpKey.Value := Self.ToString();

  Result := @tmpKey;
end;

function TelRect.Center(): PelVector2f;
var
  tmpVec: TelVector2f;
begin
  tmpVec := makeV2f((Self.W - Self.X) / 2, (Self.H - Self.Y) / 2);

  Result := @tmpVec;
end;

function TelRect.Center(aRect: TelRect): PelVector2f;
var
  tmpVec: TelVector2f;
begin
  tmpVec := makeV2f(Self.W - (aRect.W - aRect.X) / 2, Self.H - (aRect.H - aRect.Y));

  Result := @tmpVec;
end;

function TelRect.ContainsVector(aVector: TelVector2i): Boolean;
begin
  Result := ((aVector.X >= Self.X) and (aVector.Y >= Self.Y) and (aVector.X <= (Self.X + Self.W)) and (aVector.Y <= (Self.Y + Self.H)));
end;

function TelRect.ContainsVector(aVector: TelVector2f): Boolean;
begin
  Result := ((aVector.X >= Self.X) and (aVector.Y >= Self.Y) and (aVector.X <= (Self.X + Self.W)) and (aVector.Y <= (Self.Y + Self.H)));
end;

function TelRect.ContainsRect(aRect: TelRect): Boolean;
begin
  Result := ((aRect.X >= Self.X) and (aRect.Y >= Self.Y) and (aRect.X <= (Self.X + Self.W)) and (aRect.Y <= (Self.Y + Self.H)));
end;

function TelRect.GetAspectRatio(): Single;
begin
  Result := W / H;
end;

function TelRect.IsWide(): Boolean;
begin
  Result := (GetAspectRatio > (4 / 3));
end;

function TelRect.Equals(aRect: TelRect): Boolean;
begin
  Result := ((aRect.X = Self.X) and (aRect.Y = Self.Y) and (aRect.W = Self.W) and (aRect.H = Self.H));
end;

function TelRect.IsEmpty(): Boolean;
begin
  Result := ((X = 0) and (Y = 0) and (W = 0) and (H = 0));
end;

{$ELSE}

function RectContainsVector(aRect: TelRect; aVector: TelVector2i): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
begin
  Result := ((aVector.X >= aRect.X) and (aVector.Y >= aRect.Y) and (aVector.X <= (aRect.X + aRect.W)) and (aVector.Y <= (aRect.Y + aRect.H)));
end;

function RectContainsVector(aRect: TelRect; aVector: TelVector2f): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
begin
  Result := ((aVector.X >= aRect.X) and (aVector.Y >= aRect.Y) and (aVector.X <= (aRect.X + aRect.W)) and (aVector.Y <= (aRect.Y + aRect.H)));
end;

function RectContainsRect(aRect, bRect: TelRect): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
begin
  Result := (((aRect.X >= bRect.X) and (aRect.Y >= bRect.Y) and (aRect.X <= (bRect.X + bRect.W)) and (aRect.Y <= (bRect.Y + bRect.H)));
end;
{$ENDIF}

end.
