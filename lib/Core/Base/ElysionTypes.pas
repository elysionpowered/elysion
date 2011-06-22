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
  ElysionColor,
  ElysionMath,
  Classes,
  SysUtils;


const
  // Version information
  VER_MAJOR = 11;
  VER_MINOR = 06;
  VER_REVISION = 'a';

  VER_CODENAME = 'Echo'; //< Codename
  VER_STABLE = true;

  // Other random stuff mostly regarding version information
  VER_CODENAME_UNIVERSE = 'Dollhouse'; //< From which universe the codename comes from
  VER_CODENAME_RANDOMQUOTE = 'Did I fall asleep?'; //< Random quote from that universe

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



type
  PelRect = ^TelRect;
  PelSize = ^TelSize;


  // A vertex is basically just a position vector with a color attached to it
  (*TelVertex = record
    Vector: TelVector3f;
    Color: TelColor;
  end;*)

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

  // TelVideoFlags
  TelVideoFlag =
    (vfNull,      //< vfNull: Use for console applications, no video surface will be created
     vfAuto,      //< vfAuto: Automatically checks if hardware or software render mode are available
     vfHardware,  //< vfHardware: Use hardware surface
     vfSoftware); //< vfSoftware: Use software surface

  TelProjectionMode = (pmPerspective, pmOrtho);

  TelBoundingBox = (bbDefault, bbCustom, bbPixel);

  TAlignVertical = (avNone, avTop, avBottom, avCenter);
  TAlignHorizontal = (ahNone, ahLeft, ahRight, ahCenter);

  TelAssetType = (atTexture, atSprite, atParallexSprite, atSpriteSheet);

  TelAlignment = record
    Vertical: TAlignVertical;
    Horizontal: TAlignHorizontal;
  end;

  { TelExtValue }

  TelExtValue = class
  protected
    fLeft, fTop, fRight, fBottom: Single;

    function GetValue: Single; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure SetValue(const AValue: Single); {$IFDEF CAN_INLINE} inline; {$ENDIF}
  public
    constructor Create;
    destructor Destroy; Override;

    function IsEmpty(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  published
    property Value: Single read GetValue write SetValue;

    property Left: Single read fLeft write fLeft;
    property Top: Single read fTop write fTop;
    property Right: Single read fRight write fRight;
    property Bottom: Single read fBottom write fBottom;
  end;

  (*TelMargin = record
    case Byte of
    0: (Value: Single);
    1: (Left, Top, Right, Bottom: Single);
  end;

  TelPadding = record
    case Byte of
    0: (Value: Single);
    1: (Left, Top, Right, Bottom: Single);
  end;*)

  TelBorderStyle = (bsSolid, bsDashed, bsDouble, bsDotted, bsGroove, bsRidge, bsInset, bsOutset);

  TelBorderRadius = class
  protected
    fTopLeft, fTopRight, fBottomLeft, fBottomRight: Single;

    function GetValue: Single;
    procedure SetValue(const AValue: Single);
  public
    constructor Create; Overload;
    constructor Create(aTopLeft, aTopRight, aBottomLeft, aBottomRight: Single); Overload;
    constructor Create(aRect: TelRect); Overload;

    destructor Destroy; Override;

    function IsEmpty(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Equals(aBorderRadius: TelBorderRadius): Boolean; Overload;
  published
    property Value: Single read GetValue write SetValue;

    property TopLeft: Single read fTopLeft write fTopLeft;
    property TopRight: Single read fTopRight write fTopRight;
    property BottomLeft: Single read fBottomLeft write fBottomLeft;
    property BottomRight: Single read fBottomRight write fBottomRight;
  end;

  { TelBorderSide }

  TelBorderSide = class
  protected
    fWidth: Single;
    fStyle: TelBorderStyle;
  public
    constructor Create;
    destructor Destroy; Override;
  public
    Color: TelColor;
  published
    property Width: Single read fWidth write fWidth;
    property Style: TelBorderStyle read fStyle write fStyle;
  end;

  { TelBorder }

  TelBorder = class
  protected
    fLeft, fTop, fRight, fBottom: TelBorderSide;
    fRadius: TelBorderRadius;

    function GetColor: TelColor; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure SetColor(const AValue: TelColor); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function GetWidth: Single; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure SetWidth(AValue: Single); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function GetStyle: TelBorderStyle; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure SetStyle(AValue: TelBorderStyle); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function IsEmpty(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  public
    constructor Create;
    destructor Destroy; Override;
  public
    property Color: TelColor read GetColor write SetColor;
  published
    property Radius: TelBorderRadius read fRadius write fRadius;

    property Width: Single read GetWidth write SetWidth;
    property Style: TelBorderStyle read GetStyle write SetStyle;

    property Left: TelBorderSide read fLeft write fLeft;
    property Top: TelBorderSide read fTop write fTop;
    property Right: TelBorderSide read fRight write fRight;
    property Bottom: TelBorderSide read fBottom write fBottom;
  end;

  (*TelBorderRadius = record
    case Byte of
    0: (Value: Single);
    1: (LeftTop, LeftBottom, RightTop, RightBottom: Single);
  end;

  TelBorderSide = record
    Style: TelBorderStyle;
    Width: Single;
    Color: TelColor;
    Radius: TelBorderRadius;
  end;

  TelBorder = record
    case Byte of
    0:
    (
      Style: TelBorderStyle;
      Width: Single;
      Color: TelColor;
      Radius: TelBorderRadius;
    );
    1: (Left, Top, Right, Bottom: TelBorderSide);
  end;*)

  TelElementDecoration = (edMargin, edPadding, edBorder);
  TelElementDecorations = set of TelElementDecoration;


  { TelGaugeFloat }

  TelGaugeFloat = class
  protected
    fValue, fMin, fMax: Single;

    procedure SetValue(AValue: Single); {$IFDEF CAN_INLINE} inline; {$ENDIF}
  public
    constructor Create; Overload;
    constructor Create(Min, Max: Single; Value: Single = 0.0);

    destructor Destroy; Override;
  published
    property Min: Single read fMin write fMin;
    property Max: Single read fMax write fMax;
    property Value: Single read fValue write SetValue;
  end;

  { TelGaugeInt }

  TelGaugeInt = class
  protected
    fValue, fMin, fMax: Integer;

    procedure SetValue(AValue: Integer); {$IFDEF CAN_INLINE} inline; {$ENDIF}
  public
    constructor Create; Overload;
    constructor Create(Min, Max: Integer; Value: Integer = 0);

    destructor Destroy; Override;
  published
    property Min: Integer read fMin write fMin;
    property Max: Integer read fMax write fMax;
    property Value: Integer read fValue write SetValue;
  end;

  { TelGauge }

  TelGauge = TelGaugeFloat;


  // See: TelGraphicObject.Generate;
  TGenerateMode = (gmAuto, gmRGB, gmRGBA);

  {%region 'Animation types'}
  // Each animator type responds to a node property
  TelAnimationType = (atAlpha, atPosition, atOrigin, atRotation, atColor, atScale, atShadow);

  // Different animator transitions: Only linear is working right now, the others are placeholders
  TelAnimationTransition = (atLinear, atEaseIn, atEaseOut, atEaseInOut, atBounce);

  // Animation KeyFrame
  TelAnimationKeyFrame = record
    case AnimType: TelAnimationType of
      atAlpha: (Alpha: Byte);
      atPosition: (Position: TelVector3f);
      atOrigin: (Origin: TelVector2f);
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
      atOrigin: (StartOrigin, EndOrigin: TelVector2f);
      atRotation: (StartRotation, EndRotation: TelImageRotation);
      atColor: (StartColor, EndColor: TelColor);
      atScale: (StartScale, EndScale: TelVector2f);
      atShadow: (StartShadow, EndShadow: TelShadow);
  end;
  {%endregion}

  {%region 'Logger priority types'}
  // Logger message type
  TLogMessageType =
    (ltError,   //< Displays message as an error
     ltWarning, //< Displays message as a warning
     ltNote);   //< Displays message as a note

  TLogMessagePriorities = set of TLogMessageType;
  {%endregion}

  TelEvent = procedure() of object;

  {$IFNDEF CAN_METHODS}
  function RectContainsVector(aRect: TelRect; aVector: TelVector2i): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function RectContainsVector(aRect: TelRect; aVector: TelVector2f): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function RectContainsRect(aRect, bRect: TelRect): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  {$ENDIF}
  

  {%region 'General functions'}
  function makeSize(aWidth, aHeight: Integer): TelSize; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function makeRect(aX, aY, aW, aH: Single): TelRect; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  {%endregion}


  function IsRectEmpty(Rect: TelRect): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  
  function fromVector2i( aName : String; aValue : TelVector2i ): TKeyIdent;  
  function fromVector2f( aName : String; aValue : TelVector2f ): TKeyIdent;
  function fromVector3i( aName : String; aValue : TelVector3i ): TKeyIdent;
  function fromVector3f( aName : String; aValue : TelVector3f ): TKeyIdent;

  function FromVector2i( aValue : TelVector2i ): TelSize;
  function FromVector2f( aValue : TelVector2f ): TelSize;
  function FromVector3i( aValue : TelVector3i ): TelSize;
  function FromVector3f( aValue : TelVector3f ): TelSize;



implementation


function FromVector2i( aValue : TelVector2i ): TelSize;
var
  tmpSize: TelSize;
begin
  tmpSize := makeSize(Trunc(aValue.X), Trunc(aValue.Y));

  Result := tmpSize;
end;

function FromVector2f( aValue : TelVector2f ): TelSize;
var
  tmpSize: TelSize;
begin
  tmpSize := makeSize(Trunc(aValue.X), Trunc(aValue.Y));

  Result := tmpSize;
end;

function FromVector3i( aValue : TelVector3i ): TelSize;
var
  tmpSize: TelSize;
begin
  tmpSize := makeSize(Trunc(aValue.X), Trunc(aValue.Y));

  Result := tmpSize;
end;

function FromVector3f( aValue : TelVector3f ): TelSize;
var
  tmpSize: TelSize;
begin
  tmpSize := makeSize(Trunc(aValue.X), Trunc(aValue.Y));

  Result := tmpSize;
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
  Result := ((Rect.X = 0) and (Rect.Y = 0) and (Rect.W = 0) and (Rect.H = 0));
end;

{ TelShadow }

procedure TelShadow.Clear();
begin
  Self.Blur := 0;
  Self.Color.Clear();
  Self.Position.Clear();
  Self.Visible := false;
end;

{ TelGaugeFloat }

procedure TelGaugeFloat.SetValue(AValue: Single);
begin
  fValue := Clamp(AValue, fMin, fMax);
end;

constructor TelGaugeFloat.Create;
begin
  fMin := 0.0;
  fMax := 0.0;
  fValue := 0.0;
end;

constructor TelGaugeFloat.Create(Min, Max: Single; Value: Single = 0.0);
begin
  fMin := Min;
  fMax := Max;
  fValue := Value;
end;

destructor TelGaugeFloat.Destroy;
begin
  inherited Destroy;
end;

{ TelGaugeInt }

procedure TelGaugeInt.SetValue(AValue: Integer);
begin
  fValue := Clamp(AValue, fMin, fMax);
end;

constructor TelGaugeInt.Create;
begin
  fMin := 0;
  fMax := 0;
  fValue := 0;
end;

constructor TelGaugeInt.Create(Min, Max: Integer; Value: Integer = 0);
begin
  fMin := Min;
  fMax := Max;
  fValue := Value;
end;

destructor TelGaugeInt.Destroy;
begin
  inherited Destroy;
end;

{ TelBorderRadius }

constructor TelBorderRadius.Create;
begin
  fTopLeft := 0;
  fTopRight := 0;
  fBottomLeft := 0;
  fBottomRight := 0;
end;

constructor TelBorderRadius.Create(aTopLeft, aTopRight, aBottomLeft, aBottomRight: Single);
begin
  fTopLeft := aTopLeft;
  fTopRight := aTopRight;
  fBottomLeft := aBottomLeft;
  fBottomRight := aBottomRight;
end;

constructor TelBorderRadius.Create(aRect: TelRect);
begin
  fTopLeft := aRect.X;
  fTopRight := aRect.Y;
  fBottomLeft := aRect.W;
  fBottomRight := aRect.H;
end;

destructor TelBorderRadius.Destroy;
begin
  inherited;
end;

function TelBorderRadius.GetValue: Single;
begin
  Result := ((TopLeft + TopRight + BottomLeft + BottomRight) / 4);
end;

procedure TelBorderRadius.SetValue(const AValue: Single);
begin
  TopLeft := AValue;
  TopRight := AValue;
  BottomLeft := AValue;
  BottomRight := AValue;
end;

function TelBorderRadius.IsEmpty(): Boolean;
begin
  Result := (Value = 0);
end;

function TelBorderRadius.Equals(aBorderRadius: TelBorderRadius): Boolean;
begin
  Result := ((Self.TopLeft = aBorderRadius.TopLeft) or (Self.TopRight = aBorderRadius.TopRight) or (Self.BottomLeft = aBorderRadius.BottomLeft) or (Self.BottomRight = aBorderRadius.BottomRight));
end;

{ TelBorderSide }

constructor TelBorderSide.Create;
begin
  fWidth := 0;
  fStyle := bsSolid;
  Color := makeCol(0, 0, 0);
end;

destructor TelBorderSide.Destroy;
begin
  inherited Destroy;
end;

{ TelBorder }

function TelBorder.GetColor: TelColor;
begin
  if (fLeft.Color.Equals(fTop.Color) and fTop.Color.Equals(fRight.Color) and fRight.Color.Equals(fBottom.Color)) then
  begin
    Result := fLeft.Color;
  end else
  begin
    Result := makeCol(fLeft.Color.R + fTop.Color.R + fRight.Color.R + fBottom.Color.R / 4,
                      fLeft.Color.G + fTop.Color.G + fRight.Color.G + fBottom.Color.G / 4,
                      fLeft.Color.B + fTop.Color.B + fRight.Color.B + fBottom.Color.B / 4,
                      fLeft.Color.A + fTop.Color.A + fRight.Color.A + fBottom.Color.A / 4);
  end;
end;

procedure TelBorder.SetColor(const AValue: TelColor);
begin
  fLeft.Color := AValue;
  fTop.Color := AValue;
  fRight.Color := AValue;
  fBottom.Color := AValue;
end;

function TelBorder.GetWidth: Single;
begin
  Result := (fLeft.Width + fTop.Width + fRight.Width + fBottom.Width) / 4;
end;

procedure TelBorder.SetWidth(AValue: Single);
begin
  fLeft.Width := AValue;
  fTop.Width := AValue;
  fRight.Width := AValue;
  fBottom.Width := AValue;
end;

function TelBorder.GetStyle: TelBorderStyle;
begin
  if ((fLeft.Style = fTop.Style) and (fTop.Style = fRight.Style) and (fRight.Style = fBottom.Style)) then
  begin
    Result := fLeft.Style;
  end else
  begin
    Result := bsSolid;
  end;
end;

procedure TelBorder.SetStyle(AValue: TelBorderStyle);
begin
  fLeft.Style := AValue;
  fTop.Style := AValue;
  fRight.Style := AValue;
  fBottom.Style := AValue;
end;

constructor TelBorder.Create;
begin
  fLeft := TelBorderSide.Create;
  fTop := TelBorderSide.Create;
  fRight := TelBorderSide.Create;
  fBottom := TelBorderSide.Create;

  fRadius := TelBorderRadius.Create;
end;

destructor TelBorder.Destroy;
begin
  fLeft.Destroy;
  fTop.Destroy;
  fRight.Destroy;
  fBottom.Destroy;

  fRadius.Destroy;

  inherited Destroy;
end;

function TelBorder.IsEmpty(): Boolean;
begin
  Result := ((fLeft.Width = 0) or (fTop.Width = 0) or (fRight.Width = 0) or (fBottom.Width = 0));
end;

{ TelExtValue }

function TelExtValue.GetValue: Single;
begin
  Result := ((Left + Top + Right + Bottom) / 4);
end;

procedure TelExtValue.SetValue(const AValue: Single);
begin
  Left := AValue;
  Top := AValue;
  Right := AValue;
  Bottom := AValue;
end;

constructor TelExtValue.Create;
begin
  fLeft := 0;
  fTop := 0;
  fRight := 0;
  fBottom := 0;
end;

destructor TelExtValue.Destroy;
begin
  inherited Destroy;
end;

function TelExtValue.IsEmpty(): Boolean;
begin
  Result := (Value = 0);
end;



function fromVector2i( aName : String; aValue : TelVector2i ): TKeyIdent; //TelVector2f.ToKey(KeyName: String): PKeyIdent;
var
  tmpKey: TKeyIdent;
begin
  tmpKey.Name := aName;
  tmpKey.Value := aValue.ToString();

  Result := tmpKey;
end;


function fromVector2f( aName : String; aValue : TelVector2f ): TKeyIdent; //TelVector2f.ToKey(KeyName: String): PKeyIdent;
var
  tmpKey: TKeyIdent;
begin
  tmpKey.Name := aName;
  tmpKey.Value := aValue.ToString();

  Result := tmpKey;
end;


function fromVector3i( aName : String; aValue : TelVector3i ): TKeyIdent; //TelVector2f.ToKey(KeyName: String): PKeyIdent;
var
  tmpKey: TKeyIdent;
begin
  tmpKey.Name := aName;
  tmpKey.Value := aValue.ToString();

  Result := tmpKey;
end;


function fromVector3f( aName : String; aValue : TelVector3f ): TKeyIdent; //TelVector2f.ToKey(KeyName: String): PKeyIdent;
var
  tmpKey: TKeyIdent;
begin
  tmpKey.Name := aName;
  tmpKey.Value := aValue.ToString();

  Result := tmpKey;
end;

{$IFDEF CAN_METHODS}

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
