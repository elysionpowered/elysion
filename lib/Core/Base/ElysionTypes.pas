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
{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  ElysionEnums,

  ElysionHash,
  ElysionMath,
  ElysionColor,

  Classes,
  SysUtils;

type

  { TelVector2 }

  TelVector2<T> = record
  private
    fX, fY: T;

    //{$IFNDEF FPC_GEN_FIX}
    fHash: TelHash;
    //{$ENDIF}
  public
    procedure Make(aX, aY: T);
  public
    class function Copy(aSource: TelVector2{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF}): TelVector2{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF}; static; inline;
  public
    class operator Add(A, B: TelVector2{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF}): TelVector2{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF}; inline;
    class operator Subtract(A, B: TelVector2{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF}): TelVector2{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF}; inline;

    class operator Equal(A, B: TelVector2{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF}): Boolean; inline;
    class operator NotEqual(A, B: TelVector2{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF}): Boolean; inline;
  public
    property X: T read fX write fX;
    property Y: T read fY write fY;
  end;

  TelVector2f = TelVector2<Single>;
  TelVector2i = TelVector2<Integer>;

  TelVector2fArray = array of TelVector2f;
  TelVector2iArray = array of TelVector2i;


  { TelVector3 }

  TelVector3<T> = record
  private
    fX, fY, fZ: T;

    //{$IFNDEF FPC_GEN_FIX}
    fHash: TelHash;
    //{$ENDIF}
  public
    procedure Make(aX, aY, aZ: T);
  public
    class function Copy(aSource: TelVector3{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF}): TelVector3{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF}; static; inline;
  public
    class operator Add(A, B: TelVector3{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF}): TelVector3{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF}; inline;
    class operator Subtract(A, B: TelVector3{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF}): TelVector3{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF}; inline;

    class operator Equal(A, B: TelVector3{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF}): Boolean; inline;
    class operator NotEqual(A, B: TelVector3{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF}): Boolean; inline;
  public
    property X: T read fX write fX;
    property Y: T read fY write fY;
    property Z: T read fZ write fZ;
  end;

  TelVector3f = TelVector3<Single>;
  TelVector3i = TelVector3<Integer>;

  TelVector3fArray = array of TelVector3f;
  TelVector3iArray = array of TelVector3i;


  // A vertex is basically just a position vector with a color attached to it
  TelVertex = record
    Vector: TelVector3f;
    Color: TelColor;
  end;

  TelVertexArray = array of TelVertex;


  { TelSize }

  TelSize = record
  private
    fWidth, fHeight: Single;
    fHash: TelHash;

    function GetAspectRatio: Single; inline;
    function GetOrientation: TRectOrientation; inline;
    function GetHash(): TelHash; inline;

    function IsWide: Boolean; inline;
    function IsEmpty: Boolean; inline;
  public
    procedure Clear();

    procedure Make(aWidth, aHeight: Integer); Overload;
    procedure Make(aWidth, aHeight: Single); Overload;

    function ToString(): AnsiString; inline;

    function ToVector2i: TelVector2i; inline;
    function ToVector2f: TelVector2f; inline;
    function ToVector3i: TelVector3i; inline;
    function ToVector3f: TelVector3f; inline;

    function Center: TelVector2f; inline;
  public
    class function Create: TelSize; static; inline; Overload;
    class function Create(aWidth, aHeight: Single): TelSize; static; inline; Overload;

    class function Copy(aSource: TelSize): TelSize; static; inline;

    class function Lerp(A, B: TelSize; Amt: Single = 0.5): TelSize; static; inline;
  public
    class operator Add(A, B: TelSize): TelSize; inline;
    class operator Subtract(A, B: TelSize): TelSize; inline;
    class operator Multiply(A, B: TelSize): TelSize; inline; Overload;
    class operator Multiply(A: TelSize; Factor: Single): TelSize; inline; Overload;
    class operator Divide(A, B: TelSize): TelSize; inline;

    class operator IntDivide(A, B: TelSize): TelSize; inline;
    class operator Modulus(A, B: TelSize): TelSize; inline;

    class operator Equal(A, B: TelSize): Boolean; inline;
    class operator NotEqual(A, B: TelSize): Boolean; inline;
    class operator GreaterThan(A, B: TelSize): Boolean; inline;
    class operator GreaterThanOrEqual(A, B: TelSize): Boolean; inline;
    class operator LessThan(A, B: TelSize): Boolean; inline;
    class operator LessThanOrEqual(A, B: TelSize): Boolean; inline;
  public
    property Width: Single read fWidth write fWidth;
    property Height: Single read fHeight write fHeight;

    property AspectRatio: Single read GetAspectRatio;
    property Wide: Boolean read IsWide;
    property Empty: Boolean read IsEmpty;

    property Hash: TelHash read GetHash;

    property Orientation: TRectOrientation read GetOrientation;
  end;

  TelSizeArray = array of TelSize;


  { TelRect }

  TelRect = record
  private
    // Position
    fX, fY, fZ: Single;
    // Width & Height
    fW, FH: Single;

    fHash: TelHash;


    function GetAspectRatio(): Single; inline;
    function GetOrientation: TRectOrientation; inline;

    function IsWide(): Boolean; inline;

    function IsEmpty(): Boolean; inline;

    function GetHash(): TelHash; inline;
  public
    procedure Clear(); inline;

    procedure Make(aX, aY, aW, aH: Single); inline; Overload;
    procedure Make(aX, aY, aZ, aW, aH: Single); inline; Overload;

    procedure Make(aX, aY, aW, aH: Integer); inline; Overload;
    procedure Make(aX, aY, aZ, aW, aH: Integer); inline; Overload;

    procedure Make(aPosition: TelVector2f; aSize: TelSize); inline; Overload;
    procedure Make(aPosition: TelVector2i; aSize: TelSize); inline; Overload;
    procedure Make(aPosition: TelVector3f; aSize: TelSize); inline; Overload;
    procedure Make(aPosition: TelVector3i; aSize: TelSize); inline; Overload;

    function ToString(): AnsiString; inline;
    function ToSize(): TelSize; inline;

    function Center(): TelVector2f; inline;
  public
    class function Create: TelRect; static; inline; Overload;

    class function Create(aX, aY, aW, aH: Single): TelRect; static; inline; Overload;
    class function Create(aX, aY, aW, aH: Integer): TelRect; static; inline; Overload;

    class function Create(aX, aY, aZ, aW, aH: Single): TelRect; static; inline; Overload;
    class function Create(aX, aY, aZ, aW, aH: Integer): TelRect; static; inline; Overload;

    class function Create(aPosition: TelVector2f; aSize: TelSize): TelRect; static; inline; Overload;
    class function Create(aPosition: TelVector2i; aSize: TelSize): TelRect; static; inline; Overload;
    class function Create(aPosition: TelVector3f; aSize: TelSize): TelRect; static; inline; Overload;
    class function Create(aPosition: TelVector3i; aSize: TelSize): TelRect; static; inline; Overload;

    class function Copy(aSource: TelRect): TelRect; static; inline;

    class function Lerp(A, B: TelRect; Amt: Single = 0.5): TelRect; static; inline;
  public
    class operator Add(A, B: TelRect): TelRect; inline;
    class operator Subtract(A, B: TelRect): TelRect; inline;
    class operator Multiply(A, B: TelRect): TelRect; inline;
    class operator Divide(A, B: TelRect): TelRect; inline;

    class operator Equal(A, B: TelRect): Boolean; inline;
    class operator NotEqual(A, B: TelRect): Boolean; inline;

    class operator In(aVector: TelVector2i; aRect: TelRect): Boolean; inline; Overload;
    class operator In(aVector: TelVector2f; aRect: TelRect): Boolean; inline; Overload;
    class operator In(aVector: TelVector3i; aRect: TelRect): Boolean; inline; Overload;
    class operator In(aVector: TelVector3f; aRect: TelRect): Boolean; inline; Overload;
    class operator In(aSize: TelSize; aRect: TelRect): Boolean; inline; Overload;
    class operator In(A, B: TelRect): Boolean; inline; Overload;
  public
    property X: Single read fX write fX;
    property Y: Single read fY write fY;
    property Z: Single read fZ write fZ;

    property W: Single read fW write fW;
    property H: Single read fH write fH;

    property AspectRatio: Single read GetAspectRatio;
    property Wide: Boolean read IsWide;
    property Empty: Boolean read IsEmpty;

    property Hash: TelHash read GetHash;

    property Orientation: TRectOrientation read GetOrientation;
  end;

  TelRectArray = array of TelRect;


  { TelVector2iHelper }

  TelVector2iHelper = record helper for TelVector2i
  private
    function GetLength: Single; inline;

    function GetHash(): TelHash; inline;
  public
    procedure Normalize; inline; Overload;

    procedure Clear; inline;
  public
    function ToString: AnsiString; inline;

    function ToVector2f: TelVector2f; inline;
    function ToVector3i: TelVector3i; inline;
    function ToVector3f: TelVector3f; inline;

    function ToSize: TelSize; inline;
  public
    class function Create: TelVector2i; static; inline; Overload;
    class function Create(aX, aY: Integer): TelVector2i; static; inline; Overload;
  public
    class function Normalize(aVector: TelVector2i): TelVector2i; static; inline; Overload;

    class function DotProduct(A, B: TelVector2i): Single; static; inline;

    class function Lerp(A, B: TelVector2i; Amt: Single = 0.5): TelVector2i; static; inline;
  public
    property Length: Single read GetLength;

    property Hash: TelHash read GetHash;
  end;

  { TelVector2fHelper }

  TelVector2fHelper = record helper for TelVector2f
  private
    function GetLength: Single; inline;

    function GetHash(): TelHash; inline;
  public
    procedure Normalize; inline; Overload;

    procedure Clear; inline;
  public
    function ToString: AnsiString; inline;

    function ToVector2i: TelVector2i; inline;
    function ToVector3i: TelVector3i; inline;
    function ToVector3f: TelVector3f; inline;

    function ToSize: TelSize; inline;
  public
    class function Create: TelVector2f; static; inline; Overload;
    class function Create(aX, aY: Single): TelVector2f; static; inline; Overload;
  public
    class function Normalize(aVector: TelVector2f): TelVector2f; static; inline; Overload;

    class function DotProduct(A, B: TelVector2f): Single; static; inline;

    class function Lerp(A, B: TelVector2f; Amt: Single = 0.5): TelVector2f; static; inline;
  public
    property Length: Single read GetLength;

    property Hash: TelHash read GetHash;
  end;

  { TelVector3iHelper }

  TelVector3iHelper = record helper for TelVector3i
  private
    function GetLength: Single; inline;

    function GetHash(): TelHash; inline;
  public
    procedure Clear; inline;

    procedure Normalize; inline; Overload;
  public
    function ToString: AnsiString; inline;

    function ToVector2i: TelVector2i; inline;
    function ToVector2f: TelVector2f; inline;
    function ToVector3f: TelVector3f; inline;

    function ToSize: TelSize; inline;
  public
    class function Create: TelVector3i; static; inline; Overload;
    class function Create(aX, aY: Integer; aZ: Integer = 0): TelVector3i; static; inline; Overload;
  public
    class function Normalize(aVector: TelVector3i): TelVector3i; static; inline; Overload;

    class function DotProduct(A, B: TelVector3i): Single; static; inline;
    class function CrossProduct(A, B: TelVector3i): TelVector3i; static; inline;

    class function Lerp(A, B: TelVector3i; Amt: Single = 0.5): TelVector3i; static; inline;

    class function Zero(): TelVector3i; static; inline;
    class function One(): TelVector3i; static; inline;
    class function Forward(): TelVector3i; static; inline;
    class function Up(): TelVector3i; static; inline;
    class function Right(): TelVector3i; static; inline;
  public
    property Length: Single read GetLength;

    property Hash: TelHash read GetHash;
  end;

  { TelVector3fHelper }

  TelVector3fHelper = record helper for TelVector3f
  private
    function GetLength: Single; inline;

    function GetHash(): TelHash; inline;
  public
    procedure Clear; inline;

    procedure Normalize; inline; Overload;
  public
    function ToString: AnsiString; inline;

    function ToVector2i: TelVector2i; inline;
    function ToVector2f: TelVector2f; inline;
    function ToVector3i: TelVector3i; inline;

    function ToSize: TelSize; inline;
  public
    class function Create: TelVector3f; static; inline; Overload;
    class function Create(aX, aY: Single; aZ: Single = 0.0): TelVector3f; static; inline; Overload;
  public
    class function Normalize(aVector: TelVector3f): TelVector3f; static; inline; Overload;

    class function DotProduct(A, B: TelVector3f): Single; static; inline;
    class function CrossProduct(A, B: TelVector3f): TelVector3f; static; inline;

    class function Lerp(A, B: TelVector3f; Amt: Single = 0.5): TelVector3f; static; inline;

    class function Zero(): TelVector3f; static; inline;
    class function One(): TelVector3f; static; inline;
    class function Forward(): TelVector3f; static; inline;
    class function Up(): TelVector3f; static; inline;
    class function Right(): TelVector3f; static; inline;
  public
    property Length: Single read GetLength;

    property Hash: TelHash read GetHash;
  end;




  TelImageOffset = record
    Position: TelVector2f;
    Rotation: TelVector2f;
  end;

  TelRotation = record
    Angle: Single;
    Vector: TelVector3f;
  end;


  PelButtonEvent = ^TelButtonEvent;
  TelButtonEvent = record
    Position: TelVector2i;
    Called: Cardinal;
  end;

  TelRenderOptions = record
    Translation: TelVector3f;
    Rotation: TelRotation;
    Origin: TelVector2f;
    Scale: TelVector2f;
  end;
  PelRenderOptions = ^TelRenderOptions;




  {%region 'General functions'}
  function makeV2f(aX, aY: Single): TelVector2f; inline; deprecated 'Use TelVector2f.Create(...) instead';
  function makeV2i(aX, aY: Integer): TelVector2i; inline; deprecated 'Use TelVector2i.Create(...) instead';
  function makeV3f(aX, aY: Single; aZ: Single = 0.0): TelVector3f; inline; deprecated 'Use TelVector3f.Create(...) instead';
  function makeV3i(aX, aY: Integer; aZ: Integer = 0): TelVector3i; inline; deprecated 'Use TelVector3f.Create(...) instead';

  function makeSize(aWidth, aHeight: Integer): TelSize; Overload; inline; deprecated 'Use TelSize.Create(...) instead';
  function makeSize(aWidth, aHeight: Single): TelSize; Overload; inline; deprecated 'Use TelSize.Create(...) instead';

  function makeRect(aX, aY, aW, aH: Single): TelRect; inline; deprecated 'Use TelRect.Create(...) instead';
  {%endregion}




implementation


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
  tmpVec.Width := aWidth * 1.0;
  tmpVec.Height := aHeight * 1.0;

  Result := tmpVec;
end;

function makeSize(aWidth, aHeight: Single): TelSize;
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




{ TelVector2<T> }

procedure TelVector2<T>.Make(aX, aY: T);
begin
  fX := aX;
  fY := aY;
end;

class function TelVector2<T>.Copy(aSource: TelVector2{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF}): TelVector2{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF};
begin
  Result.X := aSource.X;
  Result.Y := aSource.Y;
end;

class operator TelVector2<T>.Add(A, B: TelVector2{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF}): TelVector2{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF};
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

class operator TelVector2<T>.Subtract(A, B: TelVector2{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF}): TelVector2{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF};
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

class operator TelVector2<T>.Equal(A, B: TelVector2{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF}): Boolean;
begin
  Result := ((A.X = B.X) and (A.Y = B.Y));
end;

class operator TelVector2<T>.NotEqual(A, B: TelVector2{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF}): Boolean;
begin
  Result := not ((A.X = B.X) and (A.Y = B.Y));
end;

{ TelVector3<T> }

procedure TelVector3<T>.Make(aX, aY, aZ: T);
begin
  fX := aX;
  fY := aY;
  fZ := aZ;
end;

class function TelVector3<T>.Copy(aSource: TelVector3{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF}): TelVector3{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF};
begin
  Result.X := aSource.X;
  Result.Y := aSource.Y;
  Result.Z := aSource.Z;
end;

class operator TelVector3<T>.Add(A, B: TelVector3{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF}): TelVector3{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF};
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
end;

class operator TelVector3<T>.Subtract(A, B: TelVector3{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF}): TelVector3{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF};
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
end;

class operator TelVector3<T>.Equal(A, B: TelVector3{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF}): Boolean;
begin
  Result := ((A.X = B.X) and (A.Y = B.Y) and (A.Z = B.Z));
end;

class operator TelVector3<T>.NotEqual(A, B: TelVector3{$IFNDEF FPC_GEN_FIX}<T>{$ENDIF}): Boolean;
begin
  Result := not ((A.X = B.X) and (A.Y = B.Y) and (A.Z = B.Z));
end;




procedure TelSize.Clear();
begin
  Width := 0;
  Height := 0;
end;

procedure TelSize.Make(aWidth, aHeight: Integer);
begin
  Width := Trunc(aWidth);
  Height := Trunc(aHeight);
end;

procedure TelSize.Make(aWidth, aHeight: Single);
begin
  Width := aWidth;
  Height := aHeight;
end;

function TelSize.ToString(): AnsiString;
begin
  Result := Format('size(%f, %f)', [Width, Height])
end;

function TelSize.ToVector2f(): TelVector2f;
begin
  Result := TelVector2f.Create(Width, Height);
end;

function TelSize.ToVector2i(): TelVector2i;
begin
  Result := TelVector2i.Create(Trunc(Width), Trunc(Height));
end;

function TelSize.ToVector3f(): TelVector3f;
begin
  Result := TelVector3f.Create(Width, Height);
end;

function TelSize.ToVector3i(): TelVector3i;
begin
  Result := TelVector3i.Create(Trunc(Width), Trunc(Height));
end;

function TelSize.Center(): TelVector2f;
begin
  Result := TelVector2f.Create(Self.Width / 2, Self.Height / 2);
end;

class function TelSize.Create: TelSize;
begin
  Result.Width := 0;
  Result.Height := 0;
end;

class function TelSize.Create(aWidth, aHeight: Single): TelSize;
begin
  Result.Width := aWidth;
  Result.Height := aHeight;
end;

class function TelSize.Copy(aSource: TelSize): TelSize;
begin
  Result.Width := aSource.Width;
  Result.Height := aSource.Height;
end;

class function TelSize.Lerp(A, B: TelSize; Amt: Single): TelSize;
begin
  Result := TelSize.Create(ElysionMath.Lerp(A.Width, B.Width, Amt),
                           ElysionMath.Lerp(A.Height, B.Height, Amt));
end;

class operator TelSize.Add(A, B: TelSize): TelSize;
begin
  Result.Width := A.Width + B.Width;
  Result.Height := A.Height + B.Height;
end;

class operator TelSize.Subtract(A, B: TelSize): TelSize;
begin
  Result.Width := A.Height - B.Width;
  Result.Height := A.Height - B.Height;
end;

class operator TelSize.Multiply(A, B: TelSize): TelSize;
begin
  Result.Width := A.Width * B.Width;
  Result.Height := A.Height * B.Height;
end;

class operator TelSize.Multiply(A: TelSize; Factor: Single): TelSize;
begin
  Result.Width := A.Width * Factor;
  Result.Height := A.Height * Factor;
end;

class operator TelSize.Divide(A, B: TelSize): TelSize;
begin
  Result.Width := A.Width / B.Width;
  Result.Height := A.Height / B.Height;
end;

class operator TelSize.IntDivide(A, B: TelSize): TelSize;
begin
  Result.Width := Trunc(A.Width) div Trunc(B.Width);
  Result.Height := Trunc(A.Height) div Trunc(B.Height);
end;

class operator TelSize.Modulus(A, B: TelSize): TelSize;
begin
  Result.Width := Trunc(A.Width) mod Trunc(B.Width);
  Result.Height := Trunc(A.Height) mod Trunc(B.Height);
end;

class operator TelSize.Equal(A, B: TelSize): Boolean;
begin
  Result := ((A.Width = B.Width) and (A.Height = B.Height));
end;

class operator TelSize.NotEqual(A, B: TelSize): Boolean;
begin
  Result := not ((A.Width = B.Width) and (A.Height = B.Height));
end;

class operator TelSize.GreaterThan(A, B: TelSize): Boolean;
begin
  Result := ((A.Width > B.Width) and (A.Height > B.Height));
end;

class operator TelSize.GreaterThanOrEqual(A, B: TelSize): Boolean;
begin
  Result := ((A.Width >= B.Width) and (A.Height >= B.Height));
end;

class operator TelSize.LessThan(A, B: TelSize): Boolean;
begin
  Result := ((A.Width < B.Width) and (A.Height < B.Height));
end;

class operator TelSize.LessThanOrEqual(A, B: TelSize): Boolean;
begin
  Result := ((A.Width <= B.Width) and (A.Height <= B.Height));
end;

function TelSize.GetAspectRatio(): Single;
begin
  Result := Width / Height;
end;

function TelSize.GetOrientation: TRectOrientation;
begin
  if Width > Height then Result := roLandscape
    else Result := roPortrait;
end;

function TelSize.GetHash: TelHash;
begin
  fHash.Generate(Self.ToString());

  Result := fHash;
end;

function TelSize.IsWide(): Boolean;
begin
  Result := (GetAspectRatio > (4 / 3));
end;

function TelSize.IsEmpty: Boolean;
begin
  Result := ((Self.Width = 0) and (Self.Height = 0));
end;





procedure TelRect.Clear();
begin
  X := 0;
  Y := 0;
  Z := 0;

  W := 0;
  H := 0;
end;

procedure TelRect.Make(aX, aY, aW, aH: Integer);
begin
  X := aX * 1.0;
  Y := aY * 1.0;
  Z := 0.0;

  W := aW * 1.0;
  H := aH * 1.0;
end;

procedure TelRect.Make(aX, aY, aZ, aW, aH: Integer);
begin
  X := aX * 1.0;
  Y := aY * 1.0;
  Z := aZ * 1.0;

  W := aW * 1.0;
  H := aH * 1.0;
end;

procedure TelRect.Make(aX, aY, aW, aH: Single);
begin
  X := aX;
  Y := aY;
  Z := 0.0;

  W := aW;
  H := aH;
end;

procedure TelRect.Make(aX, aY, aZ, aW, aH: Single);
begin
  X := aX;
  Y := aY;
  Z := aZ;

  W := aW;
  H := aH;
end;

procedure TelRect.Make(aPosition: TelVector2f; aSize: TelSize);
begin
  X := aPosition.X;
  Y := aPosition.Y;
  Z := 0.0;

  W := aSize.Width;
  H := aSize.Height;
end;

procedure TelRect.Make(aPosition: TelVector2i; aSize: TelSize);
begin
  X := aPosition.X * 1.0;
  Y := aPosition.Y * 1.0;
  Z := 0.0;

  W := aSize.Width;
  H := aSize.Height;
end;

procedure TelRect.Make(aPosition: TelVector3f; aSize: TelSize);
begin
  X := aPosition.X;
  Y := aPosition.Y;
  Z := aPosition.Z;

  W := aSize.Width;
  H := aSize.Height;
end;

procedure TelRect.Make(aPosition: TelVector3i; aSize: TelSize);
begin
  X := aPosition.X * 1.0;
  Y := aPosition.Y * 1.0;
  Z := aPosition.Z * 1.0;

  W := aSize.Width;
  H := aSize.Height;
end;

function TelRect.ToString(): AnsiString;
begin
  Result := Format('rect(%f, %f, %f, %f, %f)', [X, Y, Z, W, H]);
end;

function TelRect.ToSize(): TelSize;
begin
  Result := TelSize.Create(W, H);
end;

function TelRect.Center(): TelVector2f;
begin
  Result := TelVector2f.Create(Self.X + (Self.W / 2), Self.Y + (Self.H / 2));
end;

class function TelRect.Create: TelRect;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Z := 0;

  Result.W := 0;
  Result.H := 0;
end;

class function TelRect.Create(aX, aY, aW, aH: Single): TelRect;
begin
  Result.X := aX;
  Result.Y := aY;
  Result.Z := 0.0;

  Result.W := aW;
  Result.H := aH;
end;

class function TelRect.Create(aX, aY, aW, aH: Integer): TelRect;
begin
  Result.X := aX * 1.0;
  Result.Y := aY * 1.0;
  Result.Z := 0.0;

  Result.W := aW * 1.0;
  Result.H := aH * 1.0;
end;

class function TelRect.Create(aX, aY, aZ, aW, aH: Single): TelRect;
begin
  Result.X := aX;
  Result.Y := aY;
  Result.Z := aZ;

  Result.W := aW;
  Result.H := aH;
end;

class function TelRect.Create(aX, aY, aZ, aW, aH: Integer): TelRect;
begin
  Result.X := aX * 1.0;
  Result.Y := aY * 1.0;
  Result.Z := aZ * 1.0;

  Result.W := aW * 1.0;
  Result.H := aH * 1.0;
end;

class function TelRect.Create(aPosition: TelVector2f; aSize: TelSize): TelRect;
begin
  Result.X := aPosition.X;
  Result.Y := aPosition.Y;
  Result.Z := 0.0;

  Result.W := aSize.Width;
  Result.H := aSize.Height;
end;

class function TelRect.Create(aPosition: TelVector2i; aSize: TelSize): TelRect;
begin
  Result.X := aPosition.X * 1.0;
  Result.Y := aPosition.Y * 1.0;
  Result.Z := 0.0;

  Result.W := aSize.Width;
  Result.H := aSize.Height;
end;

class function TelRect.Create(aPosition: TelVector3f; aSize: TelSize): TelRect;
begin
  Result.X := aPosition.X;
  Result.Y := aPosition.Y;
  Result.Z := aPosition.Z;

  Result.W := aSize.Width;
  Result.H := aSize.Height;
end;

class function TelRect.Create(aPosition: TelVector3i; aSize: TelSize): TelRect;
begin
  Result.X := aPosition.X * 1.0;
  Result.Y := aPosition.Y * 1.0;
  Result.Z := aPosition.Z * 1.0;

  Result.W := aSize.Width;
  Result.H := aSize.Height;
end;

class function TelRect.Copy(aSource: TelRect): TelRect;
begin
  Result.X := aSource.X;
  Result.Y := aSource.Y;
  Result.Z := aSource.Z;

  Result.W := aSource.W;
  Result.H := aSource.H;
end;

class function TelRect.Lerp(A, B: TelRect; Amt: Single): TelRect;
begin
  Result := TelRect.Create(ElysionMath.Lerp(A.X, B.X, Amt),
                           ElysionMath.Lerp(A.Y, B.Y, Amt),
                           ElysionMath.Lerp(A.Z, B.Z, Amt),
                           ElysionMath.Lerp(A.W, B.W, Amt),
                           ElysionMath.Lerp(A.H, B.H, Amt));
end;

class operator TelRect.Add(A, B: TelRect): TelRect;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;

  Result.W := A.W + B.W;
  Result.H := A.H + B.H;
end;

class operator TelRect.Subtract(A, B: TelRect): TelRect;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;

  Result.W := A.W - B.W;
  Result.H := A.H - B.H;
end;

class operator TelRect.Multiply(A, B: TelRect): TelRect;
begin
  Result.X := A.X * B.X;
  Result.Y := A.Y * B.Y;
  Result.Z := A.Z * B.Z;

  Result.W := A.W * B.W;
  Result.H := A.H * B.H;
end;

class operator TelRect.Divide(A, B: TelRect): TelRect;
begin
  Result.X := A.X / B.X;
  Result.Y := A.Y / B.Y;
  Result.Z := A.Z / B.Z;

  if (not B.Empty)  then
  begin
    Result.W := A.W / B.W;
    Result.H := A.H / B.H;
  end;
end;

class operator TelRect.Equal(A, B: TelRect): Boolean;
begin
  Result := ((A.X = B.X) and (A.Y = B.Y) and (A.W = B.W) and (A.H = B.H));
end;

class operator TelRect.NotEqual(A, B: TelRect): Boolean;
begin
  Result := not ((A.X = B.X) and (A.Y = B.Y) and (A.W = B.W) and (A.H = B.H));
end;

class operator TelRect.In(aVector: TelVector2i; aRect: TelRect): Boolean;
begin
  Result := ((aVector.X >= aRect.X) and (aVector.Y >= aRect.Y) and (aVector.X <= (aRect.X + aRect.W)) and (aVector.Y <= (aRect.Y + aRect.H)));
end;

class operator TelRect.In(aVector: TelVector2f; aRect: TelRect): Boolean;
begin
  Result := ((aVector.X >= aRect.X) and (aVector.Y >= aRect.Y) and (aVector.X <= (aRect.X + aRect.W)) and (aVector.Y <= (aRect.Y + aRect.H)));
end;

class operator TelRect.In(aVector: TelVector3i; aRect: TelRect): Boolean;
begin
  Result := ((aVector.X >= aRect.X) and (aVector.Y >= aRect.Y) and (aVector.Z = aRect.Z) and (aVector.X <= (aRect.X + aRect.W)) and (aVector.Y <= (aRect.Y + aRect.H)));
end;

class operator TelRect.In(aVector: TelVector3f; aRect: TelRect): Boolean;
begin
  Result := ((aVector.X >= aRect.X) and (aVector.Y >= aRect.Y) and (aVector.Z = aRect.Z) and (aVector.X <= (aRect.X + aRect.W)) and (aVector.Y <= (aRect.Y + aRect.H)));
end;

class operator TelRect.In(aSize: TelSize; aRect: TelRect): Boolean;
begin
  Result := ((aRect.W = aSize.Width) and (aRect.H = aSize.Height));
end;

class operator TelRect.In(A, B: TelRect): Boolean;
begin
  Result := ((A.X >= B.X) and (A.Y >= B.Y) and (A.X <= (B.X + B.W)) and (A.Y <= (B.Y + B.H)));
end;

function TelRect.GetAspectRatio(): Single;
begin
  Result := W / H;
end;

function TelRect.IsWide(): Boolean;
begin
  Result := (GetAspectRatio > (4 / 3));
end;

function TelRect.GetOrientation: TRectOrientation;
begin
  if W > H then Result := roLandscape
    else Result := roPortrait;
end;

function TelRect.IsEmpty(): Boolean;
begin
  Result := ((X = 0) and (Y = 0) and (W = 0) and (H = 0));
end;

function TelRect.GetHash: TelHash;
begin
  fHash.Generate(Self.ToString());

  Result := fHash;
end;

{ TelVector2iHelper }

procedure TelVector2iHelper.Clear;
begin
  X := 0;
  Y := 0;
end;

class function TelVector2iHelper.Create: TelVector2i;
begin
  Result.X := 0;
  Result.Y := 0;
end;

class function TelVector2iHelper.Create(aX, aY: Integer): TelVector2i;
begin
  Result.X := aX;
  Result.Y := aY;
end;

class function TelVector2iHelper.DotProduct(A, B: TelVector2i): Single;
begin
  Result := (A.X * B.X) + (A.Y * B.Y);
end;

class function TelVector2iHelper.Lerp(A, B: TelVector2i; Amt: Single): TelVector2i;
begin
  Result := TelVector2i.Create(Trunc(ElysionMath.Lerp(A.X, B.X, Amt)),
                               Trunc(ElysionMath.Lerp(A.Y, B.Y, Amt)));
end;

function TelVector2iHelper.GetLength: Single;
begin
  Result := sqrt(X * X + Y * Y);
end;

function TelVector2iHelper.GetHash: TelHash;
begin
  fHash.Generate(Self.ToString());

  Result := fHash;
end;

procedure TelVector2iHelper.Normalize;
begin
  Self.X := Trunc(Self.X / Length);
  Self.Y := Trunc(Self.Y / Length);
end;

class function TelVector2iHelper.Normalize(aVector: TelVector2i): TelVector2i;
begin
  Result.X := Trunc(aVector.X / aVector.Length);
  Result.Y := Trunc(aVector.Y / aVector.Length);
end;

function TelVector2iHelper.ToSize: TelSize;
begin
  Result := TelSize.Create(X * 1.0, Y * 1.0);
end;

function TelVector2iHelper.ToString: AnsiString;
begin
  Result := Format('vec2i(%d, %d)', [X, Y]);
end;

function TelVector2iHelper.ToVector2f: TelVector2f;
begin
  Result := TelVector2f.Create(X * 1.0, Y * 1.0);
end;

function TelVector2iHelper.ToVector3f: TelVector3f;
begin
  Result := TelVector3f.Create(X * 1.0, Y * 1.0);
end;

function TelVector2iHelper.ToVector3i: TelVector3i;
begin
  Result := TelVector3i.Create(X, Y);
end;

{ TelVector2fHelper }

procedure TelVector2fHelper.Clear;
begin
  X := 0.0;
  Y := 0.0;
end;

class function TelVector2fHelper.Create: TelVector2f;
begin
  Result.X := 0.0;
  Result.Y := 0.0;
end;

class function TelVector2fHelper.Create(aX, aY: Single): TelVector2f;
begin
  Result.X := aX;
  Result.Y := aY;
end;

class function TelVector2fHelper.DotProduct(A, B: TelVector2f): Single;
begin
  Result := (A.X * B.X) + (A.Y * B.Y);
end;

class function TelVector2fHelper.Lerp(A, B: TelVector2f; Amt: Single): TelVector2f;
begin
  Result := TelVector2f.Create(ElysionMath.Lerp(A.X, B.X, Amt),
                               ElysionMath.Lerp(A.Y, B.Y, Amt));
end;

function TelVector2fHelper.GetLength: Single;
begin
  Result := sqrt(X * X + Y * Y);
end;

function TelVector2fHelper.GetHash: TelHash;
begin
  fHash.Generate(Self.ToString());

  Result := fHash;
end;

procedure TelVector2fHelper.Normalize;
begin
  Self.X := Self.X / Length;
  Self.Y := Self.Y / Length;
end;

class function TelVector2fHelper.Normalize(aVector: TelVector2f): TelVector2f;
begin
  Result.X := aVector.X / aVector.Length;
  Result.Y := aVector.Y / aVector.Length;
end;

function TelVector2fHelper.ToSize: TelSize;
begin
  Result := TelSize.Create(X, Y);
end;

function TelVector2fHelper.ToString: AnsiString;
begin
  Result := Format('vec2f(%f, %f)', [X, Y]);
end;

function TelVector2fHelper.ToVector2i: TelVector2i;
begin
  Result := TelVector2i.Create(Trunc(X), Trunc(Y));
end;

function TelVector2fHelper.ToVector3f: TelVector3f;
begin
  Result := TelVector3f.Create(X, Y);
end;

function TelVector2fHelper.ToVector3i: TelVector3i;
begin
  Result := TelVector3i.Create(Trunc(X), Trunc(Y));
end;

{ TelVector3iHelper }

procedure TelVector3iHelper.Clear;
begin
  X := 0;
  Y := 0;
  Z := 0;
end;

class function TelVector3iHelper.Create: TelVector3i;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Z := 0;
end;

class function TelVector3iHelper.Create(aX, aY: Integer; aZ: Integer = 0): TelVector3i;
begin
  Result.X := aX;
  Result.Y := aY;
  Result.Z := aZ;
end;

class function TelVector3iHelper.CrossProduct(A, B: TelVector3i): TelVector3i;
begin
  Result := TelVector3i.Create(A.Y * B.Z - B.Y * A.Z, A.Z * B.X - B.Z * A.X, A.X * B.Y - B.X * A.Y);
end;

class function TelVector3iHelper.Lerp(A, B: TelVector3i; Amt: Single
  ): TelVector3i;
begin
  Result := TelVector3i.Create(Trunc(ElysionMath.Lerp(A.X, B.X, Amt)),
                               Trunc(ElysionMath.Lerp(A.Y, B.Y, Amt)),
                               Trunc(ElysionMath.Lerp(A.Z, B.Z, Amt)));
end;

class function TelVector3iHelper.DotProduct(A, B: TelVector3i): Single;
begin
  Result := (A.X * B.X) + (A.Y * B.Y) + (A.Z * B.Z);
end;

class function TelVector3iHelper.Forward: TelVector3i;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Z := 1;
end;

function TelVector3iHelper.GetLength: Single;
begin
  Result := sqrt(X * X + Y * Y + Z * Z);
end;

function TelVector3iHelper.GetHash: TelHash;
begin
  fHash.Generate(Self.ToString());

  Result := fHash;
end;

class function TelVector3iHelper.Normalize(aVector: TelVector3i): TelVector3i;
begin
  Result.X := Trunc(aVector.X / aVector.Length);
  Result.Y := Trunc(aVector.Y / aVector.Length);
  Result.Z := Trunc(aVector.Z / aVector.Length);
end;

procedure TelVector3iHelper.Normalize;
begin
  Self.X := Trunc(Self.X / GetLength);
  Self.Y := Trunc(Self.Y / GetLength);
  Self.Z := Trunc(Self.Z / GetLength);
end;

class function TelVector3iHelper.One: TelVector3i;
begin
  Result.X := 1;
  Result.Y := 1;
  Result.Z := 1;
end;

class function TelVector3iHelper.Right: TelVector3i;
begin
  Result.X := 1;
  Result.Y := 0;
  Result.Z := 0;
end;

function TelVector3iHelper.ToSize: TelSize;
begin
  Result := TelSize.Create(X * 1.0, Y * 1.0);
end;

function TelVector3iHelper.ToString: AnsiString;
begin
  Result := Format('vec3i(%d, %d, %d)', [X, Y, Z]);
end;

function TelVector3iHelper.ToVector2f: TelVector2f;
begin
  Result := TelVector2f.Create(X * 1.0, Y * 1.0);
end;

function TelVector3iHelper.ToVector2i: TelVector2i;
begin
  Result := TelVector2i.Create(X, Y);
end;

function TelVector3iHelper.ToVector3f: TelVector3f;
begin
  Result := TelVector3f.Create(X * 1.0, Y * 1.0, Z * 1.0);
end;

class function TelVector3iHelper.Up: TelVector3i;
begin
  Result.X := 0;
  Result.Y := 1;
  Result.Z := 0;
end;

class function TelVector3iHelper.Zero: TelVector3i;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Z := 0;
end;

{ TelVector3fHelper }

procedure TelVector3fHelper.Clear;
begin
  X := 0.0;
  Y := 0.0;
  Z := 0.0;
end;

class function TelVector3fHelper.Create: TelVector3f;
begin
  Result.X := 0.0;
  Result.Y := 0.0;
  Result.Z := 0.0;
end;

class function TelVector3fHelper.Create(aX, aY: Single; aZ: Single = 0.0): TelVector3f;
begin
  Result.X := aX;
  Result.Y := aY;
  Result.Z := aZ;
end;

class function TelVector3fHelper.CrossProduct(A, B: TelVector3f): TelVector3f;
begin
  Result := TelVector3f.Create(A.Y * B.Z - B.Y * A.Z, A.Z * B.X - B.Z * A.X, A.X * B.Y - B.X * A.Y);
end;

class function TelVector3fHelper.Lerp(A, B: TelVector3f; Amt: Single): TelVector3f;
begin
  Result := TelVector3f.Create(ElysionMath.Lerp(A.X, B.X, Amt),
                            ElysionMath.Lerp(A.Y, B.Y, Amt),
                            ElysionMath.Lerp(A.Z, B.Z, Amt));
end;

class function TelVector3fHelper.DotProduct(A, B: TelVector3f): Single;
begin
  Result := (A.X * B.X) + (A.Y * B.Y) + (A.Z * B.Z);
end;

class function TelVector3fHelper.Forward: TelVector3f;
begin
  Result.X := 0.0;
  Result.Y := 0.0;
  Result.Z := 1.0;
end;

class function TelVector3fHelper.Right: TelVector3f;
begin
  Result.X := 1.0;
  Result.Y := 0.0;
  Result.Z := 0.0;
end;

function TelVector3fHelper.GetLength: Single;
begin
  Result := sqrt(X * X + Y * Y + Z * Z);
end;

function TelVector3fHelper.GetHash: TelHash;
begin
  fHash.Generate(Self.ToString());

  Result := fHash;
end;

class function TelVector3fHelper.Normalize(aVector: TelVector3f): TelVector3f;
begin
  Result.X := aVector.X / aVector.Length;
  Result.Y := aVector.Y / aVector.Length;
  Result.Z := aVector.Z / aVector.Length;
end;

procedure TelVector3fHelper.Normalize;
begin
  Self.X := Self.X / GetLength;
  Self.Y := Self.Y / GetLength;
  Self.Z := Self.Z / GetLength;
end;

class function TelVector3fHelper.One: TelVector3f;
begin
  Result.X := 1.0;
  Result.Y := 1.0;
  Result.Z := 1.0;
end;

function TelVector3fHelper.ToSize: TelSize;
begin
  Result := TelSize.Create(X, Y);
end;

function TelVector3fHelper.ToString: AnsiString;
begin
  Result := Format('vec3f(%f, %f, %f)', [X, Y, Z]);
end;

function TelVector3fHelper.ToVector2f: TelVector2f;
begin
  Result := TelVector2f.Create(X, Y);
end;

function TelVector3fHelper.ToVector2i: TelVector2i;
begin
  Result := TelVector2i.Create(Trunc(X), Trunc(Y));
end;

function TelVector3fHelper.ToVector3i: TelVector3i;
begin
  Result := TelVector3i.Create(Trunc(X), Trunc(Y), Trunc(Y));
end;

class function TelVector3fHelper.Up: TelVector3f;
begin
  Result.X := 0.0;
  Result.Y := 1.0;
  Result.Z := 0.0;
end;

class function TelVector3fHelper.Zero: TelVector3f;
begin
  Result.X := 0.0;
  Result.Y := 0.0;
  Result.Z := 0.0;
end;


end.
