(**
  *
  *
  * @author(Johannes Stein <http://www.elysionpowered.org>)
  *)
unit ElysionMath;
// Crazy source code comment #1337: Hands up if you like math   -- I like math! \O/

{$I Elysion.inc}

interface

uses
  Classes,
  SDLUtils,
  SysUtils;
  
 type

  // Forward declarations
  PelVector2f = ^TelVector2f;
  PelVector2i = ^TelVector2i;
  PelVector3f = ^TelVector3f;
  PelVector3i = ^TelVector3i;
  
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

  function VectorEquals(VecOne, VecTwo: TelVector2f): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function VectorEquals(VecOne, VecTwo: TelVector2i): Boolean; Overload;{$IFDEF CAN_INLINE} inline; {$ENDIF}

  function VectorEquals(VecOne, VecTwo: TelVector3f): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function VectorEquals(VecOne, VecTwo: TelVector3i): Boolean; Overload;{$IFDEF CAN_INLINE} inline; {$ENDIF}

  function IsInRange(Min, Max, Value: Integer): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function IsInRange(Min, Max, Value: Single): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

  function makeV2f(aX, aY: Single): TelVector2f; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function makeV2i(aX, aY: Integer): TelVector2i; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function makeV3f(aX, aY: Single; aZ: Single = 0.0): TelVector3f; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function makeV3i(aX, aY: Integer; aZ: Integer = 0): TelVector3i; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

  function makeP2D(aX, aY: Integer): TelVector2i; Overload; deprecated;
  function makeP2D(aX, aY: Single): TelVector2f; Overload; deprecated;
  function makeP3D(aX, aY: Integer; aZ: Integer = 0): TelVector3i; Overload; deprecated;
  function makeP3D(aX, aY: Single; aZ: Single = 0.0): TelVector3f; Overload; deprecated;


function Clamp(Value, Min, Max: Integer): Integer; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function Clamp(Value, Min, Max: Single): Single; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

function ClampToByte(Value: Single): Single; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function ClampToByte(Value: Integer): Integer; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

function Lerp(Min, Max: Single; Amt: Single = 0.5): Single; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function Lerp(Min, Max: Integer; Amt: Single = 0.5): Single; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

function InverseLerp(Min, Max: Single; Amt: Single = 0.5): Single; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function InverseLerp(Min, Max: Integer; Amt: Single = 0.5): Single; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

function Slerp(Min, Max: Single; Amt: Single = 0.5): Single; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function Slerp(Min, Max: Integer; Amt: Single = 0.5): Single; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

function InverseSlerp(Min, Max: Single; Amt: Single = 0.5): Single; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function Inverseslerp(Min, Max: Integer; Amt: Single = 0.5): Single; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

function CubicHermite(Min, Max: Single; StartTangent, EndTangent: Single; Amt: Single = 0.5): Single;

implementation

function Clamp(Value, Min, Max: Integer): Integer;
begin
  Result := Trunc(Clamp(Value * 1.0, Min * 1.0, Max * 1.0));
end;

function Clamp(Value, Min, Max: Single): Single;
begin
  if Value <= Min then Result := Min
  else
    if Value >= Max then Result := Max
      else Result := Value;
end;

function ClampToByte(Value: Single): Single;
begin
  Result := Clamp(Value, 0.0, 255.0);
end;

function ClampToByte(Value: Integer): Integer;
begin
  Result := Clamp(Value, 0, 255);
end;

function Lerp(Min, Max: Single; Amt: Single = 0.5): Single;
var
  tmpAmt, Diff: Single;
begin
  tmpAmt := Clamp(Amt, 0.0, 1.0);
  Diff := Abs(Max - Min);

  Result := Min + (Diff * tmpAmt);
end;

function Lerp(Min, Max: Integer; Amt: Single = 0.5): Single;
begin
  Result := Lerp(Min * 1.0, Max * 1.0, Amt);
end;

function InverseLerp(Min, Max: Single; Amt: Single = 0.5): Single;
begin
  Result := Max - Lerp(Min, Max, Amt);
end;

function InverseLerp(Min, Max: Integer; Amt: Single = 0.5): Single;
begin
  Result := InverseLerp(Min * 1.0, Max * 1.0, Amt);
end;


function Slerp(Min, Max: Single; Amt: Single = 0.5): Single;
begin
  // TODO: Implement this stuff!
end;

function Slerp(Min, Max: Integer; Amt: Single = 0.5): Single;
begin
  Result := Slerp(Min * 1.0, Max * 1.0, Amt);
end;

function InverseSlerp(Min, Max: Single; Amt: Single = 0.5): Single;
begin
  Result := Max - Slerp(Min, Max, Amt);
end;

function InverseSlerp(Min, Max: Integer; Amt: Single = 0.5): Single;
begin
  Result := InverseSlerp(Min * 1.0, Max * 1.0, Amt);
end;

function CubicHermite(Min, Max: Single; StartTangent, EndTangent: Single; Amt: Single = 0.5): Single;
var
  t2, t3: Single;
begin
  t2 := Amt * Amt;
  t3 := Amt * Amt;
  
  Result := ( 2 * t3 - 3 * t2 + 1 ) * Min + ( t3 - 2 * t2 + Amt ) * StartTangent + ( -2 * t3 + 3 * t2 ) * Max + ( t3 - t2 ) * EndTangent;
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


function IsInRange(Min, Max, Value: Integer): Boolean;
begin
  Result :=  ((Value >= Min) and (Value <= Max));
end;

function IsInRange(Min, Max, Value: Single): Boolean;
begin
  Result :=  ((Value >= Min) and (Value <= Max));
end;


{$IFDEF CAN_METHODS}

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

function TelVector2i.Equals(aVector: TelVector2i): Boolean;
begin
  Result := ((Self.X = aVector.X) and (Self.Y = aVector.Y));
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

{$ENDIF}


end.
