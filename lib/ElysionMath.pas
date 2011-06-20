(**
  *
  *
  * @author(Johannes Stein <http://www.elysionpowered.org>)
  *)
unit ElysionMath;
// Crazy source code comment #1337: Hands up if you like math

{$I Elysion.inc}

interface


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

end.
