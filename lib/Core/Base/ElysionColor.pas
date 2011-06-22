unit ElysionColor;

interface

{$I Elysion.inc}

uses
  SysUtils,
  SDLUtils;

type
  // Forward decleration
  PKeyIdent = Pointer;

  // Simple color type
  PelColor = ^TelColor;

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

  TelGradient = Record
    StartColor: TelColor;
    EndColor: TelColor;
    GradientStyle: TGradientStyle;
  end;

  TGradientStyle = SDLUtils.TGradientStyle;
  TColorVertices = array[0..3] of TelColor;


  // This class provides common colors
{
	Class: TelColorHelper
	Description:

	}
TelColorHelper = class
  private
    {
      Stores last used color in this variable as buffer
      -> See: function GetLastUsedColor: TelColor;
    }
    fLastUsedColor: TelColor;
    FBufferEmpty: Boolean;

    //function Get
  public
    constructor Create;
    destructor Destroy; Override;

	function clBlack: TelColor; {$IFDEF CAN_INLINE} inline; {$ENDIF} 		     //< R: 0   G: 0   B: 0
	function clMaroon: TelColor; {$IFDEF CAN_INLINE} inline; {$ENDIF}		     //< R: 128 G: 0   B: 0
	function clGreen: TelColor; {$IFDEF CAN_INLINE} inline; {$ENDIF}			     //< R: 0   G: 128 B: 0
	function clOlive: TelColor; {$IFDEF CAN_INLINE} inline; {$ENDIF}			     //< R: 128 G: 128 B: 0
	function clNavy: TelColor; {$IFDEF CAN_INLINE} inline; {$ENDIF}			     //< R: 0   G: 0   B: 128
	function clPurple: TelColor; {$IFDEF CAN_INLINE} inline; {$ENDIF}		     //< R: 128 G: 0   B: 128
	function clTeal: TelColor; {$IFDEF CAN_INLINE} inline; {$ENDIF}			     //< R: 0   G: 128 B: 128
	function clGray: TelColor; {$IFDEF CAN_INLINE} inline; {$ENDIF}			     //< R: 128 G: 128 B: 128
	function clSilver: TelColor; {$IFDEF CAN_INLINE} inline; {$ENDIF}		     //< R: 192 G: 192 B: 192
	function clRed: TelColor; {$IFDEF CAN_INLINE} inline; {$ENDIF}			       //< R: 255 G: 0   B: 0
	function clLime: TelColor; {$IFDEF CAN_INLINE} inline; {$ENDIF}			     //< R: 0   G: 255 B: 0
	function clBlue: TelColor; {$IFDEF CAN_INLINE} inline; {$ENDIF}			     //< R: 0   G: 0   B: 255
	function clFuchsia: TelColor; {$IFDEF CAN_INLINE} inline; {$ENDIF}		     //< R: 255 G: 0   B: 255
	function clAqua: TelColor; {$IFDEF CAN_INLINE} inline; {$ENDIF}			     //< R: 0   G: 255 B: 255
	function clCornflowerBlue: TelColor; {$IFDEF CAN_INLINE} inline; {$ENDIF} //< R: 100 G: 149 B: 237 Best color ever, right? Right? Am I right or what?
  function clFreezeDevBlue: TelColor; {$IFDEF CAN_INLINE} inline; {$ENDIF} //< R: 73 G: 92 B: 108 My own blue-ish color
  function clWhite: TelColor; {$IFDEF CAN_INLINE} inline; {$ENDIF}			     //< R: 255 G: 255 B: 255

	function GetLastUsedColor: TelColor; {$IFDEF CAN_INLINE} inline; {$ENDIF} //< Returns last used color
	procedure ClearBuffer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  published
    //property Buffer:
end;

function makeCol(aR, aG, aB: Byte; anA: Byte = 255): TelColor; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function makeCol(aR, aG, aB: Single; anA: Single = 1.0): TelColor; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

function makeGradient(StartColor: TelColor; EndColor: TelColor; GradientStyle: TGradientStyle = gsVertical): TelGradient; {$IFDEF CAN_INLINE} inline; {$ENDIF}

{$IFNDEF CAN_METHODS}
function ColorEquals(ColorOne, ColorTwo: TelColor): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
{$ENDIF}


{$IFDEF AUTO_INIT}
var
  Color: TelColorHelper;
{$ENDIF}

implementation

uses
  ElysionData;

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

{$IFDEF CAN_METHODS}

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

{$ELSE}

function ColorEquals(ColorOne, ColorTwo: TelColor): Boolean;
begin
  Result := ((ColorOne.R = ColorTwo.R) or (ColorOne.G = ColorTwo.G) or (ColorOne.B = ColorTwo.B) or (ColorOne.A = ColorOne.A));
end;

{$ENDIF}

constructor TelColorHelper.Create;
begin
  inherited Create;

  FBufferEmpty := true;
  fLastUsedColor := makeCol(0, 0, 0);
end;

destructor TelColorHelper.Destroy;
begin
  ClearBuffer;

  inherited Destroy;
end;

function TelColorHelper.clBlack: TelColor; 
begin
  FBufferEmpty := false;
  fLastUsedColor := makeCol(0, 0, 0);
  Result := fLastUsedColor;
end;

function TelColorHelper.clMaroon: TelColor; 
begin
  FBufferEmpty := false;
  fLastUsedColor := makeCol(128, 0, 0);
  Result := fLastUsedColor;
end;

function TelColorHelper.clGreen: TelColor; 
begin
  FBufferEmpty := false;
  fLastUsedColor := makeCol(0, 128, 0);
  Result := fLastUsedColor;
end;

function TelColorHelper.clOlive: TelColor; 
begin
  FBufferEmpty := false;
  fLastUsedColor := makeCol(128, 128, 0);
  Result := fLastUsedColor;
end;

function TelColorHelper.clNavy: TelColor; 
begin
  FBufferEmpty := false;
  fLastUsedColor := makeCol(0, 0, 128);
  Result := fLastUsedColor;
end;

function TelColorHelper.clPurple: TelColor; 
begin
  FBufferEmpty := false;
  fLastUsedColor := makeCol(128, 0, 128);
  Result := fLastUsedColor;
end;

function TelColorHelper.clTeal: TelColor; 
begin
  FBufferEmpty := false;
  fLastUsedColor := makeCol(0, 128, 128);
  Result := fLastUsedColor;
end;

function TelColorHelper.clGray: TelColor; 
begin
  FBufferEmpty := false;
  fLastUsedColor := makeCol(128, 128, 128);
  Result := fLastUsedColor;
end;

function TelColorHelper.clSilver: TelColor; 
begin
  FBufferEmpty := false;
  fLastUsedColor := makeCol(192, 192, 192);
  Result := fLastUsedColor;
end;

function TelColorHelper.clRed: TelColor; 
begin
  FBufferEmpty := false;
  fLastUsedColor := makeCol(255, 0, 0);
  Result := fLastUsedColor;
end;

function TelColorHelper.clLime: TelColor; 
begin
  FBufferEmpty := false;
  fLastUsedColor := makeCol(0, 255, 0);
  Result := fLastUsedColor;
end;

function TelColorHelper.clBlue: TelColor; 
begin
  FBufferEmpty := false;
  fLastUsedColor := makeCol(0, 0, 255);
  Result := fLastUsedColor;
end;

function TelColorHelper.clFuchsia: TelColor; 
begin
  FBufferEmpty := false;
  fLastUsedColor := makeCol(255, 0, 255);
  Result := fLastUsedColor;
end;

function TelColorHelper.clAqua: TelColor; 
begin
  FBufferEmpty := false;
  fLastUsedColor := makeCol(0, 255, 255);
  Result := fLastUsedColor;
end;

function TelColorHelper.clCornflowerBlue: TelColor; 
begin
  FBufferEmpty := false;
  fLastUsedColor := makeCol(100, 149, 237);
  Result := fLastUsedColor;
end;

function TelColorHelper.clFreezeDevBlue: TelColor;
begin
  FBufferEmpty := false;
  fLastUsedColor := makeCol(73, 92, 108);
  Result := fLastUsedColor;
end;

function TelColorHelper.clWhite: TelColor; 
begin
  FBufferEmpty := false;
  fLastUsedColor := makeCol(255, 255, 255);
  Result := fLastUsedColor;
end;

function TelColorHelper.GetLastUsedColor: TelColor; 
begin
  if (not FBufferEmpty) then Result := fLastUsedColor;
end;

procedure TelColorHelper.ClearBuffer; 
begin
  FBufferEmpty := true;
end;

{$IFDEF AUTO_INIT}
initialization
  Color := TelColorHelper.Create;

finalization
  Color.Destroy;
{$ENDIF}

end.
