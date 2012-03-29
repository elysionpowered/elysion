unit ElysionColor;

{$I Elysion.inc}

interface

uses
  SysUtils,

  ElysionEnums,
  ElysionMath;

type

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


 { TelColor }

 TelColor = record
 private
   fR, fG, fB, fA: Byte;
   fH, fS, fL: Single;

   function GetH: Single;
   function GetS: Single;
   function GetL: Single;

   procedure SetH(AValue: Single);
   procedure SetS(AValue: Single);
   procedure SetL(AValue: Single);

   procedure ConvertRGBToHSL;
   procedure ConvertHSLToRGB;
 public
   procedure Clear(anAlpha: Byte = 255); inline;
   procedure Make(aR, aG, aB: Byte; anA: Byte = 255); inline;

   procedure Lighten(AValue: Single; IgnoreAlpha: Boolean = true);
   procedure Darken(AValue: Single; IgnoreAlpha: Boolean = true);

   function ToString(): AnsiString;

   procedure ToFloat(out floatR: Single; out floatG: Single; out floatB: Single; out floatA: Single);
 public
   class function Create: TelColor; static; inline; Overload;
   class function Create(aR, aG, aB: Byte; anA: Byte = 255): TelColor; static; Overload;

   class function Copy(Source: TelColor): TelColor; static;

   class function From(aString: AnsiString): TelColor; static; Overload;
   class function From(aColor: Cardinal): TelColor; static; Overload;

   class function Min(A, B: TelColor): TelColor; static;
   class function Max(A, B: TelColor): TelColor; static;
   class function Lerp(A, B: TelColor; Amt: Single = 0.5): TelColor; static;
 public
   // Basic colors
   class function clAqua: TelColor; static; inline;	            //< R: 0   G: 255 B: 255
   class function clBlack: TelColor; static; inline; 	            //< R: 0   G: 0   B: 0
   class function clBlue: TelColor; static; inline;                 //< R: 0   G: 0   B: 255
   class function clFuchsia: TelColor; static; inline;	            //< R: 255 G: 0   B: 255
   class function clGray: TelColor; static; inline;	            //< R: 128 G: 128 B: 128
   class function clGrey: TelColor; static; inline;                 //< Alternative spelling for TelColor.clGray
   class function clGreen: TelColor; static; inline;	            //< R: 0   G: 128 B: 0
   class function clLime: TelColor; static; inline;	            //< R: 0   G: 255 B: 0
   class function clMaroon: TelColor; static; inline;	            //< R: 128 G: 0   B: 0
   class function clNavy: TelColor; static; inline;                 //< R: 0   G: 0   B: 128
   class function clOlive: TelColor; static; inline;	            //< R: 128 G: 128 B: 0
   class function clPurple: TelColor; static; inline;	            //< R: 128 G: 0   B: 128
   class function clRed: TelColor; static; inline;	            //< R: 255 G: 0   B: 0
   class function clSilver: TelColor; static; inline;	            //< R: 192 G: 192 B: 192
   class function clTeal: TelColor; static; inline;	            //< R: 0   G: 128 B: 128
   class function clWhite: TelColor; static; inline;	            //< R: 255 G: 255 B: 255
   class function clYellow: TelColor; static; inline;               //< R: 255 G: 255 B: 0

   class function clTransparent: TelColor; static; inline;          //< Like TelColor.clBlack, but with A value of 0

   // Advanced colors (http://www.w3schools.com/cssref/css_colornames.asp)
   class function clAliceBlue: TelColor; static; inline;            //< R: 240 G: 248 B: 255
   class function clAntiqueWhite: TelColor; static; inline;         //< R: 250 G: 235 B: 215
   class function clAquamarine: TelColor; static; inline;           //< R: 127 G: 255 B: 212
   class function clAzure: TelColor; static; inline;                 //< R: 240 G: 255 B: 255
   class function clBeige: TelColor; static; inline;	            //< R: 245 G: 245 B: 220
   class function clBisque: TelColor; static; inline;	            //<	R: 255 G: 228 B: 196
   class function clBlanchedAlmond: TelColor; static; inline;	    //<	R: 255 G: 235 B: 205
   class function clBlueViolet: TelColor; static; inline;	    //<	R: 138 G: 43  B: 226
   class function clBrown: TelColor; static; inline;	            //<	R: 165 G: 42  B: 42
   class function clBurlyWood: TelColor; static; inline;	    //<	R: 222 G: 184 B: 135
   class function clCadetBlue: TelColor; static; inline;	    //<	R: 95  G: 158 B: 160
   class function clChartreuse: TelColor; static; inline;	    //<	R: 127 G: 255 B: 0
   class function clChocolate: TelColor; static; inline;	    //<	R: 210 G: 105 B: 30
   class function clCoral: TelColor; static; inline;	            //<	R: 255 G: 127 B: 80
   class function clCornflowerBlue: TelColor; static; inline;	    //<	R: 100 G: 149 B: 237
   class function clCornsilk: TelColor; static; inline;	            //<	R: 255 G: 248 B: 220
   class function clCrimson: TelColor; static; inline;	            //<	R: 220 G: 20  B: 60
   class function clCyan: TelColor; static; inline;	            //<	R: 0   G: 255 B: 255
   class function clDarkBlue: TelColor; static; inline;	            //<	R: 0   G: 0   B: 139
   class function clDarkCyan: TelColor; static; inline;	            //<	R: 0   G: 139 B: 139
   class function clDarkGoldenRod: TelColor; static; inline;	    //<	R: 184 G: 134 B: 11
   class function clDarkGray: TelColor; static; inline;	            //<	R: 169 G: 169 B: 169
   class function clDarkGrey: TelColor; static; inline;	            //< Alternative spelling for TelColor.clDarkGray
   class function clDarkGreen: TelColor; static; inline;	    //<	R: 0   G: 100 B: 0
   class function clDarkKhaki: TelColor; static; inline;	    //<	R: 189 G: 183 B: 107
   class function clDarkMagenta: TelColor; static; inline;	    //<	R: 139 G: 0   B: 139
   class function clDarkOliveGreen: TelColor; static; inline;	    //<	R: 85  G: 107 B: 47
   class function clDarkorange: TelColor; static; inline;	    //< R: 255 G: 140 B: 0
   class function clDarkOrchid: TelColor; static; inline;	    //< R: 153 G: 50  B: 204
   class function clDarkRed: TelColor; static; inline;	            //< R: 139 G: 0   B: 0
   class function clDarkSalmon: TelColor; static; inline;	    //< R: 233 G: 150 B: 122
   class function clDarkSeaGreen: TelColor; static; inline;	    //< R: 143 G: 188 B: 143
   class function clDarkSlateBlue: TelColor; static; inline;	    //< R: 72  G: 61  B: 139
   class function clDarkSlateGray: TelColor; static; inline;	    //< R: 47  G: 79  B: 79
   class function clDarkSlateGrey: TelColor; static; inline;	    //< R: 47  G: 79  B: 79
   class function clDarkTurquoise: TelColor; static; inline;	    //< R: 0   G: 206 B: 209
   class function clDarkViolet: TelColor; static; inline;	    //< R: 148 G: 0   B: 211
   class function clDeepPink: TelColor; static; inline;	            //< R: 255 G: 20  B: 147
   class function clDeepSkyBlue: TelColor; static; inline;	    //< R: 0   G: 191 B: 255
   class function clDimGray: TelColor; static; inline;	            //< R: 105 G: 105 B: 105
   class function clDimGrey: TelColor; static; inline;	            //< Alternative spelling for TelColor.clDimGray
   class function clDodgerBlue: TelColor; static; inline;	    //< R: 30  G: 144 B: 255
   class function clFireBrick: TelColor; static; inline;	    //< R: 178 G: 34  B: 34
   class function clFloralWhite: TelColor; static; inline;	    //< R: 255 G: 250 B: 240
   class function clForestGreen: TelColor; static; inline;	    //< R: 34  G: 139 B: 34
   class function clGainsboro: TelColor; static; inline;	    //< R: 220 G: 220 B: 220
   class function clGhostWhite: TelColor; static; inline;	    //< R: 248 G: 248 B: 255
   class function clGold: TelColor; static; inline;	            //< R: 255 G: 215 B: 0
   class function clGoldenRod: TelColor; static; inline;	    //< R: 218 G: 165 B: 32
   class function clGreenYellow: TelColor; static; inline;	    //< R: 173 G: 255 B: 47
   class function clHoneyDew: TelColor; static; inline;	            //< R: 240 G: 255 B: 240
   class function clHotPink: TelColor; static; inline;	            //< R: 255 G: 105 B: 180
   class function clIndianRed: TelColor; static; inline;	    //< R: 205 G: 92  B: 92
   class function clIndigo: TelColor; static; inline;	            //< R: 75  G: 0   B: 130
   class function clIvory: TelColor; static; inline;	            //< R: 255 G: 255 B: 240
   class function clKhaki: TelColor; static; inline;	            //< R: 240 G: 230 B: 140
   class function clLavender: TelColor; static; inline;	            //< R: 230 G: 230 B: 250
   class function clLavenderBlush: TelColor; static; inline;	    //< R: 255 G: 240 B: 245
   class function clLawnGreen: TelColor; static; inline;	    //< R: 124 G: 252 B: 0
   class function clLemonChiffon: TelColor; static; inline;	    //< R: 255 G: 250 B: 205
   class function clLightBlue: TelColor; static; inline;	    //< R: 173 G: 216 B: 230
   class function clLightCoral: TelColor; static; inline;	    //< R: 240 G: 128 B: 128
   class function clLightCyan: TelColor; static; inline;	    //< R: 224 G: 255 B: 255
   class function clLightGoldenRodYellow: TelColor; static; inline; //< R: 250 G: 250 B: 210
   class function clLightGray: TelColor; static; inline;	    //< R: 211 G: 211 B: 211
   class function clLightGrey: TelColor; static; inline;	    //< Alternative spelling for TelColor.clLightGray
   class function clLightGreen: TelColor; static; inline;	    //< R: 144 G: 238 B: 144
   class function clLightPink: TelColor; static; inline;	    //< R: 255 G: 182 B: 193
   class function clLightSalmon: TelColor; static; inline;	    //< R: 255 G: 160 B: 122
   class function clLightSeaGreen: TelColor; static; inline;	    //< R: 32  G: 178 B: 170
   class function clLightSkyBlue: TelColor; static; inline;	    //< R: 135 G: 206 B: 250
   class function clLightSlateGray: TelColor; static; inline;	    //< R: 119 G: 136 B: 153
   class function clLightSlateGrey: TelColor; static; inline;	    //< Alternative spelling for TelColor.clLightSlateGray
   class function clLightSteelBlue: TelColor; static; inline;	    //< R: 176 G: 196 B: 222
   class function clLightYellow: TelColor; static; inline;	    //< R: 255 G: 255 B: 224
   class function clLimeGreen: TelColor; static; inline;	    //< R: 50  G: 205 B: 50
   class function clLinen: TelColor; static; inline;	            //< R: 250 G: 240 B: 230
   class function clMagenta: TelColor; static; inline;	            //< Same as TelColor.clFuchsia
   class function clMediumAquaMarine: TelColor; static; inline;	    //< R: 102 G: 205 B: 170
   class function clMediumBlue: TelColor; static; inline;	    //< R: 0   G: 0   B: 205
   class function clMediumOrchid: TelColor; static; inline;	    //< R: 186 G: 85  B: 211
   class function clMediumPurple: TelColor; static; inline;	    //< R: 147 G: 112 B: 219
   class function clMediumSeaGreen: TelColor; static; inline;	    //< R: 60  G: 179 B: 113
   class function clMediumSlateBlue: TelColor; static; inline;	    //< R: 123 G: 104 B: 238
   class function clMediumSpringGreen: TelColor; static; inline;    //< R: 0   G: 250 B: 154
   class function clMediumTurquoise: TelColor; static; inline;	    //< R: 72  G: 209 B: 204
   class function clMediumVioletRed: TelColor; static; inline;	    //< R: 199 G: 21  B: 133
   class function clMidnightBlue: TelColor; static; inline;	    //< R: 25  G: 25  B: 112
   class function clMintCream: TelColor; static; inline;	    //< R: 245 G: 255 B: 250
   class function clMistyRose: TelColor; static; inline;	    //< R: 255 G: 228 B: 225
   class function clMoccasin: TelColor; static; inline;	            //< R: 255 G: 228 B: 181
   class function clNavajoWhite: TelColor; static; inline;	    //< R: 255 G: 222 B: 173
   class function clOldLace: TelColor; static; inline;	            //< R: 253 G: 245 B: 230
   class function clOliveDrab: TelColor; static; inline;	    //< R: 107 G: 142 B: 35
   class function clOrange: TelColor; static; inline;	            //< R: 255 G: 165 B: 0
   class function clOrangeRed: TelColor; static; inline;	    //< R: 255 G: 69  B: 0
   class function clOrchid: TelColor; static; inline;	            //< R: 218 G: 112 B: 214
   class function clPaleGoldenRod: TelColor; static; inline;	    //< R: 238 G: 232 B: 170
   class function clPaleGreen: TelColor; static; inline;	    //< R: 152 G: 251 B: 152
   class function clPaleTurquoise: TelColor; static; inline;	    //< R: 175 G: 238 B: 238
   class function clPaleVioletRed: TelColor; static; inline;	    //< R: 219 G: 112 B: 147
   class function clPapayaWhip: TelColor; static; inline;	    //< R: 255 G: 239 B: 213
   class function clPeachPuff: TelColor; static; inline;	    //< R: 255 G: 218 B: 185
   class function clPeru: TelColor; static; inline;	            //< R: 205 G: 133 B: 63
   class function clPink: TelColor; static; inline;	            //< R: 255 G: 192 B: 203
   class function clPlum: TelColor; static; inline;	            //< R: 221 G: 160 B: 221
   class function clPowderBlue: TelColor; static; inline;	    //< R: 176 G: 224 B: 230
   class function clRosyBrown: TelColor; static; inline;	    //< R: 188 G: 143 B: 143
   class function clRoyalBlue: TelColor; static; inline;	    //< R: 65  G: 105 B: 225
   class function clSaddleBrown: TelColor; static; inline;	    //< R: 139 G: 69  B: 19
   class function clSalmon: TelColor; static; inline;	            //< R: 250 G: 128 B: 114
   class function clSandyBrown: TelColor; static; inline;	    //< R: 244 G: 164 B: 96
   class function clSeaGreen: TelColor; static; inline;	            //< R: 46  G: 139 B: 87
   class function clSeaShell: TelColor; static; inline;	            //< R: 255 G: 245 B: 238
   class function clSienna: TelColor; static; inline;	            //< R: 160 G: 82  B: 45
   class function clSkyBlue: TelColor; static; inline;	            //< R: 135 G: 206 B: 235
   class function clSlateBlue: TelColor; static; inline;	    //< R: 106 G: 90  B: 205
   class function clSlateGray: TelColor; static; inline;	    //< R: 112 G: 128 B: 144
   class function clSlateGrey: TelColor; static; inline;	    //< R: 112 G: 128 B: 144
   class function clSnow: TelColor; static; inline;	            //< R: 255 G: 250 B: 250
   class function clSpringGreen: TelColor; static; inline;	    //< R: 0   G: 255 B: 127
   class function clSteelBlue: TelColor; static; inline;	    //< R: 70  G: 130 B: 180
   class function clTan: TelColor; static; inline;	            //< R: 210 G: 180 B: 140
   class function clThistle: TelColor; static; inline;	            //< R: 216 G: 191 B: 216
   class function clTomato: TelColor; static; inline;	            //< R: 255 G: 99  B: 71
   class function clTurquoise: TelColor; static; inline;	    //< R: 64  G: 224 B: 208
   class function clViolet: TelColor; static; inline;	            //< R: 238 G: 130 B: 238
   class function clWheat: TelColor; static; inline;	            //< R: 245 G: 222 B: 179
   class function clWhiteSmoke: TelColor; static; inline;	    //< R: 245 G: 245 B: 245
   class function clYellowGreen: TelColor; static; inline;	    //< R: 154 G: 205 B: 50

   // Custom colors
   class function clFreezeDevBlue: TelColor; static; inline;        //< R: 73  G: 92  B: 108  My own blue-ish color
 public
   class operator Add(A, B: TelColor): TelColor;
   class operator Subtract(A, B: TelColor): TelColor;
   class operator Multiply(A, B: TelColor): TelColor;
   class operator Multiply(A: TelColor; Factor: Single): TelColor;
   class operator Divide(A, B: TelColor): TelColor;

   class operator IntDivide(A, B: TelColor): TelColor;
   class operator Modulus(A, B: TelColor): TelColor;

   class operator Equal(A, B: TelColor): Boolean;
   class operator NotEqual(A, B: TelColor): Boolean;
   class operator GreaterThan(A, B: TelColor): Boolean;
   class operator GreaterThanOrEqual(A, B: TelColor): Boolean;
   class operator LessThan(A, B: TelColor): Boolean;
   class operator LessThanOrEqual(A, B: TelColor): Boolean;
 public
   property R: Byte read fR write fR;
   property G: Byte read fG write fG;
   property B: Byte read fB write fB;
   property A: Byte read fA write fA;

   property H: Single read GetH write SetH;
   property S: Single read GetS write SetS;
   property L: Single read GetL write SetL;
 end;

 TelColorArray = array of TelColor;

 TelColorVertices = array[0..3] of TelColor;

 { TelColorHelper }

 (*TelColorHelper = record helper for TelColor
 public
   function ToVertices: TelColorVertices; inline; Overload;
 public
   class function ToVertices(A, B, C, D: TelColor): TelColorVertices; static; inline; Overload;
 end;*)

 {%endregion}


 TelGradient = record
 public type
   TStyle = (gsHorizontal, gsVertical);
 private
   fStartColor: TelColor;
   fEndColor: TelColor;
   fStyle: TStyle;
 public
   procedure Clear;
 public
   class function Create: TelGradient; static; Overload;
   class function Create(aStartColor, anEndColor: TelColor; aStyle: TStyle = gsVertical): TelGradient; static; Overload;

   class function Copy(aSource: TelGradient): TelGradient; static;
 public
   property StartColor: TelColor read fStartColor write fStartColor;
   property EndColor: TelColor read fEndColor write fEndColor;
   property Style: TStyle read fStyle write fStyle;
 end;


 function makeGradient(StartColor: TelColor; EndColor: TelColor; GradientStyle: TelGradient.TStyle = gsVertical): TelGradient; inline; deprecated 'Use TelGradient.Create(...) instead';

 function makeCol(aR, aG, aB: Byte; anA: Byte = 255): TelColor; Overload; inline; deprecated 'Use TelColor.Create(...) instead';

 function ColorToVertices(aColor: TelColor): TelColorVertices; Overload; deprecated 'Use TelColor.ToVertices(...) instead';
 function ColorToVertices(TopLeft, TopRight, BottomLeft, BottomRight: TelColor): TelColorVertices; Overload; deprecated 'Use TelColor.ToVertices(...) instead';


implementation

function makeGradient(StartColor: TelColor; EndColor: TelColor; GradientStyle: TelGradient.TStyle = gsVertical): TelGradient;
var
  tmpGradient: TelGradient;
begin
  tmpGradient.StartColor := StartColor;
  tmpGradient.EndColor := EndColor;
  tmpGradient.Style := GradientStyle;

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

function ColorToVertices(aColor: TelColor): TelColorVertices;
var
  tmpVertices: TelColorVertices;
begin
  tmpVertices[0] := aColor;
  tmpVertices[1] := aColor;
  tmpVertices[2] := aColor;
  tmpVertices[3] := aColor;

  Result := tmpVertices;
end;

function ColorToVertices(TopLeft, TopRight, BottomLeft, BottomRight: TelColor
  ): TelColorVertices;
var
  tmpVertices: TelColorVertices;
begin
  tmpVertices[0] := TopLeft;
  tmpVertices[1] := TopRight;
  tmpVertices[2] := BottomLeft;
  tmpVertices[3] := BottomRight;

  Result := tmpVertices;
end;


{ TelColorHelper }

(*function TelColorHelper.ToVertices: TelColorVertices;
begin
  Result[0] := TelColor.Create(fR, fG, fB, fA);
  Result[1] := TelColor.Create(fR, fG, fB, fA);
  Result[2] := TelColor.Create(fR, fG, fB, fA);
  Result[3] := TelColor.Create(fR, fG, fB, fA);
end;

class function TelColorHelper.ToVertices(A, B, C, D: TelColor
  ): TelColorVertices;
begin
  Result[0] := A;
  Result[1] := B;
  Result[2] := C;
  Result[3] := D;
end;*)


function TelColor.GetH: Single;
begin

end;

function TelColor.GetS: Single;
begin

end;

function TelColor.GetL: Single;
begin

end;

procedure TelColor.SetH(AValue: Single);
begin

end;

procedure TelColor.SetS(AValue: Single);
begin

end;

procedure TelColor.SetL(AValue: Single);
begin

end;

procedure TelColor.ConvertRGBToHSL;
begin

end;

procedure TelColor.ConvertHSLToRGB;
begin

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

procedure TelColor.Lighten(AValue: Single; IgnoreAlpha: Boolean = true);
begin
  AValue := Clamp(AValue, 0.0, 1.0);

  Self.R := Self.R + Trunc(AValue * 255);
  Self.G := Self.G + Trunc(AValue * 255);
  Self.B := Self.B + Trunc(AValue * 255);

  if not IgnoreAlpha then
    Self.A := Self.A + Trunc(AValue * 255);
end;

procedure TelColor.Darken(AValue: Single; IgnoreAlpha: Boolean = true);
begin
  AValue := Clamp(AValue, 0.0, 1.0);

  Self.R := Self.R - Trunc(AValue * 255);
  Self.G := Self.G - Trunc(AValue * 255);
  Self.B := Self.B - Trunc(AValue * 255);

  if not IgnoreAlpha then
    Self.A := Self.A - Trunc(AValue * 255);
end;

function TelColor.ToString(): AnsiString;
begin
  if A <> 255 then
    Result := Format('rgba(%d, %d, %d, %f)', [R, G, B, A / 255])
  else
    Result := Format('rgb(%d, %d, %d)', [R, G, B]);
end;

procedure TelColor.ToFloat(out floatR: Single; out floatG: Single; out floatB: Single; out floatA: Single);
begin
  floatR := R / 255;
  floatG := G / 255;
  floatB := B / 255;
  floatA := A / 255;
end;

class function TelColor.Create: TelColor;
begin
  Result.R := 0;
  Result.G := 0;
  Result.B := 0;
  Result.A := 255;
end;

class function TelColor.Create(aR, aG, aB: Byte; anA: Byte): TelColor;
begin
  Result.R := aR;
  Result.G := aG;
  Result.B := aB;
  Result.A := anA;
end;

class function TelColor.Copy(Source: TelColor): TelColor;
begin
  Result.R := Source.R;
  Result.G := Source.G;
  Result.B := Source.B;
  Result.A := Source.A;
end;

class function TelColor.From(aString: AnsiString): TelColor;
begin

end;

class function TelColor.From(aColor: Cardinal): TelColor;
begin

end;

class function TelColor.Min(A, B: TelColor): TelColor;
var
  tmpR, tmpG, tmpB, tmpA: Byte;
begin
  if A.R >= B.R then tmpR := B.R else tmpR := A.R;
  if A.G >= B.G then tmpG := B.G else tmpR := A.G;
  if A.B >= B.B then tmpB := B.B else tmpR := A.B;
  if A.A >= B.A then tmpA := B.A else tmpR := A.A;

  Result := TelColor.Create(tmpR, tmpG, tmpB, tmpA);
end;

class function TelColor.Max(A, B: TelColor): TelColor;
var
  tmpR, tmpG, tmpB, tmpA: Byte;
begin
  if A.R >= B.R then tmpR := A.R else tmpR := B.R;
  if A.G >= B.G then tmpG := A.G else tmpR := B.G;
  if A.B >= B.B then tmpB := A.B else tmpR := B.B;
  if A.A >= B.A then tmpA := A.A else tmpR := B.A;

  Result := TelColor.Create(tmpR, tmpG, tmpB, tmpA);
end;

class function TelColor.Lerp(A, B: TelColor; Amt: Single = 0.5): TelColor;
begin

end;

class function TelColor.clAqua: TelColor;
begin
  Result := TelColor.Create(0, 255, 255);
end;

class function TelColor.clBlack: TelColor;
begin
  Result := TelColor.Create(0, 0, 0);
end;

class function TelColor.clBlue: TelColor;
begin
  Result := TelColor.Create(0, 0, 255);
end;

class function TelColor.clFuchsia: TelColor;
begin
  Result := TelColor.Create(255, 0, 255);
end;

class function TelColor.clGray: TelColor;
begin
  Result := TelColor.Create(128, 128, 128);
end;

class function TelColor.clGrey: TelColor;
begin
  Result := TelColor.clGray;
end;

class function TelColor.clGreen: TelColor;
begin
  Result := TelColor.Create(0, 128, 0);
end;

class function TelColor.clLime: TelColor;
begin
  Result := TelColor.Create(0, 255, 0);
end;

class function TelColor.clMaroon: TelColor;
begin
  Result := TelColor.Create(128, 0, 0);
end;

class function TelColor.clNavy: TelColor;
begin
  Result := TelColor.Create(0, 0, 128);
end;

class function TelColor.clOlive: TelColor;
begin
  Result := TelColor.Create(128, 128, 0);
end;

class function TelColor.clPurple: TelColor;
begin
  Result := TelColor.Create(128, 0, 128);
end;

class function TelColor.clRed: TelColor;
begin
  Result := TelColor.Create(255, 0, 0);
end;

class function TelColor.clSilver: TelColor;
begin
  Result := TelColor.Create(192, 192, 192);
end;

class function TelColor.clTeal: TelColor;
begin
  Result := TelColor.Create(0, 128, 128);
end;

class function TelColor.clWhite: TelColor;
begin
  Result := TelColor.Create(255, 255, 255);
end;

class function TelColor.clYellow: TelColor;
begin
  Result := TelColor.Create(255, 255, 0);
end;

class function TelColor.clTransparent: TelColor;
begin
  Result := TelColor.Create(0, 0, 0, 0);
end;

class function TelColor.clAliceBlue: TelColor;
begin
  Result := TelColor.Create(240, 248, 255);
end;

class function TelColor.clAntiqueWhite: TelColor;
begin
  Result := TelColor.Create(250, 235, 215);
end;

class function TelColor.clAquamarine: TelColor;
begin
  Result := TelColor.Create(127, 255, 212);
end;

class function TelColor.clAzure: TelColor;
begin
  Result := TelColor.Create(240, 255, 255);
end;

class function TelColor.clBeige: TelColor;
begin
  Result := TelColor.Create(245, 245, 220);
end;

class function TelColor.clBisque: TelColor;
begin
  Result := TelColor.Create(255, 228, 196);
end;

class function TelColor.clBlanchedAlmond: TelColor;
begin
  Result := TelColor.Create(255, 235, 205);
end;

class function TelColor.clBlueViolet: TelColor;
begin
  Result := TelColor.Create(138, 43, 226);
end;

class function TelColor.clBrown: TelColor;
begin
  Result := TelColor.Create(165, 42, 42);
end;

class function TelColor.clBurlyWood: TelColor;
begin
  Result := TelColor.Create(222, 184, 135);
end;

class function TelColor.clCadetBlue: TelColor;
begin
  Result := TelColor.Create(95, 158, 160);
end;

class function TelColor.clChartreuse: TelColor;
begin
  Result := TelColor.Create(127, 255, 0);
end;

class function TelColor.clChocolate: TelColor;
begin
  Result := TelColor.Create(210, 105, 30);
end;

class function TelColor.clCoral: TelColor;
begin
  Result := TelColor.Create(255, 127, 80);
end;

class function TelColor.clCornflowerBlue: TelColor;
begin
  Result := TelColor.Create(100, 149, 237);
end;

class function TelColor.clCornsilk: TelColor;
begin
  Result := TelColor.Create(255, 248, 220);
end;

class function TelColor.clCrimson: TelColor;
begin
  Result := TelColor.Create(220, 20, 60);
end;

class function TelColor.clCyan: TelColor;
begin
  Result := TelColor.Create(0, 255, 255);
end;

class function TelColor.clDarkBlue: TelColor;
begin
  Result := TelColor.Create(0, 0, 139);
end;

class function TelColor.clDarkCyan: TelColor;
begin
  Result := TelColor.Create(0, 139, 139);
end;

class function TelColor.clDarkGoldenRod: TelColor;
begin
  Result := TelColor.Create(184, 134, 11);
end;

class function TelColor.clDarkGray: TelColor;
begin
  Result := TelColor.Create(169, 169, 169);
end;

class function TelColor.clDarkGrey: TelColor;
begin
  Result := TelColor.clDarkGray;
end;

class function TelColor.clDarkGreen: TelColor;
begin
  Result := TelColor.Create(0, 100, 0);
end;

class function TelColor.clDarkKhaki: TelColor;
begin
  Result := TelColor.Create(189, 183, 107);
end;

class function TelColor.clDarkMagenta: TelColor;
begin
  Result := TelColor.Create(139, 0, 139);
end;

class function TelColor.clDarkOliveGreen: TelColor;
begin
  Result := TelColor.Create(85, 107, 47);
end;

class function TelColor.clDarkorange: TelColor;
begin
  Result := TelColor.Create(255, 140, 0);
end;

class function TelColor.clDarkOrchid: TelColor;
begin
  Result := TelColor.Create(153, 50, 204);
end;

class function TelColor.clDarkRed: TelColor;
begin
  Result := TelColor.Create(139, 0, 0);
end;

class function TelColor.clDarkSalmon: TelColor;
begin
  Result := TelColor.Create(233, 150, 122);
end;

class function TelColor.clDarkSeaGreen: TelColor;
begin
  Result := TelColor.Create(143, 188, 143);
end;

class function TelColor.clDarkSlateBlue: TelColor;
begin
  Result := TelColor.Create(72, 61, 139);
end;

class function TelColor.clDarkSlateGray: TelColor;
begin
  Result := TelColor.Create(47, 79, 79);
end;

class function TelColor.clDarkSlateGrey: TelColor;
begin
  Result := TelColor.clDarkSlateGray;
end;

class function TelColor.clDarkTurquoise: TelColor;
begin
  Result := TelColor.Create(0, 206, 209);
end;

class function TelColor.clDarkViolet: TelColor;
begin
  Result := TelColor.Create(148, 0, 211);
end;

class function TelColor.clDeepPink: TelColor;
begin
  Result := TelColor.Create(255, 20, 147);
end;

class function TelColor.clDeepSkyBlue: TelColor;
begin
  Result := TelColor.Create(0, 191, 255);
end;

class function TelColor.clDimGray: TelColor;
begin
  Result := TelColor.Create(105, 105, 105);
end;

class function TelColor.clDimGrey: TelColor;
begin
  Result := TelColor.clDimGray;
end;

class function TelColor.clDodgerBlue: TelColor;
begin
  Result := TelColor.Create(30, 144, 255);
end;

class function TelColor.clFireBrick: TelColor;
begin
  Result := TelColor.Create(178, 34, 34);
end;

class function TelColor.clFloralWhite: TelColor;
begin
  Result := TelColor.Create(255, 250, 240);
end;

class function TelColor.clForestGreen: TelColor;
begin
  Result := TelColor.Create(34, 139, 34);
end;

class function TelColor.clGainsboro: TelColor;
begin
  Result := TelColor.Create(220, 220, 220);
end;

class function TelColor.clGhostWhite: TelColor;
begin
  Result := TelColor.Create(248, 248, 255);
end;

class function TelColor.clGold: TelColor;
begin
  Result := TelColor.Create(255, 215, 0);
end;

class function TelColor.clGoldenRod: TelColor;
begin
  Result := TelColor.Create(218, 165, 32);
end;

class function TelColor.clGreenYellow: TelColor;
begin
  Result := TelColor.Create(173, 255, 47);
end;

class function TelColor.clHoneyDew: TelColor;
begin
  Result := TelColor.Create(240, 255, 240);
end;

class function TelColor.clHotPink: TelColor;
begin
  Result := TelColor.Create(255, 105, 180);
end;

class function TelColor.clIndianRed: TelColor;
begin
  Result := TelColor.Create(205, 92, 92);
end;

class function TelColor.clIndigo: TelColor;
begin
  Result := TelColor.Create(75, 0, 130);
end;

class function TelColor.clIvory: TelColor;
begin
  Result := TelColor.Create(255, 255, 240);
end;

class function TelColor.clKhaki: TelColor;
begin
  Result := TelColor.Create(240, 230, 140);
end;

class function TelColor.clLavender: TelColor;
begin
  Result := TelColor.Create(230, 230, 250);
end;

class function TelColor.clLavenderBlush: TelColor;
begin
  Result := TelColor.Create(255, 240, 245);
end;

class function TelColor.clLawnGreen: TelColor;
begin
  Result := TelColor.Create(124, 252, 0);
end;

class function TelColor.clLemonChiffon: TelColor;
begin
  Result := TelColor.Create(255, 250, 205);
end;

class function TelColor.clLightBlue: TelColor;
begin
  Result := TelColor.Create(173, 216, 230);
end;

class function TelColor.clLightCoral: TelColor;
begin
  Result := TelColor.Create(240, 128, 128);
end;

class function TelColor.clLightCyan: TelColor;
begin
  Result := TelColor.Create(224, 255, 255);
end;

class function TelColor.clLightGoldenRodYellow: TelColor;
begin
  Result := TelColor.Create(250, 250, 210);
end;

class function TelColor.clLightGray: TelColor;
begin
  Result := TelColor.Create(211, 211, 211);
end;

class function TelColor.clLightGrey: TelColor;
begin
  Result := TelColor.clLightGray;
end;

class function TelColor.clLightGreen: TelColor;
begin
  Result := TelColor.Create(144, 238, 144);
end;

class function TelColor.clLightPink: TelColor;
begin
  Result := TelColor.Create(255, 182, 193);
end;

class function TelColor.clLightSalmon: TelColor;
begin
  Result := TelColor.Create(255, 160, 122);
end;

class function TelColor.clLightSeaGreen: TelColor;
begin
  Result := TelColor.Create(32, 178, 170);
end;

class function TelColor.clLightSkyBlue: TelColor;
begin
  Result := TelColor.Create(135, 206, 250);
end;

class function TelColor.clLightSlateGray: TelColor;
begin
  Result := TelColor.Create(119, 136, 153);
end;

class function TelColor.clLightSlateGrey: TelColor;
begin
  Result := TelColor.clLightSlateGray;
end;

class function TelColor.clLightSteelBlue: TelColor;
begin
  Result := TelColor.Create(176, 196, 222);
end;

class function TelColor.clLightYellow: TelColor;
begin
  Result := TelColor.Create(255, 255, 224);
end;

class function TelColor.clLimeGreen: TelColor;
begin
  Result := TelColor.Create(50, 205, 50);
end;

class function TelColor.clLinen: TelColor;
begin
  Result := TelColor.Create(250, 240, 230);
end;

class function TelColor.clMagenta: TelColor;
begin
  Result := TelColor.clFuchsia;
end;

class function TelColor.clMediumAquaMarine: TelColor;
begin
  Result := TelColor.Create(102, 205, 170);
end;

class function TelColor.clMediumBlue: TelColor;
begin
  Result := TelColor.Create(0, 0, 205);
end;

class function TelColor.clMediumOrchid: TelColor;
begin
  Result := TelColor.Create(186, 85, 211);
end;

class function TelColor.clMediumPurple: TelColor;
begin
  Result := TelColor.Create(147, 112, 219);
end;

class function TelColor.clMediumSeaGreen: TelColor;
begin
  Result := TelColor.Create(60, 179, 113);
end;

class function TelColor.clMediumSlateBlue: TelColor;
begin
  Result := TelColor.Create(123, 104, 238);
end;

class function TelColor.clMediumSpringGreen: TelColor;
begin
  Result := TelColor.Create(0, 250, 154);
end;

class function TelColor.clMediumTurquoise: TelColor;
begin
  Result := TelColor.Create(72, 209, 204);
end;

class function TelColor.clMediumVioletRed: TelColor;
begin
  Result := TelColor.Create(199, 21, 133);
end;

class function TelColor.clMidnightBlue: TelColor;
begin
  Result := TelColor.Create(25, 25, 112);
end;

class function TelColor.clMintCream: TelColor;
begin
  Result := TelColor.Create(245, 255, 250);
end;

class function TelColor.clMistyRose: TelColor;
begin
  Result := TelColor.Create(255, 228, 225);
end;

class function TelColor.clMoccasin: TelColor;
begin
  Result := TelColor.Create(255, 228, 181);
end;

class function TelColor.clNavajoWhite: TelColor;
begin
  Result := TelColor.Create(255, 222, 173);
end;

class function TelColor.clOldLace: TelColor;
begin
  Result := TelColor.Create(253, 245, 230);
end;

class function TelColor.clOliveDrab: TelColor;
begin
  Result := TelColor.Create(107, 142, 35);
end;

class function TelColor.clOrange: TelColor;
begin
  Result := TelColor.Create(255, 165, 0);
end;

class function TelColor.clOrangeRed: TelColor;
begin
  Result := TelColor.Create(255, 69, 0);
end;

class function TelColor.clOrchid: TelColor;
begin
  Result := TelColor.Create(218, 112, 214);
end;

class function TelColor.clPaleGoldenRod: TelColor;
begin
  Result := TelColor.Create(238, 232, 170);
end;

class function TelColor.clPaleGreen: TelColor;
begin
  Result := TelColor.Create(152, 251, 152);
end;

class function TelColor.clPaleTurquoise: TelColor;
begin
  Result := TelColor.Create(175, 238, 238);
end;

class function TelColor.clPaleVioletRed: TelColor;
begin
  Result := TelColor.Create(219, 112, 147);
end;

class function TelColor.clPapayaWhip: TelColor;
begin
  Result := TelColor.Create(255, 239, 213);
end;

class function TelColor.clPeachPuff: TelColor;
begin
  Result := TelColor.Create(255, 218, 185);
end;

class function TelColor.clPeru: TelColor;
begin
  Result := TelColor.Create(205, 133, 63);
end;

class function TelColor.clPink: TelColor;
begin
  Result := TelColor.Create(255, 192, 203);
end;

class function TelColor.clPlum: TelColor;
begin
  Result := TelColor.Create(221, 160, 221);
end;

class function TelColor.clPowderBlue: TelColor;
begin
  Result := TelColor.Create(176, 224, 230);
end;

class function TelColor.clRosyBrown: TelColor;
begin
  Result := TelColor.Create(188, 143, 143);
end;

class function TelColor.clRoyalBlue: TelColor;
begin
  Result := TelColor.Create(65, 105, 225);
end;

class function TelColor.clSaddleBrown: TelColor;
begin
  Result := TelColor.Create(139, 69, 19);
end;

class function TelColor.clSalmon: TelColor;
begin
  Result := TelColor.Create(250, 128, 114);
end;

class function TelColor.clSandyBrown: TelColor;
begin
  Result := TelColor.Create(244, 164, 96);
end;

class function TelColor.clSeaGreen: TelColor;
begin
  Result := TelColor.Create(46, 139, 87);
end;

class function TelColor.clSeaShell: TelColor;
begin
  Result := TelColor.Create(255, 245, 238);
end;

class function TelColor.clSienna: TelColor;
begin
  Result := TelColor.Create(160, 82, 45);
end;

class function TelColor.clSkyBlue: TelColor;
begin
  Result := TelColor.Create(135, 206, 235);
end;

class function TelColor.clSlateBlue: TelColor;
begin
  Result := TelColor.Create(106, 90, 205);
end;

class function TelColor.clSlateGray: TelColor;
begin
  Result := TelColor.Create(112, 128, 144);
end;

class function TelColor.clSlateGrey: TelColor;
begin
  Result := TelColor.clSlateGray;
end;

class function TelColor.clSnow: TelColor;
begin
  Result := TelColor.Create(255, 250, 250);
end;

class function TelColor.clSpringGreen: TelColor;
begin
  Result := TelColor.Create(0, 255, 127);
end;

class function TelColor.clSteelBlue: TelColor;
begin
  Result := TelColor.Create(70, 130, 180);
end;

class function TelColor.clTan: TelColor;
begin
  Result := TelColor.Create(210, 180, 140);
end;

class function TelColor.clThistle: TelColor;
begin
  Result := TelColor.Create(216, 191, 216);
end;

class function TelColor.clTomato: TelColor;
begin
  Result := TelColor.Create(255, 99, 71);
end;

class function TelColor.clTurquoise: TelColor;
begin
  Result := TelColor.Create(64, 224, 208);
end;

class function TelColor.clViolet: TelColor;
begin
  Result := TelColor.Create(238, 130, 238);
end;

class function TelColor.clWheat: TelColor;
begin
  Result := TelColor.Create(245, 222, 179);
end;

class function TelColor.clWhiteSmoke: TelColor;
begin
  Result := TelColor.Create(245, 245, 245);
end;

class function TelColor.clYellowGreen: TelColor;
begin
  Result := TelColor.Create(154, 205, 50);
end;

class function TelColor.clFreezeDevBlue: TelColor;
begin
  Result := TelColor.Create(73, 92, 108);
end;

class operator TelColor.Add(A, B: TelColor): TelColor;
begin
  Result.R := A.R + B.R;
  Result.G := A.G + B.G;
  Result.B := A.B + B.B;
  Result.A := A.A + B.A;
end;

class operator TelColor.Subtract(A, B: TelColor): TelColor;
begin
  Result.R := A.R - B.R;
  Result.G := A.G - B.G;
  Result.B := A.B - B.B;
  Result.A := A.A - B.A;
end;

class operator TelColor.Multiply(A, B: TelColor): TelColor;
begin
  Result.R := A.R * B.R;
  Result.G := A.G * B.G;
  Result.B := A.B * B.B;
  Result.A := A.A * B.A;
end;

class operator TelColor.Multiply(A: TelColor; Factor: Single): TelColor;
var
  tR, tG, tB, tA: Byte;
begin
  if (A.R * Factor) >= 255.0 then tR := 255
     else tR := Trunc(A.R * Factor);

  if (A.G * Factor) >= 255.0 then tG := 255
     else tG := Trunc(A.G * Factor);

  if (A.B * Factor) >= 255.0 then tB := 255
     else tB := Trunc(A.B * Factor);

  if (A.A * Factor) >= 255.0 then tA := 255
     else tA := Trunc(A.A * Factor);

  Result.R := tR;
  Result.G := tG;
  Result.B := tB;
  Result.A := tA;
end;

class operator TelColor.Divide(A, B: TelColor): TelColor;
begin
  Result.R := Trunc(A.R / B.R);
  Result.G := Trunc(A.G / B.G);
  Result.B := Trunc(A.B / B.B);
  Result.A := Trunc(A.A / B.A);
end;

class operator TelColor.IntDivide(A, B: TelColor): TelColor;
begin
  Result.R := A.R div B.R;
  Result.G := A.G div B.G;
  Result.B := A.B div B.B;
  Result.A := A.A div B.A;
end;

class operator TelColor.Modulus(A, B: TelColor): TelColor;
begin
  Result.R := A.R mod B.R;
  Result.G := A.G mod B.G;
  Result.B := A.B mod B.B;
  Result.A := A.A mod B.A;
end;

class operator TelColor.Equal(A, B: TelColor): Boolean;
begin
  Result := ((A.R = B.R) and (A.G = B.G) and (A.B = B.B) and (A.A = B.A));
end;

class operator TelColor.NotEqual(A, B: TelColor): Boolean;
begin
  Result := not ((A.R = B.R) and (A.G = B.G) and (A.B = B.B) and (A.A = B.A));
end;

class operator TelColor.GreaterThan(A, B: TelColor): Boolean;
begin
  Result := ((A.R > B.R) and (A.G > B.G) and (A.B > B.B) and (A.A > B.A));
end;

class operator TelColor.GreaterThanOrEqual(A, B: TelColor): Boolean;
begin
  Result := ((A.R >= B.R) and (A.G >= B.G) and (A.B >= B.B) and (A.A >= B.A));
end;

class operator TelColor.LessThan(A, B: TelColor): Boolean;
begin
  Result := ((A.R < B.R) and (A.G < B.G) and (A.B < B.B) and (A.A < B.A));
end;

class operator TelColor.LessThanOrEqual(A, B: TelColor): Boolean;
begin
  Result := ((A.R <= B.R) and (A.G <= B.G) and (A.B <= B.B) and (A.A <= B.A));
end;


{ TelGradient }

procedure TelGradient.Clear;
begin
  StartColor.Clear;
  EndColor.Clear;
end;

class function TelGradient.Copy(aSource: TelGradient): TelGradient;
begin
  Result.StartColor := aSource.StartColor;
  Result.EndColor := aSource.EndColor;
  Result.Style := aSource.Style;
end;

class function TelGradient.Create(aStartColor, anEndColor: TelColor;
  aStyle: TStyle = gsVertical): TelGradient;
begin
  Result.StartColor := aStartColor;
  Result.EndColor := anEndColor;
  Result.Style := aStyle;
end;

class function TelGradient.Create: TelGradient;
begin
  Result.StartColor := TelColor.Create;
  Result.EndColor := TelColor.Create;
  Result.Style := gsVertical;
end;

end.
