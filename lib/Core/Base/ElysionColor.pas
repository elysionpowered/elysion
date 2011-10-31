unit ElysionColor;

interface

{$I Elysion.inc}

uses
  ElysionTypes;

type
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

{$IFDEF AUTO_INIT}
var
  Color: TelColorHelper;
{$ENDIF}

implementation

constructor TelColorHelper.Create;
begin
  inherited Create;

  FBufferEmpty := true;
  fLastUsedColor := makeCol(0, 0, 0, 255);
end;

destructor TelColorHelper.Destroy;
begin
  ClearBuffer;

  inherited Destroy;
end;

function TelColorHelper.clBlack: TelColor; 
begin
  FBufferEmpty := false;
  fLastUsedColor := makeCol(0, 0, 0, 255);
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
  fLastUsedColor := makeCol(73, 92, 108, 255);
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
