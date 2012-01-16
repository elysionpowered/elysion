unit ElysionBorder;

{$I Elysion.inc}

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  SysUtils,

  ElysionTypes,
  ElysionColor,
  ElysionBounds;

type

  { TelBorder }

  TelBorder = record
  public type

    TelBorderStyle = (bsSolid, bsDashed, bsDouble, bsDotted, bsGroove, bsRidge, bsInset, bsOutset);

    { TelBorderRadius }

    TelBorderRadius = record
    private
      fTopLeft, fTopRight, fBottomLeft, fBottomRight: Single;

      function GetValue: Single; inline;
      procedure SetValue(const AValue: Single); inline;

      function IsEmpty(): Boolean; inline;
    public
      procedure Make(aTopLeft, aTopRight, aBottomLeft, aBottomRight: Single); inline; Overload;

      procedure Clear(); inline;
    public
      class function Create: TelBorderRadius; static; inline; Overload;
      class function Create(aTopLeft, aTopRight, aBottomLeft, aBottomRight: Single): TelBorderRadius; static; inline; Overload;

      class function Copy(aSource: TelBorderRadius): TelBorderRadius; static; inline;
    public
      class operator Add(A, B: TelBorderRadius): TelBorderRadius; inline;
      class operator Subtract(A, B: TelBorderRadius): TelBorderRadius; inline;
      // Apparently, some bug 'ere; Can't be bothered with this at the moment
      //class operator Mulitply(A, B: TelBorderRadius): TelBorderRadius; inline;
      class operator Divide(A, B: TelBorderRadius): TelBorderRadius; inline;

      class operator Equal(A, B: TelBorderRadius): Boolean; inline;
      class operator NotEqual(A, B: TelBorderRadius): Boolean; inline;

      class operator Implicit(AValue: Single): TelBorderRadius; inline; Overload;
      class operator Implicit(AValue: Integer): TelBorderRadius; inline; Overload;
    public
      property Empty: Boolean read IsEmpty;

      property Value: Single read GetValue write SetValue;

      property TopLeft: Single read fTopLeft write fTopLeft;
      property TopRight: Single read fTopRight write fTopRight;
      property BottomLeft: Single read fBottomLeft write fBottomLeft;
      property BottomRight: Single read fBottomRight write fBottomRight;
    end;

    { TelBorderSide }

    TelBorderSide = record
    private
      fColor: TelColor;

      fWidth: Single;
      fStyle: TelBorderStyle;
    public
      class function Create: TelBorderSide; static; inline; Overload;
      class function Create(aColor: TelColor; aWidth: Single; aStyle: TelBorderStyle = bsSolid): TelBorderSide; static; inline; Overload;

      class function Copy(aSource: TelBorderSide): TelBorderSide; static; inline;
    public
      class operator Equal(A, B: TelBorderSide): Boolean; inline;
      class operator NotEqual(A, B: TelBorderSide): Boolean; inline;
    public
      property Color: TelColor read fColor write fColor;

      property Width: Single read fWidth write fWidth;
      property Style: TelBorderStyle read fStyle write fStyle;
    end;

  private
    fLeft, fTop, fRight, fBottom: TelBorderSide;
    fRadius: TelBorderRadius;

    function GetColor: TelColor; inline;
    procedure SetColor(const AValue: TelColor); inline;

    function GetWidth: Single; inline;
    procedure SetWidth(AValue: Single); inline;

    function GetStyle: TelBorderStyle; inline;
    procedure SetStyle(AValue: TelBorderStyle); inline;

    procedure SetValue(AValue: TelBorderSide); inline;

    function IsEmpty(): Boolean; inline;
  public
    class function Create: TelBorder; static; inline; Overload;
    class function Create(aLeft, aTop, aRight, aBottom: TelBorderSide; aRadius: TelBorderRadius): TelBorder; static; inline; Overload;

    class function Copy(aSource: TelBorder): TelBorder; static; inline;
  public
    property Color: TelColor read GetColor write SetColor;

    property Radius: TelBorderRadius read fRadius write fRadius;

    property Width: Single read GetWidth write SetWidth;
    property Style: TelBorderStyle read GetStyle write SetStyle;

    property Value: TelBorderSide write SetValue;

    property Left: TelBorderSide read fLeft write fLeft;
    property Top: TelBorderSide read fTop write fTop;
    property Right: TelBorderSide read fRight write fRight;
    property Bottom: TelBorderSide read fBottom write fBottom;
  end;

implementation

{ TelBorder.TelBorderRadius }

function TelBorder.TelBorderRadius.GetValue: Single;
begin
  Result := (TopLeft + TopRight + BottomLeft + BottomRight) / 4;
end;

procedure TelBorder.TelBorderRadius.SetValue(const AValue: Single);
begin
  TopLeft := AValue;
  TopRight := AValue;
  BottomLeft := AValue;
  BottomRight := AValue;
end;

function TelBorder.TelBorderRadius.IsEmpty: Boolean;
begin
  Result := ((TopLeft = 0) and (TopRight = 0) and (BottomLeft = 0)  and (BottomRight = 0));
end;

procedure TelBorder.TelBorderRadius.Make(aTopLeft, aTopRight, aBottomLeft,
  aBottomRight: Single);
begin
  TopLeft := aTopLeft;
  TopRight := aTopRight;
  BottomLeft := aBottomLeft;
  BottomRight := aBottomRight;
end;

procedure TelBorder.TelBorderRadius.Clear;
begin
  Value := 0;
end;

class function TelBorder.TelBorderRadius.Create: TelBorderRadius;
begin
  Result.Value := 0;
end;

class function TelBorder.TelBorderRadius.Create(aTopLeft, aTopRight,
  aBottomLeft, aBottomRight: Single): TelBorderRadius;
begin
  Result.TopLeft := aTopLeft;
  Result.TopRight := aTopRight;
  Result.BottomLeft := aBottomLeft;
  Result.BottomRight := aBottomRight;
end;

class function TelBorder.TelBorderRadius.Copy(aSource: TelBorderRadius): TelBorderRadius;
begin
  Result.TopLeft := aSource.TopLeft;
  Result.TopRight := aSource.TopRight;
  Result.BottomLeft := aSource.BottomLeft;
  Result.BottomRight := aSource.BottomRight;
end;

class operator TelBorder.TelBorderRadius.Add(A, B: TelBorderRadius
  ): TelBorderRadius;
begin
  Result.TopLeft := A.TopLeft + B.TopLeft;
  Result.TopRight := A.TopRight + B.TopRight;
  Result.BottomLeft := A.BottomLeft + B.BottomLeft;
  Result.BottomRight := A.BottomRight + B.BottomRight;
end;

class operator TelBorder.TelBorderRadius.Subtract(A, B: TelBorderRadius
  ): TelBorderRadius;
begin
  Result.TopLeft := A.TopLeft - B.TopLeft;
  Result.TopRight := A.TopRight - B.TopRight;
  Result.BottomLeft := A.BottomLeft - B.BottomLeft;
  Result.BottomRight := A.BottomRight - B.BottomRight;
end;

(*class operator TelBorder.TelBorderRadius.Mulitply(A, B: TelBorderRadius
  ): TelBorderRadius;
begin
  Result.TopLeft := A.TopLeft * B.TopLeft;
  Result.TopRight := A.TopRight * B.TopRight;
  Result.BottomLeft := A.BottomLeft * B.BottomLeft;
  Result.BottomRight := A.BottomRight * B.BottomRight;
end;*)

class operator TelBorder.TelBorderRadius.Divide(A, B: TelBorderRadius
  ): TelBorderRadius;
begin
  Result.TopLeft := A.TopLeft / B.TopLeft;
  Result.TopRight := A.TopRight / B.TopRight;
  Result.BottomLeft := A.BottomLeft / B.BottomLeft;
  Result.BottomRight := A.BottomRight / B.BottomRight;
end;

class operator TelBorder.TelBorderRadius.Equal(A, B: TelBorderRadius): Boolean;
begin
  Result := ((A.TopLeft = B.TopLeft) and (A.TopRight = B.TopRight) and (A.BottomLeft = B.BottomLeft) and (A.BottomRight = B.BottomRight));
end;

class operator TelBorder.TelBorderRadius.NotEqual(A, B: TelBorderRadius
  ): Boolean;
begin
  Result := not ((A.TopLeft = B.TopLeft) and (A.TopRight = B.TopRight) and (A.BottomLeft = B.BottomLeft) and (A.BottomRight = B.BottomRight));
end;

class operator TelBorder.TelBorderRadius.Implicit(AValue: Single
  ): TelBorderRadius;
begin
  Result.Value := AValue;
end;

class operator TelBorder.TelBorderRadius.Implicit(AValue: Integer
  ): TelBorderRadius;
begin
  Result.Value := AValue * 1.0;
end;


{ TelBorder.TelBorderSide }

class function TelBorder.TelBorderSide.Create: TelBorderSide;
begin
  Result.Width := 0;
  Result.Style := bsSolid;
  Result.Color := TelColor.Create;
end;

class function TelBorder.TelBorderSide.Create(aColor: TelColor; aWidth: Single;
  aStyle: TelBorderStyle): TelBorderSide;
begin
  Result.Width := aWidth;
  Result.Style := aStyle;
  Result.Color := aColor;
end;

class function TelBorder.TelBorderSide.Copy(aSource: TelBorderSide
  ): TelBorderSide;
begin
  Result.Width := aSource.Width;
  Result.Style := aSource.Style;
  Result.Color := aSource.Color;
end;

class operator TelBorder.TelBorderSide.Equal(A, B: TelBorderSide): Boolean;
begin
  Result := ((A.Color = B.Color) and (A.Width = B.Width) and (A.Style = B.Style));
end;

class operator TelBorder.TelBorderSide.NotEqual(A, B: TelBorderSide): Boolean;
begin
  Result := not ((A.Color = B.Color) and (A.Width = B.Width) and (A.Style = B.Style));
end;


{ TelBorder }

function TelBorder.GetColor: TelColor;
begin
  if ((fLeft.Color = fTop.Color) and (fTop.Color = fRight.Color) and (fRight.Color = fBottom.Color)) then
  begin
    Result := fLeft.Color;
  end else
  begin
    Result := TelColor.Create(Trunc(Integer(fLeft.Color.R + fTop.Color.R + fRight.Color.R + fBottom.Color.R) / 4),
                              Trunc(Integer(fLeft.Color.G + fTop.Color.G + fRight.Color.G + fBottom.Color.G) / 4),
                              Trunc(Integer(fLeft.Color.B + fTop.Color.B + fRight.Color.B + fBottom.Color.B) / 4),
                              Trunc(Integer(fLeft.Color.A + fTop.Color.A + fRight.Color.A + fBottom.Color.A) / 4));
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

class function TelBorder.Create: TelBorder;
begin
  Result.Left := TelBorderSide.Create;
  Result.Top := TelBorderSide.Create;
  Result.Right := TelBorderSide.Create;
  Result.Bottom := TelBorderSide.Create;

  Result.Radius := TelBorderRadius.Create;
end;

class function TelBorder.Create(aLeft, aTop, aRight, aBottom: TelBorderSide;
  aRadius: TelBorderRadius): TelBorder;
begin
  Result.Left := aLeft;
  Result.Top := aTop;
  Result.Right := aRight;
  Result.Bottom := aBottom;

  Result.Radius := aRadius;
end;

class function TelBorder.Copy(aSource: TelBorder): TelBorder;
begin
  Result.Left := aSource.Left;
  Result.Top := aSource.Top;
  Result.Right := aSource.Right;
  Result.Bottom := aSource.Bottom;

  Result.Radius := aSource.Radius;
end;

procedure TelBorder.SetValue(AValue: TelBorderSide);
begin
  fLeft := AValue;
  fTop := AValue;
  fRight := AValue;
  fBottom := AValue;
end;

function TelBorder.IsEmpty(): Boolean;
begin
  Result := ((fLeft.Width = 0) or (fTop.Width = 0) or (fRight.Width = 0) or (fBottom.Width = 0));
end;

end.
