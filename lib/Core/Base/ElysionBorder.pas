unit ElysionBorder;

interface

uses
  ElysionTypes;

type

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

implementation

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
  Color := makeCol(0, 0, 0, 255);
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
    Result := makeCol(Trunc(Integer(fLeft.Color.R + fTop.Color.R + fRight.Color.R + fBottom.Color.R) / 4),
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

end.
