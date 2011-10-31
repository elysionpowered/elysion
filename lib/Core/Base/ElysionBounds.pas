unit ElysionBounds;

interface

type

  { TelBounds }

  TelBounds = class
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

implementation

{ TelBounds }

function TelBounds.GetValue: Single;
begin
  Result := ((Left + Top + Right + Bottom) / 4);
end;

procedure TelBounds.SetValue(const AValue: Single);
begin
  Left := AValue;
  Top := AValue;
  Right := AValue;
  Bottom := AValue;
end;

constructor TelBounds.Create;
begin
  fLeft := 0;
  fTop := 0;
  fRight := 0;
  fBottom := 0;
end;

destructor TelBounds.Destroy;
begin
  inherited Destroy;
end;

function TelBounds.IsEmpty(): Boolean;
begin
  Result := (Value = 0);
end;

end.
