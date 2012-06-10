unit ElysionGauge;

interface

type
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

implementation

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

end.
