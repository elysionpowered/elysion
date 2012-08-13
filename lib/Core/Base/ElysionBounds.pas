unit ElysionBounds;

{$I Elysion.inc}

{$mode delphi}

interface

uses
  SysUtils,

  ElysionHash,
  ElysionMath,
  ElysionTypes;

type

  { TelBounds }

  TelBounds = record
  private
    fLeft, fTop, fRight, fBottom: Single;
    fHash: TelHash;

    function GetValue: Single; inline;
    procedure SetValue(const AValue: Single); inline;

    function GetHeight: Single; inline;
    function GetWidth: Single; inline;

    function IsEmpty: Boolean; inline;

    function GetHash: TelHash; inline;
  public
    procedure Clear; inline;

    function ToString: AnsiString; inline;
    function ToRect: TelRect; inline;
  public
    class operator Implicit(AValue: Single): TelBounds; inline;
  public
    class function Create: TelBounds; static; inline; Overload;
    class function Create(aLeft, aTop, aRight, aBottom: Single): TelBounds; static; inline; Overload;
    class function Create(aRect: TelRect): TelBounds; static; inline; Overload;

    class function Copy(aSource: TelBounds): TelBounds; static; inline;

    class function Lerp(A, B: TelBounds; Amt: Single = 0.5): TelBounds; static; inline;
  public
    property Empty: Boolean read IsEmpty;

    property Value: Single read GetValue write SetValue;

    property Left: Single read fLeft write fLeft;
    property Top: Single read fTop write fTop;
    property Right: Single read fRight write fRight;
    property Bottom: Single read fBottom write fBottom;

    property Width: Single read GetWidth;
    property Height: Single read GetHeight;

    property Hash: TelHash read GetHash;
  end;

implementation

{ TelBounds }

function TelBounds.GetHeight: Single;
begin
  Result := Bottom - Top;
end;

function TelBounds.GetWidth: Single;
begin
  Result := Right - Left;
end;

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

function TelBounds.IsEmpty: Boolean;
begin
  Result := (Value = 0);
end;

function TelBounds.GetHash: TelHash;
begin
  fHash.Generate(Self.ToString());

  Result := fHash;
end;

procedure TelBounds.Clear;
begin
  fLeft := 0;
  fTop := 0;
  fRight := 0;
  fBottom := 0;
end;

function TelBounds.ToString: AnsiString;
begin
  Result := Format('bounds(%f, %f, %f, %f)', [Left, Top, Right, Bottom]);
end;

function TelBounds.ToRect: TelRect;
begin
  Result := TelRect.Create(Left, Top, Width, Height);
end;

class operator TelBounds.Implicit(AValue: Single): TelBounds;
begin
  Result.Value := AValue;
end;

class function TelBounds.Create: TelBounds;
begin
  Result.Clear;
end;

class function TelBounds.Create(aLeft, aTop, aRight, aBottom: Single
  ): TelBounds;
begin
  Result.Left := aLeft;
  Result.Top := aTop;
  Result.Right := aRight;
  Result.Bottom := aBottom;
end;

class function TelBounds.Create(aRect: TelRect): TelBounds;
begin
  Result.Left := aRect.X;
  Result.Top := aRect.Y;
  Result.Right := aRect.W + aRect.X;
  Result.Bottom := aRect.H + aRect.Y;
end;

class function TelBounds.Copy(aSource: TelBounds): TelBounds;
begin
  Result.Left := aSource.Left;
  Result.Top := aSource.Top;
  Result.Right := aSource.Right;
  Result.Bottom := aSource.Bottom;
end;

class function TelBounds.Lerp(A, B: TelBounds; Amt: Single): TelBounds;
begin
  Result := TelBounds.Create(ElysionMath.Lerp(A.Left, B.Left, Amt),
                             ElysionMath.Lerp(A.Top, B.Top, Amt),
                             ElysionMath.Lerp(A.Right, B.Right, Amt),
                             ElysionMath.Lerp(A.Bottom, B.Bottom, Amt));
end;

end.
