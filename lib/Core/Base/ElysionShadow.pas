unit ElysionShadow;

{$I Elysion.inc}

interface

uses
  SysUtils,

  ElysionTypes,
  ElysionHash,
  ElysionColor,
  ElysionBounds;

type

  { TelShadow }

  TelShadow = record
  private
    fBlur: Integer;
    fColor: TelColor;
    fBounds: TelBounds;
    fVisible: Boolean;
    fHash: TelHash;

    function GetHash(): TelHash; inline;
  public
    procedure Clear(); inline;

    function ToString(): AnsiString; inline;
  public
    class function Create(): TelShadow; static; inline; Overload;
    class function Create(aPosition: TelVector2f): TelShadow; static; inline; Overload;
    class function Create(aPosition: TelVector2f; aBlur: Integer; aColor: TelColor): TelShadow; static; inline; Overload;
    class function Create(aBounds: TelBounds; aBlur: Integer; aColor: TelColor): TelShadow; static; inline; Overload;

    class function Copy(aSource: TelShadow): TelShadow; static; inline;
  public
    property Blur: Integer read fBlur write fBlur;
    property Color: TelColor read fColor write fColor;
    property Bounds: TelBounds read fBounds write fBounds;
    property Visible: Boolean read fVisible write fVisible;

    property Hash: TelHash read GetHash;
  end;

implementation

{ TelShadow }

function TelShadow.GetHash: TelHash;
begin
  fHash.Generate(Self.ToString());

  Result := fHash;
end;

procedure TelShadow.Clear();
begin
  Self.Blur := 0;
  Self.Color := TelColor.Create;
  Self.Bounds := TelBounds.Create;
  Self.Visible := false;
end;

function TelShadow.ToString: AnsiString;
begin
  Result := Format('shadow(blur: %d, color: %s, bounds: %s)', [Self.Blur, Self.Color.ToString(), Self.Bounds.ToString()]);
end;

class function TelShadow.Create: TelShadow;
begin
  Result.Clear();
end;

class function TelShadow.Create(aPosition: TelVector2f): TelShadow;
begin
  Result.Blur := 0;
  Result.Color := TelColor.Create;
  Result.Bounds := TelBounds.Create(aPosition.X, aPosition.Y, 0, 0);
  Result.Visible := true;
end;

class function TelShadow.Create(aPosition: TelVector2f; aBlur: Integer;
  aColor: TelColor): TelShadow;
begin
  Result.Blur := aBlur;
  Result.Color := aColor;
  Result.Bounds := TelBounds.Create(aPosition.X, aPosition.Y, 0, 0);
  Result.Visible := true;
end;

class function TelShadow.Create(aBounds: TelBounds; aBlur: Integer;
  aColor: TelColor): TelShadow;
begin
  Result.Blur := aBlur;
  Result.Color := aColor;
  Result.Bounds := aBounds;
  Result.Visible := true;
end;

class function TelShadow.Copy(aSource: TelShadow): TelShadow;
begin
  Result.Blur := aSource.Blur;
  Result.Color := aSource.Color;
  Result.Bounds := aSource.Bounds;
  Result.Visible := aSource.Visible;
end;

end.
