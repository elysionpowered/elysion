unit ElysionHash;

{$I Elysion.inc}
{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  MD5;

type

  // MD5 Hash implementation (works only on FreePascal)

  TelMD5HashCode = record
  private
    fValue: TMD5Digest;
  public
    class function Create(): TelMD5HashCode; static; inline; Overload;
    class function Create(aString: AnsiString): TelMD5HashCode; static; inline; Overload;
  public
    class operator Equal(A, B: TelMD5HashCode): Boolean; inline;
    class operator NotEqual(A, B: TelMD5HashCode): Boolean; inline;
  public
    procedure Generate(aString: AnsiString);

    function ToString(): AnsiString;
  public
    property Value: TMD5Digest read fValue;
  end;

  // Standard hash code type definition
  TelHash = TelMD5HashCode;

implementation

{ TelMD5HashCode }

class function TelMD5HashCode.Create: TelMD5HashCode;
begin
  // Just for completeness
end;

class function TelMD5HashCode.Create(aString: AnsiString): TelMD5HashCode;
begin
  Result.Generate(aString);
end;

class operator TelMD5HashCode.Equal(A, B: TelMD5HashCode): Boolean;
begin
  Result := MD5Match(A.Value, B.Value);
end;

class operator TelMD5HashCode.NotEqual(A, B: TelMD5HashCode): Boolean;
begin
  Result := not MD5Match(A.Value, B.Value);
end;

procedure TelMD5HashCode.Generate(aString: AnsiString);
begin
  fValue := MD5String(aString);
end;

function TelMD5HashCode.ToString: AnsiString;
begin
  Result := MD5Print(Self.Value);
end;

end.
