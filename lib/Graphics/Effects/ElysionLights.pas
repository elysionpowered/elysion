unit ElysionLights;

interface

{$I Elysion.inc}

type

TelLight = class(TelSprite)
  public
    constructor Create; Override;
    destructor Destroy; Override;
end;


implementation

{
  #############################################################################
  # TelLight                                                                  #
  #############################################################################

  Description:
    TelLight is essentially a sprite with additative blending

  Additional Notes: -

}

constructor TelLight.Create;
begin
  inherited;

  Self.BlendMode := bmAdd;
  Self.Transparent := true;
end;

destructor TelLight.Destroy;
begin
  inherited;
end;

end.
