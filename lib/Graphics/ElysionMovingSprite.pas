unit ElysionMovingSprite;

{$I Elysion.inc}

interface

uses
  ElysionTypes,
  ElysionSprite;

type
  { TelMovingSprite }

  TelMovingSprite = class(TelSprite)
  private
    fPaused: Boolean;
  public
    procedure Update(dt: Double = 0.0); Override;
  public
    Velocity: TelVector2f;
  published
    property Paused: Boolean read fPaused write fPaused;
  end;

implementation

{ TelMovingSprite }

procedure TelMovingSprite.Update(dt: Double = 0.0);
begin
  inherited;

  if not Self.Paused then Self.Move(Velocity, dt);
end;

end.
