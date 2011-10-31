unit ElysionParticles;

interface

{$I Elysion.inc}

type

TelParticle = class(TelSprite)
  protected
    fTimer: TelTimer;
    fVelocity: Single;

    fStartPoint, fEndPoint: TelVector3f;

    fDead: Boolean;
    fLife, fMaxLife: Integer;

    procedure SetLife(Value: Integer);
  public
    constructor Create; Override;
    destructor Destroy; Override;

    procedure Draw; Override;
    procedure Update(dt: Double = 0.0); Override;

    procedure Start(); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure Pause(); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure Stop(); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    property StartPoint: TelVector3f read fStartPoint write fStartPoint;
    property EndPoint: TelVector3f read fEndPoint write fEndPoint;

    property Velocity: Single read fVelocity write fVelocity;
  published
    property Dead: Boolean read fDead write fDead;

    property Life: Integer read fLife write SetLife;

end;

TelParticleEmitter = class
  protected
    fLoop, fIsActive, fIsPaused: Boolean;
  public

  published
    property Loop: Boolean read fLoop write fLoop;

    property IsActive: Boolean read fIsActive;
    property IsPaused: Boolean read fIsPaused;
end;


implementation

constructor TelParticle.Create;
begin
  inherited;

  fDead := false;
  //fLoop := false;
  //fIsPaused := false;
  //fIsActive := true;

  fMaxLife := 1500;
  fLife := fMaxLife;

  fTimer := TelTimer.Create;
end;

destructor TelParticle.Destroy;
begin
  inherited;
end;

procedure TelParticle.SetLife(Value: Integer);
begin
  fMaxLife := Value;
  fLife := Value;
end;

procedure TelParticle.Start();
begin
  fTimer.Start();
end;

procedure TelParticle.Pause();
begin
  fTimer.Pause();
end;

procedure TelParticle.Stop();
begin
  fTimer.Stop();
end;

procedure TelParticle.Draw();
begin
  inherited;
end;

procedure TelParticle.Update(dt: Double = 0.0);
begin
  inherited;
end;

end.
