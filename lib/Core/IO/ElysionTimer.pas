{
  ElysionTimer



  @author(Johannes Stein and contributors <http://elysionpowered.org>)
}
unit ElysionTimer;

interface

uses
  ElysionTypes,
  ElysionEvents,
  ElysionObject,
  ElysionInterfaces,
  SDL;

type
  {
  #############################################################################
  # TelTimer                                                                  #
  #############################################################################
  # Function prototypes                                                       #

  Description:
    Provides basic timing functions

  Additional Notes: Inspired by Lazy Foo Productions
                    -> http://lazyfoo.net/SDL_tutorials/lesson13/index.php

}
TelTimer = class(TelObject, ITimer)
  private
    FActive, FPaused: Boolean;
    FInterval: Integer;

    FStartTicks, FStoredTicks, FOldTicks: Integer;
    fOnEvent: TelEvent;

    function GetEvent(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  public

    // Constructor
    constructor Create;overload;override;

    // Constructor with interval
    constructor Create(Interval: Integer);overload;

    // Destructor
    destructor Destroy; Override;

    // Start timer -> you need to call that manually after creation
    procedure Start(); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    // Stop timer
    procedure Stop(); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    // Pause & unpause
    procedure Pause(); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure UnPause(); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function GetTicks(): Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    procedure Update(dt: Double = 0.0); {$IFDEF CAN_INLINE} inline; {$ENDIF}
  published
    property Active: Boolean read FActive;

    property OnEvent: TelEvent read fOnEvent write fOnEvent;

    property Interval: Integer read FInterval write FInterval;

    // If Interval is set, use this to provoke an action
    // Works great if you're checking action every second or higher
    // If you're using it for < 100 ms intervals, it might not be precise enough
    // In that case use the GetTicks() - function, SDL_AddTimer or delta time (Window.DeltaTime)
    // in the update method
    property Event: Boolean read GetEvent;

    property Paused: Boolean read FPaused;
end;

implementation

//
// TelTimer
//
constructor TelTimer.Create;
begin
  inherited Create;

  FActive := false;
  FPaused := false;

  FStartTicks := 0;
  FStoredTicks := 0;
  FOldTicks := 0;

  FInterval := 0;

  OnEvent := nil;
end;

constructor TelTimer.Create(Interval: Integer);
begin
  inherited Create;

  FActive := false;
  FPaused := false;

  FStartTicks := 0;
  FStoredTicks := 0;
  FOldTicks := 0;

  FInterval := Interval;
end;

destructor TelTimer.Destroy;
begin
  inherited Destroy;
end;

procedure TelTimer.Start;
begin
  FActive := true;
  FPaused := false;

  FStartTicks := SDL_GetTicks();
  FOldTicks := 0;
end;

procedure TelTimer.Stop;
begin
  FActive := false;
  FPaused := false;
end;

procedure TelTimer.Pause;
begin
  if (Active) and (not Paused) then
  begin
    FPaused := true;

    FStoredTicks := SDL_GetTicks() - FStartTicks;
  end;
end;

procedure TelTimer.UnPause;
begin
  if Paused then
  begin
    FPaused := false;

    //Reset the starting ticks
    FStartTicks := SDL_GetTicks() - FStoredTicks;

    //Reset the paused ticks
    FStoredTicks := 0;
  end;
end;

function TelTimer.GetTicks(): Integer;
begin
  Result := 0;

  if Active then
  begin
    if Paused then Result := FStoredTicks
              else Result := SDL_GetTicks() - FStartTicks;
  end;
end;

procedure TelTimer.Update(dt: Double = 0.0);
begin
  if Self.Event then OnEvent();
end;

function TelTimer.GetEvent: Boolean;
begin
  Result := false;

  if (Active) and (not Paused) then
  begin
    if (GetTicks > 0) and (Interval > 0) then
    begin
      if (GetTicks - Interval) >= FOldTicks then
      begin
        Result := true;
        FOldTicks := GetTicks;
      end;
    end;
  end;
end;

end.
