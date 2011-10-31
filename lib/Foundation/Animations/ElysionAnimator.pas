{%region '--- Unit description ---'}
(**
  *
  * ElysionAnimator
  *   Provides a number of possible animations for nodes
  *
  *
  * (C) Johannes Stein, 2005 - 2011
  *   For more information about the license take a look at LICENSE.TXT
  *
  * http://elysion.freeze-dev.com
  *
  *)
{%endregion}

unit ElysionAnimator;

interface

{$I Elysion.inc}

uses
  ElysionObject,
  ElysionTypes,
  ElysionAnimTypes,
  ElysionTimer,
  ElysionNode,
  ElysionMath,

  Classes;

type
  // lmDefault = Use forward or inverse animation as specified
  // lmPulsating = Use forward or inverse animation first, then the other animation
  TelLoopMode = (lmDefault, lmPulsating);

  (**
   * Class: TelAnimator @br
   * Group: Optional @br
   * Description:
   *   Provides animators for nodes such as moving from one point to another
   *)
  TelAnimator = class(TelObject)
    private
      function Step(): Single; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    protected
      fTimer: TelTimer;
      fTarget: TelNode;
      fTransition: TelAnimationTransition;
      fFinished, fInverse: Boolean;

      fDelay, fMaxLoopCount, fCurrentLoop: Integer;
      fTolerance: Single;
      fLoopMode: TelLoopMode;

      function GetActive(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

      function GetDuration(): Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure SetDuration(aValue: Integer); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      function GetDelay(): Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure SetDelay(aValue: Integer); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      procedure StopEvent(); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      function GetFinished(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

      function GetPaused(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    public
      AnimProperty: TelAnimationProperty;

      constructor Create; Override; Overload;
      constructor Create(aTarget: TelNode); Overload;
      destructor Destroy; Override;

      procedure FadeEffect(aStartValue, anEndValue: Byte; aDuration: Integer = 1000; aTransition: TelAnimationTransition = atLinear);
      procedure MoveEffect(aStartValue, anEndValue: TelVector3f; aDuration: Integer = 1000; aTransition: TelAnimationTransition = atLinear); Overload;
      procedure MoveEffect(aStartValue: TelVector3f; anAngle, aDistance: Single; aDuration: Integer = 1000; aTransition: TelAnimationTransition = atLinear); Overload;
      procedure OriginEffect(aStartValue, anEndValue: TelVector2f; aDuration: Integer = 1000; aTransition: TelAnimationTransition = atLinear);
      procedure RotationEffect(aStartValue, anEndValue: TelImageRotation; aDuration: Integer = 1000; aTransition: TelAnimationTransition = atLinear); Overload;
      procedure RotationEffect(aStartValue, anEndValue: Single; aDuration: Integer = 1000; aTransition: TelAnimationTransition = atLinear); Overload;
      procedure ColorEffect(aStartValue, anEndValue: TelColor; aDuration: Integer = 1000; aTransition: TelAnimationTransition = atLinear);
      procedure ScaleEffect(aStartValue, anEndValue: TelVector2f; aDuration: Integer = 1000; aTransition: TelAnimationTransition = atLinear);

      procedure FadeInEffect(aDuration: Integer = 1000; aTransition: TelAnimationTransition = atLinear); {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure FadeOutEffect(aDuration: Integer = 1000; aTransition: TelAnimationTransition = atLinear); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      procedure Start();
      procedure Pause(); {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure UnPause(); {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure Stop(); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      procedure Reset(); {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure Resume(); {$IFDEF CAN_INLINE} inline; {$ENDIF} //< Maybe find a better name for this

      procedure Update(dt: Double = 0.0);
    published
      property Active: Boolean read GetActive;

      property Delay: Integer read GetDelay write SetDelay;
      property Duration: Integer read GetDuration write SetDuration;

      property Finished: Boolean read GetFinished;

      property Inverse: Boolean read fInverse write fInverse;

      property CurrentLoop: Integer read fCurrentLoop;


      property LoopCount: Integer read fMaxLoopCount write fMaxLoopCount;
      property LoopMode: TelLoopMode read fLoopMode write fLoopMode;

      //property KeyFrame[Index: Integer]

      property Paused: Boolean read GetPaused;

      property Target: TelNode read fTarget write fTarget;
      property Transition: TelAnimationTransition read fTransition write fTransition;
      property Tolerance: Single read fTolerance write fTolerance;
  end;

  (**
   * Class: TelAnimatorCombo @br
   * Group: Optional @br
   * Description:
   *   Provides a list of animators
   *)
  TelAnimatorCombo = class(TelObject)
     private
      fActionList: TList;
      fSequential: Boolean;
      fOldDelay: array of Integer;
      fLoopCount: Integer;

      function Get(Index: Integer): TelAnimator;
      function GetPos(Index: String): Integer;
      procedure Put(Index: Integer; const Item: TelAnimator);
      procedure PutS(Index: String; const Item: TelAnimator);
      function GetS(Index: String): TelAnimator;

      function GetCount: Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure SetSequential(Value: Boolean);

      function GetFinished(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

      procedure SetLoopCount(Value: Integer); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    public
      constructor Create; Override;
      destructor Destroy; Override;

      procedure Insert(Index: Integer; Action: TelAnimator);
      function Add(Action: TelAnimator): Integer;
      procedure Delete(Index: Integer);

      procedure Start(); {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure Pause(); {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure Stop(); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      procedure Update(dt: Double = 0.0); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      property Items[Index: Integer]: TelAnimator read Get write Put; default;
      property Find[Index: String]: TelAnimator read GetS write PutS;
    published
      property Count: Integer read GetCount;

      property Finished: Boolean read GetFinished;

      property LoopCount: Integer read fLoopCount write fLoopCount;

      property Sequential: Boolean read fSequential write SetSequential;
  end;

  // Prefabs


implementation

constructor TelAnimator.Create;
begin
  inherited Create;

  Transition := atLinear;

  fTimer := TelTimer.Create;

  Duration := 1000;
  LoopCount := 1;
  LoopMode := lmDefault;
  fCurrentLoop := 0;
  Delay := 0;

  fTolerance := 0.05;

  fFinished := false;
end;

constructor TelAnimator.Create(aTarget: TelNode);
begin
  Create();

  // TODO: Check if aTarget is instance of TelNode
  if (aTarget = nil) then
    Self.Log('Animator ' + Self.UniqueID + ' needs viable node object.')
  else fTarget := aTarget;
end;

destructor TelAnimator.Destroy;
begin
  fTimer.Destroy;

  inherited;
end;

function TelAnimator.Step(): Single;
begin
  Result := (fTimer.GetTicks() - Delay) / Duration
end;

function TelAnimator.GetActive(): Boolean;
begin
  Result := fTimer.Active;
end;

function TelAnimator.GetDuration(): Integer;
begin
  Result := fTimer.Interval;
end;

procedure TelAnimator.SetDuration(aValue: Integer);
begin
  fTimer.Interval := aValue;
end;

function TelAnimator.GetDelay(): Integer;
begin
  Result := fDelay;
end;

procedure TelAnimator.SetDelay(aValue: Integer);
begin
  fDelay := aValue;
end;

function TelAnimator.GetFinished(): Boolean;
begin
  Result := fFinished;
end;

function TelAnimator.GetPaused(): Boolean;
begin
  Result := fTimer.Paused;
end;

procedure TelAnimator.StopEvent();
begin
  Self.Stop();
  if fCurrentLoop = fMaxLoopCount then
  begin
    fCurrentLoop := 0;
    fFinished := true;
  end else
    Self.Start();
end;

procedure TelAnimator.Start();
begin
  if (not fTimer.Active) then
  begin

    if fTarget <> nil then
    begin
      case AnimProperty.AnimType of
        atAlpha: fTarget.Alpha := AnimProperty.StartAlpha;
        atPosition: fTarget.Position := AnimProperty.StartPosition;
        atOrigin: fTarget.Origin := AnimProperty.StartOrigin;
        atRotation: fTarget.Rotation := AnimProperty.StartRotation;
        atColor: fTarget.Color := AnimProperty.StartColor;
        atScale: fTarget.Scale := AnimProperty.StartScale;
      end;
    end;

    fCurrentLoop := fCurrentLoop + 1;

    fFinished := false;

    fTimer.Start();
  end;
end;

procedure TelAnimator.Pause();
begin
  fTimer.Pause();
end;

procedure TelAnimator.UnPause();
begin
  fTimer.UnPause();
end;

procedure TelAnimator.Stop();
begin
  fTimer.Stop();
end;

procedure TelAnimator.Reset();
begin
  case AnimProperty.AnimType of
    atAlpha: Target.Alpha := AnimProperty.StartAlpha;
    atPosition: Target.Position := AnimProperty.StartPosition;
    atOrigin: Target.Origin := AnimProperty.StartOrigin;
    atRotation: Target.Rotation := AnimProperty.StartRotation;
    atColor: Target.Color := AnimProperty.StartColor;
    atScale: Target.Scale := AnimProperty.StartScale;
  end;
end;

procedure TelAnimator.Resume();
begin
  case AnimProperty.AnimType of
    atAlpha: Target.Alpha := AnimProperty.EndAlpha;
    atPosition: Target.Position := AnimProperty.EndPosition;
    atOrigin: Target.Origin := AnimProperty.EndOrigin;
    atRotation: Target.Rotation := AnimProperty.EndRotation;
    atColor: Target.Color := AnimProperty.EndColor;
    atScale: Target.Scale := AnimProperty.EndScale;
  end;
end;

procedure TelAnimator.FadeEffect(aStartValue, anEndValue: Byte; aDuration: Integer = 1000; aTransition: TelAnimationTransition = atLinear);
begin
  if Duration <> aDuration then Duration := aDuration;
  if Transition <> aTransition then Transition := aTransition;

  with AnimProperty do
  begin
    AnimType := atAlpha;
    StartAlpha := aStartValue;
    EndAlpha := anEndValue;
  end;
end;

procedure TelAnimator.MoveEffect(aStartValue, anEndValue: TelVector3f; aDuration: Integer = 1000; aTransition: TelAnimationTransition = atLinear);
begin
  if Duration <> aDuration then Duration := aDuration;
  if Transition <> aTransition then Transition := aTransition;

  with AnimProperty do
  begin
    AnimType := atPosition;
    StartPosition := aStartValue;
    EndPosition := anEndValue;
  end;
end;

procedure TelAnimator.MoveEffect(aStartValue: TelVector3f; anAngle, aDistance: Single; aDuration: Integer = 1000; aTransition: TelAnimationTransition = atLinear);
begin
  if Duration <> aDuration then Duration := aDuration;
  if Transition <> aTransition then Transition := aTransition;

  with AnimProperty do
  begin
    AnimType := atPosition;

    StartPosition := aStartValue;
    EndPosition := makeV3f(aStartValue.X + aDistance * cos(anAngle),
                           aStartValue.Y + aDistance * sin(anAngle), aStartValue.Z);
  end;
end;

procedure TelAnimator.OriginEffect(aStartValue, anEndValue: TelVector2f; aDuration: Integer = 1000; aTransition: TelAnimationTransition = atLinear);
begin
  if Duration <> aDuration then Duration := aDuration;
  if Transition <> aTransition then Transition := aTransition;

  with AnimProperty do
  begin
    AnimType := atOrigin;
    StartOrigin := aStartValue;
    EndOrigin := anEndValue;
  end;
end;

procedure TelAnimator.RotationEffect(aStartValue, anEndValue: TelImageRotation; aDuration: Integer = 1000; aTransition: TelAnimationTransition = atLinear);
begin
  if Duration <> aDuration then Duration := aDuration;
  if Transition <> aTransition then Transition := aTransition;

  with AnimProperty do
  begin
    AnimType := atRotation;
    StartRotation := aStartValue;
    EndRotation := anEndValue;
  end;
end;

procedure TelAnimator.RotationEffect(aStartValue, anEndValue: Single; aDuration: Integer = 1000; aTransition: TelAnimationTransition = atLinear);
begin
  if Duration <> aDuration then Duration := aDuration;
  if Transition <> aTransition then Transition := aTransition;

  with AnimProperty do
  begin
    AnimType := atRotation;
    StartRotation.Vector := Target.Rotation.Vector;
    EndRotation.Vector := Target.Rotation.Vector;

    StartRotation.Angle := aStartValue;
    EndRotation.Angle := anEndValue;
  end;
end;

procedure TelAnimator.ColorEffect(aStartValue, anEndValue: TelColor; aDuration: Integer = 1000; aTransition: TelAnimationTransition = atLinear);
begin
  if Duration <> aDuration then Duration := aDuration;
  if Transition <> aTransition then Transition := aTransition;

  with AnimProperty do
  begin
    AnimType := atColor;
    StartColor := aStartValue;
    EndColor := anEndValue;
  end;
end;

procedure TelAnimator.ScaleEffect(aStartValue, anEndValue: TelVector2f; aDuration: Integer = 1000; aTransition: TelAnimationTransition = atLinear);
begin
  if Duration <> aDuration then Duration := aDuration;
  if Transition <> aTransition then Transition := aTransition;

  with AnimProperty do
  begin
    AnimType := atScale;
    StartScale := aStartValue;
    EndScale := anEndValue;
  end;
end;

procedure TelAnimator.FadeInEffect(aDuration: Integer = 1000; aTransition: TelAnimationTransition = atLinear);
begin
  FadeEffect(0, 255, aDuration, aTransition);
end;

procedure TelAnimator.FadeOutEffect(aDuration: Integer = 1000; aTransition: TelAnimationTransition = atLinear);
begin
  FadeEffect(255, 0, aDuration, aTransition);
end;

procedure TelAnimator.Update(dt: Double = 0.0);

  {$IFDEF FPC}
  {$Note Add tolerance for animations}
  {$ENDIF}

  procedure AnimAlpha(dt: Double = 0.0);
  begin
    if AnimProperty.StartAlpha <> AnimProperty.EndAlpha then
    begin
      case Transition of
        atLinear:
        begin
          if AnimProperty.StartAlpha > AnimProperty.EndAlpha then
            fTarget.Alpha := Trunc(InverseLerp(AnimProperty.EndAlpha, AnimProperty.StartAlpha, Step))
          else
            fTarget.Alpha := Trunc(Lerp(AnimProperty.StartAlpha, AnimProperty.EndAlpha, Step));
        end;
      end;

    end;
  end;

  procedure AnimPosition(dt: Double = 0.0);
  begin
    if not VectorEquals(AnimProperty.StartPosition, AnimProperty.EndPosition) then
    begin
      case Transition of
        atLinear:
        begin
          if AnimProperty.StartPosition.X <> AnimProperty.EndPosition.X then
          begin
            if AnimProperty.StartPosition.X > AnimProperty.EndPosition.X then
              fTarget.Position.X := InverseLerp(AnimProperty.EndPosition.X, AnimProperty.StartPosition.X, Step)
            else
              fTarget.Position.X := Lerp(AnimProperty.StartPosition.X, AnimProperty.EndPosition.X, Step);
          end;

          if AnimProperty.StartPosition.Y <> AnimProperty.EndPosition.Y then
          begin
            if AnimProperty.StartPosition.Y > AnimProperty.EndPosition.Y then
              fTarget.Position.Y := InverseLerp(AnimProperty.EndPosition.Y, AnimProperty.StartPosition.Y, Step)
            else
              fTarget.Position.Y := Lerp(AnimProperty.StartPosition.Y, AnimProperty.EndPosition.Y, Step);
          end;

          // Usually you don't need to worry about the Z position unless you are using Horde 3D or 3D in general
          if AnimProperty.StartPosition.Z <> AnimProperty.EndPosition.Z then
          begin
            if AnimProperty.StartPosition.Z > AnimProperty.EndPosition.Z then
              fTarget.Position.Z := InverseLerp(AnimProperty.EndPosition.Z, AnimProperty.StartPosition.Z, Step)
            else
              fTarget.Position.Z := Lerp(AnimProperty.StartPosition.Z, AnimProperty.EndPosition.Z, Step);
          end;

        end;
      end;
    end;
  end;

  procedure AnimOrigin(dt: Double = 0.0);
  begin
    if not VectorEquals(AnimProperty.StartOrigin, AnimProperty.EndOrigin) then
    begin
      case Transition of
        atLinear:
        begin
          if AnimProperty.StartOrigin.X <> AnimProperty.EndOrigin.X then
          begin
            if AnimProperty.StartOrigin.X > AnimProperty.EndOrigin.X then
              fTarget.Origin.X := InverseLerp(AnimProperty.EndOrigin.X, AnimProperty.StartOrigin.X, Step)
            else
              fTarget.Origin.X := Lerp(AnimProperty.StartOrigin.X, AnimProperty.EndOrigin.X, Step);
          end;

          if AnimProperty.StartOrigin.Y <> AnimProperty.EndOrigin.Y then
          begin
            if AnimProperty.StartOrigin.Y > AnimProperty.EndOrigin.Y then
              fTarget.Origin.Y := InverseLerp(AnimProperty.EndOrigin.Y, AnimProperty.StartOrigin.Y, Step)
            else
              fTarget.Origin.Y := Lerp(AnimProperty.StartOrigin.Y, AnimProperty.EndOrigin.Y, Step);
          end;
        end;
      end;
    end;
  end;

  procedure AnimRotation(dt: Double = 0.0);
  begin
    // Rotation vector
    if not VectorEquals(AnimProperty.StartRotation.Vector, AnimProperty.EndRotation.Vector) then
    begin
      case Transition of
        atLinear:
        begin
          if AnimProperty.StartRotation.Vector.X <> AnimProperty.EndRotation.Vector.X then
          begin
            if AnimProperty.StartRotation.Vector.X > AnimProperty.EndRotation.Vector.X then
              fTarget.Rotation.Vector.X := InverseLerp(AnimProperty.EndRotation.Vector.X, AnimProperty.StartRotation.Vector.X, Step)
            else
              fTarget.Rotation.Vector.X := Lerp(AnimProperty.StartRotation.Vector.X, AnimProperty.EndRotation.Vector.X, Step);
          end;

          if AnimProperty.StartRotation.Vector.Y <> AnimProperty.EndRotation.Vector.Y then
          begin
            if AnimProperty.StartRotation.Vector.Y > AnimProperty.EndRotation.Vector.Y then
              fTarget.Rotation.Vector.Y := InverseLerp(AnimProperty.EndRotation.Vector.Y, AnimProperty.StartRotation.Vector.Y, Step)
            else
              fTarget.Rotation.Vector.Y := Lerp(AnimProperty.StartRotation.Vector.Y, AnimProperty.EndRotation.Vector.Y, Step);
          end;

          // Usually you don't need to worry about the Z StartRotation.Vector unless you are using Horde 3D
          if AnimProperty.StartRotation.Vector.Z <> AnimProperty.EndRotation.Vector.Z then
          begin
            if AnimProperty.StartRotation.Vector.Z > AnimProperty.EndRotation.Vector.Z then
              fTarget.Rotation.Vector.Z := InverseLerp(AnimProperty.EndRotation.Vector.Z, AnimProperty.StartRotation.Vector.Z, Step)
            else
              fTarget.Rotation.Vector.Z := Lerp(AnimProperty.StartRotation.Vector.Z, AnimProperty.EndRotation.Vector.Z, Step);
          end;

        end;
      end;
    end;

    // Rotation angle
    if AnimProperty.StartRotation.Angle <> AnimProperty.EndRotation.Angle then
    begin
      case Transition of
        atLinear:
        begin
          if AnimProperty.StartRotation.Angle > AnimProperty.EndRotation.Angle then
            fTarget.Rotation.Angle := Trunc(InverseLerp(AnimProperty.EndRotation.Angle, AnimProperty.StartRotation.Angle, Step))
          else
            fTarget.Rotation.Angle := Trunc(Lerp(AnimProperty.StartRotation.Angle, AnimProperty.EndRotation.Angle, Step));
        end;
      end;

    end;
  end;

  procedure AnimColor(dt: Double = 0.0);
  begin
    if not ColorEquals(AnimProperty.StartColor, AnimProperty.EndColor) then
    begin
      case Transition of
        atLinear:
        begin
          if AnimProperty.StartColor.R <> AnimProperty.EndColor.R then
          begin
            if AnimProperty.StartColor.R > AnimProperty.EndColor.R then
              fTarget.Color.R := Trunc(InverseLerp(AnimProperty.EndColor.R, AnimProperty.StartColor.R, Step))
            else
              fTarget.Color.R := Trunc(Lerp(AnimProperty.StartColor.R, AnimProperty.EndColor.R, Step));
          end;

          if AnimProperty.StartColor.G <> AnimProperty.EndColor.G then
          begin
            if AnimProperty.StartColor.G > AnimProperty.EndColor.G then
              fTarget.Color.G := Trunc(InverseLerp(AnimProperty.EndColor.G, AnimProperty.StartColor.G, Step))
            else
              fTarget.Color.G := Trunc(Lerp(AnimProperty.StartColor.G, AnimProperty.EndColor.G, Step));
          end;

          if AnimProperty.StartColor.B <> AnimProperty.EndColor.B then
          begin
            if AnimProperty.StartColor.B > AnimProperty.EndColor.B then
              fTarget.Color.B := Trunc(InverseLerp(AnimProperty.EndColor.B, AnimProperty.StartColor.B, Step))
            else
              fTarget.Color.B := Trunc(Lerp(AnimProperty.StartColor.B, AnimProperty.EndColor.B, Step));
          end;

          if AnimProperty.StartColor.A <> AnimProperty.EndColor.A then
          begin
            if AnimProperty.StartColor.A > AnimProperty.EndColor.A then
              fTarget.Color.A := Trunc(InverseLerp(AnimProperty.EndColor.A, AnimProperty.StartColor.A, Step))
            else
              fTarget.Color.A := Trunc(Lerp(AnimProperty.StartColor.A, AnimProperty.EndColor.A, Step));
          end;
        end;
      end;
    end;
  end;

  procedure AnimScale(dt: Double = 0.0);
  begin
    if not VectorEquals(AnimProperty.StartScale, AnimProperty.EndScale) then
    begin
      case Transition of
        atLinear:
        begin
          if AnimProperty.StartScale.X <> AnimProperty.EndScale.X then
          begin
            if AnimProperty.StartScale.X > AnimProperty.EndScale.X then
              fTarget.Scale.X := InverseLerp(AnimProperty.EndScale.X, AnimProperty.StartScale.X, Step)
            else
              fTarget.Scale.X := Lerp(AnimProperty.StartScale.X, AnimProperty.EndScale.X, Step);
          end;

          if AnimProperty.StartScale.Y <> AnimProperty.EndScale.Y then
          begin
            if AnimProperty.StartScale.Y > AnimProperty.EndScale.Y then
              fTarget.Scale.Y := InverseLerp(AnimProperty.EndScale.Y, AnimProperty.StartScale.Y, Step)
            else
              fTarget.Scale.Y := Lerp(AnimProperty.StartScale.Y, AnimProperty.EndScale.Y, Step);
          end;
        end;
      end;
    end;
  end;

begin
  if (fTarget <> nil) and (Self.Active) and (not fTimer.Paused) then
  begin
    if fTimer.GetTicks() >= Delay then
    begin
      if fTimer.Event then
      begin
        if fTimer.GetTicks() >= (Duration + Delay) then
          StopEvent();
      end;

      if fTimer.GetTicks() > 0 then
      begin
        case AnimProperty.AnimType of
          atAlpha: AnimAlpha(dt);
          atPosition: AnimPosition(dt);
          atOrigin: AnimOrigin(dt);
          atRotation: AnimRotation(dt);
          atColor: AnimColor(dt);
          atScale: AnimScale(dt);
        end;
      end;
    end;
  end;
end;

constructor TelAnimatorCombo.Create;
begin
  inherited;

  fActionList := TList.Create;
  fLoopCount := 1;

end;

destructor TelAnimatorCombo.Destroy;
var
  i: Integer;
begin
  for i := 0 to fActionList.Count - 1 do
  begin
    TelAnimator(fActionList[i]).Destroy;
  end;
  fActionList.Free;

  inherited;
end;

function TelAnimatorCombo.GetCount: Integer;
begin
  Result := fActionList.Count;
end;

procedure TelAnimatorCombo.SetSequential(Value: Boolean);
var
  i, j, newDuration: Integer;
begin
  newDuration := 0;

  fSequential := Value;
  if fOldDelay = nil then
  begin
    SetLength(fOldDelay, Count);
    for i := 0 to Count - 1 do
      fOldDelay[i] := Items[i].Delay;
  end;

  if fSequential then
  begin
    for i := 0 to Count - 2 do
    begin
      for j := 0 to i do
      begin
        newDuration := newDuration + (Items[j].Duration * Items[j].LoopCount);
      end;

      Items[i + 1].Delay := newDuration;
    end;
  end else
  begin
    for i := 0 to Count - 1 do
      Items[i].Delay := fOldDelay[i];
  end;
end;

function TelAnimatorCombo.GetFinished(): Boolean;
begin
  Result := Items[Count - 1].Finished;
end;

procedure TelAnimatorCombo.SetLoopCount(Value: Integer);
begin
  fLoopCount := Value;
  (*if fLoopCount = 0 then
    Loop := false
  else
    Loop := true;*)
end;

function TelAnimatorCombo.Get(Index: Integer): TelAnimator;
begin
  if ((Index >= 0) and (Index <= fActionList.Count - 1)) then Result := TelAnimator(fActionList[Index]);
end;

function TelAnimatorCombo.GetPos(Index: String): Integer;
Var a, TMP: Integer;
Begin
  Try
    For a := 0 To fActionList.Count - 1 Do
    Begin
      if Items[a].Name <> Index then TMP := -1
      else begin
        TMP := a;
        Break;
      end;
    End;
  Finally
    Result := TMP;
  End;

end;

procedure TelAnimatorCombo.Put(Index: Integer; const Item: TelAnimator);
var
  TmpAction: TelAnimator;
begin
  if ((Index >= 0) and (Index <= fActionList.Count - 1)) then
  begin
    TmpAction := Get(Index);
    TmpAction.Destroy;
    Insert(Index, Item);
  end;

end;

procedure TelAnimatorCombo.PutS(Index: String; const Item: TelAnimator);
var
  TMP: Integer;
  TmpAction: TelAnimator;
Begin
  if (Index <> '') then
  begin
    TmpAction := GetS(Index);
	if TmpAction <> nil then
	begin
	  TMP := GetPos(Index);
      TmpAction.Destroy;
      Insert(TMP, Item);
	end;
   end;
end;

function TelAnimatorCombo.GetS(Index: String): TelAnimator;
Var TMP: Integer;
Begin
  TMP := GetPos(Index);
  if TMP >= 0 then Result := TelAnimator(fActionList[TMP])
			  else Result := nil;
end;

procedure TelAnimatorCombo.Insert(Index: Integer; Action: TelAnimator);
begin
  if ((Index >= 0) and (Index <= fActionList.Count - 1)) then fActionList.Insert(Index, Action);
end;

function TelAnimatorCombo.Add(Action: TelAnimator): Integer;
begin
  Result := fActionList.Add(Action);
end;

procedure TelAnimatorCombo.Delete(Index: Integer);
var
  TmpAction: TelAnimator;
begin
  if ((Index >= 0) and (Index <= fActionList.Count - 1)) then
  begin
    TmpAction := Get(Index);
    TmpAction.Destroy;
    fActionList.Delete(Index);
  end;

end;

procedure TelAnimatorCombo.Start();
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Start();
end;

procedure TelAnimatorCombo.Pause();
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Pause();
end;

procedure TelAnimatorCombo.Stop();
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Stop();
end;

procedure TelAnimatorCombo.Update(dt: Double = 0.0);
var
  i: Integer;
begin
  (*if Loop then
  begin
    if Finished then
    begin
      if LoopCount <> 0 then
      begin
        if LoopCount > 0 then LoopCount := LoopCount - 1;
        Start();
      end;
    end;
  end;*)

  //if not Finished then
  //begin
    for i := 0 to Count - 1 do
      Items[i].Update(dt);

  //end;
end;

end.
