unit ElysionAnimTypes;

interface

uses
  ElysionTypes;

type

  {%region 'Animation types'}
  // Each animator type responds to a node property
  TelAnimationType = (atAlpha, atPosition, atOrigin, atRotation, atColor, atScale, atShadow);

  // Different animator transitions: Only linear is working right now, the others are just placeholders
  TelAnimationTransition = (atLinear, atEaseIn, atEaseOut, atEaseInOut, atBounce);

  // Animation KeyFrame
  TelAnimationKeyFrame = record
    case AnimType: TelAnimationType of
      atAlpha: (Alpha: Byte);
      atPosition: (Position: TelVector3f);
      atOrigin: (Origin: TelVector2f);
      atRotation: (Rotation: TelImageRotation);
      atColor: (Color: TelColor);
      atScale: (Scale: TelVector2f);
      atShadow: (Shadow: TelShadow);
  end;

  // Animation Property
  TelAnimationProperty = record
    case AnimType: TelAnimationType of
      atAlpha: (StartAlpha, EndAlpha: Byte);
      atPosition: (StartPosition, EndPosition: TelVector3f);
      atOrigin: (StartOrigin, EndOrigin: TelVector2f);
      atRotation: (StartRotation, EndRotation: TelImageRotation);
      atColor: (StartColor, EndColor: TelColor);
      atScale: (StartScale, EndScale: TelVector2f);
      atShadow: (StartShadow, EndShadow: TelShadow);
  end;
  {%endregion}

implementation

end.
