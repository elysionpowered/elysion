unit ElysionEnums;

{$I Elysion.inc}

interface

type
  // TelVideoFlags
  TelVideoFlag =
    (vfNull,      //< vfNull: Use for console applications, no video surface will be created
     vfAuto,      //< vfAuto: Automatically checks if hardware or software render mode are available
     vfHardware,  //< vfHardware: Use hardware surface
     vfSoftware); //< vfSoftware: Use software surface

  TelBlendMode = (
    bmAdd,    //< Additive blending
    bmNormal, //< Normal blending
    bmSub);   //< Sub blending


  TelAssetType = (atTexture, atSprite, atParallexSprite, atSpriteSheet);

  TelProjectionMode = (pmOrtho, pmFrustum);

  // Rectangle or size orientation
  TRectOrientation = (roLandscape, roPortrait);

implementation

end.