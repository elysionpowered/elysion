(*
 * Localizable error strings
 *
 *)
unit ElysionStrings;

{$I Elysion.inc}

interface

resourcestring
  rsFailedInitDevice = 'Failed initializing device';
  rsFailedInitGraphicsProvider = 'Failed initializing graphics provider';
  rsFailedInitAudioProvider = 'Failed initializing audio provider';
  rsFailedInitNetworkProvider = 'Failed initializing network provider';
  rsFailedInitWindowProvider = 'Failed initializing window provider';

  rsFileNotFound = 'File %s not found';
  rsFileLoadingError = 'Error while loading file %s';
  rsFileNotSpecified = 'No filename specified';

  rsObjectCreated = 'Object created';
  rsObjectDestroyed = 'Object destroyed';

  rsAudioMusicError = 'Error with some music stuff';
  rsAudioSoundError = 'Error with some sound stuff';


implementation

end.
