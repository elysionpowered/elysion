unit ElysionAudio;

interface

{$I Elysion.inc}

uses
  ElysionTypes,
  ElysionObject,
  ElysionContainer,
  ElysionApplication,
  ElysionLogger,
  ElysionContent,

  SDL,
  SDL_mixer,

  SysUtils;

type

TelMusic = class(TelObject)
private
  fMusic: PMix_Music;
  fPaused, fIsPlaying: Boolean;

  FVolume: ShortInt;
  procedure SetVolume(Value: ShortInt); inline;
public
  constructor Create; Override;
  destructor Destroy(); Override;

  procedure SetPosition(Position: Double); inline;

  procedure LoadFromFile(Filename: String);
  procedure Play(Loop: Integer = 0); inline;
  procedure Pause(); inline;
  procedure Stop(); inline;
published
  property IsPlaying: Boolean read fIsPlaying;
  property Paused: Boolean read fPaused;
  property Volume: ShortInt read FVolume write SetVolume;
end;

TelSound = class(TelObject)
private
  fSound: PMix_Chunk;
public
  constructor Create; Override;
  destructor Destroy; Override;

  procedure LoadFromFile(Filename: String);
  procedure Play; inline;
  procedure Stop; inline;
end;


TSDLMixerAudioContainer = class(TelContainer)
private
  FAudio_rate, FAudio_rate_Mac: Integer;
  FAudio_format: UInt16;
  FAudio_channels: Integer;
  FAudio_buffers: Integer;
public
  constructor Create; Override;
  destructor Destroy; Override;

  function Initialize(): Boolean; Override;
  procedure Finalize(); Override;
end;


{$IFDEF AUTO_INIT}
var
  Audio: TSDLMixerAudioContainer;
{$ENDIF}

implementation


//
// TelMusic
//

constructor TelMusic.Create;
begin
  inherited Create;
end;

destructor TelMusic.Destroy();
begin
  Mix_HaltMusic();
  Mix_FreeMusic(fMusic);

  inherited Destroy;
end;

procedure TelMusic.LoadFromFile(Filename: String);
var Directory: String;
begin
  if Filename <> '' then
  begin
    Directory := ExtractFilePath(ParamStr(0));

    if FileExists(Directory+Content.RootDirectory+FileName) then
      fMusic := Mix_LoadMUS(PChar(Directory+Content.RootDirectory+FileName))
    else
      TelLogger.GetInstance.WriteLog('File not found: '+Directory+Content.RootDirectory+FileName, ltError);

  end else TelLogger.GetInstance.WriteLog('No filename specifies.', ltError);
end;

procedure TelMusic.SetVolume(Value: ShortInt); 
begin
  FVolume := Value;
  Mix_VolumeMusic(Value);
end;

procedure TelMusic.SetPosition(Position: Double); 
begin
  Mix_SetMusicPosition(Position);
end;

procedure TelMusic.Play(Loop: Integer = 0); 
begin
  if fPaused then
  begin
    fPaused := false;
    Mix_ResumeMusic();
  end else Mix_PlayMusic(fMusic, Loop);
  fIsPlaying := true;
end;

procedure TelMusic.Pause(); 
begin
  fPaused := true;
  fIsPlaying := false;
  Mix_PauseMusic();
end;

procedure TelMusic.Stop(); 
begin
  Mix_HaltMusic();
  fIsPlaying := false;
end;


//
// TelSound
//

constructor TelSound.Create;
begin
  inherited Create;
end;

destructor TelSound.Destroy;
Begin
  Mix_FreeChunk(fSound);

  inherited Destroy;
End;

procedure TelSound.LoadFromFile(Filename: String);
var Directory: String;
begin
  if Filename <> '' then
  begin
    Directory := ExtractFilePath(ParamStr(0));

    if FileExists(Directory+Content.RootDirectory+FileName) then
      fSound := Mix_LoadWAV(PChar(Directory+Content.RootDirectory+FileName))
    else
      TelLogger.GetInstance.WriteLog('File not found: '+Directory+Content.RootDirectory+FileName, ltError);
  end else TelLogger.GetInstance.WriteLog('No filename specifies.', ltError);
end;

Procedure TelSound.Play; 
Begin
  Mix_PlayChannel(0, fSound, 0);
End;

Procedure TelSound.Stop; 
Begin
  Mix_HaltChannel(0);
End;

constructor TSDLMixerAudioContainer.Create;
begin
  inherited;

  fDriverName := 'SDL_mixer';

  if (not Application.Initialized) then Application.Initialize;

  FAudio_rate := 22050;
  FAudio_rate_Mac := 44100;
  FAudio_format := AUDIO_S16;
  FAudio_channels := 2;
  FAudio_buffers := 4096;
end;

destructor TSDLMixerAudioContainer.Destroy;
begin
  inherited;
end;

function TSDLMixerAudioContainer.Initialize(): Boolean; 
var
  ResultInt: Integer;
begin
  {$IFDEF DARWIN}
  ResultInt := Mix_OpenAudio(FAudio_Rate_Mac, FAudio_format, FAudio_channels, FAudio_buffers);
  {$ELSE}
  ResultInt := Mix_OpenAudio(FAudio_Rate, FAudio_format, FAudio_channels, FAudio_buffers);
  {$ENDIF}

  if ResultInt = 0 then Result := true
    else Result := false;
end;

procedure TSDLMixerAudioContainer.Finalize(); 
begin
  Mix_CloseAudio;
end;

{$IFDEF AUTO_INIT}
initialization
  Audio := TSDLMixerAudioContainer.Create;

finalization
  if Audio <> nil then
  begin
    Audio.Finalize();
    Audio.Free;
  end;
{$ENDIF}


end.
