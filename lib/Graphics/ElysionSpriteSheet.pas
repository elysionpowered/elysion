unit ElysionSpriteSheet;

interface

{$I Elysion.inc}

uses
  ElysionTypes,
  ElysionGraphicsProvider,
  ElysionSprite,
  ElysionTimer,
  ElysionMath,

  Classes,
  SysUtils;

type
  { TelSpriteSheet }

  TelSpriteSheet = class(TelSprite)
  protected
    fTimer: TelTimer;
    fFrameSize: TelSize;

    fFinished: Boolean;
    fLoop: Boolean;

    fWaitUntilFinished: Boolean;

    fFrameArray: array of Integer;

    fFrame: Integer;
    fFrameIndex: Integer;

    function GetColumns: Integer;
    function GetMaxFrames: Integer;
    function GetRows: Integer;
    procedure SetColumns(AValue: Integer);
    procedure SetFrame(AValue: Integer);
    procedure SetRows(AValue: Integer);

    procedure UpdateSpritesheet;

    procedure StartFrameAnimation(aLength: Integer);
  public
    constructor Create; Override;
    destructor Destroy; Override;

    function LoadFromFile(aFilename: String): Boolean; Overload;
    function LoadFromFile(aFilename: String; aClipRect: TelRect): Boolean; Overload;

    // Plays complete sprite sheet
    procedure Play(aLength: Integer = 1000); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    // Plays specific frames in a spritesheet
    procedure PlayFrames(aFrameArray: array of Integer; aLength: Integer = 1000); Overload;
    procedure PlayFrames(aStartFrame, anEndFrame: Integer; aLength: Integer = 1000); Overload;

    // Plays specific column of a spritesheet (Stats at 0)
    procedure PlayColumn(aColumn: Integer; aLength: Integer = 1000); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    // Plays specific row of a spritesheet (Starts at 0)
    procedure PlayRow(aRow: Integer; aLength: Integer = 1000); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    // Stops spritesheet animation
    procedure Stop(); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    // Pauses spritesheet animation
    procedure Pause(); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    // Resumses spritesheet animation if paused
    procedure UnPause(); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    procedure Draw(Graphics: IGraphicsProvider; DrawChildren: Boolean = true); Override;
    procedure Update(dt: Double = 0.0); Override;

    // Sets a random frame of the spritesheet
    procedure RandomFrame(); Overload;
    procedure RandomFrame(aStartFrame, anEndFrame: Integer); Overload;
  public
    property FrameSize: TelSize read fFrameSize write fFrameSize;
  published
    property Finished: Boolean read fFinished;

    property Frame: Integer read fFrame write SetFrame;

    property MaxFrames: Integer read GetMaxFrames;

    property Columns: Integer read GetColumns write SetColumns;
    property Rows: Integer read GetRows write SetRows;

    property Loop: Boolean read fLoop write fLoop;

    // Bit verbose
    property WaitUntilFinished: Boolean read fWaitUntilFinished write fWaitUntilFinished;
  end;

implementation

constructor TelSpriteSheet.Create;
begin
  inherited;

  fTimer := TelTimer.Create;

  fWaitUntilFinished := false;

  fTimer.OnEvent := Self.UpdateSpritesheet;

  FrameSize := makeSize(64, 64);

  fFinished := true;
  fLoop := false;
end;

function TelSpriteSheet.GetMaxFrames: Integer;
begin
  Result := Columns * Rows;
end;

function TelSpriteSheet.GetColumns: Integer;
begin
  Result := Trunc(Self.TextureWidth / Self.FrameSize.Width);
end;

function TelSpriteSheet.GetRows: Integer;
begin
  Result := Trunc(Self.TextureHeight / Self.FrameSize.Height);
end;

procedure TelSpriteSheet.SetColumns(AValue: Integer);
begin
  Self.FrameSize.Width := Self.TextureWidth div AValue;
end;

procedure TelSpriteSheet.SetFrame(AValue: Integer);
begin
  if fFrame <> AValue then fFrame := AValue;
  fFrame := Clamp(AValue, 0, GetMaxFrames - 1);

  Self.ClipImage(makeRect((fFrame mod GetColumns) * FrameSize.Width, (fFrame div GetRows) * FrameSize.Height, FrameSize.Width, FrameSize.Height));
end;

procedure TelSpriteSheet.SetRows(AValue: Integer);
begin
  Self.FrameSize.Height := Self.TextureHeight div AValue;
end;

procedure TelSpriteSheet.UpdateSpritesheet;
begin
  Self.Log(fFrame);
  Self.Log(fFrameIndex);
  Self.Log(Length(fFrameArray));

  if Length(fFrameArray) = 0 then Exit;

  if fFrameIndex = Length(fFrameArray) - 1 then
  begin
    if not Loop then
    begin
      fTimer.Stop();
      fFinished := true;
      Exit;
    end else fFrameIndex := 0
  end else fFrameIndex := fFrameIndex + 1;


  SetFrame(fFrameArray[fFrameIndex]);
end;

procedure TelSpriteSheet.StartFrameAnimation(aLength: Integer);
begin
  fTimer.Interval := aLength div Length(fFrameArray);
  fFrameIndex := 0;
  Frame := fFrameArray[fFrameIndex];
  fTimer.Start();

  fFinished := false;
end;

destructor TelSpriteSheet.Destroy;
begin
  fTimer.Destroy;

  inherited;
end;

function TelSpriteSheet.LoadFromFile(aFilename: String): Boolean;
begin
  Result := inherited LoadFromFile(aFilename);
  Frame := 0;
end;

function TelSpriteSheet.LoadFromFile(aFilename: String; aClipRect: TelRect
  ): Boolean;
begin
  Result := inherited LoadFromFile(aFilename, aClipRect);
  FrameSize := makeSize(aClipRect.W, aClipRect.H);

  SetFrame((Trunc(aClipRect.X) div GetColumns) * (Trunc(aClipRect.Y) div GetRows));
end;

procedure TelSpriteSheet.Play(aLength: Integer = 1000);
begin
  PlayFrames(0, GetMaxFrames, aLength);
end;

procedure TelSpriteSheet.PlayFrames(aFrameArray: array of Integer;
  aLength: Integer = 1000);
var
  i: Integer;
begin
  if WaitUntilFinished then
    if not Self.Finished then Exit;


  SetLength(fFrameArray, Length(aFrameArray));
  for i := 0 to Length(aFrameArray) - 1 do
    fFrameArray[i] := Clamp(aFrameArray[i], 0, GetMaxFrames);

  StartFrameAnimation(aLength);
end;

procedure TelSpriteSheet.PlayFrames(aStartFrame, anEndFrame: Integer;
  aLength: Integer = 1000);
var
  tmpNumber, i: Integer;
begin
  if anEndFrame < aStartFrame then Exit;

  if WaitUntilFinished then
    if not Self.Finished then Exit;


  aStartFrame := Clamp(aStartFrame, 0, GetMaxFrames);
  anEndFrame := Clamp(anEndFrame, 0, GetMaxFrames);

  SetLength(fFrameArray, anEndFrame - aStartFrame + 1);

  tmpNumber := aStartFrame;
  for i := 0 to Length(fFrameArray) do
  begin
    fFrameArray[i] := tmpNumber;
    tmpNumber := tmpNumber + 1;
  end;

  StartFrameAnimation(aLength);
end;

procedure TelSpriteSheet.PlayColumn(aColumn: Integer; aLength: Integer = 1000);
begin
  if WaitUntilFinished then
    if not Self.Finished then Exit;


  aColumn := Clamp(aColumn, 0, GetColumns - 1);


  PlayFrames(aColumn * GetColumns, (aColumn + 1) * GetColumns - 1, aLength);
end;

procedure TelSpriteSheet.PlayRow(aRow: Integer; aLength: Integer = 1000);
var
  i: Integer;
begin
  if WaitUntilFinished then
    if not Self.Finished then Exit;


  aRow := Clamp(aRow, 0, GetRows - 1);

  SetLength(fFrameArray, GetRows);
  for i := 0 to Length(fFrameArray) - 1 do
    fFrameArray[i] := aRow + (i * GetRows);

  StartFrameAnimation(aLength);
end;

procedure TelSpriteSheet.Stop();
begin
  fTimer.Stop();
end;

procedure TelSpriteSheet.Pause();
begin
  fTimer.Pause();
end;

procedure TelSpriteSheet.UnPause();
begin
  fTimer.UnPause();
end;

procedure TelSpriteSheet.Draw(Graphics: IGraphicsProvider; DrawChildren: Boolean = true);
begin
  inherited Draw(Graphics, DrawChildren);
end;

procedure TelSpriteSheet.Update(dt: Double);
begin
  inherited Update(dt);

  fTimer.Update(dt);
end;

procedure TelSpriteSheet.RandomFrame();
begin
  RandomFrame(0, GetMaxFrames - 1);
end;

procedure TelSpriteSheet.RandomFrame(aStartFrame, anEndFrame: Integer);
begin
  aStartFrame := Clamp(0, aStartFrame, GetMaxFrames - 1);
  anEndFrame := Clamp(0, anEndFrame, GetMaxFrames - 1);

  Frame := Random(Abs(anEndFrame - aStartFrame)) + 1;
end;

end.
