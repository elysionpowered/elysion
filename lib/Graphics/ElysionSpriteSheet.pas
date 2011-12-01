unit ElysionSpriteSheet;

{$I Elysion.inc}

interface

uses
  ElysionTypes,
  ElysionSprite,
  ElysionTimer,

  Classes,
  SysUtils;

type
  { TelSpriteSheet }

  TelSpriteSheet = class(TelSprite)
  private
    fMaxFrames, fFrame: Integer;
    fTimer: TelTimer;
    fFrameSize: TelSize;
    fLoop: Boolean;

    fAnimationList: TStringList;
    fAnimFrames: array of Integer;

    function GetColumns: Integer;
    function GetMaxFrames: Integer;
    function GetRows: Integer;
    procedure SetColumns(AValue: Integer);
    procedure SetRows(AValue: Integer);

    procedure UpdateSpritesheet;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    // Define animation in pixels
    procedure Define(AnimName: AnsiString; aRect: TelRect); Overload;
    // Define animation in frames
    procedure Define(AnimName: AnsiString; StartFrame, EndFrame: Integer); Overload;
    // Define specific frames
    procedure Define(AnimName: AnsiString; Frames: array of Integer); Overload;

    // Plays complete sprite sheet
    procedure Play(Length: Integer = 1000); Overload; inline;

    // Plays specific sprite sheet animations
    procedure Play(AnimName: AnsiString; Length: Integer = 1000); Overload; inline;
    procedure Stop(); inline;
    procedure Pause(); inline;
    procedure UnPause(); inline;

    procedure Draw(DrawChildren: Boolean = true); Override;
    procedure Update(dt: Double = 0.0); Override;

    procedure RandomFrame();

    property FrameSize: TelSize read fFrameSize write fFrameSize;
  published
    property Frame: Integer read fFrame write fFrame;

    property MaxFrames: Integer read GetMaxFrames;

    property Columns: Integer read GetColumns write SetColumns;
    property Rows: Integer read GetRows write SetRows;

    property Loop: Boolean read fLoop write fLoop;
  end;

implementation

constructor TelSpriteSheet.Create;
begin
  inherited;

  fTimer := TelTimer.Create;
  fFrame := 0;

  fAnimationList := TStringList.Create;
  fAnimationList.NameValueSeparator := ':';

  (*{$IFDEF DELPHI_COMPAT}
  fTimer.OnEvent := Self.UpdateSpritesheet;
  {$ELSE}
  fTimer.OnEvent := @Self.UpdateSpritesheet;
  {$ENDIF}*)

  fTimer.OnEvent := Self.UpdateSpritesheet;

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

procedure TelSpriteSheet.SetRows(AValue: Integer);
begin
  Self.FrameSize.Height := Self.TextureHeight div AValue;
end;

procedure TelSpriteSheet.UpdateSpritesheet;
begin

end;

destructor TelSpriteSheet.Destroy;
begin
  fTimer.Destroy;

  inherited;
end;

procedure TelSpriteSheet.Define(AnimName: AnsiString; aRect: TelRect);
var
  tmpStartFrame, tmpEndFrame: Integer;
begin
  tmpStartFrame := (Trunc(aRect.X) * Columns) + (Trunc(aRect.Y) * Rows);
  tmpEndFrame := Trunc(aRect.X + aRect.W) * Columns + Trunc(aRect.Y + aRect.H) * Rows;

  fAnimationList.Add(AnimName + ':' + IntToStr(tmpStartFrame) + ' ' + IntToStr(tmpEndFrame));
end;

procedure TelSpriteSheet.Define(AnimName: AnsiString; StartFrame, EndFrame: Integer);
begin
  fAnimationList.Add(AnimName + ':' + IntToStr(StartFrame) + ' ' + IntToStr(EndFrame));
end;

procedure TelSpriteSheet.Define(AnimName: AnsiString; Frames: array of Integer);
begin

end;

procedure TelSpriteSheet.Play(Length: Integer = 1000);
begin
  //fAnimFrames := [0];
  //fEndFrame := GetMaxFrames;

  fTimer.Interval := Length div GetMaxFrames;
end;

procedure TelSpriteSheet.Play(AnimName: AnsiString; Length: Integer = 1000);
begin
  fAnimationList.Values[AnimName];

  Self.Log(fAnimationList.Values[AnimName]);
  //fTimer.Interval := ;
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

procedure TelSpriteSheet.Draw(DrawChildren: Boolean = true);
begin
  inherited Draw(DrawChildren);
end;

procedure TelSpriteSheet.Update(dt: Double);
begin
  inherited Update(dt);

  fTimer.Update(dt);
  Self.ClipImage(makeRect(fFrame div Columns, fFrame mod Rows, FrameSize.Width, FrameSize.Height));
end;

procedure TelSpriteSheet.RandomFrame();
begin
  fFrame := Random(GetMaxFrames) + 1;
end;

end.