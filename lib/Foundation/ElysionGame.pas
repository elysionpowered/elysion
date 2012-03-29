// Elysion Frameworks
// Basic Game unit
// Abstract layer for games 
//
// (C) 2005 - 2011, Johannes Stein
// Freeze Dev - http://elysion.freeze-dev.com
//

unit ElysionGame;

{$I Elysion.inc}

interface

uses
    Classes,

    ElysionObject,
    ElysionTypes,
    ElysionUtils,
    ElysionScene,
    ElysionGraphicsProvider,
<<<<<<< HEAD
    ElysionApplication;
=======
    ElysionWindowManager,
    ElysionEnvironment;
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1


type

{ TelGame }

  TelGame = class(TelObject)
  private
<<<<<<< HEAD
    function GetRun: Boolean;
=======
    //function GetRun: Boolean;
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1
  protected
    fResolutions: TelSizeArray;
    fSceneDirector: TelSceneDirector;

    function GetWidth: Integer; inline;
    function GetHeight: Integer; inline;

    procedure GetOptiomalResolution(out Size: TelSize; out Fullscreen: Boolean);
  public
    // Creates TelScene with no strings attached
    // A window needs to be created manually if not done yet
    constructor Create; Overload; Override;

    // Creates TelScene and creates a window
    constructor Create(Width, Height, BPP: Integer; Fullscreen: Boolean); Overload;

    destructor Destroy(); Override;

    procedure Initialize(); virtual; abstract;

    procedure Render(Graphics: IGraphicsProvider); virtual;
    procedure Update(dt: Double = 0.0); virtual;
    procedure HandleEvents(); virtual;

    function Param(aParam: AnsiString): Boolean;
  public
    property Resolutions: TelSizeArray read fResolutions write fResolutions;
  published
<<<<<<< HEAD
    property Run: Boolean read GetRun;
=======
    //property Run: Boolean read GetRun;
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1

    property SceneDirector: TelSceneDirector read fSceneDirector write fSceneDirector;

    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
end;

implementation

constructor TelGame.Create;
begin
  inherited;

<<<<<<< HEAD
  if Length(fResolutions) = 0 then fResolutions := PopulateArray([makeSize(1024, 600)]);
=======
  if Length(fResolutions) = 0 then fResolutions := PopulateArray([TelSize.Create(1024, 600)]);
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1
end;

constructor TelGame.Create(Width, Height, BPP: Integer; Fullscreen: Boolean);
begin
  inherited Create;

<<<<<<< HEAD
  WindowManager.CreateWindow('', Width, Height, BPP, Fullscreen);
=======
  TelWindowManager.CreateWindow('', Width, Height, BPP, Fullscreen);
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1
  
  fSceneDirector := TelSceneDirector.Create;
end;

destructor TelGame.Destroy();
begin
  fSceneDirector.Destroy;

<<<<<<< HEAD
  inherited;
=======
  inherited Destroy;
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1
end;

procedure TelGame.Render(Graphics: IGraphicsProvider);
begin
  SceneDirector.Render(Graphics);
end;

procedure TelGame.Update(dt: Double = 0.0);
begin
  SceneDirector.Update(dt);
end;

procedure TelGame.HandleEvents();
begin
  SceneDirector.HandleEvents();
end;

function TelGame.Param(aParam: AnsiString): Boolean;
var
  i: Integer;
begin
  Result := false;

  if ParamCount >= 1 then
  begin
    for i := 1 to ParamCount - 1 do
    begin
      if ParamStr(i) = aParam then
      begin
        Result := true;
        Exit;
      end;
    end;
  end;
end;

<<<<<<< HEAD
function TelGame.GetRun: Boolean;
begin
  Result := Application.Run;
end;

function TelGame.GetWidth: Integer;
begin
  if WindowManager.CurrentWindow <> nil then
    Result := WindowManager.CurrentWindow.Width;
=======
(*function TelGame.GetRun: Boolean;
begin
  Result := Application.Run;
end;*)

function TelGame.GetWidth: Integer;
begin
  if TelWindowManager.CurrentWindow <> nil then
    Result := TelWindowManager.CurrentWindow.Width;
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1
end;

function TelGame.GetHeight: Integer;
begin
<<<<<<< HEAD
  if WindowManager.CurrentWindow <> nil then
    Result := WindowManager.CurrentWindow.Height;
=======
  if TelWindowManager.CurrentWindow <> nil then
    Result := TelWindowManager.CurrentWindow.Height;
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1
end;

procedure TelGame.GetOptiomalResolution(out Size: TelSize; out
  Fullscreen: Boolean);
var
  i: Integer;
  prevWidth, prevHeight: Single;
  prevFullscreen: Boolean;
begin
  prevWidth := 0;
  prevHeight := 0;
  prevFullscreen := false;

  // Use the native desktop resolution if possible
  for i := 0 to Length(fResolutions) do
  begin
<<<<<<< HEAD
    if ((fResolutions[i].Width = Environment.Width) and (fResolutions[i].Height = Environment.Height)) then
=======
    if ((fResolutions[i].Width = TelEnvironment.Width) and (fResolutions[i].Height = TelEnvironment.Height)) then
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1
    begin
      prevWidth := fResolutions[i].Width;
      prevHeight := fResolutions[i].Height;

      prevFullscreen := true;

      Break;
    end else
    begin
<<<<<<< HEAD
      if Environment.AspectRatio = fResolutions[i].AspectRatio then
=======
      if TelEnvironment.AspectRatio = fResolutions[i].AspectRatio then
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1
      begin
        if ((prevWidth < fResolutions[i].Width) and (prevHeight < fResolutions[i].Height)) then
        begin
          prevWidth := fResolutions[i].Width;
          prevHeight := fResolutions[i].Height;

          prevFullscreen := false;
        end;
      end;
    end;
  end;

  Size.Width := prevWidth;
  Size.Height := prevHeight;
  Fullscreen := prevFullscreen;
end;

end.
