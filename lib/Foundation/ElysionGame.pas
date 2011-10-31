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
    ElysionApplication,
    ElysionScene;


type
// TODO: Merge TelGame and TelScene somehow

TelGame = class(TelObject)
  private
    function GetWidth: Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function GetHeight: Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  public
    // Creates TelScene with no strings attached
    // A window needs to be created manually if not done yet
    constructor Create(); Overload; Override;
    
    // Creates TelScene and creates a window
    constructor Create(Width, Height, BPP: Integer; Fullscreen: Boolean); Overload;

    destructor Destroy(); Override;
  
    procedure Initialize(); virtual; abstract;
    
    procedure Render(); virtual; abstract;
    procedure Update(dt: Double = 0.0); virtual; abstract;

    procedure HandleEvents(); virtual; abstract;

    function Param(aParam: String): Boolean;
  published
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
end;

implementation

constructor TelGame.Create();
begin
  inherited;
end;

constructor TelGame.Create(Width, Height, BPP: Integer; Fullscreen: Boolean);
begin
  inherited Create;

  Application.Initialize(Width, Height, BPP, Fullscreen);
end;

destructor TelGame.Destroy();
begin
  inherited;
end;

function TelGame.Param(aParam: String): Boolean;
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

function TelGame.GetWidth: Integer;
begin
  if ActiveWindow <> nil then Result := ActiveWindow.Width;
end;

function TelGame.GetHeight: Integer;
begin
  if ActiveWindow <> nil then Result := ActiveWindow.Height;
end;

end.
