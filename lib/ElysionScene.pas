// Elysion Frameworks
// Basic Game unit
// Abstract layer for games 
//
// (C) 2005 - 2011, Johannes Stein
// Freeze Dev - http://elysion.freeze-dev.com
//

unit ElysionStage;

{$I Elysion.inc}

interface

uses
    Classes,

    ElysionObject,
    ElysionApplication,
    ElysionNode;


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

TelScene = class(TelObject)
  private
    fNodeList: TelNodeList;
	fInitialized: Boolean;
  public
    constructor Create; Override;
    destructor Destroy; Override;
	
	procedure Initialize(); virtual;

    procedure Add(aNode: TelNode); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    procedure Render(); virtual;
    procedure Update(dt: Double); virtual;
	procedure HandleEvents(); virtual; abstract;
  published
    property Initialized: Boolean read fInitialized;
end;

TelSceneDirector = class(TelObject)
  private
    fList: TList;
  public
    constructor Create; Overload;
	destructor Destroy; Overload;
	
	procedure Add(aScene: TelScene); {$IFDEF CAN_INLINE} inline; {$ENDIF}
	
	
	procedure Initialize(); Overload;
	procedure Initialize(Exceptions: array of Integer); Overload;
	
	procedure SetCurrentScene();
  published
    property Count: Integer read GetCount;
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

constructor TelScene.Create;
begin
  inherited;

  fNodeList := TelNodeList.Create;
end;

destructor TelScene.Destroy;
begin
  inherited;
end;

procedure TelScene.Add(aNode: TelNode);
begin
  fNodeList.Add(aNode);
end;

procedure TelScene.Render();
var
  i: Integer;
begin
  for i := 0 to fNodeList.Count - 1 do
  begin
    if fNodeList.Items[i] <> nil then fNodeList.Items[i].Draw;
  end;
end;

procedure TelScene.Update(dt: Double);
var
  i: Integer;
begin
  for i := 0 to fNodeList.Count - 1 do
  begin
    if fNodeList.Items[i] <> nil then fNodeList.Items[i].Update;
  end;
end;

end.
