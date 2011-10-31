// Elysion Frameworks
// Basic Game unit
// Abstract layer for games 
//
// (C) 2005 - 2011, Johannes Stein
//

unit ElysionScene;

{$I Elysion.inc}

interface

uses
    Classes,

    ElysionObject,
    ElysionEvents,
    ElysionApplication,
    ElysionInput,
    ElysionNode,
    ElysionEntity,
    ElysionLayer;


type

TelGame = class(TelObject)
  private
    function GetWidth: Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function GetHeight: Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  public
    // Creates TelScene with no strings attached
    // A window needs to be created manually if not done yet
    constructor Create; Overload; Override;
    
    // Creates TelScene and creates a window
    constructor Create(Width, Height, BPP: Integer; Fullscreen: Boolean); Overload;

    destructor Destroy(); Override;
  
    procedure Initialize(); virtual; abstract;
    
    procedure Render(); virtual;
    procedure Update(dt: Double = 0.0); virtual;
    procedure HandleEvents(); virtual;

    function Param(aParam: String): Boolean;
  published
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
end;

{ TelScene }

TelScene = class(TelObject)
  private
    fNodeList: TelNodeList;
    fGUILayer: TelLayer;

    fInitialized, fAutoSave, fPaused, fPauseKeyDefined: Boolean;
    fPauseKey: Cardinal;

    fEditable: Boolean;

    fOnPause, fOnResume: TelEvent;

    procedure SetEditable(AValue: Boolean);
    function GetModified(): Boolean;
  public
    constructor Create; Override; Overload;
    constructor Create(aName: String); Overload;

    destructor Destroy; Override;

    procedure Reset(); virtual;
	
	//procedure Initialize(); virtual; abstract;

    procedure Add(aNode: TelNode); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure Add(anEntity: TelEntity); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    procedure SetPauseKey(aKey: Cardinal);
    procedure DisablePauseKey();

    procedure Render(); virtual;
    procedure Update(dt: Double = 0.0); virtual;
    procedure HandleEvents(); virtual;
  published
    property AutoSave: Boolean read fAutoSave write fAutoSave;

    property Initialized: Boolean read fInitialized;
    property Editable: Boolean read fEditable write SetEditable;

    property Modified: Boolean read GetModified;

    property GUILayer: TelLayer read fGUILayer write fGUILayer;

    property Paused: Boolean read fPaused write fPaused;

    // Pause / Resume events
    property OnPause: TelEvent read fOnPause write fOnPause;
    property OnResume: TelEvent read fOnResume write fOnResume;
end;

{ Tel3DScene }

Tel3DScene = class(TelScene)
  public
    procedure Render2D(); virtual;
    procedure Render3D(); virtual;

    procedure Render(); Override;
end;

TelSceneList = class(TelObject)
   private
    fSceneList: TList;

    function Get(Index: Integer): TelScene; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function GetPos(Index: String): Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure Put(Index: Integer; const Item: TelScene); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure PutS(Index: String; const Item: TelScene); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function GetS(Index: String): TelScene; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function GetCount: Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  public
    constructor Create; Override;
    destructor Destroy; Override;

    procedure Insert(Index: Integer; Scene: TelScene); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Add(Scene: TelScene): Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure Delete(Index: Integer); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    property Items[Index: Integer]: TelScene read Get write Put; default;
    property Find[Index: String]: TelScene read GetS write PutS;
  published
    property Count: Integer read GetCount;
end;

{ TelSceneDirector }

TelSceneDirector = class(TelObject)
  private
    fList: TelSceneList;
    fNullScene: TelScene;

    fActiveSceneID: Integer;
    
    function GetCount(): Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function GetCurrentScene(): TelScene; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function Get(Index: Integer): TelScene; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure Put(Index: Integer; const Item: TelScene); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure PutS(Index: String; const Item: TelScene); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function GetS(Index: String): TelScene; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  public
    constructor Create; Override;
    destructor Destroy; Override;
	
    procedure Insert(Index: Integer; Scene: TelScene); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function Add(aScene: TelScene): Integer; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function Add(aScene: TelScene; aName: String): Integer; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    procedure Delete(Index: Integer); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    procedure SwitchTo(Index: Integer); Overload;
    procedure SwitchTo(aSceneName: String); Overload;

    procedure Render(); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure Update(dt: Double = 0.0); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure HandleEvents(); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    property Items[Index: Integer]: TelScene read Get write Put; default;
    property Find[Index: String]: TelScene read GetS write PutS;
  published
    property Count: Integer read GetCount;

    property CurrentScene: TelScene read GetCurrentScene;
end;

{$IFDEF AUTO_INIT}
var
  SceneDirector: TelSceneDirector;
{$ENDIF}

implementation

{ Tel3DScene }

procedure Tel3DScene.Render2D();
var
  i: Integer;
begin
  for i := 0 to fNodeList.Count - 1 do
  begin
    if (fNodeList.Items[i] <> nil) then fNodeList.Items[i].Draw;
  end;
end;

procedure Tel3DScene.Render3D();
begin
  // 3D stuff goes here
end;

procedure Tel3DScene.Render();
begin
  ActiveWindow.Projection := pmPerspective;
  ActiveWindow.BeginScene();
  Self.Render3D();
  ActiveWindow.EndScene();

  ActiveWindow.Projection := pmOrtho;
  ActiveWindow.BeginScene();
  Self.Render2D();
  ActiveWindow.EndScene();
end;

constructor TelGame.Create;
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

procedure TelGame.Render();
begin
  SceneDirector.Render();
end;

procedure TelGame.Update(dt: Double = 0.0);
begin
  SceneDirector.Update(dt);
end;

procedure TelGame.HandleEvents();
begin
  SceneDirector.HandleEvents();
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

{ TelScene }

constructor TelScene.Create;
begin
  inherited;

  fOnPause := nil;
  fOnResume := nil;

  fNodeList := TelNodeList.Create;
  GUILayer := TelLayer.Create;

    fAutoSave := true;
end;

constructor TelScene.Create(aName: String);
begin
  Create;

  Self.Name := aName;
end;

destructor TelScene.Destroy;
begin
  if AutoSave and Modified then
  begin
    // TODO: Save scene content if modified
  end;

  inherited;
end;

procedure TelScene.Reset();
begin
  fEditable := false;
  fPaused := false;
end;

procedure TelScene.SetEditable(AValue: Boolean);
var
  i: Integer;
begin
  fEditable := AValue;

  for i := 0 to fNodeList.Count - 1 do fNodeList.Items[i].Editable := fEditable;
end;

function TelScene.GetModified(): Boolean;
var
  i: Integer;
begin
  for i := 0 to fNodeList.Count - 1 do
  begin
    if fNodeList.Items[i].Modified then
    begin
      Result := true;
      Exit;
    end;
  end;
end;

procedure TelScene.Add(aNode: TelNode);
begin
  fNodeList.Add(aNode);
end;

procedure TelScene.Add(anEntity: TelEntity);
begin
  // ADD IT!
end;

procedure TelScene.SetPauseKey(aKey: Cardinal);
begin
  fPauseKeyDefined := true;

  fPauseKey := aKey;
end;

procedure TelScene.DisablePauseKey();
begin
  fPauseKeyDefined := false;
end;

procedure TelScene.Render();
begin
  fNodeList.Draw();

  if GUILayer.Count > 0 then GUILayer.Draw();
end;

procedure TelScene.Update(dt: Double = 0.0);
begin
  if fPauseKeyDefined then
  begin
    if Input.Keyboard.IsKeyHit(fPauseKey) then
    begin
      if Self.Paused then
      begin
        Self.Paused := false;
        if Assigned(fOnResume) then fOnResume;
      end else
      begin
        Self.Paused := true;
        if Assigned(fOnPause) then fOnPause;
      end;

    end;
  end;

  if not Self.Paused then
  begin
    fNodeList.Update(dt);

    fGUILayer.Update(dt);
  end;
end;

procedure TelScene.HandleEvents();
begin

end;

{ TelSceneList }

constructor TelSceneList.Create;
begin
  inherited;

  fSceneList := TList.Create;
end;

destructor TelSceneList.Destroy;
var
  i: Integer;
begin
  for i := 0 to fSceneList.Count - 1 do
  begin
    TelScene(fSceneList[i]).Free;
  end;
  fSceneList.Free;

  inherited;
end;

function TelSceneList.GetCount: Integer;
begin
  Result := fSceneList.Count;
end;

function TelSceneList.Get(Index: Integer): TelScene;
begin
  if ((Index >= 0) and (Index <= fSceneList.Count - 1)) then Result := TelScene(fSceneList[Index]);
end;

function TelSceneList.GetPos(Index: String): Integer;
Var a, TMP: Integer;
Begin
  Try
    For a := 0 To fSceneList.Count - 1 Do
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

procedure TelSceneList.Put(Index: Integer; const Item: TelScene);
var
  TmpScene: TelScene;
begin
  if ((Index >= 0) and (Index <= fSceneList.Count - 1)) then
  begin
    TmpScene := Get(Index);
    TmpScene.Destroy;
    Insert(Index, Item);
  end;

end;

procedure TelSceneList.PutS(Index: String; const Item: TelScene);
var
  TMP: Integer;
  TmpScene: TelScene;
Begin
  if (Index <> '') then
  begin
    TmpScene := GetS(Index);
	if TmpScene <> nil then
	begin
	  TMP := GetPos(Index);
      TmpScene.Destroy;
      Insert(TMP, Item);
	end;
   end;
end;

function TelSceneList.GetS(Index: String): TelScene;
Var TMP: Integer;
Begin
  TMP := GetPos(Index);
  if TMP >= 0 then Result := TelScene(fSceneList[TMP])
			  else Result := nil;
end;

procedure TelSceneList.Insert(Index: Integer; Scene: TelScene);
begin
  if ((Index >= 0) and (Index <= fSceneList.Count - 1)) then fSceneList.Insert(Index, Scene);
end;

function TelSceneList.Add(Scene: TelScene): Integer;
begin
  Result := fSceneList.Add(Scene);
end;

procedure TelSceneList.Delete(Index: Integer);
var
  TmpScene: TelScene;
begin
  if ((Index >= 0) and (Index <= fSceneList.Count - 1)) then
  begin
    TmpScene := Get(Index);
    TmpScene.Destroy;
    fSceneList.Delete(Index);
  end;

end;

{ TelSceneDirector }

constructor TelSceneDirector.Create();
begin
  inherited;

  // Create empty scene
  fNullScene := TelScene.Create('');
  
  fList := TelSceneList.Create;
  fActiveSceneID := -1;
end;

destructor TelSceneDirector.Destroy();
begin
  fList.Destroy;

  inherited;
end;

function TelSceneDirector.GetCount(): Integer;
begin
  Result := fList.Count;
end;

function TelSceneDirector.Get(Index: Integer): TelScene;
begin
  Result := fList.Items[Index];
end;

procedure TelSceneDirector.Put(Index: Integer; const Item: TelScene);
begin
  fList.Items[Index] := Item;
end;

procedure TelSceneDirector.PutS(Index: String; const Item: TelScene);
begin
  fList.Find[Index] := Item;
end;

function TelSceneDirector.GetS(Index: String): TelScene;
begin
  Result := fList.Find[Index];
end;

function TelSceneDirector.GetCurrentScene(): TelScene;
begin
  if fActiveSceneID < 0 then Result := fNullScene
  else Result := fList.Items[fActiveSceneID];
end;

procedure TelSceneDirector.Insert(Index: Integer; Scene: TelScene);
begin
  fList.Insert(Index, Scene);
end;

function TelSceneDirector.Add(aScene: TelScene): Integer;
var
  newSceneID: Integer;
begin
  newSceneID := fList.Add(aScene);

  fActiveSceneID := newSceneID;
  Result := newSceneID;
end;

function TelSceneDirector.Add(aScene: TelScene; aName: String): Integer;
begin
  aScene.Name := aName;

  Result := Self.Add(aScene);
end;

procedure TelSceneDirector.Delete(Index: Integer);
begin
  fList.Delete(Index);
end;

procedure TelSceneDirector.SwitchTo(Index: Integer);
begin
  fActiveSceneID := Index;
end;

procedure TelSceneDirector.SwitchTo(aSceneName: String);
Var a, TMP: Integer;
Begin
  Try
    For a := 0 To Count - 1 Do
    Begin
      if fList.Items[a].Name <> aSceneName then TMP := -1
      else begin
        TMP := a;
        Break;
      end;
    End;
  Finally
    fActiveSceneID := TMP;
  End;

end;

procedure TelSceneDirector.Render();
begin
  if CurrentScene = fNullScene then Exit
  else CurrentScene.Render();
end;

procedure TelSceneDirector.Update(dt: Double = 0.0);
begin
  if CurrentScene = fNullScene then Exit
  else CurrentScene.Update(dt);
end;

procedure TelSceneDirector.HandleEvents();
begin
  if CurrentScene = fNullScene then Exit
  else CurrentScene.HandleEvents();
end;

{$IFDEF AUTO_INIT}
initialization
  SceneDirector := TelSceneDirector.Create;

finalization
  // TODO: Fix this here. If uncommented -> access violation
  //SceneDirector.Free;
{$ENDIF}

end.
