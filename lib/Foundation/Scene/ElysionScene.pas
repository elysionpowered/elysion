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
  ElysionList,
  ElysionGraphicsProvider,
  ElysionEvents,
  ElysionApplication,
  ElysionInput,
  ElysionNode,
  ElysionEntity,
  ElysionLayer;


type

// Forward decleration
TelSceneDirector = class;

{ TelScene }

TelScene = class(TelObject)
  private
    fNodeList: TelNodeList;
    fEntityList: TelEntityList;

    fParent: TelSceneDirector;

    fGUILayer: TelLayer;

    fInitialized, fAutoSave, fPaused, fPauseKeyDefined: Boolean;
    fPauseKey: Cardinal;

    fEditable: Boolean;

    fOnPause, fOnResume: TelEvent;

    procedure SetEditable(AValue: Boolean);
    function GetModified(): Boolean;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    procedure Reset(); virtual;
	
	//procedure Initialize(); virtual; abstract;

    procedure Add(aNode: TelNode); Overload; inline;
    procedure Add(anEntity: TelEntity); Overload; inline;
    procedure Add(aNodeArray: array of TelNode); Overload;
    procedure Add(anEntityArray: array of TelEntity); Overload;

    procedure SetPauseKey(aKey: Cardinal);
    procedure DisablePauseKey();

    procedure Render(Graphics: IGraphicsProvider); virtual;
    procedure Update(dt: Double = 0.0); virtual;
    procedure HandleEvents(); virtual;
  published
    property AutoSave: Boolean read fAutoSave write fAutoSave;

    property Initialized: Boolean read fInitialized;
    property Editable: Boolean read fEditable write SetEditable;

    property Modified: Boolean read GetModified;

    property GUILayer: TelLayer read fGUILayer write fGUILayer;

    property Paused: Boolean read fPaused write fPaused;

    property Parent: TelSceneDirector read fParent write fParent;

    // Pause / Resume events
    property OnPause: TelEvent read fOnPause write fOnPause;
    property OnResume: TelEvent read fOnResume write fOnResume;
end;

TelSceneList = TelObjectList<TelScene>;

{ TelSceneDirector }

TelSceneDirector = class(TelObject)
  private
    fList: TelSceneList;
    fNullScene: TelScene;

    fActiveSceneID: Integer;

    function GetCount(): Integer; inline;
    function GetCurrentScene(): TelScene; inline;

    function Get(Index: Integer): TelScene; inline;
    procedure Put(Index: Integer; const Item: TelScene); inline;
    procedure PutS(Index: AnsiString; const Item: TelScene); inline;
    function GetS(Index: AnsiString): TelScene; inline;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    procedure Insert(Index: Integer; Scene: TelScene); inline;

    function Add(aScene: TelScene): Integer; Overload; inline;
    function Add(aScene: TelScene; aName: AnsiString): Integer; Overload; inline;

    procedure Delete(Index: Integer); inline;

    procedure SwitchTo(Index: Integer); Overload;
    procedure SwitchTo(aSceneName: AnsiString); Overload;

    procedure Render(Graphics: IGraphicsProvider); inline;
    procedure Update(dt: Double = 0.0); inline;
    procedure HandleEvents(); inline;

    property Items[Index: Integer]: TelScene read Get write Put; default;
    property Find[Index: AnsiString]: TelScene read GetS write PutS;
  published
    property Count: Integer read GetCount;

    property CurrentScene: TelScene read GetCurrentScene;
end;

implementation

{ TelScene }

constructor TelScene.Create;
begin
  inherited Create;

  fOnPause := nil;
  fOnResume := nil;

  fParent := nil;

  fNodeList := TelNodeList.Create;
  fEntityList := TelEntityList.Create;

  GUILayer := TelLayer.Create;

  fAutoSave := true;
end;

destructor TelScene.Destroy;
begin
  if AutoSave and Modified then
  begin
    // TODO: Save scene content if modified
  end;

  GUILayer.Destroy;

  fNodeList.Destroy;
  fEntityList.Destroy;

  inherited Destroy;
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
  fEntityList.Add(anEntity);
end;

procedure TelScene.Add(aNodeArray: array of TelNode);
var
  i: Integer;
begin
  for i := 0 to Length(aNodeArray) - 1 do
    fNodeList.Add(aNodeArray[i]);
end;

procedure TelScene.Add(anEntityArray: array of TelEntity); Overload;
var
  i: Integer;
begin
  for i := 0 to Length(anEntityArray) - 1 do
    fEntityList.Add(anEntityArray[i]);
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

procedure TelScene.Render(Graphics: IGraphicsProvider);
begin
  fNodeList.Draw(Graphics);
  fEntityList.Draw(Graphics);

  if GUILayer.Count > 0 then GUILayer.Draw(Graphics);
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
    fEntityList.Update(dt);

    fGUILayer.Update(dt);
  end;
end;

procedure TelScene.HandleEvents();
begin

end;


{ TelSceneDirector }

constructor TelSceneDirector.Create();
begin
  inherited Create;

  // Create empty scene
  fNullScene := TelScene.Create;

  fList := TelSceneList.Create;
  fActiveSceneID := -1;
end;

destructor TelSceneDirector.Destroy();
begin
  fList.Destroy;

  inherited Destroy;
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

procedure TelSceneDirector.PutS(Index: AnsiString; const Item: TelScene);
begin
  fList.Find[Index] := Item;
end;

function TelSceneDirector.GetS(Index: AnsiString): TelScene;
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
  aScene.Parent := Self;

  fActiveSceneID := newSceneID;
  Result := newSceneID;
end;

function TelSceneDirector.Add(aScene: TelScene; aName: AnsiString): Integer;
begin
  aScene.Name := aName;
  Result := Add(aScene);
end;

procedure TelSceneDirector.Delete(Index: Integer);
begin
  fList.Delete(Index);
end;

procedure TelSceneDirector.SwitchTo(Index: Integer);
begin
  fActiveSceneID := Index;
end;

procedure TelSceneDirector.SwitchTo(aSceneName: AnsiString);
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

procedure TelSceneDirector.Render(Graphics: IGraphicsProvider);
begin
  if CurrentScene = fNullScene then Exit
  else CurrentScene.Render(Graphics);
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


end.
