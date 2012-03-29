unit ElysionGroupedScene;

{$I Elysion.inc}

interface

uses
  ElysionScene;

type

{ TelGroupedScene }

TelGroupedScene = class(TelScene)
protected
  fDirector: TelSceneDirector;
public
  constructor Create; Override; Overload;
  constructor Create(aName: AnsiString); Overload;

  destructor Destroy; Override;

  procedure Render(); Override;
  procedure Update(dt: Double = 0.0); Override;
  procedure HandleEvents(); Override;
published
  property Director: TelSceneDirector read fDirector write fDirector;
end;

implementation

{ TelGroupedScene }

constructor TelGroupedScene.Create;
begin
  inherited Create;

  fDirector := TelSceneDirector.Create;
end;

constructor TelGroupedScene.Create(aName: AnsiString);
begin
  Create;

  Self.Name := aName;
end;

destructor TelGroupedScene.Destroy;
begin
  fDirector.Destroy;

  inherited Destroy;
end;

procedure TelGroupedScene.Render;
begin
  fDirector.Render;

  inherited;
end;

procedure TelGroupedScene.Update(dt: Double = 0.0);
begin
  inherited Update(dt);

  fDirector.Update(dt);
end;

procedure TelGroupedScene.HandleEvents;
begin
  inherited HandleEvents;

  fDirector.HandleEvents;
end;

end.
