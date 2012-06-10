unit Elysion3DScene;

{$I Elysion.inc}

interface

uses
  ElysionApplication,
  ElysionScene;

type

{ Tel3DScene }

Tel3DScene = class(TelScene)
  public
    procedure Render2D(); virtual;
    procedure Render3D(); virtual;

    procedure Render(); Override;
end;

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

end.
