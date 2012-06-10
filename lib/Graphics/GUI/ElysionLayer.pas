unit ElysionLayer;

{$I Elysion.inc}

interface

uses
    Classes,

    ElysionNode,
    ElysionGraphicsProvider;


type

{ TelLayer }

TelLayer = class(TelNode)
  protected
    fNodeList: TelNodeList;

    function Get(Index: Integer): TelNode; inline;
    function GetCount: Integer; inline;
    function GetS(Index: String): TelNode; inline;
    procedure Put(Index: Integer; AValue: TelNode); inline;
    procedure PutS(Index: String; AValue: TelNode); inline;

    function GetMouseDown(): Boolean; Override;
    function GetMouseUp(): Boolean; Override;
    function GetMouseMove(): Boolean; Override;
    function GetMouseOver(): Boolean; Override;
    function GetMouseOut(): Boolean; Override;
    function GetDragStart(): Boolean; Override;
    function GetDragging(): Boolean; Override;
    function GetDragEnd(): Boolean; Override;
    function GetClick(): Boolean; Override;
    function GetRightClick(): Boolean; Override;
    function GetDblClick(): Boolean; Override;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    function Add(aNode: TelNode): Integer; inline;
    procedure Delete(Index: Integer); inline;

    procedure Draw(ctx: IGraphicsProvider; DrawChildren: Boolean = true); Override;
    procedure Update(dt: Double = 0.0); Override;
  published
    property Count: Integer read GetCount;

    property Items[Index: Integer]: TelNode read Get write Put; default;
    property Find[Index: String]: TelNode read GetS write PutS;
end;

implementation

{ TelLayer }

function TelLayer.Get(Index: Integer): TelNode;
begin
  Result := fNodeList.Items[Index];
end;

function TelLayer.GetCount: Integer;
begin
  Result := fNodeList.Count;
end;

function TelLayer.GetS(Index: String): TelNode;
begin
  Result := fNodeList.Find[Index];
end;

procedure TelLayer.Put(Index: Integer; AValue: TelNode);
begin
  fNodeList.Items[Index] := AValue;
end;

procedure TelLayer.PutS(Index: String; AValue: TelNode);
begin
  fNodeList.Find[Index] := AValue;
end;

function TelLayer.GetMouseDown(): Boolean;
var
  i: Integer;
begin
  inherited;

  for i := 0 to fNodeList.Count - 1 do
  begin
    if fNodeList.Items[i].MouseDown then
    begin
      Result := true;
      Exit;
    end;
  end;
end;

function TelLayer.GetMouseUp(): Boolean;
var
  i: Integer;
begin
  inherited;

  for i := 0 to fNodeList.Count - 1 do
  begin
    if fNodeList.Items[i].MouseUp then
    begin
      Result := true;
      Exit;
    end;
  end;
end;

function TelLayer.GetMouseMove(): Boolean;
var
  i: Integer;
begin
  inherited;

  for i := 0 to fNodeList.Count - 1 do
  begin
    if fNodeList.Items[i].MouseMove then
    begin
      Result := true;
      Exit;
    end;
  end;
end;

function TelLayer.GetMouseOver(): Boolean;
var
  i: Integer;
begin
  inherited;

  for i := 0 to fNodeList.Count - 1 do
  begin
    if fNodeList.Items[i].MouseOver then
    begin
      Result := true;
      Exit;
    end;
  end;

end;

function TelLayer.GetMouseOut(): Boolean;
var
  i: Integer;
begin
  inherited;

  for i := 0 to fNodeList.Count - 1 do
  begin
    if fNodeList.Items[i].MouseOut then
    begin
      Result := true;
      Exit;
    end;
  end;
end;

function TelLayer.GetDragStart(): Boolean;
var
  i: Integer;
begin
  inherited;

  for i := 0 to fNodeList.Count - 1 do
  begin
    if fNodeList.Items[i].DragStart then
    begin
      Result := true;
      Exit;
    end;
  end;

end;

function TelLayer.GetDragging(): Boolean;
var
  i: Integer;
begin
  inherited;

  for i := 0 to fNodeList.Count - 1 do
  begin
    if fNodeList.Items[i].Dragging then
    begin
      Result := true;
      Exit;
    end;
  end;
end;

function TelLayer.GetDragEnd(): Boolean;
var
  i: Integer;
begin
  inherited;

  for i := 0 to fNodeList.Count - 1 do
  begin
    if fNodeList.Items[i].DragEnd then
    begin
      Result := true;
      Exit;
    end;
  end;
end;

function TelLayer.GetClick(): Boolean;
var
  i: Integer;
begin
  inherited;

  for i := 0 to fNodeList.Count - 1 do
  begin
    if fNodeList.Items[i].Click then
    begin
      Result := true;
      Exit;
    end;
  end;
end;

function TelLayer.GetRightClick(): Boolean;
var
  i: Integer;
begin
  inherited;

  for i := 0 to fNodeList.Count - 1 do
  begin
    if fNodeList.Items[i].RightClick then
    begin
      Result := true;
      Exit;
    end;
  end;
end;

function TelLayer.GetDblClick(): Boolean;
var
  i: Integer;
begin
  inherited;

  for i := 0 to fNodeList.Count - 1 do
  begin
    if fNodeList.Items[i].DblClick then
    begin
      Result := true;
      Exit;
    end;
  end;
end;

constructor TelLayer.Create;
begin
  inherited;

  fNodeList := TelNodeList.Create;
end;

destructor TelLayer.Destroy;
begin
  fNodeList.Destroy;

  inherited;
end;

function TelLayer.Add(aNode: TelNode): Integer;
begin
  Result := fNodeList.Add(aNode);
end;

procedure TelLayer.Delete(Index: Integer);
begin
  fNodeList.Delete(Index);
end;

procedure TelLayer.Draw(ctx: IGraphicsProvider; DrawChildren: Boolean = true);
begin
  if not Self.Visible then Exit;

  fNodeList.Draw(ctx, DrawChildren);

  inherited Draw(ctx, DrawChildren);
end;

procedure TelLayer.Update(dt: Double = 0.0);
var
  i: Integer;
begin
  inherited;

  fNodeList.Update(dt);
end;

end.
