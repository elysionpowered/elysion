unit ElysionNode;

interface

{$I Elysion.inc}

uses
  Classes,
  ElysionTypes,
  ElysionApplication,
  ElysionInterfaces,
  ElysionTimer,
  ElysionObject;

type
  TelNode = class; //< forward declaration

  TelNodeEvent = procedure(aNode: TelNode; EventArgs: TelObject = nil) of object;

  { TelNode }

  TelNode = class(TelObject, INode)
  protected
    fAlign: TelAlignment;

      fParent: TelNode;

      fOnMouseDown: TelNodeEvent;
      fOnMouseUp: TelNodeEvent;
      fOnMouseMove: TelNodeEvent;
      fOnMouseOver: TelNodeEvent;
      fOnMouseOut: TelNodeEvent;
      fOnDragStart: TelNodeEvent;
      fOnDragging: TelNodeEvent;
      fOnDragEnd: TelNodeEvent;
      fOnClick: TelNodeEvent;
      fOnDblClick: TelNodeEvent;

      fVisible, fLocked, fDraggable, fIsAnimated: Boolean;

      fSelectorID, fSelectorClass: String;

      function GetAbsolutePosition(): TelVector3f; {$IFDEF CAN_INLINE} inline; {$ENDIF}

      function GetAlpha(): Byte; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure SetAlpha(anAlpha: Byte); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      procedure SetAlign(Value: TelAlignment); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      function GetWidth(): Integer; virtual;
      function GetHeight(): Integer; virtual;

      function GetMouseDown(): Boolean; virtual; abstract;
      function GetMouseUp(): Boolean; virtual; abstract;
      function GetMouseMove(): Boolean; virtual; abstract;
      function GetMouseOver(): Boolean; virtual; abstract;
      function GetMouseOut(): Boolean; virtual; abstract;
      function GetDragStart(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function GetDragging(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function GetDragEnd(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function GetClick(): Boolean; virtual; abstract;
      function GetRightClick(): Boolean; virtual; abstract;
      function GetDblClick(): Boolean; virtual; abstract;

      function GetLeft(): Single; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function GetTop(): Single; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function GetRight(): Single; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function GetBottom(): Single; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure SetLeft(Value: Single); {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure SetTop(Value: Single); {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure SetRight(Value: Single); {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure SetBottom(Value: Single); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      procedure SetLocked(Value: Boolean); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    public
      // Public methods
      constructor Create; Override;
      destructor Destroy; Override;

      procedure Add(aNode: TelNode); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      procedure Move(Delta: TelVector3f); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure Move(Delta: TelVector2i); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

      procedure Draw; virtual; abstract;

      function WriteToXML(): TStringList;
      function WriteToJSON(): TStringList;

      procedure Hover(MouseOverEvent, MouseOutEvent: TelNodeEvent);

      procedure Rotate(DeltaAngle: Single); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      // Simple Animation, for more complex animations (espacially key frame animations, multiple animations triggered at the same time and such) use ElysionAnimators.pas
      procedure Animate(AnimProperty: TelAnimationProperty; Duration: Integer = 1000; Delay: Integer = 0; Transition: TelAnimationTransition = atLinear);

      procedure Lock(); {$IFDEF CAN_INLINE} inline; {$ENDIF} //< Locks this node -> Update won't be called
      procedure UnLock(); {$IFDEF CAN_INLINE} inline; {$ENDIF} //< Unlocks this node
      
      procedure Update(dt: Double = 0.0); virtual;

    public
      // Public properties would be nicer though
      Position: TelVector3f;
      Offset: TelImageOffset;
      Rotation: TelImageRotation;
      Color: TelColor;
      Scale: TelVector2f;
	  
      Margin: TelRect;
      Shadow: TelShadow;

      property AbsolutePosition: TelVector3f read GetAbsolutePosition;
      property Align: TelAlignment read fAlign write SetAlign;
    published
      property Alpha: Byte read GetAlpha write SetAlpha default 255;

      // Event methods
      property OnMouseDown: TelNodeEvent read fOnMouseDown write fOnMouseDown;
      property OnMouseUp: TelNodeEvent read fOnMouseUp write fOnMouseUp;
      property OnMouseMove: TelNodeEvent read fOnMouseMove write fOnMouseMove;
      property OnMouseOver: TelNodeEvent read fOnMouseOver write fOnMouseOver;
      property OnMouseOut: TelNodeEvent read fOnMouseOut write fOnMouseOut;
      property OnDragStart: TelNodeEvent read fOnDragStart write fOnDragStart;
      property OnDragging: TelNodeEvent read fOnDragging write fOnDragging;
      property OnDragEnd: TelNodeEvent read fOnDragEnd write fOnDragEnd;
      property OnClick: TelNodeEvent read fOnClick write fOnClick;
      property OnDblClick: TelNodeEvent read fOnDblClick write fOnDblClick;

      property Parent: TelNode read fParent write fParent;

      // Event properties
      property MouseDown: Boolean read GetMouseDown;
      property MouseUp: Boolean read GetMouseUp;
      property MouseMove: Boolean read GetMouseMove;
      property MouseOver: Boolean read GetMouseOver;
      property MouseOut: Boolean read GetMouseOut;
      property DragStart: Boolean read GetDragStart;
      property Dragging: Boolean read GetDragging;
      property DragEnd: Boolean read GetDragEnd;
      property Click: Boolean read GetClick;
      property RightClick: Boolean read GetRightClick;
      property DblClick: Boolean read GetDblClick;

      // CSS-like selectors
      property SelectorID: String read fSelectorID write fSelectorID;
      property SelectorClass: String read fSelectorClass write fSelectorClass;

      property IsAnimated: Boolean read fIsAnimated;
      property Draggable: Boolean read fDraggable;

      property Width: Integer read GetWidth;
      property Height: Integer read GetHeight;

      property Locked: Boolean read fLocked write SetLocked;

      // Here is some alternative positionin' for ya (Use for UI elements)
      property Left: Single read GetLeft write SetLeft;
      property Top: Single read GetTop write SetTop;
      property Right: Single read GetRight write SetRight;
      property Bottom: Single read GetBottom write SetBottom;

      property Visible: Boolean read fVisible write fVisible default true;
  end;

  TelNodeArray = array of TelNode;

  TelNodeList = class(TelObject)
     private
      fNodeList: TList;

      function Get(Index: Integer): TelNode; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function GetPos(Index: String): Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure Put(Index: Integer; const Item: TelNode); {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure PutS(Index: String; const Item: TelNode); {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function GetS(Index: String): TelNode; {$IFDEF CAN_INLINE} inline; {$ENDIF}

      function GetCount: Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    public
      constructor Create; Override;
      destructor Destroy; Override;

      procedure Insert(Index: Integer; Node: TelNode); {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function Add(Node: TelNode): Integer; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function Add(NodeArray: TelNodeArray): Integer; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure Delete(Index: Integer); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      property Items[Index: Integer]: TelNode read Get write Put; default;
      property Find[Index: String]: TelNode read GetS write PutS;
    published
      property Count: Integer read GetCount;
  end;

  procedure CopyNodeValues(aNode, bNode: TelNode);
  procedure ForceNodeCopy(aNode, bNode: TelNode);

implementation

procedure CopyNodeValues(aNode, bNode: TelNode);
begin
  aNode.Position := bNode.Position;
  aNode.Offset := bNode.Offset;
  aNode.Rotation := bNode.Rotation;
  aNode.Color := bNode.Color;
  aNode.Scale := bNode.Scale;

  aNode.Align := bNode.Align;;

  aNode.Alpha := bNode.Alpha;

  aNode.Visible := bNode.Visible;
end;

procedure ForceNodeCopy(aNode, bNode: TelNode);
begin
  aNode.Position := bNode.Position;
  aNode.Offset := bNode.Offset;
  aNode.Rotation := bNode.Rotation;
  aNode.Color := bNode.Color;
  aNode.Scale := bNode.Scale;

  aNode.Align := bNode.Align;

  aNode.Alpha := bNode.Alpha;

  aNode.OnMouseDown := bNode.OnMouseDown;
  aNode.OnMouseUp := bNode.OnMouseUp;
  aNode.OnMouseMove := bNode.OnMouseMove;
  aNode.OnMouseOver := bNode.OnMouseOver;
  aNode.OnMouseOut := bNode.OnMouseOut;
  aNode.OnDragStart := bNode.OnDragStart;
  aNode.OnDragging := bNode.OnDragging;
  aNode.OnDragEnd := bNode.OnDragEnd;
  aNode.OnClick := bNode.OnClick;
  aNode.OnDblClick := bNode.OnDblClick;

  aNode.Parent := bNode.Parent;

  aNode.Visible := bNode.Visible;
end;

constructor TelNode.Create;
begin
  Position.Clear;
  Offset.Position.Clear;
  Offset.Rotation.Clear;

  Rotation.Angle := 0;
  Rotation.Vector := makeV3f(0.0, 0.0, 1.0);

  Color := makeCol(255, 255, 255, 255);
  Scale := makeV2f(1.0, 1.0);
  fVisible := true;

  fAlign.Horizontal := ahNone;
  fAlign.Vertical := avNone;

  fDraggable := true;
  fLocked := false;
  fIsAnimated := false;

  fParent := nil;

  OnMouseDown := nil;
  OnMouseUp := nil;
  OnMouseMove := nil;
  OnMouseOver := nil;
  OnMouseOut := nil;
  OnDragStart := nil;
  OnDragging := nil;
  OnDragEnd := nil;
  OnClick := nil;
  OnDblClick := nil;
end;

destructor TelNode.Destroy;
begin
  OnMouseDown := nil;
  OnMouseUp := nil;
  OnMouseMove := nil;
  OnMouseOver := nil;
  OnMouseOut := nil;
  OnDragStart := nil;
  OnDragging := nil;
  OnDragEnd := nil;
  OnClick := nil;
  OnDblClick := nil;
end;

function TelNode.GetDragStart(): Boolean;
begin
  Result := false;
  if (not Draggable) then Exit;
end;

function TelNode.GetDragging(): Boolean;
begin
  Result := false;
  if (not Draggable) then Exit;
end;

function TelNode.GetDragEnd(): Boolean;
begin
  Result := false;
  if (not Draggable) then Exit;
end;

procedure TelNode.SetAlign(Value: TelAlignment);
var
  parentWidth, parentHeight: Integer;
begin
  fAlign := Value;

  parentWidth := 0;
  parentHeight := 0;

  if fAlign.Horizontal <> ahNone then
  begin

    if (fParent = nil) then parentWidth := WindowManager.CurrentWindow.Width
      else parentWidth := Parent.Width;

    case fAlign.Horizontal of
      ahLeft: Position.X := 0;
      ahCenter: Position.X := (parentWidth - Self.Width) div 2;
      ahRight: Position.X := parentWidth - Self.Width;
    end;
  end;

  if fAlign.Vertical <> avNone then
  begin

    if (fParent = nil) then parentHeight := WindowManager.CurrentWindow.Height
      else parentHeight := Parent.Height;

    case fAlign.Vertical of
      avTop: Position.Y := 0;
      avCenter: Position.Y := (parentHeight - Self.Height) div 2;
      avBottom: Position.Y := parentHeight - Self.Height;
    end;
  end;
end;

function TelNode.GetWidth(): Integer;
begin
  Result := 0;
end;

function TelNode.GetHeight(): Integer;
begin
  Result := 0;
end;

function TelNode.GetAbsolutePosition(): TelVector3f;
begin
  if (fParent = nil) then Result := Self.Position
    else Result := fParent.GetAbsolutePosition();
end;

function TelNode.GetLeft(): Single;
begin
  Result := Position.X;
end;

function TelNode.GetTop(): Single;
begin
  Result := Position.Y;
end;

function TelNode.GetRight(): Single;
var
  parentWidth: Integer;
begin
  if (Self.Parent = nil) then parentWidth := ActiveWindow.Width
    else parentWidth := Self.Parent.Width;

  Result := (parentWidth - Self.Width - Position.X);
end;

function TelNode.GetBottom(): Single;
var
  parentHeight: Integer;
begin
  if (Self.Parent = nil) then parentHeight := ActiveWindow.Height
    else parentHeight := Self.Parent.Height;

  Result := (parentHeight - Self.Height - Position.Y);
end;

procedure TelNode.SetLeft(Value: Single);
begin
  Position.X := Value;
end;

procedure TelNode.SetTop(Value: Single);
begin
  Position.Y := Value;
end;

procedure TelNode.SetRight(Value: Single);
var
  parentWidth: Integer;
begin
  if (Self.Parent = nil) then parentWidth := ActiveWindow.Width
    else parentWidth := Self.Parent.Width;

  Position.X := parentWidth - Self.Width - Value;
end;

procedure TelNode.SetBottom(Value: Single);
var
  parentHeight: Integer;
begin
  if (Self.Parent = nil) then parentHeight := ActiveWindow.Width
    else parentHeight := Self.Parent.Width;

  Position.Y := parentHeight - Self.Height - Value;
end;

procedure TelNode.SetLocked(Value: Boolean);
begin
  if Value then Self.Lock()
  else Self.UnLock();
end;


procedure TelNode.Rotate(DeltaAngle: Single);
begin
  Rotation.Angle := Rotation.Angle + DeltaAngle;
end;

procedure TelNode.Add(aNode: TelNode);
begin
  aNode.Parent := Self;
end;

procedure TelNode.Lock();
begin
  if (not Locked) then fLocked := true;
end;

procedure TelNode.UnLock();
begin
  if (Locked) then fLocked := false;
end;

function TelNode.GetAlpha(): Byte;
begin
  Result := Color.A;
end;

procedure TelNode.SetAlpha(anAlpha: Byte);
begin
  Color.A := anAlpha;
end;

procedure TelNode.Move(Delta: TelVector3f);
begin
  Position.X := Position.X + Delta.X;
  Position.Y := Position.Y + Delta.Y;
  Position.Z := Position.Z + Delta.Z;
end;

procedure TelNode.Move(Delta: TelVector2i);
begin
  Position.X := Position.X + Delta.X;
  Position.Y := Position.Y + Delta.Y;
end;

function TelNode.WriteToXML(): TStringList;
var
  tmpStringList: TStringList;
begin
  tmpStringList := TStringList.Create;
  tmpStringList.Add('<' + Self.ClassName + '>');

  tmpStringList.Add(Position.ToKey('position')^.ToXML());
  tmpStringList.Add(Color.ToKey('color')^.ToXML());
  tmpStringList.Add(Scale.ToKey('scale')^.ToXML());

  tmpStringList.Add('</' + Self.ClassName + '>');

  Result := tmpStringList;
end;

function TelNode.WriteToJSON(): TStringList;
var
  tmpStringList: TStringList;
begin
  tmpStringList := TStringList.Create;
  tmpStringList.Add(Self.ClassName + ': {');

  tmpStringList.Add(Position.ToKey('position')^.ToJSON() + ',');
  tmpStringList.Add(Color.ToKey('color')^.ToJSON() + ',');
  tmpStringList.Add(Scale.ToKey('scale')^.ToJSON());

  tmpStringList.Add('}');

  Result := tmpStringList;
end;

procedure TelNode.Hover(MouseOverEvent, MouseOutEvent: TelNodeEvent);
begin
  OnMouseOver := MouseOverEvent;
  OnMouseOut := MouseOutEvent;
end;

procedure TelNode.Animate(AnimProperty: TelAnimationProperty;
  Duration: Integer; Delay: Integer; Transition: TelAnimationTransition);
begin
  if IsAnimated then Exit; //< Already animated? Exit here...
  
  
end;

procedure TelNode.Update(dt: Double = 0.0);
begin
  if Locked then Exit; //< Again: Locking means no Update :)

  if (Assigned(OnMouseDown)) then if MouseDown then OnMouseDown(Self);
  if (Assigned(OnMouseUp)) then if MouseUp then OnMouseUp(Self);
  if (Assigned(OnMouseMove)) then if MouseMove then OnMouseMove(Self);
  if (Assigned(OnMouseOver)) then if MouseOver then OnMouseOver(Self);
  if (Assigned(OnMouseOut)) then if MouseOut then OnMouseOut(Self);
  if (Assigned(OnDragStart)) then if DragStart then OnDragStart(Self);
  if (Assigned(OnDragging)) then if Dragging then OnDragging(Self);
  if (Assigned(OnDragEnd)) then if DragEnd then OnDragEnd(Self);
  if (Assigned(OnClick)) then if Click then OnClick(Self);
  if (Assigned(OnDblClick)) then if DblClick then OnDblClick(Self);
end;

constructor TelNodeList.Create;
begin
  inherited;

  fNodeList := TList.Create;
end;

destructor TelNodeList.Destroy;
var
  i: Integer;
begin
  for i := 0 to fNodeList.Count - 1 do
  begin
    TelNode(fNodeList[i]).Destroy;
  end;
  fNodeList.Free;

  inherited;
end;

function TelNodeList.GetCount: Integer;
begin
  Result := fNodeList.Count;
end;

function TelNodeList.Get(Index: Integer): TelNode;
begin
  if ((Index >= 0) and (Index <= fNodeList.Count - 1)) then Result := TelNode(fNodeList[Index]);
end;

function TelNodeList.GetPos(Index: String): Integer;
Var a, TMP: Integer;
Begin
  Try
    For a := 0 To fNodeList.Count - 1 Do
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

procedure TelNodeList.Put(Index: Integer; const Item: TelNode);
var
  TmpNode: TelNode;
begin
  if ((Index >= 0) and (Index <= fNodeList.Count - 1)) then
  begin
    TmpNode := Get(Index);
    TmpNode.Destroy;
    Insert(Index, Item);
  end;

end;

procedure TelNodeList.PutS(Index: String; const Item: TelNode);
var
  TMP: Integer;
  TmpNode: TelNode;
Begin
  if (Index <> '') then
  begin
    TmpNode := GetS(Index);
	if TmpNode <> nil then
	begin
	  TMP := GetPos(Index);
      TmpNode.Destroy;
      Insert(TMP, Item);
	end;
   end;
end;

function TelNodeList.GetS(Index: String): TelNode;
Var TMP: Integer;
Begin
  TMP := GetPos(Index);
  if TMP >= 0 then Result := TelNode(fNodeList[TMP])
			  else Result := nil;
end;

procedure TelNodeList.Insert(Index: Integer; Node: TelNode);
begin
  if ((Index >= 0) and (Index <= fNodeList.Count - 1)) then fNodeList.Insert(Index, Node);
end;

function TelNodeList.Add(Node: TelNode): Integer;
begin
  Result := fNodeList.Add(Node);
end;

function TelNodeList.Add(NodeArray: TelNodeArray): Integer;
var
  i: Integer;
begin
  for i := 0 to Length(NodeArray) - 1 do
    Result := fNodeList.Add(NodeArray[i]);
end;

procedure TelNodeList.Delete(Index: Integer);
var
  TmpNode: TelNode;
begin
  if ((Index >= 0) and (Index <= fNodeList.Count - 1)) then
  begin
    TmpNode := Get(Index);
    TmpNode.Destroy;
    fNodeList.Delete(Index);
  end;

end;


end.
