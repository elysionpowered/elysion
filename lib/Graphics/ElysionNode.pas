unit ElysionNode;

interface

{$I Elysion.inc}

uses
  Classes,
  SysUtils,
  ElysionTypes,
  ElysionBorder,
  ElysionBounds,
  ElysionAnimTypes,
  ElysionApplication,
  ElysionInterfaces,
  ElysionTimer,
  ElysionObject,
  ElysionUtils;

type
  // If TelNode.Editable is turned on, it allows you to edit the node according
  // to TelNode.EditModes -> Practically this allows you edit nodes in your
  // application. If you are developing a game you don't need to develop
  // an editor as you could turn your game into an editor at any time
  TelNodeEditMode = (emMove, emRotate, emScale, emColor, emOrigin);
  TelNodeEditModes = set of TelNodeEditMode;

  TelNode = class; //< forward declaration
  TelNodeArray = array of TelNode;

  // Node event types
  TelNodeEvent = procedure(Sender: TelNode; EventArgs: TelObject = nil) of object;

  TelNodeEventObject = record
    Event: TelNodeEvent;
    Name: String;
    Enabled: Boolean;
  end;

  TelNodeEventObjectArray = array of TelNodeEventObject;

  { TelNodeEventListener }

  TelNodeEventListener = class(TelObject)
  protected
    fCount, fArrayLength: Integer;
    fEventArray: TelNodeEventObjectArray;

    function FindPos(aName: String): Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  public
    constructor Create; Override;
    destructor Destroy; Override;

    function Add(anEvent: TelNodeEvent; anEventName: String = ''): Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure Delete(Index: Integer); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure Delete(Index: String); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    procedure EnableEvent(Index: String); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure EnableEvent(Index: Integer); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    procedure DisableEvent(Index: String); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure DisableEvent(Index: Integer); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    procedure Execute(Sender: TelNode; EventArgs: TelObject = nil); {$IFDEF CAN_INLINE} inline; {$ENDIF}
  published
    property Count: Integer read fCount;
  end;


  TelNodeList = class;

  { TelNode }

  TelNode = class(TelObject, INode, IWritableData, IReadableData)
  protected
    fAlign: TelAlignment;
    fChildren: TelNodeList;

    fDecorations: TelElementDecorations;

    fBorder: TelBorder;
    fMargin: TelBounds;
    fPadding: TelBounds;

    fColor: TelColor;

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

    fWidth, fHeight: Integer;

      fVisible, fLocked, fDraggable, fDidDragStart, fDidDragging, fIsAnimated, fEditable, fEnabled: Boolean;

      fEditModes: TelNodeEditModes;

      function GetAbsolutePosition(): TelVector3f; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function GetParentPosition(): TelVector3f; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function GetParentSize(): TelSize; {$IFDEF CAN_INLINE} inline; {$ENDIF}

      function GetAlpha(): Byte; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure SetAlpha(anAlpha: Byte); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      procedure SetAlign(Value: TelAlignment); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      function GetColor(): TelColor; virtual;
      procedure SetColor(AValue: TelColor); virtual;

      function GetInnerWidth(): Integer; virtual;
      function GetInnerHeight(): Integer; virtual;
      function GetOuterWidthProp(): Integer;
      function GetOuterHeightProp(): Integer;

      function GetWidth(): Integer; virtual;
      function GetHeight(): Integer; virtual;
      procedure SetWidth(aValue: Integer); virtual;
      procedure SetHeight(aValue: Integer); virtual;

      function GetMouseDown(): Boolean; virtual;
      function GetMouseUp(): Boolean; virtual;
      function GetMouseMove(): Boolean; virtual;
      function GetMouseOver(): Boolean; virtual;
      function GetMouseOut(): Boolean; virtual;
      function GetDragStart(): Boolean; virtual;
      function GetDragging(): Boolean; virtual;
      function GetDragEnd(): Boolean; virtual;
      function GetClick(): Boolean; virtual;
      function GetRightClick(): Boolean; virtual;
      function GetDblClick(): Boolean; virtual;

      function GetLeft(): Single; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function GetTop(): Single; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function GetRight(): Single; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function GetBottom(): Single; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure SetLeft(Value: Single); {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure SetTop(Value: Single); {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure SetRight(Value: Single); {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure SetBottom(Value: Single); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      procedure SetLocked(Value: Boolean); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      function GetRelCursor(): TelVector2i;

      function GetModified(): Boolean;
    public
      // Public methods
      constructor Create; Override;
      destructor Destroy; Override;

      function Add(aNode: TelNode): Integer; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function Add(NodeArray: TelNodeArray): Integer; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure Delete(Index: Integer); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      procedure Move(aPoint: TelVector3f; dt: Double = 0.0); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure Move(aPoint: TelVector2f; dt: Double = 0.0); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure Move(aPoint: TelVector2i; dt: Double = 0.0); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

      function GetOuterWidth(Value: TelElementDecorations): Integer;
      function GetOuterHeight(Value: TelElementDecorations): Integer;

      procedure Draw(DrawChildren: Boolean = true); virtual;

      function LoadFromXML(aData: TStringList): Boolean;
      function LoadFromJSON(aData: TStringList): Boolean;

      function WriteToXML(): TStringList;
      function WriteToJSON(): TStringList;

      procedure Hover(MouseOverEvent, MouseOutEvent: TelNodeEvent);

      procedure Rotate(DeltaAngle: Single; dt: Double = 0.0); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      // Simple Animation, for more complex animations (espacially key frame animations, multiple animations triggered at the same time and such) use ElysionAnimators.pas
      procedure Animate(AnimProperty: TelAnimationProperty; Duration: Integer = 1000; Delay: Integer = 0; Transition: TelAnimationTransition = atLinear);

      procedure Lock(); {$IFDEF CAN_INLINE} inline; {$ENDIF} //< Locks this node -> Update won't be called
      procedure UnLock(); {$IFDEF CAN_INLINE} inline; {$ENDIF} //< Unlocks this node
      
      procedure Update(dt: Double = 0.0); virtual;

    public
      // Public properties would be nicer though
      Position: TelVector3f;
      Origin: TelVector2f;
      Rotation: TelImageRotation;
      Scale: TelVector2f;

      Shadow: TelShadow;

      property AbsolutePosition: TelVector3f read GetAbsolutePosition;
      property ParentPosition: TelVector3f read GetParentPosition;
      property ParentSize: TelSize read GetParentSize;

      property Align: TelAlignment read fAlign write SetAlign;
      property RelCursor: TelVector2i read GetRelCursor;

      property Color: TelColor read GetColor write SetColor;
    published
      property Alpha: Byte read GetAlpha write SetAlpha default 255;

      property Border: TelBorder read fBorder write fBorder;
      property Margin: TelBounds read fMargin write fMargin;
      property Padding: TelBounds read fPadding write fPadding;

      property Decorations: TelElementDecorations read fDecorations write fDecorations;

      property EditModes: TelNodeEditModes read fEditModes write fEditModes;

      property Modified: Boolean read GetModified;

      property Enabled: Boolean read fEnabled write fEnabled;

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

      property Children: TelNodeList read fChildren write fChildren;
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

      property IsAnimated: Boolean read fIsAnimated;
      property Draggable: Boolean read fDraggable;

      property Width: Integer read GetWidth write SetWidth;
      property Height: Integer read GetHeight write SetHeight;

      property InnerWidth: Integer read GetInnerWidth;
      property InnerHeight: Integer read GetInnerHeight;
      property OuterWidth: Integer read GetOuterWidthProp;
      property OuterHeight: Integer read GetOuterHeightProp;

      property Editable: Boolean read fEditable write fEditable default false;
      property Locked: Boolean read fLocked write SetLocked default false;

      // Here is some alternative positionin' for ya (Use for UI elements)
      property Left: Single read GetLeft write SetLeft;
      property Top: Single read GetTop write SetTop;
      property Right: Single read GetRight write SetRight;
      property Bottom: Single read GetBottom write SetBottom;

      property Visible: Boolean read fVisible write fVisible default true;
  end;

  // Node with CSS styling

  { TelNodeStyle }

  TelNodeStyle = class(TelObject)
  protected
    fStyleList: TStringList;

    function GetItem(Index: Integer): String;
    procedure SetItem(Index: Integer; const AValue: String);

    function GetCount(): Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  public
    constructor Create; Override;
    destructor Destroy; Override;

    procedure Add(const S: String); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure Insert(Index: Integer; const S: String); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure Delete(Index: Integer); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    procedure LoadFromFile(const aFilename: String); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure SaveToFile(const aFilename: String); {$IFDEF CAN_INLINE} inline; {$ENDIF}
  published
    property Count: Integer read GetCount;

    property Item[Index: Integer]: String read GetItem write SetItem; default;
  end;

  { TelStyledNode }

  TelStyledNode = class(TelNode)
  protected
    fSelectorID, fSelectorClass: String;

    fStyle: TelNodeStyle;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    procedure Apply(); Overload;
    procedure Apply(const S: String); Overload;
  published
    // CSS-like selectors
    property SelectorID: String read fSelectorID write fSelectorID;
    property SelectorClass: String read fSelectorClass write fSelectorClass;

    property Style: TelNodeStyle read fStyle write fStyle;
  end;

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

      // Draws all drawable nodes in the list
      procedure Draw(); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      // Updates all nodes in the list
      procedure Update(dt: Double = 0.0); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      property Items[Index: Integer]: TelNode read Get write Put; default;
      property Find[Index: String]: TelNode read GetS write PutS;
    published
      property Count: Integer read GetCount;
  end;

  procedure CopyNodeValues(aNode, bNode: TelNode);
  procedure ForceNodeCopy(aNode, bNode: TelNode);
  function Center(aNode: TelNode): TelVector2f; {$IFDEF CAN_INLINE} inline; {$ENDIF}

implementation

procedure CopyNodeValues(aNode, bNode: TelNode);
begin
  aNode.Position := bNode.Position;
  aNode.Origin := bNode.Origin;

  aNode.Margin := bNode.Margin;
  aNode.Padding := bNode.Padding;
  aNode.Border := bNode.Border;
  aNode.Shadow := bNode.Shadow;

  aNode.Rotation := bNode.Rotation;
  aNode.Color := bNode.Color;
  aNode.Scale := bNode.Scale;

  aNode.Align := bNode.Align;

  aNode.Alpha := bNode.Alpha;

  aNode.Visible := bNode.Visible;
end;

procedure ForceNodeCopy(aNode, bNode: TelNode);
begin
  aNode.Position := bNode.Position;
  aNode.Origin := bNode.Origin;

  aNode.Margin := bNode.Margin;
  aNode.Padding := bNode.Padding;
  aNode.Border := bNode.Border;
  aNode.Shadow := bNode.Shadow;

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

  aNode.Visible := bNode.Visible;
end;

function Center(aNode: TelNode): TelVector2f;
begin
  Result := makeV2f(aNode.Width / 2, aNode.Height / 2);
end;

{ TelNodeEventListener }

constructor TelNodeEventListener.Create;
begin
  inherited Create;

  fCount := 0;
  fArrayLength := 0;
end;

destructor TelNodeEventListener.Destroy;
begin
  inherited Destroy;

end;

function TelNodeEventListener.FindPos(aName: String): Integer;
var
  i: Integer;
begin
  for i := 0 to fArrayLength - 1 do
  begin

    // Yeah, yeah, string comparisons are evil and slow... I know
    if (fEventArray[i].Name = aName) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TelNodeEventListener.Add(anEvent: TelNodeEvent; anEventName: String = ''): Integer;
begin
  fArrayLength := fArrayLength + 1;
  fCount := fCount + 1;
  SetLength(fEventArray, fArrayLength);

  fEventArray[fCount - 1].Event := anEvent;
  fEventArray[fCount - 1].Name := anEventName;
  fEventArray[fCount - 1].Enabled := true;
end;

procedure TelNodeEventListener.Delete(Index: Integer);
begin
  fEventArray[Index].Event := nil;
  fEventArray[Index].Name := '';
  fEventArray[Index].Enabled := false;
  fCount := fCount - 1;
end;

procedure TelNodeEventListener.Delete(Index: String);
begin
  Delete(FindPos(Index));
end;

procedure TelNodeEventListener.EnableEvent(Index: Integer);
begin
  fEventArray[Index].Enabled := true;
end;

procedure TelNodeEventListener.EnableEvent(Index: String);
begin
  EnableEvent(FindPos(Index));
end;

procedure TelNodeEventListener.DisableEvent(Index: Integer);
begin
  fEventArray[Index].Enabled := false;
end;

procedure TelNodeEventListener.DisableEvent(Index: String);
begin
  DisableEvent(FindPos(Index));
end;

procedure TelNodeEventListener.Execute(Sender: TelNode; EventArgs: TelObject);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if (fEventArray[i].Enabled) then fEventArray[i].Event(Sender, EventArgs);
  end;
end;

{ TelNodeStyle }

constructor TelNodeStyle.Create;
begin
  inherited Create;

  fStyleList := TStringList.Create;
end;

destructor TelNodeStyle.Destroy;
begin
  fStyleList.Free;

  inherited Destroy;
end;

function TelNodeStyle.GetItem(Index: Integer): String;
begin
  Result := fStyleList.Strings[Index];
end;

procedure TelNodeStyle.SetItem(Index: Integer; const AValue: String);
begin
  fStyleList.Strings[Index] := AValue;
end;

function TelNodeStyle.GetCount(): Integer;
begin
  Result := fStyleList.Count;
end;

procedure TelNodeStyle.Add(const S: String);
begin
  fStyleList.Add(S);
end;

procedure TelNodeStyle.Insert(Index: Integer; const S: String);
begin
  fStyleList.Insert(Index, S);
end;

procedure TelNodeStyle.Delete(Index: Integer);
begin
  fStyleList.Delete(Index);
end;

procedure TelNodeStyle.LoadFromFile(const aFilename: String);
begin
  fStyleList.LoadFromFile(aFilename);
end;

procedure TelNodeStyle.SaveToFile(const aFilename: String);
begin
  fStyleList.SaveToFile(aFilename);
end;

{ TelStyleNode }

constructor TelStyledNode.Create;
begin
  inherited;

  fStyle := TelNodeStyle.Create;
end;

destructor TelStyledNode.Destroy;
begin
  fStyle.Destroy;

  inherited;
end;

procedure TelStyledNode.Apply();
var
  i: Integer;
begin
  for i := 0 to Style.Count do Apply(Style.Item[i]);
end;

procedure TelStyledNode.Apply(const S: String);
var
  AttrName, AttrValue: String;
begin

  // Parse
  //if S = 'left' then Self.Left := AttrValue;
end;

{ TelNode }

constructor TelNode.Create;
begin
  inherited;

  Position.Clear;
  Origin.Clear();

  fMargin := TelBounds.Create;
  fPadding := TelBounds.Create;
  fBorder := TelBorder.Create;

  Shadow.Clear();

  fDecorations := [edMargin, edPadding, edBorder];

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

  fEditable := false;

  fEditModes := [emMove, emRotate, emScale, emColor, emOrigin];


  fDidDragStart := false;
  fDidDragging := false;

  fParent := nil;
  fChildren := TelNodeList.Create;

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

  fParent := nil;
  fChildren.Destroy;

  fMargin.Destroy;
  fPadding.Destroy;
  fBorder.Destroy;

  inherited;
end;

function TelNode.GetRelCursor(): TelVector2i;
begin
  Result := makeV2i(Trunc(Self.Left - ActiveWindow.Cursor.X), Trunc(Self.Top - ActiveWindow.Cursor.Y));
end;

function TelNode.GetModified(): Boolean;
begin

end;

function TelNode.GetMouseDown(): Boolean;
begin
  Result := false;
  if not Enabled then Exit;
end;

function TelNode.GetMouseUp(): Boolean;
begin
  Result := false;
  if not Enabled then Exit;
end;

function TelNode.GetMouseMove(): Boolean;
begin
  Result := false;
  if not Enabled then Exit;
end;

function TelNode.GetMouseOver(): Boolean;
begin
  Result := false;
  if not Enabled then Exit;
end;

function TelNode.GetMouseOut(): Boolean;
begin
  Result := false;
  if not Enabled then Exit;
end;

function TelNode.GetDragStart(): Boolean;
begin
  Result := false;
  if (not Draggable) then Exit;
end;

function TelNode.GetDragging(): Boolean;
begin
  Result := false;
  if (not Draggable) or (not Enabled) then Exit;
end;

function TelNode.GetDragEnd(): Boolean;
begin
  Result := false;
  if (not Draggable) or (not Enabled) then Exit;
end;

function TelNode.GetClick(): Boolean;
begin
  Result := false;
end;

function TelNode.GetRightClick(): Boolean;
begin
  Result := false;
end;

function TelNode.GetDblClick(): Boolean;
begin
  Result := false;
end;

procedure TelNode.SetAlign(Value: TelAlignment);
begin
  fAlign := Value;

  if fAlign.Horizontal <> ahNone then
  begin
    case fAlign.Horizontal of
      ahLeft: Left := 0;
      ahCenter: Position.X := (ParentSize.Width - Self.Width) div 2;
      ahRight: Right := 0;
    end;
  end;

  if fAlign.Vertical <> avNone then
  begin
    case fAlign.Vertical of
      avTop: Top := 0;
      avCenter: Position.Y := (ParentSize.Height - Self.Height) div 2;
      avBottom: Bottom := 0;
    end;
  end;
end;

function TelNode.GetInnerWidth(): Integer;
begin
  Result := 0;
end;

function TelNode.GetInnerHeight(): Integer;
begin
  Result := 0;
end;

function TelNode.GetOuterWidthProp(): Integer;
begin
  Result := GetOuterWidth(fDecorations);
end;

function TelNode.GetOuterHeightProp(): Integer;
begin
  Result := GetOuterHeight(fDecorations);
end;

function TelNode.GetWidth(): Integer;
begin
  Result := 0;
end;

function TelNode.GetHeight(): Integer;
begin
  Result := 0;
end;

procedure TelNode.SetWidth(aValue: Integer);
begin
  fWidth := aValue;
end;

procedure TelNode.SetHeight(aValue: Integer);
begin
  fHeight := aValue;
end;

function TelNode.GetColor(): TelColor;
begin
  Result := fColor;
end;

procedure TelNode.SetColor(AValue: TelColor);
begin
  fColor := AValue;
end;

function TelNode.GetAbsolutePosition(): TelVector3f;
var
  curPos: TelVector3f;
begin
  if (fParent = nil) then Result := Self.Position
    else begin
      (*if fParent.InheritsFrom(TelCamera) then
      begin
        curPos := Self.Position;
        curPos.X := curPos.X + TelCamera(fParent).Viewport.X;
        curPos.Y := curPos.Y + TelCamera(fParent).Viewport.Y;

        //Result := Position.Add(makeV3f(, TelCamera(fParent).Viewport.Y, 0));
        Result := curPos;
      end else*)
        Result := makeV3f(Self.Position.X + Parent.AbsolutePosition.X,
                          Self.Position.Y + Parent.AbsolutePosition.Y,
                          Self.Position.Z + Parent.AbsolutePosition.Z);

    end;
end;

function TelNode.GetParentPosition(): TelVector3f;
begin
  if (Parent = nil) then Result := makeV3f(0.0, 0.0, 0.0)
  else Result := Parent.GetAbsolutePosition();
end;

function TelNode.GetParentSize(): TelSize;
begin
  if (Parent = nil) then Result := makeSize(WindowManager.CurrentWindow.Width, WindowManager.CurrentWindow.Height)
  else Result := makeSize(Parent.Width, Parent.Height);
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
begin
  Result := (ParentSize.Width - Self.Width - Position.X);
end;

function TelNode.GetBottom(): Single;
begin
  Result := (ParentSize.Height - Self.Height - Position.Y);
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
begin
  Position.X := ParentSize.Width - Self.Width - Value;
end;

procedure TelNode.SetBottom(Value: Single);
begin
  Position.Y := ParentSize.Height - Self.Height - Value;
end;

procedure TelNode.SetLocked(Value: Boolean);
begin
  if Value then Self.Lock()
  else Self.UnLock();
end;


procedure TelNode.Rotate(DeltaAngle: Single; dt: Double = 0.0);
begin
  if dt = 0.0 then
    Rotation.Angle := Rotation.Angle + DeltaAngle
  else
    Rotation.Angle := Rotation.Angle + DeltaAngle * dt;
end;

function TelNode.Add(aNode: TelNode): Integer;
begin
  aNode.Parent := Self;

  Result := fChildren.Add(aNode);
end;

function TelNode.Add(NodeArray: TelNodeArray): Integer;
begin
  Result := fChildren.Add(NodeArray);
end;

procedure TelNode.Delete(Index: Integer);
begin
  fChildren.Delete(Index);
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

function TelNode.GetOuterWidth(Value: TelElementDecorations): Integer;
var
  tmpWidth: Integer;

  marLeft, marRight: Single;
  borLeft, borRight: Single;
  padLeft, padRight: Single;
begin
  tmpWidth := GetInnerHeight();

  if edMargin in Value then
  begin
    marLeft := Margin.Left;
    marRight := Margin.Right;
  end else
  begin
    marLeft := 0;
    marRight := 0;
  end;

  if edBorder in Value then
  begin
    borLeft := Border.Left.Width;
    borRight := Border.Right.Width;
  end else
  begin
    borLeft := 0;
    borRight := 0;
  end;

  if edPadding in Value then
  begin
    padLeft := Padding.Left;
    padRight := Padding.Right;
  end else
  begin
    padLeft := 0;
    padRight := 0;
  end;

  Result := Trunc(tmpWidth + marLeft + marRight + borLeft + borRight + padLeft + padRight);
end;

function TelNode.GetOuterHeight(Value: TelElementDecorations): Integer;
var
  tmpHeight: Integer;

  marTop, marBottom: Single;
  borTop, borBottom: Single;
  padTop, padBottom: Single;
begin
  tmpHeight := GetInnerHeight();

  if edMargin in Value then
  begin
    marTop := Margin.Top;
    marBottom := Margin.Bottom;
  end else
  begin
    marTop := 0;
    marBottom := 0;
  end;

  if edBorder in Value then
  begin
    borTop := Border.Top.Width;
    borBottom := Border.Bottom.Width;
  end else
  begin
    borTop := 0;
    borBottom := 0;
  end;

  if edPadding in Value then
  begin
    padTop := Padding.Top;
    padBottom := Padding.Bottom;
  end else
  begin
    padTop := 0;
    padBottom := 0;
  end;

  Result := Trunc(tmpHeight + marTop + marBottom + borTop + borBottom + padTop + padBottom);
end;

procedure TelNode.Move(aPoint: TelVector2f; dt: Double = 0.0);
begin
  if dt = 0.0 then
    Position.Add(makeV3f(aPoint.X, aPoint.Y, 0.0))
  else
    Position.Add(makeV3f(aPoint.X * dt, aPoint.Y * dt, 0.0));
end;

procedure TelNode.Move(aPoint: TelVector2i; dt: Double = 0.0);
begin
  if dt = 0.0 then
    Position.Add(makeV3f(aPoint.X, aPoint.Y, 0.0))
  else
    Position.Add(makeV3f(aPoint.X * dt, aPoint.Y * dt, 0.0));
end;

procedure TelNode.Move(aPoint: TelVector3f; dt: Double = 0.0);
begin
  if dt = 0.0 then
    Position.Add(aPoint)
  else
    Position.Add(makeV3f(aPoint.X * dt, aPoint.Y * dt, aPoint.Z * dt));
end;

function TelNode.LoadFromXML(aData: TStringList): Boolean;
begin
  if aData = nil then
  begin
    Result := false;
    Exit;
  end;
end;

function TelNode.LoadFromJSON(aData: TStringList): Boolean;
begin
  if aData = nil then
  begin
    Result := false;
    Exit;
  end;
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

procedure TelNode.Draw(DrawChildren: Boolean = true);
begin
  if DrawChildren then
    Children.Draw();
end;

procedure TelNode.Update(dt: Double = 0.0);
var
  i: Integer;
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

  Children.Update(dt);
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

procedure TelNodeList.Draw();
var
  i: Integer;
begin
  if Count > 0 then
  begin
    for i := 0 to Count - 1 do
      if (Items[i] <> nil) then Items[i].Draw;
  end;
end;

procedure TelNodeList.Update(dt: Double = 0.0);
var
  i: Integer;
begin
  if Count > 0 then
  begin
    for i := 0 to Count - 1 do Items[i].Update(dt);
  end;
end;

end.
