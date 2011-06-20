unit ElysionNode;

interface

{$I Elysion.inc}

uses
  Classes,
  SysUtils,
  ElysionTypes,
  ElysionApplication,
  ElysionInterfaces,
  ElysionTimer,
  ElysionObject,
  ElysionUtils;

type
  // Property data types for nodes
  // This is primarily used for Stylesheets and animators
  // but can and should be integrated into a scripting engine
  // This is primarily for determing how strings will be mapped to properties

                    // Simple data types
  TelNodeProperty = (npUnknown, npLeft, npTop, npRight, npBottom, npVisible, npAlpha, npWidth, npHeight,
                    // Semi-advanced data types ("Wrapper" for advanced data types, such as "Position.X", "Scale.X" or "padding-left")
                    // Position
                    npPosX, npPosY, npPosZ,
                    // Scale
                    npScaleX, npScaleY,
                    // Border
                    npBorderL, //< Left border
                    npBorderT, //< Top border
                    npBorderR, //< Right border
                    npBorderB, //< Bottom border
                    // Padding
                    npPaddingL, //< Left padding
                    npPaddingT, //< Top padding
                    npPaddingR, //< Right padding
                    npPaddingB, //< Bottom padding
                    // Margin
                    npMarginL, //< Left padding
                    npMarginT, //< Top padding
                    npMarginR, //< Right padding
                    npMarginB, //< Bottom padding
                    // Rotation
                    npRotX, npRotY, npRotZ,
                    // Shadow
                    npShadowBlur,
                    npShadowPosX, npShadowPosY,
                    npShadowColorR, npShadowColorG, npShadowColorB,
                    // Color
                    npColorR, npColorG, npColorB,
                    // Origin
                    npOriginX, npOriginY,
                    // Advanced data types
                    npPosition, npScale, npBorder, npPadding, npMargin, npRotation, npShadow, npColor, npOrigin);
  TelNodeProperties = set of TelNodeProperty;


  // If TelNode.Editable is turned on, it allows you to edit the node according
  // to TelNode.EditModes -> Practically this allows you edit nodes in your
  // application. If you are developing a game you don't need to develop
  // an editor as you could turn your game into an editor at any time
  TelNodeEditMode = (emMove, emRotate, emScale, emColor, emOrigin);
  TelNodeEditModes = set of TelNodeEditMode;

  TelNode = class; //< forward declaration

  // Event types
  // This is used to determine how strings will be mapped to events
  TelNodeEventType = (etUnknown, etCustom, etMouseDown, etMouseUp, etMouseMove, etMouseOver, etMouseOut, etDragStart, etDragging, etDragEnd, etClick, etDblClick);
  TelNodeEventTypes = set of TelNodeEventType;

  TelNodeEvent = procedure(aNode: TelNode; EventArgs: TelObject = nil) of object;

  { TelNode }

  TelNode = class(TelObject, INode)
  protected
    fAlign: TelAlignment;

    fDecorations: TelElementDecorations;
    fReadProps, fWriteProps: TelNodeProperties;
    fEventTypes: TelNodeEventTypes;

    fBorder: TelBorder;
    fMargin: TelExtValue;
    fPadding: TelExtValue;

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

      fVisible, fLocked, fDraggable, fDidDragStart, fDidDragging, fIsAnimated, fDrawable, fEditable: Boolean;

      fEditModes: TelNodeEditModes;

      function GetAbsolutePosition(): TelVector3f; {$IFDEF CAN_INLINE} inline; {$ENDIF}

      function GetAlpha(): Byte; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure SetAlpha(anAlpha: Byte); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      procedure SetAlign(Value: TelAlignment); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      function GetInnerWidth(): Integer; virtual;
      function GetInnerHeight(): Integer; virtual;
      function GetOuterWidthProp(): Integer;
      function GetOuterHeightProp(): Integer;

      function GetWidth(): Integer; virtual;
      function GetHeight(): Integer; virtual;

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

      function IsPropertyValid(aPropName: String): TelNodeProperties;
      function IsEventValid(anEventName: String): TelNodeEventTypes;

      function GetModified(): Boolean;
    public
      // Public methods
      constructor Create; Override;
      destructor Destroy; Override;

      procedure Add(aNode: TelNode); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      procedure Move(Delta: TelVector3f); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure Move(Delta: TelVector2i); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

      function GetOuterWidth(Value: TelElementDecorations): Integer;
      function GetOuterHeight(Value: TelElementDecorations): Integer;

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
      Origin: TelVector2f;
      Rotation: TelImageRotation;
      Color: TelColor;
      Scale: TelVector2f;

      Shadow: TelShadow;

      property AbsolutePosition: TelVector3f read GetAbsolutePosition;
      property Align: TelAlignment read fAlign write SetAlign;
      property RelCursor: TelVector2i read GetRelCursor;

      property ReadProps: TelNodeProperties read fReadProps write fReadProps;
      property WriteProps: TelNodeProperties read fWriteProps write fWriteProps;

      property EventTypes: TelNodeEventTypes read fEventTypes write fEventTypes;
    published
      property Alpha: Byte read GetAlpha write SetAlpha default 255;

      property Border: TelBorder read fBorder write fBorder;
      property Margin: TelExtValue read fMargin write fMargin;
      property Padding: TelExtValue read fPadding write fPadding;

      property Decorations: TelElementDecorations read fDecorations write fDecorations;

      property EditModes: TelNodeEditModes read fEditModes write fEditModes;

      property Modified: Boolean read GetModified;

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

      property IsAnimated: Boolean read fIsAnimated;
      property Draggable: Boolean read fDraggable;

      property Width: Integer read GetWidth;
      property Height: Integer read GetHeight;

      property InnerWidth: Integer read GetInnerWidth;
      property InnerHeight: Integer read GetInnerHeight;
      property OuterWidth: Integer read GetOuterWidthProp;
      property OuterHeight: Integer read GetOuterHeightProp;

      property Editable: Boolean read fEditable write fEditable default false;
      property Drawable: Boolean read fDrawable default false;
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
  function Center(aNode: TelNode): TelVector2f; {$IFDEF CAN_INLINE} inline; {$ENDIF}

  function GetSimpleProperties(): TelNodeProperties; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function GetSemiAdvProperties(): TelNodeProperties; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function GetAdvProperties(): TelNodeProperties; {$IFDEF CAN_INLINE} inline; {$ENDIF}

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

  aNode.Parent := bNode.Parent;

  aNode.Visible := bNode.Visible;
end;

function Center(aNode: TelNode): TelVector2f;
begin
  Result := makeV2f(aNode.Width / 2, aNode.Height / 2);
end;

function GetSimpleProperties(): TelNodeProperties;
begin
  Result := [npLeft, npTop, npRight, npBottom, npVisible, npAlpha, npWidth, npHeight];
end;

function GetSemiAdvProperties(): TelNodeProperties;
begin
  Result := [npPosX, npPosY, npPosZ, npScaleX, npScaleY, npBorderL, npBorderT,
    npBorderR, npBorderB, npPaddingL, npPaddingT, npPaddingR, npPaddingB, npMarginL,
    npMarginT, npMarginR, npMarginB, npRotX, npRotY, npRotZ, npShadowBlur, npShadowPosX,
    npShadowPosY, npShadowColorR, npShadowColorG, npShadowColorB, npColorR, npColorG,
    npColorB, npOriginX, npOriginY];
end;

function GetAdvProperties(): TelNodeProperties;
begin
  Result := [npPosition, npScale, npBorder, npPadding, npMargin, npRotation, npShadow, npColor, npOrigin];
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

  fMargin := TelExtValue.Create;
  fPadding := TelExtValue.Create;
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

  fDrawable := false;
  fEditable := false;

  fEditModes := [emMove, emRotate, emScale, emColor, emOrigin];

  fReadProps := GetSimpleProperties() + GetSemiAdvProperties() + GetAdvProperties();
  fWriteProps := GetSimpleProperties() + GetSemiAdvProperties();

  fEventTypes := [etMouseDown, etMouseUp, etMouseMove, etMouseOver, etMouseOut, etDragStart, etDragging, etDragEnd, etClick, etDblClick];

  fDidDragStart := false;
  fDidDragging := false;

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

  fMargin.Destroy;
  fPadding.Destroy;
  fBorder.Destroy;

  inherited;
end;

function TelNode.GetRelCursor(): TelVector2i;
begin
  Result := makeV2i(Trunc(Self.Left - ActiveWindow.Cursor.X), Trunc(Self.Top - ActiveWindow.Cursor.Y));
end;

function TelNode.IsPropertyValid(aPropName: String): TelNodeProperties;
begin

end;

function TelNode.IsEventValid(anEventName: String): TelNodeEventTypes;
begin

end;

function TelNode.GetModified(): Boolean;
begin

end;

function IsValidProperty(aPropName: String): TelNodeProperties;
var
  tmpString: String;
  tmpStringList: TStringList;
begin
  tmpString := LowerCase(aPropName);

  tmpStringList := Split(tmpString, ',', true);
end;

function TelNode.GetMouseDown(): Boolean;
begin
  Result := false;
end;

function TelNode.GetMouseUp(): Boolean;
begin
  Result := false;
end;

function TelNode.GetMouseMove(): Boolean;
begin
  Result := false;
end;

function TelNode.GetMouseOver(): Boolean;
begin
  Result := false;
end;

function TelNode.GetMouseOut(): Boolean;
begin
  Result := false;
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
