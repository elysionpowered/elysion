unit ElysionLabel;

interface

{$I Elysion.inc}

uses
  Classes,
  OpenURLUtil,

  ElysionTypes,
  ElysionUtils,

  ElysionNode,
  ElysionTrueTypeFont,

  ElysionApplication,
  ElysionInput;

type
  TelLabel = class(TelNode)
  protected
    fCaption: String;
    fStrings: TStringList;

    fRect: TelRect;

    fHyperLink: String;

    fFontContainer: TelTrueTypeFont;

    function GetWidth(): Integer; Override;
    function GetHeight(): Integer; Override;

    function GetMouseOver(): Boolean; Override;
    function GetClick(): Boolean; Override;

    function GetSize(): Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure SetSize(Value: Integer); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    procedure SetCaption(Value: String);
  public
    constructor Create; Override;
    destructor Destroy(); Override;

    procedure LoadFromFile(const aFilename: String); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure LoadFromFontContainer(aFontContainer: TelTrueTypeFont); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    procedure Draw(DrawChildren: Boolean = true); Override;
    procedure Update(dt: Double = 0.0); Override;
  published
    property Caption: String read fCaption write SetCaption;
    property FontContainer: TelTrueTypeFont read fFontContainer write fFontContainer;

    property HyperLink: String read fHyperLink write fHyperLink;

    property Size: Integer read GetSize write SetSize;
  end;

implementation

//
// TelLabel
//

constructor TelLabel.Create;
begin
  inherited;

  fRect.Clear();

  fFontContainer := TelTrueTypeFont.Create;
end;

destructor TelLabel.Destroy;
begin
  fFontContainer.Destroy;

  inherited;
end;

function TelLabel.GetWidth(): Integer;
begin
  Result := fFontContainer.GetWidth_Text(fCaption);
end;

function TelLabel.GetHeight(): Integer;
begin
  Result := fFontContainer.Height;
end;

function TelLabel.GetMouseOver(): Boolean;
begin
  inherited;

  fRect := makeRect(Position.X, Position.Y, GetWidth(), GetHeight());

  {$IFDEF CAN_METHODS}
    Result := fRect.ContainsVector(ActiveWindow.Cursor);
  {$ELSE}
    Result := RectContainsVector(fRect, ActiveWindow.Cursor);
  {$ENDIF}
end;

function TelLabel.GetClick(): Boolean;
begin
  inherited;

  Result := (Self.MouseOver and Input.Mouse.LeftClick);
end;

procedure TelLabel.SetSize(Value: Integer);
begin
  Self.FontContainer.Size := Value;
end;

function TelLabel.GetSize(): Integer;
begin
  Result := Self.FontContainer.Size;
end;

procedure TelLabel.SetCaption(Value: String);
begin
  fCaption := Value;

end;

procedure TelLabel.LoadFromFile(const aFilename: String);
begin
  if GetFilenameExtension(aFilename) = 'TTF' then
  begin

    fFontContainer.LoadFromFile(aFilename);
    fFontContainer.RenderStyle := rtBlended;
    fFontContainer.Size := 12;
  end;
end;

procedure TelLabel.LoadFromFontContainer(aFontContainer: TelTrueTypeFont);
begin
  if (aFontContainer <> nil) then
    fFontContainer := aFontContainer;
end;

procedure TelLabel.Draw();
begin
  if (fFontContainer <> nil) then
  begin
    fFontContainer.Color := Self.Color;
    fFontContainer.TextOut(Self.Position, Self.Caption);
  end;
end;

procedure TelLabel.Update(dt: Double = 0.0);
begin
  inherited;

  if (HyperLink <> '') and GetClick then OpenURL(HyperLink);
end;

end.
