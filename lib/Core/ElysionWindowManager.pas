(**
  *
  * @author(Johannes Stein and contributors <http://elysionpowered.org>)
  *
  *)
unit ElysionWindowManager;

{$I Elysion.inc}

interface

uses
  ElysionEnums,
  ElysionObject,
  ElysionWindow;

type

// Factory pattern

  { TelWindowManager }

  TelWindowManager = class sealed(TelStaticObject)
    private
      class var fWindowList: TelWindowList;

      class function GetWindowCount: Integer; static; inline;
      class function GetCurrentWindow: TelWindow; static; inline;
    public
      class constructor Create;
      class destructor Destroy;

      class function CreateWindow(const aName: AnsiString; Width, Height, Bits: Integer; Fullscreen: Boolean; VideoFlag: TelVideoFlag): TelWindow; static; Overload;
      class function CreateWindow(const aName: AnsiString; Width, Height, Bits: Integer; Fullscreen: Boolean = false): TelWindow; static; Overload; inline;
      class function DestroyWindow(aWindow: TelWindow): Boolean; static; inline;
      class procedure DestroyAllWindows(); static; inline;
    public
      class property CurrentWindow: TelWindow read GetCurrentWindow;
      class property WindowCount: Integer read GetWindowCount;
  end;

  function ActiveWindow: TelWindow; inline; deprecated 'Use TelWindowManager.CurrentWindow instead';
  function TicksNow(): Cardinal; inline;

implementation

function ActiveWindow: TelWindow;
begin
  Result := TelWindowManager.CurrentWindow;
end;

function TicksNow(): Cardinal;
begin
  Result := TelWindowManager.CurrentWindow.GetTicks();
end;

class constructor TelWindowManager.Create;
begin
  inherited;

  fWindowList := TelWindowList.Create;
end;

class destructor TelWindowManager.Destroy;
begin
  if fWindowList <> nil then DestroyAllWindows();

  inherited;
end;

class function TelWindowManager.GetCurrentWindow(): TelWindow;
begin
  Result := fWindowList.Items[0];
end;

class function TelWindowManager.GetWindowCount: Integer;
begin
  Result := fWindowList.Count;
end;

class function TelWindowManager.CreateWindow(const aName: AnsiString; Width, Height, Bits: Integer; Fullscreen: Boolean; VideoFlag: TelVideoFlag): TelWindow;
var
  tempWindow: TelWindow;
  tempName: AnsiString;
begin

  tempName := aName;
  if tempName = '' then tempName := 'Elysion Application';
  tempWindow := TelWindow.Create;
  tempWindow.ID := WindowCount;
  tempWindow.Name := tempName;
  tempWindow.SetVideoMode(Width, Height, Bits, Fullscreen, VideoFlag);

  fWindowList.Add(tempWindow);


  Result := tempWindow;
end;

class function TelWindowManager.CreateWindow(const aName: AnsiString; Width, Height, Bits: Integer; Fullscreen: Boolean = false): TelWindow;
begin
  Result := CreateWindow(aName, Width, Height, Bits, Fullscreen, vfAuto);
end;

class function TelWindowManager.DestroyWindow(aWindow: TelWindow): Boolean;
begin
  aWindow.Destroy;

  if aWindow = nil then Result := true
     else Result := false;
end;

class procedure TelWindowManager.DestroyAllWindows();
begin
  fWindowList.Clear;
end;

end.
