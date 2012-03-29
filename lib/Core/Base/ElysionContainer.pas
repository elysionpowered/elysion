unit ElysionContainer;

{$I Elysion.inc}

interface

uses
  ElysionObject,
  ElysionInterfaces;

type

  { TelContainer }

  TelContainer = class(TelObject, IContainer)
  protected
    fDriverName: AnsiString;
    fIsInitialized, fIsFinalized: Boolean;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    function Initialize(): Boolean; virtual;
    procedure Finalize(); virtual;

    function IsInitialized: Boolean;
    function IsFinalized: Boolean;

    function GetDriverName: AnsiString;
  published
    property DriverName: AnsiString read GetDriverName;

    property Initialized: Boolean read IsInitialized;
    property Finalized: Boolean read IsFinalized;
  end;

  { TelStaticContainer }

  TelStaticContainer = class(TelObject)
  protected
    class var fIsInitialized, fIsFinalized: Boolean;
  public
    class function Initialize: Boolean; virtual;
    class procedure Finalize; virtual;
  public
    class property Initialized: Boolean read fIsInitialized;
    class property Finalized: Boolean read fIsFinalized;
  end;

implementation

{ TelStaticContainer }

class function TelStaticContainer.Initialize: Boolean;
begin
  fIsInitialized := Result;
end;

class procedure TelStaticContainer.Finalize;
begin
  fIsInitialized := false;
  fIsFinalized := false;
end;

{ TelContainer }

constructor TelContainer.Create;
begin
  inherited Create;
end;

destructor TelContainer.Destroy;
begin
  if (not Self.IsFinalized) then Self.Finalize();

  inherited Destroy;
end;

function TelContainer.Initialize: Boolean;
begin
  fIsInitialized := Result;
end;

procedure TelContainer.Finalize;
begin
  fIsInitialized := false;
  fIsFinalized := false;
end;

function TelContainer.IsInitialized: Boolean;
begin
  Result := fIsInitialized;
end;

function TelContainer.IsFinalized: Boolean;
begin
  Result := fIsFinalized;
end;

function TelContainer.GetDriverName: AnsiString;
begin
  Result := fDriverName;
end;

end.
