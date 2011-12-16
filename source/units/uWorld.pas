unit uWorld;

{$I Elysion.inc}

interface

uses
  ElysionObject;

type
  // Extend this class

  { TWorld }

  TWorld = class(TelObject)
  protected
    {$WARNINGS OFF}
    constructor Create; Override;
    {$WARNINGS ON}
  public
    class function GetInstance: TWorld;

    destructor Destroy; Override;
  end;

implementation

var
  World: TWorld;

{ TWorld }

constructor TWorld.Create;
begin
  inherited Create;
end;

class function TWorld.GetInstance: TWorld;
begin
  if (World = nil) then World := TWorld.Create;
  Result := World;
end;

destructor TWorld.Destroy;
begin
  inherited Destroy;
end;

end.

