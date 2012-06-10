unit ElysionEvents;

interface

uses
  Classes;

type
  TelEvent = procedure() of object;

  TelEventList = class
    private
      fEventList: TList;
    public
      constructor Create;
      destructor Destroy;

      //procedure Add(anEvent: TelEvent);

  end;

implementation

constructor TelEventList.Create;
begin

end;

destructor TelEventList.Destroy;
begin

end;

end.
