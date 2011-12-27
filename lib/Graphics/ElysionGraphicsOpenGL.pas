unit ElysionGraphicsOpenGL;

interface

{$I Elysion.inc}

uses
  ElysionGraphicsProvider;

type
  TelGraphicsProviderLegacyOpenGL = class(TelContainer, IGraphicsProvider)
  public
    constructor Create;
    destructor Destroy;

  published
    property Name: AnsiString read fName write fName;
  end;

  TelGraphicsProviderOpenGL = class(TelContainer, IGraphicsProvider)

  end;

implementation

end.
