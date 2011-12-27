unit ElysionGraphicsProvider;

interface

{$I Elysion.inc}

uses
  ElysionTypes;

type
  IGraphicsProvider = interface
    procedure DrawPoint(aPoint: TelVector3f; aColor: TelColor; Options: PelRenderOptions = nil);

    procedure DrawLine(Src, Dst: TelVector3f; Color: TelColor; LineWidth: Single = 1.0; Options: PelRenderOptions = nil); Overload;
    procedure DrawLine(Points: array of TelVector3f; Colors: array of TelColor; LineWidth: Single = 1.0; Options: PelRenderOptions = nil); Overload;

    procedure DrawPolygon(Vertices: array of TelVector3f; aColor: TelColor; Options: PelRenderOptions = nil); Overload;
    procedure DrawPolygon(Vertices: array of TelVector3f; Colors: array of TelColor; Options: PelRenderOptions = nil); Overload;

    procedure DrawQuad(aRect: TelRect; aColor: TelColor; anOutline: Boolean = false; Options: PelRenderOptions = nil); Overload;
    procedure DrawQuad(aRect: TelRect; aColor: TelColorVertices; anOutline: Boolean = false; Options: PelRenderOptions = nil); Overload;

    procedure DrawRoundedRect(aRect: TelRect; aColor: TelColor; aRoundedRadius: Single; Options: PelRenderOptions = nil);

    procedure DrawTexture(aRect: TelRect; aColor: TelColor; Options: PelRenderOptions = nil); Overload;
    procedure DrawTexture(aRect: TelRect; aColor: TelColorVertices; aTexture: Pointer; aTexCoords: TelRect; Options: PelRenderOptions = nil); Overload;
  end;

implementation

end.
