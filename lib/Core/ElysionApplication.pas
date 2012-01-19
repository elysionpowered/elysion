(**
  * ElysionApplication.pas
  *
  * Handles application container and windows
  *
  * @author(Johannes Stein and contributors <http://elysionpowered.org>)
  *
  *)
unit ElysionApplication;

{$I Elysion.inc}

interface

uses
  ElysionStrings,
  ElysionTypes,
  ElysionEnums,
  ElysionObject,
  ElysionContent,
  ElysionLogger,

  SDL,

  SysUtils,
  Classes;

type

{
    @classname @br
    Description: @br
    Provides an application container

}

{ TelApplication }

TelApplication = class(TelObject)
  protected
    {
        Application.GetUnicodeSupport @br
        Unicode support
        @return

    }
    function GetUnicodeSupport: Boolean; inline;

    {
        Application.SetUnicodeSupport

    }
    procedure SetUnicodeSupport(Value: Boolean); inline;

    function IsRun: Boolean;
  public
    {
      Application.Create


      Returns: Instance of class Application
    }
    constructor Create; Override;

    {
      Application.Destroy


    }
    destructor Destroy; Override;

    {
      Application.CreateWindow
      @param Name: AnsiString
             Width, Height, Bits: Integer
             Fullscreen: Boolean
             VideoFlag: TelVideoFlags

    }
    function CreateWindow(aName: AnsiString; Width, Height, Bits: Integer; Fullscreen: Boolean; VideoFlag: TelVideoFlag): Boolean; Overload; inline;

    {
      Application.CreateWindow

      @seealso
    }
    function CreateWindow(aName: AnsiString; Width, Height, Bits: Integer; Fullscreen: Boolean = false): Boolean; Overload; inline;

  published
    // Use Application.Run for the main loop
    property Run: Boolean read IsRun;

    {
      If UnicodeSupport is active, all keydown events are switched to unicode
      and true type fonts will be automatically set for unicode mode
    }
    property UnicodeSupport: Boolean read GetUnicodeSupport write SetUnicodeSupport;
end;


var
  // Application
  Application: TelApplication;

implementation

uses
  ElysionWindowManager,
  ElysionTexture;

{
  #############################################################################
  # TelApplication                                                             #
  #############################################################################

  Description:


  Additional Notes: Essential class

}

constructor TelApplication.Create;
begin
  inherited Create;

  UnicodeSupport := true;
end;

destructor TelApplication.Destroy;
begin
  //Finalize();

  inherited Destroy;

  if Self.Debug then TelLogger.GetInstance.Dump();

  Halt(0);
end;

(*function TelApplication.Initialize: Boolean;
begin
  if SDL_Init(SDL_INIT_EVERYTHING) <> 0 then
  begin
    Result := false;
    Self.Log(rsFailedInitWindowProvider);
    Exit;
  end else FInitialized := true;

  SDL_JoystickEventState(SDL_ENABLE);
end;

procedure TelApplication.Finalize();
begin
  TelWindowManager.DestroyAllWindows();

  if FInitialized then
  begin
    SDL_Quit;
  end;
end; *)

function TelApplication.CreateWindow(aName: AnsiString; Width, Height, Bits: Integer; Fullscreen: Boolean; VideoFlag: TelVideoFlag): Boolean;
begin
  Result := (TelWindowManager.CreateWindow(aName, Width, Height, Bits, Fullscreen, VideoFlag) <> nil);
end;

function TelApplication.CreateWindow(aName: AnsiString; Width, Height, Bits: Integer; Fullscreen: Boolean = false): Boolean;
begin
  Result := Self.CreateWindow(aName, Width, Height, Bits, Fullscreen, vfAuto);
end;

function TelApplication.GetUnicodeSupport: Boolean;
begin
  if SDL_EnableUNICODE(-1) = 0 then Result := false
                               else Result := true;
end;

procedure TelApplication.SetUnicodeSupport(Value: Boolean);
begin
  if Value then SDL_EnableUNICODE(1)
           else SDL_EnableUNICODE(0);
end;

function TelApplication.IsRun: Boolean;
begin

end;

end.
