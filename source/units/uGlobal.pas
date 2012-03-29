(*
 * This is for constants used in your game
 * 
 *
 *)

unit uGlobal;

interface

{$I Elysion.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}
  {$DEFINE GLOBAL_PROPERTY}
{$ENDIF}

uses
  uBasic;

var
  Music: Boolean = true;
  Sound: Boolean = true;

implementation


(*{$IFDEF GLOBAL_PROPERTY}

function getGameState: TGameState; {$IFDEF CAN_INLNE} inline; {$ENDIF}
procedure setGameState(aGameState: TGameState); {$IFDEF CAN_INLNE} inline; {$ENDIF}

property GameState: TGameState read getGameState write setGameState;
{$ELSE}
  GameState: TGameState;
{$ENDIF}

implementation

{$IFDEF GLOBAL_PROPERTY}

var
  fGameState: TGameState;

function getGameState: TGameState;
begin
  Result := fGameState;
end;

procedure setGameState(aGameState: TGameState);
begin
  fGameState := aGameState;
end;

{$ENDIF}*)

end.
