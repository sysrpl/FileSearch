(********************************************************)
(*                                                      *)
(*  File Search Utility                                 *)
(*  A portable cross platform visual file search tool   *)
(*                                                      *)
(*  https://www.getlazarus.org/apps/filesearch          *)
(*  Released under the GPLv3 September 2019             *)
(*                                                      *)
(********************************************************)
unit GraphTools;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics;

type
  TEllipsesMode = (emNone, emBegin, emEnd);

function PtInRect(const P: TPoint; const R: TRect): Boolean;

procedure DrawText(Canvas: TCanvas; const S: string; Rect: TRect;
  Alignment: TAlignment = taLeftJustify; Ellipses: TEllipsesMode = emNone);

implementation

function PtInRect(const P: TPoint; const R: TRect): Boolean;
begin
  Result := (P.X >= R.Left) and (P.Y >= R.Top) and (P.X < R.Right) and (P.Y < R.Bottom);
end;

var
  EllipsesWidth: Integer;

procedure DrawText(Canvas: TCanvas; const S: string; Rect: TRect;
  Alignment: TAlignment = taLeftJustify; Ellipses: TEllipsesMode = emNone);
var
  Style: TTextStyle;
begin
  if Rect.Right - Rect.Left < 5 then
    Exit;
  Style := Canvas.TextStyle;
  Style.Alignment := Alignment;
  Style.Clipping := True;
  Style.Layout := tlCenter;
  if (Ellipses = emNone) or (Canvas.TextWidth(S) <= Rect.Width) then
    Canvas.TextRect(Rect, Rect.Left, Rect.Top, S, Style)
  else
  begin
    if EllipsesWidth = 0 then
      EllipsesWidth := Canvas.TextWidth('…');
    if Rect.Width <= EllipsesWidth then
      Exit;
    if Ellipses = emBegin then
    begin
      Style.Alignment := taLeftJustify;
      Canvas.TextRect(Rect, Rect.Left, Rect.Top, '…', Style);
      Style.Alignment := taRightJustify;
      Rect.Left := Rect.Left + EllipsesWidth;
      Canvas.TextRect(Rect, Rect.Left, Rect.Top, S, Style);
    end
    else
    begin
      Style.Alignment := taRightJustify;
      Canvas.TextRect(Rect, Rect.Left, Rect.Top, '…', Style);
      Style.Alignment := taLeftJustify;
      Rect.Right := Rect.Right - EllipsesWidth;
      Canvas.TextRect(Rect, Rect.Left, Rect.Top, S, Style);
    end;
  end;
end;

end.

