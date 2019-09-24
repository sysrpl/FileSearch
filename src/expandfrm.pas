(********************************************************)
(*                                                      *)
(*  File Search Utility                                 *)
(*  A portable cross platform visual file search tool   *)
(*                                                      *)
(*  https://www.getlazarus.org/apps/filesearch          *)
(*  Released under the GPLv3 September 2019             *)
(*                                                      *)
(********************************************************)
unit ExpandFrm;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Graphics, Controls, StdCtrls, ExtCtrls,
  SearchTools, GraphTools;

{ TExpandFrame }

type
  THeightChangeEvent = procedure(Sender: TObject; Delta: Integer) of object;

  TExpandFrame = class(TFrame)
    MouseTimer: TTimer;
    procedure MouseTimerTimer(Sender: TObject);
  private
    FCaptionArea: TRect;
    FDefalutHeight: Integer;
    FExpanded: Boolean;
    FMoused: Boolean;
    FOnExpand: TNotifyEvent;
    FPressed: Boolean;
    FOnHeightChange: THeightChangeEvent;
    procedure SetExpanded(Value: Boolean);
  protected
    procedure ExpandChange; virtual;
    procedure HeightChange(NewHeight: Integer); virtual;
    procedure Paint; override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);  override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    property DefalutHeight: Integer read FDefalutHeight;
  public
    procedure Prepare(Params: TSearchParams); virtual;
    procedure ArrangeLayout; virtual;
  published
    property Caption;
    property Expanded: Boolean read FExpanded write SetExpanded;
    property OnExpand: TNotifyEvent read FOnExpand write FOnExpand;
    property OnHeightChange: THeightChangeEvent read FOnHeightChange write FOnHeightChange;
  end;

implementation

{$R *.lfm}

{ u23F5 and u23F7 }

const
  ExpandIcons: array[Boolean] of string = ('⏵ ', '⏷ ');

procedure TExpandFrame.Loaded;
begin
  inherited Loaded;
  ExpandChange;
end;

procedure TExpandFrame.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FPressed := (Button = mbLeft) and FMoused;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TExpandFrame.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Clicked: Boolean;
begin
  Clicked := (Button = mbLeft) and FMoused and FPressed;
  FPressed := False;
  if Clicked then
    Expanded := not Expanded;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TExpandFrame.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  M: Boolean;
begin
  M := PtInRect(Point(X, Y), FCaptionArea);
  if M <> FMoused then
  begin
    FMoused := M;
    Invalidate;
    if M then
      MouseTimer.Enabled := True;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TExpandFrame.Paint;
begin
  if FDefalutHeight = 0 then
    ExpandChange;
  FCaptionArea := ClientRect;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(FCaptionArea);
  FCaptionArea.Bottom := FDefalutHeight;
  DrawText(Canvas, ExpandIcons[FExpanded], FCaptionArea);
  FCaptionArea.Left := Canvas.TextWidth(ExpandIcons[FExpanded]);
  if Caption <> '' then
  begin
    FCaptionArea.Right := FCaptionArea.Left + Canvas.TextWidth(Caption);
    if FCaptionArea.Right > Width - 4 then
      FCaptionArea.Right := Width - 4;
    if FMoused then
    begin
      Canvas.Pen.Color := clWindowText;
      Canvas.Pen.Style := psSolid;
      Canvas.MoveTo(FCaptionArea.Left, FCaptionArea.Bottom - 1);
      Canvas.LineTo(FCaptionArea.Right, FCaptionArea.Bottom - 1);
    end;
    DrawText(Canvas, Caption, FCaptionArea, taLeftJustify, emEnd);
  end
  else
    FCaptionArea.Right := FCaptionArea.Left;
  FCaptionArea.Left := 0;
end;

procedure TExpandFrame.Prepare(Params: TSearchParams);
begin
end;

procedure TExpandFrame.ArrangeLayout;
begin
end;

procedure TExpandFrame.ExpandChange;
const
  Margin = 4;
var
  C: TControl;
  H, I: Integer;
begin
  if Parent = nil then
    Exit;
  if FDefalutHeight = 0 then
    FDefalutHeight := Canvas.TextHeight('Wg');
  H := FDefalutHeight;
  if FExpanded then
  begin
    H := H * 2;
    for I := 0 to ControlCount - 1 do
    begin
      C := Controls[I];
      C.Visible := True;
      if not C.Enabled then
      begin
        C.Visible := False;
        Continue;
      end;
      if C.Top + C.Height > H then
        H := C.Top + C.Height + Margin;
    end;
  end
  else for I := 0 to ControlCount - 1 do
    Controls[I].Visible := False;
  HeightChange(H);
  if Assigned(FOnExpand) then
    FOnExpand(Self);
  ArrangeLayout;
end;

procedure TExpandFrame.HeightChange(NewHeight: Integer);
var
  Delta: Integer;
begin
  if NewHeight <> Height then
  begin
    Delta := NewHeight - Height;
    Height := NewHeight;
    if Assigned(FOnHeightChange) then
      FOnHeightChange(Self, Delta);
  end;
end;

procedure TExpandFrame.MouseTimerTimer(Sender: TObject);
var
  P: TPoint;
begin
  P := Mouse.CursorPos;
  P := ScreenToClient(P);
  if not PtInRect(P, FCaptionArea) then
  begin
    MouseTimer.Enabled := False;
    FMoused := False;
    Invalidate;
  end;
end;

procedure TExpandFrame.SetExpanded(Value: Boolean);
begin
  if FExpanded = Value then Exit;
  if CanSetFocus then
  begin
    SetFocus;
    Invalidate;
  end;
  FExpanded := Value;
  ExpandChange;
end;

end.

