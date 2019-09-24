(********************************************************)
(*                                                      *)
(*  File Search Utility                                 *)
(*  A portable cross platform visual file search tool   *)
(*                                                      *)
(*  https://www.getlazarus.org/apps/filesearch          *)
(*  Released under the GPLv3 September 2019             *)
(*                                                      *)
(********************************************************)
unit SearchGridFrm;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ComCtrls, StdCtrls, ExtCtrls,
  Types, Clipbrd, LCLIntf, Grids, Menus, GraphTools, SearchTools;

{ TSearchGrid }

type
  TSearchGrid = class(TFrame)
    OpenLocationMenuItem: TMenuItem;
    N1: TMenuItem;
    CopyLocationMenuItem: TMenuItem;
    CopyNameMenuItem: TMenuItem;
    SearchGrid: TDrawGrid;
    SearchMenu: TPopupMenu;
    SortHeader: THeaderControl;
    procedure MenuItemClick(Sender: TObject);
    procedure SearchGridDblClick(Sender: TObject);
    procedure SearchGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      ARect: TRect; AState: TGridDrawState);
    procedure SearchGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SortHeaderSectionClick(HeaderControl: TCustomHeaderControl;
      Section: THeaderSection);
    procedure SortHeaderSectionDrag(Sender: TObject; FromSection,
      ToSection: THeaderSection; var AllowDrag: Boolean);
    procedure SortHeaderSectionEndDrag(Sender: TObject);
    procedure SortHeaderSectionResize(HeaderControl: TCustomHeaderControl;
      Section: THeaderSection);
    procedure SortHeaderSectionTrack(HeaderControl: TCustomHeaderControl;
      Section: THeaderSection; Width: Integer; State: TSectionTrackState);
  private
    FImage: TRasterImage;
    FSearch: TFileSearch;
    FColumn: TFileColumn;
    FAscending: Boolean;
    FSections: array of THeaderSection;
    procedure SortChange;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure SearchStart(Matches: Boolean);
    procedure SearchChange;
    procedure SearchFinish;
    property Search: TFileSearch read FSearch write FSearch;
  end;

implementation

{$R *.lfm}
{$R folder.res}

{ TSearchGrid

  The following characters are used to symbolize sorting order:

  ⯅  2BC5 unicode medium up arrow
  ⯆  2BC6 unicode medium down arrow }

constructor TSearchGrid.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited Create(AOwner);
  BorderStyle := bsSingle;
  SearchGrid.ColWidths[0] := 9000;
  FAscending := True;
  FImage := TPortableNetworkGraphic.Create;
  FImage.LoadFromResourceName(0, 'folder');
  SetLength(FSections, SortHeader.Sections.Count);
  for I := 0 to Length(FSections) - 1 do
    FSections[I] := SortHeader.Sections.Items[I];
  FSections[Length(FSections) - 1].Visible := False;
end;

destructor TSearchGrid.Destroy;
begin
  FImage.Free;
  inherited Destroy;
end;

procedure TSearchGrid.SortChange;
const
  Arrows: array[Boolean] of string = (' ⯆', ' ⯅');
var
  S: THeaderSection;
begin
  FSearch.Sort(FColumn, FAscending);
  SearchGrid.Invalidate;
  FSections[0].Text := 'Name';
  FSections[1].Text := 'Size';
  FSections[2].Text := 'Modified';
  FSections[3].Text := 'Matches';
  S := FSections[Ord(FColumn)];
  S.Text := S.Text + Arrows[FAscending];
end;

procedure TSearchGrid.Resize;
begin
  inherited Resize;
  if SearchGrid <> nil then
    SearchGrid.Invalidate;
  if SortHeader <> nil then
    SortHeader.Invalidate;
end;

procedure TSearchGrid.SortHeaderSectionClick(HeaderControl: TCustomHeaderControl;
  Section: THeaderSection);
begin
  if FSearch.FileCount = 0 then
    Exit;
  if FSearch.Searching then
    Exit;
  if Section.OriginalIndex <> Ord(FColumn) then
  begin
    FColumn := fcName;
    Inc(FColumn, Section.OriginalIndex);
  end
  else
    FAscending := not FAscending;
  SortChange;
end;

procedure TSearchGrid.SortHeaderSectionDrag(Sender: TObject; FromSection,
  ToSection: THeaderSection; var AllowDrag: Boolean);
begin
  if FSearch.FileCount > 0 then
    SearchGrid.Invalidate;
end;

procedure TSearchGrid.SortHeaderSectionEndDrag(Sender: TObject);
begin
  if FSearch.FileCount > 0 then
    SearchGrid.Invalidate;
end;

procedure TSearchGrid.SortHeaderSectionResize(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  if FSearch.FileCount > 0 then
    SearchGrid.Invalidate;
end;

procedure TSearchGrid.SortHeaderSectionTrack(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection; Width: Integer;
  State: TSectionTrackState);
begin
  if FSearch.FileCount > 0 then
    SearchGrid.Invalidate;
end;

function BytesToStr(B: Int64): string;
var
  D: Double;
begin
  D := B;
  if D < 1024 then
    Exit(IntToStr(B) + ' bytes');
  D := D / 1024;
  if D < 1024 then
    Exit(Format('%.1f', [D]) + ' KB');
  D := D / 1024;
  if D < 1024 then
    Exit(Format('%.1f', [D]) + ' MB');
  D := D / 1024;
  if D < 1024 then
    Exit(Format('%.1f', [D]) + ' GB');
  D := D / 1024;
  Result := Format('%.1f', [D]) + ' TB';
end;

procedure TSearchGrid.SearchGridDrawCell(Sender: TObject; ACol, ARow: Integer;
  ARect: TRect; AState: TGridDrawState);
var
  Item: TFileItem;
  R: TRect;
  S: string;
begin
  Item := FSearch.Files[ARow];
  ARect.Right := SearchGrid.ClientRect.Right;
  if (gdSelected in AState) and SearchGrid.Focused then
  begin
    SearchGrid.Canvas.Font.Color := clHighlightText;
    SearchGrid.Canvas.Brush.Color := clHighlight
  end
  else
  begin
    SearchGrid.Canvas.Font.Color := clWindowText;
    SearchGrid.Canvas.Brush.Color := clDefault;
  end;
  R := ARect;
  SearchGrid.Canvas.FillRect(R);
  if FSearch.Files[ARow].IsFolder then
  begin
    InflateRect(R, -2, -2);
    SearchGrid.Canvas.Brush.Color := clBtnShadow;
    SearchGrid.Canvas.FillRect(R);
  end;
  InflateRect(R, -4, 0);
  if Item.IsFolder then
  begin
    R.Left := R.Left + 25;
    R.Right := R.Right - 60;
    DrawText(SearchGrid.Canvas, Item.FullName, R, taLeftJustify, emBegin);
    R.Right := ARect.Right - 8;
    if Item.Count = 1 then
      S := ' file'
    else
      S := ' files';
    DrawText(SearchGrid.Canvas, IntToStr(Item.Count) + S, R, taRightJustify);
    if FImage <> nil then
      SearchGrid.Canvas.Draw(ARect.Left + 3, ARect.Top + 1, FImage);
  end
  else
  begin
    R.Left := FSections[0].Left;
    R.Right := R.Left + FSections[0].Width;
    InflateRect(R, -4, 0);
    DrawText(SearchGrid.Canvas, Item.Name, R, taLeftJustify, emEnd);
    R.Left := FSections[1].Left;
    R.Right := R.Left + FSections[1].Width;
    InflateRect(R, -4, 0);
    DrawText(SearchGrid.Canvas, BytesToStr(Item.Size), R, taLeftJustify, emEnd);
    R.Left := FSections[2].Left;
    R.Right := R.Left + FSections[2].Width;
    InflateRect(R, -4, 0);
    if Abs(Now -Item.Modified) < 1 then
      S := 'h:nn:ss AM/PM'
    else
      S := 'ddd d mmm yyyy h:nn:ss AM/PM';
    DrawText(SearchGrid.Canvas, FormatDateTime(S, Item.Modified), R, taLeftJustify, emEnd);
    if Item.Matches > 0 then
    begin
      R.Left := FSections[3].Left;
      R.Right := R.Left + FSections[3].Width;
      InflateRect(R, -4, 0);
      DrawText(SearchGrid.Canvas, IntToStr(Item.Matches), R, taRightJustify, emEnd);
    end;
  end;
end;

procedure TSearchGrid.SearchGridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  I: Integer;
begin
  if Button = mbRight then
  begin
    I := SearchGrid.Selection.Top;
    if I < 0 then
      Exit;
    P.X := X;
    P.Y := Y;
    P := SearchGrid.ClientToScreen(P);
    SearchMenu.PopUp(P.X, P.Y);
  end;
end;

procedure TSearchGrid.MenuItemClick(Sender: TObject);
var
  Item: TFileItem;
  I: Integer;
begin
  I := SearchGrid.Selection.Top;
  if I < 0 then
    Exit;
  Item := FSearch.Files[I];
  I := TComponent(Sender).Tag;
  if I = 0 then
  begin
    if not Item.IsFolder then
      Item := Item.Folder;
    OpenDocument(Item.FullName);
  end
  else
  begin
    if (I = 1) and (not Item.IsFolder) then
      Item := Item.Folder;
    Clipboard.AsText := Item.FullName;
  end;
end;

procedure TSearchGrid.SearchGridDblClick(Sender: TObject);
var
  Item: TFileItem;
  I: Integer;
begin
  I := SearchGrid.Selection.Top;
  if I < 0 then
    Exit;
  Item := FSearch.Files[I];
  if not Item.IsFolder then
    Item := Item.Folder;
  OpenDocument(Item.FullName);
end;

procedure TSearchGrid.Clear;
begin
  SearchGrid.RowCount := 0;
end;

procedure TSearchGrid.SearchStart(Matches: Boolean);
begin
  SearchGrid.RowCount := 0;
  FSections[0].Text := 'Name';
  FSections[1].Text := 'Size';
  FSections[2].Text := 'Modified';
  FSections[3].Text := 'Matches';
  FSections[3].Visible := Matches;
  if (not Matches) and (FColumn = fcMatches) then
  begin
    FColumn := fcName;
    FAscending := True;
  end;
end;

procedure TSearchGrid.SearchChange;
begin
  SearchGrid.RowCount := FSearch.FileCount;
end;

procedure TSearchGrid.SearchFinish;
begin
  SortChange;
end;

end.

