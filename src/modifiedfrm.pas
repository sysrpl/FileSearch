(********************************************************)
(*                                                      *)
(*  File Search Utility                                 *)
(*  A portable cross platform visual file search tool   *)
(*                                                      *)
(*  https://www.getlazarus.org/apps/filesearch          *)
(*  Released under the GPLv3 September 2019             *)
(*                                                      *)
(********************************************************)
unit ModifiedFrm;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, EditBtn,
  SearchTools, ExpandFrm;

{ TModifiedFrame }

type
  TModifiedFrame = class(TExpandFrame)
    WhenBox: TComboBox;
    FromEdit: TDateEdit;
    ToEdit: TDateEdit;
    AndLabel: TLabel;
    procedure WhenBoxChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure ArrangeLayout; override;
    procedure Prepare(Params: TSearchParams); override;
  end;

implementation

{$R *.lfm}

{ TModifiedFrame }

const
  idxToday = 0;
  idxWeek = 1;
  idxMonth = 2;
  idxAfter = 3;
  idxBefore = 4;
  idxBetween = 5;

constructor TModifiedFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := 'When was it modified?';
  FromEdit.Date := Now - 7;
  ToEdit.Date := Now;
end;

procedure TModifiedFrame.ArrangeLayout;
const
  Margin = 8;
var
  M: Integer;
begin
  WhenBox.Top := DefalutHeight + Margin;
  M := WhenBox.Top + WhenBox.Height div 2;
  FromEdit.Left := WhenBox.Left + WhenBox.Width + Margin;
  FromEdit.Top := M - FromEdit.Height div 2;
  AndLabel.Left := FromEdit.Left + FromEdit.Width + Margin;
  AndLabel.Top := M - AndLabel.Height div 2;
  ToEdit.Left := AndLabel.Left + AndLabel.Width + Margin;
  ToEdit.Top := M - ToEdit.Height div 2;
  case WhenBox.ItemIndex of
    idxToday..idxMonth:
      begin
        FromEdit.Enabled := False;
        ToEdit.Enabled := False;
        AndLabel.Enabled := False;
        FromEdit.Visible := False;
        ToEdit.Visible := False;
        AndLabel.Visible := False;
      end;
  idxAfter..idxBefore:
    begin
      FromEdit.Enabled := Expanded;
      ToEdit.Enabled := False;
      AndLabel.Enabled := False;
      FromEdit.Visible := Expanded;
      ToEdit.Visible := False;
      AndLabel.Visible := False;
    end;
  else
    FromEdit.Enabled := Expanded;
    ToEdit.Enabled := Expanded;
    AndLabel.Enabled := Expanded;
    FromEdit.Visible := Expanded;
    ToEdit.Visible := Expanded;
    AndLabel.Visible := Expanded;
  end;
end;

procedure TModifiedFrame.Prepare(Params: TSearchParams);
begin
  Params.DateRange := Expanded;
  case WhenBox.ItemIndex of
    idxToday:
      begin
        Params.DateTo := Now;
        Params.DateFrom := Params.DateTo - 1;
      end;
    idxWeek:
      begin
        Params.DateTo := Now;
        Params.DateFrom := Params.DateTo - 7;
      end;
    idxMonth:
      begin
        Params.DateTo := Now;
        Params.DateFrom := Params.DateTo - 31;
      end;
    idxAfter:
      begin
        Params.DateFrom := FromEdit.Date - 0.00001;
        Params.DateTo := Now + 99999;
      end;
    idxBefore:
      begin
        Params.DateTo := FromEdit.Date + 1;
        Params.DateFrom := Params.DateTo - 99999;
      end;
    idxBetween:
      begin
        Params.DateFrom := FromEdit.Date - 0.00001;
        Params.DateTo := ToEdit.Date + 1;
      end;
  end;
end;

procedure TModifiedFrame.WhenBoxChange(Sender: TObject);
begin
  ArrangeLayout;
end;

end.

