(********************************************************)
(*                                                      *)
(*  File Search Utility                                 *)
(*  A portable cross platform visual file search tool   *)
(*                                                      *)
(*  https://www.getlazarus.org/apps/filesearch          *)
(*  Released under the GPLv3 September 2019             *)
(*                                                      *)
(********************************************************)
unit SizeFrm;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, EditBtn, ExtCtrls,
  SearchTools, ExpandFrm;

{ TSizeFrame }

type
  TSizeFrame = class(TExpandFrame)
    AndLabel: TLabel;
    SizeBox: TComboBox;
    FromEdit: TEdit;
    ToEdit: TEdit;
    UnitsBox: TComboBox;
    procedure FromEditExit(Sender: TObject);
    procedure SizeBoxChange(Sender: TObject);
    procedure ToEditExit(Sender: TObject);
  private
    FFromValue: Double;
    FToValue: Double;
    procedure SetFromValue(Value: Double);
    procedure SetToValue(Value: Double);
  public
    constructor Create(AOwner: TComponent); override;
    procedure ArrangeLayout; override;
    procedure Prepare(Params: TSearchParams); override;
    property FromValue: Double read FFromValue write SetFromValue;
    property ToValue: Double read FToValue write SetToValue;
  end;

implementation

{$R *.lfm}

{ TSizeFrame }

const
  idxLessThan = 0;
  idxGreaterThan = 1;
  idxBetween = 2;

constructor TSizeFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := 'What size is it?';
  FromEdit.MaxLength:= 5;
  ToEdit.MaxLength:= 5;
  SizeBox.OnChange := SizeBoxChange;
  FromValue := 100;
  ToValue := 1000;
end;

procedure TSizeFrame.ArrangeLayout;
const
  Margin = 8;
var
  M: Integer;
begin
  SizeBox.Top := DefalutHeight + Margin;
  M := SizeBox.Top + SizeBox.Height div 2;
  FromEdit.Left := SizeBox.Left + SizeBox.Width + Margin;
  FromEdit.Top := M - FromEdit.Height div 2;
  AndLabel.Left := FromEdit.Left + FromEdit.Width + Margin;
  AndLabel.Top := M - AndLabel.Height div 2;
  ToEdit.Left := AndLabel.Left + AndLabel.Width + Margin;
  ToEdit.Top := M - ToEdit.Height div 2;
  case SizeBox.ItemIndex of
    idxLessThan..idxGreaterThan:
      begin
        UnitsBox.Left := FromEdit.Left + FromEdit.Width + Margin;
        UnitsBox.Top := M - UnitsBox.Height div 2;
        ToEdit.Enabled := False;
        AndLabel.Enabled := False;
        ToEdit.Visible := False;
        AndLabel.Visible := False;
      end;
  idxBetween:
    begin
      UnitsBox.Left := ToEdit.Left + ToEdit.Width + Margin;
      UnitsBox.Top := M - UnitsBox.Height div 2;
      ToEdit.Enabled := Expanded;
      AndLabel.Enabled := Expanded;
      ToEdit.Visible := Expanded;
      AndLabel.Visible := Expanded;
    end;
  end;
end;

procedure TSizeFrame.Prepare(Params: TSearchParams);
var
  M: Double;
  I: Integer;
begin
  Params.SizeRange:= Expanded;
  M := 1;
  I := UnitsBox.ItemIndex;
  while I > 0 do
  begin
    M := M * 1024;
    Dec(I);
  end;
  case SizeBox.ItemIndex of
    idxLessThan:
      begin
        Params.SizeFrom := 0;
        Params.SizeTo := Round(M * FFromValue);
      end;
    idxGreaterThan:
      begin
        Params.SizeFrom := Round(M * FFromValue);
        Params.SizeTo := High(Int64) div 2;
      end;
    idxBetween:
      begin
        Params.SizeFrom := Round(M * FFromValue);
        Params.SizeTo := Round(M * FToValue);
      end;
  end;
  Params.SizeFrom := Params.SizeFrom - 1;
  Params.SizeTo := Params.SizeTo + 1;
end;

procedure TSizeFrame.SizeBoxChange(Sender: TObject);
begin
  ArrangeLayout;
end;

procedure TSizeFrame.FromEditExit(Sender: TObject);
begin
  FFromValue := StrToFloatDef(FromEdit.Text, FFromValue);
  if FFromValue < 0 then
    FFromValue := 0;
  FromEdit.Text := FloatToStr(FFromValue);
end;

procedure TSizeFrame.ToEditExit(Sender: TObject);
begin
  FToValue := StrToFloatDef(ToEdit.Text, FToValue);
  if FToValue < 0 then
    FToValue := 0;
  ToEdit.Text := FloatToStr(FToValue);
end;

procedure TSizeFrame.SetFromValue(Value: Double);
begin
  if Value < 0 then
    Value := 0;
  if FFromValue = Value then Exit;
  FFromValue := Value;
  FromEdit.Text := FloatToStr(FToValue);
end;

procedure TSizeFrame.SetToValue(Value: Double);
begin
  if Value < 0 then
    Value := 0;
  if FToValue = Value then Exit;
  FToValue := Value;
  ToEdit.Text := FloatToStr(FToValue);
end;

end.

