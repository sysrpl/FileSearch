(********************************************************)
(*                                                      *)
(*  File Search Utility                                 *)
(*  A portable cross platform visual file search tool   *)
(*                                                      *)
(*  https://www.getlazarus.org/apps/filesearch          *)
(*  Released under the GPLv3 September 2019             *)
(*                                                      *)
(********************************************************)
unit Main;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, Menus, Clipbrd, ExtCtrls, SearchTools, SearchGridFrm, ExpandFrm,
  ModifiedFrm, SizeFrm;

{ TSearchForm }

type
  TSearchForm = class(TForm)
    FileEdit: TComboBox;
    FolderEdit: TComboBox;
    TextLabel: TLabel;
    SearchButton: TButton;
    CloseButton: TButton;
    BrowseButton: TButton;
    RecurseBox: TCheckBox;
    CaseBox: TCheckBox;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    TextEdit: TEdit;
    FileLabel: TLabel;
    FolderLabel: TLabel;
    MatchLabel: TLabel;
    SearchTimer: TTimer;
    procedure BrowseButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SearchButtonClick(Sender: TObject);
    procedure SearchTimerTimer(Sender: TObject);
  private
    FSearch: TFileSearch;
    FModifiedFrame: TModifiedFrame;
    FSizeFrame: TSizeFrame;
    FSearchGrid: TSearchGrid;
    FMatch: string;
    procedure UpdateProgress;
    procedure ExpandChange(Sender: TObject);
    procedure SearchChange(Sender: TObject);
    procedure SearchFinish(Sender: TObject);
    procedure SearchStart(Sender: TObject);
  end;

var
  SearchForm: TSearchForm;

implementation

{$R *.lfm}

{ TSearchForm }

procedure TSearchForm.FormCreate(Sender: TObject);
var
  Settings: TStrings;
  S: string;
  I: Integer;
begin
  FSearch := TFileSearch.Create;
  FSearch.OnSearchStart := SearchStart;
  FSearch.OnSearchChange := SearchChange;
  FSearch.OnSearchFinish := SearchFinish;
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  FModifiedFrame := TModifiedFrame.Create(Self);
  FModifiedFrame.Parent := Self;
  FModifiedFrame.Left := 8;
  FModifiedFrame.TabOrder := 6;
  FModifiedFrame.OnExpand := ExpandChange;
  FSizeFrame := TSizeFrame.Create(Self);
  FSizeFrame.Parent := Self;
  FSizeFrame.Left := 8;
  FSizeFrame.TabOrder := 7;
  FSizeFrame.OnExpand := ExpandChange;
  FSearchGrid := TSearchGrid.Create(Self);
  FSearchGrid.Search := FSearch;
  FSearchGrid.Parent := Self;
  FSearchGrid.Left := 8;
  FSearchGrid.Top := SearchButton.Top + SearchButton.Height + 8;
  FSearchGrid.Width := ClientWidth - 16;
  FSearchGrid.Height := ClientHeight - FSearchGrid.Top - 8;
  FSearchGrid.Anchors := [akLeft, akTop, akRight, akBottom];
  FolderEdit.Text := GetUserDir;
  S := GetAppConfigDir(False);
  S := IncludeTrailingPathDelimiter(S) + 'settings.conf';
  if FileExists(S) then
  begin
    Settings := TStringList.Create;
    try
      Settings.LoadFromFile(S);
      FolderEdit.Text := Trim(Settings.Values['folder']);
      for I := 9 downto 0 do
      begin
        S := Trim(Settings.Values['folder' + IntToStr(I)]);
        if S <> '' then
          FolderEdit.Items.Add(S);
      end;
      FileEdit.Text := Settings.Values['file'];
      for I := 9 downto 0 do
      begin
        S := Trim(Settings.Values['file' + IntToStr(I)]);
        if S <> '' then
          FileEdit.Items.Add(S);
      end;
      TextEdit.Text := Settings.Values['text'];
      RecurseBox.Checked := Settings.Values['recurse'] = 'Y' ;
      CaseBox.Checked := Settings.Values['case'] = 'Y' ;
      FModifiedFrame.Expanded := Settings.Values['daterange'] = 'Y';
      FModifiedFrame.WhenBox.ItemIndex := StrToIntDef(Settings.Values['datekind'], 0);
      FModifiedFrame.FromEdit.Date := StrToDateDef(Settings.Values['datefrom'], FModifiedFrame.FromEdit.Date);
      FModifiedFrame.ToEdit.Date := StrToDateDef(Settings.Values['dateto'], FModifiedFrame.ToEdit.Date);
      FSizeFrame.Expanded := Settings.Values['sizerange'] = 'Y';
      FSizeFrame.SizeBox.ItemIndex := StrToIntDef(Settings.Values['sizekind'], 0);
      FSizeFrame.FromValue := StrToFloatDef(Settings.Values['sizefrom'], FSizeFrame.FromValue);
      FSizeFrame.ToValue := StrToFloatDef(Settings.Values['sizeto'], FSizeFrame.ToValue);
      FSizeFrame.UnitsBox.ItemIndex := StrToIntDef(Settings.Values['sizeunits'], 0);
    finally
      Settings.Free;
    end;
  end;
  ExpandChange(Self);
end;

procedure TSearchForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
const
  BoolStr: array[Boolean] of string = ('N', 'Y');
var
  Settings: TStrings;
  S: string;
  I: Integer;
begin
  FSearchGrid.Clear;
  FSearch.Free;
  SearchTimer.Enabled := False;
  S := GetAppConfigDir(False);
  ForceDirectories(S);
  S := IncludeTrailingPathDelimiter(S) + 'settings.conf';
  Settings := TStringList.Create;
  try
    Settings.Values['folder'] := Trim(FolderEdit.Text);
    for I := 0 to FolderEdit.Items.Count - 1 do
      Settings.Values['folder' + IntToStr(I)] := FolderEdit.Items[I];
    Settings.Values['file'] := Trim(FileEdit.Text);
    for I := 0 to FileEdit.Items.Count - 1 do
      Settings.Values['file' + IntToStr(I)] := FileEdit.Items[I];
    Settings.Values['text'] := Trim(TextEdit.Text);
    Settings.Values['recurse'] := BoolStr[RecurseBox.Checked];
    Settings.Values['case'] := BoolStr[CaseBox.Checked];
    Settings.Values['daterange'] := BoolStr[FModifiedFrame.Expanded];
    Settings.Values['datekind'] := IntToStr(FModifiedFrame.WhenBox.ItemIndex);
    Settings.Values['datefrom'] := FModifiedFrame.FromEdit.Text;
    Settings.Values['dateto'] := FModifiedFrame.ToEdit.Text;
    Settings.Values['sizerange'] := BoolStr[FSizeFrame.Expanded];
    Settings.Values['sizekind'] := IntToStr(FSizeFrame.SizeBox.ItemIndex);
    Settings.Values['sizefrom'] := FSizeFrame.FromEdit.Text;
    Settings.Values['sizeto'] := FSizeFrame.ToEdit.Text;
    Settings.Values['sizeunits'] := IntToStr(FSizeFrame.UnitsBox.ItemIndex);
    Settings.SaveToFile(S);
  finally
    Settings.Free;
  end;
end;

procedure TSearchForm.BrowseButtonClick(Sender: TObject);
begin
  if SelectDirectoryDialog.Execute then
    FolderEdit.Text := SelectDirectoryDialog.FileName;
end;

procedure TSearchForm.CloseButtonClick(Sender: TObject);
begin
  FSearch.Cancel;
  Close;
end;

procedure TSearchForm.FormShow(Sender: TObject);
begin
  OnShow := nil;
  BrowseButton.Top := FolderEdit.Top + FolderEdit.Height div 2 -
    BrowseButton.Height div 2;
  {$ifdef darwin}
  FModifiedFrame.Expanded := not FModifiedFrame.Expanded;
  FModifiedFrame.Expanded := not FModifiedFrame.Expanded;
  FSizeFrame.Expanded := not FSizeFrame.Expanded;
  FSizeFrame.Expanded := not FSizeFrame.Expanded;
  ExpandChange(Self);
  {$endif}
end;

procedure TSearchForm.ExpandChange(Sender: TObject);
begin
  FModifiedFrame.Top := RecurseBox.Top + RecurseBox.Height + 8;
  FSizeFrame.Top := FModifiedFrame.Top + FModifiedFrame.Height + 4;
  SearchButton.Top := FSizeFrame.Top + FSizeFrame.Height + 12;
  CloseButton.Top := SearchButton.Top;
  MatchLabel.Top := CloseButton.Top + CloseButton.Height div 2  - MatchLabel.Height div 2;
  FSearchGrid.Top := CloseButton.Top + CloseButton.Height + 8;
  FSearchGrid.Height := ClientHeight - FSearchGrid.Top - 8;
end;

procedure TSearchForm.UpdateProgress;
const
  Progress = '............';
begin
  MatchLabel.Caption := FMatch + ' ' + Copy(Progress, 1, SearchTimer.Tag);
end;

procedure TSearchForm.SearchStart(Sender: TObject);
begin
  SearchButton.Caption := 'Cancel';
  FMatch := '0 files found';
  FSearchGrid.SearchStart(Trim(TextEdit.Text) <> '');
  SearchTimer.Tag := 0;
  SearchTimer.Enabled := True;
  UpdateProgress;
end;

procedure TSearchForm.SearchChange(Sender: TObject);
begin
  FSearchGrid.SearchChange;
  if FSearch.Matches = 1 then
    FMatch := '1 file found'
  else
    FMatch := IntToStr(FSearch.Matches) + ' files found';
  UpdateProgress;
  {$ifdef darwin}
  Tag := (Tag + 1) mod 3;
  if Tag = 0 then
    Application.ProcessMessages;
  {$endif}
end;

procedure TSearchForm.SearchFinish(Sender: TObject);
begin
  SearchButton.Caption := 'Search';
  FSearchGrid.SearchFinish;
  SearchTimer.Enabled := False;
  SearchTimer.Tag := 0;
  MatchLabel.Caption := FMatch;
end;

procedure TSearchForm.SearchButtonClick(Sender: TObject);
var
  P: TSearchParams;
  S: string;
  I: Integer;
begin
  if SearchButton.Caption = 'Search' then
  begin
    P := TSearchParams.Create;
    try
      P.Folder := Trim(FolderEdit.Text);
      P.Pattern := Trim(FileEdit.Text);
      P.Text := Trim(TextEdit.Text);
      P.CaseSensitive := CaseBox.Checked;
      P.Recursive := RecurseBox.Checked;
      S := P.Folder;
      if S <> '' then
      begin
        I := FolderEdit.Items.IndexOf(S);
        if I > -1 then
          FolderEdit.Items.Delete(I);
        FolderEdit.Items.Insert(0, S);
        while FolderEdit.Items.Count > 10 do
          FolderEdit.Items.Delete(10);
        FolderEdit.Text := S;
      end;
      S := P.Pattern;
      if S <> '' then
      begin
        I := FileEdit.Items.IndexOf(S);
        if I > -1 then
          FileEdit.Items.Delete(I);
        FileEdit.Items.Insert(0, S);
        while FileEdit.Items.Count > 10 do
          FileEdit.Items.Delete(10);
        FileEdit.Text := S;
      end;
      FModifiedFrame.Prepare(P);
      FSizeFrame.Prepare(P);
      FSearch.Search(P);
    finally
      P.Free;
    end;
  end
  else
    FSearch.Cancel;
end;

procedure TSearchForm.SearchTimerTimer(Sender: TObject);
begin
  SearchTimer.Tag := (SearchTimer.Tag + 1) mod 4;
  UpdateProgress;
end;

end.

