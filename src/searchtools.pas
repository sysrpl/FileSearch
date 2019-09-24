(********************************************************)
(*                                                      *)
(*  File Search Utility                                 *)
(*  A portable cross platform visual file search tool   *)
(*                                                      *)
(*  https://www.getlazarus.org/apps/filesearch          *)
(*  Released under the GPLv3 September 2019             *)
(*                                                      *)
(********************************************************)
unit SearchTools;

{$mode delphi}

interface

uses
  Classes, SysUtils;

{ TSearchParams }

type
  TSearchParams = class(TPersistent)
  private
    FPatterns: TStringList;
    FFolder: string;
    FPattern: string;
    FText: string;
    FRecursive: Boolean;
    FCaseSensitive: Boolean;
    FDateRange: Boolean;
    FDateFrom: TDateTime;
    FDateTo: TDateTime;
    FSizeRange: Boolean;
    FSizeFrom: Int64;
    FSizeTo: Int64;
    procedure Prepare;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Folder: string read FFolder write FFolder;
    property Pattern: string read FPattern write FPattern;
    property Text: string read FText write FText;
    property Recursive: Boolean read FRecursive write FRecursive;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
    property DateRange: Boolean read FDateRange write FDateRange;
    property DateFrom: TDateTime read FDateFrom write FDateFrom;
    property DateTo: TDateTime read FDateTo write FDateTo;
    property SizeRange: Boolean read FSizeRange write FSizeRange;
    property SizeFrom: Int64 read FSizeFrom write FSizeFrom;
    property SizeTo: Int64 read FSizeTo write FSizeTo;
  end;

{ TFileItem }

  TFileItem = class
  private
    FIsFolder: Boolean;
    FFolder: TFileItem;
    FFullName: string;
    FMatches: Integer;
    FModified: TDateTime;
    FSize: Int64;
    FCount: Integer;
    function GetName: string;
  public
    property IsFolder: Boolean read FIsFolder;
    property Folder: TFileItem read FFolder;
    property Name: string read GetName;
    property FullName: string read FFullName;
    property Matches: Integer read FMatches;
    property Modified: TDateTime read FModified;
    property Size: Int64 read FSize;
    property Count: Integer read FCount;
  end;

{ TFileSearch }

  TFileColumn = (fcName, fcSize, fcModified, fcMatches);

  TFileSearch = class
  private
    FDestroying: Boolean;
    FList: TList;
    FMatches: Integer;
    FThread: TThread;
    FOnSearchStart: TNotifyEvent;
    FOnSearchChange: TNotifyEvent;
    FOnSearchFinish: TNotifyEvent;
    procedure Clear;
    function GetFile(Index: Integer): TFileItem;
    function GetFileCount: Integer;
    function GetSearching: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Cancel;
    procedure Search(Params: TSearchParams);
    procedure Sort(Column: TFileColumn; Ascending: Boolean);
    property Searching: Boolean read GetSearching;
    property Files[Index: Integer]: TFileItem read GetFile;
    property FileCount: Integer read GetFileCount;
    property Matches: Integer read FMatches;
    property OnSearchStart: TNotifyEvent read FOnSearchStart write FOnSearchStart;
    property OnSearchChange: TNotifyEvent read FOnSearchChange write FOnSearchChange;
    property OnSearchFinish: TNotifyEvent read FOnSearchFinish write FOnSearchFinish;
  end;

implementation

type
  TCanContinue = function: Boolean of object;
  TOnFindFile = procedure(Folder: string; FileName: string; const Search: TSearchRec; CanContinue: TCanContinue) of object;

function SysCompare(const A, B: string): Integer; inline;
begin
  {$ifdef unix}
  Result := CompareStr(A, B);
  {$else}
  Result := CompareText(A, B);
  {$endif}
end;

function CaseSort(List: TStringList; Index1, Index2: Integer): Integer;
begin
  if Index1 = Index2 then
    Result := 0
  else
    Result := SysCompare(List[Index1], List[Index2]);
end;

const
{$ifdef unix}
  faSymbolic = faSymlink{%H-};
  faHide = faHidden{%H-};
{$else}
  faSymbolic = 0;
  faHide = 0;
{$endif}
  faFile = faAnyFile or faHide or faSymbolic;
  faFolder = faDirectory or faHide or faSymbolic;

function IsNotFolder(const S: TSearchRec): Boolean; inline;
begin
  Result := S.Attr and faDirectory = 0;
end;

function IsFolder(const S: TSearchRec): Boolean; inline;
begin
  Result := (S.Attr and faDirectory = faDirectory) and (S.Attr and faSymbolic = 0) and (S.Name <> '.')
    and (S.Name <> '..');
end;

procedure FindFiles(const Folder: string; Params: TSearchParams; OnFindFile: TOnFindFile; CanContinue: TCanContinue);
var
  Items: TStringList;
  M: TDateTime;
  S: TSearchRec;
  F, P: string;
  I: Integer;
begin
  if not Assigned(OnFindFile) then
    Exit;
  F :=  Trim(Folder);
  if F = '' then
    F := GetUserDir;
  F := IncludeTrailingPathDelimiter(F);
  Items := TStringList.Create;
  try
    for I := 0 to Params.FPatterns.Count - 1 do
    begin
      P := Params.FPatterns[I];
      if FindFirst(F + P, faFile, S) = 0 then
      try
        repeat
          if not CanContinue then
            Exit;
          if IsNotFolder(S) then
          begin
            if Params.SizeRange then
              if (S.Size < Params.SizeFrom) or (S.Size > Params.SizeTo) then
                Continue;
            if Params.DateRange then
            begin
              M := FileDateToDateTime(S.Time);
              if (M < Params.DateFrom) or (M > Params.DateTo) then
                Continue;
            end;
            if (Items.IndexOf(S.Name) < 0) then
            begin
              Items.Add(S.Name);
              OnFindFile(F, S.Name, S, CanContinue);
            end;
          end;
        until FindNext(S) <> 0;
      finally
        FindClose(S);
      end;
    end;
  finally
    Items.Free;
  end;
  if Params.Recursive and CanContinue then
  begin
    Items := TStringList.Create;
    try
      if FindFirst(F + '*', faFolder, S) = 0 then
      try
        repeat
          if not CanContinue then
            Exit;
          if IsFolder(S) then
            Items.Add(F + S.Name);
        until FindNext(S) <> 0;
      finally
        FindClose(S);
      end;
      Items.CustomSort(CaseSort);
      for F in Items do
      begin
        if not CanContinue then
          Exit;
        FindFiles(F, Params, OnFindFile, CanContinue);
      end;
    finally
      Items.Free;
    end;
  end;
end;

function FindInFile(const FileName: string; const Pattern: string;
  CaseSensitive: Boolean; Buffer: PChar; BufferSize: Integer;
  CanContinue: TCanContinue): Integer;
var
  Stream: TFileStream;
  S: string;
  P: PChar;
  B, I, J: Integer;
begin
  Result := 0;
  if not FileExists(FileName) then
    Exit;
  if Pattern = '' then
    Exit;
  if Buffer = nil then
    Exit;
  if BufferSize < 1 then
    Exit;
  if CaseSensitive then
    S := Pattern
  else
    S := UpperCase(Pattern);
  Stream := nil;
  try
    Stream := TFileStream.Create(FileName, fmOpenRead);
    B := Stream.Read(Buffer^, BufferSize);
    I := 1;
    while (B > 0) and CanContinue do
    begin
      P := Buffer;
      for J := 0 to B - 1 do
      begin
        if CaseSensitive then
        begin
          if P[J] = S[I] then
            Inc(I)
          else
            I := 1;
        end
        else
        begin
          if UpCase(P[J]) = S[I] then
            Inc(I)
          else
            I := 1;
        end;
        if S[I] = #0 then
        begin
          I := 1;
          Inc(Result);
        end;
      end;
      B := Stream.Read(Buffer^, BufferSize);
    end;
    Stream.Free;
  except
    Stream.Free;
    Result := 0;
  end;
end;

{ TSearchParams }

constructor TSearchParams.Create;
begin
  inherited Create;
  FPatterns := TStringList.Create;
  FPatterns.Sorted := True;
  FPatterns.Duplicates := dupIgnore;
end;

destructor TSearchParams.Destroy;
begin
  FPatterns.Free;
  inherited Destroy;
end;

procedure TSearchParams.Assign(Source: TPersistent);
var
  P: TSearchParams;
begin
  if Source = Self then
    Exit;
  if Source is TSearchParams then
  begin
    P := Source as TSearchParams;
    Folder := P.Folder;
    Pattern := P.Pattern;
    Text := P.Text;
    Recursive := P.Recursive;
    CaseSensitive := P.CaseSensitive;
    DateRange := P.DateRange;
    DateFrom := P.DateFrom;
    DateTo := P.DateTo;
    SizeRange := P.SizeRange;
    SizeFrom := P.SizeFrom;
    SizeTo := P.SizeTo;
    Prepare;
  end
  else
    inherited Assign(Source);
end;

procedure TSearchParams.Prepare;
var
  Items: TStringList;
  S: string;
  I: Integer;
begin
  FFolder := Trim(FFolder);
  FPattern := Trim(FPattern);
  FText := Trim(FText);
  Items := TStringList.Create;
  try
    Items.Delimiter := ';';
    Items.DelimitedText := FPattern;
    FPatterns.Clear;
    for I := 0 to Items.Count - 1 do
    begin
      S := Trim(Items[I]);
      if S <> '' then
        FPatterns.Add(S);
    end;
    if FPatterns.Count = 0 then
      FPatterns.Add('*');
  finally
    Items.Free;
  end;
end;

{ TFileItem }

function TFileItem.GetName: string;
begin
  if FIsFolder then
    Result := FFullName
  else
    Result := ExtractFileName(FFullName);
end;

{ TSearchParams }

const
  MaxAdd = 100;
  MaxItems = 25;

type
  TSearchThread = class(TThread)
  private
    FSearch: TFileSearch;
    FParams: TSearchParams;
    FFolder: TFileItem;
    FItem: TFileItem;
    FBuffer: PChar;
    FBufferSize: Integer;
    FAddCount: Integer;
    FAddIndex: Integer;
    FItems: array[0..MaxItems - 1] of TFileItem;
    function AllowContinue: Boolean;
    procedure DoChange;
    procedure DoFinish;
    procedure HandleFindFile(Folder: string; FileName: string;
      const Search: TSearchRec; CanContinue: TCanContinue);
  protected
    procedure Execute; override;
  public
    constructor Create(Search: TFileSearch; Params: TSearchParams);
    destructor Destroy; override;
  end;

constructor TSearchThread.Create(Search: TFileSearch; Params: TSearchParams);
begin
  FSearch := Search;
  FSearch.FThread := Self;
  FParams := Params;
  inherited Create(False);
end;

destructor TSearchThread.Destroy;
begin
  FParams.Free;
  Inherited Destroy;
end;

function TSearchThread.AllowContinue: Boolean;
begin
  Result := (not Terminated) and (FSearch.FThread = Self);
end;

procedure TSearchThread.DoChange;
var
  I: Integer;
begin
  if AllowContinue then
  begin
    if FAddIndex = 0 then
    begin
      FSearch.FList.Add(FItem);
      if not FItem.IsFolder then
        Inc(FSearch.FMatches);
      if Assigned(FSearch.OnSearchChange) then
        FSearch.OnSearchChange(FSearch);
      Inc(FAddCount);
    end
    else
    begin
      for I := 0 to FAddIndex - 1 do
      begin
        FItem := FItems[I];
        FSearch.FList.Add(FItem);
        if not FItem.IsFolder then
          Inc(FSearch.FMatches);
        Inc(FAddCount);
      end;
      FAddIndex := 0;
      if Assigned(FSearch.OnSearchChange) then
        FSearch.OnSearchChange(FSearch);
    end;
  end
  else if FAddIndex = 0 then
    FItem.Free
  else
  begin
    for I := 0 to FAddIndex - 1 do
      FItems[I].Free;
    FAddIndex := 0;
  end;
  FItem := nil;
end;

procedure TSearchThread.DoFinish;
var
  I: Integer;
begin
  if AllowContinue then
  begin
    if FAddIndex > 0 then
      DoChange;
    FSearch.FThread := nil;
    if Assigned(FSearch.OnSearchFinish) then
      FSearch.OnSearchFinish(FSearch);
  end
  else
  begin
    FItem.Free;
    for I := 0 to FAddIndex - 1 do
      begin
        FItems[I].Free;
        FAddIndex := 0;
      end;
  end;
  for I := 0 to FAddIndex - 1 do
    FItems[I].Free;
end;

procedure TSearchThread.HandleFindFile(Folder: string; FileName: string;
  const Search: TSearchRec; CanContinue: TCanContinue);
var
  Success: Boolean;
  S: string;
  M: Integer;
begin
  Success := CanContinue;
  S := IncludeTrailingPathDelimiter(Folder) + FileName;
  Folder := ExcludeTrailingPathDelimiter(Folder);
  if Success and (FBuffer <> nil) then
  begin
    M := FindInFile(S, FParams.Text, FParams.CaseSensitive, FBuffer,
      FBufferSize, CanContinue);
    Success := M > 0;
  end
  else
    M := 0;
  Success := Success and CanContinue;
  if Success and ((FFolder = nil) or (FFolder.FFullName <> Folder)) then
  begin
    FFolder := TFileItem.Create;
    FFolder.FIsFolder := True;
    FFolder.FFolder := FFolder;
    FFolder.FFullName := Folder;
    FItem := FFolder;
    if FAddCount < MaxAdd then
      Synchronize(DoChange)
    else
    begin
      FItems[FAddIndex] := FItem;
      Inc(FAddIndex);
      if FAddIndex = MaxItems then
        Synchronize(DoChange);
    end;
  end;
  Success := Success and CanContinue;
  if Success then
  begin
    Inc(FFolder.FCount);
    FItem := TFileItem.Create;
    FItem.FFolder := FFolder;
    FItem.FFullName := S;
    FItem.FMatches := M;
    FItem.FModified := FileDateToDateTime(Search.Time);
    FItem.FSize := Search.Size;
    if FAddCount < MaxAdd then
      Synchronize(DoChange)
    else
    begin
      FItems[FAddIndex] := FItem;
      Inc(FAddIndex);
      if FAddIndex = MaxItems then
        Synchronize(DoChange);
    end;
  end;
end;

procedure TSearchThread.Execute;
begin
  FreeOnTerminate := True;
  if FParams.Text <> '' then
  begin
    FBufferSize := 4096;
    FBuffer := GetMem(FBufferSize);
  end
  else
    FBuffer := nil;
  FindFiles(FParams.Folder, FParams, HandleFindFile, AllowContinue);
  if FBuffer <> nil then
    FreeMem(FBuffer);
  if not Terminated then
    Synchronize(DoFinish);
end;

{ TFileSearch }

constructor TFileSearch.Create;
begin
  inherited Create;
  FList := TList.Create;
  FList.Capacity := 10000;
end;

destructor TFileSearch.Destroy;
begin
  FDestroying := True;
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TFileSearch.Cancel;
var
  T: TThread;
begin
  if FThread <> nil then
  begin
    T := FThread;
    FThread := nil;
    T.Terminate;
    if FDestroying then
      Exit;
    if Assigned(FOnSearchFinish) then
      FOnSearchFinish(Self);
  end;
end;

procedure TFileSearch.Clear;
var
  I: Integer;
begin
  Cancel;
  for I := 0 to FList.Count - 1 do
    TObject(FList[I]).Free;
  FList.Clear;
  FMatches := 0;
end;

procedure TFileSearch.Search(Params: TSearchParams);
var
  P: TSearchParams;
begin
  Clear;
  if Assigned(FOnSearchStart) then
    FOnSearchStart(Self);
  P := TSearchParams.Create;
  P.Assign(Params);
  FThread := TSearchThread.Create(Self, P);
end;

function SortFolder(Item1, Item2: Pointer): Integer;
var
  A: TFileItem absolute Item1;
  B: TFileItem absolute Item2;
begin
  if A.Folder = B.Folder then
    Result := 0
  else
    Result := SysCompare(A.Folder.FullName, B.Folder.FullName);
end;

function SortNameAsc(Item1, Item2: Pointer): Integer;
var
  A: TFileItem absolute Item1;
  B: TFileItem absolute Item2;
begin
  if A = B then
    Exit(0);
  Result := SortFolder(Item1, Item2);
  if Result = 0 then
    if A.IsFolder then
      Result := -1
    else if B.IsFolder then
      Result := 1
    else
      Result := SysCompare(A.Name, B.Name);
end;

function SortNameDesc(Item1, Item2: Pointer): Integer;
var
  A: TFileItem absolute Item1;
  B: TFileItem absolute Item2;
begin
  if A = B then
    Exit(0);
  Result := SortFolder(Item1, Item2);
  if Result = 0 then
    if A.IsFolder then
      Result := -1
    else if B.IsFolder then
      Result := 1
    else
      Result := -SysCompare(A.Name, B.Name);
end;

function SortModifiedAsc(Item1, Item2: Pointer): Integer;
var
  A: TFileItem absolute Item1;
  B: TFileItem absolute Item2;
begin
  if A = B then
    Exit(0);
  Result := SortFolder(Item1, Item2);
  if Result = 0 then
    if A.IsFolder then
      Result := -1
    else if B.IsFolder then
      Result := 1
    else
    begin
      if A.Modified < B.Modified then
        Result := -1
      else if A.Modified > B.Modified then
        Result := 1
      else
        Result := SortNameAsc(Item1, Item2);
    end
end;

function SortModifiedDesc(Item1, Item2: Pointer): Integer;
var
  A: TFileItem absolute Item1;
  B: TFileItem absolute Item2;
begin
  if A = B then
    Exit(0);
  Result := SortFolder(Item1, Item2);
  if Result = 0 then
    if A.IsFolder then
      Result := -1
    else if B.IsFolder then
      Result := 1
    else
    begin
      if A.Modified < B.Modified then
        Result := 1
      else if A.Modified > B.Modified then
        Result := -1
      else
        Result := SortNameAsc(Item1, Item2);
    end
end;

function SortSizeAsc(Item1, Item2: Pointer): Integer;
var
  A: TFileItem absolute Item1;
  B: TFileItem absolute Item2;
begin
  if A = B then
    Exit(0);
  Result := SortFolder(Item1, Item2);
  if Result = 0 then
    if A.IsFolder then
      Result := -1
    else if B.IsFolder then
      Result := 1
    else
    begin
      if A.Size < B.Size then
        Result := -1
      else if A.Size > B.Size then
        Result := 1
      else
        Result := SortNameAsc(Item1, Item2);
    end
end;

function SortSizeDesc(Item1, Item2: Pointer): Integer;
var
  A: TFileItem absolute Item1;
  B: TFileItem absolute Item2;
begin
  if A = B then
    Exit(0);
  Result := SortFolder(Item1, Item2);
  if Result = 0 then
    if A.IsFolder then
      Result := -1
    else if B.IsFolder then
      Result := 1
    else
    begin
      if A.Size < B.Size then
        Result := 1
      else if A.Size > B.Size then
        Result := -1
      else
        Result := SortNameAsc(Item1, Item2);
    end
end;

function SortMatchesAsc(Item1, Item2: Pointer): Integer;
var
  A: TFileItem absolute Item1;
  B: TFileItem absolute Item2;
begin
  if A = B then
    Exit(0);
  Result := SortFolder(Item1, Item2);
  if Result = 0 then
    if A.IsFolder then
      Result := -1
    else if B.IsFolder then
      Result := 1
    else
    begin
      if A.Matches < B.Matches then
        Result := -1
      else if A.Matches > B.Matches then
        Result := 1
      else
        Result := SortNameAsc(Item1, Item2);
    end
end;

function SortMatchesDesc(Item1, Item2: Pointer): Integer;
var
  A: TFileItem absolute Item1;
  B: TFileItem absolute Item2;
begin
  if A = B then
    Exit(0);
  Result := SortFolder(Item1, Item2);
  if Result = 0 then
    if A.IsFolder then
      Result := -1
    else if B.IsFolder then
      Result := 1
    else
    begin
      if A.Matches < B.Matches then
        Result := 1
      else if A.Matches > B.Matches then
        Result := -1
      else
        Result := SortNameAsc(Item1, Item2);
    end
end;

procedure TFileSearch.Sort(Column: TFileColumn; Ascending: Boolean);
begin
  case Column of
    fcName:
      if Ascending then FList.Sort(SortNameAsc) else FList.Sort(SortNameDesc);
    fcModified:
      if Ascending then FList.Sort(SortModifiedAsc) else FList.Sort(SortModifiedDesc);
    fcSize:
      if Ascending then FList.Sort(SortSizeAsc) else FList.Sort(SortSizeDesc);
    fcMatches:
      if Ascending then FList.Sort(SortMatchesAsc) else FList.Sort(SortMatchesDesc);
  end;
end;

function TFileSearch.GetFile(Index: Integer): TFileItem;
begin
  Result := TFileItem(FList[Index]);
end;

function TFileSearch.GetFileCount: Integer;
begin
  Result := FList.Count;
end;

function TFileSearch.GetSearching: Boolean;
begin
  Result := FThread <> nil;
end;

end.

