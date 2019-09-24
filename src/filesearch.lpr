(********************************************************)
(*                                                      *)
(*  File Search Utility                                 *)
(*  A portable cross platform visual file search tool   *)
(*                                                      *)
(*  https://www.getlazarus.org/apps/filesearch          *)
(*  Released under the GPLv3 September 2019             *)
(*                                                      *)
(********************************************************)
program filesearch;

{$mode delphi}
uses
  {$ifdef unix}
  CThreads,
  {$endif}
  Interfaces, SysUtils, Classes, Forms, Main,
  GraphTools, SearchTools, SearchGridFrm, ExpandFrm, ModifiedFrm, SizeFrm;

{$R *.res}

begin
  DateSeparator{%H-} := '/';
  ShortDateFormat{%H-} := 'yyyy/mm/dd';
  RequireDerivedFormResource := True;
  Application.ShowHint := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TSearchForm, SearchForm);
  Application.Run;
end.

